!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    get_list_remnode_mod   ../starter/source/interfaces/inter3d1/get_list_remnode.F90
!||--- called by ------------------------------------------------------
!||    i7remnode              ../starter/source/interfaces/inter3d1/i7remnode.F
!||====================================================================
      module get_list_remnode_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief For each main segment, find all secondary nodes that are "too close" on the mesh surface
!!        and build a list of node/segment pairs to exclude from collision detection.
!! \details This routine prevents false-positive contact pairs between a main segment and
!!          secondary nodes that are topologically adjacent (connected through shared edges/nodes
!!          on the mesh surface). The "closeness" is measured as the geodesic distance along the
!!          mesh surface (sum of Euclidean edge lengths), NOT the straight-line 3D distance.
!!
!!          ALGORITHM: For each main segment i:
!!            1. Compute dmax = maximum removal distance based on gap parameters
!!            2. BFS (Breadth-First Search) expansion from segment i through mesh connectivity:
!!               - Use node-to-segment connectivity (knod2seg/nod2seg) to find adjacent segments
!!               - Track cumulative mesh-surface distance to each visited node
!!               - Stop expanding when minimum distance of frontier exceeds dmax
!!            3. Collect all secondary nodes within dmax → store in remnode array
!!
!!          PARALLELISM: Called inside an OMP PARALLEL region. Each thread allocates its own
!!          local arrays (Fortran allocatables are thread-private). The !$omp do distributes
!!          segments across threads. A barrier + single section does the prefix sum on kremnode.
!!
!!          COMPLEXITY: O(nrtm * avg_BFS_expansion). Each BFS can visit O(dmax/minseg)^2
!!          segments on a 2D surface mesh → quadratic in (dmax/minseg).
!!
!!          OPTIMIZATION OPPORTUNITIES:
!!            - Replace level-based BFS with Dijkstra (priority queue) for earlier cutoff
!!            - Pre-filter with bounding-box / spatial hashing to skip distant segments entirely
!!            - Reduce per-thread memory: arrays sized numnod/nrtm allocated per thread
!!            - Consider C++ for better data structures (priority_queue, flat_hash_set)
!!            - The BFS expands ALL segments in a level before checking distance → wasteful
!!              if most nodes in a level already exceed dmax
!||====================================================================
!||    get_list_remnode   ../starter/source/interfaces/inter3d1/get_list_remnode.F90
!||--- called by ------------------------------------------------------
!||    i7remnode          ../starter/source/interfaces/inter3d1/i7remnode.F
!||--- calls      -----------------------------------------------------
!||    upgrade_remnode    ../starter/source/interfaces/interf1/upgrade_remnode.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine get_list_remnode(nrtm,igap ,numnod,npari,irect,kremnode, &
          knod2seg,nod2seg,tagsecnd,   &
          ipari,gapmin,gapmax,gap,drad,         &
          gaps_mx,gaps_l_mx,  &
          minseg,dgapload,x,gap_m,              &
          gap_m_l,gapsecnd,gap_s_l_tmp,    &
          intbuf_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use intbufdef_mod , only : intbuf_struct_
          use constant_mod
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nrtm !< number of main segments (quad/tri surface elements)
          integer, intent(in) :: igap !< gap formulation flag: 0=constant, 1/2=variable, 3=variable+local
          integer, intent(in) :: numnod !< total number of nodes in mesh
          integer, intent(in) :: npari !< first dimension of ipari array
          integer, dimension(4,nrtm), intent(in) ::  irect !< segment connectivity: 4 node IDs per segment (4th=0 for tris)
          integer, dimension(nrtm+1), intent(inout) ::  kremnode !< CSR index for remnode: kremnode(i+1)-kremnode(i) = nb of removed nodes for seg i
          integer, dimension(numnod+1), intent(in) ::  knod2seg !< CSR index for node-to-segment adjacency
          integer, dimension(4*nrtm), intent(in) ::  nod2seg !< CSR values: list of segments connected to each node
          integer, dimension(numnod), intent(in) :: tagsecnd !< 1 if node is a secondary (slave) node, 0 otherwise
          integer, dimension(npari), intent(in) :: ipari !< interface integer parameters
          real(kind=WP) , intent(in) :: gapmin !< minimum allowed gap value (clamp)
          real(kind=WP) , intent(in) :: gapmax !< maximum allowed gap value (clamp)
          real(kind=WP) , intent(in) :: gap !< constant gap value (used when igap==0)
          real(kind=WP) , intent(in) :: drad !< draw radius - minimum removal distance
          real(kind=WP) , intent(in) :: gaps_mx !< max secondary gap across all S nodes
          real(kind=WP) , intent(in) :: gaps_l_mx !< max local secondary gap across all S nodes
          real(kind=WP) , intent(in) :: minseg !< minimum segment edge length (used for BFS termination heuristic)
          real(kind=WP) , intent(in) :: dgapload !< additional gap due to loading
          real(kind=WP), dimension(3,numnod), intent(in) :: x !< nodal coordinates (x,y,z)
          real(kind=WP), dimension(nrtm), intent(in) :: gap_m !< per-segment main (master) gap
          real(kind=WP), dimension(nrtm), intent(in) :: gap_m_l !< per-segment main local gap
          real(kind=WP), dimension(numnod), intent(in) :: gapsecnd !< per-node secondary gap
          real(kind=WP), dimension(numnod), intent(in) :: gap_s_l_tmp !< per-node secondary local gap
          type(intbuf_struct_), intent(inout) :: intbuf_tab !< interface buffer (output: remnode array)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------

          integer :: i,j,k,seg,iseg,level,cpt,nbseg,seg1,l,cpt1,cpt_total
          integer :: cptoper         ! count of visited secondary nodes (for cleanup)
          integer :: jmax,kmax,nty
          integer :: my_size,my_new_size,local_remnode_size
          integer :: node_id,node_id_seg1,my_seg_id,my_seg_number
          integer :: my_local_address,my_address
          integer, dimension(:), allocatable :: tmp_array,local_remnode  ! thread-local output buffer (grows dynamically)
          integer, dimension(:,:), allocatable :: local_kremnode         ! thread-local CSR index: (seg_local_id, 1=offset, 2=global seg id)
          integer, dimension(:), allocatable :: listseg      ! current BFS frontier (segments to expand)
          integer, dimension(:), allocatable :: listsegtmp   ! next BFS frontier (newly discovered segments)
          integer, dimension(:), allocatable :: listsegtotal ! all segments visited during BFS (for cleanup)
          integer, dimension(:), allocatable :: itagseg      ! per-segment BFS visit marker (0=unvisited, level=visited at that level)
          integer, dimension(:), allocatable :: id_nod       ! list of all secondary nodes visited during BFS (for cleanup/filtering)
          integer, dimension(:), allocatable :: noddel       ! temporary: nodes within dmax to be stored in remnode
          integer, dimension(:), allocatable :: nod2expand   ! per-node flag: 1=already expanded neighbors from this node
          integer, dimension(:), allocatable :: tagnod       ! per-node state: 0=unvisited, 1=visited, 2=belongs to current frontier segment
          real(kind=WP), dimension(:), allocatable :: dist1  ! per-node best geodesic distance from source segment
          real(kind=WP), dimension(:), allocatable :: gapv   ! per-node effective gap (computed during BFS for igap>0)
          real(kind=WP) :: mindist  ! minimum distance found at current BFS level (controls termination)
          real(kind=WP) :: dmax     ! maximum removal distance for current segment
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
          ! MEMORY NOTE: All these arrays are allocated PER THREAD (called inside OMP PARALLEL).
          ! For large meshes with many threads, this is a significant memory overhead.
          ! OPTIMIZATION: Could size these to max expected BFS radius rather than full mesh size.
          ! E.g., if dmax/minseg ~ 5 levels, only ~25 segments are visited on average.
          my_seg_number = 0
          allocate(noddel(numnod),nod2expand(numnod) )
          allocate(listseg(nrtm),listsegtmp(nrtm),listsegtotal(nrtm))
          allocate( id_nod(numnod) )
          allocate( tagnod(numnod) )
          allocate( dist1(numnod) )
          allocate( gapv(numnod) )
          allocate(itagseg(nrtm) )
          id_nod(1:numnod) = 0
          nod2expand(1:numnod) = 0
          tagnod(1:numnod) = 0
          dist1(1:numnod) = ep30      ! "infinity" - no distance computed yet
          itagseg(1:nrtm) = 0
          gapv(1:numnod) = zero

          ! Thread-local output buffer (will grow dynamically if needed)
          local_remnode_size = 4*nrtm
          allocate( local_remnode(local_remnode_size) )
          allocate( local_kremnode(nrtm+1,2) )  ! col1=cumulative offset, col2=global segment ID
          local_kremnode(1,1:2) = 0
!$omp do schedule(guided)
          do i=1,nrtm
            my_seg_number = my_seg_number + 1

            ! ---------------
            ! STEP 1: Compute maximum removal distance (dmax) for this segment.
            ! Any secondary node with mesh-geodesic distance <= dmax from segment i
            ! will be excluded from collision detection with segment i.
            ! The sqrt(2) factor accounts for diagonal mesh traversal.
            ! ---------------
            dmax = zero
            if(igap==0)then
              ! Constant gap: same dmax for all segments
              dmax  = sqrt(two) * max(gap+dgapload,drad)
            else if(igap==1 .or. igap==2) then
              ! Variable gap: per-segment main gap + worst-case secondary gap
              dmax  = sqrt(two) * max(gap_m(i)+gaps_mx+dgapload,drad)
            else if(igap==3) then
              ! Variable gap with local constraint: min of global and local gap sums
              dmax  = sqrt(two) * max(min(gap_m(i)+gaps_mx,gap_m_l(i)+gaps_l_mx)+dgapload,drad)
            end if

            ! ---------------
            ! STEP 2: Initialize BFS from segment i.
            ! - Mark segment i as visited (itagseg = level 1)
            ! - Set distance = 0 for all nodes of segment i
            ! - These nodes are the "source" of the distance computation
            ! ---------------
            cptoper = 0       ! counter of visited secondary nodes
            level   = 1       ! current BFS level
            seg     = i       ! starting segment
            itagseg(seg) = level
            nbseg     = 1     ! number of segments in current frontier
            mindist   = zero  ! min distance found at current level
            jmax      = 4     ! nodes per segment (3 for tris, 4 for quads)
            cpt_total = 0     ! total segments visited (for cleanup)

            ! Detect triangle: 4th node is 0 or node3==node4 → triangle
            if((irect(jmax,seg) == 0) .or. irect(3,seg) == irect(4,seg) ) jmax = 3
            do j=1,jmax
              node_id = irect(j,seg)
              tagnod(node_id) = 1    ! mark as visited
              dist1(node_id) = zero  ! distance = 0 (source nodes)
            end do
            listseg(1)=seg           ! initial BFS frontier = just segment i

            ! ----------------------------------
            ! STEP 3: BFS expansion loop.
            ! Termination: stop when the minimum frontier distance + minseg exceeds dmax,
            ! meaning no further expansion can find nodes within the removal zone.
            ! NOTE: This is a level-based BFS (not Dijkstra). It processes ALL segments at
            ! the current level before advancing. This can be wasteful when most frontier
            ! nodes already exceed dmax but a few don't.
            ! OPTIMIZATION: A Dijkstra approach (priority queue sorted by distance) would
            ! allow per-node early termination and avoid expanding nodes already > dmax.
            ! ----------------------------------
            do while( (mindist + minseg) <= dmax .and. nbseg /= 0)
              level   = level + 1
              mindist = ep30   ! reset min distance for this new level
              cpt     = 0     ! counter for next frontier

              ! --- Process each segment in the current BFS frontier ---
              do iseg=1,nbseg
                seg  = listseg(iseg)
                jmax = 4
                if((irect(jmax,seg) == 0) .or. irect(3,seg) == irect(4,seg) ) jmax = 3
                ! Mark nodes of frontier segment with state=2 ("currently being expanded")
                ! This prevents computing distance from a node to itself within the same segment
                tagnod(irect(1:jmax,seg))=2

                ! --- For each node of this frontier segment, find adjacent segments ---
                do j=1,jmax
                  node_id = irect(j,seg)
                  if(nod2expand(node_id)/=0) cycle ! skip if already expanded from this node

                  nod2expand(node_id)=1 ! mark: don't expand from this node again

                  ! Traverse node→segment adjacency (CSR structure: knod2seg/nod2seg)
                  do k=knod2seg(node_id)+1,knod2seg(node_id+1)
                    seg1 = nod2seg(k) ! neighbor segment sharing node_id

                    kmax = 4
                    if((irect(kmax,seg1) == 0) .or. irect(3,seg1) == irect(4,seg1) ) kmax = 3

                    ! Only process segments not seen before OR seen at this same level
                    ! (a segment can be reached from multiple frontier nodes in the same level)
                    if(itagseg(seg1) == 0 .or. itagseg(seg1) == level) then
                      if(itagseg(seg1) == 0)then
                        ! New segment → add to next frontier
                        cpt = cpt + 1
                        listsegtmp(cpt)=seg1
                      end if
                      itagseg(seg1)=level  ! mark segment as visited at this level

                      ! --- Compute distances to nodes of the neighbor segment ---
                      do l=1,kmax
                        node_id_seg1 = irect(l,seg1)
                        ! Skip if: not a secondary node OR belongs to current frontier segment
                        ! (tagnod==2 means it's a node of 'seg', distance already known)
                        if(tagsecnd(node_id_seg1)== 0 .or.tagnod(node_id_seg1) == 2)cycle

                        ! Geodesic distance = dist_to(node_id) + Euclidean(node_id → node_id_seg1)
                        ! Take minimum over all paths (relaxation, like Dijkstra)
                        dist1(node_id_seg1)=min(dist1(node_id_seg1),dist1(node_id)+  &
                          sqrt((x(1,node_id_seg1) - x(1,node_id))**2 +             &
                          (x(2,node_id_seg1) - x(2,node_id))**2 +             &
                          (x(3,node_id_seg1) - x(3,node_id))**2 ))
                        mindist=min(mindist,dist1(node_id_seg1))
                        if(tagnod(node_id_seg1) == 0) then ! first time visiting this node
                          cptoper = cptoper + 1
                          tagnod(node_id_seg1) = 1   ! mark as visited
                          id_nod(cptoper)=node_id_seg1 ! record for later filtering & cleanup
                        end if

                        ! Compute per-node effective gap for variable-gap modes.
                        ! This determines the per-node removal threshold (used in step 4).
                        ! NOTE: This is recomputed every time a shorter path is found,
                        ! but the gap value depends only on the node, not the path.
                        ! OPTIMIZATION: Compute gapv once when node is first visited.
                        if(igap==1 .or. igap==2)then
                          gapv(irect(l,seg1))=gapsecnd(irect(l,seg1))+gap_m(i)
                          gapv(irect(l,seg1))=min(gapmax,gapv(irect(l,seg1)))
                          gapv(irect(l,seg1))=max(gapmin,gapv(irect(l,seg1)))
                          gapv(irect(l,seg1))=max(drad,gapv(irect(l,seg1))+dgapload)
                        else if(igap==3)then
                          gapv(irect(l,seg1))=gapsecnd(irect(l,seg1))+gap_m(i)
                          gapv(irect(l,seg1))= min(gap_s_l_tmp(irect(l,seg1))+gap_m_l(i),gapv(irect(l,seg1)))
                          gapv(irect(l,seg1))=min(gapmax,gapv(irect(l,seg1)))
                          gapv(irect(l,seg1))=max(gapmin,gapv(irect(l,seg1)))
                          gapv(irect(l,seg1))=max(drad,gapv(irect(l,seg1))+dgapload)
                        end if

                      end do
                    end if

                  end do

                end do

                ! Reset tagnod from 2→1: frontier segment nodes are "visited" not "active"
                tagnod(irect(1:4,seg))=1

              end do ! end loop over current frontier segments

              ! Reset nod2expand for frontier nodes so they CAN be re-used as expansion
              ! sources in the NEXT level (a node may connect to segments at multiple levels)
              ! OPTIMIZATION: This reset is O(frontier_size) per level. Could be avoided
              ! by using a generation counter instead of 0/1 flags.
              do iseg=1,nbseg
                seg  = listseg(iseg)
                jmax = 4
                if((irect(jmax,seg) == 0) .or. irect(3,seg) == irect(4,seg) ) jmax = 3
                do j=1,jmax
                  node_id = irect(j,seg)
                  nod2expand(node_id)=0
                end do
              end do

              ! Swap frontiers: next level becomes current, record for cleanup
              nbseg     = cpt
              if(nbseg ==0)exit
              do j=1,cpt
                listseg(j)    =listsegtmp(j)
                listsegtmp(j) = 0
                listsegtotal(j+cpt_total) = listseg(j)
              end do
              cpt_total = cpt_total + cpt

            end do  ! end BFS while loop
            ! ----------------------------------


            ! ----------------------------------
            ! STEP 4: Filter visited nodes and build the removal list.
            ! After BFS, id_nod(1:cptoper) contains all secondary nodes reached.
            ! Keep only those whose geodesic distance <= threshold.
            ! ----------------------------------
            if (level == 1) then ! BFS didn't expand → no adjacent secondary nodes
              kremnode(i+1) = 0
              local_kremnode(my_seg_number+1,1) = local_kremnode(my_seg_number,1)
              local_kremnode(my_seg_number,2) = i
            else
              ! Reset source node distances (don't count segment's own nodes)
              dist1(irect(1,i)) = ep30
              dist1(irect(2,i)) = ep30
              dist1(irect(3,i)) = ep30
              dist1(irect(4,i)) = ep30

              ! Filter: keep only nodes within removal distance
              cpt1 = 0
              if(igap==0)then
                ! Constant gap: same threshold for all nodes
                do l=1,cptoper
                  if(dist1(id_nod(l)) <= dmax)then
                    cpt1 = cpt1 + 1
                    noddel(cpt1) = id_nod(l)
                  end if
                end do
              else
                ! Variable gap: per-node threshold = sqrt(2) * effective_gap(node)
                do l=1,cptoper
                  if(dist1(id_nod(l)) <= sqrt(two)*gapv(id_nod(l)))then
                    cpt1 = cpt1 + 1
                    noddel(cpt1) = id_nod(l)
                  end if
                end do
              end if

              ! Store result in thread-local buffer
              kremnode(i+1) = cpt1  ! count of removed nodes for segment i
              local_kremnode(my_seg_number+1,1) = local_kremnode(my_seg_number,1) + cpt1
              local_kremnode(my_seg_number,2) = i

              ! Grow thread-local buffer if needed (amortized growth strategy)
              if(cpt1+local_kremnode(my_seg_number+1,1)>local_remnode_size) then
                my_new_size = local_remnode_size + max( local_remnode_size/10, 10*cpt1)
                allocate( tmp_array(my_new_size) )
                tmp_array(1:local_remnode_size) = local_remnode(1:local_remnode_size)
                deallocate( local_remnode )
                call move_alloc( tmp_array,local_remnode )
                local_remnode_size = my_new_size
              end if
              ! Copy removed nodes into thread-local buffer
              do l=1,cpt1
                local_remnode(local_kremnode(my_seg_number,1)+l) = noddel(l)
              end do

              ! CLEANUP: Reset all visited node/segment tags for next iteration.
              ! Only resets nodes/segments that were actually visited (sparse reset).
              ! This avoids an O(numnod) memset per iteration.
              do l=1,cptoper
                dist1(id_nod(l)) = ep30
              end do
              do l=1,cpt_total
                tagnod(irect(1:4, listsegtotal(l))) = 0
                itagseg(listsegtotal(l)) = 0
                listsegtotal(l)          = 0
              end do
              tagnod(irect(1:4,i)) = 0
              itagseg(i)           = 0
            end if
          end do  ! end main loop over segments
!$omp end do

!$omp barrier

          ! STEP 5: Build global CSR index (prefix sum) and copy results to shared output.
          ! Single thread computes prefix sum; then all threads copy their local buffers.
!$omp single
          ! Prefix sum: convert per-segment counts into global offsets
          do i=1,nrtm
            kremnode(i+1) = kremnode(i+1) + kremnode(i)
          end do
          ! Ensure output buffer is large enough (may reallocate)
          my_size = intbuf_tab%s_remnode
          nty = ipari(7)
          if(kremnode(nrtm+1)>my_size) call upgrade_remnode( ipari,kremnode(nrtm+1),intbuf_tab,nty )
!$omp end single

          ! Each thread copies its local results into the shared remnode array.
          ! No conflicts: each thread writes to disjoint portions (determined by kremnode offsets).
          do i=1,my_seg_number
            my_local_address = local_kremnode(i,1)
            my_size = local_kremnode(i+1,1) - local_kremnode(i,1)
            my_seg_id = local_kremnode(i,2)
            my_address = kremnode(my_seg_id)
            do j=1,my_size
              intbuf_tab%remnode(my_address+j) = local_remnode(my_local_address+j)
            end do
          end do


          deallocate(noddel,nod2expand )
          deallocate(listseg,listsegtmp,listsegtotal)
          deallocate( local_remnode )
          deallocate( local_kremnode )
          deallocate( itagseg )
          deallocate( gapv )



          return
        end subroutine get_list_remnode
      end module get_list_remnode_mod
