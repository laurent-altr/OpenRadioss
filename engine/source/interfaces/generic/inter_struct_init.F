Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    inter_struct_init   ../engine/source/interfaces/generic/inter_struct_init.F
      !||--- called by ------------------------------------------------------
      !||    resol               ../engine/source/engine/resol.F
      !||--- uses       -----------------------------------------------------
      !||    inter_sorting_mod   ../engine/share/modules/inter_sorting_mod.F
      !||    inter_struct_mod    ../engine/share/modules/inter_struct_mod.F
      !||====================================================================
        SUBROUTINE INTER_STRUCT_INIT(INTER_STRUCT,SORT_COMM,INTBUF_TAB, IPARI,NPARI, ISPMD, NODES)
!$COMMENT
!       INTER_STRUCT_INIT description
!       initialization of INTER_STRUCT structure
!       
!       INTER_STRUCT_INIT organization :
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE EXTEND_ARRAY_MOD
        USE UMAP_MOD
        USE NODAL_ARRAYS_MOD
        USE spmd_inter_window_update
        USE INTER_STRUCT_MOD
        USE INTER_SORTING_MOD
        USE INTBUFDEF_MOD
        USE INTER_SHM_MOD
        USE SPMD_MOD
        USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C------------------------o-----------------------
        TYPE(intbuf_struct_), INTENT(inout) :: INTBUF_TAB(NINTER)
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
        INTEGER, INTENT(in) :: NPARI !< number of parameters
        INTEGER, INTENT(inout) :: IPARI(NPARI,NINTER) !< integer parameters
        INTEGER, INTENT(in) :: ISPMD !< current rank
        type(nodal_arrays_), intent(in) :: NODES
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: KK,NIN,I,J,P
        INTEGER :: NSN,NSN_OWNED,NMN, NRTM
        INTEGER :: SENDBUF(NSPMD)
        INTEGER :: KEY,COLOR
        INTEGER :: IREM_SIZE, XREM_SIZE
        INTEGER :: nloc, k, nb_added
        integer(kind=8), dimension(:,:), pointer :: shared_int
        my_real, dimension(:), allocatable :: temp_stif
        integer, dimension(:), allocatable :: temp_itab
        integer, dimension(:), allocatable :: temp_ispmd
        double precision, dimension(:,:), allocatable :: coords
        double precision :: eps 
!   ----------------------------------------
        DO KK=1,NINTER
            INTER_STRUCT(KK)%SIZE_CAND_A = 0   
            INTER_STRUCT(KK)%I_SK_OLD = 0    
            INTER_STRUCT(KK)%INACTI = 0
            INTER_STRUCT(KK)%INACTII = 0
            INTER_STRUCT(KK)%CURV_MAX_MAX = ZERO
            INTER_STRUCT(KK)%NMN_G = 0                
            INTER_STRUCT(KK)%old_rem_size = 0
            NSN = IPARI(5,KK)
C           INTER_STRUCT(KK)%WIN%NSNS(1:NSPMD) = 0
C           INTER_STRUCT(KK)%WIN%NSNS(ISPMD+1) = NSN_OWNED
        ENDDO
        DO KK=1,NINTER
            NSN = IPARI(5,KK)
            NRTM = IPARI(4,KK)
            NMN = IPARI(6,KK) 
            KEY = ISPMD
            COLOR = 0
            
            IF(NSN + NRTM + NMN > 0) COLOR = 1

#ifdef MPI
            CALL SPMD_COMM_SPLIT(COLOR,KEY,INTBUF_TAB(KK)%MPI_COMM,
     .                          INTBUF_TAB(KK)%RANK, INTBUF_TAB(KK)%NSPMD)
#endif
            IF(IPARI(7,KK) .NE. 7) CYCLE
            NSN_OWNED = 0 
!           IF(ALLOCATED(COORDS)) DEALLOCATE(COORDS)
!           ALLOCATE(coords(3,NSN))
            DO i = 1,NSN
                IF(NODES%WEIGHT(intbuf_tab(kk)%NSV(i)) == 1) THEN
                    NSN_OWNED = NSN_OWNED + 1
!                   coords(1:3,NSN_OWNED) = NODES%X(1:3,intbuf_tab(kk)%NSV(i))
                ENDIF
            ENDDO

            IF(NSN+NRTM+NMN > 0) THEN

              eps = 100.0d0 * INTBUF_TAB(KK)%VARIABLES(2)

              inter_struct(KK)%old_rem_size = size(ITAFI(KK)%P)
        !         inter_window_open(inter_win           , n_local, intbuf_tab, rank_world, nspmd, ipari, npari, eps, nodes)
              call inter_window_open(INTER_STRUCT(KK)%WIN, NSN_OWNED, INTBUF_TAB(KK), ispmd,nspmd, ipari(:,KK), npari,eps,nodes)
              call spmd_inter_window_update_intra(INTER_STRUCT(KK)%WIN, nodes, INTBUF_TAB(KK), IPARI(:,KK), NPARI, ISPMD)
              call spmd_inter_window_update_inter(INTER_STRUCT(KK)%WIN)
              write(6,*) "mark boundary interface", KK 
              if(NSN>0) call inter_window_mark_boundary(INTER_STRUCT(KK)%WIN, nodes, nspmd, intbuf_tab(KK)%NSV, NSN, numnod)
              call spmd_inter_window_update_inter(INTER_STRUCT(KK)%WIN)

              
             

!             size_shared_data = (INTER_STRUCT(KK)%WIN%RSIZ + INTER_STRUCT(KK)%WIN%ISIZ)!
!             call c_f_pointer(INTER_STRUCT(KK)%WIN%shared_base, shared_int,[ size_shared_data, INTER_STRUCT(KK)%WIN%nsn_global ] )


!             renumber old candidates: frontier nodes with weight = 0 become remote nodes
!              nb_added = 0
!              do i = 1, INTBUF_TAB(KK)%I_STOK(1)
!                if(INTBUF_TAB(KK)%CAND_N(i) <= NSN) then ! if local
!                  nloc = INTBUF_TAB(KK)%CAND_N(i)
!                  if(NODES%WEIGHT(INTBUF_TAB(KK)%NSV(INTBUF_TAB(KK)%CAND_N(i))) == 0) then !but owned by another proc
!                    key = NODES%ITAB(INTBUF_TAB(KK)%NSV(INTBUF_TAB(KK)%CAND_N(i)))
!                    !                              position in the shared buffer   + NSN 
!                    K =  get_value_umap(inter_struct(KK)%WIN%glob2loc,key,-1)
!                    if(shared_int(INTER_STRUCT(KK)%WIN%itab_offset, k) .NE. key) write(6,*) 'ERROR: key not found in glob2loc',key
!!                   P = shared_int(INTER_STRUCT(KK)%WIN%ispmd_offset, k) +1 
!!                   k = IPARI(24,KK)  
!!                   call extend_array(ITAFI(KK)%P, k, k+1)
!!                   call extend_array(STIFI(KK)%P, k, k+1)
!!                   ITAFI(KK)%P(k+1) = key ! l ordre n est pas bon, il faut le faire en fonction the nsvfi (pack par proc)
!                    intbuf_tab(KK)%cand_n(i) = -K
!!                   STIFI(KK)%P(K+1) = intbuf_tab(kk)%stfns(nloc)
!!                   IPARI(24,KK) = IPARI(24,KK) + 1 ! nsnrold
!                    nb_added = nb_added + 1
!
!                    IF( k < 0) then 
!                       write(6,*) 'ERROR: key not found in glob2loc',key
!                         do j = 1, NSN 
!                            if(NODES%ITAB(intbuf_tab(KK)%NSV(j)) == key) then
!                               write(6,*) 'key found in NSV',j
!                               call flush(6)
!                            endif
!                         enddo
!                         call arret(2)
!                     else
!                      ! write(6,*) 'key found in glob2loc',key,"at position",k
!                     endif
!!                   INTBUF_TAB(KK)%CAND_N(I) = k + NSN
!                  endif
!                endif
!              enddo
!              if(nb_added > 0) then
!                k = IPARI(24,KK)
!                allocate(temp_stif(k))
!                allocate(temp_itab(k))
!                allocate(temp_ispmd(INTBUF_TAB(KK)%I_STOK(1)))
!                temp_stif = STIFI(KK)%P
!                temp_itab = ITAFI(KK)%P
!                allocate(STIFI(KK)%P(k+nb_added))
!                allocate(ITAFI(KK)%P(k+nb_added))
!                IPARI(24,KK) = k + nb_added
!                NSNFI(KK)%P(1:NSPMD) = 0
!                k = 0
!                do i = 1, INTBUF_TAB(KK)%I_STOK(1)
!                  if(INTBUF_TAB(KK)%CAND_N(i) < 0) then
!                    k = -INTBUF_TAB(KK)%CAND_N(i)
!
!                  else
!                    k = INTBUF_TAB(KK)%CAND_N(i)
!
!                  endif 
!                enddo
!                deallocate(temp_stif)
!                deallocate(temp_itab)
!                deallocate(temp_ispmd)
!              endif

            ELSE

              call nullify_inter_win(INTER_STRUCT(KK)%WIN)
            ENDIF

       ENDDO
        DO KK=1,NB_INTER_7_INACTI
            NIN = LIST_INTER_7_INACTI(KK)
            ALLOCATE( INTER_STRUCT(NIN)%NSNFIOLD(NSPMD) )
            INTER_STRUCT(NIN)%NSNFIOLD(1:NSPMD) = 0
        ENDDO
        DO KK=1,NINTER
            SORT_COMM(KK)%PROC_NUMBER = 0
            SORT_COMM(KK)%SIZE_GLOBAL_COARSE_CELL = 0
            ALLOCATE( SORT_COMM(KK)%DATA_PROC(NSPMD) )
            ALLOCATE( SORT_COMM(KK)%REQ_SD2(NSPMD) )
            ALLOCATE( SORT_COMM(KK)%REQ_SD3(NSPMD) )
        ENDDO

        RETURN
        END SUBROUTINE INTER_STRUCT_INIT
