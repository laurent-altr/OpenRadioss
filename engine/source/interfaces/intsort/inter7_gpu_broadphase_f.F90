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
!||====================================================================
!||    inter7_gpu_broadphase_mod   ../engine/source/interfaces/intsort/inter7_gpu_broadphase_f.F90
!||--- called by ------------------------------------------------------
!||    inter7_collision_detection  ../engine/source/interfaces/intsort/inter7_collision_detection.F90
!||--- uses       -----------------------------------------------------
!||    precision_mod               ../common_source/modules/precision_mod.F90
!||====================================================================
!
! Fortran ISO_C_BINDING wrapper for the CUDA/PhysX broad-phase
! collision detection kernel.
!
! Usage example (replacing INTER7_CANDIDATE_PAIRS with GPU path)
! ==============================================================
!
!   use INTER7_GPU_BROADPHASE_MOD
!
!   ! At simulation start:
!   if (inter7_gpu_init() == 0) then
!     use_gpu = inter7_gpu_available()
!   end if
!
!   ! At every contact sort step (instead of fill_voxel + inter7_candidate_pairs):
!   if (use_gpu) then
!     call inter7_gpu_fill_voxel(nsn, nsnr, nbx, nby, nbz, nrtm, numnod,
!    &    nsv, x, stfn, box_limit_main)
!     call inter7_gpu_candidate_pairs(nsn, nrtm, numnod, nsv, irect, x, stfn, stf,
!    &    xyzm, inter_struct%voxel, inter_struct%next_nod, nsnr, nbx, nby, nbz,
!    &    igap, marge, tzinf, gap, gapmin, gapmax, bgapsmx, drad, dgapload,
!    &    gap_s, gap_m, curv_max, flagremnode, s_kremnod, kremnod, s_remnod,
!    &    remnod, mulnsn, cand_n, cand_e, ii_stok)
!   else
!     call inter7_candidate_pairs(...)   ! CPU fallback
!   end if
!
!   ! At simulation end:
!   call inter7_gpu_destroy()
!
! Design notes on the open questions
! ====================================
!
! Q: Master surface vs slave node – is it possible on GPU?
! ---------------------------------------------------------
! Yes.  The GPU algorithm is structurally asymmetric by construction:
!   - Slave nodes are hashed into voxels (fill_voxel step).
!   - Master segments query voxels (candidate_pairs step).
! Only (slave node, master segment) pairs are ever produced – never
! (slave, slave) or (master, master).  This mirrors the CPU algorithm
! exactly.  If a full PhysX scene is used instead, the same asymmetry
! can be enforced via filter groups:
!   slave  nodes: filterGroup=1, filterMask=2
!   master segs:  filterGroup=2, filterMask=1
! preventing same-type pairs at the broad-phase level.
!
! Q: Quick rejection of kremnode and corner nodes
! -----------------------------------------------
! Three GPU-friendly filters (applied in increasing cost order):
!   (a) Corner self-pairs (M1..M4): 4 integer comparisons in registers.
!       Zero memory traffic; eliminates contact of a node with its own face.
!   (b) KREMNODE forbidden pairs: per-segment sorted list stored in
!       device memory in CSR format.  An in-register binary search over the
!       (typically short, O(10)) list rejects forbidden pairs efficiently.
!       The Fortran arrays KREMNOD(2*nrtm) / REMNOD(s_remnod) are converted
!       to 0-based CSR offsets before passing to the kernel:
!         csr_start(ne) = KREMNOD(2*ne-1)   (0-based offset into REMNOD)
!         csr_end(ne)   = KREMNOD(2*ne)     (exclusive end, 0-based)
!       The REMNOD sub-array for segment ne must be sorted in ascending
!       order for binary search to work.  If not sorted, a linear scan
!       (dev_linear_search) can be substituted at negligible extra cost for
!       short lists.
!   (c) Expanded AABB + plane-distance underestimation: same formulas as
!       the CPU code; applied after (a)+(b) pass.

      MODULE INTER7_GPU_BROADPHASE_MOD
        USE ISO_C_BINDING
        USE PRECISION_MOD, ONLY : WP
        IMPLICIT NONE
        PRIVATE
        PUBLIC :: inter7_gpu_init
        PUBLIC :: inter7_gpu_destroy
        PUBLIC :: inter7_gpu_available
        PUBLIC :: inter7_gpu_fill_voxel
        PUBLIC :: inter7_gpu_candidate_pairs
        PUBLIC :: inter7_gpu_convert_kremnod_csr

        ! ----------------------------------------------------------------
        ! Raw C interfaces (not for direct use; call the wrappers below)
        ! ----------------------------------------------------------------
        INTERFACE
          FUNCTION c_gpu_init() BIND(C, NAME='inter7_gpu_broadphase_init_')
            IMPORT :: C_INT
            INTEGER(C_INT) :: c_gpu_init
          END FUNCTION

          SUBROUTINE c_gpu_destroy() BIND(C, NAME='inter7_gpu_broadphase_destroy_')
          END SUBROUTINE

          FUNCTION c_gpu_available() BIND(C, NAME='inter7_gpu_available_')
            IMPORT :: C_INT
            INTEGER(C_INT) :: c_gpu_available
          END FUNCTION

          SUBROUTINE c_gpu_fill_voxel( &
              nsn, nsnr, nbx, nby, nbz, nrtm, numnod, &
              nsv, x, stfn, box_limit) &
            BIND(C, NAME='inter7_gpu_fill_voxel_')
            IMPORT :: C_INT, C_DOUBLE, C_FLOAT
#ifdef MYREAL8
            IMPORT :: C_DOUBLE
            INTEGER(C_INT),  INTENT(IN) :: nsn, nsnr, nbx, nby, nbz, nrtm, numnod
            INTEGER(C_INT),  INTENT(IN) :: nsv(*)
            REAL(C_DOUBLE),  INTENT(IN) :: x(*), stfn(*), box_limit(*)
#else
            IMPORT :: C_FLOAT
            INTEGER(C_INT),  INTENT(IN) :: nsn, nsnr, nbx, nby, nbz, nrtm, numnod
            INTEGER(C_INT),  INTENT(IN) :: nsv(*)
            REAL(C_FLOAT),   INTENT(IN) :: x(*), stfn(*), box_limit(*)
#endif
          END SUBROUTINE

          SUBROUTINE c_gpu_candidate_pairs( &
              nsn, nrtm, numnod, nsv, irect, x, stfn, stf, xyzm, &
              voxel, next_nod, nsnr, nbx, nby, nbz, &
              igap, marge, tzinf, gap, gapmin, gapmax, bgapsmx, drad, dgapload, &
              gap_s, gap_m, curv_max, &
              flagremnode, s_kremnod, kremnod, s_remnod, remnod, &
              mulnsn, cand_n, cand_e, ii_stok) &
            BIND(C, NAME='inter7_gpu_candidate_pairs_')
            IMPORT :: C_INT
#ifdef MYREAL8
            IMPORT :: C_DOUBLE
            INTEGER(C_INT),  INTENT(IN)    :: nsn, nrtm, numnod, nsnr
            INTEGER(C_INT),  INTENT(IN)    :: nbx, nby, nbz, igap
            INTEGER(C_INT),  INTENT(IN)    :: flagremnode, s_kremnod, s_remnod, mulnsn
            INTEGER(C_INT),  INTENT(IN)    :: nsv(*), irect(*), voxel(*), next_nod(*)
            INTEGER(C_INT),  INTENT(IN)    :: kremnod(*), remnod(*)
            REAL(C_DOUBLE),  INTENT(IN)    :: x(*), stfn(*), stf(*), xyzm(*)
            REAL(C_DOUBLE),  INTENT(IN)    :: marge, tzinf, gap, gapmin, gapmax
            REAL(C_DOUBLE),  INTENT(IN)    :: bgapsmx, drad, dgapload
            REAL(C_DOUBLE),  INTENT(IN)    :: gap_s(*), gap_m(*), curv_max(*)
            INTEGER(C_INT),  INTENT(OUT)   :: cand_n(*), cand_e(*), ii_stok
#else
            IMPORT :: C_FLOAT
            INTEGER(C_INT),  INTENT(IN)    :: nsn, nrtm, numnod, nsnr
            INTEGER(C_INT),  INTENT(IN)    :: nbx, nby, nbz, igap
            INTEGER(C_INT),  INTENT(IN)    :: flagremnode, s_kremnod, s_remnod, mulnsn
            INTEGER(C_INT),  INTENT(IN)    :: nsv(*), irect(*), voxel(*), next_nod(*)
            INTEGER(C_INT),  INTENT(IN)    :: kremnod(*), remnod(*)
            REAL(C_FLOAT),   INTENT(IN)    :: x(*), stfn(*), stf(*), xyzm(*)
            REAL(C_FLOAT),   INTENT(IN)    :: marge, tzinf, gap, gapmin, gapmax
            REAL(C_FLOAT),   INTENT(IN)    :: bgapsmx, drad, dgapload
            REAL(C_FLOAT),   INTENT(IN)    :: gap_s(*), gap_m(*), curv_max(*)
            INTEGER(C_INT),  INTENT(OUT)   :: cand_n(*), cand_e(*), ii_stok
#endif
          END SUBROUTINE
        END INTERFACE

      CONTAINS

! ======================================================================
! inter7_gpu_init
!   Initialise the GPU device and CUDA/PhysX context.
!   Returns 0 on success, non-zero if no CUDA device is available.
! ======================================================================
        INTEGER FUNCTION inter7_gpu_init()
          IMPLICIT NONE
          INTEGER(C_INT) :: ret
          ret = c_gpu_init()
          inter7_gpu_init = INT(ret)
        END FUNCTION inter7_gpu_init

! ======================================================================
! inter7_gpu_destroy
!   Release all GPU resources.  Call at simulation end.
! ======================================================================
        SUBROUTINE inter7_gpu_destroy()
          IMPLICIT NONE
          CALL c_gpu_destroy()
        END SUBROUTINE inter7_gpu_destroy

! ======================================================================
! inter7_gpu_available
!   Returns .TRUE. if the GPU path was successfully initialised.
! ======================================================================
        LOGICAL FUNCTION inter7_gpu_available()
          IMPLICIT NONE
          INTEGER(C_INT) :: ret
          ret = c_gpu_available()
          inter7_gpu_available = (ret /= 0)
        END FUNCTION inter7_gpu_available

! ======================================================================
! inter7_gpu_fill_voxel
!   GPU replacement for FILL_VOXEL_LOCAL.
!   box_limit uses the BMINMA layout (see fill_voxel.F90):
!     box_limit(1:3)  = xmax, ymax, zmax
!     box_limit(4:6)  = xmin, ymin, zmin
!     box_limit(7:9)  = xmaxb, ymaxb, zmaxb  (reduced bbox)
!     box_limit(10:12)= xminb, yminb, zminb
! ======================================================================
        SUBROUTINE inter7_gpu_fill_voxel( &
            nsn, nsnr, nbx, nby, nbz, nrtm, numnod, &
            nsv, x, stfn, box_limit)
          IMPLICIT NONE
          INTEGER,         INTENT(IN) :: nsn, nsnr, nbx, nby, nbz, nrtm, numnod
          INTEGER,         INTENT(IN) :: nsv(nsn)
          REAL(KIND=WP),   INTENT(IN) :: x(3,numnod), stfn(nsn), box_limit(12)
          CALL c_gpu_fill_voxel( &
              nsn, nsnr, nbx, nby, nbz, nrtm, numnod, &
              nsv, x, stfn, box_limit)
        END SUBROUTINE inter7_gpu_fill_voxel

! ======================================================================
! inter7_gpu_candidate_pairs
!   GPU replacement for INTER7_CANDIDATE_PAIRS.
!
!   The voxel and next_nod arrays are passed as host pointers; the GPU
!   kernel uploads them to device memory.  This allows the caller to use
!   either the GPU fill_voxel path or the existing CPU fill_voxel path
!   – both produce equivalent voxel/next_nod arrays.
!
!   The KREMNODE/REMNOD arrays are converted to 0-based CSR offsets
!   before use (see inter7_gpu_convert_kremnod_csr).
!
!   xyzm layout (Fortran, 1-based):
!     xyzm(1..6)  = xmin, ymin, zmin, xmax, ymax, zmax
!     xyzm(7..12) = xminb, yminb, zminb, xmaxb, ymaxb, zmaxb
! ======================================================================
        SUBROUTINE inter7_gpu_candidate_pairs( &
            nsn, nrtm, numnod, nsv, irect, x, stfn, stf, xyzm, &
            voxel, next_nod, nsnr, nbx, nby, nbz, &
            igap, marge, tzinf, gap, gapmin, gapmax, bgapsmx, drad, dgapload, &
            gap_s, gap_m, curv_max, &
            flagremnode, s_kremnod, kremnod, s_remnod, remnod, &
            mulnsn, cand_n, cand_e, ii_stok)
          IMPLICIT NONE
          INTEGER,         INTENT(IN)    :: nsn, nrtm, numnod, nsnr
          INTEGER,         INTENT(IN)    :: nbx, nby, nbz, igap
          INTEGER,         INTENT(IN)    :: flagremnode, s_kremnod, s_remnod, mulnsn
          INTEGER,         INTENT(IN)    :: nsv(nsn)
          INTEGER,         INTENT(IN)    :: irect(4,nrtm)
          INTEGER,         INTENT(IN)    :: voxel(*)
          INTEGER,         INTENT(IN)    :: next_nod(nsn+nsnr)
          INTEGER,         INTENT(IN)    :: kremnod(s_kremnod)
          INTEGER,         INTENT(IN)    :: remnod(s_remnod)
          REAL(KIND=WP),   INTENT(IN)    :: x(3,numnod)
          REAL(KIND=WP),   INTENT(IN)    :: stfn(nsn)
          REAL(KIND=WP),   INTENT(IN)    :: stf(nrtm)
          REAL(KIND=WP),   INTENT(IN)    :: xyzm(12)
          REAL(KIND=WP),   INTENT(IN)    :: marge, tzinf, gap
          REAL(KIND=WP),   INTENT(IN)    :: gapmin, gapmax, bgapsmx, drad, dgapload
          REAL(KIND=WP),   INTENT(IN)    :: gap_s(nsn), gap_m(nrtm), curv_max(nrtm)
          INTEGER,         INTENT(INOUT) :: cand_n(mulnsn), cand_e(mulnsn)
          INTEGER,         INTENT(OUT)   :: ii_stok

          INTEGER, ALLOCATABLE :: kremnod_csr(:)
          INTEGER :: s_krem_csr

          ! Convert KREMNODE to 0-based CSR if kremnode is active
          IF (flagremnode == 2 .AND. s_kremnod > 0) THEN
            s_krem_csr = 2 * nrtm
            ALLOCATE(kremnod_csr(s_krem_csr))
            CALL inter7_gpu_convert_kremnod_csr(nrtm, kremnod, kremnod_csr)
            CALL c_gpu_candidate_pairs( &
                nsn, nrtm, numnod, nsv, irect, x, stfn, stf, xyzm, &
                voxel, next_nod, nsnr, nbx, nby, nbz, &
                igap, marge, tzinf, gap, gapmin, gapmax, bgapsmx, drad, dgapload, &
                gap_s, gap_m, curv_max, &
                flagremnode, s_krem_csr, kremnod_csr, s_remnod, remnod, &
                mulnsn, cand_n, cand_e, ii_stok)
            DEALLOCATE(kremnod_csr)
          ELSE
            ! Dummy kremnode arrays when not used
            CALL c_gpu_candidate_pairs( &
                nsn, nrtm, numnod, nsv, irect, x, stfn, stf, xyzm, &
                voxel, next_nod, nsnr, nbx, nby, nbz, &
                igap, marge, tzinf, gap, gapmin, gapmax, bgapsmx, drad, dgapload, &
                gap_s, gap_m, curv_max, &
                flagremnode, 0, kremnod, 0, remnod, &
                mulnsn, cand_n, cand_e, ii_stok)
          END IF
        END SUBROUTINE inter7_gpu_candidate_pairs

! ======================================================================
! inter7_gpu_convert_kremnod_csr
!
! Convert the Fortran KREMNODE array to 0-based CSR offsets used by
! the GPU kernel.
!
! Fortran KREMNODE layout (1-based, NE from 1 to NRTM):
!   KREMNODE(2*NE-1) = REMNOD start offset (exclusive, 0-based in C)
!   KREMNODE(2*NE)   = REMNOD end offset   (exclusive, 0-based in C)
!
! The GPU kernel expects (0-based C):
!   kremnod_csr[2*ne]   = start index into REMNOD (0-based)
!   kremnod_csr[2*ne+1] = end   index into REMNOD (0-based, exclusive)
!
! Reminder: in Fortran (1-based NE, K from KREMNODE(2*(NE-1)+1)+1 to
! KREMNODE(2*(NE-1)+2)), the forbidden nodes for segment NE are at
! REMNOD(K..L) where K and L are 1-based Fortran indices.
! Conversion: CSR_start = KREMNODE(2*NE-1)  (already 0-based offset)
!             CSR_end   = KREMNODE(2*NE)     (already 0-based offset)
! The REMNOD array is assumed to be sorted per segment.
! ======================================================================
        SUBROUTINE inter7_gpu_convert_kremnod_csr(nrtm, kremnod, kremnod_csr)
          IMPLICIT NONE
          INTEGER, INTENT(IN)  :: nrtm
          INTEGER, INTENT(IN)  :: kremnod(2*nrtm+1)  ! Fortran 1-based layout
          INTEGER, INTENT(OUT) :: kremnod_csr(2*nrtm) ! 0-based CSR output
          INTEGER :: ne
          DO ne = 1, nrtm
            ! KREMNOD(2*(NE-1)+1) = start-1 (Fortran)  → 0-based start
            ! KREMNOD(2*(NE-1)+2) = end      (Fortran)  → 0-based end
            kremnod_csr(2*ne-1) = kremnod(2*(ne-1)+1)
            kremnod_csr(2*ne)   = kremnod(2*(ne-1)+2)
          END DO
        END SUBROUTINE inter7_gpu_convert_kremnod_csr

      END MODULE INTER7_GPU_BROADPHASE_MOD
