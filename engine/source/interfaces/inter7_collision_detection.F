Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    inter7_collision_detection_mod   ../engine/source/interfaces/inter7_collision_detection.F
      !||--- called by ------------------------------------------------------
      !||    inter_sort_07                    ../engine/source/interfaces/int07/inter_sort_07.F
      !||====================================================================
       MODULE INTER7_COLLISION_DETECTION_MOD
         interface
            subroutine free_c_int(arr_ptr) bind(C, name="free_c_int")
            use iso_c_binding
            type(c_ptr) :: arr_ptr
           end subroutine free_c_int 
        end interface
       CONTAINS
      !||====================================================================
      !||    inter7_collision_detection   ../engine/source/interfaces/inter7_collision_detection.F
      !||--- called by ------------------------------------------------------
      !||    inter_sort_07                ../engine/source/interfaces/int07/inter_sort_07.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                       ../engine/source/output/message/message.F
      !||    arret                        ../engine/source/system/arret.F
      !||    inter7_candidate_pairs       ../engine/source/interfaces/intsort/inter7_candidate_pairs.F
      !||    my_barrier                   ../engine/source/system/machine.F
      !||    spmd_oldnumcd                ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||--- uses       -----------------------------------------------------
      !||    inter7_candidate_pairs_mod   ../engine/source/interfaces/intsort/inter7_candidate_pairs.F
      !||    message_mod                  ../engine/share/message_module/message_mod.F
      !||    tri7box                      ../engine/share/modules/tri7box.F
      !||====================================================================
       SUBROUTINE INTER7_COLLISION_DETECTION(
     1   X        ,IRECT   ,NSV     ,INACTI   ,CAND_P  ,
     2   NMN      ,NRTM    ,NSN     ,CAND_E   ,CAND_N  ,
     3   GAP      ,II_STOK ,NCONTACT ,BMINMA  ,
     4   TZINF    ,CAND_A,CURV_MAX,
     5   ESHIFT  ,IFQ      ,IFPEN   ,
     8   STFN     ,STF     ,IGAP     ,GAP_S   ,
     A   NSNR     ,RENUM   ,NSNROLD  ,GAP_M   ,
     B   GAPMIN   ,GAPMAX  ,NUM_IMP,GAP_S_L ,
     C   GAP_M_L  ,ITASK   ,BGAPSMX  ,I_MEM   ,  
     D   KREMNOD  ,REMNOD  ,FLAGREMNODE, DRAD ,
     E   ITIED    ,CAND_F  ,DGAPLOAD,
     F   TOTAL_NB_NRTM, s_cand_a,
     F   S_KREMNOD, S_REMNOD)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE INTER7_CANDIDATE_PAIRS_MOD
      USE MESSAGE_MOD
      USE iso_c_binding
      USE my_alloc_mod, only: realloc
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN,NSN, INACTI,IFQ,  NSNR, NSNROLD
      INTEGER, INTENT(in) :: NRTM !< number of segments per threads
      INTEGER, INTENT(in) :: TOTAL_NB_NRTM !< total number of segments
      integer, intent(in) :: s_cand_a
      INTEGER IRECT(4,*),NSV(*),CAND_A(s_cand_a), RENUM(*),NUM_IMP, ITASK
      INTEGER, DIMENSION(:),  ALLOCATABLE :: CAND_E,CAND_N,IFPEN
      INTEGER ::  KREMNOD(*),REMNOD(*)
      INTEGER NCONTACT,ESHIFT, IGAP, I_MEM,
     *        II_STOK, FLAGREMNODE, ITIED
      INTEGER, intent(in) :: S_KREMNOD, S_REMNOD

C     REAL
      my_real
     .   GAP,TZINF,
     .   GAPMIN, GAPMAX, BMINMA(12),CURV_MAX(NRTM),BGAPSMX
      my_real , INTENT(IN) :: DRAD,DGAPLOAD
      my_real, dimension(:), allocatable :: CAND_P, CAND_F
      my_real
     .   X(3,*), STFN(*),
     .   STF(*), GAP_S(*), GAP_M(*), 
     .   GAP_S_L(*), GAP_M_L(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,
     .        N, S_PREV_REMOTE_NUMBER,
     .        NSNFIOLD(NSPMD)
C     REAL
      my_real
     .        XYZM(6,2), MARGE, AAA
      INTEGER NBX,NBY,NBZ ! number of cells in each direction
      INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8
      INTEGER :: SIREM, SXREM
      INTEGER(C_INT), POINTER :: C_CAND_E(:), C_CAND_N(:)
      TYPE(C_PTR) :: PTR_CAND_E, PTR_CAND_N
      INTEGER :: NCONTACT_SAVE
      INTEGER :: II_STOK_SAVE
      my_real :: dx, dy, dz
      my_real :: bgap 

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C-----INITIALISATION 
C----- Get bounds of the domains
C
      XYZM(1,1) = BMINMA(4)
      XYZM(2,1) = BMINMA(5)
      XYZM(3,1) = BMINMA(6)
      XYZM(4,1) = BMINMA(1)
      XYZM(5,1) = BMINMA(2)
      XYZM(6,1) = BMINMA(3)
      XYZM(1,2) = BMINMA(10)
      XYZM(2,2) = BMINMA(11)
      XYZM(3,2) = BMINMA(12)
      XYZM(4,2) = BMINMA(7)
      XYZM(5,2) = BMINMA(8)
      XYZM(6,2) = BMINMA(9)
      I_MEM = 0
C
      IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.
     .   IFQ>0.OR.NUM_IMP>0.OR.ITIED/=0) THEN
        S_PREV_REMOTE_NUMBER = NSNR
      ELSE
        S_PREV_REMOTE_NUMBER = 0
      END IF
      MARGE = TZINF-MAX(GAP+DGAPLOAD,DRAD) ! margin 
C Work on the reduced box
      IF( NMN /= 0 ) THEN
        AAA = SQRT(NMN /
     .           ((BMINMA(7)-BMINMA(10))*(BMINMA(8)-BMINMA(11))
     .           +(BMINMA(8)-BMINMA(11))*(BMINMA(9)-BMINMA(12))
     .           +(BMINMA(9)-BMINMA(12))*(BMINMA(7)-BMINMA(10))))
      ELSE
        AAA = 0
      ENDIF

      AAA = 0.75*AAA

      NBX = NINT(AAA*(BMINMA(7)-BMINMA(10)))
      NBY = NINT(AAA*(BMINMA(8)-BMINMA(11)))
      NBZ = NINT(AAA*(BMINMA(9)-BMINMA(12)))
C
      NBX = MAX(NBX,1)
      NBY = MAX(NBY,1)
      NBZ = MAX(NBZ,1)
     
      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      LVOXEL8 = LVOXEL      

      IF(RES8 > LVOXEL8) THEN
        AAA = LVOXEL
        AAA = AAA/((NBX8+2)*(NBY8+2)*(NBZ8+2))
        AAA = AAA**(THIRD)
        NBX = INT((NBX+2)*AAA)-2
        NBY = INT((NBY+2)*AAA)-2
        NBZ = INT((NBZ+2)*AAA)-2
        NBX = MAX(NBX,1)
        NBY = MAX(NBY,1)
        NBZ = MAX(NBZ,1)
      ENDIF
      
      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      
C!$OMP SINGLE
C      IF(IGAP > 0) THEN 
C! Make sure that the cell size is ~ bgapmax
C            dx = (BMINMA(7)-BMINMA(10))/REAL(NBX)
C            dy = (BMINMA(8)-BMINMA(11))/REAL(NBY)
C            dz = (BMINMA(9)-BMINMA(12))/REAL(NBZ)
C            bgap = bgapsmx * 1.0D0
C            if(dx > bgap) then
C              NBX8 = IDNINT((BMINMA(7)-BMINMA(10))/bgap)
C              NBX8 = MAX(1,MIN(150,MAX(NBX8,1)))
C            endif 
C            if(dy > bgap) then
C              NBY8 = IDNINT((BMINMA(8)-BMINMA(11))/bgap)
C              NBY8 = MAX(1,MIN(150,MAX(NBY8,1)))
C            endif
C            if(dz > bgap) then
C              NBZ8 = IDNINT((BMINMA(9)-BMINMA(12))/bgap)
C              NBZ8 = MAX(1,MIN(150,MAX(NBZ8,1)))
C            endif
C            NBX = NBX8
C            NBY = NBY8
C            NBZ = NBZ8
C      ENDIF 
C!$OMP END SINGLE
C
      IF(RES8 > LVOXEL8) THEN
        NBX = MIN(100,MAX(NBX8,1))
        NBY = MIN(100,MAX(NBY8,1))
        NBZ = MIN(100,MAX(NBZ8,1))
      ENDIF


!$OMP SINGLE
      DO I=1,(NBX+2)*(NBY+2)*(NBZ+2)
        VOXEL1(I)=0
      ENDDO
!$OMP END SINGLE NOWAIT
      IF(ITASK == 0) THEN 
         ALLOCATE(NEXT_NOD(NSN+NSNR))
         ALLOCATE(PREV_REMOTE_NUMBER(S_PREV_REMOTE_NUMBER))
         ! find the old id of remote candidate nodes (inactive, ifq, itied)
       if(nspmd>1.and.(inacti==5.or.inacti==6.or.inacti==7.or.ifq>0.or.itied/=0)) then
         CALL SPMD_OLDNUMCD(RENUM,PREV_REMOTE_NUMBER,S_PREV_REMOTE_NUMBER,NSNROLD)
       end if
      endif
      call MY_BARRIER

C       CALL INTER7_CANDIDATE_PAIRS(
C    1   NSN     ,PREV_REMOTE_NUMBER ,NSNR     ,S_PREV_REMOTE_NUMBER  ,I_MEM   ,
C    2   IRECT   ,X        ,STF      ,STFN     ,XYZM    ,
C    3   NSV     ,II_STOK  ,CAND_N   ,ESHIFT   ,CAND_E  ,
C    4   NCONTACT,TZINF    ,GAP_S_L  ,GAP_M_L ,
C    5   VOXEL1  ,NBX      ,NBY      ,NBZ      ,
C    6   INACTI  ,IFQ      ,CAND_A,CAND_P   ,IFPEN   ,
C    7   NRTM    ,NSNROLD  ,IGAP     ,GAP      ,GAP_S   ,
C    8   GAP_M   ,GAPMIN   ,GAPMAX   ,MARGE    ,CURV_MAX,
C    9   ITASK    ,BGAPSMX  ,S_KREMNOD, KREMNOD  ,S_REMNOD, REMNOD  ,
C    A   FLAGREMNODE,DRAD   ,ITIED    ,CAND_F  ,
C    B   DGAPLOAD, s_cand_a, 
C    C   TOTAL_NB_NRTM,  NUMNOD, XREM, SIZE(XREM,1), 
C    D   IREM, size(irem,1), NEXT_NOD)

        SXREM = SIZE(XREM,1)
        SIREM = SIZE(IREM,1)
c        print_address(oldnum, std::to_string(__LINE__)+" oldnum");
c       print_address(nsv, std::to_string(__LINE__)+" nsv");
c       print_address(irect, std::to_string(__LINE__)+" irect");
c       print_address(x, std::to_string(__LINE__)+" x");
c       print_address(stf, std::to_string(__LINE__)+" stf");
c       print_address(stfn, std::to_string(__LINE__)+" stfn");
       !write(6,*) "LOC(nsn)=" , LOC(nsn)
       !write(6,*) "LOC(PREV_REMOTE_NUMBER)=" , LOC(PREV_REMOTE_NUMBER)
       !write(6,*) "LOC(NSNR)=" , LOC(NSNR)
       !write(6,*) "LOC(S_PREV_REMOTE_NUMBER)=" , LOC(S_PREV_REMOTE_NUMBER)
       !write(6,*) "LOC(I_MEM)=" , LOC(I_MEM)
       !write(6,*) "LOC(IRECT)=" , LOC(IRECT)
       !write(6,*) "LOC(X)=" , LOC(X)
       !write(6,*) "LOC(STF)=" , LOC(STF)
       !write(6,*) "LOC(STFN)=" , LOC(STFN)
       !write(6,*) "LOC(XYZM)=" , LOC(XYZM)
       !write(6,*) "LOC(NSV)=" , LOC(NSV)
       !write(6,*) "LOC(II_STOK)=" , LOC(II_STOK)     
       !write(6,*) "LOC(CAND_N)=" , LOC(PTR_CAND_N)
       !write(6,*) "LOC(ESHIFT)=" , LOC(ESHIFT)
       !write(6,*) "LOC(CAND_E)=" , LOC(PTR_ CAND_E)
       !write(6,*) "LOC(NCONTACT)=" , LOC(NCONTACT)
       !write(6,*) "LOC(TZINF)=" , LOC(TZINF)
       !write(6,*) "LOC(GAP_S_L)=" , LOC(GAP_S_L)
       !write(6,*) "LOC(GAP_M_L)=" , LOC(GAP_M_L)
       !write(6,*) "LOC(VOXEL1)=" , LOC(VOXEL1)
       !write(6,*) "LOC(NBX)=" , LOC(NBX)
       !write(6,*) "LOC(NBY)=" , LOC(NBY)
       !write(6,*) "LOC(NBZ)=" , LOC(NBZ)
       !write(6,*) "LOC(INACTI)=" , LOC(INACTI)
       !write(6,*) "LOC(IFQ)=" , LOC(IFQ)
       !write(6,*) "LOC(CAND_A)=" , LOC(CAND_A)
       !write(6,*) "LOC(CAND_P)=" , LOC(CAND_P)
       !write(6,*) "LOC(IFPEN)=" , LOC(IFPEN)
       !write(6,*) "LOC(NRTM)=" , LOC(NRTM)
       !write(6,*) "LOC(NSNROLD)=" , LOC(NSNROLD)
       !write(6,*) "LOC(IGAP)=" , LOC(IGAP)
       !write(6,*) "LOC(GAP)=" , LOC(GAP)
       !write(6,*) "LOC(GAP_S)=" , LOC(GAP_S)
       !write(6,*) "LOC(GAP_M)=" , LOC(GAP_M)
       !write(6,*) "LOC(GAPMIN)=" , LOC(GAPMIN)
       !write(6,*) "LOC(GAPMAX)=" , LOC(GAPMAX)
       !write(6,*) "LOC(MARGE)=" , LOC(MARGE)
       !write(6,*) "LOC(CURV_MAX)=" , LOC(CURV_MAX)
       !write(6,*) "LOC(ITASK)=" , LOC(ITASK)
       !write(6,*) "LOC(BGAPSMX)=" , LOC(BGAPSMX)
       !write(6,*) "LOC(S_KREMNOD)=" , LOC(S_KREMNOD)
       !write(6,*) "LOC(KREMNOD)=" , LOC(KREMNOD)
       !write(6,*) "LOC(S_REMNOD)=" , LOC(S_REMNOD)
       !write(6,*) "LOC(REMNOD)=" , LOC(REMNOD)
       !write(6,*) "LOC(FLAGREMNODE)=" , LOC(FLAGREMNODE)
       !write(6,*) "LOC(DRAD)=" , LOC(DRAD)
       !write(6,*) "LOC(ITIED)=" , LOC(ITIED)
       !write(6,*) "LOC(CAND_F)=" , LOC(CAND_F)
       !write(6,*) "LOC(DGAPLOAD)=" , LOC(DGAPLOAD)
       !write(6,*) "LOC(s_cand_a)=" , LOC(s_cand_a)
       !write(6,*) "LOC(TOTAL_NB_NRTM)=" , LOC(TOTAL_NB_NRTM)
       !write(6,*) "LOC(NUMNOD)=" , LOC(NUMNOD)
       !write(6,*) "LOC(XREM)=" , LOC(XREM)
       !write(6,*) "LOC(SXREM)=" , LOC(SXREM)
       !write(6,*) "LOC(IREM)=" , LOC(IREM)
       !write(6,*) "LOC(SIREM)=" , LOC(SIREM)
       !write(6,*) "LOC(NEXT_NOD)=" , LOC(NEXT_NOD)

        !write(6,*) "II_STOK BEFORE=", II_STOK
        NCONTACT_SAVE = NCONTACT
        II_STOK_SAVE = II_STOK
        CALL CPP_INTER7_CANDIDATE_PAIRS(
     1   NSN     ,PREV_REMOTE_NUMBER ,NSNR     ,S_PREV_REMOTE_NUMBER  ,I_MEM   ,
     2   IRECT   ,X        ,STF      ,STFN     ,XYZM    ,
     3   NSV     ,II_STOK  ,PTR_CAND_N   ,ESHIFT,  PTR_CAND_E  ,
     4   NCONTACT,TZINF    ,GAP_S_L  ,GAP_M_L ,
     5   VOXEL1  ,NBX      ,NBY      ,NBZ      ,
     6   INACTI  ,IFQ      ,CAND_A,
     7   NRTM    ,NSNROLD  ,IGAP     ,GAP      ,GAP_S   ,
     8   GAP_M   ,GAPMIN   ,GAPMAX   ,MARGE    ,CURV_MAX,
     9   ITASK    ,BGAPSMX  ,S_KREMNOD, KREMNOD  ,S_REMNOD, REMNOD  ,
     A   FLAGREMNODE,DRAD   ,ITIED    ,
     B   DGAPLOAD, s_cand_a, 
     C   TOTAL_NB_NRTM,  NUMNOD, XREM, SXREM, 
     D   IREM, SIREM, NEXT_NOD, CAND_N, CAND_E)
      !write(6,*) "II_STOK AFTER=", II_STOK


       !write(6,*) "C_ASSOCIATED(PTR_CAND_N)", C_ASSOCIATED(PTR_CAND_N)

      
         ! check the address of the arrays C_CAND_N and C_CAND_E
         ! write the pointers to the arrays C_CAND_N and C_CAND_E
       ! write(6,*) "ASSOCIATED(C_CAND_N)", ASSOCIATED(C_CAND_N)

         IF(ITASK==0)  THEN
        if(ii_stok > 0) then
         CALL c_f_pointer(PTR_CAND_N, C_CAND_N,[ii_stok])
         call c_f_pointer(PTR_CAND_E, C_CAND_E,[ii_stok])
          !write(6,*) "C_CAND_E(1)=" , C_CAND_E(1), "C_CAND_N(1)=" , C_CAND_N(1)


       !    write(6,*) "Size of cand_n", size(cand_n),"II_STOK"       ,II_STOK,"size(cande)",size(cand_e) 
              IF(II_STOK > NCONTACT_SAVE) THEN
                NCONTACT = II_STOK
                call realloc(cand_n, ncontact_save, ncontact)
                call realloc(cand_e, ncontact_save, ncontact)
                if(ifq .ne.0)  call realloc(ifpen, ncontact_save, ncontact)
                if(inacti == 5 .or. inacti == 6 .or. inacti == 7)  call  realloc(cand_p, ncontact_save, ncontact)
                if(itied .ne. 0) call realloc(cand_f, 8*ncontact_save, 8*ncontact)
              ENDIF

          do i=1,II_STOK
       !      CALL flush(6)
              CAND_N(i) = C_CAND_N(i)
              CAND_E(i) = C_CAND_E(i)
C             if(ifq > 0)ifpen(i)  = 0
C             if(inacti==5 .or. inacti==6 .or. inacti==7) cand_p(i) = zero
C             if(itied /= 0) cand_f(8*(i-1):8*i)=zero
          enddo
          do i=II_STOK_SAVE+1,II_STOK
       !      CALL flush(6)
              if(ifq > 0)ifpen(i)  = 0
              if(inacti==5 .or. inacti==6 .or. inacti==7) cand_p(i) = zero
              if(itied /= 0) cand_f(8*(i-1):8*i)=zero
          enddo

      ENDIF

          call free_c_int(PTR_CAND_N)
          call free_c_int(PTR_CAND_E)

c           DO I = 1, II_STOK
c                 write(*,*) 'CAND_N', CAND_N(I), 'CAND_E', CAND_E(I)
c           ENDDO
            IF(ALLOCATED(NEXT_NOD)) DEALLOCATE(NEXT_NOD)
            IF(ALLOCATED(PREV_REMOTE_NUMBER)) DEALLOCATE(PREV_REMOTE_NUMBER)
      ENDIF
C
      RETURN
      END
      END MODULE
