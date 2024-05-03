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
      !||    intbuf_fric_mod                 ../starter/share/modules1/intbuf_fric_mod.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit                         ../starter/source/restart/ddsplit/ddsplit.F
      !||    hm_read_friction_models         ../starter/source/interfaces/friction/reader/hm_read_friction_models.F
      !||    hm_read_friction_orientations   ../starter/source/interfaces/friction/reader/hm_read_friction_orientations.F
      !||    hm_read_inter_struct            ../starter/source/interfaces/reader/hm_read_inter_struct.F
      !||    hm_read_inter_type07            ../starter/source/interfaces/int07/hm_read_inter_type07.F
      !||    hm_read_inter_type11            ../starter/source/interfaces/int11/hm_read_inter_type11.F
      !||    hm_read_inter_type24            ../starter/source/interfaces/int24/hm_read_inter_type24.F
      !||    hm_read_inter_type25            ../starter/source/interfaces/int25/hm_read_inter_type25.F
      !||    hm_read_interfaces              ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||    i11sti3                         ../starter/source/interfaces/inter3d1/i11sti3.F
      !||    i24gapm                         ../starter/source/interfaces/inter3d1/i24sti3.F
      !||    i24sti3                         ../starter/source/interfaces/inter3d1/i24sti3.F
      !||    i25gapm                         ../starter/source/interfaces/inter3d1/i25sti3.F
      !||    i25sti3                         ../starter/source/interfaces/inter3d1/i25sti3.F
      !||    i25sti_edg                      ../starter/source/interfaces/inter3d1/i25sti_edg.F
      !||    i7sti3                          ../starter/source/interfaces/inter3d1/i7sti3.F
      !||    inint3                          ../starter/source/interfaces/inter3d1/inint3.F
      !||    inintr                          ../starter/source/interfaces/interf1/inintr.F
      !||    inintr_orthdirfric              ../starter/source/interfaces/interf1/inintr_orthdirfric.F
      !||    intbuf_fric_copy                ../starter/source/interfaces/interf1/intbuf_fric_copy.F
      !||    intbuf_fric_ini_starter         ../starter/source/interfaces/intbuf/intbufFric_ini_starter.F
      !||    intbuf_ini_starter              ../starter/source/interfaces/intbuf/intbuf_ini_starter.F
      !||    inter_dcod_friction             ../starter/source/interfaces/reader/inter_dcod_friction.F
      !||    intfric_wresti                  ../starter/source/interfaces/intbuf/intbufFric_ini_starter.F
      !||    intfric_wrestr                  ../starter/source/interfaces/intbuf/intbufFric_ini_starter.F
      !||    lecint                          ../starter/source/interfaces/interf1/lecint.F
      !||    lectur                          ../starter/source/starter/lectur.F
      !||    split_interfaces                ../starter/source/restart/ddsplit/split_interfaces.F
      !||    st_qaprint_driver               ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_friction             ../starter/source/output/qaprint/st_qaprint_friction.F
      !||    triintfric                      ../starter/source/interfaces/interf1/trintfric.F
      !||====================================================================
      MODULE INTBUF_FRIC_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      TYPE INTBUF_FRIC_STRUCT_  

      INTEGER ::    NSETPRTS       ! number of couple of parts 
      INTEGER ::    FRICMOD        ! the friction model
      INTEGER ::    FRICFORM       ! the friction formulation ( viscous or incremental)
      INTEGER ::    IFFILTER       !Friction filtering flag 
      INTEGER ::    S_TABPARTS_FRIC  ! number of parts  
      INTEGER ::    IORTHFRIC          ! Flag for orthotropic friction
       my_real :: XFILTR_FRIC        !Filtering coefficient

      INTEGER, DIMENSION(:), ALLOCATABLE :: TABCOUPLEPARTS_FRIC    ! Table of couple of parts
      INTEGER, DIMENSION(:), ALLOCATABLE :: TABPARTS_FRIC          ! Table of parts
      INTEGER, DIMENSION(:), ALLOCATABLE :: ADPARTS_FRIC           ! Table of adress of couple of parts
      INTEGER, DIMENSION(:), ALLOCATABLE :: IFRICORTH              ! Table of Orthotropic type of couple of parts 
      my_real,DIMENSION(:), ALLOCATABLE :: TABCOEF_FRIC            ! Table of friction coefficients

      END TYPE INTBUF_FRIC_STRUCT_

      END MODULE INTBUF_FRIC_MOD



      !||====================================================================
      !||    intfric_wresti    ../starter/source/interfaces/intbuf/intbufFric_ini_starter.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit           ../starter/source/restart/ddsplit/ddsplit.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    intbuf_fric_mod   ../starter/share/modules1/intbuf_fric_mod.F
      !||====================================================================
      SUBROUTINE INTFRIC_WRESTI(INTBUF_FRIC_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUF_FRIC_MOD                     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, J, NSET
C--------------------------------------
C     ECRITURE DES DIMENSIONS
C--------------------------------------
      DO N=1,NINTERFRIC
        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%NSETPRTS,LEN)

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%FRICMOD,LEN)

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%FRICFORM,LEN)

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%IFFILTER,LEN)

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%IORTHFRIC,LEN)

        LEN    =1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%S_TABPARTS_FRIC ,LEN)

        NSET = INTBUF_FRIC_TAB(N)%NSETPRTS
        LEN    = NSET
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%TABCOUPLEPARTS_FRIC,LEN)

        LEN    = INTBUF_FRIC_TAB(N)%S_TABPARTS_FRIC 
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%TABPARTS_FRIC,LEN)

        LEN    = INTBUF_FRIC_TAB(N)%S_TABPARTS_FRIC +1
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%ADPARTS_FRIC,LEN)

        LEN  = INTBUF_FRIC_TAB(N)%NSETPRTS
        CALL WRITE_I_C(INTBUF_FRIC_TAB(N)%IFRICORTH,LEN)
      END DO
      RETURN
      END SUBROUTINE INTFRIC_WRESTI
      !||====================================================================
      !||    intfric_wrestr    ../starter/source/interfaces/intbuf/intbufFric_ini_starter.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit           ../starter/source/restart/ddsplit/ddsplit.F
      !||--- calls      -----------------------------------------------------
      !||    write_db          ../starter/source/restart/ddsplit/wrrest.F
      !||--- uses       -----------------------------------------------------
      !||    intbuf_fric_mod   ../starter/share/modules1/intbuf_fric_mod.F
      !||====================================================================
      SUBROUTINE INTFRIC_WRESTR(INTBUF_FRIC_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUF_FRIC_MOD                     
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSET 
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, J, IORTH ,MFROT ,LENC
C--------------------------------------
      DO N=1,NINTERFRIC
        LEN    =1
        CALL WRITE_DB(INTBUF_FRIC_TAB(N)%XFILTR_FRIC,LEN)

        NSET = INTBUF_FRIC_TAB(N)%NSETPRTS
        IORTH = INTBUF_FRIC_TAB(N)%IORTHFRIC
        MFROT = INTBUF_FRIC_TAB(N)%FRICMOD
        IF(MFROT ==0 ) THEN   
           LENC =2  
        ELSE
           LENC = 8
        ENDIF 
        IF(IORTH == 0) THEN
           LEN    =LENC*(NSET+1)
           CALL WRITE_DB(INTBUF_FRIC_TAB(N)%TABCOEF_FRIC,LEN)
        ELSE
           LEN    =LENC+2*LENC*NSET
           CALL WRITE_DB(INTBUF_FRIC_TAB(N)%TABCOEF_FRIC,LEN)
        ENDIF
      END DO
      RETURN
      END SUBROUTINE INTFRIC_WRESTR

      !||====================================================================
      !||    intfric_rresti    ../engine/share/modules/intbuf_fric_mod.F
      !||--- called by ------------------------------------------------------
      !||    rdresb            ../engine/source/output/restart/rdresb.F
      !||--- calls      -----------------------------------------------------
      !||    read_i_c          ../common_source/tools/input_output/write_routtines.c
      !||--- uses       -----------------------------------------------------
      !||    intbuf_fric_mod   ../engine/share/modules/intbuf_fric_mod.F
      !||====================================================================
      SUBROUTINE INTFRIC_RRESTI(INTBUF_FRIC_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUF_FRIC_MOD                     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, J, NSET,LENI
C--------------------------------------
C     ECRITURE DES DIMENSIONS
C--------------------------------------
      LENI=0
      DO N=1,NINTERFRIC
        LEN    =1
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%NSETPRTS,LEN)
        LENI = LENI + LEN

        LEN    =1
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%FRICMOD,LEN)
        LENI = LENI + LEN

        LEN    =1
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%FRICFORM,LEN)
        LENI = LENI + LEN

        LEN    =1
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%IFFILTER,LEN)
        LENI = LENI + LEN

        LEN    =1
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%IORTHFRIC,LEN)
        LENI = LENI + LEN

        LEN    =1
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%S_TABPARTS_FRIC,LEN)
        LENI = LENI + LEN

        LEN = INTBUF_FRIC_TAB(N)%NSETPRTS
        ALLOCATE(INTBUF_FRIC_TAB(N)%TABCOUPLEPARTS_FRIC(LEN))
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%TABCOUPLEPARTS_FRIC,LEN)
        LENI = LENI + LEN

        LEN    = INTBUF_FRIC_TAB(N)%S_TABPARTS_FRIC
        ALLOCATE(INTBUF_FRIC_TAB(N)%TABPARTS_FRIC(LEN))
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%TABPARTS_FRIC,LEN)
        LENI = LENI + LEN

        LEN = INTBUF_FRIC_TAB(N)%S_TABPARTS_FRIC +1
        ALLOCATE(INTBUF_FRIC_TAB(N)%ADPARTS_FRIC(LEN))
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%ADPARTS_FRIC,LEN)
        LENI = LENI + LEN

        LEN = INTBUF_FRIC_TAB(N)%NSETPRTS
        ALLOCATE(INTBUF_FRIC_TAB(N)%IFRICORTH(LEN))
        CALL READ_I_C(INTBUF_FRIC_TAB(N)%IFRICORTH,LEN)
        LENI = LENI + LEN

      END DO
      RETURN
      END SUBROUTINE INTFRIC_RRESTI

      !||====================================================================
      !||    intfric_rrestr    ../engine/share/modules/intbuf_fric_mod.F
      !||--- called by ------------------------------------------------------
      !||    rdresb            ../engine/source/output/restart/rdresb.F
      !||--- calls      -----------------------------------------------------
      !||    read_db           ../engine/source/output/tools/read_db.F
      !||--- uses       -----------------------------------------------------
      !||    intbuf_fric_mod   ../engine/share/modules/intbuf_fric_mod.F
      !||====================================================================
      SUBROUTINE INTFRIC_RRESTR(INTBUF_FRIC_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUF_FRIC_MOD                     
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, J ,NSET ,IORTH ,MFROT ,LENC
C--------------------------------------
      DO N=1,NINTERFRIC
        LEN    =1
        CALL READ_DB(INTBUF_FRIC_TAB(N)%XFILTR_FRIC,LEN)
        NSET = INTBUF_FRIC_TAB(N)%NSETPRTS
        IORTH = INTBUF_FRIC_TAB(N)%IORTHFRIC
        MFROT = INTBUF_FRIC_TAB(N)%FRICMOD
        IF(MFROT ==0 ) THEN   
           LENC =2  
        ELSE
           LENC = 8
        ENDIF

        IF(IORTH == 0) THEN
           LEN    =LENC*(NSET+1)
        ELSE
           LEN    =LENC+2*LENC*NSET
        ENDIF

        IF(LEN>0)THEN
           ALLOCATE(INTBUF_FRIC_TAB(N)%TABCOEF_FRIC(LEN))
           CALL READ_DB(INTBUF_FRIC_TAB(N)%TABCOEF_FRIC,LEN)
        ENDIF

      END DO
      RETURN
      END SUBROUTINE INTFRIC_RRESTR
