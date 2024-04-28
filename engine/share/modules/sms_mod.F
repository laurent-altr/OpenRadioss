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
Chd|====================================================================
Chd|  SMS_MOD                       share/modules/sms_mod.F       
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE SMS_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      my_real,
     .         DIMENSION(:), ALLOCATABLE :: LT_SMS, LTK_SMS,
     .                                      MSKYI_SMS, LTI_SMS,
     .                                      PREC_SMS
      INTEGER, DIMENSION(:), ALLOCATABLE :: 
     .                 INDX1_SMS, INDX2_SMS
      INTEGER, TARGET, DIMENSION(:), ALLOCATABLE :: 
     .                 NATIV_SMS, TAGNOD_SMS, NODXI_SMS, NODII_SMS
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGPRT_SMS, TAGREL_SMS,
     .                      TAGSLV_RBY_SMS, TAGMSR_RBY_SMS,
     .                      TAG_LNK_SMS, NRWL_SMS,
     .                      TAGMSR_I21_SMS,TAGSLV_I21_SMS,JADI21_SMS
      INTEGER, DIMENSION(:),ALLOCATABLE :: JAD_SMS, JDI_SMS, JSM_SMS,
     .                      IAD_SMS, IDI_SMS,
     .                      JADI_SMS, JDII_SMS,
     .                      LAD_SMS, KAD_SMS, KDI_SMS, PK_SMS,
     .                      JADC_SMS, JADS_SMS, JADS10_SMS, JADT_SMS,
     .                      JADP_SMS, JADR_SMS, JADTG_SMS
      my_real,
     .        DIMENSION(:,:), ALLOCATABLE :: X_SMS ,P_SMS ,Z_SMS ,Y_SMS,
     .                                       XMOM_SMS
      my_real,
     .        DIMENSION(:,:), ALLOCATABLE :: DIAG_SMS3, PREC_SMS3
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: ISKYI_SMS
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: T2MAIN_SMS2
      my_real, DIMENSION(:),ALLOCATABLE :: T2FAC_SMS2

C-----------------------------------------------
        CONTAINS
        SUBROUTINE SMS_ALLOCATE()
            IMPLICIT NONE
               ALLOCATE(TAGNOD_SMS(0))
               ALLOCATE(NATIV_SMS(0))
               ALLOCATE(TAGPRT_SMS(0))
               ALLOCATE(TAGREL_SMS(0))
               ALLOCATE(INDX1_SMS(0))
               ALLOCATE(INDX2_SMS(0))
               ALLOCATE(TAGSLV_RBY_SMS(0))
               ALLOCATE(TAGMSR_RBY_SMS(0))
               ALLOCATE(KAD_SMS(0))
               ALLOCATE(JAD_SMS(0))
               ALLOCATE(LAD_SMS(0))
               ALLOCATE(JADC_SMS(0))
               ALLOCATE(JADS_SMS(0))
               ALLOCATE(JADS10_SMS(0))
               ALLOCATE(JADT_SMS(0))
               ALLOCATE(JADP_SMS(0))
               ALLOCATE(JADR_SMS(0))
               ALLOCATE(JADTG_SMS(0))
               ALLOCATE(X_SMS(0,0))
               ALLOCATE(P_SMS(0,0))
               ALLOCATE(Y_SMS(0,0))
               ALLOCATE(Z_SMS(0,0))
               ALLOCATE(PREC_SMS(0))
               ALLOCATE(XMOM_SMS(0,0))
               ALLOCATE(PREC_SMS3(0,0))
               ALLOCATE(DIAG_SMS3(0,0))
               ALLOCATE(T2MAIN_SMS2(0,0))
               ALLOCATE(T2FAC_SMS2(0))
               ALLOCATE(JADI21_SMS(0))
               ALLOCATE(TAGSLV_I21_SMS(0))
               ALLOCATE(TAGMSR_I21_SMS(0))
               ALLOCATE(NODXI_SMS(0))
               ALLOCATE(NODII_SMS(0))
               ALLOCATE(MSKYI_SMS(0))
               ALLOCATE(ISKYI_SMS(0,0))
               ALLOCATE(JADI_SMS(0))
               ALLOCATE(JDII_SMS(0))
               ALLOCATE(LTI_SMS(0))
        RETURN
      END SUBROUTINE
        SUBROUTINE SMS_DEALLOCATE()
            IMPLICIT NONE
              DEALLOCATE(TAGNOD_SMS)
              DEALLOCATE(NATIV_SMS)
              DEALLOCATE(TAGPRT_SMS)
              DEALLOCATE(TAGREL_SMS)
              DEALLOCATE(INDX1_SMS)
              DEALLOCATE(INDX2_SMS)
              DEALLOCATE(TAGSLV_RBY_SMS)
              DEALLOCATE(TAGMSR_RBY_SMS)
              DEALLOCATE(KAD_SMS)
              DEALLOCATE(JAD_SMS)
              DEALLOCATE(LAD_SMS)
              DEALLOCATE(JADC_SMS)
              DEALLOCATE(JADS_SMS)
              DEALLOCATE(JADS10_SMS)
              DEALLOCATE(JADT_SMS)
              DEALLOCATE(JADP_SMS)
              DEALLOCATE(JADR_SMS)
              DEALLOCATE(JADTG_SMS)
              DEALLOCATE(X_SMS)
              DEALLOCATE(P_SMS)
              DEALLOCATE(Y_SMS)
              DEALLOCATE(Z_SMS)
              DEALLOCATE(PREC_SMS)
              DEALLOCATE(XMOM_SMS)
              DEALLOCATE(PREC_SMS3)
              DEALLOCATE(DIAG_SMS3)
              DEALLOCATE(T2MAIN_SMS2)
              DEALLOCATE(T2FAC_SMS2)
              DEALLOCATE(JADI21_SMS)
              DEALLOCATE(TAGSLV_I21_SMS)
              DEALLOCATE(TAGMSR_I21_SMS)
              DEALLOCATE(NODXI_SMS)
              DEALLOCATE(NODII_SMS)
              DEALLOCATE(MSKYI_SMS)
              DEALLOCATE(ISKYI_SMS)
              DEALLOCATE(JADI_SMS)
              DEALLOCATE(JDII_SMS)
              DEALLOCATE(LTI_SMS)
            RETURN
      END SUBROUTINE

      END MODULE SMS_MOD
Chd|====================================================================
Chd|  SMS_PCG_PROJ                  share/modules/sms_mod.F       
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|        SMS_INISI                     source/ams/sms_proj.F         
Chd|        SMS_INIST                     source/ams/sms_proj.F         
Chd|        SMS_INIX                      source/ams/sms_proj.F         
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|        SMS_PRO_P                     source/ams/sms_proj.F         
Chd|        SMS_UPDST                     source/ams/sms_proj.F         
Chd|-- calls ---------------
Chd|====================================================================
      MODULE SMS_PCG_PROJ
#include      "my_real.inc"
      my_real,
     .         DIMENSION(:,:),ALLOCATABLE :: PROJ_S,PROJ_T
      my_real,
     .         DIMENSION(:,:),ALLOCATABLE :: PROJ_K
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: PROJ_LA_1,PROJ_W
 
      END MODULE SMS_PCG_PROJ
