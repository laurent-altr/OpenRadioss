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
C=======================================================================
C     // MEMOIRE PARTAGEE
C=======================================================================
      !||====================================================================
      !||    my_barrier   ../starter/source/system/machine.F
      !||====================================================================
      SUBROUTINE MY_BARRIER
#include     "implicit_f.inc"
#include     "comlock.inc"
#include     "task_c.inc"
#include     "units_c.inc"
#if defined(_OPENMP)
!$OMP BARRIER
#endif
      RETURN
      END
C
      !||====================================================================
      !||    idp_lock       ../engine/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    asspar2        ../engine/source/assembly/asspar2.F
      !||    bafil2         ../engine/source/ale/bimat/bafil2.F
      !||    balph2         ../engine/source/ale/bimat/balph2.F
      !||    befil2         ../engine/source/ale/bimat/befil2.F
      !||    qmass2         ../engine/source/elements/solid_2d/quad/qmass2.F
      !||    qmassreal2     ../engine/source/elements/solid_2d/quad/qmassreal2.F
      !||    user_lock      ../engine/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE IDP_LOCK(ID)
#include "comlock.inc"
      INTEGER ID
#if defined(_OPENMP)
        CALL OMP_SET_LOCK(LLOCK(1,ID))
#endif
      RETURN
      END
C
      !||====================================================================
      !||    user_lock   ../engine/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||    idp_lock    ../engine/source/system/machine.F
      !||====================================================================
      SUBROUTINE USER_LOCK()
      CALL IDP_LOCK(1)
      RETURN
      END
C
      !||====================================================================
      !||    idp_free         ../engine/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    asspar2          ../engine/source/assembly/asspar2.F
      !||    bafil2           ../engine/source/ale/bimat/bafil2.F
      !||    balph2           ../engine/source/ale/bimat/balph2.F
      !||    befil2           ../engine/source/ale/bimat/befil2.F
      !||    qmass2           ../engine/source/elements/solid_2d/quad/qmass2.F
      !||    qmassreal2       ../engine/source/elements/solid_2d/quad/qmassreal2.F
      !||    user_free        ../engine/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE IDP_FREE(ID)
#include "comlock.inc"
      INTEGER ID
#if defined(_OPENMP)
        CALL OMP_UNSET_LOCK(LLOCK(1,ID))
#endif
      RETURN
      END
C
      !||====================================================================
      !||    user_free   ../engine/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||    idp_free    ../engine/source/system/machine.F
      !||====================================================================
      SUBROUTINE USER_FREE()
      CALL IDP_FREE(1)
      RETURN
      END
C
      !||====================================================================
      !||    wait_task   ../engine/source/system/machine.F
      !||====================================================================
      SUBROUTINE WAIT_TASK(ITID)
      INTEGER ITID(*),IT,JT
#if defined(_OPENMP)
#endif
      RETURN
      END

C=======================================================================
C=======================================================================
C     ROUTINES CRAY
C=======================================================================
#if CPP_mach != CPP_c90 && CPP_mach != CPP_ymp && CPP_mach != CPP_ymp_spmd && CPP_mach != CPP_t90 && CPP_mach != CPP_t90_i3e
      my_real
     .   FUNCTION CVMGN(X1,X2,X3)
#include      "implicit_f.inc"
      my_real
     .   X1, X2, X3
      IF(X3==0.) THEN
       CVMGN=X2
      ELSE
       CVMGN=X1
      ENDIF
      RETURN
      END
      my_real
     .   FUNCTION CVMGT(X1,X2,X3)
#include      "implicit_f.inc"
      my_real
     .   X1, X2
      LOGICAL X3
      IF(X3) THEN
       CVMGT=X1
      ELSE
       CVMGT=X2
      ENDIF
      RETURN
      END
#endif
C=======================================================================
C  ROUTINE DE SYNCHRO POUR LE CRAY EN SMP
C=======================================================================
C cette routine doit etre appelee pour eviter certaines
C optimisations super-scalaire incompatible avec notion
C de var. partagee.
      !||====================================================================
      !||    sync_data      ../engine/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    admregul       ../engine/source/model/remesh/admregul.F
      !||    i10optcd       ../engine/source/interfaces/intsort/i10optcd.F
      !||    i11optcd       ../engine/source/interfaces/intsort/i11optcd.F
      !||    i20optcd       ../engine/source/interfaces/intsort/i20optcd.F
      !||    i20optcde      ../engine/source/interfaces/intsort/i20optcd.F
      !||    i23optcd       ../engine/source/interfaces/int23/i23optcd.F
      !||    i24optcd       ../engine/source/interfaces/intsort/i24optcd.F
      !||    i25optcd       ../engine/source/interfaces/intsort/i25optcd.F
      !||    i25optcd_e2s   ../engine/source/interfaces/intsort/i25optcd_e2s.F
      !||    i25optcd_edg   ../engine/source/interfaces/intsort/i25optcd_edg.F
      !||    i7optcd        ../engine/source/interfaces/intsort/i7optcd.F
      !||    thnod          ../engine/source/output/th/thnod.F
      !||====================================================================
      SUBROUTINE SYNC_DATA(II)
      INTEGER II(*)
      RETURN
      END
C=======================================================================
C  ROUTINE ETIME
C=======================================================================
      !||====================================================================
      !||    my_etime   ../starter/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    startime   ../starter/source/system/timer.F
      !||    stoptime   ../starter/source/system/timer.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE MY_ETIME(TIMER)
      REAL  RESULT
      REAL, DIMENSION(2) :: TIMER
#if CPP_mach == CPP_rs7 || CPP_mach == CPP_rs9 || CPP_mach == CPP_sp2  || CPP_mach == CPP_ppc || CPP_mach == CPP_pwr4 || CPP_mach == CPP_pwr4_spmd    
       RESULT = ETIME_(TIMER)
#elif CPP_mach == CPP_ymp || CPP_mach == CPP_c90 || CPP_mach == CPP_ymp_spmd || CPP_mach == CPP_t90  || CPP_mach == CPP_t90_i3e
      CALL TSECND(TIMER)
#elif CPP_mach == CPP_sx4 || CPP_mach == CPP_sx4_i3e
      RESULT = ETIME(TIMER)
/* el51e9 #elif CPP_mach == CPP_w95 || CPP_mach == CPP_ant || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr */
#elif CPP_mach == CPP_w95 || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr
      TIMER(1) = 0.
#elif CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_p4win64
      CALL CPU_TIME(TIMER(1))
#elif CPP_mach == CPP_cx1_spmd || CPP_mach == CPP_cx1
      RESULT = ETIMEC(TIMER)
#elif 1
      RESULT = ETIME(TIMER)
#endif
      RETURN
      END
C=======================================================================
C  ROUTINE  VERIFICATION LIMITATIONS MACHINE
C=======================================================================
      !||====================================================================
      !||    my_flush   ../starter/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE MY_FLUSH(IUNIT)
C appel Fortran flush
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IUNIT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C call a rendre specifique machine en cas de pb de compilation
#if CPP_mach == CPP_pwr4 || CPP_mach == CPP_pwr4_spmd
C   pas de routine FLUSH sur AIX
#elif 1
      CALL FLUSH(IUNIT)
#endif
      RETURN
      END

      !||====================================================================
      !||    mkl_init          ../engine/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE MKL_INIT(MKL_DYN,OMP_NEST)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      LOGICAL MKL_DYN,OMP_NEST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL omp_get_nested, mkl_get_dynamic
      EXTERNAL omp_get_nested, mkl_get_dynamic
C call a rendre specifique machine en cas de pb de compilation
C#if CPP_mach == CPP_p4linux964 || CPP_mach == CPP_p4linux932 || CPP_mach == CPP_il || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
#if defined mkl
         MKL_DYN = omp_get_nested()
         OMP_NEST = mkl_get_dynamic()
         call omp_set_nested(.true.)
         call mkl_set_dynamic(.false.)
#endif
      RETURN
      END

      !||====================================================================
      !||    mkl_reset         ../engine/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE MKL_RESET(MKL_DYN,OMP_NEST)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      LOGICAL MKL_DYN,OMP_NEST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL omp_get_nested, mkl_get_dynamic
      EXTERNAL omp_get_nested, mkl_get_dynamic
C call a rendre specifique machine en cas de pb de compilation
C#if CPP_mach == CPP_p4linux964 || CPP_mach == CPP_p4linux932 || CPP_mach == CPP_il || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
#if defined mkl
         call omp_set_nested(OMP_NEST)
         call mkl_set_dynamic(MKL_DYN)
c         MKL_DYN = omp_get_nested()
c         OMP_NEST = mkl_get_dynamic()
#endif
      RETURN
      END

#if CPP_mach == CPP_p4linux964 || CPP_mach == CPP_p4win64
ccc|| CPP_mach == CPP_p4win64  || CPP_mach == CPP_p4win32 || CPP_mach==CPP_macosx64
      !||====================================================================
      !||    check_kmp_stack    ../starter/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    set_stack_values   ../starter/source/system/machine.F
      !||====================================================================
      SUBROUTINE CHECK_KMP_STACK()
C-----------------------------------------------
#ifdef __INTEL_COMPILER
      use omp_lib
C-----------------------------------------------      
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER   (KIND=KMP_SIZE_T_KIND)  NEWSIZ
      CHARACTER*256 KMP_STACKSIZE_VALUE
 
      KMP_SET = 0
 
      CALL GETENV('KMP_STACKSIZE',KMP_STACKSIZE_VALUE)
C      
         IF (KMP_STACKSIZE_VALUE(1:1) == ' ') THEN
         KMP_SET = 1
         NEWSIZ = 134217728
         CALL KMP_SET_STACKSIZE_S(NEWSIZ)
      ELSE
         KMPSTSIZ = KMP_GET_STACKSIZE()
         IF (KMPSTSIZ < 134217728)THEN
           KMP_SET = 2
         ENDIF
      ENDIF
#elif 1
#include      "implicit_f.inc"

#endif
      RETURN
      END
      !||====================================================================
      !||    get_kmp_stack   ../starter/source/system/machine.F
      !||====================================================================
      SUBROUTINE GET_KMP_STACK( )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#ifdef __INTEL_COMPILER
      use omp_lib
#include      "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  KMPSTSIZ,NEWSIZ
C         
      KMPSTSIZ = KMP_GET_STACKSIZE()
      print*,'KMPSTSIZ=',KMPSTSIZ

#elif 1

#endif
      RETURN
      END
#elif 1
      !||====================================================================
      !||    check_kmp_stack    ../starter/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    set_stack_values   ../starter/source/system/machine.F
      !||====================================================================
      SUBROUTINE CHECK_KMP_STACK( )
      RETURN
      END
C
      !||====================================================================
      !||    get_kmp_stack   ../starter/source/system/machine.F
      !||====================================================================
      SUBROUTINE GET_KMP_STACK( )
      RETURN
      END
#endif

      !||====================================================================
      !||    set_stack_values   ../starter/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    starter            ../starter/source/starter/starter.F
      !||--- calls      -----------------------------------------------------
      !||    check_kmp_stack    ../starter/source/system/machine.F
      !||====================================================================
      SUBROUTINE SET_STACK_VALUES( )
#include      "implicit_f.inc"
#include      "task_c.inc"
C Check & Set Stacksize value
       INTEGER Stack

       CALL SET_STACK_C(Stack)
       STACKSIZ = Stack

C Check KMP_STACKSIZE Value
       CALL  CHECK_KMP_STACK( )
      END

      !||====================================================================
      !||    write_iout   ../starter/source/user_interface/uaccess.F
      !||====================================================================
      SUBROUTINE WRITE_IOUT(LINE,LEN1)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include  "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN1
      CHARACTER(len=LEN1)  LINE
C-----------------------------------------------
#include "lockon.inc"
      WRITE(IOUT,'(A)') LINE(1:LEN1)
#include "lockoff.inc"

      END
C-----------------------------------------------

