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
      !||    arret                   ../starter/source/system/arret.F
      !||--- called by ------------------------------------------------------
      !||    ani_fasolfr1            ../starter/source/output/anim/ani_fasolfr.F
      !||    arret_message           ../starter/source/interfaces/int24/arret_message.F
      !||    arret_message_slid      ../starter/source/interfaces/int24/arret_message.F
      !||    c3grtails               ../starter/source/elements/sh3n/coque3n/c3grtails.F
      !||    c_tricall               ../starter/stub/fvmbags_stub.F
      !||    cgrtails                ../starter/source/elements/shell/coque/cgrtails.F
      !||    contrl                  ../starter/source/starter/contrl.F
      !||    error_alloc             ../starter/source/system/error_alloc.F
      !||    facepoly                ../starter/source/airbag/facepoly.F
      !||    fredec2im               ../starter/source/output/message/fredec2im.F
      !||    fredec3m                ../starter/source/output/message/fredec3m.F
      !||    fvbric0                 ../starter/source/airbag/fvbric0.F
      !||    fvbric01                ../starter/source/airbag/fvbric01.F
      !||    hm_preread_bem          ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_preread_pblast       ../starter/source/loads/pblast/hm_preread_pblast.F
      !||    hm_read_bem             ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_read_fail_user       ../starter/source/materials/fail/failuser/hm_read_fail_user.F
      !||    hm_read_mat             ../starter/source/materials/mat/hm_read_mat.F
      !||    hm_read_mat29_31        ../starter/source/materials/mat/matuser/hm_read_mat_user29_31.F
      !||    hm_read_mat_99          ../starter/source/materials/mat/matuser/hm_read_mat_user_99.F
      !||    hm_read_pblast          ../starter/source/loads/pblast/hm_read_pblast.F
      !||    hm_read_prop06          ../starter/source/properties/solid/hm_read_prop06.F
      !||    hm_read_prop14          ../starter/source/properties/solid/hm_read_prop14.F
      !||    hm_read_prop19          ../starter/source/properties/shell/hm_read_prop19.F
      !||    hm_read_prop_user       ../starter/source/properties/user_spring_solid/hm_read_prop_user.F
      !||    hm_read_retractor       ../starter/source/tools/seatbelts/hm_read_retractor.F
      !||    hm_read_slipring        ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    hm_read_window_user     ../starter/source/tools/userwi/hm_read_window_user.F
      !||    i24fic_getn             ../starter/source/interfaces/inter3d1/i24surfi.F
      !||    initia                  ../starter/source/elements/initia/initia.F
      !||    law111_upd              ../starter/source/materials/mat/mat111/law111_upd.F
      !||    law69_upd               ../starter/source/materials/mat/mat069/law69_upd.F
      !||    law88_upd               ../starter/source/materials/mat/mat088/law88_upd.F
      !||    lecstack_ply            ../starter/source/properties/composite_options/stack/lecstack_ply.F
      !||    lectur                  ../starter/source/starter/lectur.F
      !||    m20dcod                 ../starter/source/system/fsdcod.F
      !||    matini                  ../starter/source/materials/mat_share/matini.F
      !||    monvol_orient_surf      ../starter/share/modules1/monvol_struct_mod.F
      !||    polyhedr1               ../starter/source/airbag/polyhedr1.F
      !||    rbe3cl                  ../starter/source/constraints/general/kinchk.F
      !||    read_msgfile            ../starter/source/output/message/read_msgfile.F
      !||    read_sensor_user        ../starter/source/tools/sensor/read_sensor_user.F
      !||    sgrtails                ../starter/source/elements/solid/solide/sgrtails.F
      !||    sms_ini_jad_3           ../starter/source/ams/sms_init.F
      !||    sort_set                ../starter/source/model/sets/sort_sets.F
      !||    sptri                   ../starter/source/elements/sph/sptri.F
      !||    t3grtails               ../starter/source/elements/solid_2d/tria/t3grtails.F
      !||    tribox3                 ../starter/stub/fvmbags_stub.F
      !||    tritri3                 ../starter/stub/fvmbags_stub.F
      !||    w_bufbric_22            ../starter/source/interfaces/inter3d1/w_bufbric_22.F
      !||    w_front                 ../starter/source/restart/ddsplit/w_front.F
      !||    w_intbuf_size           ../starter/source/restart/ddsplit/inter_tools.F
      !||--- calls      -----------------------------------------------------
      !||    my_exit                 ../starter/source/output/analyse/analyse.c
      !||    summsg                  ../starter/source/output/message/summsg.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod             ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE ARRET (N)  
      USE H3D_MOD
      USE INOUTFILE_MOD
      USE QA_OUT_MOD
      USE DYNLIB_MOD
      USE SCRATCHFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "comlock.inc"
#include      "scr20_c.inc"
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "rad2r_c.inc"
#include      "chara_c.inc"
#include      "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*2048 INAME
      INTEGER IFIL
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME 
C-----------------------------------------------
C Different End Values
C ----------------------------------------------
C N=0  : Same as N=4
C N=1  : Normal Termination - normal case
C        ARRET(1) should be called after MPI_FINALIZE only (radioss2.F)
C N=2  : Error Termination  - calls MPI_ABORT
C N=20 : Error Termination  - calls MPI_ABORT
C N=3  : Normal termination with "USER BREAK" (crontrol file...)
C        ARRET(3) should be called after  MPI_FINALIZE only (radioss2.F)
C N=4  : Stop Before Engine output creation (*_0001.out)
C N=5  : Restart files not found
C N=6  : Abort with system Error (return code=3)
C N=7  : Exiting only - errors before MPP_INIT or after MPI_FINALIZE
C ----------------------------------------------


C ----------------------------------------------
C TEMPO FILES Clean
C-----------------------------------------------
       IF(ISPMD == 0 )THEN
         IF(IS_H3D_USED) CALL C_H3D_CLOSE_FILE()
       ENDIF
C -----------------------------------------------
C ABF FILES
C -----------------------------------------------
      IF (ISPMD == 0 .AND. NABFILE /= 0 .AND. 
     .  (ABINP /= 0 .AND. ABOUT /= 0 )  ) THEN
#ifdef DNC

           CALL RELEASE_ABFPIPE(ABINP,ABOUT)

           IF(ABFILE(1) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.tmp'
             IFIL = 11
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 9
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(2) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_a.tmp'
             IFIL = 12
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(3) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_b.tmp'
             IFIL = 13
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(4) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_c.tmp'
             IFIL = 14
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(5) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_d.tmp'
             IFIL = 15
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(6) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_e.tmp'
             IFIL = 16
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(7) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_f.tmp'
             IFIL = 17
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(8) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_g.tmp'
             IFIL = 18
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(9) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_h.tmp'
             IFIL = 19
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
           IF(ABFILE(10) /= 0) THEN
             INAME=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_i.tmp'
             IFIL = 20
             LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + 11
             TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//INAME(1:LEN_TRIM(INAME))
             CALL CUR_FIL_C(IFIL)
             CALL DELETE_TMPFILE(TMP_NAME,LEN_TMP_NAME)
           ENDIF
#endif
      ENDIF

      ! ----------------------
      ! user library : check in
      IF(DLIB_STRUCT(ID_ENGINE_USER_FINALIZE)%DLIB_BOOL) THEN
        ISPMD_USER = ISPMD
        CALL ENGINE_USER_FINALIZE( ISPMD_USER )
      ENDIF
      ! ----------------------
#ifdef DNC
      IF(MDS_AVAIL==1) THEN
        ISPMD_USER = ISPMD
        CALL MDS_ENGINE_USER_FINALIZE( ISPMD_USER )
      ENDIF
#endif

      ! ------------------------------------------
      ! Delete Temp. scratchfiles before exiting.
      ! ------------------------------------------
      IF(USE_IFXM == 1)THEN
        ClOSE(UNIT=IFXM)
        CALL DELETE_USER_FILE(IFXM_FN,LEN_IFXM_FN)
      ENDIF
      
      IF(USE_IFXS == 1)THEN
        CLOSE(UNIT=IFXS)
        CALL DELETE_USER_FILE(IFXS_FN,LEN_IFXS_FN)
      ENDIF
      
      IF (USE_IEIGM==1)THEN
        CLOSE(IEIGM)
        CALL DELETE_USER_FILE(IEIGM_FN,LEN_IEIGM_FN)
      ENDIF

C -----------------------------------------------
      IF (ISPMD == 0) THEN
       CALL QACLOSE()
      ENDIF
C
      SELECT CASE (N)
        CASE(0)                                  ! Stop before engine output file was written
              WRITE(ISTDO,1100)
              CALL SPMD_KILL(2)
              CALL MY_EXIT(2)
        CASE (1)
              IF (ISPMD==0) THEN
                WRITE(IOUT,1000)          ! Normal termination 
                WRITE(ISTDO,1050)

                WRITE(IOUT,1200) NCYCLE   ! Total Number of cycles
                WRITE(ISTDO,1250) NCYCLE
                CALL PRINT_RUNTIME( )

                IF (IHTML == 1) CALL REPORT(IH3DREADER,N,IMVW)                ! Htlm report
                IF (IMVW /= 0) CALL SORTIE_MVW(IMVW,IMVW_REF)               ! MVW output
                IF ((IRAD2R==1).AND.(ISPMD==0)) CALL SEND_IBUF_C(1,1)      ! Rad2rad termination

              ENDIF

              CLOSE(UNIT=IOUT)
              IF(ISPMD /=0) THEN
                 CALL DELETE_USER_FILE(IUSC4_FNAM,LEN_IUSC4_FNAM)
              ENDIF

              CLOSE(UNIT=IUHIS)
              CALL MY_EXIT(0)
 
        CASE (2)
              WRITE(IOUT,1100)
              WRITE(ISTDO,1150)

              CALL SPMD_KILL(2)
              CALL MY_EXIT(2)

        CASE (20)
              WRITE(IOUT,1100)
              WRITE(ISTDO,1150)

              CALL SPMD_KILL(2)
              CALL MY_EXIT(2)

        CASE (3)
              IF (ISPMD==0) THEN
                WRITE(IOUT,1000)
                WRITE(ISTDO,1050)
                WRITE(IOUT,1300)
                WRITE(ISTDO,1300)

                WRITE(IOUT,1200) NCYCLE
                WRITE(ISTDO,1250) NCYCLE
                CALL PRINT_RUNTIME( )

                IF (IHTML == 1) CALL REPORT(IH3DREADER,N,IMVW)
                IF (IMVW /= 0) CALL SORTIE_MVW(IMVW,IMVW_REF)
              ENDIF

              CLOSE(UNIT=IOUT)
              IF(ISPMD /=0) THEN
                 CALL DELETE_USER_FILE(IUSC4_FNAM,LEN_IUSC4_FNAM)
              ENDIF

              CLOSE(UNIT=IUHIS)

              CALL MY_EXIT(0)

        CASE(4)
              WRITE(ISTDO,1400)
              IF (ISPMD==0) THEN     
              ENDIF
              CALL SPMD_KILL(2)
              CALL MY_EXIT(2)
        CASE(5)                                  ! restart file not found
              WRITE(ISTDO,1400)
              CALL SPMD_KILL(2)
              CALL MY_EXIT(2)
        CASE (6)                                     ! system trapped error (SIGKILL, SIGSEV etc...)
              WRITE(IOUT,1100)
              WRITE(ISTDO,1150)

              IF (ISPMD==0)THEN
              ENDIF
              CALL SPMD_KILL(3)
        CASE (7)                                     ! Exiting only - errors before MPP_INIT
             CALL MY_EXIT(2)
      END SELECT 

C
 1000 FORMAT(///'     NORMAL TERMINATION      ')
 1050 FORMAT('     NORMAL TERMINATION      ')
 1100 FORMAT(///'     ERROR TERMINATION')
 1400 FORMAT(/'     ERROR TERMINATION')
 1150 FORMAT('     ERROR TERMINATION')
 1200 FORMAT(///'     TOTAL NUMBER OF CYCLES  :', I8 / )
 1250 FORMAT('     TOTAL NUMBER OF CYCLES  :', I8)
 1300 FORMAT('     USER BREAK ')
C

      END
