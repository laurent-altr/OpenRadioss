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
      !||    write_db               ../starter/source/restart/ddsplit/wrrest.F
      !||--- called by ------------------------------------------------------
      !||    convwrest              ../starter/source/restart/ddsplit/wrrest.F
      !||    copy_rval              ../starter/source/restart/ddsplit/inter_tools.F
      !||    ddsplit                ../starter/source/restart/ddsplit/ddsplit.F
      !||    dswrest                ../starter/source/restart/ddsplit/wrrest.F
      !||    dswrestp               ../starter/source/restart/ddsplit/wrrest.F
      !||    eigwrest               ../starter/source/restart/ddsplit/wrrest.F
      !||    eigwrestp              ../starter/source/restart/ddsplit/wrrest.F
      !||    fvwrestp               ../starter/source/restart/ddsplit/fvwrestp.F
      !||    fxbwrestp              ../starter/source/restart/ddsplit/wrrest.F
      !||    fxtempwrest            ../starter/source/restart/ddsplit/wrrest.F
      !||    intfric_wrestr         ../starter/source/interfaces/intbuf/intbufFric_ini_starter.F
      !||    intstamp_wrestr        ../starter/share/modules1/intstamp_mod.F
      !||    nfwrest                ../starter/source/restart/ddsplit/wrrest.F
      !||    nfwrestp               ../starter/source/restart/ddsplit/wrrest.F
      !||    plyxfem_wrest          ../starter/source/restart/ddsplit/wrrest.F
      !||    rigmatwrest            ../starter/source/restart/ddsplit/wrrest.F
      !||    split_cand_rval        ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_node_rval        ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_seg_rval_i20     ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_xsav             ../starter/source/restart/ddsplit/inter_tools.F
      !||    spmd_userwi_rest       ../starter/source/user_interface/user_windows_tools.F
      !||    table_wrestr           ../starter/source/tools/curve/table_tools.F
      !||    thcwrest               ../starter/source/restart/ddsplit/wrrest.F
      !||    w_alelag               ../starter/source/restart/ddsplit/w_alelag.F
      !||    w_anim_crk             ../starter/source/restart/ddsplit/w_anim_crk.F
      !||    w_avuply               ../starter/source/restart/ddsplit/w_avuply.F
      !||    w_bid                  ../starter/source/restart/ddsplit/w_bid.F
      !||    w_bufbric_22           ../starter/source/interfaces/inter3d1/w_bufbric_22.F
      !||    w_bufel                ../starter/source/restart/ddsplit/w_bufel.F
      !||    w_cluster              ../starter/source/restart/ddsplit/w_cluster.F
      !||    w_disp0_vel0_cload     ../starter/source/restart/ddsplit/w_disp0_vel0_cload.F
      !||    w_drape                ../starter/source/restart/ddsplit/w_drape.F
      !||    w_elbuf_str            ../starter/source/restart/ddsplit/w_elbuf_str.F
      !||    w_fac                  ../starter/source/restart/ddsplit/w_fac.F
      !||    w_fbft                 ../starter/source/restart/ddsplit/w_fbft.F
      !||    w_fconv                ../starter/source/restart/ddsplit/w_fconv.F
      !||    w_fixvel               ../starter/source/restart/ddsplit/w_fixvel.F
      !||    w_fradia               ../starter/source/restart/ddsplit/w_fradia.F
      !||    w_frbe3                ../starter/source/restart/ddsplit/w_frbe3.F
      !||    w_fthflux              ../starter/source/restart/ddsplit/w_fthflux.F
      !||    w_knotloc              ../starter/source/restart/ddsplit/w_knotloc.F
      !||    w_outmaxn              ../starter/source/restart/ddsplit/w_outmaxn.F
      !||    w_pxfem                ../starter/source/restart/ddsplit/w_pxfem.F
      !||    w_r3nloc               ../starter/source/restart/ddsplit/w_r3nloc.F
      !||    w_reloc                ../starter/source/restart/ddsplit/w_reloc.F
      !||    w_reloc2               ../starter/source/restart/ddsplit/w_reloc2.F
      !||    w_rfilloc              ../starter/source/restart/ddsplit/w_rfilloc.F
      !||    w_rnloc                ../starter/source/restart/ddsplit/w_rnloc.F
      !||    w_rnnloc               ../starter/source/restart/ddsplit/w_rnnloc.F
      !||    w_rsph                 ../starter/source/restart/ddsplit/w_rsph.F
      !||    w_rwar                 ../starter/source/restart/ddsplit/w_rwar.F
      !||    w_seatbelts            ../starter/source/restart/ddsplit/w_seatbelts.F
      !||    w_secbuf               ../starter/source/restart/ddsplit/w_secbuf.F
      !||    w_skwsph               ../starter/source/restart/ddsplit/w_skwsph.F
      !||    w_skwsub               ../starter/source/restart/ddsplit/w_skwsub.F
      !||    w_stack                ../starter/source/restart/ddsplit/w_stack.F
      !||    w_tabmat_prop          ../starter/source/restart/ddsplit/w_tabmat_prop.F
      !||    write_db_debug         ../starter/source/output/tools/write_debug.F
      !||    write_elgroup_param    ../starter/source/restart/ddsplit/write_elgroup_param.F
      !||    write_failparam        ../starter/source/materials/fail/write_failparam.F
      !||    write_mat_table        ../starter/source/materials/tools/write_mat_table.F
      !||    write_matparam         ../starter/source/materials/mat/write_matparam.F
      !||    write_nloc_struct      ../starter/source/restart/ddsplit/write_nloc_struct.F
      !||    write_pcyl             ../starter/source/loads/general/load_pcyl/write_pcyl.F
      !||    write_sensors          ../starter/source/tools/sensor/write_sensors.F
      !||    write_viscparam        ../starter/source/materials/mat/write_viscparam.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE WRITE_DB(A,N)
C ecriture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C     REAL
      my_real
     .   A(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N1,N2
      DOUBLE PRECISION R8(1000)
      REAL R4(1000)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      K=0
      N1=N/1000
      IF(ICRAY==1)THEN
C fortran double precision : 128 bits C double : 64 bits
C fortran real : 64 bits C double : 64 bits
       DO I = 1, N1
        DO J = 1, 1000
          K=K+1
          R4(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R4,1000)
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        DO J = 1, N-N2
          K=K+1
          R4(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R4,N-N2)
       ENDIF
      ELSE
C fortran double precision : 64 bits C double : 64 bits
       DO I = 1, N1
        DO J = 1, 1000
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,1000)
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        DO J = 1, N-N2
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,N-N2)
       ENDIF
      ENDIF
C
      RETURN
      END
      !||====================================================================
      !||    write_dpdb        ../starter/source/restart/ddsplit/wrrest.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit           ../starter/source/restart/ddsplit/ddsplit.F
      !||    intstamp_wrestr   ../starter/share/modules1/intstamp_mod.F
      !||    w_elbuf_str       ../starter/source/restart/ddsplit/w_elbuf_str.F
      !||    write_sensors     ../starter/source/tools/sensor/write_sensors.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE WRITE_DPDB(A,N)
C ecriture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
      DOUBLE PRECISION
     .   A(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N1,N2
      DOUBLE PRECISION R8(1000)
      REAL R4(1000)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      K=0
      N1=N/1000
C fortran double precision : 64 bits C double : 64 bits
       DO I = 1, N1
        DO J = 1, 1000
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,1000)
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        DO J = 1, N-N2
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,N-N2)
       ENDIF
C
      RETURN
      END
