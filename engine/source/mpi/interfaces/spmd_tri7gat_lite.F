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
            SUBROUTINE SPMD_TRI7GAT_LITE(WIN,
     .       RESULT ,NSN    ,CAND_N  ,I_STOK  ,NIN    ,
     2                              IGAP   ,NSNR   ,MULTIMP ,ITY     ,INTTH  ,
     3                              ILEV   ,NSNFIOLD,IPARI  ,H3D_DATA,INTFRIC,
     4                              MULTI_FVM,NODADT_THERM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
              USE ISO_C_BINDING
              USE inter_shm_mod
              USE TRI7BOX
              USE MESSAGE_MOD
              USE H3D_MOD
              USE MULTI_FVM_MOD
              USE SPMD_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
              TYPE(inter_win_), intent(inout) :: WIN
              INTEGER RESULT, NIN, NSN, I_STOK, IGAP, NSNR, MULTIMP, ITY,
     .                CAND_N(*),INTTH,ILEV, INTFRIC,
     .                NSNFIOLD(*), IPARI(NPARI,NINTER)
              INTEGER , INTENT(IN) :: NODADT_THERM
              TYPE(H3D_DATABASE) :: H3D_DATA
              TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
              INTEGER nb_remote,NODFI,NNP,LSKYFI,
     .                NOD, LOC_PROC, I, N, NN, P, IDEB, J, K,
     .                NN2,RSHIFT,ISHIFT, IOLDNSNFI, ND, NSNR_OLD, Q

              INTEGER, DIMENSION(:), ALLOCATABLE :: IAUX
              my_real,
     .            DIMENSION(:), ALLOCATABLE :: PENEFI_OLD, STIFFI_OLD
              my_real,
     .             DIMENSION(:,:), ALLOCATABLE :: SECND_FRFI_OLD
              logical, dimension(:), allocatable :: NEEDED_NODE
              integer, dimension(:), allocatable :: INDEX
              integer :: ielec_offset, temp_offset, areas_offset, gap_s_offset, gap_sl_offset, ipartfrics_offset
              integer :: x_offset, v_offset, ms_offset, stfns_offset
              integer :: ispmd_offset, itab_offset, IKINET_offset, i_offset
              double precision,  dimension(:,:), pointer :: shared_real
              integer(kind=8), dimension(:,:), pointer :: shared_int
              integer :: size_shared_data
C-----------------------------------------------
              size_shared_data = (WIN%RSIZ + WIN%ISIZ)!
              call c_f_pointer(win%shared_base, shared_int,[ size_shared_data, win%nsn_global ] )
              call c_f_pointer(win%shared_base, shared_real,[ size_shared_data, win%nsn_global ] )
              ielec_offset     =  win%ielec_offset               
              temp_offset      =  win%temp_offset              
              areas_offset     =  win%areas_offset                
              gap_s_offset     =  win%gap_s_offset                
              gap_sl_offset    =  win%gap_sl_offset                
              ipartfrics_offset=  win%ipartfrics_offset                      
              x_offset         =  win%x_offset            
              v_offset         =  win%v_offset             
              ms_offset        =  win%ms_offset             
              stfns_offset     =  win%stfns_offset           
              ispmd_offset     =  win%ispmd_offset           
              itab_offset      =  win%itab_offset          
              ikinet_offset    =  win%ikinet_offset          
              i_offset         =  win%i_offset              

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
              LOC_PROC = ISPMD + 1
C
C
              NODFI = 0
              LSKYFI= 0
              ALLOCATE(INDEX(WIN%nsn_global))
              ALLOCATE(NEEDED_NODE(WIN%nsn_global))
                NEEDED_NODE = .FALSE.

              IF(RESULT==0) THEN
C Reperage des candidats
C

                DO I = 1, I_STOK
                  N = CAND_N(I)
                  NN = N-NSN
                  IF(NN>0)THEN
                    IF(.NOT. NEEDED_NODE(NN)) THEN
                      NODFI = NODFI + 1
                      NEEDED_NODE(NN) = .TRUE.
                    ENDIF
                  ENDIF
                ENDDO
!               IF(NODFI == 0) write(6,*) __LINE__, NODFI,I_STOK
C
C Allocation des tableaux de frontieres interfaces
                IF(ASSOCIATED(NSVFI(NIN)%P)) DEALLOCATE(NSVFI(NIN)%P)
                ALLOCATE(NSVFI(NIN)%P(NODFI))
                IF(ASSOCIATED(PMAINFI(NIN)%P)) DEALLOCATE(PMAINFI(NIN)%P)
                ALLOCATE(PMAINFI(NIN)%P(NODFI))
                IF(ASSOCIATED(XFI(NIN)%P)) DEALLOCATE(XFI(NIN)%P)
                ALLOCATE(XFI(NIN)%P(3,NODFI))
                IF(ASSOCIATED(VFI(NIN)%P)) DEALLOCATE(VFI(NIN)%P)
                ALLOCATE(VFI(NIN)%P(3,NODFI))
                IF(ASSOCIATED(MSFI(NIN)%P)) DEALLOCATE(MSFI(NIN)%P)
                ALLOCATE(MSFI(NIN)%P(NODFI))
                IF(ASSOCIATED(STIFI(NIN)%P)) DEALLOCATE(STIFI(NIN)%P)
                ALLOCATE(STIFI(NIN)%P(NODFI))
                IF(ASSOCIATED(ITAFI(NIN)%P)) DEALLOCATE(ITAFI(NIN)%P)
                ALLOCATE(ITAFI(NIN)%P(NODFI))
                IF(ITY==7.OR.ITY==22.OR.ITY==23.OR.ITY==24) THEN
                  IF(ASSOCIATED(KINFI(NIN)%P)) DEALLOCATE(KINFI(NIN)%P)
                  ALLOCATE(KINFI(NIN)%P(NODFI))
                  IF(INTTH > 0 ) THEN
                    IF(ASSOCIATED(TEMPFI(NIN)%P)) DEALLOCATE(TEMPFI(NIN)%P)
                    ALLOCATE(TEMPFI(NIN)%P(NODFI))
                    IF(ASSOCIATED(MATSFI(NIN)%P)) DEALLOCATE(MATSFI(NIN)%P)
                    ALLOCATE(MATSFI(NIN)%P(NODFI))
                    IF(ASSOCIATED(AREASFI(NIN)%P)) DEALLOCATE(AREASFI(NIN)%P)
                    ALLOCATE(AREASFI(NIN)%P(NODFI))
                  ENDIF
                ENDIF
                IF(IDTMINS == 2) THEN
                  IF(ASSOCIATED(NODNXFI(NIN)%P)) DEALLOCATE(NODNXFI(NIN)%P)
                  ALLOCATE(NODNXFI(NIN)%P(NODFI))
                  IF(ASSOCIATED(NODAMSFI(NIN)%P)) DEALLOCATE(NODAMSFI(NIN)%P)
                  ALLOCATE(NODAMSFI(NIN)%P(NODFI))
                  IF(ASSOCIATED(PROCAMSFI(NIN)%P)) DEALLOCATE(PROCAMSFI(NIN)%P)
                  ALLOCATE(PROCAMSFI(NIN)%P(NODFI))
                ELSEIF(IDTMINS_INT /= 0) THEN
                  IF(ASSOCIATED(NODAMSFI(NIN)%P)) DEALLOCATE(NODAMSFI(NIN)%P)
                  ALLOCATE(NODAMSFI(NIN)%P(NODFI))
                  IF(ASSOCIATED(PROCAMSFI(NIN)%P)) DEALLOCATE(PROCAMSFI(NIN)%P)
                  ALLOCATE(PROCAMSFI(NIN)%P(NODFI))
                ENDIF
                IF(IGAP/=0) THEN
                  IF(ASSOCIATED(GAPFI(NIN)%P)) DEALLOCATE(GAPFI(NIN)%P)
                  ALLOCATE(GAPFI(NIN)%P(NODFI))
                  IF(IGAP==3) THEN
                    IF(ASSOCIATED(GAP_LFI(NIN)%P)) DEALLOCATE(GAP_LFI(NIN)%P)
                    ALLOCATE(GAP_LFI(NIN)%P(NODFI))
                  ENDIF
                ENDIF
                IF(ITY==24)THEN
                  IF(ASSOCIATED(IRTLM_FI(NIN)%P)) DEALLOCATE(IRTLM_FI(NIN)%P)
                  ALLOCATE(IRTLM_FI(NIN)%P(2,NODFI))

                  IF(ASSOCIATED(TIME_SFI(NIN)%P)) DEALLOCATE(TIME_SFI(NIN)%P)
                  ALLOCATE(TIME_SFI(NIN)%P(NODFI))

                  IF(ASSOCIATED(SECND_FRFI(NIN)%P)) DEALLOCATE(SECND_FRFI(NIN)%P)
                  ALLOCATE(SECND_FRFI(NIN)%P(6,NODFI))

                  IF(ASSOCIATED(PENE_OLDFI(NIN)%P))DEALLOCATE(PENE_OLDFI(NIN)%P)
                  ALLOCATE(PENE_OLDFI(NIN)%P(5,NODFI))

                  IF(ASSOCIATED(STIF_OLDFI(NIN)%P))DEALLOCATE(STIF_OLDFI(NIN)%P)
                  ALLOCATE(STIF_OLDFI(NIN)%P(2,NODFI))

                  IF(ASSOCIATED(ICONT_I_FI(NIN)%P))DEALLOCATE(ICONT_I_FI(NIN)%P)
                  ALLOCATE(ICONT_I_FI(NIN)%P(NODFI))
                ENDIF
c
                IF(ITY==7) THEN
                  IF(INTFRIC > 0 ) THEN
                    IF(ASSOCIATED(IPARTFRICSFI(NIN)%P)) DEALLOCATE(IPARTFRICSFI(NIN)%P)
                    ALLOCATE(IPARTFRICSFI(NIN)%P(NODFI))
                  ENDIF
                ENDIF
C
C               ! ----------------------
C               ! /TYPE18 + /LAW 151
C               IF( MULTI_FVM%IS_INT18_LAW151.AND.IPARIT/=0 ) THEN
C                 ! -----------
C                 ! check if the present interface is a TYPE18+LAW151
C                 IF( MULTI_FVM%INT18_GLOBAL_LIST(NIN) ) THEN
C                   IF( ALLOCATED( MULTI_FVM%R_AFI(NIN)%R_FORCE_INT ) ) DEALLOCATE( MULTI_FVM%R_AFI(NIN)%R_FORCE_INT )
C                   MULTI_FVM%R_AFI(NIN)%NODFI = NODFI
C                   ALLOCATE( MULTI_FVM%R_AFI(NIN)%R_FORCE_INT(3,6,NODFI*NTHREAD) )
C                   MULTI_FVM%R_AFI(NIN)%R_FORCE_INT(1:3,1:6,1:NODFI*NTHREAD) = 0d+00
C                 ENDIF
C               ENDIF
                ! ----------------------
                 NSNFI(NIN)%P(1:NSPMD) = 0

C Compactage des candidats
                NN2 = 0
                DO P = 1, NSPMD ! PP = proceesses that owns a part of the interface
                  if(P == ispmd + 1) CYCLE
                  IDEB = win%starting_node_of_proc(P)
                  if(IDEB > 0)  THEN ! skip rank that is not in the interface
                    NN = 0
                    nb_remote =  win%nsns_in_world(P) 
                    IF(nb_remote > 0) THEN
                      ALLOCATE(IAUX(nb_remote))
                      NNP = NN2
                      ! points to the first node of the shared data (2D)
                      DO I = 1, nb_remote
                        if(i + IDEB > win%nsn_global) write(6,*) __LINE__,'ERROR',I+IDEB,win%nsn_global
                        call flush(6)
                        IF(NEEDED_NODE(I+IDEB)) THEN
                          NN = NN + 1
                          IAUX(NN) = I+IDEB
                        ENDIF
                      ENDDO

                      DO J = 1, NN
                        I = IAUX(J)
                        INDEX(I) = NN2+J
                        if(i <= 0) write(6,*) 'ERROR', I
                        if(I > win%nsn_global) write(6,*) 'ERROR', I, win%nsn_global
                        call flush(6)
                        if(NN2+J > NODFI) write(6,*) 'ERROR',J,NN             
                        call flush(6)
                        XFI(NIN)%P(1,NN2+J) = SHARED_REAL(x_offset,I)
                        XFI(NIN)%P(2,NN2+J) = SHARED_REAL(x_offset+1,I)
                        XFI(NIN)%P(3,NN2+J) = SHARED_REAL(x_offset+2,I)
                        VFI(NIN)%P(1,NN2+J) = SHARED_REAL(v_offset+0,I)
                        VFI(NIN)%P(2,NN2+J) = SHARED_REAL(v_offset+1,I)
                        VFI(NIN)%P(3,NN2+J) = SHARED_REAL(v_offset+2,I)
                        MSFI(NIN)%P(NN2+J)  = SHARED_REAL(ms_offset,I)
                        STIFI(NIN)%P(NN2+J) = SHARED_REAL(stfns_offset,I)
                        NSVFI(NIN)%P(NN2+J) = SHARED_INT(i_offset,I)
                        ITAFI(NIN)%P(NN2+J) = SHARED_INT(itab_offset,I)
                        KINFI(NIN)%P(NN2+J) = SHARED_INT(IKINET_offset,I)
                        PMAINFI(NIN)%P(NN2+J) = SHARED_INT(ispmd_offset,I)
                        IF(IGAP==1 .OR. IGAP==2)THEN
                          GAPFI(NIN)%P(NN2+J) = SHARED_REAL(gap_s_offset,I)
                        ELSEIF(IGAP==3)THEN
                          GAPFI(NIN)%P(NN2+J)   = SHARED_REAL(gap_s_offset,I)
                          GAP_LFI(NIN)%P(NN2+J) = SHARED_REAL(gap_sl_offset,I)
                        ENDIF
                        IF(INTFRIC>0)THEN
                          IPARTFRICSFI(NIN)%P(NN2+J)  = SHARED_INT(ipartfrics_offset,I)
                        ENDIF
                      ENDDO
                      NN2 = NN2 + NN
                      IDEB = IDEB + nb_remote
                      NSNFI(NIN)%P(P) = NN2-NNP
                      DEALLOCATE(IAUX)
                    ENDIF
                  ENDIF !IF(nb_remote/=0)
                ENDDO  ! end do NSPMD

                LSKYFI = NN2*MULTIMAX
C   nsnr nouveau utile pour inacti
                NSNR = NN2
              ENDIF
C
C Deallocation de SHARED_REAL SHARED_INT
C
              IF(ALLOCATED(XREM)) DEALLOCATE(XREM)
              IF(ALLOCATED(IREM)) DEALLOCATE(IREM)

C
              IF(INTTH == 0 ) THEN
C
C Allocation Parith/OFF
C
                IF(IPARIT==0) THEN

                  IF(ASSOCIATED(AFI(NIN)%P)) THEN
                    DEALLOCATE(AFI(NIN)%P)
                    NULLIFY(AFI(NIN)%P)
                  ENDIF
                  IF(ASSOCIATED(STNFI(NIN)%P)) THEN
                    DEALLOCATE(STNFI(NIN)%P)
                    NULLIFY(AFI(NIN)%P)
                  ENDIF

                  IF(NODFI>0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD))
                  IF(NODFI>0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD))
C Init a 0
                  DO I = 1, NODFI*NTHREAD
                    AFI(NIN)%P(1,I) = ZERO
                    AFI(NIN)%P(2,I) = ZERO
                    AFI(NIN)%P(3,I) = ZERO
                    STNFI(NIN)%P(I) = ZERO
                  ENDDO
C
                  IF(KDTINT/=0)THEN
                    IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
                    IF(NODFI>0)ALLOCATE(VSCFI(NIN)%P(NODFI*NTHREAD))
C Init a 0
                    DO I = 1, NODFI*NTHREAD
                      VSCFI(NIN)%P(I) = ZERO
                    ENDDO
                  ENDIF
                  NLSKYFI(NIN) = NODFI
C
                ELSE
C
C Allocation Parith/ON Done in upgrade_rem_slv
C
                ENDIF
              ELSE
C
C Allocation Parith/OFF
C
                IF(IPARIT==0) THEN
                  IF(ASSOCIATED(AFI(NIN)%P)) DEALLOCATE(AFI(NIN)%P)
                  IF(ASSOCIATED(STNFI(NIN)%P)) DEALLOCATE(STNFI(NIN)%P)
                  IF(ASSOCIATED(FTHEFI(NIN)%P)) DEALLOCATE(FTHEFI(NIN)%P)
                  IF(NODFI>0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD))
                  IF(NODFI>0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD))
                  IF(NODFI>0)ALLOCATE(FTHEFI(NIN)%P(NODFI*NTHREAD))
C
                  IF(NODADT_THERM ==1) THEN
                    IF(ASSOCIATED(CONDNFI(NIN)%P)) DEALLOCATE(CONDNFI(NIN)%P)
                    IF(NODFI>0.AND.NODADT_THERM ==1)ALLOCATE(CONDNFI(NIN)%P(NODFI*NTHREAD))
                  ENDIF
C


C Init a 0

                  DO I = 1, NODFI*NTHREAD
                    AFI(NIN)%P(1,I) = ZERO
                    AFI(NIN)%P(2,I) = ZERO
                    AFI(NIN)%P(3,I) = ZERO
                    STNFI(NIN)%P(I) = ZERO
                    FTHEFI(NIN)%P(I) = ZERO
                  ENDDO
                  IF(NODADT_THERM ==1) THEN
                    DO I = 1, NODFI
                      CONDNFI(NIN)%P(I) = ZERO
                    ENDDO
                  ENDIF
C
                  IF(KDTINT/=0)THEN
                    IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
                    IF(NODFI>0)ALLOCATE(VSCFI(NIN)%P(NODFI))
                    DO I = 1, NODFI
                      VSCFI(NIN)%P(I) = ZERO
                    ENDDO
                  ENDIF
                ELSE
                ENDIF
              ENDIF
C
C allocations conditionnelles output pression
C
              IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT >0)THEN
                IF(ASSOCIATED(FNCONTI(NIN)%P)) DEALLOCATE(FNCONTI(NIN)%P)
                IF(ASSOCIATED(FTCONTI(NIN)%P)) DEALLOCATE(FTCONTI(NIN)%P)
                ALLOCATE(FNCONTI(NIN)%P(3,NODFI))
                ALLOCATE(FTCONTI(NIN)%P(3,NODFI))
                DO J = 1, NODFI
                  FNCONTI(NIN)%P(1,J)=ZERO
                  FNCONTI(NIN)%P(2,J)=ZERO
                  FNCONTI(NIN)%P(3,J)=ZERO
                  FTCONTI(NIN)%P(1,J)=ZERO
                  FTCONTI(NIN)%P(2,J)=ZERO
                  FTCONTI(NIN)%P(3,J)=ZERO
                END DO
              END IF

              IF(H3D_DATA%N_SCAL_CSE_FRICINT >0)THEN
                IF(H3D_DATA%N_CSE_FRIC_INTER (NIN) >0)THEN
                  IF(ASSOCIATED(EFRICFI(NIN)%P)) DEALLOCATE(EFRICFI(NIN)%P)
                  ALLOCATE(EFRICFI(NIN)%P(NODFI))
                   DO J = 1, NODFI
                     EFRICFI(NIN)%P(J)=ZERO
                   END DO
                END IF
              ENDIF
              IF(H3D_DATA%N_SCAL_CSE_FRIC >0)THEN
                IF(ASSOCIATED(EFRICGFI(NIN)%P)) DEALLOCATE(EFRICGFI(NIN)%P)
                ALLOCATE(EFRICGFI(NIN)%P(NODFI))
                  DO J = 1, NODFI
                    EFRICGFI(NIN)%P(J)=ZERO
                  END DO
              END IF
C
C
C Renumerotation des candidats
C
              DO I = 1, I_STOK
                N = CAND_N(I)
                NN = N-NSN
                IF(NN>0)THEN
                  CAND_N(I) = INDEX(NN)+NSN
                ENDIF
              ENDDO
              DEALLOCATE(INDEX)
              DEALLOCATE(NEEDED_NODE)
C
#endif
              RETURN
            END SUBROUTINE
