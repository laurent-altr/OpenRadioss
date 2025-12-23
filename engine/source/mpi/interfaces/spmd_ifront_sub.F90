!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      module SPMD_IFRONT_SUB_MOD

        contains
      SUBROUTINE SPMD_IFRONT_SUB(NSPMD,IPARI, NPARI,NINTER, NEWFRONT, NBINTC  ,INTLIST ,INTBUF_TAB,&
        & NISUBSFI,NISUBSFIE, LISUBSFI,ADDSUBSFI,INFLG_SUBSFI,ADDSUBSFIE, LISUBSFIE,INFLG_SUBSFIE,&
        & NSNSI,NSVSI,NSNFI,NSNSIE,NSVSIE,NSNFIE,IT_SPMD,ISCOMS,IRCOM,ISCOM)
        use SPMD_MOD
        USE INTBUFDEF_MOD
        USE POINTERDEF
        implicit none
        INTEGER, intent(in) ::  NBINTC                                               
        INTEGER, intent(in) :: NSPMD
        integer, intent(in) :: NINTER
        integer, intent(in) :: NPARI
        INTEGER ::      IPARI(NPARI,NINTER)
        INTEGER ::      NEWFRONT(*), INTLIST(*)
        TYPE(int_pointer) :: NSNSI(NINTER)
        TYPE(int_pointer) :: NSVSI(NINTER)
        TYPE(int_pointer) :: NSNFI(NINTER)
        TYPE(int_pointer) :: NSNSIE(NINTER)
        TYPE(int_pointer) :: NSVSIE(NINTER)
        TYPE(int_pointer) :: NSNFIE(NINTER)
        INTEGER :: IT_SPMD(NSPMD)
        INTEGER :: ISCOMS(NSPMD)
        INTEGER :: IRCOM(NSPMD)
        INTEGER :: ISCOM(NSPMD)
        TYPE(INTBUF_STRUCT_) INTBUF_TAB(NINTER)
        TYPE(int_pointer) :: NISUBSFI(NINTER)
        TYPE(int_pointer) :: NISUBSFIE(NINTER)
        TYPE(int_pointer) :: LISUBSFI(NINTER)
        TYPE(int_pointer) :: ADDSUBSFI(NINTER)
        TYPE(int_pointer) :: INFLG_SUBSFI(NINTER)
        TYPE(int_pointer) :: ADDSUBSFIE(NINTER)
        TYPE(int_pointer) :: LISUBSFIE(NINTER)
        TYPE(int_pointer) :: INFLG_SUBSFIE(NINTER)

 

        INTEGER :: ISUBTMP(NINTER,2,NSPMD),ISUBTMP2(NINTER,2,NSPMD)
        INTEGER :: IBEGIN(NSPMD),IBEGIN_EDGE(NSPMD)
        INTEGER :: LEN, IDEB,LENOUT,P,NIN,II,NS,J,I0,I0_EDGE,I
        INTEGER :: LENOUT_EDGE
        INTEGER :: IDEB2,IDEB3,IDEB_EDGE
        INTEGER :: NB_SUBINT
        INTEGER:: REQ_S(NSPMD),REQ_EDGE_S(NSPMD)

        TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: MSGBUF_S, MSGBUF_R

        TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: MSGBUF_EDGE_S, MSGBUF_EDGE_R


        INTEGER, PARAMETER :: MSGOFF3 = 1011
        INTEGER, PARAMETER :: MSGOFF4 = 1012
        INTEGER :: MSGTYP,SIZ
 
! interface with sub-interface output
!
! Calculation and sending of the size of sub-interface parts on the border part
      ALLOCATE(MSGBUF_R(NSPMD))
      ALLOCATE(MSGBUF_S(NSPMD))
      ALLOCATE(MSGBUF_EDGE_R(NSPMD))
      ALLOCATE(MSGBUF_EDGE_S(NSPMD))
      REQ_S = SPMD_REQUEST_NULL
      REQ_EDGE_S = SPMD_REQUEST_NULL


      DO P = 1, NSPMD
        IF(IRCOM(P)>0) THEN
          DO II = 1, NBINTC
            I = INTLIST(II)
            ISUBTMP(I,1,P) = 0
            ISUBTMP(I,2,P) = 0
          END DO
        END IF
      END DO
      DO II = 1, NBINTC
        NIN = INTLIST(II)
! interface stripped with sub-interfaces ?
        IF(NEWFRONT(NIN)==2.AND.IPARI(36,NIN)>0.AND.IPARI(7,NIN)/=17) THEN
          IDEB = 0
          DO P = 1, NSPMD
            LEN = NSNSI(NIN)%P(P)
            LENOUT = 0
            IF(LEN>0) THEN
              DO I = 1, LEN
                NS = NSVSI(NIN)%P(IDEB+I) !NS is the node shared by current processor to the other that need it.
! addition of +1 to send the number of sub-interfaces per node
                LENOUT = LENOUT + INTBUF_TAB(NIN)%ADDSUBS(NS+1)-INTBUF_TAB(NIN)%ADDSUBS(NS) + 1
              END DO
              IDEB = IDEB + LEN
            END IF
            ISUBTMP(NIN,1,P) = LENOUT
          ENDDO
          IF(IPARI(7,NIN) ==25 .AND. IPARI(58,NIN) > 0) THEN
            IDEB = 0
            DO P=1,NSPMD
!              Partie Eedge
              LEN = NSNSIE(NIN)%P(P)
              LENOUT_EDGE = 0
              IF(LEN>0) THEN
                DO I = 1, LEN
                  NS = NSVSIE(NIN)%P(IDEB+I) ! NS is the edge id, shared by current processor to the other that need it.
! addition of +1 to send the number of sub-interfaces per node
                  LENOUT_EDGE = LENOUT_EDGE + INTBUF_TAB(NIN)%ADDSUBE(NS+1)-INTBUF_TAB(NIN)%ADDSUBE(NS) + 1
                END DO
                IDEB = IDEB + LEN
              END IF
!               WRITE(6,*) "ISUBTMP(",P-1,")=",LENOUT
              ISUBTMP(NIN,2,P) = LENOUT_EDGE
            END DO ! NSPMD
          ENDIF ! type 25 E2E
        END IF ! NEWFRONT
      END DO ! NBINTC
!
      DO P = 1, NSPMD
        IF(IRCOM(P)>0) THEN
          LENOUT = 0
          LENOUT_EDGE = 0
          DO II = 1, NBINTC
            NIN = INTLIST(II)
            LENOUT = LENOUT + ISUBTMP(NIN,1,P)
            IF(NEWFRONT(NIN)==2.AND.IPARI(36,NIN)>0.AND.&
            &(IPARI(7,NIN) == 7.OR.IPARI(7,NIN) == 11.OR.IPARI(7,NIN) == 24.OR.IPARI(7,NIN) == 25)) THEN
              LENOUT = LENOUT + ISUBTMP(NIN,1,P) - NSNSI(NIN)%P(P)
            ENDIF
            IF(NEWFRONT(NIN)==2.AND.IPARI(36,NIN)>0.AND.IPARI(7,NIN)==25) THEN
              IF(IPARI(58,NIN) /= 0) THEN
                LENOUT_EDGE = LENOUT_EDGE + 2*ISUBTMP(NIN,2,P) - NSNSIE(NIN)%P(P)
              ENDIF
            ENDIF
          END DO
! Comm Save length in IRCOM
          IRCOM(P) = LENOUT+LENOUT_EDGE
          IF(IRCOM(P)>0) THEN
! allocate communication structure
            ALLOCATE(MSGBUF_S(P)%P(LENOUT))
            ALLOCATE(MSGBUF_EDGE_S(P)%P(LENOUT_EDGE))
            MSGTYP = MSGOFF3
            SIZ = NINTER * 2
            CALL SPMD_ISEND(ISUBTMP(1,1,P),SIZ,IT_SPMD(P),MSGTYP,REQ_S(P))
          END IF
        END IF
      END DO
!
! Reception of the size of sub-interface parts on the border part
!
      DO P = 1, NSPMD
        IF(ISCOMS(P)>0) THEN
          MSGTYP = MSGOFF3
          LENOUT = 0
          LENOUT_EDGE = 0
          SIZ = NINTER * 2
! received in the unused part of the isubtmp buffer
          CALL SPMD_RECV(ISUBTMP2(1,1,P),SIZ,IT_SPMD(P),MSGTYP)
          DO II = 1, NBINTC
            NIN = INTLIST(II)
! interface stripped with sub-interfaces ?
            IF(NEWFRONT(NIN)==2.AND.IPARI(36,NIN)>0.AND.IPARI(7,NIN)/=17) THEN
! subtracting the number of nodes to find the length of sub-interfaces
              NB_SUBINT = ISUBTMP2(NIN,1,P) - NSNFI(NIN)%P(P)
              NISUBSFI(NIN)%P(P) = NB_SUBINT
              LENOUT = LENOUT + ISUBTMP2(NIN,1,P)
              IF(IPARI(7,NIN) == 7.OR.IPARI(7,NIN) == 11.OR.IPARI(7,NIN) == 24.OR.IPARI(7,NIN) == 25) THEN
                LENOUT = LENOUT + NB_SUBINT
              ENDIF
              IF(IPARI(7,NIN)==25) THEN !if TYPE25 interface
                IF(IPARI(58,NIN) /= 0) THEN ! edge to edge
                  NISUBSFIE(NIN)%P(P) = ISUBTMP2(NIN,2,P) - NSNFIE(NIN)%P(P)
                  LENOUT_EDGE = LENOUT_EDGE + 2*ISUBTMP2(NIN,2,P) - NSNFIE(NIN)%P(P)
! Buffer is [ Size , I_1, I_2, ...., wize]
                ENDIF
              ENDIF
            END IF
          END DO
! Comm Save length in Iscom
          ISCOM(P) = LENOUT + LENOUT_EDGE
!         IF(LENOUT>0) THEN
            ALLOCATE(MSGBUF_R(P)%P(LENOUT))
!         END IF
!         IF(LENOUT_EDGE>0) THEN
              ALLOCATE(MSGBUF_EDGE_R(P)%P(LENOUT_EDGE))
!         END IF
        ELSE
          ISCOM(P) = 0
        END IF
      END DO
!
      DO P = 1, NSPMD
        IF(IRCOM(P)>0) THEN
          CALL SPMD_WAIT(REQ_S(P))
        END IF
      END DO
!
! Sending of sub-interface parts on the border part
!
      DO P = 1, NSPMD
        IBEGIN(P) = 0
        IBEGIN_EDGE(P) = 0
      END DO
      DO II = 1, NBINTC
        NIN = INTLIST(II)
! interface stripped with sub-interfaces ?
        IF(NEWFRONT(NIN)==2.AND.IPARI(36,NIN)>0.AND.IPARI(7,NIN)/=17) THEN
          IDEB = 0
          DO P = 1, NSPMD
            LEN = NSNSI(NIN)%P(P)
            IF(LEN>0) THEN
              I0 = IBEGIN(P)
              DO I = 1, LEN
                NS = NSVSI(NIN)%P(IDEB+I)
                I0 = I0 + 1
! retrieves the number of sub-interfaces for the node
                MSGBUF_S(P)%P(I0) = INTBUF_TAB(NIN)%ADDSUBS(NS+1)-INTBUF_TAB(NIN)%ADDSUBS(NS)
                DO J = INTBUF_TAB(NIN)%ADDSUBS(NS),INTBUF_TAB(NIN)%ADDSUBS(NS+1)-1
                  I0 = I0 + 1
                  MSGBUF_S(P)%P(I0) = INTBUF_TAB(NIN)%LISUBS(J)
                END DO
                IF(IPARI(7,NIN) == 7.OR.IPARI(7,NIN) == 11.OR.IPARI(7,NIN) == 24.OR.IPARI(7,NIN) == 25) THEN
                  DO J = INTBUF_TAB(NIN)%ADDSUBS(NS),INTBUF_TAB(NIN)%ADDSUBS(NS+1)-1
                    I0 = I0 + 1
                    MSGBUF_S(P)%P(I0) = INTBUF_TAB(NIN)%INFLG_SUBS(J)
                  END DO
                END IF
              END DO
              IBEGIN(P) = I0
              IDEB = IDEB + LEN
            END IF
          END DO
          IF(IPARI(7,NIN) == 25 .AND. IPARI(58,NIN) /= 0)THEN ! INTER/TYPE25 EDGE PART
            IDEB_EDGE = 0
            DO P = 1,NSPMD
              LEN = NSNSIE(NIN)%P(P)
              IF(LEN>0) THEN
                I0_EDGE = IBEGIN_EDGE(P)
                DO I = 1, LEN
                  NS = NSVSIE(NIN)%P(IDEB_EDGE+I)
                  I0_EDGE = I0_EDGE + 1
! retrieves the number of sub-interfaces for the node
                  MSGBUF_EDGE_S(P)%P(I0_EDGE) = INTBUF_TAB(NIN)%ADDSUBE(NS+1)-INTBUF_TAB(NIN)%ADDSUBE(NS)
                  DO J = INTBUF_TAB(NIN)%ADDSUBE(NS),INTBUF_TAB(NIN)%ADDSUBE(NS+1)-1
                    I0_EDGE = I0_EDGE + 1
                    MSGBUF_EDGE_S(P)%P(I0_EDGE) = HUGE(I0_EDGE)/2 + INTBUF_TAB(NIN)%LISUBE(J)
                  END DO
                  DO J = INTBUF_TAB(NIN)%ADDSUBE(NS),INTBUF_TAB(NIN)%ADDSUBE(NS+1)-1
                    I0_EDGE = I0_EDGE + 1
                    MSGBUF_EDGE_S(P)%P(I0_EDGE) = HUGE(I0_EDGE)/2 +  INTBUF_TAB(NIN)%INFLG_SUBE(J)
                  END DO
                END DO
                IBEGIN_EDGE(P) = I0_EDGE
                IDEB_EDGE = IDEB_EDGE + LEN
              END IF ! LEN
            END DO
          ENDIF ! IEDGE
        END IF
      END DO
!
      DO P = 1, NSPMD
! Comm Save length in IRCOM
        IF(IRCOM(P)>0) THEN
          MSGTYP = MSGOFF4
!           WRITE(6,*) "SEND",IRCOM(P) ,"TO",P-1
          I0 = SIZE(MSGBUF_S(P)%P)
          CALL SPMD_ISEND(MSGBUF_S(P)%P(1),I0,IT_SPMD(P),MSGTYP,REQ_S(P))
          I0_EDGE = SIZE(MSGBUF_EDGE_S(P)%P)
          IF(I0_EDGE >0) CALL SPMD_ISEND(MSGBUF_EDGE_S(P)%P(1),I0_EDGE,IT_SPMD(P),MSGTYP,REQ_EDGE_S(P))
        END IF
      END DO
!
! Reception of sub-interface parts on the border part
!
      DO P = 1, NSPMD
! Comm Save length in Iscom
        IF(ISCOM(P)>0) THEN
          MSGTYP = MSGOFF4
          I0 = SIZE(MSGBUF_R(P)%P)
          CALL SPMD_RECV(MSGBUF_R(P)%P(1),I0,IT_SPMD(P),MSGTYP)
          I0_EDGE = SIZE(MSGBUF_EDGE_R(P)%P)
          IF(I0_EDGE >0) CALL SPMD_RECV(MSGBUF_EDGE_R(P)%P(1),I0_EDGE,IT_SPMD(P),MSGTYP)
        END IF
      END DO
!
!
      DO P = 1, NSPMD
        IBEGIN(P) = 0
        IBEGIN_EDGE(P) = 0
      END DO
      DO II = 1, NBINTC
        NIN = INTLIST(II)
! interface stripped with sub-interfaces ?
        IF(NEWFRONT(NIN)==2.AND.IPARI(36,NIN)>0.AND.IPARI(7,NIN)/=17) THEN
          IF(ASSOCIATED(LISUBSFI(NIN)%P))DEALLOCATE(LISUBSFI(NIN)%P)
          LEN = 0
          DO P = 1, NSPMD
            LEN = LEN + NISUBSFI(NIN)%P(P)
          END DO
          IF(.TRUE.) THEN
            ALLOCATE(LISUBSFI(NIN)%P(LEN))
            IF(IPARI(7,NIN) == 7.OR.IPARI(7,NIN) == 11.OR.IPARI(7,NIN) == 24.OR.IPARI(7,NIN) == 25) THEN
              IF(ASSOCIATED(INFLG_SUBSFI(NIN)%P))DEALLOCATE(INFLG_SUBSFI(NIN)%P)
              ALLOCATE(INFLG_SUBSFI(NIN)%P(LEN))
            END IF
            LEN = 1
            IF(ASSOCIATED(ADDSUBSFI(NIN)%P))DEALLOCATE(ADDSUBSFI(NIN)%P)
            DO P = 1, NSPMD
              LEN = LEN + NSNFI(NIN)%P(P)
            END DO
            ALLOCATE(ADDSUBSFI(NIN)%P(LEN))
            IDEB  = 0
            IDEB3 = 0
            ADDSUBSFI(NIN)%P(1:LEN) = 1
            DO P = 1, NSPMD
              IF(ISCOM(P)>0) THEN
                DO I = 1, NSNFI(NIN)%P(P)
                  IDEB2 = IBEGIN(P)
                  IDEB2 = IDEB2 + 1
                  LEN = MSGBUF_R(P)%P(IDEB2)
                  ADDSUBSFI(NIN)%P(IDEB3+I+1) =ADDSUBSFI(NIN)%P(IDEB3+I) + LEN
                  DO J = 1, LEN
                    LISUBSFI(NIN)%P(IDEB+J) = MSGBUF_R(P)%P(IDEB2+J)
                  END DO
                  IBEGIN(P) = IBEGIN(P) + LEN + 1
                  IF(IPARI(7,NIN) == 7.OR.IPARI(7,NIN) == 11.OR.IPARI(7,NIN) == 24.OR.IPARI(7,NIN) == 25) THEN
                    IDEB2 = IDEB2 + LEN
                    DO J = 1, LEN
                      INFLG_SUBSFI(NIN)%P(IDEB+J) = MSGBUF_R(P)%P(IDEB2+J)
                    END DO
                    IBEGIN(P) = IBEGIN(P) + LEN
                  END IF
                  IDEB = IDEB + LEN
                END DO
                IDEB3 = IDEB3 + NSNFI(NIN)%P(P)
              ENDIF
            END DO
          ELSE

          END IF
          IF(IPARI(7,NIN) == 25 .AND. IPARI(58,NIN) > 0) THEN
            !type 25 edge part
            IF(ASSOCIATED(LISUBSFIE(NIN)%P))DEALLOCATE(LISUBSFIE(NIN)%P)
            LEN = 0
            DO P = 1, NSPMD
              LEN = LEN + NISUBSFIE(NIN)%P(P)
            END DO
!             WRITE(6,*) NIN,"SIZE LISUBSFIE=",LEN
            IF(.TRUE.) THEN
              ALLOCATE(LISUBSFIE(NIN)%P(LEN))
              IF(IPARI(7,NIN)==25)THEN
                IF(ASSOCIATED(INFLG_SUBSFIE(NIN)%P))DEALLOCATE(INFLG_SUBSFIE(NIN)%P)
                ALLOCATE(INFLG_SUBSFIE(NIN)%P(LEN))
              END IF
              LEN = 1
              IF(ASSOCIATED(ADDSUBSFIE(NIN)%P))  DEALLOCATE(ADDSUBSFIE(NIN)%P)
              DO P = 1, NSPMD
                LEN = LEN + NSNFIE(NIN)%P(P)
              END DO
              ALLOCATE(ADDSUBSFIE(NIN)%P(LEN))
!               WRITE(6,*) NIN,"ADDSUBSFIE size:",LEN
              IDEB  = 0
              IDEB3 = 0
              ADDSUBSFIE(NIN)%P(1:LEN) = 1
              DO P = 1, NSPMD
                IF(ISCOM(P)>0) THEN
                  DO I = 1, NSNFIE(NIN)%P(P)
                    IDEB2 = IBEGIN_EDGE(P)
                    IDEB2 = IDEB2 + 1
                    LEN = MSGBUF_EDGE_R(P)%P(IDEB2)
                    ADDSUBSFIE(NIN)%P(IDEB3+I+1) = ADDSUBSFIE(NIN)%P(IDEB3+I) + LEN
                    DO J = 1, LEN
                      LISUBSFIE(NIN)%P(IDEB+J) = MSGBUF_EDGE_R(P)%P(IDEB2+J) -HUGE(I0_EDGE)/2
                    END DO
                    IBEGIN_EDGE(P) = IBEGIN_EDGE(P) + LEN + 1
                    IDEB2 = IDEB2 + LEN
                    DO J = 1, LEN
                      INFLG_SUBSFIE(NIN)%P(IDEB+J) = MSGBUF_EDGE_R(P)%P(IDEB2+J) -HUGE(I0_EDGE)/2
                    END DO
                    IBEGIN_EDGE(P) = IBEGIN_EDGE(P) + LEN
                    IDEB = IDEB + LEN
                  END DO
                  IDEB3 = IDEB3 + NSNFIE(NIN)%P(P)
                ENDIF
              END DO
            ELSE

            END IF
          ENDIF ! TYPE25 EDGE PART
        END IF
      END DO
!
      DO P = 1, NSPMD
        CALL SPMD_WAIT(REQ_S(P))
        CALL SPMD_WAIT(REQ_EDGE_S(P))
      END DO

      DEALLOCATE(MSGBUF_R)
      DEALLOCATE(MSGBUF_S)
      DEALLOCATE(MSGBUF_EDGE_R)
      DEALLOCATE(MSGBUF_EDGE_S)
    


    end subroutine
    end module SPMD_IFRONT_SUB_MOD