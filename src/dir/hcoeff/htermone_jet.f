C*****************************************************
CC  TERMES QUI VIENNENT DE 5 COLLINEAIRE A 1
C*****************************************************
C PARTIE EN 1/(1-Z1)+
	SUBROUTINE VEC_15ZD(SC,TC,UC,Z1,M,PTM,CQI,CQK,VPART15Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VPART15Z(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VPART15Z(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
	EQI = CQI(1)
	EQK = CQK(1)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(1) = t0
c       qi + qk --> qk + photon
      t0 = -eqk**2*E(UC,SC,TC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2
     #)-FAGQ(z1))/CF/2.D0
	VTEMP(2) = t0
	VPART15Z(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(3) = t0
c       qi + qk --> qk + photon
      t0 = -eqk**2*E(UC,SC,TC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2
     #)-FAGQ(z1))/CF/2.D0
	VTEMP(4) = t0
	VPART15Z(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(5) = t0
c       qi + qk --> qk + photon
      t0 = -eqk**2*E(UC,SC,TC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2
     #)-FAGQ(z1))/CF/2.D0
	VTEMP(6) = t0
	VPART15Z(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*(-ANM4_GQ(z1)-A4_GQ(z1)*dlog(ptm**2/M**2
     #)+FAGQ(z1))/CF/2.D0
	VTEMP(8) = t0
	VPART15Z(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(9) = t0
c       qi + qbk --> qbk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*(-ANM4_GQ(z1)-A4_GQ(z1)*dlog(ptm**2/M**2
     #)+FAGQ(z1))/CF/2.D0
	VTEMP(10) = t0
	VPART15Z(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(11) = t0
c       qi + qbk --> qbk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*(-ANM4_GQ(z1)-A4_GQ(z1)*dlog(ptm**2/M**2
     #)+FAGQ(z1))/CF/2.D0
	VTEMP(12) = t0
	VPART15Z(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      t0 = -eqi**2*E(UC,SC,TC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2
     #)-FAGQ(z1))/CF/2.D0
	VTEMP(13) = t0
	VPART15Z(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = 0.D0
	VTEMP(14) = t0 + VTEMP(14)
c       qi + qbi --> qbk + photon
      t0 = 0.D0
	VTEMP(15) = t0 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      t0 = 0.D0
	VTEMP(16) = t0
c       qi + qbi --> qbi + photon
      t0 = -eqi**2*E(UC,SC,TC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2
     #)-FAGQ(z1))/CF/2.D0
	VTEMP(17) = t0
c       qi + qbi --> g + photon
      t0 = -eqi**2*E(SC,TC,UC,N)*(-ANM4_QQ(z1)-A4_QQ(z1)*dlog(ptm**2/M**
     #2)+FAQQ(z1))
	VTEMP(18) = t0
	VPART15Z(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = 0.D0
	VTEMP(19) = t0 + VTEMP(19)
c       qi + qbi --> qbk + photon
      t0 = 0.D0
	VTEMP(20) = t0 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      t0 = 0.D0
	VTEMP(21) = t0
c       qi + qbi --> qbi + photon
      t0 = -eqi**2*E(UC,SC,TC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2
     #)-FAGQ(z1))/CF/2.D0
	VTEMP(22) = t0
c       qi + qbi --> g + photon
      t0 = -eqi**2*E(SC,TC,UC,N)*(-ANM4_QQ(z1)-A4_QQ(z1)*dlog(ptm**2/M**
     #2)+FAQQ(z1))
	VTEMP(23) = t0
	VPART15Z(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      t0 = -eqi**2*E(TC,SC,UC,N)*(ANM4_QQ(z1)+A4_QQ(z1)*dlog(ptm**2/M**2
     #)-FAQQ(z1))
	VTEMP(24) = t0
c       qi + g --> g + photon
      t0 = 0.D0
	VTEMP(25) = t0
	VPART15Z(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      t0 = -eqi**2*E(TC,SC,UC,N)*(N-1.D0)*(N+1.D0)*(ANM4_QG(z1)+A4_QG(z1
     #)*dlog(ptm**2/M**2)-FAQG(z1))/N
	VTEMP(26) = t0 + VTEMP(26)
c       g + g --> qbi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*(N-1.D0)*(N+1.D0)*(-ANM4_QG(z1)-A4_QG(z1
     #)*dlog(ptm**2/M**2)+FAQG(z1))/N
	VTEMP(27) = t0 + VTEMP(27)
	ENDDO
	VPART15Z(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
C PARTIE EN LOG(1-Z1)/(1-Z1)+
	SUBROUTINE VEC_15LD(SC,TC,UC,Z1,CQI,CQK,VPART15L)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VPART15L(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VPART15L(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
	EQI = CQI(1)
	EQK = CQK(1)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(1) = t0
c       qi + qk --> qk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(2) = t0
	VPART15L(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(3) = t0
c       qi + qk --> qk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(4) = t0
	VPART15L(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(5) = t0
c       qi + qk --> qk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(6) = t0
	VPART15L(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(8) = t0
	VPART15L(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(9) = t0
c       qi + qbk --> qbk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(10) = t0
	VPART15L(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(11) = t0
c       qi + qbk --> qbk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(12) = t0
	VPART15L(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      t0 = eqi**2*E(UC,SC,TC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(13) = t0
	VPART15L(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = 0.D0
	VTEMP(14) = t0 + VTEMP(14)
c       qi + qbi --> qbk + photon
      t0 = 0.D0
	VTEMP(15) = t0 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      t0 = 0.D0
	VTEMP(16) = t0
c       qi + qbi --> qbi + photon
      t0 = eqi**2*E(UC,SC,TC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(17) = t0
c       qi + qbi --> g + photon
      t0 = -eqi**2*E(SC,TC,UC,N)*FBQQ(z1)
	VTEMP(18) = t0
	VPART15L(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = 0.D0
	VTEMP(19) = t0 + VTEMP(19)
c       qi + qbi --> qbk + photon
      t0 = 0.D0
	VTEMP(20) = t0 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      t0 = 0.D0
	VTEMP(21) = t0
c       qi + qbi --> qbi + photon
      t0 = eqi**2*E(UC,SC,TC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(22) = t0
c       qi + qbi --> g + photon
      t0 = -eqi**2*E(SC,TC,UC,N)*FBQQ(z1)
	VTEMP(23) = t0
	VPART15L(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FBQQ(z1)
	VTEMP(24) = t0
c       qi + g --> g + photon
      t0 = 0.D0
	VTEMP(25) = t0
	VPART15L(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FBQG(z1)*(N-1.D0)*(N+1.D0)/N
	VTEMP(26) = t0 + VTEMP(26)
c       g + g --> qbi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FBQG(z1)*(N-1.D0)*(N+1.D0)/N
	VTEMP(27) = t0 + VTEMP(27)
	ENDDO
	VPART15L(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
C PARTIE EN DELTA(1-Z1)
	SUBROUTINE VEC_15DD(SC,TC,UC,M,PTM,CQI,CQK,VPART15D)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VPART15D(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VPART15D(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
	EQI = CQI(1)
	EQK = CQK(1)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(1) = t0
c       qi + qk --> qk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(2) = t0
	VPART15D(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(3) = t0
c       qi + qk --> qk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(4) = t0
	VPART15D(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(5) = t0
c       qi + qk --> qk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(6) = t0
	VPART15D(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(8) = t0
	VPART15D(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(9) = t0
c       qi + qbk --> qbk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(10) = t0
	VPART15D(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(11) = t0
c       qi + qbk --> qbk + photon
      t0 = eqk**2*E(UC,SC,TC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(12) = t0
	VPART15D(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      t0 = eqi**2*E(UC,SC,TC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(13) = t0
	VPART15D(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = 0.D0
	VTEMP(14) = t0 + VTEMP(14)
c       qi + qbi --> qbk + photon
      t0 = 0.D0
	VTEMP(15) = t0 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      t0 = 0.D0
	VTEMP(16) = t0
c       qi + qbi --> qbi + photon
      t0 = eqi**2*E(UC,SC,TC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(17) = t0
c       qi + qbi --> g + photon
      t0 = -eqi**2*E(SC,TC,UC,N)*(-dlog(ptm**2/M**2)*B_QQ()+FCQQ(1.D0))
	VTEMP(18) = t0
	VPART15D(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = 0.D0
	VTEMP(19) = t0 + VTEMP(19)
c       qi + qbi --> qbk + photon
      t0 = 0.D0
	VTEMP(20) = t0 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      t0 = 0.D0
	VTEMP(21) = t0
c       qi + qbi --> qbi + photon
      t0 = eqi**2*E(UC,SC,TC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(22) = t0
c       qi + qbi --> g + photon
      t0 = -eqi**2*E(SC,TC,UC,N)*(-dlog(ptm**2/M**2)*B_QQ()+FCQQ(1.D0))
	VTEMP(23) = t0
	VPART15D(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      t0 = -eqi**2*E(TC,SC,UC,N)*(B_QQ()*dlog(ptm**2/M**2)-FCQQ(1.D0))
	VTEMP(24) = t0
c       qi + g --> g + photon
      t0 = 0.D0
	VTEMP(25) = t0
	VPART15D(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FCQG(1.D0)*(N-1.D0)*(N+1.D0)/N
	VTEMP(26) = t0 + VTEMP(26)
c       g + g --> qbi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FCQG(1.D0)*(N-1.D0)*(N+1.D0)/N
	VTEMP(27) = t0 + VTEMP(27)
	ENDDO
	VPART15D(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
C*****************************************************
CC  TERMES QUI VIENNENT DE 5 COLLINEAIRE A 2
C*****************************************************
C PARTIE EN 1/(1-Z2)+
	SUBROUTINE VEC_25ZD(SC,TC,UC,Z2,M,PTM,CQI,CQK,VPART25Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VPART25Z(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VPART25Z(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
	EQI = CQI(1)
	EQK = CQK(1)
c       qi + qk --> qi + photon
      t0 = -eqi**2*E(TC,SC,UC,N)*(ANM4_GQ(z2)+A4_GQ(z2)*dlog(ptm**2/M**2
     #)-FAGQ(z2))/CF/2.D0
	VTEMP(1) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(2) = t0
	VPART25Z(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      t0 = -eqi**2*E(TC,SC,UC,N)*(ANM4_GQ(z2)+A4_GQ(z2)*dlog(ptm**2/M**2
     #)-FAGQ(z2))/CF/2.D0
	VTEMP(3) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(4) = t0
	VPART25Z(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      t0 = -eqi**2*E(TC,SC,UC,N)*(ANM4_GQ(z2)+A4_GQ(z2)*dlog(ptm**2/M**2
     #)-FAGQ(z2))/CF/2.D0
	VTEMP(5) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(6) = t0
	VPART25Z(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      t0 = -eqi**2*E(TC,SC,UC,N)*(ANM4_GQ(z2)+A4_GQ(z2)*dlog(ptm**2/M**2
     #)-FAGQ(z2))/CF/2.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(8) = t0
	VPART25Z(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      t0 = -eqi**2*E(TC,SC,UC,N)*(ANM4_GQ(z2)+A4_GQ(z2)*dlog(ptm**2/M**2
     #)-FAGQ(z2))/CF/2.D0
	VTEMP(9) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(10) = t0
	VPART25Z(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      t0 = -eqi**2*E(TC,SC,UC,N)*(ANM4_GQ(z2)+A4_GQ(z2)*dlog(ptm**2/M**2
     #)-FAGQ(z2))/CF/2.D0
	VTEMP(11) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(12) = t0
	VPART25Z(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      t0 = -eqi**2*E(TC,SC,UC,N)*(ANM4_GQ(z2)+A4_GQ(z2)*dlog(ptm**2/M**2
     #)-FAGQ(z2))/CF/2.D0
	VTEMP(13) = t0
	VPART25Z(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = 0.D0
	VTEMP(14) = t0 + VTEMP(14)
c       qi + qbi --> qbk + photon
      t0 = 0.D0
	VTEMP(15) = t0 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*(-ANM4_GQ(z2)-A4_GQ(z2)*dlog(ptm**2/M**2
     #)+FAGQ(z2))/CF/2.D0
	VTEMP(16) = t0
c       qi + qbi --> qbi + photon
      t0 = 0.D0
	VTEMP(17) = t0
c       qi + qbi --> g + photon
      t0 = eqi**2*E(SC,TC,UC,N)*(ANM4_QQ(z2)+A4_QQ(z2)*dlog(ptm**2/M**2)
     #-FAQQ(z2))
	VTEMP(18) = t0
	VPART25Z(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = 0.D0
	VTEMP(19) = t0 + VTEMP(19)
c       qi + qbi --> qbk + photon
      t0 = 0.D0
	VTEMP(20) = t0 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*(-ANM4_GQ(z2)-A4_GQ(z2)*dlog(ptm**2/M**2
     #)+FAGQ(z2))/CF/2.D0
	VTEMP(21) = t0
c       qi + qbi --> qbi + photon
      t0 = 0.D0
	VTEMP(22) = t0
c       qi + qbi --> g + photon
      t0 = eqi**2*E(SC,TC,UC,N)*(ANM4_QQ(z2)+A4_QQ(z2)*dlog(ptm**2/M**2)
     #-FAQQ(z2))
	VTEMP(23) = t0
	VPART25Z(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*(-ANM4_GG(z2)-A4_GG(z2)*dlog(ptm**2/M**2
     #)+FAGG(z2))
	VTEMP(24) = t0
c       qi + g --> g + photon
      t0 = eqi**2*E(SC,TC,UC,N)*(N-1.D0)*(N+1.D0)*(ANM4_QG(z2)+A4_QG(z2)
     #*dlog(ptm**2/M**2)-FAQG(z2))/N
	VTEMP(25) = t0
	VPART25Z(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      t0 = -eqi**2*E(UC,SC,TC,N)*(N-1.D0)*(N+1.D0)*(ANM4_QG(z2)+A4_QG(z2
     #)*dlog(ptm**2/M**2)-FAQG(z2))/N
	VTEMP(26) = t0 + VTEMP(26)
c       g + g --> qbi + photon
      t0 = -eqi**2*E(UC,SC,TC,N)*(N-1.D0)*(N+1.D0)*(ANM4_QG(z2)+A4_QG(z2
     #)*dlog(ptm**2/M**2)-FAQG(z2))/N
	VTEMP(27) = t0 + VTEMP(27)
	ENDDO
	VPART25Z(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
C PARTIE EN LOG(1-Z2)/(1-Z2)+
	SUBROUTINE VEC_25LD(SC,TC,UC,Z2,CQI,CQK,VPART25L)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VPART25L(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VPART25L(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
	EQI = CQI(1)
	EQK = CQK(1)
c       qi + qk --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FBGQ(z2)/CF/2.D0
	VTEMP(1) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(2) = t0
	VPART25L(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FBGQ(z2)/CF/2.D0
	VTEMP(3) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(4) = t0
	VPART25L(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FBGQ(z2)/CF/2.D0
	VTEMP(5) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(6) = t0
	VPART25L(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FBGQ(z2)/CF/2.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(8) = t0
	VPART25L(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FBGQ(z2)/CF/2.D0
	VTEMP(9) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(10) = t0
	VPART25L(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FBGQ(z2)/CF/2.D0
	VTEMP(11) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(12) = t0
	VPART25L(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FBGQ(z2)/CF/2.D0
	VTEMP(13) = t0
	VPART25L(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = 0.D0
	VTEMP(14) = t0 + VTEMP(14)
c       qi + qbi --> qbk + photon
      t0 = 0.D0
	VTEMP(15) = t0 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FBGQ(z2)/CF/2.D0
	VTEMP(16) = t0
c       qi + qbi --> qbi + photon
      t0 = 0.D0
	VTEMP(17) = t0
c       qi + qbi --> g + photon
      t0 = -eqi**2*E(SC,TC,UC,N)*FBQQ(z2)
	VTEMP(18) = t0
	VPART25L(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = 0.D0
	VTEMP(19) = t0 + VTEMP(19)
c       qi + qbi --> qbk + photon
      t0 = 0.D0
	VTEMP(20) = t0 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FBGQ(z2)/CF/2.D0
	VTEMP(21) = t0
c       qi + qbi --> qbi + photon
      t0 = 0.D0
	VTEMP(22) = t0
c       qi + qbi --> g + photon
      t0 = -eqi**2*E(SC,TC,UC,N)*FBQQ(z2)
	VTEMP(23) = t0
	VPART25L(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FBGG(z2)
	VTEMP(24) = t0
c       qi + g --> g + photon
      t0 = -eqi**2*E(SC,TC,UC,N)*FBQG(z2)*(N-1.D0)*(N+1.D0)/N
	VTEMP(25) = t0
	VPART25L(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      t0 = eqi**2*E(UC,SC,TC,N)*FBQG(z2)*(N-1.D0)*(N+1.D0)/N
	VTEMP(26) = t0 + VTEMP(26)
c       g + g --> qbi + photon
      t0 = eqi**2*E(UC,SC,TC,N)*FBQG(z2)*(N-1.D0)*(N+1.D0)/N
	VTEMP(27) = t0 + VTEMP(27)
	ENDDO
	VPART25L(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
C PARTIE EN DELTA(1-Z2)
	SUBROUTINE VEC_25DD(SC,TC,UC,M,PTM,CQI,CQK,VPART25D)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VPART25D(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VPART25D(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
	EQI = CQI(1)
	EQK = CQK(1)
c       qi + qk --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(1) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(2) = t0
	VPART25D(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(3) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(4) = t0
	VPART25D(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(5) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(6) = t0
	VPART25D(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(8) = t0
	VPART25D(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(9) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(10) = t0
	VPART25D(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(11) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(12) = t0
	VPART25D(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(13) = t0
	VPART25D(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = 0.D0
	VTEMP(14) = t0 + VTEMP(14)
c       qi + qbi --> qbk + photon
      t0 = 0.D0
	VTEMP(15) = t0 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(16) = t0
c       qi + qbi --> qbi + photon
      t0 = 0.D0
	VTEMP(17) = t0
c       qi + qbi --> g + photon
      t0 = -eqi**2*E(SC,TC,UC,N)*(-dlog(ptm**2/M**2)*B_QQ()+FCQQ(1.D0))
	VTEMP(18) = t0
	VPART25D(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = 0.D0
	VTEMP(19) = t0 + VTEMP(19)
c       qi + qbi --> qbk + photon
      t0 = 0.D0
	VTEMP(20) = t0 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(21) = t0
c       qi + qbi --> qbi + photon
      t0 = 0.D0
	VTEMP(22) = t0
c       qi + qbi --> g + photon
      t0 = -eqi**2*E(SC,TC,UC,N)*(-dlog(ptm**2/M**2)*B_QQ()+FCQQ(1.D0))
	VTEMP(23) = t0
	VPART25D(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      t0 = eqi**2*E(TC,SC,UC,N)*(-dlog(ptm**2/M**2)*B_GG()+FCGG(1.D0))
	VTEMP(24) = t0
c       qi + g --> g + photon
      t0 = -eqi**2*E(SC,TC,UC,N)*FCQG(1.D0)*(N-1.D0)*(N+1.D0)/N
	VTEMP(25) = t0
	VPART25D(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      t0 = eqi**2*E(UC,SC,TC,N)*FCQG(1.D0)*(N-1.D0)*(N+1.D0)/N
	VTEMP(26) = t0 + VTEMP(26)
c       g + g --> qbi + photon
      t0 = eqi**2*E(UC,SC,TC,N)*FCQG(1.D0)*(N-1.D0)*(N+1.D0)/N
	VTEMP(27) = t0 + VTEMP(27)
	ENDDO
	VPART25D(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
C*****************************************************
CC  TERMES QUI VIENNENT DE 5 COLLINEAIRE A 3
C*****************************************************
C PARTIE EN LOG(R**2)
	SUBROUTINE VEC_35RD(SC,TC,UC,ZM,R,CQI,CQK,VPART35R)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VPART35R(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VPART35R(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
	EQI = CQI(1)
	EQK = CQK(1)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(1) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(2) = t0
	VPART35R(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(3) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(4) = t0
	VPART35R(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(5) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(6) = t0
	VPART35R(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(8) = t0
	VPART35R(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(9) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(10) = t0
	VPART35R(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(11) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(12) = t0
	VPART35R(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      t0 = 0.D0
	VTEMP(13) = t0
	VPART35R(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = (zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*eqi**2*E
     #(SC,TC,UC,N)
	VTEMP(14) = t0 + VTEMP(14)
c       qi + qbi --> qbk + photon
      t0 = (zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*eqi**2*E
     #(SC,TC,UC,N)
	VTEMP(15) = t0 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      t0 = (zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*eqi**2*E
     #(SC,TC,UC,N)
	VTEMP(16) = t0
c       qi + qbi --> qbi + photon
      t0 = (zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*eqi**2*E
     #(SC,TC,UC,N)
	VTEMP(17) = t0
c       qi + qbi --> g + photon
      t0 = N*(-2.D0*dlog(1.D0-zm)+2.D0*dlog(zm)-4.D0*zm+zm**2-2.D0/3.D0*
     #zm**3+11.D0/6.D0)*dlog(R**2)*eqi**2*E(SC,TC,UC,N)
	VTEMP(18) = t0
	VPART35R(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = (zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*eqi**2*E
     #(SC,TC,UC,N)
	VTEMP(19) = t0 + VTEMP(19)
c       qi + qbi --> qbk + photon
      t0 = (zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*eqi**2*E
     #(SC,TC,UC,N)
	VTEMP(20) = t0 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      t0 = (zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*eqi**2*E
     #(SC,TC,UC,N)
	VTEMP(21) = t0
c       qi + qbi --> qbi + photon
      t0 = (zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*eqi**2*E
     #(SC,TC,UC,N)
	VTEMP(22) = t0
c       qi + qbi --> g + photon
      t0 = N*(-2.D0*dlog(1.D0-zm)+2.D0*dlog(zm)-4.D0*zm+zm**2-2.D0/3.D0*
     #zm**3+11.D0/6.D0)*dlog(R**2)*eqi**2*E(SC,TC,UC,N)
	VTEMP(23) = t0
	VPART35R(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      t0 = -(N**2-1.D0)/N*(5.D0/8.D0-2.D0*dlog(2.D0)-2.D0*dlog(1.D0-zm)-
     #zm-zm**2/2.D0)*dlog(R**2)*eqi**2*E(TC,SC,UC,N)/2.D0
	VTEMP(24) = t0
c       qi + g --> g + photon
      t0 = -(N**2-1.D0)/N*(7.D0/8.D0+2.D0*dlog(2.D0)+2.D0*dlog(zm)-2.D0*
     #zm+zm**2/2.D0)*dlog(R**2)*eqi**2*E(TC,SC,UC,N)/2.D0
	VTEMP(25) = t0
	VPART35R(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      t0 = 0.D0
	VTEMP(26) = t0 + VTEMP(26)
c       g + g --> qbi + photon
      t0 = 0.D0
	VTEMP(27) = t0 + VTEMP(27)
	ENDDO
	VPART35R(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
C PARTIE EN DELTA(1-Z3)
	SUBROUTINE VEC_35DD(SC,TC,UC,CQI,CQK,VPART35D)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VPART35D(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VPART35D(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
	EQI = CQI(1)
	EQK = CQK(1)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(1) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(2) = t0
	VPART35D(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(3) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(4) = t0
	VPART35D(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      t0 = 0.D0
	VTEMP(5) = t0
c       qi + qk --> qk + photon
      t0 = 0.D0
	VTEMP(6) = t0
	VPART35D(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(8) = t0
	VPART35D(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(9) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(10) = t0
	VPART35D(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      t0 = 0.D0
	VTEMP(11) = t0
c       qi + qbk --> qbk + photon
      t0 = 0.D0
	VTEMP(12) = t0
	VPART35D(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      t0 = 0.D0
	VTEMP(13) = t0
	VPART35D(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = -23.D0/36.D0*eqi**2*E(SC,TC,UC,N)
	VTEMP(14) = t0 + VTEMP(14)
c       qi + qbi --> qbk + photon
      t0 = -23.D0/36.D0*eqi**2*E(SC,TC,UC,N)
	VTEMP(15) = t0 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      t0 = -23.D0/36.D0*eqi**2*E(SC,TC,UC,N)
	VTEMP(16) = t0
c       qi + qbi --> qbi + photon
      t0 = -23.D0/36.D0*eqi**2*E(SC,TC,UC,N)
	VTEMP(17) = t0
c       qi + qbi --> g + photon
      t0 = N*(67.D0/9.D0-2.D0/3.D0*0.3141592653589793D1**2)*eqi**2*E(SC,
     #TC,UC,N)
	VTEMP(18) = t0
	VPART35D(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = -23.D0/36.D0*eqi**2*E(SC,TC,UC,N)
	VTEMP(19) = t0 + VTEMP(19)
c       qi + qbi --> qbk + photon
      t0 = -23.D0/36.D0*eqi**2*E(SC,TC,UC,N)
	VTEMP(20) = t0 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      t0 = -23.D0/36.D0*eqi**2*E(SC,TC,UC,N)
	VTEMP(21) = t0
c       qi + qbi --> qbi + photon
      t0 = -23.D0/36.D0*eqi**2*E(SC,TC,UC,N)
	VTEMP(22) = t0
c       qi + qbi --> g + photon
      t0 = N*(67.D0/9.D0-2.D0/3.D0*0.3141592653589793D1**2)*eqi**2*E(SC,
     #TC,UC,N)
	VTEMP(23) = t0
	VPART35D(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      t0 = -(N**2-1.D0)/N*(27.D0/8.D0-0.3141592653589793D1**2/3.D0+4.D0*
     #dlog(2.D0)**2.D0+dlog(2.D0)/2.D0)*eqi**2*E(TC,SC,UC,N)/2.D0
	VTEMP(24) = t0
c       qi + g --> g + photon
      t0 = -(N**2-1.D0)/N*(25.D0/8.D0-0.3141592653589793D1**2/3.D0-4.D0*
     #dlog(2.D0)**2.D0-dlog(2.D0)/2.D0)*eqi**2*E(TC,SC,UC,N)/2.D0
	VTEMP(25) = t0
	VPART35D(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      t0 = 0.D0
	VTEMP(26) = t0 + VTEMP(26)
c       g + g --> qbi + photon
      t0 = 0.D0
	VTEMP(27) = t0 + VTEMP(27)
	ENDDO
	VPART35D(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
C*****************************************************
CC  TERMES QUI VIENNENT DE 5 COLLINEAIRE A 4
C*****************************************************
C PARTIE EN LOG(R**2)
	SUBROUTINE VEC_45RD(SC,TC,UC,Z4,R,CQI,CQK,VPART45R)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VPART45R(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VPART45R(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
	EQI = CQI(1)
	EQK = CQK(1)
c       qi + qk --> qi + photon
      t0 = eqk**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(SC,TC,UC,N)
	VTEMP(1) = t0
c       qi + qk --> qk + photon
      t0 = eqi**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(SC,UC,TC,N)
	VTEMP(2) = t0
	VPART45R(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      t0 = eqk**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(SC,TC,UC,N)
	VTEMP(3) = t0
c       qi + qk --> qk + photon
      t0 = eqi**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(SC,UC,TC,N)
	VTEMP(4) = t0
	VPART45R(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      t0 = eqk**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(SC,TC,UC,N)
	VTEMP(5) = t0
c       qi + qk --> qk + photon
      t0 = eqi**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(SC,UC,TC,N)
	VTEMP(6) = t0
	VPART45R(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      t0 = eqk**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(UC,TC,SC,N)
	VTEMP(7) = t0
c       qi + qbk --> qbk + photon
      t0 = eqi**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(TC,UC,SC,N)
	VTEMP(8) = t0
	VPART45R(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      t0 = eqk**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(UC,TC,SC,N)
	VTEMP(9) = t0
c       qi + qbk --> qbk + photon
      t0 = eqi**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(TC,UC,SC,N)
	VTEMP(10) = t0
	VPART45R(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      t0 = eqk**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(UC,TC,SC,N)
	VTEMP(11) = t0
c       qi + qbk --> qbk + photon
      t0 = eqi**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(TC,UC,SC,N)
	VTEMP(12) = t0
	VPART45R(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      t0 = eqi**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*(A(SC,TC,UC,N)+A(SC,UC,
     #TC,N)+B(SC,TC,UC,N))
	VTEMP(13) = t0
	VPART45R(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = eqk**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(UC,SC,TC,N)
	VTEMP(14) = t0 + VTEMP(14)
c       qi + qbi --> qbk + photon
      t0 = eqk**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(TC,SC,UC,N)
	VTEMP(15) = t0 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      t0 = eqi**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*(A(UC,TC,SC,N)+A(UC,SC,
     #TC,N)+B(UC,TC,SC,N))
	VTEMP(16) = t0
c       qi + qbi --> qbi + photon
      t0 = eqi**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*(A(TC,UC,SC,N)+A(TC,SC,
     #UC,N)+B(TC,UC,SC,N))
	VTEMP(17) = t0
c       qi + qbi --> g + photon
      t0 = 0.D0
	VTEMP(18) = t0
	VPART45R(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = eqk**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(UC,SC,TC,N)
	VTEMP(19) = t0 + VTEMP(19)
c       qi + qbi --> qbk + photon
      t0 = eqk**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*A(TC,SC,UC,N)
	VTEMP(20) = t0 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      t0 = eqi**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*(A(UC,TC,SC,N)+A(UC,SC,
     #TC,N)+B(UC,TC,SC,N))
	VTEMP(21) = t0
c       qi + qbi --> qbi + photon
      t0 = eqi**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*(A(TC,UC,SC,N)+A(TC,SC,
     #UC,N)+B(TC,UC,SC,N))
	VTEMP(22) = t0
c       qi + qbi --> g + photon
      t0 = 0.D0
	VTEMP(23) = t0
	VPART45R(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      t0 = 0.D0
	VTEMP(24) = t0
c       qi + g --> g + photon
      t0 = -eqi**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*C(UC,SC,TC,N)
	VTEMP(25) = t0
	VPART45R(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      t0 = eqi**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*C(SC,TC,UC,N)
	VTEMP(26) = t0 + VTEMP(26)
c       g + g --> qbi + photon
      t0 = eqi**2*A4_PQ(z4)*dlog(R**2)/(1.D0-z4)*C(SC,TC,UC,N)
	VTEMP(27) = t0 + VTEMP(27)
	ENDDO
	VPART45R(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
C PARTIE EN 1/(1-Z4)+
	SUBROUTINE VEC_45ZD(SC,TC,UC,Z4,MF,PT4,CQI,CQK,VPART45Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VPART45Z(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VPART45Z(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
	EQI = CQI(1)
	EQK = CQK(1)
c       qi + qk --> qi + photon
      t0 = eqk**2*A(SC,TC,UC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(1) = t0
c       qi + qk --> qk + photon
      t0 = eqi**2*A(SC,UC,TC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(2) = t0
	VPART45Z(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      t0 = eqk**2*A(SC,TC,UC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(3) = t0
c       qi + qk --> qk + photon
      t0 = eqi**2*A(SC,UC,TC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(4) = t0
	VPART45Z(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      t0 = eqk**2*A(SC,TC,UC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(5) = t0
c       qi + qk --> qk + photon
      t0 = eqi**2*A(SC,UC,TC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(6) = t0
	VPART45Z(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      t0 = eqk**2*A(UC,TC,SC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(7) = t0
c       qi + qbk --> qbk + photon
      t0 = eqi**2*A(TC,UC,SC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(8) = t0
	VPART45Z(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      t0 = eqk**2*A(UC,TC,SC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(9) = t0
c       qi + qbk --> qbk + photon
      t0 = eqi**2*A(TC,UC,SC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(10) = t0
	VPART45Z(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      t0 = eqk**2*A(UC,TC,SC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(11) = t0
c       qi + qbk --> qbk + photon
      t0 = eqi**2*A(TC,UC,SC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(12) = t0
	VPART45Z(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      t0 = -eqi**2*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))*(-ANM4_PQ
     #(z4)+DAPQ(z4)-dlog(pt4**2/Mf**2)*A4_PQ(z4))
	VTEMP(13) = t0
	VPART45Z(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = eqk**2*A(UC,SC,TC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(14) = t0 + VTEMP(14)
c       qi + qbi --> qbk + photon
      t0 = eqk**2*A(TC,SC,UC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(15) = t0 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      t0 = eqi**2*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2)-DAPQ(z4))*(A
     #(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(16) = t0
c       qi + qbi --> qbi + photon
      t0 = -eqi**2*(-ANM4_PQ(z4)-A4_PQ(z4)*dlog(pt4**2/Mf**2)+DAPQ(z4))*
     #(A(TC,UC,SC,N)+A(TC,SC,UC,N)+B(TC,UC,SC,N))
	VTEMP(17) = t0
c       qi + qbi --> g + photon
      t0 = 0.D0
	VTEMP(18) = t0
	VPART45Z(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = eqk**2*A(UC,SC,TC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(19) = t0 + VTEMP(19)
c       qi + qbi --> qbk + photon
      t0 = eqk**2*A(TC,SC,UC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(20) = t0 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      t0 = eqi**2*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2)-DAPQ(z4))*(A
     #(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(21) = t0
c       qi + qbi --> qbi + photon
      t0 = -eqi**2*(-ANM4_PQ(z4)-A4_PQ(z4)*dlog(pt4**2/Mf**2)+DAPQ(z4))*
     #(A(TC,UC,SC,N)+A(TC,SC,UC,N)+B(TC,UC,SC,N))
	VTEMP(22) = t0
c       qi + qbi --> g + photon
      t0 = 0.D0
	VTEMP(23) = t0
	VPART45Z(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      t0 = 0.D0
	VTEMP(24) = t0
c       qi + g --> g + photon
      t0 = -eqi**2*C(UC,SC,TC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**
     #2)-DAPQ(z4))
	VTEMP(25) = t0
	VPART45Z(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      t0 = eqi**2*C(SC,TC,UC,N)*(ANM4_PQ(z4)+A4_PQ(z4)*dlog(pt4**2/Mf**2
     #)-DAPQ(z4))
	VTEMP(26) = t0 + VTEMP(26)
c       g + g --> qbi + photon
      t0 = eqi**2*C(SC,TC,UC,N)*(ANM4_PQ(z4)+dlog(pt4**2/Mf**2)*A4_PQ(z4
     #)-DAPQ(z4))
	VTEMP(27) = t0 + VTEMP(27)
	ENDDO
	VPART45Z(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
C PARTIE EN LOG(1-Z4)/(1-Z4)+
	SUBROUTINE VEC_45LD(SC,TC,UC,Z4,CQI,CQK,VPART45L)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VPART45L(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VPART45L(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
	EQI = CQI(1)
	EQK = CQK(1)
c       qi + qk --> qi + photon
      t0 = -eqk**2*A(SC,TC,UC,N)*(DBPQ(z4)-2.D0*A4_PQ(z4))
	VTEMP(1) = t0
c       qi + qk --> qk + photon
      t0 = -eqi**2*A(SC,UC,TC,N)*(DBPQ(z4)-2.D0*A4_PQ(z4))
	VTEMP(2) = t0
	VPART45L(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      t0 = -eqk**2*A(SC,TC,UC,N)*(DBPQ(z4)-2.D0*A4_PQ(z4))
	VTEMP(3) = t0
c       qi + qk --> qk + photon
      t0 = -eqi**2*A(SC,UC,TC,N)*(DBPQ(z4)-2.D0*A4_PQ(z4))
	VTEMP(4) = t0
	VPART45L(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      t0 = -eqk**2*A(SC,TC,UC,N)*(DBPQ(z4)-2.D0*A4_PQ(z4))
	VTEMP(5) = t0
c       qi + qk --> qk + photon
      t0 = -eqi**2*A(SC,UC,TC,N)*(DBPQ(z4)-2.D0*A4_PQ(z4))
	VTEMP(6) = t0
	VPART45L(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      t0 = eqk**2*A(UC,TC,SC,N)*(-DBPQ(z4)+2.D0*A4_PQ(z4))
	VTEMP(7) = t0
c       qi + qbk --> qbk + photon
      t0 = eqi**2*A(TC,UC,SC,N)*(-DBPQ(z4)+2.D0*A4_PQ(z4))
	VTEMP(8) = t0
	VPART45L(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      t0 = eqk**2*A(UC,TC,SC,N)*(-DBPQ(z4)+2.D0*A4_PQ(z4))
	VTEMP(9) = t0
c       qi + qbk --> qbk + photon
      t0 = eqi**2*A(TC,UC,SC,N)*(-DBPQ(z4)+2.D0*A4_PQ(z4))
	VTEMP(10) = t0
	VPART45L(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      t0 = eqk**2*A(UC,TC,SC,N)*(-DBPQ(z4)+2.D0*A4_PQ(z4))
	VTEMP(11) = t0
c       qi + qbk --> qbk + photon
      t0 = eqi**2*A(TC,UC,SC,N)*(-DBPQ(z4)+2.D0*A4_PQ(z4))
	VTEMP(12) = t0
	VPART45L(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      t0 = -eqi**2*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))*(-2.D0*A4
     #_PQ(z4)+DBPQ(z4))
	VTEMP(13) = t0
	VPART45L(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = eqk**2*A(UC,SC,TC,N)*(-DBPQ(z4)+2.D0*A4_PQ(z4))
	VTEMP(14) = t0 + VTEMP(14)
c       qi + qbi --> qbk + photon
      t0 = -eqk**2*A(TC,SC,UC,N)*(DBPQ(z4)-2.D0*A4_PQ(z4))
	VTEMP(15) = t0 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      t0 = eqi**2*(-DBPQ(z4)+2.D0*A4_PQ(z4))*(A(UC,TC,SC,N)+A(UC,SC,TC,N
     #)+B(UC,TC,SC,N))
	VTEMP(16) = t0
c       qi + qbi --> qbi + photon
      t0 = -eqi**2*(DBPQ(z4)-2.D0*A4_PQ(z4))*(A(TC,UC,SC,N)+A(TC,SC,UC,N
     #)+B(TC,UC,SC,N))
	VTEMP(17) = t0
c       qi + qbi --> g + photon
      t0 = 0.D0
	VTEMP(18) = t0
	VPART45L(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = eqk**2*A(UC,SC,TC,N)*(-DBPQ(z4)+2.D0*A4_PQ(z4))
	VTEMP(19) = t0 + VTEMP(19)
c       qi + qbi --> qbk + photon
      t0 = -eqk**2*A(TC,SC,UC,N)*(DBPQ(z4)-2.D0*A4_PQ(z4))
	VTEMP(20) = t0 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      t0 = eqi**2*(-DBPQ(z4)+2.D0*A4_PQ(z4))*(A(UC,TC,SC,N)+A(UC,SC,TC,N
     #)+B(UC,TC,SC,N))
	VTEMP(21) = t0
c       qi + qbi --> qbi + photon
      t0 = -eqi**2*(DBPQ(z4)-2.D0*A4_PQ(z4))*(A(TC,UC,SC,N)+A(TC,SC,UC,N
     #)+B(TC,UC,SC,N))
	VTEMP(22) = t0
c       qi + qbi --> g + photon
      t0 = 0.D0
	VTEMP(23) = t0
	VPART45L(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      t0 = 0.D0
	VTEMP(24) = t0
c       qi + g --> g + photon
      t0 = eqi**2*C(UC,SC,TC,N)*(DBPQ(z4)-2.D0*A4_PQ(z4))
	VTEMP(25) = t0
	VPART45L(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      t0 = -eqi**2*C(SC,TC,UC,N)*(DBPQ(z4)-2.D0*A4_PQ(z4))
	VTEMP(26) = t0 + VTEMP(26)
c       g + g --> qbi + photon
      t0 = -eqi**2*C(SC,TC,UC,N)*(DBPQ(z4)-2.D0*A4_PQ(z4))
	VTEMP(27) = t0 + VTEMP(27)
	ENDDO
	VPART45L(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
C PARTIE EN DELTA(1-Z4)
	SUBROUTINE VEC_45DD(SC,TC,UC,MF,PT4,CQI,CQK,VPART45D)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VPART45D(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VPART45D(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
	EQI = CQI(1)
	EQK = CQK(1)
c       qi + qk --> qi + photon
      t0 = -eqk**2*A(SC,TC,UC,N)*DCPQ(1.D0)
	VTEMP(1) = t0
c       qi + qk --> qk + photon
      t0 = -eqi**2*A(SC,UC,TC,N)*DCPQ(1.D0)
	VTEMP(2) = t0
	VPART45D(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      t0 = -eqk**2*A(SC,TC,UC,N)*DCPQ(1.D0)
	VTEMP(3) = t0
c       qi + qk --> qk + photon
      t0 = -eqi**2*A(SC,UC,TC,N)*DCPQ(1.D0)
	VTEMP(4) = t0
	VPART45D(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      t0 = -eqk**2*A(SC,TC,UC,N)*DCPQ(1.D0)
	VTEMP(5) = t0
c       qi + qk --> qk + photon
      t0 = -eqi**2*A(SC,UC,TC,N)*DCPQ(1.D0)
	VTEMP(6) = t0
	VPART45D(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      t0 = -eqk**2*A(UC,TC,SC,N)*DCPQ(1.D0)
	VTEMP(7) = t0
c       qi + qbk --> qbk + photon
      t0 = -eqi**2*A(TC,UC,SC,N)*DCPQ(1.D0)
	VTEMP(8) = t0
	VPART45D(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      t0 = -eqk**2*A(UC,TC,SC,N)*DCPQ(1.D0)
	VTEMP(9) = t0
c       qi + qbk --> qbk + photon
      t0 = -eqi**2*A(TC,UC,SC,N)*DCPQ(1.D0)
	VTEMP(10) = t0
	VPART45D(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      t0 = -eqk**2*A(UC,TC,SC,N)*DCPQ(1.D0)
	VTEMP(11) = t0
c       qi + qbk --> qbk + photon
      t0 = -eqi**2*A(TC,UC,SC,N)*DCPQ(1.D0)
	VTEMP(12) = t0
	VPART45D(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      t0 = -eqi**2*DCPQ(1.D0)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N)
     #)
	VTEMP(13) = t0
	VPART45D(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = -eqk**2*A(UC,SC,TC,N)*DCPQ(1.D0)
	VTEMP(14) = t0 + VTEMP(14)
c       qi + qbi --> qbk + photon
      t0 = -eqk**2*A(TC,SC,UC,N)*DCPQ(1.D0)
	VTEMP(15) = t0 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      t0 = -eqi**2*DCPQ(1.D0)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N)
     #)
	VTEMP(16) = t0
c       qi + qbi --> qbi + photon
      t0 = -eqi**2*DCPQ(1.D0)*(A(TC,UC,SC,N)+A(TC,SC,UC,N)+B(TC,UC,SC,N)
     #)
	VTEMP(17) = t0
c       qi + qbi --> g + photon
      t0 = 0.D0
	VTEMP(18) = t0
	VPART45D(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      t0 = -eqk**2*A(UC,SC,TC,N)*DCPQ(1.D0)
	VTEMP(19) = t0 + VTEMP(19)
c       qi + qbi --> qbk + photon
      t0 = -eqk**2*A(TC,SC,UC,N)*DCPQ(1.D0)
	VTEMP(20) = t0 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      t0 = -eqi**2*DCPQ(1.D0)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N)
     #)
	VTEMP(21) = t0
c       qi + qbi --> qbi + photon
      t0 = -eqi**2*DCPQ(1.D0)*(A(TC,UC,SC,N)+A(TC,SC,UC,N)+B(TC,UC,SC,N)
     #)
	VTEMP(22) = t0
c       qi + qbi --> g + photon
      t0 = 0.D0
	VTEMP(23) = t0
	VPART45D(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      t0 = 0.D0
	VTEMP(24) = t0
c       qi + g --> g + photon
      t0 = eqi**2*C(UC,SC,TC,N)*DCPQ(1.D0)
	VTEMP(25) = t0
	VPART45D(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      t0 = -eqi**2*C(SC,TC,UC,N)*DCPQ(1.D0)
	VTEMP(26) = t0 + VTEMP(26)
c       g + g --> qbi + photon
      t0 = -eqi**2*C(SC,TC,UC,N)*DCPQ(1.D0)
	VTEMP(27) = t0 + VTEMP(27)
	ENDDO
	VPART45D(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
C*****************************************************
CC  TERMES QUI VIENNENT DE LA PARTIE VIRTUELLE
C*****************************************************
	SUBROUTINE VEC_VID(S,SC,TC,UC,MU,PT,PTM,CQI,CQK,VPARTVI)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VPARTVI(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VPARTVI(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
	MU2 = MU**2
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
	EQI = CQI(1)
	EQK = CQK(1)
c       qi + qk --> qi + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(1) = TERAJ+TERES
c       qi + qk --> qk + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(2) = TERAJ+TERES
	VPARTVI(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(3) = TERAJ+TERES
c       qi + qk --> qk + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(4) = TERAJ+TERES
	VPARTVI(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(5) = TERAJ+TERES
c       qi + qk --> qk + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(6) = TERAJ+TERES
	VPARTVI(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(7) = TERAJ+TERES
c       qi + qbk --> qbk + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(8) = TERAJ+TERES
	VPARTVI(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(9) = TERAJ+TERES
c       qi + qbk --> qbk + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(10) = TERAJ+TERES
	VPARTVI(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(11) = TERAJ+TERES
c       qi + qbk --> qbk + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(12) = TERAJ+TERES
	VPARTVI(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(13) = TERAJ+TERES
	VPARTVI(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(14) = TERAJ+TERES + VTEMP(14)
c       qi + qbi --> qbk + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(15) = TERAJ+TERES + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(16) = TERAJ+TERES
c       qi + qbi --> qbi + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(17) = TERAJ+TERES
c       qi + qbi --> g + photon
      teraj = -2.D0*B_QQ()*dlog(ptm**2/S)*eqi**2*E(SC,TC,UC,N)-B_GG()*dl
     #og(pt**2/S)*eqi**2*E(SC,TC,UC,N)+((N**2-1.D0)/N*dlog(SC/S)+N*(dlog
     #(-UC/S)+dlog(-TC/S)-dlog(SC/S)))*E(SC,UC,TC,N)*eqi**2*dlog(ptm**2/
     #S)+A4_GG(1.D0)*eqi**2*E(SC,TC,UC,N)*(dlog(pt**2/S)**2.D0/4.D0-dlog
     #(pt**2/S)*dlog(ptm**2/S)/2.D0)-A4_QQ(1.D0)*eqi**2*E(SC,TC,UC,N)*dl
     #og(ptm**2/S)**2.D0/2.D0
      s1 = 4.D0*(11.D0/6.D0*N*dlog(MU2/S)-2.D0/3.D0*GTR*dlog(MU2/S))*(N*
     #*2-1.D0)/TC/UC*(UC**2+TC**2)*eqi**2
      s3 = (2.D0*N**2-2.D0)*eqi**2*(-1.D0/N*(SC**2*(dlog(SC/S)**2.D0-0.3
     #141592653589793D1**2)+(UC**2+SC**2)*dlog(-UC/S)**2.D0+(-2.D0*UC**2
     #-2.D0*SC**2)*dlog(SC/S)*dlog(-UC/S)-2.D0*UC*TC*dlog(SC/S)+TC*(3.D0
     #*TC+2.D0*UC)*dlog(-UC/S)+0.3141592653589793D1**2*SC**2+0.314159265
     #3589793D1**2*UC**2/2.D0+0.3141592653589793D1**2*TC**2/2.D0-7.D0/2.
     #D0*UC**2-7.D0/2.D0*TC**2)+N*(3.D0*TC**2*dlog(-UC/S)+(0.31415926535
     #89793D1**2/2.D0-7.D0/2.D0-dlog(-UC/S)*dlog(-TC/S))*(UC**2+TC**2)))
     #/TC/UC
      s4 = (2.D0*N**2-2.D0)*eqi**2*(-1.D0/N*(SC**2*(dlog(SC/S)**2.D0-0.3
     #141592653589793D1**2)+(TC**2+SC**2)*dlog(-TC/S)**2.D0+(-2.D0*TC**2
     #-2.D0*SC**2)*dlog(SC/S)*dlog(-TC/S)-2.D0*UC*TC*dlog(SC/S)+UC*(3.D0
     #*UC+2.D0*TC)*dlog(-TC/S)+0.3141592653589793D1**2*SC**2+0.314159265
     #3589793D1**2*UC**2/2.D0+0.3141592653589793D1**2*TC**2/2.D0-7.D0/2.
     #D0*TC**2-7.D0/2.D0*UC**2)+N*(3.D0*UC**2*dlog(-TC/S)+(0.31415926535
     #89793D1**2/2.D0-7.D0/2.D0-dlog(-UC/S)*dlog(-TC/S))*(UC**2+TC**2)))
     #/UC/TC
      s2 = s3+s4
      teres = s1+s2
	VTEMP(18) = TERAJ+TERES
	VPARTVI(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(19) = TERAJ+TERES + VTEMP(19)
c       qi + qbi --> qbk + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(20) = TERAJ+TERES + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(21) = TERAJ+TERES
c       qi + qbi --> qbi + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(22) = TERAJ+TERES
c       qi + qbi --> g + photon
      teraj = -2.D0*B_QQ()*dlog(ptm**2/S)*eqi**2*E(SC,TC,UC,N)-B_GG()*dl
     #og(pt**2/S)*eqi**2*E(SC,TC,UC,N)+((N**2-1.D0)/N*dlog(SC/S)+N*(dlog
     #(-UC/S)+dlog(-TC/S)-dlog(SC/S)))*E(SC,UC,TC,N)*eqi**2*dlog(ptm**2/
     #S)+A4_GG(1.D0)*eqi**2*E(SC,TC,UC,N)*(dlog(pt**2/S)**2.D0/4.D0-dlog
     #(pt**2/S)*dlog(ptm**2/S)/2.D0)-A4_QQ(1.D0)*eqi**2*E(SC,TC,UC,N)*dl
     #og(ptm**2/S)**2.D0/2.D0
      s1 = 4.D0*(11.D0/6.D0*N*dlog(MU2/S)-2.D0/3.D0*GTR*dlog(MU2/S))*(N*
     #*2-1.D0)/TC/UC*(UC**2+TC**2)*eqi**2
      s3 = (2.D0*N**2-2.D0)*eqi**2*(-1.D0/N*(SC**2*(dlog(SC/S)**2.D0-0.3
     #141592653589793D1**2)+(UC**2+SC**2)*dlog(-UC/S)**2.D0+(-2.D0*UC**2
     #-2.D0*SC**2)*dlog(SC/S)*dlog(-UC/S)-2.D0*UC*TC*dlog(SC/S)+TC*(3.D0
     #*TC+2.D0*UC)*dlog(-UC/S)+0.3141592653589793D1**2*SC**2+0.314159265
     #3589793D1**2*UC**2/2.D0+0.3141592653589793D1**2*TC**2/2.D0-7.D0/2.
     #D0*UC**2-7.D0/2.D0*TC**2)+N*(3.D0*TC**2*dlog(-UC/S)+(0.31415926535
     #89793D1**2/2.D0-7.D0/2.D0-dlog(-UC/S)*dlog(-TC/S))*(UC**2+TC**2)))
     #/TC/UC
      s4 = (2.D0*N**2-2.D0)*eqi**2*(-1.D0/N*(SC**2*(dlog(SC/S)**2.D0-0.3
     #141592653589793D1**2)+(TC**2+SC**2)*dlog(-TC/S)**2.D0+(-2.D0*TC**2
     #-2.D0*SC**2)*dlog(SC/S)*dlog(-TC/S)-2.D0*UC*TC*dlog(SC/S)+UC*(3.D0
     #*UC+2.D0*TC)*dlog(-TC/S)+0.3141592653589793D1**2*SC**2+0.314159265
     #3589793D1**2*UC**2/2.D0+0.3141592653589793D1**2*TC**2/2.D0-7.D0/2.
     #D0*TC**2-7.D0/2.D0*UC**2)+N*(3.D0*UC**2*dlog(-TC/S)+(0.31415926535
     #89793D1**2/2.D0-7.D0/2.D0-dlog(-UC/S)*dlog(-TC/S))*(UC**2+TC**2)))
     #/UC/TC
      s2 = s3+s4
      teres = s1+s2
	VTEMP(23) = TERAJ+TERES
	VPARTVI(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      teraj = (B_QQ()+B_GG())*dlog(ptm**2/S)*eqi**2*E(TC,SC,UC,N)+B_QQ()
     #*dlog(pt**2/S)*eqi**2*E(TC,SC,UC,N)-((N**2-1.D0)/N*dlog(-TC/S)+N*(
     #dlog(-UC/S)+dlog(SC/S)-dlog(-TC/S)))*E(TC,UC,SC,N)*eqi**2*dlog(ptm
     #**2/S)-A4_QQ(1.D0)*eqi**2*E(TC,SC,UC,N)*(dlog(pt**2/S)**2.D0/4.D0-
     #dlog(pt**2/S)*dlog(ptm**2/S)/2.D0)+(A4_QQ(1.D0)+A4_GG(1.D0))*eqi**
     #2*E(TC,SC,UC,N)*dlog(ptm**2/S)**2.D0/4.D0
      s1 = 4.D0*(-11.D0/6.D0*N*dlog(MU2/S)+2.D0/3.D0*GTR*dlog(MU2/S))*(N
     #**2-1.D0)/SC/UC*(UC**2+SC**2)*eqi**2
      s3 = (-2.D0*N**2+2.D0)*eqi**2*(-1.D0/N*(TC**2*dlog(-TC/S)**2.D0+(U
     #C**2+TC**2)*dlog(-UC/S)**2.D0+(-2.D0*UC**2-2.D0*TC**2)*dlog(-TC/S)
     #*dlog(-UC/S)-2.D0*UC*SC*dlog(-TC/S)+SC*(3.D0*SC+2.D0*UC)*dlog(-UC/
     #S)+0.3141592653589793D1**2*TC**2+0.3141592653589793D1**2*UC**2/2.D
     #0+0.3141592653589793D1**2*SC**2/2.D0-7.D0/2.D0*UC**2-7.D0/2.D0*SC*
     #*2)+N*(3.D0*SC**2*dlog(-UC/S)+(0.3141592653589793D1**2/2.D0-7.D0/2
     #.D0-dlog(-UC/S)*dlog(SC/S))*(UC**2+SC**2)))/SC/UC
      s4 = (-2.D0*N**2+2.D0)*eqi**2*(-1.D0/N*(TC**2*dlog(-TC/S)**2.D0+(S
     #C**2+TC**2)*(dlog(SC/S)**2.D0-0.3141592653589793D1**2)+(-2.D0*SC**
     #2-2.D0*TC**2)*dlog(-TC/S)*dlog(SC/S)-2.D0*UC*SC*dlog(-TC/S)+UC*(3.
     #D0*UC+2.D0*SC)*dlog(SC/S)+0.3141592653589793D1**2*TC**2+0.31415926
     #53589793D1**2*UC**2/2.D0+0.3141592653589793D1**2*SC**2/2.D0-7.D0/2
     #.D0*SC**2-7.D0/2.D0*UC**2)+N*(3.D0*UC**2*dlog(SC/S)+(0.31415926535
     #89793D1**2/2.D0-7.D0/2.D0-dlog(-UC/S)*dlog(SC/S))*(UC**2+SC**2)))/
     #UC/SC
      s2 = s3+s4
      teres = s1+s2
	VTEMP(24) = TERAJ+TERES
c       qi + g --> g + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(25) = TERAJ+TERES
	VPARTVI(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(26) = TERAJ+TERES + VTEMP(26)
c       g + g --> qbi + photon
      teraj = 0.D0
      teres = 0.D0
	VTEMP(27) = TERAJ+TERES + VTEMP(27)
	ENDDO
	VPARTVI(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
C*****************************************************	
	SUBROUTINE VEC_VI2D(S,SC,TC,UC,YS,FI,CQI,CQK,VPARTVI2)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	PARAMETER (J0MAX=11)
	DIMENSION VPARTVI2(J0MAX),HH34(J0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX)
	PI=4.D0*DATAN(1.D0)
	DO I = 1,J0MAX
	  VPARTVI2(I) = 0.D0
	  HH34(I) = 0.D0
	ENDDO
C-------------------------------------------------------------------
	s12 = sc/2.d0
	s13 = -tc/2.d0
	s23 = -uc/2.d0
	s14 = s23
	s24 = s13
	s34 = s12
	s15 = 0.d0
	s25 = 0.d0
	s35 = 0.d0
	s45 = 0.d0
	CALL VEC_H34D(S12,S13,S14,S15,S23,S24,S25,S34,S35,S45,
     #	CQI,CQK,HH34)
	DW=DCOSH(2.D0*YS)+DCOS(2.D0*FI)
	XJ1=DLOG(DSIN(FI))
	XJ2=DSIN(2.D0*FI)*DLOG(DSIN(FI))*DATAN(DSIN(FI)/(1.D0-DCOS(FI)))
	A34=4.D0*XJ2+DSINH(2.D0*YS)*2.D0*YS*XJ1
	FFG=A34/DW+DLOG(2.D0)*DLOG(4.D0*DCOSH(YS)**2)
	DO I = 1,J0MAX
	  VPARTVI2(I) = HH34(I)*(FFG+FFG)/PI
	ENDDO
	RETURN
	END
C*****************************************************	
C pour production d'un photon	
	SUBROUTINE STRFRAD(X1,IH1,X2,IH2,SFO)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/GDF/JNF
	COMMON/SCALE/M,MF,MU
	DIMENSION F1(-6:6),F2(-6:6),D3(-6:6)
	PARAMETER (K0MAX=22,J0MAX=11)
c qcharge est la valeur absolue de la charge des quarks fois 3 au carre
	DIMENSION SF(K0MAX),SFO(K0MAX),QCHARGE(6)
	DATA QCHARGE/1.D0,4.D0,1.D0,4.D0,1.D0,4.D0/
	CALL FSTRU(X1,M*M,IH1, F1)
	CALL FSTRU(X2,M*M,IH2, F2)
C initialisation du tableau SF(K0MAX)
	ZERO = 0.D0
	CALL VEC_DINIT(SF,K0MAX,ZERO)
	CALL VEC_DINIT(SFO,K0MAX,ZERO)
C on commence les processus
	DO I=1,JNF,2
	  DO K=2,JNF,2
C	qi + qk --> jet + ph
C	j0 = 1 : D + U --> jet + ph 
	    SF(1) = F1(I)*F2(K) + 	    
     #              F1(-I)*F2(-K) + SF(1)
	    SF(1+11) = F1(K)*F2(I) + 	    
     #                 F1(-K)*F2(-I) + SF(1+11)
C	qi + qbk --> qi + ph
C	j0 = 4 : D + Ub --> jet + ph
	    SF(4) = F1(I)*F2(-K) + 	    
     #              F1(-I)*F2(K) + SF(4)
	    SF(4+11) = F1(-K)*F2(I) + 	    
     #                 F1(K)*F2(-I) + SF(4+11)
	  ENDDO
	ENDDO
	DO I=1,JNF,2
	  DO K=I+2,JNF,2
C	j0 = 2 : D + Dp --> jet + ph
	    SF(2) = F1(I)*F2(K) + 	    
     #              F1(-I)*F2(-K) + SF(2)
	    SF(2+11) = F1(K)*F2(I) + 	    
     #                 F1(-K)*F2(-I) + SF(2+11)
C	j0 = 3 : U + Up --> jet + ph
	    SF(3) = F1(I+1)*F2(K+1) + 	    
     #              F1(-I-1)*F2(-K-1) + SF(3)
	    SF(3+11) = F1(K+1)*F2(I+1) + 	    
     #                 F1(-K-1)*F2(-I-1) + SF(3+11)
C	j0 = 5 : D + Dpb --> jet + ph
	    SF(5) = F1(I)*F2(-K) + 	    
     #              F1(-I)*F2(K) + SF(5)
	    SF(5+11) = F1(-K)*F2(I) + 	    
     #                 F1(K)*F2(-I) + SF(5+11)
C	j0 = 6 : U + Upb --> jet + ph
	    SF(6) = F1(I+1)*F2(-K-1) + 	    
     #              F1(-I-1)*F2(K+1) + SF(6)
	    SF(6+11) = F1(-K-1)*F2(I+1) + 	    
     #                 F1(K+1)*F2(-I-1) + SF(6+11)
	  ENDDO
	ENDDO
	DO I=1,JNF,2
C	qi + qbi --> jet + ph
C	j0 = 8 : D + Db --> jet + ph
	  SF(8) = F1(I)*F2(-I) + SF(8)
	  SF(8+11) = F1(-I)*F2(I) + SF(8+11)
C	j0 = 9 : U + Ub --> jet + ph
	  SF(9) = F1(I+1)*F2(-I-1) + SF(9)
	  SF(9+11) = F1(-I-1)*F2(I+1) + SF(9+11)
	ENDDO
	DO I=1,JNF
C	j0 = 7 : qi + qi --> jet + ph
	  SF(7) = QCHARGE(I)*(F1(I)*F2(I) + 	    
     #             F1(-I)*F2(-I))/2.D0 + SF(7)
C	j0 = 10 : qi + g --> jet + ph
	  SF(10) = QCHARGE(I)*(F1(I)*F2(0) + 	    
     #            F1(-I)*F2(0)) + SF(10)
	  SF(10+11) = QCHARGE(I)*(F1(0)*F2(I) + 	    
     #               F1(0)*F2(-I)) + SF(10+11)
	ENDDO 	    
C	j0 = 11 : g + g --> qi + ph
	SF(11) = F1(0)*F2(0)/2.d0 + SF(11)
C
     	SF(7+11) = SF(7)
C
     	SF(11+11) = SF(11)
C
C on divise tout par x1 x2
	XX = 1.D0/(X1*X2)
	CALL VEC_DMULT_CONSTANT(SF,K0MAX,XX,SFO)	    
	RETURN
	END
C
