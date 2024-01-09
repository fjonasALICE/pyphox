C coefficients devant p_1.p_2/(p_1.p_5 p_2.p_5)
C ----------------------------------------------------
	SUBROUTINE VEC_CONSD(S12,S13,S14,S15,S23,S24,S25,S34,S35,S45,
     #	CQI,CQK,VCONS)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VCONS(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VCONS(I) = 0.D0
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
      hCONS = 0.D0
	VTEMP(1) = HCONS
c       qi + qk --> qk + photon
      hCONS = 0.D0
	VTEMP(2) = HCONS
	VCONS(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      hCONS = 0.D0
	VTEMP(3) = HCONS
c       qi + qk --> qk + photon
      hCONS = 0.D0
	VTEMP(4) = HCONS
	VCONS(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      hCONS = 0.D0
	VTEMP(5) = HCONS
c       qi + qk --> qk + photon
      hCONS = 0.D0
	VTEMP(6) = HCONS
	VCONS(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      hCONS = 0.D0
	VTEMP(7) = HCONS
c       qi + qbk --> qbk + photon
      hCONS = 0.D0
	VTEMP(8) = HCONS
	VCONS(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      hCONS = 0.D0
	VTEMP(9) = HCONS
c       qi + qbk --> qbk + photon
      hCONS = 0.D0
	VTEMP(10) = HCONS
	VCONS(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      hCONS = 0.D0
	VTEMP(11) = HCONS
c       qi + qbk --> qbk + photon
      hCONS = 0.D0
	VTEMP(12) = HCONS
	VCONS(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      hCONS = 0.D0
	VTEMP(13) = HCONS
	VCONS(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      hCONS = 0.D0
	VTEMP(14) = HCONS + VTEMP(14)
c       qi + qbi --> qbk + photon
      hCONS = 0.D0
	VTEMP(15) = HCONS + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      hCONS = 0.D0
	VTEMP(16) = HCONS
c       qi + qbi --> qbi + photon
      hCONS = 0.D0
	VTEMP(17) = HCONS
c       qi + qbi --> g + photon
      hCONS = 0.D0
	VTEMP(18) = HCONS
	VCONS(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      hCONS = 0.D0
	VTEMP(19) = HCONS + VTEMP(19)
c       qi + qbi --> qbk + photon
      hCONS = 0.D0
	VTEMP(20) = HCONS + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      hCONS = 0.D0
	VTEMP(21) = HCONS
c       qi + qbi --> qbi + photon
      hCONS = 0.D0
	VTEMP(22) = HCONS
c       qi + qbi --> g + photon
      hCONS = 0.D0
	VTEMP(23) = HCONS
	VCONS(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
c       qi + g --> qi + photon
      hCONS = 0.D0
	VTEMP(24) = HCONS
c       qi + g --> g + photon
      hCONS = 0.D0
	VTEMP(25) = HCONS
	VCONS(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      hCONS = 0.D0
	VTEMP(26) = HCONS + VTEMP(26)
c       g + g --> qbi + photon
      hCONS = 0.D0
	VTEMP(27) = HCONS + VTEMP(27)
	ENDDO
	VCONS(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
