C coefficients devant p_1.p_3/(p_1.p_5 p_3.p_5)
C ----------------------------------------------------
	SUBROUTINE VEC_H13D(S12,S13,S14,S15,S23,S24,S25,S34,S35,S45,
     #	CQI,CQK,VH13)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VH13(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VH13(I) = 0.D0
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
      hh13 = 0.D0
	VTEMP(1) = HH13
c       qi + qk --> qk + photon
      hh13 = 0.D0
	VTEMP(2) = HH13
	VH13(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      hh13 = 0.D0
	VTEMP(3) = HH13
c       qi + qk --> qk + photon
      hh13 = 0.D0
	VTEMP(4) = HH13
	VH13(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      hh13 = 0.D0
	VTEMP(5) = HH13
c       qi + qk --> qk + photon
      hh13 = 0.D0
	VTEMP(6) = HH13
	VH13(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      hh13 = 0.D0
	VTEMP(7) = HH13
c       qi + qbk --> qbk + photon
      hh13 = 0.D0
	VTEMP(8) = HH13
	VH13(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      hh13 = 0.D0
	VTEMP(9) = HH13
c       qi + qbk --> qbk + photon
      hh13 = 0.D0
	VTEMP(10) = HH13
	VH13(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      hh13 = 0.D0
	VTEMP(11) = HH13
c       qi + qbk --> qbk + photon
      hh13 = 0.D0
	VTEMP(12) = HH13
	VH13(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      hh13 = 0.D0
	VTEMP(13) = HH13
	VH13(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      hh13 = 0.D0
	VTEMP(14) = HH13 + VTEMP(14)
c       qi + qbi --> qbk + photon
      hh13 = 0.D0
	VTEMP(15) = HH13 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      hh13 = 0.D0
	VTEMP(16) = HH13
c       qi + qbi --> qbi + photon
      hh13 = 2.D0*Vc*eqi**2*(N*s35*s12*s13**2+N*s35**3*s12+N*s35*s12**3+
     #N*s35*s12*s25**2+N*s23*s15*s13**2+N*s23**3*s15+N*s23*s15*s25**2+N*
     #s23*s15**3-s13**3*s25+s13**2*s35*s12+s13**2*s23*s15-s25**3*s13+s25
     #**2*s35*s12+s25**2*s23*s15)*(-s13*s24+s23*s14+s34*s12-s34*s14)/s13
     #/N/s23/s12/s34/s14/s24
	VTEMP(17) = HH13
c       qi + qbi --> g + photon
      hh13 = 2.D0/s13*Vc*eqi**2*N/s14/s24/s23*(s14**3*s24+s14*s24**3+s13
     #**3*s23+s13*s23**3+s15**3*s25+s15*s25**3)
	VTEMP(18) = HH13
	VH13(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      hh13 = 0.D0
	VTEMP(19) = HH13 + VTEMP(19)
c       qi + qbi --> qbk + photon
      hh13 = 0.D0
	VTEMP(20) = HH13 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      hh13 = 0.D0
	VTEMP(21) = HH13
c       qi + qbi --> qbi + photon
      hh13 = 2.D0*Vc*eqi**2*(N*s35*s12*s13**2+N*s35**3*s12+N*s35*s12**3+
     #N*s35*s12*s25**2+N*s23*s15*s13**2+N*s23**3*s15+N*s23*s15*s25**2+N*
     #s23*s15**3-s13**3*s25+s13**2*s35*s12+s13**2*s23*s15-s25**3*s13+s25
     #**2*s35*s12+s25**2*s23*s15)*(-s13*s24+s23*s14+s34*s12-s34*s14)/s13
     #/N/s23/s12/s34/s14/s24
	VTEMP(22) = HH13
c       qi + qbi --> g + photon
      hh13 = 2.D0/s13*Vc*eqi**2*N/s14/s24/s23*(s14**3*s24+s14*s24**3+s13
     #**3*s23+s13*s23**3+s15**3*s25+s15*s25**3)
	VTEMP(23) = HH13
	VH13(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      hh13 = 2.D0*Vc*eqi**2*(2.D0*CF-N)*(s14**3*s34+s14*s34**3+s12**3*s2
     #3+s12*s23**3+s15**3*s35+s15*s35**3)/s14/s12/s34/s23
	VTEMP(24) = HH13
c       qi + g --> g + photon
      hh13 = 0.D0
	VTEMP(25) = HH13
	VH13(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      hh13 = 0.D0
	VTEMP(26) = HH13 + VTEMP(26)
c       g + g --> qbi + photon
      hh13 = 0.D0
	VTEMP(27) = HH13 + VTEMP(27)
	ENDDO
	VH13(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
