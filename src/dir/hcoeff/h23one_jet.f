C coefficients devant p_2.p_3/(p_2.p_5 p_3.p_5)
C ----------------------------------------------------
	SUBROUTINE VEC_H23D(S12,S13,S14,S15,S23,S24,S25,S34,S35,S45,
     #	CQI,CQK,VH23)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VH23(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VH23(I) = 0.D0
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
      hh23 = 0.D0
	VTEMP(1) = HH23
c       qi + qk --> qk + photon
      hh23 = 0.D0
	VTEMP(2) = HH23
	VH23(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      hh23 = 0.D0
	VTEMP(3) = HH23
c       qi + qk --> qk + photon
      hh23 = 0.D0
	VTEMP(4) = HH23
	VH23(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      hh23 = 0.D0
	VTEMP(5) = HH23
c       qi + qk --> qk + photon
      hh23 = 0.D0
	VTEMP(6) = HH23
	VH23(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      hh23 = 0.D0
	VTEMP(7) = HH23
c       qi + qbk --> qbk + photon
      hh23 = 0.D0
	VTEMP(8) = HH23
	VH23(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      hh23 = 0.D0
	VTEMP(9) = HH23
c       qi + qbk --> qbk + photon
      hh23 = 0.D0
	VTEMP(10) = HH23
	VH23(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      hh23 = 0.D0
	VTEMP(11) = HH23
c       qi + qbk --> qbk + photon
      hh23 = 0.D0
	VTEMP(12) = HH23
	VH23(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      hh23 = 0.D0
	VTEMP(13) = HH23
	VH23(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      hh23 = 0.D0
	VTEMP(14) = HH23 + VTEMP(14)
c       qi + qbi --> qbk + photon
      hh23 = 0.D0
	VTEMP(15) = HH23 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      hh23 = 2.D0*Vc*eqi**2*(N*s35*s12*s15**2+N*s35**3*s12+N*s35*s12**3+
     #N*s35*s12*s23**2+N*s25*s13*s15**2+N*s25**3*s13+N*s25*s13*s23**2+N*
     #s25*s13**3-s15**3*s23+s15**2*s35*s12+s15**2*s25*s13-s23**3*s15+s23
     #**2*s35*s12+s23**2*s25*s13)*(-s23*s14+s12*s34+s24*s13-s24*s34)/s23
     #/N/s12/s13/s14/s34/s24
	VTEMP(16) = HH23
c       qi + qbi --> qbi + photon
      hh23 = 0.D0
	VTEMP(17) = HH23
c       qi + qbi --> g + photon
      hh23 = 2.D0/s13*Vc*eqi**2*N/s14/s24/s23*(s14**3*s24+s14*s24**3+s13
     #**3*s23+s13*s23**3+s15**3*s25+s15*s25**3)
	VTEMP(18) = HH23
	VH23(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      hh23 = 0.D0
	VTEMP(19) = HH23 + VTEMP(19)
c       qi + qbi --> qbk + photon
      hh23 = 0.D0
	VTEMP(20) = HH23 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      hh23 = 2.D0*Vc*eqi**2*(N*s35*s12*s15**2+N*s35**3*s12+N*s35*s12**3+
     #N*s35*s12*s23**2+N*s25*s13*s15**2+N*s25**3*s13+N*s25*s13*s23**2+N*
     #s25*s13**3-s15**3*s23+s15**2*s35*s12+s15**2*s25*s13-s23**3*s15+s23
     #**2*s35*s12+s23**2*s25*s13)*(-s23*s14+s12*s34+s24*s13-s24*s34)/s23
     #/N/s12/s13/s14/s34/s24
	VTEMP(21) = HH23
c       qi + qbi --> qbi + photon
      hh23 = 0.D0
	VTEMP(22) = HH23
c       qi + qbi --> g + photon
      hh23 = 2.D0/s13*Vc*eqi**2*N/s14/s24/s23*(s14**3*s24+s14*s24**3+s13
     #**3*s23+s13*s23**3+s15**3*s25+s15*s25**3)
	VTEMP(23) = HH23
	VH23(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      hh23 = 2.D0/s12*Vc*eqi**2*N/s14/s34/s23*(s14**3*s34+s14*s34**3+s12
     #**3*s23+s12*s23**3+s15**3*s35+s15*s35**3)
	VTEMP(24) = HH23
c       qi + g --> g + photon
      hh23 = 2.D0*Vc*eqi**2*(2.D0*CF*s15*s23+N*s12*s35+N*s13*s25-N*s15*s
     #23)*(s14**2+s45**2)/s23**2/s12/s13
	VTEMP(25) = HH23
	VH23(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      hh23 = 0.D0
	VTEMP(26) = HH23 + VTEMP(26)
c       g + g --> qbi + photon
      hh23 = 0.D0
	VTEMP(27) = HH23 + VTEMP(27)
	ENDDO
	VH23(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
