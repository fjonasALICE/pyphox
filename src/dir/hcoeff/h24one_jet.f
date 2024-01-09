C coefficients devant p_2.p_4/(p_2.p_5 p_4.p_5)
C ----------------------------------------------------
	SUBROUTINE VEC_H24D(S12,S13,S14,S15,S23,S24,S25,S34,S35,S45,
     #	CQI,CQK,VH24)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=11,I0MAX=27)
	DIMENSION VH24(J0MAX),VTEMP(I0MAX)
	DIMENSION CQI(J0MAX),CQK(J0MAX),QCH(6)
	DATA QCH/-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0,
     #	-0.333333333333D0,0.666666666667D0/
	DO I = 1,J0MAX
	  VH24(I) = 0.D0
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
      hh24 = 2.D0*Vc*(s12**2+s23**2+s15**2+s35**2)*(-eqi*eqk*s12*s34*s45
     #+eqi*eqk*s23*s14*s45+eqi*eqk*s15*s24*s34-eqi*eqk*s35*s24*s14+eqk**
     #2*s25*s14*s34+eqi**2*s13*s24*s45)/s24**2/s13/s14/s34
	VTEMP(1) = HH24
c       qi + qk --> qk + photon
      hh24 = 0.D0
	VTEMP(2) = HH24
	VH24(1) = VTEMP(1) + VTEMP(2)
C	j0 = 2 : D + Dp --> jet + ph
	EQI = CQI(2)
	EQK = CQK(2)
c       qi + qk --> qi + photon
      hh24 = 2.D0*Vc*(s12**2+s23**2+s15**2+s35**2)*(-eqi*eqk*s12*s34*s45
     #+eqi*eqk*s23*s14*s45+eqi*eqk*s15*s24*s34-eqi*eqk*s35*s24*s14+eqk**
     #2*s25*s14*s34+eqi**2*s13*s24*s45)/s24**2/s13/s14/s34
	VTEMP(3) = HH24
c       qi + qk --> qk + photon
      hh24 = 0.D0
	VTEMP(4) = HH24
	VH24(2) = VTEMP(3) + VTEMP(4)
C	j0 = 3 : U + Up --> jet + ph
	EQI = CQI(3)
	EQK = CQK(3)
c       qi + qk --> qi + photon
      hh24 = 2.D0*Vc*(s12**2+s23**2+s15**2+s35**2)*(-eqi*eqk*s12*s34*s45
     #+eqi*eqk*s23*s14*s45+eqi*eqk*s15*s24*s34-eqi*eqk*s35*s24*s14+eqk**
     #2*s25*s14*s34+eqi**2*s13*s24*s45)/s24**2/s13/s14/s34
	VTEMP(5) = HH24
c       qi + qk --> qk + photon
      hh24 = 0.D0
	VTEMP(6) = HH24
	VH24(3) = VTEMP(5) + VTEMP(6)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	EQI = CQI(4)
	EQK = CQK(4)
c       qi + qbk --> qi + photon
      hh24 = 2.D0*Vc*(s15**2+s35**2+s12**2+s23**2)*(-eqi*eqk*s15*s34*s24
     #+eqi*eqk*s35*s14*s24+eqi*eqk*s12*s45*s34-eqi*eqk*s23*s45*s14+eqk**
     #2*s25*s14*s34+eqi**2*s13*s45*s24)/s24**2/s13/s14/s34
	VTEMP(7) = HH24
c       qi + qbk --> qbk + photon
      hh24 = 0.D0
	VTEMP(8) = HH24
	VH24(4) = VTEMP(7) + VTEMP(8)
C	j0 = 5 : D + Dpb --> jet + ph
	EQI = CQI(5)
	EQK = CQK(5)
c       qi + qbk --> qi + photon
      hh24 = 2.D0*Vc*(s15**2+s35**2+s12**2+s23**2)*(-eqi*eqk*s15*s34*s24
     #+eqi*eqk*s35*s14*s24+eqi*eqk*s12*s45*s34-eqi*eqk*s23*s45*s14+eqk**
     #2*s25*s14*s34+eqi**2*s13*s45*s24)/s24**2/s13/s14/s34
	VTEMP(9) = HH24
c       qi + qbk --> qbk + photon
      hh24 = 0.D0
	VTEMP(10) = HH24
	VH24(5) = VTEMP(9) + VTEMP(10)
C	j0 = 6 : U + Upb --> jet + ph
	EQI = CQI(6)
	EQK = CQK(6)
c       qi + qbk --> qi + photon
      hh24 = 2.D0*Vc*(s15**2+s35**2+s12**2+s23**2)*(-eqi*eqk*s15*s34*s24
     #+eqi*eqk*s35*s14*s24+eqi*eqk*s12*s45*s34-eqi*eqk*s23*s45*s14+eqk**
     #2*s25*s14*s34+eqi**2*s13*s45*s24)/s24**2/s13/s14/s34
	VTEMP(11) = HH24
c       qi + qbk --> qbk + photon
      hh24 = 0.D0
	VTEMP(12) = HH24
	VH24(6) = VTEMP(11) + VTEMP(12)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	EQI = CQI(7)
c       qi + qi --> qi + photon
      hh24 = -2.D0*Vc*eqi**2*(N*s23*s15*s12**2+N*s23**3*s15+N*s23*s15**3
     #+N*s23*s15*s35**2+N*s25*s13*s12**2+N*s25**3*s13+N*s25*s13*s35**2+N
     #*s25*s13**3-s12**3*s35+s12**2*s23*s15+s12**2*s25*s13-s35**3*s12+s3
     #5**2*s23*s15+s35**2*s25*s13)*(-s34+s14)/N/s23/s13/s24/s14/s34
	VTEMP(13) = HH24
	VH24(7) = VTEMP(13)
C
C	j0 = 8 : D + Db --> jet + ph
C
	EQI = QCH(1)
	DO I=2,JNF
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      hh24 = 0.D0
	VTEMP(14) = HH24 + VTEMP(14)
c       qi + qbi --> qbk + photon
      hh24 = 0.D0
	VTEMP(15) = HH24 + VTEMP(15)
	ENDDO
c       qi + qbi --> qi + photon
      hh24 = 2.D0*Vc*eqi**2*(N*s35*s12*s15**2+N*s35**3*s12+N*s35*s12**3+
     #N*s35*s12*s23**2+N*s25*s13*s15**2+N*s25**3*s13+N*s25*s13*s23**2+N*
     #s25*s13**3-s15**3*s23+s15**2*s35*s12+s15**2*s25*s13-s23**3*s15+s23
     #**2*s35*s12+s23**2*s25*s13)*(s14-s34)/N/s12/s13/s14/s34/s24
	VTEMP(16) = HH24
c       qi + qbi --> qbi + photon
      hh24 = 0.D0
	VTEMP(17) = HH24
c       qi + qbi --> g + photon
      hh24 = 0.D0
	VTEMP(18) = HH24
	VH24(8) = VTEMP(14) + VTEMP(15) +
     #	VTEMP(16) + VTEMP(17) + VTEMP(18)
C
C	j0 = 9 : U + Ub --> jet + ph
C
	EQI = QCH(2)
	DO I=1,JNF
	  IF (I.NE.2) THEN
	  EQK = QCH(I)
c       qi + qbi --> qk + photon
      hh24 = 0.D0
	VTEMP(19) = HH24 + VTEMP(19)
c       qi + qbi --> qbk + photon
      hh24 = 0.D0
	VTEMP(20) = HH24 + VTEMP(20)
	  ENDIF
	ENDDO
c       qi + qbi --> qi + photon
      hh24 = 2.D0*Vc*eqi**2*(N*s35*s12*s15**2+N*s35**3*s12+N*s35*s12**3+
     #N*s35*s12*s23**2+N*s25*s13*s15**2+N*s25**3*s13+N*s25*s13*s23**2+N*
     #s25*s13**3-s15**3*s23+s15**2*s35*s12+s15**2*s25*s13-s23**3*s15+s23
     #**2*s35*s12+s23**2*s25*s13)*(s14-s34)/N/s12/s13/s14/s34/s24
	VTEMP(21) = HH24
c       qi + qbi --> qbi + photon
      hh24 = 0.D0
	VTEMP(22) = HH24
c       qi + qbi --> g + photon
      hh24 = 0.D0
	VTEMP(23) = HH24
	VH24(9) = VTEMP(19) + VTEMP(20) +
     #	VTEMP(21) + VTEMP(22) + VTEMP(23)
C
C	j0 = 10 : qi + g --> jet + ph
C
	EQI = CQI(10)
c       qi + g --> qi + photon
      hh24 = 0.D0
	VTEMP(24) = HH24
c       qi + g --> g + photon
      hh24 = 2.D0*Vc*eqi**2*(2.D0*CF*s15*s23+N*s12*s35+N*s13*s25-N*s15*s
     #23)*(s13**2+s35**2)/s24/s23/s12/s14
	VTEMP(25) = HH24
	VH24(10) = VTEMP(24) + VTEMP(25)
C
C	j0 = 11 : g + g --> jet + ph
C
	DO I=1,JNF
	  EQI = QCH(I)
c       g + g --> qi + photon
      hh24 = 2.D0*Vc*eqi**2*(2.D0*CF*s35*s12+N*s25*s13+N*s15*s23-N*s35*s
     #12)*(s15**2+s13**2)/s24/s12/s23/s34
	VTEMP(26) = HH24 + VTEMP(26)
c       g + g --> qbi + photon
      hh24 = 2.D0*Vc*eqi**2*(2.D0*CF*s35*s12+N*s23*s15+N*s13*s25-N*s35*s
     #12)*(s13**2+s15**2)/s24/s12/s23/s34
	VTEMP(27) = HH24 + VTEMP(27)
	ENDDO
	VH24(11) = VTEMP(26) + VTEMP(27)
	RETURN
	END
