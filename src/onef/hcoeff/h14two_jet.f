C coefficients devant p_1.p_4/(p_1.p_5 p_4.p_5)
C ----------------------------------------------------
	SUBROUTINE VEC_H14O(S12,S13,S14,S15,S23,S24,S25,S34,S35,S45,
     #	VH14)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VH14(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VH14(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      hh14 = 2.D0*CF*(2.D0*CF*N-1.D0)*(s12**2+s34**2+s14**2+s23**2)/s13/
     #s24
	VTEMP(1) = HH14
c       qi + qk --> g + qk
      hh14 = 2.D0/3.D0*s45/s14*CF*(2.D0*CF*N*s14*s23-s14*s23+2.D0*s12*s3
     #4-s24*s13)/s24/s13/s34/s23*(s12**2+s45**2+s14**2+s25**2)
	VTEMP(2) = HH14
	VH14(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      hh14 = -2.D0/3.D0*s15/s14*CF/s13/s24*(s12**2+s35**2+s15**2+s23**2)
	VTEMP(3) = HH14
c       qi + qk --> qk + g
      hh14 = 2.D0/3.D0*CF*(s12**2+s35**2+s13**2+s25**2)*(2.D0*CF*N*s13*s
     #24*s45-s13*s24*s45+2.D0*s12*s34*s45-s23*s14*s45-s15*s34*s24+6.D0*C
     #F*N*s25*s14*s34-3.D0*s25*s14*s34+6.D0*s35*s14*s24)/s14**2/s23/s34/
     #s24
	VTEMP(4) = HH14
	VH14(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      hh14 = 4.D0/s13*CF/s24*(s14**2+s23**2+s12**2+s34**2)
	VTEMP(5) = HH14
c       qi + qbk --> g + qbk
      hh14 = 2.D0/3.D0*s45/s14*CF*(2.D0*CF*N*s12*s34-s12*s34+2.D0*s14*s2
     #3-s24*s13)/s24/s13/s23/s34*(s14**2+s25**2+s12**2+s45**2)
	VTEMP(6) = HH14
	VH14(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      hh14 = -2.D0/3.D0*s15/s14*CF/s13/s24*(s15**2+s23**2+s12**2+s35**2)
	VTEMP(7) = HH14
c       qi + qbk --> qbk + g
      hh14 = 2.D0/3.D0*CF*(s13**2+s25**2+s12**2+s35**2)*(2.D0*CF*N*s12*s
     #34*s45-s12*s34*s45+2.D0*s13*s24*s45-s23*s14*s45-s15*s24*s34+6.D0*C
     #F*N*s35*s14*s24-3.D0*s35*s14*s24+6.D0*s25*s14*s34)/s14**2/s23/s24/
     #s34
	VTEMP(8) = HH14
	VH14(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      ah14 = 2.D0*CF*(2.D0*CF*N-1.D0)*(s12**2+s34**2+s14**2+s23**2)/s13/
     #s24
      bh14 = -2.D0*CF/s14/s23*(s12**2+s34**2+s13**2+s24**2)
      ch14 = 2.D0/s13*CF/s24/s14/s23/N*(s12*s34-s13*s24-s14*s23)*(s12**2
     #+s34**2)
	VTEMP(9) = AH14 + BH14 + CH14
c       qi + qi --> g + qi
      ah14 = 2.D0/3.D0*s45/s14*CF*(2.D0*CF*N*s14*s23-s14*s23+2.D0*s12*s3
     #4-s24*s13)/s24/s13/s34/s23*(s12**2+s45**2+s14**2+s25**2)
      bh14 = 0.D0
      ch14 = 0.D0
	VTEMP(10) = AH14 + BH14 + CH14
	VH14(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      ah14 = -2.D0/3.D0*s15/s14*CF/s13/s24*(s12**2+s35**2+s15**2+s23**2)
      bh14 = 2.D0/3.D0*CF*(s12**2+s35**2+s13**2+s25**2)*(2.D0*CF*N*s13*s
     #45*s24+2.D0*s12*s45*s34-s13*s45*s24-s23*s14*s45-s15*s24*s34+6.D0*C
     #F*N*s25*s14*s34+6.D0*s35*s14*s24-3.D0*s25*s14*s34)/s14**2/s23/s24/
     #s34
      ch14 = -2.D0*CF*(s12*s35-s13*s25-s15*s23)*(s12**2+s35**2)/s14*(-s3
     #4+2.D0*s24*CF*N+2.D0*s24)/s13/s23/s24/s34/N
	VTEMP(11) = AH14 + BH14 + CH14
	VH14(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      hh14 = 4.D0/s12*CF/s34*(s14**2+s23**2+s13**2+s24**2)
	VTEMP(12) = HH14
c       qi + qbi --> g + qbk
      hh14 = 2.D0/3.D0*s15/s14*CF*(2.D0*s14*s23-s12*s34+2.D0*CF*N*s24*s1
     #3-s24*s13)/s12/s13/s34/s23*(s14**2+s25**2+s15**2+s24**2)
	VTEMP(13) = HH14
	VH14(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      ah14 = 4.D0/s13*CF/s24*(s14**2+s23**2+s12**2+s34**2)
      bh14 = 4.D0*CF/s12/s34*(s14**2+s23**2+s13**2+s24**2)
      ch14 = 4.D0*CF*(1.D0+CF*N)*(-s14*s23+s13*s24+s12*s34)*(s14**2+s23*
     #*2)/s13/s24/s12/s34/N
	VTEMP(14) = AH14 + BH14 + CH14
c       qi + qbi --> g + qbi
      ah14 = 2.D0/3.D0*s45/s14*CF*(2.D0*CF*N*s12*s34-s12*s34+2.D0*s14*s2
     #3-s24*s13)/s24/s13/s23/s34*(s14**2+s25**2+s12**2+s45**2)
      bh14 = 2.D0/3.D0*s15/s14*CF*(2.D0*CF*N*s24*s13-s12*s34+2.D0*s14*s2
     #3-s24*s13)/s12/s13/s23/s34*(s14**2+s25**2+s15**2+s24**2)
      ch14 = 2.D0*CF*(-s14*s25+s15*s24+s12*s45)*(s14**2+s25**2)*(2.D0*CF
     #*N*s14*s23-s12*s34+2.D0*s14*s23-s24*s13+2.D0*s13*s34*CF*N+2.D0*s13
     #*s34)/s14/s24/s12/s13/s34/s23/N
	VTEMP(15) = AH14 + BH14 + CH14
	VH14(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      hh14 = -2.D0/3.D0*s15/s14*CF/s12/s34*(s15**2+s23**2+s13**2+s25**2)
	VTEMP(16) = HH14
c       qi + qbi --> qbk + g
      hh14 = -2.D0/3.D0*s15/s14*CF/s12/s34*(s13**2+s25**2+s15**2+s23**2)
	VTEMP(17) = HH14
c       qi + qbi --> qi + g
      ah14 = -2.D0/3.D0*s15/s14*CF/s13/s24*(s15**2+s23**2+s12**2+s35**2)
      bh14 = -2.D0/3.D0*s15/s14*CF/s12/s34*(s15**2+s23**2+s13**2+s25**2)
      ch14 = 0.D0
	VTEMP(18) = AH14 + BH14 + CH14
c       qi + qbi --> qbi + g
      ah14 = 2.D0/3.D0*CF*(s13**2+s25**2+s12**2+s35**2)*(2.D0*CF*N*s12*s
     #34*s45-s12*s34*s45+2.D0*s13*s24*s45-s23*s14*s45-s15*s24*s34+6.D0*C
     #F*N*s35*s14*s24-3.D0*s35*s14*s24+6.D0*s25*s14*s34)/s14**2/s23/s24/
     #s34
      bh14 = -2.D0/3.D0*s15/s14*CF/s12/s34*(s13**2+s25**2+s15**2+s23**2)
      ch14 = 2.D0*CF*(-s13*s25+s15*s23+s12*s35)*(s13**2+s25**2)/s14*(-s2
     #4+2.D0*s34*CF*N+2.D0*s34)/s23/s12/s34/s24/N
	VTEMP(19) = AH14 + BH14 + CH14
c       qi + qbi --> g + g
      t1 = s13**2
      t8 = s25**2
      t9 = t8*s25
      t12 = s34*s14*s12
      t14 = s23**2
      t16 = t14*s23*s14
      t21 = s15**2
      t26 = t21*s15
      t29 = s24**2
      t31 = s14**2
      t33 = s34*s12
      t36 = N**2
      t37 = t1**2
      t39 = s14*s24
      t40 = t39*s23
      t43 = t29**2
      t49 = t31**2
      t59 = t21**2
      t68 = t39*s13
      t75 = -3.D0*t1*s13*s14*s23*s34*s12-s15*t9*t12-3.D0*t16*s13*s34*s12
     #-t21*t8*s24*s34*s12-t26*s25*t12-3.D0*t29*s24*t31*t33+3.D0*t36*t37*
     #t40+3.D0*t36*t43*t31*s13+3.D0*t36*t29*t49*s13+t36*t21*t8*t40-3.D0*
     #t49*s24*t33+t36*t59*t40+3.D0*t36*t1*t16*s24+t36*s15*t9*t68-t59*s24
     #*t33+t36*t26*s25*t68
      hh14 = Vc*t75/t31/s24/s23/s13/s34/s12/3.D0
	VTEMP(20) = HH14
	VH14(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      hh14 = 2.D0/3.D0*s15/s14*CF*(2.D0*CF*N*s14*s23-s13*s24-s14*s23+2.D
     #0*s34*s12)/s13/s12/s24/s23*(s15**2+s34**2+s14**2+s35**2)
	VTEMP(21) = HH14
c       qi + g --> qbk + qk
      hh14 = 2.D0/3.D0*s45/s14*CF*(2.D0*CF*N*s14*s23-s14*s23+2.D0*s13*s2
     #4-s34*s12)/s34/s12/s24/s23*(s13**2+s45**2+s14**2+s35**2)
	VTEMP(22) = HH14
	VH14(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      hh14 = 2.D0/3.D0*s15/s14*CF*(2.D0*s14*s23-s13*s24+2.D0*CF*N*s34*s1
     #2-s34*s12)/s13/s12/s24/s23*(s14**2+s35**2+s15**2+s34**2)
	VTEMP(23) = HH14
c       qi + g --> qk + qbk
      hh14 = 2.D0/3.D0*s45/s14*CF*(2.D0*CF*N*s13*s24-s13*s24+2.D0*s14*s2
     #3-s34*s12)/s34/s12/s23/s24*(s14**2+s35**2+s13**2+s45**2)
	VTEMP(24) = HH14
	VH14(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      ah14 = 2.D0/3.D0*s15/s14*CF*(2.D0*s14*s23-s13*s24+2.D0*CF*N*s34*s1
     #2-s34*s12)/s13/s12/s24/s23*(s14**2+s35**2+s15**2+s34**2)
      bh14 = 2.D0/3.D0*s45/s14*CF*(2.D0*CF*N*s13*s24+2.D0*s14*s23-s13*s2
     #4-s34*s12)/s34/s12/s24/s23*(s14**2+s35**2+s13**2+s45**2)
      ch14 = 2.D0*CF*(-s14*s35+s13*s45+s15*s34)*(s14**2+s35**2)*(2.D0*CF
     #*N*s14*s23+2.D0*s14*s23-s13*s24-s34*s12+2.D0*s12*s24*CF*N+2.D0*s12
     #*s24)/s14/s13/s34/s12/s24/s23/N
	VTEMP(25) = AH14 + BH14 + CH14
	VH14(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t1 = N**2
      t2 = s35**2
      t4 = s15**2
      t7 = s14*s23*s34
      t9 = t2*s35
      t11 = s24*s13
      t12 = t11*s14
      t14 = s34**2
      t15 = t14**2
      t17 = s14**2
      t21 = t4**2
      t29 = s12**2
      t30 = t29**2
      t34 = t17**2
      t40 = s23**2
      t41 = t40*s23
      t47 = s24*s14
      t51 = t4*s15
      t55 = s14*s12*s34
      t58 = s24*s34
      t76 = t1*t2*t4*t7-s15*t9*t12+3.D0*t1*t15*t17*s12+t1*t21*t7-3.D0*t1
     #4*s34*s13*s24*t17+3.D0*t1*t30*t7+3.D0*t1*t34*t14*s12+3.D0*t1*t29*t
     #41*s14*s34-3.D0*t41*s13*t47*s12+t1*t51*s35*t55-t21*s13*t58-3.D0*t3
     #4*s13*t58+t1*s15*t9*t55-t2*t4*t11*s34-3.D0*t29*s12*s13*t47*s23-t51
     #*s35*t12
      hh14 = Vc*t76/s13/s24/t17/s12/s23/s34/3.D0
	VTEMP(26) = HH14
c       qi + g --> g + g
      t1 = N**2
      t2 = s12**2
      t3 = t1*t2
      t4 = t3*s45
      t5 = s25**2
      t6 = t5*s24
      t9 = s13**2
      t10 = t1*t9
      t11 = t10*s45
      t12 = s35**2
      t21 = s45**2
      t23 = t1*t21*s45
      t24 = s24*s13
      t25 = s14**2
      t28 = t2**2
      t29 = t1*t28
      t31 = s45*s23*s14
      t33 = t25**2
      t34 = t1*t33
      t36 = s45*s24*s13
      t39 = s24*s34
      t42 = s45*s34*s12
      t44 = t9**2
      t45 = t1*t44
      t47 = t12*s35
      t51 = s34*s14*s12
      t58 = t9*s13
      t68 = t4*t6*s13+t11*t12*s23*s14-s15*t5*s24*s23*s14*s12+t23*t24*t25
     #+t29*t31+t34*t36-t28*s15*t39+t34*t42+t45*t42+3.D0*t1*t47*s13*t51+t
     #29*t36+t4*t5*s23*s14+3.D0*t1*t58*s35*t51-s15*t12*s23*s13*s34*s14
      t69 = t5*s25
      t76 = t2*s12
      t122 = 3.D0*t1*t69*s24*s13*s14*s12+3.D0*t1*t76*s25*t24*s14-t44*s15
     #*t39+t11*t12*s34*s12+t45*t31+3.D0*t29*s25*s34*s14+3.D0*t3*t69*s34*
     #s14-t76*s15*s24*s23*s14+t23*s34*t25*s12-t9*s15*t12*s24*s34-t58*s15
     #*s23*s34*s14-t2*s15*t6*s34+3.D0*t10*t47*s24*s14+3.D0*t45*s35*s24*s
     #14
      hh14 = Vc*(t68+t122)/s24/s23/s13/s34/t25/s12/3.D0
	VTEMP(27) = HH14
	VH14(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      hh14 = 0.D0
	VTEMP(28) = HH14
c       qi + g --> qbk + qi
      hh14 = 0.D0
	VTEMP(29) = HH14
c       qi + g --> qi + qi
      ah14 = 2.D0/3.D0*s15/s14*CF*(2.D0*CF*N*s14*s23-s13*s24-s14*s23+2.D
     #0*s34*s12)/s13/s12/s24/s23*(s15**2+s34**2+s14**2+s35**2)
      bh14 = 0.D0
      ch14 = 0.D0
	VTEMP(30) = AH14 + BH14 + CH14
c       qi + g --> qbi + qi
      ah14 = 2.D0/3.D0*s45/s14*CF*(2.D0*CF*N*s14*s23-s14*s23+2.D0*s13*s2
     #4-s34*s12)/s34/s12/s24/s23*(s13**2+s45**2+s14**2+s35**2)
      bh14 = 0.D0
      ch14 = 0.D0
	VTEMP(31) = AH14 + BH14 + CH14
c       qi + g --> g + qi
      t1 = s24**2
      t4 = s12**2
      t7 = s34**2
      t10 = s13**2
      t15 = N**2
      hh14 = -Vc*(t1*s24*s12+t4*s12*s24+t7*s34*s13+t10*s13*s34)*(t15*s34
     #*s12+t15*s13*s24-s14*s23-t15*s23*s14)/t15/s13/s34/s23/s24/s14/s12
	VTEMP(32) = HH14
	VH14(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t1 = N**2
      t2 = s24**2
      t3 = t2**2
      t6 = s14*s23*s13
      t9 = s45**2
      t10 = t9**2
      t13 = s13**2
      t16 = s14**2
      t22 = s14*s12
      t26 = s35**2
      t27 = t26*s35
      t31 = s14*s24*s13
      t34 = t16**2
      t38 = s23**2
      t39 = t38*s23
      t49 = t13**2
      t55 = s12*s13
      t64 = t9*s45
      t67 = s34*s14*s12
      t77 = 3.D0*t1*t3*t6+t1*t10*t6-3.D0*t13*s13*s34*t16*s12-3.D0*t2*s24
     #*s34*t22*s23+t1*t27*s45*t31+3.D0*t1*t13*t34*s24-3.D0*t39*s34*t22*s
     #24+3.D0*t1*t39*t2*s14*s13+3.D0*t1*t49*t16*s24-3.D0*t34*s34*t55-t10
     #*s34*t55-t26*t9*s34*s12*s13-s35*t64*t67+t1*t26*t9*t6-t27*s45*t67+t
     #1*s35*t64*t31
      hh14 = Vc*t77/s34/t16/s12/s24/s23/s13/3.D0
	VTEMP(33) = HH14
c       g + g --> g + qi
      t1 = N**2
      t2 = s14**2
      t3 = t2**2
      t4 = t1*t3
      t6 = s15*s24*s13
      t8 = s35**2
      t11 = s14*s34
      t14 = s15**2
      t16 = t1*t14*s15
      t20 = s34**2
      t21 = t20*s34
      t28 = t20**2
      t29 = t1*t28
      t31 = s24**2
      t32 = t1*t31
      t33 = t32*s15
      t34 = s25**2
      t38 = t1*t20
      t39 = t8*s35
      t44 = t38*s15
      t48 = t31**2
      t49 = t1*t48
      t51 = s15*s23*s14
      t53 = t31*s24
      t60 = s23*s14
      t64 = t34*s12
      t70 = t4*t6-t8*s45*s23*t11*s13+t16*t2*s24*s13+3.D0*t1*t21*s35*s14*
     #s24*s13+t29*t6+t33*t34*s23*s14+3.D0*t38*t39*s12*s14+t44*t8*s24*s13
     #+t49*t51-t53*s45*s12*s23*s14-t34*s45*s12*t60*s24+t29*t51+t33*t64*s
     #34+t16*s12*t2*s34
      t72 = s12*s13
      t80 = t34*s25
      t91 = s15*s12*s34
      t123 = -t48*s45*t72+3.D0*t1*t39*s14*s34*s24*s13+3.D0*t1*t80*s12*t1
     #1*s24+3.D0*t49*s25*s14*s13+t4*t91-t31*s45*t64*s13-t20*s45*t8*s12*s
     #13+t44*t8*s23*s14+t49*t91+3.D0*t29*s35*s12*s14-t21*s45*t60*s13+3.D
     #0*t32*t80*s14*s13+3.D0*t1*t53*s25*s12*s14*s34-t28*s45*t72
      hh14 = Vc*(t70+t123)/s12/s23/t2/s34/s24/s13/3.D0
	VTEMP(34) = HH14
	VH14(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t1 = s23**2
      t2 = t1*s23
      t6 = N**2
      t7 = s13*s24*t6
      t9 = s25**2
      t11 = t9*s25*s14
      t18 = s24*t6
      t21 = s13**2
      t22 = s15**2
      t23 = t22*s15
      t26 = s14*s24*t6
      t29 = s35*t9*s14
      t30 = s12*s23
      t31 = t30*t18
      t37 = t1**2
      t39 = s14*s12
      t40 = t39*t6
      t46 = s34**2
      t47 = t46**2
      t50 = t2*s35
      t53 = s12*s24*t6
      t62 = t2*s45*s12*t7+3.D0*t11*s23*t7+s45*t9*s12*s23*s13*t18+t21*t23
     #*t26-3.D0*t29*t31+t1*s15*t9*t26+t37*s45*t40+3.D0*t11*s34*t30*t6+t4
     #7*s45*t40-3.D0*t50*s14*t53-3.D0*t50*t39*s24+t2*s15*s34*t53
      t71 = t2*s25*s14
      t73 = s34*s12*t6
      t79 = s45**2
      t80 = t79*s45
      t88 = t21**2
      t105 = -3.D0*t29*t30*s24+t46*s34*s45*s14*t7+3.D0*t71*t73+s15*t9*s3
     #4*t31+t46*t80*t40+t37*s15*t26+t80*s14*s34*t7+t88*s15*t26+3.D0*t71*
     #t7+t23*s14*s34*s12*s13*t6+t1*s45*t9*t40+t21*s13*s15*s14*t73
      t108 = s14**2
      hh14 = -Vc*(t62+t105)/t108/s34/s12/s23/s13/s24/t6/3.D0
	VTEMP(35) = HH14
c       g + g --> g + g
      hh14 = 2.D0/s23*(s24*s13+s12*s34)*Vc*N**3*(s12**4+s13**4+s14**4+s1
     #5**4+s23**4+s24**4+s25**4+s34**4+s35**4+s45**4)/s12/s24/s34/s13/s1
     #4
	VTEMP(36) = HH14
	VH14(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
