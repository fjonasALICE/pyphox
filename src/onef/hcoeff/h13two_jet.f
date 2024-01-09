C coefficients devant p_1.p_3/(p_1.p_5 p_3.p_5)
C ----------------------------------------------------
	SUBROUTINE VEC_H13O(S12,S13,S14,S15,S23,S24,S25,S34,S35,S45,
     #	VH13)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VH13(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VH13(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      hh13 = -2.D0/s13*CF/s24*(s12**2+s34**2+s14**2+s23**2)
	VTEMP(1) = HH13
c       qi + qk --> g + qk
      hh13 = 2.D0/3.D0*CF*(s12**2+s45**2+s14**2+s25**2)*(2.D0*CF*N*s14*s
     #23*s35-s14*s23*s35+2.D0*s12*s34*s35-s24*s13*s35-s15*s34*s23+6.D0*C
     #F*N*s25*s13*s34-3.D0*s25*s13*s34+6.D0*s45*s13*s23)/s13**2/s24/s34/
     #s23
	VTEMP(2) = HH13
	VH13(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      hh13 = 0.D0
	VTEMP(3) = HH13
c       qi + qk --> qk + g
      hh13 = 2.D0/3.D0*s35/s13*CF*(2.D0*CF*N*s13*s24-s13*s24+2.D0*s12*s3
     #4-s23*s14)/s23/s14/s34/s24*(s12**2+s35**2+s13**2+s25**2)
	VTEMP(4) = HH13
	VH13(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      hh13 = -2.D0/s13*CF/s24*(s14**2+s23**2+s12**2+s34**2)
	VTEMP(5) = HH13
c       qi + qbk --> g + qbk
      hh13 = 2.D0/3.D0*CF*(s14**2+s25**2+s12**2+s45**2)*(2.D0*CF*N*s12*s
     #34*s35-s12*s34*s35+2.D0*s14*s23*s35-s24*s13*s35-s15*s23*s34+6.D0*C
     #F*N*s45*s13*s23-3.D0*s45*s13*s23+6.D0*s25*s13*s34)/s13**2/s24/s23/
     #s34
	VTEMP(6) = HH13
	VH13(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      hh13 = 0.D0
	VTEMP(7) = HH13
c       qi + qbk --> qbk + g
      hh13 = 2.D0/3.D0*s35/s13*CF*(2.D0*CF*N*s12*s34-s12*s34+2.D0*s13*s2
     #4-s23*s14)/s23/s14/s24/s34*(s13**2+s25**2+s12**2+s35**2)
	VTEMP(8) = HH13
	VH13(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      ah13 = -2.D0/s13*CF/s24*(s12**2+s34**2+s14**2+s23**2)
      bh13 = 2.D0*CF*(2.D0*CF*N-1.D0)*(s12**2+s34**2+s13**2+s24**2)/s14/
     #s23
      ch13 = 2.D0/s13*CF/s24/s14/s23/N*(s12*s34-s13*s24-s14*s23)*(s12**2
     #+s34**2)
	VTEMP(9) = AH13 + BH13 + CH13
c       qi + qi --> g + qi
      ah13 = 2.D0/3.D0*CF*(s12**2+s45**2+s14**2+s25**2)*(2.D0*CF*N*s14*s
     #23*s35-s14*s23*s35+2.D0*s12*s34*s35-s24*s13*s35-s15*s34*s23+6.D0*C
     #F*N*s25*s13*s34-3.D0*s25*s13*s34+6.D0*s45*s13*s23)/s13**2/s24/s34/
     #s23
      bh13 = -2.D0/3.D0*s15/s13*CF/s14/s23*(s12**2+s45**2+s15**2+s24**2)
      ch13 = -2.D0*CF*(s12*s45-s15*s24-s14*s25)*(s12**2+s45**2)/s13*(-s3
     #4+2.D0*s23*CF*N+2.D0*s23)/s24/s14/s23/s34/N
	VTEMP(10) = AH13 + BH13 + CH13
	VH13(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      ah13 = 0.D0
      bh13 = 2.D0/3.D0*s35/s13*CF*(2.D0*CF*N*s13*s24+2.D0*s12*s34-s13*s2
     #4-s23*s14)/s23/s14/s24/s34*(s12**2+s35**2+s13**2+s25**2)
      ch13 = 0.D0
	VTEMP(11) = AH13 + BH13 + CH13
	VH13(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      hh13 = 2.D0*CF*(2.D0*CF*N-1.D0)*(s14**2+s23**2+s13**2+s24**2)/s12/
     #s34
	VTEMP(12) = HH13
c       qi + qbi --> g + qbk
      hh13 = -2.D0/3.D0*s15/s13*CF/s12/s34*(s14**2+s25**2+s15**2+s24**2)
	VTEMP(13) = HH13
	VH13(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      ah13 = -2.D0/s13*CF/s24*(s14**2+s23**2+s12**2+s34**2)
      bh13 = 2.D0*CF*(2.D0*CF*N-1.D0)*(s14**2+s23**2+s13**2+s24**2)/s12/
     #s34
      ch13 = -2.D0/s13*CF/s24/s12/s34/N*(-s14*s23+s13*s24+s12*s34)*(s14*
     #*2+s23**2)
	VTEMP(14) = AH13 + BH13 + CH13
c       qi + qbi --> g + qbi
      ah13 = 2.D0/3.D0*CF*(s14**2+s25**2+s12**2+s45**2)*(2.D0*CF*N*s12*s
     #34*s35-s12*s34*s35+2.D0*s14*s23*s35-s24*s13*s35-s15*s23*s34+6.D0*C
     #F*N*s45*s13*s23-3.D0*s45*s13*s23+6.D0*s25*s13*s34)/s13**2/s24/s23/
     #s34
      bh13 = -2.D0/3.D0*s15/s13*CF/s12/s34*(s14**2+s25**2+s15**2+s24**2)
      ch13 = 2.D0*CF*(-s14*s25+s15*s24+s12*s45)*(s14**2+s25**2)/s13*(-s2
     #3+2.D0*s34*CF*N+2.D0*s34)/s24/s12/s34/s23/N
	VTEMP(15) = AH13 + BH13 + CH13
	VH13(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      hh13 = 2.D0/3.D0*s15/s13*CF*(2.D0*CF*N*s13*s24-s12*s34-s13*s24+2.D
     #0*s23*s14)/s12/s14/s34/s24*(s15**2+s23**2+s13**2+s25**2)
	VTEMP(16) = HH13
c       qi + qbi --> qbk + g
      hh13 = 2.D0/3.D0*s15/s13*CF*(2.D0*s13*s24-s12*s34+2.D0*CF*N*s23*s1
     #4-s23*s14)/s12/s14/s34/s24*(s13**2+s25**2+s15**2+s23**2)
	VTEMP(17) = HH13
c       qi + qbi --> qi + g
      ah13 = 0.D0
      bh13 = 2.D0/3.D0*s15/s13*CF*(2.D0*CF*N*s13*s24-s13*s24-s12*s34+2.D
     #0*s23*s14)/s12/s14/s24/s34*(s15**2+s23**2+s13**2+s25**2)
      ch13 = 0.D0
	VTEMP(18) = AH13 + BH13 + CH13
c       qi + qbi --> qbi + g
      ah13 = 2.D0/3.D0*s35/s13*CF*(2.D0*CF*N*s12*s34-s12*s34+2.D0*s13*s2
     #4-s23*s14)/s23/s14/s24/s34*(s13**2+s25**2+s12**2+s35**2)
      bh13 = 2.D0/3.D0*s15/s13*CF*(2.D0*CF*N*s23*s14-s12*s34+2.D0*s13*s2
     #4-s23*s14)/s12/s14/s24/s34*(s13**2+s25**2+s15**2+s23**2)
      ch13 = 2.D0*CF*(-s13*s25+s15*s23+s12*s35)*(s13**2+s25**2)*(2.D0*CF
     #*N*s13*s24-s12*s34+2.D0*s13*s24-s23*s14+2.D0*s14*s34*CF*N+2.D0*s14
     #*s34)/s13/s23/s12/s14/s34/s24/N
	VTEMP(19) = AH13 + BH13 + CH13
c       qi + qbi --> g + g
      t1 = N**2
      t2 = s14**2
      t3 = t2**2
      t5 = s23*s13
      t6 = t5*s24
      t10 = s24**2
      t11 = t10*s24
      t17 = s13*s12
      t21 = s15**2
      t22 = t21*s15
      t25 = s34*s13*s12
      t28 = s25**2
      t29 = t28*s25
      t31 = t5*s14
      t33 = s23**2
      t34 = t33**2
      t36 = s13**2
      t40 = t36**2
      t46 = s12*s23
      t49 = t21**2
      t76 = 3.D0*t1*t3*t6+3.D0*t1*t2*t11*s13*s23-3.D0*t11*s34*t17*s14-t2
     #2*s25*t25+t1*s15*t29*t31+3.D0*t1*t34*t36*s14+3.D0*t1*t40*t33*s14-3
     #.D0*t40*s34*t46-t49*s34*t46-3.D0*t2*s14*s34*t17*s24-3.D0*t33*s23*s
     #34*t36*s12+t1*t22*s25*t31+t1*t21*t28*t6-s15*t29*t25+t1*t49*t6-t21*
     #t28*s34*s12*s23
      hh13 = Vc*t76/s34/t36/s12/s23/s14/s24/3.D0
	VTEMP(20) = HH13
	VH13(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      hh13 = 0.D0
	VTEMP(21) = HH13
c       qi + g --> qbk + qk
      hh13 = 2.D0/3.D0*s35/s13*CF*(2.D0*CF*N*s14*s23-s14*s23+2.D0*s13*s2
     #4-s34*s12)/s34/s12/s24/s23*(s13**2+s45**2+s14**2+s35**2)
	VTEMP(22) = HH13
	VH13(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      hh13 = 0.D0
	VTEMP(23) = HH13
c       qi + g --> qk + qbk
      hh13 = 2.D0/3.D0*s35/s13*CF*(2.D0*CF*N*s13*s24-s13*s24+2.D0*s14*s2
     #3-s34*s12)/s34/s12/s23/s24*(s14**2+s35**2+s13**2+s45**2)
	VTEMP(24) = HH13
	VH13(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      ah13 = 0.D0
      bh13 = 2.D0/3.D0*s35/s13*CF*(2.D0*CF*N*s13*s24+2.D0*s14*s23-s13*s2
     #4-s34*s12)/s34/s12/s24/s23*(s14**2+s35**2+s13**2+s45**2)
      ch13 = 0.D0
	VTEMP(25) = AH13 + BH13 + CH13
	VH13(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t1 = s23**2
      t4 = s12**2
      t7 = s34**2
      t10 = s14**2
      t15 = N**2
      t20 = s24*s13
      hh13 = -Vc*(t1*s23*s12+t4*s12*s23+t7*s34*s14+t10*s14*s34)*(s34*t15
     #*s12+s14*t15*s23-t20-t20*t15)/s24/s23/s13/s12/s14/s34/t15
	VTEMP(26) = HH13
c       qi + g --> g + g
      t1 = N**2
      t2 = s12**2
      t3 = t2*s12
      t6 = s23*s13
      t10 = s35**2
      t12 = t1*t10*s35
      t13 = s13**2
      t17 = s45**2
      t20 = s13*s34
      t23 = s14**2
      t24 = t1*t23
      t25 = t24*s35
      t29 = t17*s45
      t34 = t23**2
      t35 = t1*t34
      t37 = s35*s34*s12
      t42 = t2**2
      t43 = t1*t42
      t45 = s35*s24*s13
      t47 = t23*s14
      t53 = s25**2
      t54 = t53*s25
      t70 = t13**2
      t71 = t1*t70
      t73 = s35*s23*s14
      t75 = 3.D0*t1*t3*s25*t6*s14+t12*s23*t13*s14-s15*t17*s24*t20*s14+t2
     #5*t17*s34*s12+3.D0*t24*t29*s23*s13+t35*t37+t12*t13*s34*s12+t43*t45
     #+3.D0*t1*t47*s45*t20*s12+3.D0*t1*t54*s23*s13*s12*s14-s15*t53*s24*t
     #6*s12-t3*s15*s24*s23*s13+t35*t45+t71*t73
      t80 = t1*t2
      t86 = t80*s35
      t87 = t53*s23
      t103 = s23*s34
      t123 = -t23*s15*t17*s23*s34+3.D0*t80*t54*s13*s34+t43*t73+t86*t87*s
     #14-t47*s15*s24*s13*s34+3.D0*t43*s25*s13*s34+3.D0*t35*s45*s23*s13-t
     #34*s15*t103-t42*s15*t103+t25*t17*s24*s13-t2*s15*t87*s34+t71*t37+3.
     #D0*t1*t29*s13*s34*s12*s14+t86*t53*s24*s13
      hh13 = Vc*(t75+t123)/s24/s23/t13/s34/s12/s14/3.D0
	VTEMP(27) = HH13
	VH13(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      hh13 = 2.D0/3.D0*s15/s13*CF*(2.D0*CF*N*s13*s24-s14*s23-s13*s24+2.D
     #0*s34*s12)/s14/s12/s23/s24*(s15**2+s34**2+s13**2+s45**2)
	VTEMP(28) = HH13
c       qi + g --> qbk + qi
      hh13 = 2.D0/3.D0*s15/s13*CF*(2.D0*s13*s24-s14*s23+2.D0*CF*N*s34*s1
     #2-s34*s12)/s14/s12/s23/s24*(s13**2+s45**2+s15**2+s34**2)
	VTEMP(29) = HH13
c       qi + g --> qi + qi
      ah13 = 0.D0
      bh13 = 2.D0/3.D0*s15/s13*CF*(2.D0*CF*N*s13*s24-s13*s24-s14*s23+2.D
     #0*s34*s12)/s14/s12/s24/s23*(s15**2+s34**2+s13**2+s45**2)
      ch13 = 0.D0
	VTEMP(30) = AH13 + BH13 + CH13
c       qi + g --> qbi + qi
      ah13 = 2.D0/3.D0*s35/s13*CF*(2.D0*CF*N*s14*s23-s14*s23+2.D0*s13*s2
     #4-s34*s12)/s34/s12/s24/s23*(s13**2+s45**2+s14**2+s35**2)
      bh13 = 2.D0/3.D0*s15/s13*CF*(2.D0*CF*N*s34*s12-s14*s23+2.D0*s13*s2
     #4-s34*s12)/s14/s12/s24/s23*(s13**2+s45**2+s15**2+s34**2)
      ch13 = -2.D0*CF*(s13*s45-s15*s34-s14*s35)*(s13**2+s45**2)*(2.D0*CF
     #*N*s13*s24-s14*s23+2.D0*s13*s24-s34*s12+2.D0*s12*s23*CF*N+2.D0*s12
     #*s23)/s13/s34/s14/s12/s24/s23/N
	VTEMP(31) = AH13 + BH13 + CH13
c       qi + g --> g + qi
      t1 = s15**2
      t2 = t1**2
      t4 = s23*s34
      t6 = s13**2
      t7 = t6**2
      t11 = s34**2
      t14 = s14*s23
      t17 = N**2
      t18 = t11**2
      t23 = s45**2
      t24 = t23*s45
      t27 = s14*s23*s13
      t31 = s24*s13*s34
      t33 = t1*s15
      t37 = s12*s13*s34
      t39 = s24**2
      t40 = t39*s24
      t44 = s12**2
      t50 = t44**2
      t73 = -t2*s14*t4-3.D0*t7*s14*t4-3.D0*t11*s34*t6*t14+3.D0*t17*t18*s
     #12*t6-s15*t24*t27+t17*t2*t31+t17*t33*s45*t37-3.D0*t40*s12*t27+3.D0
     #*t17*t44*t40*s13*s34+3.D0*t17*t50*t31+3.D0*t17*t7*t11*s12-t1*t23*t
     #14*s34-3.D0*t44*s12*s24*t27+t17*s15*t24*t37-t33*s45*t27+t17*t1*t23
     #*t31
      hh13 = Vc*t73/s12/s24/t6/s14/s23/s34/3.D0
	VTEMP(32) = HH13
	VH13(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t1 = s24**2
      t2 = t1*s24
      t8 = s14**2
      t10 = s13**2
      t12 = s34*s12
      t15 = N**2
      t17 = t10**2
      t21 = s35**2
      t22 = s45**2
      t27 = s23**2
      t33 = t21*s35
      t36 = s13*s34*s12
      t38 = t21**2
      t47 = t22*s45
      t52 = s14*s13
      t53 = t52*s24
      t55 = t27**2
      t61 = t52*s23
      t69 = t8**2
      t76 = -3.D0*t2*s13*s23*s34*s12-3.D0*t8*s14*t10*t12+3.D0*t15*t8*t17
     #*s23-t21*t22*s14*s34*s12+3.D0*t15*t27*t2*s14*s13-t33*s45*t36-t38*s
     #14*t12-3.D0*t27*s23*s13*s24*s34*s12-s35*t47*t36+t15*t21*t22*t53+3.
     #D0*t15*t55*t53+t15*t33*s45*t61+t15*s35*t47*t61-3.D0*t17*s14*t12+3.
     #D0*t15*t69*t10*s23+t15*t38*t53
      hh13 = Vc*t76/s14/t10/s24/s23/s34/s12/3.D0
	VTEMP(33) = HH13
c       g + g --> g + qi
      t1 = s35**2
      t2 = t1*s35
      t6 = N**2
      t7 = s14*s34*t6
      t9 = s24**2
      t10 = t9*s24
      t12 = t10*s25*s13
      t14 = s12*s34*t6
      t18 = s25**2
      t21 = s13*s23*t6
      t23 = s34**2
      t25 = s13*s12
      t26 = t25*t6
      t28 = s15**2
      t29 = t28*s15
      t33 = t9**2
      t38 = s14**2
      t44 = s23*s14*t6
      t58 = t2*s13*s23*t7+3.D0*t12*t14+t9*s15*t18*t21+t23*t2*t26+t29*s13
     #*s12*t7+t33*s15*t21+t33*s35*t26+t38*s14*s15*s13*t14+3.D0*t12*t44+t
     #9*s35*t18*t26+t10*s15*s12*s23*s34*t6+t10*s35*s12*t44
      t61 = s12*s23
      t65 = t23**2
      t75 = t18*s25*s13*s24
      t82 = t38**2
      t85 = t10*s45
      t90 = t18*s45*s13
      t91 = s24*s12
      t107 = s35*t18*s24*t61*s14*t6+t65*s35*t26+s15*t18*s24*t61*s34*t6+3
     #.D0*t75*t44+3.D0*t75*t14+t38*t29*t21+t82*s15*t21-3.D0*t85*t25*s23-
     #3.D0*t90*t91*s23-3.D0*t85*s13*t61*t6-3.D0*t90*t91*s23*t6+t23*s34*s
     #35*s13*t44
      t110 = s13**2
      hh13 = -Vc*(t58+t107)/t110/s24/s12/s23/s14/s34/t6/3.D0
	VTEMP(34) = HH13
	VH13(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t1 = s34**2
      t2 = t1**2
      t4 = s12*s14
      t6 = s23**2
      t7 = t6**2
      t10 = N**2
      t11 = t10*t6
      t12 = t11*s15
      t13 = s25**2
      t14 = t13*s12
      t17 = t10*t2
      t19 = s15*s23*s14
      t21 = t6*s23
      t24 = s13*s12
      t25 = t24*s34
      t28 = s15**2
      t30 = t10*t28*s15
      t31 = s13**2
      t35 = t13*s25
      t40 = t10*t1
      t41 = t40*s15
      t42 = s45**2
      t54 = t24*s24
      t63 = t1*s34
      t68 = -t2*s35*t4-t7*s35*t4+t12*t14*s34+t17*t19+3.D0*t10*t21*s25*t2
     #5+t30*s23*t31*s14+3.D0*t11*t35*s13*s14+t41*t42*s23*s14+t41*t42*s13
     #*s24+t12*t13*s13*s24-s35*t13*s23*t54-s35*t42*s13*s24*s14*s34-t21*s
     #35*t54-t63*s35*s13*s24*s14
      t72 = t10*t7
      t77 = t31**2
      t78 = t10*t77
      t80 = s15*s12*s34
      t90 = s15*s13*s24
      t93 = t42*s45
      t120 = t30*t31*s12*s34+3.D0*t72*s25*s13*s14+t78*t80-t1*s35*t42*s12
     #*s14-t6*s35*t14*s14+t17*t90+t72*t80+3.D0*t10*t93*s23*s13*s14*s34+3
     #.D0*t17*s45*s13*s12+t78*t19+3.D0*t40*t93*s13*s12+t72*t90+3.D0*t10*
     #t63*s45*s23*s13*s14+3.D0*t10*t35*s23*t25
      hh13 = Vc*(t68+t120)/s23/t31/s12/s24/s14/s34/3.D0
	VTEMP(35) = HH13
c       g + g --> g + g
      hh13 = 2.D0/s13*(s23*s14+s12*s34)*Vc*N**3*(s12**4+s13**4+s14**4+s1
     #5**4+s23**4+s24**4+s25**4+s34**4+s35**4+s45**4)/s12/s24/s34/s23/s1
     #4
	VTEMP(36) = HH13
	VH13(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
