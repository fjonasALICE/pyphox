C coefficients devant p_3.p_4/(p_3.p_5 p_4.p_5)
C ----------------------------------------------------
	SUBROUTINE VEC_H34O(S12,S13,S14,S15,S23,S24,S25,S34,S35,S45,
     #	VH34)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VH34(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VH34(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      hh34 = 4.D0/s13*CF/s24*(s12**2+s34**2+s14**2+s23**2)
	VTEMP(1) = HH34
c       qi + qk --> g + qk
      hh34 = -2.D0/3.D0*s45/s34*CF/s24/s13*(s12**2+s45**2+s14**2+s25**2)
	VTEMP(2) = HH34
	VH34(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      hh34 = -2.D0/3.D0*s35/s34*CF/s13/s24*(s12**2+s35**2+s15**2+s23**2)
	VTEMP(3) = HH34
c       qi + qk --> qk + g
      hh34 = -2.D0/3.D0*s35/s34*CF/s23/s14*(s12**2+s35**2+s13**2+s25**2)
	VTEMP(4) = HH34
	VH34(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      hh34 = 2.D0*CF*(2.D0*CF*N-1.D0)*(s14**2+s23**2+s12**2+s34**2)/s13/
     #s24
	VTEMP(5) = HH34
c       qi + qbk --> g + qbk
      hh34 = -2.D0/3.D0*s45/s34*CF/s24/s13*(s14**2+s25**2+s12**2+s45**2)
	VTEMP(6) = HH34
	VH34(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      hh34 = -2.D0/3.D0*s35/s34*CF/s13/s24*(s15**2+s23**2+s12**2+s35**2)
	VTEMP(7) = HH34
c       qi + qbk --> qbk + g
      hh34 = -2.D0/3.D0*s35/s34*CF/s23/s14*(s13**2+s25**2+s12**2+s35**2)
	VTEMP(8) = HH34
	VH34(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      ah34 = 4.D0/s13*CF/s24*(s12**2+s34**2+s14**2+s23**2)
      bh34 = 4.D0*CF/s14/s23*(s12**2+s34**2+s13**2+s24**2)
      ch34 = -4.D0*CF*(CF*N+1.D0)*(s12*s34-s13*s24-s14*s23)*(s12**2+s34*
     #*2)/s13/s24/s14/s23/N
	VTEMP(9) = AH34 + BH34 + CH34
c       qi + qi --> g + qi
      ah34 = -2.D0/3.D0*s45/s34*CF/s24/s13*(s12**2+s45**2+s14**2+s25**2)
      bh34 = -2.D0/3.D0*s45/s34*CF/s14/s23*(s12**2+s45**2+s15**2+s24**2)
      ch34 = 0.D0
	VTEMP(10) = AH34 + BH34 + CH34
	VH34(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      ah34 = -2.D0/3.D0*s35/s34*CF/s13/s24*(s12**2+s35**2+s15**2+s23**2)
      bh34 = -2.D0/3.D0*s35/s34*CF/s23/s14*(s12**2+s35**2+s13**2+s25**2)
      ch34 = 0.D0
	VTEMP(11) = AH34 + BH34 + CH34
	VH34(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      hh34 = -2.D0/s12*CF/s34*(s14**2+s23**2+s13**2+s24**2)
	VTEMP(12) = HH34
c       qi + qbi --> g + qbk
      hh34 = 2.D0/3.D0*CF*(s14**2+s25**2+s15**2+s24**2)*(2.D0*s14*s35*s2
     #3-s12*s35*s34+2.D0*CF*N*s24*s13*s35-s24*s13*s35-s45*s13*s23+6.D0*C
     #F*N*s15*s34*s23-3.D0*s15*s34*s23+6.D0*s25*s13*s34)/s34**2/s12/s13/
     #s23
	VTEMP(13) = HH34
	VH34(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      ah34 = 2.D0*CF*(2.D0*CF*N-1.D0)*(s14**2+s23**2+s12**2+s34**2)/s13/
     #s24
      bh34 = -2.D0*CF/s12/s34*(s14**2+s23**2+s13**2+s24**2)
      ch34 = -2.D0/s13*CF/s24/s12/s34/N*(-s14*s23+s13*s24+s12*s34)*(s14*
     #*2+s23**2)
	VTEMP(14) = AH34 + BH34 + CH34
c       qi + qbi --> g + qbi
      ah34 = -2.D0/3.D0*s45/s34*CF/s24/s13*(s14**2+s25**2+s12**2+s45**2)
      bh34 = 2.D0/3.D0*CF*(s14**2+s25**2+s15**2+s24**2)*(2.D0*CF*N*s24*s
     #13*s35-s12*s34*s35+2.D0*s14*s23*s35-s24*s13*s35-s45*s13*s23+6.D0*C
     #F*N*s15*s23*s34+6.D0*s25*s13*s34-3.D0*s15*s23*s34)/s34**2/s12/s13/
     #s23
      ch34 = -2.D0*CF*(-s14*s25+s15*s24+s12*s45)*(s14**2+s25**2)/s34*(s2
     #3+2.D0*s13*CF*N+2.D0*s13)/s24/s12/s13/s23/N
	VTEMP(15) = AH34 + BH34 + CH34
	VH34(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      hh34 = 2.D0/3.D0*CF*(s15**2+s23**2+s13**2+s25**2)*(2.D0*CF*N*s13*s
     #45*s24-s12*s34*s45-s13*s45*s24+2.D0*s23*s14*s45-s35*s14*s24+6.D0*C
     #F*N*s25*s14*s34+6.D0*s15*s34*s24-3.D0*s25*s14*s34)/s34**2/s12/s14/
     #s24
	VTEMP(16) = HH34
c       qi + qbi --> qbk + g
      hh34 = 2.D0/3.D0*CF*(s13**2+s25**2+s15**2+s23**2)*(2.D0*s13*s45*s2
     #4-s12*s45*s34+2.D0*CF*N*s23*s14*s45-s23*s14*s45-s35*s14*s24+6.D0*C
     #F*N*s15*s34*s24-3.D0*s15*s34*s24+6.D0*s25*s14*s34)/s34**2/s12/s14/
     #s24
	VTEMP(17) = HH34
c       qi + qbi --> qi + g
      ah34 = -2.D0/3.D0*s35/s34*CF/s13/s24*(s15**2+s23**2+s12**2+s35**2)
      bh34 = 2.D0/3.D0*CF*(s15**2+s23**2+s13**2+s25**2)*(2.D0*CF*N*s13*s
     #24*s45-s13*s24*s45-s12*s45*s34+2.D0*s23*s14*s45-s35*s14*s24+6.D0*C
     #F*N*s25*s14*s34+6.D0*s15*s24*s34-3.D0*s25*s14*s34)/s34**2/s12/s14/
     #s24
      ch34 = -2.D0*CF*(-s15*s23+s13*s25+s12*s35)*(s15**2+s23**2)/s34*(s1
     #4+2.D0*s24*CF*N+2.D0*s24)/s13/s12/s14/s24/N
	VTEMP(18) = AH34 + BH34 + CH34
c       qi + qbi --> qbi + g
      ah34 = -2.D0/3.D0*s35/s34*CF/s23/s14*(s13**2+s25**2+s12**2+s35**2)
      bh34 = 2.D0/3.D0*CF*(s13**2+s25**2+s15**2+s23**2)*(2.D0*CF*N*s23*s
     #14*s45-s12*s34*s45+2.D0*s13*s24*s45-s23*s14*s45-s35*s14*s24+6.D0*C
     #F*N*s15*s24*s34+6.D0*s25*s14*s34-3.D0*s15*s24*s34)/s34**2/s12/s14/
     #s24
      ch34 = -2.D0*CF*(-s13*s25+s15*s23+s12*s35)*(s13**2+s25**2)/s34*(s2
     #4+2.D0*s14*CF*N+2.D0*s14)/s23/s12/s14/s24/N
	VTEMP(19) = AH34 + BH34 + CH34
c       qi + qbi --> g + g
      t1 = N**2
      t2 = t1*s45
      t3 = s15**2
      t4 = t3*s15
      t6 = s23*s13
      t9 = t1*s35
      t10 = s25**2
      t11 = t10*s25
      t17 = s13*s25
      t18 = s14*s23
      t23 = s14*s24
      t27 = s34*s12
      t28 = t27*s24
      t33 = s13**2
      t34 = t33**2
      t36 = s34*s23
      t37 = t36*s24
      t41 = t27*s14
      t44 = t27*s13
      t52 = s23**2
      t53 = t52**2
      t55 = s34*s13
      t56 = t55*s14
      t62 = s14**2
      t64 = s24**2
      t70 = t2*t4*t6*s24+t9*t11*s13*s14*s24+t2*t3*t17*t18+t9*s15*t10*s23
     #*t23-s35*t4*t28+t9*t3*t17*t23+3.D0*t1*t34*t37-s35*t11*t41-s45*t11*
     #t44-s35*s15*t10*t28-s35*t3*s25*t41+3.D0*t1*t53*t56+t9*t4*t18*s24+3
     #.D0*t1*t62*t64*s24*s34*s23
      t78 = t1*s15*t11
      t79 = t36*s14
      t83 = t27*s23
      t86 = t1*t4*s25
      t89 = t55*s24
      t105 = t64**2
      t124 = t62**2
      t128 = 3.D0*t1*t33*t52*s23*s34*s24+3.D0*t78*t79-s45*t4*t83+3.D0*t8
     #6*t79+3.D0*t86*t89+3.D0*t1*t64*t62*s14*s34*s13+3.D0*t78*t89+t2*s15
     #*t10*s13*s23*s24+3.D0*t1*t105*t56+t2*t11*t6*s14-s45*t3*s25*t44-s45
     #*s15*t10*t83+3.D0*t1*t33*s13*t52*s34*s14+3.D0*t1*t124*t37
      t132 = s34**2
      hh34 = Vc*(t70+t128)/s12/t132/s13/s23/s14/s24/3.D0
	VTEMP(20) = HH34
	VH34(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      hh34 = 2.D0/3.D0*s35/s34*CF*(2.D0*CF*N*s14*s23-s13*s24-s14*s23+2.D
     #0*s34*s12)/s13/s12/s24/s23*(s15**2+s34**2+s14**2+s35**2)
	VTEMP(21) = HH34
c       qi + g --> qbk + qk
      hh34 = 0.D0
	VTEMP(22) = HH34
	VH34(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      hh34 = 2.D0/3.D0*s35/s34*CF*(2.D0*s14*s23-s13*s24+2.D0*CF*N*s34*s1
     #2-s34*s12)/s13/s12/s24/s23*(s14**2+s35**2+s15**2+s34**2)
	VTEMP(23) = HH34
c       qi + g --> qk + qbk
      hh34 = 0.D0
	VTEMP(24) = HH34
	VH34(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      ah34 = 2.D0/3.D0*s35/s34*CF*(2.D0*s14*s23-s13*s24+2.D0*CF*N*s34*s1
     #2-s34*s12)/s13/s12/s24/s23*(s14**2+s35**2+s15**2+s34**2)
      bh34 = 0.D0
      ch34 = 0.D0
	VTEMP(25) = AH34 + BH34 + CH34
	VH34(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t1 = s35**2
      t2 = s15**2
      t7 = t2*s15
      t10 = s34*s13*s24
      t12 = s12**2
      t13 = t12*s12
      t17 = N**2
      t19 = t1*s35
      t22 = s23*s34*s14
      t29 = s14**2
      t31 = s34**2
      t33 = s24*s13
      t39 = s12*s34*s14
      t42 = s23**2
      t47 = t1**2
      t50 = t42**2
      t56 = t31**2
      t68 = t29**2
      t73 = -t1*t2*s14*s13*s24-t7*s35*t10-3.D0*t13*s23*t10+t17*s15*t19*t
     #22-s15*t19*t10+t17*t7*s35*t22-3.D0*t29*s14*t31*t33+t17*t1*t2*t39+3
     #.D0*t17*t13*t42*s34*s14+t17*t47*t39+3.D0*t17*t50*t39-t47*s14*t33-3
     #.D0*t56*s14*t33+3.D0*t17*t29*t56*s23-3.D0*t42*s23*s12*t10+3.D0*t17
     #*t68*s23*t31
      hh34 = Vc*t73/s12/s23/t31/s14/s13/s24/3.D0
	VTEMP(26) = HH34
c       qi + g --> g + g
      t1 = s12**2
      t2 = t1*s12
      t3 = t2*s25
      t6 = N**2
      t7 = s23*s14*t6
      t10 = s14**2
      t15 = s13*s34*t6
      t18 = s25**2
      t21 = s34*s23*t6
      t23 = t1**2
      t26 = s35**2
      t27 = t26*s35
      t34 = s23*t6
      t37 = s13**2
      t39 = s34*s24
      t40 = t39*t6
      t43 = s15*t18*s24
      t44 = s34*s12
      t48 = t37**2
      t51 = t18*s25
      t64 = 3.D0*t3*s34*t7+t10*s14*s45*s24*t15+t1*s45*t18*t21+t23*s45*t2
     #1+t27*s13*s34*t7+s45*t18*s24*s13*s12*t34+t37*t27*t40-3.D0*t43*t44*
     #t34+t48*s35*t40+3.D0*t51*s34*s12*t7+t2*s35*s24*t7+t2*s45*s24*s13*s
     #23*t6
      t69 = s45**2
      t70 = t69*s45
      t76 = t10**2
      t87 = t2*s15
      t110 = t37*s13*s35*s34*t7+t10*t70*t21+t1*s35*t18*t40+t76*s45*t21+t
     #23*s35*t40+s35*t18*s24*s12*s23*s14*t6-3.D0*t87*s24*t21+3.D0*t51*s2
     #4*s13*t44*t6+t70*s24*s13*s34*s14*t6-3.D0*t87*t39*s23+3.D0*t3*s24*t
     #15-3.D0*t43*t44*s23
      t117 = s34**2
      hh34 = -Vc*(t64+t110)/s24/s13/t117/s12/s23/s14/t6/3.D0
	VTEMP(27) = HH34
	VH34(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      hh34 = 2.D0/3.D0*s45/s34*CF*(2.D0*CF*N*s13*s24-s14*s23-s13*s24+2.D
     #0*s34*s12)/s14/s12/s23/s24*(s15**2+s34**2+s13**2+s45**2)
	VTEMP(28) = HH34
c       qi + g --> qbk + qi
      hh34 = 2.D0/3.D0*s45/s34*CF*(2.D0*s13*s24-s14*s23+2.D0*CF*N*s34*s1
     #2-s34*s12)/s14/s12/s23/s24*(s13**2+s45**2+s15**2+s34**2)
	VTEMP(29) = HH34
c       qi + g --> qi + qi
      ah34 = 2.D0/3.D0*s35/s34*CF*(2.D0*CF*N*s14*s23-s13*s24-s14*s23+2.D
     #0*s34*s12)/s13/s12/s24/s23*(s15**2+s34**2+s14**2+s35**2)
      bh34 = 2.D0/3.D0*s45/s34*CF*(2.D0*CF*N*s13*s24-s13*s24-s14*s23+2.D
     #0*s34*s12)/s14/s12/s24/s23*(s15**2+s34**2+s13**2+s45**2)
      ch34 = 2.D0*CF*(-s15*s34+s13*s45+s14*s35)*(s15**2+s34**2)*(2.D0*CF
     #*N*s34*s12-s13*s24-s14*s23+2.D0*s34*s12-2.D0*s23*s24*CF*N-2.D0*s23
     #*s24)/s34/s13/s14/s12/s23/s24/N
	VTEMP(30) = AH34 + BH34 + CH34
c       qi + g --> qbi + qi
      ah34 = 0.D0
      bh34 = 2.D0/3.D0*s45/s34*CF*(2.D0*CF*N*s34*s12-s14*s23+2.D0*s13*s2
     #4-s34*s12)/s14/s12/s24/s23*(s13**2+s45**2+s15**2+s34**2)
      ch34 = 0.D0
	VTEMP(31) = AH34 + BH34 + CH34
c       qi + g --> g + qi
      t1 = s24**2
      t8 = s45**2
      t9 = s15**2
      t14 = t8**2
      t16 = s14*s23
      t18 = N**2
      t21 = s34*s13
      t22 = t21*s12
      t24 = t8*s45
      t27 = s34*s14*s23
      t29 = s12**2
      t30 = t29*s12
      t36 = t9*s15
      t39 = t21*s24
      t48 = s34**2
      t49 = t48**2
      t53 = s13**2
      t62 = t53**2
      t67 = t1**2
      t76 = -3.D0*t1*s24*s34*s12*s14*s23-t8*t9*s13*s14*s23-t14*s13*t16+t
     #18*t8*t9*t22-s15*t24*t27-3.D0*t30*s34*s24*s14*s23+t18*t36*s45*t39+
     #t18*t14*t22-t36*s45*t27+t18*s15*t24*t39-3.D0*t49*s13*t16-3.D0*t53*
     #s13*t48*t16+3.D0*t18*t53*t49*s24+3.D0*t18*t62*t48*s24+3.D0*t18*t67
     #*t22+3.D0*t18*t30*t1*s34*s13
      hh34 = Vc*t76/t48/s13/s12/s24/s14/s23/3.D0
	VTEMP(32) = HH34
	VH34(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t1 = s23**2
      t4 = s14**2
      t7 = s13**2
      t10 = s24**2
      t15 = N**2
      t20 = s34*s12
      hh34 = Vc*(t1*s23*s24+t4*s14*s13+t7*s13*s14+s23*t10*s24)*(-t15*s24
     #*s13-s23*t15*s14+t20+t20*t15)/s12/s23/s34/t15/s13/s14/s24
	VTEMP(33) = HH34
c       g + g --> g + qi
      t1 = N**2
      t2 = s24**2
      t3 = t1*t2
      t4 = t3*s35
      t5 = s25**2
      t9 = s34**2
      t10 = t9**2
      t11 = t1*t10
      t13 = s35*s23*s14
      t15 = t2**2
      t16 = t1*t15
      t18 = s35*s12*s34
      t20 = s14**2
      t21 = t20**2
      t22 = t1*t21
      t33 = s35*s13*s24
      t35 = t5*s25
      t42 = s24*s34
      t45 = t2*s24
      t50 = t1*t20
      t51 = s15**2
      t52 = t51*s15
      t65 = s34*s23*s14
      t68 = t20*s14
      t73 = t4*t5*s23*s14+t11*t13+t16*t18+t22*t18+3.D0*t16*s25*s13*s34-t
     #2*s45*t5*s13*s23+t22*t33+3.D0*t3*t35*s13*s34-t5*s45*s12*t42*s23-t4
     #5*s45*s12*s34*s23+3.D0*t50*t52*s34*s23-t51*s45*s12*s13*s34*s14+3.D
     #0*t1*t35*s24*t65-t68*s45*s12*s13*s34
      t79 = t50*s35
      t80 = t51*s13
      t83 = s35**2
      t85 = t1*t83*s35
      t106 = s13*s24
      t117 = s13*s23
      t121 = t16*t13+3.D0*t1*t45*s25*t65+t79*t80*s24+t85*t9*s23*s14+3.D0
     #*t22*s15*s34*s23+t79*t51*s12*s34+3.D0*t1*t52*s13*t42*s14+t4*t5*s12
     #*s34+3.D0*t1*t68*s15*t106*s34+t11*t33+t85*t106*t9-t20*s45*t80*s23-
     #t21*s45*t117-t15*s45*t117
      hh34 = Vc*(t73+t121)/s12/s13/s24/t9/s23/s14/3.D0
	VTEMP(34) = HH34
	VH34(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t1 = N**2
      t2 = s45**2
      t4 = t1*t2*s45
      t5 = s34**2
      t9 = s23**2
      t10 = t9**2
      t11 = t1*t10
      t13 = s45*s34*s12
      t15 = s13**2
      t16 = t1*t15
      t17 = s15**2
      t18 = t17*s15
      t23 = s25**2
      t24 = t23*s25
      t33 = s34*s13
      t36 = t15*s13
      t38 = s14*s34
      t41 = t9*s23
      t47 = t1*t9
      t52 = t47*s45
      t60 = t5**2
      t61 = t1*t60
      t63 = s45*s14*s23
      t73 = t15**2
      t75 = s14*s24
      t77 = t4*t5*s13*s24+t11*t13+3.D0*t16*t18*s34*s24+3.D0*t1*t24*s34*s
     #13*s24*s23-s35*t17*s14*t33*s12-t36*s35*t38*s12+3.D0*t1*t41*s25*t33
     #*s24+3.D0*t47*t24*s14*s34+t52*t23*s13*s24+3.D0*t11*s25*s14*s34+t61
     #*t63+t4*s14*t5*s23-s35*t23*s34*s12*s24*s23-t73*s35*t75
      t88 = t1*t73
      t93 = s45*s13*s24
      t96 = t16*s45
      t111 = t17*s14
      t122 = 3.D0*t1*t18*s14*t33*s23+3.D0*t1*t36*s15*t38*s23+t88*t63-t10
     #*s35*t75+t11*t93+t88*t13+t96*t17*s34*s12+3.D0*t88*s15*s34*s24+t52*
     #t23*s34*s12-t9*s35*t23*s14*s24+t96*t111*s23+t61*t93-t41*s35*s34*s1
     #2*s24-t15*s35*t111*s24
      hh34 = Vc*(t77+t122)/s14/t5/s13/s12/s24/s23/3.D0
	VTEMP(35) = HH34
c       g + g --> g + g
      hh34 = 2.D0/s12*(s23*s14+s24*s13)*Vc*N**3*(s12**4+s13**4+s14**4+s1
     #5**4+s23**4+s24**4+s25**4+s34**4+s35**4+s45**4)/s24/s34/s13/s23/s1
     #4
	VTEMP(36) = HH34
	VH34(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
