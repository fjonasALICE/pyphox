C coefficients devant p_1.p_2/(p_1.p_5 p_2.p_5)
C ----------------------------------------------------
	SUBROUTINE VEC_H12O(S12,S13,S14,S15,S23,S24,S25,S34,S35,S45,
     #	VH12)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VH12(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VH12(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      hh12 = 4.D0/s13*CF/s24*(s12**2+s34**2+s14**2+s23**2)
	VTEMP(1) = HH12
c       qi + qk --> g + qk
      hh12 = 2.D0/3.D0*s25/s12*CF*(2.D0*CF*N*s14*s23-s14*s23+2.D0*s12*s3
     #4-s24*s13)/s24/s13/s34/s23*(s12**2+s45**2+s14**2+s25**2)
	VTEMP(2) = HH12
	VH12(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      hh12 = 2.D0/3.D0*s15/s12*CF*(2.D0*s12*s34-s13*s24+2.D0*CF*N*s23*s1
     #4-s23*s14)/s13/s14/s24/s34*(s12**2+s35**2+s15**2+s23**2)
	VTEMP(3) = HH12
c       qi + qk --> qk + g
      hh12 = 2.D0/3.D0*s25/s12*CF*(2.D0*CF*N*s13*s24-s13*s24+2.D0*s12*s3
     #4-s23*s14)/s23/s14/s34/s24*(s12**2+s35**2+s13**2+s25**2)
	VTEMP(4) = HH12
	VH12(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      hh12 = 2.D0*CF*(2.D0*CF*N-1.D0)*(s14**2+s23**2+s12**2+s34**2)/s13/
     #s24
	VTEMP(5) = HH12
c       qi + qbk --> g + qbk
      hh12 = 2.D0/3.D0*s25/s12*CF*(2.D0*CF*N*s12*s34-s12*s34+2.D0*s14*s2
     #3-s24*s13)/s24/s13/s23/s34*(s14**2+s25**2+s12**2+s45**2)
	VTEMP(6) = HH12
	VH12(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      hh12 = 2.D0/3.D0*s15/s12*CF*(2.D0*CF*N*s12*s34-s13*s24-s12*s34+2.D
     #0*s23*s14)/s13/s14/s24/s34*(s15**2+s23**2+s12**2+s35**2)
	VTEMP(7) = HH12
c       qi + qbk --> qbk + g
      hh12 = 2.D0/3.D0*s25/s12*CF*(2.D0*CF*N*s12*s34-s12*s34+2.D0*s13*s2
     #4-s23*s14)/s23/s14/s24/s34*(s13**2+s25**2+s12**2+s35**2)
	VTEMP(8) = HH12
	VH12(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      ah12 = 4.D0/s13*CF/s24*(s12**2+s34**2+s14**2+s23**2)
      bh12 = 4.D0*CF/s14/s23*(s12**2+s34**2+s13**2+s24**2)
      ch12 = -4.D0*CF*(CF*N+1.D0)*(s12*s34-s13*s24-s14*s23)*(s12**2+s34*
     #*2)/s13/s24/s14/s23/N
	VTEMP(9) = AH12 + BH12 + CH12
c       qi + qi --> g + qi
      ah12 = 2.D0/3.D0*s25/s12*CF*(2.D0*CF*N*s14*s23-s14*s23+2.D0*s12*s3
     #4-s24*s13)/s24/s13/s34/s23*(s12**2+s45**2+s14**2+s25**2)
      bh12 = 2.D0/3.D0*s15/s12*CF*(2.D0*CF*N*s24*s13-s14*s23+2.D0*s12*s3
     #4-s24*s13)/s14/s13/s34/s23*(s12**2+s45**2+s15**2+s24**2)
      ch12 = -2.D0*CF*(s12*s45-s15*s24-s14*s25)*(s12**2+s45**2)*(2.D0*CF
     #*N*s12*s34-s14*s23+2.D0*s12*s34-s24*s13-2.D0*s13*s23*CF*N-2.D0*s13
     #*s23)/s12/s24/s14/s13/s23/s34/N
	VTEMP(10) = AH12 + BH12 + CH12
	VH12(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      ah12 = 2.D0/3.D0*s15/s12*CF*(2.D0*s12*s34-s13*s24+2.D0*CF*N*s23*s1
     #4-s23*s14)/s13/s14/s24/s34*(s12**2+s35**2+s15**2+s23**2)
      bh12 = 2.D0/3.D0*s25/s12*CF*(2.D0*CF*N*s13*s24+2.D0*s12*s34-s13*s2
     #4-s23*s14)/s23/s14/s24/s34*(s12**2+s35**2+s13**2+s25**2)
      ch12 = -2.D0*CF*(s12*s35-s13*s25-s15*s23)*(s12**2+s35**2)*(2.D0*CF
     #*N*s12*s34+2.D0*s12*s34-s13*s24-s23*s14-2.D0*s14*s24*CF*N-2.D0*s14
     #*s24)/s12/s13/s23/s14/s24/s34/N
	VTEMP(11) = AH12 + BH12 + CH12
	VH12(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      hh12 = -2.D0/s12*CF/s34*(s14**2+s23**2+s13**2+s24**2)
	VTEMP(12) = HH12
c       qi + qbi --> g + qbk
      hh12 = 0.D0
	VTEMP(13) = HH12
	VH12(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      ah12 = 2.D0*CF*(2.D0*CF*N-1.D0)*(s14**2+s23**2+s12**2+s34**2)/s13/
     #s24
      bh12 = -2.D0*CF/s12/s34*(s14**2+s23**2+s13**2+s24**2)
      ch12 = -2.D0/s13*CF/s24/s12/s34/N*(-s14*s23+s13*s24+s12*s34)*(s14*
     #*2+s23**2)
	VTEMP(14) = AH12 + BH12 + CH12
c       qi + qbi --> g + qbi
      ah12 = 2.D0/3.D0*s25/s12*CF*(2.D0*CF*N*s12*s34-s12*s34+2.D0*s14*s2
     #3-s24*s13)/s24/s13/s23/s34*(s14**2+s25**2+s12**2+s45**2)
      bh12 = 0.D0
      ch12 = 0.D0
	VTEMP(15) = AH12 + BH12 + CH12
	VH12(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      hh12 = 0.D0
	VTEMP(16) = HH12
c       qi + qbi --> qbk + g
      hh12 = 0.D0
	VTEMP(17) = HH12
c       qi + qbi --> qi + g
      ah12 = 2.D0/3.D0*s15/s12*CF*(2.D0*CF*N*s12*s34-s13*s24-s12*s34+2.D
     #0*s23*s14)/s13/s14/s24/s34*(s15**2+s23**2+s12**2+s35**2)
      bh12 = 0.D0
      ch12 = 0.D0
	VTEMP(18) = AH12 + BH12 + CH12
c       qi + qbi --> qbi + g
      ah12 = 2.D0/3.D0*s25/s12*CF*(2.D0*CF*N*s12*s34-s12*s34+2.D0*s13*s2
     #4-s23*s14)/s23/s14/s24/s34*(s13**2+s25**2+s12**2+s35**2)
      bh12 = 0.D0
      ch12 = 0.D0
	VTEMP(19) = AH12 + BH12 + CH12
c       qi + qbi --> g + g
      t1 = s23**2
      t4 = s13**2
      t7 = s24**2
      t10 = s14**2
      t15 = N**2
      hh12 = Vc*(t1*s23*s13+t4*s13*s23+t7*s24*s14+t10*s14*s24)*(-t15*s24
     #*s13-t15*s14*s23+t15*s34*s12+s34*s12)/t15/s14/s24/s34/s12/s23/s13
	VTEMP(20) = HH12
	VH12(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      hh12 = -2.D0/3.D0*s15/s12*CF/s13/s24*(s15**2+s34**2+s14**2+s35**2)
	VTEMP(21) = HH12
c       qi + g --> qbk + qk
      hh12 = 2.D0/3.D0*CF*(s13**2+s45**2+s14**2+s35**2)*(2.D0*CF*N*s14*s
     #23*s25-s14*s23*s25+2.D0*s13*s24*s25-s34*s12*s25-s15*s24*s23+6.D0*C
     #F*N*s35*s12*s24-3.D0*s35*s12*s24+6.D0*s45*s12*s23)/s12**2/s34/s24/
     #s23
	VTEMP(22) = HH12
	VH12(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      hh12 = -2.D0/3.D0*s15/s12*CF/s13/s24*(s14**2+s35**2+s15**2+s34**2)
	VTEMP(23) = HH12
c       qi + g --> qk + qbk
      hh12 = 2.D0/3.D0*CF*(s14**2+s35**2+s13**2+s45**2)*(2.D0*CF*N*s13*s
     #24*s25-s13*s24*s25+2.D0*s14*s23*s25-s34*s12*s25-s15*s23*s24+6.D0*C
     #F*N*s45*s12*s23-3.D0*s45*s12*s23+6.D0*s35*s12*s24)/s12**2/s34/s23/
     #s24
	VTEMP(24) = HH12
	VH12(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      ah12 = -2.D0/3.D0*s15/s12*CF/s13/s24*(s14**2+s35**2+s15**2+s34**2)
      bh12 = 2.D0/3.D0*CF*(s14**2+s35**2+s13**2+s45**2)*(2.D0*CF*N*s13*s
     #25*s24+2.D0*s14*s25*s23-s13*s25*s24-s34*s12*s25-s15*s24*s23+6.D0*C
     #F*N*s45*s12*s23+6.D0*s35*s12*s24-3.D0*s45*s12*s23)/s12**2/s34/s24/
     #s23
      ch12 = -2.D0*CF*(-s14*s35+s13*s45+s15*s34)*(s14**2+s35**2)/s12*(s2
     #3+2.D0*s24*CF*N+2.D0*s24)/s13/s34/s24/s23/N
	VTEMP(25) = AH12 + BH12 + CH12
	VH12(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t1 = N**2
      t2 = s15**2
      t4 = s35**2
      t7 = s12*s34*s23
      t9 = s12**2
      t10 = t9**2
      t12 = s24*s23
      t15 = s23**2
      t25 = t2*s15
      t29 = s14*s12*s23
      t31 = s14**2
      t33 = s34**2
      t34 = t33*s34
      t39 = t31**2
      t43 = t2**2
      t52 = s13*s24
      t64 = t4*s35
      t68 = t52*s12
      t70 = t15**2
      t77 = t1*t2*t4*t7-3.D0*t10*s13*t12-3.D0*t15*s23*s13*s24*t9+3.D0*t1
     #*t10*t15*s14+t1*t25*s35*t29+3.D0*t1*t31*t34*s12*s23+3.D0*t1*t39*t7
     #+t1*t43*t7-3.D0*t34*s13*s24*s14*s12-t2*t4*t52*s23-3.D0*t31*s14*s13
     #*s24*s12*s34-t43*s13*t12+t1*s15*t64*t29-s15*t64*t68+3.D0*t1*t70*s1
     #4*t9-t25*s35*t68
      hh12 = Vc*t77/s13/s24/s14/t9/s34/s23/3.D0
	VTEMP(26) = HH12
c       qi + g --> g + g
      t1 = N**2
      t2 = s25**2
      t4 = t1*t2*s25
      t5 = s12**2
      t9 = s14**2
      t10 = t1*t9
      t11 = t10*s25
      t12 = s45**2
      t13 = t12*s24
      t16 = s13**2
      t17 = t16**2
      t18 = t1*t17
      t20 = s25*s23*s14
      t22 = t1*t16
      t23 = t22*s25
      t24 = s35**2
      t34 = t5**2
      t35 = t1*t34
      t37 = s25*s24*s13
      t44 = s25*s34*s12
      t46 = t9*s14
      t51 = t9**2
      t52 = t1*t51
      t55 = s24*s23
      t63 = t4*s24*t5*s13+t11*t13*s13+t18*t20+t23*t24*s34*s12+t23*t24*s2
     #3*s14+t11*t12*s34*s12+t35*t37-t16*s15*t24*s24*s23+t18*t44-t46*s15*
     #s24*s34*s12+t52*t37-t17*s15*t55-t51*s15*t55+3.D0*t52*s45*s12*s23
      t64 = t12*s45
      t80 = t24*s35
      t95 = t16*s13
      t97 = s34*s12
      t105 = s12*s23
      t123 = 3.D0*t10*t64*s12*s23+t52*t44+3.D0*t1*t64*s24*s12*s14*s13+3.
     #D0*t18*s35*s24*s12+3.D0*t22*t80*s24*s12+t35*t20+t4*t5*s23*s14+3.D0
     #*t1*t46*s45*s24*s12*s13-t95*s15*t97*s23-t9*s15*t13*s23-s15*t24*s34
     #*t105*s13-s15*t12*s24*t97*s14+3.D0*t1*t80*s12*s23*s14*s13+3.D0*t1*
     #t95*s35*t105*s14
      hh12 = Vc*(t63+t123)/s24/s34/t5/s23/s14/s13/3.D0
	VTEMP(27) = HH12
	VH12(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      hh12 = -2.D0/3.D0*s15/s12*CF/s14/s23*(s15**2+s34**2+s13**2+s45**2)
	VTEMP(28) = HH12
c       qi + g --> qbk + qi
      hh12 = -2.D0/3.D0*s15/s12*CF/s14/s23*(s13**2+s45**2+s15**2+s34**2)
	VTEMP(29) = HH12
c       qi + g --> qi + qi
      ah12 = -2.D0/3.D0*s15/s12*CF/s13/s24*(s15**2+s34**2+s14**2+s35**2)
      bh12 = -2.D0/3.D0*s15/s12*CF/s14/s23*(s15**2+s34**2+s13**2+s45**2)
      ch12 = 0.D0
	VTEMP(30) = AH12 + BH12 + CH12
c       qi + g --> qbi + qi
      ah12 = 2.D0/3.D0*CF*(s13**2+s45**2+s14**2+s35**2)*(2.D0*CF*N*s14*s
     #23*s25-s14*s23*s25+2.D0*s13*s24*s25-s34*s12*s25-s15*s24*s23+6.D0*C
     #F*N*s35*s12*s24-3.D0*s35*s12*s24+6.D0*s45*s12*s23)/s12**2/s34/s24/
     #s23
      bh12 = -2.D0/3.D0*s15/s12*CF/s14/s23*(s13**2+s45**2+s15**2+s34**2)
      ch12 = 2.D0*CF*(s13*s45-s15*s34-s14*s35)*(s13**2+s45**2)/s12*(s24+
     #2.D0*s23*CF*N+2.D0*s23)/s34/s14/s24/s23/N
	VTEMP(31) = AH12 + BH12 + CH12
c       qi + g --> g + qi
      t1 = N**2
      t2 = s15**2
      t3 = t2**2
      t6 = s12*s34*s24
      t9 = s45**2
      t10 = t9*s45
      t13 = s13*s12*s24
      t18 = t2*s15
      t20 = s14*s23
      t21 = t20*s12
      t23 = s34**2
      t24 = t23*s34
      t30 = s13**2
      t40 = s24**2
      t41 = t40**2
      t43 = s12**2
      t55 = t43**2
      t60 = t30**2
      t70 = s23*s24
      t77 = t1*t3*t6+t1*s15*t10*t13+t1*t2*t9*t6-t18*s45*t21-3.D0*t24*s14
     #*s23*s13*s12-3.D0*t30*s13*s14*s23*s12*s34+t1*t18*s45*t13+3.D0*t1*t
     #41*s13*t43-t2*t9*t20*s24+3.D0*t1*t30*t24*s12*s24+3.D0*t1*t55*t40*s
     #13+3.D0*t1*t60*t6-3.D0*t40*s24*s14*s23*t43-t3*s14*t70-3.D0*t55*s14
     #*t70-s15*t10*t21
      hh12 = Vc*t77/s14/s23/s13/t43/s34/s24/3.D0
	VTEMP(32) = HH12
	VH12(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t1 = N**2
      t2 = s35**2
      t3 = t2*s35
      t5 = t1*t3*s45
      t6 = s12*s24
      t7 = t6*s13
      t10 = t1*s25
      t11 = s45**2
      t12 = t11*s45
      t14 = s13*s23
      t17 = t1*s15
      t24 = s23*s14
      t32 = t11*s24
      t39 = s14**2
      t40 = t39**2
      t43 = s12*s13*s23
      t48 = s34*s12
      t49 = t48*s24
      t52 = s24*s13
      t56 = s12*s23*s14
      t59 = s13**2
      t68 = t48*s14
      t70 = s23**2
      t71 = t70**2
      t73 = t6*s14
      t76 = 3.D0*t5*t7+t10*t12*t14*s14+t17*t3*s24*s23*s14+t10*t2*s45*s13
     #*t24+t17*t2*s45*s24*t14+t17*s35*t32*t24+t10*s35*t32*s13*s14+3.D0*t
     #1*t40*t43-s15*s35*t11*t49+t10*t3*t52*s14+3.D0*t5*t56+3.D0*t1*t59*t
     #39*s14*s12*s24-s25*s35*t11*t68+3.D0*t1*t71*t73
      t77 = t59**2
      t82 = t48*s13
      t86 = s24**2
      t105 = t48*s23
      t114 = t86**2
      t121 = t1*s35*t12
      t129 = 3.D0*t1*t77*t73-s25*t12*t82+3.D0*t1*t70*s23*t86*s12*s13+3.D
     #0*t1*t70*t86*s24*s12*s14+3.D0*t1*t39*t59*s13*s12*s23-s15*t2*s45*t1
     #05-s25*t3*t68+t17*t12*t52*s23-s15*t12*t105+3.D0*t1*t114*t43-s15*t3
     #*t49+3.D0*t121*t56-s25*t2*s45*t82+3.D0*t121*t7
      t133 = s12**2
      hh12 = Vc*(t76+t129)/s34/t133/s24/s13/s23/s14/3.D0
	VTEMP(33) = HH12
c       g + g --> g + qi
      t1 = s35**2
      t2 = t1*s35
      t6 = N**2
      t10 = s34**2
      t13 = s13*s12
      t14 = t13*t6
      t16 = t10*s34
      t17 = t16*s35
      t19 = s23*s14
      t20 = t19*t6
      t25 = s24*s23
      t28 = s14**2
      t29 = s15**2
      t30 = t29*s15
      t32 = s12*s23
      t33 = t32*t6
      t41 = t28**2
      t44 = t16*s45
      t49 = t1*s45*s13
      t56 = t10**2
      t62 = 3.D0*t2*s12*s23*s14*s34*t6+t10*s25*t1*t14+3.D0*t17*s12*t20+t
     #16*s15*s13*t25*t6+t28*t30*t33+3.D0*t2*s13*s24*s12*s34*t6+t41*s15*t
     #33-3.D0*t44*t13*s23-3.D0*t49*t32*s34-3.D0*t44*s13*t33+t56*s25*t14+
     #t10*s15*t1*t33
      t63 = s24**2
      t64 = t63**2
      t69 = s34*t6
      t76 = s25**2
      t77 = t76*s25
      t90 = s24*s12*t6
      t109 = t64*s25*t14+s25*t1*s13*t19*t69+s15*t1*s13*t25*t69+t77*s24*s
     #12*t20+t63*t77*t14-3.D0*t49*t32*t69+t28*s14*s15*s13*t90+t16*s25*s1
     #3*t20+t30*s13*s24*s12*s14*t6+t63*s24*s25*s12*t20+t56*s15*t33+3.D0*
     #t17*s13*t90
      t116 = s12**2
      hh12 = -Vc*(t62+t109)/s13/s24/t116/s23/s14/s34/t6/3.D0
	VTEMP(34) = HH12
	VH12(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t1 = s45**2
      t2 = s35*t1
      t8 = s23**2
      t9 = s25**2
      t10 = t9*s25
      t12 = N**2
      t13 = t12*s12
      t14 = t13*s14
      t16 = s34**2
      t17 = t16*s34
      t18 = t17*s35
      t19 = s12*s24
      t20 = t19*s14
      t25 = s13*s24
      t29 = s34*s14
      t36 = s13*s12
      t37 = t36*s24
      t40 = t17*s45*t12
      t42 = s14*s23*s12
      t52 = s23*s24
      t55 = s13**2
      t56 = t55**2
      t58 = t13*s24
      t61 = t1*s45*t12
      t67 = -3.D0*t2*s12*s24*s34*s14+t8*t10*t14-3.D0*t18*t20+t17*s25*t12
     #*t25*s14-3.D0*t2*t12*t19*t29+t8*s23*s25*t12*t37+3.D0*t40*t42+t10*t
     #12*s23*t37+3.D0*t40*t37+s15*t1*t12*t52*t29+t56*s15*t58+3.D0*t61*s2
     #3*s12*s34*s14
      t71 = s15**2
      t72 = t71*s15
      t75 = t16**2
      t106 = t8**2
      t109 = -3.D0*t18*t12*t20+t55*t72*t58+t75*s15*t58+t17*s15*t12*t52*s
     #14+t16*s15*t1*t58+3.D0*t61*s13*t19*s34+t55*s13*s15*t12*t42+s25*t1*
     #t12*t25*t29+t75*s25*t14+t72*t12*s23*t36*s14+t16*s25*t1*t14+t106*s2
     #5*t14
      t117 = s12**2
      hh12 = -Vc*(t67+t109)/t12/s23/s13/t117/s24/s34/s14/3.D0
	VTEMP(35) = HH12
c       g + g --> g + g
      hh12 = 2.D0/s12*(s23*s14+s24*s13)*Vc*N**3*(s12**4+s13**4+s14**4+s1
     #5**4+s23**4+s24**4+s25**4+s34**4+s35**4+s45**4)/s24/s34/s13/s23/s1
     #4
	VTEMP(36) = HH12
	VH12(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
