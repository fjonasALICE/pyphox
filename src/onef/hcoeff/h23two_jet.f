C coefficients devant p_2.p_3/(p_2.p_5 p_3.p_5)
C ----------------------------------------------------
	SUBROUTINE VEC_H23O(S12,S13,S14,S15,S23,S24,S25,S34,S35,S45,
     #	VH23)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VH23(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VH23(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      hh23 = 2.D0*CF*(2.D0*CF*N-1.D0)*(s12**2+s34**2+s14**2+s23**2)/s13/
     #s24
	VTEMP(1) = HH23
c       qi + qk --> g + qk
      hh23 = -2.D0/3.D0*s25/s23*CF/s24/s13*(s12**2+s45**2+s14**2+s25**2)
	VTEMP(2) = HH23
	VH23(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      hh23 = 2.D0/3.D0*s35/s23*CF*(2.D0*s12*s34-s13*s24+2.D0*CF*N*s23*s1
     #4-s23*s14)/s13/s14/s24/s34*(s12**2+s35**2+s15**2+s23**2)
	VTEMP(3) = HH23
c       qi + qk --> qk + g
      hh23 = 0.D0
	VTEMP(4) = HH23
	VH23(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      hh23 = 4.D0/s13*CF/s24*(s14**2+s23**2+s12**2+s34**2)
	VTEMP(5) = HH23
c       qi + qbk --> g + qbk
      hh23 = -2.D0/3.D0*s25/s23*CF/s24/s13*(s14**2+s25**2+s12**2+s45**2)
	VTEMP(6) = HH23
	VH23(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      hh23 = 2.D0/3.D0*s35/s23*CF*(2.D0*CF*N*s12*s34-s13*s24-s12*s34+2.D
     #0*s23*s14)/s13/s14/s24/s34*(s15**2+s23**2+s12**2+s35**2)
	VTEMP(7) = HH23
c       qi + qbk --> qbk + g
      hh23 = 0.D0
	VTEMP(8) = HH23
	VH23(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      ah23 = 2.D0*CF*(2.D0*CF*N-1.D0)*(s12**2+s34**2+s14**2+s23**2)/s13/
     #s24
      bh23 = -2.D0*CF/s14/s23*(s12**2+s34**2+s13**2+s24**2)
      ch23 = 2.D0/s13*CF/s24/s14/s23/N*(s12*s34-s13*s24-s14*s23)*(s12**2
     #+s34**2)
	VTEMP(9) = AH23 + BH23 + CH23
c       qi + qi --> g + qi
      ah23 = -2.D0/3.D0*s25/s23*CF/s24/s13*(s12**2+s45**2+s14**2+s25**2)
      bh23 = 2.D0/3.D0*CF*(s12**2+s45**2+s15**2+s24**2)*(2.D0*CF*N*s24*s
     #13*s35-s14*s23*s35+2.D0*s12*s34*s35-s24*s13*s35-s25*s13*s34+6.D0*C
     #F*N*s15*s34*s23+6.D0*s45*s13*s23-3.D0*s15*s34*s23)/s14/s13/s34/s23
     #**2
      ch23 = -2.D0*CF*(s12*s45-s15*s24-s14*s25)*(s12**2+s45**2)/s23*(-s3
     #4+2.D0*s13*CF*N+2.D0*s13)/s24/s14/s13/s34/N
	VTEMP(10) = AH23 + BH23 + CH23
	VH23(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      ah23 = 2.D0/3.D0*s35/s23*CF*(2.D0*s12*s34-s13*s24+2.D0*CF*N*s23*s1
     #4-s23*s14)/s13/s14/s24/s34*(s12**2+s35**2+s15**2+s23**2)
      bh23 = 0.D0
      ch23 = 0.D0
	VTEMP(11) = AH23 + BH23 + CH23
	VH23(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      hh23 = 4.D0/s12*CF/s34*(s14**2+s23**2+s13**2+s24**2)
	VTEMP(12) = HH23
c       qi + qbi --> g + qbk
      hh23 = -2.D0/3.D0*s25/s23*CF/s12/s34*(s14**2+s25**2+s15**2+s24**2)
	VTEMP(13) = HH23
	VH23(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      ah23 = 4.D0/s13*CF/s24*(s14**2+s23**2+s12**2+s34**2)
      bh23 = 4.D0*CF/s12/s34*(s14**2+s23**2+s13**2+s24**2)
      ch23 = 4.D0*CF*(1.D0+CF*N)*(-s14*s23+s13*s24+s12*s34)*(s14**2+s23*
     #*2)/s13/s24/s12/s34/N
	VTEMP(14) = AH23 + BH23 + CH23
c       qi + qbi --> g + qbi
      ah23 = -2.D0/3.D0*s25/s23*CF/s24/s13*(s14**2+s25**2+s12**2+s45**2)
      bh23 = -2.D0/3.D0*s25/s23*CF/s12/s34*(s14**2+s25**2+s15**2+s24**2)
      ch23 = 0.D0
	VTEMP(15) = AH23 + BH23 + CH23
	VH23(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      hh23 = 2.D0/3.D0*s25/s23*CF*(2.D0*CF*N*s13*s24-s12*s34-s13*s24+2.D
     #0*s23*s14)/s12/s14/s34/s24*(s15**2+s23**2+s13**2+s25**2)
	VTEMP(16) = HH23
c       qi + qbi --> qbk + g
      hh23 = 2.D0/3.D0*s25/s23*CF*(2.D0*s13*s24-s12*s34+2.D0*CF*N*s23*s1
     #4-s23*s14)/s12/s14/s34/s24*(s13**2+s25**2+s15**2+s23**2)
	VTEMP(17) = HH23
c       qi + qbi --> qi + g
      ah23 = 2.D0/3.D0*s35/s23*CF*(2.D0*CF*N*s12*s34-s13*s24-s12*s34+2.D
     #0*s23*s14)/s13/s14/s24/s34*(s15**2+s23**2+s12**2+s35**2)
      bh23 = 2.D0/3.D0*s25/s23*CF*(2.D0*CF*N*s13*s24-s13*s24-s12*s34+2.D
     #0*s23*s14)/s12/s14/s24/s34*(s15**2+s23**2+s13**2+s25**2)
      ch23 = 2.D0*CF*(-s15*s23+s13*s25+s12*s35)*(s15**2+s23**2)*(2.D0*CF
     #*N*s23*s14-s13*s24-s12*s34+2.D0*s23*s14+2.D0*s34*s24*CF*N+2.D0*s34
     #*s24)/s23/s13/s12/s14/s34/s24/N
	VTEMP(18) = AH23 + BH23 + CH23
c       qi + qbi --> qbi + g
      ah23 = 0.D0
      bh23 = 2.D0/3.D0*s25/s23*CF*(2.D0*CF*N*s23*s14-s12*s34+2.D0*s13*s2
     #4-s23*s14)/s12/s14/s24/s34*(s13**2+s25**2+s15**2+s23**2)
      ch23 = 0.D0
	VTEMP(19) = AH23 + BH23 + CH23
c       qi + qbi --> g + g
      t1 = s25**2
      t2 = t1**2
      t4 = s34*s13
      t6 = N**2
      t7 = s15**2
      t11 = s14*s23*s13
      t13 = t7*s15
      t16 = s34*s23*s12
      t18 = s13**2
      t20 = s23**2
      t22 = s34*s12
      t25 = s24**2
      t34 = t20**2
      t38 = t1*s25
      t41 = t18**2
      t46 = s14**2
      t47 = t46*s14
      t59 = s24*s23*s13
      t63 = t25**2
      t73 = -t2*s12*t4+t6*t7*t1*t11-t13*s25*t16-3.D0*t18*s13*t20*t22-3.D
     #0*t25*s24*s14*t16-t7*t1*t22*s13+3.D0*t6*t18*t34*s24-s15*t38*t16+3.
     #D0*t6*t41*s24*t20-3.D0*t47*s24*t16+3.D0*t6*t25*t47*s23*s13+t6*s15*
     #t38*t59+t6*t2*t11+3.D0*t6*t63*t11+t6*t13*s25*t59-3.D0*t34*s12*t4
      hh23 = Vc*t73/s14/s24/t20/s12/s34/s13/3.D0
	VTEMP(20) = HH23
	VH23(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      hh23 = -2.D0/3.D0*s35/s23*CF/s13/s24*(s15**2+s34**2+s14**2+s35**2)
	VTEMP(21) = HH23
c       qi + g --> qbk + qk
      hh23 = -2.D0/3.D0*s35/s23*CF/s34/s12*(s13**2+s45**2+s14**2+s35**2)
	VTEMP(22) = HH23
	VH23(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      hh23 = -2.D0/3.D0*s35/s23*CF/s13/s24*(s14**2+s35**2+s15**2+s34**2)
	VTEMP(23) = HH23
c       qi + g --> qk + qbk
      hh23 = -2.D0/3.D0*s35/s23*CF/s34/s12*(s14**2+s35**2+s13**2+s45**2)
	VTEMP(24) = HH23
	VH23(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      ah23 = -2.D0/3.D0*s35/s23*CF/s13/s24*(s14**2+s35**2+s15**2+s34**2)
      bh23 = -2.D0/3.D0*s35/s23*CF/s34/s12*(s14**2+s35**2+s13**2+s45**2)
      ch23 = 0.D0
	VTEMP(25) = AH23 + BH23 + CH23
	VH23(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t1 = s35**2
      t2 = t1*s35
      t5 = s13*s24*s23
      t7 = N**2
      t8 = s34**2
      t9 = t8**2
      t12 = s23*s14*s12
      t15 = s15**2
      t16 = t15*s15
      t19 = s12**2
      t21 = s23**2
      t22 = t21**2
      t29 = t19**2
      t35 = s24*s13
      t38 = s14**2
      t39 = t38*s14
      t48 = s23*s34*s12
      t67 = t1**2
      t77 = -s15*t2*t5+3.D0*t7*t9*t12-t16*s35*t5+3.D0*t7*t19*t22*s34+t7*
     #t1*t15*t12+3.D0*t7*t29*t21*s34-3.D0*t22*s12*t35+3.D0*t7*t39*t8*s23
     #*s12+t7*s15*t2*t48-3.D0*t8*s34*s23*s14*s13*s24+t7*t16*s35*t48-3.D0
     #*t19*s12*t21*t35-t1*t15*s12*s13*s24+t7*t67*t12-t67*s12*t35-3.D0*t3
     #9*s23*s34*s13*s24
      hh23 = Vc*t77/t21/s14/s34/s12/s13/s24/3.D0
	VTEMP(26) = HH23
c       qi + g --> g + g
      t1 = s14**2
      t2 = t1*s14
      t3 = t2*s15
      t5 = s24*s34
      t6 = N**2
      t7 = t5*t6
      t10 = s45**2
      t12 = t10*s45*s23
      t18 = s25**2
      t19 = t18*s25
      t25 = t1**2
      t27 = s23*s24
      t28 = t27*t6
      t31 = s15*t10*s23
      t36 = t2*s45*s23
      t38 = s12*s34*t6
      t47 = t5*s14*t6
      t54 = s13**2
      t55 = t54**2
      t63 = -3.D0*t3*s23*t7+3.D0*t12*s12*s34*s14*t6+t19*s23*s13*s12*s24*
     #t6+t25*s35*t28-3.D0*t31*t5*s14+3.D0*t36*t38-3.D0*t3*t27*s34+s35*t1
     #0*s12*t47+3.D0*t12*s13*s24*s14*t6+t55*s35*t28+t2*s35*s12*t7-3.D0*t
     #31*t47
      t65 = s13*s24*t6
      t68 = s35**2
      t69 = t68*s35
      t75 = s12**2
      t76 = t75**2
      t79 = s23*s34*t6
      t105 = 3.D0*t36*t65+t54*t69*t28+s25*t10*s13*t47+t76*s25*t79+t75*s1
     #2*s25*s23*t65+t75*t19*t79+t1*s35*t10*t28+t2*s25*s13*t7+t54*s13*s35
     #*s23*t38+t25*s25*t79+t69*s23*s13*t38+t1*s25*t10*t79
      t108 = s23**2
      hh23 = -Vc*(t63+t105)/t108/s13/s12/s24/s34/s14/t6/3.D0
	VTEMP(27) = HH23
	VH23(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      hh23 = 2.D0/3.D0*CF*(s15**2+s34**2+s13**2+s45**2)*(2.D0*CF*N*s13*s
     #25*s24-s14*s23*s25-s13*s25*s24+2.D0*s34*s12*s25-s35*s12*s24+6.D0*C
     #F*N*s45*s12*s23+6.D0*s15*s23*s24-3.D0*s45*s12*s23)/s23**2/s14/s12/
     #s24
	VTEMP(28) = HH23
c       qi + g --> qbk + qi
      hh23 = 2.D0/3.D0*CF*(s13**2+s45**2+s15**2+s34**2)*(2.D0*s13*s25*s2
     #4-s14*s25*s23+2.D0*CF*N*s34*s12*s25-s34*s12*s25-s35*s12*s24+6.D0*C
     #F*N*s15*s23*s24-3.D0*s15*s23*s24+6.D0*s45*s12*s23)/s23**2/s14/s12/
     #s24
	VTEMP(29) = HH23
c       qi + g --> qi + qi
      ah23 = -2.D0/3.D0*s35/s23*CF/s13/s24*(s15**2+s34**2+s14**2+s35**2)
      bh23 = 2.D0/3.D0*CF*(s15**2+s34**2+s13**2+s45**2)*(2.D0*CF*N*s13*s
     #24*s25-s13*s24*s25-s14*s25*s23+2.D0*s34*s12*s25-s35*s12*s24+6.D0*C
     #F*N*s45*s12*s23+6.D0*s15*s24*s23-3.D0*s45*s12*s23)/s14/s12/s24/s23
     #**2
      ch23 = 2.D0*CF*(-s15*s34+s13*s45+s14*s35)*(s15**2+s34**2)/s23*(-s1
     #2+2.D0*s24*CF*N+2.D0*s24)/s13/s14/s12/s24/N
	VTEMP(30) = AH23 + BH23 + CH23
c       qi + g --> qbi + qi
      ah23 = -2.D0/3.D0*s35/s23*CF/s34/s12*(s13**2+s45**2+s14**2+s35**2)
      bh23 = 2.D0/3.D0*CF*(s13**2+s45**2+s15**2+s34**2)*(2.D0*CF*N*s34*s
     #12*s25-s14*s23*s25+2.D0*s13*s24*s25-s34*s12*s25-s35*s12*s24+6.D0*C
     #F*N*s15*s24*s23+6.D0*s45*s12*s23-3.D0*s15*s24*s23)/s14/s12/s24/s23
     #**2
      ch23 = -2.D0*CF*(s13*s45-s15*s34-s14*s35)*(s13**2+s45**2)/s23*(-s2
     #4+2.D0*s12*CF*N+2.D0*s12)/s34/s14/s12/s24/N
	VTEMP(31) = AH23 + BH23 + CH23
c       qi + g --> g + qi
      t1 = N**2
      t2 = t1*s25
      t3 = s45**2
      t4 = t3*s45
      t6 = s34*s12
      t9 = s15**2
      t12 = s14*s23
      t13 = t12*s13
      t15 = t1*s35
      t16 = t9*s15
      t20 = s34**2
      t21 = t20**2
      t24 = s23*s12*s13
      t28 = t1*s15*t4
      t29 = s23*s34
      t30 = t29*s12
      t34 = s12*s24
      t37 = s13**2
      t44 = s24**2
      t45 = t44**2
      t51 = t12*s34
      t59 = t12*s24
      t63 = t12*s12
      t68 = t1*t16*s45
      t71 = t2*t4*t6*s13-s25*t9*s45*t13+t15*t16*t6*s24+3.D0*t1*t21*t24+3
     #.D0*t28*t30+t15*t4*t34*s13+3.D0*t1*t37*s13*t20*s23*s12+3.D0*t1*t45
     #*t24-s25*s15*t3*t51+t2*t9*s45*s34*s12*s13-s35*t16*t59-s35*t9*s45*t
     #63-s25*t16*t51+3.D0*t68*t30
      t74 = s24*s13
      t78 = t3*s34
      t81 = s12**2
      t88 = t37**2
      t90 = t29*s24
      t99 = t81**2
      t107 = s23*s24*s13
      t129 = t15*t9*s45*s12*t74+t15*s15*t78*t34+3.D0*t1*t81*s12*t44*s23*
     #s13+3.D0*t1*t88*t90+3.D0*t1*t81*t44*s24*s23*s34+3.D0*t1*t99*t90+t2
     #*s15*t78*t74+3.D0*t28*t107+3.D0*t1*t37*t20*s34*s23*s24-s25*t4*t13+
     #3.D0*t68*t107+t2*t16*s34*s24*s13-s35*t4*t63-s35*s15*t3*t59
      t133 = s23**2
      hh23 = Vc*(t71+t129)/s14/t133/s34/s12/s24/s13/3.D0
	VTEMP(32) = HH23
	VH23(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t1 = N**2
      t2 = s35**2
      t4 = s45**2
      t7 = s23*s14*s24
      t9 = t2*s35
      t13 = s23*s13*s24
      t16 = s34*s12
      t19 = t2**2
      t23 = t16*s23
      t25 = s24**2
      t26 = t25**2
      t28 = s23**2
      t33 = t4*s45
      t42 = s12*s24
      t44 = t28**2
      t48 = s13**2
      t51 = s12*s23
      t55 = t48**2
      t63 = s14**2
      t64 = t63*s14
      t76 = t1*t2*t4*t7+t1*t9*s45*t13-t2*t4*t16*s24+t1*t19*t7-t9*s45*t23
     #+3.D0*t1*t26*t28*s13+t1*s35*t33*t13-3.D0*t25*s24*s34*s12*t28-t19*s
     #34*t42-3.D0*t44*s34*t42-3.D0*t48*s13*s34*t51*s14+3.D0*t1*t55*t7+3.
     #D0*t1*t44*t25*s13-3.D0*t64*s34*t51*s13+3.D0*t1*t48*t64*s23*s24-s35
     #*t33*t23
      hh23 = Vc*t76/s34/s12/t28/s13/s14/s24/3.D0
	VTEMP(33) = HH23
c       g + g --> g + qi
      t1 = s34**2
      t2 = s35**2
      t3 = t2*s35
      t6 = N**2
      t7 = s23*s12*t6
      t9 = s14**2
      t10 = t9**2
      t12 = s23*s13
      t13 = t12*t6
      t15 = s15**2
      t18 = s13*s12
      t22 = s25**2
      t23 = t22*s25
      t27 = s12*s34*t6
      t29 = t9*s14
      t35 = s24**2
      t43 = t1**2
      t46 = t35**2
      t51 = s14*s13
      t53 = t51*s12*t6
      t56 = t15*s45*s23
      t59 = -t1*t3*t7-t10*s25*t13-s25*t15*s14*t18*s34*t6-t23*s23*s24*t27
     #-t29*s25*s13*t27-t10*s35*t7-t35*s24*s25*s23*t27-t9*s35*t15*t7-t43*
     #s35*t7-t46*s25*t13-s35*t15*s24*t53+3.D0*t56*t53
      t68 = t29*s45
      t76 = t18*t6
      t85 = s24*s13*t6
      t88 = t29*s15*s23
      t92 = t15*s15*s23
      t105 = -t3*s23*s24*s13*s34*t6-t9*s25*t15*t13+3.D0*t68*t12*s12+3.D0
     #*t56*t51*s12+3.D0*t68*s23*t76-t35*t23*t13-t1*s34*s35*s23*t85-3.D0*
     #t88*t85-3.D0*t92*s24*t51*t6-3.D0*t88*t27-3.D0*t92*s14*t27-t29*s35*
     #s24*t76
      t108 = s23**2
      hh23 = Vc*(t59+t105)/t108/s24/s14/s13/s12/s34/t6/3.D0
	VTEMP(34) = HH23
	VH23(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t1 = N**2
      t2 = s23**2
      t3 = t2**2
      t4 = t1*t3
      t6 = s25*s12*s34
      t8 = s13**2
      t9 = t8**2
      t10 = t1*t9
      t12 = s25*s23*s14
      t14 = t1*t8
      t15 = t14*s25
      t16 = s15**2
      t17 = t16*s12
      t20 = s45**2
      t26 = t20*s45
      t33 = t8*s13
      t36 = s23*s12
      t42 = s13*s12
      t45 = s25**2
      t47 = t1*t45*s25
      t54 = s34**2
      t55 = t1*t54
      t60 = t55*s25
      t64 = t16*s15
      t74 = s12*s24
      t76 = t4*t6+t10*t12+t15*t17*s34-s35*t20*s23*s24*s14*s34+3.D0*t1*t2
     #6*s23*s13*s24*s34+3.D0*t1*t33*s15*t36*s34-s35*t16*s23*t42*s14+t47*
     #t2*s13*s24-t33*s35*t36*s14+3.D0*t55*t26*s23*s12+t60*t20*s13*s24+3.
     #D0*t14*t64*s23*s24+3.D0*t10*s15*s23*s24-t9*s35*t74
      t77 = t54**2
      t84 = s25*s13*s24
      t93 = t1*t77
      t109 = t54*s34
      t123 = -t77*s35*t74+t60*t20*s23*s14+t4*t84-t8*s35*t17*s24-t54*s35*
     #t20*s12*s24+t93*t84+t10*t6+t93*t12+3.D0*t93*s45*s23*s12+3.D0*t1*t6
     #4*s23*t42*s34+t15*t16*s23*s14+3.D0*t1*t109*s45*s23*s13*s24+t47*t2*
     #s12*s34-t109*s35*s23*s24*s14
      hh23 = Vc*(t76+t123)/t2/s13/s12/s24/s14/s34/3.D0
	VTEMP(35) = HH23
c       g + g --> g + g
      hh23 = 2.D0/s23*(s24*s13+s12*s34)*Vc*N**3*(s12**4+s13**4+s14**4+s1
     #5**4+s23**4+s24**4+s25**4+s34**4+s35**4+s45**4)/s12/s24/s34/s13/s1
     #4
	VTEMP(36) = HH23
	VH23(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
