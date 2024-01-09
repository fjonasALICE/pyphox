C coefficients devant p_2.p_4/(p_2.p_5 p_4.p_5)
C ----------------------------------------------------
	SUBROUTINE VEC_H24O(S12,S13,S14,S15,S23,S24,S25,S34,S35,S45,
     #	VH24)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VH24(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VH24(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      hh24 = -2.D0/s13*CF/s24*(s12**2+s34**2+s14**2+s23**2)
	VTEMP(1) = HH24
c       qi + qk --> g + qk
      hh24 = 0.D0
	VTEMP(2) = HH24
	VH24(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      hh24 = 2.D0/3.D0*CF*(s12**2+s35**2+s15**2+s23**2)*(2.D0*s12*s45*s3
     #4-s13*s45*s24+2.D0*CF*N*s23*s14*s45-s23*s14*s45-s25*s14*s34+6.D0*C
     #F*N*s15*s24*s34-3.D0*s15*s24*s34+6.D0*s35*s14*s24)/s24**2/s13/s14/
     #s34
	VTEMP(3) = HH24
c       qi + qk --> qk + g
      hh24 = -2.D0/3.D0*s25/s24*CF/s23/s14*(s12**2+s35**2+s13**2+s25**2)
	VTEMP(4) = HH24
	VH24(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      hh24 = -2.D0/s13*CF/s24*(s14**2+s23**2+s12**2+s34**2)
	VTEMP(5) = HH24
c       qi + qbk --> g + qbk
      hh24 = 0.D0
	VTEMP(6) = HH24
	VH24(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      hh24 = 2.D0/3.D0*CF*(s15**2+s23**2+s12**2+s35**2)*(2.D0*CF*N*s12*s
     #45*s34-s13*s24*s45-s12*s45*s34+2.D0*s23*s14*s45-s25*s14*s34+6.D0*C
     #F*N*s35*s14*s24+6.D0*s15*s24*s34-3.D0*s35*s14*s24)/s24**2/s13/s14/
     #s34
	VTEMP(7) = HH24
c       qi + qbk --> qbk + g
      hh24 = -2.D0/3.D0*s25/s24*CF/s23/s14*(s13**2+s25**2+s12**2+s35**2)
	VTEMP(8) = HH24
	VH24(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      ah24 = -2.D0/s13*CF/s24*(s12**2+s34**2+s14**2+s23**2)
      bh24 = 2.D0*CF*(2.D0*CF*N-1.D0)*(s12**2+s34**2+s13**2+s24**2)/s14/
     #s23
      ch24 = 2.D0/s13*CF/s24/s14/s23/N*(s12*s34-s13*s24-s14*s23)*(s12**2
     #+s34**2)
	VTEMP(9) = AH24 + BH24 + CH24
c       qi + qi --> g + qi
      ah24 = 0.D0
      bh24 = 2.D0/3.D0*s45/s24*CF*(2.D0*CF*N*s24*s13-s14*s23+2.D0*s12*s3
     #4-s24*s13)/s14/s13/s34/s23*(s12**2+s45**2+s15**2+s24**2)
      ch24 = 0.D0
	VTEMP(10) = AH24 + BH24 + CH24
	VH24(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      ah24 = 2.D0/3.D0*CF*(s12**2+s35**2+s15**2+s23**2)*(2.D0*s12*s45*s3
     #4-s13*s45*s24+2.D0*CF*N*s23*s14*s45-s23*s14*s45-s25*s14*s34+6.D0*C
     #F*N*s15*s24*s34-3.D0*s15*s24*s34+6.D0*s35*s14*s24)/s24**2/s13/s14/
     #s34
      bh24 = -2.D0/3.D0*s25/s24*CF/s23/s14*(s12**2+s35**2+s13**2+s25**2)
      ch24 = -2.D0*CF*(s12*s35-s13*s25-s15*s23)*(s12**2+s35**2)/s24*(-s3
     #4+2.D0*s14*CF*N+2.D0*s14)/s13/s23/s14/s34/N
	VTEMP(11) = AH24 + BH24 + CH24
	VH24(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      hh24 = 2.D0*CF*(2.D0*CF*N-1.D0)*(s14**2+s23**2+s13**2+s24**2)/s12/
     #s34
	VTEMP(12) = HH24
c       qi + qbi --> g + qbk
      hh24 = 2.D0/3.D0*s25/s24*CF*(2.D0*s14*s23-s12*s34+2.D0*CF*N*s24*s1
     #3-s24*s13)/s12/s13/s34/s23*(s14**2+s25**2+s15**2+s24**2)
	VTEMP(13) = HH24
	VH24(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      ah24 = -2.D0/s13*CF/s24*(s14**2+s23**2+s12**2+s34**2)
      bh24 = 2.D0*CF*(2.D0*CF*N-1.D0)*(s14**2+s23**2+s13**2+s24**2)/s12/
     #s34
      ch24 = -2.D0/s13*CF/s24/s12/s34/N*(-s14*s23+s13*s24+s12*s34)*(s14*
     #*2+s23**2)
	VTEMP(14) = AH24 + BH24 + CH24
c       qi + qbi --> g + qbi
      ah24 = 0.D0
      bh24 = 2.D0/3.D0*s25/s24*CF*(2.D0*CF*N*s24*s13-s12*s34+2.D0*s14*s2
     #3-s24*s13)/s12/s13/s23/s34*(s14**2+s25**2+s15**2+s24**2)
      ch24 = 0.D0
	VTEMP(15) = AH24 + BH24 + CH24
	VH24(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      hh24 = -2.D0/3.D0*s25/s24*CF/s12/s34*(s15**2+s23**2+s13**2+s25**2)
	VTEMP(16) = HH24
c       qi + qbi --> qbk + g
      hh24 = -2.D0/3.D0*s25/s24*CF/s12/s34*(s13**2+s25**2+s15**2+s23**2)
	VTEMP(17) = HH24
c       qi + qbi --> qi + g
      ah24 = 2.D0/3.D0*CF*(s15**2+s23**2+s12**2+s35**2)*(2.D0*CF*N*s12*s
     #45*s34-s13*s24*s45-s12*s45*s34+2.D0*s23*s14*s45-s25*s14*s34+6.D0*C
     #F*N*s35*s14*s24+6.D0*s15*s24*s34-3.D0*s35*s14*s24)/s24**2/s13/s14/
     #s34
      bh24 = -2.D0/3.D0*s25/s24*CF/s12/s34*(s15**2+s23**2+s13**2+s25**2)
      ch24 = 2.D0*CF*(-s15*s23+s13*s25+s12*s35)*(s15**2+s23**2)/s24*(-s1
     #4+2.D0*s34*CF*N+2.D0*s34)/s13/s12/s14/s34/N
	VTEMP(18) = AH24 + BH24 + CH24
c       qi + qbi --> qbi + g
      ah24 = -2.D0/3.D0*s25/s24*CF/s23/s14*(s13**2+s25**2+s12**2+s35**2)
      bh24 = -2.D0/3.D0*s25/s24*CF/s12/s34*(s13**2+s25**2+s15**2+s23**2)
      ch24 = 0.D0
	VTEMP(19) = AH24 + BH24 + CH24
c       qi + qbi --> g + g
      t1 = N**2
      t2 = s23**2
      t3 = t2**2
      t6 = s24*s13*s14
      t9 = s14**2
      t11 = s24**2
      t12 = t11**2
      t16 = s15**2
      t17 = t16*s15
      t19 = s34*s12
      t20 = t19*s24
      t23 = s34*s14
      t28 = s34*s24
      t32 = s13**2
      t33 = t32*s13
      t43 = t9**2
      t48 = s25**2
      t49 = t48*s25
      t58 = s24*s23*s14
      t60 = t48**2
      t76 = 3.D0*t1*t3*t6+3.D0*t1*t9*t12*s23-t17*s25*t20-3.D0*t12*s12*t2
     #3-3.D0*t2*s23*s12*t28*s13-3.D0*t33*s12*t28*s23+3.D0*t1*t33*t2*s24*
     #s14+3.D0*t1*t43*t11*s23-s15*t49*t20-t16*t48*t19*s14+t1*t17*s25*t58
     #-t60*s12*t23+t1*s15*t49*t58+t1*t16*t48*t6-3.D0*t9*s14*s12*s34*t11+
     #t1*t60*t6
      hh24 = Vc*t76/s12/s34/t11/s13/s23/s14/3.D0
	VTEMP(20) = HH24
	VH24(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      hh24 = 2.D0/3.D0*CF*(s15**2+s34**2+s14**2+s35**2)*(2.D0*CF*N*s14*s
     #25*s23-s13*s24*s25-s14*s25*s23+2.D0*s34*s12*s25-s45*s12*s23+6.D0*C
     #F*N*s35*s12*s24+6.D0*s15*s24*s23-3.D0*s35*s12*s24)/s24**2/s13/s12/
     #s23
	VTEMP(21) = HH24
c       qi + g --> qbk + qk
      hh24 = -2.D0/3.D0*s45/s24*CF/s34/s12*(s13**2+s45**2+s14**2+s35**2)
	VTEMP(22) = HH24
	VH24(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      hh24 = 2.D0/3.D0*CF*(s14**2+s35**2+s15**2+s34**2)*(2.D0*s14*s25*s2
     #3-s13*s25*s24+2.D0*CF*N*s34*s12*s25-s34*s12*s25-s45*s12*s23+6.D0*C
     #F*N*s15*s24*s23-3.D0*s15*s24*s23+6.D0*s35*s12*s24)/s24**2/s13/s12/
     #s23
	VTEMP(23) = HH24
c       qi + g --> qk + qbk
      hh24 = -2.D0/3.D0*s45/s24*CF/s34/s12*(s14**2+s35**2+s13**2+s45**2)
	VTEMP(24) = HH24
	VH24(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      ah24 = 2.D0/3.D0*CF*(s14**2+s35**2+s15**2+s34**2)*(2.D0*s14*s25*s2
     #3-s13*s25*s24+2.D0*CF*N*s34*s12*s25-s34*s12*s25-s45*s12*s23+6.D0*C
     #F*N*s15*s24*s23-3.D0*s15*s24*s23+6.D0*s35*s12*s24)/s24**2/s13/s12/
     #s23
      bh24 = -2.D0/3.D0*s45/s24*CF/s34/s12*(s14**2+s35**2+s13**2+s45**2)
      ch24 = 2.D0*CF*(-s14*s35+s13*s45+s15*s34)*(s14**2+s35**2)/s24*(-s2
     #3+2.D0*s12*CF*N+2.D0*s12)/s13/s34/s12/s23/N
	VTEMP(25) = AH24 + BH24 + CH24
	VH24(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t1 = N**2
      t2 = t1*s45
      t3 = s15**2
      t10 = s35**2
      t11 = t10*s23
      t12 = s34*s12
      t15 = t1*s25
      t17 = s14*s34
      t24 = s12**2
      t27 = s23**2
      t32 = t3*s15
      t34 = s24*s13
      t35 = t34*s23
      t37 = t10*s35
      t39 = t34*s12
      t41 = s34**2
      t42 = t41**2
      t45 = s24*s14*s12
      t48 = s14**2
      t62 = t34*s34
      t71 = t34*s14
      t76 = t2*t3*s35*s23*s14*s12+t2*s15*t11*t12+t15*s15*t11*t17+t15*t3*
     #s35*s14*t12+3.D0*t1*t24*s12*t27*s24*s14-s45*t32*t35-s45*t37*t39+3.
     #D0*t1*t42*t45+3.D0*t1*t48*s14*t41*s24*s12+3.D0*t1*t24*t27*s23*s24*
     #s34-s25*t32*t62+3.D0*t1*t48*t41*s34*s24*s23-s25*t37*t71-s45*s15*t1
     #0*t35
      t88 = t1*s15*t37
      t90 = s24*s34*s12
      t93 = t48**2
      t95 = s24*s23
      t96 = t95*s34
      t99 = t24**2
      t103 = t27**2
      t108 = t1*t32*s35
      t111 = t95*s14
      t117 = s23*s14
      t129 = -s45*t3*s35*t39+t15*t37*t17*s12+t2*t32*s23*s34*s12+3.D0*t88
     #*t90+3.D0*t1*t93*t96+3.D0*t1*t99*t96+3.D0*t1*t103*t45+3.D0*t108*t9
     #0+3.D0*t108*t111+3.D0*t88*t111+t2*t37*t117*s12+t15*t32*t117*s34-s2
     #5*t3*s35*t71-s25*s15*t10*t62
      t133 = s24**2
      hh24 = Vc*(t76+t129)/s13/t133/s23/s14/s34/s12/3.D0
	VTEMP(26) = HH24
c       qi + g --> g + g
      t1 = s14**2
      t2 = s45**2
      t3 = t2*s45
      t6 = N**2
      t7 = s24*s23*t6
      t9 = s13**2
      t11 = s35**2
      t13 = s34*s24
      t14 = t13*t6
      t16 = s12**2
      t21 = s23*s14*t6
      t23 = t9**2
      t26 = t1**2
      t29 = s25**2
      t30 = t29*s25
      t47 = s23*t6
      t50 = t11*s35
      t53 = s13*s24
      t59 = t9*s13
      t60 = t59*s35
      t64 = t1*t3*t7+t9*s25*t11*t14+t16*s12*s25*s24*t21+t23*s25*t14+t26*
     #s45*t7+t16*t30*t14+t3*s34*s12*s24*s14*t6+s25*t11*s34*s13*s23*s14*t
     #6+s45*t11*s34*s12*s13*t47+3.D0*t50*s34*s12*t53*t6+t23*s45*t7+3.D0*
     #t60*s24*t21
      t67 = s12*s24*t6
      t84 = s15*t11*s34
      t92 = t16**2
      t100 = t59*s15
      t110 = 3.D0*t60*s34*t67+t9*s45*t11*t7+t59*s25*s34*t21+t30*s12*s24*
     #t21+t1*s14*s45*s34*t67-3.D0*t84*t53*t47+3.D0*t50*s13*s24*t21+t92*s
     #25*t14+t59*s45*s34*s12*s23*t6-3.D0*t100*t13*s23-3.D0*t100*s34*t7-3
     #.D0*t84*t53*s23
      t118 = s24**2
      hh24 = -Vc*(t64+t110)/s34/s12/s13/t118/s23/s14/t6/3.D0
	VTEMP(27) = HH24
	VH24(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      hh24 = -2.D0/3.D0*s45/s24*CF/s14/s23*(s15**2+s34**2+s13**2+s45**2)
	VTEMP(28) = HH24
c       qi + g --> qbk + qi
      hh24 = -2.D0/3.D0*s45/s24*CF/s14/s23*(s13**2+s45**2+s15**2+s34**2)
	VTEMP(29) = HH24
c       qi + g --> qi + qi
      ah24 = 2.D0/3.D0*CF*(s15**2+s34**2+s14**2+s35**2)*(2.D0*CF*N*s14*s
     #25*s23-s13*s24*s25-s14*s25*s23+2.D0*s34*s12*s25-s45*s12*s23+6.D0*C
     #F*N*s35*s12*s24+6.D0*s15*s24*s23-3.D0*s35*s12*s24)/s24**2/s13/s12/
     #s23
      bh24 = -2.D0/3.D0*s45/s24*CF/s14/s23*(s15**2+s34**2+s13**2+s45**2)
      ch24 = 2.D0*CF*(-s15*s34+s13*s45+s14*s35)*(s15**2+s34**2)/s24*(-s1
     #2+2.D0*s23*CF*N+2.D0*s23)/s13/s14/s12/s23/N
	VTEMP(30) = AH24 + BH24 + CH24
c       qi + g --> qbi + qi
      ah24 = -2.D0/3.D0*s45/s24*CF/s34/s12*(s13**2+s45**2+s14**2+s35**2)
      bh24 = -2.D0/3.D0*s45/s24*CF/s14/s23*(s13**2+s45**2+s15**2+s34**2)
      ch24 = 0.D0
	VTEMP(31) = AH24 + BH24 + CH24
c       qi + g --> g + qi
      t1 = N**2
      t2 = s45**2
      t3 = t2**2
      t6 = s24*s13*s12
      t8 = s15**2
      t9 = t8*s15
      t13 = s24*s34*s12
      t15 = t2*s45
      t17 = s14*s23
      t18 = t17*s24
      t26 = s13**2
      t27 = t26*s13
      t29 = s34**2
      t36 = s23*s24
      t44 = t29**2
      t48 = s12**2
      t50 = s24**2
      t51 = t50**2
      t55 = t48**2
      t66 = s23*s12
      t76 = t1*t3*t6+t1*t9*s45*t13-s15*t15*t18+t1*t2*t8*t6+t1*s15*t15*t1
     #3+3.D0*t1*t27*t29*s24*s12-3.D0*t29*s34*s14*t36*s13-3.D0*t27*s14*t3
     #6*s34+3.D0*t1*t44*t6+3.D0*t1*t48*t51*s34+3.D0*t1*t55*t50*s34-t2*t8
     #*t17*s12-t9*s45*t18-t3*s14*t66-3.D0*t51*s14*t66-3.D0*t48*s12*s14*s
     #23*t50
      hh24 = Vc*t76/s14/s23/t50/s34/s13/s12/3.D0
	VTEMP(32) = HH24
	VH24(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t1 = s13**2
      t2 = t1*s13
      t4 = s34*s12
      t8 = N**2
      t9 = s14**2
      t20 = s23**2
      t22 = s24**2
      t27 = t22**2
      t31 = t20**2
      t36 = s35**2
      t37 = t36*s35
      t40 = s34*s12*s24
      t42 = t9**2
      t44 = s23*s24
      t45 = t44*s13
      t48 = s45**2
      t53 = t48**2
      t59 = t48*s45
      t64 = t44*s14
      t74 = -3.D0*t2*s24*t4*s14+3.D0*t8*t9*t2*s23*s24-3.D0*t9*s14*s24*t4
     #*s13-3.D0*t20*s23*t22*t4+3.D0*t8*t20*t27*s14+3.D0*t8*t31*t22*s14-t
     #37*s45*t40+3.D0*t8*t42*t45-t48*t36*s23*s34*s12+t8*t53*t45+t8*t48*t
     #36*t45-s35*t59*t40+t8*s35*t59*t64-t53*s23*t4+t8*t37*s45*t64-3.D0*t
     #27*s23*t4
      hh24 = Vc*t74/s23/t22/s34/s12/s14/s13/3.D0
	VTEMP(33) = HH24
c       g + g --> g + qi
      t1 = N**2
      t2 = s34**2
      t3 = t1*t2
      t4 = s35**2
      t5 = t4*s35
      t14 = t2**2
      t16 = s12*s23
      t18 = s14**2
      t19 = t18**2
      t22 = t1*t14
      t24 = s25*s13*s24
      t26 = t3*s25
      t30 = t18*s14
      t36 = s25*s23*s14
      t38 = s15**2
      t41 = s13*s24
      t56 = s24*s34
      t63 = t38*s12
      t66 = 3.D0*t3*t5*s12*s24-t2*s45*t4*s12*s23-t14*s45*t16-t19*s45*t16
     #+t22*t24+t26*t4*s13*s24-t30*s45*s12*s13*s24+t22*t36-t38*s45*s12*t4
     #1*s14+3.D0*t1*t5*s24*s34*s23*s14+3.D0*t22*s35*s12*s24-t4*s45*s13*t
     #56*s23+t26*t4*s23*s14-t18*s45*t63*s23
      t67 = t1*t18
      t68 = t38*s15
      t73 = t2*s34
      t80 = s25**2
      t82 = t1*t80*s25
      t83 = s24**2
      t87 = t83**2
      t88 = t1*t87
      t90 = t1*t19
      t96 = t67*s25
      t114 = s25*s12*s34
      t123 = 3.D0*t67*t68*s24*s23+3.D0*t1*t73*s35*s24*s23*s14+t82*t83*s2
     #3*s14+t88*t36+3.D0*t90*s15*s24*s23+t90*t24+t96*t38*s13*s24+t82*s12
     #*t83*s34-t73*s45*t41*s23+t96*t63*s34+3.D0*t1*t68*s12*t56*s14+t90*t
     #114+t88*t114+3.D0*t1*t30*s15*s12*s24*s34
      hh24 = Vc*(t66+t123)/s12/s13/t83/s34/s23/s14/3.D0
	VTEMP(34) = HH24
	VH24(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t1 = s13**2
      t2 = t1*s13
      t6 = N**2
      t7 = s12*s14*t6
      t9 = s15**2
      t12 = s34*s12
      t13 = s14*t6
      t16 = s23**2
      t17 = t16**2
      t20 = s24*s14*t6
      t24 = s24*s12
      t25 = t24*t6
      t27 = s25**2
      t28 = t27*s25
      t31 = t12*t6
      t37 = t1**2
      t40 = s45**2
      t41 = t40*s45
      t50 = s34**2
      t51 = t50**2
      t61 = s23*s14*t6
      t63 = t2*s25*s34*t7+s25*t9*s13*t12*t13+t17*s25*t20+t1*s45*t9*t25+t
     #28*s24*s23*t31+t16*s23*s25*s24*t31+t37*s45*t25+t41*s24*s23*s34*s14
     #*t6+t1*s25*t9*t20+t51*s45*t25+t2*s45*s23*t7+t50*s34*s45*s24*t61
      t65 = s35*t9*s24
      t66 = s13*s12
      t67 = t66*t13
      t71 = t2*s15*s24
      t75 = t9*s15*s24
      t93 = t2*s35
      t105 = -3.D0*t65*t67+3.D0*t71*t31+3.D0*t75*s23*s13*s14*t6+3.D0*t75
     #*s13*t31+3.D0*t71*t61+t16*t28*t20+t37*s25*t20-3.D0*t65*t66*s14-3.D
     #0*t93*s24*t7+t50*t41*t25-3.D0*t93*t24*s14+s45*t9*s23*t67
      t108 = s24**2
      hh24 = -Vc*(t63+t105)/t108/s23/s13/s34/s12/s14/t6/3.D0
	VTEMP(35) = HH24
c       g + g --> g + g
      hh24 = 2.D0/s13*(s23*s14+s12*s34)*Vc*N**3*(s12**4+s13**4+s14**4+s1
     #5**4+s23**4+s24**4+s25**4+s34**4+s35**4+s45**4)/s12/s24/s34/s23/s1
     #4
	VTEMP(36) = HH24
	VH24(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
