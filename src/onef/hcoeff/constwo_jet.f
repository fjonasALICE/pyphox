C termes constant
C ----------------------------------------------------
	SUBROUTINE VEC_CONSO(S12,S13,S14,S15,S23,S24,S25,S34,S35,S45,
     #	VCONS)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VCONS(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VCONS(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      hcons = 0.D0
	VTEMP(1) = HCONS
c       qi + qk --> g + qk
      hcons = 0.D0
	VTEMP(2) = HCONS
	VCONS(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      hcons = 0.D0
	VTEMP(3) = HCONS
c       qi + qk --> qk + g
      hcons = 0.D0
	VTEMP(4) = HCONS
	VCONS(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      hcons = 0.D0
	VTEMP(5) = HCONS
c       qi + qbk --> g + qbk
      hcons = 0.D0
	VTEMP(6) = HCONS
	VCONS(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      hcons = 0.D0
	VTEMP(7) = HCONS
c       qi + qbk --> qbk + g
      hcons = 0.D0
	VTEMP(8) = HCONS
	VCONS(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      acons = 0.D0
      bcons = 0.D0
      ccons = 0.D0
	VTEMP(9) = ACONS + BCONS + CCONS
c       qi + qi --> g + qi
      acons = 0.D0
      bcons = 0.D0
      ccons = 0.D0
	VTEMP(10) = ACONS + BCONS + CCONS
	VCONS(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      acons = 0.D0
      bcons = 0.D0
      ccons = 0.D0
	VTEMP(11) = ACONS + BCONS + CCONS
	VCONS(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      hcons = 0.D0
	VTEMP(12) = HCONS
c       qi + qbi --> g + qbk
      hcons = 0.D0
	VTEMP(13) = HCONS
	VCONS(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      acons = 0.D0
      bcons = 0.D0
      ccons = 0.D0
	VTEMP(14) = ACONS + BCONS + CCONS
c       qi + qbi --> g + qbi
      acons = 0.D0
      bcons = 0.D0
      ccons = 0.D0
	VTEMP(15) = ACONS + BCONS + CCONS
	VCONS(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      hcons = 0.D0
	VTEMP(16) = HCONS
c       qi + qbi --> qbk + g
      hcons = 0.D0
	VTEMP(17) = HCONS
c       qi + qbi --> qi + g
      acons = 0.D0
      bcons = 0.D0
      ccons = 0.D0
	VTEMP(18) = ACONS + BCONS + CCONS
c       qi + qbi --> qbi + g
      acons = 0.D0
      bcons = 0.D0
      ccons = 0.D0
	VTEMP(19) = ACONS + BCONS + CCONS
c       qi + qbi --> g + g
      t1 = s15**2
      t2 = s25**2
      t5 = N**2
      hcons = Vc*(t1+t2)*(-t5*s24*s13-t5*s14*s23+t5*s34*s12+s34*s12)/s14
     #/s24/s34/s23/s13/t5
	VTEMP(20) = HCONS
	VCONS(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      hcons = 0.D0
	VTEMP(21) = HCONS
c       qi + g --> qbk + qk
      hcons = 0.D0
	VTEMP(22) = HCONS
	VCONS(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      hcons = 0.D0
	VTEMP(23) = HCONS
c       qi + g --> qk + qbk
      hcons = 0.D0
	VTEMP(24) = HCONS
	VCONS(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      acons = 0.D0
      bcons = 0.D0
      ccons = 0.D0
	VTEMP(25) = ACONS + BCONS + CCONS
	VCONS(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t1 = s35**2
      t2 = s15**2
      t5 = N**2
      t10 = s24*s13
      hcons = -Vc*(t1+t2)*(s34*t5*s12+s14*t5*s23-t10-t10*t5)/s24/s14/s12
     #/s34/s23/t5
	VTEMP(26) = HCONS
c       qi + g --> g + g
      hcons = 0.D0
	VTEMP(27) = HCONS
	VCONS(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      hcons = 0.D0
	VTEMP(28) = HCONS
c       qi + g --> qbk + qi
      hcons = 0.D0
	VTEMP(29) = HCONS
c       qi + g --> qi + qi
      acons = 0.D0
      bcons = 0.D0
      ccons = 0.D0
	VTEMP(30) = ACONS + BCONS + CCONS
c       qi + g --> qbi + qi
      acons = 0.D0
      bcons = 0.D0
      ccons = 0.D0
	VTEMP(31) = ACONS + BCONS + CCONS
c       qi + g --> g + qi
      t1 = s15**2
      t2 = s45**2
      t5 = N**2
      hcons = -Vc*(t1+t2)*(t5*s34*s12+t5*s13*s24-s14*s23-t5*s23*s14)/s23
     #/s13/s12/s34/s24/t5
	VTEMP(32) = HCONS
	VCONS(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t1 = s35**2
      t2 = s45**2
      t5 = N**2
      t10 = s34*s12
      hcons = Vc*(t1+t2)*(-t5*s24*s13-s23*t5*s14+t10+t10*t5)/s12/s24/s13
     #/s23/s14/t5
	VTEMP(33) = HCONS
c       g + g --> g + qi
      hcons = 0.D0
	VTEMP(34) = HCONS
	VCONS(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      hcons = 0.D0
	VTEMP(35) = HCONS
c       g + g --> g + g
      hcons = 0.D0
	VTEMP(36) = HCONS
	VCONS(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
