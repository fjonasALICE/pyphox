C*****************************************************
CC  TERMES QUI VIENNENT DE 5 COLLINEAIRE A 1
C*****************************************************
C PARTIE EN 1/(1-Z1)+
	SUBROUTINE VEC_15ZO(SC,TC,UC,Z1,M,PTM,VPART15Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VPART15Z(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VPART15Z(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      t0 = -A(SC,TC,UC,N)*(-ANM4_QQ(z1)-A4_QQ(z1)*dlog(ptm**2/M**2)+FAQQ
     #(z1))
	VTEMP(1) = t0
c       qi + qk --> g + qk
      t0 = -C(TC,SC,UC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2)-FAGQ(
     #z1))/CF/2.D0
	VTEMP(2) = t0
	VPART15Z(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      t0 = 0.D0
	VTEMP(3) = t0
c       qi + qk --> qk + g
      t0 = C(UC,SC,TC,N)*(-ANM4_GQ(z1)-A4_GQ(z1)*dlog(ptm**2/M**2)+FAGQ(
     #z1))/CF/2.D0
	VTEMP(4) = t0
	VPART15Z(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      t0 = A(UC,TC,SC,N)*(ANM4_QQ(z1)+A4_QQ(z1)*dlog(ptm**2/M**2)-FAQQ(z
     #1))
	VTEMP(5) = t0
c       qi + qbk --> g + qbk
      t0 = -C(TC,SC,UC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2)-FAGQ(
     #z1))/CF/2.D0
	VTEMP(6) = t0
	VPART15Z(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      t0 = 0.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + g
      t0 = -C(UC,SC,TC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2)-FAGQ(
     #z1))/CF/2.D0
	VTEMP(8) = t0
	VPART15Z(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      t0 = -(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))*(-ANM4_QQ(z1)-A4
     #_QQ(z1)*dlog(ptm**2/M**2)+FAQQ(z1))
	VTEMP(9) = t0
c       qi + qi --> g + qi
      t0 = C(TC,SC,UC,N)*(-ANM4_GQ(z1)-A4_GQ(z1)*dlog(ptm**2/M**2)+FAGQ(
     #z1))/CF/2.D0
	VTEMP(10) = t0
	VPART15Z(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      t0 = -C(UC,SC,TC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2)-FAGQ(
     #z1))/CF/2.D0
	VTEMP(11) = t0
	VPART15Z(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      t0 = A(UC,SC,TC,N)*(ANM4_QQ(z1)+A4_QQ(z1)*dlog(ptm**2/M**2)-FAQQ(z
     #1))
	VTEMP(12) = t0
c       qi + qbi --> g + qbk
      t0 = 0.D0
	VTEMP(13) = t0
	VPART15Z(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      t0 = (ANM4_QQ(z1)+A4_QQ(z1)*dlog(ptm**2/M**2)-FAQQ(z1))*(A(UC,TC,S
     #C,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(14) = t0
c       qi + qbi --> g + qbi
      t0 = -C(TC,SC,UC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2)-FAGQ(
     #z1))/CF/2.D0
	VTEMP(15) = t0
	VPART15Z(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      t0 = 0.D0
	VTEMP(16) = t0
c       qi + qbi --> qbk + g
      t0 = 0.D0
	VTEMP(17) = t0
c       qi + qbi --> qi + g
      t0 = 0.D0
	VTEMP(18) = t0
c       qi + qbi --> qbi + g
      t0 = -C(UC,SC,TC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2)-FAGQ(
     #z1))/CF/2.D0
	VTEMP(19) = t0
c       qi + qbi --> g + g
      t0 = C(SC,TC,UC,N)*(ANM4_QQ(z1)+A4_QQ(z1)*dlog(ptm**2/M**2)-FAQQ(z
     #1))
	VTEMP(20) = t0
	VPART15Z(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      t0 = 0.D0
	VTEMP(21) = t0
c       qi + g --> qbk + qk
      t0 = -C(SC,TC,UC,N)*(-ANM4_GQ(z1)-A4_GQ(z1)*dlog(ptm**2/M**2)+FAGQ
     #(z1))/CF/2.D0
	VTEMP(22) = t0
	VPART15Z(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      t0 = 0.D0
	VTEMP(23) = t0
c       qi + g --> qk + qbk
      t0 = C(SC,TC,UC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2)-FAGQ(z
     #1))/CF/2.D0
	VTEMP(24) = t0
	VPART15Z(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      t0 = C(SC,TC,UC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2)-FAGQ(z
     #1))/CF/2.D0
	VTEMP(25) = t0
	VPART15Z(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t0 = -C(TC,SC,UC,N)*(ANM4_QQ(z1)+A4_QQ(z1)*dlog(ptm**2/M**2)-FAQQ(
     #z1))
	VTEMP(26) = t0
c       qi + g --> g + g
      t0 = N*Dg(SC,TC,UC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2)-FAG
     #Q(z1))/(N-1.D0)/(N+1.D0)
	VTEMP(27) = t0
	VPART15Z(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      t0 = 0.D0
	VTEMP(28) = t0
c       qi + g --> qbk + qi
      t0 = 0.D0
	VTEMP(29) = t0
c       qi + g --> qi + qi
      t0 = 0.D0
	VTEMP(30) = t0
c       qi + g --> qbi + qi
      t0 = C(SC,TC,UC,N)*(ANM4_GQ(z1)+A4_GQ(z1)*dlog(ptm**2/M**2)-FAGQ(z
     #1))/CF/2.D0
	VTEMP(31) = t0
c       qi + g --> g + qi
      t0 = C(UC,SC,TC,N)*(-ANM4_QQ(z1)-A4_QQ(z1)*dlog(ptm**2/M**2)+FAQQ(
     #z1))
	VTEMP(32) = t0
	VPART15Z(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t0 = C(SC,TC,UC,N)*(ANM4_GG(z1)+A4_GG(z1)*dlog(ptm**2/M**2)-FAGG(z
     #1))
	VTEMP(33) = t0
c       g + g --> g + qi
      t0 = -C(UC,SC,TC,N)*(N-1.D0)*(N+1.D0)*(ANM4_QG(z1)+A4_QG(z1)*dlog(
     #ptm**2/M**2)-FAQG(z1))/N
	VTEMP(34) = t0
	VPART15Z(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t0 = -C(TC,SC,UC,N)*(N-1.D0)*(N+1.D0)*(ANM4_QG(z1)+A4_QG(z1)*dlog(
     #ptm**2/M**2)-FAQG(z1))/N
	VTEMP(35) = t0
c       g + g --> g + g
      t0 = Dg(SC,TC,UC,N)*(ANM4_GG(z1)+A4_GG(z1)*dlog(ptm**2/M**2)-FAGG(
     #z1))
	VTEMP(36) = t0
	VPART15Z(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
C PARTIE EN LOG(1-Z1)/(1-Z1)+
	SUBROUTINE VEC_15LO(SC,TC,UC,Z1,VPART15L)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VPART15L(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VPART15L(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      t0 = -A(SC,TC,UC,N)*FBQQ(z1)
	VTEMP(1) = t0
c       qi + qk --> g + qk
      t0 = C(TC,SC,UC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(2) = t0
	VPART15L(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      t0 = 0.D0
	VTEMP(3) = t0
c       qi + qk --> qk + g
      t0 = C(UC,SC,TC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(4) = t0
	VPART15L(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      t0 = -A(UC,TC,SC,N)*FBQQ(z1)
	VTEMP(5) = t0
c       qi + qbk --> g + qbk
      t0 = C(TC,SC,UC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(6) = t0
	VPART15L(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      t0 = 0.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + g
      t0 = C(UC,SC,TC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(8) = t0
	VPART15L(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      t0 = -FBQQ(z1)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))
	VTEMP(9) = t0
c       qi + qi --> g + qi
      t0 = C(TC,SC,UC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(10) = t0
	VPART15L(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      t0 = C(UC,SC,TC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(11) = t0
	VPART15L(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      t0 = -A(UC,SC,TC,N)*FBQQ(z1)
	VTEMP(12) = t0
c       qi + qbi --> g + qbk
      t0 = 0.D0
	VTEMP(13) = t0
	VPART15L(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      t0 = -FBQQ(z1)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(14) = t0
c       qi + qbi --> g + qbi
      t0 = C(TC,SC,UC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(15) = t0
	VPART15L(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      t0 = 0.D0
	VTEMP(16) = t0
c       qi + qbi --> qbk + g
      t0 = 0.D0
	VTEMP(17) = t0
c       qi + qbi --> qi + g
      t0 = 0.D0
	VTEMP(18) = t0
c       qi + qbi --> qbi + g
      t0 = C(UC,SC,TC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(19) = t0
c       qi + qbi --> g + g
      t0 = -C(SC,TC,UC,N)*FBQQ(z1)
	VTEMP(20) = t0
	VPART15L(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      t0 = 0.D0
	VTEMP(21) = t0
c       qi + g --> qbk + qk
      t0 = -C(SC,TC,UC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(22) = t0
	VPART15L(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      t0 = 0.D0
	VTEMP(23) = t0
c       qi + g --> qk + qbk
      t0 = -C(SC,TC,UC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(24) = t0
	VPART15L(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      t0 = -C(SC,TC,UC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(25) = t0
	VPART15L(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t0 = C(TC,SC,UC,N)*FBQQ(z1)
	VTEMP(26) = t0
c       qi + g --> g + g
      t0 = -N*Dg(SC,TC,UC,N)*FBGQ(z1)/(N-1.D0)/(N+1.D0)
	VTEMP(27) = t0
	VPART15L(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      t0 = 0.D0
	VTEMP(28) = t0
c       qi + g --> qbk + qi
      t0 = 0.D0
	VTEMP(29) = t0
c       qi + g --> qi + qi
      t0 = 0.D0
	VTEMP(30) = t0
c       qi + g --> qbi + qi
      t0 = -C(SC,TC,UC,N)*FBGQ(z1)/CF/2.D0
	VTEMP(31) = t0
c       qi + g --> g + qi
      t0 = C(UC,SC,TC,N)*FBQQ(z1)
	VTEMP(32) = t0
	VPART15L(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t0 = -C(SC,TC,UC,N)*FBGG(z1)
	VTEMP(33) = t0
c       g + g --> g + qi
      t0 = C(UC,SC,TC,N)*FBQG(z1)*(N-1.D0)*(N+1.D0)/N
	VTEMP(34) = t0
	VPART15L(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t0 = C(TC,SC,UC,N)*FBQG(z1)*(N-1.D0)*(N+1.D0)/N
	VTEMP(35) = t0
c       g + g --> g + g
      t0 = -Dg(SC,TC,UC,N)*FBGG(z1)
	VTEMP(36) = t0
	VPART15L(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
C PARTIE EN DELTA(1-Z1)
	SUBROUTINE VEC_15DO(SC,TC,UC,M,PTM,VPART15D)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VPART15D(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VPART15D(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      t0 = -A(SC,TC,UC,N)*(-dlog(ptm**2/M**2)*B_QQ()+FCQQ(1.D0))
	VTEMP(1) = t0
c       qi + qk --> g + qk
      t0 = C(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(2) = t0
	VPART15D(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      t0 = 0.D0
	VTEMP(3) = t0
c       qi + qk --> qk + g
      t0 = C(UC,SC,TC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(4) = t0
	VPART15D(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      t0 = -A(UC,TC,SC,N)*(-dlog(ptm**2/M**2)*B_QQ()+FCQQ(1.D0))
	VTEMP(5) = t0
c       qi + qbk --> g + qbk
      t0 = C(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(6) = t0
	VPART15D(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      t0 = 0.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + g
      t0 = C(UC,SC,TC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(8) = t0
	VPART15D(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      t0 = -(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))*(-dlog(ptm**2/M*
     #*2)*B_QQ()+FCQQ(1.D0))
	VTEMP(9) = t0
c       qi + qi --> g + qi
      t0 = C(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(10) = t0
	VPART15D(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      t0 = C(UC,SC,TC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(11) = t0
	VPART15D(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      t0 = A(UC,SC,TC,N)*(B_QQ()*dlog(ptm**2/M**2)-FCQQ(1.D0))
	VTEMP(12) = t0
c       qi + qbi --> g + qbk
      t0 = 0.D0
	VTEMP(13) = t0
	VPART15D(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      t0 = (B_QQ()*dlog(ptm**2/M**2)-FCQQ(1.D0))*(A(UC,TC,SC,N)+A(UC,SC,
     #TC,N)+B(UC,TC,SC,N))
	VTEMP(14) = t0
c       qi + qbi --> g + qbi
      t0 = C(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(15) = t0
	VPART15D(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      t0 = 0.D0
	VTEMP(16) = t0
c       qi + qbi --> qbk + g
      t0 = 0.D0
	VTEMP(17) = t0
c       qi + qbi --> qi + g
      t0 = 0.D0
	VTEMP(18) = t0
c       qi + qbi --> qbi + g
      t0 = C(UC,SC,TC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(19) = t0
c       qi + qbi --> g + g
      t0 = C(SC,TC,UC,N)*(B_QQ()*dlog(ptm**2/M**2)-FCQQ(1.D0))
	VTEMP(20) = t0
	VPART15D(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      t0 = 0.D0
	VTEMP(21) = t0
c       qi + g --> qbk + qk
      t0 = -C(SC,TC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(22) = t0
	VPART15D(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      t0 = 0.D0
	VTEMP(23) = t0
c       qi + g --> qk + qbk
      t0 = -C(SC,TC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(24) = t0
	VPART15D(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      t0 = -C(SC,TC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(25) = t0
	VPART15D(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t0 = -C(TC,SC,UC,N)*(B_QQ()*dlog(ptm**2/M**2)-FCQQ(1.D0))
	VTEMP(26) = t0
c       qi + g --> g + g
      t0 = -N*Dg(SC,TC,UC,N)*FCGQ(1.D0)/(N-1.D0)/(N+1.D0)
	VTEMP(27) = t0
	VPART15D(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      t0 = 0.D0
	VTEMP(28) = t0
c       qi + g --> qbk + qi
      t0 = 0.D0
	VTEMP(29) = t0
c       qi + g --> qi + qi
      t0 = 0.D0
	VTEMP(30) = t0
c       qi + g --> qbi + qi
      t0 = -C(SC,TC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(31) = t0
c       qi + g --> g + qi
      t0 = C(UC,SC,TC,N)*(-dlog(ptm**2/M**2)*B_QQ()+FCQQ(1.D0))
	VTEMP(32) = t0
	VPART15D(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t0 = C(SC,TC,UC,N)*(B_GG()*dlog(ptm**2/M**2)-FCGG(1.D0))
	VTEMP(33) = t0
c       g + g --> g + qi
      t0 = C(UC,SC,TC,N)*FCQG(1.D0)*(N-1.D0)*(N+1.D0)/N
	VTEMP(34) = t0
	VPART15D(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t0 = C(TC,SC,UC,N)*FCQG(1.D0)*(N-1.D0)*(N+1.D0)/N
	VTEMP(35) = t0
c       g + g --> g + g
      t0 = -Dg(SC,TC,UC,N)*(-B_GG()*dlog(ptm**2/M**2)+FCGG(1.D0))
	VTEMP(36) = t0
	VPART15D(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
C*****************************************************
CC  TERMES QUI VIENNENT DE 5 COLLINEAIRE A 2
C*****************************************************
C PARTIE EN 1/(1-Z2)+
	SUBROUTINE VEC_25ZO(SC,TC,UC,Z2,M,PTM,VPART25Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VPART25Z(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VPART25Z(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      t0 = A(SC,TC,UC,N)*(ANM4_QQ(z2)+A4_QQ(z2)*dlog(ptm**2/M**2)-FAQQ(z
     #2))
	VTEMP(1) = t0
c       qi + qk --> g + qk
      t0 = 0.D0
	VTEMP(2) = t0
	VPART25Z(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      t0 = -C(TC,SC,UC,N)*(ANM4_GQ(z2)+A4_GQ(z2)*dlog(ptm**2/M**2)-FAGQ(
     #z2))/CF/2.D0
	VTEMP(3) = t0
c       qi + qk --> qk + g
      t0 = 0.D0
	VTEMP(4) = t0
	VPART25Z(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      t0 = A(UC,TC,SC,N)*(ANM4_QQ(z2)+A4_QQ(z2)*dlog(ptm**2/M**2)-FAQQ(z
     #2))
	VTEMP(5) = t0
c       qi + qbk --> g + qbk
      t0 = 0.D0
	VTEMP(6) = t0
	VPART25Z(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      t0 = -C(TC,SC,UC,N)*(ANM4_GQ(z2)+A4_GQ(z2)*dlog(ptm**2/M**2)-FAGQ(
     #z2))/CF/2.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + g
      t0 = 0.D0
	VTEMP(8) = t0
	VPART25Z(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      t0 = (A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))*(A4_QQ(z2)*dlog(p
     #tm**2/M**2)+ANM4_QQ(z2)-FAQQ(z2))
	VTEMP(9) = t0
c       qi + qi --> g + qi
      t0 = C(UC,SC,TC,N)*(-ANM4_GQ(z2)-A4_GQ(z2)*dlog(ptm**2/M**2)+FAGQ(
     #z2))/CF/2.D0
	VTEMP(10) = t0
	VPART25Z(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      t0 = -C(TC,SC,UC,N)*(ANM4_GQ(z2)+A4_GQ(z2)*dlog(ptm**2/M**2)-FAGQ(
     #z2))/CF/2.D0
	VTEMP(11) = t0
	VPART25Z(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      t0 = A(UC,SC,TC,N)*(ANM4_QQ(z2)+A4_QQ(z2)*dlog(ptm**2/M**2)-FAQQ(z
     #2))
	VTEMP(12) = t0
c       qi + qbi --> g + qbk
      t0 = 0.D0
	VTEMP(13) = t0
	VPART25Z(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      t0 = (ANM4_QQ(z2)+A4_QQ(z2)*dlog(ptm**2/M**2)-FAQQ(z2))*(A(UC,TC,S
     #C,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(14) = t0
c       qi + qbi --> g + qbi
      t0 = 0.D0
	VTEMP(15) = t0
	VPART25Z(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      t0 = 0.D0
	VTEMP(16) = t0
c       qi + qbi --> qbk + g
      t0 = 0.D0
	VTEMP(17) = t0
c       qi + qbi --> qi + g
      t0 = -C(TC,SC,UC,N)*(ANM4_GQ(z2)+A4_GQ(z2)*dlog(ptm**2/M**2)-FAGQ(
     #z2))/CF/2.D0
	VTEMP(18) = t0
c       qi + qbi --> qbi + g
      t0 = 0.D0
	VTEMP(19) = t0
c       qi + qbi --> g + g
      t0 = C(SC,TC,UC,N)*(ANM4_QQ(z2)+A4_QQ(z2)*dlog(ptm**2/M**2)-FAQQ(z
     #2))
	VTEMP(20) = t0
	VPART25Z(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      t0 = 2.D0*CF*A(SC,TC,UC,N)*(ANM4_QG(z2)+A4_QG(z2)*dlog(ptm**2/M**2
     #)-FAQG(z2))
	VTEMP(21) = t0
c       qi + g --> qbk + qk
      t0 = 2.D0*CF*A(TC,SC,UC,N)*(ANM4_QG(z2)+A4_QG(z2)*dlog(ptm**2/M**2
     #)-FAQG(z2))
	VTEMP(22) = t0
	VPART25Z(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      t0 = -2.D0*CF*A(UC,TC,SC,N)*(-ANM4_QG(z2)-A4_QG(z2)*dlog(ptm**2/M*
     #*2)+FAQG(z2))
	VTEMP(23) = t0
c       qi + g --> qk + qbk
      t0 = 2.D0*CF*A(UC,SC,TC,N)*(ANM4_QG(z2)+A4_QG(z2)*dlog(ptm**2/M**2
     #)-FAQG(z2))
	VTEMP(24) = t0
	VPART25Z(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      t0 = 2.D0*CF*(ANM4_QG(z2)+A4_QG(z2)*dlog(ptm**2/M**2)-FAQG(z2))*(A
     #(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(25) = t0
	VPART25Z(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t0 = -C(TC,SC,UC,N)*(ANM4_GG(z2)+A4_GG(z2)*dlog(ptm**2/M**2)-FAGG(
     #z2))
	VTEMP(26) = t0
c       qi + g --> g + g
      t0 = -C(SC,TC,UC,N)*(N-1.D0)*(N+1.D0)*(-ANM4_QG(z2)-A4_QG(z2)*dlog
     #(ptm**2/M**2)+FAQG(z2))/N
	VTEMP(27) = t0
	VPART25Z(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      t0 = 2.D0*CF*A(SC,UC,TC,N)*(ANM4_QG(z2)+A4_QG(z2)*dlog(ptm**2/M**2
     #)-FAQG(z2))
	VTEMP(28) = t0
c       qi + g --> qbk + qi
      t0 = 2.D0*CF*A(TC,UC,SC,N)*(ANM4_QG(z2)+A4_QG(z2)*dlog(ptm**2/M**2
     #)-FAQG(z2))
	VTEMP(29) = t0
c       qi + g --> qi + qi
      t0 = 2.D0*CF*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))*(ANM4_QG(
     #z2)+A4_QG(z2)*dlog(ptm**2/M**2)-FAQG(z2))
	VTEMP(30) = t0
c       qi + g --> qbi + qi
      t0 = 2.D0*CF*(A(TC,UC,SC,N)+A(TC,SC,UC,N)+B(TC,UC,SC,N))*(ANM4_QG(
     #z2)+A4_QG(z2)*dlog(ptm**2/M**2)-FAQG(z2))
	VTEMP(31) = t0
c       qi + g --> g + qi
      t0 = -C(UC,SC,TC,N)*(ANM4_GG(z2)+A4_GG(z2)*dlog(ptm**2/M**2)-FAGG(
     #z2))
	VTEMP(32) = t0
	VPART25Z(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t0 = C(SC,TC,UC,N)*(ANM4_GG(z2)+A4_GG(z2)*dlog(ptm**2/M**2)-FAGG(z
     #2))
	VTEMP(33) = t0
c       g + g --> g + qi
      t0 = -C(TC,SC,UC,N)*(N-1.D0)*(N+1.D0)*(ANM4_QG(z2)+A4_QG(z2)*dlog(
     #ptm**2/M**2)-FAQG(z2))/N
	VTEMP(34) = t0
	VPART25Z(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t0 = -C(UC,SC,TC,N)*(N-1.D0)*(N+1.D0)*(ANM4_QG(z2)+A4_QG(z2)*dlog(
     #ptm**2/M**2)-FAQG(z2))/N
	VTEMP(35) = t0
c       g + g --> g + g
      t0 = Dg(SC,TC,UC,N)*(ANM4_GG(z2)+A4_GG(z2)*dlog(ptm**2/M**2)-FAGG(
     #z2))
	VTEMP(36) = t0
	VPART25Z(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
C PARTIE EN LOG(1-Z2)/(1-Z2)+
	SUBROUTINE VEC_25LO(SC,TC,UC,Z2,VPART25L)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VPART25L(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VPART25L(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      t0 = -A(SC,TC,UC,N)*FBQQ(z2)
	VTEMP(1) = t0
c       qi + qk --> g + qk
      t0 = 0.D0
	VTEMP(2) = t0
	VPART25L(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      t0 = C(TC,SC,UC,N)*FBGQ(z2)/CF/2.D0
	VTEMP(3) = t0
c       qi + qk --> qk + g
      t0 = 0.D0
	VTEMP(4) = t0
	VPART25L(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      t0 = -A(UC,TC,SC,N)*FBQQ(z2)
	VTEMP(5) = t0
c       qi + qbk --> g + qbk
      t0 = 0.D0
	VTEMP(6) = t0
	VPART25L(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      t0 = C(TC,SC,UC,N)*FBGQ(z2)/CF/2.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + g
      t0 = 0.D0
	VTEMP(8) = t0
	VPART25L(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      t0 = -FBQQ(z2)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))
	VTEMP(9) = t0
c       qi + qi --> g + qi
      t0 = C(UC,SC,TC,N)*FBGQ(z2)/CF/2.D0
	VTEMP(10) = t0
	VPART25L(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      t0 = C(TC,SC,UC,N)*FBGQ(z2)/CF/2.D0
	VTEMP(11) = t0
	VPART25L(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      t0 = -A(UC,SC,TC,N)*FBQQ(z2)
	VTEMP(12) = t0
c       qi + qbi --> g + qbk
      t0 = 0.D0
	VTEMP(13) = t0
	VPART25L(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      t0 = -FBQQ(z2)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(14) = t0
c       qi + qbi --> g + qbi
      t0 = 0.D0
	VTEMP(15) = t0
	VPART25L(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      t0 = 0.D0
	VTEMP(16) = t0
c       qi + qbi --> qbk + g
      t0 = 0.D0
	VTEMP(17) = t0
c       qi + qbi --> qi + g
      t0 = C(TC,SC,UC,N)*FBGQ(z2)/CF/2.D0
	VTEMP(18) = t0
c       qi + qbi --> qbi + g
      t0 = 0.D0
	VTEMP(19) = t0
c       qi + qbi --> g + g
      t0 = -C(SC,TC,UC,N)*FBQQ(z2)
	VTEMP(20) = t0
	VPART25L(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      t0 = -2.D0*CF*A(SC,TC,UC,N)*FBQG(z2)
	VTEMP(21) = t0
c       qi + g --> qbk + qk
      t0 = -2.D0*CF*A(TC,SC,UC,N)*FBQG(z2)
	VTEMP(22) = t0
	VPART25L(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      t0 = -2.D0*CF*A(UC,TC,SC,N)*FBQG(z2)
	VTEMP(23) = t0
c       qi + g --> qk + qbk
      t0 = -2.D0*CF*A(UC,SC,TC,N)*FBQG(z2)
	VTEMP(24) = t0
	VPART25L(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      t0 = -2.D0*CF*FBQG(z2)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(25) = t0
	VPART25L(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t0 = C(TC,SC,UC,N)*FBGG(z2)
	VTEMP(26) = t0
c       qi + g --> g + g
      t0 = -C(SC,TC,UC,N)*FBQG(z2)*(N-1.D0)*(N+1.D0)/N
	VTEMP(27) = t0
	VPART25L(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      t0 = -2.D0*CF*A(SC,UC,TC,N)*FBQG(z2)
	VTEMP(28) = t0
c       qi + g --> qbk + qi
      t0 = -2.D0*CF*A(TC,UC,SC,N)*FBQG(z2)
	VTEMP(29) = t0
c       qi + g --> qi + qi
      t0 = -2.D0*CF*FBQG(z2)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))
	VTEMP(30) = t0
c       qi + g --> qbi + qi
      t0 = -2.D0*CF*FBQG(z2)*(A(TC,UC,SC,N)+A(TC,SC,UC,N)+B(TC,UC,SC,N))
	VTEMP(31) = t0
c       qi + g --> g + qi
      t0 = C(UC,SC,TC,N)*FBGG(z2)
	VTEMP(32) = t0
	VPART25L(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t0 = -C(SC,TC,UC,N)*FBGG(z2)
	VTEMP(33) = t0
c       g + g --> g + qi
      t0 = C(TC,SC,UC,N)*FBQG(z2)*(N-1.D0)*(N+1.D0)/N
	VTEMP(34) = t0
	VPART25L(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t0 = C(UC,SC,TC,N)*FBQG(z2)*(N-1.D0)*(N+1.D0)/N
	VTEMP(35) = t0
c       g + g --> g + g
      t0 = -Dg(SC,TC,UC,N)*FBGG(z2)
	VTEMP(36) = t0
	VPART25L(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
C PARTIE EN DELTA(1-Z2)
	SUBROUTINE VEC_25DO(SC,TC,UC,M,PTM,VPART25D)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VPART25D(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VPART25D(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      t0 = -A(SC,TC,UC,N)*(-dlog(ptm**2/M**2)*B_QQ()+FCQQ(1.D0))
	VTEMP(1) = t0
c       qi + qk --> g + qk
      t0 = 0.D0
	VTEMP(2) = t0
	VPART25D(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      t0 = C(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(3) = t0
c       qi + qk --> qk + g
      t0 = 0.D0
	VTEMP(4) = t0
	VPART25D(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      t0 = -A(UC,TC,SC,N)*(-dlog(ptm**2/M**2)*B_QQ()+FCQQ(1.D0))
	VTEMP(5) = t0
c       qi + qbk --> g + qbk
      t0 = 0.D0
	VTEMP(6) = t0
	VPART25D(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      t0 = C(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + g
      t0 = 0.D0
	VTEMP(8) = t0
	VPART25D(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      t0 = -(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))*(-dlog(ptm**2/M*
     #*2)*B_QQ()+FCQQ(1.D0))
	VTEMP(9) = t0
c       qi + qi --> g + qi
      t0 = C(UC,SC,TC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(10) = t0
	VPART25D(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      t0 = C(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(11) = t0
	VPART25D(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      t0 = A(UC,SC,TC,N)*(B_QQ()*dlog(ptm**2/M**2)-FCQQ(1.D0))
	VTEMP(12) = t0
c       qi + qbi --> g + qbk
      t0 = 0.D0
	VTEMP(13) = t0
	VPART25D(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      t0 = (B_QQ()*dlog(ptm**2/M**2)-FCQQ(1.D0))*(A(UC,TC,SC,N)+A(UC,SC,
     #TC,N)+B(UC,TC,SC,N))
	VTEMP(14) = t0
c       qi + qbi --> g + qbi
      t0 = 0.D0
	VTEMP(15) = t0
	VPART25D(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      t0 = 0.D0
	VTEMP(16) = t0
c       qi + qbi --> qbk + g
      t0 = 0.D0
	VTEMP(17) = t0
c       qi + qbi --> qi + g
      t0 = C(TC,SC,UC,N)*FCGQ(1.D0)/CF/2.D0
	VTEMP(18) = t0
c       qi + qbi --> qbi + g
      t0 = 0.D0
	VTEMP(19) = t0
c       qi + qbi --> g + g
      t0 = C(SC,TC,UC,N)*(B_QQ()*dlog(ptm**2/M**2)-FCQQ(1.D0))
	VTEMP(20) = t0
	VPART25D(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      t0 = -2.D0*CF*A(SC,TC,UC,N)*FCQG(1.D0)
	VTEMP(21) = t0
c       qi + g --> qbk + qk
      t0 = -2.D0*CF*A(TC,SC,UC,N)*FCQG(1.D0)
	VTEMP(22) = t0
	VPART25D(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      t0 = -2.D0*CF*A(UC,TC,SC,N)*FCQG(1.D0)
	VTEMP(23) = t0
c       qi + g --> qk + qbk
      t0 = -2.D0*CF*A(UC,SC,TC,N)*FCQG(1.D0)
	VTEMP(24) = t0
	VPART25D(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      t0 = -2.D0*CF*FCQG(1.D0)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N
     #))
	VTEMP(25) = t0
	VPART25D(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t0 = -C(TC,SC,UC,N)*(B_GG()*dlog(ptm**2/M**2)-FCGG(1.D0))
	VTEMP(26) = t0
c       qi + g --> g + g
      t0 = -C(SC,TC,UC,N)*FCQG(1.D0)*(N-1.D0)*(N+1.D0)/N
	VTEMP(27) = t0
	VPART25D(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      t0 = -2.D0*CF*A(SC,UC,TC,N)*FCQG(1.D0)
	VTEMP(28) = t0
c       qi + g --> qbk + qi
      t0 = -2.D0*CF*A(TC,UC,SC,N)*FCQG(1.D0)
	VTEMP(29) = t0
c       qi + g --> qi + qi
      t0 = -2.D0*CF*FCQG(1.D0)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N
     #))
	VTEMP(30) = t0
c       qi + g --> qbi + qi
      t0 = -2.D0*CF*FCQG(1.D0)*(A(TC,UC,SC,N)+A(TC,SC,UC,N)+B(TC,UC,SC,N
     #))
	VTEMP(31) = t0
c       qi + g --> g + qi
      t0 = -C(UC,SC,TC,N)*(B_GG()*dlog(ptm**2/M**2)-FCGG(1.D0))
	VTEMP(32) = t0
	VPART25D(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t0 = C(SC,TC,UC,N)*(B_GG()*dlog(ptm**2/M**2)-FCGG(1.D0))
	VTEMP(33) = t0
c       g + g --> g + qi
      t0 = C(TC,SC,UC,N)*FCQG(1.D0)*(N-1.D0)*(N+1.D0)/N
	VTEMP(34) = t0
	VPART25D(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t0 = C(UC,SC,TC,N)*FCQG(1.D0)*(N-1.D0)*(N+1.D0)/N
	VTEMP(35) = t0
c       g + g --> g + g
      t0 = -Dg(SC,TC,UC,N)*(-B_GG()*dlog(ptm**2/M**2)+FCGG(1.D0))
	VTEMP(36) = t0
	VPART25D(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
C*****************************************************
CC  TERMES QUI VIENNENT DE 5 COLLINEAIRE A 3
C*****************************************************
C PARTIE EN LOG(R**2)
	SUBROUTINE VEC_35RO(SC,TC,UC,ZM,R,VPART35R)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VPART35R(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VPART35R(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      t0 = CF*(5.D0/8.D0-2.D0*dlog(2.D0)-2.D0*dlog(1.D0-zm)-zm-zm**2/2.D
     #0)*dlog(R**2)*A(SC,TC,UC,N)
	VTEMP(1) = t0
c       qi + qk --> g + qk
      t0 = CF*(7.D0/8.D0+2.D0*dlog(2.D0)+2.D0*dlog(zm)-2.D0*zm+zm**2/2.D
     #0)*dlog(R**2)*A(SC,TC,UC,N)
	VTEMP(2) = t0
	VPART35R(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      t0 = 0.D0
	VTEMP(3) = t0
c       qi + qk --> qk + g
      t0 = 0.D0
	VTEMP(4) = t0
	VPART35R(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      t0 = CF*(5.D0/8.D0-2.D0*dlog(2.D0)-2.D0*dlog(1.D0-zm)-zm-zm**2/2.D
     #0)*dlog(R**2)*A(UC,TC,SC,N)
	VTEMP(5) = t0
c       qi + qbk --> g + qbk
      t0 = CF*(7.D0/8.D0+2.D0*dlog(2.D0)+2.D0*dlog(zm)-2.D0*zm+zm**2/2.D
     #0)*dlog(R**2)*A(UC,TC,SC,N)
	VTEMP(6) = t0
	VPART35R(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      t0 = 0.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + g
      t0 = 0.D0
	VTEMP(8) = t0
	VPART35R(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      t0 = CF*(5.D0/8.D0-2.D0*dlog(2.D0)-2.D0*dlog(1.D0-zm)-zm-zm**2/2.D
     #0)*dlog(R**2)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))
	VTEMP(9) = t0
c       qi + qi --> g + qi
      t0 = CF*(7.D0/8.D0+2.D0*dlog(2.D0)+2.D0*dlog(zm)-2.D0*zm+zm**2/2.D
     #0)*dlog(R**2)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))
	VTEMP(10) = t0
	VPART35R(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      t0 = 0.D0
	VTEMP(11) = t0
	VPART35R(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      t0 = CF*(5.D0/8.D0-2.D0*dlog(2.D0)-2.D0*dlog(1.D0-zm)-zm-zm**2/2.D
     #0)*dlog(R**2)*A(UC,SC,TC,N)
	VTEMP(12) = t0
c       qi + qbi --> g + qbk
      t0 = CF*(7.D0/8.D0+2.D0*dlog(2.D0)+2.D0*dlog(zm)-2.D0*zm+zm**2/2.D
     #0)*dlog(R**2)*A(UC,SC,TC,N)
	VTEMP(13) = t0
	VPART35R(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      t0 = CF*(5.D0/8.D0-2.D0*dlog(2.D0)-2.D0*dlog(1.D0-zm)-zm-zm**2/2.D
     #0)*dlog(R**2)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(14) = t0
c       qi + qbi --> g + qbi
      t0 = CF*(7.D0/8.D0+2.D0*dlog(2.D0)+2.D0*dlog(zm)-2.D0*zm+zm**2/2.D
     #0)*dlog(R**2)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(15) = t0
	VPART35R(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      t0 = (zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*C(SC,TC,
     #UC,N)
	VTEMP(16) = t0
c       qi + qbi --> qbk + g
      t0 = (zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*C(SC,TC,
     #UC,N)
	VTEMP(17) = t0
c       qi + qbi --> qi + g
      t0 = (zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*C(SC,TC,
     #UC,N)
	VTEMP(18) = t0
c       qi + qbi --> qbi + g
      t0 = (zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*C(SC,TC,
     #UC,N)
	VTEMP(19) = t0
c       qi + qbi --> g + g
      t0 = N*(-2.D0*dlog(1.D0-zm)+2.D0*dlog(zm)-4.D0*zm+zm**2-2.D0/3.D0*
     #zm**3+11.D0/6.D0)*dlog(R**2)*C(SC,TC,UC,N)
	VTEMP(20) = t0
	VPART35R(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      t0 = 0.D0
	VTEMP(21) = t0
c       qi + g --> qbk + qk
      t0 = 0.D0
	VTEMP(22) = t0
	VPART35R(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      t0 = 0.D0
	VTEMP(23) = t0
c       qi + g --> qk + qbk
      t0 = 0.D0
	VTEMP(24) = t0
	VPART35R(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      t0 = 0.D0
	VTEMP(25) = t0
	VPART35R(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t0 = -(N**2-1.D0)/N*(5.D0/8.D0-2.D0*dlog(2.D0)-2.D0*dlog(1.D0-zm)-
     #zm-zm**2/2.D0)*dlog(R**2)*C(TC,SC,UC,N)/2.D0
	VTEMP(26) = t0
c       qi + g --> g + g
      t0 = -(N**2-1.D0)/N*(7.D0/8.D0+2.D0*dlog(2.D0)+2.D0*dlog(zm)-2.D0*
     #zm+zm**2/2.D0)*dlog(R**2)*C(TC,SC,UC,N)/2.D0
	VTEMP(27) = t0
	VPART35R(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      t0 = -(zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*C(UC,SC
     #,TC,N)
	VTEMP(28) = t0
c       qi + g --> qbk + qi
      t0 = -(zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*C(UC,SC
     #,TC,N)
	VTEMP(29) = t0
c       qi + g --> qi + qi
      t0 = -(zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*C(UC,SC
     #,TC,N)
	VTEMP(30) = t0
c       qi + g --> qbi + qi
      t0 = -(zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*C(UC,SC
     #,TC,N)
	VTEMP(31) = t0
c       qi + g --> g + qi
      t0 = -N*(-2.D0*dlog(1.D0-zm)+2.D0*dlog(zm)-4.D0*zm+zm**2-2.D0/3.D0
     #*zm**3+11.D0/6.D0)*dlog(R**2)*C(UC,SC,TC,N)
	VTEMP(32) = t0
	VPART35R(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t0 = (N**2-1.D0)/N*(5.D0/8.D0-2.D0*dlog(2.D0)-2.D0*dlog(1.D0-zm)-z
     #m-zm**2/2.D0)*dlog(R**2)*C(SC,TC,UC,N)/2.D0
	VTEMP(33) = t0
c       g + g --> g + qi
      t0 = (N**2-1.D0)/N*(7.D0/8.D0+2.D0*dlog(2.D0)+2.D0*dlog(zm)-2.D0*z
     #m+zm**2/2.D0)*dlog(R**2)*C(SC,TC,UC,N)/2.D0
	VTEMP(34) = t0
	VPART35R(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t0 = (zm**3/3.D0+zm/2.D0-zm**2/2.D0-1.D0/6.D0)*dlog(R**2)*Dg(SC,TC
     #,UC,N)
	VTEMP(35) = t0
c       g + g --> g + g
      t0 = N*(-2.D0*dlog(1.D0-zm)+2.D0*dlog(zm)-4.D0*zm+zm**2-2.D0/3.D0*
     #zm**3+11.D0/6.D0)*dlog(R**2)*Dg(SC,TC,UC,N)
	VTEMP(36) = t0
	VPART35R(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
C PARTIE EN DELTA(1-Z3)
	SUBROUTINE VEC_35DO(SC,TC,UC,VPART35D)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VPART35D(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VPART35D(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      t0 = CF*(27.D0/8.D0-0.3141592653589793D1**2/3.D0+4.D0*dlog(2.D0)**
     #2.D0+dlog(2.D0)/2.D0)*A(SC,TC,UC,N)
	VTEMP(1) = t0
c       qi + qk --> g + qk
      t0 = CF*(25.D0/8.D0-0.3141592653589793D1**2/3.D0-4.D0*dlog(2.D0)**
     #2.D0-dlog(2.D0)/2.D0)*A(SC,TC,UC,N)
	VTEMP(2) = t0
	VPART35D(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      t0 = 0.D0
	VTEMP(3) = t0
c       qi + qk --> qk + g
      t0 = 0.D0
	VTEMP(4) = t0
	VPART35D(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      t0 = CF*(27.D0/8.D0-0.3141592653589793D1**2/3.D0+4.D0*dlog(2.D0)**
     #2.D0+dlog(2.D0)/2.D0)*A(UC,TC,SC,N)
	VTEMP(5) = t0
c       qi + qbk --> g + qbk
      t0 = CF*(25.D0/8.D0-0.3141592653589793D1**2/3.D0-4.D0*dlog(2.D0)**
     #2.D0-dlog(2.D0)/2.D0)*A(UC,TC,SC,N)
	VTEMP(6) = t0
	VPART35D(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      t0 = 0.D0
	VTEMP(7) = t0
c       qi + qbk --> qbk + g
      t0 = 0.D0
	VTEMP(8) = t0
	VPART35D(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      t0 = CF*(27.D0/8.D0-0.3141592653589793D1**2/3.D0+4.D0*dlog(2.D0)**
     #2.D0+dlog(2.D0)/2.D0)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))
	VTEMP(9) = t0
c       qi + qi --> g + qi
      t0 = CF*(25.D0/8.D0-0.3141592653589793D1**2/3.D0-4.D0*dlog(2.D0)**
     #2.D0-dlog(2.D0)/2.D0)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))
	VTEMP(10) = t0
	VPART35D(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      t0 = 0.D0
	VTEMP(11) = t0
	VPART35D(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      t0 = CF*(27.D0/8.D0-0.3141592653589793D1**2/3.D0+4.D0*dlog(2.D0)**
     #2.D0+dlog(2.D0)/2.D0)*A(UC,SC,TC,N)
	VTEMP(12) = t0
c       qi + qbi --> g + qbk
      t0 = CF*(25.D0/8.D0-0.3141592653589793D1**2/3.D0-4.D0*dlog(2.D0)**
     #2.D0-dlog(2.D0)/2.D0)*A(UC,SC,TC,N)
	VTEMP(13) = t0
	VPART35D(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      t0 = CF*(27.D0/8.D0-0.3141592653589793D1**2/3.D0+4.D0*dlog(2.D0)**
     #2.D0+dlog(2.D0)/2.D0)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(14) = t0
c       qi + qbi --> g + qbi
      t0 = CF*(25.D0/8.D0-0.3141592653589793D1**2/3.D0-4.D0*dlog(2.D0)**
     #2.D0-dlog(2.D0)/2.D0)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(15) = t0
	VPART35D(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      t0 = -23.D0/36.D0*C(SC,TC,UC,N)
	VTEMP(16) = t0
c       qi + qbi --> qbk + g
      t0 = -23.D0/36.D0*C(SC,TC,UC,N)
	VTEMP(17) = t0
c       qi + qbi --> qi + g
      t0 = -23.D0/36.D0*C(SC,TC,UC,N)
	VTEMP(18) = t0
c       qi + qbi --> qbi + g
      t0 = -23.D0/36.D0*C(SC,TC,UC,N)
	VTEMP(19) = t0
c       qi + qbi --> g + g
      t0 = N*(67.D0/9.D0-2.D0/3.D0*0.3141592653589793D1**2)*C(SC,TC,UC,N
     #)
	VTEMP(20) = t0
	VPART35D(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      t0 = 0.D0
	VTEMP(21) = t0
c       qi + g --> qbk + qk
      t0 = 0.D0
	VTEMP(22) = t0
	VPART35D(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      t0 = 0.D0
	VTEMP(23) = t0
c       qi + g --> qk + qbk
      t0 = 0.D0
	VTEMP(24) = t0
	VPART35D(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      t0 = 0.D0
	VTEMP(25) = t0
	VPART35D(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t0 = -(N**2-1.D0)/N*(27.D0/8.D0-0.3141592653589793D1**2/3.D0+4.D0*
     #dlog(2.D0)**2.D0+dlog(2.D0)/2.D0)*C(TC,SC,UC,N)/2.D0
	VTEMP(26) = t0
c       qi + g --> g + g
      t0 = -(N**2-1.D0)/N*(25.D0/8.D0-0.3141592653589793D1**2/3.D0-4.D0*
     #dlog(2.D0)**2.D0-dlog(2.D0)/2.D0)*C(TC,SC,UC,N)/2.D0
	VTEMP(27) = t0
	VPART35D(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      t0 = 23.D0/36.D0*C(UC,SC,TC,N)
	VTEMP(28) = t0
c       qi + g --> qbk + qi
      t0 = 23.D0/36.D0*C(UC,SC,TC,N)
	VTEMP(29) = t0
c       qi + g --> qi + qi
      t0 = 23.D0/36.D0*C(UC,SC,TC,N)
	VTEMP(30) = t0
c       qi + g --> qbi + qi
      t0 = 23.D0/36.D0*C(UC,SC,TC,N)
	VTEMP(31) = t0
c       qi + g --> g + qi
      t0 = -N*(67.D0/9.D0-2.D0/3.D0*0.3141592653589793D1**2)*C(UC,SC,TC,
     #N)
	VTEMP(32) = t0
	VPART35D(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t0 = (N**2-1.D0)/N*(27.D0/8.D0-0.3141592653589793D1**2/3.D0+4.D0*d
     #log(2.D0)**2.D0+dlog(2.D0)/2.D0)*C(SC,TC,UC,N)/2.D0
	VTEMP(33) = t0
c       g + g --> g + qi
      t0 = (N**2-1.D0)/N*(25.D0/8.D0-0.3141592653589793D1**2/3.D0-4.D0*d
     #log(2.D0)**2.D0-dlog(2.D0)/2.D0)*C(SC,TC,UC,N)/2.D0
	VTEMP(34) = t0
	VPART35D(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t0 = -23.D0/36.D0*Dg(SC,TC,UC,N)
	VTEMP(35) = t0
c       g + g --> g + g
      t0 = N*(67.D0/9.D0-2.D0/3.D0*0.3141592653589793D1**2)*Dg(SC,TC,UC,
     #N)
	VTEMP(36) = t0
	VPART35D(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
C*****************************************************
CC  TERMES QUI VIENNENT DE 5 COLLINEAIRE A 4
C*****************************************************
C PARTIE EN LOG(R**2)
	SUBROUTINE VEC_45RO(SC,TC,UC,Z4,R,VPART45R)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VPART45R(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VPART45R(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      t0 = A4_QQ(z4)*dlog(R**2)/(1.D0-z4)*A(SC,TC,UC,N)
	VTEMP(1) = t0
c       qi + qk --> g + qk
      t0 = 0.D0
	VTEMP(2) = t0
	VPART45R(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      t0 = A4_GQ(z4)*dlog(R**2)/(1.D0-z4)*A(SC,TC,UC,N)
	VTEMP(3) = t0
c       qi + qk --> qk + g
      t0 = A4_GQ(z4)*dlog(R**2)/(1.D0-z4)*A(SC,UC,TC,N)
	VTEMP(4) = t0
	VPART45R(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      t0 = A4_QQ(z4)*dlog(R**2)/(1.D0-z4)*A(UC,TC,SC,N)
	VTEMP(5) = t0
c       qi + qbk --> g + qbk
      t0 = 0.D0
	VTEMP(6) = t0
	VPART45R(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      t0 = A4_GQ(z4)*dlog(R**2)/(1.D0-z4)*A(UC,TC,SC,N)
	VTEMP(7) = t0
c       qi + qbk --> qbk + g
      t0 = A4_GQ(z4)*dlog(R**2)/(1.D0-z4)*A(TC,UC,SC,N)
	VTEMP(8) = t0
	VPART45R(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      t0 = A4_QQ(z4)*dlog(R**2)/(1.D0-z4)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B
     #(SC,TC,UC,N))
	VTEMP(9) = t0
c       qi + qi --> g + qi
      t0 = 0.D0
	VTEMP(10) = t0
	VPART45R(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      t0 = A4_GQ(z4)*dlog(R**2)/(1.D0-z4)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B
     #(SC,TC,UC,N))
	VTEMP(11) = t0
	VPART45R(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      t0 = A4_QQ(z4)*dlog(R**2)/(1.D0-z4)*A(UC,SC,TC,N)
	VTEMP(12) = t0
c       qi + qbi --> g + qbk
      t0 = A4_QG(z4)*dlog(R**2)/(1.D0-z4)*C(SC,TC,UC,N)
	VTEMP(13) = t0
	VPART45R(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      t0 = A4_QQ(z4)*dlog(R**2)/(1.D0-z4)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B
     #(UC,TC,SC,N))
	VTEMP(14) = t0
c       qi + qbi --> g + qbi
      t0 = A4_QG(z4)*dlog(R**2)/(1.D0-z4)*C(SC,TC,UC,N)
	VTEMP(15) = t0
	VPART45R(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      t0 = A4_GQ(z4)*dlog(R**2)/(1.D0-z4)*A(UC,SC,TC,N)
	VTEMP(16) = t0
c       qi + qbi --> qbk + g
      t0 = A4_GQ(z4)*dlog(R**2)/(1.D0-z4)*A(TC,SC,UC,N)
	VTEMP(17) = t0
c       qi + qbi --> qi + g
      t0 = A4_GQ(z4)*dlog(R**2)/(1.D0-z4)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B
     #(UC,TC,SC,N))
	VTEMP(18) = t0
c       qi + qbi --> qbi + g
      t0 = A4_GQ(z4)*dlog(R**2)/(1.D0-z4)*(A(TC,UC,SC,N)+A(TC,SC,UC,N)+B
     #(TC,UC,SC,N))
	VTEMP(19) = t0
c       qi + qbi --> g + g
      t0 = A4_GG(z4)*dlog(R**2)/(1.D0-z4)*C(SC,TC,UC,N)
	VTEMP(20) = t0
	VPART45R(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      t0 = -A4_QG(z4)*dlog(R**2)/(1.D0-z4)*C(TC,SC,UC,N)
	VTEMP(21) = t0
c       qi + g --> qbk + qk
      t0 = 0.D0
	VTEMP(22) = t0
	VPART45R(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      t0 = -A4_QG(z4)*dlog(R**2)/(1.D0-z4)*C(TC,SC,UC,N)
	VTEMP(23) = t0
c       qi + g --> qk + qbk
      t0 = 0.D0
	VTEMP(24) = t0
	VPART45R(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      t0 = -A4_QG(z4)*dlog(R**2)/(1.D0-z4)*C(TC,SC,UC,N)
	VTEMP(25) = t0
	VPART45R(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t0 = -A4_GG(z4)*dlog(R**2)/(1.D0-z4)*C(TC,SC,UC,N)
	VTEMP(26) = t0
c       qi + g --> g + g
      t0 = -A4_GQ(z4)*dlog(R**2)/(1.D0-z4)*C(UC,SC,TC,N)
	VTEMP(27) = t0
	VPART45R(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      t0 = 0.D0
	VTEMP(28) = t0
c       qi + g --> qbk + qi
      t0 = 0.D0
	VTEMP(29) = t0
c       qi + g --> qi + qi
      t0 = -A4_QG(z4)*dlog(R**2)/(1.D0-z4)*C(TC,SC,UC,N)
	VTEMP(30) = t0
c       qi + g --> qbi + qi
      t0 = 0.D0
	VTEMP(31) = t0
c       qi + g --> g + qi
      t0 = -A4_QQ(z4)*dlog(R**2)/(1.D0-z4)*C(UC,SC,TC,N)
	VTEMP(32) = t0
	VPART45R(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t0 = A4_QQ(z4)*dlog(R**2)/(1.D0-z4)*C(SC,TC,UC,N)
	VTEMP(33) = t0
c       g + g --> g + qi
      t0 = A4_QG(z4)*dlog(R**2)/(1.D0-z4)*Dg(SC,TC,UC,N)
	VTEMP(34) = t0
	VPART45R(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t0 = A4_GQ(z4)*dlog(R**2)/(1.D0-z4)*C(SC,TC,UC,N)
	VTEMP(35) = t0
c       g + g --> g + g
      t0 = A4_GG(z4)*dlog(R**2)/(1.D0-z4)*Dg(SC,TC,UC,N)
	VTEMP(36) = t0
	VPART45R(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
C PARTIE EN 1/(1-Z4)+
	SUBROUTINE VEC_45ZO(SC,TC,UC,Z4,MF,PT4,VPART45Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VPART45Z(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VPART45Z(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      t0 = -A(SC,TC,UC,N)*(-ANM4_QQ(z4)-A4_QQ(z4)*dlog(pt4**2/Mf**2)+DAQ
     #Q(z4))
	VTEMP(1) = t0
c       qi + qk --> g + qk
      t0 = 0.D0
	VTEMP(2) = t0
	VPART45Z(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      t0 = A(SC,TC,UC,N)*(ANM4_GQ(z4)+A4_GQ(z4)*dlog(pt4**2/Mf**2)-DAGQ(
     #z4))
	VTEMP(3) = t0
c       qi + qk --> qk + g
      t0 = -A(SC,UC,TC,N)*(-ANM4_GQ(z4)-A4_GQ(z4)*dlog(pt4**2/Mf**2)+DAG
     #Q(z4))
	VTEMP(4) = t0
	VPART45Z(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      t0 = -A(UC,TC,SC,N)*(-ANM4_QQ(z4)-A4_QQ(z4)*dlog(pt4**2/Mf**2)+DAQ
     #Q(z4))
	VTEMP(5) = t0
c       qi + qbk --> g + qbk
      t0 = 0.D0
	VTEMP(6) = t0
	VPART45Z(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      t0 = A(UC,TC,SC,N)*(ANM4_GQ(z4)+A4_GQ(z4)*dlog(pt4**2/Mf**2)-DAGQ(
     #z4))
	VTEMP(7) = t0
c       qi + qbk --> qbk + g
      t0 = A(TC,UC,SC,N)*(ANM4_GQ(z4)+A4_GQ(z4)*dlog(pt4**2/Mf**2)-DAGQ(
     #z4))
	VTEMP(8) = t0
	VPART45Z(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      t0 = (ANM4_QQ(z4)+A4_QQ(z4)*dlog(pt4**2/Mf**2)-DAQQ(z4))*(A(SC,TC,
     #UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))
	VTEMP(9) = t0
c       qi + qi --> g + qi
      t0 = 0.D0
	VTEMP(10) = t0
	VPART45Z(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      t0 = (A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))*(ANM4_GQ(z4)+A4_G
     #Q(z4)*dlog(pt4**2/Mf**2)-DAGQ(z4))
	VTEMP(11) = t0
	VPART45Z(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      t0 = A(UC,SC,TC,N)*(ANM4_QQ(z4)+A4_QQ(z4)*dlog(pt4**2/Mf**2)-DAQQ(
     #z4))
	VTEMP(12) = t0
c       qi + qbi --> g + qbk
      t0 = -C(SC,TC,UC,N)*(-ANM4_QG(z4)-A4_QG(z4)*dlog(pt4**2/Mf**2)+DAQ
     #G(z4))
	VTEMP(13) = t0
	VPART45Z(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      t0 = -(-ANM4_QQ(z4)-A4_QQ(z4)*dlog(pt4**2/Mf**2)+DAQQ(z4))*(A(UC,T
     #C,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(14) = t0
c       qi + qbi --> g + qbi
      t0 = C(SC,TC,UC,N)*(ANM4_QG(z4)+A4_QG(z4)*dlog(pt4**2/Mf**2)-DAQG(
     #z4))
	VTEMP(15) = t0
	VPART45Z(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      t0 = -A(UC,SC,TC,N)*(-ANM4_GQ(z4)-A4_GQ(z4)*dlog(pt4**2/Mf**2)+DAG
     #Q(z4))
	VTEMP(16) = t0
c       qi + qbi --> qbk + g
      t0 = -A(TC,SC,UC,N)*(-ANM4_GQ(z4)-A4_GQ(z4)*dlog(pt4**2/Mf**2)+DAG
     #Q(z4))
	VTEMP(17) = t0
c       qi + qbi --> qi + g
      t0 = (A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))*(ANM4_GQ(z4)+A4_G
     #Q(z4)*dlog(pt4**2/Mf**2)-DAGQ(z4))
	VTEMP(18) = t0
c       qi + qbi --> qbi + g
      t0 = (ANM4_GQ(z4)+A4_GQ(z4)*dlog(pt4**2/Mf**2)-DAGQ(z4))*(A(TC,UC,
     #SC,N)+A(TC,SC,UC,N)+B(TC,UC,SC,N))
	VTEMP(19) = t0
c       qi + qbi --> g + g
      t0 = C(SC,TC,UC,N)*(ANM4_GG(z4)+A4_GG(z4)*dlog(pt4**2/Mf**2)-DAGG(
     #z4))
	VTEMP(20) = t0
	VPART45Z(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      t0 = C(TC,SC,UC,N)*(-ANM4_QG(z4)-A4_QG(z4)*dlog(pt4**2/Mf**2)+DAQG
     #(z4))
	VTEMP(21) = t0
c       qi + g --> qbk + qk
      t0 = 0.D0
	VTEMP(22) = t0
	VPART45Z(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      t0 = C(TC,SC,UC,N)*(-ANM4_QG(z4)-A4_QG(z4)*dlog(pt4**2/Mf**2)+DAQG
     #(z4))
	VTEMP(23) = t0
c       qi + g --> qk + qbk
      t0 = 0.D0
	VTEMP(24) = t0
	VPART45Z(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      t0 = C(TC,SC,UC,N)*(-ANM4_QG(z4)-A4_QG(z4)*dlog(pt4**2/Mf**2)+DAQG
     #(z4))
	VTEMP(25) = t0
	VPART45Z(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t0 = -C(TC,SC,UC,N)*(ANM4_GG(z4)+A4_GG(z4)*dlog(pt4**2/Mf**2)-DAGG
     #(z4))
	VTEMP(26) = t0
c       qi + g --> g + g
      t0 = -C(UC,SC,TC,N)*(ANM4_GQ(z4)+A4_GQ(z4)*dlog(pt4**2/Mf**2)-DAGQ
     #(z4))
	VTEMP(27) = t0
	VPART45Z(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      t0 = 0.D0
	VTEMP(28) = t0
c       qi + g --> qbk + qi
      t0 = 0.D0
	VTEMP(29) = t0
c       qi + g --> qi + qi
      t0 = C(TC,SC,UC,N)*(-ANM4_QG(z4)-A4_QG(z4)*dlog(pt4**2/Mf**2)+DAQG
     #(z4))
	VTEMP(30) = t0
c       qi + g --> qbi + qi
      t0 = 0.D0
	VTEMP(31) = t0
c       qi + g --> g + qi
      t0 = -C(UC,SC,TC,N)*(ANM4_QQ(z4)+A4_QQ(z4)*dlog(pt4**2/Mf**2)-DAQQ
     #(z4))
	VTEMP(32) = t0
	VPART45Z(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t0 = C(SC,TC,UC,N)*(ANM4_QQ(z4)+A4_QQ(z4)*dlog(pt4**2/Mf**2)-DAQQ(
     #z4))
	VTEMP(33) = t0
c       g + g --> g + qi
      t0 = Dg(SC,TC,UC,N)*(ANM4_QG(z4)+A4_QG(z4)*dlog(pt4**2/Mf**2)-DAQG
     #(z4))
	VTEMP(34) = t0
	VPART45Z(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t0 = C(SC,TC,UC,N)*(ANM4_GQ(z4)+A4_GQ(z4)*dlog(pt4**2/Mf**2)-DAGQ(
     #z4))
	VTEMP(35) = t0
c       g + g --> g + g
      t0 = Dg(SC,TC,UC,N)*(ANM4_GG(z4)+A4_GG(z4)*dlog(pt4**2/Mf**2)-DAGG
     #(z4))
	VTEMP(36) = t0
	VPART45Z(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
C PARTIE EN LOG(1-Z4)/(1-Z4)+
	SUBROUTINE VEC_45LO(SC,TC,UC,Z4,VPART45L)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VPART45L(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VPART45L(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      t0 = -A(SC,TC,UC,N)*(DBQQ(z4)-2.D0*A4_QQ(z4))
	VTEMP(1) = t0
c       qi + qk --> g + qk
      t0 = 0.D0
	VTEMP(2) = t0
	VPART45L(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      t0 = -A(SC,TC,UC,N)*(DBGQ(z4)-2.D0*A4_GQ(z4))
	VTEMP(3) = t0
c       qi + qk --> qk + g
      t0 = A(SC,UC,TC,N)*(-DBGQ(z4)+2.D0*A4_GQ(z4))
	VTEMP(4) = t0
	VPART45L(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      t0 = A(UC,TC,SC,N)*(-DBQQ(z4)+2.D0*A4_QQ(z4))
	VTEMP(5) = t0
c       qi + qbk --> g + qbk
      t0 = 0.D0
	VTEMP(6) = t0
	VPART45L(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      t0 = A(UC,TC,SC,N)*(-DBGQ(z4)+2.D0*A4_GQ(z4))
	VTEMP(7) = t0
c       qi + qbk --> qbk + g
      t0 = -A(TC,UC,SC,N)*(DBGQ(z4)-2.D0*A4_GQ(z4))
	VTEMP(8) = t0
	VPART45L(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      t0 = -(DBQQ(z4)-2.D0*A4_QQ(z4))*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,
     #TC,UC,N))
	VTEMP(9) = t0
c       qi + qi --> g + qi
      t0 = 0.D0
	VTEMP(10) = t0
	VPART45L(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      t0 = -(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))*(DBGQ(z4)-2.D0*A
     #4_GQ(z4))
	VTEMP(11) = t0
	VPART45L(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      t0 = A(UC,SC,TC,N)*(-DBQQ(z4)+2.D0*A4_QQ(z4))
	VTEMP(12) = t0
c       qi + qbi --> g + qbk
      t0 = -C(SC,TC,UC,N)*(DBQG(z4)-2.D0*A4_QG(z4))
	VTEMP(13) = t0
	VPART45L(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      t0 = -(DBQQ(z4)-2.D0*A4_QQ(z4))*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,
     #TC,SC,N))
	VTEMP(14) = t0
c       qi + qbi --> g + qbi
      t0 = -C(SC,TC,UC,N)*(DBQG(z4)-2.D0*A4_QG(z4))
	VTEMP(15) = t0
	VPART45L(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      t0 = A(UC,SC,TC,N)*(-DBGQ(z4)+2.D0*A4_GQ(z4))
	VTEMP(16) = t0
c       qi + qbi --> qbk + g
      t0 = A(TC,SC,UC,N)*(-DBGQ(z4)+2.D0*A4_GQ(z4))
	VTEMP(17) = t0
c       qi + qbi --> qi + g
      t0 = (-DBGQ(z4)+2.D0*A4_GQ(z4))*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,
     #TC,SC,N))
	VTEMP(18) = t0
c       qi + qbi --> qbi + g
      t0 = (-DBGQ(z4)+2.D0*A4_GQ(z4))*(A(TC,UC,SC,N)+A(TC,SC,UC,N)+B(TC,
     #UC,SC,N))
	VTEMP(19) = t0
c       qi + qbi --> g + g
      t0 = -C(SC,TC,UC,N)*(DBGG(z4)-2.D0*A4_GG(z4))
	VTEMP(20) = t0
	VPART45L(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      t0 = C(TC,SC,UC,N)*(DBQG(z4)-2.D0*A4_QG(z4))
	VTEMP(21) = t0
c       qi + g --> qbk + qk
      t0 = 0.D0
	VTEMP(22) = t0
	VPART45L(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      t0 = C(TC,SC,UC,N)*(DBQG(z4)-2.D0*A4_QG(z4))
	VTEMP(23) = t0
c       qi + g --> qk + qbk
      t0 = 0.D0
	VTEMP(24) = t0
	VPART45L(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      t0 = -C(TC,SC,UC,N)*(-DBQG(z4)+2.D0*A4_QG(z4))
	VTEMP(25) = t0
	VPART45L(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t0 = C(TC,SC,UC,N)*(DBGG(z4)-2.D0*A4_GG(z4))
	VTEMP(26) = t0
c       qi + g --> g + g
      t0 = C(UC,SC,TC,N)*(DBGQ(z4)-2.D0*A4_GQ(z4))
	VTEMP(27) = t0
	VPART45L(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      t0 = 0.D0
	VTEMP(28) = t0
c       qi + g --> qbk + qi
      t0 = 0.D0
	VTEMP(29) = t0
c       qi + g --> qi + qi
      t0 = -C(TC,SC,UC,N)*(-DBQG(z4)+2.D0*A4_QG(z4))
	VTEMP(30) = t0
c       qi + g --> qbi + qi
      t0 = 0.D0
	VTEMP(31) = t0
c       qi + g --> g + qi
      t0 = C(UC,SC,TC,N)*(DBQQ(z4)-2.D0*A4_QQ(z4))
	VTEMP(32) = t0
	VPART45L(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t0 = C(SC,TC,UC,N)*(-DBQQ(z4)+2.D0*A4_QQ(z4))
	VTEMP(33) = t0
c       g + g --> g + qi
      t0 = -Dg(SC,TC,UC,N)*(DBQG(z4)-2.D0*A4_QG(z4))
	VTEMP(34) = t0
	VPART45L(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t0 = C(SC,TC,UC,N)*(-DBGQ(z4)+2.D0*A4_GQ(z4))
	VTEMP(35) = t0
c       g + g --> g + g
      t0 = Dg(SC,TC,UC,N)*(-DBGG(z4)+2.D0*A4_GG(z4))
	VTEMP(36) = t0
	VPART45L(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
C PARTIE EN DELTA(1-Z4)
	SUBROUTINE VEC_45DO(SC,TC,UC,MF,PT4,VPART45D)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VPART45D(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VPART45D(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      t0 = -A(SC,TC,UC,N)*(-B_QQ()*dlog(pt4**2/Mf**2)+DCQQ(1.D0))
	VTEMP(1) = t0
c       qi + qk --> g + qk
      t0 = 0.D0
	VTEMP(2) = t0
	VPART45D(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      t0 = -A(SC,TC,UC,N)*DCGQ(1.D0)
	VTEMP(3) = t0
c       qi + qk --> qk + g
      t0 = -A(SC,UC,TC,N)*DCGQ(1.D0)
	VTEMP(4) = t0
	VPART45D(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      t0 = A(UC,TC,SC,N)*(B_QQ()*dlog(pt4**2/Mf**2)-DCQQ(1.D0))
	VTEMP(5) = t0
c       qi + qbk --> g + qbk
      t0 = 0.D0
	VTEMP(6) = t0
	VPART45D(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      t0 = -A(UC,TC,SC,N)*DCGQ(1.D0)
	VTEMP(7) = t0
c       qi + qbk --> qbk + g
      t0 = -A(TC,UC,SC,N)*DCGQ(1.D0)
	VTEMP(8) = t0
	VPART45D(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      t0 = (A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))*(B_QQ()*dlog(pt4*
     #*2/Mf**2)-DCQQ(1.D0))
	VTEMP(9) = t0
c       qi + qi --> g + qi
      t0 = 0.D0
	VTEMP(10) = t0
	VPART45D(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      t0 = -DCGQ(1.D0)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))
	VTEMP(11) = t0
	VPART45D(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      t0 = A(UC,SC,TC,N)*(B_QQ()*dlog(pt4**2/Mf**2)-DCQQ(1.D0))
	VTEMP(12) = t0
c       qi + qbi --> g + qbk
      t0 = -C(SC,TC,UC,N)*DCQG(1.D0)
	VTEMP(13) = t0
	VPART45D(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      t0 = -(-B_QQ()*dlog(pt4**2/Mf**2)+DCQQ(1.D0))*(A(UC,TC,SC,N)+A(UC,
     #SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(14) = t0
c       qi + qbi --> g + qbi
      t0 = -C(SC,TC,UC,N)*DCQG(1.D0)
	VTEMP(15) = t0
	VPART45D(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      t0 = -A(UC,SC,TC,N)*DCGQ(1.D0)
	VTEMP(16) = t0
c       qi + qbi --> qbk + g
      t0 = -A(TC,SC,UC,N)*DCGQ(1.D0)
	VTEMP(17) = t0
c       qi + qbi --> qi + g
      t0 = -DCGQ(1.D0)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))
	VTEMP(18) = t0
c       qi + qbi --> qbi + g
      t0 = -DCGQ(1.D0)*(A(TC,UC,SC,N)+A(TC,SC,UC,N)+B(TC,UC,SC,N))
	VTEMP(19) = t0
c       qi + qbi --> g + g
      t0 = C(SC,TC,UC,N)*(B_GG()*dlog(pt4**2/Mf**2)-DCGG(1.D0))
	VTEMP(20) = t0
	VPART45D(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      t0 = C(TC,SC,UC,N)*DCQG(1.D0)
	VTEMP(21) = t0
c       qi + g --> qbk + qk
      t0 = 0.D0
	VTEMP(22) = t0
	VPART45D(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      t0 = C(TC,SC,UC,N)*DCQG(1.D0)
	VTEMP(23) = t0
c       qi + g --> qk + qbk
      t0 = 0.D0
	VTEMP(24) = t0
	VPART45D(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      t0 = C(TC,SC,UC,N)*DCQG(1.D0)
	VTEMP(25) = t0
	VPART45D(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      t0 = C(TC,SC,UC,N)*(-dlog(pt4**2/Mf**2)*B_GG()+DCGG(1.D0))
	VTEMP(26) = t0
c       qi + g --> g + g
      t0 = C(UC,SC,TC,N)*DCGQ(1.D0)
	VTEMP(27) = t0
	VPART45D(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      t0 = 0.D0
	VTEMP(28) = t0
c       qi + g --> qbk + qi
      t0 = 0.D0
	VTEMP(29) = t0
c       qi + g --> qi + qi
      t0 = C(TC,SC,UC,N)*DCQG(1.D0)
	VTEMP(30) = t0
c       qi + g --> qbi + qi
      t0 = 0.D0
	VTEMP(31) = t0
c       qi + g --> g + qi
      t0 = -C(UC,SC,TC,N)*(B_QQ()*dlog(pt4**2/Mf**2)-DCQQ(1.D0))
	VTEMP(32) = t0
	VPART45D(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      t0 = C(SC,TC,UC,N)*(B_QQ()*dlog(pt4**2/Mf**2)-DCQQ(1.D0))
	VTEMP(33) = t0
c       g + g --> g + qi
      t0 = -Dg(SC,TC,UC,N)*DCQG(1.D0)
	VTEMP(34) = t0
	VPART45D(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      t0 = -C(SC,TC,UC,N)*DCGQ(1.D0)
	VTEMP(35) = t0
c       g + g --> g + g
      t0 = -Dg(SC,TC,UC,N)*(-dlog(pt4**2/Mf**2)*B_GG()+DCGG(1.D0))
	VTEMP(36) = t0
	VPART45D(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
C*****************************************************
CC  TERMES QUI VIENNENT DE LA PARTIE VIRTUELLE
C*****************************************************
	SUBROUTINE VEC_VIO(S,SC,TC,UC,MU,PT,PTM,VPARTVI)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	COMMON/GDF/JNF
	PARAMETER (J0MAX=16,I0MAX=36)
	DIMENSION VPARTVI(J0MAX),VTEMP(I0MAX)
	DO I = 1,J0MAX
	  VPARTVI(I) = 0.D0
	ENDDO
	DO I = 1,I0MAX
	  VTEMP(I) = 0.D0
	ENDDO
	VC = (N**2-1.D0)
	MU2 = MU**2
C
C	j0 = 1 : qi + qk --> jet + qk
C
c       qi + qk --> qi + qk
      teraj = -2.D0*B_QQ()*dlog(ptm**2/S)*A(SC,TC,UC,N)-2.D0*B_QQ()*dlog
     #(pt**2/S)*A(SC,TC,UC,N)+(-CF*(8.D0*dlog(SC/S)-8.D0*dlog(-UC/S)-4.D
     #0*dlog(-TC/S))+N*(4.D0*dlog(SC/S)-2.D0*dlog(-UC/S)-2.D0*dlog(-TC/S
     #)))*A(SC,TC,UC,N)*dlog(ptm**2/S)+2.D0*A4_QQ(1.D0)*A(SC,TC,UC,N)*(d
     #log(pt**2/S)**2.D0/4.D0-dlog(pt**2/S)*dlog(ptm**2/S)/2.D0)-A4_QQ(1
     #.D0)*A(SC,TC,UC,N)*dlog(ptm**2/S)**2.D0/2.D0
      s2 = 4.D0
      s5 = CF*(-16.D0-2.D0*dlog(-TC/S)**2.D0+dlog(-TC/S)*(6.D0+8.D0*dlog
     #(SC/S)-8.D0*dlog(-UC/S))+(-2.D0*SC**2+2.D0*UC**2)/(SC**2+UC**2)*(0
     #.3141592653589793D1**2+dlog(-TC/S)**2.D0-2.D0*dlog(-TC/S)*dlog(SC/
     #S)+dlog(SC/S)**2.D0+(dlog(-TC/S)-dlog(-UC/S))**2.D0)+(2.D0*SC+2.D0
     #*UC)/(SC**2+UC**2)*((SC+UC)*(dlog(-UC/S)-dlog(SC/S))+(UC-SC)*(2.D0
     #*dlog(-TC/S)-dlog(SC/S)-dlog(-UC/S))))
      s6 = N*(85.D0/9.D0+0.3141592653589793D1**2+2.D0*dlog(-TC/S)*(dlog(
     #-TC/S)+dlog(-UC/S)-2.D0*dlog(SC/S))+(SC**2-UC**2)/(2.D0*SC**2+2.D0
     #*UC**2)*(0.3141592653589793D1**2+2.D0*dlog(-TC/S)**2.D0-4.D0*dlog(
     #-TC/S)*dlog(SC/S)+2.D0*dlog(SC/S)**2.D0+(dlog(-TC/S)-dlog(-UC/S))*
     #*2.D0)-SC*TC/(SC**2+UC**2)*(dlog(-TC/S)-dlog(-UC/S))+2.D0*UC*TC/(S
     #C**2+UC**2)*(dlog(-TC/S)-dlog(SC/S))+11.D0/3.D0*dlog(MU2/S)-11.D0/
     #3.D0*dlog(-TC/S))+GTR*(4.D0/3.D0*dlog(-TC/S)-4.D0/3.D0*dlog(MU2/S)
     #-20.D0/9.D0)
      s4 = s5+s6
      s5 = N
      s3 = s4*s5
      s1 = s2*s3
      s2 = CF*(SC**2+UC**2)/TC**2
      teres = s1*s2
	VTEMP(1) = TERAJ+TERES
c       qi + qk --> g + qk
      teraj = 0.D0
      teres = 0.D0
	VTEMP(2) = TERAJ+TERES
	VPARTVI(1) = VTEMP(1) + VTEMP(2)
C
C	j0 = 2 : qi + qk --> jet + g
C
c       qi + qk --> qi + g
      teraj = 0.D0
      teres = 0.D0
	VTEMP(3) = TERAJ+TERES
c       qi + qk --> qk + g
      teraj = 0.D0
      teres = 0.D0
	VTEMP(4) = TERAJ+TERES
	VPARTVI(2) = VTEMP(3) + VTEMP(4)
C
C	j0 = 3 : qi + qbk --> jet + qbk
C
c       qi + qbk --> qi + qbk
      teraj = -2.D0*B_QQ()*dlog(ptm**2/S)*A(UC,TC,SC,N)-2.D0*B_QQ()*dlog
     #(pt**2/S)*A(UC,TC,SC,N)+(-CF*(8.D0*dlog(-UC/S)-8.D0*dlog(SC/S)-4.D
     #0*dlog(-TC/S))+N*(4.D0*dlog(-UC/S)-2.D0*dlog(SC/S)-2.D0*dlog(-TC/S
     #)))*A(UC,TC,SC,N)*dlog(ptm**2/S)+2.D0*A4_QQ(1.D0)*A(UC,TC,SC,N)*(d
     #log(pt**2/S)**2.D0/4.D0-dlog(pt**2/S)*dlog(ptm**2/S)/2.D0)-A4_QQ(1
     #.D0)*A(UC,TC,SC,N)*dlog(ptm**2/S)**2.D0/2.D0
      s2 = 4.D0
      s5 = CF*(-16.D0-2.D0*dlog(-TC/S)**2.D0+dlog(-TC/S)*(6.D0+8.D0*dlog
     #(-UC/S)-8.D0*dlog(SC/S))+(-2.D0*UC**2+2.D0*SC**2)/(UC**2+SC**2)*(0
     #.3141592653589793D1**2+(dlog(-TC/S)-dlog(-UC/S))**2.D0+dlog(-TC/S)
     #**2.D0-2.D0*dlog(-TC/S)*dlog(SC/S)+dlog(SC/S)**2.D0)+(2.D0*UC+2.D0
     #*SC)/(UC**2+SC**2)*((UC+SC)*(dlog(SC/S)-dlog(-UC/S))+(SC-UC)*(2.D0
     #*dlog(-TC/S)-dlog(-UC/S)-dlog(SC/S))))
      s6 = N*(85.D0/9.D0+0.3141592653589793D1**2+2.D0*dlog(-TC/S)*(dlog(
     #-TC/S)+dlog(SC/S)-2.D0*dlog(-UC/S))+(UC**2-SC**2)/(2.D0*UC**2+2.D0
     #*SC**2)*(2.D0*0.3141592653589793D1**2+2.D0*(dlog(-TC/S)-dlog(-UC/S
     #))**2.D0+dlog(-TC/S)**2.D0-2.D0*dlog(-TC/S)*dlog(SC/S)+dlog(SC/S)*
     #*2.D0)-UC*TC/(UC**2+SC**2)*(dlog(-TC/S)-dlog(SC/S))+2.D0*SC*TC/(UC
     #**2+SC**2)*(dlog(-TC/S)-dlog(-UC/S))+11.D0/3.D0*dlog(MU2/S)-11.D0/
     #3.D0*dlog(-TC/S))+GTR*(4.D0/3.D0*dlog(-TC/S)-4.D0/3.D0*dlog(MU2/S)
     #-20.D0/9.D0)
      s4 = s5+s6
      s5 = N
      s3 = s4*s5
      s1 = s2*s3
      s2 = CF*(UC**2+SC**2)/TC**2
      teres = s1*s2
	VTEMP(5) = TERAJ+TERES
c       qi + qbk --> g + qbk
      teraj = 0.D0
      teres = 0.D0
	VTEMP(6) = TERAJ+TERES
	VPARTVI(3) = VTEMP(5) + VTEMP(6)
C
C	j0 = 4 : qi + qbk --> jet + g
C
c       qi + qbk --> qi + g
      teraj = 0.D0
      teres = 0.D0
	VTEMP(7) = TERAJ+TERES
c       qi + qbk --> qbk + g
      teraj = 0.D0
      teres = 0.D0
	VTEMP(8) = TERAJ+TERES
	VPARTVI(4) = VTEMP(7) + VTEMP(8)
C
C	j0 = 5 : qi + qi --> jet + qi
C
c       qi + qi --> qi + qi
      s1 = -2.D0*B_QQ()*dlog(ptm**2/S)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC
     #,TC,UC,N))-2.D0*B_QQ()*dlog(pt**2/S)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+
     #B(SC,TC,UC,N))
      teraj = s1+((-CF*(4.D0*dlog(SC/S)-4.D0*dlog(-TC/S)-4.D0*dlog(-UC/S
     #))+2.D0*N*(2.D0*dlog(SC/S)-dlog(-TC/S)-dlog(-UC/S)))*B(SC,TC,UC,N)
     #+(-CF*(8.D0*dlog(SC/S)-8.D0*dlog(-UC/S)-4.D0*dlog(-TC/S))+N*(4.D0*
     #dlog(SC/S)-2.D0*dlog(-UC/S)-2.D0*dlog(-TC/S)))*A(SC,TC,UC,N)+(-CF*
     #(8.D0*dlog(SC/S)-8.D0*dlog(-TC/S)-4.D0*dlog(-UC/S))+N*(4.D0*dlog(S
     #C/S)-2.D0*dlog(-UC/S)-2.D0*dlog(-TC/S)))*A(SC,UC,TC,N))*dlog(ptm**
     #2/S)+2.D0*A4_QQ(1.D0)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))*
     #(dlog(pt**2/S)**2.D0/4.D0-dlog(pt**2/S)*dlog(ptm**2/S)/2.D0)-A4_QQ
     #(1.D0)*(A(SC,TC,UC,N)+A(SC,UC,TC,N)+B(SC,TC,UC,N))*dlog(ptm**2/S)*
     #*2.D0/2.D0
      s1 = (CF*(-16.D0-3.D0/2.D0*(dlog(-TC/S)+dlog(-UC/S))**2.D0+2.D0*dl
     #og(SC/S)*(dlog(-TC/S)+dlog(-UC/S))+2.D0*dlog(-TC/S)+2.D0*dlog(-UC/
     #S)-0.3141592653589793D1**2/2.D0)+N*(85.D0/9.D0+5.D0/4.D0*(dlog(-TC
     #/S)+dlog(-UC/S))**2.D0-dlog(-TC/S)*dlog(-UC/S)-2.D0*dlog(SC/S)*(dl
     #og(-TC/S)+dlog(-UC/S))-4.D0/3.D0*dlog(-TC/S)-4.D0/3.D0*dlog(-UC/S)
     #+5.D0/4.D0*0.3141592653589793D1**2+11.D0/3.D0*dlog(MU2/S))+GTR*(-2
     #0.D0/9.D0+2.D0/3.D0*dlog(-TC/S)+2.D0/3.D0*dlog(-UC/S)-4.D0/3.D0*dl
     #og(MU2/S))+1.D0/N*((0.3141592653589793D1**2/2.D0+(dlog(-TC/S)-dlog
     #(-UC/S))**2.D0/2.D0)*UC*TC/SC**2+UC/SC*dlog(-TC/S)+TC/SC*dlog(-UC/
     #S)))*B(SC,TC,UC,N)
      s5 = 4.D0
      s8 = CF*(-16.D0-2.D0*dlog(-TC/S)**2.D0+dlog(-TC/S)*(6.D0+8.D0*dlog
     #(SC/S)-8.D0*dlog(-UC/S))+(-2.D0*SC**2+2.D0*UC**2)/(SC**2+UC**2)*(0
     #.3141592653589793D1**2+dlog(-TC/S)**2.D0-2.D0*dlog(-TC/S)*dlog(SC/
     #S)+dlog(SC/S)**2.D0+(dlog(-TC/S)-dlog(-UC/S))**2.D0)+(2.D0*SC+2.D0
     #*UC)/(SC**2+UC**2)*((SC+UC)*(dlog(-UC/S)-dlog(SC/S))+(UC-SC)*(2.D0
     #*dlog(-TC/S)-dlog(SC/S)-dlog(-UC/S))))
      s9 = N*(85.D0/9.D0+0.3141592653589793D1**2+2.D0*dlog(-TC/S)*(dlog(
     #-TC/S)+dlog(-UC/S)-2.D0*dlog(SC/S))+(SC**2-UC**2)/(2.D0*SC**2+2.D0
     #*UC**2)*(0.3141592653589793D1**2+2.D0*dlog(-TC/S)**2.D0-4.D0*dlog(
     #-TC/S)*dlog(SC/S)+2.D0*dlog(SC/S)**2.D0+(dlog(-TC/S)-dlog(-UC/S))*
     #*2.D0)-SC*TC/(SC**2+UC**2)*(dlog(-TC/S)-dlog(-UC/S))+2.D0*UC*TC/(S
     #C**2+UC**2)*(dlog(-TC/S)-dlog(SC/S))+11.D0/3.D0*dlog(MU2/S)-11.D0/
     #3.D0*dlog(-TC/S))+GTR*(4.D0/3.D0*dlog(-TC/S)-4.D0/3.D0*dlog(MU2/S)
     #-20.D0/9.D0)
      s7 = s8+s9
      s8 = N
      s6 = s7*s8
      s4 = s5*s6
      s5 = CF*(SC**2+UC**2)/TC**2
      s3 = s4*s5
      s6 = 4.D0
      s9 = CF*(-16.D0-2.D0*dlog(-UC/S)**2.D0+dlog(-UC/S)*(6.D0+8.D0*dlog
     #(SC/S)-8.D0*dlog(-TC/S))+(-2.D0*SC**2+2.D0*TC**2)/(SC**2+TC**2)*(0
     #.3141592653589793D1**2+dlog(-UC/S)**2.D0-2.D0*dlog(-UC/S)*dlog(SC/
     #S)+dlog(SC/S)**2.D0+(dlog(-UC/S)-dlog(-TC/S))**2.D0)+(2.D0*SC+2.D0
     #*TC)/(SC**2+TC**2)*((SC+TC)*(dlog(-TC/S)-dlog(SC/S))+(TC-SC)*(2.D0
     #*dlog(-UC/S)-dlog(SC/S)-dlog(-TC/S))))
      s10 = N*(85.D0/9.D0+0.3141592653589793D1**2+2.D0*dlog(-UC/S)*(dlog
     #(-TC/S)+dlog(-UC/S)-2.D0*dlog(SC/S))+(SC**2-TC**2)/(2.D0*SC**2+2.D
     #0*TC**2)*(0.3141592653589793D1**2+2.D0*dlog(-UC/S)**2.D0-4.D0*dlog
     #(-UC/S)*dlog(SC/S)+2.D0*dlog(SC/S)**2.D0+(dlog(-UC/S)-dlog(-TC/S))
     #**2.D0)-SC*UC/(SC**2+TC**2)*(dlog(-UC/S)-dlog(-TC/S))+2.D0*TC*UC/(
     #SC**2+TC**2)*(dlog(-UC/S)-dlog(SC/S))+11.D0/3.D0*dlog(MU2/S)-11.D0
     #/3.D0*dlog(-UC/S))+GTR*(4.D0/3.D0*dlog(-UC/S)-4.D0/3.D0*dlog(MU2/S
     #)-20.D0/9.D0)
      s8 = s9+s10
      s9 = N
      s7 = s8*s9
      s5 = s6*s7
      s6 = CF*(SC**2+TC**2)/UC**2
      s4 = s5*s6
      s2 = s3+s4
      teres = s1+s2
	VTEMP(9) = TERAJ+TERES
c       qi + qi --> g + qi
      teraj = 0.D0
      teres = 0.D0
	VTEMP(10) = TERAJ+TERES
	VPARTVI(5) = VTEMP(9) + VTEMP(10)
C
C	j0 = 6 : qi + qi --> jet + g
C
c       qi + qi --> qi + g
      teraj = 0.D0
      teres = 0.D0
	VTEMP(11) = TERAJ+TERES
	VPARTVI(6) = VTEMP(11)
C
C	j0 = 7 : qi + qbi --> jet + qbk
C
c       qi + qbi --> qk + qbk
      teraj = -2.D0*B_QQ()*dlog(ptm**2/S)*A(UC,SC,TC,N)-2.D0*B_QQ()*dlog
     #(pt**2/S)*A(UC,SC,TC,N)+(-CF*(8.D0*dlog(-UC/S)-8.D0*dlog(-TC/S)-4.
     #D0*dlog(SC/S))+N*(4.D0*dlog(-UC/S)-2.D0*dlog(-TC/S)-2.D0*dlog(SC/S
     #)))*A(UC,SC,TC,N)*dlog(ptm**2/S)+2.D0*A4_QQ(1.D0)*A(UC,SC,TC,N)*(d
     #log(pt**2/S)**2.D0/4.D0-dlog(pt**2/S)*dlog(ptm**2/S)/2.D0)-A4_QQ(1
     #.D0)*A(UC,SC,TC,N)*dlog(ptm**2/S)**2.D0/2.D0
      s2 = 4.D0
      s5 = CF*(-16.D0-2.D0*dlog(SC/S)**2.D0+2.D0*0.3141592653589793D1**2
     #+dlog(SC/S)*(6.D0+8.D0*dlog(-UC/S)-8.D0*dlog(-TC/S))+(-2.D0*UC**2+
     #2.D0*TC**2)/(UC**2+TC**2)*(2.D0*dlog(SC/S)**2.D0-2.D0*dlog(SC/S)*d
     #log(-UC/S)+dlog(-UC/S)**2.D0-2.D0*dlog(SC/S)*dlog(-TC/S)+dlog(-TC/
     #S)**2.D0)+(2.D0*UC+2.D0*TC)/(UC**2+TC**2)*((UC+TC)*(dlog(-TC/S)-dl
     #og(-UC/S))+(TC-UC)*(2.D0*dlog(SC/S)-dlog(-UC/S)-dlog(-TC/S))))
      s6 = N*(85.D0/9.D0-0.3141592653589793D1**2+2.D0*dlog(SC/S)*(dlog(S
     #C/S)+dlog(-TC/S)-2.D0*dlog(-UC/S))+(UC**2-TC**2)/(2.D0*UC**2+2.D0*
     #TC**2)*(3.D0*dlog(SC/S)**2.D0-4.D0*dlog(SC/S)*dlog(-UC/S)+2.D0*dlo
     #g(-UC/S)**2.D0-2.D0*dlog(SC/S)*dlog(-TC/S)+dlog(-TC/S)**2.D0)-UC*S
     #C/(UC**2+TC**2)*(dlog(SC/S)-dlog(-TC/S))+2.D0*TC*SC/(UC**2+TC**2)*
     #(dlog(SC/S)-dlog(-UC/S))+11.D0/3.D0*dlog(MU2/S)-11.D0/3.D0*dlog(SC
     #/S))+GTR*(4.D0/3.D0*dlog(SC/S)-4.D0/3.D0*dlog(MU2/S)-20.D0/9.D0)
      s4 = s5+s6
      s5 = N
      s3 = s4*s5
      s1 = s2*s3
      s2 = CF*(UC**2+TC**2)/SC**2
      teres = s1*s2
	VTEMP(12) = TERAJ+TERES
c       qi + qbi --> g + qbk
      teraj = 0.D0
      teres = 0.D0
	VTEMP(13) = TERAJ+TERES
	VPARTVI(7) = VTEMP(12) + VTEMP(13)
C
C	j0 = 8 : qi + qbi --> jet + qbi
C
c       qi + qbi --> qi + qbi
      s1 = -2.D0*B_QQ()*dlog(ptm**2/S)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC
     #,TC,SC,N))-2.D0*B_QQ()*dlog(pt**2/S)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+
     #B(UC,TC,SC,N))
      teraj = s1+((-CF*(4.D0*dlog(-UC/S)-4.D0*dlog(-TC/S)-4.D0*dlog(SC/S
     #))+2.D0*N*(2.D0*dlog(-UC/S)-dlog(-TC/S)-dlog(SC/S)))*B(UC,TC,SC,N)
     #+(-CF*(8.D0*dlog(-UC/S)-8.D0*dlog(SC/S)-4.D0*dlog(-TC/S))+N*(4.D0*
     #dlog(-UC/S)-2.D0*dlog(SC/S)-2.D0*dlog(-TC/S)))*A(UC,TC,SC,N)+(-CF*
     #(8.D0*dlog(-UC/S)-8.D0*dlog(-TC/S)-4.D0*dlog(SC/S))+N*(4.D0*dlog(-
     #UC/S)-2.D0*dlog(SC/S)-2.D0*dlog(-TC/S)))*A(UC,SC,TC,N))*dlog(ptm**
     #2/S)+2.D0*A4_QQ(1.D0)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))*
     #(dlog(pt**2/S)**2.D0/4.D0-dlog(pt**2/S)*dlog(ptm**2/S)/2.D0)-A4_QQ
     #(1.D0)*(A(UC,TC,SC,N)+A(UC,SC,TC,N)+B(UC,TC,SC,N))*dlog(ptm**2/S)*
     #*2.D0/2.D0
      s1 = (CF*(-16.D0-3.D0/2.D0*dlog(-TC/S)**2.D0-3.D0*dlog(-TC/S)*dlog
     #(SC/S)-3.D0/2.D0*dlog(SC/S)**2.D0+0.3141592653589793D1**2+2.D0*dlo
     #g(-UC/S)*(dlog(-TC/S)+dlog(SC/S))+2.D0*dlog(-TC/S)+2.D0*dlog(SC/S)
     #)+N*(85.D0/9.D0+5.D0/4.D0*dlog(-TC/S)**2.D0+3.D0/2.D0*dlog(-TC/S)*
     #dlog(SC/S)+5.D0/4.D0*dlog(SC/S)**2.D0-2.D0*dlog(-UC/S)*(dlog(-TC/S
     #)+dlog(SC/S))-4.D0/3.D0*dlog(-TC/S)-4.D0/3.D0*dlog(SC/S)+11.D0/3.D
     #0*dlog(MU2/S))+GTR*(-20.D0/9.D0+2.D0/3.D0*dlog(-TC/S)+2.D0/3.D0*dl
     #og(SC/S)-4.D0/3.D0*dlog(MU2/S))+1.D0/N*(-(-dlog(-TC/S)**2.D0/2.D0+
     #dlog(-TC/S)*dlog(SC/S)-dlog(SC/S)**2.D0/2.D0)*SC*TC/UC**2+SC/UC*dl
     #og(-TC/S)+TC/UC*dlog(SC/S)))*B(UC,TC,SC,N)
      s5 = 4.D0
      s8 = CF*(-16.D0-2.D0*dlog(-TC/S)**2.D0+dlog(-TC/S)*(6.D0+8.D0*dlog
     #(-UC/S)-8.D0*dlog(SC/S))+(-2.D0*UC**2+2.D0*SC**2)/(UC**2+SC**2)*(0
     #.3141592653589793D1**2+(dlog(-TC/S)-dlog(-UC/S))**2.D0+dlog(-TC/S)
     #**2.D0-2.D0*dlog(-TC/S)*dlog(SC/S)+dlog(SC/S)**2.D0)+(2.D0*UC+2.D0
     #*SC)/(UC**2+SC**2)*((UC+SC)*(dlog(SC/S)-dlog(-UC/S))+(SC-UC)*(2.D0
     #*dlog(-TC/S)-dlog(-UC/S)-dlog(SC/S))))
      s9 = N*(85.D0/9.D0+0.3141592653589793D1**2+2.D0*dlog(-TC/S)*(dlog(
     #-TC/S)+dlog(SC/S)-2.D0*dlog(-UC/S))+(UC**2-SC**2)/(2.D0*UC**2+2.D0
     #*SC**2)*(2.D0*0.3141592653589793D1**2+2.D0*(dlog(-TC/S)-dlog(-UC/S
     #))**2.D0+dlog(-TC/S)**2.D0-2.D0*dlog(-TC/S)*dlog(SC/S)+dlog(SC/S)*
     #*2.D0)-UC*TC/(UC**2+SC**2)*(dlog(-TC/S)-dlog(SC/S))+2.D0*SC*TC/(UC
     #**2+SC**2)*(dlog(-TC/S)-dlog(-UC/S))+11.D0/3.D0*dlog(MU2/S)-11.D0/
     #3.D0*dlog(-TC/S))+GTR*(4.D0/3.D0*dlog(-TC/S)-4.D0/3.D0*dlog(MU2/S)
     #-20.D0/9.D0)
      s7 = s8+s9
      s8 = N
      s6 = s7*s8
      s4 = s5*s6
      s5 = CF*(UC**2+SC**2)/TC**2
      s3 = s4*s5
      s6 = 4.D0
      s9 = CF*(-16.D0-2.D0*dlog(SC/S)**2.D0+2.D0*0.3141592653589793D1**2
     #+dlog(SC/S)*(6.D0+8.D0*dlog(-UC/S)-8.D0*dlog(-TC/S))+(-2.D0*UC**2+
     #2.D0*TC**2)/(UC**2+TC**2)*(2.D0*dlog(SC/S)**2.D0-2.D0*dlog(SC/S)*d
     #log(-UC/S)+dlog(-UC/S)**2.D0-2.D0*dlog(-TC/S)*dlog(SC/S)+dlog(-TC/
     #S)**2.D0)+(2.D0*UC+2.D0*TC)/(UC**2+TC**2)*((UC+TC)*(dlog(-TC/S)-dl
     #og(-UC/S))+(TC-UC)*(2.D0*dlog(SC/S)-dlog(-UC/S)-dlog(-TC/S))))
      s10 = N*(85.D0/9.D0-0.3141592653589793D1**2+2.D0*dlog(SC/S)*(dlog(
     #-TC/S)+dlog(SC/S)-2.D0*dlog(-UC/S))+(UC**2-TC**2)/(2.D0*UC**2+2.D0
     #*TC**2)*(3.D0*dlog(SC/S)**2.D0-4.D0*dlog(SC/S)*dlog(-UC/S)+2.D0*dl
     #og(-UC/S)**2.D0-2.D0*dlog(-TC/S)*dlog(SC/S)+dlog(-TC/S)**2.D0)-UC*
     #SC/(UC**2+TC**2)*(dlog(SC/S)-dlog(-TC/S))+2.D0*TC*SC/(UC**2+TC**2)
     #*(dlog(SC/S)-dlog(-UC/S))+11.D0/3.D0*dlog(MU2/S)-11.D0/3.D0*dlog(S
     #C/S))+GTR*(4.D0/3.D0*dlog(SC/S)-4.D0/3.D0*dlog(MU2/S)-20.D0/9.D0)
      s8 = s9+s10
      s9 = N
      s7 = s8*s9
      s5 = s6*s7
      s6 = CF*(UC**2+TC**2)/SC**2
      s4 = s5*s6
      s2 = s3+s4
      teres = s1+s2
	VTEMP(14) = TERAJ+TERES
c       qi + qbi --> g + qbi
      teraj = 0.D0
      teres = 0.D0
	VTEMP(15) = TERAJ+TERES
	VPARTVI(8) = VTEMP(14) + VTEMP(15)
C
C	j0 = 9 : qi + qbi --> jet + g
C
c       qi + qbi --> qk + g
      teraj = 0.D0
      teres = 0.D0
	VTEMP(16) = TERAJ+TERES
c       qi + qbi --> qbk + g
      teraj = 0.D0
      teres = 0.D0
	VTEMP(17) = TERAJ+TERES
c       qi + qbi --> qi + g
      teraj = 0.D0
      teres = 0.D0
	VTEMP(18) = TERAJ+TERES
c       qi + qbi --> qbi + g
      teraj = 0.D0
      teres = 0.D0
	VTEMP(19) = TERAJ+TERES
c       qi + qbi --> g + g
      s1 = -2.D0*B_QQ()*dlog(ptm**2/S)*C(SC,TC,UC,N)-2.D0*B_GG()*dlog(pt
     #**2/S)*C(SC,TC,UC,N)+(dlog(SC/S)*((2.D0*N**2*(N**2-1.D0)+2.D0*(N**
     #2-1.D0)/N**2)*(TC**2+UC**2)/TC/UC-4.D0*(N**2-1.D0)**2.D0*(TC**2+UC
     #**2)/SC**2)+4.D0*N**2*(N**2-1.D0)*(dlog(-TC/S)*(UC/TC-2.D0/SC**2*U
     #C**2)+dlog(-UC/S)*(1.D0/UC*TC-2.D0/SC**2*TC**2))-4.D0*(N**2-1.D0)*
     #(UC/TC+1.D0/UC*TC)*(dlog(-TC/S)+dlog(-UC/S)))*dlog(ptm**2/S)
      s2 = s1-32.D0*(SC**2/4.D0+SC*TC/4.D0+TC**2/4.D0)*(N-1.D0)*(N+1.D0)
     #*(dlog(-TC/S)*N**4*TC**2/2.D0+dlog(-TC/S)*N**4*SC*TC-dlog(-TC/S)*N
     #**2*SC**2/2.D0+dlog(-TC/S)*N**4*SC**2/2.D0+dlog(-UC/S)*N**4*TC**2/
     #2.D0-dlog(-UC/S)*N**2*SC**2/2.D0+N**4*TC**2*dlog(SC/S)/2.D0+cmplx(
     #0.D0,1.D0)*N**4*TC**2*0.3141592653589793D1/2.D0+SC**2*N**4*dlog(SC
     #/S)/4.D0+cmplx(0.D0,1.D0)*SC**2*N**4*0.3141592653589793D1/4.D0+SC*
     #N**4*TC*dlog(SC/S)/2.D0+cmplx(0.D0,1.D0)*SC*N**4*TC*0.314159265358
     #9793D1/2.D0-SC*TC*N**2*dlog(SC/S)/2.D0-cmplx(0.D0,1.D0)*SC*TC*N**2
     #*0.3141592653589793D1/2.D0-TC**2*N**2*dlog(SC/S)/2.D0-cmplx(0.D0,1
     #.D0)*TC**2*N**2*0.3141592653589793D1/2.D0+SC**2*dlog(SC/S)/4.D0+cm
     #plx(0.D0,1.D0)*SC**2*0.3141592653589793D1/4.D0)/(SC/2.D0+TC/2.D0)/
     #SC**2/N**2/TC
      teraj = s2+2.D0*A4_GG(1.D0)*C(SC,TC,UC,N)*(dlog(pt**2/S)**2.D0/4.D
     #0-dlog(pt**2/S)*dlog(ptm**2/S)/2.D0)-A4_QQ(1.D0)*C(SC,TC,UC,N)*dlo
     #g(ptm**2/S)**2.D0/2.D0
      s1 = (-7.D0*N+7.D0/N+22.D0/3.D0*N*dlog(MU2/S)-8.D0/3.D0*GTR*dlog(M
     #U2/S))*(N**2-1.D0)/N*(1.D0/UC/TC*N**2-1.D0/UC/TC-2.D0*N**2/SC**2)*
     #(TC**2+UC**2)
      s4 = 4.D0*N
      s6 = N**2-1.D0
      s8 = dlog(-TC/S)*dlog(-UC/S)/N*(TC**2+UC**2)/TC/UC/2.D0+(dlog(SC/S
     #)**2.D0-0.3141592653589793D1**2)*(1.D0/N**3*SC**2/TC/UC/4.D0+1.D0/
     #N/8.D0+1.D0/N*UC/TC/4.D0+1.D0/N*TC/UC/4.D0-1.D0/N*TC**2/SC**2/4.D0
     #-1.D0/N*UC**2/SC**2/4.D0-N/SC**2*TC**2/4.D0-N/SC**2*UC**2/4.D0)+dl
     #og(SC/S)*(5.D0/8.D0*N-9.D0/8.D0/N-1.D0/N**3-TC/UC*N/2.D0-1.D0/TC*U
     #C*N/2.D0-TC/UC/N**3/2.D0-1.D0/TC*UC/N**3/2.D0-N/SC**2*TC**2/4.D0-N
     #/SC**2*UC**2/4.D0+1.D0/N*TC**2/SC**2/4.D0+1.D0/N*UC**2/SC**2/4.D0)
     #+0.3141592653589793D1**2/N/8.D0+3.D0/8.D0*0.3141592653589793D1**2/
     #TC*UC/N**3+3.D0/8.D0*0.3141592653589793D1**2*TC/UC/N**3+0.31415926
     #53589793D1**2/N**3/2.D0+0.3141592653589793D1**2/TC*UC*N/8.D0+0.314
     #1592653589793D1**2*TC/UC*N/8.D0-0.3141592653589793D1**2*N/SC**2*TC
     #**2/2.D0
      s7 = s8-0.3141592653589793D1**2*N/SC**2*UC**2/2.D0+N/8.D0-N/SC**2*
     #TC**2/4.D0-N/SC**2*UC**2/4.D0+1.D0/N/8.D0-1.D0/N*TC**2/SC**2/4.D0-
     #1.D0/N*UC**2/SC**2/4.D0+dlog(-TC/S)**2.D0*(N*SC/TC/4.D0-N*UC/SC-N/
     #4.D0+1.D0/N*TC/UC/2.D0-1.D0/N*UC/SC/4.D0+1.D0/TC*UC/N**3/4.D0-1.D0
     #/N**3*SC/UC/2.D0)+dlog(-TC/S)*(N/SC**2*TC**2+N/SC**2*UC**2+3.D0/4.
     #D0*N*TC/SC-5.D0/4.D0/TC*UC*N-N/4.D0-1.D0/N*UC/SC/4.D0-2.D0/N*SC/UC
     #-1.D0/N*SC/TC/2.D0-3.D0/4.D0/N**3*SC/TC-1.D0/N**3/4.D0)+dlog(SC/S)
     #*dlog(-TC/S)*(N/SC**2*TC**2+N/SC**2*UC**2-1.D0/TC*UC*N/2.D0+1.D0/N
     #*UC/SC/2.D0-1.D0/N*TC/UC+1.D0/N**3*SC/UC-1.D0/TC*UC/N**3/2.D0)
      s5 = s6*s7
      s3 = s4*s5
      s5 = 4.D0*N
      s7 = N**2-1.D0
      s9 = dlog(-TC/S)*dlog(-UC/S)/N*(TC**2+UC**2)/TC/UC/2.D0+(dlog(SC/S
     #)**2.D0-0.3141592653589793D1**2)*(1.D0/N**3*SC**2/TC/UC/4.D0+1.D0/
     #N/8.D0+1.D0/N*UC/TC/4.D0+1.D0/N*TC/UC/4.D0-1.D0/N*TC**2/SC**2/4.D0
     #-1.D0/N*UC**2/SC**2/4.D0-N/SC**2*TC**2/4.D0-N/SC**2*UC**2/4.D0)+dl
     #og(SC/S)*(5.D0/8.D0*N-9.D0/8.D0/N-1.D0/N**3-TC/UC*N/2.D0-1.D0/TC*U
     #C*N/2.D0-TC/UC/N**3/2.D0-1.D0/TC*UC/N**3/2.D0-N/SC**2*TC**2/4.D0-N
     #/SC**2*UC**2/4.D0+1.D0/N*TC**2/SC**2/4.D0+1.D0/N*UC**2/SC**2/4.D0)
     #+0.3141592653589793D1**2/N/8.D0+3.D0/8.D0*0.3141592653589793D1**2/
     #TC*UC/N**3+3.D0/8.D0*0.3141592653589793D1**2*TC/UC/N**3+0.31415926
     #53589793D1**2/N**3/2.D0+0.3141592653589793D1**2/TC*UC*N/8.D0+0.314
     #1592653589793D1**2*TC/UC*N/8.D0-0.3141592653589793D1**2*N/SC**2*TC
     #**2/2.D0
      s8 = s9-0.3141592653589793D1**2*N/SC**2*UC**2/2.D0+N/8.D0-N/SC**2*
     #TC**2/4.D0-N/SC**2*UC**2/4.D0+1.D0/N/8.D0-1.D0/N*TC**2/SC**2/4.D0-
     #1.D0/N*UC**2/SC**2/4.D0+dlog(-UC/S)**2.D0*(N*SC/UC/4.D0-N*TC/SC-N/
     #4.D0+1.D0/N*UC/TC/2.D0-1.D0/N*TC/SC/4.D0+TC/UC/N**3/4.D0-1.D0/N**3
     #*SC/TC/2.D0)+dlog(-UC/S)*(N/SC**2*TC**2+N/SC**2*UC**2+3.D0/4.D0*N*
     #UC/SC-5.D0/4.D0*TC/UC*N-N/4.D0-1.D0/N*TC/SC/4.D0-2.D0/N*SC/TC-1.D0
     #/N*SC/UC/2.D0-3.D0/4.D0/N**3*SC/UC-1.D0/N**3/4.D0)+dlog(SC/S)*dlog
     #(-UC/S)*(N/SC**2*TC**2+N/SC**2*UC**2-TC/UC*N/2.D0+1.D0/N*TC/SC/2.D
     #0-1.D0/N*UC/TC+1.D0/N**3*SC/TC-TC/UC/N**3/2.D0)
      s6 = s7*s8
      s4 = s5*s6
      s2 = s3+s4
      teres = s1+s2
	VTEMP(20) = TERAJ+TERES
	VPARTVI(9) = (2.D0*GTR-1.D0)*(VTEMP(16) + VTEMP(17)) +
     #	VTEMP(18) + VTEMP(19) + VTEMP(20)
C
C	j0 = 10 : qi + g --> jet + qk
C
c       qi + g --> qi + qk
      teraj = 0.D0
      teres = 0.D0
	VTEMP(21) = TERAJ+TERES
c       qi + g --> qbk + qk
      teraj = 0.D0
      teres = 0.D0
	VTEMP(22) = TERAJ+TERES
	VPARTVI(10) = VTEMP(21) + VTEMP(22)
C
C	j0 = 11 : qi + g --> jet + qbk
C
c       qi + g --> qi + qbk
      teraj = 0.D0
      teres = 0.D0
	VTEMP(23) = TERAJ+TERES
c       qi + g --> qk + qbk
      teraj = 0.D0
      teres = 0.D0
	VTEMP(24) = TERAJ+TERES
	VPARTVI(11) = VTEMP(23) + VTEMP(24)
C
C	j0 = 12 : qi + g --> jet + qbi
C
c       qi + g --> qi + qbi
      teraj = 0.D0
      teres = 0.D0
	VTEMP(25) = TERAJ+TERES
	VPARTVI(12) = VTEMP(25)
C
C	j0 = 13 : qi + g --> jet + g
C
c       qi + g --> qi + g
      s1 = (B_QQ()+B_GG())*dlog(ptm**2/S)*C(TC,SC,UC,N)+(B_QQ()+B_GG())*
     #dlog(pt**2/S)*C(TC,SC,UC,N)-(dlog(-TC/S)*((2.D0*N**2*(N**2-1.D0)+2
     #.D0*(N**2-1.D0)/N**2)*(SC**2+UC**2)/SC/UC-4.D0*(N**2-1.D0)**2.D0*(
     #SC**2+UC**2)/TC**2)+4.D0*N**2*(N**2-1.D0)*(dlog(SC/S)*(UC/SC-2.D0/
     #TC**2*UC**2)+dlog(-UC/S)*(1.D0/UC*SC-2.D0/TC**2*SC**2))-4.D0*(N**2
     #-1.D0)*(UC/SC+1.D0/UC*SC)*(dlog(SC/S)+dlog(-UC/S)))*dlog(ptm**2/S)
      s2 = s1+32.D0*(SC**2/4.D0+SC*TC/4.D0+TC**2/4.D0)*(N-1.D0)*(N+1.D0)
     #*(dlog(-TC/S)*N**4*SC**2/2.D0+dlog(-TC/S)*SC*N**4*TC/2.D0+dlog(-TC
     #/S)*N**4*TC**2/4.D0-dlog(-TC/S)*SC**2*N**2/2.D0-dlog(-TC/S)*SC*N**
     #2*TC/2.D0+dlog(-TC/S)*TC**2/4.D0+dlog(-UC/S)*N**4*SC**2/2.D0-dlog(
     #-UC/S)*N**2*TC**2/2.D0+N**4*SC**2*dlog(SC/S)/2.D0+cmplx(0.D0,1.D0)
     #*N**4*SC**2*0.3141592653589793D1/2.D0+N**4*SC*TC*dlog(SC/S)+cmplx(
     #0.D0,1.D0)*N**4*SC*TC*0.3141592653589793D1-N**2*TC**2*dlog(SC/S)/2
     #.D0-cmplx(0.D0,1.D0)*N**2*TC**2*0.3141592653589793D1/2.D0+N**4*TC*
     #*2*dlog(SC/S)/2.D0+cmplx(0.D0,1.D0)*N**4*TC**2*0.3141592653589793D
     #1/2.D0)/TC**2/SC/(SC/2.D0+TC/2.D0)/N**2
      teraj = s2-(A4_QQ(1.D0)+A4_GG(1.D0))*C(TC,SC,UC,N)*(dlog(pt**2/S)*
     #*2.D0/4.D0-dlog(pt**2/S)*dlog(ptm**2/S)/2.D0)+(A4_QQ(1.D0)+A4_GG(1
     #.D0))*C(TC,SC,UC,N)*dlog(ptm**2/S)**2.D0/4.D0
      s1 = -(-7.D0*N+7.D0/N+22.D0/3.D0*N*dlog(MU2/S)-8.D0/3.D0*GTR*dlog(
     #MU2/S))*(N**2-1.D0)/N*(1.D0/UC/SC*N**2-1.D0/UC/SC-2.D0*N**2/TC**2)
     #*(SC**2+UC**2)
      s4 = -4.D0*N
      s6 = N**2-1.D0
      s8 = dlog(SC/S)*dlog(-UC/S)/N*(SC**2+UC**2)/SC/UC/2.D0+dlog(-TC/S)
     #**2.D0*(1.D0/N**3*TC**2/SC/UC/4.D0+1.D0/N/8.D0+1.D0/N*SC/UC/4.D0+1
     #.D0/N*UC/SC/4.D0-1.D0/N*SC**2/TC**2/4.D0-1.D0/N*UC**2/TC**2/4.D0-N
     #/TC**2*SC**2/4.D0-N/TC**2*UC**2/4.D0)+dlog(-TC/S)*(5.D0/8.D0*N-9.D
     #0/8.D0/N-1.D0/N**3-SC/UC*N/2.D0-1.D0/SC*UC*N/2.D0-SC/UC/N**3/2.D0-
     #1.D0/SC*UC/N**3/2.D0-N/TC**2*SC**2/4.D0-N/TC**2*UC**2/4.D0+1.D0/N*
     #SC**2/TC**2/4.D0+1.D0/N*UC**2/TC**2/4.D0)+0.3141592653589793D1**2/
     #N/8.D0+3.D0/8.D0*0.3141592653589793D1**2*SC/UC/N**3+3.D0/8.D0*0.31
     #41592653589793D1**2/SC*UC/N**3+0.3141592653589793D1**2/N**3/2.D0+0
     #.3141592653589793D1**2*SC/UC*N/8.D0+0.3141592653589793D1**2/SC*UC*
     #N/8.D0-0.3141592653589793D1**2*N/TC**2*SC**2/2.D0
      s7 = s8-0.3141592653589793D1**2*N/TC**2*UC**2/2.D0+N/8.D0-N/TC**2*
     #SC**2/4.D0-N/TC**2*UC**2/4.D0+1.D0/N/8.D0-1.D0/N*SC**2/TC**2/4.D0-
     #1.D0/N*UC**2/TC**2/4.D0+(dlog(SC/S)**2.D0-0.3141592653589793D1**2)
     #*(N*TC/SC/4.D0-N*UC/TC-N/4.D0+1.D0/N*SC/UC/2.D0-1.D0/N*UC/TC/4.D0+
     #1.D0/SC*UC/N**3/4.D0-1.D0/N**3*TC/UC/2.D0)+dlog(SC/S)*(N/TC**2*SC*
     #*2+N/TC**2*UC**2+3.D0/4.D0*N*SC/TC-5.D0/4.D0/SC*UC*N-N/4.D0-1.D0/N
     #*UC/TC/4.D0-2.D0/N*TC/UC-1.D0/N*TC/SC/2.D0-3.D0/4.D0/N**3*TC/SC-1.
     #D0/N**3/4.D0)+dlog(-TC/S)*dlog(SC/S)*(N/TC**2*SC**2+N/TC**2*UC**2-
     #1.D0/SC*UC*N/2.D0+1.D0/N*UC/TC/2.D0-1.D0/N*SC/UC+1.D0/N**3*TC/UC-1
     #.D0/SC*UC/N**3/2.D0)
      s5 = s6*s7
      s3 = s4*s5
      s5 = -4.D0*N
      s7 = N**2-1.D0
      s9 = dlog(SC/S)*dlog(-UC/S)/N*(SC**2+UC**2)/SC/UC/2.D0+dlog(-TC/S)
     #**2.D0*(1.D0/N**3*TC**2/SC/UC/4.D0+1.D0/N/8.D0+1.D0/N*SC/UC/4.D0+1
     #.D0/N*UC/SC/4.D0-1.D0/N*SC**2/TC**2/4.D0-1.D0/N*UC**2/TC**2/4.D0-N
     #/TC**2*SC**2/4.D0-N/TC**2*UC**2/4.D0)+dlog(-TC/S)*(5.D0/8.D0*N-9.D
     #0/8.D0/N-1.D0/N**3-SC/UC*N/2.D0-1.D0/SC*UC*N/2.D0-SC/UC/N**3/2.D0-
     #1.D0/SC*UC/N**3/2.D0-N/TC**2*SC**2/4.D0-N/TC**2*UC**2/4.D0+1.D0/N*
     #SC**2/TC**2/4.D0+1.D0/N*UC**2/TC**2/4.D0)+0.3141592653589793D1**2/
     #N/8.D0+3.D0/8.D0*0.3141592653589793D1**2*SC/UC/N**3+3.D0/8.D0*0.31
     #41592653589793D1**2/SC*UC/N**3+0.3141592653589793D1**2/N**3/2.D0+0
     #.3141592653589793D1**2*SC/UC*N/8.D0+0.3141592653589793D1**2/SC*UC*
     #N/8.D0-0.3141592653589793D1**2*N/TC**2*SC**2/2.D0
      s8 = s9-0.3141592653589793D1**2*N/TC**2*UC**2/2.D0+N/8.D0-N/TC**2*
     #SC**2/4.D0-N/TC**2*UC**2/4.D0+1.D0/N/8.D0-1.D0/N*SC**2/TC**2/4.D0-
     #1.D0/N*UC**2/TC**2/4.D0+dlog(-UC/S)**2.D0*(N*TC/UC/4.D0-N*SC/TC-N/
     #4.D0+1.D0/N*UC/SC/2.D0-1.D0/N*SC/TC/4.D0+SC/UC/N**3/4.D0-1.D0/N**3
     #*TC/SC/2.D0)+dlog(-UC/S)*(N/TC**2*SC**2+N/TC**2*UC**2+3.D0/4.D0*N*
     #UC/TC-5.D0/4.D0*SC/UC*N-N/4.D0-1.D0/N*SC/TC/4.D0-2.D0/N*TC/SC-1.D0
     #/N*TC/UC/2.D0-3.D0/4.D0/N**3*TC/UC-1.D0/N**3/4.D0)+dlog(-TC/S)*dlo
     #g(-UC/S)*(N/TC**2*SC**2+N/TC**2*UC**2-SC/UC*N/2.D0+1.D0/N*SC/TC/2.
     #D0-1.D0/N*UC/SC+1.D0/N**3*TC/SC-SC/UC/N**3/2.D0)
      s6 = s7*s8
      s4 = s5*s6
      s2 = s3+s4
      teres = s1+s2
	VTEMP(26) = TERAJ+TERES
c       qi + g --> g + g
      teraj = 0.D0
      teres = 0.D0
	VTEMP(27) = TERAJ+TERES
	VPARTVI(13) = VTEMP(26) + VTEMP(27)
C
C	j0 = 14 : qi + g --> jet + qi
C
c       qi + g --> qk + qi
      teraj = 0.D0
      teres = 0.D0
	VTEMP(28) = TERAJ+TERES
c       qi + g --> qbk + qi
      teraj = 0.D0
      teres = 0.D0
	VTEMP(29) = TERAJ+TERES
c       qi + g --> qi + qi
      teraj = 0.D0
      teres = 0.D0
	VTEMP(30) = TERAJ+TERES
c       qi + g --> qbi + qi
      teraj = 0.D0
      teres = 0.D0
	VTEMP(31) = TERAJ+TERES
c       qi + g --> g + qi
      s1 = (B_QQ()+B_GG())*dlog(ptm**2/S)*C(UC,SC,TC,N)+(B_QQ()+B_GG())*
     #dlog(pt**2/S)*C(UC,SC,TC,N)-(dlog(-UC/S)*((2.D0*N**2*(N**2-1.D0)+2
     #.D0*(N**2-1.D0)/N**2)*(SC**2+TC**2)/SC/TC-4.D0*(N**2-1.D0)**2.D0*(
     #SC**2+TC**2)/UC**2)+4.D0*N**2*(N**2-1.D0)*(dlog(SC/S)*(TC/SC-2.D0/
     #UC**2*TC**2)+dlog(-TC/S)*(1.D0/TC*SC-2.D0*SC**2/UC**2))-4.D0*(N**2
     #-1.D0)*(TC/SC+1.D0/TC*SC)*(dlog(SC/S)+dlog(-TC/S)))*dlog(ptm**2/S)
      s2 = s1-16.D0*(SC**2/4.D0+SC*TC/4.D0+TC**2/4.D0)*(N-1.D0)*(N+1.D0)
     #*(-dlog(-TC/S)*N**2*TC**2/2.D0-dlog(-TC/S)*N**2*SC*TC-dlog(-TC/S)*
     #N**2*SC**2/2.D0+dlog(-TC/S)*N**4*SC**2/2.D0+dlog(-UC/S)*N**4*SC**2
     #/4.D0+dlog(-UC/S)*N**4*TC**2/4.D0+dlog(-UC/S)*SC*N**2*TC/2.D0+dlog
     #(-UC/S)*SC**2/4.D0+dlog(-UC/S)*SC*TC/2.D0+dlog(-UC/S)*TC**2/4.D0-N
     #**2*SC**2*dlog(SC/S)/2.D0-cmplx(0.D0,1.D0)*N**2*SC**2*0.3141592653
     #589793D1/2.D0-N**2*SC*TC*dlog(SC/S)-cmplx(0.D0,1.D0)*N**2*SC*TC*0.
     #3141592653589793D1-N**2*TC**2*dlog(SC/S)/2.D0-cmplx(0.D0,1.D0)*N**
     #2*TC**2*0.3141592653589793D1/2.D0+N**4*TC**2*dlog(SC/S)/2.D0+cmplx
     #(0.D0,1.D0)*N**4*TC**2*0.3141592653589793D1/2.D0)/(SC/2.D0+TC/2.D0
     #)**2.D0/N**2/SC/TC
      teraj = s2-(A4_GG(1.D0)+A4_QQ(1.D0))*C(UC,SC,TC,N)*(dlog(pt**2/S)*
     #*2.D0/4.D0-dlog(pt**2/S)*dlog(ptm**2/S)/2.D0)+(A4_GG(1.D0)+A4_QQ(1
     #.D0))*C(UC,SC,TC,N)*dlog(ptm**2/S)**2.D0/4.D0
      s1 = -(-7.D0*N+7.D0/N+22.D0/3.D0*N*dlog(MU2/S)-8.D0/3.D0*GTR*dlog(
     #MU2/S))*(N**2-1.D0)/N*(1.D0/TC/SC*N**2-1.D0/TC/SC-2.D0*N**2/UC**2)
     #*(SC**2+TC**2)
      s4 = -4.D0*N
      s6 = N**2-1.D0
      s8 = dlog(SC/S)*dlog(-TC/S)/N*(SC**2+TC**2)/SC/TC/2.D0+dlog(-UC/S)
     #**2.D0*(1.D0/N**3*UC**2/SC/TC/4.D0+1.D0/N/8.D0+1.D0/N*SC/TC/4.D0+1
     #.D0/N*TC/SC/4.D0-1.D0/N*SC**2/UC**2/4.D0-1.D0/N*TC**2/UC**2/4.D0-N
     #/UC**2*SC**2/4.D0-N/UC**2*TC**2/4.D0)+dlog(-UC/S)*(5.D0/8.D0*N-9.D
     #0/8.D0/N-1.D0/N**3-SC/TC*N/2.D0-1.D0/SC*TC*N/2.D0-SC/TC/N**3/2.D0-
     #1.D0/SC*TC/N**3/2.D0-N/UC**2*SC**2/4.D0-N/UC**2*TC**2/4.D0+1.D0/N*
     #SC**2/UC**2/4.D0+1.D0/N*TC**2/UC**2/4.D0)+0.3141592653589793D1**2/
     #N/8.D0+3.D0/8.D0*0.3141592653589793D1**2*SC/TC/N**3+3.D0/8.D0*0.31
     #41592653589793D1**2/SC*TC/N**3+0.3141592653589793D1**2/N**3/2.D0+0
     #.3141592653589793D1**2*SC/TC*N/8.D0+0.3141592653589793D1**2/SC*TC*
     #N/8.D0-0.3141592653589793D1**2*N/UC**2*SC**2/2.D0
      s7 = s8-0.3141592653589793D1**2*N/UC**2*TC**2/2.D0+N/8.D0-N/UC**2*
     #SC**2/4.D0-N/UC**2*TC**2/4.D0+1.D0/N/8.D0-1.D0/N*SC**2/UC**2/4.D0-
     #1.D0/N*TC**2/UC**2/4.D0+(dlog(SC/S)**2.D0-0.3141592653589793D1**2)
     #*(N*UC/SC/4.D0-N*TC/UC-N/4.D0+1.D0/N*SC/TC/2.D0-1.D0/N*TC/UC/4.D0+
     #1.D0/SC*TC/N**3/4.D0-1.D0/N**3*UC/TC/2.D0)+dlog(SC/S)*(N/UC**2*SC*
     #*2+N/UC**2*TC**2+3.D0/4.D0*N*SC/UC-5.D0/4.D0/SC*TC*N-N/4.D0-1.D0/N
     #*TC/UC/4.D0-2.D0/N*UC/TC-1.D0/N*UC/SC/2.D0-3.D0/4.D0/N**3*UC/SC-1.
     #D0/N**3/4.D0)+dlog(-UC/S)*dlog(SC/S)*(N/UC**2*SC**2+N/UC**2*TC**2-
     #1.D0/SC*TC*N/2.D0+1.D0/N*TC/UC/2.D0-1.D0/N*SC/TC+1.D0/N**3*UC/TC-1
     #.D0/SC*TC/N**3/2.D0)
      s5 = s6*s7
      s3 = s4*s5
      s5 = -4.D0*N
      s7 = N**2-1.D0
      s9 = dlog(SC/S)*dlog(-TC/S)/N*(SC**2+TC**2)/SC/TC/2.D0+dlog(-UC/S)
     #**2.D0*(1.D0/N**3*UC**2/SC/TC/4.D0+1.D0/N/8.D0+1.D0/N*SC/TC/4.D0+1
     #.D0/N*TC/SC/4.D0-1.D0/N*SC**2/UC**2/4.D0-1.D0/N*TC**2/UC**2/4.D0-N
     #/UC**2*SC**2/4.D0-N/UC**2*TC**2/4.D0)+dlog(-UC/S)*(5.D0/8.D0*N-9.D
     #0/8.D0/N-1.D0/N**3-SC/TC*N/2.D0-1.D0/SC*TC*N/2.D0-SC/TC/N**3/2.D0-
     #1.D0/SC*TC/N**3/2.D0-N/UC**2*SC**2/4.D0-N/UC**2*TC**2/4.D0+1.D0/N*
     #SC**2/UC**2/4.D0+1.D0/N*TC**2/UC**2/4.D0)+0.3141592653589793D1**2/
     #N/8.D0+3.D0/8.D0*0.3141592653589793D1**2*SC/TC/N**3+3.D0/8.D0*0.31
     #41592653589793D1**2/SC*TC/N**3+0.3141592653589793D1**2/N**3/2.D0+0
     #.3141592653589793D1**2*SC/TC*N/8.D0+0.3141592653589793D1**2/SC*TC*
     #N/8.D0-0.3141592653589793D1**2*N/UC**2*SC**2/2.D0
      s8 = s9-0.3141592653589793D1**2*N/UC**2*TC**2/2.D0+N/8.D0-N/UC**2*
     #SC**2/4.D0-N/UC**2*TC**2/4.D0+1.D0/N/8.D0-1.D0/N*SC**2/UC**2/4.D0-
     #1.D0/N*TC**2/UC**2/4.D0+dlog(-TC/S)**2.D0*(N*UC/TC/4.D0-N*SC/UC-N/
     #4.D0+1.D0/N*TC/SC/2.D0-1.D0/N*SC/UC/4.D0+SC/TC/N**3/4.D0-1.D0/N**3
     #*UC/SC/2.D0)+dlog(-TC/S)*(N/UC**2*SC**2+N/UC**2*TC**2+3.D0/4.D0*N*
     #TC/UC-5.D0/4.D0*SC/TC*N-N/4.D0-1.D0/N*SC/UC/4.D0-2.D0/N*UC/SC-1.D0
     #/N*UC/TC/2.D0-3.D0/4.D0/N**3*UC/TC-1.D0/N**3/4.D0)+dlog(-UC/S)*dlo
     #g(-TC/S)*(N/UC**2*SC**2+N/UC**2*TC**2-SC/TC*N/2.D0+1.D0/N*SC/UC/2.
     #D0-1.D0/N*TC/SC+1.D0/N**3*UC/SC-SC/TC/N**3/2.D0)
      s6 = s7*s8
      s4 = s5*s6
      s2 = s3+s4
      teres = s1+s2
	VTEMP(32) = TERAJ+TERES
	VPARTVI(14) = (2.D0*GTR-1.D0)*(VTEMP(28) + VTEMP(29)) +
     #	VTEMP(30) + VTEMP(31) + VTEMP(32)
C
C	j0 = 15 : g + g --> jet + qi
C
c       g + g --> qbi + qi
      s1 = -2.D0*B_GG()*dlog(ptm**2/S)*C(SC,TC,UC,N)-2.D0*B_QQ()*dlog(pt
     #**2/S)*C(SC,TC,UC,N)+(dlog(SC/S)*((2.D0*N**2*(N**2-1.D0)+2.D0*(N**
     #2-1.D0)/N**2)*(UC**2+TC**2)/UC/TC-4.D0*(N**2-1.D0)**2.D0*(UC**2+TC
     #**2)/SC**2)+4.D0*N**2*(N**2-1.D0)*(dlog(-UC/S)*(1.D0/UC*TC-2.D0*TC
     #**2/SC**2)+dlog(-TC/S)*(UC/TC-2.D0*UC**2/SC**2))-4.D0*(N**2-1.D0)*
     #(1.D0/UC*TC+UC/TC)*(dlog(-UC/S)+dlog(-TC/S)))*dlog(ptm**2/S)
      s2 = s1-32.D0*(SC**2/4.D0+SC*TC/4.D0+TC**2/4.D0)*(N-1.D0)*(N+1.D0)
     #*(dlog(-TC/S)*N**4*TC**2/2.D0+dlog(-TC/S)*N**4*SC*TC+dlog(-TC/S)*N
     #**4*SC**2/2.D0-dlog(-TC/S)*N**2*SC**2/2.D0+dlog(-UC/S)*N**4*TC**2/
     #2.D0-dlog(-UC/S)*N**2*SC**2/2.D0+SC*N**4*TC*dlog(SC/S)/2.D0+cmplx(
     #0.D0,1.D0)*SC*N**4*TC*0.3141592653589793D1/2.D0+SC**2*N**4*dlog(SC
     #/S)/4.D0+cmplx(0.D0,1.D0)*SC**2*N**4*0.3141592653589793D1/4.D0+N**
     #4*TC**2*dlog(SC/S)/2.D0+cmplx(0.D0,1.D0)*N**4*TC**2*0.314159265358
     #9793D1/2.D0-SC*N**2*TC*dlog(SC/S)/2.D0-cmplx(0.D0,1.D0)*SC*N**2*TC
     #*0.3141592653589793D1/2.D0-N**2*TC**2*dlog(SC/S)/2.D0-cmplx(0.D0,1
     #.D0)*N**2*TC**2*0.3141592653589793D1/2.D0+SC**2*dlog(SC/S)/4.D0+cm
     #plx(0.D0,1.D0)*SC**2*0.3141592653589793D1/4.D0)/N**2/(SC/2.D0+TC/2
     #.D0)/SC**2/TC
      teraj = s2+2.D0*A4_QQ(1.D0)*C(SC,TC,UC,N)*(dlog(pt**2/S)**2.D0/4.D
     #0-dlog(pt**2/S)*dlog(ptm**2/S)/2.D0)-A4_GG(1.D0)*C(SC,TC,UC,N)*dlo
     #g(ptm**2/S)**2.D0/2.D0
      s1 = (-7.D0*N+7.D0/N+22.D0/3.D0*N*dlog(MU2/S)-8.D0/3.D0*GTR*dlog(M
     #U2/S))*(N**2-1.D0)/N*(1.D0/TC/UC*N**2-1.D0/TC/UC-2.D0*N**2/SC**2)*
     #(UC**2+TC**2)
      s4 = 4.D0*N
      s6 = N**2-1.D0
      s8 = dlog(-UC/S)*dlog(-TC/S)/N*(UC**2+TC**2)/UC/TC/2.D0+(dlog(SC/S
     #)**2.D0-0.3141592653589793D1**2)*(1.D0/N**3*SC**2/UC/TC/4.D0+1.D0/
     #N/8.D0+1.D0/N*TC/UC/4.D0+1.D0/N*UC/TC/4.D0-1.D0/N*UC**2/SC**2/4.D0
     #-1.D0/N*TC**2/SC**2/4.D0-N/SC**2*UC**2/4.D0-N/SC**2*TC**2/4.D0)+dl
     #og(SC/S)*(5.D0/8.D0*N-9.D0/8.D0/N-1.D0/N**3-UC/TC*N/2.D0-1.D0/UC*T
     #C*N/2.D0-UC/TC/N**3/2.D0-1.D0/UC*TC/N**3/2.D0-N/SC**2*UC**2/4.D0-N
     #/SC**2*TC**2/4.D0+1.D0/N*UC**2/SC**2/4.D0+1.D0/N*TC**2/SC**2/4.D0)
     #+0.3141592653589793D1**2/N/8.D0+3.D0/8.D0*0.3141592653589793D1**2/
     #UC*TC/N**3+3.D0/8.D0*0.3141592653589793D1**2*UC/TC/N**3+0.31415926
     #53589793D1**2/N**3/2.D0+0.3141592653589793D1**2/UC*TC*N/8.D0+0.314
     #1592653589793D1**2*UC/TC*N/8.D0-0.3141592653589793D1**2*N/SC**2*UC
     #**2/2.D0
      s7 = s8-0.3141592653589793D1**2*N/SC**2*TC**2/2.D0+N/8.D0-N/SC**2*
     #UC**2/4.D0-N/SC**2*TC**2/4.D0+1.D0/N/8.D0-1.D0/N*UC**2/SC**2/4.D0-
     #1.D0/N*TC**2/SC**2/4.D0+dlog(-UC/S)**2.D0*(N*SC/UC/4.D0-N*TC/SC-N/
     #4.D0+1.D0/N*UC/TC/2.D0-1.D0/N*TC/SC/4.D0+1.D0/UC*TC/N**3/4.D0-1.D0
     #/N**3*SC/TC/2.D0)+dlog(-UC/S)*(N/SC**2*UC**2+N/SC**2*TC**2+3.D0/4.
     #D0*N*UC/SC-5.D0/4.D0/UC*TC*N-N/4.D0-1.D0/N*TC/SC/4.D0-2.D0/N*SC/TC
     #-1.D0/N*SC/UC/2.D0-3.D0/4.D0/N**3*SC/UC-1.D0/N**3/4.D0)+dlog(SC/S)
     #*dlog(-UC/S)*(N/SC**2*UC**2+N/SC**2*TC**2-1.D0/UC*TC*N/2.D0+1.D0/N
     #*TC/SC/2.D0-1.D0/N*UC/TC+1.D0/N**3*SC/TC-1.D0/UC*TC/N**3/2.D0)
      s5 = s6*s7
      s3 = s4*s5
      s5 = 4.D0*N
      s7 = N**2-1.D0
      s9 = dlog(-UC/S)*dlog(-TC/S)/N*(UC**2+TC**2)/UC/TC/2.D0+(dlog(SC/S
     #)**2.D0-0.3141592653589793D1**2)*(1.D0/N**3*SC**2/UC/TC/4.D0+1.D0/
     #N/8.D0+1.D0/N*TC/UC/4.D0+1.D0/N*UC/TC/4.D0-1.D0/N*UC**2/SC**2/4.D0
     #-1.D0/N*TC**2/SC**2/4.D0-N/SC**2*UC**2/4.D0-N/SC**2*TC**2/4.D0)+dl
     #og(SC/S)*(5.D0/8.D0*N-9.D0/8.D0/N-1.D0/N**3-UC/TC*N/2.D0-1.D0/UC*T
     #C*N/2.D0-UC/TC/N**3/2.D0-1.D0/UC*TC/N**3/2.D0-N/SC**2*UC**2/4.D0-N
     #/SC**2*TC**2/4.D0+1.D0/N*UC**2/SC**2/4.D0+1.D0/N*TC**2/SC**2/4.D0)
     #+0.3141592653589793D1**2/N/8.D0+3.D0/8.D0*0.3141592653589793D1**2/
     #UC*TC/N**3+3.D0/8.D0*0.3141592653589793D1**2*UC/TC/N**3+0.31415926
     #53589793D1**2/N**3/2.D0+0.3141592653589793D1**2/UC*TC*N/8.D0+0.314
     #1592653589793D1**2*UC/TC*N/8.D0-0.3141592653589793D1**2*N/SC**2*UC
     #**2/2.D0
      s8 = s9-0.3141592653589793D1**2*N/SC**2*TC**2/2.D0+N/8.D0-N/SC**2*
     #UC**2/4.D0-N/SC**2*TC**2/4.D0+1.D0/N/8.D0-1.D0/N*UC**2/SC**2/4.D0-
     #1.D0/N*TC**2/SC**2/4.D0+dlog(-TC/S)**2.D0*(N*SC/TC/4.D0-N*UC/SC-N/
     #4.D0+1.D0/N*TC/UC/2.D0-1.D0/N*UC/SC/4.D0+UC/TC/N**3/4.D0-1.D0/N**3
     #*SC/UC/2.D0)+dlog(-TC/S)*(N/SC**2*UC**2+N/SC**2*TC**2+3.D0/4.D0*N*
     #TC/SC-5.D0/4.D0*UC/TC*N-N/4.D0-1.D0/N*UC/SC/4.D0-2.D0/N*SC/UC-1.D0
     #/N*SC/TC/2.D0-3.D0/4.D0/N**3*SC/TC-1.D0/N**3/4.D0)+dlog(SC/S)*dlog
     #(-TC/S)*(N/SC**2*UC**2+N/SC**2*TC**2-UC/TC*N/2.D0+1.D0/N*UC/SC/2.D
     #0-1.D0/N*TC/UC+1.D0/N**3*SC/UC-UC/TC/N**3/2.D0)
      s6 = s7*s8
      s4 = s5*s6
      s2 = s3+s4
      teres = s1+s2
	VTEMP(33) = TERAJ+TERES
c       g + g --> g + qi
      teraj = 0.D0
      teres = 0.D0
	VTEMP(34) = TERAJ+TERES
	VPARTVI(15) = VTEMP(33) + VTEMP(34)
C
C	j0 = 16 : g + g --> jet + g
C
c       g + g --> qi + g
      teraj = 0.D0
      teres = 0.D0
	VTEMP(35) = TERAJ+TERES
c       g + g --> g + g
      s1 = -2.D0*B_GG()*dlog(ptm**2/S)*Dg(SC,TC,UC,N)-2.D0*B_GG()*dlog(p
     #t**2/S)*Dg(SC,TC,UC,N)+(16.D0*(N**2-1.D0)*N**3*dlog(SC/S)*(3.D0-2.
     #D0*TC*UC/SC**2+(TC**4+UC**4)/UC**2/TC**2)+16.D0*(N**2-1.D0)*N**3*d
     #log(-TC/S)*(3.D0-2.D0*UC*SC/TC**2+(UC**4+SC**4)/SC**2/UC**2)+16.D0
     #*(N**2-1.D0)*N**3*dlog(-UC/S)*(3.D0-2.D0*SC*TC/UC**2+(SC**4+TC**4)
     #/SC**2/TC**2))*dlog(ptm**2/S)
      teraj = s1+512.D0*N**3*(TC**2/4.D0+TC*SC/4.D0+SC**2/4.D0)**2.D0*(N
     #-1.D0)*(N+1.D0)*(dlog(SC/S)*SC**2/4.D0+dlog(SC/S)*TC*SC/2.D0+dlog(
     #SC/S)*TC**2/2.D0+dlog(-TC/S)*TC**2/4.D0+dlog(-TC/S)*TC*SC/2.D0+dlo
     #g(-TC/S)*SC**2/2.D0+dlog(-UC/S)*TC**2/4.D0+dlog(-UC/S)*SC**2/4.D0)
     #/TC**2/SC**2/(SC/2.D0+TC/2.D0)**2.D0+2.D0*A4_GG(1.D0)*Dg(SC,TC,UC,
     #N)*(dlog(pt**2/S)**2.D0/4.D0-dlog(pt**2/S)*dlog(ptm**2/S)/2.D0)-A4
     #_GG(1.D0)*Dg(SC,TC,UC,N)*dlog(ptm**2/S)**2.D0/2.D0
      s1 = (-1072.D0/9.D0*N+320.D0/9.D0*GTR+16.D0*N*0.3141592653589793D1
     #**2+176.D0/3.D0*N*dlog(MU2/S)-64.D0/3.D0*GTR*dlog(MU2/S))*(N**2-1.
     #D0)*N**2*(3.D0-TC*UC/SC**2-UC*SC/TC**2-SC*TC/UC**2)
      s3 = 4.D0*N**2-4.D0
      s5 = N**2
      s7 = N*((2.D0*TC**2+2.D0*UC**2)/TC/UC*(dlog(SC/S)**2.D0-0.31415926
     #53589793D1**2)+(4.D0*SC*TC/UC**2+4.D0*UC*SC/TC**2-6.D0)*dlog(-TC/S
     #)*dlog(-UC/S)+(4.D0/3.D0*TC*UC/SC**2-14.D0/3.D0*TC/UC-14.D0/3.D0/T
     #C*UC-14.D0-8.D0*TC**2/UC**2-8.D0*UC**2/TC**2)*dlog(SC/S)-1.D0-0.31
     #41592653589793D1**2)+GTR*((10.D0/3.D0*TC/UC+10.D0/3.D0/TC*UC+16.D0
     #/3.D0*TC*UC/SC**2-2.D0)*dlog(SC/S)+(-SC**2-UC*TC)/TC/UC*(dlog(SC/S
     #)**2.D0-0.3141592653589793D1**2)+(-2.D0*TC**2-2.D0*UC**2)/TC/UC*dl
     #og(-TC/S)*dlog(-UC/S)+2.D0-0.3141592653589793D1**2)+N*(-(-2.D0*UC*
     #*2-2.D0*SC**2)/UC/SC*dlog(-TC/S)**2.D0+(4.D0*TC*UC/SC**2+4.D0*SC*T
     #C/UC**2-6.D0)*dlog(-UC/S)*dlog(SC/S)+(4.D0/3.D0*UC*SC/TC**2-14.D0/
     #3.D0*UC/SC-14.D0/3.D0/UC*SC-14.D0-8.D0*UC**2/SC**2-8.D0*SC**2/UC**
     #2)*dlog(-TC/S)-1.D0-0.3141592653589793D1**2)
      s6 = s7+GTR*((10.D0/3.D0*UC/SC+10.D0/3.D0/UC*SC+16.D0/3.D0*UC*SC/T
     #C**2-2.D0)*dlog(-TC/S)-(TC**2+UC*SC)/UC/SC*dlog(-TC/S)**2.D0-(2.D0
     #*UC**2+2.D0*SC**2)/UC/SC*dlog(-UC/S)*dlog(SC/S)+2.D0-0.31415926535
     #89793D1**2)+N*(-(-2.D0*SC**2-2.D0*TC**2)/SC/TC*dlog(-UC/S)**2.D0+(
     #4.D0*UC*SC/TC**2+4.D0*TC*UC/SC**2-6.D0)*dlog(SC/S)*dlog(-TC/S)+(4.
     #D0/3.D0*SC*TC/UC**2-14.D0/3.D0*SC/TC-14.D0/3.D0/SC*TC-14.D0-8.D0*S
     #C**2/TC**2-8.D0*TC**2/SC**2)*dlog(-UC/S)-1.D0-0.3141592653589793D1
     #**2)+GTR*((10.D0/3.D0*SC/TC+10.D0/3.D0/SC*TC+16.D0/3.D0*SC*TC/UC**
     #2-2.D0)*dlog(-UC/S)-(UC**2+TC*SC)/SC/TC*dlog(-UC/S)**2.D0-(2.D0*SC
     #**2+2.D0*TC**2)/SC/TC*dlog(SC/S)*dlog(-TC/S)+2.D0-0.31415926535897
     #93D1**2)
      s4 = s5*s6
      s2 = s3*s4
      teres = s1+s2
	VTEMP(36) = TERAJ+TERES
	VPARTVI(16) = 4.D0*GTR*VTEMP(35) + VTEMP(36)
	RETURN
	END
C*****************************************************	
	SUBROUTINE VEC_VI2O(S,SC,TC,UC,YS,FI,VPARTVI2)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	PARAMETER (J0MAX=16)
	DIMENSION VPARTVI2(J0MAX),HH34(J0MAX)
	PI=4.D0*DATAN(1.D0)
	DO I = 1,J0MAX
	  VPARTVI2(I) = 0.D0
	  HH34(I) = 0.D0
	ENDDO
C-------------------------------------------------------------------
	s12 = sc/2.d0
	s13 = -tc/2.d0
	s23 = -uc/2.d0
	s14 = s23
	s24 = s13
	s34 = s12
	s15 = 0.d0
	s25 = 0.d0
	s35 = 0.d0
	s45 = 0.d0
	CALL VEC_H34O(S12,S13,S14,S15,S23,S24,S25,S34,S35,S45,
     #	HH34)
	DW=DCOSH(2.D0*YS)+DCOS(2.D0*FI)
	XJ1=DLOG(DSIN(FI))
	XJ2=DSIN(2.D0*FI)*DLOG(DSIN(FI))*DATAN(DSIN(FI)/(1.D0-DCOS(FI)))
	A34=4.D0*XJ2+DSINH(2.D0*YS)*2.D0*YS*XJ1
	FFG=A34/DW+DLOG(2.D0)*DLOG(4.D0*DCOSH(YS)**2)
	DO I = 1,J0MAX
	  VPARTVI2(I) = HH34(I)*(FFG+FFG)/PI
	ENDDO
	RETURN
	END
C*****************************************************	
C pour production d'un photon	
	SUBROUTINE STRFRAO(X1,IH1,X2,IH2,X4,IH4,SFO)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/GDF/JNF
	COMMON/SCALE/M,MF,MU
	DIMENSION F1(-6:6),F2(-6:6),D4(-6:6)
	PARAMETER (K0MAX=32,J0MAX=16)
	DIMENSION SF(K0MAX),SFO(K0MAX)
	CALL FSTRU(X1,M*M,IH1,F1)
	CALL FSTRU(X2,M*M,IH2,F2)
	CALL DFRAG(X4,MF*MF,IH4,D4)
C initialisation du tableau SF(K0MAX)
	ZERO = 0.D0
	CALL VEC_DINIT(SF,K0MAX,ZERO)
	CALL VEC_DINIT(SFO,K0MAX,ZERO)
C on commence les processus
	DO I=1,JNF-1
	  DO K=I+1,JNF
C	j0 = 1 : qi + qk --> jet + qk
	    SF(1) = F1(I)*F2(K)*D4(K) + F1(K)*F2(I)*D4(I) +	    
     #              F1(-I)*F2(-K)*D4(-K) + F1(-K)*F2(-I)*D4(-I) + SF(1)
	    SF(1+J0MAX) = F1(K)*F2(I)*D4(K) + F1(I)*F2(K)*D4(I) + 	    
     #                 F1(-K)*F2(-I)*D4(-K) + F1(-I)*F2(-K)*D4(-I) + 
     #                 SF(1+J0MAX)
C	j0 = 2 : qi + qk --> jet + g
	    SF(2) = F1(I)*F2(K)*D4(0) + 	    
     #              F1(-I)*F2(-K)*D4(0) + SF(2)
	    SF(2+J0MAX) = F1(K)*F2(I)*D4(0) + 	    
     #                 F1(-K)*F2(-I)*D4(0) + SF(2+J0MAX)
C	j0 = 3 : qi + qbk --> jet + qbk
	    SF(3) = F1(I)*F2(-K)*D4(-K) + F1(K)*F2(-I)*D4(-I) + 	    
     #              F1(-I)*F2(K)*D4(K) + F1(-K)*F2(I)*D4(I) + SF(3)
	    SF(3+J0MAX) = F1(-K)*F2(I)*D4(-K) + F1(-I)*F2(K)*D4(-I) +	    
     #                 F1(K)*F2(-I)*D4(K) + F1(I)*F2(-K)*D4(I) + 
     #                 SF(3+J0MAX)
C	j0 = 7 : qi + qbi --> jet + qbk
	    SF(7) = F1(I)*F2(-I)*D4(-K) + F1(K)*F2(-K)*D4(-I) + 	    
     #              F1(-I)*F2(I)*D4(K) + F1(-K)*F2(K)*D4(I) + SF(7)
	    SF(7+J0MAX) = F1(-I)*F2(I)*D4(-K) +  F1(-K)*F2(K)*D4(-I) +	    
     #                 F1(I)*F2(-I)*D4(K) + F1(K)*F2(-K)*D4(I) + 
     #                 SF(7+J0MAX)
	  ENDDO
	ENDDO
	DO I=1,JNF
	  DO K=1,JNF
	    IF (K.NE.I) THEN
C	j0 = 4 : qi + qbk --> jet + g
	      SF(4) = F1(I)*F2(-K)*D4(0) + SF(4)
	      SF(4+J0MAX) = F1(-K)*F2(I)*D4(0) + SF(4+J0MAX)
C	j0 = 10 : qi + g --> jet + qk
	      SF(10) = F1(I)*F2(0)*D4(K) + 	    
     #                F1(-I)*F2(0)*D4(-K) + SF(10)
	      SF(10+J0MAX) = F1(0)*F2(I)*D4(K) + 	    
     #                   F1(0)*F2(-I)*D4(-K) + SF(10+J0MAX)
C	j0 = 11 : qi + g --> jet + qbk
	      SF(11) = F1(I)*F2(0)*D4(-K) + 	    
     #                F1(-I)*F2(0)*D4(K) + SF(11)
	      SF(11+J0MAX) = F1(0)*F2(I)*D4(-K) + 	    
     #                   F1(0)*F2(-I)*D4(K) + SF(11+J0MAX)
	    ENDIF
	  ENDDO
	ENDDO 	    
	DO I=1,JNF
C	j0 = 5 : qi + qi --> jet + qi
	  SF(5) = (F1(I)*F2(I)*D4(I) + 	    
     #             F1(-I)*F2(-I)*D4(-I))/2.D0 + SF(5)
C	j0 = 6 : qi + qi --> jet + g
	  SF(6) = (F1(I)*F2(I)*D4(0) + 	    
     #             F1(-I)*F2(-I)*D4(0))/2.D0 + SF(6)
C	j0 = 8 : qi + qbi --> jet + qbi
	    SF(8) = F1(I)*F2(-I)*D4(-I) + 	    
     #              F1(-I)*F2(I)*D4(I) + SF(8)
	    SF(8+J0MAX) = F1(-I)*F2(I)*D4(-I) + 	    
     #                 F1(I)*F2(-I)*D4(I) + SF(8+J0MAX)
C	j0 = 9 : qi + qbi --> jet + g
	    SF(9) = F1(I)*F2(-I)*D4(0) + SF(9)
	    SF(9+J0MAX) = F1(-I)*F2(I)*D4(0) + SF(9+J0MAX)
C	j0 = 12 : qi + g --> jet + qbi
	    SF(12) = F1(I)*F2(0)*D4(-I) + 	    
     #              F1(-I)*F2(0)*D4(I) + SF(12)
	    SF(12+J0MAX) = F1(0)*F2(I)*D4(-I) + 	    
     #                 F1(0)*F2(-I)*D4(I) + SF(12+J0MAX)
C	j0 = 13 : qi + g --> jet + g
	    SF(13) = F1(I)*F2(0)*D4(0) + 	    
     #              F1(-I)*F2(0)*D4(0) + SF(13)
	    SF(13+J0MAX) = F1(0)*F2(I)*D4(0) + 	    
     #                 F1(0)*F2(-I)*D4(0) + SF(13+J0MAX)
C	j0 = 14 : qi + g --> jet + qi
	    SF(14) = F1(I)*F2(0)*D4(I) + 	    
     #              F1(-I)*F2(0)*D4(-I) + SF(14)
	    SF(14+J0MAX) = F1(0)*F2(I)*D4(I) + 	    
     #                 F1(0)*F2(-I)*D4(-I) + SF(14+J0MAX)
C	j0 = 15 : g + g --> jet + qi
	  SF(15) = (F1(0)*F2(0)*D4(I) + 	    
     #             F1(0)*F2(0)*D4(-I))/2.d0 + SF(15)
	ENDDO 	    
C	j0 = 16 : g + g --> jet + g
	SF(16) = (F1(0)*F2(0)*D4(0))/2.d0
C
     	SF(5+J0MAX) = SF(5)
     	SF(6+J0MAX) = SF(6)
C
     	SF(15+J0MAX) = SF(15)
     	SF(16+J0MAX) = SF(16)
C
C on divise tout par x1 x2
	XX = 1.D0/(X1*X2)
	CALL VEC_DMULT_CONSTANT(SF,K0MAX,XX,SFO)	    
	RETURN
	END
C
