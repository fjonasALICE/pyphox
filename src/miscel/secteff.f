C*****************************************************
c terme proportionnel a 1/(1-z)_+ dans les noyaux d'altarelli-parisi
c p_ij^4(z) = a_ij^4(z)/(1-z)_+ + b_ij \delta(1-z)
	DOUBLE PRECISION FUNCTION A4_QQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	A4_QQ = CF*(1.D0+Z**2)
	RETURN
	END
C--------------------------------------------------------------
	DOUBLE PRECISION FUNCTION A4_GG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	A4_GG = 2.D0*N*(Z+(1.D0-Z)**2/Z+Z*(1.D0-Z)**2)
	RETURN
	END
C--------------------------------------------------------------
	DOUBLE PRECISION FUNCTION A4_QG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	TF = .5D0
	A4_QG = TF*(Z**2+(1.D0-Z)**2)*(1.D0-Z)
	RETURN
	END
C--------------------------------------------------------------
	DOUBLE PRECISION FUNCTION A4_GQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	A4_GQ = CF*(1.D0+(1.D0-Z)**2)/Z*(1.D0-Z)
	RETURN
	END
C--------------------------------------------------------------
	DOUBLE PRECISION FUNCTION A4_QP(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	A4_QP = N*(Z**2+(1.D0-Z)**2)*(1.D0-Z)
	RETURN
	END
C--------------------------------------------------------------
	DOUBLE PRECISION FUNCTION A4_PQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	A4_PQ = (1.D0+(1.D0-Z)**2)/Z*(1.D0-Z)
	RETURN
	END
C*****************************************************
c terme proportionnel a \delta(1-z) dans les noyaux d'altarelli-parisi
c p_ij^4(z) = a_ij^4(z)/(1-z)_+ + b_ij \delta(1-z)
	DOUBLE PRECISION FUNCTION B_QQ()
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	B_QQ = 3.D0/2.D0*CF
	RETURN
	END
C--------------------------------------------------------------
	DOUBLE PRECISION FUNCTION B_GG()
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	B_GG = (11.D0*N-4.D0*GTR)/6.D0
	RETURN
	END
C*****************************************************
c extra terme venant des dimensions n-4
c a_ij^n(z) = a_ij^4(z) - epsilon a_ij^(n-4)(z)  , n = 4-2*epsilon
	DOUBLE PRECISION FUNCTION ANM4_QQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	ANM4_QQ = CF*(1.D0-Z)**2
	RETURN
	END
C--------------------------------------------------------------
	DOUBLE PRECISION FUNCTION ANM4_GG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	ANM4_GG = 0.D0
	RETURN
	END
C--------------------------------------------------------------
	DOUBLE PRECISION FUNCTION ANM4_QG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	TF = .5D0
	ANM4_QG = TF*2.D0*Z*(1.D0-Z)**2
	RETURN
	END
C--------------------------------------------------------------
	DOUBLE PRECISION FUNCTION ANM4_GQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	ANM4_GQ = CF*Z*(1.D0-Z)
	RETURN
	END
C--------------------------------------------------------------
	DOUBLE PRECISION FUNCTION ANM4_QP(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/COUL/N,CF,GTR
	ANM4_QP = N*2.D0*Z*(1.D0-Z)**2
	RETURN
	END
C--------------------------------------------------------------
	DOUBLE PRECISION FUNCTION ANM4_PQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	ANM4_PQ = Z*(1.D0-Z)
	RETURN
	END
C
C*****************************************************
C elements de matrice 2 --> 2
C ces elements de matrice ne sont pas moyennes sur les
C spin et couleurs initiaux
C*****************************************************
	SUBROUTINE VEC_BO(SC,TC,UC,N,VBORN)
	IMPLICIT REAL*8 (A-H,L-Z)
	PARAMETER (J0MAX=16)
	DIMENSION VBORN(J0MAX)
	DO I = 1,J0MAX
		VBORN(I) = 0.D0
	ENDDO
C on commence les processus
C	j0 = 1 : qi + qk --> jet + qk
	VBORN(1) = A(SC,TC,UC,N)
C	j0 = 2 : qi + qk --> jet + g
	VBORN(2) = 0.D0
c	j0 = 3 : qi + qbk --> jet + qbk
	VBORN(3) = A(UC,TC,SC,N)
C	j0 = 4 : qi + qbk --> jet + g
	VBORN(4) = 0.D0
C	j0 = 5 : qi + qi --> jet + qi
	VBORN(5) = A(SC,TC,UC,N) + A(SC,UC,TC,N) + B(SC,TC,UC,N)
C	j0 = 6 : qi + qi --> jet + g
	VBORN(6) = 0.D0
C	j0 = 7 : qi + qbi --> jet + qbk
	VBORN(7) = A(TC,SC,UC,N)
C	j0 = 8 : qi + qbi --> jet + qbi
	VBORN(8) = A(UC,TC,SC,N) + A(UC,SC,TC,N) + B(UC,TC,SC,N)
C	j0 = 9 : qi + qbi --> jet + g
	VBORN(9) = C(SC,TC,UC,N)
C	j0 = 10 : qi + g --> jet + qk
	VBORN(10) = 0.D0
C	j0 = 11 : qi + g --> jet + qbk
	VBORN(11) = 0.D0
C	j0 = 12 : qi + g --> jet + qbi
	VBORN(12) = 0.D0
C	j0 = 13 : qi + g --> jet + g
	VBORN(13) = -C(TC,SC,UC,N)
C	j0 = 14 : qi + g --> jet + qi
	VBORN(14) = -C(UC,SC,TC,N)
C	j0 = 15 : g + g --> jet + qi
	VBORN(15) = C(SC,UC,TC,N)
C	j0 = 16 : g + g --> jet + g
	VBORN(16) = Dg(SC,TC,UC,N)
C
	RETURN
	END
C
	SUBROUTINE VEC_BD(SC,TC,UC,N,VBORN)
	IMPLICIT REAL*8 (A-H,L-Z)
	PARAMETER (J0MAX=11)
	PARAMETER (EQI=0.3333333333333333)
	DIMENSION VBORN(J0MAX)
	DO I = 1,J0MAX
		VBORN(I) = 0.D0
	ENDDO
C on commence les processus
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
c       qi + qk --> qi + photon
c       qi + qk --> qk + photon
	VBORN(1) = 0.D0
C	j0 = 2 : D + Dp --> jet + ph
c       qi + qk --> qi + photon
c       qi + qk --> qk + photon
	VBORN(2) = 0.D0
C	j0 = 3 : U + Up --> jet + ph
c       qi + qk --> qi + photon
c       qi + qk --> qk + photon
	VBORN(3) = 0.D0
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
c       qi + qbk --> qi + photon
c       qi + qbk --> qbk + photon
	VBORN(4) = 0.D0
C	j0 = 5 : D + Dpb --> jet + ph
c       qi + qbk --> qi + photon
c       qi + qbk --> qbk + photon
	VBORN(5) = 0.D0
C	j0 = 6 : U + Upb --> jet + ph
c       qi + qbk --> qi + photon
c       qi + qbk --> qbk + photon
	VBORN(6) = 0.D0
C
C	j0 = 7 : qi + qi --> jet + ph
C
c       qi + qi --> qi + photon
c       qi + qbi --> qi + photon
	VBORN(7) = 0.D0
C
C	qi + qbi --> jet + ph
C
C	j0 = 8 : D + Db --> jet + ph
c       qi + qbi --> qk + photon
c       qi + qbi --> qbk + photon
c       qi + qbi --> qi + photon
c       qi + qbi --> qbi + photon
c       qi + qbi --> g + photon
	VBORN(8) = E(SC,TC,UC,N)*EQI**2
C	j0 = 9 : U + Ub --> jet + ph
c       qi + qbi --> qk + photon
c       qi + qbi --> qbk + photon
c       qi + qbi --> qi + photon
c       qi + qbi --> qbi + photon
c       qi + qbi --> g + photon
	VBORN(9) = E(SC,TC,UC,N)*EQI**2*4.D0
C
C	j0 = 10 : qi + g --> jet + ph
C
c       qi + g --> qi + photon
c       qi + g --> g + photon
 	VBORN(10) = -E(TC,SC,UC,N)*EQI**2
C
C	j0 = 11 : g + g --> jet + ph
C
c       g + g --> qi + photon
c       g + g --> qbi + photon
	VBORN(11) = 0.D0
C
	RETURN
	END
C
C*****************************************************
C facteur de couleur pour les differents elements de matrice 2 --> 2
C*****************************************************
	SUBROUTINE FSPCOUO(N,SPCOU)
	IMPLICIT REAL*8 (A-H,L-Z)
	PARAMETER (K0MAX=32,J0MAX=16)
	DIMENSION SPCOU(K0MAX)
	ZERO =0.D0
	CALL VEC_DINIT(SPCOU,K0MAX,ZERO)
	VC = N*N - 1.D0
C on commence les processus
C	j0 = 1 : qi + qk --> jet + qk
	SPCOU(1) = 1.D0/(4.D0*N*N)
C	j0 = 2 : qi + qk --> jet + g
	SPCOU(2) = 1.D0/(4.D0*N*N)
c	j0 = 3 : qi + qbk --> jet + qbk
	SPCOU(3) = 1.D0/(4.D0*N*N)
C	j0 = 4 : qi + qbk --> jet + g
	SPCOU(4) = 1.D0/(4.D0*N*N)
C	j0 = 5 : qi + qi --> jet + qi
	SPCOU(5) = 1.D0/(4.D0*N*N)
C	j0 = 6 : qi + qi --> jet + g
	SPCOU(6) = 1.D0/(4.D0*N*N)
C	j0 = 7 : qi + qbi --> jet + qbk
	SPCOU(7) = 1.D0/(4.D0*N*N)
C	j0 = 8 : qi + qbi --> jet + qbi
	SPCOU(8) = 1.D0/(4.D0*N*N)
C	j0 = 9 : qi + qbi --> jet + g
	SPCOU(9) = 1.D0/(4.D0*N*N)
C	j0 = 10 : qi + g --> jet + qk
	SPCOU(10) = 1.D0/(4.D0*N*VC)
C	j0 = 11 : qi + g --> jet + qbk
	SPCOU(11) = 1.D0/(4.D0*N*VC)
C	j0 = 12 : qi + g --> jet + qbi
	SPCOU(12) = 1.D0/(4.D0*N*VC)
C	j0 = 13 : qi + g --> jet + g
	SPCOU(13) = 1.D0/(4.D0*N*VC)
C	j0 = 14 : qi + g --> jet + qi
	SPCOU(14) = 1.D0/(4.D0*N*VC)
C	j0 = 15 : g + g --> jet + qi
	SPCOU(15) = 1.D0/(4.D0*VC*VC)
C	j0 = 16 : g + g --> jet + g
	SPCOU(16) = 1.D0/(4.D0*VC*VC)
C
	DO I=1,J0MAX
	  SPCOU(I+J0MAX) = SPCOU(I)
	ENDDO
	RETURN
	END
C
	SUBROUTINE FSPCOUD(N,SPCOU)
	IMPLICIT REAL*8 (A-H,L-Z)
	PARAMETER (K0MAX=22,J0MAX=11)
	DIMENSION SPCOU(K0MAX)
	ZERO =0.D0
	CALL VEC_DINIT(SPCOU,K0MAX,ZERO)
	VC = N*N - 1.D0
C on commence les processus
C
C	qi + qk --> jet + ph
C
C	j0 = 1 : D + U --> jet + ph
	SPCOU(1) = 1.D0/(4.D0*N*N)
C	j0 = 2 : D + Dp --> jet + ph
	SPCOU(2) = 1.D0/(4.D0*N*N)
C	j0 = 3 : U + Up --> jet + ph
	SPCOU(3) = 1.D0/(4.D0*N*N)
C
C	qi + qbk --> jet + ph
C
C	j0 = 4 : D + Ub --> jet + ph
	SPCOU(4) = 1.D0/(4.D0*N*N)
C	j0 = 5 : D + Dpb --> jet + ph
	SPCOU(5) = 1.D0/(4.D0*N*N)
C	j0 = 6 : U + Upb --> jet + ph
	SPCOU(6) = 1.D0/(4.D0*N*N)
C
C	j0 = 7 : qi + qi --> jet + ph
C
	SPCOU(7) = 1.D0/(4.D0*N*N)
C
C	qi + qbi --> jet + ph
C
C	j0 = 8 : D + Db --> jet + ph
	SPCOU(8) = 1.D0/(4.D0*N*N)
C	j0 = 9 : U + Ub --> jet + ph
	SPCOU(9) = 1.D0/(4.D0*N*N)
C
C	j0 = 10 : qi + g --> jet + ph
C
	SPCOU(10) = 1.D0/(4.D0*N*VC)
C
C	j0 = 11 : g + g --> jet + ph
C
	SPCOU(11) = 1.D0/(4.D0*VC*VC)
C
	DO I=1,J0MAX
	  SPCOU(I+11) = SPCOU(I)
	ENDDO
	RETURN
	END
C	
	REAL*8 FUNCTION SCA(P,Q)
	IMPLICIT REAL*8 (A-H,L-Z)
	DIMENSION P(4),Q(4)
	SCA=P(1)*Q(1)-P(2)*Q(2)-P(3)*Q(3)-P(4)*Q(4)
	RETURN
	END
C*********************************************************************
C element de matrice 2 --> 2 a la Ellis et Sexton
C*********************************************************************
	DOUBLE PRECISION FUNCTION A(S,T,U,N)
	IMPLICIT REAL*8 (A-H,L-Z)
	VC = N*N - 1.D0
	A = 2.D0*VC*(S**2+U**2)/T**2
	RETURN
	END	
C
	DOUBLE PRECISION FUNCTION B(S,T,U,N)
	IMPLICIT REAL*8 (A-H,L-Z)
	VC = N*N - 1.D0
	B = -4.D0*VC/N*S**2/(U*T)
	RETURN
	END	
C
	DOUBLE PRECISION FUNCTION C(S,T,U,N)
	IMPLICIT REAL*8 (A-H,L-Z)
	VC = N*N - 1.D0
	C = 2.D0*VC/N*(VC/(U*T)-2.D0*N**2/S**2)*(T**2+U**2)
	RETURN
	END	
C
	DOUBLE PRECISION FUNCTION Dg(S,T,U,N)
	IMPLICIT REAL*8 (A-H,L-Z)
	VC = N*N - 1.D0
	Dg = 16.D0*VC*N**2*(3.D0-U*T/S**2-U*S/T**2-S*T/U**2)
	RETURN
	END	
C
	DOUBLE PRECISION FUNCTION E(S,T,U,N)
	IMPLICIT REAL*8 (A-H,L-Z)
	VC = N*N - 1.D0
	E = 4.D0*VC/(U*T)*(T**2+U**2)
	RETURN
	END	
C
	DOUBLE PRECISION FUNCTION F(S,T,U,N)
	IMPLICIT REAL*8 (A-H,L-Z)
	F = 8.D0*N/(U*T)*(T**2+U**2)
	RETURN
	END	
C*****************************************************
C reaction gluon + gluon --> gluon + photon
	DOUBLE PRECISION FUNCTION GGGP(SC,TC,UC)
	IMPLICIT REAL*8 (A-H,L-Z)
	DIMENSION QCHA(6)
	COMMON/GDF/JNF
	DATA QCHA/-1.D0,2.D0,-1.D0,2.D0,-1.D0,2.D0/
	PI=4.D0*DATAN(1.D0)
	SUMC = 0.D0
	DO I=1,JNF
	  SUMC = QCHA(I) + SUMC
	ENDDO
	SUMC4 = SUMC**2/9.D0
      t0 = 256*(-1-(TC-UC)/SC*dlog(TC/UC)-(TC**2+UC**2)/SC**2*(dlog(TC/U
     #C)**2+Pi**2)/2)**2+256*(-1-(SC-UC)/TC*dlog(-SC/UC)-(SC**2+UC**2)/T
     #C**2*dlog(-SC/UC)**2/2)**2+256*Pi**2*((SC-UC)/TC+(SC**2+UC**2)/TC*
     #*2*dlog(-SC/UC))**2+256*(-1-(SC-TC)/UC*dlog(-SC/TC)-(SC**2+TC**2)/
     #UC**2*dlog(-SC/TC)**2/2)**2+256*Pi**2*((SC-TC)/UC+(SC**2+TC**2)/UC
     #**2*dlog(-SC/TC))**2+1280
c        FACTOR = (N**2-4)/4/N 
        FACTOR = 5.d0/12.d0 
     	GGGP = SUMC4*T0*FACTOR
	RETURN
	END			
C*****************************************************
	DOUBLE PRECISION FUNCTION FAQQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/FSCHEME/ISCHEME
	COMMON/COUL/N,CF,GTR
	IF (ISCHEME.EQ.0) THEN
	  FAQQ = 0.D0
	ELSE IF (ISCHEME.EQ.1) THEN
	  FAQQ = CF*(-3.D0/2.D0-(1.D0+Z**2)*DLOG(Z)+(3.D0+2.D0*Z)*
     #	         (1.D0-Z))
	ENDIF 
	RETURN
	END
	DOUBLE PRECISION FUNCTION FBQQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/FSCHEME/ISCHEME
	COMMON/COUL/N,CF,GTR
	IF (ISCHEME.EQ.0) THEN
	  FBQQ = 0.D0
	ELSE IF (ISCHEME.EQ.1) THEN
	  FBQQ = CF*(1.D0+Z**2)
	ENDIF 
	RETURN
	END
	DOUBLE PRECISION FUNCTION FCQQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/FSCHEME/ISCHEME
	COMMON/COUL/N,CF,GTR
	PI=DATAN(1.D0)*4.D0
	IF (ISCHEME.EQ.0) THEN
	  FCQQ = 0.D0
	ELSE IF (ISCHEME.EQ.1) THEN
	  FCQQ = -CF*(9.D0/2.D0+PI**2/3.D0)
	ENDIF 
	RETURN
	END
	DOUBLE PRECISION FUNCTION FAQG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/FSCHEME/ISCHEME
	COMMON/AURENCHE/IAUREN
	IF (ISCHEME.EQ.0) THEN
	  FAQG = 0.D0
	ELSE IF (ISCHEME.EQ.1) THEN
c convention 2*(1-epsilon) degrees de polaristion pour le gluon
	  IF (IAUREN.EQ.0) THEN
	    FAQG = 1.D0/2.D0*(-(Z**2+(1.D0-Z)**2)*DLOG(Z)
     #	           )*(1.D0-Z)
c convention 2 degrees de polaristion pour le gluon
	  ELSE IF (IAUREN.EQ.1) THEN
	    FAQG = 1.D0/2.D0*(-(Z**2+(1.D0-Z)**2)*DLOG(Z)
     #	           +6.D0*Z*(1.D0-Z))*(1.D0-Z)
	  ENDIF 
	ENDIF 
	RETURN
	END
	DOUBLE PRECISION FUNCTION FBQG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/FSCHEME/ISCHEME
	IF (ISCHEME.EQ.0) THEN
	  FBQG=0.D0
	ELSE IF (ISCHEME.EQ.1) THEN
	  FBQG = 1.D0/2.D0*(Z**2+(1.D0-Z)**2)
     #	         *(1.D0-Z)
	ENDIF 
	RETURN
	END
	DOUBLE PRECISION FUNCTION FCQG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	COMMON/FSCHEME/ISCHEME
	IF (ISCHEME.EQ.0) THEN
	  FCQG=0.D0
	ELSE IF (ISCHEME.EQ.1) THEN
	  FCQG=0.D0
	ENDIF 
	RETURN
	END
	DOUBLE PRECISION FUNCTION FAGQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	FAGQ=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION FBGQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	FBGQ=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION FCGQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	FCGQ=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION FAGG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	FAGG=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION FBGG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	FBGG=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION FCGG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	FCGG=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION FAQP(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	FAQP=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION FBQP(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	FBQP=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION FCQP(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	FCQP=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION FAPQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	FAPQ=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION FBPQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	FBPQ=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION FCPQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	FCPQ=0.D0
	RETURN
	END
C*****************************************************
	DOUBLE PRECISION FUNCTION DAQQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DAQQ=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DBQQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DBQQ=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DCQQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DCQQ=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DAQG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DAQG=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DBQG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DBQG=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DCQG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DCQG=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DAGQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DAGQ=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DBGQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DBGQ=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DCGQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DCGQ=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DAGG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DAGG=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DBGG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DBGG=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DCGG(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DCGG=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DAQP(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DAQP=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DBQP(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DBQP=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DCQP(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DCQP=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DAPQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DAPQ=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DBPQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DBPQ=0.D0
	RETURN
	END
	DOUBLE PRECISION FUNCTION DCPQ(Z)
	IMPLICIT REAL*8 (A-H,L-Z)
	DCPQ=0.D0
	RETURN
	END
