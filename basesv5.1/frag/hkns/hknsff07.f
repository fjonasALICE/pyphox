c----------------------------------------------------------------------

c     HKNS 2007 WRAPPER

c Gives anti-proton FF from isospin relations when ih=
c Gives charged hadrons (as sum of pi,K,p/pbar) when ih=
c HKNS 2007 otherwise
c----------------------------------------------------------------------
      SUBROUTINE hknsffwrap(Q2,X,ISET,ICHARGE,FF,GRAD)
      IMPLICIT REAL*8 (A-H,M-Z)
      DIMENSION FF(-5:5),GRAD(-5:5,17)
      DIMENSION FFPI(-5:5),GRADPI(-5:5,17)
      DIMENSION FFK(-5:5),GRADK(-5:5,17)
      DIMENSION FFKP(-5:5),GRADKP(-5:5,17)
      DIMENSION FFKM(-5:5),GRADKM(-5:5,17)
      DIMENSION FFP(-5:5),GRADP(-5:5,17)
C     ISET=1: Pion LO Fragmentation functions and their gradient terms 
C          2: Pion NLO
C          3: Kaon LO
C          4: Kaon NLO
C          5: Proton LO
C          6: Proton NLO
C          7: Charged hadrons LO
C          8: Charged hadrons NLO
C          9: Antiproton LO
C          10: Antiproton NLO
C
C     ICHARGE=1: pi^+, K^+, or proton
C     ICHARGE=2: pi^-, K^-, or neutron
C     ICHARGE=3: pi^0=[pi^+ + pi^-]/2, [K^0+K^0b]/2, or [p+pb]/2
c ICHARGE irrelevant for the antiproton
C        If you want to obtain the fragmentation functions for each
C        K0, K0b, pb, or nb, you may use the relations in Appendix of
C        hep-ph/0702250.

      if(iset.le.6)then
		if((iset.ge.3).and.(iset.le.4).and.(icharge.eq.4))then
			CALL hknsffv2(Q2,X,ISET,1,FFKP,GRADKP)
			CALL hknsffv2(Q2,X,ISET,2,FFKM,GRADKM)
				do i=-5,5
			      	FF(i)=FFKP(i)+FFKM(i)
			      	do j=1,17
			            	GRAD(i,j)=GRADKP(i,j)+GRADKM(i,j)
			            enddo
			      enddo
			
		endif
         CALL hknsffv2(Q2,X,ISET,ICHARGE,FF,GRAD)
      elseif((iset.eq.7).or.(iset.eq.8))then
         call hknsffv2(Q2,X,iset-6,ICHARGE,FFPI,GRADPI)
         call hknsffv2(Q2,X,iset-4,ICHARGE,FFK,GRADK)
         call hknsffv2(Q2,X,iset-2,ICHARGE,FFP,GRADP)
         do i=-5,5
            FF(i)=FFPI(i)+FFK(i)+FFP(i)
c            do j=1,17
c               GRAD(i,j)=GRADPI(i,j)+GRADK(i,j)+GRADP(i,j)
c            enddo
         enddo
      elseif((iset.eq.9).or.(iset.eq.10))then
         call hknsffv2(Q2,X,iset-4,1,FFP,GRADP)
         do i=-5,5
            FF(i)=FFP(-i)
            do j=1,17
               GRAD(i,j)=GRADP(-i,j)
            enddo
         enddo
      else
         print*, 'species not available in HKNS'
         stop
      endif
      end

C ---------------------------------------------------------------------
      SUBROUTINE lspline_hkns(N,X,Y,B,C,D,ISET,I,J)
C ---------------------------------------------------------------------
C CALCULATE THE COEFFICIENTS B,C,D IN A CUBIC SPLINE INTERPOLATION.
C INTERPOLATION SUBROUTINES ARE TAKEN FROM
C G.E. FORSYTHE, M.A. MALCOLM AND C.B. MOLER,
C COMPUTER METHODS FOR MATHEMATICAL COMPUTATIONS (PRENTICE-HALL, 1977).
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NQ=33, NX=53, ND=146)
      DIMENSION Y(NX,NQ,ND),B(NX,NQ,ND),C(NX,NQ,ND),D(NX,NQ,ND)
     1         ,X(NX) 
      NM1=N-1
      IF(N.LT.2) RETURN
      IF(N.LT.3) GO TO 250
      D(1,J,I)=X(2)-X(1)
      C(2,J,I)=(Y(2,J,I)-Y(1,J,I))/D(1,J,I)
      DO 210 K=2,NM1
        D(K,J,I)=X(K+1)-X(K)
        B(K,J,I)=2.0D0*(D(K-1,J,I)+D(K,J,I))
        C(K+1,J,I)=(Y(K+1,J,I)-Y(K,J,I))/D(K,J,I)
        C(K,J,I)=C(K+1,J,I)-C(K,J,I)
  210 CONTINUE
      B(1,J,I)=-D(1,J,I)
      B(N,J,I)=-D(N-1,J,I)
      C(1,J,I)=0.0D0
      C(N,J,I)=0.0D0
      IF(N.EQ.3) GO TO 215
      C(1,J,I)=C(3,J,I)/(X(4)-X(2))-C(2,J,I)/(X(3)-X(1))
      C(N,J,I)=C(N-1,J,I)/(X(N)-X(N-2))-C(N-2,J,I)/(X(N-1)-X(N-3))
      C(1,J,I)=C(1,J,I)*D(1,J,I)**2.0D0/(X(4)-X(1))
      C(N,J,I)=-C(N,J,I)*D(N-1,J,I)**2.0D0/(X(N)-X(N-3))
  215 CONTINUE
      DO 220 K=2,N
        T=D(K-1,J,I)/B(K-1,J,I)
        B(K,J,I)=B(K,J,I)-T*D(K-1,J,I)
        C(K,J,I)=C(K,J,I)-T*C(K-1,J,I)
  220 CONTINUE
      C(N,J,I)=C(N,J,I)/B(N,J,I)
      DO 230 IB=1,NM1
        K=N-IB
        C(K,J,I)=(C(K,J,I)-D(K,J,I)*C(K+1,J,I))/B(K,J,I)
  230 CONTINUE
      B(N,J,I)=(Y(N,J,I)-Y(NM1,J,I))/D(NM1,J,I)
     1        +D(NM1,J,I)*(C(NM1,J,I)+2.0D0*C(N,J,I))
      DO 240 K=1,NM1
        B(K,J,I)=(Y(K+1,J,I)-Y(K,J,I))/D(K,J,I)
     1          -D(K,J,I)*(C(K+1,J,I)+2.0D0*C(K,J,I))
        D(K,J,I)=(C(K+1,J,I)-C(K,J,I))/D(K,J,I)
        C(K,J,I)=3.0D0*C(K,J,I)
  240 CONTINUE
      C(N,J,I)=3.0D0*C(N,J,I)
      D(N,J,I)=D(N-1,J,I)
      RETURN
  250 CONTINUE
      B(1,J,I)=(Y(2,J,I)-Y(1,J,I))/(X(2)-X(1))
      C(1,J,I)=0.0D0
      D(1,J,I)=0.0D0
      B(2,J,I)=B(1,J,I)
      C(2,J,I)=0.0D0
      D(2,J,I)=0.0D0
      RETURN
      END
C ---------------------------------------------------------------------
      INTEGER FUNCTION ISERCH(N,X,Y)
C ---------------------------------------------------------------------
C THIS FUNCTION SEARCHES "I" WHICH SATISFIES THE RELATION
C X(I) <= Y < X(I+1) BY USING A BINARY SEARCH.
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(53)

      MIN=1
      MAX=N+1

   10 CONTINUE
      MID=(MIN+MAX)/2
      IF(Y.LT.X(MID)) THEN
        MAX=MID
      ELSE
        MIN=MID
      END IF
      IF((MAX-MIN).GT.1) GO TO 10

      ISERCH=MIN

      RETURN
      END
C *********************************************************************
C THE END OF THE PROGRAM.
C *********************************************************************
