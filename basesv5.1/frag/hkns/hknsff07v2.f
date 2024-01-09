C *********************************************************************
C  hknsff07.f  Version 1.0                                2006/FEB/27
C
C  modified by F. Arleo - initialization for ALL sets 2008/SEP/25
C
C  [Package for the HKNS fragmentation functions]
C  Reference:
C    Determination of fragmentation functions and their uncertainties
C    M. Hirai, S. Kumano, T.-H. Nagai, and K. Sudoh
C    hep-ph/0702250 (KEK-TH-1108), submitted for publication.
C *********************************************************************
C ---------------------------------------------------------------------
C  SUBROUTINE HKNSFF(Q2,X,ISET,ICHARGE,FF,GRAD):
C
C   Subroutine HKNSFF returns the values of fragmentaion functions
C   and their gradient terms at specified Q^2 and x point 
C   by interpolating the grid data.
C   [ Log(Q^2): LINEAR INTERPOLATION, x: CUBIC SPLINE INTERPOLATION ]
C
C   INPUT:
C     Q2, X ... Q^2 and x values at which the functions are calculated. 
C               Available range: 10^-2 <= X <= 1.0,
C                           1.0 GeV^2 <= Q^2 <= 10^8 GeV^2.
C     ISET=1: Pion LO Fragmentation functions and their gradient terms 
C          2: Pion NLO
C          3: Kaon LO
C          4: Kaon NLO
C          5: Proton LO
C          6: Proton NLO
C
C     ICHARGE=1: pi^+, K^+, or proton
C     ICHARGE=2: pi^-, K^-, or neutron
C     ICHARGE=3: pi^0=[pi^+ + pi^-]/2, [K^0+K^0b]/2, or [p+pb]/2
C        If you want to obtain the fragmentation functions for each
C        K0, K0b, pb, or nb, you may use the relations in Appendix of
C        hep-ph/0702250.
C
C   OUTPUT: Arrays FF(-5:5) & GRAD(I,J)
C
C     FF(I) --> HKNS fragmentation functions (FFs).
C      I = -5 ... b-bar quark (D_b-bar = D_b)
C          -4 ... c-bar quark (D_c-bar = D_c)
C          -3 ... s-bar quark
C          -2 ... d-bar quark 
C          -1 ... u-bar quark 
C           0 ... gluon D_g(x)
C           1 ... u quark 
C           2 ... d quark 
C           3 ... s quark 
C           4 ... c quark 
C           5 ... b quark 
C
C     GRAD(I,J) --> Gradient terms of HKSN FFs
C      I is the same index as the one in FF(I).
C      J indicates the parameter index for a gradient term dFF(I)/da_J
C      (a_J = parameter).
C
C Pion,J= 1..2: g (2ndM, alpha)           2ndM = second moment
C         3..5: u (2ndM, alpha, beta)
C         6..8: d (2ndM, alpha, beta)
C        9..11: c (2ndM, alpha, beta)
C       12..14: b (2ndM, alpha, beta)
C   For example, the above J=14 indicates d D_b^{pi^+}/d beta_b^{pi^+}.
C
C Kaon,J= 1..2: g (2ndM, beta)
C         3..5: u (2ndM, alpha, beta)
C         6..8: d (2ndM, alpha, beta)
C        9..11: sb(2ndM, alpha, beta)
C       12..14: c (2ndM, alpha, beta)
C       15..17: b (2ndM, alpha, beta)
C
CProton,J=1..2: g (2ndM, beta)
C         3..5: u (2nsM, alpha, beta)
C         6..8: qb(2ndM, alpha, beta)
C        9..11: c (2ndM, alpha, beta)
C       11..13: b (2ndM, alpha, beta)
C
C   NOTE: The returned values are not multiplied by x.
C
C      *  Error matrix can be used by declaring a common block:
C         COMMON/ERRM/EMI(17,17,6). This matrix is defined as
C         the inverse matrix of Hessian multiplied by Delta chi^2:
C         EM(i,j)=Delta chi^2*H_ij^-1.
C         The values of Delta chi^2 are as follows:
C         15.9359730(pion), 19.1977555(kaon), 14.8470228(proton).
C *********************************************************************
      SUBROUTINE HKNSFFV2(Q2,X,ISET,ICHARGE,FF,GRAD)
C ---------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*5 pref
      PARAMETER (NQ=33, NX=53, ND=146, NFF=8, NSET=6, IFILE=40)
      DIMENSION QG(NQ), XG(NX),PDFJ1(ND), PDFJ2(ND)
     +       ,FF(-5:5), GRADFF(8,17), GRAD(-5:5,17)
     +       ,BXG(NX,NQ,NSET,ND), CXG(NX,NQ,NSET,ND), DXG(NX,NQ,NSET,ND)
     +       ,PDFG(NX,NQ,NSET,ND), EMI(17,17,NSET),NPAR(NSET)

      COMMON/ERRM/EMI
      SAVE PDFG
      SAVE IREAD, NPAR, BXG, CXG, DXG
      DATA IREAD /1/ 
      DATA CTHRE,BTHRE/2.0449D0,18.490D0/
      DATA EPS/1.D-12/

      INCLUDE 'EM.inc'

C Q2 AND X GRID.
      DATA QG /
     +  1.000000D+00, 1.467799D+00, 2.154435D+00,
     +  3.162278D+00, 4.641589D+00, 6.812921D+00,
     +  1.000000D+01, 1.467799D+01, 2.154435D+01,
     +  3.162278D+01, 4.641589D+01, 6.812921D+01,
     +  1.000000D+02, 1.778279D+02, 3.162278D+02, 5.623413D+02,
     +  1.000000D+03, 1.778279D+03, 3.162278D+03, 5.623413D+03,
     +  1.000000D+04, 1.778279D+04, 3.162278D+04, 5.623413D+04,
     +  1.000000D+05, 1.778279D+05, 3.162278D+05, 5.623413D+05,
     +  1.000000D+06, 4.641589D+06, 
     +  1.000000D+07, 4.641589D+07,  
     +  1.000000D+08  /

      DATA XG / 
     +  1.000000D-02, 1.154782D-02, 1.333521D-02, 1.539927D-02,
     +  1.778279D-02, 2.053525D-02, 2.371374D-02, 2.738420D-02,
     +  3.162278D-02, 3.651741D-02, 4.216965D-02, 4.869675D-02,
     +  5.623413D-02, 6.493816D-02, 7.498942D-02, 8.659643D-02,
     +  1.000000D-1, 1.250000D-1, 1.500000D-1, 1.750000D-1,
     +  2.000000D-1, 2.250000D-1, 2.500000D-1, 2.750000D-1,
     +  3.000000D-1, 3.250000D-1, 3.500000D-1, 3.750000D-1,
     +  4.000000D-1, 4.250000D-1, 4.500000D-1, 4.750000D-1, 
     +  5.000000D-1, 5.250000D-1, 5.500000D-1, 5.750000D-1,
     +  6.000000D-1, 6.250000D-1, 6.500000D-1, 6.750000D-1,
     +  7.000000D-1, 7.250000D-1, 7.500000D-1, 7.750000D-1,
     +  8.000000D-1, 8.250000D-1, 8.500000D-1, 8.750000D-1,
     +  9.000000D-1, 9.250000D-1, 9.500000D-1, 9.750000D-1,
     +  1.000000D+0 /

c ----------------------
c OUT OF BOUNDARIES
      XSAVE=X
      Q2SAVE=Q2
      IF(X.LT.1.D-02)X=1.D-02
      IF(X.GT.1.D0)X=1.D0
      IF(Q2.LT.1.D0)Q2=1.D0
      IF(Q2.GT.1.D8)Q2=1.D8
c ----------------------

C CALCULATE SPLINE COEFFICIENTS.
      IF(IREAD.NE.1) GO TO 20
      CTHRE=CTHRE-EPS
      BTHRE=BTHRE-EPS

C READ GRID DATA AND CALCULATE SPLINE COEFFICIENTS. 
      pref='hkns/'

C READ ALL THE FILES THE FIRST TIME THE ROUTINE IS CALLED
      DO JSET=1,NSET
         IFILE2=IFILE+(JSET-1)*10
         IF(JSET.EQ.1) THEN
         OPEN(UNIT=IFILE2,FILE=pref//'hkns_pilo.grd',STATUS='OLD')
         OPEN(UNIT=IFILE2+1,FILE=pref//'gradpilo.grd',STATUS='OLD')
         NPAR(JSET)=14

         ELSE IF(JSET.EQ.2) THEN
         OPEN(UNIT=IFILE2,FILE=pref//'hkns_pinlo.grd',STATUS='OLD')
         OPEN(UNIT=IFILE2+1,FILE=pref//'gradpinlo.grd',STATUS='OLD')
         NPAR(JSET)=14
            
         ELSE IF(JSET.EQ.3) THEN
         OPEN(UNIT=IFILE2,FILE=pref//'hkns_klo.grd',STATUS='OLD')
         OPEN(UNIT=IFILE2+1,FILE=pref//'gradklo.grd',STATUS='OLD')
         NPAR(JSET)=17
            
         ELSE IF(JSET.EQ.4) THEN
         OPEN(UNIT=IFILE2,FILE=pref//'hkns_knlo.grd',STATUS='OLD')
         OPEN(UNIT=IFILE2+1,FILE=pref//'gradknlo.grd',STATUS='OLD')
         NPAR(JSET)=17

         ELSE IF(JSET.EQ.5) THEN
         OPEN(UNIT=IFILE2,FILE=pref//'hkns_plo.grd',STATUS='OLD')
         OPEN(UNIT=IFILE2+1,FILE=pref//'gradplo.grd',STATUS='OLD')
         NPAR(JSET)=13

         ELSE IF(JSET.EQ.6) THEN
         OPEN(UNIT=IFILE2,FILE=pref//'hkns_pnlo.grd',STATUS='OLD')
         OPEN(UNIT=IFILE2+1,FILE=pref//'gradpnlo.grd',STATUS='OLD')
         NPAR(JSET)=13

         ELSE
            WRITE(*,1010) JSET
 1010       FORMAT(' ','HKNSFF ERROR: ISET =', I3)
            STOP
         END IF
      ENDDO

      DO JSET=1,NSET
         IFILE2=IFILE+(JSET-1)*10
         DO J=1,NQ
            DO K=1,NX-1
               READ(IFILE2,1025) (PDFG(K,J,JSET,I), I=1,8)
               DO NR=1,NPAR(JSET)
                  NI=10+NFF*(NR-1)
                  READ(IFILE2+1,1025) (PDFG(K,J,JSET,I),I=NI,NI+NFF-1)
               ENDDO
            ENDDO
         ENDDO
 1025  FORMAT(1X,8(1PE14.6))
       CLOSE(IFILE2)
       CLOSE(IFILE2+1)
      ENDDO

      DO JSET=1,NSET
         DO I=1,ND
            DO J=1,NQ
               PDFG(NX,J,JSET,I)=0.D0 ! x=1 FF=0.D0
               CALL lsplinev2_hkns(NX,XG,PDFG,BXG,CXG,DXG,JSET,I,J)
            ENDDO
         ENDDO
      ENDDO

      IREAD=2
   20 CONTINUE

      DO I=1,11
        FF(I-6)=0.D0
      END DO
      DO J=1,17
        DO I=1,8
          GRADFF(I,J)=0.D0
        ENDDO
      ENDDO

C CHECK X AND Q2 VALUES.
      IF((ICHARGE.GT.4).OR.(ICHARGE.LT.1)) THEN
        WRITE(*,1026) ICHARGE
 1026   FORMAT(' ','HKNSFF ERROR: ICHARGE =', I3)
c        STOP
      ENDIF

      IF((X.LT.1.D-2).OR.(X.GT.1.D0)) THEN
        WRITE(*,1030) X
 1030   FORMAT (' ','FF WARNING: OUT OF RANGE --> X =', 1PE12.3)
c        STOP
      ENDIF
      IF((Q2.LT.1.D0).OR.(Q2.GT.1.D8)) THEN
        WRITE(*,1040) Q2
 1040   FORMAT (' ','FF WARNING: OUT OF RANGE --> Q2 =', 1PE12.3)
c        STOP
      ENDIF

C INTERPOLATION.
C X: CUBIC SPLINE INTERPOLATION, LOG(Q2): LINEAR INTERPOLATION.
      J=ISERCH(NQ,QG,Q2)
      IF(J.EQ.NQ) J=NQ-1
      K=ISERCH(NX,XG,X)
      DO I=1,ND
        DX=X-XG(K)
        PDFJ1(I)=PDFG(K,J,ISET,I)
     >    +DX*(BXG(K,J,ISET,I)+DX*(CXG(K,J,ISET,I)+DX*DXG(K,J,ISET,I)))
        PDFJ2(I)=PDFG(K,J+1,ISET,I)+DX*
     >   (BXG(K,J+1,ISET,I)+DX*(CXG(K,J+1,ISET,I)+DX*DXG(K,J+1,ISET,I)))
      ENDDO

C -- Fragmentation functions --
      T=(DLOG(Q2)-DLOG(QG(J)))/(DLOG(QG(J+1))-DLOG(QG(J)))
      DO I=1,3
        FF(I-1)=(1.D0-T)*PDFJ1(I)+T*PDFJ2(I)     ! g, u, d
        FF(-I)=(1.D0-T)*PDFJ1(I+3)+T*PDFJ2(I+3)  ! ub, [db, s]^pi+, [sb, s]^K; 
        FF(I+2)=(1.D0-T)*PDFJ1(I+5)+T*PDFJ2(I+5) ! s, c, b
      ENDDO
      IF(Q2.LT.CTHRE) FF(4)=0.D0
      IF(Q2.LT.BTHRE) FF(5)=0.D0
      FF(-4)=FF(4) ! cb=c
      FF(-5)=FF(5) ! bb=b

      IF((ISET.EQ.3).or.(ISET.EQ.4)) THEN  ! K+ 
        FF(-3)=FF(-2) ! FF(-2)=sb -> FF(-3)
        FF(-2)=FF(2)  ! db=d
      ENDIF

      IF(ICHARGE.EQ.2) THEN ! Negative charge
        IF(ISET.LT.3) THEN  ! pi:u<->ub, d<->db
          TEMP1=FF(-1);TEMP2=FF(2)
          FF(-1)=FF(1);FF(2)=FF(-2)
          FF(1)=TEMP1;FF(-2)=TEMP2

        ELSE IF((ISET.EQ.3).OR.(ISET.EQ.4)) THEN ! K:u<->ub, s<->sb
          TEMP1=FF(-1);TEMP2=FF(3)
          FF(-1)=FF(1);FF(3)=FF(-3)
          FF(1)=TEMP1;FF(-3)=TEMP2

        ELSE IF((ISET.EQ.4).OR.(ISET.EQ.5)) THEN ! neutron, u<->d, ub<->db
          TEMP1=FF(-1);TEMP2=FF(1)
          FF(-1)=FF(-2);FF(1)=FF(2)
          FF(-2)=TEMP1;FF(2)=TEMP2
        ENDIF

      ELSE IF(ICHARGE.EQ.3) THEN ! (pi^+ + pi^-)/2, (K^0 + K^0b)/2, (p+pb)/2
        IF((ISET.EQ.3).OR.(ISET.EQ.4)) THEN ! K^0: u<->d, ub<->db
          TEMP1=FF(-1);TEMP2=FF(1)
          FF(-1)=FF(-2);FF(1)=FF(2)
          FF(-2)=TEMP1;FF(2)=TEMP2
        ENDIF
        DO I=1,5 ! (pi^+ + pi^-)/2, (K^0 + K^0b)/2, (p+pb)/2
          FF(I)=(FF(I)+FF(-I))/2.D0
          FF(-I)=FF(I)
        ENDDO
      ENDIF

C -- Gradient terms of parameters for fragmentation functions --
      DO J=1,NPAR(ISET)
        DO I=1,NFF
          NI=9+NFF*(J-1)
          GRADFF(I,J)=(1.D0-T)*PDFJ1(NI+I)+T*PDFJ2(NI+I)
        ENDDO
      ENDDO

      IF(Q2.LT.CTHRE) THEN
        DO J=1,NPAR(ISET)
          GRADFF(NFF-1,J)=0.D0
          IF(J.GT.NPAR(ISET)-6) THEN
            DO I=1,NFF-2
              GRADFF(I,J)=0.D0
            ENDDO
          ENDIF
        ENDDO
      ENDIF 

      IF(Q2.LT.BTHRE) THEN
        DO J=1,NPAR(ISET)
          GRADFF(NFF,J)=0.D0
          IF(J.GT.NPAR(ISET)-3) THEN
            DO I=1,NFF-1
              GRADFF(I,J)=0.D0
            ENDDO
          ENDIF
        ENDDO
      ENDIF

      DO J=1,NPAR(ISET)
        DO I=1,3
          GRAD(I-1,J)=GRADFF(I,J)   ! g, u, d
          GRAD(-I,J)=GRADFF(I+3,J)  ! ub, [db, s]^pi+, [sb, s]^K+
          GRAD(I+2,J)=GRADFF(I+5,J) ! s, c, b
        ENDDO
        GRAD(-4,J)=GRAD(4,J) ! cb=c      
        GRAD(-5,J)=GRAD(5,J) ! bb=b     

        IF((ISET.EQ.3).OR.(ISET.EQ.4)) THEN ! K+
          GRAD(-3,J)=GRAD(-2,J)
          GRAD(-2,J)=GRAD(2,J)
        ENDIF
      ENDDO

      DO J=1,NPAR(ISET)
        IF(ICHARGE.EQ.2) THEN
          IF(ISET.LT.3) THEN
            TEMP1=GRAD(-1,J);TEMP2=GRAD(2,J)
            GRAD(-1,J)=GRAD(1,J);GRAD(2,J)=GRAD(-2,J)
            GRAD(1,J)=TEMP1;GRAD(-2,J)=TEMP2

          ELSE IF((ISET.EQ.3).OR.(ISET.EQ.4)) THEN
            TEMP1=GRAD(-1,J);TEMP2=GRAD(3,J)
            GRAD(-1,J)=GRAD(1,J);GRAD(3,J)=GRAD(-3,J)
            GRAD(1,J)=TEMP1;GRAD(-3,J)=TEMP2

          ELSE IF(ISET.GT.4) THEN
            TEMP1=GRAD(-1,J);TEMP2=GRAD(1,J)
            GRAD(-1,J)=GRAD(-2,J);GRAD(1,J)=GRAD(2,J)
            GRAD(-2,J)=TEMP1;GRAD(2,J)=TEMP2
          ENDIF

        ELSE IF(ICHARGE.EQ.3) THEN
          IF((ISET.EQ.3).OR.(ISET.EQ.4)) THEN
            TEMP1=GRAD(-1,J);TEMP2=GRAD(1,J)
            GRAD(-1,J)=GRAD(-2,J);GRAD(1,J)=GRAD(2,J)
            GRAD(-2,J)=TEMP1;GRAD(2,J)=TEMP2
          ENDIF
          DO I=1,5
            GRAD(I,J)=(GRAD(I,J)+GRAD(-I,J))*0.5D0
            GRAD(-I,J)=GRAD(I,J)
          ENDDO
        ENDIF
      ENDDO

      X=XSAVE
      Q2=Q2SAVE

      RETURN
      END
C ---------------------------------------------------------------------
      SUBROUTINE lsplinev2_hkns(N,X,Y,B,C,D,JSET,I,J)
C ---------------------------------------------------------------------
C CALCULATE THE COEFFICIENTS B,C,D IN A CUBIC SPLINE INTERPOLATION.
C INTERPOLATION SUBROUTINES ARE TAKEN FROM
C G.E. FORSYTHE, M.A. MALCOLM AND C.B. MOLER,
C COMPUTER METHODS FOR MATHEMATICAL COMPUTATIONS (PRENTICE-HALL, 1977).
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NQ=33, NX=53, ND=146, NSET=6)
      DIMENSION Y(NX,NQ,NSET,ND),B(NX,NQ,NSET,ND),C(NX,NQ,NSET,ND),
     1      D(NX,NQ,NSET,ND),X(NX) 
      NM1=N-1
      IF(N.LT.2) RETURN
      IF(N.LT.3) GO TO 250
      D(1,J,JSET,I)=X(2)-X(1)
      C(2,J,JSET,I)=(Y(2,J,JSET,I)-Y(1,J,JSET,I))/D(1,J,JSET,I)
      DO 210 K=2,NM1
        D(K,J,JSET,I)=X(K+1)-X(K)
        B(K,J,JSET,I)=2.0D0*(D(K-1,J,JSET,I)+D(K,J,JSET,I))
        C(K+1,J,JSET,I)=(Y(K+1,J,JSET,I)-Y(K,J,JSET,I))/D(K,J,JSET,I)
        C(K,J,JSET,I)=C(K+1,J,JSET,I)-C(K,J,JSET,I)
  210 CONTINUE
      B(1,J,JSET,I)=-D(1,J,JSET,I)
      B(N,J,JSET,I)=-D(N-1,J,JSET,I)
      C(1,J,JSET,I)=0.0D0
      C(N,J,JSET,I)=0.0D0
      IF(N.EQ.3) GO TO 215
      C(1,J,JSET,I)=C(3,J,JSET,I)/(X(4)-X(2))-C(2,J,JSET,I)/(X(3)-X(1))
      C(N,J,JSET,I)=C(N-1,J,JSET,I)/(X(N)-X(N-2))
     #                              -C(N-2,J,JSET,I)/(X(N-1)-X(N-3))
      C(1,J,JSET,I)=C(1,J,JSET,I)*D(1,J,JSET,I)**2.0D0/(X(4)-X(1))
      C(N,J,JSET,I)=-C(N,J,JSET,I)*D(N-1,J,JSET,I)**2.0D0/(X(N)-X(N-3))
  215 CONTINUE
      DO 220 K=2,N
        T=D(K-1,J,JSET,I)/B(K-1,J,JSET,I)
        B(K,J,JSET,I)=B(K,J,JSET,I)-T*D(K-1,J,JSET,I)
        C(K,J,JSET,I)=C(K,J,JSET,I)-T*C(K-1,J,JSET,I)
  220 CONTINUE
      C(N,J,JSET,I)=C(N,J,JSET,I)/B(N,J,JSET,I)
      DO 230 IB=1,NM1
        K=N-IB
        C(K,J,JSET,I)=(C(K,J,JSET,I)
     #                -D(K,J,JSET,I)*C(K+1,J,JSET,I))/B(K,J,JSET,I)
  230 CONTINUE
      B(N,J,JSET,I)=(Y(N,J,JSET,I)-Y(NM1,J,JSET,I))/D(NM1,J,JSET,I)
     1        +D(NM1,J,JSET,I)*(C(NM1,J,JSET,I)+2.0D0*C(N,J,JSET,I))
      DO 240 K=1,NM1
        B(K,J,JSET,I)=(Y(K+1,J,JSET,I)-Y(K,J,JSET,I))/D(K,J,JSET,I)
     1          -D(K,J,JSET,I)*(C(K+1,J,JSET,I)+2.0D0*C(K,J,JSET,I))
        D(K,J,JSET,I)=(C(K+1,J,JSET,I)-C(K,J,JSET,I))/D(K,J,JSET,I)
        C(K,J,JSET,I)=3.0D0*C(K,J,JSET,I)
  240 CONTINUE
      C(N,J,JSET,I)=3.0D0*C(N,J,JSET,I)
      D(N,J,JSET,I)=D(N-1,J,JSET,I)
      RETURN
  250 CONTINUE
      B(1,J,JSET,I)=(Y(2,J,JSET,I)-Y(1,J,JSET,I))/(X(2)-X(1))
      C(1,J,JSET,I)=0.0D0
      D(1,J,JSET,I)=0.0D0
      B(2,J,JSET,I)=B(1,J,JSET,I)
      C(2,J,JSET,I)=0.0D0
      D(2,J,JSET,I)=0.0D0
      RETURN
      END
c$$$C ---------------------------------------------------------------------
c$$$      INTEGER FUNCTION ISERCH(N,X,Y)
c$$$C ---------------------------------------------------------------------
c$$$C THIS FUNCTION SEARCHES "I" WHICH SATISFIES THE RELATION
c$$$C X(I) <= Y < X(I+1) BY USING A BINARY SEARCH.
c$$$      IMPLICIT REAL*8(A-H,O-Z)
c$$$      DIMENSION X(53)
c$$$
c$$$      MIN=1
c$$$      MAX=N+1
c$$$
c$$$   10 CONTINUE
c$$$      MID=(MIN+MAX)/2
c$$$      IF(Y.LT.X(MID)) THEN
c$$$        MAX=MID
c$$$      ELSE
c$$$        MIN=MID
c$$$      END IF
c$$$      IF((MAX-MIN).GT.1) GO TO 10
c$$$
c$$$      ISERCH=MIN
c$$$
c$$$      RETURN
c$$$      END
C *********************************************************************
C THE END OF THE PROGRAM.
C *********************************************************************
