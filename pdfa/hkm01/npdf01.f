C *********************************************************************
C  pdfa_int.for  Version 1.0                            Mar. 19, 2001
C
C  [Package for the nuclear parton distribution functions]
C *********************************************************************
C  Nuclear Parton Distribution Functions.
C
C  This package contains 2 sets of parton distribution functions:
C    1) TYPE I  (Cubic type distributions),
C    2) TYPE II (Quadratic type distributions).
C
C  Available nuclei:
C    Proton, Deuteron, He, Li, Be, C, N, Al, Ca, Fe, Cu, Ag,
C    Sn, Xe, Au, Pb.
C    (Other nuclei are also available by preparing the grid files).
C
C  REFERENCE:
C    "Determination of nuclear parton distributions",
C     M. Hirai, S. Kumano, and M. Miyama,
C     hep-ph/0103208, SAGA-HE-170-01, TMU-NT-01-01,
C     Submitted for publication.
C
C  QUESTION OR COMMENT TO:
C    Masanori Hirai   [hirai@hsg.phys.saga-u.ac.jp]
C    Shunzo Kumano    [kumanos@cc.saga-u.ac.jp]
C    Masanori Miyama  [miyama@comp.metro-u.ac.jp]
C ---------------------------------------------------------------------
C INPUT PARAMETERS:
C
C    ISET  1 ... TYPE I  (Cubic type distributions)
C          2 ... TYPE II (Quadratic type distributions)
C
C    NUCL  1 ... Proton   (A=1, Z=1)
C          2 ... Deuteron (A=2, Z=1)
C          3 ... He (A=4, Z=2)
C          4 ... Li (A=7, Z=3)
C          5 ... Be (A=9, Z=4)
C          6 ... C  (A=12, Z=6)
C          7 ... N  (A=14, Z=7)
C          8 ... Al (A=27, Z=13)
C          9 ... Ca (A=40, Z=20)
C         10 ... Fe (A=56, Z=26)
C         11 ... Cu (A=63, Z=29)
C         12 ... Ag (A=107, Z=47)
C         13 ... Sn (A=118, Z=50)
C         14 ... Xe (A=131, Z=54)
C         15 ... Au (A=197, Z=79)
C         16 ... Pb (A=208, Z=82)
C         17 ... user1.grid (provided by a user for other nucleus)
C         18 ... user2.grid (provided by a user for other nucleus)
C         19 ... user3.grid (provided by a user for other nucleus)
C         20 ... user4.grid (provided by a user for other nucleus)
C         21 ... user5.grid (provided by a user for other nucleus)
C                Please read the following * NOTE for NUCL=17-21.
C
C    Q2 & X  ... Q^2 and x values at which the distributions are
C                calculated. Available range: 10^-9 <= X <= 1.0,
C                1.0 <= Q2 <= 100,000 GeV^2.
C
C * NOTE: The grid files are provided for the nuclei used in our
C         analysis (NUCL=1-16) in this package.
C         For other nucleus, a grid file can be obtained by
C         running "mgrid.for" together with "pdfa_evo.for".
C         It produces the output file "user.grid", which should
C         be renamed user1.grid, user2.grid, ..., or user5.grid.
C         In this way, the distributions in other nuclei can be
C         handled by this library with the input NUCL=17-21.
C ---------------------------------------------------------------------
C OUTPUT: PDF(I)
C
C      I = 1 ... u-valence quark (x u_v^A)
C          2 ... d-valence quark (x d_v^A)
C          3 ... q-bar (x q-bar^A)
C          4 ... gluon (x g^A)
C          5 ... structure function (F_2^A)
C
C   NOTE: The returned values are the distributions multiplied by x.
C *********************************************************************
      SUBROUTINE NPDF01(ISET, NUCL, Q2, X, PDF)
C ---------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NQ=23, NX=68, ND=4, IFILE=11)
      DIMENSION IREAD(3), PDF(5), QG(NQ), XG(NX),
     1          PDFG(NX,NQ,ND), PDFJ1(ND), PDFJ2(ND),
     2          BXG(NX,NQ,ND), CXG(NX,NQ,ND), DXG(NX,NQ,ND)
      CHARACTER*10 FNAME(21,2)
      SAVE IREAD, PDFG, BXG, CXG, DXG

      DATA IREAD / 1, 1, 1 /

C FILE NAME.
      DATA ((FNAME(NUCL,ISET), NUCL=1,21), ISET=1,2) /
     1     'P.grid',     'D_1.grid',   'He_1.grid',  'Li_1.grid',
     2     'Be_1.grid',  'C_1.grid',   'N_1.grid',   'Al_1.grid',
     3     'Ca_1.grid',  'Fe_1.grid',  'Cu_1.grid',  'Ag_1.grid',
     4     'Sn_1.grid',  'Xe_1.grid',  'Au_1.grid',  'Pb_1.grid',
     5     'user1.grid', 'user2.grid', 'user3.grid', 'user4.grid',
     6     'user5.grid',
     1     'P.grid',     'D_2.grid',   'He_2.grid',  'Li_2.grid',
     2     'Be_2.grid',  'C_2.grid',   'N_2.grid',   'Al_2.grid',
     3     'Ca_2.grid',  'Fe_2.grid',  'Cu_2.grid',  'Ag_2.grid',
     4     'Sn_2.grid',  'Xe_2.grid',  'Au_2.grid',  'Pb_2.grid',
     5     'user1.grid', 'user2.grid', 'user3.grid', 'user4.grid',
     6     'user5.grid'  /

C Q2 AND X GRID.
      DATA QG / 1.0D0, 1.3D0, 1.8D0, 2.7D0, 4.0D0, 6.4D0,
     +          1.0D1, 1.6D1, 2.5D1, 4.0D1, 6.4D1,
     +          1.0D2, 1.8D2, 3.2D2, 5.7D2,
     +          1.0D3, 1.8D3, 3.2D3, 5.7D3,
     +          1.0D4, 2.2D4, 4.6D4,
     +          1.0D5 /

      DATA XG / 1.0D-9, 1.8D-9, 3.2D-9, 5.7D-9,
     +          1.0D-8, 1.8D-8, 3.2D-8, 5.7D-8,
     +          1.0D-7, 1.8D-7, 3.2D-7, 5.7D-7,
     +          1.0D-6, 1.4D-6, 2.0D-6, 3.0D-6, 4.5D-6, 6.7D-6,
     +          1.0D-5, 1.4D-5, 2.0D-5, 3.0D-5, 4.5D-5, 6.7D-5,
     +          1.0D-4, 1.4D-4, 2.0D-4, 3.0D-4, 4.5D-4, 6.7D-4,
     +          1.0D-3, 1.4D-3, 2.0D-3, 3.0D-3, 4.5D-3, 6.7D-3,
     +          1.0D-2, 1.4D-2, 2.0D-2, 3.0D-2, 4.5D-2,
     +          0.06D0, 0.08D0, 0.1D0, 0.125D0, 0.15D0, 0.175D0,
     +          0.2D0, 0.225D0, 0.25D0, 0.275D0, 0.3D0, 0.325D0,
     +          0.35D0, 0.375D0, 0.4D0,  0.45D0, 0.5D0, 0.55D0,
     +          0.6D0, 0.65D0,  0.7D0,  0.75D0,  0.8D0, 0.85D0,
     +          0.9D0, 0.95D0, 1.0D0 /

C CHECK ISET AND NUCL VALUES.
      IF((ISET.LT.1).OR.(ISET.GT.2)) THEN
        WRITE(*,1010) ISET
 1010   FORMAT(' ','ERROR: ISET =', I3)
        STOP
      ELSE IF((NUCL.LT.1).OR.(NUCL.GT.21)) THEN
        WRITE(*,1020) NUCL
 1020   FORMAT(' ','ERROR: NUCL =', I3)
        STOP
      END IF

      IF((IREAD(1).NE.1).AND.(IREAD(2).EQ.ISET)
     1    .AND.(IREAD(3).EQ.NUCL)) GO TO 30

C READ GRID DATA AND CALCULATE SPLINE COEFFICIENTS.

      OPEN(UNIT=IFILE,FILE='hkm01/'//FNAME(NUCL,ISET),STATUS='OLD')

      DO 10 J=1,NQ
        DO 10 K=1,NX-1
          READ(IFILE,1030) (PDFG(K,J,I), I=1,4)
 1030       FORMAT(1X,4(1PE12.4))
   10 CONTINUE
      CLOSE(IFILE)
      DO 20 I=1,ND
        DO 20 J=1,NQ
          PDFG(NX,J,I)=0.D0
          CALL spline_hkm01(NX,XG,PDFG,BXG,CXG,DXG,I,J)
   20 CONTINUE

      IREAD(1)=2
      IREAD(2)=ISET
      IREAD(3)=NUCL

   30 CONTINUE

C CHECK X AND Q2 VALUES.
      IF((X.LT.1.D-9).OR.(X.GT.1.D0)) THEN
        WRITE(*,1040) X
 1040   FORMAT (' ','WARNING: OUT OF RANGE --> X =', 1PE12.3)
        X=1.D0
      ENDIF
      IF((Q2.LT.1.D0).OR.(Q2.GT.1.D5)) THEN
        WRITE(*,1050) Q2
 1050   FORMAT (' ','WARNING: OUT OF RANGE --> Q2 =', 1PE12.3)
        Q2=1.D0
      ENDIF

C INTERPOLATION.
C X: CUBIC SPLINE INTERPOLATION, LOG(Q2): LINEAR INTERPOLATION.
      J=ISERCH_NPDF(NQ,QG,Q2)
      IF(J.EQ.NQ) J=NQ-1
      K=ISERCH_NPDF(NX,XG,X)
      DO 40 I=1,ND
        DX=X-XG(K)
        PDFJ1(I)=PDFG(K,J,I)
     1       +DX*(BXG(K,J,I)+DX*(CXG(K,J,I)+DX*DXG(K,J,I)))
        PDFJ2(I)=PDFG(K,J+1,I)
     1       +DX*(BXG(K,J+1,I)+DX*(CXG(K,J+1,I)+DX*DXG(K,J+1,I)))
   40 CONTINUE
      T=(DLOG(Q2)-DLOG(QG(J)))/(DLOG(QG(J+1))-DLOG(QG(J)))
      DO 50 I=1,ND
        PDF(I)=(1.D0-T)*PDFJ1(I)+T*PDFJ2(I)
   50 CONTINUE
      PDF(5)=(4.D0*PDF(1)+PDF(2)+12.D0*PDF(3))/9.D0

      RETURN
      END
C ---------------------------------------------------------------------
      SUBROUTINE spline_hkm01(N,X,Y,B,C,D,I,J)
C ---------------------------------------------------------------------
C CALCULATE THE COEFFICIENTS B,C,D IN A CUBIC SPLINE INTERPOLATION.
C INTERPOLATION SUBROUTINES ARE TAKEN FROM
C G.E. FORSYTHE, M.A. MALCOLM AND C.B. MOLER,
C COMPUTER METHODS FOR MATHEMATICAL COMPUTATIONS (PRENTICE-HALL, 1977).
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NQ=23, NX=68, ND=4)
      DIMENSION X(NX), Y(NX,NQ,ND),
     1          B(NX,NQ,ND), C(NX,NQ,ND), D(NX,NQ,ND)
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
C      INTEGER FUNCTION ISERCH(N,X,Y)
C ---------------------------------------------------------------------
C THIS FUNCTION SEARCHES "I" WHICH SATISFIES THE RELATION
C X(I) <= Y < X(I+1) BY USING A BINARY SEARCH.
C      IMPLICIT REAL*8(A-H,O-Z)
C      DIMENSION X(70)
C
C      MIN=1
C      MAX=N+1
C
C   10 CONTINUE
C      MID=(MIN+MAX)/2
C      IF(Y.LT.X(MID)) THEN
C        MAX=MID
C      ELSE
C        MIN=MID
C      END IF
C      IF((MAX-MIN).GT.1) GO TO 10
C
C      ISERCH=MIN
C
C      RETURN
C      END
C *********************************************************************
C THE END OF THE PROGRAM.
C *********************************************************************
