C *********************************************************************
C  npdf04.f    Version 1.3                               Apr. 7, 2004
C
C *********************************************************************
C  Nuclear Parton Distribution Functions.
C
C  REFERENCE:
C    "Nuclear parton distribution functions and their uncertainties",
C     M. Hirai, S. Kumano, and T.-H. Nagai,
C     hep-ph/0404093, SAGA-HE-197-04,
C     Submitted for publication.
C
C  QUESTION OR COMMENT TO:
C    Masanori Hirai   [mhirai@rarfaxp.riken.jp]
C    Shunzo Kumano    [kumanos@cc.saga-u.ac.jp]
C    Takahiro Nagai   [03sm27@edu.cc.saga-u.ac.jp]
C ---------------------------------------------------------------------
C INPUT PARAMETERS:
C
C    ISET  1 ... Proton (A=1, Z=1)
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
C         12 ... Kr (A=84, Z=36)
C         13 ... Ag (A=107, Z=47)
C         14 ... Sn (A=118, Z=50)
C         15 ... Xe (A=131, Z=54)
C         16 ... W  (A=184, Z=74)
C         17 ... Au (A=197, Z=79)
C         18 ... Pb (A=208, Z=82)
C
C     Q2, X   ... Q^2 and x values at which the distributions are
C                 calculated. Available range: 10^-9 <= X <= 1.0,
C                 1.0 GeV^2 <= Q2 <= 10^8 GeV^2.
C
C   OUTPUT: Arrays PDFN(-4:4)
C
C     PDFN(I) --> Nuclear Parton Distribution Functions.
C      I = -4 ... c-bar quark (x c-bar = x c)
C          -3 ... s-bar quark (x s-bar = x s)
C          -2 ... d-bar quark (x d-bar = x d_sea)
C          -1 ... u-bar quark (x u-bar = x u_sea)
C           0 ... gluon (x g)
C           1 ... u quark (x u)
C           2 ... d quark (x d)
C           3 ... s quark (x s = x s-bar)
C           4 ... c quark (x c = x c-bar)
C
C   NOTE: The returned values are the distributions multiplied by x.
C *********************************************************************
      SUBROUTINE NPDF04(ISET,Q2,X,PDFN)
C ---------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NQ=33, NX=117, ND=7, NSET=3, IFILE=11)
      DIMENSION BXG(NX,NQ,ND), CXG(NX,NQ,ND), DXG(NX,NQ,ND)
     +         ,PDFG(NX,NQ,ND), PDFN(-4:4)
     +         ,QG(NQ), XG(NX),PDFJ1(ND), PDFJ2(ND)
     +         ,IREAD(2)
       CHARACTER*11 FNAME(18)
      SAVE IREAD, BXG, CXG, DXG

      DATA IREAD / 1, 1 /

C FILE NAME.
      DATA (FNAME(ISET), ISET=1,18) /
     +     'npdfp.grd',  'npdfD.grd',   'npdf4He.grd', 'npdfLi.grd',
     +     'npdfBe.grd', 'npdfC.grd',   'npdfN.grd',   'npdfAl.grd',
     +     'npdfCa.grd', 'npdfFe.grd',  'npdfCu.grd',  'npdfKr.grd',
     +     'npdfAg.grd', 'npdfSn.grd',  'npdfXe.grd',  'npdfW.grd', 
     +     'npdfAu.grd', 'npdfPb.grd'/ 

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
     +  1.000000D-09, 1.333521D-09, 1.778279D-09, 2.371374D-09,
     +  3.162278D-09, 4.216965D-09, 5.623413D-09, 7.498942D-09,
     +  1.000000D-08, 1.333521D-08, 1.778279D-08, 2.371374D-08,
     +  3.162278D-08, 4.216965D-08, 5.623413D-08, 7.498942D-08,
     +  1.000000D-07, 1.333521D-07, 1.778279D-07, 2.371374D-07,
     +  3.162278D-07, 4.216965D-07, 5.623413D-07, 7.498942D-07,
     +  1.000000D-06, 1.333521D-06, 1.778279D-06, 2.371374D-06,
     +  3.162278D-06, 4.216965D-06, 5.623413D-06, 7.498942D-06,
     +  1.000000D-05, 1.333521D-05, 1.778279D-05, 2.371374D-05,
     +  3.162278D-05, 4.216965D-05, 5.623413D-05, 7.498942D-05,
     +  1.000000D-04, 1.333521D-04, 1.778279D-04, 2.371374D-04,
     +  3.162278D-04, 4.216965D-04, 5.623413D-04, 7.498942D-04,
     +  1.000000D-03, 1.154782D-03, 1.333521D-03, 1.539927D-03,
     +  1.778279D-03, 2.053525D-03, 2.371374D-03, 2.738420D-03,
     +  3.162278D-03, 3.651741D-03, 4.216965D-03, 4.869675D-03,
     +  5.623413D-03, 6.493816D-03, 7.498942D-03, 8.659643D-03,
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


C CALCULATE SPLINE COEFFICIENTS.
      IF((ISET.LT.1).OR.(ISET.GT.18)) THEN
        WRITE(*,1010) ISET
 1010   FORMAT(' ','ERROR: ISET =', I3)
        STOP
      END IF
      IF((IREAD(1).NE.1).AND.(IREAD(2).EQ.ISET)) GO TO 20

C READ GRID DATA AND CALCULATE SPLINE COEFFICIENTS. 
      OPEN(UNIT=IFILE,FILE='hkn04/'//FNAME(ISET),STATUS='OLD')

      DO J=1,NQ
        DO K=1,NX-1
          READ(IFILE,1020) (PDFG(K,J,I), I=1,ND)
 1020       FORMAT(1X,7(1PE14.6))
        ENDDO
      ENDDO
      CLOSE(IFILE)

      DO I=1,ND
        DO J=1,NQ
          PDFG(NX,J,I)=0.D0
          CALL spline_hkn04(NX,XG,PDFG,BXG,CXG,DXG,ISET,I,J)
        ENDDO
      ENDDO

      IREAD(1)=2
      IREAD(2)=ISET

   20 CONTINUE

C CHECK X AND Q2 VALUES.
      IF((X.LT.1.D-9).OR.(X.GT.1.D0)) THEN
        WRITE(*,1030) X
 1030   FORMAT (' ','NPDF04 WARNING: OUT OF RANGE --> X =', 1PE12.3)
        STOP
      ENDIF
      IF((Q2.LT.1.D0).OR.(Q2.GT.1.D8)) THEN
        WRITE(*,1040) Q2
 1040   FORMAT (' ','NPDF04 WARNING: OUT OF RANGE --> Q2 =', 1PE12.3)
        STOP
      ENDIF

C INTERPOLATION.
C X: CUBIC SPLINE INTERPOLATION, LOG(Q2): LINEAR INTERPOLATION.
      J=ISERCH_NPDF(NQ,QG,Q2)
      IF(J.EQ.NQ) J=NQ-1
      K=ISERCH_NPDF(NX,XG,X)
      DO I=1,ND
        DX=X-XG(K)
        PDFJ1(I)=PDFG(K,J,I)
     1       +DX*(BXG(K,J,I)+DX*(CXG(K,J,I)+DX*DXG(K,J,I)))
        PDFJ2(I)=PDFG(K,J+1,I)
     1       +DX*(BXG(K,J+1,I)+DX*(CXG(K,J+1,I)+DX*DXG(K,J+1,I)))
      ENDDO

      T=(DLOG(Q2)-DLOG(QG(J)))/(DLOG(QG(J+1))-DLOG(QG(J)))
      DO I=1,5
        PDFN(I-1)=(1.D0-T)*PDFJ1(I)+T*PDFJ2(I)
      ENDDO
	DO I=1,2
        PDFN(-I)=(1.D0-T)*PDFJ1(I+5)+T*PDFJ2(I+5)
	ENDDO
      PDFN(-3)=PDFN(3)
      PDFN(-4)=PDFN(4)

      RETURN
      END
C ---------------------------------------------------------------------
      SUBROUTINE spline_hkn04(N,X,Y,B,C,D,ISET,I,J)
C ---------------------------------------------------------------------
C CALCULATE THE COEFFICIENTS B,C,D IN A CUBIC SPLINE INTERPOLATION.
C INTERPOLATION SUBROUTINES ARE TAKEN FROM
C G.E. FORSYTHE, M.A. MALCOLM AND C.B. MOLER,
C COMPUTER METHODS FOR MATHEMATICAL COMPUTATIONS (PRENTICE-HALL, 1977).
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NQ=33, NX=117, ND=7)
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
      INTEGER FUNCTION ISERCH_NPDF(N,X,Y)
C ---------------------------------------------------------------------
C THIS FUNCTION SEARCHES "I" WHICH SATISFIES THE RELATION
C X(I) <= Y < X(I+1) BY USING A BINARY SEARCH.
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(118)

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

      ISERCH_NPDF=MIN

      RETURN
      END
C *********************************************************************
C THE END OF THE PROGRAM.
C *********************************************************************

