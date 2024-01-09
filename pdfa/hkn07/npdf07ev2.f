C *********************************************************************
C  npdf07e.f  Version 1.0                              2007/Oct/05
C
C  [Package for the HKN nuclear PDF]
C *********************************************************************
C  Nuclear Parton Distribution Functions (2007).
C
C  REFERENCE:
C    Determination of nuclear parton distribution functions
C    and their uncertainties in next-to-leading order,
C    M. Hirai, S. Kumano, and T.-H. Nagai,
C    arXiv:0709.3038 [hep-ph],
C    http://arxiv.org/abs/0709.3038,
C    submitted for publication.
C
C  QUESTION OR COMMENT TO:
C    Masanori Hirai   [mhirai@sakura.juntendo.ac.jp]
C    Shunzo Kumano    [shunzo.kumano@kek.jp]
C    Takahiro Nagai   [tnagai@post.kek.jp]
C ---------------------------------------------------------------------
C  SUBROUTINE HKN07(Q2,X,ISET,IORDER,DNPDF,GRAD):
C
C   Subroutine HKN07 returns the values of nuclear PDFs
C   and gradient terms of their functions at specified Q^2 and x point 
C   by interpolating the grid data.
C   [ Log(Q^2): LINEAR INTERPOLATION, x: CUBIC SPLINE INTERPOLATION ]
C
C   INPUT:
C     Q2, X ... Q^2 and x values at which the functions are calculated. 
C               Available range: 10^-9 <= X <= 1.0,
C                           1.0 GeV^2 <= Q^2 <= 10^8 GeV^2.

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
C         19 ... O  (A=16, Z=8)
C
C     IORDER=1: LO
C     IORDER=2: NLO
C
C   OUTPUT: Arrays DNPFD(-4:4) & GRAD(I,J)
C
C     DNPDF(I) --> HKNS FFs.
C      I = -4 ... c-bar quark (c-bar = c)
C          -3 ... s-bar quark
C          -2 ... d-bar quark 
C          -1 ... u-bar quark 
C           0 ... gluon 
C           1 ... u quark 
C           2 ... d quark 
C           3 ... s quark 
C           4 ... c quark 
C
C     GRAD(I,J) --> Gradient terms of HKN NPDFs
C      I is the same index as the DNPDF(I)
C      J indicates the parameter index for gradient terms dDNPDF(I)/da_J
C
C      J= 1..3: g (x_0^+, d^{(1)}, d^{(2)})
C         4..7: valence (x_0^+, x_0^-, d^{(1)}, d^{(2)})
C         8..12: anti-quark (a^{(1)}, a^{(2)}, x_0^+, x_0^-, d^{(2)})
C
C
C   NOTE: The returned values are not multiplied by z.
C
C      *  Error matrix can be used by declaring a common block:
C         COMMON/ERRM/EM(12,12,19). This matrix is defined as multiplying
C         Delta chi^2 by inverse matrix of Hessian: 
C         EM(i,j)=Delta chi^2*H_ij^-1
C
C *********************************************************************
      SUBROUTINE HKNNPDFV2(Q2,X,ISET,IORDER,DNPDF,GRAD)
C ---------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NQ=33, NX=117, ND=94, NFF=7, IFILE=20,NPAR=12,NSET=19)
      DIMENSION IREAD(2), QG(NQ), XG(NX),PDFJ1(ND), PDFJ2(ND)
     +         ,DNPDF(-4:4), GRADFF(NFF,12), GRAD(-4:4,12)
     +         ,BXG(NX,NQ,NSET,ND), CXG(NX,NQ,NSET,ND)
     +         ,DXG(NX,NQ,NSET,ND),PDFG(NX,NQ,NSET,ND)
     +         ,EMI(12,12,2), EM(12,12,NSET)

      CHARACTER*26 FNPDF_LO(19),FNPDF_NLO(19),FGRAD_LO(19),
     +                                       FGRAD_NLO(19)
      COMMON/ERRM/EM
      SAVE IREAD, BXG, CXG, DXG, PDFG
      DATA IREAD / 1, 1 / 

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


      DATA (FNPDF_LO(ISET), ISET=1,19) /
     +  'npdfp_LO.grd',  'npdfD_LO.grd',  'npdf4He_LO.grd',
     +  'npdfLi_LO.grd', 'npdfBe_LO.grd', 'npdfC_LO.grd', 
     +  'npdfN_LO.grd',  'npdfAl_LO.grd', 'npdfCa_LO.grd', 
     +  'npdfFe_LO.grd', 'npdfCu_LO.grd', 'npdfKr_LO.grd',
     +  'npdfAg_LO.grd', 'npdfSn_LO.grd', 'npdfXe_LO.grd',  
     +  'npdfW_LO.grd',  'npdfAu_LO.grd', 'npdfPb_LO.grd',
     +  'npdfO_LO.grd'/

      DATA (FNPDF_NLO(ISET), ISET=1,19) /
     +  'npdfp_NLO.grd',  'npdfD_NLO.grd',  'npdf4He_NLO.grd',
     +  'npdfLi_NLO.grd', 'npdfBe_NLO.grd', 'npdfC_NLO.grd',
     +  'npdfN_NLO.grd',  'npdfAl_NLO.grd', 'npdfCa_NLO.grd',
     +  'npdfFe_NLO.grd', 'npdfCu_NLO.grd', 'npdfKr_NLO.grd',
     +  'npdfAg_NLO.grd', 'npdfSn_NLO.grd', 'npdfXe_NLO.grd',
     +  'npdfW_NLO.grd',  'npdfAu_NLO.grd', 'npdfPb_NLO.grd',
     +  'npdfO_NLO.grd'/

      DATA (FGRAD_LO(ISET), ISET=1,19) /
     +  'gradp_LO.grd',  'gradD_LO.grd',  'grad4He_LO.grd',
     +  'gradLi_LO.grd', 'gradBe_LO.grd', 'gradC_LO.grd',
     +  'gradN_LO.grd',  'gradAl_LO.grd', 'gradCa_LO.grd',
     +  'gradFe_LO.grd', 'gradCu_LO.grd', 'gradKr_LO.grd',
     +  'gradAg_LO.grd', 'gradSn_LO.grd', 'gradXe_LO.grd',
     +  'gradW_LO.grd',  'gradAu_LO.grd', 'gradPb_LO.grd',
     +  'gradO_LO.grd'/

      DATA (FGRAD_NLO(ISET), ISET=1,19) /
     +  'gradp_NLO.grd',  'gradD_NLO.grd' , 'grad4He_NLO.grd',
     +  'gradLi_NLO.grd', 'gradBe_NLO.grd', 'gradC_NLO.grd',
     +  'gradN_NLO.grd',  'gradAl_NLO.grd', 'gradCa_NLO.grd',
     +  'gradFe_NLO.grd', 'gradCu_NLO.grd', 'gradKr_NLO.grd',
     +  'gradAg_NLO.grd', 'gradSn_NLO.grd', 'gradXe_NLO.grd',
     +  'gradW_NLO.grd',  'gradAu_NLO.grd', 'gradPb_NLO.grd',
     +  'gradO_NLO.grd'/

      IF((ISET.LT.1).OR.(ISET.GT.19)) THEN
        WRITE(*,1010) ISET
 1010      FORMAT(' ','ERROR: ISET =', I3)
        STOP
      END IF

      IF((IORDER.LT.1).OR.(IORDER.GT.2)) THEN
        WRITE(*,1011) IORDER
 1011      FORMAT(' ','ERROR: IORDER =', I3)
        STOP
      END IF

C CALCULATE SPLINE COEFFICIENTS.
C       IF((IREAD(1).NE.1).AND.(IREAD(2).EQ.ISET)) GO TO 20
      IF(IREAD(1).NE.1) GO TO 20
C READ GRID DATA AND CALCULATE SPLINE COEFFICIENTS. 

C READ ALL THE FILES THE FIRST TIME THE ROUTINE IS CALLED
C START OF THE LOOPS ON ALL NUCLEI INDEXED BY JSET
      DO JSET=1,NSET

        IFILE2=IFILE+(JSET-1)*2
        DO I=1,12
          DO J=1,12
            EM(I,J,JSET)=EMI(I,J,IORDER)
          ENDDO
        ENDDO

        IF(IORDER.EQ.1)THEN 
            OPEN(UNIT=IFILE2,FILE='hkn07/'//FNPDF_LO(JSET),
     *STATUS='OLD')
            OPEN(UNIT=IFILE2+1,FILE='hkn07/'//FGRAD_LO(JSET),
     *STATUS='OLD')
        ELSE IF(IORDER.EQ.2)THEN
            OPEN(UNIT=IFILE2,FILE='hkn07/'//FNPDF_NLO(JSET),
     *STATUS='OLD')
            OPEN(UNIT=IFILE2+1,FILE='hkn07/'//FGRAD_NLO(JSET),
     *STATUS='OLD')
        END IF

      ENDDO


      DO JSET=1,NSET
        IFILE2=IFILE+(JSET-1)*2
        DO J=1,NQ
            DO K=1,NX-1
              READ(IFILE2,1025) (PDFG(K,J,JSET,I), I=1,NFF)
              DO NR=1,NPAR
                NI=10+NFF*(NR-1)
                READ(IFILE2+1,1025) (PDFG(K,J,JSET,I),I=NI,NI+NFF-1)
              ENDDO
            ENDDO
          ENDDO

 1025   FORMAT(1X,7(1PE14.6))
        CLOSE(IFILE2)
        CLOSE(IFILE2+1)
      ENDDO

      DO JSET=1,NSET
        DO I=1,ND
            DO J=1,NQ
              PDFG(NX,J,JSET,I)=0.D0 ! x=1 NPDF=0.D0
              CALL LSPLINEV3(NX,XG,PDFG,BXG,CXG,DXG,JSET,I,J)
            ENDDO
          ENDDO
      ENDDO

      IREAD(1)=2
C      IREAD(2)=ISET

   20 CONTINUE
      DO I=-4,4
        DNPDF(I)=0.D0
      END DO
      DO J=1,12
        DO I=1,NFF
          GRADFF(I,J)=0.D0
        ENDDO
      ENDDO

C CHECK X AND Q2 VALUES.
      IF((X.LT.1.D-9).OR.(X.GT.1.D0)) THEN
        WRITE(*,1030) X
 1030   FORMAT (' ','FF WARNING: OUT OF RANGE --> X =', 1PE12.3)
        STOP
      ENDIF
      IF((Q2.LT.1.D0).OR.(Q2.GT.1.D8)) THEN
        WRITE(*,1040) Q2
 1040   FORMAT (' ','FF WARNING: OUT OF RANGE --> Q2 =', 1PE12.3)
        STOP
      ENDIF

C INTERPOLATION.
C X: CUBIC SPLINE INTERPOLATION, LOG(Q2): LINEAR INTERPOLATION.
      J=ISERCH_NPDF(NQ,QG,Q2)
      IF(J.EQ.NQ) J=NQ-1
      K=ISERCH_NPDF(NX,XG,X)
      DO I=1,ND
        DX=X-XG(K)
        PDFJ1(I)=PDFG(K,J,ISET,I)
     >    +DX*(BXG(K,J,ISET,I)+DX*(CXG(K,J,ISET,I)+DX*DXG(K,J,ISET,I)))
        PDFJ2(I)=PDFG(K,J+1,ISET,I)+DX*
     >   (BXG(K,J+1,ISET,I)+DX*(CXG(K,J+1,ISET,I)+DX*DXG(K,J+1,ISET,I)))
      ENDDO

C -- Nuclear PDF functions --
      T=(DLOG(Q2)-DLOG(QG(J)))/(DLOG(QG(J+1))-DLOG(QG(J)))
      DO I=1,3
        DNPDF(I-1)=(1.D0-T)*PDFJ1(I)+T*PDFJ2(I)     ! g, u, d
        DNPDF(-I)=(1.D0-T)*PDFJ1(I+3)+T*PDFJ2(I+3)  ! ub, db,sb
       ENDDO
        DNPDF(4)=(1.D0-T)*PDFJ1(7)+T*PDFJ2(7) !  c
      DNPDF(3)=DNPDF(-3) ! s=sb
      DNPDF(-4)=DNPDF(4) ! cb=c

C -- Gradient terms of each parameters for Nuclear PDF --
      DO J=1,NPAR
        DO I=1,NFF
          NI=9+NFF*(J-1)
          GRADFF(I,J)=(1.D0-T)*PDFJ1(NI+I)+T*PDFJ2(NI+I)
        ENDDO
      ENDDO

      DO J=1,NPAR
        DO I=1,3
          GRAD(I-1,J)=GRADFF(I,J)   ! g, u, d
          GRAD(-I,J)=GRADFF(I+3,J)  ! ub, db, sb
        ENDDO
          GRAD(4,J)=GRADFF(7,J)
        GRAD(3,J)=GRAD(-3,J) ! s=sb 
        GRAD(-4,J)=GRAD(4,J) ! cb=c      

      ENDDO

      RETURN
      END
C ---------------------------------------------------------------------
      SUBROUTINE LSPLINEV3(N,X,Y,B,C,D,JSET,I,J)
C C ---------------------------------------------------------------------
C C CALCULATE THE COEFFICIENTS B,C,D IN A CUBIC SPLINE INTERPOLATION.
C C INTERPOLATION SUBROUTINES ARE TAKEN FROM
C C G.E. FORSYTHE, M.A. MALCOLM AND C.B. MOLER,
C C COMPUTER METHODS FOR MATHEMATICAL COMPUTATIONS (PRENTICE-HALL, 1977).
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NQ=33, NX=117, ND=94, NSET=19)
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
C       IMPLICIT REAL*8(A-H,O-Z)
C       PARAMETER (NQ=33, NX=117, ND=94)
C       DIMENSION Y(NX,NQ,ND),B(NX,NQ,ND),C(NX,NQ,ND),D(NX,NQ,ND)
C      1         ,X(NX) 
C       NM1=N-1
C       IF(N.LT.2) RETURN
C       IF(N.LT.3) GO TO 250
C       D(1,J,I)=X(2)-X(1)
C       C(2,J,I)=(Y(2,J,I)-Y(1,J,I))/D(1,J,I)
C       DO 210 K=2,NM1
C         D(K,J,I)=X(K+1)-X(K)
C         B(K,J,I)=2.0D0*(D(K-1,J,I)+D(K,J,I))
C         C(K+1,J,I)=(Y(K+1,J,I)-Y(K,J,I))/D(K,J,I)
C         C(K,J,I)=C(K+1,J,I)-C(K,J,I)
C   210 CONTINUE
C       B(1,J,I)=-D(1,J,I)
C       B(N,J,I)=-D(N-1,J,I)
C       C(1,J,I)=0.0D0
C       C(N,J,I)=0.0D0
C       IF(N.EQ.3) GO TO 215
C       C(1,J,I)=C(3,J,I)/(X(4)-X(2))-C(2,J,I)/(X(3)-X(1))
C       C(N,J,I)=C(N-1,J,I)/(X(N)-X(N-2))-C(N-2,J,I)/(X(N-1)-X(N-3))
C       C(1,J,I)=C(1,J,I)*D(1,J,I)**2.0D0/(X(4)-X(1))
C       C(N,J,I)=-C(N,J,I)*D(N-1,J,I)**2.0D0/(X(N)-X(N-3))
C   215 CONTINUE
C       DO 220 K=2,N
C         T=D(K-1,J,I)/B(K-1,J,I)
C         B(K,J,I)=B(K,J,I)-T*D(K-1,J,I)
C         C(K,J,I)=C(K,J,I)-T*C(K-1,J,I)
C   220 CONTINUE
C       C(N,J,I)=C(N,J,I)/B(N,J,I)
C       DO 230 IB=1,NM1
C         K=N-IB
C         C(K,J,I)=(C(K,J,I)-D(K,J,I)*C(K+1,J,I))/B(K,J,I)
C   230 CONTINUE
C       B(N,J,I)=(Y(N,J,I)-Y(NM1,J,I))/D(NM1,J,I)
C      1        +D(NM1,J,I)*(C(NM1,J,I)+2.0D0*C(N,J,I))
C       DO 240 K=1,NM1
C         B(K,J,I)=(Y(K+1,J,I)-Y(K,J,I))/D(K,J,I)
C      1          -D(K,J,I)*(C(K+1,J,I)+2.0D0*C(K,J,I))
C         D(K,J,I)=(C(K+1,J,I)-C(K,J,I))/D(K,J,I)
C         C(K,J,I)=3.0D0*C(K,J,I)
C   240 CONTINUE
C       C(N,J,I)=3.0D0*C(N,J,I)
C       D(N,J,I)=D(N-1,J,I)
C       RETURN
C   250 CONTINUE
C       B(1,J,I)=(Y(2,J,I)-Y(1,J,I))/(X(2)-X(1))
C       C(1,J,I)=0.0D0
C       D(1,J,I)=0.0D0
C       B(2,J,I)=B(1,J,I)
C       C(2,J,I)=0.0D0
C       D(2,J,I)=0.0D0
C       RETURN
C       END
C ---------------------------------------------------------------------
C       INTEGER FUNCTION ISERCH(N,X,Y)
C C ---------------------------------------------------------------------
C C THIS FUNCTION SEARCHES "I" WHICH SATISFIES THE RELATION
C C X(I) <= Y < X(I+1) BY USING A BINARY SEARCH.
C       IMPLICIT REAL*8(A-H,O-Z)
C       DIMENSION X(117)
C 
C       MIN=1
C       MAX=N+1
C 
C    10 CONTINUE
C       MID=(MIN+MAX)/2
C       IF(Y.LT.X(MID)) THEN
C         MAX=MID
C       ELSE
C         MIN=MID
C       END IF
C       IF((MAX-MIN).GT.1) GO TO 10
C 
C       ISERCH=MIN
C 
C       RETURN
C       END
C *********************************************************************
C THE END OF THE PROGRAM.
C *********************************************************************

