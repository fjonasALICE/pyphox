C
********************************************************************
*                                                                  *
*          UNPOLARIZED AND POLARIZED FRAGMENTATION FUNCTIONS       *
*                     FOR LAMBDA - BARYONS                         *
*               D. DE FLORIAN, M. STRATMANN, W. VOGELSANG          *
*                                                                  *
*   INPUT:   ISET = number of the parton set :                     *
*              ISET = 1  NLO UNPOLARIZED                           *
*                        (DATA FILE 'unpnlo.grid' UNIT=11)         *
*              ISET = 2  LO UNPOLARIZED                            *
*                        (DATA FILE 'unplo.grid' UNIT=21)          *
*								   *
*              ISET = 3  NLO POLARIZED SCENARIO 1                  *
*                        (DATA FILE 'pnlo1.grid' UNIT=12)          *
*              ISET = 4  NLO POLARIZED SCENARIO 2                  *
*                        (DATA FILE 'pnlo2.grid' UNIT=13)          *
*              ISET = 5  NLO POLARIZED SCENARIO 3                  *
*                        (DATA FILE 'pnlo3.grid' UNIT=14)          *
*              ISET = 6  LO POLARIZED SCENARIO 1                   *
*                        (DATA FILE 'plo1.grid' UNIT=22)           *
*              ISET = 7  LO POLARIZED SCENARIO 2                   *
*                        (DATA FILE 'plo2.grid' UNIT=23)           *
*              ISET = 8  LO POLARIZED SCENARIO 3                   *
*                        (DATA FILE 'plo3.grid' UNIT=24)           *
*                                                                  *
*            Z                    (between  0.05   and  1.0)       *
*            Q2 = scale in GeV**2 (between  1.0    and  1.D4)      *
*             (for values outside the allowed range the program    *
*              writes a warning and extrapolates to the x and      *
*              Q2 values requested)                                *
*                                                                  *
*   OUTPUT:  UTOT/DTOT/STOT/CTOT/BTOT/GL/UVAL/DVAL/SVAL            *
*             UTOT means U+Ubar ...                                *
*             UVAL/DVAL/SVAL only for the polarized case           *  
*	     Always LAMBDA/LAMBDAbar                               *
*            Always x times the distribution is returned           *
*                                                                  *
*                                                                  *
*   COMMON:  The main program or the calling routine has to have   *
*            a common block  COMMON / FRAGINI / FINI , and  FINI   *
*            has always to be zero when PARPOL is called for the   *
*            first time or when 'ISET' has been changed.           *
*                                                                  *
********************************************************************
*
      SUBROUTINE FRAGPAR (ISET, X, Q2, UTOT, DTOT, STOT, CTOT,
     1                                 BTOT, GL, UVAL, DVAL, SVAL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER FINI
      PARAMETER (NPART=9, NX=33, NQ=20, NARG=2)
      DIMENSION XUTOTF(NX,NQ), XDTOTF(NX,NQ), XSTOTF(NX,NQ)
      DIMENSION XUVALF(NX,NQ), XDVALF(NX,NQ), XSVALF(NX,NQ)
      DIMENSION XCTOTF(NX,NQ), XBTOTF(NX,NQ)
      DIMENSION XGF(NX,NQ), PARTON (NPART,NQ,NX-1)
      DIMENSION QS(NQ), XB(NX), XT(NARG), NA(NARG), ARRF(NX+NQ) 
c      COMMON / FRAGINI / FINI
      DATA FINI/0./
      SAVE FINI
      SAVE XUTOTF, XDTOTF, XSTOTF, XCTOTF, XBTOTF, XGF, NA, ARRF
      SAVE XUVALF, XDVALF, XSVALF
*...BJORKEN-X AND Q**2 VALUES OF THE GRID :
       DATA QS / 1.d0, 1.25D0, 1.5D0, 2.5D0, 
     1           4.0D0, 6.4D0, 1.0D1, 1.5D1, 2.5D1, 4.0D1, 6.4D1,
     2           1.0D2, 1.8D2, 3.2D2, 5.8D2, 1.0D3, 1.8D3,
     3           3.2D3, 5.8D3, 1.0D4 /
       DATA XB / 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.095,
     4           0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275,
     5           0.3, 0.325, 0.35, 0.375, 0.4, 0.45,  0.5, 0.55,
     6           0.6, 0.65,  0.7,  0.75,  0.8, 0.85,  0.9 , 0.93, 1.0/
*...CHECK OF X AND Q2 VALUES : 
       IF ( (X.LT.0.01D0) .OR. (X.GT.1.0D0) ) THEN
           WRITE(6,91) 
  91       FORMAT (2X,'PARTON INTERPOLATION: X OUT OF RANGE')
C          GOTO 60
       ENDIF
       IF ( (Q2.LT.1.D0) .OR. (Q2.GT.1.D4) ) THEN
c           WRITE(6,92) 
  92       FORMAT (2X,'PARTON INTERPOLATION: Q2 OUT OF RANGE')
C          GOTO 60
       ENDIF
*...INITIALIZATION :
*    SELECTION AND READING OF THE GRID :
       IF (FINI.NE.0) GOTO 16
*       write(6,*) 'reading grids'
      IF (ISET.EQ.1) THEN
       IIREAD=11       
       OPEN(IIREAD,FILE='dsv/unpnlo.grid')
      ELSEIF (ISET.EQ.2) THEN
       IIREAD=21
       OPEN(IIREAD,FILE='dsv/unplo.grid')
       ELSEIF (ISET.EQ.3) THEN
       IIREAD=12
       OPEN(IIREAD,FILE='dsv/pnlo1.grid')
      ELSEIF (ISET.EQ.4) THEN
       IIREAD=13
       OPEN(IIREAD,FILE='dsv/pnlo2.grid')
      ELSEIF (ISET.EQ.5) THEN
       IIREAD=14
       OPEN(IIREAD,FILE='dsv/pnlo3.grid')
      ELSEIF (ISET.EQ.6) THEN
       IIREAD=22
       OPEN(IIREAD,FILE='dsv/plo1.grid')
      ELSEIF (ISET.EQ.7) THEN
       IIREAD=23
       OPEN(IIREAD,FILE='dsv/plo2.grid')
      ELSEIF (ISET.EQ.8) THEN
       IIREAD=24
       OPEN(IIREAD,FILE='dsv/plo3.grid')

	ELSE
         WRITE(6,93)
 93      FORMAT (2X,'PARTON INTERPOLATION: ISET OUT OF RANGE')
         GOTO 60
      END IF
C
       DO 15 M = 1, NX-1 
       DO 15 N = 1, NQ
       READ(IIREAD,90) PARTON(1,N,M), PARTON(2,N,M), PARTON(3,N,M), 
     1                 PARTON(4,N,M), PARTON(5,N,M), PARTON(6,N,M),
     2                 PARTON(7,N,M), PARTON(8,N,M), PARTON(9,N,M)
  90   FORMAT (9(1PE10.3))
  15   CONTINUE
       CLOSE(IIREAD)
C
      FINI = 1
*....ARRAYS FOR THE INTERPOLATION SUBROUTINE :
      DO 10 IQ = 1, NQ
      DO 20 IX = 1, NX-1
        XB0 = XB(IX) 
        XB1 = 1.D0-XB(IX)
        XUTOTF(IX,IQ) = PARTON(1,IQ,IX) / (XB1**4 * XB0**0.5)
        XDTOTF(IX,IQ) = PARTON(2,IQ,IX) / (XB1**4 * XB0**0.5)
        XSTOTF(IX,IQ) = PARTON(3,IQ,IX) / (XB1**4 * XB0**0.5) 
        XCTOTF(IX,IQ) = PARTON(4,IQ,IX) / (XB1**7 * XB0**0.3) 
        XBTOTF(IX,IQ) = PARTON(5,IQ,IX) / (XB1**7 * XB0**0.3)
        XGF(IX,IQ)    = PARTON(6,IQ,IX) / (XB1**8 * XB0**0.3)
        XUVALF(IX,IQ) = PARTON(7,IQ,IX) / (XB1**4 * XB0**0.5)
        XDVALF(IX,IQ) = PARTON(8,IQ,IX) / (XB1**4 * XB0**0.5)
        XSVALF(IX,IQ) = PARTON(9,IQ,IX) / (XB1**4 * XB0**0.5) 
  20  CONTINUE
        XUTOTF(NX,IQ) = 0.D0
        XDTOTF(NX,IQ) = 0.D0
        XSTOTF(NX,IQ) = 0.D0
        XCTOTF(NX,IQ) = 0.D0
        XBTOTF(NX,IQ) = 0.D0
        XGF(NX,IQ)    = 0.D0
        XUVALF(NX,IQ) = 0.D0
        XDVALF(NX,IQ) = 0.D0
        XSVALF(NX,IQ) = 0.D0
  10  CONTINUE  
      NA(1) = NX
      NA(2) = NQ
      DO 30 IX = 1, NX
        ARRF(IX) = DLOG(XB(IX))
  30  CONTINUE
      DO 40 IQ = 1, NQ
        ARRF(NX+IQ) = DLOG(QS(IQ))
  40  CONTINUE
  16  CONTINUE
*...INTERPOLATION :
      XT(1) = DLOG(X)
      XT(2) = DLOG(Q2)
      UTOT = FINTDSV(NARG,XT,NA,ARRF,XUTOTF) * (1.D0-X)**4 * X**0.5
      DTOT = FINTDSV(NARG,XT,NA,ARRF,XDTOTF) * (1.D0-X)**4 * X**0.5 
      STOT = FINTDSV(NARG,XT,NA,ARRF,XSTOTF) * (1.D0-X)**4 * X**0.5
      CTOT = FINTDSV(NARG,XT,NA,ARRF,XCTOTF) * (1.D0-X)**7 * X**0.3
      BTOT = FINTDSV(NARG,XT,NA,ARRF,XBTOTF) * (1.D0-X)**7 * X**0.3
      GL   = FINTDSV(NARG,XT,NA,ARRF,XGF)    * (1.D0-X)**8 * X**0.3
      UVAL = FINTDSV(NARG,XT,NA,ARRF,XUVALF) * (1.D0-X)**4 * X**0.5
      DVAL = FINTDSV(NARG,XT,NA,ARRF,XDVALF) * (1.D0-X)**4 * X**0.5 
      SVAL = FINTDSV(NARG,XT,NA,ARRF,XSVALF) * (1.D0-X)**4 * X**0.5
 60   RETURN
      END
*
*...CERN LIBRARY ROUTINE E104 (INTERPOLATION) :
*
      FUNCTION FINTDSV(NARG,ARG,NENT,ENT,TABLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ARG(5),NENT(5),ENT(63),TABLE(882)
      DIMENSION D(5),NCOMB(5),IENT(5)
      KD=1
      M=1
      JA=1
         DO 5 I=1,NARG
      NCOMB(I)=1
      JB=JA-1+NENT(I)
         DO 2 J=JA,JB
      IF (ARG(I).LE.ENT(J)) GO TO 3
    2 CONTINUE
      J=JB
    3 IF (J.NE.JA) GO TO 4
      J=J+1
    4 JR=J-1
      D(I)=(ENT(J)-ARG(I))/(ENT(J)-ENT(JR))
      IENT(I)=J-JA
      KD=KD+IENT(I)*M
      M=M*NENT(I)
    5 JA=JB+1
      FINTDSV=0.D0
   10 FAC=1.D0
      IADR=KD
      IFADR=1
         DO 15 I=1,NARG
      IF (NCOMB(I).EQ.0) GO TO 12
      FAC=FAC*(1.D0-D(I))
      GO TO 15
   12 FAC=FAC*D(I)
      IADR=IADR-IFADR
   15 IFADR=IFADR*NENT(I)
      FINTDSV=FINTDSV+FAC*TABLE(IADR)
      IL=NARG
   40 IF (NCOMB(IL).EQ.0) GO TO 80
      NCOMB(IL)=0
      IF (IL.EQ.NARG) GO TO 10
      IL=IL+1
         DO 50  K=IL,NARG
   50 NCOMB(K)=1
      GO TO 10
   80 IL=IL-1
      IF(IL.NE.0) GO TO 40
      RETURN
      END
