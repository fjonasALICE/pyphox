***********************************************************************
*                                                                     *
*  LO and NLO FRAGMENTATION FUNCTIONS                                 *
*  for charged pions, kaons and the inclusive sum of charged hadrons  *
*                                                                     *
*  as in S. Kretzer (kretzer@pa.msu.edu):                             *
*                                                                     * 
*  `Fragmentaion Functions from Flavour-inclusive and Flavour-tagged  *
*  e^+ e^- Annihilations';                                            *
*  Phys. Rev. D 62, 054001 (2000)                                     *
*                                                                     *
*  See above reference for details!                                   *  
*								      *	
*  INPUT:     ISET   defines hadron and perturbative order            *
*             ISET = 1  LO pion                                       *
*                       (DATA FILE 'plo.grid'  UNIT=11)               *
*             ISET = 2  NLO pion                                      *
*                       (DATA FILE 'pnlo.grid' UNIT=12)               *
*             ISET = 3  LO kaon                                       *
*                       (DATA FILE 'klo.grid'  UNIT=13)               *
*             ISET = 4  NLO kaon                                      *
*                       (DATA FILE 'knlo.grid' UNIT=14)               *
*             ISET = 5  LO inclusive charged hadrons                  *
*                       (DATA FILE 'hlo.grid'  UNIT=15)               *
*             ISET = 6  NLO inclusive charged hadrons                 *
*                       (DATA FILE 'hnlo.grid' UNIT=16)               *
*                                                                     *
*             ICHARGE  defines hadron charge                          *
*             ICHARGE =     1,     2,          3                      *
*             corr.  to (h^+), (h^-), (h^+)+(h^-)                     *   
*                                                                     *
*  This interpolation routine returs FFs in the range:                *
*             Z                    (between  0.01   and  1.0)         *
*             Q2 = scale in GeV^2  (between  0.8    and  1.D6)        *
*             Attention: Z \lesssim 0.05   is (very!) delicate        *
*                        ( as discussed in above reference )          *
*                                                                     *
*  The quality of the interpolation (as compared to the exact         * 
*  evolution) is about:                                               *
*  | z < 0.75  | z < 0.9 | z < 1.0 |                                  *                       
*   -------------------------------                                   *
*  |  < 3%     |  < 10%  |  > 10%  |                                  *
*                                                                     *
*  OUTPUT:  uff, dff, sff, cff, bff, gff                              *
*  where :  qff(1) is FF for quark and qff(2) for antiquark           *
*           e.g. uff(1) = D_u ; uff(2) = D_{\bar u}                   *
*  and   :  gff is FF for gluons; D_g                                 *  
*                                                                     *
*  The routine returns D_i^h(Z,Q^2), i.e. number (NOT momentum)       *
*                                    densities                        *  
*                                                                     *
*                                                                     *
*  COMMON:  The main program or the calling routine has to have       *
*           a common block  COMMON / FRAGINI / FINI , and  FINI       *
*           has always to be zero when PKHFF is called for the        *
*           first time or when 'ISET' has been changed.               *
*                                                                     *
*
**********************************************************************
* when parton interpolation is out of range in z, set all fragmentation
* functions to zero, write a warning message  and returns instead of stops
*
* assume grids in directory ../frag/grids 
* mod M.Werlen
**********
      SUBROUTINE PKHFF(ISET,ICHARGE,Z,Q2,uff,dff,sff,cff,bff,gff)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      dimension uff(2), dff(2), sff(2), cff(2), bff(2)
      PARAMETER (NPART=9, NZ=49, NQ=27, NARG=2)
      DIMENSION ZU(NZ,NQ), ZD(NZ,NQ), ZS(NZ,NQ)
      DIMENSION ZC(NZ,NQ), ZB(NZ,NQ), ZG(NZ,NQ), 
     >          ZUB(NZ,NQ), ZDB(NZ,NQ), ZSB(NZ,NQ), 
     >          PARTON (NPART,NQ,NZ-1)
      DIMENSION QS(NQ), ZF(NZ), ZT(NARG), NA(NARG), ARRF(NZ+NQ) 
      COMMON / FRAGINI / IFINI
      SAVE ZU, ZD, ZS, ZC, ZB, ZG, NA, ARRF, ZUB, ZDB, ZSB
* ... Z AND Q**2 VALUES OF THE GRID :
       DATA QS / 0.8d0, 
     1           1.0d0, 1.3d0, 1.8d0, 2.7d0, 4.0d0, 6.4d0,
     2           1.0d1, 1.6d1, 2.5d1, 4.0d1, 6.4d1,
     3           1.0d2, 1.8d2, 3.2d2, 5.7d2,
     4           1.0d3, 1.8d3, 3.2d3, 5.7d3,
     5           1.0d4, 2.2d4, 4.6d4,
     6           1.0d5, 2.2d5, 4.6d5, 
     7           1.d6 /
       DATA ZF / 0.0095d0, 0.01d0, 0.02d0 , 0.03d0, 0.04d0 , 0.05d0, 
     1           0.06d0  , 0.07d0, 0.08d0 , 0.09d0, 0.095d0, 0.1d0 , 
     2           0.125d0 , 0.15d0, 0.175d0, 0.2d0 , 0.225d0, 0.25d0, 
     3           0.275d0 , 0.3d0 , 0.325d0, 0.35d0, 0.375d0, 0.4d0 ,
     4           0.425d0 , 0.45d0, 0.475d0, 0.5d0 , 0.525d0, 0.55d0, 
     5           0.575d0 , 0.6d0 , 0.625d0, 0.65d0, 0.675d0, 0.7d0 ,
     6           0.725d0 , 0.75d0, 0.775d0, 0.8d0 , 0.825d0, 0.85d0, 
     7           0.875d0 , 0.9d0 , 0.925d0, 0.95d0, 0.975d0, 0.99d0, 
     8             1.0d0 /
*...CHECK OF Z AND Q2 VALUES : 
       ZTMP=Z
       Q2TMP=Q2
       IF ( (Z.LT.0.05D0) .OR. (Z.GT.1.0D0) ) THEN
           WRITE(6,91) Z
  91       FORMAT (2X,'PARTON INTERPOLATION: Z OUT OF RANGE',D12.5)
           IF (Z.LT.0.05D0) Z=0.05D0
       ENDIF
       IF ( (Q2.LT.1.D0) .OR. (Q2.GT.1.D6) ) THEN
           WRITE(6,92) Q2
  92       FORMAT (2X,'PARTON INTERPOLATION: Q2 OUT OF RANGE',D12.5)
           IF (Q2.LT.1.D0) Q2=1.D0 
           IF (Q2.GT.1.D6) Q2=1.D6
c       stop
       ENDIF
*...INITIALIZATION :
*    SELECTION AND READING OF THE GRID :
       IF (IFINI.NE.0) GOTO 16
      IF (ISET.EQ.1) THEN
       IIREAD=11       
       OPEN(IIREAD,FILE='kretzer/'//'plo.grid')
      ELSEIF (ISET.EQ.2) THEN
       IIREAD=12
       OPEN(IIREAD,file='kretzer/'//'pnlo.grid')
       ELSEIF (ISET.EQ.3) THEN
       IIREAD=13
       OPEN(IIREAD,file='kretzer/'//'klo.grid')
      ELSEIF (ISET.EQ.4) THEN
       IIREAD=14
       OPEN(IIREAD,file='kretzer/'//'knlo.grid')
      ELSEIF (ISET.EQ.5) THEN
       IIREAD=15
       OPEN(IIREAD,FILE='kretzer/'//'hlo.grid')
      ELSEIF (ISET.EQ.6) THEN
       IIREAD=16
       OPEN(IIREAD,FILE='kretzer/'//'hnlo.grid')

	ELSE
         WRITE(6,93)
 93      FORMAT (2X,'PARTON INTERPOLATION: ISET OUT OF RANGE')
         write(6,*) 'iset =', iset
         stop
      END IF
C
       DO 15 M = 1, NZ-1 
       DO 15 N = 1, NQ
       READ(IIREAD,90) PARTON(1,N,M), PARTON(2,N,M), PARTON(3,N,M), 
     1                 PARTON(4,N,M), PARTON(5,N,M), PARTON(6,N,M),
     2                 PARTON(7,N,M), PARTON(8,N,M), PARTON(9,N,M)
  90   FORMAT (9(1PE10.3))
  15   CONTINUE
       CLOSE(IIREAD)
C
      IFINI = 1
*....ARRAYS FOR THE INTERPOLATION SUBROUTINE :
      DO 10 IQ = 1, NQ
      DO 20 IZ = 1, NZ-1
        ZF0 = ZF(IZ) 
        ZF1 = 1.D0-ZF(IZ)
        ZU (IZ,IQ) = PARTON(1,IQ,IZ) / (ZF1**4 * ZF0**0.5)
        ZUB(IZ,IQ) = PARTON(2,IQ,IZ) / (ZF1**4 * ZF0**0.5)
        ZD (IZ,IQ) = PARTON(3,IQ,IZ) / (ZF1**4 * ZF0**0.5)
        ZDB(IZ,IQ) = PARTON(4,IQ,IZ) / (ZF1**4 * ZF0**0.5)
        ZS (IZ,IQ) = PARTON(5,IQ,IZ) / (ZF1**4 * ZF0**0.5)
        ZSB(IZ,IQ) = PARTON(6,IQ,IZ) / (ZF1**4 * ZF0**0.5)
        ZC(IZ,IQ)  = PARTON(7,IQ,IZ) / (ZF1**7 * ZF0**0.3)
        ZB(IZ,IQ)  = PARTON(8,IQ,IZ) / (ZF1**7 * ZF0**0.3)
        ZG(IZ,IQ)  = PARTON(9,IQ,IZ) / (ZF1**8 * ZF0**0.3)
  20  CONTINUE
        ZU (NZ,IQ) = 0.D0
        ZUB(NZ,IQ) = 0.D0
        ZD (NZ,IQ) = 0.D0
        ZDB(NZ,IQ) = 0.D0
        ZS (NZ,IQ) = 0.D0
        ZSB(NZ,IQ) = 0.D0
        ZC (NZ,IQ) = 0.D0
        ZB (NZ,IQ) = 0.D0
        ZG (NZ,IQ) = 0.D0
  10  CONTINUE  
      NA(1) = NZ
      NA(2) = NQ
      DO 30 IZ = 1, NZ
        ARRF(IZ) = DLOG(ZF(IZ))
  30  CONTINUE
      DO 40 IQ = 1, NQ
        ARRF(NZ+IQ) = DLOG(QS(IQ))
  40  CONTINUE
  16  CONTINUE
*...INTERPOLATION :
      ZT(1) = DLOG(Z)
      ZT(2) = DLOG(Q2)
      if ( icharge .eq. 1 ) then
      uff(1) = FINT(NARG,ZT,NA,ARRF,ZU ) * (1.D0-Z)**4 * Z**0.5 / Z
      uff(2) = FINT(NARG,ZT,NA,ARRF,ZUB) * (1.D0-Z)**4 * Z**0.5 / Z
      dff(1) = FINT(NARG,ZT,NA,ARRF,ZD ) * (1.D0-Z)**4 * Z**0.5 / Z
      dff(2) = FINT(NARG,ZT,NA,ARRF,ZDB) * (1.D0-Z)**4 * Z**0.5 / Z
      sff(1) = FINT(NARG,ZT,NA,ARRF,ZS ) * (1.D0-Z)**4 * Z**0.5 / Z
      sff(2) = FINT(NARG,ZT,NA,ARRF,ZSB) * (1.D0-Z)**4 * Z**0.5 / Z
      cff(1) = FINT(NARG,ZT,NA,ARRF,ZC)  * (1.D0-Z)**7 * Z**0.3 / Z
      cff(2) = cff(1)
      bff(1) = FINT(NARG,ZT,NA,ARRF,ZB)  * (1.D0-Z)**7 * Z**0.3 / Z
      bff(2) = bff(1)
      gff    = FINT(NARG,ZT,NA,ARRF,ZG)  * (1.D0-Z)**8 * Z**0.3 / Z
      elseif ( icharge .eq. 2 ) then
      uff(2) = FINT(NARG,ZT,NA,ARRF,ZU ) * (1.D0-Z)**4 * Z**0.5 / Z
      uff(1) = FINT(NARG,ZT,NA,ARRF,ZUB) * (1.D0-Z)**4 * Z**0.5 / Z
      dff(2) = FINT(NARG,ZT,NA,ARRF,ZD ) * (1.D0-Z)**4 * Z**0.5 / Z
      dff(1) = FINT(NARG,ZT,NA,ARRF,ZDB) * (1.D0-Z)**4 * Z**0.5 / Z
      sff(2) = FINT(NARG,ZT,NA,ARRF,ZS ) * (1.D0-Z)**4 * Z**0.5 / Z
      sff(1) = FINT(NARG,ZT,NA,ARRF,ZSB) * (1.D0-Z)**4 * Z**0.5 / Z
      cff(2) = FINT(NARG,ZT,NA,ARRF,ZC)  * (1.D0-Z)**7 * Z**0.3 / Z
      cff(1) = cff(2)
      bff(2) = FINT(NARG,ZT,NA,ARRF,ZB)  * (1.D0-Z)**7 * Z**0.3 / Z
      bff(1) = bff(2)
      gff    = FINT(NARG,ZT,NA,ARRF,ZG)  * (1.D0-Z)**8 * Z**0.3 / Z
      elseif ( icharge .eq. 3 ) then
      uff(1) = FINT(NARG,ZT,NA,ARRF,ZU ) * (1.D0-Z)**4 * Z**0.5 / Z
     >       + FINT(NARG,ZT,NA,ARRF,ZUB) * (1.D0-Z)**4 * Z**0.5 / Z
      uff(2) = uff(1)
      dff(1) = FINT(NARG,ZT,NA,ARRF,ZD ) * (1.D0-Z)**4 * Z**0.5 / Z
     >       + FINT(NARG,ZT,NA,ARRF,ZDB) * (1.D0-Z)**4 * Z**0.5 / Z
      dff(2) = dff(1)
      sff(1) = FINT(NARG,ZT,NA,ARRF,ZS ) * (1.D0-Z)**4 * Z**0.5 / Z
     >       + FINT(NARG,ZT,NA,ARRF,ZSB) * (1.D0-Z)**4 * Z**0.5 / Z
      sff(2) = sff(1)
      cff(1) = FINT(NARG,ZT,NA,ARRF,ZC)  * (1.D0-Z)**7 * Z**0.3 / Z 
     >       * 2.d0
      cff(2) = cff(1)
      bff(1) = FINT(NARG,ZT,NA,ARRF,ZB)  * (1.D0-Z)**7 * Z**0.3 / Z
     >       * 2.d0
      bff(2) = bff(1)
      gff    = FINT(NARG,ZT,NA,ARRF,ZG)  * (1.D0-Z)**8 * Z**0.3 / Z
     >       * 2.d0
      else
      WRITE(6,94) 
 94   FORMAT (2X,'PARTON INTERPOLATION: ICHARGE OUT OF RANGE')
      stop
      endif

      Z=ZTMP
      Q2=Q2TMP
 60   RETURN
      END

*
*
*
*
c$$$      FUNCTION FINT(NARG,ARG,NENT,ENT,TABLE)
c$$$*********************************************************************
c$$$*                                                                   *
c$$$*   THE INTERPOLATION ROUTINE (CERN LIBRARY ROUTINE E104)           *
c$$$*                                                                   *
c$$$*********************************************************************
c$$$      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
c$$$      DIMENSION ARG(5),NENT(5),ENT(10),TABLE(10)
c$$$      DIMENSION D(5),NCOMB(5),IENT(5)
c$$$      KD=1
c$$$      M=1
c$$$      JA=1
c$$$         DO 5 I=1,NARG
c$$$      NCOMB(I)=1
c$$$      JB=JA-1+NENT(I)
c$$$         DO 2 J=JA,JB
c$$$      IF (ARG(I).LE.ENT(J)) GO TO 3
c$$$    2 CONTINUE
c$$$      J=JB
c$$$    3 IF (J.NE.JA) GO TO 4
c$$$      J=J+1
c$$$    4 JR=J-1
c$$$      D(I)=(ENT(J)-ARG(I))/(ENT(J)-ENT(JR))
c$$$      IENT(I)=J-JA
c$$$      KD=KD+IENT(I)*M
c$$$      M=M*NENT(I)
c$$$    5 JA=JB+1
c$$$      FINT=0.
c$$$   10 FAC=1.
c$$$      IADR=KD
c$$$      IFADR=1
c$$$         DO 15 I=1,NARG
c$$$      IF (NCOMB(I).EQ.0) GO TO 12
c$$$      FAC=FAC*(1.-D(I))
c$$$      GO TO 15
c$$$   12 FAC=FAC*D(I)
c$$$      IADR=IADR-IFADR
c$$$   15 IFADR=IFADR*NENT(I)
c$$$      FINT=FINT+FAC*TABLE(IADR)
c$$$      IL=NARG
c$$$   40 IF (NCOMB(IL).EQ.0) GO TO 80
c$$$      NCOMB(IL)=0
c$$$      IF (IL.EQ.NARG) GO TO 10
c$$$      IL=IL+1
c$$$         DO 50  K=IL,NARG
c$$$   50 NCOMB(K)=1
c$$$      GO TO 10
c$$$   80 IL=IL-1
c$$$      IF(IL.NE.0) GO TO 40
c$$$      RETURN
c$$$      END
c$$$
c$$$
c$$$
c$$$
