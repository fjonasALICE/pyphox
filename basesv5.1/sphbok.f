************************************************************************
*    ===============================                                   *
      SUBROUTINE SPHBOK( IOFSET )
*    ===============================                                   *
* ((Purpose))                                                          *
*      To write the ID-th histogram on the unit LUNIT.                 *
* ((Input))                                                            *
*      LUNIT: Logical unit number                                      *
*      ID   : Historgram ID                                            *
*                                                                      *
* ((Author))                                                           *
*       S.Kawabata   June '90 at KEK                                   *
*                                                                      *
************************************************************************
 
      COMMON /SPRNG2/ MXTRY,NEVENT, NTRIAL, MISS 

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))

      CHARACTER*80 TITLE
 
      COMMON/PLOTLU/ LU

      REAL*4 COUNT(NHS), ECOUNT(NHS)
 
C
      IF( NHIST .GT. 0 ) THEN


          DO 500 IHIST = 1, NHIST
C                             ID1 : ID number of IHIST-th histogram
C                             BMAX : Maximum content of Hist ID1 from BASES
             IFLH  = IFBASE(IHIST)
             IF( IFLH .EQ. 1 ) THEN
                 ID1   = MAPL(1,IHIST)
                 BSUM  = HSUM( ID1 )
             ELSE
                 BSUM  = 0.0
             ENDIF

             ID    = MAPL(1,IHIST)
             ID    = ID + IOFSET + 20000
             IP1   = MAPL(2,IHIST)
             XMIN  = BUFF(IP1)
             XMAX  = BUFF(IP1+1)
             NXBIN = IBUF(IP1+2)
             DEV   = BUFF(IP1+3)
             IP2   = MAPL(3,IHIST)
             IP3   = MAPL(4,IHIST)
 
 
             WRITE( TITLE, 9500) (BUFF(I), I=IP3+1,IP3+16)
 9500        FORMAT(16A4)

             CALL HBOOK1( ID, TITLE, NXBIN, XMIN, XMAX, 0.0 )

             IPX   = IP2 + 52
C               To obtain the maximum content of Spring hist ID1
             SSUM  = NEVENT
C                        FACT : Normalization factor
             FACT  = BSUM/SSUM
             DO  400 I   = 0, NXBIN

                 XX     = XMIN + DEV*(FLOAT(I) - 0.5)
 
	        CALL HFILL( ID, XX, 0.0, BUFF( I + IPX ) )

                COUNT(I)  = BUFF( I+IPX )
                ECOUNT(I) = SQRT( BUFF( I+IPX ) )
                IF( IFLH .EQ. 1 ) THEN
                    COUNT(I) = COUNT(I) * FACT
                    ECOUNT(I) = ECOUNT(I) * FACT
                ENDIF
  400        CONTINUE

             CALL HPAK(  ID,  COUNT )
             CALL HPAKE( ID, ECOUNT )
 
  500     CONTINUE

      ENDIF

      IF( NSCAT .GT. 0 ) THEN
         DO 900 ISCAT = 1, NSCAT
 
            IP3   = MAPD(4,ISCAT)
 
            WRITE( TITLE, 9500) (BUFF(I), I=IP3+2,IP3+17)

            ID    = MAPD(1,ISCAT)
            ID    = ID + IOFSET + 30000
 
            IP1   = MAPD(2,ISCAT)
            XL    = BUFF(IP1)
            XU    = BUFF(IP1+1)
            NX    = IBUF(IP1+2)
            DX    = BUFF(IP1+3)
            YL    = BUFF(IP1+4)
            YU    = BUFF(IP1+5)
            NY    = IBUF(IP1+6)
            DY    = BUFF(IP1+7)

            CALL HBOOK2( ID, TITLE, NX, XL, XU, NY, YL, YU, 0.0 )
 
            IP2   = MAPD(3,ISCAT)
            
            DO 700 L = 0, NY-1
               IB     = NX*L + IP2
               DO 600 I = 1,NX

                  XX    = XL + DX*(FLOAT(I) - 0.5)
                  YY    = YL + DY*(FLOAT(L) - 0.5)
 
                  CALL HFILL( ID, XX, YY, BUFF( I + IB ) )

  600          CONTINUE
  700       CONTINUE
 
  900    CONTINUE
      ENDIF
 
      RETURN
      END
