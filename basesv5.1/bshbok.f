************************************************************************
*    ===============================                                   *
      SUBROUTINE BSHBOK( IOFSET )
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
 
      REAL*8         SCALLS,WGT,TI,TSI,TACC
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT
 
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
 
C
      IF( NHIST .GT. 0 ) THEN

          NTOTAL= SCALLS

          DO 500 IHIST = 1, NHIST
             ID    = MAPL(1,IHIST)
             ID    = ID + IOFSET
             IP1   = MAPL(2,IHIST)
             XMIN  = BUFF(IP1)
             XMAX  = BUFF(IP1+1)
             NXBIN = IBUF(IP1+2)
             DEV   = BUFF(IP1+3)
             IP2   = MAPL(3,IHIST)
             IP3   = MAPL(4,IHIST)
 
*        WRITE(LU,9200) ID,LUNIT,(BUFF(I),I=IP3+1,IP3+15),
*    .              NTOTAL,NXBIN,DEV
*9200    FORMAT(/1H1,
*    .         1X,'** Histogram ID(',I5,' ) was saved in Unit(',I2,') **',
*    .         /1X,'Title : ',15A4,
*    .         /1X,'Entries     =',I10,
*    .         /1X,'No. of bins =',I10,'  Width =',G13.4)
 
             WRITE( TITLE, 9500) (BUFF(I), I=IP3+1,IP3+16)
 9500        FORMAT(16A4)

             CALL HBOOK1( ID, TITLE, NXBIN, XMIN, XMAX, 0.0 )
 
             IPF   = IP2 + 156
             IPF2  = IPF + 52
             FACT       = 1./(NTOTAL*DEV)
             DO 400 I = 1, NXBIN
                TX     = BUFF(I+IPF)
                NX     = IBUF(I+IP2)
                VLS    = TX*FACT
*               IF( NX .GT. 1 ) THEN
*                   DEV2   =  NX*BUFF(I+IPF2)-TX*TX
*                   IF( DEV2 .LE. 0.0 ) THEN
*                       VER = 0.0
*                   ELSE
*                       VER = FACT*SQRT( DEV2/( NX-1 ))
*                   ENDIF
*               ELSEIF( NX .EQ. 1 ) THEN
*                   VER = VLS
*               ELSE
*                   VER = 0.0
*               ENDIF
                XX     = XMIN + DEV*(FLOAT(I) - 0.5)
 
	        CALL HFILL( ID, XX, 0.0, VLS )
  400        CONTINUE
 
  500     CONTINUE

      ENDIF

      IF( NSCAT .GT. 0 ) THEN
         DO 900 ISCAT = 1, NSCAT
 
            IP3   = MAPD(4,ISCAT)
 
            WRITE( TITLE, 9500) (BUFF(I), I=IP3+2,IP3+17)

            ID    = MAPD(1,ISCAT)
            ID    = ID + IOFSET + 10000
 
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
            NTOTAL= IBUF(IP2)
            FACT       = 1./(NTOTAL*DX*DY)
            
            DO 300 L = 0, NY-1
               IB     = NX*L + IP2
               DO 200 I = 1,NX
                  VLS   = BUFF( I + IB )* FACT
                  XX    = XL + DX*(FLOAT(I) - 0.5)
                  YY    = YL + DY*(FLOAT(L) - 0.5)
 
                  CALL HFILL( ID, XX, YY, VLS )

  200          CONTINUE
  300       CONTINUE
 
  900    CONTINUE
      ENDIF

      RETURN
      END
