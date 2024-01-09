C------------------------------------------------------------           
C------------- Subroutine to compute a point in x and q**2 --           
C------------- FOR THE FOLLOWING FRAGMENTATION FUNCTIONS:  --           
C-------------     GLUON,  UP   ,DOWN,                     --           
C-------------        STRANGE , CHARM , BOTTOM , TOP .     --           
C------------- Range of validity :                         --           
C-------------                     1.E-03 < X < .99        --           
C-------------                   LOG10 2  <LOG10 Q**2 < 10 --           
C------------------------------------------------------------           
C-------------        AUTHORS :    P CHIAPPETTA
C-------------                     J.PH. GUILLET                        
C-------------                     M.GRECO                              
C-------------    CGW FOR Q02=2GEV2         METHOD P.NASON
C-------------    NEXT TO LEADING EVOLUTION
C-------------    TABLE FOR LHC
*         I = 1    GLUON
*         I = 2    UP ( = ANTIUP )
*         I = 3    DOWN ( = ANTIDOWN )
*         I = 4    STRANGE ( = ANTISTRANGE )
*         I = 5    CHARM ( = ANTICHARM )
*         I = 6    BOTTOM ( = ANTIBOTTOM )
*         I = 7    TOP ( = ANTITOP )
*
C------------------------------------------------------------           
      SUBROUTINE  distributionNonPert_setI(Z,QSTAR2,STRFUN,FUNC)                       
              
      IMPLICIT REAL*8 (A-H,O-Z)
C------------------------------------------------------------           
C-------------   Z IS X AND QSTAR2 IS Q SQUARE  -------------           
      CHARACTER*(*) STRFUN                                              
C------------------------------------------------------------           
C------------ STRFUN specifies the desired function----------           
C------------------------------------------------------------           
C------------------------------------------------------------           
C        IFUN =  1     GLUON           'GLUON' (abbrev. 'GL')                       
C                2     UP              'UPVAL' (abbrev. 'UP')                   
C                3     DOWN            'DOVAL' (abbrev. 'DO')                   
C                4     STRANGE         'SBAR'  (abbrev. 'SB')                   
C                5     CHARM           'CBAR ' (abbrev. 'CB')                   
C                6     BOTTOM          'BBAR ' (abbrev. 'BB')                   
C                7     TOP             'TBAR ' (abbrev. 'TB')                   
C------------------------------------------------------------           
C-------------  FUNC is the output (parton density value)----           
C------------------------------------------------------------           


	include "Knots.f"
      PARAMETER (KFUN=6)
      DIMENSION XQDUM1(nb_x,nb_Q2_4flav,KFUN)                                   
      DIMENSION XQDUM2(nb_x,nb_Q2_5flav,KFUN)                                   
	include "gridNonPert_setI.f" 
         
c!!     parameter (IQmax= max(nb_Q2_4flav,nb_Q2_5flav) )
c!!      Not currently supported by g77
      parameter (IQmax=22)   
      PARAMETER (IGT=4)                             
      COMMON/STEPnonpert_setI/ISTEP

      dimension X(nb_x), Y_4flav(IQmax), Y_5flav(IQmax)
      dimension Y(IQmax)
      DIMENSION F(nb_x,IQmax)
      DIMENSION Q2(IQmax)                                          
      DIMENSION X1A(IGT),X2A(IGT),YA(IGT,IGT)                                              
 
      DATA ISTEP/0/      
      save 
      IF(ISTEP.EQ.0) THEN
	do i=1,nb_x
      	   X(i)=LOG10(x_grid(i))
	enddo
    	do i=1,nb_Q2_4flav
	   Y_4flav(i)=LOG10(Q2_4flav(i)) 
      	enddo
       	do i=1,nb_Q2_5flav
	   Y_5flav(i)=LOG10(Q2_5flav(i))          
      	enddo
        ISTEP=1
      ENDIF
      	
      XMIN = x_grid(1)
      XMAX = x_grid(nb_x)

c
c  impose minimum Q^2
      if(qstar2.lt.20.26) then      	
	Q2MIN=Q2_4flav(1)
      else
        Q2MIN=Q2_5flav(1)
      endif
      IF (QSTAR2.LE.Q2MIN) then 
	WRITE(65,*)' Q2 TOO SMALL!!!!!!!!!'
	stop
      endif
c      
      IF (STRFUN(1:2) .EQ. 'UP' ) THEN
      IFUN =2
      ELSE IF (STRFUN(1:2) .EQ. 'DO') THEN
      IFUN =3
      ELSE IF (STRFUN(1:2) .EQ. 'GL') THEN
      IFUN =1
      ELSE IF (STRFUN(1:2) .EQ. 'SB') THEN
      IFUN =4
      ELSE IF (STRFUN(1:2) .EQ. 'CB') THEN
      IFUN =5
      ELSE IF (STRFUN(1:2) .EQ. 'BB') THEN
      IFUN =6
      ELSE IF (STRFUN(1:2) .EQ. 'TB') THEN
      write(65,*) 'error : quark top not supported'
      stop
      ENDIF

      if(qstar2.lt.20.26) then
    	  IQ=nb_Q2_4flav
          DO J=1,IQ
            DO I=1,nb_x
              F(I,J)=XQDUM1(I,J,IFUN)
            ENDDO
            Y(j)=Y_4flav(j)
          ENDDO
      else
          IQ=nb_Q2_5flav
          do j=1,iq
             do i=1,nb_x
                 f(i,j)=xqdum2(i,j,ifun)
             enddo
             Y(j)=Y_5flav(j)
          enddo
      endif
      	
      FUNC=0.
      IF(Z.GT.XMIN.AND.Z.LT.XMAX) THEN
      A=LOG10(Z)
      B=LOG10(QSTAR2)
      CALL locate1_bfg(X,nb_x,A,JX)
      CALL locate1_bfg(Y,IQ,B,JQ)
      DO IJX=1,IGT
        IF (JX.EQ.1) THEN
          X1A(IJX)=X(IJX)
          DO IJQ=1,IGT
            IF (JQ.EQ.1) THEN
              X2A(IJQ)=Y(IJQ)
              YA(IJX,IJQ)=F(IJX,IJQ)
            ELSE IF (JQ.EQ.(IQ-1)) THEN
              X2A(IJQ)=Y(JQ-3+IJQ)
              YA(IJX,IJQ)=F(IJX,JQ-3+IJQ)
            ELSE IF (JQ.EQ.IQ) THEN
              X2A(IJQ)=Y(JQ-4+IJQ)
              YA(IJX,IJQ)=F(IJX,JQ-4+IJQ)
            ELSE
              X2A(IJQ)=Y(JQ-2+IJQ)
              YA(IJX,IJQ)=F(IJX,JQ-2+IJQ)
            ENDIF
          ENDDO
        ELSE IF (JX.EQ.(nb_x-1)) THEN
          X1A(IJX)=X(JX-3+IJX)
          DO IJQ=1,IGT
            IF (JQ.EQ.1) THEN
              X2A(IJQ)=Y(IJQ)
              YA(IJX,IJQ)=F(JX-3+IJX,IJQ)
            ELSE IF (JQ.EQ.(IQ-1)) THEN
              X2A(IJQ)=Y(JQ-3+IJQ)
              YA(IJX,IJQ)=F(JX-3+IJX,JQ-3+IJQ)
            ELSE IF (JQ.EQ.IQ) THEN
              X2A(IJQ)=Y(JQ-4+IJQ)
              YA(IJX,IJQ)=F(JX-3+IJX,JQ-4+IJQ)
            ELSE
              X2A(IJQ)=Y(JQ-2+IJQ)
              YA(IJX,IJQ)=F(JX-3+IJX,JQ-2+IJQ)
            ENDIF
          ENDDO
        ELSE IF (JX.EQ.nb_x) THEN
          X1A(IJX)=X(JX-4+IJX)
          DO IJQ=1,IGT
            IF (JQ.EQ.1) THEN
              X2A(IJQ)=Y(IJQ)
              YA(IJX,IJQ)=F(JX-4+IJX,IJQ)
            ELSE IF (JQ.EQ.(IQ-1)) THEN
              X2A(IJQ)=Y(JQ-3+IJQ)
              YA(IJX,IJQ)=F(JX-4+IJX,JQ-3+IJQ)
            ELSE IF (JQ.EQ.IQ) THEN
              X2A(IJQ)=Y(JQ-4+IJQ)
              YA(IJX,IJQ)=F(JX-4+IJX,JQ-4+IJQ)
            ELSE
              X2A(IJQ)=Y(JQ-2+IJQ)
              YA(IJX,IJQ)=F(JX-4+IJX,JQ-2+IJQ)
            ENDIF
          ENDDO
        ELSE
          X1A(IJX)=X(JX-2+IJX)
          DO IJQ=1,IGT
            IF (JQ.EQ.1) THEN
              X2A(IJQ)=Y(IJQ)
              YA(IJX,IJQ)=F(JX-2+IJX,IJQ)
            ELSE IF (JQ.EQ.(IQ-1)) THEN
              X2A(IJQ)=Y(JQ-3+IJQ)
              YA(IJX,IJQ)=F(JX-2+IJX,JQ-3+IJQ)
            ELSE IF (JQ.EQ.IQ) THEN
              X2A(IJQ)=Y(JQ-4+IJQ)
              YA(IJX,IJQ)=F(JX-2+IJX,JQ-4+IJQ)
            ELSE
              X2A(IJQ)=Y(JQ-2+IJQ)
              YA(IJX,IJQ)=F(JX-2+IJX,JQ-2+IJQ)
            ENDIF
          ENDDO
	ENDIF
      ENDDO
      CALL dpolin2_bfg(X1A,X2A,YA,4,4,A,B,YY,DY)                            
      FUNC = YY
C      WRITE (8,*) YY,DY
      ENDIF
C     IF (FUNC.LT.0.) FUNC=0.
      END
