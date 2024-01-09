      SUBROUTINE dpolint_bfg(XA,YA,N,X,Y,DY)
      IMPLICIT REAL*8 (A-H,O-Z)                                       
      PARAMETER (NMAX=10)                                                     
      DIMENSION XA(N),YA(N),C(NMAX),D(NMAX)                                   
      NS=1                                                                    
      DIF=ABS(X-XA(1))                                                        
      DO 11 I=1,N                                                             
        DIFT=ABS(X-XA(I))                                                     
        IF (DIFT.LT.DIF) THEN                                                 
          NS=I                                                                
          DIF=DIFT                                                            
        ENDIF                                                                 
        C(I)=YA(I)                                                            
        D(I)=YA(I)                                                            
11    CONTINUE                                                                
      Y=YA(NS)                                                                
      NS=NS-1                                                                 
      DO 13 M=1,N-1                                                           
        DO 12 I=1,N-M                                                         
          HO=XA(I)-X                                                          
          HP=XA(I+M)-X                                                        
          W=C(I+1)-D(I)                                                       
          DEN=HO-HP                                                           
          IF(DEN.EQ.0.D0) then
	  write(65,*) ' stop in polint den',den
	  write(65,*) ' probably problem with compiler'
	  write(65,*) ' has to be static in distr* etc '
c	  write(65,*) ' N ',N
c	  write(65,*)  ' XA',XA
c	  write(65,*)  ' X',X
c	  write(65,*)  ' M',M,' I',I                                                  
          stop                                                  
          endif
	  DEN=W/DEN                                                           
          D(I)=HP*DEN                                                         
          C(I)=HO*DEN                                                         
12      CONTINUE                                                              
        IF (2*NS.LT.N-M)THEN                                                  
          DY=C(NS+1)                                                          
        ELSE                                                                  
          DY=D(NS)                                                            
          NS=NS-1                                                             
        ENDIF                                                                 
        Y=Y+DY                                                                
13    CONTINUE                                                                
      RETURN                                                                  
      END                                                                     
