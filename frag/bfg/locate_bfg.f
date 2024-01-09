      SUBROUTINE locate1_bfg(XX,N,X,J)
      IMPLICIT REAL*8 (A-H,O-Z)                                             
      DIMENSION XX(N)
c      write(65,*) 'In LOCATE1_BFG N',N
c      write(65,*) 'XX ',XX 
c      write(65,*) 'X ',X                                                        
      JL=0                                                                    
      JU=N+1                                                                  
10    IF(JU-JL.GT.1)THEN                                                      
        JM=(JU+JL)/2                                                          
        IF((XX(N).GT.XX(1)).EQV.(X.GT.XX(JM)))THEN                            
          JL=JM                                                               
        ELSE                                                                  
          JU=JM                                                               
        ENDIF                                                                 
      GO TO 10                                                                
      ENDIF                                                                   
      J=JL                                                                    
      RETURN                                                                  
      END                                                                     
