      SUBROUTINE dpolin2_bfg(X1A,X2A,YA,M,N,X1,X2,Y,DY)                            
      IMPLICIT REAL*8 (A-H,O-Z)                                       
      PARAMETER (NMAX=20,MMAX=20)                                             
      DIMENSION X1A(M),X2A(N),YA(M,N),YNTMP(NMAX),YMTMP(MMAX)
c      write(65,*) 'In dpolin2_bfg M',M,' N ',N
c      write(65,*) 'X1A ',X1A
c      write(65,*) 'X2A ',X2A
c      write(65,*)' YA ', YA 
c      write(65,*) 'X1 ',X1
c      write(65,*) 'X2 ',X2                   
      DO 12 J=1,M                                                             
        DO 11 K=1,N                                                           
          YNTMP(K)=YA(J,K)                                                    
11      CONTINUE
        CALL dpolint_bfg(X2A,YNTMP,N,X2,YMTMP(J),DY)                               
12    CONTINUE                                                                
      CALL dpolint_bfg(X1A,YMTMP,M,X1,Y,DY)                                        
      RETURN                                                                  
      END                                                                     
