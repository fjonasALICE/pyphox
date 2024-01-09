C***********************************************************************        
C*=======================                                              *        
       SUBROUTINE BSDATE                                                        
C*=======================                                              *        
C*((Purpose))                                                          *        
C*    Changethe format of the time stamp.                              *        
C*    This program should be modified according to the machine.        *        
C*((Author))                                                           *        
C*    S.Kawabata  Nov. '91 at KEK                                      *        
C***********************************************************************        
       COMMON /BDATE/ IDATE(3),ITIME(2)
*            IDATE(1) : year        ITIME(1) : hour
*            IDATE(2) : month       ITIME(2) : minute
* 
 
       CHARACTER   DD*8
       CHARACTER   NOW*12
 
       CALL DATE( DD )
*      WRITE(6,*) 'DD = ',DD
       READ( DD, 9000) (IDATE(I),I=1,3)
 9000  FORMAT(I2,1X,I2,1X,I2)
 
CFACOM CALL TIME( ITIM )
CFACOM ITIM = ITIM/60000
CFACOM ITIME(2) = MOD( ITIM, 60 )
CFACOM ITIME(1) = ITIM/60

       CALL CLOCK(NOW,1)
*      WRITE(6,*) 'NOW = ',NOW
       READ( NOW(1:2) , '(I2)' ) ITIME(1)
       READ( NOW(4:5) , '(I2)' ) ITIME(2)
       RETURN
       END
