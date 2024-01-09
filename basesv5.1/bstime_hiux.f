C                                                                               
C***********************************************************************        
C*=================================                                    *        
C* SUBROUTINE BSTIME( TIME, IFLG )                                     *        
C*=================================                                    *        
C*((Purpose))                                                          *        
C*        Interface routine to get used CPU time from FORTRAN          *        
C*        Library routine CLOCK etc.                                   *        
C*((Input))                                                            *        
C*        IFLG  : Flag                                                 *        
C*          IFLG = 0 : Initialization of clock routine.                *        
C*          IFLG = 1 : Get used CPU time.                              *        
C*((Output))                                                           *        
C*        TIME  : Used CPU time in second.                             *        
C*                                                                     *        
C*       Coded by S.Kawabata        Oct. '85                           *        
C*                                                                     *        
C***********************************************************************        
C                                                                               
      SUBROUTINE BSTIME( TIME, IFLG )                                           
C                                                                               
      IF( IFLG .NE. 0 ) THEN                                                    
C                                 FACOM Only                                    
C         CALL CLOCK( TIME, 0, 1 )                                              
C                                 HITAC Only                                    
          CALL CLOCK( TIME, 5 )                                                 
C                                                                               
      ELSE                                                                      
C                                 HITAC Only                                    
          CALL CLOCK                                                            
C                                                                               
      ENDIF                                                                     
C                                                                               
      RETURN                                                                    
      END                                                                       
