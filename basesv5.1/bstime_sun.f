*CMZ :          24/06/94  10.51.47  by  Unknown
*-- Author :
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
      save time_init
      save ticks
C
 
      IF( IFLG .NE. 0 ) THEN
C
          time = ixtime()/ticks - time_init
C
      ELSE
 
          ticks     = imtime()
          time_init = ixtime()/ticks
          TIME      = 0.0
 
      ENDIF
C
      RETURN
      END
