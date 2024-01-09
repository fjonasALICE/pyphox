C***********************************************************************
C*                                                                     *
C*========================                                             *
C*    SUBROUTINE BSPUTW( WEIGHT )                                      *
C*========================                                             *
C*((Function))                                                         *
C*    Put Weight                                                       *
C*                                                                     *
C*    Coded   by T.Ishikawa    Jun. 1995 at KEK                        *
C*    Last update              Jun. 1995 at KEK                        *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE BSPUTW( WEIGHT )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT
*
*========= Save the grid information for the best accuracy ===========
*
      WGT = WEIGHT
C
      RETURN
      END
