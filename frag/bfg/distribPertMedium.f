      SUBROUTINE  distributionPertMedium(Z,QSTAR2,XEPTN,STRFUN,FUNC)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) STRFUN                                              
      COMMON/WLOSS/XOMC

      IF (XOMC.EQ.0.D0) THEN
        CALL distributionPert(Z,QSTAR2,STRFUN,FUNC)
      ENDIF
 
      RETURN
      END
