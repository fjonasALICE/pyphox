      PROGRAM TEST
      IMPLICIT REAL*8 (A-H,O-Z)
      real*8 lambdaQCD(3:5)
      DIMENSION DH(0:10)
      IH=9
      Z=0.1045d0
      Q=6.d0
      CALL AKK(IH,Z,Q,DH)
      write(6,*)'Z,Q=',Z,Q
      write(6,*)'CHARGE-SIGN UNIDENTIFIED CHARGED PION:'
      write(6,*)'gluon   =',3.*DH(0)
      write(6,*)'strange =',DH(5)
      CALL AKK_LAMBDAQCD(LAMBDAQCD)
      write(6,*)'Lambda(3)=',lambdaQCD(3)
      write(6,*)'Lambda(4)=',lambdaQCD(4)
      write(6,*)'Lambda(5)=',lambdaQCD(5)
      END

c--------------------------------------------------------------

      include 'akk.f'
