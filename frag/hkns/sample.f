C *********************************************************************
      PROGRAM SAMPLE
C ---------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NX=200, NPAR=17, NFF=8)
      DIMENSION FF(-5:5), GRADFF(8,17), GRAD(-5:5,17)
     >          ,E(0:5), AA(17,6)

      DATA ISET,ICHARGE/3,1/
      DATA XMIN,XMAX/1.D-2,1.D0/
      DATA Q/91.2D0/      ! sqrt(Q^2[GeV^2])

      COMMON/ERRM/EM(17,17)   ! Error matrix: Dchi^2*H_ij^-1
                              ! EM(17,17) is defined in hknsff07.f
      Q2=Q*Q
      XLSTEP=(XMAX-XMIN)/DFLOAT(NX)
      DO L=1,NX+1
        X=XMIN+DFLOAT(L-1)*XLSTEP
        Call HKNSFF(Q2,X,ISET,ICHARGE,FF,GRAD)

        DO J=1,6
          E(J-1)=0.D0
          DO I=1,NPAR
            AA(I,J)=0.D0
          ENDDO
        ENDDO

C Calculating the uncertainties of FFs by the Hessian method
       DO N=1,6
          EE=0.D0
          DO I=1,NPAR 
            DO J=1,NPAR
                K=N-1
               IF(((ISET.EQ.3).OR.(ISET.EQ.4)).AND.((N.EQ.4))) K=-3 ! for Kaon
              AA(I,N)=AA(I,N)+EM(I,J)*GRAD(K,J)
            ENDDO
            EE=EE+GRAD(K,I)*AA(I,N)
          ENDDO
          E(N-1)=DSQRT(EE)
        ENDDO
        Write(12,1010) X,(X*FF(I),I=0,5),X*FF(-3)
        Write(13,1010) X,(X*E(I),I=0,5)
      ENDDO
 1010 FORMAT(' ', 8(1PE15.5))

      STOP
      END
C *********************************************************************
C THE END OF THE PROGRAM.
C *********************************************************************
