C===================================================================
C
C               ALFA STRONG AT 1 OR 2 LOOPS AT SCALE Q2
C
C===================================================================
      double precision function alfas(iloop,q2)
      implicit real*8(a-h,l-z)
      double precision scale2
      double precision ALS1LOOP, ALS2LOOP      

        if (iloop.eq.0) then
c alphas given by LHAPDF (iloop=0)
                q = dsqrt(q2)
                alfas = alphasPDF(q)
        else if (iloop.eq.1) then 
c alphas given by solving numerically the RGE at 1 or 2 loops
                alfas = ALS1LOOP(q2)
        else if (iloop .eq. 2) then
                alfas = ALS2LOOP(q2)
        else
                write(*,*) 'illegal label for alpha_s'
        endif
        end


! Compute AlphaS at 1 loop
!--------------------------
 
        double precision function ALS1LOOP(scale2)
                implicit none
                double precision scale2
 
                double precision masch2,masbo2,masto2,lambda4Square
                common/alfa/masch2,masbo2,masto2,lambda4Square
 
        double precision Lambda2(4:5)
        common / AlphaS_Lambdas / Lambda2
 
        double precision b0(4:5), b1(4:5)
        common / AlphaS_betaCoeffs / b0, b1
 
        double precision M2bottom
        common / AlphaS_mass2 / M2bottom
 
        double precision pi
        data pi / 3.141592653589793 /
 
 
        integer Nf
 
        ! Initialisation: we compute Beta's and Lambda's
 
        M2bottom=masbo2
        do Nf=4,5
                b0(Nf)=( 33-2*Nf)/(12*pi)
        enddo
        Lambda2(4)=lambda4Square
        Lambda2(5)=Lambda4Square**( b0(4)/b0(5) )
     _                  * M2bottom**(1 - b0(4)/b0(5))
 
        ! We compute alphaS
 
        if (scale2 .gt. M2bottom) then
                ALS1LOOP=1/( b0(5) * log( scale2/Lambda2(5) ) )
        else
                ALS1LOOP=1/( b0(4) * log( scale2/Lambda2(4) ) )
        endif
 
        end
 
 
! Compute AlphaS at 2 loops
!--------------------------
 
        double precision function ALS2LOOP(scale2)
                implicit none
                double precision scale2
 
                double precision masch2,masbo2,masto2,lambda4Square
                common/alfa/masch2,masbo2,masto2,lambda4Square
 
                ! Uses
                double precision getLambda5Sqr, alphaSflav
 
                double precision Lambda2(4:5)
                common / AlphaS_Lambdas / Lambda2
 
                double precision b0(4:5), b1(4:5)
                common / AlphaS_betaCoeffs / b0, b1
 
                double precision M2bottom
                common / AlphaS_mass2 / M2bottom
 
        double precision pi
        data pi / 3.141592653589793 /
 
        integer Nf
 
        ! Initialisation: we compute Beta's and Lambda's
 
        M2bottom=masbo2
        do Nf=4,5
                b0(Nf)=( 33-2*Nf)/(12*pi)
                b1(Nf)=(153-19*Nf)/(24*pi**2*b0(Nf))
        enddo
        Lambda2(4)=lambda4Square
        Lambda2(5)=getLambda5Sqr(lambda4Square)
 
        ! We compute alphaS
 
        if (scale2 .gt. M2bottom) then
                ALS2LOOP=alphaSflav(scale2, 5, Lambda2(5))
        else
                ALS2LOOP=alphaSflav(scale2, 4, Lambda2(4))
        endif
 
        end
 
 
 
        ! Compute alphaS with f flavors at 2 loops
        !-----------------------------------------
 
        double precision function alphaSflav(scale2, Nf, Lambda2)
                implicit none
                double precision scale2
                integer Nf
                double precision Lambda2
 
                double precision b0(4:5), b1(4:5)
                common / AlphaS_betaCoeffs / b0, b1
 
                ! uses
                double precision oneOverBetaInt, beta
 
                ! The relative precision for the computation of alphaS
                double precision eps
                data eps / 1d-5 /
 
        double precision Logar, alpha, alpha1
 
        ! We solve the equation whose solution is alphaS with the Newton's
        ! method. The initial value is the 1loop value.
 
        Logar=b0(Nf)*log( scale2/lambda2 )
 
        alpha1=1/Logar
1          alpha=alpha1
           alpha1 = alpha - (oneOverBetaInt(alpha,Nf) - Logar)
     +                          * beta(alpha,Nf)
        if ( abs(alpha1-alpha)/alpha .gt. eps ) goto 1
 
        alphaSflav=alpha1
 
        end
 
 
        ! Compute Lambda5**2 from Lambda4**2 by requiring the continuity of
        ! alphaS at the threshold, at 2 loops
        !-------------------------------------------------------------------
 
        double precision function getLambda5Sqr(Lambda4Sqr)
                implicit none
                double precision Lambda4Sqr
 
                double precision b0(4:5), b1(4:5)
                common / AlphaS_betaCoeffs / b0, b1
 
                double precision M2bottom
                common / AlphaS_mass2 / M2bottom
 
                !uses
                double precision alphaSflav, oneOverBetaInt
 
        double precision alpha
 
        alpha=alphaSflav(M2bottom, 4, Lambda4Sqr)
        getLambda5Sqr = M2bottom * exp( -oneOverBetaInt(alpha,5)/b0(5) )
        end
 
 
        ! The primitiv of 1/beta at 2 loop
        !---------------------------------
 
        double precision function oneOverBetaInt(alphaS, Nf)
                implicit none
                double precision alphaS
                integer Nf
 
                double precision b0(4:5), b1(4:5)
                common / AlphaS_betaCoeffs / b0, b1
 
        oneOverBetaInt=
     _  1/alphaS + b1(Nf)*log( b0(Nf)*alphaS/(1 + b1(Nf)*alphaS) )
        end
 
 
        ! The beta function at 2 loops
        !-----------------------------
 
        double precision function beta(alphaS, Nf)
                implicit none
                double precision alphaS
                integer Nf
 
                double precision b0(4:5), b1(4:5)
                common / AlphaS_betaCoeffs / b0, b1
 
        beta=-b0(Nf)*alphaS**2 * (1 + b1(Nf)*alphaS)
        end
