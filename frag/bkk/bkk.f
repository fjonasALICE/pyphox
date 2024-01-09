c Dear user,
c
c here is a parameterization of the LO and NLO fragmentation functions for
c charged pions, neutral and charged kaons in the MS-bar scheme.
c
c References:
c [1] Pion and kaon production in $e^+e^-$ and $ep$ collisions at next-to-leading order,
c J. Binnewies, B.A. Kniehl, and G. Kramer,
c Phys. Rev. D 52 (1995) 4947-4960
c [2] Neutral-kaon production in $e^+e^-$, $ep$, and $p\bar p$ collisions at
c next-to-leading order,
c J. Binnewies, B.A. Kniehl, and G. Kramer,
c Phys. Rev. D 53 (1996) 3573-3581

      subroutine bkk(ih,iset,x,qs,dh)
        implicit real*8 (a-h,o-z)
        dimension dh(0:10)
C Fragmentation functions by Binnewies,Kniehl,Kramer 
C new version with mass-threshholds for c and b quarks
C fitted to new data from ALEPH and data from TPC (charged)
C and to ALEPH and MARKII (neutral Kaon)
C
C
C ih, iset, x, qs are input; dh is output.
C ih   = 1 : (pi^+ + pi^-)/2
C ih   = 2 : (K^+ + K^-)/2   
C ih   = 3 : (K^0 + K^0_bar)/2   
C ih   = 5 :  pi^0  
C ih   = 7 :  h^+ + h^-       as sum of pions and kaons 
C iset = 0 : LO         
C iset = 1 : NLO
C x    = longitudinal-momentum fraction
C qs   = fragmentation scale (in GeV)
C Parton label:
C 0    1    2    3    4    5    6    7    8     9    10
C g    u   ubar  d   dbar  s   sbar  c   cbar   b   bbar
C Lambda_QCD (in GeV):
C     0.108 in LO
C     0.227 in NLO
C
C Mass-thresholds:
        rmcc=    2.9788d0
        rmbb=    9.46037d0
C Q_0 (in GeV):
        q0=  sqrt(2.D0)
C BAK
        XTMP=X
        QSTMP=QS
        IF (X.LT.0.05D0) X=0.05D0
        IF (X.GT.1D0) THEN
           PRINT*, 'X GREATER THAN ONE'
           STOP
        ENDIF
        IF (QS.LT.Q0) QS=Q0  
        IF (QS.GT.200) QS=200.D0
        if (iset.eq.0) then
          rlam=    0.108d0
          s=  DLOG(DLOG(qs**2/rlam**2)/DLOG(q0**2/rlam**2))
          sc=  DLOG(DLOG(qs**2/rlam**2)/DLOG(rmcc**2/rlam**2))
          sb=  DLOG(DLOG(qs**2/rlam**2)/DLOG(rmbb**2/rlam**2))
           
      dpg=  (+6.570-9.142*s +2.431*s**2 +0.681*s**3)*
     &       x**(  -.460  -.118*s  +0.530*s**2 -0.720*s**3)*
     &       (1.0-x)**( +3.010 +1.924*s +0.247*s**2-0.731*s**3)
     &       *(1.0+(  +.256*s  +0.458*s**2)/x)
      dpu=  (+1.090-1.198*s  +.757*s**2  -.179*s**3)*
     &       x**(  -.850 -1.007*s  +.434*s**2 )*
     &       (1.0-x)**( +1.440  +.321*s  +.176*s**2)
      dps=  (+3.480-2.463*s  +.523*s**2 )*
     &       x**( -1.030  -.362*s  +.030*s**2 )*
     &       (1.0-x)**( +3.900  +.833*s  -.056*s**2 )
      dpc=  (+4.510-3.933*sc +1.156*sc**2 )*
     &       x**(  -.860  -.441*sc  +.043*sc**2 )*
     &       (1.0-x)**( +4.530  +.752*sc  -.040*sc**2 )
      dpb=  (+3.600-6.873*sb+8.697*sb**2 -5.258*sb**3)*
     &       x**( -1.120 -0.743*sb +.367*sb**2 +0.075*sb**3)*
     &       (1.0-x)**( +7.120  -.596*sb  
     &                       +1.698*sb**2 -0.759*sb**3)
      dkg=  ( +.370 -.872*s  +.797*s**2  -.272*s**3)*
     &       x**(  -.210  -.280*s  -.246*s**2 )*
     &       (1.0-x)**( +3.070  +.910*s  -.422*s**2 )
     &       *(1.0+(  +.835*s +1.279*s**2)/x)
      dku=  ( +.380 -.117*s  -.010*s**2 )*
     &       x**( -1.230  -.080*s  -.039*s**2 )*
     &       (1.0-x)**( +1.060  +.716*s  -.043*s**2 )
      dkd=  (+1.120 -.669*s  +.104*s**2 )*
     &       x**(  -.920  -.210*s  -.034*s**2 )*
     &       (1.0-x)**( +2.850  +.851*s  -.090*s**2 )
      dkc=  ( +.620 -.414*sc  +.089*sc**2 )*
     &       x**(  -.670  -.364*sc  +.008*sc**2 )*
     &       (1.0-x)**( +2.480  +.700*sc  -.029*sc**2 )
      dkb=  ( +.730 -.498*sb  +.123*sb**2 )*
     &       x**(  -.800  -.311*sb  +.012*sb**2 )*
     &       (1.0-x)**( +2.830  +.710*sb  +.012*sb**2 )
      dk0g=  ( +.370 -.879*s  +.971*s**2  -.398*s**3)*
     &       x**( -.210 -2.857*s +2.094*s**2 -.604*s**3)*
     &       (1.0-x)**( +3.070 +1.356*s  -.584*s**2 )
      dk0u=  ( +.540 -.218*s )*
     &       x**( -0.770  -.245*s )*
     &       (1.0-x)**( +1.490  +.774*s  -.091*s**2 )
      dk0d=  (+1.540 -1.096*s  +.229*s**2 )*
     &       x**(  -.720  -.227*s  -.032*s**2 )*
     &       (1.0-x)**( +3.70  +.878*s  -.109*s**2 )
      dk0c=  ( +1.130 -.762*sc  +.168*sc**2 )*
     &       x**(  -.700  -.303*sc )*
     &       (1.0-x)**( +3.020  +.763*sc  -.034*sc**2 )
      dk0b=  ( +.640 -.379*sb  +.084*sb**2 )*
     &       x**(  -.630  -.355*sb  +.041*sb**2 )*
     &       (1.0-x)**( +1.840  +.621*sb  +.025*sb**2 )

        else
          if (iset.ne.1) then
            write(*,*) 'iset must be 0 (for LO) or 1 (for NLO).'
          endif
          rlam=    0.227d0
          s=  DLOG(DLOG(qs**2/rlam**2)/DLOG(q0**2/rlam**2))
          sc=  DLOG(DLOG(qs**2/rlam**2)/DLOG(rmcc**2/rlam**2))
          sb=  DLOG(DLOG(qs**2/rlam**2)/DLOG(rmbb**2/rlam**2))

      dpg=  (+5.530-9.228*s+5.192*s**2 -.966*s**3)*
     &       x**( -.320 +.318*s -.561*s**2 )*
     &       (1.0-x)**(+2.700+2.553*s -.907*s**2 )
     &       *(1.0+( +.751*s +.496*s**2)/x)
      dpu=  (+1.150-1.522*s+1.378*s**2 -.527*s**3)*
     &       x**( -.740-1.680*s+1.546*s**2 -.596*s**3)*
     &       (1.0-x)**(+1.430 +.543*s -.023*s**2 )
      dps=  (+4.250-3.147*s +.755*s**2 )*
     &       x**( -.770 -.573*s +.117*s**2 )*
     &       (1.0-x)**(+4.480 +.890*s -.138*s**2 )
      dpc=  (+3.990-3.321*sc +.925*sc**2 )*
     &       x**( -.790 -.520*sc +.081*sc**2 )*
     &       (1.0-x)**(+4.780 +.716*sc -.075*sc**2 )
      dpb=  (+4.010-13.199*sb+18.208*sb**2-6.536*sb**3)*
     &       x**(-1.030-1.931*sb-1.372*sb**2+5.020*sb**3)*
     &       (1.0-x)**(+7.860-2.224*sb+0.206*sb**2+4.215*sb**3)
     &       *(1.0+( -.179*sb +.205*sb**2)/x)
      dkg=  ( +.310 -.325*s  +.092*s**2 )*
     &       x**(  -.170  -.214*s  -.184*s**2 )*
     &       (1.0-x)**(  +.890 +2.185*s  -.471*s**2 )
     &       *(1.0+( +1.154*s  -.026*s**2)/x)
      dku=  ( +.310 -.038*s  -.042*s**2 )*
     &       x**(  -.980  -.260*s  +.008*s**2 )*
     &       (1.0-x)**(  +.970  +.978*s  -.229*s**2 )
      dkd=  (+1.080 -.469*s  +.003*s**2 )*
     &       x**(  -.820  -.240*s  -.035*s**2 )*
     &       (1.0-x)**( +2.550 +1.026*s  -.246*s**2 )
      dkc=  ( +.810 -.493*sc  +.089*sc**2 )*
     &       x**(  -.690  -.413*sc  +.042*sc**2 )*
     &       (1.0-x)**( +2.980  +.686*sc  -.045*sc**2 )
      dkb=  ( +.610 -.378*sb  +.070*sb**2 )*
     &       x**(  -.880  -.348*sb  +.031*sb**2 )*
     &       (1.0-x)**( +2.930  +.638*sb  +.010*sb**2 )
      dk0g=  ( +.310 -.585*s  +.668*s**2 -.28*s**3 )*
     &       x**( -.170 -3.581*s +3.890*s**2 -1.582*s**3)*
     &       (1.0-x)**( +.890 +.965*s +.959*s**2 -.406*s**3)
      dk0u=  ( +.530 -.253*s  +0.033*s**2 )*
     &       x**(  -.570  -.593*s  +.141*s**2 )*
     &       (1.0-x)**(  +1.870  +.892*s  -.148*s**2 )
      dk0d=  (+1.450 -1.694*s  +1.081*s**2 -.287*s**3)*
     &       x**( -.620 -.584*s +.088*s**2 +.038*s**3 )*
     &       (1.0-x)**( +3.840 +.108*s +.272*s**2 )
      dk0c=  ( +1.700 -1.255*sc  +.307*sc**2 )*
     &       x**(  -.510  -.436*sc  +.032*sc**2 )*
     &       (1.0-x)**( +3.760  +.640*sc  -.036*sc**2 )
      dk0b=  ( +.470 -.319*sb  +.109*sb**2 )*
     &       x**(  -.660  -.537*sb  +.155*sb**2 )*
     &       (1.0-x)**( +1.490  +.420*sb  +.082*sb**2 )

        endif
        dpd=   dpu
        dks=   dku
        dk0s=   dk0d
        if (qs.lt.rmbb) then
          dpb= 0.d0
          dkb= 0.d0
          dk0b= 0.d0
        endif
        if (qs.lt.rmcc) then
          dpc= 0.d0
          dkc= 0.d0
          dk0c= 0.d0
        endif
        if (ih.eq.1) then
          dh(0)= dpg/2.d0
          dh(1)= dpu/2.d0
          dh(2)= dpu/2.d0
          dh(3)= dpd/2.d0
          dh(4)= dpd/2.d0
          dh(5)= dps/2.d0
          dh(6)= dps/2.d0
          dh(7)= dpc/2.d0
          dh(8)= dpc/2.d0
          dh(9)= dpb/2.d0
          dh(10)=dpb/2.d0
        else if (ih.eq.2) then
          dh(0)= dkg/2.d0
          dh(1)= dku/2.d0
          dh(2)= dku/2.d0
          dh(3)= dkd/2.d0
          dh(4)= dkd/2.d0
          dh(5)= dks/2.d0
          dh(6)= dks/2.d0
          dh(7)= dkc/2.d0
          dh(8)= dkc/2.d0
          dh(9)= dkb/2.d0
          dh(10)=dkb/2.d0
        else if (ih.eq.3) then
          dh(0)= dk0g/2.d0
          dh(1)= dk0u/2.d0
          dh(2)= dk0u/2.d0
          dh(3)= dk0d/2.d0
          dh(4)= dk0d/2.d0
          dh(5)= dk0s/2.d0
          dh(6)= dk0s/2.d0
          dh(7)= dk0c/2.d0
          dh(8)= dk0c/2.d0
          dh(9)= dk0b/2.d0
          dh(10)=dk0b/2.d0
        else if (ih.eq.5) then
          dh(0)= dpg/2.d0
          dh(1)= dpu/2.d0
          dh(2)= dpu/2.d0
          dh(3)= dpd/2.d0
          dh(4)= dpd/2.d0
          dh(5)= dps/2.d0
          dh(6)= dps/2.d0
          dh(7)= dpc/2.d0
          dh(8)= dpc/2.d0
          dh(9)= dpb/2.d0
          dh(10)=dpb/2.d0
        else
          dh(0)= dpg+dkg
          dh(1)= dpu+dku
          dh(2)= dpu+dku
          dh(3)= dpd+dkd
          dh(4)= dpd+dkd
          dh(5)= dps+dks
          dh(6)= dps+dks
          dh(7)= dpc+dkc
          dh(8)= dpc+dkc
          dh(9)= dpb+dkb
          dh(10)=dpb+dkb
        end if
        X=XTMP
        QS=QSTMP
      return
      end
