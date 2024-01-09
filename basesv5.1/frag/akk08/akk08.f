c----------------------------------------------------------------------

c     AKK 2008 WRAPPER
c
c F. ARLEO
c
c SAME AS AKK08 BUT GIVES DIRECTLY THE INDIVIDUAL FLAVOUR-TAGGED FF 
c INSTEAD OF THE DIFFERENCE
c----------------------------------------------------------------------
      SUBROUTINE AKK08wrap(IH,Z,Q,DH)
      IMPLICIT REAL*8 (A-H,M-Z)
      DIMENSION DH(0:10),DHSUM(0:10),DHDIFF(0:10)
C FOR IH<100 AKK08wrap=AKK08
c 101 h^+
c 102 h^-
c 111 pi^+
c 112 pi^-
c 121 K^+
c 122 K^-
c 131 p
c 132 anti-p

      if(ih.le.100)then
         CALL AKK08(IH,Z,Q,DH)
      else
         if((ih.eq.101).or.(ih.eq.102))then
            CALL AKK08(9,Z,Q,DHSUM)
            CALL AKK08(10,Z,Q,DHDIFF)
            do i=1,10
               if(ih.eq.101)DH(i)=0.5D0*(DHSUM(i)+DHDIFF(i))
               if(ih.eq.102)DH(i)=0.5D0*(DHSUM(i)-DHDIFF(i))
            enddo
         elseif((ih.eq.111).or.(ih.eq.112))then
            CALL AKK08(1,Z,Q,DHSUM)
            CALL AKK08(11,Z,Q,DHDIFF)
            do i=1,10
               if(ih.eq.111)DH(i)=0.5D0*(DHSUM(i)+DHDIFF(i))
               if(ih.eq.112)DH(i)=0.5D0*(DHSUM(i)-DHDIFF(i))
            enddo
         elseif((ih.eq.121).or.(ih.eq.122))then
            CALL AKK08(2,Z,Q,DHSUM)
            CALL AKK08(12,Z,Q,DHDIFF)
            do i=1,10
               if(ih.eq.121)DH(i)=0.5D0*(DHSUM(i)+DHDIFF(i))
               if(ih.eq.122)DH(i)=0.5D0*(DHSUM(i)-DHDIFF(i))
            enddo
         elseif((ih.eq.131).or.(ih.eq.132))then
            CALL AKK08(3,Z,Q,DHSUM)
            CALL AKK08(13,Z,Q,DHDIFF)
            do i=1,10
               if(ih.eq.131)DH(i)=0.5D0*(DHSUM(i)+DHDIFF(i))
               if(ih.eq.132)DH(i)=0.5D0*(DHSUM(i)-DHDIFF(i))
            enddo
         endif
      endif

      return
      end

c----------------------------------------------------------------------

c     AKK ROUTINES 2008

c----------------------------------------------------------------------

      SUBROUTINE AKK08(IH,Z,Q,DH)

C Input:

C IH hadron species
C***************************************************************
C*************** CHARGE-SIGN UNIDENTIFIED FFS ******************
C***************************************************************
C  1  pi^+ + pi^-
C  2  K^+ + K^-
C  3  p + anti-p
C  4  K0short
C  5  Lambda+anti-Lambda
C  6  pi^0, calculated from (pi^+ + pi^-)/2
C  7  K0short, calculated from (K^+ + K^-)/2 with u<->d
C  8  n + anti-n, calculated from p + anti-p with u<->d
C  9  h^+ + h^- (= pi^+ + pi^- + K^+ + K^- + p + anti-p)
C***************************************************************
C************** CHARGE-SIGN ASYMMETRY FFS **********************
C***************************************************************
C 10  h^+ - h^- (= pi^+ - pi^- + K^+ - K^- + p - anti-p)
C 11  pi^+ - pi^-
C 12  K^+ - K^-
C 13  p - anti-p

C Z: longitudinal-momentum fraction

C Q: fragmentation scale (in GeV)

C Output:

C DH(i): fragmentation function of parton i
C 0  1    2    3    4    5    6    7    8    9   10
C g  u  u-bar  d  d-bar  s  s-bar  c  c-bar  b  b-bar

      IMPLICIT REAL*8 (A-H,M-Z)
      DIMENSION DH(0:10),FF(0:5)
      DIMENSION FF_pion(0:5),FF_kaon(0:5),FF_proton(0:5)
      Q2=Q*Q

      IF (IH.EQ.1) THEN
      call akk08_pion(z,Q2,FF)
      DH(0)=FF(0)
      DH(1)=FF(1)
      DH(2)=DH(1)
      DH(3)=FF(2)
      DH(4)=DH(3)
      DH(5)=FF(3)
      DH(6)=DH(5)
      DH(7)=FF(4)
      DH(8)=DH(7)
      DH(9)=FF(5)
      DH(10)=DH(9)
      ELSE IF (IH.EQ.2) THEN
      call akk08_kaon(z,Q2,FF)
      DH(0)=FF(0)
      DH(1)=FF(1)
      DH(2)=DH(1)
      DH(3)=FF(2)
      DH(4)=DH(3)
      DH(5)=FF(3)
      DH(6)=DH(5)
      DH(7)=FF(4)
      DH(8)=DH(7)
      DH(9)=FF(5)
      DH(10)=DH(9)
      ELSE IF (IH.EQ.3) THEN
      call akk08_proton(z,Q2,FF)
      DH(0)=FF(0)
      DH(1)=FF(1)
      DH(2)=DH(1)
      DH(3)=FF(2)
      DH(4)=DH(3)
      DH(5)=FF(3)
      DH(6)=DH(5)
      DH(7)=FF(4)
      DH(8)=DH(7)
      DH(9)=FF(5)
      DH(10)=DH(9)
      ELSE IF (IH.EQ.4) THEN
      call akk08_K0S(z,Q2,FF)
      DH(0)=FF(0)
      DH(1)=FF(1)
      DH(2)=DH(1)
      DH(3)=FF(2)
      DH(4)=DH(3)
      DH(5)=FF(3)
      DH(6)=DH(5)
      DH(7)=FF(4)
      DH(8)=DH(7)
      DH(9)=FF(5)
      DH(10)=DH(9)
      ELSE IF (IH.EQ.5) THEN
      call akk08_Lambda(z,Q2,FF)
      DH(0)=FF(0)
      DH(1)=FF(1)
      DH(2)=DH(1)
      DH(3)=FF(2)
      DH(4)=DH(3)
      DH(5)=FF(3)
      DH(6)=DH(5)
      DH(7)=FF(4)
      DH(8)=DH(7)
      DH(9)=FF(5)
      DH(10)=DH(9)
      ELSE IF (IH.EQ.6) THEN
      call akk08_pion(z,Q2,FF)
      DH(0)=FF(0)/2.D0
      DH(1)=FF(1)/2.D0
      DH(2)=DH(1)
      DH(3)=FF(2)/2.D0
      DH(4)=DH(3)
      DH(5)=FF(3)/2.D0
      DH(6)=DH(5)
      DH(7)=FF(4)/2.D0
      DH(8)=DH(7)
      DH(9)=FF(5)/2.D0
      DH(10)=DH(9)
      ELSE IF (IH.EQ.7) THEN
      call akk08_kaon(z,Q2,FF)
      DH(0)=FF(0)/2.D0
      DH(1)=FF(2)/2.D0
      DH(2)=DH(1)
      DH(3)=FF(1)/2.D0
      DH(4)=DH(3)
      DH(5)=FF(3)/2.D0
      DH(6)=DH(5)
      DH(7)=FF(4)/2.D0
      DH(8)=DH(7)
      DH(9)=FF(5)/2.D0
      DH(10)=DH(9)
      ELSE IF (IH.EQ.8) THEN
      call akk08_proton(z,Q2,FF)
      DH(0)=FF(0)
      DH(1)=FF(2)
      DH(2)=DH(1)
      DH(3)=FF(1)
      DH(4)=DH(3)
      DH(5)=FF(3)
      DH(6)=DH(5)
      DH(7)=FF(4)
      DH(8)=DH(7)
      DH(9)=FF(5)
      DH(10)=DH(9)
      ELSE IF (IH.EQ.9) THEN
      call akk08_pion(z,Q2,FF_pion)
      call akk08_kaon(z,Q2,FF_kaon)
      call akk08_proton(z,Q2,FF_proton)
      DH(0)=FF_pion(0)+FF_kaon(0)+FF_proton(0)
      DH(1)=FF_pion(1)+FF_kaon(1)+FF_proton(1)
      DH(2)=DH(1)
      DH(3)=FF_pion(2)+FF_kaon(2)+FF_proton(2)
      DH(4)=DH(3)
      DH(5)=FF_pion(3)+FF_kaon(3)+FF_proton(3)
      DH(6)=DH(5)
      DH(7)=FF_pion(4)+FF_kaon(4)+FF_proton(4)
      DH(8)=DH(7)
      DH(9)=FF_pion(5)+FF_kaon(5)+FF_proton(5)
      DH(10)=DH(9)
      ELSE IF (IH.EQ.10) THEN
      call akk08_pionCSA(z,Q2,FF_pion)
      call akk08_kaonCSA(z,Q2,FF_kaon)
      call akk08_protonCSA(z,Q2,FF_proton)
      DH(0)=FF_pion(0)+FF_kaon(0)+FF_proton(0)
      DH(1)=FF_pion(1)+FF_kaon(1)+FF_proton(1)
      DH(2)=DH(1)
      DH(3)=FF_pion(2)+FF_kaon(2)+FF_proton(2)
      DH(4)=DH(3)
      DH(5)=FF_pion(3)+FF_kaon(3)+FF_proton(3)
      DH(6)=DH(5)
      DH(7)=FF_pion(4)+FF_kaon(4)+FF_proton(4)
      DH(8)=DH(7)
      DH(9)=FF_pion(5)+FF_kaon(5)+FF_proton(5)
      DH(10)=DH(9)
      ELSE IF (IH.EQ.11) THEN
      call akk08_pionCSA(z,Q2,FF)
      DH(0)=0.d0
      DH(1)=FF(1)
      DH(2)=-DH(1)
      DH(3)=FF(2)
      DH(4)=-DH(3)
      DH(5)=0.d0
      DH(6)=0.d0
      DH(7)=0.d0
      DH(8)=0.d0
      DH(9)=0.d0
      DH(10)=0.d0
      ELSE IF (IH.EQ.12) THEN
      call akk08_kaonCSA(z,Q2,FF)
      DH(0)=0.d0
      DH(1)=FF(1)
      DH(2)=-DH(1)
      DH(3)=0.d0
      DH(4)=0.d0
      write(6,*)'Warning for s->K+/- CSA: SU(3) used !!'
      DH(5)=-DH(1)
      DH(6)=-DH(2)
      DH(7)=0.d0
      DH(8)=0.d0
      DH(9)=0.d0
      DH(10)=0.d0
      ELSE IF (IH.EQ.13) THEN
      call akk08_protonCSA(z,Q2,FF)
      DH(0)=0.d0
      DH(1)=FF(1)
      DH(2)=-DH(1)
      DH(3)=FF(2)
      DH(4)=-DH(3)
      DH(5)=0.d0
      DH(6)=0.d0
      DH(7)=0.d0
      DH(8)=0.d0
      DH(9)=0.d0
      DH(10)=0.d0
      END IF
      RETURN
      END

c---------------------------------------------------------------------

c     MORE DETAILS ON USING AKK ROUTINES (FOR USER'S INTEREST ONLY)

c     The subroutines akk08_ff(x,Q2,FF), for ff=pion, kaon and proton
c     (summed over charges) return the AKK FFs (obtained from a NLO
c     MSbar scheme fit) at the momentum fraction x and factorisation
c     scale squared Q2 (GeV^2) in the array FF(0:5), whose components
c     have the following meanings:

c     FF(0) = gluon
c     FF(1) = down q    = down qbar
c     FF(2) = up q      = up qbar
c     FF(3) = strange q = strange qbar
c     FF(4) = charm q   = charm qbar
c     FF(5) = bottom q  = bottom qbar

c     Note: For a given flavour, the quark FF is equal to the antiquark
c     FF, since the FF for emission of a charged summed hadron from a
c     quark is equal to that from its antiquark, by charge conjugation
c     invariance.

c     Note: The charm and bottom FFs will be zero when Q is less than
c     their initial scales 1.65 and 4.85 GeV respectively. There is no
c     top quark FF.

c     Likewise, the subroutines akk08_ffCSA(x,Q2,FF), for ff=pion, kaon
c     and proton return the AKK charge-sign assymetry FFs, being in each
c     case the FF for the particle with positive charge minus the FF for
c     the particle with negative charge. In the case of charged kaons,
c     the strange q is set to zero because it is not constrained by any
c     data or reliable theory.

c     The subroutine akk08_lambdaQCD(lambdaQCD) returns the QCD
c     LambdaQCD's for different numbers of active quark flavours
c     nf=3,4,5, calculated from the CTEQ6S.0 lambdaQCD = 226 MeV for 5
c     flavours, in the variable lambdaQCD(nf).

c     The subroutine akk08_unit finds a valid, unused unit number with
c     which the akk routines will then read in from the akk data files.

c     All remaining routines that appear in this file below the
c     subroutines akk08_ff, akk08_lambdaQCD and akk08_unit described above
c     must also be included, but the user never needs to call them
c     explicitly.

c---------------------------------------------------------------------

c     Returns all 6 pion FFs from AKK fit evaluated at (x,Q2)

      subroutine akk08_pion(x,Q2,FFpion)

      implicit none

      real*8 x,Q2,FFpion(0:5)
      integer i,j

      integer nx,nQ2
      parameter(nx=500,nQ2=250)

      real*8 FFpion_a(nQ2,nx,0:5)
      common/FF_interp_pion/FFpion_a
      real*8 bpion_a(nQ2,nx,0:5),cpion_a(nQ2,nx,0:5),
     .     dpion_a(nQ2,nx,0:5)
      common/splines_pion/bpion_a,cpion_a,dpion_a

      integer precalc_done
      save precalc_done

      integer grid_read
      common/grid_read_com/grid_read

      if(precalc_done.ne.2375)then         
         call akk08_unit
         open(unit=grid_read,file='akk08/grid_pion',
     .        status='old')
         do i=1,nQ2
            do j=1,nx
               read(grid_read,'(6e14.6)')
     .              FFpion_a(i,j,0),
     .              FFpion_a(i,j,1),
     .              FFpion_a(i,j,2),FFpion_a(i,j,3),
     .              FFpion_a(i,j,4),FFpion_a(i,j,5)
            enddo
         enddo
         close(grid_read)
         call spline_fit_akk08(FFpion_a,bpion_a,cpion_a,
     .        dpion_a)
         precalc_done=2375
      endif
      call getFFs_akk08(x,Q2,FFpion,FFpion_a,bpion_a,
     .     cpion_a,dpion_a)
      
      end 

c---------------------------------------------------------------------

c     Returns all 6 kaon FFs from AKK fit evaluated at (x,Q2)

      subroutine akk08_kaon(x,Q2,FFkaon)

      implicit none

      real*8 x,Q2,FFkaon(0:5)
      integer i,j

      integer nx,nQ2
      parameter(nx=500,nQ2=250)

      real*8 FFkaon_a(nQ2,nx,0:5)
      common/FF_interp_kaon/FFkaon_a
      real*8 bkaon_a(nQ2,nx,0:5),ckaon_a(nQ2,nx,0:5),
     .     dkaon_a(nQ2,nx,0:5)
      common/splines_kaon/bkaon_a,ckaon_a,dkaon_a

      integer precalc_done
      save precalc_done

      integer grid_read
      common/grid_read_com/grid_read

      if(precalc_done.ne.2375)then
         call akk08_unit
         open(unit=grid_read,file='akk08/grid_kaon',
     .        status='old')
         do i=1,nQ2
            do j=1,nx
               read(grid_read,'(6e14.6)')
     .              FFkaon_a(i,j,0),
     .              FFkaon_a(i,j,1),
     .              FFkaon_a(i,j,2),FFkaon_a(i,j,3),
     .              FFkaon_a(i,j,4),FFkaon_a(i,j,5)
            enddo
         enddo
         close(grid_read)
         call spline_fit_akk08(FFkaon_a,bkaon_a,ckaon_a,
     .        dkaon_a)
         precalc_done=2375
      endif
      call getFFs_akk08(x,Q2,FFkaon,FFkaon_a,bkaon_a,
     .     ckaon_a,dkaon_a)
      
      end 

c---------------------------------------------------------------------

c     Returns all 6 proton FFs from AKK fit evaluated at (x,Q2)

      subroutine akk08_proton(x,Q2,FFproton)

      implicit none

      real*8 x,Q2,FFproton(0:5)
      integer i,j

      integer nx,nQ2
      parameter(nx=500,nQ2=250)

      real*8 FFproton_a(nQ2,nx,0:5)
      common/FF_interp_proton/FFproton_a
      real*8 bproton_a(nQ2,nx,0:5),
     .     cproton_a(nQ2,nx,0:5),
     .     dproton_a(nQ2,nx,0:5)
      common/splines_proton/bproton_a,
     .     cproton_a,dproton_a

      integer precalc_done
      save precalc_done

      integer grid_read
      common/grid_read_com/grid_read

      if(precalc_done.ne.2375)then
         call akk08_unit
         open(unit=grid_read,file='akk08/grid_proton'
     .        ,status='old')
         do i=1,nQ2
            do j=1,nx
               read(grid_read,'(6e14.6)')
     .              FFproton_a(i,j,0),
     .              FFproton_a(i,j,1),
     .              FFproton_a(i,j,2),
     .              FFproton_a(i,j,3),
     .              FFproton_a(i,j,4),
     .              FFproton_a(i,j,5)
            enddo
         enddo
         close(grid_read)
         call spline_fit_akk08(FFproton_a,bproton_a,
     .        cproton_a,dproton_a)
         precalc_done=2375
      endif
      call getFFs_akk08(x,Q2,FFproton,FFproton_a,
     .     bproton_a,cproton_a,dproton_a)
      
      end

c---------------------------------------------------------------------

c     Returns all 6 pionCSA FFs from AKK fit evaluated at (x,Q2)

      subroutine akk08_pionCSA(x,Q2,FFpionCSA)

      implicit none

      real*8 x,Q2,FFpionCSA(0:5)
      integer i,j

      integer nx,nQ2
      parameter(nx=500,nQ2=250)

      real*8 FFpionCSA_a(nQ2,nx,0:5)
      common/FF_interp_pionCSA/FFpionCSA_a
      real*8 bpionCSA_a(nQ2,nx,0:5),cpionCSA_a(nQ2,nx,0:5),
     .     dpionCSA_a(nQ2,nx,0:5)
      common/splines_pionCSA/bpionCSA_a,cpionCSA_a,dpionCSA_a

      integer precalc_done
      save precalc_done

      integer grid_read
      common/grid_read_com/grid_read

      if(precalc_done.ne.2375)then         
         call akk08_unit
         open(unit=grid_read,file='akk08/grid_pionCSA',
     .        status='old')
         do i=1,nQ2
            do j=1,nx
               read(grid_read,'(6e14.6)')
     .              FFpionCSA_a(i,j,0),
     .              FFpionCSA_a(i,j,1),
     .              FFpionCSA_a(i,j,2),FFpionCSA_a(i,j,3),
     .              FFpionCSA_a(i,j,4),FFpionCSA_a(i,j,5)
            enddo
         enddo
         close(grid_read)
         call spline_fit_akk08(FFpionCSA_a,bpionCSA_a,cpionCSA_a,
     .        dpionCSA_a)
         precalc_done=2375
      endif
      call getFFs_akk08(x,Q2,FFpionCSA,FFpionCSA_a,bpionCSA_a,
     .     cpionCSA_a,dpionCSA_a)
      
      end 

c---------------------------------------------------------------------

c     Returns all 6 kaonCSA FFs from AKK fit evaluated at (x,Q2)

      subroutine akk08_kaonCSA(x,Q2,FFkaonCSA)

      implicit none

      real*8 x,Q2,FFkaonCSA(0:5)
      integer i,j

      integer nx,nQ2
      parameter(nx=500,nQ2=250)

      real*8 FFkaonCSA_a(nQ2,nx,0:5)
      common/FF_interp_kaonCSA/FFkaonCSA_a
      real*8 bkaonCSA_a(nQ2,nx,0:5),ckaonCSA_a(nQ2,nx,0:5),
     .     dkaonCSA_a(nQ2,nx,0:5)
      common/splines_kaonCSA/bkaonCSA_a,ckaonCSA_a,dkaonCSA_a

      integer precalc_done
      save precalc_done

      integer grid_read
      common/grid_read_com/grid_read

      if(precalc_done.ne.2375)then
         call akk08_unit
         open(unit=grid_read,file='akk08/grid_kaonCSA',
     .        status='old')
         do i=1,nQ2
            do j=1,nx
               read(grid_read,'(6e14.6)')
     .              FFkaonCSA_a(i,j,0),
     .              FFkaonCSA_a(i,j,1),
     .              FFkaonCSA_a(i,j,2),FFkaonCSA_a(i,j,3),
     .              FFkaonCSA_a(i,j,4),FFkaonCSA_a(i,j,5)
            enddo
         enddo
         close(grid_read)
         call spline_fit_akk08(FFkaonCSA_a,bkaonCSA_a,ckaonCSA_a,
     .        dkaonCSA_a)
         precalc_done=2375
      endif
      call getFFs_akk08(x,Q2,FFkaonCSA,FFkaonCSA_a,bkaonCSA_a,
     .     ckaonCSA_a,dkaonCSA_a)
      
      end 

c---------------------------------------------------------------------

c     Returns all 6 protonCSA FFs from AKK fit evaluated at (x,Q2)

      subroutine akk08_protonCSA(x,Q2,FFprotonCSA)

      implicit none

      real*8 x,Q2,FFprotonCSA(0:5)
      integer i,j

      integer nx,nQ2
      parameter(nx=500,nQ2=250)

      real*8 FFprotonCSA_a(nQ2,nx,0:5)
      common/FF_interp_protonCSA/FFprotonCSA_a
      real*8 bprotonCSA_a(nQ2,nx,0:5),
     .     cprotonCSA_a(nQ2,nx,0:5),
     .     dprotonCSA_a(nQ2,nx,0:5)
      common/splines_protonCSA/bprotonCSA_a,
     .     cprotonCSA_a,dprotonCSA_a

      integer precalc_done
      save precalc_done

      integer grid_read
      common/grid_read_com/grid_read

      if(precalc_done.ne.2375)then
         call akk08_unit
         open(unit=grid_read,file='akk08/grid_protonCSA'
     .        ,status='old')
         do i=1,nQ2
            do j=1,nx
               read(grid_read,'(6e14.6)')
     .              FFprotonCSA_a(i,j,0),
     .              FFprotonCSA_a(i,j,1),
     .              FFprotonCSA_a(i,j,2),
     .              FFprotonCSA_a(i,j,3),
     .              FFprotonCSA_a(i,j,4),
     .              FFprotonCSA_a(i,j,5)
            enddo
         enddo
         close(grid_read)
         call spline_fit_akk08(FFprotonCSA_a,bprotonCSA_a,
     .        cprotonCSA_a,dprotonCSA_a)
         precalc_done=2375
      endif
      call getFFs_akk08(x,Q2,FFprotonCSA,FFprotonCSA_a,
     .     bprotonCSA_a,cprotonCSA_a,dprotonCSA_a)
      
      end

c------------------------------------------------------------------------

c     Returns all 6 K0S FFs from AKK fit evaluated at (x,Q2)

      subroutine akk08_K0S(x,Q2,FFK0S)

      implicit none

      real*8 x,Q2,FFK0S(0:5)
      integer i,j

      integer nx,nQ2
      parameter(nx=500,nQ2=250)

      real*8 FFK0S_a(nQ2,nx,0:5)
      common/FF_interp_K0S/FFK0S_a
      real*8 bK0S_a(nQ2,nx,0:5),cK0S_a(nQ2,nx,0:5),
     .     dK0S_a(nQ2,nx,0:5)
      common/splines_K0S/bK0S_a,cK0S_a,dK0S_a

      integer precalc_done
      save precalc_done

      integer grid_read
      common/grid_read_com/grid_read

      if(precalc_done.ne.2375)then
         call akk08_unit
         open(unit=grid_read,file='akk08/grid_K0S',
     .        status='old')
         do i=1,nQ2
            do j=1,nx
               read(grid_read,'(6e14.6)')
     .              FFK0S_a(i,j,0),
     .              FFK0S_a(i,j,1),
     .              FFK0S_a(i,j,2),FFK0S_a(i,j,3),
     .              FFK0S_a(i,j,4),FFK0S_a(i,j,5)
            enddo
         enddo
         close(grid_read)
         call spline_fit_akk08(FFK0S_a,bK0S_a,
     .        cK0S_a,dK0S_a)
         precalc_done=2375
      endif
      call getFFs_akk08(x,Q2,FFK0S,FFK0S_a,bK0S_a,
     .     cK0S_a,dK0S_a)
      
      end 

c------------------------------------------------------------------------

c     Returns all 6 Lambda FFs from AKK fit evaluated at (x,Q2)

      subroutine akk08_Lambda(x,Q2,FFLambda)

      implicit none

      real*8 x,Q2,FFLambda(0:5)
      integer i,j

      integer nx,nQ2
      parameter(nx=500,nQ2=250)

      real*8 FFLambda_a(nQ2,nx,0:5)
      common/FF_interp_Lambda/FFLambda_a
      real*8 bLambda_a(nQ2,nx,0:5),
     .     cLambda_a(nQ2,nx,0:5),
     .     dLambda_a(nQ2,nx,0:5)
      common/splines_Lambda/bLambda_a,
     .     cLambda_a,dLambda_a

      integer precalc_done
      save precalc_done

      integer grid_read
      common/grid_read_com/grid_read

      if(precalc_done.ne.2375)then
         call akk08_unit
         open(unit=grid_read,file='akk08/grid_Lambda',
     .        status='old')
         do i=1,nQ2
            do j=1,nx
               read(grid_read,'(6e14.6)')
     .              FFLambda_a(i,j,0),
     .              FFLambda_a(i,j,1),
     .              FFLambda_a(i,j,2),
     .              FFLambda_a(i,j,3),
     .              FFLambda_a(i,j,4),
     .              FFLambda_a(i,j,5)
            enddo
         enddo
         close(grid_read)
         call spline_fit_akk08(FFLambda_a,bLambda_a,
     .        cLambda_a,dLambda_a)
         precalc_done=2375
      endif
      call getFFs_akk08(x,Q2,FFLambda,FFLambda_a,
     .     bLambda_a,cLambda_a,dLambda_a)
      
      end 

c------------------------------------------------------------------------

      subroutine akk08_lambdaQCD(lambdaQCD)

      implicit none

      real*8 lambdaQCD(3:5)

      integer grid_read
      common/grid_read_com/grid_read

      call akk08_unit
      open(unit=grid_read,file='akk08/lambdaQCD',
     .     status='old')
      read(grid_read,'(3e14.6)')lambdaQCD(3),
     .     lambdaQCD(4),lambdaQCD(5)
      close(grid_read)

      end 

c-------------------------------------------------------------------------

      subroutine akk08_unit

      implicit none

      integer unit_

      integer grid_read
      common/grid_read_com/grid_read

      logical opened_

      do unit_=10,300
         inquire (unit=unit_,opened=opened_)
         if(.not.opened_)then
            grid_read=unit_
            return
         endif
      enddo
      write(6,*)'There is no available I/O unit.'
      stop 

      end

c------------------------------------------------------------------------

      subroutine akk08_x_Q2_grid

      implicit none

      integer i

      integer nx,nQ2
      parameter(nx=500,nQ2=250)

      real*8 x_a(nx),Q2_a(nQ2)
      common/x_Q2_grid/x_a,Q2_a

      integer grid_read
      common/grid_read_com/grid_read

      open(unit=grid_read,file='akk08/Q2_grid',
     .     status='old')
      do i=1,nQ2-4,5
         read(grid_read,'(5e14.6)')Q2_a(i),
     .        Q2_a(i+1),Q2_a(i+2),
     .        Q2_a(i+3),Q2_a(i+4)
      enddo
      close(grid_read)

      open(unit=grid_read,file='akk08/x_grid',
     .     status='old')
      do i=1,nx-4,5
         read(grid_read,'(5e14.6)')x_a(i),
     .        x_a(i+1),x_a(i+2),
     .        x_a(i+3),x_a(i+4)
      enddo
      close(grid_read)

      end 

c-------------------------------------------------------------------------

c Obtains FFs at Q2 by interpolation and at x from a,b,c,d (obtained from spline fitting)

      subroutine getFFs_akk08(x,Q2,FF,FF_a,b_a,c_a,d_a)

      implicit none

      integer xpos,Q2pos,i,iabs,ifail

      integer nx,nQ2
      parameter(nx=500,nQ2=250)

      real*8 x_a(nx),Q2_a(nQ2)
      common/x_Q2_grid/x_a,Q2_a

      real*8 x,FF(0:5),tq,dx,FF1,FF2,Q2,
     .     FF_a(nQ2,nx,0:5),b_a(nQ2,nx,0:5),
     .     c_a(nQ2,nx,0:5),d_a(nQ2,nx,0:5)

 10   call neighbours_akk08(x,x_a,nx,xpos,ifail)
      if(ifail.eq.1)then
         print*, 
     .        'Error: z = ',x,
     .        ', must be in the range ',x_a(1),
     .        ' --- ',x_a(nx)
         print*, 'WARNING: FF frozen at the boundaries',
     #              x,' not in [',x_a(1),',',x_a(nx),']'
         if(x.lt.x_a(1))x=x_a(1)
         if(x.gt.x_a(nx))x=x_a(nx)
         goto 10
      endif

 20   call neighbours_akk08(Q2,Q2_a,nQ2,Q2pos,ifail)
      if(ifail.eq.1)then
         write(6,'(A,f5.1,A,f3.1,A,f5.1,A)')
     .        'Error: Q = ',dsqrt(Q2),
     .        ' GeV, must be in range ',dsqrt(Q2_a(1)),
     .        ' --- ',dsqrt(Q2_a(nQ2)),' GeV'
         print*, 'WARNING: FF frozen at the boundaries',
     #              Q2,' not in [',Q2_a(nQ2),',',Q2_a(nQ2),']'
         if(q2.lt.q2_a(1))q2=q2_a(1)
         if(q2.gt.q2_a(nq2))q2=q2_a(nq2)
         goto 20
      endif

      do i=0,5
         iabs=i
         if(iabs.lt.0)iabs=-iabs
         dx=x-x_a(xpos)
         FF1=FF_a(Q2pos,xpos,iabs)
     .        +dx*(b_a(Q2pos,xpos,iabs)
     .        +dx*(c_a(Q2pos,xpos,iabs)
     .        +dx*d_a(Q2pos,xpos,iabs)))
         FF2=FF_a(Q2pos+1,xpos,iabs)
     .        +dx*(b_a(Q2pos+1,xpos,iabs)
     .        +dx*(c_a(Q2pos+1,xpos,iabs)
     .        +dx*d_a(Q2pos+1,xpos,iabs)))
         tq=(dlog(Q2)-dlog(Q2_a(Q2pos)))
     .        /(dlog(Q2_a(Q2pos+1))
     .        -dlog(Q2_a(Q2pos)))
         FF(i)=(1.d0-tq)*FF1+tq*FF2
      enddo

      end 

c---------------------------------------------------------------------

c     This subroutine finds j where j obeys val_a(j) < val <=
c     val_a(j+1). val_a is an array of size (1:max)     

      subroutine neighbours_akk08(val,val_a,max,j,ifail)

      implicit none

      integer max,j,ifail
      real*8 val,val_a(max)

      ifail=0
      if(val.lt.val_a(1).or.val.gt.val_a(max))then
         ifail=1
      else
         do j=1,max-1
            if(val.ge.val_a(j).and.
     .           val.lt.val_a(j+1))goto 1
         enddo
 1       continue
      endif

      return

      end 

c-------------------------------------------------------------------------

      subroutine spline_fit_akk08(FF_a,b_a,c_a,d_a)

      implicit none

      integer i,j,k

      integer nx,nQ2
      parameter(nx=500,nQ2=250)

      real*8 y(nx),FF_a(nQ2,nx,0:5),b(nx),c(nx),
     .     d(nx),b_a(nQ2,nx,0:5),c_a(nQ2,nx,0:5),
     .     d_a(nQ2,nx,0:5)

      real*8 x_a(nx),Q2_a(nQ2)
      common/x_Q2_grid/x_a,Q2_a

      integer precalc_done
      save precalc_done

      if(precalc_done.ne.2375)then
         call akk08_x_Q2_grid
         precalc_done=2375
      endif
      do i=0,5
         do j=1,nQ2
            do k=1,nx
               y(k)=FF_a(j,k,i)
            enddo
            call spline_akk(nx,x_a,y,b,c,d)
            do k=1,nx
               b_a(j,k,i)=b(k)
               c_a(j,k,i)=c(k)
               d_a(j,k,i)=d(k)
            enddo
         enddo
      enddo

      end 


c-------------------------------------------------------------------------

c     END OF AKK08 ROUTINES

c---------------------------------------------------------------------------

