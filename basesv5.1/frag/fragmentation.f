      subroutine frag_hadron(x,q,ih3,iff,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      implicit real*8 (a-h,l-z)
      COMMON/OUTPUT_UNITS/IO,IER,ITEST  
      IER=6

      if(x.ge.1.)then
	      xdgp=0.D0
	      xdup=0.D0
	      xdubp=0.D0
	      xddp=0.D0
	      xddbp=0.D0
	      xdsp=0.D0
	      xdsbp=0.D0
	      xdcp=0.D0
	      xdcbp=0.D0
	      xdbp=0.D0
	      xdbbp=0.D0
	      xdtp=0.D0
	      xdtbp=0.D0
	      return
      endif

      xsave=x
      qsave=q
      iordersave=iorder

c dummy values
      xdgp=1d-15
      xdup=1d-15
      xdubp=1d-15
      xddp=1d-15
      xddbp=1d-15
      xdsp=1d-15
      xdsbp=1d-15
      xdcp=1d-15
      xdcbp=1d-15
      xdbp=1d-15
      xdbbp=1d-15
      xdtp=1d-15
      xdtbp=1d-15
c end dummy value
c      print*, iff
      if(ih3.eq.70) then
        call frag_neutrals(x,q,iff,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      else
      if(iff.eq.1)call frag_akk08(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      if(iff.eq.2)call frag_dss(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      if(iff.eq.3)call frag_hkns(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      if(iff.eq.4)call frag_akk05(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      if(iff.eq.5)call frag_bkk(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      if(iff.eq.6)call frag_kkp(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      if(iff.eq.7)call frag_kretzer(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      if(iff.eq.8)call frag_bfg(x,q,1,ih3,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      if(iff.eq.9)call frag_bfg(x,q,2,ih3,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      if(iff.eq.10)call frag_bfg(x,q,3,ih3,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      if(iff.eq.13)call frag_kkks08(x,q,ih3,iorder,xdup,xdubp,xddp, ! FF into heavy-quark hadrons
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      if(iff.eq.14)call frag_aesss(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      if(iff.eq.15)call frag_dsv(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      endif

      if(qsave.ne.q)then
         print*, 'The FF scale has been changed!'
         q=qsave
      endif
      if(xsave.ne.x)then
         print*, 'The x has been changed!'
         x=xsave
      endif
      if(iordersave.ne.iorder)then
         print*, 'The order of the calculation has been changed!'
         iorder=iordersave
      endif
      
      return
      end
c --------------------------------------------------------
      subroutine frag_neutrals(x,q,iff,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      implicit real*8 (a-h,l-z)

      call frag_hadron(x,q,12,iff,iorder,xdup1,xdubp1,xddp1,xddbp1,
     #xdsp1,xdsbp1,xdcp1,xdcbp1,xdbp1,xdbbp1,xdtp1,xdtbp1,xdgp1)
      call frag_hadron(x,q,62,14,iorder,xdup2,xdubp2,xddp2,xddbp2,
     #xdsp2,xdsbp2,xdcp2,xdcbp2,xdbp2,xdbbp2,xdtp2,xdtbp2,xdgp2)

      xdup=xdup1+xdup2
      xdubp=xdubp1+xdubp2
      xddp=xddp1+xddp2
      xddbp=xddbp1+xddbp2
      xdsp=xdsp1+xdsp2
      xdsbp=xdsbp1+xdsbp2
      xdcp=xdcp1+xdcp2
      xdcbp=xdcbp1+xdcbp2
      xdbp=xdbp1+xdbp2
      xdbbp=xdbbp1+xdbbp2
      xdtp=xdtp1+xdtp2
      xdtbp=xdtbp1+xdtbp2
      xdgp=xdgp1+xdgp2

      return
      end
c --------------------------------------------------------
      subroutine frag_photon(x,q,ianord,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      implicit real*8 (a-h,l-z)
      QP2=Q*Q
      qp2save=qp2
      CALL distributionPert(x,QP2,'UP',up_Pert)
      CALL distributionPert(x,QP2,'DO',down_Pert)
      CALL distributionPert(x,QP2,'SB',strange_Pert)
      CALL distributionPert(x,QP2,'CB',charm_Pert)
      CALL distributionPert(x,QP2,'BB',bottom_Pert)
      CALL distributionPert(x,QP2,'GL',gluon_Pert)
      if(ianord .eq. 1) then
         CALL distributionNonPert_setI(x,QP2,'UP',up_VDM)
         CALL distributionNonPert_setI(x,QP2,'DO',down_VDM)
         CALL distributionNonPert_setI(x,QP2,'SB',strange_VDM)
         CALL distributionNonPert_setI(x,QP2,'CB',charm_VDM)
         CALL distributionNonPert_setI(x,QP2,'BB',bottom_VDM)
         CALL distributionNonPert_setI(x,QP2,'GL',gluon_VDM)
      elseif(ianord .eq. 2) then
         CALL distributionNonPert_setII(x,QP2,'UP',up_VDM)
         CALL distributionNonPert_setII(x,QP2,'DO',down_VDM)
         CALL distributionNonPert_setII(x,QP2,'SB',strange_VDM)
         CALL distributionNonPert_setII(x,QP2,'CB',charm_VDM)
         CALL distributionNonPert_setII(x,QP2,'BB',bottom_VDM)
         CALL distributionNonPert_setII(x,QP2,'GL',gluon_VDM)
      else
         write(*,*) 'unknown set for photons'
         return
      endif
      if(qp2save.ne.qp2) then
         write(8,*)'scale changed in BFG photon from',qp2save,'to',qp2
      endif
      up = up_Pert + up_VDM
      down = down_Pert + down_VDM
      strange = strange_Pert + strange_VDM
      charm = charm_Pert + charm_VDM
      bottom = bottom_Pert + bottom_VDM
      gluon = gluon_Pert + gluon_VDM
      xup=up*x
      xdown=down*x
      xstrange=strange*x
      xcharm=charm*x
      xbottom=bottom*x
      xglue=gluon*x
c     rajoute pour interface avec hadlib.f
      xdgp = xglue
      xdup = xup
      xdubp = xup
      xddp = xdown
      xddbp = xdown
      xdsp = xstrange
      xdsbp = xstrange
      xdcp = xcharm
      xdcbp = xcharm
      xdbp = xbottom
      xdbbp = xbottom
      xdtp = 0.d0
      xdtbp = 0.d0
      return
      end
c --------------------------------------------------------
      subroutine frag_bfg(x,q,ipi,ih3,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      implicit real*8 (a-h,l-z)
c      dimension dh(0:10)
      COMMON/OUTPUT_UNITS/IO,IER,ITEST  
      if(ih3.eq.41) then
         resc=1.D0              ! (h+ + h-)/2
      elseif(ih3.eq.41) then
         resc=2.D0              ! (h+ + h-)
      else
         write(ier,*) 'This species is not available in BFGW<br>'
        stop
         return
      endif
      call fonfrac(x,ipi,q*q,xdup,xdubp,xddp,xddbp,xdsp,
     # xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp) ! returns number density
      xdgp=resc*xdgp              ! already momentum densities
      xdup=resc*xdup
      xdubp=resc*xdubp
      xddp=resc*xddp
      xddbp=resc*xddbp
      xdsp=resc*xdsp
      xdsbp=resc*xdsbp
      xdcp=resc*xdcp
      xdcbp=resc*xdcbp
      xdbp=resc*xdbp
      xdbbp=resc*xdbbp
      xdtp = 0.d0
      xdtbp = 0.d0
      return
      end
c --------------------------------------------------------
      subroutine frag_kkp(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
c iorder dummy, returns only NLO
      implicit real*8 (a-h,l-z)
      dimension dh(0:10)
      COMMON/OUTPUT_UNITS/IO,IER,ITEST  
      resc=1.D0
      if(ih3.eq.10) then        ! (pi+ + pi-)/2
         ih=1
      elseif(ih3.eq.11) then    ! (pi+ + pi-)
         ih=1
         resc=2.D0
      elseif(ih3.eq.12) then       ! pi0
         ih=5
      elseif(ih3.eq.20) then    ! (K+ + K-)/2
         ih=2
      elseif(ih3.eq.21) then    ! (K+ + K-)
         ih=2
         resc=2.D0
      elseif(ih3.eq.22) then    ! (K0 + K0bar)/2 | K0short
         ih=3
      elseif(ih3.eq.23) then    ! (K0 + K0bar) | 2*K0short
         ih=3
         resc=2.D0
      elseif(ih3.eq.30) then    ! (p + anti-p)/2
         ih=4
      elseif(ih3.eq.31) then    ! (p + anti-p)
         ih=4
         resc=2.D0
      elseif(ih3.eq.40) then    ! (h+ + h-)/2
         ih=7
         resc=0.5D0
      elseif(ih3.eq.41) then    ! (h+ + h-)
         ih=7
      else
         write(ier,*) 'This species is not available in KKP'
        stop
         return
      endif
      call kkp(ih,iorder,x,q,dh) ! returns number density
      xdgp=resc*dh(0)*x              ! converts to momentum density
      xdup=resc*dh(1)*x
      xdubp=resc*dh(2)*x
      xddp=resc*dh(3)*x
      xddbp=resc*dh(4)*x
      xdsp=resc*dh(5)*x
      xdsbp=resc*dh(6)*x
      xdcp=resc*dh(7)*x
      xdcbp=resc*dh(8)*x
      xdbp=resc*dh(9)*x
      xdbbp=resc*dh(10)*x
      xdtp = 0.d0
      xdtbp = 0.d0
      return
      end
c --------------------------------------------------------
      subroutine frag_akk05(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
c iorder dummy, returns only NLO
      implicit real*8 (a-h,l-z)
      dimension dh(0:10)
      COMMON/OUTPUT_UNITS/IO,IER,ITEST  
      resc=1.D0
      if(ih3.eq.10) then        ! (pi+ + pi-)/2
         ih=1
      elseif(ih3.eq.11) then    ! (pi+ + pi-)
         ih=1
         resc=2.D0
      elseif(ih3.eq.12) then       ! pi0
         ih=2
      elseif(ih3.eq.20) then    ! (K+ + K-)/2
         ih=3
      elseif(ih3.eq.21) then    ! (K+ + K-)
         ih=3
         resc=2.D0
      elseif(ih3.eq.22) then    ! (K0 + K0bar)/2 | K0short
         ih=8
      elseif(ih3.eq.23) then    ! (K0 + K0bar) | 2*K0short
         ih=8
         resc=2.D0
      elseif(ih3.eq.30) then    ! (p + anti-p)/2
         ih=5
      elseif(ih3.eq.31) then    ! (p + anti-p)
         ih=3
         resc=2.D0
      elseif(ih3.eq.40) then    ! (h+ + h-)/2
         ih=7
         resc=0.5D0
      elseif(ih3.eq.41) then    ! (h+ + h-)
         ih=7
      elseif(ih3.eq.80) then    ! (Lambda + anti-Lambda)/2
         ih=9
      elseif(ih3.eq.81) then    ! (Lambda + anti-Lambda)
         ih=9
         resc=2.D0
      else
         write(ier,*) 'This species is not available in AKK05'
        stop
         return
      endif
      call akk(ih,iorder,x,q,dh) ! returns number density
      xdgp=resc*dh(0)*x              ! converts to momentum density
      xdup=resc*dh(1)*x
      xdubp=resc*dh(2)*x
      xddp=resc*dh(3)*x
      xddbp=resc*dh(4)*x
      xdsp=resc*dh(5)*x
      xdsbp=resc*dh(6)*x
      xdcp=resc*dh(7)*x
      xdcbp=resc*dh(8)*x
      xdbp=resc*dh(9)*x
      xdbbp=resc*dh(10)*x
      xdtp = 0.d0
      xdtbp = 0.d0
      return
      end
c --------------------------------------------------------
      subroutine frag_kretzer(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      implicit real*8 (a-h,l-z)
      dimension  uff(2), dff(2), sff(2), cff(2), bff(2)
      COMMON/OUTPUT_UNITS/IO,IER,ITEST  
      resc=1.D0
      if(ih3.eq.10) then        ! (pi+ + pi-)/2
         if(iorder.eq.0)iset=1 ! pi LO
         if(iorder.eq.1)iset=2 ! pi NLO
         icharge=3
         resc=0.5D0
      elseif(ih3.eq.11) then    ! (pi+ + pi-)
         if(iorder.eq.0)iset=1 ! pi LO
         if(iorder.eq.1)iset=2 ! pi NLO
         icharge=3
      elseif(ih3.eq.12) then       ! pi0
         if(iorder.eq.0)iset=1 ! pi LO
         if(iorder.eq.1)iset=2 ! pi NLO
         icharge=3
         resc=0.5D0
      elseif(ih3.eq.13) then       ! pi+
         if(iorder.eq.0)iset=1 ! pi LO
         if(iorder.eq.1)iset=2 ! pi NLO
         icharge=1
      elseif(ih3.eq.14) then       ! pi-
         if(iorder.eq.0)iset=1 ! pi LO
         if(iorder.eq.1)iset=2 ! pi NLO
         icharge=2
      elseif(ih3.eq.20) then    ! (K+ + K-)/2
         if(iorder.eq.0)iset=3 ! K LO
         if(iorder.eq.1)iset=4 ! K NLO
         icharge=3
         resc=0.5D0
      elseif(ih3.eq.21) then    ! (K+ + K-)
         if(iorder.eq.0)iset=3 ! K LO
         if(iorder.eq.1)iset=4 ! K NLO
         icharge=3
      elseif(ih3.eq.24) then    ! K+
         if(iorder.eq.0)iset=3 ! K LO
         if(iorder.eq.1)iset=4 ! K NLO
         icharge=1
      elseif(ih3.eq.25) then    ! K-
         if(iorder.eq.0)iset=3 ! K LO
         if(iorder.eq.1)iset=4 ! K NLO
         icharge=2
      elseif(ih3.eq.40) then    ! (h+ + h-)/2
         if(iorder.eq.0)iset=5 ! h LO
         if(iorder.eq.1)iset=6 ! h NLO
         icharge=3
         resc=0.5D0
      elseif(ih3.eq.41) then    ! (h+ + h-)
         if(iorder.eq.0)iset=5 ! h LO
         if(iorder.eq.1)iset=6 ! h NLO
         icharge=3
      elseif(ih3.eq.42) then    ! h+
         if(iorder.eq.0)iset=5 ! h LO
         if(iorder.eq.1)iset=6 ! h NLO
         icharge=1
      elseif(ih3.eq.43) then    ! h-
         if(iorder.eq.0)iset=5 ! h LO
         if(iorder.eq.1)iset=6 ! h NLO
         icharge=2
      else
         write(ier,*) 'This species is not available in Kretzer'
        stop
         return
      endif
      call pkhff(iset,icharge,x,q*q,uff,dff,sff,cff,bff,gff) ! returns number density
      xdup=resc*uff(1)*x             ! converts to momentum density
      xdubp=resc*uff(2)*x
      xddp=resc*dff(1)*x
      xddbp=resc*dff(2)*x
      xdsp=resc*sff(1)*x
      xdsbp=resc*sff(2)*x
      xdcp=resc*cff(1)*x
      xdcbp=resc*cff(2)*x
      xdbp=resc*bff(1)*x
      xdbbp=resc*bff(2)*x
      xdtp = 0.d0
      xdtbp = 0.d0
      xdgp=resc*gff*x  	
      return
      end
c --------------------------------------------------------
      subroutine frag_dss(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      implicit real*8 (a-h,l-z)
      COMMON/OUTPUT_UNITS/IO,IER,ITEST  
      COMMON / FRAGINI / FINI
      resc=1.D0
      if(ih3.eq.10) then        ! (pi+ + pi-)/2
         ih=1
         ic=0
      elseif(ih3.eq.11) then    ! (pi+ + pi-)
         ih=1
         ic=0
         resc=2.D0
      elseif(ih3.eq.12) then       ! pi0
         ih=1
         ic=0
      elseif(ih3.eq.13) then       ! pi+
         ih=1
         ic=1
      elseif(ih3.eq.14) then       ! pi-
         ih=1
         ic=-1
      elseif(ih3.eq.20) then    ! (K+ + K-)/2
         ih=2
         ic=0
      elseif(ih3.eq.21) then    ! (K+ + K-)
         ih=2
         ic=0
         resc=2.D0
c need to write a wrapper for K0
c      elseif(ih3.eq.22) then    ! (K0 + K0bar)/2 | K0short
c         ih=2
c         ic=0
c      elseif(ih3.eq.23) then    ! (K0 + K0bar) | 2*K0short
c         ih=2
c         ic=0
c         resc=2.D0
      elseif(ih3.eq.24) then    ! K+
         ih=2
         ic=1
      elseif(ih3.eq.25) then    ! K-
         ih=2
         ic=-1
      elseif(ih3.eq.30) then    ! (p + anti-p)/2
         ih=3
         ic=0
      elseif(ih3.eq.31) then    ! (p + anti-p)
         ih=3
         ic=0
         resc=2.D0
      elseif(ih3.eq.32) then    ! p
         ih=3
         ic=1
      elseif(ih3.eq.33) then    ! anti-p
         ih=3
         ic=-1
      elseif(ih3.eq.40) then    ! (h+ + h-)/2
         ih=4
         ic=0
      elseif(ih3.eq.41) then    ! (h+ + h-)
         ih=4
         ic=0
         resc=2.D0
      elseif(ih3.eq.42) then    ! h+
         ih=4
         ic=1
      elseif(ih3.eq.43) then    ! h-
         ih=4
         ic=-1
      else
         write(ier,*) 'This species is not available in DSS'
        stop
         return
      endif
      call fDSS(ih,ic,iorder,x,q*q,xdup,xdubp,xddp,xddbp,xdsp,
     *           xdsbp,xdcp,xdbp,xdgp) ! returns momentum density
c	print*, 'after fDSS ',ih,ic,iorder,x,q,xdup
      xdgp=resc*xdgp
      xdup=resc*xdup
      xdubp=resc*xdubp
      xddp=resc*xddp
      xddbp=resc*xddbp
      xdsp=resc*xdsp
      xdsbp=resc*xdsbp
      xdcp=resc*xdcp
      xdcbp=xdcp
      xdbp=resc*xdbp
      xdbbp=xdbp
      xdtp = 0.d0
      xdtbp = 0.d0
c      print*, 'after DSS ', xdup,xdubp,xddp,xddbp,xdsp
      return
      end
c --------------------------------------------------------
      subroutine frag_aesss(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      implicit real*8 (a-h,l-z)
      resc=1.D0
      if(ih3.eq.62) then       ! eta
c        print*, 'new test'
         ih=1
         ic=0
      else
         write(ier,*) 'This species is not available in AESSS'
        stop
         return
      endif
      call etaff(x,q*q,xdup,xdubp,xddp,xddbp,xdsp,
     *           xdsbp,xdcp,xdbp,xdgp) ! returns momentum density
      xdgp=resc*xdgp
      xdup=resc*xdup
      xdubp=resc*xdubp
      xddp=resc*xddp
      xddbp=resc*xddbp
      xdsp=resc*xdsp
      xdsbp=resc*xdsbp
      xdcp=resc*xdcp
      xdcbp=xdcp
      xdbp=resc*xdbp
      xdbbp=xdbp
      xdtp = 0.d0
      xdtbp = 0.d0
c      print*, 'after AESSS ', x,q,ih3,xdgp
      return
      end
c --------------------------------------------------------
      subroutine frag_dsv(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      implicit real*8 (a-h,l-z)
      if(ih3.eq.80) then       ! (lambda+lambdabar)/2.D0
      	resc=1.D0
      else if(ih3.eq.81) then       ! lambda+lambdabar
	resc=2.D0
      else
         write(ier,*) 'This species is not available in DSV'
        stop
         return
      endif
      if(iorder.eq.0)iset=2
      if(iorder.eq.1)iset=1
      call fragpar(iset,x,q*q,utot,dtot,stot,ctot,btot,gl,
     *                                     uval,dval,sval) ! returns momentum density

      xdgp=resc*gl/2.D0
      xdup=resc*utot/2.D0
      xdubp=resc*utot/2.D0
      xddp=resc*dtot/2.D0
      xddbp=resc*dtot/2.D0
      xdsp=resc*stot/2.D0
      xdsbp=resc*stot/2.D0
      xdcp=resc*ctot/2.D0
      xdcbp=xdcp*ctot/2.D0
      xdbp=resc*btot/2.D0
      xdbbp=resc*btot/2.D0
      xdtp = 0.d0
      xdtbp = 0.d0

      return
      end
c --------------------------------------------------------
      subroutine frag_hkns(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      implicit real*8 (a-h,l-z)
      COMMON/OUTPUT_UNITS/IO,IER,ITEST  
      dimension grad(-5:5,17),ffhkns(-5:5) ! for HKNS FF
      resc=1.D0
      if(ih3.eq.10) then        ! (pi+ + pi-)/2
         if(iorder.eq.0)iset=1 ! pi LO
         if(iorder.eq.1)iset=2 ! pi NLO
         icharge=3
      elseif(ih3.eq.11) then    ! (pi+ + pi-)
         if(iorder.eq.0)iset=1 ! pi LO
         if(iorder.eq.1)iset=2 ! pi NLO
         icharge=3
         resc=2.D0
      elseif(ih3.eq.12) then       ! pi0
         if(iorder.eq.0)iset=1 ! pi LO
         if(iorder.eq.1)iset=2 ! pi NLO
         icharge=3
      elseif(ih3.eq.13) then       ! pi+
         if(iorder.eq.0)iset=1 ! pi LO
         if(iorder.eq.1)iset=2 ! pi NLO
         icharge=1
      elseif(ih3.eq.14) then       ! pi-
         if(iorder.eq.0)iset=1 ! pi LO
         if(iorder.eq.1)iset=2 ! pi NLO
         icharge=2
      elseif(ih3.eq.20) then    ! (K+ + K-)/2
         if(iorder.eq.0)iset=3 ! K LO
         if(iorder.eq.1)iset=4 ! K NLO
         icharge=4
      elseif(ih3.eq.21) then    ! (K+ + K-)
         if(iorder.eq.0)iset=3 ! K LO
         if(iorder.eq.1)iset=4 ! K NLO
         icharge=4
         resc=2.D0
      elseif(ih3.eq.22) then    ! (K0 + K0bar)/2
         if(iorder.eq.0)iset=3 ! K LO
         if(iorder.eq.1)iset=4 ! K NLO
         icharge=3
      elseif(ih3.eq.23) then    ! (K0 + K0bar)
         if(iorder.eq.0)iset=3 ! K LO
         if(iorder.eq.1)iset=4 ! K NLO
         icharge=3
         resc=2.D0
      elseif(ih3.eq.24) then    ! K+
         if(iorder.eq.0)iset=3 ! K LO
         if(iorder.eq.1)iset=4 ! K NLO
         icharge=1
      elseif(ih3.eq.25) then    ! K-
         if(iorder.eq.0)iset=3 ! K LO
         if(iorder.eq.1)iset=4 ! K NLO
         icharge=2
      elseif(ih3.eq.30) then    ! (p + anti-p)/2
         if(iorder.eq.0)iset=5 ! p LO
         if(iorder.eq.1)iset=6 ! p NLO
         icharge=3
      elseif(ih3.eq.31) then    ! (p + anti-p)
         if(iorder.eq.0)iset=5 ! p LO
         if(iorder.eq.1)iset=6 ! p NLO
         icharge=3
         resc=2.D0
      elseif(ih3.eq.32) then    ! p
         if(iorder.eq.0)iset=5 ! p LO
         if(iorder.eq.1)iset=6 ! p NLO
         icharge=1
      elseif(ih3.eq.33) then    ! anti-p
         if(iorder.eq.0)iset=9 ! anti-p LO
         if(iorder.eq.1)iset=10 ! anti-p NLO
         icharge=2 ! dummy parameter when iset=9,10
      elseif(ih3.eq.40) then    ! (h+ + h-)/2
         if(iorder.eq.0)iset=7 ! h LO
         if(iorder.eq.1)iset=8 ! h NLO
         icharge=3
      elseif(ih3.eq.41) then    ! (h+ + h-)
         if(iorder.eq.0)iset=7 ! h LO
         if(iorder.eq.1)iset=8 ! h NLO
         icharge=3
         resc=2.D0
      elseif(ih3.eq.42) then    ! h+
         if(iorder.eq.0)iset=7 ! h LO
         if(iorder.eq.1)iset=8 ! h NLO
         icharge=1
      elseif(ih3.eq.43) then    ! h-
         if(iorder.eq.0)iset=7 ! h LO
         if(iorder.eq.1)iset=8 ! h NLO
         icharge=2
      else
         write(ier,*) 'This species is not available in HKNS'
        stop
         return
      endif
      call hknsffwrap(q*q,x,iset,icharge,ffhkns,grad) ! returns number density
c      call hkns(q*q,x,iset,icharge,ffhkns,grad) ! returns number density
c      print*, 'right after hkns',q,x,iset,icharge,ffhkns
      xdgp=resc*ffhkns(0)*x     ! converts to momentum density
      xdup=resc*ffhkns(1)*x
      xdubp=resc*ffhkns(-1)*x
      xddp=resc*ffhkns(2)*x
      xddbp=resc*ffhkns(-2)*x
      xdsp=resc*ffhkns(3)*x
      xdsbp=resc*ffhkns(-3)*x
      xdcp=resc*ffhkns(4)*x
      xdcbp=resc*ffhkns(-4)*x
      xdbp=resc*ffhkns(5)*x
      xdbbp=resc*ffhkns(-5)*x
      xdtp = 0.d0
      xdtbp = 0.d0
      return
      end
c --------------------------------------------------------
      subroutine frag_akk08(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
c iorder dummy, returns only NLO
      implicit real*8 (a-h,l-z)
      dimension dh(0:10)
      COMMON/OUTPUT_UNITS/IO,IER,ITEST  
      resc=1.D0
      if(ih3.eq.10) then        ! (pi+ + pi-)/2
         ih=1
         resc=0.5D0
      elseif(ih3.eq.11) then    ! (pi+ + pi-)
         ih=1
      elseif(ih3.eq.12) then       ! pi0
         ih=6
      elseif(ih3.eq.13) then       ! pi+
         ih=111
      elseif(ih3.eq.14) then       ! pi-
         ih=112
      elseif(ih3.eq.20) then    ! (K+ + K-)/2
         ih=2
         resc=0.5D0
      elseif(ih3.eq.21) then    ! (K+ + K-)
         ih=2
      elseif(ih3.eq.22) then    ! (K0 + K0bar)/2 | K0short
         ih=7
      elseif(ih3.eq.23) then    ! (K0 + K0bar) | 2*K0short
         ih=7
         resc=2.D0
      elseif(ih3.eq.24) then    ! K+
         ih=121
      elseif(ih3.eq.25) then    ! K-
         ih=122
      elseif(ih3.eq.30) then    ! (p + anti-p)/2
         ih=3
         resc=0.5D0
      elseif(ih3.eq.31) then    ! (p + anti-p)
         ih=3
      elseif(ih3.eq.32) then    ! p
         ih=131
      elseif(ih3.eq.33) then    ! anti-p
         ih=132
      elseif(ih3.eq.40) then    ! (h+ + h-)/2
         ih=9
         resc=0.5D0
      elseif(ih3.eq.41) then    ! (h+ + h-)
         ih=9
      elseif(ih3.eq.42) then    ! h+
         ih=101
      elseif(ih3.eq.43) then    ! h-
         ih=102
      elseif(ih3.eq.80) then    ! (Lambda + anti-Lambda)/2
         ih=5
         resc=0.5D0
      elseif(ih3.eq.81) then    ! (Lambda + anti-Lambda)
         ih=5
      else
         write(ier,*) 'This species is not available in AKK08'
        stop
         return
      endif
      call akk08wrap(ih,x,q,dh) ! returns number density
      xdgp=resc*dh(0)*x              ! converts to momentum density
      xdup=resc*dh(1)*x
      xdubp=resc*dh(2)*x
      xddp=resc*dh(3)*x
      xddbp=resc*dh(4)*x
      xdsp=resc*dh(5)*x
      xdsbp=resc*dh(6)*x
      xdcp=resc*dh(7)*x
      xdcbp=resc*dh(8)*x
      xdbp=resc*dh(9)*x
      xdbbp=resc*dh(10)*x
      xdtp = 0.d0
      xdtbp = 0.d0
      return
      end
c --------------------------------------------------------
      subroutine frag_bkk(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      implicit real*8 (a-h,l-z)
      dimension dh(0:10)
      COMMON/OUTPUT_UNITS/IO,IER,ITEST  
      resc=1.D0
      if(ih3.eq.10) then        ! (pi+ + pi-)/2
         ih=1
      elseif(ih3.eq.11) then    ! (pi+ + pi-)
         ih=1
         resc=2.D0
      elseif(ih3.eq.12) then       ! pi0
         ih=5
      elseif(ih3.eq.20) then    ! (K+ + K-)/2
         ih=2
      elseif(ih3.eq.21) then    ! (K+ + K-)
         ih=2
         resc=2.D0
      elseif(ih3.eq.22) then    ! (K0 + K0bar)/2
         ih=3
      elseif(ih3.eq.23) then    ! (K0 + K0bar)
         ih=3
         resc=2.D0
      elseif(ih3.eq.40) then    ! (h+ + h-)/2
         ih=7
         resc=0.5D0
      elseif(ih3.eq.41) then    ! (h+ + h-)
         ih=7
      else
         write(ier,*) 'This species is not available in BKK'
        stop
         return
      endif
      call bkk(ih,iorder,x,q,dh) ! returns number density
      xdgp=resc*dh(0)*x              ! converts to momentum density
      xdup=resc*dh(1)*x
      xdubp=resc*dh(2)*x
      xddp=resc*dh(3)*x
      xddbp=resc*dh(4)*x
      xdsp=resc*dh(5)*x
      xdsbp=resc*dh(6)*x
      xdcp=resc*dh(7)*x
      xdcbp=resc*dh(8)*x
      xdbp=resc*dh(9)*x
      xdbbp=resc*dh(10)*x
      xdtp = 0.d0
      xdtbp = 0.d0
      return
      end
c --------------------------------------------------------
c FF into heavy-quark hadrons D*, D0 ,D+
c ih3=90 D*
c ih3=91 D0
c ih3=92 D+
      subroutine frag_kkks08(x,q,ih3,iorder,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
c iorder dummy, returns only NLO
      implicit real*8 (a-h,l-z)
	  integer NO,NH
      double precision DH(0:10)
      double precision DH1(0:10),DH2(0:10),DH3(0:10)
      COMMON/OUTPUT_UNITS/IO,IER,ITEST  
C 	ih3=dummy, always NLO
      resc=1.D0
      if(ih3.eq.90) then        ! D*
         NH=1 !   	  NH=1,2,3 ! global fit
      elseif(ih3.eq.91) then    ! D0
         NH=2 !   	  NH=1,2,3 ! global fit
      elseif(ih3.eq.92) then       ! D+
         NH=3 !   	  NH=1,2,3 ! global fit
      elseif(ih3.eq.93) then       ! D+
         NH=0!   flag for the sum over charmed hadrons
      else
         write(ier,*) 'This species is not available in KKKS08'
        stop
         return
      endif
      NO=1 ! include mass effects
      Q2=q*q
      if(NH.ge.1)then
         call kkks08_D(NO,NH,x,Q2,DH) ! returns number density
      else
         call kkks08_D(NO,1,x,Q2,DH1) ! sum over charmed hadrons
         call kkks08_D(NO,2,x,Q2,DH2) ! sum over charmed hadrons
         call kkks08_D(NO,3,x,Q2,DH3) ! sum over charmed hadrons
         do i=0,10
            DH(i)=DH1(i)+DH2(i)+DH3(i)
         enddo
      endif
      xdgp=resc*DH(0)*x              ! converts to momentum density
      xdup=resc*DH(1)*x
      xdubp=resc*DH(2)*x
      xddp=resc*DH(3)*x
      xddbp=resc*DH(4)*x
      xdsp=resc*DH(5)*x
      xdsbp=resc*DH(6)*x
      xdcp=resc*DH(7)*x
      xdcbp=resc*DH(8)*x
      xdbp=resc*DH(9)*x
      xdbbp=resc*DH(10)*x
      xdtp = 0.d0
      xdtbp = 0.d0
      return
      end
