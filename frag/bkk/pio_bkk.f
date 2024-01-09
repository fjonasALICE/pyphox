      subroutine fonfra(xc,ipi,qp2,xdup,xdubp,xddp,xddbp,xdsp,xdcp
     # ,xdbp,xdbbp,xdtp,xdtbp,xdgp)
      implicit real*8 (a-h,l-z)
      dimension dh(0:10)
      common/BKK_choice/ih,iset
      qs = dsqrt(qp2)
      call bkk(ih,iset,xc,qs,dh)
      xdup=dh(1)*xc
      xdubp=dh(2)*xc
      xddp=dh(3)*xc
      xddbp=dh(4)*xc
      xdsp=dh(5)*xc
      xdcp=dh(7)*xc
c      xdbp=dh(9)*xc
c      xdbbp=dh(10)*xc
      xdbp=0.
      xdbbp=0.
      xdtp=0.
      xdtbp=0.
      xdgp=dh(0)*xc
      return
      end
