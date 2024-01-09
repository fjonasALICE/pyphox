ccccccccccccccccccccccccccccccccccccccccc
c
c file: fragfun_all.f
c date: 26/2/2001 
c
c Type of fragmentation functions for hadron/photon 3
c
c conventions: 4 digit number, xxyy
c 
c xx = group label:
c
c photons
c 	  01: BFG small gluon
c	  02: BFG large gluon
c hadrons
c 	  01:  AKK08
c	  02:  DSS
c	  03:  HKNS
c	  04:  AKK05
c	  05:  BKK
c	  06:  KKP
c	  07:  Kretzer 
c	  08:  BFGW best fit
c	  09:  BFGW large Ng
c	  10: BFGW low Ng
c	  13: KKKS (heavy-quark)
c
c yy = hadron label:             
c
c	 00  : photon
c	 10 : (pi^+ + pi^-)/2
c	 11 : pi^+ + pi^-
c	 12 : pi^0
c	 13 : pi^+
c	 14 : pi^-
c	 20 : (K^+ + K^-)/2
c	 21 : K^+ + K^-
c 	 22 : (K^0 + K^0bar)/2
c 	 23 : K^0 + K^0bar
c 	 24 : K^+
c 	 25 : K^-
c 	 30 : (p + pbar)/2
c 	 31 : p + pbar
c 	 32 : p
c 	 33 : pbar
c 	 40 : (h^+ + h^-)/2
c 	 41 : h^+ + h^-
c 	 42 : h^+
c 	 43 : h^-
c 	 50 : (Lambda + Lambdabar)/2
c 	 51 : Lambda + Lambdabar
c    90 : D*
c    91 : D0
c    92 : D+
c
cccccccccccccccccccccccccccccccccccccccc
	subroutine dfrag(x,m2,ih,d)
	implicit real*8 (a-h,l-z)
	common/fragflag/iflag
        common/alfa/masch2,masbo2,masto2,lambda4square
	common/alem/iloopem
	common/approx/lo,nlo
	logical lo,nlo
	dimension uff(2), dff(2), sff(2), cff(2), bff(2)
	dimension d(-6:6),dh(0:10)
c 
	call extract_ihadroniff(ih,ihadron,iff)
c	print*, 'ihadron dans fragfun ',ihadron,iff

c parton labels in d array:
c	
c       1=d   2=u   3=s   4=c   5=b   6=t 
c  0=g -1=db -2=ub -3=sb -4=cb -5=bb -6=tb
c
	if (x.ge.1.d0) then
	  d(0)=0.d0
	  do i=1,6
	    d(i)=0.d0
	    d(-i)=0.d0
	  enddo
	  return
	endif
	q=dsqrt(m2)

	if (ihadron.eq.0)then ! photons (BFG)
            call frag_photon(x,q,iff,xdup,xdubp,xddp,
     #xddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)
c	call fonfra(x,iff,m2,xdup,xdubp,xddp,xddbp,xdsp,xdcp,xdbp
c     c ,xdbbp,xdtp,xdtbp,xdgp)
	else ! hadrons
	   if (nlo) then
		iorder=1
	   else
		iorder=0
	   endif
	   call frag_hadron(x,q,ihadron,iff,iorder,xdup,xdubp,xddp,
     cxddbp,xdsp,xdsbp,xdcp,xdcbp,xdbp,xdbbp,xdtp,xdtbp,xdgp)

	endif

	d(0) = xdgp/x
	d(1) = xddp/x
	d(-1) = xddbp/x
	d(2) = xdup/x
	d(-2) = xdubp/x
	d(3) = xdsp/x
	d(-3) = d(3)
	d(4) = xdcp/x
	d(-4) = d(4)
	d(5) = xdbp/x
	d(-5) = xdbbp/x
	d(6) = xdtp/x
	d(-6) = xdtbp/x

	return
	end
c
	subroutine extract_ihadroniff(inumb,ihadron,iff)
	implicit real*8(a-h,l-z)
	iff = int(dfloat(inumb)/100.d0)
	ihadron = inumb-iff*100
	return
	end
