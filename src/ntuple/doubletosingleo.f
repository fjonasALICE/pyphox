c********************************************************************
	subroutine doubletosingleo(rnumb1,pi,poid,iiprov,intrack)
	implicit real*8 (a-h,l-z)
        logical lhalf
        integer*4 ntrack,iprov,maxtrk
        integer*4 intrack,iiprov
        parameter (maxtrk = 3)
	common/sortie/xpt1,xy1,xpt2,xy2,xfi12,xpt3,xy3,xfi13,resfunc
	common/fixle/weight,iprov,ntrack
        common/sortier/xe(maxtrk),xpx(maxtrk),
     #	xpy(maxtrk),xpz(maxtrk)
	common/sfragvr/xx1,xx2
	common/sfragv/xfrag1,xfrag2
	iprov = iiprov
	ntrack = intrack
	half = 0.5d0
	dpi = 2.d0*pi     
	xfi = dpi*rnumb1
	xfi1 = xfi
	xfi2 = dmod(xfi12+xfi,dpi)
	xfi3 = dmod(xfi13+xfi,dpi)
	do i = 1,ntrack
	  if(i.eq.1) then
	    xe(i) = xpt1*dcosh(xy1)
	    xpx(i) = xpt1*dcos(xfi1)
	    xpy(i) = xpt1*dsin(xfi1)
	    xpz(i) = xpt1*dsinh(xy1)
	  elseif(i.eq.2)then
	    xe(i) = xpt2*dcosh(xy2)
	    xpx(i) = xpt2*dcos(xfi2)
	    xpy(i) = xpt2*dsin(xfi2)
	    xpz(i) = xpt2*dsinh(xy2)
	  elseif(i.eq.3)then
	    xe(i) = xpt3*dcosh(xy3)
	    xpx(i) = xpt3*dcos(xfi3)
	    xpy(i) = xpt3*dsin(xfi3)
	    xpz(i) = xpt3*dsinh(xy3)
	  endif
	enddo
	xx1 = xfrag1
	xx2 = xfrag2
	weight = dsign(poid,resfunc)
	return
	end

