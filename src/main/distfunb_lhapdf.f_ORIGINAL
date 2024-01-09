	subroutine fstru(x,m2,ih,f)
	implicit real*8 (a-h,l-z)
	dimension f(-6:6),dxpdf(-6:6)
        common/w50513/xmin,xmax,q2min,q2max
c ARLEO 06.03.07
	COMMON/NUCLEUS/ish
	COMMON/APPROX/lo,nlo
	logical lo,nlo
c 1=d 2=u 3=s 4=c 5=b 6=t 0=g
c -1=db -2=ub -3=sb -4=cb -5=bb -6=tb
	  if (x.ge.1.d0.or.m2.gt.q2max) then
	    f(0)=0.d0
	    do i=1,6
	      f(i)=0.d0
	      f(-i)=0.d0
	    enddo
	    return
	  endif
	if (ih.eq.0) then
c ih = 0 fonctions de distribution dans le proton
	  if (m2.lt.q2min) then
	    m2 = q2min
	  endif
	  m = dsqrt(m2)
	  call evolvepdf(x,m,dxpdf)
	  f(0) = dxpdf(0)/x
	  do i=1,6
	    f(i) = dxpdf(i)/x
	    f(-i) = dxpdf(-i)/x
	  enddo
	else if (ih.eq.1) then
c ih = 1 fonctions de distribution dans l antiproton
	  if (m2.lt.q2min) then
	    m2 = q2min
	  endif
	  m = dsqrt(m2)
	  call evolvepdf(x,m,dxpdf)
	  f(0) = dxpdf(0)/x
	  do i=1,6
	    f(i) = dxpdf(-i)/x
	    f(-i) = dxpdf(i)/x
	  enddo
	else if (ih.eq.3) then
c ih = 3 fonctions de distribution dans le pion
	  if (m2.lt.q2min) then
	    m2 = q2min
	  endif
	  m = dsqrt(m2)
	  call evolvepdf(x,m,dxpdf)
	  f(0) = dxpdf(0)/x
	  do i=1,6
	    f(i) = dxpdf(-i)/x
	    f(-i) = dxpdf(i)/x
	  enddo
	else
c MODIFIED BY ARLEO ON 06.03.07 SO AS TO INCLUDE SHADOWING EFFECTS
	  IF (m2.lt.q2min) THEN
	    m2 = q2min
	  ENDIF
	  m = dsqrt(m2)
	  CALL evolvepdf(x,m,dxpdf)
	  CALL swap(ih,izzz,iaaa)
	  aaa = dfloat(iaaa)

	  CALL pdfa(nlo,x,m,ish,aaa,ruv,rdv,ru,rd,rs,rc,rb,rg)

c distributions ("up" et "down") du proton a l'interieur du noyau
	  xupa = ruv*(dxpdf(2)-dxpdf(-2))+ru*dxpdf(-2)
	  xdpa = rdv*(dxpdf(1)-dxpdf(-1))+rd*dxpdf(-1)
	  xupa_bar = ru*dxpdf(-2)
	  xdpa_bar = rd*dxpdf(-1)
c distributions ("up" et "down") du neutron a l'interieur du noyau
	  xuna = xdpa
	  xdna = xupa
	  xdna_bar = xupa_bar
	  xuna_bar = xdpa_bar
c distributions partoniques pour diffusion de deux noyaux
	  f(0) = rg*dxpdf(0)/x
	  f(1) = (izzz*xdpa+(iaaa-izzz)*xdna)/(dble(iaaa)*x)
	  f(2) = (izzz*xupa+(iaaa-izzz)*xuna)/(dble(iaaa)*x)
	  f(-1) = (izzz*xdpa_bar+(iaaa-izzz)*xdna_bar)/
     #          (dble(iaaa)*x)
	  f(-2) = (izzz*xupa_bar+(iaaa-izzz)*xuna_bar)/
     #         (dble(iaaa)*x)
          f(3) = rs*dxpdf(3)/x
	  f(4) = rc*dxpdf(4)/x
	  f(5) = rb*dxpdf(5)/x
	  f(6) = dxpdf(6)/x
	  f(-3) = rs*dxpdf(-3)/x
	  f(-4) = rc*dxpdf(-4)/x
	  f(-5) = rb*dxpdf(-5)/x
	  f(-6) = dxpdf(-6)/x
	endif
	return
	end
c --------------------------------------------------------------------------
        subroutine swap(inumb1,izzz,iaaa)
	implicit real*8 (a-h,l-z)
c extract a
        iterm1 = int(dble(inumb1)/1000.d0)
	iaaa = iterm1
c extract z
        izzz = inumb1-iterm1*1000
	return
	end
c --------------------------------------------------------------------------
