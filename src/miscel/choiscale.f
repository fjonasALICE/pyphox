	subroutine choiscale(p3,p4,cm,cmu,cmf,ichoi_scale,m,mu,mf)
	implicit real*8 (a-h,l-z)
	dimension p3(4),p4(4)
	pt3 = dsqrt(p3(2)**2+p3(3)**2)
	pt4 = dsqrt(p4(2)**2+p4(3)**2)
	s1 = (pt3+pt4)
	s2 = dsqrt(pt3**2+pt4**2)
	s3 = dsqrt(2.d0*sca(p3,p4))
	s4 = pt4
	y3 = dlog((p3(1)+p3(4))/(p3(1)-p3(4)))*0.5d0
	y4 = dlog((p4(1)+p4(4))/(p4(1)-p4(4)))*0.5d0
	ystar = (y3-y4)*0.5d0
	s5 = pt4*dsqrt( (1.d0+dexp(-2.d0*dabs(ystar))/2.d0) )
	if (ichoi_scale.eq.1) then
	  m = s1*cm
	  mu = s1*cmu
	  mf = s1*cmf
	else if (ichoi_scale.eq.2) then
	  m = s2*cm
	  mu = s2*cmu
	  mf = s2*cmf
	else if (ichoi_scale.eq.3) then
	  m = s3*cm
	  mu = s3*cmu
	  mf = s3*cmf
	else if (ichoi_scale.eq.4) then
	  m = s4*cm
	  mu = s4*cmu
	  mf = s4*cmf
	else if (ichoi_scale.eq.5) then
	  m = s5*cm
	  mu = s5*cmu
	  mf = s5*cmf
	endif
	return
	end
	
