c********************************************************************
	double precision function zacos(x)
	implicit real*8 (a-h,l-z)
	un = 1.d0
	unp = 1.d0 + 1.d-8
	zero = 0.d0
	pi=datan(1.d0)*4.d0
	if (dabs(x).le.un) then
	  zacos = dacos(x)
	else if (dabs(x).gt.un.and.dabs(x).lt.unp) then
	  if (x.lt.zero) then
	    zacos = pi
	  else
	    zacos = 0.d0
	  endif
	else
	  write (*,*) 'alerte dans zacos argument > 1:',x
	  zacos = 0.d0
	endif
	return
	end
c********************************************************************
	double precision function zasin(x)
	implicit real*8 (a-h,l-z)
	un = 1.d0
	unp = 1.d0 + 1.d-8
	zero = 0.d0
	pi=datan(1.d0)*4.d0
	if (dabs(x).le.un) then
	  zasin = dasin(x)
	else if (dabs(x).gt.un.and.dabs(x).lt.unp) then
	  if (x.lt.zero) then
	    zasin = -pi/2.d0
	  else
	    zasin = pi/2.d0
	  endif
	else
	  write (*,*) 'alerte dans zasin argument > 1:',x
	  zasin = 0.d0
	endif
	return
	end
c********************************************************************
	double precision function coshmcos(y,fi)
	implicit real*8 (a-h,l-z)
	petit = 1.d-3
	yy = dabs(y)
	if (yy.le.petit.and.fi.gt.petit) then
	  coshmcos = 1+y**2/2.d0+y**4/24.d0+y**6/720.d0-dcos(fi)
	else if (yy.gt.petit.and.fi.le.petit) then
	  coshmcos = dcosh(y)-(1-fi**2/2.d0+fi**4/24.d0-fi**6/720.d0)
	else if (yy.le.petit.and.fi.le.petit) then
	  coshmcos = (y**2+fi**2)/2.d0+(y**4-fi**4)/24.d0+
     #	  (y**6+fi**6)/720.d0
	else if (yy.gt.petit.and.fi.gt.petit) then
	  coshmcos = dcosh(y)-dcos(fi)
	endif
	end
c********************************************************************
	double precision function un_si_neg(x)
	implicit real*8 (a-h,l-z)
	un = 1.d0
	zero = 0.d0
	if (x.le.zero) then
	  un_si_neg = un
	else
	  un_si_neg = zero
	endif
	return
	end
c********************************************************************
