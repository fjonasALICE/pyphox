c********************************************************************
	double precision function zsign(x)
	implicit real*8 (a-h,l-z)
	un = 1.d0
	zero = 0.d0
	if (x.le.zero) then
	  zsign = un
	else
	  zsign = zero
	endif
	return
	end
c********************************************************************
	double precision function zinvcos(x,y,pt,pi)
	implicit real*8 (a-h,l-z)
	zero = 0.d0
	if (y.le.zero) then
	  zinvcos = 2.d0*pi-zacos(x/pt)
	else
	  zinvcos = zacos(x/pt)
	endif
	return
	end
c********************************************************************
	real function wacos(x)
	implicit real*4 (a-h,l-z)
	un = 1.
	unp = 1. + 1.e-4
	zero = 0.
	pi=atan(1.)*4.
	if (abs(x).le.un) then
	  wacos = acos(x)
	else if (abs(x).gt.un.and.abs(x).lt.unp) then
	  if (x.lt.zero) then
	    wacos = pi
	  else
	    wacos = 0.
	  endif
	else
	  write (*,*) 'alerte dans wacos argument > 1:',x
	  wacos = 0.
	endif
	return
	end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc	
C correction d'un bug le 29/07/99, pt5 n'etait pas definie pour le twof
c photon est la particule 3!!!!!!!!!!!!!!!!!!!!
c attention les quadrivecteurs sont en double precision
	subroutine gfill
	use ftn_c
	implicit real*8 (a-h,l-z)
	integer*4 iprov,maxtrk,ntrack
	character*2 algorithm,merging,acceptance
        parameter (maxtrk = 3)
	logical rapidite,pete,isol,masscut
	logical dir,onef
	logical cut_jet,cut_phi,merged
	logical cut_fin
	dimension ptjet(2),yjet(2),fijet(2)
        common/sortier/xe(maxtrk),xpx(maxtrk),
     #	xpy(maxtrk),xpz(maxtrk)
	common/fixle/weight,iprov,ntrack
	common/pclass/dir,onef
	common/sfragvr/xx3,xx4
	common/jets/jetmode
	common/parami/ysup,yinf,gptsup,gptinf,s
	dimension xej(2),xpjx(2),xpjy(2),xpjz(2)
** 	common/coup_histo/zr_isol,zetmax
** 	call further_param(pt1min,pt2min,ymax,ymin,etmin,
**      #	r,xmasmax,xmasmin)
	call further_param(ptgamma_min,ymax,ymin,algorithm,
     #	r_kt,r_c,r_sep,merging,acceptance,ptjet_max,
     #	ptjet_min,yjet_max,yjet_min)
	xinfty = 1.d+8	
	pi = 4.d0*atan(1.d0)
	if (dir) then
	  xx3 = 1.d0
	  xx4 = 1.d0
	endif 
	pt3 = sqrt(xpx(1)**2+xpy(1)**2)
	pt4 = sqrt(xpx(2)**2+xpy(2)**2)
	y3 = log((xe(1)+xpz(1))/(xe(1)-xpz(1)))*0.5
	y4 = log((xe(2)+xpz(2))/(xe(2)-xpz(2)))*0.5
	if (ntrack.eq.3) then
	  pt5 = sqrt(xpx(3)**2+xpy(3)**2)
	  xnumy5 = (xe(3)+xpz(3))
	  xdeny5 = (xe(3)-xpz(3))
	  if (xnumy5.eq.0..or.xdeny5.eq.0.) then
	    if (xpz(3).le.0.) then
	      y5 = -xinfty
	    else
	      y5 = xinfty
	    endif
	  else if (xnumy5.gt.0..and.xdeny5.gt.0.) then
	    y5 = log((xe(3)+xpz(3))/(xe(3)-xpz(3)))*0.5
	  endif
	else if (iprov.eq.23) then
	  y5 = y3
	  if (dir) then
	    pt5 = pt4-pt3
	  else if (onef) then
	    pt5 = pt4-pt3/xx3
	  endif
	else
	  pt5 =0.
	  y5 = 0.
	endif
	cfi34 = (xpx(1)*xpx(2)+xpy(1)*xpy(2))/(pt3*pt4)
	if (ntrack.eq.3) then
	  cfi35 = (xpx(1)*xpx(3)+xpy(1)*xpy(3))/(pt3*pt5)
	  cfi45 = (xpx(3)*xpx(2)+xpy(3)*xpy(2))/(pt5*pt4)
	  fi35 = zacos(cfi35)
	  fi45 = zacos(cfi45)
	  fi5 = zinvcos(xpx(3),xpy(3),pt5,pi)
	else
	  fi45 = 0.
	  fi5 = 0.
	endif
	pi = atan(1.)*4.
	if (abs(cfi34).gt.1.0001) then
	  write (6,*) 'alerte',cfi34
	else if (abs(cfi34).gt.1.) then
	  cfi34 = sign(1.d0,cfi34)
	else
	  cfi34 = cfi34
	endif
	fi34 = zacos( cfi34 )
	fi3 = zinvcos(xpx(1),xpy(1),pt3,pi)
	fi4 = zinvcos(xpx(2),xpy(2),pt4,pi)
	tolerance = 1.d-6
	test34 = dmin1(abs(abs(fi3-fi4)-fi34),
     #	abs(2.*pi-abs(fi3-fi4)-fi34))
	if ( test34.gt.tolerance) then
	  write (37,*) 'attention',test34
	endif
	if (ntrack.eq.3) then
	  test35 = dmin1(abs(abs(fi3-fi5)-fi35),
     #	  abs(2.*pi-abs(fi3-fi5)-fi35))
	  test45 = dmin1(abs(abs(fi4-fi5)-fi45),
     #	  abs(2.*pi-abs(fi4-fi5)-fi45))
	  if ( (test35.gt.tolerance).or.
     #	  (test45.gt.tolerance) ) then
	    write (37,*) 'attention',test35,test45,test34
	    write (37,*) 'attention1',fi35,fi45,fi34
	    write (37,*) 'attention2',fi3,fi4,fi5
	  endif
	  call jet_selection(y4,y5,fi45,fi4,fi5,pt4,pt5,
     #	  algorithm,r_kt,r_c,r_sep,ptjet_min,ptjet_max,yjet_min,
     #	  yjet_max,merging,acceptance,
     #	  inb_jet,xej,xpjx,xpjy,xpjz,cut_jet,merged)
	else
	  call jet_selection(y4,y4,0.d0,fi4,0.d0,pt4,0.d0,
     #	  algorithm,r_kt,r_c,r_sep,ptjet_min,ptjet_max,yjet_min,
     #	  yjet_max,merging,acceptance,
     #	  inb_jet,xej,xpjx,xpjy,xpjz,cut_jet,merged)
	endif
	ptjet_lead = sqrt(xpjx(1)**2+xpjy(1)**2)
	yjet_lead = 0.5*log((xej(1)+xpjz(1))/(xej(1)-xpjz(1)))
	fijet_lead = zinvcos(xpjx(1),xpjy(1),ptjet_lead,pi)
c on definit un super jet qui est la somme des 4-vecteurs des deux jets
	if (inb_jet.eq.2) then
	  pt_sup_jet = sqrt((xpjx(1)+xpjx(2))**2+(xpjy(1)+xpjy(2))**2)
	  fi_sup_jet = zinvcos(xpjx(1)+xpjx(2),xpjy(1)+xpjy(2),
     #			pt_sup_jet,pi)
	else
	  pt_sup_jet = ptjet_lead
	  fi_sup_jet = fijet_lead
	endif
	cfi_gamma_jet = (xpx(1)*xpjx(1)+xpy(1)*xpjy(1))/(pt3*ptjet_lead)
	cfi_gamma_supjet = ( xpx(1)*(xpjx(1)+xpjx(2))
     #	+xpy(1)*(xpjy(1)+xpjy(2)) )/(pt3*pt_sup_jet)
	fi_gamma_jet = zacos(cfi_gamma_jet)
	fi_gamma_supjet = zacos(cfi_gamma_supjet)
	qt_gamma_jet = sqrt((xpx(1)+xpjx(1))**2+(xpy(1)+xpjy(1))**2)
	z_gamma_trig = -ptjet_lead/pt3*cfi_gamma_jet
	z_jet_trig = -pt3/ptjet_lead*cfi_gamma_jet
	x_obs_plus = (pt3*exp(y3)+ptjet_lead*exp(yjet_lead))/sqrt(s)
	x_obs_moins = (pt3*exp(-y3)+ptjet_lead*exp(-yjet_lead))/sqrt(s)
        x_ll_plus = pt3*(exp(y3)+exp(yjet_lead))/sqrt(s)
	x_ll_moins = pt3*(exp(-y3)+exp(-yjet_lead))/sqrt(s)
** 	z_gamma_trig = -pt_sup_jet/pt3*cfi_gamma_supjet
** 	z_jet_trig = -pt3/pt_sup_jet*cfi_gamma_supjet
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Pour Joey
	r = 0.4
	if (ntrack.eq.3) then
	  dist35 = sqrt((y3-y5)**2+(fi35)**2)
	  if (dist35.gt.r) then
	    edep = (1.-xx3)/xx3*pt3
	  else if (dist35.le.r) then
	    edep = pt5 + (1.-xx3)/xx3*pt3
	  endif
	else if (iprov.eq.23) then
	  edep = pt4-pt3 
	else
	  dist35 = 0.
	  dist45 = 0.
	  edep = (1.-xx3)/xx3*pt3
	endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c cut en phi pour ALICE et sa tranche de Camembert
** 	phi_min = 0.
** 	phi_max = 1.745329252
** 	cut_phi = (fi3.ge.phi_min.and.fi3.le.phi_max)	
	cut_phi = .true.	
	rapidite = (y3.le.ymax.and.y3.ge.ymin)
	pete = pt3.ge.ptgamma_min
c
	isol = .true.
c on convertit les variables
c	wpt3 = sngl(pt3)
c	wy3 = sngl(y3)
c	wptjet_lead = sngl(ptjet_lead)
c	wyjet_lead = sngl(yjet_lead)
c warning changement de definition..............
** 	wfi_gamma_jet = sngl(fi_gamma_supjet)
c	wfi_gamma_jet = sngl(fi_gamma_jet)
c
c	wqt_gamma_jet = sngl(qt_gamma_jet)
c	wx_obs_plus = sngl(x_obs_plus)
c	wx_obs_moins = sngl(x_obs_moins)
c	wx_ll_plus = sngl(x_ll_plus)
c	wx_ll_moins = sngl(x_ll_moins)
c	wedep = sngl(edep)
c	wz_gamma_trig = sngl(z_gamma_trig)
c	wz_jet_trig = sngl(z_jet_trig)
c si jetmode = 0 on ne s'occupe pas du jet..... cas inclusif
	if (jetmode.eq.0) then
	  cut_fin = rapidite.and.pete
	else if (jetmode.eq.1) then
	  cut_fin = rapidite.and.pete.and.cut_jet.and.cut_phi
	endif
	if (cut_fin) then
c	  call c_remplie(iprov,wpt3,wy3,wptjet_lead,wyjet_lead,
c     #		wfi_gamma_jet,wqt_gamma_jet,wedep,wz_gamma_trig,
c     #		wz_jet_trig,wx_obs_plus,wx_obs_moins,
c     #		wx_ll_plus,wx_ll_moins,weight)
	  call c_remplie(iprov,pt3,y3,ptjet_lead,yjet_lead,
     #		fi_gamma_jet,qt_gamma_jet,edep,z_gamma_trig,
     #		z_jet_trig,x_obs_plus,x_obs_moins,
     #		x_ll_plus,x_ll_moins,weight)
	endif
	end
