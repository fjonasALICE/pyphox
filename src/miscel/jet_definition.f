c cette routine renvoie vrai ou faux suivant que le(s) jet(s) sont dans
c les coupures cinematiques de l'experience:
c 1 est le parton de plus grand pt
	subroutine jet_selection(y1,y2,fi12,fi1,fi2,pt1,pt2,
     #	algorithm,r_kt,r_c,r_sep,ptjet_min,ptjet_max,yjet_min,
     #	yjet_max,merging,acceptance,
     #	inb_jet,xej,xpjx,xpjy,xpjz,cut_jet,merged)
	implicit real*8 (a-h,l-z)
	logical merged,cut_jet,proche_pi
	character*2 algorithm,merging,acceptance
	dimension xej(2),xpjx(2),xpjy(2),xpjz(2)
	zero = 0.d0
	r12s = (y1-y2)**2 + fi12**2
	r12 = dsqrt(r12s)
c attention au cas ou les partons 1 et 2 sont de part et d'autre de l'axe des x
	pi = 4.d0*atan(1.d0)
	if (fi12.eq.zero) then
	  proche_pi = .false.
	else
	  testa = abs(abs(fi1-fi2)-fi12)
	  testb = abs(2.*pi-abs(fi1-fi2)-fi12)
	  if (testa.le.testb) then
	    proche_pi = .false.
	  else if (testa.gt.testb) then 
	    proche_pi = .true.
	  endif
	endif
c
** 	call jet_algorithm(r12,pt1,pt2,algorithm,r_kt,r_c,r_sep,merged)
	call jet_algorithm(r12,pt1,y1,fi1,pt2,y2,fi2,pi,algorithm,
     #	r_kt,r_c,r_sep,merged)
c
	call jet_merging(pt1,y1,fi1,pt2,y2,fi2,proche_pi,pi,
     #	merging,merged,inb_jet,xej,xpjx,xpjy,xpjz)
c
	call jet_cut(xej,xpjx,xpjy,xpjz,ptjet_max,ptjet_min,
     #	yjet_max,yjet_min,acceptance,merged,cut_jet)
c
	return
	end
c
	subroutine jet_cut(xej,xpjx,xpjy,xpjz,ptjet_max,ptjet_min,
     #	yjet_max,yjet_min,acceptance,merged,cut_jet)
	implicit real*8 (a-h,l-z)
	logical cut_pt1,cut_y1,cut_pt2,cut_y2
	logical cut_jet,merged
	character*2 acceptance
	dimension xej(2),xpjx(2),xpjy(2),xpjz(2)
	ptjet1 = dsqrt(xpjx(1)**2+xpjy(1)**2)
	yjet1 = 0.5d0*dlog((xej(1)+xpjz(1))/(xej(1)-xpjz(1)))
	if (merged) then
	  ptjet2 = 0.d0
	  yjet2 = 0.d0
	else
	  ptjet2 = dsqrt(xpjx(2)**2+xpjy(2)**2)
	  yjet2 = 0.5d0*dlog((xej(2)+xpjz(2))/(xej(2)-xpjz(2)))
	endif
	cut_pt1 = (ptjet1.le.ptjet_max).and.(ptjet1.ge.ptjet_min)
	cut_y1 = (yjet1.le.yjet_max).and.(yjet1.ge.yjet_min)
	if (merged) then
	  cut_pt2 = .true.
	  cut_y2 = .true.
	else
	  cut_pt2 = (ptjet2.le.ptjet_max).and.(ptjet2.ge.ptjet_min)
	  cut_y2 = (yjet2.le.yjet_max).and.(yjet2.ge.yjet_min)
	endif
c si le jet de plus grand pt (pt1) est dans l'acceptance
	if (acceptance(1:2).eq.'gp') then
	  if (cut_pt1.and.cut_y1) then
	    cut_jet = .true.
	  else
	    cut_jet = .false.
	  endif
c si un des jets (pt1 ou pt2) est dans l'acceptance
	else if (acceptance(1:2).eq.'up') then
	  if ((cut_pt1.and.cut_y1).or.(cut_pt2.and.cut_y2)) then
	    cut_jet = .true.
	  else
	    cut_jet = .false.
	  endif
	endif
	return
	end
c
	subroutine jet_merging(pt1,y1,fi1,pt2,y2,fi2,proche_pi,pi,
     #	merging,merged,inb_jet,xej,xpjx,xpjy,xpjz)
	implicit real*8 (a-h,l-z)
	logical merged,proche_pi
	character*2 merging
	dimension xej(2),xpjx(2),xpjy(2),xpjz(2)
	zero = 0.d0
	if (merged) then
	  inb_jet = 1
c regle d'association: snowmass
	  if (merging(1:2).eq.'sn') then
	    ptjet = pt1 + pt2
	    yjet = pt1/ptjet*y1 + pt2/ptjet*y2
	    if (proche_pi) then
	      if (fi1.gt.fi2) then
	        fijet = pt1/ptjet*(2.d0*pi-fi1) + pt2/ptjet*fi2
		if (pt1.gt.pt2) then
		  fijet = 2.d0*pi-fijet
		endif
	      else if (fi1.le.fi2) then
	        fijet = pt1/ptjet*fi1 + pt2/ptjet*(2.d0*pi-fi2)
		if (pt2.gt.pt1) then
		  fijet = 2.d0*pi-fijet
		endif
	      endif
	      if (fijet.lt.zero) then
		fijet = 2.d0*pi-fijet
	      endif
	    else
	      fijet = pt1/ptjet*fi1 + pt2/ptjet*fi2
	    endif
	    xej(1) = ptjet*dcosh(yjet)
	    xpjx(1) = ptjet*dcos(fijet)
	    xpjy(1) = ptjet*dsin(fijet)
	    xpjz(1) = ptjet*dsinh(yjet)
c regle d'association: houches99 (on somme les 4-vecteurs)
	  else if (merging(1:2).eq.'ho') then
	   xej(1) = pt1*dcosh(y1)+pt2*dcosh(y2)
	   xpjx(1) = pt1*dcos(fi1)+pt2*dcos(fi2)
	   xpjy(1) = pt1*dsin(fi1)+pt2*dsin(fi2)
	   xpjz(1) = pt1*dsinh(y1)+pt2*dsinh(y2)
	  endif
	  xej(2) = 0.d0
	  xpjx(2) = 0.d0
	  xpjy(2) = 0.d0
	  xpjz(2) = 0.d0
	else
	  inb_jet = 2
	  xej(1) = pt1*dcosh(y1)
	  xpjx(1) = pt1*dcos(fi1)
	  xpjy(1) = pt1*dsin(fi1)
	  xpjz(1) = pt1*dsinh(y1)
	  xej(2) = pt2*dcosh(y2)
	  xpjx(2) = pt2*dcos(fi2)
	  xpjy(2) = pt2*dsin(fi2)
	  xpjz(2) = pt2*dsinh(y2)
	endif
	return
	end
c
	subroutine jet_algorithm(r12,pt1,y1,fi1,pt2,y2,fi2,pi,algorithm,
     #	r_kt,r_c,r_sep,merged)
	implicit real*8 (a-h,l-z)
	logical merged
	character*2 algorithm
c algorithme en kt
c d12 = min(pt2**2,pt1**2)*r12s
c d1 = pt1**2*r_kt**2
c d2 = pt2**2*r_kt**2
c comme pt2<=pt1 la condition d12 <= d1,d2 devient
c r12s <= r_kt**2
	if (algorithm(1:2).eq.'kt') then
	  if (r12.le.r_kt) then
	    merged = .true.
	  else
	    merged = .false.
	  endif
c algorithme en cone
c comme pt2<=pt1, on trace un cone autour de 1, la condition 2 
c appartient a ce cone est:
c r12s <= r_c**2
	else if (algorithm(1:2).eq.'co') then
	  if (r12.le.r_c) then
	    merged = .true.
	  else
	    merged = .false.
	  endif
c algorithme en cone (avec des semences qui ne sont pas dans la direction 
c des partons) on balade un cone dans le plan y-phi, si les deux partons 
c tombent dedans (et si leur distance est plus petite que r_sep alors 
c on associe)
c la condition pour que le parton 1 soit dans le cone est:
c r12 <= p_tjet/pt2*r_c
c la condition pour que le parton 2 soit dans le cone est:
c r12 <= p_tjet/pt1*r_c
c comme pt2<=pt1, la condition pour que les deux partons soient dans le 
c cone est: r12 <= p_tjet/pt1*r_c
c Attention les conditions precedentes utilisent implicitement que la 
c regle d'association est "snow-mass".
	else if (algorithm(1:2).eq.'se') then
	  ptjet = pt1+pt2
	  if (pt1.ne.0.d0) then
	    ptjet_s_pt1 = ptjet/pt1
	  else
	    ptjet_s_pt1 = 1.d+8
	  endif
	  if (r12.le.ptjet_s_pt1*r_c.and.r12.le.r_sep) then
	    merged = .true.
	  else
	    merged = .false.
	  endif
c algorithme "Midpoint"  (defnie dans QCD and Weak Boson Physics in Run II
c Fermilab-pub-00/297 page 62-63)
c Attention ces conditions s'utilisent avec 
c regle d'association de "houches99" (ou "E-scheme").
	else if (algorithm(1:2).eq.'d0') then
	  if (r12.le.r_c) then
	    merged = .true.
	  else if (r12.ge.2.d0*r_c) then
	    merged = .false.
	  else
c on definit un centroid
	   xej = pt1*dcosh(y1)+pt2*dcosh(y2)
	   xpjx = pt1*dcos(fi1)+pt2*dcos(fi2)
	   xpjy = pt1*dsin(fi1)+pt2*dsin(fi2)
	   xpjz = pt1*dsinh(y1)+pt2*dsinh(y2)
	   rap = dlog((xej+xpjz)/(xej-xpjz))/2.d0
	   pt = dsqrt(xpjx**2+xpjy**2)
	   if (xpjy.le.0.d0) then
	     phi = 2.d0*pi-zacos(xpjx/pt)
	   else
	     phi = zacos(xpjx/pt)
	   endif
c on calcule la distance entre le centroid et 1 ou 2
	   rc1 = dsqrt((y1-rap)**2+(fi1-phi)**2)
	   rc2 = dsqrt((y2-rap)**2+(fi2-phi)**2)
	   if ((rc1.le.r_c).and.(rc2.le.r_c)) then
	     merged = .true.
	   else
	     merged = .false.
	   endif
	  endif
	endif
	return
	end
