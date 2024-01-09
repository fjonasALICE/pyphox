c*****************************************************
c fonction p3di+p4di
c*****************************************************	
c*****************************************************
c partie finie pour pt5 > ptm
c*****************************************************
c*****************************************************
	subroutine ssy3r(s,gpt,x10,x20,ptm,r,y3,y4,ih1,ih2,sy3r)
	implicit real*8 (a-h,l-z)
	parameter (k0max=22)
	dimension part35r(k0max),sf(k0max)
	dimension sy3r(k0max),temp(k0max)
	zero = 0.d0
	un = 1.d0
c attention les elements de matrice 2 -> 2 sont independant de 
c l'echelle d'energie
	sc = x10*x20*s
	tc = -x20*dsqrt(s)*gpt*dexp(y4)
	uc = -x10*dsqrt(s)*gpt*dexp(-y4)
	zm = (gpt-ptm)/gpt
	call strfrad(x10,ih1,x20,ih2,sf)
	call spart35rd(sc,tc,uc,zm,r,part35r)
	tyu = gpt
	call vec_dmult_vector(sf,part35r,k0max,temp)
	call vec_dmult_constant(temp,k0max,tyu,sy3r)
	return
	end
c**********************************************************************
	subroutine ssy4r(s,gpt4,x10,x20,z4,r,y3,y4,ih1,ih2,sy4r)
	implicit real*8 (a-h,l-z)
	parameter (k0max=22)
	dimension part45r(k0max),sf(k0max)
	dimension sy4r(k0max),temp(k0max)
	zero = 0.d0
	un = 1.d0
	pt4 = gpt4
	x1s = x10/z4
	x2s = x20/z4
	if (x1s.gt.un.or.x2s.gt.un) then
	  call vec_dinit(sy4r,k0max,zero)
	  return
	endif
c attention les elements de matrice 2 -> 2 sont independant de 
c l'echelle d'energie
	sc = x10*x20*s
	tc = -x20*dsqrt(s)*pt4*dexp(y4)
	uc = -x10*dsqrt(s)*pt4*dexp(-y4)
	call strfrad(x1s,ih1,x2s,ih2,sf)
	call spart45rd(sc,tc,uc,z4,r,part45r)
	tyu = gpt4/z4**2
	call vec_dmult_vector(sf,part45r,k0max,temp)
	call vec_dmult_constant(temp,k0max,tyu,sy4r)
	return
	end
c*****************************************************
c fonctions pour p3di+p4di	
c*****************************************************	
	subroutine ssv12(s,gpt3,gpt4,ptm,y3,y4,x10,x20,m,mu,ih1,
     #	ih2,sv12)
c integrale a 1 dimensions
	implicit real*8 (a-h,l-z)
	common/coup/ptmm,r
	common/flag/icoup
	parameter (k0max=22)
	dimension part15z(k0max),part15l(k0max),part15d(k0max)
	dimension part25z(k0max),part25l(k0max),part25d(k0max)
	dimension partvi(k0max),sf(k0max)
	dimension temp0(k0max),temp1(k0max),temp2(k0max),temp3(k0max)
	dimension au1(k0max),au2(k0max),au3(k0max),sv12(k0max)
	zero = 0.d0
	un = 1.d0
	x3min = 2.d0*gpt3/dsqrt(s)*dcosh(y3)
	pt3 = gpt3
	pt4 = pt3
	sc = x10*x20*s
	tc = -x10*dsqrt(s)*pt3*dexp(-y3)
	uc = -x20*dsqrt(s)*pt3*dexp(y3)
	call strfrad(x10,ih1,x20,ih2,sf)
	call spart15zd(sc,tc,uc,un,m,ptm,part15z)
	call spart15ld(sc,tc,uc,un,part15l)
	call spart15dd(sc,tc,uc,m,ptm,part15d)
	call spart25zd(sc,tc,uc,un,m,ptm,part25z)
	call spart25ld(sc,tc,uc,un,part25l)
	call spart25dd(sc,tc,uc,m,ptm,part25d)
	call spartvid(s,sc,tc,uc,mu,pt3,ptm,partvi)
c
	call vec_dmult_vector(part15z,sf,k0max,temp0)
	call vec_dmult_vector(part15l,sf,k0max,temp1)
	call vec_dmult_vector(part15d,sf,k0max,temp2)
	ty1 = dlog(1-x10)
	ty2 = dlog(1-x10)**2/2.d0
	call vec_dmult_add(temp2,temp1,k0max,ty2,temp3)
	call vec_dmult_add(temp3,temp0,k0max,ty1,au1)
c
	call vec_dmult_vector(part25z,sf,k0max,temp0)
	call vec_dmult_vector(part25l,sf,k0max,temp1)
	call vec_dmult_vector(part25d,sf,k0max,temp2)
	ty1 = dlog(1-x20)
	ty2 = dlog(1-x20)**2/2.d0
	call vec_dmult_add(temp2,temp1,k0max,ty2,temp3)
	call vec_dmult_add(temp3,temp0,k0max,ty1,au2)
c
	call vec_dmult_vector(partvi,sf,k0max,au3)
c
	call vec_dadd_vector(au1,au2,k0max,temp0)
	ty3 = gpt3
	call vec_dadd_mult_constant(temp0,au3,k0max,ty3,sv12)
	return
	end
c*****************************************************	
	subroutine ssv2(s,gpt3,y3,y4,x10,x20,u1,ih1,ih2,sv2)
c integrale a 2 dimensions
	implicit real*8 (a-h,l-z)
	common/coup/ptm,r
	common/flag/icoup
	parameter (k0max=22)
	dimension partvi2(k0max),sf(k0max)
	dimension sv2(k0max),temp0(k0max)
	pi=4.d0*datan(1.d0)
	zero = 0.d0
	un = 1.d0
	fi = pi*u1
	pt3 = gpt3
	pt4 = pt3
	ys = (y3-y4)/2.d0
	sc = x10*x20*s
	tc = -x10*dsqrt(s)*pt3*dexp(-y3)
	uc = -x20*dsqrt(s)*pt3*dexp(y3)
	call strfrad(x10,ih1,x20,ih2,sf)
	call spartvi2d(s,sc,tc,uc,ys,fi,partvi2)
	call vec_dmult_vector(partvi2,sf,k0max,temp0)
	ty = gpt3*pi
	call vec_dmult_constant(temp0,k0max,ty,sv2)
	return
	end
c*****************************************************	
	subroutine ssv3(s,gpt,y3,y4,x10,x20,ih1,ih2,sv3)
c integrale a 1 dimensions
	implicit real*8 (a-h,l-z)
	common/flagexp/icoup_exp
	parameter (k0max=22)
	dimension part35d(k0max),sf(k0max)
	dimension sv3(k0max),au3(k0max)
	zero = 0.d0
	un = 1.d0
	ys = (y3-y4)/2.d0
	y = (y3+y4)/2.d0
c
	sc = x10*x20*s
	tc = -x20*dsqrt(s)*gpt*dexp(y4)
	uc = -x10*dsqrt(s)*gpt*dexp(-y4)
	call strfrad(x10,ih1,x20,ih2,sf)
	call spart35dd(sc,tc,uc,part35d)
	call vec_dmult_vector(part35d,sf,k0max,au3)
	ty3 = gpt
	call vec_dmult_constant(au3,k0max,ty3,sv3)
	return
	end
c*****************************************************	
	subroutine ssv4(s,gpt4,gpt3sup,x4min_isol,y3,y4,x10,x20,mf,
     #	ih1,ih2,sv4)
c integrale a 1 dimensions
	implicit real*8 (a-h,l-z)
	common/coup/ptmm,r
	parameter (k0max=22)
	dimension part45z(k0max),part45l(k0max),part45d(k0max),sf(k0max)
	dimension sv4(k0max),au4(k0max)
	dimension temp0(k0max),temp1(k0max),temp2(k0max),temp3(k0max)
	zero = 0.d0
	un = 1.d0
	pt4 = gpt4
	pt3 = pt4
c
	x4min_incl = 2.d0*gpt4/dsqrt(s)*dcosh(y4)
** 	x4min_isol = gpt4/(gpt4+etmax)
	x4min = dmax1(x4min_incl,x4min_isol)
	z4min = dmax1(x4min,gpt4/gpt3sup,x10,x20)	
	z4max = 1.d0	
	sc = x10*x20*s
	tc = -x20*dsqrt(s)*pt4*dexp(y4)
	uc = -x10*dsqrt(s)*pt4*dexp(-y4)
	call strfrad(x10,ih1,x20,ih2,sf)
	call spart45zd(sc,tc,uc,un,mf,pt4,part45z)
	call spart45ld(sc,tc,uc,un,part45l)
	call spart45dd(sc,tc,uc,mf,pt4,part45d)
	call vec_dmult_vector(part45z,sf,k0max,temp0)
	call vec_dmult_vector(part45l,sf,k0max,temp1)
	call vec_dmult_vector(part45d,sf,k0max,temp2)
	ty1 = dlog(1-z4min)
	ty2 = dlog(1-z4min)**2/2.d0
	call vec_dmult_add(temp2,temp1,k0max,ty2,temp3)
	call vec_dmult_add(temp3,temp0,k0max,ty1,au4)
	ty3 = gpt4
	call vec_dmult_constant(au4,k0max,ty3,sv4)
	return
	end
c*****************************************************	
	subroutine ssy3(s,gpt3,y3,y4,x10,x20,m,u1,ih1,ih2,sy3)
	implicit real*8 (a-h,l-z)
	common/coup/ptmm,r
	parameter (k0max=22)
	dimension part15z(k0max),part15l(k0max)
	dimension part15z0(k0max),part15l0(k0max)
	dimension part25z(k0max),part25l(k0max)
	dimension part25z0(k0max),part25l0(k0max)
	dimension sf(k0max),sf0(k0max)
	dimension sy3(k0max),bu1(k0max),bu2(k0max)
	dimension temp0(k0max),temp1(k0max),temp2(k0max),temp3(k0max)
	dimension temp4(k0max),temp5(k0max),temp6(k0max),temp7(k0max)
	zero = 0.d0
	un = 1.d0
	ys = (y3-y4)/2.d0
	y = (y3+y4)/2.d0
	pt3 = gpt3
	pt5x = dsqrt(s)/(2.d0*dcosh(ys))*(1.d0-x10)*(1.d0-x20)/
     #  ((1.d0-x10)*dexp(-y)+(1.d0-x20)*dexp(y))
cc	ptm = dmin1(ptmm,pt5x)
	ptm = ptmm
c
	sc = x10*x20*s
	tc = -x10*dsqrt(s)*pt3*dexp(-y3)
	uc = -x20*dsqrt(s)*pt3*dexp(y3)
	z1max = 1.d0
	z1min = x10
	z1 = z1min + (z1max-z1min)*u1
	call strfrad(x10/z1,ih1,x20,ih2,sf)
	call strfrad(x10,ih1,x20,ih2,sf0)
	call spart15zd(sc,tc,uc,z1,m,ptm,part15z)
	call spart15zd(sc,tc,uc,un,m,ptm,part15z0)
	call spart15ld(sc,tc,uc,z1,part15l)
	call spart15ld(sc,tc,uc,un,part15l0)
	call vec_dmult_vector(part15z,sf,k0max,temp0)
	call vec_dmult_vector(part15z0,sf0,k0max,temp1)
	ty1 = 1.d0/z1**2
	call vec_dmult_sub(temp1,temp0,k0max,ty1,temp2)
	ty2 = 1.d0/(1.d0-z1)
	call vec_dmult_constant(temp2,k0max,ty2,temp3)
c
	call vec_dmult_vector(part15l,sf,k0max,temp4)
	call vec_dmult_vector(part15l0,sf0,k0max,temp5)
	call vec_dmult_sub(temp5,temp4,k0max,ty1,temp6)
	ty3 = dlog(1.d0-z1)/(1.d0-z1)
	call vec_dmult_constant(temp6,k0max,ty3,temp7)
c
	ty4 = (z1max-z1min)*gpt3
	call vec_dadd_mult_constant(temp3,temp7,k0max,ty4,bu1)
	z2max = 1.d0
	z2min = x20
	z2 = z2min + (z2max-z2min)*u1
	call strfrad(x10,ih1,x20/z2,ih2,sf)
	call strfrad(x10,ih1,x20,ih2,sf0)
	call spart25zd(sc,tc,uc,z2,m,ptm,part25z)
	call spart25zd(sc,tc,uc,un,m,ptm,part25z0)
	call spart25ld(sc,tc,uc,z2,part25l)
	call spart25ld(sc,tc,uc,un,part25l0)
	call vec_dmult_vector(part25z,sf,k0max,temp0)
	call vec_dmult_vector(part25z0,sf0,k0max,temp1)
	ty1 = 1.d0/z2**2
	call vec_dmult_sub(temp1,temp0,k0max,ty1,temp2)
	ty2 = 1.d0/(1.d0-z2)
	call vec_dmult_constant(temp2,k0max,ty2,temp3)
c
	call vec_dmult_vector(part25l,sf,k0max,temp4)
	call vec_dmult_vector(part25l0,sf0,k0max,temp5)
	call vec_dmult_sub(temp5,temp4,k0max,ty1,temp6)
	ty3 = dlog(1.d0-z2)/(1.d0-z2)
	call vec_dmult_constant(temp6,k0max,ty3,temp7)
c
	ty4 = (z2max-z2min)*gpt3
	call vec_dadd_mult_constant(temp3,temp7,k0max,ty4,bu2)
c
	call vec_dadd_vector(bu1,bu2,k0max,sy3)
	return
	end
c*****************************************************	
	subroutine ssy4p(s,gpt4,x10,x20,z4,y3,y4,mf,ih1,ih2,sy4p)
	implicit real*8 (a-h,l-z)
	common/aurenche/iauren
	parameter (k0max=22)
	dimension part45z(k0max),part45l(k0max)
	dimension part45z0(k0max),part45l0(k0max),sf0(k0max)
	dimension sf(k0max),sy4p(k0max)
	dimension temp0(k0max),temp1(k0max),temp2(k0max),temp3(k0max)
	dimension temp4(k0max),temp5(k0max),temp6(k0max),temp7(k0max)
	zero = 0.d0
	un = 1.d0
	pt4 = gpt4
	x1s = x10/z4
	x2s = x20/z4
c en principe vrai mais a verif
	if (x1s.gt.un.or.x2s.gt.un) then
	  call vec_dinit(sy4p,k0max,zero)
	  write (18,*) 'attention  x1s > 1 ou x2s > 1',
     #		x1s,x2s
	  return
	endif
c attention les elements de matrice 2 -> 2 sont independant de 
c l'echelle d'energie
	sc = x10*x20*s
	tc = -x20*dsqrt(s)*pt4*dexp(y4)
	uc = -x10*dsqrt(s)*pt4*dexp(-y4)
cc	sc = x1s*x2s*s
cc	tc = -x2s*dsqrt(s)*pt4/z4*dexp(y4)
cc	uc = -x1s*dsqrt(s)*pt4/z4*dexp(-y4)
** 	if (iauren.eq.1) then
** 	  mf = dsqrt(sc)
** 	endif
	call strfrad(x1s,ih1,x2s,ih2,sf)
	call strfrad(x10,ih1,x20,ih2,sf0)
	call spart45zd(sc,tc,uc,z4,mf,pt4,part45z)
	call spart45zd(sc,tc,uc,un,mf,pt4,part45z0)
	call spart45ld(sc,tc,uc,z4,part45l)
	call spart45ld(sc,tc,uc,un,part45l0)
c
	call vec_dmult_vector(part45z,sf,k0max,temp0)
	call vec_dmult_vector(part45z0,sf0,k0max,temp1)
	ty1 = 1.d0/z4**2
	call vec_dmult_sub(temp1,temp0,k0max,ty1,temp2)
	ty2 = 1.d0/(1.d0-z4)
	call vec_dmult_constant(temp2,k0max,ty2,temp3)
c
	call vec_dmult_vector(part45l,sf,k0max,temp4)
	call vec_dmult_vector(part45l0,sf0,k0max,temp5)
	call vec_dmult_sub(temp5,temp4,k0max,ty1,temp6)
	ty3 = dlog(1.d0-z4)/(1.d0-z4)
	call vec_dmult_constant(temp6,k0max,ty3,temp7)
c
	ty4 = gpt4
	call vec_dadd_mult_constant(temp3,temp7,k0max,ty4,sy4p)
	return
	end
c*****************************************************	
	subroutine ssy4pp(s,gpt4,x10,x20,z4,y3,y4,mf,ih1,ih2,sy4p)
	implicit real*8 (a-h,l-z)
** 	common/aurenche/iauren
	parameter (k0max=22)
	dimension part45z(k0max),part45l(k0max)
** 	dimension part45z0(k0max),part45l0(k0max),sf0(k0max)
	dimension sf(k0max),sy4p(k0max)
	dimension temp0(k0max),temp1(k0max),temp2(k0max),temp3(k0max)
	dimension temp4(k0max)
** 	dimension temp4(k0max),temp5(k0max),temp6(k0max),temp7(k0max)
	zero = 0.d0
	un = 1.d0
	pt4 = gpt4
	x1s = x10/z4
	x2s = x20/z4
c en principe vrai mais a verif
	if (x1s.gt.un.or.x2s.gt.un) then
	  call vec_dinit(sy4p,k0max,zero)
	  write (18,*) 'attention  x1s > 1 ou x2s > 1',
     #		x1s,x2s
	  return
	endif
c attention les elements de matrice 2 -> 2 sont independant de 
c l'echelle d'energie
	sc = x10*x20*s
	tc = -x20*dsqrt(s)*pt4*dexp(y4)
	uc = -x10*dsqrt(s)*pt4*dexp(-y4)
cc	sc = x1s*x2s*s
cc	tc = -x2s*dsqrt(s)*pt4/z4*dexp(y4)
cc	uc = -x1s*dsqrt(s)*pt4/z4*dexp(-y4)
** 	if (iauren.eq.1) then
** 	  mf = dsqrt(sc)
** 	endif
	call strfrad(x1s,ih1,x2s,ih2,sf)
** 	call strfrad(x10,ih1,x20,ih2,sf0)
	call spart45zd(sc,tc,uc,z4,mf,pt4,part45z)
** 	call spart45zd(sc,tc,uc,un,mf,pt4,part45z0)
	call spart45ld(sc,tc,uc,z4,part45l)
** 	call spart45ld(sc,tc,uc,un,part45l0)
c
	call vec_dmult_vector(part45z,sf,k0max,temp0)
** 	call vec_dmult_vector(part45z0,sf0,k0max,temp1)
** 	ty1 = 1.d0/z4**2
** 	call vec_dmult_sub(temp1,temp0,k0max,ty1,temp2)
	ty2 = 1.d0/(1.d0-z4)
	call vec_dmult_constant(temp0,k0max,ty2,temp1)
c
	call vec_dmult_vector(part45l,sf,k0max,temp2)
** 	call vec_dmult_vector(part45l0,sf0,k0max,temp5)
** 	call vec_dmult_sub(temp5,temp4,k0max,ty1,temp6)
	ty3 = dlog(1.d0-z4)/(1.d0-z4)
	call vec_dmult_constant(temp3,k0max,ty3,temp4)
c
	ty4 = gpt4/z4**2
	call vec_dadd_mult_constant(temp1,temp4,k0max,ty4,sy4p)
	return
	end
c*****************************************************
c terme a l'ordre de born	
c*****************************************************	
	subroutine sfborn(s,n,gpt3,y3,y4,ih1,ih2,fborn)
	implicit real*8 (a-h,l-z)
	parameter (k0max=22)
	dimension sf1(k0max),rborn1(k0max)
	dimension fborn(k0max),temp0(k0max)
	zero = 0.d0
	un = 1.d0
	ys = (y3-y4)/2.d0
	y = (y3+y4)/2.d0
	pt = gpt3
	x10 = 2.d0*pt/dsqrt(s)*dexp(y)*dcosh(ys)	
	x20 = 2.d0*pt/dsqrt(s)*dexp(-y)*dcosh(ys)
	if (x10.gt.un.or.x20.gt.un) then
	  call vec_dinit(fborn,k0max,zero)
	  return
	endif
	sc = 4.d0*pt**2*dcosh(ys)**2
	tc = -2.d0*pt**2*dcosh(ys)*dexp(-ys)
	uc = -2.d0*pt**2*dcosh(ys)*dexp(ys)
	call strfrad(x10,ih1,x20,ih2,sf1)
	call bornd(sc,tc,uc,n,rborn1)
	call vec_dmult_vector(rborn1,sf1,k0max,temp0)
	ty3 = gpt3
	call vec_dmult_constant(temp0,k0max,ty3,fborn)
	return
	end
c
	subroutine xthet(zmax,bsup)
	implicit real*8 (a-h,l-z)
	un = 1.d0
	if (zmax.eq.un) then
	  bsup = un
	else if (zmax.lt.un) then
	  bsup = 0.d0
	endif
	return
	end 
c
	subroutine glgl_glga(s,n,gpt3,y3,y4,ih1,ih2,boite)
	implicit real*8 (a-h,l-z)
	dimension f1(-6:6),f2(-6:6)
	common/scale/m,mf,mu
	un = 1.d0
	vc = (n*n-1.d0)
	ys = (y3-y4)/2.d0
	y = (y3+y4)/2.d0
	pt = gpt3
	x10 = 2.d0*pt/dsqrt(s)*dexp(y)*dcosh(ys)	
	x20 = 2.d0*pt/dsqrt(s)*dexp(-y)*dcosh(ys)
	if (x10.gt.un.or.x20.gt.un) then
	  boite = 0.d0
	  return
	endif
	sc = 4.d0*pt**2*dcosh(ys)**2
	tc = -2.d0*pt**2*dcosh(ys)*dexp(-ys)
	uc = -2.d0*pt**2*dcosh(ys)*dexp(ys)
	call fstru(x10,m*m,ih1,f1)
	call fstru(x20,m*m,ih2,f2)
	boite = gggp(sc,tc,uc)*f1(0)*f2(0)/(x10*x20)/(4.d0*vc**2)
	boite = boite*gpt3
	return
	end 
