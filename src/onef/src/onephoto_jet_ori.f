c
c starting point le 09/06/00
c on ajoute ce common qui porte sur la cinematique du jet (partons)
c	common/param_jet/y_jetsup,y_jetinf,gpt_jetsup,gpt_jetinf
c bon accord avec l'ancien programme le 04/10/00
	subroutine onef_sub
	implicit real*8 (a-h,l-v,x-z)
	implicit real*4 (w)
	logical lo,nlo,gener,integ,intega
	logical lvirt
	character*128 path_bsfile
        integer*4 ntrack,iprov
        parameter (irandom=500)
	common/npt/inpt
	common/bparm1/xl(50),xu(50),idim,iwild,ig(50),icall
	common/bparm2/acc1,acc2,itmx1,itmx2
	common/born/iborn
	common/baspring/ispring
	common/approx/lo,nlo
	common/calcul/integ,intega,gener
 	common/nbevent/inbevent
	common/baseparam/jtmx1,jtmx2,jcall,jcall1,jcall2,ixtry
	common/accuracy/accu
	common/randd/rnumb
	common/cheminbs/path_bsfile
	common/long/ilen
	common/processo/j_processo_min,j_processo_max
	common/parami/ysup,yinf,gptsup,gptinf,s
	common/param_jet/y_jetsup,y_jetinf,gpt_jetsup,gpt_jetinf
	common/testcount/j(11)
	dimension wrvec(irandom),wrvec1(irandom)
	external f4dimo3,f4dimo4,f2dimo1,f2dimo3,f1dimo
c
	pi=datan(1.d0)*4.d0
	half = 0.5d0
c******************************************************************
c       partie 2 --> 3
c******************************************************************
c
	lvirt = .false.
	do i=1,11
	  j(i) = 0
	enddo
c
cccc
	if (nlo) then
cccc
	iborn = 0
c###
	if (integ) then
	ispring = 0
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall1
	idim = 7
	iwild = 7
	do i=1,idim
	  xl(i) = 0.d0
	  xu(i) = 1.d0
	enddo
	call bases (f4dimo3,resf34,sdf34,ctime,it1,it2)
	endif
c###
c
c###
	if (intega) then
	ispring = 1
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall1
	idim = 7
	iwild = 7
	do i=1,idim
	  xl(i) = 0.d0
	  xu(i) = 1.d0
	enddo
	call bases (f4dimo3,resf34,sdf34,ctime,it1,it2)
c on ecrit la grille
	open(111,file=path_bsfile(1:ilen)//'threepart1.bs',
     #	status='unknown',form='unformatted')
	call bswrit(111)
	close(111)
c	
	endif
c###
c
c###
	if (integ) then
	ispring = 0
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall1
	idim = 7
	iwild = 7
	do i=1,idim
	  xl(i) = 0.d0
	  xu(i) = 1.d0
	enddo
	call bases (f4dimo4,resf44,sdf44,ctime,it1,it2)
	endif
c###
c
c###
	if (intega) then
	ispring = 1
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall1
	idim = 7
	iwild = 7
	do i=1,idim
	  xl(i) = 0.d0
	  xu(i) = 1.d0
	enddo
	call bases (f4dimo4,resf44,sdf44,ctime,it1,it2)
c on ecrit la grille
	open(112,file=path_bsfile(1:ilen)//'threepart2.bs',
     #	status='unknown',form='unformatted')
	call bswrit(112)
	close(112)
c	
	endif
c###
c******************************************************************
c       partie 2 --> 2 colineaire
c******************************************************************
c
c###
	if (integ) then
	ispring = 0
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall
	idim = 5
	iwild = 5
	do i=1,idim
          xl(i) = 0.d0
          xu(i) = 1.d0
	enddo
	call bases (f2dimo1,resf21,sdf21,ctime,it1,it2)
	endif
c###
c
c###
	if (intega) then
	ispring = 1
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall
	idim = 5
	iwild = 5
	do i=1,idim
          xl(i) = 0.d0
          xu(i) = 1.d0
	enddo
	call bases (f2dimo1,resf21,sdf21,ctime,it1,it2)
c on ecrit la grille
	open(221,file=path_bsfile(1:ilen)//'qtwopart1.bs',
     #	status='unknown',form='unformatted')
	call bswrit(221)
	close(221)
c	
	endif
c###
c
c###
	if (integ) then
	ispring = 0
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall
	idim = 5
	iwild = 5
	do i=1,idim
          xl(i) = 0.d0
          xu(i) = 1.d0
	enddo
	call bases (f2dimo3,resf23,sdf23,ctime,it1,it2)
	endif
c###
c
c###
	if (intega) then
	ispring = 1
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall
	idim = 5
	iwild = 5
	do i=1,idim
          xl(i) = 0.d0
          xu(i) = 1.d0
	enddo
	call bases (f2dimo3,resf23,sdf23,ctime,it1,it2)
c on ecrit la grille
	open(223,file=path_bsfile(1:ilen)//'qtwopart3.bs',
     #	status='unknown',form='unformatted')
	call bswrit(223)
	close(223)
c	
	endif
c###
c
c###
c
c******************************************************************
c       partie 2 --> 2 virtuelle et born
c******************************************************************
	lvirt = (j_processo_min.eq.2.and.j_processo_max.eq.2).or.
     #		(j_processo_min.eq.4.and.j_processo_max.eq.4).or.
     #		(j_processo_min.eq.6.and.j_processo_max.eq.6).or.
     #		(j_processo_min.eq.10.and.j_processo_max.eq.10).or.
     #		(j_processo_min.eq.11.and.j_processo_max.eq.11).or.
     #		(j_processo_min.eq.12.and.j_processo_max.eq.12)
c###
	if (integ) then
	ispring = 0
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall2
	idim = 4
	iwild = 4
	do i=1,idim
          xl(i) = 0.d0
          xu(i) = 1.d0
        enddo
	if (lvirt) then
	  resf1 = 0.d0
	  sdf1 = 0.d0
	else
	  call bases (f1dimo,resf1,sdf1,ctime,it1,it2)
	endif
	endif
c###
c
c###
	if (intega) then
	ispring = 1
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall2
	idim = 4
	iwild = 4
	do i=1,idim
          xl(i) = 0.d0
          xu(i) = 1.d0
        enddo
	call bases (f1dimo,resf1,sdf1,ctime,it1,it2)
c on ecrit la grille
	open(331,file=path_bsfile(1:ilen)//'twopart1.bs',
     #	status='unknown',form='unformatted')
	call bswrit(331)
	close(331)
c	
	endif
c###
c
cccc
	endif
cccc
cccc
	if (lo) then
cccc
c
	if (intega.or.integ) then
	iborn = 1
	ispring = 1
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall2
	idim = 4
	iwild = 4
	do i=1,idim
          xl(i) = 0.d0
          xu(i) = 1.d0
        enddo
	if (lvirt) then
	  resf1 = 0.d0
	  sdf1 = 0.d0
	else
	  call bases (f1dimo,resfb,sdfb,ctime,it1,it2)
c on ecrit la grille
	  open(332,file=path_bsfile(1:ilen)//'twopart2.bs',
     #	  status='unknown',form='unformatted')
	  call bswrit(332)
	  close(332)
	endif
c
	endif
cccc
	endif
cccc
c******************************************************************
c       calcul de la section efficace totale ordre superieur
c******************************************************************
	if (integ) then
	  resf2 = resf21+resf23
	  sdf2 = sdf21+sdf23
c
	  resho = resf34+resf44+resf2+resf1
	  sdho = sdf34+sdf44+sdf2+sdf1
	  resb = resfb
	  sdfb = sdfb
	  write (8,*) 'cccccccccccccccccccccc'
	  write (8,*) 'born',resfb,' sdfb',sdfb
	  write (8,*) 'h.o.',resho,' sdfho',sdho
	  write (8,*) 'cccccccccccccccccccccc'
	endif
c******************************************************************
c       calcul du nombre d'evenements a generer pour chacune des
c       parties
c******************************************************************
	if (intega) then
	  if (nlo) then
	    resf2 = resf21+resf23
	    totho = resf34+resf44+resf2+resf1
	  else
	    totho = 0.d0
	  endif
	  if (lo) then
	    totb = resfb
	  else
	    totb = 0.d0
	  endif
	  tot = totho + totb
	  if (tot.eq.0.d0) then
	    write (8,*) 'tot=0. job finished'
	    return
	  endif
	  open(666,file=path_bsfile(1:ilen)//'integral.res',
     #	  status='unknown')
	  write(666,*) resf34, resf44
	  write(666,*) resf21, resf23
	  write(666,*) resf1,resfb
	  write(666,*) tot
	  close(666)	  
	endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccc              generation des evenements                     ccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c******************************************************************
cccc
	if (gener) then
cccc
	open(666,file=path_bsfile(1:ilen)//'integral.res',
     #	status='unknown')
	read(666,*) resf34, resf44
	read(666,*) resf21, resf23
	read(666,*) resf1,resfb
	read(666,*) tot
	close(666)
	xnorm = dfloat(inbevent)/tot
c###	  
c   saves for normalization
        write(12,100) inbevent,tot
100     format(1x,i12,d12.5)
cccc
	if (nlo) then
cccc
c###
	iborn = 0
c###
	ispring = 1
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall1
	idim = 7
	iwild = 7
	do i=1,idim
	  xl(i) = 0.d0
	  xu(i) = 1.d0
	enddo
	inbrealevent = int(resf34*xnorm)
c        
	open(111,file=path_bsfile(1:ilen)//'threepart1.bs',
     #	status='unknown',form='unformatted')
	call bsread(111)
	close(111)
c	
	write(6,*)
	write(6,*)'============================================='
	write(6,*)'event generation : 3 partons : 5//3'
	write(6,*) inbrealevent,' events'
	write(6,*)'============================================='
	write(6,*)
c	
        iprov = 34
        ntrack = 3
        poid = 1.d0
	do i = 1,inbrealevent
	  i1 = mod(i-1,irandom)
	  if (i1.eq.0) then
            call ranlux(wrvec,irandom)
            call ranlux(wrvec1,irandom)
          endif
	  rnumb = dble(wrvec(i1+1))
	  rnumb1 = dble(wrvec1(i1+1))
	  call spring(f4dimo3,ixtry)
	  call doubletosingleo(rnumb1,pi,poid,iprov,ntrack)
	  call gfill
	enddo
c###
c
c###
	ispring = 1
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall1
	idim = 7
	iwild = 7
	do i=1,idim
          xl(i) = 0.d0
          xu(i) = 1.d0
	enddo
	inbrealevent = int(resf44*xnorm)
c
	open(112,file=path_bsfile(1:ilen)//'threepart2.bs',
     #	status='unknown',form='unformatted')
	call bsread(112)
	close(112)
c	
	write(6,*)
	write(6,*)'============================================='
	write(6,*)'event generation : 3 partons : 5//4'
	write(6,*) inbrealevent,' events'
	write(6,*)'============================================='
	write(6,*)
c	
        iprov = 44
        ntrack = 3
        poid = 1.d0
	do i = 1,inbrealevent
	  i1 = mod(i-1,irandom)
	  if (i1.eq.0) then
            call ranlux(wrvec,irandom)
            call ranlux(wrvec1,irandom)
          endif
	  rnumb = dble(wrvec(i1+1))
	  rnumb1 = dble(wrvec1(i1+1))
	  call spring(f4dimo4,ixtry)
	  call doubletosingleo(rnumb1,pi,poid,iprov,ntrack)
	  call gfill
	enddo
c###
c
c###
	ispring = 1
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall
	idim = 5
	iwild = 5
	do i=1,idim
          xl(i) = 0.d0
          xu(i) = 1.d0
	enddo
	inbrealevent = int(resf21*xnorm)
c
	open(221,file=path_bsfile(1:ilen)//'qtwopart1.bs',
     #	status='unknown',form='unformatted')
	call bsread(221)
	close(221)
c	
	write(6,*)
	write(6,*)'============================================='
	write(6,*)'event generation : quasi 2 partons : col. ini'
	write(6,*) inbrealevent,' events'
	write(6,*)'============================================='
	write(6,*)
c	
        iprov = 21
        ntrack = 2
        poid = 1.d0
	do i = 1,inbrealevent
	  i1 = mod(i-1,irandom)
	  if (i1.eq.0) then
             call ranlux(wrvec1,irandom)
          endif
	  rnumb1 = dble(wrvec1(i1+1))
	  call spring(f2dimo1,ixtry)
	  call doubletosingleo(rnumb1,pi,poid,iprov,ntrack)
	  call gfill
	enddo
c###
c
c###
	ispring = 1
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall
	idim = 5
	iwild = 5
	do i=1,idim
          xl(i) = 0.d0
          xu(i) = 1.d0
	enddo
	inbrealevent = int(resf23*xnorm)
c
	open(223,file=path_bsfile(1:ilen)//'qtwopart3.bs',
     #	status='unknown',form='unformatted')
	call bsread(223)
	close(223)
c	
	write(6,*)
	write(6,*)'============================================='
	write(6,*)'event generation : quasi 2 partons : col. fi4'
	write(6,*) inbrealevent,' events'
	write(6,*)'============================================='
	write(6,*)
c	
        iprov = 23
        ntrack = 2
        poid = 1.d0
	do i = 1,inbrealevent
	  i1 = mod(i-1,irandom)
	  if (i1.eq.0) then
             call ranlux(wrvec1,irandom)
          endif
	  rnumb1 = dble(wrvec1(i1+1))
	  call spring(f2dimo3,ixtry)
	  call doubletosingleo(rnumb1,pi,poid,iprov,ntrack)
	  call gfill
	enddo
c###
c
c###
	ispring = 1
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall2
	idim = 4
	iwild = 4
	do i=1,idim
          xl(i) = 0.d0
          xu(i) = 1.d0
        enddo
	inbrealevent = int(resf1*xnorm)
c
	open(331,file=path_bsfile(1:ilen)//'twopart1.bs',
     #	status='unknown',form='unformatted')
	call bsread(331)
	close(331)
c	
	write(6,*)
	write(6,*)'============================================='
	write(6,*)'event generation : 2 partons : virtuelle'
	write(6,*) inbrealevent,' events'
	write(6,*)'============================================='
	write(6,*)
c	
        iprov = 10
        ntrack = 2
        poid = 1.d0
	do i = 1,inbrealevent
	  i1 = mod(i-1,irandom)
	  if (i1.eq.0) then
             call ranlux(wrvec1,irandom)
          endif
	  rnumb1 = dble(wrvec1(i1+1))
	  call spring(f1dimo,ixtry)
	  call doubletosingleo(rnumb1,pi,poid,iprov,ntrack)
	  call gfill
	enddo
c###
c
cccc
	endif
cccc
c###
	if (lo) then
c###
	inbrealevent = int(resfb*xnorm)
c	
	iborn = 1
	ispring = 1
	call bsinit
	acc1 = accu
	acc2 = accu
	itmx1 = jtmx1
	itmx2 = jtmx2
	icall = jcall2
	idim = 4
	iwild = 4
	do i=1,idim
          xl(i) = 0.d0
          xu(i) = 1.d0
        enddo
c
	open(332,file=path_bsfile(1:ilen)//'twopart2.bs',
     #	status='unknown',form='unformatted')
	call bsread(332)
	close(332)
c	
	write(6,*)
	write(6,*)'============================================='
	write(6,*)'event generation : 2 partons : born'
	write(6,*) inbrealevent,' events'
	write(6,*)'============================================='
	write(6,*)
c	
        iprov = 11
        ntrack = 2
        poid = 1.d0
	do i = 1,inbrealevent
	  i1 = mod(i-1,irandom)
	  if (i1.eq.0) then
            call ranlux(wrvec1,irandom)
          endif
	  rnumb1 = dble(wrvec1(i1+1))
	  call spring(f1dimo,ixtry)
	  call doubletosingleo(rnumb1,pi,poid,iprov,ntrack)
	  call gfill
	enddo
c###
	endif  
cccc
	endif  
cccc
	return
	end
c
c*****************************************************
c  partie ou pt5 < ptm
c*****************************************************
c integrale a 4 dimensions
	double precision function f4dimo3(xx)
	implicit real*8 (a-h,l-v,x-z)
	implicit real*4 (w)
	logical isol,cut_jet,merged
	common/hadron/ih1,ih2,ih3,ih4
	common/coul/n,cf,gtr
	common/parami/ysup,yinf,gptsup,gptinf,s
	common/param_jet/y_jetsup,y_jetinf,gpt_jetsup,gpt_jetinf
	common/coup/ptm,r
	common/flag/icoup
	common/coupexp/ptm_exp,r_exp
	common/flagexp/icoup_exp
	common/alem/iloopem
	common/scale/m,mf,mu
	common/faconv/hc2
	common/sortie/xpt1,xy1,xpt2,xy2,xfi12,xpt3,xy3,xfi13,resfunc
	common/sfragv/xfrag1,xfrag2
	common/baspring/ispring
	common/nboucle/iloop
	common/aurenche/iauren
	common/processo/j_processo_min,j_processo_max
	common/typesc/ichoi_scale
	common/normasc/cm,cmu,cmf
	common/randd/rnumb
	common/coup_histo/r_isol,etmax
	common/jets/jetmode
	common/jet_coup/yjet_min,yjet_max,ptjet_min,ptjet_max
	common/jet_para/r_kt,r_c,r_sep
	character*2 algorithm,merging,acceptance
	common/algorithme/algorithm,merging,acceptance
	dimension xx(20)
	parameter (k0max=32)
	dimension sf0(k0max),camp0(k0max)
	dimension temp0(k0max),temp1(k0max),spcou(k0max)
	dimension p1(4),p2(4),pp4(4)
	dimension p3(4),p4(4),p5(4)
	dimension p1p(4),p2p(4),p4p(4),p3p(4),p5p(4)
	dimension xej(2),xpjx(2),xpjy(2),xpjz(2)
	pi=datan(1.d0)*4.d0
	un = 1.d0
c ------------------ y3 -------------------------------------------
	y3max = y_jetsup
	y3min = y_jetinf
	y3 = y3min + (y3max-y3min)*xx(1)
c ------------------ y4 -------------------------------------------
	y4max = ysup
	y4min = yinf
	y4 = y4min + (y4max-y4min)*xx(2)
c ------------------ gpt4 ------------------------------------------
	gpt4maxp = dmin1(gptsup,dsqrt(s)/(2.d0*dcosh(y4)))
	gpt4max = dmax1(gptinf,gpt4maxp)
	gpt4min = gptinf
	gpt4 = gpt4min + (gpt4max-gpt4min)*xx(3)
c ------------------ x4 ---------------------------------------------
	x4max = 1.d0
	x4min_incl = 2.d0*gpt4/dsqrt(s)*dcosh(y4)
	x4min_isol = gpt4/(gpt4+etmax)
	x4minp = dmax1(x4min_incl,x4min_isol)
	x4min = dmin1(x4max,x4minp)
	x4 = x4min + (x4max-x4min)*xx(4)
	pt4 = gpt4/x4
c ------------------ pt5 --------------------------------------------
c x1c_min = pt5/dsqrt(s)*exp(y3) + pt4/dsqrt(s)*exp(y4)
c x2c_min = pt5/dsqrt(s)*exp(-y3) + pt4/dsqrt(s)*exp(-y4)
c comme pt5**2 <= s*(1-x1c)*(1-x2c), on a donc
c pt5**2 <= s*(1-x1c_min)*(1-x2c_min) d'ou
c pt5 <= (s + pt4**2 -2*rs*pt4*cosh(y4))/
c (2*(rs*cosh(y3)-pt4*cosh(y3-y4))))
	pt5max1 = (dsqrt(s)-pt4*dexp(y4))*dexp(-y3)
	pt5max2 = (dsqrt(s)-pt4*dexp(-y4))*dexp(y3)
	at1 = 2.d0*(dsqrt(s)*dcosh(y3)-pt4*dcosh(y3-y4))
	if (at1.gt.0.d0) then
	  pt5max3 = (s+pt4**2-2.d0*dsqrt(s)*pt4*dcosh(y4))/at1
	else
	  pt5max3 = dsqrt(s)/2.d0
	endif
	pt5max = dmin1(pt5max1,pt5max2,pt5max3)
	pt5min = ptm
	if (pt5min.gt.pt5max) then
	  f4dimo3 = 0.d0
	  return
	endif	
	pt5 = pt5min + (pt5max-pt5min)*xx(5)
c ------------------ fi35 -------------------------------------------
c x1c_min = pt5/dsqrt(s)*exp(y3) + pt4/dsqrt(s)*exp(y4)
c x2c_min = pt5/dsqrt(s)*exp(-y3) + pt4/dsqrt(s)*exp(-y4)
	x11 = pt5/dsqrt(s)*dexp(y3)+pt4/dsqrt(s)*dexp(y4)
	x21 = pt5/dsqrt(s)*dexp(-y3)+pt4/dsqrt(s)*dexp(-y4)
c en principe vrai
	if (x11.gt.un.or.x21.gt.un) then
	  f4dimo3 = 0.d0
	  return
	endif
	sy5max = dlog(dsqrt(s)/pt5*(1.d0-x11))
	sy5min = -dlog(dsqrt(s)/pt5*(1.d0-x21))
c en principe vrai
	if (sy5max.lt.sy5min) then
	  f4dimo3 = 0.d0
	  return
	endif
	if (pt5.le.pt4/2.d0) then
	  fimin = 0.d0
	else
	  fimin = dacos(pt4**2/(2.d0*pt5**2)-1.d0)
	endif
	gpt3max = dmin1(pt4+pt5,dsqrt(s)/(2.d0*dcosh(y3))
     #	,gpt_jetsup)
	xarg = (pt4**2-gpt3max**2-pt5**2)/
     #	(2.d0*gpt3max*pt5)
	if (xarg.lt.-1.d0) then
	  xmax_fi = pi
	else if (xarg.gt.1.d0) then
	  f4dimo3 = 0.d0
	  return
	else
	  xmax_fi = dacos(xarg)
	endif
	fimax = dmin1(pi,xmax_fi)
	if (y3.le.sy5max.and.y3.ge.sy5min) then
	  omegamin = datan(fimin/(sy5max-y3))
	  omegamax = pi-datan(fimin/dabs(sy5min-y3))
	  srmax2 = dmax1((sy5max-y3)**2+fimax**2,
     #	  (sy5min-y3)**2+fimax**2)
	  rmax2 = dsqrt(srmax2)
	  rmin2 = fimin
	else if (y3.lt.sy5max.and.y3.lt.sy5min) then
	  omegamin = datan(fimin/(sy5max-y3))
	  omegamax = datan(fimax/(sy5min-y3))
	  srmax2 = (sy5max-y3)**2+fimax**2
	  rmax2 = dsqrt(srmax2)
	  rmin2 = dsqrt(fimin**2+(sy5min-y3)**2)
	else if (y3.gt.sy5max.and.y3.gt.sy5min) then
	  omegamin = pi-datan(fimax/dabs(sy5max-y3))
	  omegamax = pi-datan(fimin/dabs(sy5min-y3))
	  srmax2 = (sy5min-y3)**2+fimax**2
	  rmax2 = dsqrt(srmax2)
	  rmin2 = dsqrt(fimin**2+(sy5max-y3)**2)
	endif
	if (omegamin.le.omegamax) then
	  omega = omegamin + (omegamax-omegamin)*xx(6)
	else	  
	   f4dimo3 = 0.d0
	   return
	endif
	xbig = 1.d+20
	xsmall = 1.d-12
	if (omega.lt.xsmall.or.omega.gt.(pi-xsmall)) then
	  rmax1 = xbig
	else
	  rmax1 = fimax/dsin(omega)
	endif
	rrmax = dmin1(rmax1,rmax2)
	rrmin = rmin2
	if (rrmin.le.rrmax) then
	  rr = rrmin + (rrmax-rrmin)*xx(7)
	else	  
	   f4dimo3 = 0.d0
	   return
	endif
	fi35 = rr*dsin(omega)
	if (fi35.gt.pi) then
	   f4dimo3 = 0.d0
	   return
	endif
c ------------------ gpt3 ------------------------------------------
	gpt3min = dmax1(pt5,gpt_jetinf)
	if ((pt4-pt5*dsin(fi35)).le.0.d0) then
	  f4dimo3 = 0.d0
	  return
	endif
	gpt3 = -pt5*dcos(fi35)+dsqrt(pt4**2-(pt5*dsin(fi35))**2)
	if (gpt3.gt.gpt3max.or.gpt3.lt.gpt3min) then
	  f4dimo3 = 0.d0
	  return
	endif
	pt3 = gpt3
c ------------------ x -------------------------------------------
	x1c = gpt3/dsqrt(s)*dexp(y3) + pt4/dsqrt(s)*dexp(y4)
	x2c = gpt3/dsqrt(s)*dexp(-y3) + pt4/dsqrt(s)*dexp(-y4)
	if (x1c.ge.un.or.x2c.ge.un) then
	  f4dimo3 = 0.d0
	  return
	endif
	sup_pt5 = dsqrt(s*(1.d0-x1c)*(1.d0-x2c))
	if (pt5.gt.sup_pt5) then
	  f4dimo3 = 0.d0
	  return
	endif	
c ------------------ y5 -------------------------------------------
	y5max = dlog(dsqrt(s)/pt5*(1.d0-x1c))
	y5min = -dlog(dsqrt(s)/pt5*(1.d0-x2c))
	y5 = y3 + rr*dcos(omega)
	if (y5.gt.y5max.or.y5.lt.y5min) then
	   f4dimo3 = 0.d0
	   return
	endif
c ------------------ x -------------------------------------------
	x1 = x1c + pt5/dsqrt(s)*dexp(y5)
	x2 = x2c + pt5/dsqrt(s)*dexp(-y5)
	if (x1.ge.un.or.x2.ge.un) then
	  f4dimo3 = 0.d0
	  return
	endif
c coupure en angle-------------------------------------------------
        r35s = (y3-y5)**2+fi35**2
        rs = r**2
	icorr = 0
        if (r35s.lt.rs) then	
           icorr = 1
        endif
c coupure en isolement-------------------------------------------------
	cos_fi45 = -(gpt3*dcos(fi35)+pt5)/pt4
	fi45 = zacos(cos_fi45)
	call isolement(y4,y5,fi45,x4,pt5,gpt4,r_isol,etmax,
     #	isol)
	xisol = 1.d0
	if (.not.(isol)) then
	  if (icorr.eq.0) then
	    f4dimo3 = 0.d0
	    return
	  else
	    xisol = 0.d0
	  endif
	endif
c ------------------ fin isolement ------------------------------------
	if (iauren.eq.0) then
	  xjacob = (y3max-y3min)*(y4max-y4min)*(gpt4max-gpt4min)*
     #	  (x4max-x4min)*rr*(rrmax-rrmin)*(omegamax-omegamin)*
     #	  (pt5max-pt5min)
	else if (iauren.eq.1) then
	  xjacob = (y3max-y3min)*
     #	  (x4max-x4min)*rr*(rrmax-rrmin)*(omegamax-omegamin)*
     #	  (pt5max-pt5min)
	endif
	x1p = pt4/dsqrt(s)*(dexp(y3)+dexp(y4))
	x2p = pt4/dsqrt(s)*(dexp(-y3)+dexp(-y4))
	gpt3p = (pt4-pt5)
c ------------------------------------------- 
	p1(1) = x1*dsqrt(s)/2.d0
	p1(2) = 0.d0
	p1(3) = 0.d0
	p1(4) = x1*dsqrt(s)/2.d0
c ------------------------------------------- 
	p2(1) = x2*dsqrt(s)/2.d0
	p2(2) = 0.d0
	p2(3) = 0.d0
	p2(4) = -x2*dsqrt(s)/2.d0
c ------------------------------------------- 
	p4(1) = gpt4*dcosh(y4)
	p4(2) = gpt4
	p4(3) = 0.d0
	p4(4) = gpt4*dsinh(y4)
c ------------------------------------------- 
	pp4(1) = pt4*dcosh(y4)
	pp4(2) = pt4
	pp4(3) = 0.d0
	pp4(4) = pt4*dsinh(y4)
c ------------------------------------------- 
c attention fi35 est l'angle entre 3 et 5
	p5(1) = pt5*dcosh(y5)
	p5(2) = -pt5*(gpt3*dcos(fi35)+pt5)/pt4
	p5(3) = -pt5*gpt3*dsin(fi35)/pt4
	p5(4) = pt5*dsinh(y5)
c ------------------------------------------- 
	p3(1) = p1(1) + p2(1) - pp4(1) - p5(1)
	p3(2) = p1(2) + p2(2) - pp4(2) - p5(2)
	p3(3) = p1(3) + p2(3) - pp4(3) - p5(3)
	p3(4) = p1(4) + p2(4) - pp4(4) - p5(4)
c -------------------------------------------
	p1p(1) = x1p*dsqrt(s)/2.d0
	p1p(2) = 0.d0
	p1p(3) = 0.d0
	p1p(4) = x1p*dsqrt(s)/2.d0
c ------------------------------------------- 
	p2p(1) = x2p*dsqrt(s)/2.d0
	p2p(2) = 0.d0
	p2p(3) = 0.d0
	p2p(4) = -x2p*dsqrt(s)/2.d0
c ------------------------------------------- 
	p5p(1) = pt5*dcosh(y3)
	p5p(2) = -pt5
	p5p(3) = 0.d0
	p5p(4) = pt5*dsinh(y3)
c ------------------------------------------- 
	p3p(1) = p1p(1) + p2p(1) - pp4(1) - p5p(1)
	p3p(2) = p1p(2) + p2p(2) - pp4(2) - p5p(2)
	p3p(3) = p1p(3) + p2p(3) - pp4(3) - p5p(3)
	p3p(4) = p1p(4) + p2p(4) - pp4(4) - p5p(4)
c -------------------------------------------
c coupure jet-------------------------------------------------
c if no jets are measured, cut_jet is always true since any cut 
c conditions will be fulfilled
	if (jetmode.eq.0) then
	  cut_jet = .true.
	else
	  fi3c = zacos(p3(2)/gpt3)
	  fi3s = zasin(p3(3)/gpt3)
	  fi3 = fi3c+2.d0*fi3s
	  fi5 = zacos(p5(2)/pt5)
	  test = fi3-fi5-fi35
	  tolerance = 1.d-6
	  if (test.gt.tolerance) then
	    write (27,*) 'attention:',test,fi35,fi3,fi5
	  endif
	  call jet_selection(y3,y5,fi35,fi3,fi5,gpt3,pt5,
     #	  algorithm,r_kt,r_c,r_sep,ptjet_min,ptjet_max,yjet_min,
     #	  yjet_max,merging,acceptance,
     #	  inb_jet,xej,xpjx,xpjy,xpjz,cut_jet,merged)
        endif
	xjet = 1.d0
	if (.not.(cut_jet)) then
	  if (icorr.eq.0) then
	    f4dimo3 = 0.d0
	    return
	  else if (icorr.eq.1) then
	    xjet = 0.d0
	  endif
	endif
c ------------------ fin jet ------------------------------------
c set the scales-------------------------------------------------
	if (iauren.eq.0) then
	  call choiscale(p3,p4,cm,cmu,cmf,ichoi_scale,m,mu,mf)
	else if (iauren.eq.1) then
	  m = gpt4*cm
	  mu = m
	  mf = m
	endif
	call fspcouo(n,spcou)
c	
c c_{ij} = alpha_s*alpha^2/(4*c_i*c_j*pi*s^2)
	cij = alfas(iloop,mu*mu)**3/(pi*s*s)
c
	call strfrao(x1,ih1,x2,ih2,x4,ih4,sf0)
	call ampo3(s,x1,x2,y3,gpt3,y5,pt5,fi35,camp0)
	call vec_dmult_vector(camp0,sf0,k0max,temp0)
	call vec_dmult_vector(temp0,spcou,k0max,temp1)
	f4dimo3 = 0.d0
	do i = j_processo_min,j_processo_max
	  f4dimo3 = temp1(i) + temp1(16+i) + f4dimo3
	enddo
	f4dimo3 = cij*f4dimo3*gpt3/dsqrt(pt4**2-(pt5*dsin(fi35))**2)
	if (icorr.eq.1) then
c coupure en isolement-------------------------------------------------
	  call isolement(y4,y3,pi,x4,pt5,gpt4,r_isol,etmax,
     #	  isol)
	  xisolp = 1.d0
	  if (.not.(isol)) then
	    xisolp = 0.d0
	  endif
c ------------------ fin isolement ------------------------------------
c ------------------coupure jet------------------------------
c if no jets are measured, cut_jet is always true since any cut 
c conditions will be fulfilled
	  if (jetmode.eq.0) then
	    cut_jet = .true.
	  else
	    gpt3p = pt4-pt5
	    fi3c = zacos(p3p(2)/gpt3p)
	    fi3s = zasin(p3p(3)/gpt3p)
	    fi3 = fi3c+2.d0*fi3s
	    fi5 = zacos(p5p(2)/pt5)
	    test = dabs(fi3-fi5)
	    tolerance = 1.d-6
	    if (test.gt.tolerance) then
	       write (27,*) 'attention:',test,fi3,fi5
	       write (27,*) 'test0:',p3p(2),p3p(3),gpt3p
	    endif
	    call jet_selection(y3,y3,0.d0,fi3,fi5,gpt3p,pt5,
     #	    algorithm,r_kt,r_c,r_sep,ptjet_min,ptjet_max,yjet_min,
     #	    yjet_max,merging,acceptance,
     #	    inb_jet,xej,xpjx,xpjy,xpjz,cut_jet,merged)
          endif
	  xjetp = 1.d0
	  if (.not.(cut_jet)) then
	    xjetp = 0.d0
	  endif
c ------------------ fin jet ------------------------------------
	  if (iauren.eq.0) then
	    call choiscale(p3p,p4,cm,cmu,cmf,ichoi_scale,m,mu,mf)
	  else if (iauren.eq.1) then
	    m = gpt4*cm
	    mu = m
	    mf = m
	  endif
c c_{ij} = alpha_s*alpha^2/(4*c_i*c_j*pi*s^2)
	  cij = alfas(iloop,mu*mu)**3/(pi*s*s)
c
	  call strfrao(x1p,ih1,x2p,ih2,x4,ih4,sf0)
	  call ampo_corr3(s,x1p,x2p,y3,pt4-pt5,pt5,r35s,camp0)
	  call vec_dmult_vector(camp0,sf0,k0max,temp0)
	  call vec_dmult_vector(temp0,spcou,k0max,temp1)
	  f4dimo3p = 0.d0
	  do i = j_processo_min,j_processo_max
	    f4dimo3p = temp1(i) + temp1(16+i) + f4dimo3p
	  enddo
	  f4dimo3p = cij*f4dimo3p*(pt4-pt5)/pt4
	  f4dimo3 = f4dimo3*xisol*xjet - 
     #	  f4dimo3p*xisolp*xjetp
	endif
c
	f4dimo3 = f4dimo3 * xjacob * (pt5/x4) * hc2
     #            *pt4
c
	if (ispring.eq.1) then
	  half = 0.5d0
	  if (rnumb.le.half) then
	    xfi12 = 2.d0*pi-zacos(p3(2)/gpt3)
	    xfi13 = zacos(-(gpt3*dcos(fi35)+pt5)/pt4)
	  else
	    xfi12 = zacos(p3(2)/gpt3)
	    xfi13 = 2.d0*pi-zacos(-(gpt3*dcos(fi35)+pt5)/pt4)
	  endif
	  xpt1 = gpt4
	  xy1 = y4
	  xpt2 = gpt3
	  xy2 = y3
	  xpt3 = pt5
	  xy3 = y5
	  xfrag1 = x4
	  xfrag2 = un
	  resfunc = f4dimo3
	  f4dimo3 = dabs(f4dimo3)
	endif
c
	return
	end 
c********************************************************************
c integrale a 4 dimensions
	double precision function f4dimo4(xx)
	implicit real*8 (a-h,l-v,x-z)
	implicit real*4 (w)
	logical isol,cut_jet,merged
	common/hadron/ih1,ih2,ih3,ih4
	common/coul/n,cf,gtr
	common/parami/ysup,yinf,gptsup,gptinf,s
	common/param_jet/y_jetsup,y_jetinf,gpt_jetsup,gpt_jetinf
	common/coup/ptm,r
	common/flag/icoup
	common/coupexp/ptm_exp,r_exp
	common/flagexp/icoup_exp
	common/alem/iloopem
	common/scale/m,mf,mu
	common/faconv/hc2
	common/sortie/xpt1,xy1,xpt2,xy2,xfi12,xpt3,xy3,xfi13,resfunc
	common/sfragv/xfrag1,xfrag2
	common/baspring/ispring
	common/nboucle/iloop
	common/aurenche/iauren
	common/processo/j_processo_min,j_processo_max
	common/typesc/ichoi_scale
	common/normasc/cm,cmu,cmf
	common/randd/rnumb
	common/coup_histo/r_isol,etmax
	common/jets/jetmode
	common/jet_coup/yjet_min,yjet_max,ptjet_min,ptjet_max
	common/jet_para/r_kt,r_c,r_sep
	character*2 algorithm,merging,acceptance
	common/algorithme/algorithm,merging,acceptance
	dimension xx(20)
	parameter (k0max=32)
	dimension sf0(k0max),camp0(k0max)
	dimension temp0(k0max),temp1(k0max),spcou(k0max)
	dimension p1(4),p2(4),pp4(4)
	dimension p3(4),p4(4),p5(4)
	dimension p1p(4),p2p(4),p3p(4),p5p(4)
	dimension xej(2),xpjx(2),xpjy(2),xpjz(2)
	pi=datan(1.d0)*4.d0
	un = 1.d0
c ------------------ y3 -------------------------------------------
	y3max = y_jetsup
	y3min = y_jetinf
	y3 = y3min + (y3max-y3min)*xx(1)
c ------------------ y4 -------------------------------------------
	y4max = ysup
	y4min = yinf
	y4 = y4min + (y4max-y4min)*xx(2)
c ------------------ gpt4 ------------------------------------------
	gpt4maxp = dmin1(gptsup,dsqrt(s)/(2.d0*dcosh(y4)))
	gpt4max = dmax1(gptinf,gpt4maxp)
	gpt4min = gptinf
	gpt4 = gpt4min + (gpt4max-gpt4min)*xx(3)
c ------------------ x4 -------------------------------------------
	x4max = 1.d0
	x4min_incl = 2.d0*gpt4/dsqrt(s)*dcosh(y4)
	x4min_isol = gpt4/(gpt4+etmax)
	x4minp = dmax1(x4min_incl,x4min_isol)
	x4min = dmin1(x4max,x4minp)
	x4 = x4min + (x4max-x4min)*xx(4)
	pt4 = gpt4/x4
c ------------------ pt5 --------------------------------------------
	pt5max = dsqrt(pt4**2+gpt_jetsup**2)
	pt5min = ptm
	pt5 = pt5min + (pt5max-pt5min)*xx(5)
c ------------------ fi45 -------------------------------------------
	x11 = pt4/dsqrt(s)*dexp(y4)+dabs(pt4-pt5)/dsqrt(s)*dexp(y3)
	x21 = pt4/dsqrt(s)*dexp(-y4)+dabs(pt4-pt5)/dsqrt(s)*dexp(-y3)
	if (x11.gt.un.or.x21.gt.un) then
	  f4dimo4 = 0.d0
	  return
	endif
	sy5max = dlog(dsqrt(s)/pt5*(1.d0-x11))
	sy5min = -dlog(dsqrt(s)/pt5*(1.d0-x21))
	if (sy5max.lt.sy5min) then
	  f4dimo4 = 0.d0
	  return
	endif
	if (pt5.le.pt4/2.d0) then
	  fimax = pi
	else
	  fimax = dacos(-pt4/(2.d0*pt5))
	endif
	gpt3max = dmin1(pt4+pt5,dsqrt(s)/(2.d0*dcosh(y3))
     #	,gpt_jetsup)
	xarg = (gpt3max**2-pt4**2-pt5**2)/
     #	(2.d0*pt4*pt5)
	if (xarg.gt.1.d0) then
	  fimin = 0.d0
	else if (xarg.lt.-1.d0) then
	  f4dimo4 = 0.d0
	  return
	else
	  fimin = dacos(xarg)
	endif
	if (y4.le.sy5max.and.y4.ge.sy5min) then
	  omegamin = datan(fimin/(sy5max-y4))
	  omegamax = pi-datan(fimin/dabs(sy5min-y4))
	  srmax2 = dmax1((sy5max-y4)**2+fimax**2,
     #	  (sy5min-y4)**2+fimax**2)
	  rmax2 = dsqrt(srmax2)
	  rmin2 = fimin
	else if (y4.lt.sy5max.and.y4.lt.sy5min) then
	  omegamin = datan(fimin/(sy5max-y4))
	  omegamax = datan(fimax/(sy5min-y4))
	  srmax2 = (sy5max-y4)**2+fimax**2
	  rmax2 = dsqrt(srmax2)
	  rmin2 = dsqrt(fimin**2+(sy5min-y4)**2)
	else if (y4.gt.sy5max.and.y4.gt.sy5min) then
	  omegamin = pi-datan(fimax/dabs(sy5max-y4))
	  omegamax = pi-datan(fimin/dabs(sy5min-y4))
	  srmax2 = (sy5min-y4)**2+fimax**2
	  rmax2 = dsqrt(srmax2)
	  rmin2 = dsqrt(fimin**2+(sy5max-y4)**2)
	endif
	if (omegamin.le.omegamax) then
	  omega = omegamin + (omegamax-omegamin)*xx(6)
	else	  
	   f4dimo4 = 0.d0
	   return
	endif
	xbig = 1.d+20
	xsmall = 1.d-12
	if (omega.lt.xsmall.or.omega.gt.(pi-xsmall)) then
	  rmax1 = xbig
	else
	  rmax1 = pi/dsin(omega)
	endif
	rrmax = dmin1(rmax1,rmax2)
	rrmin = rmin2
	if (rrmin.le.rrmax) then
	  rr = rrmin + (rrmax-rrmin)*xx(7)
	else	  
	   f4dimo4 = 0.d0
	   return
	endif
	fi45 = rr*dsin(omega)
	if (fi45.gt.pi) then
	  f4dimo4 = 0.d0
	  return
	endif
c ------------------ gpt3 -------------------------------------------
	gpt3min = dmax1(pt5,gpt_jetinf)
	gpt3 = dsqrt(pt4**2+pt5**2+2.d0*pt4*pt5*dcos(fi45))
	if (gpt3.gt.gpt3max.or.gpt3.lt.gpt3min) then
	  f4dimo4 = 0.d0
	  return
	endif
c ------------------ x -------------------------------------------
	x1c = pt4/dsqrt(s)*dexp(y4) + gpt3/dsqrt(s)*dexp(y3)
	x2c = pt4/dsqrt(s)*dexp(-y4) + gpt3/dsqrt(s)*dexp(-y3)
	if (x1c.ge.un.or.x2c.ge.un) then
	  f4dimo4 = 0.d0
	  return
	endif
c ------------------ y5 -------------------------------------------
	y5max = dlog(dsqrt(s)/pt5*(1.d0-x1c))
	y5min = -dlog(dsqrt(s)/pt5*(1.d0-x2c))
	y5 = y4 + rr*dcos(omega)
	if (y5.gt.y5max.or.y5.lt.y5min) then
	  f4dimo4 = 0.d0
	  return
	endif
c ------------------ x -------------------------------------------
	x1 = x1c + pt5/dsqrt(s)*dexp(y5)
	x2 = x2c + pt5/dsqrt(s)*dexp(-y5)
	if (x1.ge.un.or.x2.ge.un) then
	  f4dimo4 = 0.d0
	  return
	endif
c coupure en angle-------------------------------------------------
        r45s = (y4-y5)**2+fi45**2
        rs = r**2
	icorr = 0
        if (r45s.lt.rs) then
          icorr = 1
        endif
c coupure en isolement-------------------------------------------------
	call isolement(y4,y5,fi45,x4,pt5,gpt4,r_isol,etmax,
     #	isol)
	xisol = 1.d0
	if (.not.(isol)) then
	  if (icorr.eq.0) then
	    f4dimo4 = 0.d0
	    return
	  else
	    xisol = 0.d0
	  endif
	endif
c ------------------ fin isolement ------------------------------------
	if (iauren.eq.0) then
	  xjacob = (y3max-y3min)*(y4max-y4min)*(gpt4max-gpt4min)*
     #	  rr*(rrmax-rrmin)*(omegamax-omegamin)*(x4max-x4min)*
     #	  (pt5max-pt5min)
	else if (iauren.eq.1) then
	  xjacob = (y3max-y3min)*
     #	  rr*(rrmax-rrmin)*(omegamax-omegamin)*(x4max-x4min)*
     #	  (pt5max-pt5min)
	endif
	x1p = (pt4+pt5)/dsqrt(s)*(dexp(y3)+dexp(y4))
	x2p = (pt4+pt5)/dsqrt(s)*(dexp(-y3)+dexp(-y4))
c ------------------------------------------- 
	p1(1) = x1*dsqrt(s)/2.d0
	p1(2) = 0.d0
	p1(3) = 0.d0
	p1(4) = x1*dsqrt(s)/2.d0
c ------------------------------------------- 
	p2(1) = x2*dsqrt(s)/2.d0
	p2(2) = 0.d0
	p2(3) = 0.d0
	p2(4) = -x2*dsqrt(s)/2.d0
c ------------------------------------------- 
	pp4(1) = pt4*dcosh(y4)
	pp4(2) = pt4
	pp4(3) = 0.d0
	pp4(4) = pt4*dsinh(y4)
c ------------------------------------------- 
	p4(1) = gpt4*dcosh(y4)
	p4(2) = gpt4
	p4(3) = 0.d0
	p4(4) = gpt4*dsinh(y4)
c ------------------------------------------- 
	p5(1) = pt5*dcosh(y5)
	p5(2) = pt5*dcos(fi45)
	p5(3) = pt5*dsin(fi45)
	p5(4) = pt5*dsinh(y5)
c ------------------------------------------- 
	p3(1) = p1(1) + p2(1) - pp4(1) - p5(1)
	p3(2) = p1(2) + p2(2) - pp4(2) - p5(2)
	p3(3) = p1(3) + p2(3) - pp4(3) - p5(3)
	p3(4) = p1(4) + p2(4) - pp4(4) - p5(4)
c -------------------------------------------
	p1p(1) = x1p*dsqrt(s)/2.d0
	p1p(2) = 0.d0
	p1p(3) = 0.d0
	p1p(4) = x1p*dsqrt(s)/2.d0
c ------------------------------------------- 
	p2p(1) = x2p*dsqrt(s)/2.d0
	p2p(2) = 0.d0
	p2p(3) = 0.d0
	p2p(4) = -x2p*dsqrt(s)/2.d0
c ------------------------------------------- 
	p5p(1) = pt5*dcosh(y4)
	p5p(2) = pt5
	p5p(3) = 0.d0
	p5p(4) = pt5*dsinh(y4)
c ------------------------------------------- 
	p3p(1) = p1p(1) + p2p(1) - pp4(1) - p5p(1)
	p3p(2) = p1p(2) + p2p(2) - pp4(2) - p5p(2)
	p3p(3) = p1p(3) + p2p(3) - pp4(3) - p5p(3)
	p3p(4) = p1p(4) + p2p(4) - pp4(4) - p5p(4)
c -------------------------------------------
c coupure jet-------------------------------------------------
	cos_fi35 = -(pt4*dcos(fi45)+pt5)/gpt3
	fi35 = zacos(cos_fi35)
	if (jetmode.eq.0) then
	  cut_jet = .true.
	else
	  fi3c = zacos(p3(2)/gpt3)
	  fi3s = zasin(p3(3)/gpt3)
	  fi3 = fi3c+2.d0*fi3s
	  fi5 = zacos(p5(2)/pt5)
	  test = fi3-fi5-fi35
	  tolerance = 1.d-6
	  if (test.gt.tolerance) then
	    write (27,*) 'attention4:',test,fi35,fi3,fi5
	  endif
	  call jet_selection(y3,y5,fi35,fi3,fi5,gpt3,pt5,
     #	  algorithm,r_kt,r_c,r_sep,ptjet_min,ptjet_max,yjet_min,
     #	  yjet_max,merging,acceptance,
     #	  inb_jet,xej,xpjx,xpjy,xpjz,cut_jet,merged)
        endif
	xjet = 1.d0
	if (.not.(cut_jet)) then
	  if (icorr.eq.0) then
	    f4dimo4 = 0.d0
	    return
	  else if (icorr.eq.1) then
	    xjet = 0.d0
	  endif
	endif
c ------------------ fin jet ------------------------------------
c set the scales-------------------------------------------------
	if (iauren.eq.0) then
	  call choiscale(p3,p4,cm,cmu,cmf,ichoi_scale,m,mu,mf)
	else if (iauren.eq.1) then
	  m = gpt4*cm
	  mu = m
	  mf = m
	endif
	call fspcouo(n,spcou)
c
c c_{ij} = alpha_s^2*alpha/(4*c_i*c_j*pi*s^2)
	cij = alfas(iloop,mu*mu)**3/(pi*s*s)
c
	call strfrao(x1,ih1,x2,ih2,x4,ih4,sf0)
	call ampo4(s,x1,x2,y4,pt4,y5,pt5,fi45,camp0)
	call vec_dmult_vector(camp0,sf0,k0max,temp0)
	call vec_dmult_vector(temp0,spcou,k0max,temp1)
	f4dimo4 = 0.d0
	do i = j_processo_min,j_processo_max
	  f4dimo4 = temp1(i) + temp1(i+16) + f4dimo4
	enddo
	f4dimo4 = cij*f4dimo4
	if (icorr.eq.1) then
c coupure en isolement-------------------------------------------------
	  call isolement(y4,y4,0.d0,x4,pt5,gpt4,r_isol,etmax,
     #	  isol)
	  xisolp = 1.d0
	  if (.not.(isol)) then
	    xisolp = 0.d0
	  endif
c ------------------ fin isolement ------------------------------------
c coupure jet-------------------------------------------------
	  if (jetmode.eq.0) then
	    cut_jet = .true.
	  else
	    gpt3p = pt4+pt5
	    fi3c = zacos(p3p(2)/gpt3p)
	    fi3s = zasin(p3p(3)/gpt3p)
	    fi3 = fi3c+2.d0*fi3s
	    fi5 = zacos(p5p(2)/pt5)
	    test = dabs(fi3-fi5)-pi
	    tolerance = 1.d-6
	    if (test.gt.tolerance) then
	      write (27,*) 'attention4:',test,pi,fi3,fi5
	    endif
	    call jet_selection(y3,y4,pi,fi3,fi5,gpt3p,pt5,
     #	    algorithm,r_kt,r_c,r_sep,ptjet_min,ptjet_max,yjet_min,
     #	    yjet_max,merging,acceptance,
     #	    inb_jet,xej,xpjx,xpjy,xpjz,cut_jet,merged)
          endif
	  xjetp = 1.d0
	  if (.not.(cut_jet)) then
	    xjetp = 0.d0
	  endif
c ------------------ fin jet ------------------------------------
	  if (iauren.eq.0) then
	    call choiscale(p3p,p4,cm,cmu,cmf,ichoi_scale,m,mu,mf)
	  else if (iauren.eq.1) then
	    m = gpt4*cm
	    mu = m
	    mf = m
	  endif
c c_{ij} = alpha_s^2*alpha/(4*c_i*c_j*pi*s^2)
	  cij = alfas(iloop,mu*mu)**3/(pi*s*s)
c
	  call strfrao(x1p,ih1,x2p,ih2,x4,ih4,sf0)
	  call ampo_corr4(s,x1p,x2p,y4,pt4,pt5,r45s,camp0)
	  call vec_dmult_vector(camp0,sf0,k0max,temp0)
	  call vec_dmult_vector(temp0,spcou,k0max,temp1)
	  f4dimo4p = 0.d0
	  do i = j_processo_min,j_processo_max
	    f4dimo4p = temp1(i) + temp1(i+16) + f4dimo4p
	  enddo
	  f4dimo4p = cij*f4dimo4p
	  f4dimo4 = f4dimo4*xisol*xjet - 
     #	  f4dimo4p*xisolp*xjetp
	endif
c
	f4dimo4 = f4dimo4 * xjacob * (pt4/x4*pt5) * hc2
	if (ispring.eq.1) then
	  half = 0.5d0
	  if (rnumb.le.half) then
	    xfi12 = 2.d0*pi-zacos(p3(2)/gpt3)
	    xfi13 = fi45
	  else
	    xfi12 = zacos(p3(2)/gpt3)
	    xfi13 = 2.d0*pi-fi45
	  endif
	  xpt1 = gpt4
	  xy1 = y4
	  xpt2 = gpt3
	  xy2 = y3
	  xpt3 = pt5
	  xy3 = y5
	  xfrag1 = x4
	  xfrag2 = un
	  resfunc = f4dimo4
	  f4dimo4 = dabs(f4dimo4)
	endif
c
	return
	end 
c*****************************************************
c integrale a 2 dimensions
	double precision function f2dimo1(xx)
	implicit real*8 (a-h,l-v,x-z)
	implicit real*4 (w)
	logical isol,cut_jet,merged
	common/hadron/ih1,ih2,ih3,ih4
	common/scale/m,mf,mu
	common/coul/n,cf,gtr
	common/parami/ysup,yinf,gptsup,gptinf,s
	common/param_jet/y_jetsup,y_jetinf,gpt_jetsup,gpt_jetinf
	common/alem/iloopem
	common/faconv/hc2
	common/sortie/xpt1,xy1,xpt2,xy2,xfi12,xpt3,xy3,xfi13,resfunc
	common/sfragv/xfrag1,xfrag2
	common/baspring/ispring
	common/nboucle/iloop
	common/aurenche/iauren
	common/processo/j_processo_min,j_processo_max
	common/typesc/ichoi_scale
	common/normasc/cm,cmu,cmf
	common/coup_histo/r_isol,etmax
	common/jets/jetmode
	common/jet_coup/yjet_min,yjet_max,ptjet_min,ptjet_max
	common/jet_para/r_kt,r_c,r_sep
	character*2 algorithm,merging,acceptance
	common/algorithme/algorithm,merging,acceptance
	dimension xx(20)
	parameter (k0max=32)
	dimension sv2(k0max),sy3(k0max),spcou(k0max)
	dimension temp0(k0max),temp1(k0max)
	dimension p3(4),p4(4),p5(4)
	dimension xej(2),xpjx(2),xpjy(2),xpjz(2)
	pi=datan(1.d0)*4.d0
	un = 1.d0
c ------------------ y3 -------------------------------------------
	y3max = y_jetsup
	y3min = y_jetinf
	y3 = y3min + (y3max-y3min)*xx(1)
c ------------------ y4 -------------------------------------------
	y4max = ysup
	y4min = yinf
	y4 = y4min + (y4max-y4min)*xx(2)
c ------------------ gpt4 ------------------------------------------
	gpt4maxp = dmin1(gptsup,dsqrt(s)/(2.d0*dcosh(y4))
     #	,dsqrt(s)/(2.d0*dcosh(y3)))
	gpt4max = dmax1(gptinf,gpt4maxp)
	gpt4min = gptinf
	gpt4 = gpt4min + (gpt4max-gpt4min)*xx(3)
c ------------------ gpt3 ------------------------------------------
c x4 = gpt4/gpt3 et x4min=2*gpt4/dsqrt(s)*dcosh(y4) <= gpt4/gpt3
c d'ou gpt3 <= dsqrt(s)/(2.d0*dcosh(y4)) ou gpt3 <= gpt4/x4min
	x4min_incl = 2.d0*gpt4/dsqrt(s)*dcosh(y4)
	x4min_isol = gpt4/(gpt4+etmax)
	x4min = dmax1(x4min_incl,x4min_isol)
	gpt3maxp = dmin1(gpt_jetsup,dsqrt(s)/(2.d0*dcosh(y3))
     #	,gpt4/x4min)
	gpt3max = dmax1(gpt4,gpt3maxp)
	gpt3min = gpt4
	gpt3 = gpt3min + (gpt3max-gpt3min)*xx(4)
c ------------------ u -------------------------------------------
	u2 = xx(5)
	x4 = gpt4/gpt3
	x10 = gpt3*(dexp(y3)+dexp(y4))/dsqrt(s)
	x20 = gpt3*(dexp(-y3)+dexp(-y4))/dsqrt(s)
	if (x10.gt.un.or.x20.gt.un) then
	  f2dimo1 = 0.d0
	  return
	endif
c coupure en isolement-------------------------------------------------
	call isolement(0.d0,0.d0,0.d0,x4,0.d0,gpt4,r_isol,etmax,
     #	isol)
	if (.not.(isol)) then
	  f2dimo1 = 0.d0
	  return
	endif
c ------------------ fin isolement ------------------------------------
c ----------coupure jet-------------------------------------------------
	if (jetmode.eq.0) then
	  cut_jet = .true.
	else
c second y3 serves only to produce r35=0
	  fi3 = pi 
	  fi5 = 0.d0
	  call jet_selection(y3,y3,0.d0,fi3,fi5,gpt3,0.d0,
     #	  algorithm,r_kt,r_c,r_sep,ptjet_min,ptjet_max,yjet_min,
     #	  yjet_max,merging,acceptance,
     #	  inb_jet,xej,xpjx,xpjy,xpjz,cut_jet,merged)
        endif
	if (.not.(cut_jet)) then
	  f2dimo1 = 0.d0
	  return
	endif
c ------------------ fin jet ------------------------------------
	if (iauren.eq.0) then
	  xjacob = (y3max-y3min)*(y4max-y4min)*(gpt4max-gpt4min)
     #	  *(gpt3max-gpt3min)
	else if (iauren.eq.1) then
	  xjacob = (y3max-y3min)*(gpt3max-gpt3min)
	endif
	call fspcouo(n,spcou)
c ------------------------------------------- 
	  p3(1) = gpt3*dcosh(y3)
	  p3(2) = gpt3
	  p3(3) = 0.d0
	  p3(4) = gpt3*dsinh(y3)
c ------------------------------------------- 
	  p5(1) = 0.d0
	  p5(2) = 0.d0
	  p5(3) = 0.d0
	  p5(4) = 0.d0
c ------------------------------------------- 
	  p4(1) = gpt4*dcosh(y4)
	  p4(2) = -gpt4
	  p4(3) = 0.d0
	  p4(4) = gpt4*dsinh(y4)
c -------------------------------------------
c set the scales-------------------------------------------------
	if (iauren.eq.0) then
	  call choiscale(p3,p4,cm,cmu,cmf,ichoi_scale,m,mu,mf)
	else if (iauren.eq.1) then
	  m = gpt4*cm
	  mu = m
	  mf = m
	endif
	alphas = alfas(iloop,mu*mu)
c c^b_{ij} = 2*pi*alpha_s*alpha/(4*c_i*c_j*s**2)
	cbij = alphas**2*2.d0*pi/(s*s)
	alpi = alphas/(2.d0*pi)
c
	call ssy3o(s,gpt4,y3,y4,m,x10,x20,x4,u2,ih1,ih2,ih4,sy3)
	call ssv2o(s,gpt4,y3,y4,x10,x20,x4,u2,ih1,ih2,ih4,sv2)
	call vec_dadd_vector(sy3,sv2,k0max,temp0)
	call vec_dmult_vector(temp0,spcou,k0max,temp1)
	f2dim = 0.d0
	do i = j_processo_min,j_processo_max
	  f2dim = temp1(i) + temp1(i+16) + f2dim
	enddo
c
	f2dimo1 = f2dim * xjacob * alpi * cbij * hc2
	if (ispring.eq.1) then
	  xfi12 = pi
	  xfi13 = 0.d0
	  xpt1 = gpt4
	  xy1 = y4
	  xpt2 = gpt3
	  xy2 = y3
	  xpt3 = 0.d0
	  xy3 = 0.d0
	  xfrag1 = x4
	  xfrag2 = un
	  resfunc = f2dimo1
	  f2dimo1 = dabs(f2dimo1)
	endif
	return
	end 
c **********************************************************************
c integrale a 2 dimensions
	double precision function f2dimo3(xx)
	implicit real*8 (a-h,l-v,x-z)
	implicit real*4 (w)
	logical isol,cut_jet,merged
	common/hadron/ih1,ih2,ih3,ih4
	common/scale/m,mf,mu
	common/coul/n,cf,gtr
	common/parami/ysup,yinf,gptsup,gptinf,s
	common/param_jet/y_jetsup,y_jetinf,gpt_jetsup,gpt_jetinf
	common/alem/iloopem
	common/faconv/hc2
	common/flagexp/icoup_exp
	common/coupexp/ptm_exp,r_exp
	common/coup/ptmm,r
	common/sortie/xpt1,xy1,xpt2,xy2,xfi12,xpt3,xy3,xfi13,resfunc
	common/sfragv/xfrag1,xfrag2
	common/baspring/ispring
	common/nboucle/iloop
	common/aurenche/iauren
	common/processo/j_processo_min,j_processo_max
	common/typesc/ichoi_scale
	common/normasc/cm,cmu,cmf
	common/coup_histo/r_isol,etmax
	common/jets/jetmode
	common/jet_coup/yjet_min,yjet_max,ptjet_min,ptjet_max
	common/jet_para/r_kt,r_c,r_sep
	character*2 algorithm,merging,acceptance
	common/algorithme/algorithm,merging,acceptance
	dimension xx(20)
	parameter (k0max=32)
	dimension sy4r(k0max),sy4p(k0max),spcou(k0max)
	dimension temp0(k0max),temp1(k0max)
	dimension p3(4),p4(4),p5(4)
	dimension xej(2),xpjx(2),xpjy(2),xpjz(2)
	pi=datan(1.d0)*4.d0
	zero = 0.d0
	un = 1.d0
c ------------------ y3 -------------------------------------------
	y3max = y_jetsup
	y3min = y_jetinf
	y3 = y3min + (y3max-y3min)*xx(1)
c ------------------ y4 -------------------------------------------
	y4max = ysup
	y4min = yinf
	y4 = y4min + (y4max-y4min)*xx(2)
	ys = (y3-y4)/2.d0
	y = (y3+y4)/2.d0
c ------------------ gpt4 ------------------------------------------
	gpt4maxp = dmin1(gptsup,dsqrt(s)/(2.d0*dcosh(y3))
     #	,dsqrt(s)/(2.d0*dcosh(y4)))
	gpt4max = dmax1(gptinf,gpt4maxp)
	gpt4min = gptinf
	gpt4 = gpt4min + (gpt4max-gpt4min)*xx(3)
c ------------------ x4 -------------------------------------------
	gpt3max = dmin1(gpt_jetsup,dsqrt(s)/(2.d0*dcosh(y3)))
	gpt3min = gpt_jetinf
c si x4 <= gpt4/gpt3min  alors gpt4/(gpt3min*x4) => 1 et z4max = 1
	if (gpt3min.eq.0.d0) then
	  x4max = 1.d0
	else
	  x4max = dmin1(1.d0,gpt4/gpt3min)
	endif
	x4min_incl = 2.d0*gpt4/dsqrt(s)*dcosh(y4)
	x4min_isol = gpt4/(gpt4+etmax)
	x4minpp = dmax1(x4min_incl,x4min_isol)
	x4minp = dmax1(x4minpp,gpt4/gpt3max)
	x4min = dmin1(x4max,x4minp)
	x4 = x4min + (x4max-x4min)*xx(4)
	pt4 = gpt4/x4
	x10 = pt4*(dexp(y3)+dexp(y4))/dsqrt(s)
	x20 = pt4*(dexp(-y3)+dexp(-y4))/dsqrt(s)
	if (x10.gt.un.or.x20.gt.un) then
	  f2dimo3 =0.d0
	  return
	endif
	pt5x = dsqrt(s)/(2.d0*dcosh(ys))*(1.d0-x10)*(1.d0-x20)/
     #  ((1.d0-x10)*dexp(-y)+(1.d0-x20)*dexp(y))
	ptm = dmin1(ptmm,pt5x)
cc	ptm = ptmm
c
	z4max0 = 1.d0
	z4min0 = dmax1(x4minp/x4,x10,x20)
	z4min1 = pt4/(pt4+ptm_exp)	
	z4sup = pt4/(pt4+ptm)
	if (icoup_exp.eq.0) then
	  z4min = z4min0	
	  z4max = z4max0	
	else if (icoup_exp.eq.1) then
	  z4min = dmax1(z4min0,z4min1)	
	  z4max = z4sup	
	endif
c juste pour tester (en principe inutile)
	if (z4min.gt.z4max) then
	  f2dimo3 = 0.d0
	  write (18,*) 'attention z4min > z4max',z4min,z4max
	  return
	endif
	z4 = z4min + (z4max-z4min)*xx(5)
	gpt3 = gpt4/(x4*z4)
c juste pour tester (en principe inutile)
	if (gpt3.gt.gpt3max.or.gpt3.lt.gpt3min) then
	  f2dimo3 = 0.d0
	  write (18,*) 'attention gpt3 > gpt3min ou gpt3 < gpt3max',
     #	              gpt3,gpt3min,gpt3max
	  return
	endif
c coupure en isolement-------------------------------------------------
	pt5 = (1.d0-z4)/(z4*x4)*gpt4
	call isolement(0.d0,0.d0,0.d0,x4,pt5,gpt4,r_isol,etmax,
     #	isol)
	if (.not.(isol)) then
	  f2dimo3 = 0.d0
	  return
	endif
c ------------------ fin isolement ------------------------------------
c coupure jet-------------------------------------------------
	if (jetmode.eq.0) then
	  cut_jet = .true.
	else
	  fi3 = pi 
	  fi5 = pi
	  call jet_selection(y3,y3,0.d0,fi3,fi5,gpt3,0.d0,
     #	  algorithm,r_kt,r_c,r_sep,ptjet_min,ptjet_max,yjet_min,
     #	  yjet_max,merging,acceptance,
     #	  inb_jet,xej,xpjx,xpjy,xpjz,cut_jet,merged)
        endif
	if (.not.(cut_jet)) then
	  f2dimo3 = 0.d0
	  return
	endif
c ------------------ fin jet ------------------------------------
	if (iauren.eq.0) then
	  xjacob = (y3max-y3min)*(y4max-y4min)*(gpt4max-gpt4min)
     #	  *(x4max-x4min)*(z4max-z4min)
	else if (iauren.eq.1) then
	  xjacob = (y3max-y3min)
     #	  *(x4max-x4min)*(z4max-z4min)
	endif
	call fspcouo(n,spcou)
c ------------------------------------------- 
	  p3(1) = gpt3*dcosh(y3)
	  p3(2) = gpt3
	  p3(3) = 0.d0
	  p3(4) = gpt3*dsinh(y3)
c ------------------------------------------- 
	  p5(1) = 0.d0
	  p5(2) = 0.d0
	  p5(3) = 0.d0
	  p5(4) = 0.d0
c ------------------------------------------- 
	  p4(1) = gpt4*dcosh(y4)
	  p4(2) = -gpt4
	  p4(3) = 0.d0
	  p4(4) = gpt4*dsinh(y4)
c -------------------------------------------
c set the scales-------------------------------------------------
	if (iauren.eq.0) then
	  call choiscale(p3,p4,cm,cmu,cmf,ichoi_scale,m,mu,mf)
	else if (iauren.eq.1) then
	  m = gpt4*cm
	  mu = m
	  mf = m
	endif
	alphas = alfas(iloop,mu*mu)
c c^b_{ij} = 2*pi*alpha_s*alpha/(4*c_i*c_j*s**2)
	cbij = alphas**2*2.d0*pi/(s*s)
	alpi = alphas/(2.d0*pi)
c
	call ssy4po(s,gpt4,y3,y4,mf,x10,x20,x4,z4,ih1,ih2,ih4,sy4p)
	if (z4.le.z4sup) then
	  call ssy4ro(s,gpt4,y3,y4,r,x10,x20,x4,z4,ih1,ih2,ih4,sy4r)
	else
	  call vec_dinit(sy4r,k0max,zero)	  
	endif
	call vec_dadd_vector(sy4p,sy4r,k0max,temp0)
	call vec_dmult_vector(temp0,spcou,k0max,temp1)
	f2dim = 0.d0
	do i = j_processo_min,j_processo_max
	  f2dim = temp1(i) + temp1(i+16) + f2dim
	enddo
c
	f2dimo3 = f2dim * xjacob * alpi * cbij * hc2
	if (ispring.eq.1) then
	  xfi12 = pi
	  xfi13 = 0.d0
	  xpt1 = gpt4
	  xy1 = y4
	  xpt2 = gpt3
	  xy2 = y3
	  xpt3 = 0.d0
	  xy3 = 0.d0
	  xfrag1 = x4
	  xfrag2 = un
	  resfunc = f2dimo3
	  f2dimo3 = dabs(f2dimo3)
	endif
	return
	end 
c*****************************************************
c integrale a 1 dimension
	double precision function f1dimo(xx)
	implicit real*8 (a-h,l-v,x-z)
	implicit real*4 (w)
	logical isol,cut_jet,merged
	common/hadron/ih1,ih2,ih3,ih4
	common/scale/m,mf,mu
	common/coul/n,cf,gtr
	common/parami/ysup,yinf,gptsup,gptinf,s
	common/param_jet/y_jetsup,y_jetinf,gpt_jetsup,gpt_jetinf
	common/alem/iloopem
	common/faconv/hc2
	common/born/iborn
	common/coup/ptmm,r
	common/sortie/xpt1,xy1,xpt2,xy2,xfi12,xpt3,xy3,xfi13,resfunc
	common/sfragv/xfrag1,xfrag2
	common/baspring/ispring
	common/nboucle/iloop
	common/aurenche/iauren
	common/processo/j_processo_min,j_processo_max
	common/typesc/ichoi_scale
	common/normasc/cm,cmu,cmf
	common/coup_histo/r_isol,etmax
	common/jets/jetmode
	common/jet_coup/yjet_min,yjet_max,ptjet_min,ptjet_max
	common/jet_para/r_kt,r_c,r_sep
	character*2 algorithm,merging,acceptance
	common/algorithme/algorithm,merging,acceptance
	dimension xx(20)
	parameter (k0max=32)
	dimension temp0(k0max),temp1(k0max),temp2(k0max),temp3(k0max)
	dimension c0sv12(k0max),c0sv3(k0max),c0sv4(k0max),c0fborn(k0max)
	dimension c0sy3r(k0max),temp4(k0max)
	dimension spcou(k0max)
	dimension p3(4),p4(4),p5(4)
	dimension xej(2),xpjx(2),xpjy(2),xpjz(2)
	pi=datan(1.d0)*4.d0
	un = 1.d0
c ------------------ y3 -------------------------------------------
	y3max = y_jetsup
	y3min = y_jetinf
	y3 = y3min + (y3max-y3min)*xx(1)
c ------------------ y4 -------------------------------------------
	y4max = ysup
	y4min = yinf
	y4 = y4min + (y4max-y4min)*xx(2)
	ys = (y3-y4)/2.d0
	y = (y3+y4)/2.d0
c ------------------ gpt4 ------------------------------------------
	gpt4maxp = dmin1(gptsup,dsqrt(s)/(2.d0*dcosh(y4))
     #	,dsqrt(s)/(2.d0*dcosh(y3)))
	gpt4max = dmax1(gptinf,gpt4maxp)
	gpt4min = gptinf
	gpt4 = gpt4min + (gpt4max-gpt4min)*xx(3)
c ------------------ gpt3 ------------------------------------------
c x4 = gpt4/gpt3 et x4min=2*gpt4/dsqrt(s)*dcosh(y4) <= gpt4/gpt3
c d'ou gpt3 <= dsqrt(s)/(2.d0*dcosh(y4)) ou gpt3 <= gpt4/x4min
	x4min_incl = 2.d0*gpt4/dsqrt(s)*dcosh(y4)
	x4min_isol = gpt4/(gpt4+etmax)
	x4minp = dmax1(x4min_incl,x4min_isol)
	gpt3maxp = dmin1(gpt_jetsup,dsqrt(s)/(2.d0*dcosh(y3))
     #	,gpt4/x4minp)
	gpt3minp = dmax1(gpt4,gpt_jetinf)
	gpt3max = dmax1(gpt3minp,gpt3maxp)
	gpt3min = gpt3minp
	gpt3 = gpt3min + (gpt3max-gpt3min)*xx(4)
c
	x10 = gpt3*(dexp(y3)+dexp(y4))/dsqrt(s)
	x20 = gpt3*(dexp(-y3)+dexp(-y4))/dsqrt(s)
	if (x10.gt.un.or.x20.gt.un) then
	  f1dimo = 0.d0
	  return
	endif
	pt5x = dsqrt(s)/(2.d0*dcosh(ys))*(1.d0-x10)*(1.d0-x20)/
     #  ((1.d0-x10)*dexp(-y)+(1.d0-x20)*dexp(y))
	ptm = dmin1(ptmm,pt5x)
cc	ptm = ptmm
c
	if (iauren.eq.0) then
	  xjacob = (y3max-y3min)*(y4max-y4min)*(gpt4max-gpt4min)
     #	  *(gpt3max-gpt3min)
	else if (iauren.eq.1) then
	  xjacob = (y3max-y3min)*(gpt3max-gpt3min)
	endif
	call fspcouo(n,spcou)
	x4 = gpt4/gpt3
c en principe vrai mais a verif
	x4max = 1.d0
	x4min = dmin1(x4max,x4minp)
	if (x4.gt.x4max.or.x4.lt.x4min) then
	  f1dimo = 0.d0
	  write (18,*) 'attention x4 ne verifie pas x4min < x4 < x4max',
     #		x4,x4min,x4max
	endif
c coupure en isolement-------------------------------------------------
	call isolement(0.d0,0.d0,0.d0,x4,0.d0,gpt4,r_isol,etmax,
     #	isol)
	if (.not.(isol)) then
	  f1dimo = 0.d0
	  return
	endif
c ------------------ fin isolement ------------------------------------
c coupure jet-------------------------------------------------
	if (jetmode.eq.0) then
	  cut_jet = .true.
	else
	  fi3 = pi 
	  fi5 = 0.d0
	  call jet_selection(y3,y3,0.d0,fi3,fi5,gpt3,0.d0,
     #	  algorithm,r_kt,r_c,r_sep,ptjet_min,ptjet_max,yjet_min,
     #	  yjet_max,merging,acceptance,
     #	  inb_jet,xej,xpjx,xpjy,xpjz,cut_jet,merged)
        endif
	if (.not.(cut_jet)) then
	  f1dimo = 0.d0
	  return
	endif
c ------------------ fin jet ------------------------------------
c ------------------------------------------- 
	  p3(1) = gpt3*dcosh(y3)
	  p3(2) = gpt3
	  p3(3) = 0.d0
	  p3(4) = gpt3*dsinh(y3)
c ------------------------------------------- 
	  p5(1) = 0.d0
	  p5(2) = 0.d0
	  p5(3) = 0.d0
	  p5(4) = 0.d0
c ------------------------------------------- 
	  p4(1) = gpt4*dcosh(y4)
	  p4(2) = -gpt4
	  p4(3) = 0.d0
	  p4(4) = gpt4*dsinh(y4)
c -------------------------------------------
c set the scales-------------------------------------------------
	if (iauren.eq.0) then
	  call choiscale(p3,p4,cm,cmu,cmf,ichoi_scale,m,mu,mf)
	else if (iauren.eq.1) then
	  m = gpt4*cm
	  mu = m
	  mf = m
	endif
c	alpha_em = alphaem(iloopem,m*m)
	alphas = alfas(iloop,mu*mu)
c c^b_{ij} = 2*pi*alpha_s*alpha/(4*c_i*c_j*s**2)
	cbij = alphas**2*2.d0*pi/(s*s)
	alpi = alphas/(2.d0*pi)
c 
	if (iborn.eq.0) then 
	  call ssv12o(s,gpt4,y3,y4,m,mu,x10,x20,ptm,x4,ih1,ih2,ih4,
     #	  c0sv12)
	  call ssv4o(s,gpt4,gpt_jetsup,etmax,y3,y4,mf,x10,x20,x4,
     #	  ih1,ih2,ih4,c0sv4)
	  call ssy3ro(s,gpt4,y3,y4,x10,x20,ptm,r,x4,ih1,ih2,ih4,c0sy3r)
	  call ssv3o(s,gpt4,y3,y4,mf,x10,x20,x4,ih1,ih2,ih4,c0sv3)
	  call vec_dmult_vector(c0sv12,spcou,k0max,temp0)
	  call vec_dmult_vector(c0sv3,spcou,k0max,temp1)
	  call vec_dmult_vector(c0sv4,spcou,k0max,temp2)
	  call vec_dmult_vector(c0sy3r,spcou,k0max,temp3)
	  sv12 = 0.d0
	  sv3 = 0.d0
	  sv4 = 0.d0
	  sy3r = 0.d0
	  do i = j_processo_min,j_processo_max
	    sv12 = temp0(i) + temp0(i+16) + sv12
	    sv3 = temp1(i) + temp1(i+16) + sv3
	    sv4 = temp2(i) + temp2(i+16) + sv4
	    sy3r = temp3(i) + temp3(i+16) + sy3r
	  enddo
	  fborn = 0.d0
	else if (iborn.eq.1) then
	  call sfborno(s,n,gpt4,y3,y4,x4,ih1,ih2,ih4,c0fborn)
	  call vec_dmult_vector(c0fborn,spcou,k0max,temp4)
	  fborn = 0.d0
	  do i = j_processo_min,j_processo_max
	    fborn = temp4(i) + temp4(i+16) +fborn
	  enddo
	  sv12 = 0.d0
	  sv3 = 0.d0
	  sv4 = 0.d0
	  sy3r = 0.d0
	endif
c 
	resb = fborn + (sv12+sv3+sv4+sy3r)*alpi
	f1dimo = resb * xjacob * cbij * hc2
c
	if (ispring.eq.1) then
	  xfi12 = pi
	  xfi13 = 0.d0
	  xpt1 = gpt4
	  xy1 = y4
	  xpt2 = gpt3
	  xy2 = y3
	  xpt3 = 0.d0
	  xy3 = 0.d0
	  xfrag1 = x4
	  xfrag2 = un
	  resfunc = f1dimo
	  f1dimo = dabs(f1dimo)
	endif
c
	return
	end 
c===================================================================
	subroutine ampo3(s,x1,x2,y3,pt3,y5,pt5,fi35,camp)
	implicit real*8(a-h,l-z)
	parameter (k0max=32)
	dimension h12(k0max),h13(k0max),h14(k0max)
	dimension h34(k0max),h23(k0max),h24(k0max),cons(k0max)
	dimension camp(k0max)
	s12 =  x1*x2*s/2.d0
	s13 =  x1*pt3*dsqrt(s)/2.d0*dexp(-y3)
	s23 =  x2*pt3*dsqrt(s)/2.d0*dexp(y3)
	s15 =  x1*pt5*dsqrt(s)/2.d0*dexp(-y5)
	s25 =  x2*pt5*dsqrt(s)/2.d0*dexp(y5)
	s35 =  pt3*pt5*coshmcos(y3-y5,fi35)
	s14 =  s12-s13-s15
	s24 =  s12-s23-s25
	s34 =  s13+s23-s35
	s45 =  s15+s25-s35
	e12 = s12/(s15*s25)
	e13 = s13/(s15*s35)
	e23 = s23/(s25*s35)
	e34p = s34/(s15+s25)/s35
c -------------------------------------------
	call sh12o(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,h12)
	call sh13o(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,h13)
	call sh23o(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,h23)
	call sh34o(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,h34)
	call sconso(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,cons)
	do i=1,k0max
	  camp(i) = (0.5d0*h12(i)*e12+h13(i)*e13
     #	             +h23(i)*e23+h34(i)*e34p+0.5d0*cons(i))
	enddo
	return
	end
c===================================================================
	subroutine ampo_corr3(s,x1,x2,y3,pt3,pt5,rs,camp)
	implicit real*8(a-h,l-z)
	parameter (k0max=32)
	dimension h12(k0max),h13(k0max),h14(k0max)
	dimension h34(k0max),h23(k0max),h24(k0max),cons(k0max)
	dimension camp(k0max)
	s12 =  x1*x2*s/2.d0
	s13 =  x1*pt3*dsqrt(s)/2.d0*dexp(-y3)
	s23 =  x2*pt3*dsqrt(s)/2.d0*dexp(y3)
	s15 =  x1*pt5*dsqrt(s)/2.d0*dexp(-y3)
	s25 =  x2*pt5*dsqrt(s)/2.d0*dexp(y3)
	s35 =  0.d0
	s14 =  s12-s13-s15
	s24 =  s12-s23-s25
	s34 =  s13+s23-s35
	s45 =  s15+s25-s35
c -------------------------------------------
	call sh13o(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,h13)
	call sh23o(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,h23)
	call sh34o(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,h34)
	do i=1,k0max
	  camp(i) = (h13(i)+h23(i)+h34(i))*2.d0/rs/(pt5*pt5)
	enddo
	return
	end
c===================================================================
	subroutine ampo4(s,x1,x2,y4,pt4,y5,pt5,fi45,camp)
	implicit real*8(a-h,l-z)
	parameter (k0max=32)
	dimension h12(k0max),h13(k0max),h14(k0max)
	dimension h34(k0max),h23(k0max),h24(k0max),cons(k0max)
	dimension camp(k0max)
	s12 =  x1*x2*s/2.d0
	s14 =  x1*pt4*dsqrt(s)/2.d0*dexp(-y4)
	s24 =  x2*pt4*dsqrt(s)/2.d0*dexp(y4)
	s15 =  x1*pt5*dsqrt(s)/2.d0*dexp(-y5)
	s25 =  x2*pt5*dsqrt(s)/2.d0*dexp(y5)
	s45 =  pt4*pt5*coshmcos(y4-y5,fi45)
	s13 =  s12-s14-s15
	s23 =  s12-s24-s25
	s34 =  s14+s24-s45
	s35 =  s15+s25-s45
	e12 = s12/(s15*s25)
	e13 = s13/(s15*s35)
	e23 = s23/(s25*s35)
	e14 = s14/(s15*s45)
	e24 = s24/(s25*s45)
	e34s = s34/(s15+s25)/s45
c -------------------------------------------
	call sh12o(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,h12)
	call sh14o(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,h14)
	call sh24o(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,h24)
	call sh34o(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,h34)
	call sconso(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,cons)
	do i=1,k0max
	  camp(i) = (0.5d0*h12(i)*e12+h14(i)*e14
     #	             +h24(i)*e24+h34(i)*e34s+0.5d0*cons(i))
	enddo
	return
	end
c===================================================================
	subroutine ampo_corr4(s,x1,x2,y4,pt4,pt5,rs,camp)
	implicit real*8(a-h,l-z)
	parameter (k0max=32)
	dimension h12(k0max),h13(k0max),h14(k0max)
	dimension h34(k0max),h23(k0max),h24(k0max),cons(k0max)
	dimension camp(k0max)
	s12 =  x1*x2*s/2.d0
	s14 =  x1*pt4*dsqrt(s)/2.d0*dexp(-y4)
	s24 =  x2*pt4*dsqrt(s)/2.d0*dexp(y4)
	s15 =  x1*pt5*dsqrt(s)/2.d0*dexp(-y4)
	s25 =  x2*pt5*dsqrt(s)/2.d0*dexp(y4)
	s45 =  0.d0
	s13 =  s12-s14-s15
	s23 =  s12-s24-s25
	s34 =  s14+s24-s45
	s35 =  s15+s25-s45
c -------------------------------------------
	call sh14o(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,h14)
	call sh24o(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,h24)
	call sh34o(s12,s13,s14,s15,s23,s24,s25,s34,s35,s45,h34)
	do i=1,k0max
	  camp(i) = (h14(i)+h24(i)+h34(i))*2.d0/rs/(pt5*pt5)
	enddo
	return
	end
c********************************************************************
	subroutine count(i)
	implicit real*8(a-h,l-z)
	common/testcount/j(11)
	j(i) = j(i) + 1
	return
	end
c********************************************************************
	subroutine trapnan(i,wx)
	implicit real*8 (a-h,l-v,x-z)
	implicit real*4 (w)
	wzero = 0.
	if (wx.lt.wzero.and.wx.gt.wzero) then
	  write (*,*) 'i=',' nan=',wx
	endif
	return
	end
c********************************************************************

