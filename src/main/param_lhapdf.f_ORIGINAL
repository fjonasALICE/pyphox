c choix des differents parametres
	subroutine param_jetphox
	implicit real*8 (a-h,l-z)
	logical lo,nlo,gener,integ,intega
	logical dir,onef
	logical flag_pdf_error
        common/alfa/masch2,masbo2,masto2,lambda4square
	common/parami/ysup,yinf,gptsup,gptinf,s
	common/param_jet/y_jetsup,y_jetinf,gpt_jetsup,gpt_jetinf
	common/gdf/jnf
	common/coul/n,cf,gtr
	common/hadron/ih1,ih2,ih3,ih4
	common/coup/ptm,r
	common/flag/icoup
	common/coupexp/ptm_exp,r_exp
	common/flagexp/icoup_exp
	common/w50512/qcdl4,qcdl5
	common/alem/iloopem
	common/faconv/hc2
	common/nboucle/iloop
	common/fscheme/ischeme
	common/aurenche/iauren
	common/box/ibox
	common/processd/j_processd_min,j_processd_max
	common/processo/j_processo_min,j_processo_max
	common/approx/lo,nlo
	common/calcul/integ,intega,gener
	common/nbevent/tot,inbevent
	common/baseparam/jtmx1,jtmx2,jcall,jcall1,jcall2,ixtry
	common/accuracy/accu
	common/typesc/ichoi_scale
	common/normasc/cm,cmu,cmf
	common/pclass/dir,onef
	parameter(imax_errorpdf=1000)
	common/error_pdf/list_errorpdf(0:imax_errorpdf)
** 	common/phocut/iphocut
** 	common/phocut1/mmin,mmax,dmin
** 	common/ficut/fimin
        character*20 parm(20)
	DOUBLE PRECISION VALUE(20)
c	DOUBLE PRECISION valuep(20)
c	common/pdf/value(20),valuep(20)
	character*256 path_bsfile,path_rzfile
	common/cheminbs/path_bsfile
	common/long/ilen
	common/cheminrz/path_rzfile
	common/longrz/ilenrz
	character*128 name_experiment
	common/name/name_experiment
	common/lname/ilname
	common/flag_iso/i_flag_iso
c	common/coup_histo/r_isol,etmax
	common/coup_histo/r_isol(10),etmax(10)
	common/coup_histo_n/inb_cone,ieff_nb_cone
	common/jets/jetmode
	common/jet_coup/yjet_min,yjet_max,ptjet_min,ptjet_max
	common/jet_para/r_kt,r_c,r_sep
	character*2 algorithm,merging,acceptance
	common/algorithme/algorithm,merging,acceptance
	common/nucleus/ish
	common/w50513/xmin,xmax,q2min,q2max
	common/pdf_parameter/ilist_member,flag_pdf_error,val_pdf
        character*128 name_pdf
	common/seed_bases/iseed
      
c initialisation vecteur pdfs
      do i=0,imax_errorpdf
	list_errorpdf(i)=0.D0
      enddo
c nom pour le chemin ou se trouverons les fichiers .bs
	read *,path_bsfile
	ilen = icharnum(path_bsfile)	
	open(unit=8,file=path_bsfile(1:ilen)//'output.param',
     #	status='unknown')
	write (8,*) 'ilen=',ilen
	write (8,*) 'path_bsfile=',path_bsfile(1:ilen)
c nom pour le chemin ou se trouverons les fichiers .rz et .res
	read *,path_rzfile
	ilenrz = icharnum(path_rzfile)	
	write (8,*) 'ilenrz=',ilenrz
	write (8,*) 'path_rzfile=',path_rzfile(1:ilenrz)
c determination dans lhapdf des fonctions de distribution dans 
c le hadron h1
	read *,name_pdf
	write(8,*) 'value(1)=',name_pdf
	read *,ichoix_member
	write(8,*) 'value(2)=',ichoix_member
	read *, flag_pdf_error 
	write(8,*) 'pdf_error=',flag_pdf_error
c ih1 et ih2 determine le choix des particules initiales, ih = 0:
c proton; ih = 1: antiproton, ih = 2: photon, ih = 3: pion
c ih2 est la particule cible, ih1 est la particule incidente
	read *,ih1
	write(8,*) 'ih1=',ih1
	read *,ih2
	write(8,*) 'ih2=',ih2
c flag for the nuclear pdf set (0: none 1: nDS 2: nDSg 3: EKS ...)
	read *, ish
	write(8,*) 'ish=', ish
c on suppose que value = valuep, c'est a dire que l'on peut calculer
c pp ou ppbar (ou meme pN) mais pas p-pion ou p-photon
c	call SetLHAPARM('SILENT')
c	call  InitPDFsetByName('cteq66.LHgrid')
	call  InitPDFsetByName(trim(name_pdf))
c	call pdfset(parm,value) ! change
       call initpdf(ichoix_member)
	call numberPDF(ilist_member)
	print*, 'test ilist_member ',ilist_member
      call GetXmin(0,xmin)
      call GetXmax(0,xmax)
      call GetQ2min(0,q2min)
      call GetQ2max(0,q2max)
c alpha electromagnetique, iloopem = 0 alpha_em = 1/137, autrement
c alpha_em est running comme dans jetset
	read *,iloopem
	write(8,*) 'iloopem=',iloopem
c ibox = 2 le terme de born de twophod.f est en fait: born + box
c ibox = 0 le terme de born de twophod.f est: born
c ibox = 1 le terme de born de twophod.f est: box
	read *,ibox
	write(8,*) 'ibox=',ibox
c saveurs actives
	read *,jnf
	write(8,*) 'jnf=',jnf
c masses des differents quarks. ces masses rentrent dans alpha_s pour
c savoir le nombre de saveurs actives.
        call getqmass(4,xmassc)
c	masch2 = (1.5d0)**2
	masch2 = (xmassc)**2
	if (jnf.eq.5) then
	  call getqmass(5,xmassb)
c	  masbo2 = (4.5d0)**2
	  masbo2 = (xmassb)**2
	else if (jnf.eq.4) then
	  masbo2 = (1.d+10)**2
	endif
	masto2 = (1.d+10)**2
C	call getlam4(ilist_member,qcdl4)
C	lambda4square = qcdl4**2
	lambda4square = 0.d0
c facteur (hbarr*c)**2
	read *,hc2
	write(8,*) 'hc2=',hc2
c choice between alpha_s from LHAPDF (iloop=0)
c or from the numerical solution of the RGE (iloop=1,2)
	read *,iloop
	write(8,*) 'iloop=',iloop
	if ( (iloop .ne. 0) .and. (qcdl4 .lt. 0.001d0) ) then
	write (*,*) 'Warning the pdf set selected does not supply '
	write (*,*) 'the value of Lambda_QCD, the program stops'
	write (*,*) 'Please, use the alphas routine provided by LHAPDF'
	  stop
	endif
c ischeme: schema de factorisation pour les pattes initiales
c 0: msbarr; 1:dis
	read *,ischeme
	write(8,*) 'ischeme=',ischeme
c pour retrouver les calculs de aurenche et al. (scale=pt du photon)
	read *,iauren
	write(8,*) 'iauren=',iauren
c ici : pour choisir entre collider et cible fixe
c ici = 0 collider, ici = 1 ciblefixe
	read *,ici
	write (8,*) 'ici=',ici
c energie disponible dans le cdm proton-(anti)proton pour collider
c ou ebeam pour experience cible fixe
	read *,ebeam
c rapidite max. du photon
	read *,ysup
	write(8,*) 'ysup=',ysup
c rapidite min. du photon
	read *,yinf
	write(8,*) 'yinf=',yinf
c rapidite max. du jet
	read *,y_jetsup
	write(8,*) 'y_jetsup=',y_jetsup
c rapidite min. du jet
	read *,y_jetinf
	write(8,*) 'y_jetinf=',y_jetinf
c impulsion transverse max. du photon
	read *,gptsup
	write(8,*) 'gptsup=',gptsup
c impulsion transverse min. du photon
	read *,gptinf
	write(8,*) 'gptinf=',gptinf
c impulsion transverse max. du jet
	read *,gpt_jetsup
	write(8,*) 'gpt_jetsup=',gpt_jetsup
c impulsion transverse min. du jet
	read *,gpt_jetinf
	write(8,*) 'gpt_jetinf=',gpt_jetinf
c constantes de su(3)_couleur
	n = 3.d0
	cf = (n*n-1.d0)/(2.d0*n)
	gtr = dfloat(jnf)/2.d0
c calcul de racine de s
	if (ici.eq.0) then
	  rs = ebeam
	  s = rs**2
	  write(8,*) 'rs =',rs
	else if (ici.eq.1) then
	  xmp = .93828d0
	  xmpi = .139567d0
	  if (ih1.le.1) then
	    xm_inci = xmp
	  else if (ih1.eq.3) then
	    xm_inci = xmpi
	  else
	    write (*,*) 'bad choice of incident particle'
	  endif
	  if (ih2.le.1) then
	    xm_cible = xmp
	  else
	    write (*,*) 'bad choice of target particle'
	  endif
	  rs = dsqrt(xm_inci**2+xm_cible**2+2.d0*xm_cible*ebeam)
	  s = rs**2
	  write(8,*) 'ebeam =',ebeam
	  write(8,*) 'rs =',rs
	endif
c ih3 et ih4 determine le choix des fonctions de fragmentation de
c parton en photon: 21 owens,221 bourhis set i 222 bourhis set ii
c ou des partons en pi0: 1 bkk ll, 2 bkk ntl
	read *,ih3
	write(8,*) 'ih3=',ih3
	ih4 = ih3
	write(8,*) 'ih4=',ih4
c coupures experimentale d'isolation: 0 pas de coupures; 1 coupures a
c la cdf (ca ne sert a rien dans cette version du programme)
	ptm_exp = 2.d0
	r_exp = .7d0
	icoup_exp = 0
c coupures theoriques en pt et r
	icoup = 0
	read *,ptm
	write(8,*) 'ptm=',ptm
	read *,r
	write(8,*) 'r=',r
c selection des sousprocessus
c partie directe
c
c	qi + qk --> jet + ph
c
c	j0 = 1 : d + u --> jet + ph
c	j0 = 2 : d + dp --> jet + ph
c	j0 = 3 : u + up --> jet + ph
c
c	qi + qbk --> jet + ph
c
c	j0 = 4 : d + ub --> jet + ph
c	j0 = 5 : d + dpb --> jet + ph
c	j0 = 6 : u + upb --> jet + ph
c
c	j0 = 7 : qi + qi --> jet + ph
c
c	qi + qbi --> jet + ph
c	j0 = 8 : d + db --> jet + ph
c	j0 = 9 : u + ub --> jet + ph
c
c	j0 = 10 : qi + g --> jet + ph
c
c	j0 = 11 : g + g --> jet + ph
	read *,j_processd_min
	write(8,*) 'j_processd_min=',j_processd_min
	read *,j_processd_max
	write(8,*) 'j_processd_max=',j_processd_max
c partie un brem
c	j0 = 1 : qi + qk --> jet + qk
c	j0 = 2 : qi + qk --> jet + g
c	j0 = 3 : qi + qbk --> jet + qbk
c	j0 = 4 : qi + qbk --> jet + g
c	j0 = 5 : qi + qi --> jet + qi
c	j0 = 6 : qi + qi --> jet + g
c	j0 = 7 : qi + qbi --> jet + qbk
c	j0 = 8 : qi + qbi --> jet + qbi
c	j0 = 9 : qi + qbi --> jet + g
c	j0 = 10 : qi + g --> jet + qk
c	j0 = 11 : qi + g --> jet + qbk
c	j0 = 12 : qi + g --> jet + qbi
c	j0 = 13 : qi + g --> jet + g
c	j0 = 14 : qi + g --> jet + qi
c	j0 = 15 : g + g --> jet + qi
c	j0 = 16 : g + g --> jet + g
	read *,j_processo_min
	write(8,*) 'j_processo_min=',j_processo_min
	read *,j_processo_max
	write(8,*) 'j_processo_max=',j_processo_max
c
	read *,nlo
	write(8,*) 'nlo=',nlo
c
	read *,lo
	write(8,*) 'lo=',lo
c si integ est true il n'y a que l'integration (section efficace
c integree)
	read *,integ
	write(8,*) 'integ=',integ
c si intega est true il n'y a que l'integration (valeur absolue de la
c section efficace)
	read *,intega
	write(8,*) 'intega=',intega
c si gener est true alors on a la generation et l'integration	
	read *,gener
	write(8,*) 'gener=',gener
c inbevent est le nb d'evenements generes
 	read *,inbevent
	write(8,*) 'inbevent=',inbevent
	read *,iseed
	write(8,*) 'iseed=',iseed
c jtmx1 et jtmx2 sont le nb d'iteration dans bases pour la grille et
c l'integrale
  	read *,jtmx1
	write(8,*) 'jtmx1=',jtmx1
	read *,jtmx2
	write(8,*) 'jtmx2=',jtmx2
c jcall,jcall1 et jcall2 sont le nb d'appelles par iteration pour
c respectivement les quasi deux donne deux, les deux donne trois et les
c vrai deux donne deux
  	read *,jcall1
	write(8,*) 'jcall1=',jcall1
  	read *,jcall
	write(8,*) 'jcall=',jcall
  	read *,jcall2
	write(8,*) 'jcall2=',jcall2
c ixtry est le nb d'essai pour la rejection d'evenement dans spring
  	read *,ixtry
	write(8,*) 'ixtry=',ixtry
c accu est la precion en pour cent de bases
	read *,accu
	write(8,*) 'accu=',accu
c choix de l'echelle: ichoi_scale selectionne les familles d'echelle: 1
c (pt3+pt4)*n, 2 sqrt(pt3^2+pt4^2), 3 mgg (masse invariante gamma-gamma)
  	read *,ichoi_scale
	write(8,*) 'ichoi_scale=',ichoi_scale
c cm,cmu et cmf sont des facteurs de normalisation des differents choix
c pour respectivement les echelles de factorisation initial, de
c renormalisation et de factorisation final
	read *,cm
	write(8,*) 'cm=',cm
	read *,cmu
	write(8,*) 'cmu=',cmu
	read *,cmf
	write(8,*) 'cmf=',cmf
c dir, onef  servent a selectionner les parties directes et one
c frag
	read *,dir
	write (8,*) 'dir=',dir
	read *,onef
	write (8,*) 'onef=',onef
** c coupure entre les 2 photons
** c iphocut=0 angle azymuthal > fimin, iphocut = 1 masse invariante
** c photon-photon > mmin et < mmax, iphocut = 2 distance en pseudo-rapidite angle
** c azymuthal > dmin
** 	read *,iphocut
** 	write(8,*) 'iphocut=',iphocut
** 	read *,fimin
** 	write(8,*) 'fimin=',fimin
** 	read *,mmin
** 	write(8,*) 'mmin=',mmin
** 	read *,mmax
** 	write(8,*) 'mmax=',mmax
** 	read *,dmin
** 	write(8,*) 'dmin=',dmin
c coupure d'isolement du photon: ne marche qu'en mode histo
	read *,i_flag_iso
	write(8,*) 'i_flag_iso=',i_flag_iso
c	read *,r_isol
c	write(8,*) 'r_isol=',r_isol
c	read *,etmax
c	write(8,*) 'etmax=',etmax
	read *,inb_cone
	write(8,*) 'nb_cone=',inb_cone
	read *,ieff_nb_cone
	write(8,*) 'eff_nb_cone=',ieff_nb_cone
	do i=1,inb_cone
	  read *,r_isol(i)
	  write(8,*) 'r_isol=',r_isol
	enddo
	do i=1,inb_cone
	  read *,etmax(i)
	  write(8,*) 'etmax=',etmax
	enddo
c caracteristique du(des) jet(s)
	read *,jetmode
	write(8,*) 'jet mode=',jetmode
	read *,algorithm
	write(8,*) 'algorithm=',algorithm
	read *,r_kt
	write(8,*) 'r_kt=',r_kt
	read *,r_c
	write(8,*) 'r_c=',r_c
	read *,r_sep
	write(8,*) 'r_sep=',r_sep
	read *,merging
	write(8,*) 'merging=',merging
	read *,acceptance
	write(8,*) 'acceptance=',acceptance
	read *,ptjet_max
	write(8,*) 'ptjet_max=',ptjet_max
	read *,ptjet_min
	write(8,*) 'ptjet_min=',ptjet_min
	read *,yjet_max
	write(8,*) 'yjet_max=',yjet_max
	read *,yjet_min
	write(8,*) 'yjet_min=',yjet_min
c
c	
	close (8)
	return
	end
	
