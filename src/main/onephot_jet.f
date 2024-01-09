c
	program main
	use inter
	implicit real*8 (a-h,l-v,x-z)
	implicit real*4 (w)
	logical lo,nlo,gener,integ,intega
	logical dir,onef
	character*256 path_bsfile,path_rzfile,path_rootfile
        integer*4 ntrack,iprov
c        parameter (maxtrk = 3)
	common/npt/inpt
c	common/fixle/iprov,weight
c	common/sortier/ntrack,we(maxtrk),wpx(maxtrk),
c     #	wpy(maxtrk),wpz(maxtrk)
c	common/sfragvr/wx3,wx4
	common/parami/ysup,yinf,gptsup,gptinf,s
	common/approx/lo,nlo
	common/calcul/integ,intega,gener
	common/nbevent/tot,inbevent
c        parameter(iwpawc = 3000000)
c        common/pawc/hmemor(iwpawc)
c	common/quest/iquest(100)
	common/pclass/dir,onef
	common/cheminbs/path_bsfile
	common/long/ilen
	common/cheminrz/path_rzfile
	common/longrz/ilenrz
c	common/calchoi/distrib,resom
c	common/diffpt/pt_pair,pt_photon
c	common/qtpara1/ibinqt
c	common/qtpara2/wbinqt(100)
c	common/ptpara1/ibinpt
c	common/ptpara2/wbinpt(100)
c	character*128 name_experiment
c	common/name/name_experiment
c	common/lname/ilname
	call param_jetphox
c###
c	if (gener) then
c        iwpawt = 150000*7*inbevent/1000000
c        if(iwpawt.gt.iwpawc) then
c          write(6,*)' you must have iwpawc at least ',iwpawt 
c          stop  
c        endif
c	call hlimit(iwpawc)
c	endif
c###
	open(unit=8,file=path_bsfile(1:ilen)//'sigmaint.res',
     #	status='unknown')
c
c******************************************************************
c###
	if (gener) then
	  path_rootfile = path_rzfile(1:ilenrz)//'.root'//char(0)
	  call c_init_ntuple(path_rootfile(1:ilenrz+6))
c	iquest(10) = 65000
c	call hcdir('//pawc',' ')
c	call hropen(50,'ggd',
c     #	path_rzfile(1:ilenrz)//'.rz',
c     #	'nq',8192,istat)
c	if (istat.ne.0) then
c	  write (8,*) 'error in hropen',istat
c	  stop
c	endif
c	call hbnt(10,'gamma-gamma events','d')
c        call hldir('//pawc','t')
c        call hldir('//gg','t')
c	call hbname(10,'fixblo',iprov,'iprov, weight')
c	call hbname(10,'varblok2',ntrack,'ntrack[1,3],'//
c     +            'we(ntrack), wpx(ntrack), wpy(ntrack), wpz(ntrack)')
c     	if (onef) then
c	  call hbname(10,'fragblo',wx3,'x3, x4')
c	endif
c	call hbset('bsize',8192,ierr)
c	if (ierr.ne.0) then
c	  write (8,*) 'error in hbset',ierr
c	  stop
c	endif
	
c	write(6,*)' before rzldir deb'	
c	call rzldir(' ',' ')
	endif	
c******************************************************************
c        
	open(unit=12,
     #	file=path_rzfile(1:ilenrz)//'.res'
     #	,status='unknown')
cccc
	if ((dir.and.onef)) then
	  write(6,*) 'stop...too many options selected'
	  write(8,*) 'stop...too many options selected'
	  stop
	endif
cccc
	if (dir) then
	  call dir_sub
	endif
	if (onef) then
	  call onef_sub
c	  write(8,*) 'stop...pas implemente'
	endif
cccc	
        close(unit=12)
c	
c
c###
	if (gener) then
c remplissage de la stucture norma
	norma = norma_evt( nb_evt=inbevent,sqrt_s=dsqrt(s),xsec=tot)
	call c_end_ntuple()
c	write(6,*)' before hcdir //gg'
c	call hcdir('//ggd',' ')
c	call hldir('//ggd','t')
c	write(6,*)' before rzldir end'
c	call rzldir(' ',' ')
c	write(6,*)' before hrout'
c	call hrout(10,icycle,' ')
c        call hldir(' ',' ')
c        call hldir(' ','a')
c	write(6,*)' before hrend'
c	call hrend('ggd')
	endif  
c###
	close (8)
	end
