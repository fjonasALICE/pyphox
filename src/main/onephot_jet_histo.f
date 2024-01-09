c
	program main
	use ftn_c
	implicit real*8 (a-h,l-v,x-z)
c	implicit real*4 (w)
	logical lo,nlo,gener,integ,intega
	logical dir,onef
	character*256 path_bsfile,path_rzfile,path_rootfile
        integer*4 ntrack,iprov,maxtrk
        parameter (maxtrk = 3)
	common/npt/inpt
c	common/fixle/iprov,weight
c	common/sortier/ntrack,we(maxtrk),wpx(maxtrk),
c     #	wpy(maxtrk),wpz(maxtrk)
c	common/sfragvr/wx3,wx4
	common/approx/lo,nlo
	common/calcul/integ,intega,gener
C	common/nbevent/inbevent
	common/nbevent/tot,inbevent
c        parameter(iwpawc = 3000000)
c        parameter(iwpawc = 60000000)
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
c	
	integer nbhisto
	parameter (nbhisto=1)
c	dimension ibin(nbhisto),wxmin(nbhisto),wxmax(nbhisto),
c    #  wbin_size(nbhisto),wxnorma(nbhisto)
c
	call param_jetphox
c###
c	if (gener) then
c        iwpawt = 150000*7*inbevent/1000000
c       if(iwpawt.gt.iwpawc) then
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
c	  call histo_init(inbevent)
	  path_rootfile = path_rzfile(1:ilenrz)//'.root'//char(0)
	  call c_init_h1(path_rootfile(1:ilenrz+6))
	endif	
c###
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
c	  write(*,*) 'dir selected'
	  call dir_sub
	endif
	if (onef) then
c	  write(*,*) 'onef selected'
	  call onef_sub
c	  write(8,*) 'stop...pas implemente'
	endif
cccc	
        close(unit=12)
c	
c
c###
	if (gener) then
c normalisation
	  open(unit=30,file=path_rzfile(1:ilenrz)//'.res',
     #	  status='unknown')
c	  read(30,100) inumb_event,wfirst_int
	  read(30,100) inumb_event,first_int
100       format(1x,i12,e12.5)
** 	write (*,*) 'coucou1',inumb_event,wfirst_int
c
c	  call histo_close(inbevent,wfirst_int)
c	  call c_end_h1(inumb_event,wfirst_int)
	  call c_end_h1(inumb_event,first_int)
	endif  
c###
	close (8)
	end
