	subroutine isolement(y4,y5,fi45,x4,pt5,gpt4,isol)
	implicit real*8 (a-h,l-z)
	common/flag_iso/i_flag_iso
	common/coup_histo/r_isol(10),etmax(10)
	common/coup_histo_n/inb_cone,ieff_nb_cone
	logical isol
	dimension et_depf(10)
c
	r45s = (y4-y5)**2 + fi45**2
	r45 = dsqrt(r45s)
c critere standard
	if (i_flag_iso.eq.1) then
c
	  if (r45.le.r_isol(1)) then
	    et_dep = pt5 + (1.d0-x4)/x4*gpt4
	  else
	    et_dep = (1.d0-x4)/x4*gpt4
	  endif
c
	  etcut = etmax(1)
c
	  if (et_dep.le.etcut) then
	    isol = .true.
	  else
	    isol = .false.
	  endif
c critere fraction du pt du photon
	else if (i_flag_iso.eq.2) then
c
	  if (r45.le.r_isol(1)) then
	    et_dep = pt5 + (1.d0-x4)/x4*gpt4
	  else
	    et_dep = (1.d0-x4)/x4*gpt4
	  endif
c
	  etcut = etmax(1)*gpt4
c
	  if (et_dep.le.etcut) then
	    isol = .true.
	  else
	    isol = .false.
	  endif
c critere en couronne
	else if (i_flag_iso.eq.3)  then
c
          do i=1,inb_cone
	    et_depf(i) = 0.d0
	  enddo
c
          do i=1,ieff_nb_cone-1
	    if ( (r45.le.r_isol(i)).and.(r45.gt.r_isol(i+1)) ) then
	      et_depf(i) = pt5
	    else
	      et_depf(i) = 0.d0
	    endif
	  enddo
	  if (r45.le.r_isol(ieff_nb_cone)) then
	    et_depf(ieff_nb_cone) = pt5 + (1.d0-x4)/x4*gpt4
	  else
	    et_depf(ieff_nb_cone) = (1.d0-x4)/x4*gpt4
	  endif
c
	  isol = .true.
	  do i=1,ieff_nb_cone
	    isol = isol.and.(et_depf(i).le.etmax(i))
	  enddo
c critere Frixione
	else if (i_flag_iso.eq.4) then
c
          do i=1,inb_cone
	    et_depf(i) = 0.d0
	  enddo
c
          do i=1,ieff_nb_cone
	    if ( r45.le.r_isol(i) ) then
	      et_depf(i) = pt5 + (1.d0-x4)/x4*gpt4
	    else
	      et_depf(i) = (1.d0-x4)/x4*gpt4
	    endif
	  enddo
c
	  isol = .true.
	  do i=1,ieff_nb_cone
	    et_max = etmax(i)*gpt4
	    isol = isol.and.(et_depf(i).le.et_max)
	  enddo
c critere fraction du pt du photon (ATLAS)
	else if (i_flag_iso.eq.5) then
c
	  if (r45.le.r_isol(1)) then
	    et_dep = pt5 + (1.d0-x4)/x4*gpt4
	  else
	    et_dep = (1.d0-x4)/x4*gpt4
	  endif
c
	  etcut = etmax(1)*gpt4+etmax(2)
c
	  if (et_dep.le.etcut) then
	    isol = .true.
	  else
	    isol = .false.
	  endif
c
c
	endif
	end
