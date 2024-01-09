program main
  implicit none
  integer, parameter :: ki=kind(1.d0)
  real(ki) :: born,ho,sdfb,sdfho,vol
  real(ki) :: pt_min,pt_max
  integer :: i,j
  open(unit=12, file='total_onef.res', status='unknown')
  open(unit=13, file='finish_onef_case3_pt2_61m_sm.txt', status='unknown')
  write (13,*) 'integrated cross section in pb for '
  write (13,*) 'd sigma/dpt_gamma/dy_gamma/dy_jet'
  write (13,*) 'in a bin of 1 GeV for pt_gamma,'
  write (13,*) '1 unit of rapidity for y_gamma,'
  write (13,*) 'and 1 unit of rapidity for y_jet'
  write (13,*) ''
  write (13,*) 'PDF CTEQ6.1'
  write (13,*) 'scale M=Mf=mu=Pt_gamma/2*sqrt((1+exp(-2*|y_star|))/2)'
  write (13,*) ''
  write (13,*) 'pt_jet_min = 15 GeV'
  write (13,*) ''
  write (13,*) 'case3'
  write (13,*) '-1 < y_gamma < 0'
  write (13,*) '0 < y_jet < 0.8'
  write (13,*) 'one fragmentation'
  write (13,*) ''
  write (13,"(5x,'Pt min',10x,'Pt max',11x,'LO',14x,'error',10x,'NLO',13x,'error')") 
  do i=1,90,6
    read (12,*) pt_min
    read (12,*) pt_max
    read (12,*) born
    read (12,*) sdfb
    read (12,*) ho
    read (12,*) sdfho
    vol =  (pt_max-pt_min)*1.D0*0.8D0
    write (13,"(e14.6,2x,e14.6,2x,e14.6,2x,e14.6,2x,e14.6,2x,e14.6)") &
                                & pt_min, pt_max,born/vol,sdfb/vol, (born+ho)/vol,(sdfb+sdfho)/vol
  end do
  close(12)
  close(13)
end program main
