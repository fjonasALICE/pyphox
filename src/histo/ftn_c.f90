!
! convention de nommage de g++ pour les functions
! _Z[nb_char][name][type_arg]
! ex: void Truc() --> _Z4Trucv  (4 char pour Truc, type void)
!     void Truc(Int a, Double t) --> _Z4Trucid  (4 char pour Truc, type Int et Double)
!
module ftn_c
  !
  use, intrinsic :: iso_c_binding
  implicit none
  !
  interface
    !
    subroutine c_init_h1(path_rootfile) bind(c, name="_Z6InitH1Pc")
      import c_char
      CHARACTER(KIND= c_char), dimension(*) :: path_rootfile
    end subroutine c_init_h1
    !
    subroutine c_end_h1(numb_event,first_int) bind(c, name="_Z5EndH1id")
      import c_double,c_int
      integer(kind= c_int ), value :: numb_event
      real(kind= c_double ), value :: first_int
    end subroutine c_end_h1
    !
    subroutine c_remplie(iprov,pt3,y3,ptjet_lead,yjet_lead,&
		&fi_gamma_jet,qt_gamma_jet,edep,z_gamma_trig,&
		&z_jet_trig,x_obs_plus,x_obs_moins,&
		&x_ll_plus,x_ll_moins,weight) bind(c, name="_Z7remplieidddddddddddddd")
      import c_double,c_int
      integer(kind= c_int ), value :: iprov
      real(kind= c_double ), value :: pt3
      real(kind= c_double ), value :: y3
      real(kind= c_double ), value :: ptjet_lead
      real(kind= c_double ), value :: yjet_lead
      real(kind= c_double ), value :: fi_gamma_jet
      real(kind= c_double ), value :: qt_gamma_jet
      real(kind= c_double ), value :: edep
      real(kind= c_double ), value :: z_gamma_trig
      real(kind= c_double ), value :: z_jet_trig
      real(kind= c_double ), value :: x_obs_plus
      real(kind= c_double ), value :: x_obs_moins
      real(kind= c_double ), value :: x_ll_plus
      real(kind= c_double ), value :: x_ll_moins
      real(kind= c_double ), value :: weight
    end subroutine c_remplie
    !
  end interface
  !
end module ftn_c
!
