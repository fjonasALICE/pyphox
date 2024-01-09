module inter
  use iso_c_binding
  !
  integer, parameter :: maxtrk=3
  integer,parameter :: imax_errorpdf=1000
  !
  !type, bind(c) :: diphox_evt
    !integer(kind=c_int) :: iprov,ntrack,nb_member
    !real(kind=c_float) :: x3!,x4
    !real(kind=c_float), dimension(maxtrk) :: e,px,py,pz
    !real(kind=c_float), dimension(imax_errorpdf) :: pdf_weight
  !end type diphox_evt
  !!
  type, bind(c) :: diphox_evt
    integer(kind=c_int) :: iprov,ntrack,nb_member
    real(kind=c_double) :: x3!,x4
    real(kind=c_double), dimension(maxtrk) :: e,px,py,pz
    real(kind=c_float), dimension(imax_errorpdf) :: pdf_weight
  end type diphox_evt
  !
  type, bind(c) :: norma_evt
    integer(kind=c_int) :: nb_evt
    real(kind=c_float) :: sqrt_s,xsec
  end type norma_evt
!
type(diphox_evt) :: truc
  bind(c,name='dip') :: truc  ! on lie la structure fortran truc avec la structure C dip
!
type(norma_evt) :: norma
  bind(c,name='norma_cxx') :: norma  ! on lie la structure fortran norma avec la structure C norma_cxx
  !
  interface
    subroutine remplie() bind (C, name="_Z7rempliev")
    end subroutine remplie
    !
    subroutine c_init_ntuple(path_rootfile) bind(c, name="_Z11Init_ntuplePc")
      import c_char
      CHARACTER(KIND= c_char), dimension(*) :: path_rootfile
    end subroutine c_init_ntuple
    !
    subroutine c_end_ntuple() bind(c, name="_Z10End_ntuplev")
    end subroutine c_end_ntuple
  end interface
  !
end module inter
