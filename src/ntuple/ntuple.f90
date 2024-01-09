subroutine gfill()
  use inter
  !
  implicit none
  real(kind=8) :: weight,xx1,xx2,val_pdf
  real(kind=8) :: xe,xpx,xpy,xpz
  real(kind=8) :: list_errorpdf
  integer :: iprov,ntrack,ilist_member
  logical :: flag_pdf_error
  common/fixle/weight,iprov,ntrack
  common/sortier/xe(maxtrk),xpx(maxtrk),xpy(maxtrk),xpz(maxtrk)
  common/sfragvr/xx1,xx2
  common/error_pdf/list_errorpdf(0:imax_errorpdf)
  common/pdf_parameter/ilist_member,flag_pdf_error,val_pdf
  real(kind=8), dimension(0:imax_errorpdf) :: new_pdf_error
  !
  new_pdf_error = list_errorpdf*weight
  !truc = diphox_evt( iprov=iprov,ntrack=ntrack,nb_member=ilist_member+1,x3=real(xx1,4),&
    !&e=real(xe,4),px=real(xpx,4),py=real(xpy,4),pz=real(xpz,4),pdf_weight= real(new_pdf_error,4))
  truc = diphox_evt( iprov=iprov,ntrack=ntrack,nb_member=ilist_member+1,x3=xx1,&
    &e=xe,px=xpx,py=xpy,pz=xpz,pdf_weight= real(new_pdf_error,4))
  call remplie()

end subroutine gfill

