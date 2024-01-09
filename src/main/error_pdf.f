	subroutine pdf_array(func)
	implicit real*8 (a-h,l-v,x-z)
	logical flag_pdf_error
c	character*20 parm(20)
c	double precision value(20)
	parameter(imax_errorpdf=1000)
	common/error_pdf/list_errorpdf(0:imax_errorpdf)
	common/xxsave/xx_phasespace(20)
        common/pdf_parameter/ilist_member,flag_pdf_error,val_pdf
	external func

	if (.not.flag_pdf_error) then
		list_errorpdf(0)=1.D0
		return
	endif
	
c	parm(1) = 'DEFAULT'
	
	do i=0,ilist_member
		call InitPDF(i)
		if(i.eq.0) then
			temp=func(xx_phasespace)
			list_errorpdf(0)=1.D0
		else
			list_errorpdf(i)=func(xx_phasespace)/temp
		endif
	enddo
	return
	end