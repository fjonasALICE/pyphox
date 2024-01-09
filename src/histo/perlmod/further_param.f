	subroutine further_param(ptgamma_min,ymax,ymin,algorithm,
     #	r_kt,r_c,r_sep,merging,acceptance,ptjet_max,
     #	ptjet_min,yjet_max,yjet_min)
	implicit real*8 (a-h,l-z)
	character*2 algorithm,merging,acceptance
	ptgamma_min = 0.0
	ymax = 0.8
	ymin = -0.8
	algorithm = 'kt'
	merging = 'sn'
	acceptance = 'gp'
	r_kt = 0.3
	r_c = 0.7
	r_sep = 1.4
	ptjet_max = 1000.
	ptjet_min = 2.
	yjet_max = 5.8
	yjet_min = -5.8
	return
	end
