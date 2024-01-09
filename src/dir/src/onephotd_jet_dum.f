c
c dummy routine
	subroutine dir_sub
	implicit real*8 (a-h,l-v,x-z)
	implicit real*4 (w)
	write (*,*) 'Error: the choice of the contributions'
	write (*,*) 'does not match the choice of final state particle'
	return
	end
