C	
	SUBROUTINE VEC_DMULT_CONSTANT(U,IDIM,A,R)
	IMPLICIT DOUBLE PRECISION (A-H,L-Z)
	DIMENSION U(IDIM),R(IDIM)
	DO I=1,IDIM
	  R(I) = U(I)*A
	ENDDO
	RETURN
	END