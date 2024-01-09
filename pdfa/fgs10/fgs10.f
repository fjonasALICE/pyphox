* V. Guzey, July 2010, JLab

* Nuclear parton distributions in the leading twist theory of nuclear shadowing
* L. Frankfurt, V. Guzey and M. Strikman, Phys. Rept. (in preparation)
* Suggested abbreviation: FGS10

* For the nucleus of C-12

* FOR ANY GIVEN VALUE OF BJORKEN X (10^{-5} < X < 0.95) AND 
* Q^2 (4 < Q^2 < 16,000) GEV^2, THE CODE GIVES THE RATIOS OF NUCLEAR TO FREE
* NUCLEON NEXT-TO-LEADING ORDER (NLO) DISTRIBUTION FUNCTIONS AND ALSO 
* THE RATIO OF THE NUCLEAR TO NUCLEON
* NLO INCLUSIVE STRUCTURE FUNCTIONS F_2.
* THE WORD "NUCLEON" MEANS (Z*proton+N*neutron)/A

* FOR THE FREE PROTON NLO DISTRIBUTON FUNCTION, CTEQ5M PARAMETRIZATION IS USED.

* NUCLEAR SHADOWING FOR THE VALENCE QUARKS IS GIVEN BY THE PARAMETRIZAION
* BY ESKOLA AND COLLABORATORS
* (K.J. Eskola, V.J. Kolhinen and P.V. Ruuskanen, Nucl. Phys. B 535 (1998) 351;
* K.J. Eskola, V.J. Kolhinen and C.A. Salgado, Eur. Phys. J. C 9 (1999) 61.)

*  **** HOW TO USE THIS CODE ***

* THE MAIN SOUBROUTINE IS CALLED ''LT(xbj,Q2,uv,dv,ubar,dbar,sbar,cbar,glue,f2)''
* WHEN THE USER SPECIFIES THE VALUES OF BJORKEN X (xbj) and Q^2 (Q2),
* THE SUBROUTINE OUTPUTS THE FOLLOWING RATIOS AT THE GIVEN X AND Q^2:
*
* U_VAL(NUCLEUS)/(Z*U_VAL+N*D_VAL) .... uv    (valence u-quark ratios)
* D_VAL(NUCLEUS)/(Z*D_VAL+N*U_VAL) .... dv    (valence d-quark ratios)
* U_BAR(NUCLEUS)/(Z*U_BAR+N*D_BAR) .... ubar  (anti u-quark ratios)
* D_BAR(NUCLEUS)/(Z*D_BAR+N*U_BAR)  .... dbar  (anti d-quark ratios)
* S_BAR(NUCLEUS)/(A*S_BAR)         .... sbar  (anti s-quark ratios)
* C_BAR(NUCLEUS)/(A*C_BAR)         .... cbar  (anti c-quark ratios)
* GLUE(NUCLEUS)/(A*GLUE)           .... glue  (gluon ratios)
* F_2(NUCLEUS)/(Z*F_2p+N*F_2n)     .... f2    (F_2 ratios)

* **** WHAT DOES THE CODE DO?

* THE CODE READS THE DATA FILE (QCDEvolution_XXproton_2009_modelXX.dat) AND
* INTERPOLATES BETWEEN THE X AND Q^2 GRID POINTS OF THE  DATA FILE 
* (subroutine polin2)  USING THE CUBIC SPLINE INTERPOLATION (subroutine SPLINE). 
* THE DATA FILE CONTAINS 90 GRID POINTS IN X AND 7 GRID POINTS IN Q^2 
* (ACTUALLY, IN LOG(Q^2)/LOG(4.)) 

      subroutine fgs10(imode,xbj,Q2,A,uv,dv,ubar,dbar,sbar,cbar,
     >glue,f2)

      implicit double precision (a-h,o-z)

      integer iread,imode
	character*40 grid

      dimension x1a(90),x2a(7),ya1(90,7),ya2(90,7),
     >ya3(90,7),ya4(90,7),ya5(90,7),ya6(90,7)
     >,ya7(90,7),ya8(90,7),res(8)

      common/fgs03/x1a,x2a,ya1,ya2,ya3,ya4,
     >ya5,ya6,ya7,ya8

      data iread/0/

      if (iread.eq.0) then

      if(imode.eq.1)then
        if(A.eq.12.D0) grid='QCDEvolution_c12proton_2009_model1.dat'
      	if(A.eq.40.D0) grid='QCDEvolution_ca40proton_2009_model1.dat'
      	if(A.eq.110.D0) grid='QCDEvolution_pd110proton_2009_model1.dat'
      	if(A.eq.208.D0) grid='QCDEvolution_pb208proton_2009_model1.dat'
      else if(imode.eq.2)then
	if(A.eq.12.D0) grid='QCDEvolution_c12proton_2009_model2.dat'
      	if(A.eq.40.D0) grid='QCDEvolution_caproton_2009_model2.dat'
      	if(A.eq.110.D0) grid='QCDEvolution_pd110proton_2009_model2.dat'
      	if(A.eq.208.D0) grid='QCDEvolution_pb208proton_2009_model2.dat'
	endif
	if((A.ne.12.D0).and.(A.ne.40.D0).and.(A.ne.110.D0)
     &.and.(A.ne.208.D0))then
		print*, 'The nucleus A=',int(A),'does not exist in FGS10'
	      stop
	endif
	
c	print*, imode,A,grid
	  open(1,file='fgs10/'//grid,status='unknown')

      do j=1,7
      read(1,*)x2a(j)
      do i=1,90
      read(1,*)x1a(i),ya1(i,j),ya2(i,j),
     >ya3(i,j),ya4(i,j),ya5(i,j)
     >,ya6(i,j),ya7(i,j),ya8(i,j)
      enddo
      enddo

      close(1)

      endif
      iread=1

      call polin2(xbj,q2,res)

      dv=res(1)
      uv=res(2)
      ubar=res(3)
      dbar=res(4)
      sbar=res(5)
      cbar=res(6)
      glue=res(7)
      f2=res(8)

      return
      end

      subroutine polin2(x1,x2,res)
      
      implicit double precision (a-h,o-z)

      integer j,k,inuc,iparton,NMAX,MMAX

      dimension x1a(90),x2a(7),
     > B(100),C(100),D(100),res(8)

      dimension yntmp1(7),ymtmp1(90),yntmp2(7),
     >ymtmp2(90),yntmp3(7),ymtmp3(90),yntmp4(7),
     >ymtmp4(90),yntmp5(7),ymtmp5(90),yntmp6(7),
     >ymtmp6(90),yntmp7(7),ymtmp7(90),yntmp8(7),
     >ymtmp8(90)

      dimension ya1(90,7),ya2(90,7),
     >ya3(90,7),ya4(90,7),ya5(90,7),ya6(90,7)
     >,ya7(90,7),ya8(90,7)

      parameter(NMAX=7,MMAX=90)

      common/fgs03/x1a,x2a,ya1,ya2,ya3,ya4,
     >ya5,ya6,ya7,ya8

      x2a(1)=1.  
      x2a(2)=2.
      x2a(3)=3.
      x2a(4)=4.
      x2a(5)=5.
      x2a(6)=6.
      x2a(7)=7.

      do j=1,90
      do k=1,7

      yntmp1(k)=ya1(j,k)
      yntmp2(k)=ya2(j,k)
      yntmp3(k)=ya3(j,k)
      yntmp4(k)=ya4(j,k)
      yntmp5(k)=ya5(j,k)
      yntmp6(k)=ya6(j,k)
      yntmp7(k)=ya7(j,k)
      yntmp8(k)=ya8(j,k)


      enddo

      x2d=Log(x2)/Log(4.0)

      call spline_FGS10(NMAX,x2a,yntmp1,B,C,D)    
      ymtmp1(j)=seval(NMAX,x2d,x2a,yntmp1,B,C,D)

      call spline_FGS10(NMAX,x2a,yntmp2,B,C,D)    
      ymtmp2(j)=seval(NMAX,x2d,x2a,yntmp2,B,C,D)

      call spline_FGS10(NMAX,x2a,yntmp3,B,C,D)    
      ymtmp3(j)=seval(NMAX,x2d,x2a,yntmp3,B,C,D)

      call spline_FGS10(NMAX,x2a,yntmp4,B,C,D)    
      ymtmp4(j)=seval(NMAX,x2d,x2a,yntmp4,B,C,D)

      call spline_FGS10(NMAX,x2a,yntmp5,B,C,D)    
      ymtmp5(j)=seval(NMAX,x2d,x2a,yntmp5,B,C,D)

      call spline_FGS10(NMAX,x2a,yntmp6,B,C,D)    
      ymtmp6(j)=seval(NMAX,x2d,x2a,yntmp6,B,C,D)

      call spline_FGS10(NMAX,x2a,yntmp7,B,C,D)    
      ymtmp7(j)=seval(NMAX,x2d,x2a,yntmp7,B,C,D)

      call spline_FGS10(NMAX,x2a,yntmp8,B,C,D)    
      ymtmp8(j)=seval(NMAX,x2d,x2a,yntmp8,B,C,D)

      enddo

      call spline_FGS10(MMAX,x1a,ymtmp1,B,C,D)
      res(1)=seval(MMAX,x1,x1a,ymtmp1,B,C,D)

      call spline_FGS10(MMAX,x1a,ymtmp2,B,C,D)
      res(2)=seval(MMAX,x1,x1a,ymtmp2,B,C,D)

      call spline_FGS10(MMAX,x1a,ymtmp3,B,C,D)
      res(3)=seval(MMAX,x1,x1a,ymtmp3,B,C,D)

      call spline_FGS10(MMAX,x1a,ymtmp4,B,C,D)
      res(4)=seval(MMAX,x1,x1a,ymtmp4,B,C,D)

      call spline_FGS10(MMAX,x1a,ymtmp5,B,C,D)
      res(5)=seval(MMAX,x1,x1a,ymtmp5,B,C,D)

      call spline_FGS10(MMAX,x1a,ymtmp6,B,C,D)
      res(6)=seval(MMAX,x1,x1a,ymtmp6,B,C,D)

      call spline_FGS10(MMAX,x1a,ymtmp7,B,C,D)
      res(7)=seval(MMAX,x1,x1a,ymtmp7,B,C,D)

      call spline_FGS10(MMAX,x1a,ymtmp8,B,C,D)
      res(8)=seval(MMAX,x1,x1a,ymtmp8,B,C,D)


       return
       end


C ---------------------------------------------------------------------
      SUBROUTINE SPLINE_FGS10(N,X,Y,B,C,D)
C ---------------------------------------------------------------------
c***************************************************************************
C CALCULATE THE COEFFICIENTS B,C,D IN A CUBIC SPLINE INTERPOLATION.
C INTERPOLATION SUBROUTINES ARE TAKEN FROM
C G.E. FORSYTHE, M.A. MALCOLM AND C.B. MOLER,
C COMPUTER METHODS FOR MATHEMATICAL COMPUTATIONS (PRENTICE-HALL, 1977).
      IMPLICIT double precision (A-H,O-Z)
      DIMENSION X(100),Y(100),B(100),C(100),D(100)
      NM1=N-1
      IF(N.LT.2) RETURN
      IF(N.LT.3) GO TO 250
      D(1)=X(2)-X(1)
      C(2)=(Y(2)-Y(1))/D(1)
      DO 210 I=2,NM1
        D(I)=X(I+1)-X(I)
        B(I)=2.0D0*(D(I-1)+D(I))
        C(I+1)=(Y(I+1)-Y(I))/D(I)
        C(I)=C(I+1)-C(I)
 210             CONTINUE
      B(1)=-D(1)
      B(N)=-D(N-1)
      C(1)=0.0D0
      C(N)=0.0D0
      IF(N.EQ.3) GO TO 215
      C(1)=C(3)/(X(4)-X(2))-C(2)/(X(3)-X(1))
      C(N)=C(N-1)/(X(N)-X(N-2))-C(N-2)/(X(N-1)-X(N-3))
      C(1)=C(1)*D(1)**2.0D0/(X(4)-X(1))
      C(N)=-C(N)*D(N-1)**2.0D0/(X(N)-X(N-3))
 215       CONTINUE
      DO 220 I=2,N
        T=D(I-1)/B(I-1)
        B(I)=B(I)-T*D(I-1)
        C(I)=C(I)-T*C(I-1)
 220             CONTINUE
      C(N)=C(N)/B(N)
      DO 230 IB=1,NM1
        I=N-IB
        C(I)=(C(I)-D(I)*C(I+1))/B(I)
 230             CONTINUE
      B(N)=(Y(N)-Y(NM1))/D(NM1)+D(NM1)*(C(NM1)+2.0D0*C(N))
      DO 240 I=1,NM1
        B(I)=(Y(I+1)-Y(I))/D(I)-D(I)*(C(I+1)+2.0D0*C(I))
        D(I)=(C(I+1)-C(I))/D(I)
        C(I)=3.0D0*C(I)
 240             CONTINUE
      C(N)=3.0D0*C(N)
      D(N)=D(N-1)
      RETURN
 250       CONTINUE
      B(1)=(Y(2)-Y(1))/(X(2)-X(1))
      C(1)=0.0D0
      D(1)=0.0D0
      B(2)=B(1)
      C(2)=0.0D0
      D(2)=0.0D0
      RETURN
      END
c
c***************************************************************************
C ---------------------------------------------------------------------
      double precision FUNCTION SEVAL(N,XX,X,Y,B,C,D)
C ---------------------------------------------------------------------
c***************************************************************************
C CALCULATE THE DISTRIBUTION AT XX BY CUBIC SPLINE INTERPOLATION.
      implicit double precision(A-H,O-Z)
      DIMENSION X(100),Y(100),B(100),C(100),D(100)
      DATA I/1/
      IF(I.GE.N) I=1
      IF(XX.LT.X(I)) GO TO 310
      IF(XX.LE.X(I+1)) GO TO 330
 310       CONTINUE
      I=1
      J=N+1
 320       CONTINUE
      K=(I+J)/2
      IF(XX.LT.X(K)) J=K
      IF(XX.GE.X(K)) I=K
      IF(J.GT.I+1) GO TO 320
 330       CONTINUE
      DX=XX-X(I)
      SEVAL=Y(I)+DX*(B(I)+DX*(C(I)+DX*D(I)))
      RETURN
      END
c
c***************************************************************************
