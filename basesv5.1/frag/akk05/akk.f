
c----------------------------------------------------------------------

c     AKK ROUTINES

c----------------------------------------------------------------------

c     INSTRUCTIONS ON USING AKK ROUTINES

c     THE USER WHO IS USED TO USING THE SUBROUTINE KKP FROM THE PREVIOUS
c     KKP FF DETERMINATION MAY PREFER TO CALL THE ROUTINES IN THIS FILE
c     FROM THE SUBROUTINE AKK IN THE FILE FFAKK.F AT
c     http://www.desy.de/~simon/AKK/ffakk.f

c     Note: When compiling, the user must instruct the compiler that the
c     line length of this FORTRAN code is not limited. For example, with
c     g77, the flag "-ffixed-line-length-150" will suffice.

c     The subroutines akk1_ff(xi,mu2,FF), for ff=pion, kaon and proton
c     (summed over charges) return the AKK FF's (obtained from a NLO
c     MSbar factorization scheme fit) at the momentum fraction xi and
c     factorisation scale squared mu2 (GeV^2) in the array FF(0:5),
c     whose components have the following meanings:

c     FF(0) = gluon
c     FF(1) = down q    = down qbar
c     FF(2) = up q      = up qbar
c     FF(3) = strange q = strange qbar
c     FF(4) = charm q   = charm qbar
c     FF(5) = bottom q  = bottom qbar

c     Note: For a given flavour, the quark FF is equal to the antiquark
c     FF, since the FF for emission of a charged summed hadron from a
c     quark is equal to that from its antiquark, by charge conjugation
c     invariance.

c     Note: The charm and bottom FF's will be zero when mu is less than
c     their initial scales 2.9788 and 9.46037 GeV respectively. There is
c     no top quark FF.

c     The subroutine akk1_lambdaQCD(lambdaQCD) returns the QCD
c     LambdaQCD's for different numbers of flavours nf=3,4,5, calculated
c     from the AKK alpha_s(M_Z) (obtained from a NLO MSbar
c     renormalization scheme fit), in the variable lambdaQCD(nf).

c     The subroutine akk_unit finds a valid, unused unit number with
c     which the akk routines will then read in from the akk data files.

c     All remaining routines that appear in this file below the
c     subroutines akk1_ff, akk1_lambdaQCD and akk_unit described above must
c     also be included, but the user never needs to call them
c     explicitly.

c---------------------------------------------------------------------

c     Returns all 6 pion FF's from AKK fit evaluated at (xi,mu2)

      subroutine akk1_pion(xi,mu2,FFpion)

      implicit none

      real*8 xi,mu2,FFpion(0:5)
      integer i,j

      integer nx,nq2
      parameter(nx=200,nq2=150)

      real*8 FFpion_a(nq2,nx,0:5)
      common/FF_interp_pion/FFpion_a
      real*8 bpion_a(nq2,nx,0:5),cpion_a(nq2,nx,0:5),
     .     dpion_a(nq2,nx,0:5)
      common/splines_pion/bpion_a,cpion_a,dpion_a

      integer precalc_done
      save precalc_done

      integer grid_read
      common/grid_read_com/grid_read

      if(precalc_done.ne.2375)then         
         call akk_unit
         open(unit=grid_read,file='akk05/grid_pion',status='old')
         do i=1,nq2
            do j=1,nx
               read(grid_read,'(6e14.6)')
     .              FFpion_a(i,j,0),
     .              FFpion_a(i,j,1),
     .              FFpion_a(i,j,2),FFpion_a(i,j,3),
     .              FFpion_a(i,j,4),FFpion_a(i,j,5)
            enddo
         enddo
         close(grid_read)
         call spline_fit(FFpion_a,bpion_a,cpion_a,
     .        dpion_a)
         precalc_done=2375
      endif
      call getFFs(xi,mu2,FFpion,FFpion_a,bpion_a,
     .     cpion_a,dpion_a)
      
      end 

c---------------------------------------------------------------------

c     Returns all 6 kaon FF's from AKK fit evaluated at (xi,mu2)

      subroutine akk1_kaon(xi,mu2,FFkaon)

      implicit none

      real*8 xi,mu2,FFkaon(0:5)
      integer i,j

      integer nx,nq2
      parameter(nx=200,nq2=150)

      real*8 FFkaon_a(nq2,nx,0:5)
      common/FF_interp_kaon/FFkaon_a
      real*8 bkaon_a(nq2,nx,0:5),ckaon_a(nq2,nx,0:5),
     .     dkaon_a(nq2,nx,0:5)
      common/splines_kaon/bkaon_a,ckaon_a,dkaon_a

      integer precalc_done
      save precalc_done

      integer grid_read
      common/grid_read_com/grid_read

      if(precalc_done.ne.2375)then
         call akk_unit
         open(unit=grid_read,file='akk05/grid_kaon',status='old')
         do i=1,nq2
            do j=1,nx
               read(grid_read,'(6e14.6)')
     .              FFkaon_a(i,j,0),
     .              FFkaon_a(i,j,1),
     .              FFkaon_a(i,j,2),FFkaon_a(i,j,3),
     .              FFkaon_a(i,j,4),FFkaon_a(i,j,5)
            enddo
         enddo
         close(grid_read)
         call spline_fit(FFkaon_a,bkaon_a,ckaon_a,
     .        dkaon_a)
         precalc_done=2375
      endif
      call getFFs(xi,mu2,FFkaon,FFkaon_a,bkaon_a,
     .     ckaon_a,dkaon_a)
      
      end 

c---------------------------------------------------------------------

c     Returns all 6 proton FF's from AKK fit evaluated at (xi,mu2)

      subroutine akk1_proton(xi,mu2,FFproton)

      implicit none

      real*8 xi,mu2,FFproton(0:5)
      integer i,j

      integer nx,nq2
      parameter(nx=200,nq2=150)

      real*8 FFproton_a(nq2,nx,0:5)
      common/FF_interp_proton/FFproton_a
      real*8 bproton_a(nq2,nx,0:5),
     .     cproton_a(nq2,nx,0:5),
     .     dproton_a(nq2,nx,0:5)
      common/splines_proton/bproton_a,
     .     cproton_a,dproton_a

      integer precalc_done
      save precalc_done

      integer grid_read
      common/grid_read_com/grid_read

      if(precalc_done.ne.2375)then
         call akk_unit
         open(unit=grid_read,file='akk05/grid_proton',status='old')
         do i=1,nq2
            do j=1,nx
               read(grid_read,'(6e14.6)')
     .              FFproton_a(i,j,0),
     .              FFproton_a(i,j,1),
     .              FFproton_a(i,j,2),
     .              FFproton_a(i,j,3),
     .              FFproton_a(i,j,4),
     .              FFproton_a(i,j,5)
            enddo
         enddo
         close(grid_read)
         call spline_fit(FFproton_a,bproton_a,
     .        cproton_a,dproton_a)
         precalc_done=2375
      endif
      call getFFs(xi,mu2,FFproton,FFproton_a,
     .     bproton_a,cproton_a,dproton_a)
      
      end

c------------------------------------------------------------------------

c     Returns all 6 K0S FF's from AKK fit evaluated at (xi,mu2)

      subroutine akk1_K0S(xi,mu2,FFK0S)

      implicit none

      real*8 xi,mu2,FFK0S(0:5)
      integer i,j

      integer nx,nq2
      parameter(nx=200,nq2=150)

      real*8 FFK0S_a(nq2,nx,0:5)
      common/FF_interp_K0S/FFK0S_a
      real*8 bK0S_a(nq2,nx,0:5),cK0S_a(nq2,nx,0:5),
     .     dK0S_a(nq2,nx,0:5)
      common/splines_K0S/bK0S_a,cK0S_a,dK0S_a

      integer precalc_done
      save precalc_done

      integer grid_read
      common/grid_read_com/grid_read

      if(precalc_done.ne.2375)then
         call akk_unit
         open(unit=grid_read,file='akk05/grid_K0S',status='old')
         do i=1,nq2
            do j=1,nx
               read(grid_read,'(6e14.6)')
     .              FFK0S_a(i,j,0),
     .              FFK0S_a(i,j,1),
     .              FFK0S_a(i,j,2),FFK0S_a(i,j,3),
     .              FFK0S_a(i,j,4),FFK0S_a(i,j,5)
            enddo
         enddo
         close(grid_read)
         call spline_fit(FFK0S_a,bK0S_a,
     .        cK0S_a,dK0S_a)
         precalc_done=2375
      endif
      call getFFs(xi,mu2,FFK0S,FFK0S_a,bK0S_a,
     .     cK0S_a,dK0S_a)
      
      end 

c------------------------------------------------------------------------

c     Returns all 6 Lambda FF's from AKK fit evaluated at (xi,mu2)

      subroutine akk1_Lambda(xi,mu2,FFLambda)

      implicit none

      real*8 xi,mu2,FFLambda(0:5)
      integer i,j

      integer nx,nq2
      parameter(nx=200,nq2=150)

      real*8 FFLambda_a(nq2,nx,0:5)
      common/FF_interp_Lambda/FFLambda_a
      real*8 bLambda_a(nq2,nx,0:5),
     .     cLambda_a(nq2,nx,0:5),
     .     dLambda_a(nq2,nx,0:5)
      common/splines_Lambda/bLambda_a,
     .     cLambda_a,dLambda_a

      integer precalc_done
      save precalc_done

      integer grid_read
      common/grid_read_com/grid_read

      if(precalc_done.ne.2375)then
         call akk_unit
         open(unit=grid_read,file='akk05/grid_Lambda',status='old')
         do i=1,nq2
            do j=1,nx
               read(grid_read,'(6e14.6)')
     .              FFLambda_a(i,j,0),
     .              FFLambda_a(i,j,1),
     .              FFLambda_a(i,j,2),
     .              FFLambda_a(i,j,3),
     .              FFLambda_a(i,j,4),
     .              FFLambda_a(i,j,5)
            enddo
         enddo
         close(grid_read)
         call spline_fit(FFLambda_a,bLambda_a,
     .        cLambda_a,dLambda_a)
         precalc_done=2375
      endif
      call getFFs(xi,mu2,FFLambda,FFLambda_a,
     .     bLambda_a,cLambda_a,dLambda_a)
      
      end 

c------------------------------------------------------------------------

      subroutine akk1_lambdaQCD(lambdaQCD)

      implicit none

      real*8 lambdaQCD(3:5)

      integer grid_read
      common/grid_read_com/grid_read

      call akk_unit
      open(unit=grid_read,file='akk05/lambdaQCD',status='old')
      read(grid_read,'(3e14.6)')lambdaQCD(3),
     .     lambdaQCD(4),lambdaQCD(5)
      close(grid_read)

      end 

c-------------------------------------------------------------------------

      subroutine akk_unit

      implicit none

      integer unit_

      integer grid_read
      common/grid_read_com/grid_read

      logical opened_

      do unit_=10,300
         inquire (unit=unit_,opened=opened_)
         if(.not.opened_)then
            grid_read=unit_
            return
         endif
      enddo
      write(6,*)'There is no available I/O unit.'
      stop 

      end

c------------------------------------------------------------------------

      subroutine akk_x_q2_grid

      implicit none

      integer i

      integer nx,nq2
      parameter(nx=200,nq2=150)

      real*8 x_a(nx),q2_a(nq2)
      common/x_q2_grid/x_a,q2_a

      integer grid_read
      common/grid_read_com/grid_read

      open(unit=grid_read,file='akk05/q2_grid',status='old')
      do i=1,nq2-9,10
         read(grid_read,'(10e14.6)'),q2_a(i),
     .        q2_a(i+1),q2_a(i+2),q2_a(i+3),
     .        q2_a(i+4),q2_a(i+5),q2_a(i+6),
     .        q2_a(i+7),q2_a(i+8),q2_a(i+9)
      enddo
      close(grid_read)

      open(unit=grid_read,file='akk05/x_grid',status='old')
      do i=1,nx-9,10
         read(grid_read,'(10e14.6)'),x_a(i),
     .        x_a(i+1),x_a(i+2),x_a(i+3),
     .        x_a(i+4),x_a(i+5),x_a(i+6),
     .        x_a(i+7),x_a(i+8),x_a(i+9)
      enddo
      close(grid_read)

      end 

c-------------------------------------------------------------------------

c Obtains FF's at mu2 by interpolation and at xi from a,b,c,d (obtained from spline fitting)

      subroutine getFFs(xi,mu2,FF,FF_a,b_a,c_a,d_a)

      implicit none

      integer xpos,q2pos,i,iabs

      integer nx,nq2
      parameter(nx=200,nq2=150)

      real*8 x_a(nx),q2_a(nq2)
      common/x_q2_grid/x_a,q2_a

      real*8 xi,FF(0:5),tq,dx,FF1,FF2,mu2,
     .     FF_a(nq2,nx,0:5),b_a(nq2,nx,0:5),
     .     c_a(nq2,nx,0:5),d_a(nq2,nx,0:5)

      if(xi.lt.x_a(1))then
         write(6,*)'akk: x=',xi
         write(6,*)'akk: x out of range.'
         write(6,*)'akk: x must be > ',
     .        x_a(1)
c         stop
      endif

      call neighbours(xi,x_a,nx,xpos)

      if(mu2.lt.q2_a(1))then
         write(6,*)'akk: q2=',mu2
         write(6,*)'akk: q2 out of range.'
         write(6,*)'akk: q2 must be > ',
     .        q2_a(1)
         stop
      endif

      if(mu2.gt.q2_a(nq2))then
         write(6,*)'akk: q2=',mu2
         write(6,*)'akk: q2 out of range.'
         write(6,*)'akk: q2 must be < ',
     .        q2_a(nq2)
         stop
      endif

      call neighbours(mu2,q2_a,nq2,q2pos)

      do i=0,5
         iabs=i
         if(iabs.lt.0)iabs=-iabs
         dx=xi-x_a(xpos)
         FF1=FF_a(q2pos,xpos,iabs)
     .        +dx*(b_a(q2pos,xpos,iabs)
     .        +dx*(c_a(q2pos,xpos,iabs)
     .        +dx*d_a(q2pos,xpos,iabs)))
         FF2=FF_a(q2pos+1,xpos,iabs)
     .        +dx*(b_a(q2pos+1,xpos,iabs)
     .        +dx*(c_a(q2pos+1,xpos,iabs)
     .        +dx*d_a(q2pos+1,xpos,iabs)))
         tq=(dlog(mu2)-dlog(q2_a(q2pos)))
     .        /(dlog(q2_a(q2pos+1))
     .        -dlog(q2_a(q2pos)))
         FF(i)=(1.d0-tq)*FF1+tq*FF2
      enddo

      end 

c---------------------------------------------------------------------

c     This subroutine finds j where j obeys val_a(j) < val <=
c     val_a(j+1). val_a is an array of size (1:max)     

      subroutine neighbours(val,val_a,max,j)

      implicit none

      integer max,j
      real*8 val,val_a(max)

      if(val.le.val_a(1))then
         write(6,*)'val too small, val,val_a(1)=',
     .        val,val_a(1)
         stop
      elseif(val.gt.val_a(1).and.
     .        val.le.val_a(max))then
         do j=1,max-1
            if(val.gt.val_a(j).and.
     .           val.le.val_a(j+1))goto 1
         enddo
 1       continue
      elseif(val.gt.val_a(max))then
         write(6,*)'val too big, val,val_a(max)=',
     .        val,val_a(max)
c         stop
      endif

      return

      end 

c-------------------------------------------------------------------------

      subroutine spline_fit(FF_a,b_a,c_a,d_a)

      implicit none

      integer i,j,k

      integer nx,nq2
      parameter(nx=200,nq2=150)

      real*8 y(nx),FF_a(nq2,nx,0:5),b(nx),c(nx),
     .     d(nx),b_a(nq2,nx,0:5),c_a(nq2,nx,0:5),
     .     d_a(nq2,nx,0:5)

      real*8 x_a(nx),q2_a(nq2)
      common/x_q2_grid/x_a,q2_a

      integer precalc_done
      save precalc_done

      if(precalc_done.ne.2375)then
         call akk_x_q2_grid
         precalc_done=2375
      endif

      do i=0,5
         do j=1,nq2
            do k=1,nx
               y(k)=FF_a(j,k,i)
            enddo
            call spline_akk(nx,x_a,y,b,c,d)
            do k=1,nx
               b_a(j,k,i)=b(k)
               c_a(j,k,i)=c(k)
               d_a(j,k,i)=d(k)
            enddo
         enddo
      enddo

      end 

c-------------------------------------------------------------------------

      SUBROUTINE SPLINE_AKK(N,X,Y,B,C,D)
*     ---------------------------------------------------------------------
*     CALCULATE THE COEFFICIENTS B,C,D IN A CUBIC SPLINE INTERPOLATION.
*     INTERPOLATION SUBROUTINES ARE TAKEN FROM
*     G.E. FORSYTHE, M.A. MALCOLM AND C.B. MOLER,
*     COMPUTER METHODS FOR MATHEMATICAL COMPUTATIONS (PRENTICE-HALL, 1977).
*     
*     SUBROUTINE TAKEN FROM AAC GROUP (KUMANO et al.)
*     
      integer N,K,IB,NM1
      REAL*8 X(N),Y(N),B(N),C(N),D(N),T
*     
      NM1=N-1
      IF(N.LT.2) then
         write(6,*)'N too small, =',N
         stop
      else
         D(1)=X(2)-X(1)
         C(2)=(Y(2)-Y(1))/D(1)
         DO  K=2,NM1
            D(K)=X(K+1)-X(K)
            B(K)=2.0D0*(D(K-1)+D(K))
            C(K+1)=(Y(K+1)-Y(K))/D(K)
            C(K)=C(K+1)-C(K)
         ENDDO
         B(1)=-D(1)
         B(N)=-D(N-1)
         C(1)=0.0D0
         C(N)=0.0D0
         IF(N.ne.3) then
            C(1)=C(3)/(X(4)-X(2))-C(2)/(X(3)-X(1))
            C(N)=C(N-1)/(X(N)-X(N-2))-C(N-2)
     .           /(X(N-1)-X(N-3))
            C(1)=C(1)*D(1)**2.0D0/(X(4)-X(1))
            C(N)=-C(N)*D(N-1)**2.0D0/(X(N)-X(N-3))
         endif
         DO K=2,N
            T=D(K-1)/B(K-1)
            B(K)=B(K)-T*D(K-1)
            C(K)=C(K)-T*C(K-1)
         enddo
         C(N)=C(N)/B(N)
         DO IB=1,NM1
            K=N-IB
            C(K)=(C(K)-D(K)*C(K+1))/B(K)
         enddo
         B(N)=(Y(N)-Y(NM1))/D(NM1)
     1        +D(NM1)*(C(NM1)+2.0D0*C(N))
         DO  K=1,NM1
            B(K)=(Y(K+1)-Y(K))/D(K)
     1           -D(K)*(C(K+1)+2.0D0*C(K))
            D(K)=(C(K+1)-C(K))/D(K)
            C(K)=3.0D0*C(K)
         enddo
         C(N)=3.0D0*C(N)
         D(N)=D(N-1)
         RETURN
      endif
      B(1)=(Y(2)-Y(1))/(X(2)-X(1))
      C(1)=0.0D0
      D(1)=0.0D0
      B(2)=B(1)
      C(2)=0.0D0
      D(2)=0.0D0
      RETURN
      END

c-------------------------------------------------------------------------

c     END OF AKK ROUTINES

c---------------------------------------------------------------------------

