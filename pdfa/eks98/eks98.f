C***************************************************************************
C
C		 	eks98.f
C
C An interface for calculating the SCALE DEPENDENT NUCLEAR RATIOS
C 		R_f^A(x,Q) = f_A(x,Q)/f_p(x,Q) 
C where f_A is the distribution of parton flavour f in a proton of a 
C nucleus A, and f_p is the corresponding parton distribution in the 
C free proton.
C  
C When you are using this interface, please REFER TO:
C K.J. Eskola, V.J. Kolhinen and C.A. Salgado, 
C "The scale dependent nuclear effects in parton distributions for 
C practical applications", Eur. Phys. J. C9 (1999) 61,
C JYFL-8/98, US-FT/14-98, hep-ph/9807297.
C
C The detailed formulation of our approach is given in 
C K.J. Eskola, V.J. Kolhinen and P.V. Ruuskanen,
C "Scale evolution of nuclear parton distributions"
C Nucl. Phys. B535 (1998) 351, CERN-TH/97-345, JYFL-2/98, hep-ph/9802350,
C so please refer also to this paper.
C
C The ratios R_f^A are to a good approximation independent of the choice
C of the parton distribution set for the free proton, so the absolute
C distributions of parton flavour f in a proton of a nucleus A can be 
C obtained simply by:
C f_A(x,Q) = R_f^A(x,Q) * f_p(x,Q), 
C where f_p is from any modern (lowest order) set of parton distributions.
C The corresponding distributions in a neutron of the nucleus can be 
C obtained through the isospin symmetry (=an approximation for non-isoscalar 
C nuclei)
C
C Questions & comments to:
C   	salgado@fpaxp1.usc.es
C   	vesa.kolhinen@phys.jyu.fi
C   	kari.eskola@phys.jyu.fi
C 
C August 4, 1998 / April 10, 2000 (new references added)
C-------------------------------------------------------------------
C
C INSTRUCTIONS:
C
C call eks98(x,Q,A,ruv,rdv,ru,rd,rs,rc,rb,rt,rg)
C
C Returns the nuclear corrections R_f^A(x,Q) in double precision for f=
C	
C	u_valence: ruv
C	d_valence: rdv (=ruv)
C	u_sea: ru
C	d_sea: rd (=ru)
C	s: rs
C	c: rc
C	b: rb
C	t: rt (always set to 1)
C	glue: rg
C
C For x, Q (Q is in GeV) and atomic number A. 
C x, Q and A are in DOUBLE PRECISION
C 
C No initialization is needed.
C
C This program needs data files par0.all and parxQA.all.
C They must be located in current working directory.
C
C This parametrization should only be applied at
C 1e-6 < x < almost 1,     1.5 < Q< 100 GeV
C Warning: No warning is given if the above kinematic region
C           in x&Q is exceeded.
C If A<=2, the function returns 1.
C
C
C
C-------------------------------------------------------------------

      subroutine eks98(x,q,a,ruv,rdv,ru,rd,rs,rc,rb,rt,rg)
      implicit double precision (a-h,o-z)
      dimension pqq(3)
      dimension r(5)
      common/eks983/qq0(3),x1,x116,aa1,aa8
      common/eks984/ptm(10)
      common/eks985/p0,p1,p2
      common/eks981/pa(3,10,3,8)
      common/eks982/pk0(3,180,8)
      common/eks986/indx,indpi,ikpt
      common/eks987/nm(5), kpt(5)
      common/eks988/ v1, v2
      data readFR/0/
      if (readFR.ne.1) then
         call eksinit
         readFR=1
      endif
      rt=1.d0
      if ((a.le.2.d0).or.(1.d0.le.x)) then
         ruv=1.d0
         rdv=1.d0
         ru=1.d0
         rd=1.d0
         rs=1.d0
         rc=1.d0
         rb=1.d0
         rg=1.d0
         return
      endif
      v1=dlog(a/aa1)
      v2=dlog(x/x1)
      if (x.le.0.6d0) then
         vtem=dlog10(0.6d0)+6.d0
         indx=1+dint((dlog10(x)+6.d0)/vtem*
     #        149.d0+1.d-7)
         if (indx.lt.2) indx=2
         else 
            indx=150+dint(75.d0*(x-0.6d0)+1.d-7)
      endif
      do 33 inum=1,5
         aqq=q*q
         nmin=nm(inum)
         ikpt=kpt(inum)
         if (aqq.lt.qq0(nmin)) aqq=qq0(nmin)
         qlq=dlog(aqq/qq0(nmin))
         q1=eksar0(a)
         indx=indx+1
         q2=eksar0(a)
         indx=indx-1
         if (x.le.0.6d0) then
            xxp=10.d0**(-6.d0+(indx-1)*vtem/149.d0)
            xxu=10.d0**(-6.d0+(indx)*vtem/149.d0)
            else
               xxp=0.6d0+(indx-150)*0.4d0/30.d0
               xxu=0.6d0+(indx+1-150)*0.4d0/30.d0
         endif
         r0=q1+(q2-q1)/(xxp-xxu)*(xxp-x)
         do 32 jk=1,3
            indpi=jk
            do 31 kl=1,10
               p0=pa(1,kl,indpi,ikpt)
               p1=pa(2,kl,indpi,ikpt)
               p2=pa(3,kl,indpi,ikpt)
               ptm(kl)=eksara(a)
31          continue
         pqq(jk)=eksarp(x)
32       continue
         r(inum)=r0+pqq(1)*qlq+pqq(2)*qlq*qlq+pqq(3)*dsqrt(qlq)
         if (inum.eq.2) then
            do 34 jk=1,3
               indpi=jk
               do 35 kl=1,10
                  p0=pa(1,kl,indpi,5)
                  p1=pa(2,kl,indpi,5)
                  p2=pa(3,kl,indpi,5)
                  ptm(kl)=eksara(a)
35             continue
            pqq(jk)=eksarp(x)
34          continue
            rs=r0+pqq(1)*qlq+pqq(2)*qlq*qlq+pqq(3)*dsqrt(qlq)
         endif
33    continue
      ruv=r(1)
      rdv=ruv
      ru=r(2)
      rd=ru
      rc=r(3)
      rb=r(4)
      rg=r(5)
      return
      end

      function eksar0(aa)
      implicit double precision (a-h,o-z)
      common/eks986/indx,indpi,kpt
      common/eks982/pk0(3,180,8)
      common/eks988/ v1, v2
      z=v1
      eksar0=pk0(1,indx,kpt)+pk0(2,indx,kpt)*z
     #+pk0(3,indx,kpt)*z*z
      return
      end

      function eksara(aa)
      implicit double precision (a-h,o-z)
      common/eks985/yy1,p1,p2
      common/eks988/ v1, v2
      z=v1
      eksara=yy1+p1*z+p2*z*z
      return
      end

      function eksarp(x)
      implicit double precision (a-h,o-z)
      common/eks986/indx,indpi,kpt
      common/eks983/qq0(3),x1,x116,aa1,aa8
      common/eks984/yy1,p1,p2,p3,p4,p5,p6,p7,p8,p9
      common/eks988/ v1, v2
      z=v2
      xx=x-x1
      if (x.le.x116) then
         eksarp=yy1+p1*z+p2*z**2+p3*xx+p4*xx**2
         else
            z1=dlog(x116/x1)
            xx1=x116-x1
            ff0=yy1+p1*z1+p2*z1**2+p3*xx1+p4*xx1**2
            xx16=x-x116
            z16=dlog(x/x116)
            qexp=19.d0+(indpi-1)*(indpi-2)*8.d0/2.d0
            eksarp=ff0+p5*xx16+p6*xx16**2+p7*xx16**3
     #             +p8*xx16**qexp+p9*z16
      endif
11    return
      end

      subroutine eksinit
      implicit double precision (a-h,o-z)
      common/eks981/pa(3,10,3,8)
      common/eks982/pk0(3,180,8)
      common/eks983/qq0(3),x1,x116,aa1,aa8
      common/eks987/nm(5), kpt(5)
      data qq0 /2.25d0, 2.54958d0, 21.3474d0/, 
     #     x1 /1.d-6/, x116 /.263553d-01/,
     #     aa1 /4.d0/, aa8 /208.d0/  
      data nm/1, 1, 2, 3, 1/,
     #     kpt/1, 3, 6, 7, 8/
      data readFR2/0/
      if (readFR2.ne.1) then
         open(11,file='eks98/parxQA.all',status='UNKNOWN')
         do 30 i=1,8
            do 40 j=1,3
               do 50 k=1,10
50             read(11,*),pa(1,k,j,i),pa(2,k,j,i),pa(3,k,j,i)
40       continue
30       continue
         close(11)
         open(11,file='eks98/par0.all',status='UNKNOWN')
         do 10 i=1,8
            do 20 j=1,180
20             read(11,137),pk0(1,j,i),pk0(2,j,i),pk0(3,j,i)
10       continue
         close(11)
         readFR2=1
      endif
      return
137   format(3e15.8)
      end
      
