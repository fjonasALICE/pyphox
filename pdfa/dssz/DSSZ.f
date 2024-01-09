      SUBROUTINE DSSZ(X,Q,A,F2A,F2P,F2N,UVA,DVA,UBA,DBA,SA,CA,BA,GA,
     1                                  UV,DV,UB,DB,S,C,B,G)
*************************************************************************
*
*       DSSZ NLO GLOBAL ABALYSIS OF NUCLEAR PARTON DISTRIBUTIONS         
*
*       D. de Florian, R. Sassot, M. Stratmann, P. Zurita
*                             
*  REFERENCE:   arXiv:1112.6324
*
*  USAGE:
*                                               
*       The distributions are obtained by calling the subroutine
*
*            DSSZ(X,Q,A,F2A,F2P,F2N,UVA,DVA,UBA,DBA,SA,CA,BA,GA,
*                         UV,DV,UB,DB,S,C,B,G) 
*                                                                      
*  INPUT:                                                     
*            X= x_bjorken                                                
*    
*            Q= factorization scale   
*                                       
*            A= atomic number  
*
*  OUTPUT: the subroutine returns 
*
*         - the NUCLEAR PDFS            f_A(x,Q)
*          where f_A corresponds the pdf of a parton of flavour f 
*          in a proton of a nucleus A
*         - the PROTON PDFS            f_p(x,Q)
*          following MSTW's evolution w 
*         - the EM F2 for nucleus A, proton and neutron
*
*                    F2A :    ELECTROMAGNETIC NUCLEAR F2 
*                    F2P :    ELECTROMAGNETIC PROTON F2 
*                    F2N :    ELECTROMAGNETIC NEUTRON F2 
*
*                    UVA :    NUCLEAR U VALENCE DISTRIBUTION
*                    DVA :    NUCLEAR D VALENCE DISTRIBUTION
*                    UBA :    NUCLEAR UBAR DISTRIBUTION
*                    DBA :    NUCLEAR DBAR DISTRIBUTION
*                    SA  :    NUCLEAR STR(=STRBAR) DISTRIBUTION
*                    CA  :    NUCLEAR CHARM DISTRIBUTION 
*                    BA  :    NUCLEAR BOTTOM DISTRIBUTION
*                    GA  :    NUCLEAR GLUON DISTRIBUTION     
*
*                    UV  :    U VALENCE DISTRIBUTION
*                    DV  :    D VALENCE DISTRIBUTION
*                    UB  :    UBAR DISTRIBUTION
*                    DB  :    DBAR DISTRIBUTION
*                    S   :    STR(=STRBAR) DISTRIBUTION
*                    C   :    CHARM DISTRIBUTION 
*                    B   :    BOTTOM DISTRIBUTION
*                    G   :    GLUON DISTRIBUTION       
*              
*          The pdfs in the neutron can be obtained by isospin symmetry,
*          i.e., UVA_proton=DVA_neutron, etc
*          i.e., UV_proton=DV_neutron, etc
*                           
*  COMMON:  The main program or the calling routine has to have    
*           a common block  COMMON / INITNPDF / INIDSSZ , and the     
*           integer variable  INIDSSZ has always to be zero when    
*           DSSZ is called for the first time.                           
*                   
*  RANGE OF VALIDITY OF THE INTERPOLATION:                              
*                           10^(-4) < X < 1.0                     
*                           1 < Q**2 < 10^4                    
*                           A = 4, 12,  40, 56, 117, 184, 208
*
*  IN CASE OF PROBLEMS, DOUBTS, ETC, PLEASE REPORT TO :                 
*           deflo@df.uba.ar
*           sassot@df.uba.ar     
*           marco@bnl.gov
*           pia@df.uba.ar
*                                                                   
**************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
c      implicit none
      integer nx,nq,narga,nna,nargp,XA(7),A,i,k,j,inidssz,NAA(3),NAP(2)
      double precision XUVFA(49,23,7),XDVFA(49,23,7),XUBFA(49,23,7),
     1     XDBFA(49,23,7),XSFA(49,23,7),x,XCFA(49,23,7),XBFA(49,23,7),
     2     XGFA(49,23,7),XF2A(49,23,7),XUVFP(49,23),XDVFP(49,23),
     1     XUBFP(49,23),XDBFP(49,23),XSFP(49,23),XF2N(49,23),
     2     XCFP(49,23),XBFP(49,23),XGFP(49,23),XF2P(49,23)
      double precision QS(23), XB(49),q,xb1,xb0,F2A,F2P,F2N
      dimension ARRFA(79),XTA(3),XTP(2),ARRFP(72)
      double precision uva,dva,uba,dba,ba,ca,ga,sa,dsszfint
      double precision uv,dv,ub,db,b,c,g,s
C---
      COMMON / INITNPDFDSSZ / INIDSSZ
      SAVE XF2A,XUVFA,XDVFA,XUBFA,XDBFA,XSFA,XCFA,XBFA,XGFA,NAA,ARRFA,
     1  XF2P,XF2N,XUVFP,XDVFP,XUBFP,XDBFP,XSFP,XCFP,XBFP,XGFP,NAP,ARRFP


C---TABLE OF (X,Q2) SUPPORT POINTS 
      DATA QS / 1.D0, 1.5D0, 2.D0, 3.D0, 4.5D0, 6.D0, 8.D0, 10.D0,
     1     15.D0, 20.D0, 30.D0, 45.d0, 60.D0, 80.D0, 100.D0,
     2     200.D0, 300.D0, 450.D0, 700.D0, 1000.D0,
     3     2500.D0, 6000.D0, 10000.D0/

      DATA XB / 1.0E-6, 2.0E-6, 6.0E-6, 1.0E-5, 2.0E-5, 6.0E-5,
     3     1.0E-4, 2.0E-4, 6.0E-4, 1.0E-3, 2.0E-3, 6.0E-3,
     4     0.01D0, 0.014D0, 0.02D0, 0.03D0, 0.045D0, 0.06D0, 0.08D0,
     5     0.1D0, 0.125D0, 0.15D0, 0.175D0, 0.2D0, 0.225D0, 0.25D0,
     6     0.3D0, 0.325D0, 0.35D0, 0.375D0, 0.4D0, 0.425D0, 0.45D0,
     7     0.475D0, 0.5D0, 0.525D0, 0.55D0, 0.575D0, 0.6D0,
     8     0.625D0, 0.65D0, 0.7D0, 0.75D0, 0.8D0, 0.85D0,
     9     0.90D0, 0.925D0, 0.95D0, 1.D0/
C---SUPPORTED NUCLEI: Be, Fe, Au, Pb
c      DATA XA/ 9, 56, 197, 208/
      DATA XA/ 4, 12,  40, 56, 117, 184, 208/

C---
      nx=49
      nq=23
      narga=3
      nargp=2
      nna=7
      if(inidssz.ne.0) THEN
         goto 10
      ELSE
        OPEN(UNIT=88,FILE='dssz/dssz-nlo-central-nuc.grid',
     *  STATUS='unknown')
        OPEN(UNIT=89,FILE='dssz/dssz-nlo-central-prot.grid',
     *STATUS='unknown')
      ENDIF
C---

C---
 
       do 34 i=1,nx
          do 35 j=1,nq
             do 36 k=1,nna
                read(88,90) xuvfa(i,j,k),xdvfa(i,j,k),xubfa(i,j,k),
     1               xdbfa(i,j,k),xsfa(i,j,k),
     2               xcfa(i,j,k),xbfa(i,j,k),xgfa(i,j,k),xf2a(i,j,k)

c       write(6,*) xUVf(i,j,k)

 36          continue
 35       continue
 34    continue
       close(88)

       do 334 i=1,nx
          do 335 j=1,nq
                read(89,90) xuvfp(i,j),xdvfp(i,j),xubfp(i,j),
     1               xdbfp(i,j),xsfp(i,j),
     2               xcfp(i,j),xbfp(i,j),xgfp(i,j),xf2p(i,j),xf2n(i,j)

 335       continue
 334    continue
       close(89)
       inidssz=1


C---
       do 55 k=1,nna
        do 56 j=1,nq
         do 57 i=1,nx-1
 
         xb0=xb(i)
         xb1=1.d0-xb(i)

         XUVfa(i,j,k)=xUVfa(i,j,k)/(xb1**4.d0 * xb0**0.5d0)
         XDVfa(i,j,k)=xDVfa(i,j,k)/(xb1**4.d0 * xb0**0.5d0)
         xUBfa(i,j,k)=xUBfa(i,j,k)/(xb1**5.d0 * xb0**(-0.1d0))
         xDBfa(i,j,k)=xDBfa(i,j,k)/(xb1**5.d0 * xb0**(-0.1d0))
         xSfa(i,j,k)=xSfa(i,j,k)/(xb1**5.d0 * xb0**(-0.1d0))
         xCfa(i,j,k)=xCfa(i,j,k)/(xb1**5.d0 * xb0**(-0.1d0))
         xBfa(i,j,k)=xBfa(i,j,k)/(xb1**5.d0 * xb0**(-0.1d0))
         xGfa(i,j,k)=xGfa(i,j,k)/(xb1**5.d0 * xb0**(-0.1d0))
         xf2a(i,j,k)=xf2a(i,j,k)/(xb1**4.d0 * xb0**(-0.1d0))
      
 57      continue
         xUVfa(nx,j,k)=0.d0
         xDVfa(nx,j,k)=0.d0
         xUBfa(nx,j,k)=0.d0
         xDBfa(nx,j,k)=0.d0
         xSfa(nx,j,k)=0.d0
         xCfa(nx,j,k)=0.d0
         xBfa(nx,j,k)=0.d0
         xGfa(nx,j,k)=0.d0
         xf2a(nx,j,k)=0.d0
 56      continue
 55      continue
       NAA(1)=NX
       NAA(2)=NQ
       NAA(3)=NNA
      do i=1,NX
         ARRFA(I) = XB(I)     
      enddo
      do j=1,NQ
         ARRFA(J+NX) = DLOG(QS(J))     
      enddo
      do K=1,NNA
         ARRFA(k+NX+NQ) = XA(k)*1.d0     
      enddo

        do 556 j=1,nq
         do 557 i=1,nx-1
 
         xb0=xb(i)
         xb1=1.d0-xb(i)

         XUVfp(i,j)=xUVfp(i,j)/(xb1**4.d0 * xb0**0.5d0)
         XDVfp(i,j)=xDVfp(i,j)/(xb1**4.d0 * xb0**0.5d0)
         xUBfp(i,j)=xUBfp(i,j)/(xb1**5.d0 * xb0**(-0.1d0))
         xDBfp(i,j)=xDBfp(i,j)/(xb1**5.d0 * xb0**(-0.1d0))
         xSfp(i,j)=xSfp(i,j)/(xb1**5.d0 * xb0**(-0.1d0))
         xCfp(i,j)=xCfp(i,j)/(xb1**5.d0 * xb0**(-0.1d0))
         xBfp(i,j)=xBfp(i,j)/(xb1**5.d0 * xb0**(-0.1d0))
         xGfp(i,j)=xGfp(i,j)/(xb1**5.d0 * xb0**(-0.1d0))
         Xf2p(i,j)=xf2p(i,j)/(xb1**4.d0 * xb0**(-0.2d0))
         Xf2n(i,j)=xf2n(i,j)/(xb1**4.d0 * xb0**(-0.2d0))

 557      continue
         xUVfp(nx,j)=0.d0
         xDVfp(nx,j)=0.d0
         xUBfp(nx,j)=0.d0
         xDBfp(nx,j)=0.d0
         xSfp(nx,j)=0.d0
         xCfp(nx,j)=0.d0
         xBfp(nx,j)=0.d0
         xGfp(nx,j)=0.d0
         xf2p(nx,j)=0.d0
         xf2n(nx,j)=0.d0

 556      continue
       NAP(1)=NX
       NAP(2)=NQ
      do i=1,NX
         ARRFP(I) = XB(I)     
      enddo
      do j=1,NQ
         ARRFP(J+NX) = DLOG(QS(J))     
      enddo

 10   continue



      xta(1)=x
      xta(2)=dlog(Q*Q)
      xta(3)=A*1.d0
      F2A=DSSZFINT(NARGA,XTA,NAA,ARRFA,XF2A)*(1.d0-x)**4.d0*x**(-0.1d0)
     1     /2.d0   
      UVA=DSSZFINT(NARGA,XTA,NAA,ARRFA,XUVFA)*(1.d0-x)**4.d0*x**0.5d0
      DVA= DSSZFINT(NARGA,XTA,NAA,ARRFA,XDVFA)*(1.d0-x)**4.d0* x**0.5d0
      UBA=DSSZFINT(NARGA,XTA,NAA,ARRFA,XUBFA)*(1.d0-x)**5.d0*x**(-0.1d0)
      DBA=DSSZFINT(NARGA,XTA,NAA,ARRFA,XDBFA)*(1.d0-x)**5.d0*x**(-0.1d0)
      SA=DSSZFINT(NARGA,XTA,NAA,ARRFA,XSFA)* (1.d0-x)**5.d0*x**(-0.1d0)
      CA=DSSZFINT(NARGA,XTA,NAA,ARRFA,XCFA)* (1.d0-x)**5.d0*x**(-0.1d0)
      BA=DSSZFINT(NARGA,XTA,NAA,ARRFA,XBFA)* (1.d0-x)**5.d0*x**(-0.1d0)
      GA=DSSZFINT(NARGA,XTA,NAA,ARRFA,XGFA)* (1.d0-x)**5.d0*x**(-0.1d0)

       xtp(1)=x
       xtp(2)=dlog(Q*Q)
       f2p=DSSZFINT(NARGp,XTp,NAp,ARRFp,XF2p)*(1.d0-x)**4.d0*x**(-0.2d0)
       f2n=DSSZFINT(NARGp,XTp,NAp,ARRFp,XF2n)*(1.d0-x)**4.d0*x**(-0.2d0)
       UV=DSSZFINT(NARGp,XTp,NAp,ARRFp,XUVFp)* (1.d0-x)**4.d0 * x**0.5d0
       DV=DSSZFINT(NARGp,XTp,NAp,ARRFp,XDVFp)* (1.d0-x)**4.d0 * x**0.5d0
       UB=DSSZFINT(NARGp,XTp,NAp,ARRFp,XUBFp)*(1.d0-x)**5.d0*x**(-0.1d0)
       DB=DSSZFINT(NARGp,XTp,NAp,ARRFp,XDBFp)*(1.d0-x)**5.d0*x**(-0.1d0)
       S=DSSZFINT(NARGp,XTp,NAp,ARRFp,XSFp) * (1.d0-x)**5.d0*x**(-0.1d0)
       C=DSSZFINT(NARGp,XTp,NAp,ARRFp,XCFp) * (1.d0-x)**5.d0*x**(-0.1d0)
       B=DSSZFINT(NARGp,XTp,NAp,ARRFp,XBFp) * (1.d0-x)**5.d0*x**(-0.1d0)
       G=DSSZFINT(NARGp,XTp,NAp,ARRFp,XGFp) * (1.d0-x)**5.d0*x**(-0.1d0)


  90   FORMAT (8(1PE15.7))


       END
C       
C      
      double precision FUNCTION DSSZFINT(NARG,ARG,NENT,ENT,TABLE)
*********************************************************************
*                                                                   *
*   THE INTERPOLATION ROUTINE (CERN LIBRARY ROUTINE E104)           *
*                                                                   *
*********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DIMENSION ARG(5),NENT(5),ENT(10),TABLE(10)
      DIMENSION D(5),NCOMB(5),IENT(5)
      KD=1
      M=1
      JA=1
         DO 5 I=1,NARG
      NCOMB(I)=1
      JB=JA-1+NENT(I)
         DO 2 J=JA,JB
      IF (ARG(I).LE.ENT(J)) GO TO 3
    2 CONTINUE
      J=JB
    3 IF (J.NE.JA) GO TO 4
      J=J+1
    4 JR=J-1
      D(I)=(ENT(J)-ARG(I))/(ENT(J)-ENT(JR))
      IENT(I)=J-JA
      KD=KD+IENT(I)*M
      M=M*NENT(I)
    5 JA=JB+1
      DSSZFINT=0.
   10 FAC=1.
      IADR=KD
      IFADR=1
         DO 15 I=1,NARG
      IF (NCOMB(I).EQ.0) GO TO 12
      FAC=FAC*(1.-D(I))
      GO TO 15
   12 FAC=FAC*D(I)
      IADR=IADR-IFADR
   15 IFADR=IFADR*NENT(I)
      DSSZFINT=DSSZFINT+FAC*TABLE(IADR)
      IL=NARG
   40 IF (NCOMB(IL).EQ.0) GO TO 80
      NCOMB(IL)=0
      IF (IL.EQ.NARG) GO TO 10
      IL=IL+1
         DO 50  K=IL,NARG
   50 NCOMB(K)=1
      GO TO 10
   80 IL=IL-1
      IF(IL.NE.0) GO TO 40
      RETURN
      END


