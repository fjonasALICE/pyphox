**************************************************************************
*                                                                            
*       NUCLEAR PARTON DISTRIBUTIONS (NLO and LO)                            
*       FROM  hep-ph/0311227   PRD                                           
*       D. de Florian and R. Sassot                                          
*                                                                            
*    The distributions are obtained by calling the subroutine                
*                                                                            
*                nDS(ISET,X,Q,A,RUV,RDV,RUB,RDB,RS,RG)                       
*                                                                            
*  INPUT:                                                     
*       iSET: 1 LO SET                                                       
*             2 NLO SET (MSbar scheme)                                       
*            X= x_bjorken                                                    
*            Q= factorization scale                                          
*            A= atomic number  
*  OUTPUT: the subroutine returns the NUCLEAR RATIOS
*               R_f(x,Q,A) = f_A(x,Q)/f_p(x,Q)
*  where f_A corresponds the pdf of a parton of flavour f in a proton of a

      subroutine nDS(ISET,X,Q,A,RUV,RDV,RUB,RDB,RS,RG)
**************************************************************************
*                                                                            
*       NUCLEAR PARTON DISTRIBUTIONS (NLO and LO)                            
*       FROM  hep-ph/0311xxx                                                 
*       D. de Florian and R. Sassot                                          
*                                                                            
*    The distributions are obtained by calling the subroutine                
*                                                                            
*                nDS(ISET,X,Q,A,RUV,RDV,RUB,RDB,RS,RG)                       
*                                                                            
*  INPUT:                                                     
*       iSET: 1 LO SET                                                       
*             2 NLO SET (MSbar scheme)                                       
*             3 LO SET  gluon shadowing                       
*             4 NLO SET gluon shadowing (MSbar scheme)  
*            X= x_bjorken                                                    
*            Q= factorization scale                                          
*            A= atomic number  
*  OUTPUT: the subroutine returns the NUCLEAR RATIOS
*               R_f(x,Q,A) = f_A(x,Q)/f_p(x,Q)
*  where f_A corresponds the pdf of a parton of flavour f in a proton of a
*  nucleus A, and f_p is the corresponding pdf in the free proton. 
*
*                    RUV :    U VALENCE DISTRIBUTION                     
*                    RDV :    D VALENCE DISTRIBUTION                     
*                    RUB :    UBAR DISTRIBUTION                          
*                    RDB :    DBAR DISTRIBUTION                          
*                    RS  :    STR(=STRBAR) DISTRIBUTION               
*                    RG  :    GLUON DISTRIBUTION                         
*  Heavy flavor quarks are not considered in the analysis. The 
*  "nuclear ratio" in the neutron can be obtained by isospin symmetry,
*   i.e. RUV_proton=RDV_neutron, etc
*                                                                            
*  COMMON:  The main program or the calling routine has to have    
*            a common block  COMMON / INTINPDF / IINPDF , and the     
*            integer variable  IINPDF  has always to be zero when    
*            nDS is called for the first time or when  ISET     
*            has been changed.                                      
*                                                                   
*   GRIDS:  nDS_LO.GRID, nDS_NLO.GRID, nDSg_LO.GRID, nDSg_NLO.GRID   
*                   
*        RANGE OF VALIDITY OF THE INTERPOLATION:                              
*                                       10^(-6)< X < 1.0                     
*                                       1 < Q**2 < 10^6                    
*                                       4 < A < 208      
*
*       IN CASE OF PROBLEMS, DOUBTS, ETC, PLEASE REPORT TO                   
*        deflo@df.uba.ar                                                     
*        sassot@df.uba.ar                                                    
*                                                                            
**************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      PARAMETER (NPART=6, NX=52, NQ=18, NNA=7, NARG=3)
      DIMENSION XUVF(NX,NQ,NNA), XDVF(NX,NQ,NNA), XUBF(NX,NQ,NNA),
     1  XDBF(NX,NQ,NNA), XSF(NX,NQ,NNA),XGF(NX,NQ,NNA),
     2     QS(NQ), XB(NX), XA(NNA), XT(NARG), NA(NARG), ARRF(NX+NQ+NNA) 
      COMMON / INTINPDF / IINPDF
      SAVE XUVF, XDVF, XUBF, XDBF, XSF, XGF, NA, ARRF

       DATA QS / 1E0, 1.5E0, 2E0, 4E0, 7E0,
     .          10E0, 15E0, 20E0, 40E0, 70E0,
     .          100E0, 400E0, 800E0,
     .           1.0E4,  5E4, 1.0E5,  5E5, 1.E6 /
 
        DATA XB / 1.0E-6,5.E-6,
     1           1.0E-5, 1.4E-5, 2.0E-5, 3.0E-5, 4.5E-5, 6.7E-5,
     2           1.0E-4, 1.4E-4, 2.0E-4, 3.0E-4, 4.5E-4, 6.7E-4,
     3           1.0E-3, 1.4E-3, 2.0E-3, 3.0E-3, 4.5E-3, 6.7E-3,
     4           1.0E-2, 1.4E-2, 2.0E-2, 3.0E-2, 4.5E-2, 0.06, 0.08,
     5           0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275,
     6           0.3, 0.325, 0.35, 0.375, 0.4,  0.45, 0.5, 0.55,
     7           0.6, 0.65,  0.7,  0.75,  0.8,  0.85, 0.9, 0.95, 1. /
       DATA XA/ 4, 12,  40, 56, 117, 184, 208/
       

c TEMPORARY
       if(A.eq.2.D0)then
          RUV=1.D0
          RDV=1.D0
          RUB=1.D0
          RDB=1.D0
          RS=1.D0
          RG=1.D0
          return
       endif

       if(iinpdf.ne.0) goto 10
       if (iset.eq.1) then
        OPEN(UNIT=1,FILE='nds/nDS_LO.GRID',STATUS='old')
        PRINT*, 'you are using the nDS_LO.GRID'
       else if (iset.eq.2) then
        OPEN(UNIT=1,FILE='nds/nDS_NLO.GRID',STATUS='old')
        PRINT*, 'you are using the nDS_NLO.GRID'
       else if (iset.eq.3) then
        OPEN(UNIT=1,FILE='nds/nDSg_LO.GRID',STATUS='old')
        PRINT*, 'you are using the nDSg_LO.GRID'
       else if (iset.eq.4) then
        OPEN(UNIT=1,FILE='nds/nDSg_NLO.GRID',STATUS='old')
        PRINT*, 'you are using the nDSg_NLO.GRID'
       else   
        write(6,*)'WRONG ISET NUMBER'
        STOP
       endif

       do i=1,NX
         ARRF(I) = XB(I)     
       enddo
       do j=1,NQ
         ARRF(J+NX) = dlog(QS(J))     
       enddo
       do k=1,NNA
          ARRF(k+NX+NQ) = XA(k)     
       enddo
       NA(1)=NX
       NA(2)=NQ
       NA(3)=NNA
       do i=1,nx
       do j=1,nq
       do k=1,nna
        read(1,90) xuvf(i,j,k),xdvf(i,j,k),xubf(i,j,k),xdbf(i,j,k),
     .            xsf(i,j,k),xgf(i,j,k)
       enddo
       enddo
       enddo
c ------------------------------------
c added by arleo on 09/08/2006
       rewind(1)
       close(1)
c ------------------------------------
       iinpdf=1
 10    continue

       xt(1)=x
       xt(2)=dlog(Q*Q)
       xt(3)=A
       RUV = FINTNDS(NARG,XT,NA,ARRF,XUVF) 
       RDV = FINTNDS(NARG,XT,NA,ARRF,XDVF) 
       RUB = FINTNDS(NARG,XT,NA,ARRF,XUBF) 
       RDB = FINTNDS(NARG,XT,NA,ARRF,XDBF) 
       RS = FINTNDS(NARG,XT,NA,ARRF,XSF) 
       RG = FINTNDS(NARG,XT,NA,ARRF,XGF)       
  90   FORMAT (6(1PE15.7))
       end
       
       
      FUNCTION FINTNDS(NARG,ARG,NENT,ENT,TABLE)
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
      FINTNDS=0.
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
      FINTNDS=FINTNDS+FAC*TABLE(IADR)
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


