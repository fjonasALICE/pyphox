 	! Parton-to-photon fragmentation function
 	!	x : fraction of energy taken by the photon
 	!	iflag : 1 -> set I, 2 -> set II.
 	!	xglue, .., xbottom : x*{fragmentation function}

      SUBROUTINE FONFRA(X,IFLAG,QP2,XDUP,XDUBP,XDDP,XDDBP,XDSP,XDCP,XDBP
     # ,XDBBP,XDTP,XDTBP,XDGP)
c      SUBROUTINE FONFRA(x,iflag,QP2,
c     _			xglue,xup,xdown,xstrange,xcharm,xbottom)
      IMPLICIT REAL*8 (A-H,L-Z)

        CALL distributionPert(x,QP2,'UP',up_Pert)
        CALL distributionPert(x,QP2,'DO',down_Pert)
        CALL distributionPert(x,QP2,'SB',strange_Pert)
        CALL distributionPert(x,QP2,'CB',charm_Pert)
        CALL distributionPert(x,QP2,'BB',bottom_Pert)
        CALL distributionPert(x,QP2,'GL',gluon_Pert)
	
	if(iflag .eq. 1) then
 	     CALL distributionNonPert_setI(x,QP2,'UP',up_VDM)
 	     CALL distributionNonPert_setI(x,QP2,'DO',down_VDM)
 	     CALL distributionNonPert_setI(x,QP2,'SB',strange_VDM)
 	     CALL distributionNonPert_setI(x,QP2,'CB',charm_VDM)
 	     CALL distributionNonPert_setI(x,QP2,'BB',bottom_VDM)
 	     CALL distributionNonPert_setI(x,QP2,'GL',gluon_VDM)
        elseif(iflag .eq. 2) then
 	     CALL distributionNonPert_setII(x,QP2,'UP',up_VDM)
 	     CALL distributionNonPert_setII(x,QP2,'DO',down_VDM)
 	     CALL distributionNonPert_setII(x,QP2,'SB',strange_VDM)
 	     CALL distributionNonPert_setII(x,QP2,'CB',charm_VDM)
 	     CALL distributionNonPert_setII(x,QP2,'BB',bottom_VDM)
 	     CALL distributionNonPert_setII(x,QP2,'GL',gluon_VDM)
        else
 	     write(*,*) 'unknown set'
 	     stop
        endif

	up = up_Pert + up_VDM
	down = down_Pert + down_VDM
	strange = strange_Pert + strange_VDM
	charm = charm_Pert + charm_VDM
	bottom = bottom_Pert + bottom_VDM
	gluon = gluon_Pert + gluon_VDM

      xup=up*x
      xdown=down*x
      xstrange=strange*x
      xcharm=charm*x
      xbottom=bottom*x
      xglue=gluon*x
c rajoute pour interface avec hadlib.f      
      xdup = xup
      xdubp = xup
      xddp = xdown
      xddbp = xdown
      xdsp = xstrange
      xdcp = xcharm
      xdbp = xbottom
      xdbbp = xbottom
      xdtp = 0.d0
      xdtbp = 0.d0
      xdgp = xglue

      END
