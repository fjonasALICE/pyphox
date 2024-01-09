C ---------------------------------------------------
	double precision function dmax(x,y)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	if(x.gt.y)then
		dmax=x
	else
		dmax=y
	endif
	return
	end
C ---------------------------------------------------
C ---------------------------------------------------
	SUBROUTINE pdfa(nlo,x,Q,ish,A,ruv,rdv,ru,rd,rs,rc,rb,rg)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	COMMON / INTINPDF / IINPDF
	COMMON / INITNPDFDSSZ / INIDSSZ 
	DIMENSION PDFN(-4:4),PDFNA(-4:4),GRADN(-4:4,12),GRADA(-4:4,12)
	LOGICAL NLO

c -------------------------
c LABEL ish
c ish=1  nDS LO/NLO
c ish=2  nDSg LO/NLO
c ish=3  EKS98 LO
c ish=4  EPS08 LO
c ish=5  HKN04 LO
c ish=6  EPS09 LO/NLO
c ish=7  HKN07 LO/NLO
c ish=8  FGS10 LO/NLO mode 1
c ish=9  FGS10 LO/NLO mode 2
c ish=10 DSSZ NLO
c -------------------------

c -------------------------
c INITIALIZATION
c -------------------------
	ruv=1.D0
	rdv=1.D0
	ru=1.D0
	rd=1.D0
	rs=1.D0
	rc=1.D0
	rb=1.D0
	rg=1.D0
	if ((ish.eq.0).or.(A.le.2.D0)) return ! no shadowing corrections
c	if (ish.eq.0) return ! no shadowing corrections
c -------------------------
	if ((ish.ne.5).and.(ish.ne.7)) then	! not HKN04/HKN07
c 	   IINPDF=0		! internal variable in DS
c 	   INIDSSZ=0		! internal variable in DSSZ

		if ((ish.eq.1).and.(.not. nlo)) 
     *               CALL nDS(1,x,Q,A,ruv,rdv,ru,rd,rs,rg) ! nDS LO
		if ((ish.eq.2).and.(.not. nlo)) 
     *               CALL nDS(3,x,Q,A,ruv,rdv,ru,rd,rs,rg) ! nDSg LO
		if ((ish.eq.1).and.(nlo)) 
     *               CALL nDS(2,x,Q,A,ruv,rdv,ru,rd,rs,rg) ! nDS NLO
		if ((ish.eq.2).and.(nlo)) 
     *               CALL nDS(4,x,Q,A,ruv,rdv,ru,rd,rs,rg) ! nDSg NLO
		if (ish.eq.3) 
     *               CALL eks98(x,Q,A,ruv,rdv,ru,rd,rs,rc,rb,rt,rg) ! EKS98 LO
		if (ish.eq.4) 
     *               CALL eps08(x,Q,A,ruv,rdv,ru,rd,rs,rc,rb,rt,rg) ! EPS08 LO
		if ((ish.eq.6).and.(A.gt.2.D0))then
			if(.not. nlo) 
     *     CALL eps09(1,1,int(A),x,Q,ruv,rdv,ru,rd,rs,rc,rb,rg) ! EPS09 LO
			if(nlo) 
     *     CALL eps09(2,1,int(A),x,Q,ruv,rdv,ru,rd,rs,rc,rb,rg) ! EPS09 NLO
           endif
		if ((ish.ge.601).and.(ish.le.631).and.(A.gt.2.D0))then
			if(.not. nlo) 
     *     CALL eps09(1,ish-600,int(A),x,Q,ruv,rdv,ru,rd,rs,rc,rb,rg) ! EPS09 LO
			if(nlo) 
     *     CALL eps09(2,ish-600,int(A),x,Q,ruv,rdv,ru,rd,rs,rc,rb,rg) ! EPS09 NLO
           endif
	     if (ish.eq.8)
     *	CALL fgs10(1,x,Q*Q,A,ruv,rdv,ru,rd,rs,rc,rg,dummy)! FGS10 mode 1
	     if (ish.eq.9)
     *	CALL fgs10(2,x,Q*Q,A,ruv,rdv,ru,rd,rs,rc,rg,dummy)! FGS10 mode 2
		if (ish.eq.10)then 										! DSSZ
 		CALL DSSZ(x,Q,int(A),F2A,F2P,F2N,UVA,DVA,UBA,DBA,SA,CA,BA,GA,
     *                                   UV,DV,UB,DB,S,C,B,G)
	    ruv=UVA/UV
		rdv=DVA/DV
		ru=UBA/UB
		rd=DBA/DB
		rs=SA/S
		rc=CA/C
		rb=BA/B
		rg=GA/G
c		print*, x,rg,GA,G
		endif
	 else  ! HKN04 (5) or HKN07 (7)
	   if (A.eq.1.D0) IHKM=1
	   if (A.eq.2.D0) IHKM=2
	   if (A.eq.4.D0) IHKM=3
	   if (A.eq.7.D0) IHKM=4
	   if (A.eq.9.D0) IHKM=5
	   if (A.eq.12.D0) IHKM=6
	   if (A.eq.14.D0) IHKM=7
	   if (A.eq.27.D0) IHKM=8
	   if (A.eq.40.D0) IHKM=9
	   if (A.eq.56.D0) IHKM=10
	   if (A.eq.63.D0) IHKM=11
	   if (A.eq.84.D0) IHKM=12
	   if (A.eq.107.D0) IHKM=13
	   if (A.eq.108.D0) IHKM=13
	   if (A.eq.118.D0) IHKM=14
	   if (A.eq.131.D0) IHKM=15
	   if (A.eq.184.D0) IHKM=16
	   if (A.eq.197.D0) IHKM=17
	   if (A.eq.196.D0) IHKM=17
	   if (A.eq.208.D0) IHKM=18
	   if (A.eq.207.D0) IHKM=18
	   if (A.eq.16.D0) IHKM=19
	   if (A.eq.1.D0) Z=1.D0
	   if (A.eq.2.D0) Z=1.D0
	   if (A.eq.4.D0) Z=2.D0
	   if (A.eq.7.D0) Z=3.D0
	   if (A.eq.9.D0) Z=4.D0
	   if (A.eq.12.D0) Z=6.D0
	   if (A.eq.14.D0) Z=7.D0
	   if (A.eq.27.D0) Z=13.D0
	   if (A.eq.40.D0) Z=20.D0
	   if (A.eq.56.D0) Z=26.D0
	   if (A.eq.63.D0) Z=29.D0
	   if (A.eq.84.D0) Z=36.D0
	   if (A.eq.107.D0) Z=46.D0
	   if (A.eq.108.D0) Z=47.D0
	   if (A.eq.118.D0) Z=50.D0
	   if (A.eq.131.D0) Z=54.D0
	   if (A.eq.184.D0) Z=74.D0
	   if (A.eq.197.D0) Z=79.D0
	   if (A.eq.196.D0) Z=78.D0
	   if (A.eq.208.D0) Z=82.D0
	   if (A.eq.207.D0) Z=82.D0
	   if (A.eq.16.D0) Z=8.D0
	   
c	   if(A.le.2.D0) return ! no shadowing in deuterium (for comparing with hannu)
	   if(ish.eq.5)then
			CALL NPDF04(IHKM,Q*Q,x,PDFNA) ! HKM LO in a nucleus
		   	CALL NPDF04(1,Q*Q,x,PDFN) ! HKM LO in a proton
	   elseif(ish.eq.7)then
			if(nlo)then
				CALL HKNNPDFV2(Q*Q,x,IHKM,2,PDFNA,GRADA) ! HKN07 NLO in a nucleus
	   			CALL HKNNPDFV2(Q*Q,x,1,2,PDFN,GRADN) ! HKN07 NLO in a proton
			else
				CALL HKNNPDFV2(Q*Q,x,IHKM,1,PDFNA,GRADA) ! HKN07 LO in a nucleus
		   		CALL HKNNPDFV2(Q*Q,x,1,1,PDFN,GRADN) ! HKN07 LO in a proton		
			endif
		endif

	up=PDFN(1)
	dp=PDFN(2)
	ubp=PDFN(-1)
	dbp=PDFN(-2)

	ua=PDFNA(1)
	da=PDFNA(2)
	uba=PDFNA(-1)
	dba=PDFNA(-2)

	if(A.ne.2.D0*Z)then
		dpa=((A-Z)*ua-Z*da)/(A-2.D0*Z)
		upa=((A-Z)*da-Z*ua)/(A-2.D0*Z)
		dbpa=((A-Z)*uba-Z*dba)/(A-2.D0*Z)
		ubpa=((A-Z)*dba-Z*uba)/(A-2.D0*Z)
	else
		print*, 'cannot determine the ratios for isoscalar nuclei' 
		stop
	endif
		
	ruv=(upa-ubpa)/(up-ubp)
	rdv=(dpa-dbpa)/(dp-dbp)
	ru=ubpa/ubp
	rd=dbpa/dbp
	rs=PDFNA(3)/PDFN(3)
	rc=PDFNA(4)/PDFN(4)
	rb=1.D0
	rg=PDFNA(0)/PDFN(0)


	endif
c 	print*, 'nlo ',nlo,ish,x,Q,A,ruv,rdv,ru,rd,rs,rc,rg
	RETURN
	END
C ---------------------------------------------------
