c$$$      PROGRAM TEST
c$$$      IMPLICIT REAL*8 (A-H,O-Z)
c$$$      DIMENSION DH(0:10)
c$$$      Z=0.5D0
c$$$      Q=100.D0
c$$$      CALL AKK(IH,ISET,Z,Q,DH)
c$$$      WRITE (6,10) Z,Q
c$$$      WRITE (6,10) DH(0),DH(1),DH(3),DH(5),DH(7),DH(9)
c$$$      STOP
c$$$10    FORMAT (1X,6(1X,E10.3))
c$$$      END
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      SUBROUTINE AKK(IH,ISET,Z,Q,DH)
      IMPLICIT REAL*8 (A-H,M-Z)
      DIMENSION DH(0:10),FFpion(0:5),FFkaon(0:5),
     .     FFproton(0:5),FFK0S(0:5),FFLambda(0:5)
C Input:
C IH hadron species
C 1  (pi^+ + pi^-)/2
C 2  pi^0
C 3  (K^+ + K^-)/2 
C 4  (K^0 + anti-K^0)/2, calculated from (K^+ + K^-)/2 with u<->d
C 5  (p + anti-p)/2
C 6  (n + anti-n)/2
C 7  pi^+ + pi^- + K^+ + K^- + p + anti-p
C 8  K0 short
C 9  (Lambda^0+anti-Lambda^0)/2
C ISET: DUMMY, always NLO
C Z: longitudinal-momentum fraction
C Q: fragmentation scale (in GeV)
C Output:
C DH(i): fragmentation function of parton i
C 0  1    2    3    4    5    6    7    8    9   10
C g  u  u-bar  d  d-bar  s  s-bar  c  c-bar  b  b-bar
      XI=DMAX1(Z,0.1046D0)
c      MU2=DMAX1(Q**2,2.D0)
c ARLEO NEW 21.05.07
c      print*, 'in akk ',Q
      mu2=q*q
      IF(Q*Q.gt.40000.D0)then
         MU2=39999.D0
c      ELSEIF(Q*Q.lt.2.D0)then
c         MU2=2.1D0
      ENDIF
c      print*, 'in akk ',mu2
      IF (IH.EQ.1.OR.IH.EQ.2) THEN
      call akk1_pion(xi,mu2,FFpion)
      DH(0)=FFpion(0)/2.D0
      DH(1)=FFpion(2)/2.D0
      DH(2)=DH(1)
      DH(3)=FFpion(1)/2.D0
      DH(4)=DH(3)
      DH(5)=FFpion(3)/2.D0
      DH(6)=DH(5)
      DH(7)=FFpion(4)/2.D0
      DH(8)=DH(7)
      DH(9)=FFpion(5)/2.D0
      DH(10)=DH(9)
      ELSE IF (IH.EQ.3) THEN
      call akk1_kaon(xi,mu2,FFkaon)
      DH(0)=FFkaon(0)/2.D0
      DH(1)=FFkaon(2)/2.D0
      DH(2)=DH(1)
      DH(3)=FFkaon(1)/2.D0
      DH(4)=DH(3)
      DH(5)=FFkaon(3)/2.D0
      DH(6)=DH(5)
      DH(7)=FFkaon(4)/2.D0
      DH(8)=DH(7)
      DH(9)=FFkaon(5)/2.D0
      DH(10)=DH(9)
      ELSE IF (IH.EQ.4) THEN
      call akk1_kaon(xi,mu2,FFkaon)
      DH(0)=FFkaon(0)/2.D0
      DH(1)=FFkaon(1)/2.D0
      DH(2)=DH(1)
      DH(3)=FFkaon(2)/2.D0
      DH(4)=DH(3)
      DH(5)=FFkaon(3)/2.D0
      DH(6)=DH(5)
      DH(7)=FFkaon(4)/2.D0
      DH(8)=DH(7)
      DH(9)=FFkaon(5)/2.D0
      DH(10)=DH(9)
      ELSE IF (IH.EQ.5) THEN
      call akk1_proton(xi,mu2,FFproton)
      DH(0)=FFproton(0)/2.D0
      DH(1)=FFproton(2)/2.D0
      DH(2)=DH(1)
      DH(3)=FFproton(1)/2.D0
      DH(4)=DH(3)
      DH(5)=FFproton(3)/2.D0
      DH(6)=DH(5)
      DH(7)=FFproton(4)/2.D0
      DH(8)=DH(7)
      DH(9)=FFproton(5)/2.D0
      DH(10)=DH(9)
      ELSE IF (IH.EQ.6) THEN
      call akk1_proton(xi,mu2,FFproton)
      DH(0)=FFproton(0)/2.D0
      DH(1)=FFproton(1)/2.D0
      DH(2)=DH(1)
      DH(3)=FFproton(2)/2.D0
      DH(4)=DH(3)
      DH(5)=FFproton(3)/2.D0
      DH(6)=DH(5)
      DH(7)=FFproton(4)/2.D0
      DH(8)=DH(7)
      DH(9)=FFproton(5)/2.D0
      DH(10)=DH(9)
      ELSE IF (IH.EQ.7) THEN
      call akk1_pion(xi,mu2,FFpion)
      call akk1_kaon(xi,mu2,FFkaon)
      call akk1_proton(xi,mu2,FFproton)
      DH(0)=FFpion(0)+FFkaon(0)+FFproton(0)
      DH(1)=FFpion(2)+FFkaon(2)+FFproton(2)
      DH(2)=DH(1)
      DH(3)=FFpion(1)+FFkaon(1)+FFproton(1)
      DH(4)=DH(3)
      DH(5)=FFpion(3)+FFkaon(3)+FFproton(3)
      DH(6)=DH(5)
      DH(7)=FFpion(4)+FFkaon(4)+FFproton(4)
      DH(8)=DH(7)
      DH(9)=FFpion(5)+FFkaon(5)+FFproton(5)
      DH(10)=DH(9)
      ELSE IF (IH.EQ.8) THEN
      call akk1_K0S(xi,mu2,FFK0S)
      DH(0)=FFK0S(0)
      DH(1)=FFK0S(2)
      DH(2)=DH(1)
      DH(3)=FFK0S(1)
      DH(4)=DH(3)
      DH(5)=FFK0S(3)
      DH(6)=DH(5)
      DH(7)=FFK0S(4)
      DH(8)=DH(7)
      DH(9)=FFK0S(5)
      DH(10)=DH(9)
      ELSE IF (IH.EQ.9) THEN
      call akk1_Lambda(xi,mu2,FFLambda)
      DH(0)=FFLambda(0)/2.d0
      DH(1)=FFLambda(2)/2.d0
      DH(2)=DH(1)
      DH(3)=FFLambda(1)/2.d0
      DH(4)=DH(3)
      DH(5)=FFLambda(3)/2.d0
      DH(6)=DH(5)
      DH(7)=FFLambda(4)/2.d0
      DH(8)=DH(7)
      DH(9)=FFLambda(5)/2.d0
      DH(10)=DH(9)
      END IF
      RETURN
      END
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      Function NextUt()
C                                 Returns an unallocated FORTRAN i/o unit.
      Logical EX
C
      Do 10 N = 50, 300
         INQUIRE (UNIT=N, OPENED=EX)
         If (.NOT. EX) then
            NextUt = N
            Return
         Endif
 10   Continue
      Stop ' There is no available I/O unit. '
C               *************************
      End
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
