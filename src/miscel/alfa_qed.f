C ce programme calcul le running de alpha_qed suivant jetset (c.f
C R.Kleiss et al. CERN 89-08, Vol. 3, p 129-131.
	double precision function alphaem(iloop,q2)
	implicit real*8 (a-h,l-z)
C...Calculate real part of photon vacuum polarization. 
C...For leptons simplify by using asymptotic (Q^2 >> m^2) expressions. 
C...For hadrons use parametrization of H. Burkhardt et al. 
C...See R. Kleiss et al, CERN 89-08, vol. 3, pp. 129-131.
	pi = 4.d0*datan(1.d0)
	alphaem = 1.d0/137.d0
	if (iloop.ne.0) then 
	  aempi = alphaem/(3.d0*pi) 
	  if (q2.lt.0.09d0) then 
	    rpigg=aempi*(13.4916+log(q2))+0.00835*log(1.+q2) 
	  elseif(q2.lt.9.d0) then 
	    rpigg=aempi*(16.3200+2.*log(q2))+0.00238*log(1.+3.927*q2) 
	  elseif(q2.lt.1d4) then 
	    rpigg=aempi*(13.4955+3.*log(q2))+0.00165+0.00299*log(1.+q2) 
	  else 
	    rpigg=aempi*(13.4955+3.*log(q2))+0.00221+0.00293*log(1.+q2) 
	  endif  
c...calculate running alpha_em. 
        alphaem = alphaem/(1.-rpigg)
      endif 
c 
      return 
      end 
