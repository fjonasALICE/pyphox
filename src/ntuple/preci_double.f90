! 
!****h* src/module/precision
! NAME
!
!  Module precision
!
! USAGE
!
!  use precision
!
! DESCRIPTION
!
!  This module defines the parameter ki which gives the representation 
!  of the real and complex numbers in golem (for double precision)
!
! OUTPUT
!
!  The integer parameter ki
!
! USES
!
!  No uses
!
!*****
module precision
  !
  integer, parameter :: ki=kind(1.d0)
  !
end module precision

