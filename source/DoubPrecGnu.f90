!=======================================================================
MODULE Precision


   ! This module stores constants to specify the KIND of variables.

   ! NOTE: Use compile option "-fdefault-double-8" when using ReKi = 2 with the Gnu Fortran compiler.


IMPLICIT                           NONE

INTEGER, PARAMETER              :: B1Ki     =  3                                ! Default kind for one-byte numbers.
INTEGER, PARAMETER              :: B2Ki     =  5                                ! Default kind for two-byte numbers.
INTEGER, PARAMETER              :: B4Ki     =  1                                ! Default kind for four-byte numbers.
INTEGER, PARAMETER              :: B8Ki     =  2                                ! Default kind for eight-byte numbers.

INTEGER, PARAMETER              :: DbKi     =  2                                ! Default kind for double-precision, floating-point numbers.
!INTEGER, PARAMETER              :: QuKi     = 16                                ! Kind for 16-byte, floating-point numbers.
INTEGER, PARAMETER              :: R8Ki     =  2                                ! Default kind for eight-byte, floating-point numbers.
INTEGER, PARAMETER              :: ReKi     =  2                                ! Default kind for floating-point numbers.
INTEGER, PARAMETER              :: SiKi     =  1                                ! Kind for four-byte, floating-point numbers.

!INTEGER, PARAMETER              :: B1Ki     = SELECTED_INT_KIND(  2 )           ! Default kind for one-byte numbers.
!INTEGER, PARAMETER              :: B2Ki     = SELECTED_INT_KIND(  4 )           ! Default kind for two-byte numbers.
!INTEGER, PARAMETER              :: B4Ki     = SELECTED_INT_KIND(  9 )           ! Default kind for four-byte numbers.
!INTEGER, PARAMETER              :: B8Ki     = SELECTED_INT_KIND( 18 )           ! Default kind for eight-byte numbers.
!INTEGER, PARAMETER              :: DbKi     = SELECTED_REAL_KIND( 20, 500 )     ! Default kind for double-precision, floating-point numbers.
!INTEGER, PARAMETER              :: QuKi     = SELECTED_REAL_KIND( 20, 500 )     ! Kind for 16-byte, floating-point numbers.
!INTEGER, PARAMETER              :: R8Ki     = SELECTED_REAL_KIND( 14, 300 )     ! Default kind for eight-byte floating-point numbers.
!INTEGER, PARAMETER              :: ReKi     = SELECTED_REAL_KIND( 14, 300 )     ! Default kind for floating-point numbers.
!INTEGER, PARAMETER              :: SiKi     = SELECTED_REAL_KIND(  6,  30 )     ! Kind for four-byte, floating-point numbers.


END MODULE Precision
