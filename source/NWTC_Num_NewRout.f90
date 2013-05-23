MODULE NWTC_Num_NewRout


      ! This module contains numeric-type routines with non-system-specific logic and references.


   USE                           NWTC_Library

   IMPLICIT                      NONE


      ! Derived types used by routines in this module.

   TYPE, PUBLIC               :: RegGridType                                  ! This derived type is used to hold the contents of a regular grid of data.
      INTEGER                                :: NumDims                       ! The number of dimensions for this grid.
      REAL(ReKi), ALLOCATABLE                :: Mins      (:)                 ! The set of minimums for the grid in each NumDims dimensions.
      REAL(ReKi), ALLOCATABLE                :: Steps     (:)                 ! The set of step sizes for the grid in each NumDims dimensions.
      REAL(ReKi), ALLOCATABLE                :: Grid      (:)                 ! The NumDims dimensional grid.
   END TYPE RegGridType


   TYPE, PUBLIC               :: CubSplineType                                ! This derived type is used to hold data for performing cubic splines.
      INTEGER                                :: NumPts                        ! The number of points in the XAry and YAry arrays.
      REAL(ReKi), ALLOCATABLE                :: Coef      (:,:)               ! The NumPts-1 length array of cubic coefficients.  The second dimension must be "0:3".
      REAL(ReKi), ALLOCATABLE                :: XAry      (:)                 ! The NumPts length array of x values for the interpolation.
      REAL(ReKi), ALLOCATABLE                :: YAry      (:)                 ! The NumPts length array of y values for the interpolation.
   END TYPE CubSplineType


   TYPE, PUBLIC               :: RegCubSplineType                             ! This derived type is used to hold data for performing cubic splines wuth regularly-spaced data.
      INTEGER                                :: NumPts                        ! The number of points in the XAry and YAry arrays.
      REAL(ReKi), ALLOCATABLE                :: Coef      (:,:)               ! The NumPts-1 length array of cubic coefficients.  The second dimension must be "0:3".
      REAL(ReKi)                             :: DelX                          ! The distance between the equally spaced points in XAry.
      REAL(ReKi), ALLOCATABLE                :: XAry      (:)                 ! The NumPts length array of x values for the interpolation.
      REAL(ReKi), ALLOCATABLE                :: YAry      (:)                 ! The NumPts length array of y values for the interpolation.
   END TYPE RegCubSplineType


CONTAINS

      ! It contains the following routines:

   !  SUBROUTINE    CubicSplineInit   (    AryLen, XAry, YAry,       Coef )   ! Calculate coefficients for irregularly spaced array to use cubic splines.
   !  FUNCTION      CubicSplineInterp ( X, AryLen, XAry, YAry,       Coef )   ! Interpolate a irregularly spaced array using cubic splines.
   !  SUBROUTINE RegCubicSplineInit   (    AryLen, XAry, YAry, DelX, Coef )   ! Calculate coefficients for regularly spaced array to use cubic splines.
   !  FUNCTION   RegCubicSplineInterp ( X, AryLen, XAry, YAry, DelX, Coef )   ! Interpolate a regularly spaced array using cubic splines.

!=======================================================================
   SUBROUTINE CubicSplineInit ( AryLen, XAry, YAry, Coef, ErrStat, ErrMsg )


      ! This routine calculates the parameters needed to compute a irregularly-spaced natural cubic spline.
      ! Natural cubic splines are used in that the curvature at the end points is zero.
      ! It assumes the XAry values are equally spaced for speed.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                     ! Length of the array.

   REAL(ReKi), INTENT(OUT)      :: Coef  (AryLen-1,0:3)                       ! The coefficients for the cubic polynomials.
   REAL(ReKi), INTENT(IN)       :: XAry  (AryLen)                             ! Input array of x values.
   REAL(ReKi), INTENT(IN)       :: YAry  (AryLen)                             ! Input array of y values.

   INTEGER(IntKi), INTENT(OUT)  :: ErrStat                                    ! Error status.

   CHARACTER(4096), INTENT(OUT) :: ErrMsg                                     ! Error message.


      ! Local declarations.

   REAL(ReKi), ALLOCATABLE      :: DelX  (:)                                  ! The distances between the randomly spaced points.
   REAL(ReKi), ALLOCATABLE      :: Slope (:)                                  ! The AryLen-1 length array of slopes between points.
   REAL(ReKi), ALLOCATABLE      :: U     (:)                                  ! An AryLen-1 length array used in the Gaussian elimination.
   REAL(ReKi), ALLOCATABLE      :: V     (:)                                  ! An AryLen-1 length array used in the Gaussian elimination.
   REAL(ReKi)                   :: ZHi                                        ! A parameter used to calculate the polynomial coefficients.
   REAL(ReKi)                   :: ZLo                                        ! A parameter used to calculate the polynomial coefficients.

   INTEGER(IntKi)               :: ErrStatLcL                                 ! Local error status.
   INTEGER                      :: I                                          ! The index into the arrays.



      ! Allocate the various intermediate arrays.

   ALLOCATE ( DelX( AryLen - 1 ), STAT=ErrStatLcL )
   IF ( ErrStatLcL /= 0 )  THEN
      CALL ExitThisRoutine ( ErrID_Fatal, NewLine//' >> Error allocating memory for the DelX array in CubicSplineInit.' )
      RETURN
   ENDIF

   ALLOCATE ( Slope( AryLen - 1 ), STAT=ErrStatLcL )
   IF ( ErrStatLcL /= 0 )  THEN
      CALL ExitThisRoutine ( ErrID_Fatal, NewLine//' >> Error allocating memory for the Slope array in CubicSplineInit.' )
      RETURN
   ENDIF

   ALLOCATE ( U( AryLen - 1 ), STAT=ErrStatLcL )
   IF ( ErrStatLcL /= 0 )  THEN
      CALL ExitThisRoutine ( ErrID_Fatal, NewLine//' >> Error allocating memory for the U array in CubicSplineInit.' )
      RETURN
   ENDIF

   ALLOCATE ( V( AryLen - 1 ), STAT=ErrStatLcL )
   IF ( ErrStatLcL /= 0 )  THEN
      CALL ExitThisRoutine ( ErrID_Fatal, NewLine//' >> Error allocating memory for the V array in CubicSplineInit.' )
      RETURN
   ENDIF


      ! Compute the distance between XAry values and the slopes between points.

   DO I=1,AryLen-1
      DelX (I) =   XAry(I+1) - XAry(I)
      Slope(I) = ( YAry(I+1) - YAry(I) )/DelX(I)
   END DO ! I


      ! Use Gaussian elimination to solve the tri-diagonal matrix.

   U(1) = 2.0_ReKi*( DelX (2) + DelX (1) )
   V(1) = 6.0_ReKi*( Slope(2) - Slope(1) )

   DO I=2,AryLen-1
      U(I) = 2.0_ReKi*( DelX(I-1) + DelX(I)    ) - DelX(I-1)*DelX(I-1)/U(I-1)
      V(I) = 6.0_ReKi*( Slope(I)  - Slope(I-1) ) - DelX(I-1)*   V(I-1)/U(I-1)
   END DO ! I


      ! Determine the coefficients of the polynomials.

   Coef(:,0) = YAry(:)

   ZHi = 0.0_ReKi

   DO I=AryLen-1,1,-1
      ZLo       = ( V(I) - DelX(I)*ZHi )/U(I)
      Coef(I,1) = Slope(I) - DelX(I)*( ZHi/6.0_ReKi + ZLo/3.0_ReKi )
      Coef(I,2) = 0.5_ReKi*ZLo
      Coef(I,3) = ( ZHi - ZLo )/( 6.0_ReKi*DelX(I) )
      ZHi       = ZLo
   END DO ! I



   CALL ExitThisRoutine ( ErrID_None, 'No Problemo' )

   RETURN

   !=======================================================================
   CONTAINS
   !=======================================================================
      SUBROUTINE ExitThisRoutine ( ErrID, Msg )

         ! This subroutine cleans up the parent routine before exiting.


            ! Argument declarations.

         INTEGER(IntKi), INTENT(IN)       :: ErrID                            ! The error identifier (ErrLev)

         CHARACTER(*),   INTENT(IN)       :: Msg                              ! The error message (ErrMsg)


            ! Local declarations.

         LOGICAL                          :: IsOpen                           ! A flage that indicates if the input unit is still open.


            ! Set error status/message

         ErrStat = ErrID
         ErrMsg  = Msg


            ! Deallocate the Words array if it had been allocated.

         IF ( ALLOCATED( DelX  ) )  DEALLOCATE( DelX  )
         IF ( ALLOCATED( Slope ) )  DEALLOCATE( Slope )
         IF ( ALLOCATED( U     ) )  DEALLOCATE( U     )
         IF ( ALLOCATED( V     ) )  DEALLOCATE( V     )


         RETURN

      END SUBROUTINE ExitThisRoutine ! ( ErrID, Msg )

   END SUBROUTINE CubicSplineInit ! ( AryLen, XAry, YAry, YAry, Coef, ErrStat, ErrMsg )
!=======================================================================
   FUNCTION CubicSplineInterp ( X, AryLen, XAry, YAry, Coef, ErrStat, ErrMsg )


      ! This routine interpolates a pair of arrays using cubic splines to find the function value at X.
      ! One must call CubicSplineInit() first to compute the coefficients of the cubics.
      ! This routine does not require that the XAry be regularly spaced.


      ! Function declaration.

   REAL(ReKi)                   :: CubicSplineInterp                          ! This function.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                     ! Length of the array.

   REAL(ReKi), INTENT(IN)       :: Coef  (AryLen-1,0:3)                       ! The coefficients for the cubic polynomials.
   REAL(ReKi), INTENT(IN)       :: X                                          ! The value we are trying to interpolate for.
   REAL(ReKi), INTENT(IN)       :: XAry (AryLen)                              ! Input array of regularly spaced x values.
   REAL(ReKi), INTENT(IN)       :: YAry (AryLen)                              ! Input array of y values.

   INTEGER(IntKi), INTENT(OUT)  :: ErrStat                                    ! Error status.

   CHARACTER(4096), INTENT(OUT) :: ErrMsg                                     ! Error message.


      ! Local declarations.

   REAL(ReKi)                   :: XOff                                       ! The distance from X to XAry(ILo).

   INTEGER(IntKi)               :: ErrStatLcL                                 ! Local error status.
   INTEGER                      :: ILo                                        ! The index into the array for which X is just above or equal to XAry(ILo).



      ! See if X is withing the range of XAry.  Return the end point if it is not.

   IF ( X <= XAry(1) )  THEN
      CubicSplineInterp = YAry(1)
      RETURN
   ELSEIF ( X >= XAry(AryLen) )  THEN
      CubicSplineInterp = YAry(AryLen)
      RETURN
   ENDIF ! ( X <= XAry(1) )


      ! We are somewhere inside XAry.  Find the segment that bounds X using binary search.

   CALL LocateBin( X, XAry, ILo, AryLen )

   XOff = X - XAry(ILo)

   CubicSplineInterp = Coef(ILo,0) + XOff*( Coef(ILo,1) + XOff*( Coef(ILo,2) + XOff*Coef(ILo,3) ) )


   CALL ExitThisRoutine ( ErrID_None, 'No Problemo' )

   RETURN

   !=======================================================================
   CONTAINS
   !=======================================================================
      SUBROUTINE ExitThisRoutine ( ErrID, Msg )

         ! This subroutine cleans up the parent routine before exiting.


            ! Argument declarations.

         INTEGER(IntKi), INTENT(IN)       :: ErrID                            ! The error identifier (ErrLev)

         CHARACTER(*),   INTENT(IN)       :: Msg                              ! The error message (ErrMsg)


            ! Local declarations.

         LOGICAL                          :: IsOpen                           ! A flage that indicates if the input unit is still open.


            ! Set error status/message

         ErrStat = ErrID
         ErrMsg  = Msg


         RETURN

      END SUBROUTINE ExitThisRoutine ! ( ErrID, Msg )

   END FUNCTION CubicSplineInterp ! ( X, AryLen, XAry, YAry, Coef, ErrStat, ErrMsg )
!=======================================================================
   SUBROUTINE RegCubicSplineInit ( AryLen, XAry, YAry, DelX, Coef, ErrStat, ErrMsg )


      ! This routine calculates the parameters needed to compute a regularly-spaced natural cubic spline.
      ! Natural cubic splines are used in that the curvature at the end points is zero.
      ! It assumes the XAry values are equally spaced for speed.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                     ! Length of the array.

   REAL(ReKi), INTENT(OUT)      :: Coef  (AryLen-1,0:3)                       ! The coefficients for the cubic polynomials.
   REAL(ReKi), INTENT(OUT)      :: DelX                                       ! The distance between the equally spaced points.
   REAL(ReKi), INTENT(IN)       :: XAry  (AryLen)                             ! Input array of x values.
   REAL(ReKi), INTENT(IN)       :: YAry  (AryLen)                             ! Input array of y values.

   INTEGER(IntKi), INTENT(OUT)  :: ErrStat                                    ! Error status.

   CHARACTER(4096), INTENT(OUT) :: ErrMsg                                     ! Error message.


      ! Local declarations.

   REAL(ReKi)                   :: DelX2                                      ! The square of the distance between points.
   REAL(ReKi)                   :: DelX4                                      ! Four times the distance between points.
   REAL(ReKi)                   :: DelX6                                      ! Six times the distance between points.
   REAL(ReKi), ALLOCATABLE      :: Slope (:)                                  ! The AryLen-1 length array of slopes between points.
   REAL(ReKi), ALLOCATABLE      :: U     (:)                                  ! An AryLen-1 length array used in the Gaussian elimination.
   REAL(ReKi), ALLOCATABLE      :: V     (:)                                  ! An AryLen-1 length array used in the Gaussian elimination.
   REAL(ReKi)                   :: ZHi                                        ! A parameter used to calculate the polynomial coefficients.
   REAL(ReKi)                   :: ZLo                                        ! A parameter used to calculate the polynomial coefficients.

   INTEGER(IntKi)               :: ErrStatLcL                                 ! Local error status.
   INTEGER                      :: I                                          ! The index into the arrays.



      ! Allocate the various intermediate arrays.

   ALLOCATE ( Slope( AryLen - 1 ), STAT=ErrStatLcL )
   IF ( ErrStatLcL /= 0 )  THEN
      CALL ExitThisRoutine ( ErrID_Fatal, NewLine//' >> Error allocating memory for the Slope array in RegCubicSplineInit.' )
      RETURN
   ENDIF

   ALLOCATE ( U( AryLen - 1 ), STAT=ErrStatLcL )
   IF ( ErrStatLcL /= 0 )  THEN
      CALL ExitThisRoutine ( ErrID_Fatal, NewLine//' >> Error allocating memory for the U array in RegCubicSplineInit.' )
      RETURN
   ENDIF

   ALLOCATE ( V( AryLen - 1 ), STAT=ErrStatLcL )
   IF ( ErrStatLcL /= 0 )  THEN
      CALL ExitThisRoutine ( ErrID_Fatal, NewLine//' >> Error allocating memory for the V array in RegCubicSplineInit.' )
      RETURN
   ENDIF


      ! Compute the distance between XAry values and the slopes between points.

   DelX  = ( XAry(AryLen) - XAry(1) )/REAL( AryLen-1, ReKi )                   ! Is this more accurate than XAry(2) - XAry(1)?
   DelX2 = DelX*DelX
   DelX4 = 4_ReKI*DelX
   DelX6 = 6_ReKI*DelX

   DO I=1,AryLen-1
      Slope(I) = ( YAry(I+1) - YAry(I) )/DelX
   END DO ! I


      ! Use Gaussian elimination to solve the tri-diagonal matrix.

   U(1) = DelX4
   V(1) = 6.0_ReKi*( Slope(2) - Slope(1) )

   DO I=2,AryLen-1
      U(I) = DelX4 - DelX2/U(I-1)
      V(I) = 6.0_ReKi*( Slope(I) - Slope(I-1) ) - DelX*V(I-1)/U(I-1)
   END DO ! I


      ! Determine the coefficients of the polynomials.

   Coef(:,0) = YAry(1:AryLen-1)

   ZHi = 0.0_ReKi

   DO I=AryLen-1,1,-1
      ZLo       = ( V(I) - DelX*ZHi )/U(I)
      Coef(I,1) = Slope(I) - DelX*( ZHi/6.0_ReKi + ZLo/3.0_ReKi )
      Coef(I,2) = 0.5_ReKi*ZLo
      Coef(I,3) = ( ZHi - ZLo )/DelX6
      ZHi       = ZLo
   END DO ! I


   CALL ExitThisRoutine ( ErrID_None, 'No Problemo' )

   RETURN

   !=======================================================================
   CONTAINS
   !=======================================================================
      SUBROUTINE ExitThisRoutine ( ErrID, Msg )

         ! This subroutine cleans up the parent routine before exiting.


            ! Argument declarations.

         INTEGER(IntKi), INTENT(IN)       :: ErrID                            ! The error identifier (ErrLev)

         CHARACTER(*),   INTENT(IN)       :: Msg                              ! The error message (ErrMsg)


            ! Local declarations.

         LOGICAL                          :: IsOpen                           ! A flage that indicates if the input unit is still open.


            ! Set error status/message

         ErrStat = ErrID
         ErrMsg  = Msg


            ! Deallocate the Words array if it had been allocated.

         IF ( ALLOCATED( Slope ) )  DEALLOCATE( Slope )
         IF ( ALLOCATED( U     ) )  DEALLOCATE( U     )
         IF ( ALLOCATED( V     ) )  DEALLOCATE( V     )


         RETURN

      END SUBROUTINE ExitThisRoutine ! ( ErrID, Msg )

   END SUBROUTINE RegCubicSplineInit ! ( AryLen, XAry, YAry, DelX, Coef, ErrStat, ErrMsg )
!=======================================================================
   FUNCTION RegCubicSplineInterp ( X, AryLen, XAry, YAry, DelX, Coef, ErrStat, ErrMsg )


      ! This routine interpolates a pair of arrays using cubic splines to find the function value at X.
      ! One must call RegCubicSplineInit() first to compute the coefficients of the cubics.
      ! This routine requires that the XAry be regularly spaced, which improves performance.


      ! Function declaration.

   REAL(ReKi)                   :: RegCubicSplineInterp                       ! This function.


      ! Argument declarations:

   INTEGER, INTENT(IN)          :: AryLen                                     ! Length of the array.

   REAL(ReKi), INTENT(IN)       :: Coef  (AryLen-1,0:3)                       ! The coefficients for the cubic polynomials.
   REAL(ReKi), INTENT(IN)       :: DelX                                       ! The distance between X values in XAry.
   REAL(ReKi), INTENT(IN)       :: X                                          ! The value we are trying to interpolate for.
   REAL(ReKi), INTENT(IN)       :: XAry (AryLen)                              ! Input array of regularly spaced x values.
   REAL(ReKi), INTENT(IN)       :: YAry (AryLen)                              ! Input array of y values.

   INTEGER(IntKi), INTENT(OUT)  :: ErrStat                                    ! Error status.

   CHARACTER(4096), INTENT(OUT) :: ErrMsg                                     ! Error message.


      ! Local declarations.

   REAL(ReKi)                   :: XOff                                       ! The distance from X to XAry(ILo).

   INTEGER                      :: ILo                                        ! The index into the array for which X is just above or equal to XAry(ILo).



      ! See if X is withing the range of XAry.  Return the end point if it is not.

   IF ( X <= XAry(1) )  THEN
      RegCubicSplineInterp = YAry(1)
      RETURN
   ELSEIF ( X >= XAry(AryLen) )  THEN
      RegCubicSplineInterp = YAry(AryLen)
      RETURN
   ENDIF ! ( X <= XAry(1) )


      ! We are somewhere inside XAry.  Find the segment that bounds X.

   ILo = INT( ( X - XAry(1) )/DelX ) + 1

   XOff = X - XAry(ILo)

   RegCubicSplineInterp = Coef(ILo,0) + XOff*( Coef(ILo,1) + XOff*( Coef(ILo,2) + XOff*Coef(ILo,3) ) )


   CALL ExitThisRoutine ( ErrID_None, 'No Problemo' )

   RETURN

   !=======================================================================
   CONTAINS
   !=======================================================================
      SUBROUTINE ExitThisRoutine ( ErrID, Msg )

         ! This subroutine cleans up the parent routine before exiting.


            ! Argument declarations.

         INTEGER(IntKi), INTENT(IN)       :: ErrID                            ! The error identifier (ErrLev)

         CHARACTER(*),   INTENT(IN)       :: Msg                              ! The error message (ErrMsg)


            ! Local declarations.

         LOGICAL                          :: IsOpen                           ! A flage that indicates if the input unit is still open.


            ! Set error status/message

         ErrStat = ErrID
         ErrMsg  = Msg


         RETURN

      END SUBROUTINE ExitThisRoutine ! ( ErrID, Msg )

   END FUNCTION RegCubicSplineInterp ! ( X, AryLen, XAry, YAry, DelX, Coef, ErrStat, ErrMsg )
!=======================================================================

END MODULE NWTC_Num_NewRout
