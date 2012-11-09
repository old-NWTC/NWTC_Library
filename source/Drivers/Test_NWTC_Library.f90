!**********************************************************************************************************************************
!
!  PROGRAM: Test_NWTC_Library
!
!  PURPOSE:  Tests the functionality of the NWTC Subroutine Library using IVF for Windows.
! 
!  TESTED ROUTINES:
!     NWTC_Init
!     WrScr
!     WrScr1
!     Num2LStr: Int2LStr, R2LStr16
!  TESTED DATA/PARAMETERS:
!     QuKi
!     

!**********************************************************************************************************************************

PROGRAM Test_NWTC_Library

    USE NWTC_Library
    IMPLICIT NONE   
    
   

    ! Variables

      ! IVF for Windows hard-coded kinds
    
    REAL(16)   :: Real16Var      ! a 16-byte real variable
    REAL(8)    :: Real8Var       ! an 8-byte real variable
    REAL(4)    :: Real4Var       ! a 4-byte real variable
    
      ! Type-specific Library kinds
    
    REAL(QuKi) :: RealQuVar      ! a 16-byte real variable
    REAL(R8Ki) :: RealR8Var      ! an 8-byte real variable
    REAL(SiKi) :: RealSiVar      ! a 4-byte real variable
    
      ! Default kinds from Library
      
   REAL(ReKi)  :: RealReVar      ! a default real variable (different in SingPrec.f90 or DoubPrec.f90)
   REAL(DbKi)  :: RealDbVar      ! a default double variable (different in SingPrec.f90 or DoubPrec.f90)
    
   !...............................................................................................................................    
   ! Initialize the library
   !...............................................................................................................................    
   
   CALL NWTC_Init(  )
   

   !...............................................................................................................................    
   ! Let's check that the PRECISION kinds are specified correctly:
   !...............................................................................................................................    
   CALL WrScr( 'Real KIND parameters:' )
   CALL WrScr( '  QuKi is '//Num2LStr(QuKi)//'-> It should be 16.' )
   CALL WrScr( '  R8Ki is '//Num2LStr(R8Ki)//'-> It should be  8.' )
   CALL WrScr( '  SiKi is '//Num2LStr(SiKi)//'-> It should be  4.' )
   CALL WrScr1('  ReKi is '//Num2LStr(ReKi) )
   CALL WrScr( '  DbKi is '//Num2LStr(DbKi) )
   
   
   
   !...............................................................................................................................    
   ! Test NWTC_Num routines: EqualRealNos 
   !...............................................................................................................................    
   
   CALL WrScr1( ' Testing EqualRealNos for quad kinds: ')

   Real16Var = 5.0_QuKi
   RealQuVar = Real16Var + 0.00005_QuKi

   print *, Real16Var
   print *, RealQuVar   
   
   IF ( EqualRealNos  ( Real16Var, RealQuVar ) ) THEN
      CALL WrScr( ' '//TRIM(Num2LStr(Real16Var))//' is approximately equal to '//Num2LStr(RealQuVar) )
   ELSE      
      CALL WrScr( ' '//TRIM(Num2LStr(Real16Var))//' is not equal to '//Num2LStr(RealQuVar) )

   END IF   
   
   
   CALL WrScr1( ' Testing EqualRealNos for double kinds: ')
   Real8Var = 5.0_R8Ki
   RealR8Var = Real8Var + 0.00005_R8Ki
     
   print *, Real8Var
   print *, RealR8Var
      
   IF ( EqualRealNos  ( Real8Var, RealR8Var ) ) THEN
      CALL WrScr( ' '//TRIM(Num2LStr(Real8Var))//' is approximately equal to '//Num2LStr(RealR8Var) )
   ELSE      
      CALL WrScr( ' '//TRIM(Num2LStr(Real8Var))//' is not equal to '//Num2LStr(RealR8Var) )

   END IF      
   
   
   CALL WrScr1( ' Testing EqualRealNos for single kinds: ')
   Real4Var = 5.0_SiKi
   RealSiVar = Real8Var + 0.00005_SiKi
     
   print *, Real4Var
   print *, RealSiVar   
   
   IF ( EqualRealNos  ( Real4Var, RealSiVar ) ) THEN
      CALL WrScr( ' '//TRIM(Num2LStr(Real4Var))//' is approximately equal to '//Num2LStr(RealSiVar) )
   ELSE      
      CALL WrScr( ' '//TRIM(Num2LStr(Real4Var))//' is not equal to '//Num2LStr(RealSiVar) )
   END IF  
   
   
   
END PROGRAM Test_NWTC_Library

