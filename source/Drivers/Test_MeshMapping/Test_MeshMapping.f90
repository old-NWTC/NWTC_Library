PROGRAM Test_TestMeshMapping

   USE NWTC_Library
   IMPLICIT NONE   

   TYPE(meshtype) :: mesh1_I, mesh1_O
   TYPE(meshtype) :: mesh2_I, mesh2_O
   
   TYPE(MeshMapType) :: Map_Mod1_Mod2        ! Data for mapping meshes from mod1 to mod2
   TYPE(MeshMapType) :: Map_Mod2_Mod1        ! Data for mapping meshes from mod1 to mod2
   
   REAL(ReKi)     :: Orientation(3,3)
   REAL(ReKi)     :: Angle
   
   INTEGER :: NNodes, j
   
   INTEGER :: un_out        ! UNITS for File I/O
   
   INTEGER :: Mesh1Type
   INTEGER :: Mesh2Type
   
   INTEGER(IntKi) :: ErrStat
   CHARACTER(1024) :: ErrMsg   
   
   INTEGER :: TestNumber = 2
   CHARACTER(256) :: BinOutputName 
   
   CALL NWTC_Init()
   
   call wrscr( NewLine//'Creating two meshes with siblings and writing to binary files.'//NewLine//NewLine )
   un_out = -1

   ! ..............................................................................................................................   
   ! Mesh1 fields:
   !   Mesh1_I (input) has loads
   !   Mesh1_O (output) has motions
   ! ..............................................................................................................................   

   ! ..............................................................................................................................   
   ! Mesh2 fields:
   !   Mesh2_I (input) has motions
   !   Mesh2_O (output) has loads
   ! ..............................................................................................................................   
      
   
   
   ! ..............................................................................................................................   
   ! Create input meshes: 
   !   Mesh1_O (output) has motions
   !   Mesh2_O (output) has loads
   ! ..............................................................................................................................   
      
   ! These subroutines will set the Mesh1Type and Mesh2Type variables, then create the output meshes, set reference  
   ! position/orientation, and set initial outputs for each module (to test mapping to inputs)
   SELECT CASE ( TestNumber )
   CASE(1) ! 1 point to 5 points
      CALL CreateOutputMeshes_Test1()
   CASE(2) ! 1 point to 5 points
      CALL CreateOutputMeshes_Test2()
   END SELECT
   
   WRITE(BinOutputName,'(A,I1,A)') 'Test', TestNumber ,'Meshes.bin'
      
   ! ..............................................................................................................................   
   ! Create sibling input meshes: 
   !   Mesh1_I (input) has loads
   !   Mesh2_I (input) has motions
   ! ..............................................................................................................................   
   
   
   CALL MeshCopy (        SrcMesh      = mesh1_O                &
                        , DestMesh     = mesh1_I                &
                        , CtrlCode     = MESH_SIBLING           &
                        , IOS          = COMPONENT_INPUT        &
                        ,Force         = .TRUE.                 &
                        ,Moment        = .TRUE.                 &
                        ,ErrStat       = ErrStat                &
                        ,ErrMess       = ErrMsg                 )   
            IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
   !..............................
   
   CALL MeshCopy (        SrcMesh          = mesh2_O          &
                        , DestMesh         = mesh2_I          &
                        , CtrlCode         = MESH_SIBLING     &
                        , IOS              = COMPONENT_INPUT  &
                        , Orientation      = .TRUE.           &
                        , TranslationDisp  = .TRUE.           &
                        , TranslationVel   = .TRUE.           &
                        , RotationVel      = .TRUE.           &
                        , TranslationAcc   = .TRUE.           &
                        , RotationAcc      = .TRUE.           &
                        , ErrStat          = ErrStat          &
                        , ErrMess          = ErrMsg           )
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
    
   ! ..............................................................................................................................   
   ! Initialize the mapping data: 
   ! ..............................................................................................................................   

   CALL AllocMapping( Mesh1_O, Mesh2_I, Map_Mod1_Mod2, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))   
   CALL AllocMapping( Mesh2_O, Mesh1_I, Map_Mod2_Mod1, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
            
      
   ! ..............................................................................................................................   
   ! Map the outputs to inputs and print results:
   ! ..............................................................................................................................   
   IF (Mesh1Type == ELEMENT_POINT ) THEN
      IF ( Mesh2Type == ELEMENT_POINT ) THEN
         CALL Transfer_Point_to_Point( Mesh1_O, Mesh2_I, Map_Mod1_Mod2, ErrStat, ErrMsg );                     IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))     
         CALL Transfer_Point_to_Point( Mesh2_O, Mesh1_I, Map_Mod2_Mod1, ErrStat, ErrMsg, Mesh2_I, Mesh1_O );   IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))   
      ELSEIF ( Mesh2Type == ELEMENT_LINE2) THEN                                                                
         CALL Transfer_Point_to_Line2( Mesh1_O, Mesh2_I, Map_Mod1_Mod2, ErrStat, ErrMsg );                     IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))     
         CALL Transfer_Line2_to_Point( Mesh2_O, Mesh1_I, Map_Mod2_Mod1, ErrStat, ErrMsg, Mesh2_I, Mesh1_O );   IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))         
      END IF                                                                                                   
   ELSEIF ( Mesh1Type == ELEMENT_LINE2 ) THEN                                                                  
      IF ( Mesh2Type == ELEMENT_LINE2 ) THEN                                                                   
         CALL Transfer_Line2_to_Line2( Mesh1_O, Mesh2_I, Map_Mod1_Mod2, ErrStat, ErrMsg );                     IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))   
         CALL Transfer_Line2_to_Line2( Mesh2_O, Mesh1_I, Map_Mod2_Mod1, ErrStat, ErrMsg, Mesh2_I, Mesh1_O );   IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))        
      ELSEIF ( Mesh2Type == ELEMENT_POINT ) THEN        
         CALL Transfer_Line2_to_Point( Mesh1_O, Mesh2_I, Map_Mod1_Mod2, ErrStat, ErrMsg );                     IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))                  
         CALL Transfer_Point_to_Line2( Mesh2_O, Mesh1_I, Map_Mod2_Mod1, ErrStat, ErrMsg, Mesh2_I, Mesh1_O );   IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))     
      END IF      
   END IF

!PRINT *, 'Mesh1_I'
!call meshprintinfo(CU,Mesh1_I)      
!PRINT *, 'Mesh2_I'
!call meshprintinfo(CU,Mesh2_I)       
      
!PRINT *, 'Mesh1_O'
!call meshprintinfo(CU,Mesh1_O,Mesh1_O%NNodes)      
!PRINT *, 'Mesh2_O'
!call meshprintinfo(CU,Mesh2_O,Mesh2_O%NNodes)      
   
   
   
   CALL MeshWrBin ( un_out, Mesh1_I, ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshWrBin ( un_out, Mesh1_O, ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshWrBin ( un_out, Mesh2_I, ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshWrBin ( un_out, Mesh2_O, ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
  
PRINT *, 'Mesh1_O'
call meshprintinfo(CU,Mesh1_O,Mesh1_O%NNodes)    
PRINT *, 'Mesh2_O'
call meshprintinfo(CU,Mesh2_O,Mesh2_O%NNodes)    
   
   ! ..............................................................................................................................   
   ! Destroy them:
   ! ..............................................................................................................................   

   CALL MeshDestroy( mesh1_I, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshDestroy( mesh1_O, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshDestroy( mesh2_I, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshDestroy( mesh2_O, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
   call MeshMapDestroy(Map_Mod1_Mod2, ErrStat, ErrMsg);IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   call MeshMapDestroy(Map_Mod2_Mod1, ErrStat, ErrMsg);IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   ! ..............................................................................................................................   
   ! Close files
   ! ..............................................................................................................................   
   close( un_out )
   
contains
   ! ..............................................
   subroutine CreateMod1Mesh( TypeOfMesh )
      integer, intent(in) :: TypeOfMesh
      real(reki) :: dx, dz
      integer NumHorizNodes, NumVertNodes, ConnectionNode
      REAL(ReKi), PARAMETER :: Len_Horiz = 4
      REAL(ReKi), PARAMETER :: Len_Vert  = 2

      
      NumHorizNodes  = 5
      ConnectionNode = 3
      NumVertNodes   = 2
      
      NumHorizNodes  = 9  
      ConnectionNode = 5
      NumVertNodes   = 8
      
      
      Nnodes = NumHorizNodes + NumVertNodes !7 
      dx = Len_Horiz/(NumHorizNodes-1) !1.0
      dz = Len_Vert/(NumVertNodes) !1.0
   
      CALL MeshCreate( BlankMesh       = mesh1_I       &
                        ,IOS           = COMPONENT_INPUT        &
                        ,NNodes        = NNodes                 &
                        ,Force         = .TRUE.                 &
                        ,Moment        = .TRUE.                 &
                        ,ErrStat       = ErrStat                &
                        ,ErrMess       = ErrMsg                 )   
            IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))

            ! create a "T"
      ! horizontal line:            
      DO j=1,NumHorizNodes 
         CALL MeshPositionNode ( mesh1_I, j, (/dx*(j-ConnectionNode), 0.0_ReKi, 0.0_ReKi  /), ErrStat, ErrMsg )     
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))             
      END DO   
      
      
      ! vertical line:
      DO j=(NumHorizNodes+1),Nnodes
         CALL MeshPositionNode ( mesh1_I, j, (/0.0_ReKi, 0.0_ReKi, dz*(j-NumHorizNodes)  /), ErrStat, ErrMsg )     
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))             
      END DO   

      
      
      IF ( TypeOfMesh == ELEMENT_POINT ) THEN
         
         do j=1,NNodes
               ! create an element from this point   
      
            CALL MeshConstructElement ( mesh1_I, ELEMENT_POINT, ErrStat, ErrMsg, j )
            IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
         END DO
         
      ELSEIF ( TypeOfMesh == ELEMENT_LINE2 ) THEN
         
         ! horizontal line:
         do j=2,NumHorizNodes
            CALL MeshConstructElement ( mesh1_I, ELEMENT_LINE2, ErrStat, ErrMsg, P1=j-1, P2=J )
            IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
         END DO
         
         ! vertical line:
         CALL MeshConstructElement ( mesh1_I, ELEMENT_LINE2, ErrStat, ErrMsg, P1=ConnectionNode, P2=NumHorizNodes+1 )
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
         
         DO j=(NumHorizNodes+2),Nnodes      
            CALL MeshConstructElement ( mesh1_I, ELEMENT_LINE2, ErrStat, ErrMsg, P1=j-1, P2=j )
            IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
         END DO
                              
      END IF      
      
         ! that's our entire mesh:
      CALL MeshCommit ( mesh1_I, ErrStat, ErrMsg )   
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg)) 
      if (ErrStat >= AbortErrLev) CALL ProgAbort("")
         
   end subroutine CreateMod1Mesh
   ! ..............................................
   subroutine CreateMod2Mesh( TypeOfMesh )
      integer, intent(in) :: TypeOfMesh
      real(reki) :: dx, dz
      integer NumHorizNodes, NumVertNodes, ConnectionNode
      REAL(ReKi), PARAMETER :: Len_Horiz = 4
      REAL(ReKi), PARAMETER :: Len_Vert  = 2
      
      
      NumHorizNodes  = 9  
      ConnectionNode = 5
      NumVertNodes   = 8
      dx = Len_Horiz/(NumHorizNodes-1) !1/2
      dz = Len_Vert/(NumVertNodes) !1/4
      
      Nnodes = NumHorizNodes + NumVertNodes !17 
               
      CALL MeshCreate( BlankMesh       = mesh2_I           &
                        ,IOS           = COMPONENT_INPUT   &
                        ,NNodes        = NNodes            &
                        , Orientation      = .TRUE.  &
                        , TranslationDisp  = .TRUE.  &
                        , TranslationVel   = .TRUE.  &
                        , RotationVel      = .TRUE.  &
                        , TranslationAcc   = .TRUE.  &
                        , RotationAcc      = .TRUE.  &
                        ,ErrStat       = ErrStat                &
                        ,ErrMess       = ErrMsg                 )   
            IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))

            
            ! create a "T"
      ! horizontal line:            
      DO j=1,NumHorizNodes 
         CALL MeshPositionNode ( mesh2_I, j, (/dx*(j-ConnectionNode), 0.0_ReKi, 0.0_ReKi  /), ErrStat, ErrMsg )     
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))             
      END DO   
      
      
      ! vertical line:
      DO j=(NumHorizNodes+1),Nnodes
         CALL MeshPositionNode ( mesh2_I, j, (/0.0_ReKi, 0.0_ReKi, dz*(j-NumHorizNodes)  /), ErrStat, ErrMsg )     
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))             
      END DO               
            
            
      !      ! create a "T"
      !! horizontal line:            
      !DO j=1,9 
      !   CALL MeshPositionNode ( mesh2_I, j, (/dx*(j-5), 0.0_ReKi, 0.0_ReKi  /), ErrStat, ErrMsg )     
      !   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))             
      !END DO   
      !
      !! vertical line:
      !DO j=10,Nnodes 
      !   CALL MeshPositionNode ( mesh2_I, j, (/0.0_ReKi, 0.0_ReKi, dz*(j-9)  /), ErrStat, ErrMsg )     
      !   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))             
      !END DO                                                   
      
      
      !do j=1,NNodes
      !            
      !   !Angle = (-25. + j*j)*D2R  !note this "looks" like the transpose, but isn't
      !   Angle = 0  !note this "looks" like the transpose, but isn't
      !   Orientation(:,1) = (/ COS(Angle), -1.*SIN(Angle), 0.0 /)
      !   Orientation(:,2) = (/ SIN(Angle),     COS(Angle), 0.0 /)
      !   Orientation(:,3) = (/      0.,        0.0,        1.0 /)
      !
      !   
      !   
      !   
      !      ! place nodes in a line
      !   CALL MeshPositionNode ( mesh2_I, j, (/0.0_ReKi, 0.0_ReKi, 0.25_ReKi*(j-1) /), ErrStat, ErrMsg, &
      !         Orient= Orientation )     
      !   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      !
      !
      !END DO
   
      IF ( TypeOfMesh == ELEMENT_POINT ) THEN
         
         do j=1,NNodes
               ! create an element from this point   
      
            CALL MeshConstructElement ( mesh2_I, ELEMENT_POINT, ErrStat, ErrMsg, j )
            IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
         END DO
         
      ELSEIF ( TypeOfMesh == ELEMENT_LINE2 ) THEN
                      
         
         ! horizontal line:
         do j=2,NumHorizNodes
            CALL MeshConstructElement ( mesh2_I, ELEMENT_LINE2, ErrStat, ErrMsg, P1=j-1, P2=J )
            IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
         END DO
         
         ! vertical line:
         CALL MeshConstructElement ( mesh2_I, ELEMENT_LINE2, ErrStat, ErrMsg, P1=ConnectionNode, P2=NumHorizNodes+1 )
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
         
         DO j=(NumHorizNodes+2),Nnodes      
            CALL MeshConstructElement ( mesh2_I, ELEMENT_LINE2, ErrStat, ErrMsg, P1=j-1, P2=j )
            IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
         END DO
         

         !! horizontal line:
         !DO j=2,9
         !   CALL MeshConstructElement ( mesh2_I, ELEMENT_LINE2, ErrStat, ErrMsg, P1=j-1, P2=J )
         !   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
         !END DO
         !
         !! vertical line:
         !CALL MeshConstructElement ( mesh2_I, ELEMENT_LINE2, ErrStat, ErrMsg, P1=5, P2=10 )
         !IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
         !
         !DO j=11,Nnodes
         !   CALL MeshConstructElement ( mesh2_I, ELEMENT_LINE2, ErrStat, ErrMsg, P1=j-1, P2=J )
         !   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
         !END DO                                     
                                    
      END IF
                  
         ! that's our entire mesh:
      CALL MeshCommit ( mesh2_I, ErrStat, ErrMsg )   
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))   
      if (ErrStat >= AbortErrLev) CALL ProgAbort("")
            
   end subroutine CreateMod2Mesh
   ! ..............................................
   
   
   subroutine CreateOutputMeshes_Test1()   
      ! this is a point-to-point mapping, with one point going to many.
      ! it is a figure in the AIAA paper.
   
      Mesh1Type = ELEMENT_POINT
      Mesh2Type = ELEMENT_POINT
      
      !.........................
      ! Mesh1 (Output: Motions)
      !.........................
      
      Nnodes = 1
            
      CALL MeshCreate( BlankMesh          = mesh1_O           &
                       , IOS              = COMPONENT_OUTPUT  &
                       , NNodes           = NNodes            &
                       , Orientation      = .TRUE.            &
                       , TranslationDisp  = .TRUE.            &
                       , TranslationVel   = .TRUE.            &
                       , RotationVel      = .TRUE.            &
                       , TranslationAcc   = .TRUE.            &
                       , RotationAcc      = .TRUE.            &                                
                       , ErrStat          = ErrStat           &
                       , ErrMess          = ErrMsg            )
      
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))

                                    
      do j=1,NNodes 
            ! place nodes in a line
         CALL MeshPositionNode ( mesh1_O, j, (/0.0_ReKi, 0.0_ReKi, 1.0_ReKi*(j-1) /), ErrStat, ErrMsg )     
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))             

         CALL MeshConstructElement ( mesh1_O, ELEMENT_POINT, ErrStat, ErrMsg, j )
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))                     
         
      END DO   
         
      CALL MeshCommit ( mesh1_O, ErrStat, ErrMsg )   
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg)) 
      if (ErrStat >= AbortErrLev) CALL ProgAbort("Error creating Mesh1 output for test 1.")

      !..............
      ! initialize output fields:
      !..............      
      
      do j=1,Mesh1_O%NNodes
      
   !      Angle = 0      
   !      Angle = (20*j)*D2R      
         Angle = (20)*D2R      
         !note this "looks" like the transpose, but isn't
         Mesh1_O%Orientation(:,1,j) = (/  1.0,       0.0 ,            0.0 /)
         Mesh1_O%Orientation(:,2,j) = (/  0.0, COS(Angle), -1.*SIN(Angle) /)
         Mesh1_O%Orientation(:,3,j) = (/  0.0, SIN(Angle),     COS(Angle) /)
            
         Mesh1_O%TranslationVel(:,j)  = 0.0_ReKi ! (/ 1., 1.,  0. /)*.5
         Mesh1_O%RotationVel(:,j)     = 0.0_ReKi ! (/ 0., 0.5, 0.5 /)*.5
         Mesh1_O%TranslationAcc(:,j)  = 0.0_ReKi ! (/ 1., 1., 0. /)*.115
         Mesh1_O%RotationAcc(:,j)     = 0.0_ReKi ! (/ 1., 1., 1. /)*.115
      
      end do
      
         Mesh1_O%TranslationDisp(:,1) = (/ 2., 0.,  0. /)
      !if (Mesh1_O%NNodes > 1) &
      !   Mesh1_O%TranslationDisp(:,2) = (/ 2., -SIN(angle),  COS(ANGLE)-1. /)
      
               
      !.........................
      ! Mesh2 (Input: Motions)
      !.........................
            
        
      NNodes = 5
   
      CALL MeshCreate(  BlankMesh       = mesh2_O           &
                        , IOS           = COMPONENT_OUTPUT  &
                        , NNodes        = NNodes            &
                        , Force         = .TRUE.            &
                        , Moment        = .TRUE.            &
                        , ErrStat       = ErrStat           &
                        , ErrMess       = ErrMsg            )   
            IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))

      do j=1,NNodes
                  
         Angle = (-25. + j*j)*D2R  !note this "looks" like the transpose, but isn't
         Orientation(:,1) = (/ COS(Angle), -1.*SIN(Angle), 0.0 /)
         Orientation(:,2) = (/ SIN(Angle),     COS(Angle), 0.0 /)
         Orientation(:,3) = (/      0.,        0.0,        1.0 /)
      
            ! place nodes in a line
         CALL MeshPositionNode ( mesh2_O, j, (/0.0_ReKi, 0.0_ReKi, 0.25_ReKi*(j-1) /), ErrStat, ErrMsg, &
               Orient= Orientation )     
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
         CALL MeshConstructElement ( mesh2_O, ELEMENT_POINT, ErrStat, ErrMsg, j )
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
         

      END DO
                  
         ! that's our entire mesh:
      CALL MeshCommit ( mesh2_O, ErrStat, ErrMsg )   
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))   
      if (ErrStat >= AbortErrLev) CALL ProgAbort("Error creating Mesh2 output for test 1.")
         
      !..............
      ! initialize output fields:
      !..............
      do j=1,Mesh2_O%NNodes
         Mesh2_O%Force( :,j) = (/  1.0, 0.,  0.   /)  !*(j*0.5)
         Mesh2_O%Moment(:,j) = (/  0.0, 0.5, 0.5  /)*(-j*0.0)
      end do

                  
   end subroutine CreateOutputMeshes_Test1
         
   
   subroutine CreateOutputMeshes_Test2()   
      ! this is a line2-to-line2 mapping, with the same meshes in an upside-down "T" shape, though
      ! one is discretized more than the other.
      
      real(reki) :: dx, dz
      integer NumHorizNodes, NumVertNodes, ConnectionNode
      REAL(ReKi), PARAMETER :: Len_Horiz = 4
      REAL(ReKi), PARAMETER :: Len_Vert  = 2

   
      Mesh1Type = ELEMENT_LINE2
      Mesh2Type = ELEMENT_LINE2
      
      !.........................
      ! Mesh1 (Output: Motions)
      !.........................
                  
      NumHorizNodes  = 3  ! 5
      ConnectionNode = 2  ! 3
      NumVertNodes   = 1  ! 2
            
      Nnodes = NumHorizNodes + NumVertNodes !7 
      dx = Len_Horiz/(NumHorizNodes-1) !1.0
      dz = Len_Vert/(NumVertNodes) !1.0
   
      CALL MeshCreate( BlankMesh          = mesh1_O           &
                       , IOS              = COMPONENT_OUTPUT  &
                       , NNodes           = NNodes            &
                       , Orientation      = .TRUE.            &
                       , TranslationDisp  = .TRUE.            &
                       , TranslationVel   = .TRUE.            &
                       , RotationVel      = .TRUE.            &
                       , TranslationAcc   = .TRUE.            &
                       , RotationAcc      = .TRUE.            &                                
                       , ErrStat          = ErrStat           &
                       , ErrMess          = ErrMsg            )
      
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      

      CALL PositionNodesElements(mesh1_O, NumHorizNodes, ConnectionNode, dx,dz,ErrStat,ErrMsg)                 
      if (ErrStat >= AbortErrLev) CALL ProgAbort("Error creating Mesh1 output for test 2.")

      !..............
      ! initialize Mesh1 output fields:
      !..............      
      
      do j=1,Mesh1_O%NNodes
      
         Angle = 0      
   !      Angle = (20*j)*D2R      
         !Angle = (20)*D2R      
         !note this "looks" like the transpose, but isn't
         Mesh1_O%Orientation(:,1,j) = (/  1.0,       0.0 ,            0.0 /)
         Mesh1_O%Orientation(:,2,j) = (/  0.0, COS(Angle), -1.*SIN(Angle) /)
         Mesh1_O%Orientation(:,3,j) = (/  0.0, SIN(Angle),     COS(Angle) /)
            
         Mesh1_O%TranslationVel(:,j)  = 0.0_ReKi ! (/ 1., 1.,  0. /)*.5
         Mesh1_O%RotationVel(:,j)     = 0.0_ReKi ! (/ 0., 0.5, 0.5 /)*.5
         Mesh1_O%TranslationAcc(:,j)  = 0.0_ReKi ! (/ 1., 1., 0. /)*.115
         Mesh1_O%RotationAcc(:,j)     = 0.0_ReKi ! (/ 1., 1., 1. /)*.115

         Mesh1_O%TranslationDisp(:,j) = 0.0_ReKi !(/ 2., 1.,  1. /)
                  
      end do
                           
      !.........................
      ! Mesh2 (Input: Motions)
      !.........................
                                                      
      !NumHorizNodes  = 9  
      !ConnectionNode = 5
      !NumVertNodes   = 8
      
      dx = Len_Horiz/(NumHorizNodes-1) !1/2
      dz = Len_Vert/(NumVertNodes) !1/4
      
      Nnodes = NumHorizNodes + NumVertNodes !17 
               
      CALL MeshCreate(  BlankMesh       = mesh2_O           &
                        , IOS           = COMPONENT_OUTPUT  &
                        , NNodes        = NNodes            &
                        , Force         = .TRUE.            &
                        , Moment        = .TRUE.            &
                        , ErrStat       = ErrStat           &
                        , ErrMess       = ErrMsg            )   
            IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
            
      CALL PositionNodesElements(mesh2_O, NumHorizNodes, ConnectionNode, dx,dz,ErrStat,ErrMsg)                 
      if (ErrStat >= AbortErrLev) CALL ProgAbort("Error creating Mesh2 output for test 2.")
                                                                   
      !..............
      ! initialize Mesh2 output fields:
      !..............
      
      Mesh2_O%Force    = 0.0
      Mesh2_O%Moment   = 0.0
      
      Mesh2_O%Force(3, 1: NumHorizNodes) = 1.         
      Mesh2_O%Force(1, ConnectionNode)   = 0.5
      do j=NumHorizNodes+1,Mesh2_O%NNodes
         Mesh2_O%Force(1, j) = (.125*(j-NumHorizNodes))**5 + Mesh2_O%Force(1, ConnectionNode)
      end do
                              
      
      return
   end subroutine CreateOutputMeshes_Test2   
   !-------------------------------------------------------------------------------------------------
   subroutine PositionNodesElements(ThisMesh, NumHorizNodes, ConnectionNode, dx,dz,ErrStat,ErrMsg)
   ! this positions nodes, creates elements, and committs the mesh for Test2
   
      TYPE(MeshType), INTENT(INOUT) :: ThisMesh
      real(reki),     INTENT(IN)    :: dx, dz
      integer,        INTENT(IN)    :: NumHorizNodes, ConnectionNode
      INTEGER(IntKi), INTENT(OUT)   :: ErrStat      
      character(*),   INTENT(OUT)   :: ErrMsg      
      
      integer :: j
      
            ! create a "T"
            
      ! position nodes:
            
            
      ! horizontal line:            
      DO j=1,NumHorizNodes 
         CALL MeshPositionNode ( ThisMesh, j, (/dx*(j-ConnectionNode), 0.0_ReKi, 0.0_ReKi  /), ErrStat, ErrMsg )     
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))             
      END DO   
      
      
      ! vertical line:
      DO j=(NumHorizNodes+1),ThisMesh%Nnodes
         CALL MeshPositionNode ( ThisMesh, j, (/0.0_ReKi, 0.0_ReKi, dz*(j-NumHorizNodes)  /), ErrStat, ErrMsg )     
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))             
      END DO                  
   
         ! construct elements:

        
      ! horizontal line:
      do j=2,NumHorizNodes
         CALL MeshConstructElement ( ThisMesh, ELEMENT_LINE2, ErrStat, ErrMsg, P1=j-1, P2=J )
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
      END DO
         
      ! vertical line:
      CALL MeshConstructElement ( ThisMesh, ELEMENT_LINE2, ErrStat, ErrMsg, P1=ConnectionNode, P2=NumHorizNodes+1 )
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
         
      DO j=(NumHorizNodes+2),ThisMesh%Nnodes      
         CALL MeshConstructElement ( ThisMesh, ELEMENT_LINE2, ErrStat, ErrMsg, P1=j-1, P2=j )
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))            
      END DO
                                    
         ! that's our entire mesh:
      CALL MeshCommit ( ThisMesh, ErrStat, ErrMsg )   
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg)) 
      if (ErrStat >= AbortErrLev) CALL ProgAbort("")        
               
   end subroutine PositionNodesElements   
   !-------------------------------------------------------------------------------------------------
   
   
END PROGRAM

