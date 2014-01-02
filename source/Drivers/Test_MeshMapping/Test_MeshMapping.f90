PROGRAM Test_TestMeshMapping

   USE NWTC_Library
   IMPLICIT NONE   

   TYPE(meshtype) :: mesh1_I, mesh1_O
   TYPE(meshtype) :: mesh2_I, mesh2_O 
   TYPE(meshtype) :: mesh1_O_1PT, mesh1_I_1PT
   TYPE(meshtype) :: mesh2_O_1PT, mesh2_I_1PT

   
   TYPE(MeshMapType) :: Map_Mod1_Mod2        ! Data for mapping meshes from mod1 to mod2
   TYPE(MeshMapType) :: Map_Mod2_Mod1        ! Data for mapping meshes from mod1 to mod2
   TYPE(MeshMapType) :: Map_Mod1_O_1PT, Map_Mod1_I_1PT
   TYPE(MeshMapType) :: Map_Mod2_O_1PT, Map_Mod2_I_1PT
   
   REAL(ReKi)     :: Orientation(3,3)
   REAL(ReKi)     :: Angle
   
   INTEGER :: NNodes, j
   
   INTEGER :: un_out        ! UNITS for File I/O
   
   INTEGER :: Mesh1Type
   INTEGER :: Mesh2Type
   
   INTEGER(IntKi) :: ErrStat
   CHARACTER(1024) :: ErrMsg   
   
   INTEGER :: TestNumber 
   CHARACTER(256) :: BinOutputName 
   
   CALL NWTC_Init()
   
   !call wrscr( NewLine//'Creating two meshes with siblings and writing to binary files.'//NewLine//NewLine )
   
   DO TestNumber=1,5
      un_out = -1

      print *, '---------------------------------------------------------------'
      print *, '   Test ', TestNumber
      print *, '---------------------------------------------------------------'
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
      ! Create output meshes: 
      !   Mesh1_O (output) has motions
      !   Mesh2_O (output) has loads
      ! ..............................................................................................................................   
      
      ! These subroutines will set the Mesh1Type and Mesh2Type variables, then create the output meshes, set reference  
      ! position/orientation, and set initial outputs for each module (to test mapping to inputs)
      SELECT CASE ( TestNumber )
      CASE(1) ! 1 point to 5 points
         CALL CreateOutputMeshes_Test1()
      CASE(2) ! 'T' with resolution gain
         CALL CreateOutputMeshes_Test2('A')
      CASE(3) ! 'T' with loss of resolution
         CALL CreateOutputMeshes_Test2('B')
      CASE(4) ! 'T' with equal nodes
         CALL CreateOutputMeshes_Test2('C')
      CASE(5)
         CALL CreateOutputMeshes_Test3()
      END SELECT
   
      WRITE(BinOutputName,'(A,I1,A)') 'Test', TestNumber ,'Meshes.bin'
      
      CALL CreateTotalLoadsPointMeshes()
   
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

      CALL AllocMapping( Mesh1_O, Mesh2_I,     Map_Mod1_Mod2, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))   
      CALL AllocMapping( Mesh2_O, Mesh1_I,     Map_Mod2_Mod1, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      
      CALL AllocMapping( Mesh1_I, Mesh1_I_1PT, Map_Mod1_I_1PT,  ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL AllocMapping( Mesh1_O, Mesh1_O_1PT, Map_Mod1_O_1PT,  ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL AllocMapping( Mesh2_I, Mesh2_I_1PT, Map_Mod2_I_1PT,  ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL AllocMapping( Mesh2_O, Mesh2_O_1PT, Map_Mod2_O_1PT,  ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
            
      
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
            if (TestNumber == 5 ) call InitTest3Loads()
            CALL Transfer_Line2_to_Line2( Mesh2_O, Mesh1_I, Map_Mod2_Mod1, ErrStat, ErrMsg, Mesh2_I, Mesh1_O );   IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))        
         ELSEIF ( Mesh2Type == ELEMENT_POINT ) THEN        
            CALL Transfer_Line2_to_Point( Mesh1_O, Mesh2_I, Map_Mod1_Mod2, ErrStat, ErrMsg );                     IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))                  
            CALL Transfer_Point_to_Line2( Mesh2_O, Mesh1_I, Map_Mod2_Mod1, ErrStat, ErrMsg, Mesh2_I, Mesh1_O );   IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))     
         END IF            
      END IF

      
         ! map the loads to a single point to verify the two modules have the same total loads:
      IF (Mesh1Type == ELEMENT_POINT ) THEN
         CALL Transfer_Point_to_Point( Mesh1_O, mesh1_O_1PT, Map_Mod1_O_1PT,ErrStat,ErrMsg);
         CALL Transfer_Point_to_Point( Mesh1_I, Mesh1_I_1PT, Map_Mod1_I_1PT,ErrStat,ErrMsg,mesh1_O,mesh1_O_1PT);         
      ELSE
         CALL Transfer_Line2_to_Point( Mesh1_O, mesh1_O_1PT, Map_Mod1_O_1PT,ErrStat,ErrMsg);
         CALL Transfer_Line2_to_Point( Mesh1_I, Mesh1_I_1PT, Map_Mod1_I_1PT,ErrStat,ErrMsg,mesh1_O,mesh1_O_1PT);         
      END IF
      
      IF (Mesh2Type == ELEMENT_POINT ) THEN
         CALL Transfer_Point_to_Point( mesh2_I, mesh2_I_1PT, Map_Mod2_I_1PT,ErrStat,ErrMsg);
         CALL Transfer_Point_to_Point( Mesh2_O, Mesh2_O_1PT, Map_Mod2_O_1PT,ErrStat,ErrMsg,mesh2_I,mesh2_I_1PT);
      ELSE
         CALL Transfer_Line2_to_Point( mesh2_I, mesh2_I_1PT, Map_Mod2_I_1PT,ErrStat,ErrMsg);
         CALL Transfer_Line2_to_Point( Mesh2_O, Mesh2_O_1PT, Map_Mod2_O_1PT,ErrStat,ErrMsg,mesh2_I,mesh2_I_1PT);         
      END IF
      
      
      
   
   !PRINT *, 'Mesh1_I'
   !call meshprintinfo(CU,Mesh1_I)      
   !PRINT *, 'Mesh2_I'
   !call meshprintinfo(CU,Mesh2_I)       
      
   !PRINT *, 'Mesh1_O'
   !call meshprintinfo(CU,Mesh1_O,Mesh1_O%NNodes)      
   !PRINT *, 'Mesh2_O'
   !call meshprintinfo(CU,Mesh2_O,Mesh2_O%NNodes)      
   
   
   
      CALL MeshWrBin ( un_out, Mesh1_I,     ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL MeshWrBin ( un_out, Mesh1_O,     ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL MeshWrBin ( un_out, Mesh2_I,     ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL MeshWrBin ( un_out, Mesh2_O,     ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL MeshWrBin ( un_out, mesh1_I_1PT, ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL MeshWrBin ( un_out, mesh2_O_1PT, ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))   
      CALL MeshWrBin ( un_out, mesh1_O_1PT, ErrStat, ErrMsg, BinOutputName);  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))   
      CALL MeshMapWrBin( un_out, Mesh1_O, Mesh2_I, Map_Mod1_Mod2, ErrStat, ErrMsg, BinOutputName );  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg)) 
      CALL MeshMapWrBin( un_out, Mesh2_O, Mesh1_I, Map_Mod2_Mod1, ErrStat, ErrMsg, BinOutputName );  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg)) 
      
  
   !PRINT *, 'mesh1_I_1PT:'
   !call meshprintinfo(CU,mesh1_I_1PT,mesh1_I_1PT%NNodes)    
   !PRINT *, 'mesh2_O_1PT:'
   !call meshprintinfo(CU,mesh2_O_1PT,mesh2_O_1PT%NNodes)    
   
      print *, 'Total Force:'
      print *, '     Mesh 1:',mesh1_I_1PT%Force 
      print *, '     Mesh 2:',mesh2_O_1PT%Force 
      print *, 'Total Moment:'
      print *, '     Mesh 1:',mesh1_I_1PT%Moment 
      print *, '     Mesh 2:',mesh2_O_1PT%Moment 
   
      ! ..............................................................................................................................   
      ! Destroy them:
      ! ..............................................................................................................................   

      CALL MeshDestroy( mesh1_I, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL MeshDestroy( mesh1_O, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL MeshDestroy( mesh2_I, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL MeshDestroy( mesh2_O, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))

      call MeshMapDestroy(Map_Mod1_Mod2, ErrStat, ErrMsg);IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      call MeshMapDestroy(Map_Mod2_Mod1, ErrStat, ErrMsg);IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      

      
      CALL MeshDestroy( mesh1_I_1PT, ErrStat, ErrMsg );   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL MeshDestroy( mesh1_O_1PT, ErrStat, ErrMsg );   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL MeshDestroy( mesh2_O_1PT, ErrStat, ErrMsg );   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL MeshDestroy( mesh2_I_1PT, ErrStat, ErrMsg );   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
      call MeshMapDestroy(Map_Mod1_I_1PT, ErrStat, ErrMsg);IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      call MeshMapDestroy(Map_Mod2_O_1PT, ErrStat, ErrMsg);IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      call MeshMapDestroy(Map_Mod1_O_1PT, ErrStat, ErrMsg);IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      call MeshMapDestroy(Map_Mod2_I_1PT, ErrStat, ErrMsg);IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
      ! ..............................................................................................................................   
      ! Close files
      ! ..............................................................................................................................   
      close( un_out )
   
   end do
   
contains
   ! ..............................................  
   subroutine CreateTotalLoadsPointMeshes( )
   
      CALL MeshCreate( BlankMesh       = mesh1_I_1PT        &
                     , IOS              = COMPONENT_INPUT   &
                     , NNodes           = 1                 &
                     , Force            = .TRUE.            &
                     , Moment           = .TRUE.            &
                     , ErrStat          = ErrStat           &
                     , ErrMess          = ErrMsg            )
      
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
         
         
      CALL MeshPositionNode ( mesh1_I_1PT, 1, (/0.0_ReKi, 0.0_ReKi, 0.0_ReKi/), ErrStat, ErrMsg )     
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))             

      CALL MeshConstructElement ( mesh1_I_1PT, ELEMENT_POINT, ErrStat, ErrMsg, 1 )
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))                     
                  
      CALL MeshCommit ( mesh1_I_1PT, ErrStat, ErrMsg )   
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg)) 
      
      
      CALL MeshCopy( mesh1_I_1PT, mesh1_O_1PT, MESH_SIBLING, ErrStat, ErrMsg &
                     , IOS              = COMPONENT_OUTPUT  &
                     , TranslationDisp  = .TRUE.            )
                     
      CALL MeshCopy( mesh1_I_1PT, mesh2_O_1PT, MESH_NEWCOPY, ErrStat, ErrMsg )  ! This thinks it's for input, but really it's for output. I don't think it matters...
      CALL MeshCopy( mesh2_O_1PT, mesh2_I_1PT, MESH_SIBLING, ErrStat, ErrMsg &
                     , IOS              = COMPONENT_INPUT  &
                     , TranslationDisp  = .TRUE.            )

      
      
   end subroutine CreateTotalLoadsPointMeshes
   
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
   ! ..............................................
   subroutine CreateOutputMeshes_Test2(ThisCase)   
      ! this is a line2-to-line2 mapping, with the same meshes in an upside-down "T" shape, though
      ! one is discretized more than the other.
      character(1), intent(in) :: ThisCase
      real(reki) :: dx, dz
      integer NumHorizNodes, NumVertNodes, ConnectionNode
      REAL(ReKi), PARAMETER :: Len_Horiz = 4
      REAL(ReKi), PARAMETER :: Len_Vert  = 2
      type(meshtype) :: rotateMesh
      type(meshmaptype) :: rotateMesh_map

   
      Mesh1Type = ELEMENT_LINE2
      Mesh2Type = ELEMENT_LINE2
      
      !.........................
      ! Mesh1 (Output: Motions)
      !.........................
      SELECT CASE (ThisCase)
      CASE ('A')
         NumHorizNodes  = 9  
         ConnectionNode = 5
         NumVertNodes   = 8
      CASE ('B','C')
         NumHorizNodes  =  5
         ConnectionNode =  3
         NumVertNodes   =  2
      END SELECT
      
            
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
      
      !... temp mesh, for rotation only:
      CALL MeshCreate(BlankMesh  = RotateMesh                 &
                       , IOS              = COMPONENT_OUTPUT  &
                       , NNodes           = 1                 &
                       , Orientation      = .TRUE.            &
                       , TranslationDisp  = .TRUE.            &
                       , ErrStat          = ErrStat           &
                       , ErrMess          = ErrMsg            )
      
      CALL MeshPositionNode(RotateMesh, 1, mesh1_O.Position(:,ConnectionNode), ErrStat=ErrStat, ErrMess=ErrMsg, &
                            Orient=mesh1_O.RefOrientation(:,:,ConnectionNode) )      ; IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL MeshConstructElement ( RotateMesh, ELEMENT_POINT, ErrStat, ErrMsg, 1 )    ; IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL MeshCommit ( RotateMesh, ErrStat, ErrMsg )                                ; IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
      Angle = (15.)*D2R      
      !note this "looks" like the transpose, but isn't
      RotateMesh%Orientation(:,1,1) = (/  COS(Angle) , -1.*SIN(Angle), 0.0 /)
      RotateMesh%Orientation(:,2,1) = (/  SIN(Angle),      COS(Angle), 0.0 /)
      RotateMesh%Orientation(:,3,1) = (/  0.0,         0.0,            1.0 /)
      RotateMesh%TranslationDisp(:,1) = (/ 0.01, 0.01,  0.00 /) !bjj: this needs to be calculated, based on the point-to-point equations...
       
      CALL AllocMapping( RotateMesh, Mesh1_O, rotateMesh_map, ErrStat, ErrMsg) ; IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      CALL Transfer_Point_to_Line2( RotateMesh, Mesh1_O, rotateMesh_map, ErrStat, ErrMsg); IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
      
      call MeshDestroy(rotateMesh,ErrStat,ErrMsg)
      call MeshMapDestroy( rotateMesh_map,ErrStat,ErrMsg)
      
      
      
   !   do j=1,Mesh1_O%NNodes
   !   
   !      !Angle = 0      
   !!      Angle = (20*j)*D2R      
   !      Angle = (15)*D2R      
   !      !note this "looks" like the transpose, but isn't
   !      Mesh1_O%Orientation(:,1,j) = (/  COS(Angle) , -1.*SIN(Angle), 0.0 /)
   !      Mesh1_O%Orientation(:,2,j) = (/  SIN(Angle),      COS(Angle), 0.0 /)
   !      Mesh1_O%Orientation(:,3,j) = (/  0.0,         0.0,            1.0 /)
   !         
   !      Mesh1_O%TranslationVel(:,j)  = 0.0_ReKi ! (/ 1., 1.,  0. /)*.5
   !      Mesh1_O%RotationVel(:,j)     = 0.0_ReKi ! (/ 0., 0.5, 0.5 /)*.5
   !      Mesh1_O%TranslationAcc(:,j)  = 0.0_ReKi ! (/ 1., 1., 0. /)*.115
   !      Mesh1_O%RotationAcc(:,j)     = 0.0_ReKi ! (/ 1., 1., 1. /)*.115
   !
   !      Mesh1_O%TranslationDisp(:,j) = (/ 0.01, 0.01,  0.00 /) !bjj: this needs to be calculated, based on the point-to-point equations...
   !               
   !   end do
                           
      !.........................
      ! Mesh2 (Input: Motions)
      !.........................
                 
      SELECT CASE (ThisCase)
      CASE ('B')
         NumHorizNodes  = 9  
         ConnectionNode = 5
         NumVertNodes   = 8
      CASE ('A','C')
         NumHorizNodes  =  5
         ConnectionNode =  3
         NumVertNodes   =  2
      END SELECT
      
      
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
         Mesh2_O%Force(1, j) = (0.5*Mesh2_O%Position(3,j))**5 + Mesh2_O%Force(1, ConnectionNode)
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
   ! ..............................................
   subroutine CreateOutputMeshes_Test3
   
      real(reki) :: z
      
      
      Mesh1Type = ELEMENT_LINE2
      Mesh2Type = ELEMENT_LINE2
      
      !.........................
      ! Mesh1 (Output: Motions)
      !.........................
      
      Nnodes = 6            
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
         CALL MeshPositionNode ( mesh1_O, j, (/0.0_ReKi, 0.0_ReKi, 1.0_ReKi*(j-1)/(Nnodes-1) /), ErrStat, ErrMsg )     
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))                      
      END DO   


      do j=2,NNodes 
         CALL MeshConstructElement ( mesh1_O, ELEMENT_LINE2, ErrStat, ErrMsg, J-1, j )
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))                     
      end do
      
      
      CALL MeshCommit ( mesh1_O, ErrStat, ErrMsg )   
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg)) 
      if (ErrStat >= AbortErrLev) CALL ProgAbort("Error creating Mesh1 output for test 3.")

      !..............
      ! initialize output fields:
      !..............      
      
      do j=1,Mesh1_O%NNodes
      
         Angle = 0      
   !      Angle = (20*j)*D2R      
         Angle = 0.5*Mesh1_O%Position(3,j)
         
         !note this "looks" like the transpose, but isn't
         Mesh1_O%Orientation(:,1,j) = (/  COS(Angle),   0.0,  1.*SIN(Angle) /)
         Mesh1_O%Orientation(:,2,j) = (/  0.0,          1.0,            0.0 /)
         Mesh1_O%Orientation(:,3,j) = (/-1.*SIN(Angle), 0.0,     COS(Angle) /)
            
         Mesh1_O%TranslationVel(:,j)  = 0.0_ReKi ! (/ 1., 1.,  0. /)*.5
         Mesh1_O%RotationVel(:,j)     = 0.0_ReKi ! (/ 0., 0.5, 0.5 /)*.5
         Mesh1_O%TranslationAcc(:,j)  = 0.0_ReKi ! (/ 1., 1., 0. /)*.115
         Mesh1_O%RotationAcc(:,j)     = 0.0_ReKi ! (/ 1., 1., 1. /)*.115
         
         Mesh1_O%TranslationDisp(:,j) = (/ 2.*(1.0-COS(Angle)), 0., 2.0*SIN(Angle)-Mesh1_O%Position(3,j) /)
      
      end do
      
               
      !.........................
      ! Mesh2 (Input: Motions)
      !.........................
                 
      NNodes = 8
   
      CALL MeshCreate(  BlankMesh       = mesh2_O           &
                        , IOS           = COMPONENT_OUTPUT  &
                        , NNodes        = NNodes            &
                        , Force         = .TRUE.            &
                        , Moment        = .TRUE.            &
                        , ErrStat       = ErrStat           &
                        , ErrMess       = ErrMsg            )   
            IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))

      do j=1,NNodes
                  
         !Angle = (-25. + j*j)*D2R  !note this "looks" like the transpose, but isn't
         Angle = 30.*D2R
         Orientation(:,1) = (/    COS(Angle), 0.0,  1.*SIN(Angle) /)
         Orientation(:,2) = (/    0.0,        1.0,            0.0 /)
         Orientation(:,3) = (/-1.*SIN(Angle), 0.0,     COS(Angle) /)
      
            ! place nodes in a line
         z = (j-1.0)/(NNodes-1.0)
         CALL MeshPositionNode ( mesh2_O, j, (/(sqrt(3.0)/3.0)*z-(sqrt(3.0)/6.0), 0.0_ReKi, z /), ErrStat, ErrMsg, &
               Orient= Orientation )     
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))        

      END DO
      
      do j=2,NNodes 
         CALL MeshConstructElement ( mesh2_O, ELEMENT_LINE2, ErrStat, ErrMsg, J-1, j )
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))                     
      end do      
                  
         ! that's our entire mesh:
      CALL MeshCommit ( mesh2_O, ErrStat, ErrMsg )   
      IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))   
      if (ErrStat >= AbortErrLev) CALL ProgAbort("Error creating Mesh2 output for test 1.")
         
      !..............
      ! initialize output fields:
      !..............
      ! This is done in another subroutine
      
      !do j=1,Mesh2_O%NNodes
      !   Mesh2_I%Orientation(:,:,j)
      !   
      !   Mesh2_O%Force( :,j) = Mesh2_I%Orientation(:,1,j)*5
      !   Mesh2_O%Moment(:,j) = 0.0 ! (/  0.0, 0.0, 0.5  /)*(-j*0.0)
      !end do
      

                  
   end subroutine CreateOutputMeshes_Test3
   subroutine InitTest3Loads()
      !..............
      ! initialize output fields:
      !..............
      do j=1,Mesh2_O%NNodes         
         Mesh2_O%Force( :,j) = Mesh2_I%Orientation(1,:,j)*1.25
         Mesh2_O%Moment(:,j) = 0.0 ! (/  0.0, 0.0, 0.5  /)*(-j*0.0)
      end do   
   end subroutine InitTest3Loads
   
   ! ..............................................


   
END PROGRAM

