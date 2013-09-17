PROGRAM Test_TestMeshMapping

   USE NWTC_Library
   IMPLICIT NONE   

   TYPE(meshtype) :: mesh1_I, mesh1_O
   TYPE(meshtype) :: mesh2_I, mesh2_O
   
   TYPE(MeshMapType) :: Map_Mod1_Mod2        ! Data for mapping meshes from mod1 to mod2
   TYPE(MeshMapType) :: Map_Mod2_Mod1        ! Data for mapping meshes from mod1 to mod2
   
   REAL(ReKi)     :: Orientation(3,3)
   REAL(ReKi)     :: Angle
   REAL(ReKi), PARAMETER :: Len_Horiz = 4
   REAL(ReKi), PARAMETER :: Len_Vert  = 2
   
   INTEGER :: NNodes, j
   
   INTEGER :: un_mesh1_I,   un_mesh1_O, &
              un_mesh2_I,   un_mesh2_O          ! UNITS for File I/O
   
   INTEGER :: TheseMeshTypes
   
   INTEGER(IntKi) :: ErrStat
   CHARACTER(1024) :: ErrMsg
   
   !integer a(0:3, -1:2)
   !integer, allocatable :: b (:)
   !
   CALL NWTC_Init()
   
   call wrscr( NewLine//'Creating two meshes with siblings and writing to binary files.'//NewLine//NewLine )
        
   
!   TheseMeshTypes = ELEMENT_POINT   
   TheseMeshTypes = ELEMENT_LINE2   
   !print *, 'calculating values: element=', i, ' node=',MeshMap%MapLoads(i)%OtherMesh_Element
   
   ! ..............................................................................................................................   
   ! Create Mesh1: a point mesh with one point
   !   Mesh1_I (input) has loads
   !   Mesh1_O (output) as motions
   ! ..............................................................................................................................   
   
   CALL CreateMod1Mesh( TheseMeshTypes )
      
   !..............................
   
   CALL MeshCopy (     SrcMesh  = mesh1_I &
                     , DestMesh = mesh1_O &
                     , CtrlCode = MESH_SIBLING     &
                     , IOS      = COMPONENT_OUTPUT &
                     , Orientation      = .TRUE.   &
                     , TranslationDisp  = .TRUE.   &
                     , ErrStat  = ErrStat          &
                     , ErrMess  = ErrMsg           )
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))

                     !, TranslationVel   = .TRUE.   &
                     !, RotationVel      = .TRUE.   &
                     !, TranslationAcc   = .TRUE.   &
                     !, RotationAcc      = .TRUE.   &
                     !
         
   ! ..............................................................................................................................   
   ! Create Mesh2: a point mesh with three points
   !   Mesh2_I (input) has motions
   !   Mesh2_O (output) as loads
   ! ..............................................................................................................................   

   CALL CreateMod2Mesh( TheseMeshTypes )

   !..............................
   
   CALL MeshCopy (      SrcMesh  = mesh2_I &
                     , DestMesh = mesh2_O &
                     , CtrlCode = MESH_SIBLING     &
                     , IOS      = COMPONENT_OUTPUT &
                     , Force    = .TRUE.           &
                     , Moment   = .TRUE.           &
                     , ErrStat  = ErrStat          &
                     , ErrMess  = ErrMsg           )
         IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
    
   ! ..............................................................................................................................   
   ! Initialize the data: 
   ! ..............................................................................................................................   

   CALL AllocMapping( Mesh1_O, Mesh2_I, Map_Mod1_Mod2, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))   
   CALL AllocMapping( Mesh2_O, Mesh1_I, Map_Mod2_Mod1, ErrStat, ErrMsg );       IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
   un_mesh1_I = -1
   un_mesh1_O = -1
   un_mesh2_I = -1
   un_mesh2_O = -1  
      
   
   Mesh1_O%TranslationDisp(1,:) =  1.
   Mesh1_O%TranslationDisp(2,:) =  0.
   Mesh1_O%TranslationDisp(3,:) = -2.

   Mesh1_O%TranslationVel = 1.
   !Mesh1_O%RotationVel    = 0.
   !Mesh1_O%TranslationAcc = 0.
   !Mesh1_O%RotationAcc    = 0.
   

   !do j=1,Mesh1_O%NNodes
   !   
   !   Angle = (20)*D2R      
   !   !note this "looks" like the transpose, but isn't
   !   Mesh1_O%Orientation(:,1,j) = (/  1.0,       0.0 ,            0.0 /)
   !   Mesh1_O%Orientation(:,2,j) = (/  0.0, COS(Angle), -1.*SIN(Angle) /)
   !   Mesh1_O%Orientation(:,3,j) = (/  0.0, SIN(Angle),     COS(Angle) /)
   !         
   !   Mesh1_O%TranslationVel(:,j)  = (/ 1., 1.,  0. /)*.5
   !   Mesh1_O%RotationVel(:,j)     = (/ 0., 0.5, 0.5 /)*.5
   !   Mesh1_O%TranslationAcc(:,j)  = (/ 1., 1., 0. /)*.115
   !   Mesh1_O%RotationAcc(:,j)     = (/ 1., 1., 1. /)*.115
   !   
   !end do   
   
   Mesh2_O%Force = 0
   Mesh2_O%Moment       = 0.0
      
   Mesh2_O%Force(3, 1: 9) = 1.         
   Mesh2_O%Force(1, 5)    = 0.5
   do j=10,Mesh2_O%NNodes
      Mesh2_O%Force(1, j) = (.125*(j-9))**5 + Mesh2_O%Force(1, 5)
   end do
   
   
      
   ! ..............................................................................................................................   
   ! Map the outputs to inputs and print results:
   ! ..............................................................................................................................   
   IF (TheseMeshTypes == ELEMENT_POINT) THEN
      CALL Transfer_Point_to_Point( Mesh1_O, Mesh2_I, Map_Mod1_Mod2, ErrStat, ErrMsg );           IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))     
      CALL Transfer_Point_to_Point( Mesh2_O, Mesh1_I, Map_Mod2_Mod1, ErrStat, ErrMsg, Mesh2_I );  IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))   
   ELSEIF (TheseMeshTypes == ELEMENT_LINE2 ) THEN        
      CALL Transfer_Line2_to_Line2( Mesh1_O, Mesh2_I, Map_Mod1_Mod2, ErrStat, ErrMsg );           IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))   
PRINT *, 'Mesh1_O'
call meshprintinfo(CU,Mesh1_O,Mesh1_O%NNodes)      
PRINT *, 'Mesh2_O'
call meshprintinfo(CU,Mesh2_O,Mesh2_O%NNodes)      
      CALL Transfer_Line2_to_Line2( Mesh2_O, Mesh1_I, Map_Mod2_Mod1, ErrStat, ErrMsg, Mesh2_I, Mesh1_O );   IF (ErrStat /= ErrID_None) CALL WrScr("*******"//TRIM(ErrMsg))        
!PRINT *, 'Mesh1_I'
!call meshprintinfo(CU,Mesh1_I)      
!PRINT *, 'Mesh2_I'
!call meshprintinfo(CU,Mesh2_I)      
   END IF

   
   CALL MeshWrBin ( un_mesh1_O, Mesh1_O, ErrStat, ErrMsg, "Mesh1_Output.bin");  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshWrBin ( un_mesh1_I, Mesh2_I, ErrStat, ErrMsg, "Mesh2_Input.bin");   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshWrBin ( un_mesh2_O, Mesh2_O, ErrStat, ErrMsg, "Mesh2_Output.bin");  IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   CALL MeshWrBin ( un_mesh2_I, Mesh1_I, ErrStat, ErrMsg, "Mesh1_Input.bin");   IF (ErrStat /= ErrID_None) CALL WrScr(TRIM(ErrMsg))
   
  
   
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
   close( un_mesh1_I )
   close( un_mesh1_O )
   close( un_mesh2_I )
   close( un_mesh2_O )
   
contains
   ! ..............................................
   subroutine CreateMod1Mesh( TypeOfMesh )
      integer, intent(in) :: TypeOfMesh
      real(reki) :: dx, dz
      integer NumHorizNodes, NumVertNodes, ConnectionNode
      
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
   
   
END PROGRAM

