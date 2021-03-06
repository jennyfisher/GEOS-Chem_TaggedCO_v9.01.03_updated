#if defined( DEVEL )
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: gc_environment_mod
!
! !DESCRIPTION: Module GC\_ENVIRONMENT\_MOD establishes the runtime environment
! for the GEOS-Chem model. It is designed to receive model parameter and geophys.
! environment information and allocate memory based upon it.
!
! It provides routines to do the following:
! (1) Allocate geo-spatial arrays
! (2) Initialize met. field derived type.
! (3) Initialize CHEM, PHYS, and EMISSIONS states
! (4) ...
!\\
!\\
!  NOTE: This is mostly for testing the grid-independent code in the current 
!  GEOS-Chem.  Many of these inputs will come from the GEOS-5 interface. It will
!  remain in DEVEL state for some time.
!\\
!\\
! !INTERFACE: 
!
MODULE GC_ENVIRONMENT_MOD
!
! !USES
!        
  USE GC_TYPE_MOD                  ! Various derived type definitions

  IMPLICIT NONE
# include "define.h"

  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: ALLOCATE_ALL
  PUBLIC :: INIT_ALL
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE :: INIT_LOCAL_MET
!
! !REVISION HISTORY:
!  26 Jan 2012 - M. Long     - Created module file
!  13 Aug 2012 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
CONTAINS
!EOC        
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: allocate_all
!
! !DESCRIPTION: Subroutine ALLOCATE\_ALL allocates all LAT/LON ALLOCATABLE 
!  arrays for global use by the GEOS-Chem either as a standalone program or
!  module.

!\\
!\\
! !INTERFACE:
!
  SUBROUTINE ALLOCATE_ALL
!
! !USES:
!

    USE GC_TYPE2_MOD,      ONLY : CHEM_STATE, INIT_CHEMSTATE
    USE CMN_DEP_MOD,       ONLY : SET_CMN_DEP_MOD
    USE CMN_NOX_MOD,       ONLY : SET_CMN_NOX_MOD
    USE CMN_O3_MOD,        ONLY : SET_CMN_O3_MOD
    USE CMN_MOD,           ONLY : SET_CMN_MOD
    USE CMN_FJ_MOD,        ONLY : SET_CMN_FJ_MOD
    USE JV_CMN_MOD,        ONLY : SET_JV_CMN_MOD
    USE COMMSOIL_MOD,      ONLY : SET_COMMSOIL_MOD
    USE VDIFF_PRE_MOD,     ONLY : SET_VDIFF_PRE_MOD
    USE CMN_SIZE_MOD,      ONLY : SET_CMN_SIZE_MOD
    USE CMN_DIAG_MOD,      ONLY : SET_CMN_DIAG_MOD
    USE COMODE_LOOP_MOD,   ONLY : SET_COMODE_LOOP_MOD
          
    IMPLICIT NONE
! 
! !REVISION HISTORY: 
!  26 Jan 2012 - M. Long     - Initial version
!  13 Aug 2012 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
    CALL SET_CMN_SIZE_MOD
    CALL SET_CMN_DEP_MOD
    CALL SET_CMN_DIAG_MOD
    CALL SET_CMN_NOX_MOD
    CALL SET_CMN_O3_MOD
    CALL SET_CMN_MOD
    CALL SET_CMN_FJ_MOD
    CALL SET_COMMSOIL_MOD
    CALL SET_COMODE_LOOP_MOD
    CALL SET_JV_CMN_MOD
    
    CALL SET_VDIFF_PRE_MOD
          
  END SUBROUTINE ALLOCATE_ALL
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_all
!
! !DESCRIPTION: Subroutine INIT\_ALL initializes the top-level data structures
!  that are either passed to/from GC or between GC components 
!  (emis->transport->chem->etc)
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE INIT_ALL( LOCAL_MET, CHEM_STATE ) 

!
! !USES:
!
    USE GC_TYPE_MOD
    USE GC_TYPE2_MOD, ONLY : INIT_CHEMSTATE, CHEMSTATE

    IMPLICIT NONE
!
! !OUTPUT PARAMETERS:
!
    TYPE(GC_MET_LOCAL), INTENT(OUT) :: LOCAL_MET
    TYPE(CHEMSTATE   ), INTENT(OUT) :: CHEM_STATE
!
! !REVISION HISTORY: 
!  26 Jan 2012 - M. Long     - Initial version
!  13 Aug 2012 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC

    ! Initialize object for met fields
    CALL INIT_LOCAL_MET( LOCAL_MET )

    ! Initialize object for chemical state
    CALL INIT_CHEMSTATE( CHEM_STATE )
    
  END SUBROUTINE INIT_ALL
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_local_met
!
! !DESCRIPTION: Subroutine INIT\_LOCAL\_MET allocates all fields of an
!  object based on derived type GC_MET_LOCAL (from gc_type_mod.F90).
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE INIT_LOCAL_MET( LOCAL_MET )
!
! !USES:
!
    USE GC_TYPE_MOD
    USE CMN_SIZE_MOD, ONLY : IIPAR, JJPAR, LLPAR
    USE ERROR_MOD,    ONLY : ALLOC_ERR
    USE PRESSURE_MOD, ONLY : GET_PEDGE, GET_PCENTER
    USE TRACER_MOD,   ONLY : N_TRACERS
    
    IMPLICIT NONE
!
! !OUTPUT PARAMETERS:
!
    TYPE(GC_MET_LOCAL), INTENT(OUT) :: LOCAL_MET
!
! !REVISION HISTORY: 
!  01 Oct 1995 - R. Yantosca - Initial version
!  08 Dec 2009 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER :: I,J,L,AS

          ! Allocate 2-D Arrays
    ALLOCATE( &
         LOCAL_MET%ALBD(IIPAR, JJPAR), &   ! Visible surface albedo [unitless]
         LOCAL_MET%CLDFRC(IIPAR, JJPAR), & ! Column cloud fraction [unitless]
         LOCAL_MET%FRCLND(IIPAR, JJPAR), & ! Olson land fraction [unitless]
         LOCAL_MET%GWETTOP(IIPAR, JJPAR), &! Top soil moisture [unitless]
         LOCAL_MET%HFLUX(IIPAR, JJPAR), &  ! Sensible heat flux [W/m2]
         LOCAL_MET%LWI(IIPAR, JJPAR), &    ! Land/water indices [unitless]
         LOCAL_MET%PARDR(IIPAR, JJPAR), &  ! Direct  photsyn active rad [W/m2]
         LOCAL_MET%PARDF(IIPAR, JJPAR), &  ! Diffuse photsyn active rad [W/m2]
         LOCAL_MET%PBLH(IIPAR, JJPAR), &   ! PBL height [m]
         LOCAL_MET%PRECCON(IIPAR, JJPAR), &! Conv  precip @ ground [kg/m2/s]
         LOCAL_MET%PRECTOT(IIPAR, JJPAR), &! Total precip @ ground [kg/m2/s]
         LOCAL_MET%RADSWG(IIPAR,JJPAR), & ! Solar radiation @ ground [W/m2]
         LOCAL_MET%SST(IIPAR, JJPAR), &    ! Sea surface temperature [K]
         LOCAL_MET%SUNCOS(IIPAR*JJPAR), & ! Cosine of solar zenith angle
         LOCAL_MET%TO3(IIPAR, JJPAR), &    ! Total overhead O3 column [DU]
         LOCAL_MET%TROPP(IIPAR, JJPAR), &  ! Tropopause pressure [hPa]
         LOCAL_MET%TS(IIPAR, JJPAR), &     ! Surface temperature [K]
         LOCAL_MET%U10M(IIPAR, JJPAR), &   ! E/W wind speed @ 10m height [m/s]
         LOCAL_MET%USTAR(IIPAR, JJPAR), &  ! Friction velocity [m/s]
         LOCAL_MET%UVALBEDO(IIPAR, JJPAR), &! UV surface albedo [unitless]
         LOCAL_MET%V10M(IIPAR, JJPAR), &   ! N/S wind speed @ 10m height [m/s]
         LOCAL_MET%Z0(IIPAR, JJPAR), &
         STAT = AS )
    IF (AS /= 0) CALL ALLOC_ERR('LOCAL_MET 2D')

          ! Allocate 3-D Arrays
    ALLOCATE( &
         LOCAL_MET%AD(IIPAR,JJPAR,LLPAR), &        ! Air mass [kg]
         LOCAL_MET%AIRDENS(LLPAR,IIPAR,JJPAR), &   ! Air density [kg/m3]
         LOCAL_MET%AIRVOL(IIPAR,JJPAR,LLPAR), &    ! Grid box volume [m3]
         LOCAL_MET%AREA_M2(IIPAR, JJPAR, LLPAR), & ! Grid box surface area [cm2]
         LOCAL_MET%BXHEIGHT(IIPAR,JJPAR,LLPAR), &  ! Grid box height [m]
         LOCAL_MET%CLDF(LLPAR,IIPAR,JJPAR), &      ! 3-D cloud fraction [unitless]
         LOCAL_MET%CMFMC(IIPAR,JJPAR,LLPAR+1), &     ! Cloud mass flux [kg/m2/s]
         LOCAL_MET%DQIDTMST(IIPAR,JJPAR,LLPAR), &  ! Ice tendency, mst proc [kg/kg/s]
         LOCAL_MET%DQLDTMST(IIPAR,JJPAR,LLPAR), &  ! H2O tendency, mst proc [kg/kg/s]
         LOCAL_MET%DQVDTMST(IIPAR,JJPAR,LLPAR), &  ! Vapor tendency, mst proc [kg/kg/s]
         LOCAL_MET%DTRAIN(IIPAR,JJPAR,LLPAR), &    ! Detrainment flux [kg/m2/s]
         LOCAL_MET%MOISTQ(LLPAR,IIPAR,JJPAR), &    ! Tendency in sp. humidity [kg/kg/s]
         LOCAL_MET%OPTD(LLPAR,IIPAR,JJPAR), &      ! Visible optical depth [unitless]
         LOCAL_MET%PEDGE(IIPAR,JJPAR,LLPAR+1), &     ! Pressure @ level edges [Pa]
         LOCAL_MET%PMID(IIPAR,JJPAR,LLPAR), &      ! Pressure @ level centers [Pa]
         LOCAL_MET%DELP(LLPAR,IIPAR,JJPAR), &      ! Pressure thickness for layer [Pa]
         LOCAL_MET%RH(IIPAR,JJPAR,LLPAR), &        ! Relative humidity [unitless]
         LOCAL_MET%SPHU(IIPAR,JJPAR,LLPAR), &      ! Specific humidity [kg/kg]
         LOCAL_MET%T(IIPAR,JJPAR,LLPAR), &         ! Temperature [K]
         LOCAL_MET%TAUCLI(IIPAR,JJPAR,LLPAR), &    ! Opt depth of ice clouds [unitless]
         LOCAL_MET%TAUCLW(IIPAR,JJPAR,LLPAR), &    ! Opt depth of H2O clouds [unitless]
         STAT=AS)
    
    IF (AS /= 0) CALL ALLOC_ERR('LOCAL_MET 3D')
    
! Comment out DO loop
!    DO I = 1,IIPAR
!       DO J = 1, JJPAR
!          DO L = 1, LLPAR
!!             LOCAL_MET%PEDGE(I,J,L) = GET_PEDGE(I,J,L)
!!                   LOCAL_MET%PMID (I,J,L) = GET_PCENTER(I,J,L)
!                ENDDO
!             ENDDO
!          ENDDO

!          ALLOCATE( TRACER_INDEX(N_TRACERS), STAT = AS )
!          write(*,*) 'TRACER_INDEX: ', shape(tracer_index)

  END SUBROUTINE INIT_LOCAL_MET
!EOC           
END MODULE GC_ENVIRONMENT_MOD
#endif
