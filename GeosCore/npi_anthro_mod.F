!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: npi_anthro_mod
!
! !DESCRIPTION: Module NPI\_ANTHRO\_MOD contains variables and routines to 
!  read the Australian National Pollutant Inventory anthropogenic emissions 
!  (jaf, 11/08/2012)
!\\
!\\
! !INTERFACE: 
!
      MODULE NPI_ANTHRO_MOD
! 
! !USES:
!
      IMPLICIT NONE
#     include "define.h"
      PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
      PUBLIC :: CLEANUP_NPI_ANTHRO
      PUBLIC :: EMISS_NPI_ANTHRO
      PUBLIC :: GET_AUSTRALIA_MASK
      PUBLIC :: GET_NPI_ANTHRO
!
! !PRIVATE MEMBER FUNCTIONS:
!
      PRIVATE :: NPI_SCALE_FUTURE
      PRIVATE :: READ_AUSTRALIA_MASK
      PRIVATE :: INIT_NPI_ANTHRO
      PRIVATE :: TOTAL_ANTHRO_TG
!
! !REVISION HISTORY:
!  08 Nov 2012 - J.A. Fisher - Initial version, based on cac_anthro_mod.F
!
!EOP
!------------------------------------------------------------------------------
!
! !PRIVATE DATA MEMBERS:
!

      ! Arrays for data masks
      INTEGER, ALLOCATABLE :: MASK_AUSTRALIA_1x1(:,:)
      REAL*8,  ALLOCATABLE :: MASK_AUSTRALIA(:,:)
      
      ! Arrays for emissions
      REAL*8,  ALLOCATABLE :: NOx(:,:)
      REAL*8,  ALLOCATABLE :: CO(:,:)
      REAL*8,  ALLOCATABLE :: SO2(:,:)
      REAL*8,  ALLOCATABLE :: NH3(:,:)
!
! !DEFINED PARAMETERS:
!
      REAL*8,  PARAMETER   :: SEC_IN_YEAR  = 86400d0 * 365.25d0

      CONTAINS
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_australia_mask
!
! !DESCRIPTION: Function GET\_AUSTRALIA\_MASK returns the value of the Australian 
!  geographic mask at grid box (I,J).  MASK=1 if (I,J) is within Australia, 
!  MASK=0 otherwise. (jaf, 11/8/12)
!\\
!\\
! !INTERFACE:
!
      FUNCTION GET_AUSTRALIA_MASK( I, J ) RESULT( THISMASK )
!
! !INPUT PARAMETERS: 
!
      ! Longitude and latitude indices
      INTEGER, INTENT(IN) :: I, J
!
! !REVISION HISTORY: 
!  08 Nov 2012 - J.A. Fisher - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      ! Local variables
      REAL*8              :: THISMASK

      !=================================================================
      ! GET_AUSTRALIA_MASK begins here!
      !=================================================================
      THISMASK = MASK_AUSTRALIA(I,J)

      END FUNCTION GET_AUSTRALIA_MASK
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_npi_anthro
!
! !DESCRIPTION: Function GET\_NPI\_ANTHRO returns the National Pollutant Inventory
!  emission for GEOS-Chem grid box (I,J) and tracer N.  Emissions can be 
!  returned in units of [kg/s] or [molec/cm2/s].  (jaf, 11/8/12)
!\\
!\\
! !INTERFACE:
!
      FUNCTION GET_NPI_ANTHRO( I,    J,     N, 
     &                         MOLEC_CM2_S, KG_S ) RESULT( VALUE )
!
! !USES:
!
      USE GRID_MOD,     ONLY : GET_AREA_CM2
      USE TRACER_MOD,   ONLY : XNUMOL
      USE TRACERID_MOD, ONLY : IDTNOx, IDTCO, IDTSO2, IDTNH3
!
! !INPUT PARAMETERS: 
!
      ! Longitude, latitude, and tracer indices
      INTEGER, INTENT(IN)           :: I, J, N

      ! OPTIONAL -- return emissions in [molec/cm2/s]
      LOGICAL, INTENT(IN), OPTIONAL :: MOLEC_CM2_S  

      ! OPTIONAL -- return emissions in [kg/s]
      LOGICAL, INTENT(IN), OPTIONAL :: KG_S
!
! !RETURN VALUE:
!     
      ! Emissions output
      REAL*8                        :: VALUE     
!
! !REVISION HISTORY: 
!  08 Nov 2012 - J.A. Fisher - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      LOGICAL                       :: DO_KGS, DO_MCS

      !=================================================================
      ! GET_NPI_ANTHRO begins here!
      !=================================================================

      ! Initialize
      DO_KGS = .FALSE.
      DO_MCS = .FALSE.
      
      ! Return data in [kg/s] or [molec/cm2/s]?
      IF ( PRESENT( KG_S        ) ) DO_KGS = KG_S
      IF ( PRESENT( MOLEC_CM2_S ) ) DO_MCS = MOLEC_CM2_S

      IF ( N == IDTNOx ) THEN

         ! NOx [kg/yr]
         VALUE = NOx(I,J)

      ELSE IF ( N == IDTCO ) THEN

         ! CO [kg/yr]
         VALUE = CO(I,J)

      ELSE IF ( N == IDTSO2 ) THEN

         ! SO2 [kg/yr]
         VALUE = SO2(I,J)

      ELSE IF ( N == IDTNH3 ) THEN

         ! NH3 [kg/yr]
         VALUE = NH3(I,J)

      ELSE

         ! Otherwise return a negative value to indicate
         ! that there are no NPI emissions for tracer N
         VALUE = -1d0
         RETURN

      ENDIF

      !------------------------------
      ! Convert units (if necessary)
      !------------------------------
      IF ( DO_KGS ) THEN
            
         ! Convert from [kg/yr] to [kg/s]
         VALUE = VALUE / SEC_IN_YEAR

      ELSE IF ( DO_MCS ) THEN

         ! Convert NOx from [kg/yr] to [molec/cm2/s]
         VALUE = ( VALUE       * XNUMOL(N)               )
     &         / ( SEC_IN_YEAR * GET_AREA_CM2( I, J, 1 ) )

      ENDIF

      END FUNCTION GET_NPI_ANTHRO
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: emiss_npi_anthro
!
! !DESCRIPTION: Subroutine EMISS\_NPI\_ANTHRO reads the Critical Air 
!  Contaminants emission fields at 0.5x0.5 resolution and regrids them to the 
!  current model resolution. (jaf, 11/08/12)
!\\
!\\
! !INTERFACE:
!
#if defined( DEVEL )
      SUBROUTINE EMISS_NPI_ANTHRO(EMISSIONS)
#else
      SUBROUTINE EMISS_NPI_ANTHRO
#endif
!
! !USES:
! 
      USE BPCH2_MOD,         ONLY : GET_TAU0,      READ_BPCH2
      USE DIRECTORY_MOD,     ONLY : DATA_DIR_1x1 
      USE LOGICAL_MOD,       ONLY : LFUTURE
      USE TIME_MOD,          ONLY : GET_YEAR
      USE SCALE_ANTHRO_MOD,  ONLY : GET_ANNUAL_SCALAR_1x1
      USE REGRID_A2A_MOD,    ONLY : DO_REGRID_A2A
      USE CMN_SIZE_MOD            ! Size parameters
      USE CMN_O3_MOD              ! FSCALYR

#if defined( DEVEL )
      USE TRACERID_MOD, ONLY : IDTNOx, IDTCO, IDTSO2, IDTNH3
      USE TRACER_MOD, ONLY   : N_TRACERS 
#endif
!
! !REVISION HISTORY: 
!  08 Nov 2012 - J.A. Fisher - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      LOGICAL, SAVE              :: FIRST = .TRUE.
      INTEGER                    :: I, J, THISYEAR, SPECIES, SNo
      REAL*4                     :: ARRAY(I1x1,J1x1-1,1)
      REAL*8, TARGET             :: GENERIC_1x1(I1x1,J1x1-1,1)
      REAL*8                     :: TAU
      CHARACTER(LEN=255)         :: FILENAME
      CHARACTER(LEN=4)           :: SYEAR, SNAME
      CHARACTER(LEN=255)         :: LLFILENAME
      REAL*8, POINTER            :: INGRID(:,:) => NULL()

#if defined( DEVEL )
      REAL*8, INTENT(INOUT)      :: EMISSIONS(IIPAR,JJPAR,N_TRACERS+1)
#endif
      !=================================================================
      ! EMISS_NPI_ANTHRO begins here!
      !=================================================================

      ! First-time initialization
      IF ( FIRST ) THEN
         CALL INIT_NPI_ANTHRO
         FIRST = .FALSE.
      ENDIF

      ! Get emissions year
      IF ( FSCALYR < 0 ) THEN
         THISYEAR = GET_YEAR()
      ELSE
         THISYEAR = FSCALYR
      ENDIF

      ! File with lat/lon edges for regridding
      LLFILENAME = TRIM( DATA_DIR_1x1) //
     &            'MAP_A2A_Regrid_201203/MAP_A2A_latlon_generic1x1.nc'

      DO SPECIES = 1,4

         IF ( SPECIES .eq. 1 ) THEN
            SNAME = 'NOx'
            SNo = 1
         ELSEIF ( SPECIES .eq. 2 ) THEN
            SNAME = 'CO'
            SNo = 4
         ELSEIF ( SPECIES .eq. 3 ) THEN
            SNAME = 'SOx'
            SNo = 26
         ELSEIF ( SPECIES .eq. 4 ) THEN
            SNAME = 'NH3'
            SNo = 30
         ENDIF

!!!! Temporary kludge - only one year of emissions so far (jaf, 11/8/12)
         ! TAU values 
         TAU = GET_TAU0( 1, 1, 2007 )
         WRITE( SYEAR, '(i4)' ) 2007

         ! File name
!!!! Temporary kludge - not in data directory yet (jaf, 11/8/12)
!         FILENAME  = TRIM( DATA_DIR_1x1 ) // 'NPI_201211/NPI' // 
          FILENAME  = '/home/574/jaf574/data/NPI/NPI' //
     &                SYEAR // '-' // TRIM( SNAME ) // 
     &                'generic.1x1'

         ! Echo info
         WRITE( 6, 100 ) TRIM( FILENAME )
 100     FORMAT( '     - EMISS_NPI_ANTHRO: Reading ', a )

         ! Read data
         CALL READ_BPCH2( FILENAME, 'ANTHSRCE', SNo, 
     &                    TAU,      I1x1,       J1x1-1,     
     &                    1,        ARRAY,      QUIET=.TRUE. ) 

         ! Cast to REAL*8 before regridding
         GENERIC_1x1(:,:,1) = ARRAY(:,:,1)

         ! Regrid from GENERIC 1x1 --> current model resolution [kg/yr]
         INGRID => GENERIC_1x1(:,:,1)

         IF ( SPECIES .eq. 1 ) THEN

            CALL DO_REGRID_A2A( LLFILENAME, I1x1,    J1x1-1, 
     &                          INGRID,     NOx,     PERAREA=1,
     &                          netCDF=.TRUE.                   )

         ELSEIF ( SPECIES .eq. 2 ) THEN

            CALL DO_REGRID_A2A( LLFILENAME, I1x1,    J1x1-1,
     &                          INGRID,     CO,      PERAREA=1,
     &                          netCDF=.TRUE.                   )

         ELSEIF ( SPECIES .eq. 3 ) THEN

            CALL DO_REGRID_A2A( LLFILENAME, I1x1,    J1x1-1,
     &                          INGRID,     SO2,     PERAREA=1,
     &                          netCDF=.TRUE.                   )

         ELSEIF ( SPECIES .eq. 4 ) THEN

            CALL DO_REGRID_A2A( LLFILENAME, I1x1,    J1x1-1,
     &                          INGRID,     NH3,     PERAREA=1,
     &                          netCDF=.TRUE.                   )
         ENDIF

         ! Free pointer
         NULLIFY( INGRID )

      ENDDO

      !--------------------------
      ! Compute future emissions
      !--------------------------
      IF ( LFUTURE ) THEN 
         CALL NPI_SCALE_FUTURE
      ENDIF

      !--------------------------
      ! Print emission totals
      !--------------------------
      CALL TOTAL_ANTHRO_Tg( THISYEAR )

#if defined( DEVEL )
      EMISSIONS(:,:,IDTNH3) = EMISSIONS(:,:,IDTNH3)+NH3
      EMISSIONS(:,:,IDTSO2) = EMISSIONS(:,:,IDTSO2)+SO2
      EMISSIONS(:,:,IDTNOx) = EMISSIONS(:,:,IDTNOx)+NOx
      EMISSIONS(:,:,IDTCO)  = EMISSIONS(:,:,IDTCO) +CO
#endif

      END SUBROUTINE EMISS_NPI_ANTHRO
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: npi_scale_future
!
! !DESCRIPTION: Subroutine NPI\_SCALE\_FUTURE applies the IPCC future scale 
!  factors to the National Pollutant Inventory anthropogenic emissions.
!  (jaf, 11/8/12)
!\\
!\\
! !INTERFACE:

      SUBROUTINE NPI_SCALE_FUTURE
!
! !USES:
! 
      USE FUTURE_EMISSIONS_MOD, ONLY : GET_FUTURE_SCALE_COff
      USE FUTURE_EMISSIONS_MOD, ONLY : GET_FUTURE_SCALE_NH3an 
      USE FUTURE_EMISSIONS_MOD, ONLY : GET_FUTURE_SCALE_NOxff
      USE FUTURE_EMISSIONS_MOD, ONLY : GET_FUTURE_SCALE_SO2ff

      USE CMN_SIZE_MOD             ! Size parameters
!
! !REVISION HISTORY: 
!  08 Nov 2012 - J.A. Fisher - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER                       :: I, J

      !=================================================================
      ! STREETS_SCALE_FUTURE begins here!
      !=================================================================

!$OMP PARALLEL DO
!$OMP+DEFAULT( SHARED )
!$OMP+PRIVATE( I, J )
      DO J = 1, JJPAR
      DO I = 1, IIPAR

         ! Future NOx [kg NO2/yr]
         NOx(I,J)  = NOx(I,J) * GET_FUTURE_SCALE_NOxff( I, J )

         ! Future CO  [kg CO /yr]
         CO(I,J)   = CO(I,J)  * GET_FUTURE_SCALE_COff(  I, J )

         ! Future SO2 [kg SO2/yr] 
         SO2(I,J)  = SO2(I,J) * GET_FUTURE_SCALE_SO2ff( I, J )

         ! Future NH3 [kg NH3/yr] 
         NH3(I,J)  = NH3(I,J) * GET_FUTURE_SCALE_NH3an( I, J )

      ENDDO
      ENDDO
!$OMP END PARALLEL DO

      END SUBROUTINE NPI_SCALE_FUTURE
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: total_anthro_tg
!
! !DESCRIPTION: Subroutine TOTAL\_ANTHRO\_TG prints the totals for the 
!  anthropogenic emissions of NOx, CO, SO2 and NH3. (amv, phs, 1/28/09)
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE TOTAL_ANTHRO_TG( YEAR )
!
! !USES:
! 
      USE CMN_SIZE_MOD            ! Size parameters
!
! !INPUT PARAMETERS:
!
      INTEGER, INTENT(IN) :: YEAR   ! Year of data to compute totals
!
! !REVISION HISTORY: 
!  08 Nov 2012 - J.A. Fisher - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER             :: I,     J
      REAL*8              :: T_NOX, T_CO,  T_SO2,  T_NH3
      CHARACTER(LEN=3)    :: UNIT

      !=================================================================
      ! TOTAL_ANTHRO_TG begins here!
      !=================================================================

      ! Fancy output
      WRITE( 6, '(a)' ) REPEAT( '=', 79 )
      WRITE( 6, 100  )
 100  FORMAT( 'N. P. I.   A U S T R A L I A N   E M I S S I O N S', / )


      ! Total NOx [Tg N]
      T_NOX = SUM( NOx ) * 1d-9 * ( 14d0 / 46d0 )

      ! Total CO  [Tg CO]
      T_CO  = SUM( CO  ) * 1d-9

      ! Total SO2 [Tg S]
      T_SO2 = SUM( SO2 ) * 1d-9 * ( 32d0 / 64d0 )

      ! Total NH3 [Tg NH3]
      T_NH3 = SUM( NH3 ) * 1d-9

      ! Print totals in [kg]
      WRITE( 6, 110 ) 'NOx ', YEAR, T_NOx,  '[Tg N  ]'
      WRITE( 6, 110 ) 'CO  ', YEAR, T_CO,   '[Tg CO ]'
      WRITE( 6, 110 ) 'SO2 ', YEAR, T_SO2,  '[Tg S  ]'
      WRITE( 6, 110 ) 'NH3 ', YEAR, T_NH3,  '[Tg NH3]'

      ! Format statement
 110  FORMAT( 'N.P.I. Australian anthro ', a5, 
     &        'for year ', i4, ': ', f11.4, 1x, a8 )

      ! Fancy output
      WRITE( 6, '(a)' ) REPEAT( '=', 79 )
      
      END SUBROUTINE TOTAL_ANTHRO_Tg
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: read_australia_mask
!
! !DESCRIPTION: Subroutine READ\_AUSTRALIA\_MASK reads and regrids the Australian 
!  geographic mask from disk. (jaf, 11/8/12)
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE READ_AUSTRALIA_MASK
!
! !USES:
! 
      USE BPCH2_MOD,      ONLY : GET_TAU0, READ_BPCH2
      USE DIRECTORY_MOD,  ONLY : DATA_DIR_1x1
      USE REGRID_A2A_MOD, ONLY : DO_REGRID_A2A

      USE CMN_SIZE_MOD         ! Size parameters
!
! !REVISION HISTORY: 
!  08 Nov 2012 - J.A. Fisher - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      REAL*4                  :: ARRAY(I1x1,J1x1-1,1)
      REAL*8, TARGET          :: GENERIC_1x1(I1x1,J1x1-1,1)
      REAL*8                  :: TAU2000
      CHARACTER(LEN=255)      :: FILENAME
      CHARACTER(LEN=255)      :: LLFILENAME
      REAL*8, POINTER         :: INGRID(:,:) => NULL()

      !=================================================================
      ! READ_AUSTRALIA_MASK begins here!
      !=================================================================

      TAU2000 = GET_TAU0(1,1,2000)

      ! File name
!!!! Temporary kludge - not in data directory yet (jaf, 11/8/12)
!      FILENAME  = TRIM( DATA_DIR_1x1 ) // 'NPI_201211/'
      FILENAME  = '/home/574/jaf574/data/NPI/' //
     &            'AustraliaMask.generic.1x1'

      ! File with lat/lon edges for regridding
      LLFILENAME = TRIM( DATA_DIR_1x1) //
     &             'MAP_A2A_Regrid_201203/MAP_A2A_latlon_generic1x1.nc'

      ! Echo info
      WRITE( 6, 100 ) TRIM( FILENAME )
 100  FORMAT( '     - READ_AUSTRALIA_MASK: Reading ', a )

      ! Read data [unitless]
      CALL READ_BPCH2( FILENAME, 'LANDMAP', 2, 
     &                 TAU2000,   I1x1,     J1x1-1,     
     &                 1,         ARRAY,    QUIET=.TRUE. ) 

      ! Cast to REAL*8 before regridding
      GENERIC_1x1(:,:,1) = ARRAY(:,:,1)

      ! Save the 1x1 China mask for future use
      MASK_AUSTRALIA_1x1(:,:) = GENERIC_1x1(:,:,1)

      ! Regrid from GENERIC 1x1 to current model resolution [unitless]
      INGRID => GENERIC_1x1(:,:,1)
      CALL DO_REGRID_A2A( LLFILENAME, I1x1,        J1x1-1, 
     &                    INGRID,     MASK_AUSTRALIA, PERAREA=0,
     &                    netCDF=.TRUE.                       )

      ! Free pointer
      NULLIFY( INGRID )

      WHERE ( MASK_AUSTRALIA > 0D0 ) MASK_AUSTRALIA = 1D0

      END SUBROUTINE READ_AUSTRALIA_MASK
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_npi_anthro
!
! !DESCRIPTION: Subroutine INIT\_NPI\_ANTHRO allocates and zeroes all 
!  module arrays. (jaf, 11/8/12)
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE INIT_NPI_ANTHRO
!
! !USES:
! 
      USE ERROR_MOD,   ONLY : ALLOC_ERR
      USE LOGICAL_MOD, ONLY : LNPI

      USE CMN_SIZE_MOD    ! Size parameters
!
! !REVISION HISTORY: 
!  08 Nov 2012 - J.A. Fisher - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER              :: AS, J

      !=================================================================
      ! INIT_NPI_ANTHRO begins here!
      !=================================================================

      ! Return if LNPI is false
      IF ( .not. LNPI ) RETURN
      
      !--------------------------------------------------
      ! Allocate and zero arrays for emissions
      !--------------------------------------------------

      ALLOCATE( NOx( IIPAR, JJPAR ), STAT=AS )
      IF ( AS /= 0 ) CALL ALLOC_ERR( 'NOx' )
      NOx = 0d0

      ALLOCATE( CO( IIPAR, JJPAR ), STAT=AS )
      IF ( AS /= 0 ) CALL ALLOC_ERR( 'CO' )
      CO = 0d0

      ALLOCATE( SO2( IIPAR, JJPAR ), STAT=AS )
      IF ( AS /= 0 ) CALL ALLOC_ERR( 'SO2' )
      SO2 = 0d0

      ALLOCATE( NH3( IIPAR, JJPAR ), STAT=AS )
      IF ( AS /= 0 ) CALL ALLOC_ERR( 'NH3' )
      NH3 = 0d0

      !---------------------------------------------------
      ! Read & Regrid masks for NPI emissions
      !---------------------------------------------------

      ALLOCATE( MASK_AUSTRALIA_1x1( I1x1, J1x1-1 ), STAT=AS )
      IF ( AS /= 0 ) CALL ALLOC_ERR( 'MASK_AUSTRALIA_1x1' )
      MASK_AUSTRALIA_1x1 = 0

      ALLOCATE( MASK_AUSTRALIA( IIPAR, JJPAR ), STAT=AS )
      IF ( AS /= 0 ) CALL ALLOC_ERR( 'MASK_AUSTRALIA' )
      MASK_AUSTRALIA = 0d0

      ! Read Australia mask from disk
      CALL READ_AUSTRALIA_MASK

      END SUBROUTINE INIT_NPI_ANTHRO
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: cleanup_npi_anthro
!
! !DESCRIPTION: Subroutine CLEANUP\_NPI\_ANTHRO deallocates all module 
!  arrays. (jaf, 11/8/12)
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE CLEANUP_NPI_ANTHRO
!
! !REVISION HISTORY: 
!  08 Nov 2012 - J.A. Fisher - Initial Version
!EOP
!------------------------------------------------------------------------------
!BOC
      !=================================================================
      ! CLEANUP_NPI_ANTHRO begins here!
      !=================================================================
      IF ( ALLOCATED( MASK_AUSTRALIA_1x1) ) 
     &                               DEALLOCATE( MASK_AUSTRALIA_1x1 ) 
      IF ( ALLOCATED( MASK_AUSTRALIA ) ) DEALLOCATE( MASK_AUSTRALIA ) 
      IF ( ALLOCATED( NOx            ) ) DEALLOCATE( NOx            )
      IF ( ALLOCATED( CO             ) ) DEALLOCATE( CO             )
      IF ( ALLOCATED( SO2            ) ) DEALLOCATE( SO2            )
      IF ( ALLOCATED( NH3            ) ) DEALLOCATE( NH3            )

      END SUBROUTINE CLEANUP_NPI_ANTHRO
!EOC
      END MODULE CAC_ANTHRO_MOD

