!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: global_oh_mod
!
! !DESCRIPTION: Module GLOBAL\_OH\_MOD contains variables and routines for 
!  reading the global monthly mean OH concentration from disk. 
!\\
!\\
! !INTERFACE: 
!
      MODULE GLOBAL_OH_MOD
!
! !USES:
!
      IMPLICIT NONE
#     include "define.h"
      PRIVATE
!
! !PUBLIC DATA MEMBERS:
!
      ! Array to store global monthly mean OH field [molec/cm3]
      REAL*8, PUBLIC, ALLOCATABLE :: OH(:,:,:)
!
! !PUBLIC MEMBER FUNCTIONS:
!
      PUBLIC :: CLEANUP_GLOBAL_OH
      PUBLIC :: GET_GLOBAL_OH
      PUBLIC :: GET_GLOBAL_OH_NCDF
      PUBLIC :: INIT_GLOBAL_OH
!
! !REVISION HISTORY:
!  28 Jul 2000 - R. Yantosca - Initial version
!  (1 ) Updated comments (bmy, 9/4/01)
!  (2 ) Now use routines from "transfer_mod.f" to regrid OH to 30 levels
!        for reduced GEOS-3 grid.  Also size OH array properly. (bmy, 1/14/02)
!  (3 ) Eliminate obsolete code from 11/01 (bmy, 2/27/02)
!  (4 ) Now divide module header into MODULE PRIVATE, MODULE VARIABLES, and
!        MODULE ROUTINES sections.  Updated comments (bmy, 5/28/02)
!  (5 ) Now use updated OH fields (bmy, 10/2/02)
!  (6 ) Now references "error_mod.f" (bmy, 10/15/02)
!  (7 ) Minor bug fixes in FORMAT statements (bmy, 3/23/03)
!  (8 ) Cosmetic changes to simplify output (bmy, 3/27/03)
!  (9 ) Bug fix: OH should be (IIPAR,JJPAR,LLPAR) (bmy, 5/4/04)
!  (10) Now make sure all USE statements are USE, ONLY (bmy, 10/3/05)
!  01 Dec 2010 - R. Yantosca - Added ProTeX headers
!  22 May 2013 - J.A. Fisher - Added ability to read yearly varying OH from
!                netcdf files
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
! !IROUTINE: get_global_oh
!
! !DESCRIPTION: Subroutine GET\_GLOBAL\_OH reads global OH from binary 
!  punch files stored in the /data/ctm/GEOS\_MEAN directory.  This OH data 
!  is needed as oxidant for various offline chemistry mechanisms.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE GET_GLOBAL_OH( THISMONTH )
!
! !USES:
!
      USE BPCH2_MOD,     ONLY : GET_NAME_EXT
      USE BPCH2_MOD,     ONLY : GET_RES_EXT
      USE BPCH2_MOD,     ONLY : GET_TAU0
      USE BPCH2_MOD,     ONLY : READ_BPCH2
      USE DIRECTORY_MOD, ONLY : OH_DIR
      USE TRANSFER_MOD,  ONLY : TRANSFER_3D

      USE CMN_SIZE_MOD                  ! Size parameters
!
! !INPUT PARAMETERS: 
!
      INTEGER, INTENT(IN)  :: THISMONTH   ! Current month
! 
! !REVISION HISTORY: 
!  28 Jul 2000 - R. Yantosca - Initial version
!  (1 ) GET_GLOBAL_OH assumes that we are reading global OH data that occupies
!        all CTM levels.  Contact Bob Yantosca (bmy@io.harvard.edu) for IDL
!        regridding code which will produce the appropriate OH files.
!  (2 ) Now use version of GET_TAU0 with 3 arguments.  Now call READ_BPCH2
!        with IIPAR,JJPAR,LGLOB.  Call TRANSFER_3D to cast from REAL*4 to
!        REAL*8 and to regrid to 30 levels for GEOS-3 (if necessary).
!        ARRAY should now be of size (IIPAR,JJPAR,LGLOB). (bmy, 1/11/02)
!  (3 ) Now point to new OH files in the v4-26 subdirectory.  Also eliminated
!        obsolete code from 11/01. (bmy, 2/27/02)
!  (4 ) Now point to OH files in the v4-33 subdirectory. (bmy, 10/2/02)
!  (5 ) Replace missing commas in the FORMAT statement (bmy, 3/23/03)
!  (6 ) Cosmetic changes to simplify output (bmy, 3/27/03)
!  (7 ) Add Mat's OH as an option.  Also read bpch file quietly (bmy, 5/4/04)
!  (8 ) Now use OH_DIR from "directory_mod.f" (bmy, 7/20/04)
!  (9 ) Now make sure all USE statements are USE, ONLY (bmy, 10/3/05)
!  01 Dec 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER            :: I, J, L
      REAL*4             :: ARRAY(IIPAR,JJPAR,LGLOB)
      REAL*8             :: XTAU
      CHARACTER(LEN=255) :: FILENAME

      ! First time flag
      LOGICAL, SAVE      :: FIRST = .TRUE. 

      !=================================================================
      ! GET_GLOBAL_OH begins here!
      !=================================================================

      ! Allocate OH array, if this is the first call
      IF ( FIRST ) THEN
         CALL INIT_GLOBAL_OH
         FIRST = .FALSE.
      ENDIF

      ! Filename
      FILENAME = TRIM( OH_DIR ) // 'OH_3Dglobal.' // GET_NAME_EXT() // 
     &                              '.'           // GET_RES_EXT()

      ! Echo some information to the standard output
      WRITE( 6, 110 ) TRIM( FILENAME )
 110  FORMAT( '     - GET_GLOBAL_OH: Reading OH from: ', a )

      ! Get the TAU0 value for the start of the given month
      ! Assume "generic" year 1985 (TAU0 = [0, 744, ... 8016])
      XTAU = GET_TAU0( THISMONTH, 1, 1985 )

      ! Read OH data from the binary punch file
      CALL READ_BPCH2( FILENAME, 'CHEM-L=$', 1,     
     &                 XTAU,      IIPAR,     JJPAR,      
     &                 LGLOB,     ARRAY,     QUIET=.TRUE. )

      ! Assign data from ARRAY to the module variable OH
      CALL TRANSFER_3D( ARRAY, OH )

      END SUBROUTINE GET_GLOBAL_OH
!EOC
!------------------------------------------------------------------------------
!         J. Fisher - U. Wollongong                                           !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: get_global_oh_ncdf
!
! !DESCRIPTION: Subroutine GET\_GLOBAL\_OH\_NCDF reads the OH concentrations
!  archived from a full chemistry simulation
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE GET_GLOBAL_OH_NCDF( THISMONTH, THISYEAR )
!
! !USES:
!
      USE BPCH2_MOD,      ONLY : GET_NAME_EXT, GET_RES_EXT
      USE DIRECTORY_MOD,  ONLY : DATA_DIR
      USE ERROR_MOD,      ONLY : IT_IS_NAN, GEOS_CHEM_STOP
      USE NCDF_MOD,       ONLY : NC_READ
      USE TIME_MOD,       ONLY : EXPAND_DATE
      USE TRANSFER_MOD,   ONLY : TRANSFER_3D

      USE CMN_SIZE_MOD          ! Size parameters

!
! !INPUT PARAMETERS:
!
      ! Year and month
      INTEGER, INTENT(IN) :: THISYEAR
      INTEGER, INTENT(IN) :: THISMONTH
!
! !REVISION HISTORY:
!  22 May 2013 - J. Fisher - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      CHARACTER(LEN=255)    :: FILENAME
      CHARACTER(LEN=20)     :: PARNAME
      REAL*4, POINTER       :: NCARRAY(:,:,:,:) => NULL()
      REAL*4                :: ARRAY(IIPAR,JJPAR,LGLOB)
      INTEGER               :: I, J, L, LDIM, NYMD

      ! First time flag
      LOGICAL, SAVE      :: FIRST = .TRUE.

      !=================================================================
      ! GET_GLOBAL_OH_NCDF begins here!
      !=================================================================

      ! Allocate OH array, if this is the first call
      IF ( FIRST ) THEN
         CALL INIT_GLOBAL_OH
         FIRST = .FALSE.
      ENDIF

      ! Initialize reading variables
      NCARRAY   = 0E0
      ARRAY     = 0E0
      OH        = 0D0

      ! Filename
      FILENAME = TRIM( DATA_DIR ) // 
     &           'tagged_CO_201401/OH_3Dglobal.YYYY.'//
     &           GET_NAME_EXT()   // '.' // GET_RES_EXT() // '.nc'

#if   defined( GEOS_5 ) || defined( MERRA ) || defined( GEOS_57 )

#else
      WRITE(6,*) 'Monthly-varying OH has only been saved for GEOS-5!'
      WRITE(6,*) 'Create a new input file, or turn off '
      WRITE(6,*) '  "Use full chem OH?" in input.geos'
      CALL GEOS_CHEM_STOP

#endif

      ! YYYYMMDD for first day of year
      NYMD = ( THISYEAR * 10000 ) + 0101 

      ! Expand filename
      CALL EXPAND_DATE( FILENAME, NYMD, 000000 )

      ! Echo to stdout
      WRITE( 6, '(a)' ) 'READING ', TRIM( FILENAME )

      ! Read OH
      PARNAME  = 'OH'
      CALL NC_READ ( NC_PATH = TRIM(FILENAME) ,
     &               PARA    = TRIM(PARNAME)  ,
     &               ARRAY   = NCARRAY        ,
     &               YEAR    = THISYEAR       ,
     &               MONTH   = THISMONTH      ,
     &               VERBOSE = .FALSE.         )

      ! Resize and cast to REAL*8
      LDIM = SIZE(NCARRAY,3)
      ARRAY(:,:,1:LDIM) = NCARRAY(:,:,:,1)
      CALL TRANSFER_3D( ARRAY, OH )

      ! Deallocate ncdf-array
      IF ( ASSOCIATED ( NCARRAY ) ) DEALLOCATE ( NCARRAY )

      ! Return to calling program
      END SUBROUTINE GET_GLOBAL_OH_NCDF
!EOC
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_global_oh
!
! !DESCRIPTION: Subroutine INIT\_GLOBAL\_OH allocates and zeroes
!  all module arrays.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE INIT_GLOBAL_OH
!
! !USES:
!
      USE ERROR_MOD, ONLY : ALLOC_ERR

      USE CMN_SIZE_MOD 
! 
! !REVISION HISTORY: 
!  28 Jul 2000 - R. Yantosca - Initial version
!  (1 ) OH array now needs to be sized (IIPAR,JJPAR,LGLOB) (bmy, 1/14/02)
!  (2 ) Also eliminated obsolete code from 11/01 (bmy, 2/27/02)
!  (3 ) Now references ALLOC_ERR from "error_mod.f" (bmy, 10/15/02)
!  (4 ) OH should be (IIPAR,JJPAR,LLPAR): avoid subscript errors (bmy, 5/4/04)
!  01 Dec 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER :: AS

      !=================================================================
      ! INIT_GLOBAL_OH begins here!
      !=================================================================

      ! Allocate OH array
      ALLOCATE( OH( IIPAR, JJPAR, LLPAR ), STAT=AS )
      IF ( AS /= 0 ) CALL ALLOC_ERR( 'OH' )

      ! Zero OH array
      OH = 0d0

      END SUBROUTINE INIT_GLOBAL_OH
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: cleanup_global_oh
!
! !DESCRIPTION: Subroutine CLEANUP\_GLOBAL\_OH deallocates all module arrays.
!\\
!\\
! !INTERFACE:
!
      SUBROUTINE CLEANUP_GLOBAL_OH
! 
! !REVISION HISTORY: 
!  28 Jul 2000 - R. Yantosca - Initial version
!  01 Dec 2010 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
      !=================================================================
      ! CLEANUP_GLOBAL_OH begins here!
      !=================================================================
      IF ( ALLOCATED( OH ) ) DEALLOCATE( OH ) 
     
      END SUBROUTINE CLEANUP_GLOBAL_OH
!EOC
      END MODULE GLOBAL_OH_MOD
