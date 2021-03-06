!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: benchmark_mod
!
! !DESCRIPTION: Module BENCHMARK\_MOD contains routines to save out initial 
!  and final tracer masses which are needed for GEOS-Chem 1-month benchmark 
!  simulations
!\\
!\\
! !INTERFACE: 
!
      MODULE BENCHMARK_MOD
!
! !USES:
!
      IMPLICIT NONE
#     include "define.h"
      PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
      PUBLIC :: STDRUN
!
! !PUBLIC DATA MEMBERS:
!
      CHARACTER(LEN=255), PUBLIC :: INITIAL_FILE ! File w/ initial tracer mass
      CHARACTER(LEN=255), PUBLIC :: FINAL_FILE   ! File w/ final   tracer mass
!
! !REVISION HISTORY:
!  (1 ) Now expand date & time tokens in filenames (bmy, 1/31/05)
!  (2 ) Now modified for GCAP grid (swu, bmy, 6/28/05)
!  (3 ) Now make sure all USE statements are USE, ONLY (bmy, 10/3/05)
!  01 Aug 2012 - R. Yantosca - Add reference to findFreeLUN from inqure_mod.F90
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
! !ROUTINE: 
!
! !DESCRIPTION: Subroutine STDRUN dumps the mass of either Ox [kg] or 222Rn, 
!  210Pb, and 7Be [kg] at the start & end of each run.  This is necessary
!  for GEOS-CHEM benchmarking.

!\\
!\\
! !INTERFACE:
!
      SUBROUTINE STDRUN( LBEGIN )
!
! !USES:
!
      USE BPCH2_MOD,    ONLY : OPEN_BPCH2_FOR_WRITE, BPCH2             
      USE BPCH2_MOD,    ONLY : GET_HALFPOLAR,        GET_MODELNAME
      USE FILE_MOD,     ONLY : IOERROR
      USE TIME_MOD,     ONLY : EXPAND_DATE,          GET_NYMD
      USE TIME_MOD,     ONLY : GET_NHMS,             GET_TAU
      USE TRACER_MOD,   ONLY : ITS_A_FULLCHEM_SIM,   ITS_A_RnPbBe_SIM
      USE TRACER_MOD,   ONLY : STT,                  N_TRACERS
      USE TRACERID_MOD, ONLY : IDTOX
      USE CMN_SIZE_MOD       ! Size parameters
      USE inquireMod,   ONLY : findFreeLUN
!
! !INPUT PARAMETERS: 
!
      ! TRUE  denotes beginning of simulation
      ! FALSE denotes end of simulation
      LOGICAL, INTENT(IN) :: LBEGIN 
! 
! !REVISION HISTORY: 
!  12 Aug 2002 - R. Yantosca - Initial version
!  03 Aug 2012 - R. Yantosca - Added ProTeX headers
!  (1 ) Changed name from STDRUN_Ox to STDRUN, since we now can also save out 
!        Rn/Pb/Be for NSRCX==1.  Also deleted obsolete code from 6/02.  Added 
!        LBEGIN as an argument to determine if this is the start or end of the 
!        run.  (bmy, 8/12/02)
!  (2 ) Bundled into "benchmark_mod.f" (bmy, 7/20/04)
!  (3 ) Now expand date tokens in the filename (bmy, 1/31/05)
!  (4 ) Now call GET_HALFPOLAR from "bpch2_mod.f" to get the HALFPOLAR flag
!        value for GEOS or GCAP grids .  Also removed references to CMN_DIAG
!        and TRCOFFSET. (bmy, 6/28/05)
!  (5 ) Now make sure all USE statements are USE, ONLY (bmy, 10/3/05)
!  03 Aug 2012 - R. Yantosca - Now use findFreeLUN to obtain file unit #'s
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      INTEGER             :: IU_FILE
      INTEGER             :: N,        NYMD,     NHMS
      INTEGER, PARAMETER  :: IFIRST=1, JFIRST=1, LFIRST=1
      INTEGER, PARAMETER  :: CENTER180=1
      INTEGER             :: HALFPOLAR
      REAL*4              :: ARRAY(IIPAR,JJPAR,LLPAR)
      REAL*4              :: LONRES, LATRES
      REAL*8              :: TAU
      CHARACTER(LEN=20)   :: MODELNAME 
      CHARACTER(LEN=40)   :: CATEGORY, RESERVED, UNIT
      CHARACTER(LEN=80)   :: TITLE
      CHARACTER(LEN=255)  :: FILENAME

      !=================================================================
      ! STDRUN begins here!
      !=================================================================

      ! Return if we are not doing either a radon or fullchem stdrun
      IF ( ( .not. ITS_A_FULLCHEM_SIM() ) .and. 
     &     ( .not. ITS_A_RnPbBe_SIM() ) ) RETURN

      ! Find a free file LUN
      IU_FILE   = findFreeLUN()

      ! Define variables for binary punch file
      MODELNAME = GET_MODELNAME()
      HALFPOLAR = GET_HALFPOLAR()
      CATEGORY  = 'TCMASS-$'
      UNIT      = 'kg'
      RESERVED  = ''      
      LONRES    = DISIZE
      LATRES    = DJSIZE
      NYMD      = GET_NYMD()
      NHMS      = GET_NHMS()
      TAU       = GET_TAU()

      ! Define filename for beginning or end of benchmark run
      IF ( LBEGIN ) THEN
         TITLE    = 'GEOS-CHEM Benchmark: Initial Tracer Mass'
         FILENAME = INITIAL_FILE
      ELSE
         TITLE    = 'GEOS-CHEM Benchmark: Final Tracer Mass'
         FILENAME = FINAL_FILE
      ENDIF
           
      ! Expand any date tokens in the filename
      CALL EXPAND_DATE( FILENAME, NYMD, NHMS )

      !=================================================================
      ! Save the mass of 222Rn, 210Pb, 7Be to a file
      !=================================================================
      IF ( ITS_A_RnPbBE_SIM() ) THEN

         ! Open binary punch file for writing
         CALL OPEN_BPCH2_FOR_WRITE( IU_FILE, FILENAME, TITLE )

         ! Loop over tracers
         DO N = 1, N_TRACERS

            ! Save Rn, Pb, Be as REAL*4
            ARRAY(:,:,:) = STT(:,:,:,N)

            ! Write Rn, Pb, Be to binary punch file
            CALL BPCH2( IU_FILE,   MODELNAME, LONRES,    LATRES,
     &                  HALFPOLAR, CENTER180, CATEGORY,  N,    
     &                  UNIT,      TAU,       TAU,       RESERVED,   
     &                  IIPAR,     JJPAR,     LLPAR,     IFIRST,     
     &                  JFIRST,    LFIRST,    ARRAY(:,:,:) )

         ENDDO

      !=================================================================
      ! Save the mass of Ox to a file
      !=================================================================
      ELSE IF ( ITS_A_FULLCHEM_SIM() .and. IDTOX > 0 ) THEN

         ! Open binary punch file for writing
         CALL OPEN_BPCH2_FOR_WRITE( IU_FILE, FILENAME, TITLE )
        
         ! Save Ox as REAL*4
         ARRAY(:,:,:) = STT(:,:,:,IDTOX)

         ! Write Ox to binary punch file
         CALL BPCH2( IU_FILE,   MODELNAME, LONRES,    LATRES,
     &               HALFPOLAR, CENTER180, CATEGORY,  IDTOX,    
     &               UNIT,      GET_TAU(), GET_TAU(), RESERVED,   
     &               IIPAR,     JJPAR,     LLPAR,     IFIRST,     
     &               JFIRST,    LFIRST,    ARRAY(:,:,:) )
               
      ENDIF

      ! Close file
      CLOSE( IU_FILE )

      END SUBROUTINE STDRUN
!EOC
      END MODULE BENCHMARK_MOD
