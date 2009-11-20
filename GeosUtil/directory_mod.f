! $Id: directory_mod.f,v 1.1 2009/11/20 21:43:07 bmy Exp $
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: directory_mod.f
!
! !DESCRIPTION: Module DIRECTORY\_MOD contains the directory path variables
!  used by GEOS-Chem.
!\\
!\\
! !INTERFACE: 
!
      MODULE DIRECTORY_MOD
! 
! !USES:
!
      IMPLICIT NONE
      PUBLIC
!
! !PUBLIC DATA MEMBERS:
!
      CHARACTER(LEN=255) :: DATA_DIR      ! Main DAO met field directory
      CHARACTER(LEN=255) :: DATA_DIR_1x1  ! Root data dir for 1x1 emissions
      CHARACTER(LEN=255) :: GCAP_DIR      ! Subdir for GCAP met data
      CHARACTER(LEN=255) :: GEOS_1_DIR    ! !%%% OBSOLETE %%%
      CHARACTER(LEN=255) :: GEOS_S_DIR    ! !%%% OBSOLETE 
      CHARACTER(LEN=255) :: GEOS_3_DIR    ! Subdir for GEOS-3 met data
      CHARACTER(LEN=255) :: GEOS_4_DIR    ! Subdir for GEOS-4 met data
      CHARACTER(LEN=255) :: GEOS_5_DIR    ! Subdir for GEOS-5 met data
      CHARACTER(LEN=255) :: TEMP_DIR      ! Temp dir for unzipping met data
      CHARACTER(LEN=255) :: RUN_DIR       ! Run directory for GEOS-Chem
      CHARACTER(LEN=255) :: OH_DIR        ! Dir w/ mean OH files
      CHARACTER(LEN=255) :: O3PL_DIR      ! Dir w/ archived O3 P/L rate files 
      CHARACTER(LEN=255) :: TPBC_DIR      ! Dir w/ TPCORE boundary conditions
!
! !REVISION HISTORY:
!  20 Jul 2004 - R. Yantosca - Initial version
!  25 May 2005 - R. Yantosca - Added variables GCAP_DIR and GEOS_5_DIR
!  24 Oct 2005 - R. Yantosca - Added DATA_DIR_1x1
!  20 Nov 2009 - R. Yantosca - Added ProTeX header
!EOP
!------------------------------------------------------------------------------
!BOC
      END MODULE DIRECTORY_MOD
!EOC
