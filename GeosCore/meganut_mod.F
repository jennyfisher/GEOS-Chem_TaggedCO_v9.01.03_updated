!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: meganut_mod
!
! !DESCRIPTION: Module MEGANUT\_MOD contains functions used by MEGAN.
!\\
!\\
! !INTERFACE:
!
      MODULE MEGANUT_MOD
!
! !USES:
!
      IMPLICIT NONE
#     include "define.h"
      PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
      PUBLIC :: XLTMMP
      PUBLIC :: XLPARDF
      PUBLIC :: XLPARDR
!
! !REVISION HISTORY
!  20 Nov 2009 - C. Carouge - Create the module with xltmmp, xlpardf and 
!                             xlpardr functions.
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
! !FUNCTION: xltmmp
!
! !DESCRIPTION: Function XLTMMP passes the value of the DAO meterological 
!  field TS(IIPAR,JJPAR) back to the calling subroutine.  This preserves the 
!  functionality of the H/G/I CTM function XLTMMP.  XLTMMP is written in 
!  Fixed-Form Fortran 90.  I, J are the long/lat indices of the grid box.  
!  IJLOOP is passed in order to maintain compatibility with the H/G/I 
!  subroutines, but is not used. 
!\\
!\\
! !INTERFACE:
!
      FUNCTION XLTMMP( I, J, IJLOOP ) RESULT( VALUE )
!
! !USES:
!
      USE DAO_MOD, ONLY : TS
      
      USE CMN_SIZE_MOD
!
! !INPUT PARAMETERS:
!
      INTEGER, INTENT(IN)           :: I, J
      INTEGER, INTENT(IN), OPTIONAL :: IJLOOP
! 
! !RETURN VALUE:
!
      REAL*8                        :: VALUE
!
! !REVISION HISTORY:
!
!                              Use C-preprocessor #include statement to 
!                              include CMN_SIZE, which has IIPAR, JJPAR, 
!                              LLPAR, IIPAR, JJPAR, LGLOB.
!  23 Jun 2000 - R. Yantosca - Now reference TS from "dao_mod.f" instead of 
!                              from common block header file "CMN_TS". 
!  31 Aug 2000 - R. Yantosca - Eliminated obsolete code from 6/23/00
!  26 Sep 2001 - R. Yantosca - Now declare XLTMMP as REAL*8 w/in program body.
!                              Also updated comments.
!  24 Oct 2001 - R. Yantosca - Remove obsolete commented out code from 9/01
!  20 Jul 2004 - R. Yantosca - IJLOOP is now not declared optional...this 
!                              facilitates compiling with -C on Altix
!  04 Aug 2005 - R. Yantosca - Now make IJLOOP an optional argument; it's only 
!                              kept for backwards compatibility w/ older code
!------------------------------------------------------------------------------
!BOC
      VALUE = TS(I,J)

      END FUNCTION XLTMMP
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !FUNCTION: xlpardr
!
! !DESCRIPTION: Function XLPARDR passes the value of the DAO meterological 
!  field PARDR(IIPAR,JJPAR) back to the calling subroutine.  This preserves 
!  the functionality of the H/G/I CTM function PARDR.  I, J are the long/lat 
!  indices of the grid box. IJLOOP is passed in order to maintain compatibility
!  with the H/G/I subroutines, but is not used.
!\\
!\\
! !INTERFACE:
!
      FUNCTION XLPARDR( I, J, IJLOOP ) RESULT( VALUE )
!
! !USES
!
      USE DAO_MOD, ONLY : PARDR 
      
      USE CMN_SIZE_MOD
!
! !INPUT PARAMETERS 
!
      INTEGER, INTENT(IN)           :: I, J
      INTEGER, INTENT(IN), OPTIONAL :: IJLOOP
!
! !RETURN VALUE:
!
      REAL*8                        :: VALUE
!
! !REVISION HISTORY
!
! 20 Nov 2009 - M. Barkley - Original version
!
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES
!
      VALUE = PARDR(I,J)
 
      END FUNCTION XLPARDR
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !FUNCTION: xlpardf
!
! !DESCRIPTION: Function XLPARDF passes the value of the DAO meterological 
!  field PARDF(IIPAR,JJPAR) back to the calling subroutine.  This preserves 
!  the functionality of the H/G/I CTM function PARDF.  I, J are the long/lat 
!  indices of the grid box. IJLOOP is passed in order to maintain compatibility
!  with the H/G/I subroutines, but is not used.
!\\
!\\
! !INTERFACE:
!
      FUNCTION XLPARDF( I, J, IJLOOP ) RESULT( VALUE )
!
! !USES
!
      USE DAO_MOD, ONLY : PARDF 
      
      USE CMN_SIZE_MOD
!
! !INPUT PARAMETERS 
!
      INTEGER, INTENT(IN)           :: I, J
      INTEGER, INTENT(IN), OPTIONAL :: IJLOOP
!
! !RETURN VALUE:
!
      REAL*8                        :: VALUE

!
! !REVISION HISTORY
!  20 Nov 2009 - M. Barkley - Original version
!!EOP
!------------------------------------------------------------------------------
!BOC
      VALUE = PARDF(I,J)
 
      END FUNCTION XLPARDF
!EOC
      END MODULE MEGANUT_MOD
