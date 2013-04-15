! $Id: get_global_ch4.f,v 1.1 2009/09/16 14:06:26 bmy Exp $
      SUBROUTINE GET_GLOBAL_CH4( THISYEAR, VARIABLE_CH4, 
     &                           A3090S, A0030S, A0030N, A3090N,
     &                           am_I_Root )
!
!******************************************************************************
!  Subroutine GET_GLOBAL_CH4 computes the latitudinal gradient in CH4
!  corresponding to year (jsw, bnd, bmy, 1/3/01, 1/25/08)
!
!  Arguments as Input:
!  ===========================================================================
!  (1 ) THISYEAR     (INTEGER) : Current month number (1-12)
!  (2 ) VARIABLE_CH4 (LOGICAL) : Flag for selecting variable or constant CH4
!
!  Arguments as Output:
!  ===========================================================================
!  (3 ) A3090S      (REAL*8 ) : CH4 concentration [ppbv], 90S - 30S lat
!  (4 ) A0030S      (REAL*8 ) : CH4 concentration [ppbv], 30S - 00  lat
!  (5 ) A0030N      (REAL*8 ) : CH4 concentration [ppbv], 00  - 30N lat
!  (6 ) A3090N      (REAL*8 ) : CH4 concentration [ppbv], 30N - 90N lat
!
!  NOTES:
!  (1 ) GET_GLOBAL_CH4 only has to be called at the start of the new year,
!        as long as A3090S, A0030S, A0030N, A3090N are saved in the
!        calling program (bmy, 1/3/01)
!  (2 ) Also need to compute yearly gradients for CH4 beyond 1997 --
!        will do this later (bmy, 1/3/01)
!  (3 ) Bug fix: add missing comma to FORMAT statement (bmy, 3/23/03)
!  (4 ) Place WRITE statments w/in an !$OMP CRITICAL block, so as to make
!        sure that only one processor at a time writes them.  Also now use
!        F90 REPEAT intrinsic function.  Also replaced old CH4 gradient values
!        with updated values for 1983-2001.  Use data for 2001 as a proxy for
!        years past 2001, since data for those years has not been reported
!        yet. (mje, bmy, 7/7/03)
!  (5 ) Split off from module "global_ch4_mod.f".  Updated for IPCC future
!        emissions scenarios. (swu, bmy, 5/30/06)     
!  (6 ) Add the preindustrial CH4 scenarios.  Also set 2001 as the default
!        in case we are running 2030 or 2050 met but present-day emissions.
!        (swu, havala, bmy, 1/25/08)
!  (7 ) Updated CH4 vales with version 2008-07-02 for 1983-2007. Also use
!        2007 for years past 2007 (jaf, 4/15/09) 
!  30 Jul 2012 - R. Yantosca - Now accept am_I_Root as an argument when
!                              running with the traditional driver main.F
!  15 Apr 2013 - J.A. Fisher - Updated CH4 values to 2010 (v 2012-09-24)
!******************************************************************************
!
      ! References to F90 modules
      USE FUTURE_EMISSIONS_MOD, ONLY : GET_FUTURE_SCENARIO
      USE LOGICAL_MOD,          ONLY : LFUTURE

      IMPLICIT NONE
#     include "define.h"

      ! Arguments
      INTEGER, INTENT(IN)           :: THISYEAR
      LOGICAL, INTENT(IN)           :: VARIABLE_CH4
      LOGICAL, INTENT(IN)           :: am_I_Root
      REAL*8,  INTENT(OUT)          :: A3090S, A0030S, A0030N, A3090N

      ! Local variables
      CHARACTER(LEN=2)              :: FUTURE_SCENARIO

      !=================================================================
      ! GET_GLOBAL_CH4 begins here!
      !
      ! New methane data from 1983-2010 (jaf, 4/15/13)
      ! version is 2012-09-24
      !
      ! Methane measurements are from CMDL website:
      ! ftp://140.172.192.211/ccg/ch4/flask/month
      ! 
      ! Measurements includes all sites other than:
      ! BAL BSC HUN MHD OXK TAP SEY IZO KUM MID ASK
      !
      ! Now also exclude recent sites since their representativeness
      ! hasn't been tested:
      ! ABP BKT HPB LLB LLN LMP MEX MKN OXK
      !
      ! Sites are separated into 4 latitude bands:
      !    (1) 90S - 30S;  (2) 30S - 00S;  
      !    (3) 00N - 30N;  (4) 30N - 90N
      ! 
      ! Bob Yantosca (bmy@io.harvard.edu) maintains the archive 
      ! of the IDL code needed to process the methane data.
      !
      ! Also add future emission scenarios for GCAP, as well as
      ! the preindustrial CH4 levels (swu, havala, bmy, 1/25/08)
      !=================================================================
      IF ( VARIABLE_CH4 ) THEN

         ! Get IPCC future scenario (e.g. A1, A2, B1, B2)
         IF ( LFUTURE ) THEN
            FUTURE_SCENARIO = GET_FUTURE_SCENARIO()
         ENDIF

         ! Select latitudinal CH4 gradient by year...
         SELECT CASE ( THISYEAR )

            ! Preindustrial years
            CASE ( :1750 )
               A3090S = 700.0d0
               A0030S = 700.0d0
               A0030N = 700.0d0
               A3090N = 700.0d0
               
            ! Modern-day years ...
            CASE( 1983 )
               A3090S = 1578.69d0
               A0030S = 1595.22d0
               A0030N = 1647.22d0
               A3090N = 1703.16d0

            CASE( 1984 )
               A3090S = 1597.30d0
               A0030S = 1606.60d0
               A0030N = 1655.49d0
               A3090N = 1723.74d0

            CASE( 1985 )
               A3090S = 1608.05d0
               A0030S = 1620.45d0
               A0030N = 1668.14d0
               A3090N = 1737.10d0
               
            CASE( 1986 )
               A3090S = 1619.61d0  
               A0030S = 1632.66d0  
               A0030N = 1682.51d0  
               A3090N = 1752.71d0

            CASE( 1987 )
               A3090S = 1630.27d0  
               A0030S = 1640.19d0  
               A0030N = 1702.95d0  
               A3090N = 1763.01d0

            CASE( 1988 )
               A3090S = 1642.08d0  
               A0030S = 1651.58d0  
               A0030N = 1713.16d0  
               A3090N = 1775.74d0

            CASE( 1989 )
               A3090S = 1653.95d0  
               A0030S = 1666.10d0  
               A0030N = 1720.47d0  
               A3090N = 1781.29d0

            CASE( 1990 )
               A3090S = 1663.22d0  
               A0030S = 1672.43d0  
               A0030N = 1733.85d0  
               A3090N = 1791.88d0

            CASE( 1991 )
               A3090S = 1673.51d0  
               A0030S = 1683.85d0  
               A0030N = 1748.17d0  
               A3090N = 1800.77d0

            CASE( 1992 )
               A3090S = 1687.93d0  
               A0030S = 1693.00d0  
               A0030N = 1756.13d0  
               A3090N = 1807.84d0

            CASE( 1993 )
               A3090S = 1687.80d0  
               A0030S = 1696.48d0  
               A0030N = 1758.76d0  
               A3090N = 1810.57d0

            CASE( 1994 )
               A3090S = 1692.76d0  
               A0030S = 1700.87d0  
               A0030N = 1767.02d0  
               A3090N = 1817.17d0

            CASE( 1995 )
               A3090S = 1700.88d0  
               A0030S = 1711.02d0  
               A0030N = 1778.23d0  
               A3090N = 1822.23d0

            CASE( 1996 )
               A3090S = 1701.96d0  
               A0030S = 1711.02d0  
               A0030N = 1778.22d0  
               A3090N = 1825.37d0
            
            CASE( 1997 )
               A3090S = 1708.06d0  
               A0030S = 1714.95d0  
               A0030N = 1781.31d0  
               A3090N = 1825.04d0

            CASE( 1998 )
               A3090S = 1716.41d0  
               A0030S = 1725.53d0  
               A0030N = 1783.76d0  
               A3090N = 1839.81d0

            CASE( 1999 )
               A3090S = 1725.81d0  
               A0030S = 1734.35d0  
               A0030N = 1791.53d0  
               A3090N = 1842.52d0

            CASE( 2000 )
               A3090S = 1728.14d0  
               A0030S = 1737.37d0  
               A0030N = 1792.40d0  
               A3090N = 1840.99d0

            CASE( 2001 )
               A3090S = 1726.92d0  
               A0030S = 1729.79d0  
               A0030N = 1789.07d0  
               A3090N = 1843.49d0

            CASE( 2002 )
               A3090S = 1729.79d0
               A0030S = 1735.34d0
               A0030N = 1789.92d0
               A3090N = 1845.44d0

            CASE( 2003 )
               A3090S = 1729.72d0
               A0030S = 1735.47d0
               A0030N = 1795.82d0
               A3090N = 1857.04d0

            CASE( 2004 )
               A3090S = 1728.77d0
               A0030S = 1738.59d0
               A0030N = 1797.41d0
               A3090N = 1852.52d0

            CASE( 2005 )
               A3090S = 1729.72d0
               A0030S = 1735.47d0
               A0030N = 1795.82d0
               A3090N = 1853.79d0

            CASE( 2006 )
               A3090S = 1726.63d0
               A0030S = 1735.32d0
               A0030N = 1796.37d0
               A3090N = 1852.39d0

            CASE( 2007 )
               A3090S = 1732.62d0
               A0030S = 1741.91d0
               A0030N = 1801.30d0
               A3090N = 1860.62d0

            CASE( 2008 )
               A3090S = 1740.54d0
               A0030S = 1748.16d0
               A0030N = 1807.91d0
               A3090N = 1868.26d0

            CASE( 2009 )
               A3090S = 1743.98d0
               A0030S = 1753.05d0
               A0030N = 1816.43d0
               A3090N = 1872.96d0

            CASE( 2010 )
               A3090S = 1750.15d0
               A0030S = 1759.71d0
               A0030N = 1820.59d0
               A3090N = 1878.29d0

            ! Future year 2030
            CASE( 2025:2035 )
            
               ! Pick the IPCC scenario.  If LFUTURE=F and FUTURE_SCENARIO
               ! are undefined, then we are running 2030 meteorology with 
               ! present-day emissions.  In this case, default to 2010 CH4 
               ! concentrations. (havala, 1/25/08)
               SELECT CASE( FUTURE_SCENARIO )
                  CASE( 'A1' )
                     A3090S = 2202.0d0 
                     A0030S = 2202.0d0
                     A0030N = 2202.0d0
                     A3090N = 2202.0d0 
                  CASE( 'B1' )
                     A3090S = 1927.0d0 
                     A0030S = 1927.0d0
                     A0030N = 1927.0d0
                     A3090N = 1927.0d0 
                  CASE( 'A2' )
                     ! Not defined yet
                  CASE( 'B2' )
                     ! Not defined yet
                  CASE DEFAULT
                     ! 2010 is the default
                     A3090S = 1750.15d0
                     A0030S = 1759.71d0
                     A0030N = 1820.59d0
                     A3090N = 1878.29d0
               END SELECT

            ! Future year 2050
            CASE( 2045:2055 )

               ! Pick the IPCC scenario.  If LFUTURE=F and FUTURE_SCENARIO
               ! is undefined, then we are running 2050 meteorology with 
               ! present-day emissions.  In this case, default to 2010 CH4 
               ! concentrations. (havala, 1/25/08)
               SELECT CASE ( FUTURE_SCENARIO )
                  CASE ( 'A1' )
                     A3090S = 2400.0d0 
                     A0030S = 2400.0d0
                     A0030N = 2400.0d0
                     A3090N = 2400.0d0 
                  CASE ( 'B1' )
                     A3090S = 1881.0d0 
                     A0030S = 1881.0d0
                     A0030N = 1881.0d0
                     A3090N = 1881.0d0 
                  CASE ( 'A2' )
                     A3090S = 2562.0d0 
                     A0030S = 2562.0d0
                     A0030N = 2562.0d0
                     A3090N = 2562.0d0
                  CASE ( 'B2' )
                     A3090S = 2363.0d0 
                     A0030S = 2363.0d0
                     A0030N = 2363.0d0
                     A3090N = 2363.0d0
                  CASE DEFAULT
                     ! 2010 is the default
                     A3090S = 1750.15d0
                     A0030S = 1759.71d0
                     A0030N = 1820.59d0
                     A3090N = 1878.29d0
               END SELECT

            ! Default is to use 2010 data for other years
            ! for which we do not yet have data (jaf, 4/15/09)
            CASE DEFAULT
               A3090S = 1750.15d0
               A0030S = 1759.71d0
               A0030N = 1820.59d0
               A3090N = 1878.29d0

         END SELECT

      ELSE
         
         ! ...otherwise assume constant global CH4
         A3090S = 1700.0d0
         A0030S = 1700.0d0
         A0030N = 1700.0d0
         A3090N = 1700.0d0
         
      ENDIF

      !=================================================================
      ! Print the latitudinal CH4 gradient for this year to stdout
      !=================================================================
!$OMP CRITICAL
      IF ( am_I_Root ) THEN
         WRITE( 6, '(a)' ) REPEAT( '=', 79 )
         WRITE( 6, 105   ) THISYEAR
 105     FORMAT( 'GET_GLOBAL_CH4: YEAR = ', i4 )

         WRITE( 6, 110 ) A3090N, A0030N, A0030S, A3090S 
 110     FORMAT( 'CH4 (90N - 30N) : ', f7.1, ' [ppbv]', /,
     &           'CH4 (30N - 00 ) : ', f7.1, ' [ppbv]', /,
     &           'CH4 (00  - 30S) : ', f7.1, ' [ppbv]', /,
     &           'CH4 (30S - 90S) : ', f7.1, ' [ppbv]' )

         ! Indicate to the log file if we are using CH4 gradient data
         ! from 2010 as a proxy for years past 2010 (mje, bmy, 7/7/03,
         ! jaf, 4/15/09)
         IF ( THISYEAR > 2010 ) THEN
            WRITE( 6, 115 ) 
 115        FORMAT( /, 'Using CH4 gradient data from 2010 as a proxy',
     &           /, 'since 2010 is the last year with reported data!' )
         ENDIF

         WRITE( 6, '(a)' ) REPEAT( '=', 79 )
      ENDIF
!$OMP END CRITICAL

      ! Return to calling program
      END SUBROUTINE GET_GLOBAL_CH4
