#------------------------------------------------------------------------------
#          Harvard University Atmospheric Chemistry Modeling Group            !
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: Makefile (in the Headers subdirectory)
#
# !DESCRIPTION: This makefile compiles the various GEOS-Chem Header modules,
#  which contain many PARAMETERs and global arrays for GEOS-Chem routines.
#
# !REMARKS:
# To build the programs, call "make" with the following syntax:
#                                                                             .
#   make TARGET [ OPTIONAL-FLAGS ]
#                                                                             .
# To display a complete list of options, type "make help".
#                                                                             .
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%% NOTE: Normally you will not have to call this Makefile directly,     %%%
# %%% it will be called automatically from the Makefile in the directory   %%%
# %%% just above this one!                                                 %%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                                                             .
# Makefile uses the following variables:
#                                                                             .
# Variable   Description
# --------   -----------
# SHELL      Specifies the shell for "make" to use (usually SHELL=/bin/sh)
# ROOTDIR    Specifies the root-level directory of the GEOS-Chem code
# HDR        Specifies the directory where GEOS-Chem include files are found
# LIB        Specifies the directory where library files (*.a) are stored
# MOD        Specifies the directory where module files (*.mod) are stored
# AR         Sys var w/ name of library creator program (i.e., "ar", "ranlib")
# MAKE       Sys var w/ name of Make command (i.e, "make" or "gmake")
#
# !REVISION HISTORY: 
#  23 Aug 2011 - M. Long     - Initial version
#  09 Apr 2012 - R. Yantosca - Removed CMN_VEL_mod.F; it's obsolete
#EOP
#------------------------------------------------------------------------------
#BOC

# Define variables
ROOTDIR = ..
HDR     = $(ROOTDIR)/Headers
HELP    = $(ROOTDIR)/help
LIB     = $(ROOTDIR)/lib
MOD     = $(ROOTDIR)/mod

# Include header file.  This returns CC, F90, FREEFORM, LD, R8, SHELL,
# as well as the default Makefile compilation rules for source code files.
include $(ROOTDIR)/Makefile_header.mk

#=============================================================================
# List of files to compile.  Here the order is not important,
# as we will explicity define the dependencies listing below.
#=============================================================================

# List of source files
SRC = $(wildcard *.F) $(wildcard *.F90)

# Replace .f and .f90 extensions with *.o
TMP = $(SRC:.F=.o)
OBJ = $(TMP:.F90=.o)

#=============================================================================
# Makefile targets: type "make help" for a complete listing!
#=============================================================================

.PHONY: clean help

lib: $(OBJ)
	$(AR) crs libHeaders.a $(OBJ)
	mv libHeaders.a $(LIB)

clean:
	rm -f *.o *.mod

help:
	@$(MAKE) -C $(HELP)

#=============================================================================
# Dependencies listing (grep "USE " to get the list of module references!)
#
# From this list of dependencies, the "make" utility will figure out the
# correct order of compilation (so we don't have to do that ourselves).
# This also allows us to compile on multiple processors with "make -j".
#
# NOTES:
# (1) Only specify object-file dependencies that are within this directory.  
#      Object files in other directories will be referenced at link-time.
# (2) For "make -j" to work, all files in this directory must have a 
#      listed dependency.
#=============================================================================
#
CMN_DEP_mod.o       : CMN_DEP_mod.F          CMN_SIZE_mod.o

CMN_DIAG_mod.o      : CMN_DIAG_mod.F         CMN_SIZE_mod.o

CMN_GCTM_mod.o      : CMN_GCTM_mod.F	   

CMN_ISOP_mod.o      : CMN_ISOP_mod.F         CMN_SIZE_mod.o

CMN_MONOT_mod.o     : CMN_MONOT_mod.F        CMN_SIZE_mod.o

CMN_NOX_mod.o       : CMN_NOX_mod.F          CMN_SIZE_mod.o

CMN_O3_mod.o        : CMN_O3_mod.F           CMN_SIZE_mod.o

CMN_SIZE_mod.o      : CMN_SIZE_mod.F       

CMN_mod.o           : CMN_mod.F              CMN_SIZE_mod.o

cmn_fj_mod.o        : cmn_fj_mod.F           CMN_SIZE_mod.o

commsoil_mod.o      : commsoil_mod.F         CMN_SIZE_mod.o

comode_loop_mod.o   : comode_loop_mod.F      CMN_SIZE_mod.o

jv_cmn_mod.o        : jv_cmn_mod.F           CMN_SIZE_mod.o  \
                                             cmn_fj_mod.o

jv_mie_mod.o        : jv_mie_mod.F

smv_dimension_mod.o : smv_dimension_mod.F

smv_errcode_mod.o   : smv_errcode_mod.F

smv_physconst_mod.o : smv_physconst_mod.F

#EOC
