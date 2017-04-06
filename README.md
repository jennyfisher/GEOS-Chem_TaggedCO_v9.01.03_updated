# GEOS-Chem_TaggedCO_v9.01.03_updated

This repository contains the code for the version of the GEOS-Chem "tagged" CO only simulation
described in Fisher et al., 2017, in review for Geoscientific Model Development.

The base version is GEOS-Chem v9-01-03 but has been updated here as described in the paper.
Relative to the standard tagged CO capability, this version is different in that it uses
full chemistry OH and calculates secondary CO production from full chemistry CO production fields,
separated into methane and NMVOC contributions. Other small bug fixes and updates that were
implemented in v11-01 (no tagged CO capability in v10-01) are included.

A more updated version of this code built on v11-01, as well as ancillary run files including
OH fields, P(CO) fields, HEMCO_Config.rc, and input.geos are available by contacting 
Jenny Fisher (jennyf@uow.edu.au). This version will be implemented in the public GEOS-Chem code
(likely in v11-02 or later) following acceptance of the manuscript.

J. Fisher, April 2017.
