#  README

Dec 17, 2021

Function in this package were originally taken
from the airMotive package, contextModels, heR.Activity,
and annotatoR packages, whichever had the newest
version as of Dec 17, 2021.  [also folding stuff back
in from contextualizeR and airMotiveST]

The 'tact' and 'mtact' (and related) legacy functions from heR.Activity
are included so the old lattic-based plot.monitors function works, but 
they are deprecated for new code.  We are using the tibble
as the base rectangular object for time-activity data.

# sensoRplot

This R package contains functions to plot data on sensor levels and locations,
i.e., any discrete or continuous data derived from "sensing" or "observing" physical, social,
biometric, or environmental phenomena in time and space, including data from chemical sensors,
sensors of physical conditions, human activity sensors, biosensors, or data observed and recorded
directly by a human being using their eyes, ears, mouth, nose, skin, etc. acting as "sensors".
It has functions to plot static and interactive discrete contextual timelines and
continuous data streams, as well as combination plots containing data for both
streams and contexts.  It has function to plot static or interactive maps showing
the locations/trajectories and levels of sensors. It has functions to fit continuous 
sensor data to a lognormal distribution and create log-probability and density
plots of sensor levels.  It includes helper functions to convert data between standard formats
and process or manipulate data files.  It uses the ggplot2, lattice, plotly, and leaflet libraries to
generate both static and interactive plots and maps.
