Reference Information
=====================

Provenance for this README
--------------------------

* File name: README.md
* Authors: Wouter M.G. Vansteelant
* Other contributors: L. Gangoso, D. Viana, J.Z. Shamoun-Baranes, J. Figuerola
* Date created: 2023-04-30

- - -

Accompanying Paper and Data
---------------------------

* Paper Title: "A trans-African migrant shows repeatable route choice in males and repeatable timing in females"

* Paper identifier: 10.1111/jav.03050

* Dataset Title: Data for the article "A trans-African migrant shows repeatable route choice in males and repeatable timing in females"

* Persistent Identifier: https://doi.org/10.5061/dryad.2bvq83btg

* Dataset Contributors:

  * Creators: Wouter M.G. Vansteelant, L. Gangoso, D. Viana, J.Z. Shamoun-Baranes, J. Figuerola

* Date of Issue: 2023-04-30

- - -

Methodological Information
==========================

* Methods of data collection/generation: see manuscript for details

- - -

Instructions for replicating analyses
=====================================

Code/Software
-----------------
Code in this GitHub repository includes all steps to further process open access tracking data and metada, to combine these with additional third-party date, and to replicate analyses and visualisatios.

Setup
-----
 * store all codes in a dedicated working directory
 * store tracking and metadata made available through Dryad in a subfolder 'Data' of your working directory 
 * store third-party environmental data in a subfolder 'Environmental_Data' on main hard drive, or store in local working directory and adjust code to read data sources from appropriate folder. 
 * open R Studio and create R project in the same working directory
 * run scripts one by one in order indicated by first 2-3 symbols of script. Scripts p1 and p1b were used for downloading full tracking data and pre-processing (resampling). To replicate analyses, start from p2, and proceed by runnings scripts p2b, p2c, p3...

The code automatically installs all required packages for reproducing the analyses. 

Pre-processed tracking data and metadata on Dryad
==================================================

Details for: EF-resampled-v20210322b.csv
---------------------------------------
* Description: a comma-delimited file containing the tracking data of 19 Eleonora's falcons for which we recorded at least one full migration cycle, resampled to an hourly resolution. 

* Format(s): .csv

* Size(s): 19.58 MB

* Variables:
  * cycle: identifier of migration cycle, defined as 2nd half of year x (incl the outbound migration) and first half of year x+1 (incl the return migration)
  * tripID: identifier of unique trips, composed from columns dev, yr and out/return
  * indday: identifiers for unique birddays, composed of columns dev and a day number with 0000 = 1 Jan 2012
  * dev: identifier of UvA-BiTS device (each value also corresponds to an unique individual)
  * dt: timestamp in UTC
  * lat: latitude (decimal degrees, WGS84)
  * long: longtiude (decimal degrees, WGS84)
  * alt: altitude recorded by GPS relative to mean sea level
  * date: date
  * mth: month
  * yr: year
  * yday: day of year (0-365)
  * dist.to.colony: distance to colony in meters
  * trip: factor indicating whether data belongs to outward (out) or return migration
  * country: name of country in which location was recorded (NA = no value = over sea)
  * travel: discrete value indicating whether data pertains to a travel bout (1) or stop-over event (0)
  * phase: factor indicating whether data belongs to summer period (summer), outward migration (out), winter period (winter), or return migration (return). This data file is clipped to outward and return migration data only
  * cycle.time: numeric value indicating decimal days passed since start of each migration period (calculated from July 1st of year x in cycle)


Details for: Metadata_full-v20201207b.csv
----------------------------------------
* Description: a comma-delimited file containing the individual metadata of Canarian falcons.

* Format(s): .csv

* Size(s): 576 Bytes

* Variables:
  * dev: identifier of UvA-BiTS device (each value also corresponds to an unique individual)
  * colony: all from Canaries
  * age: all adult
  * sex: male/female

Third-party data
-----------------
Links to other publicly accessible third-party data:
  * country borders via: https://www.naturalearthdata.com/downloads/50m-cultural-vectors/
  * lake borders via: https://www.naturalearthdata.com/downloads/50m-physical-vectors/
  * NDVI composites Africa: reproduce with GGE code here: https://static-content.springer.com/esm/art%3A10.1186%2Fs40462-021-00272-8/MediaObjects/40462_2021_272_MOESM3_ESM.docx
  * 30 arcsec DEM: https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-global-30-arc-second-elevation-gtopo30

- - -
END OF README
