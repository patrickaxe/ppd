ppd:  R package containing functions for accessing SILO Patched Point weather data using their URL Web Service
====================================================

Last updated (original: 2019-07-26; patrickaxe: 2021-10-02)

See https://longpaddock.qld.gov.au/silo/ for information on SILO Patched Point data.

Includes a data frame STATIONS containing weather stations with good long term records in south west Western Australia.

Functions developed by Fiona Evan's original package:
* nearestStations: Find the nearest stations (in STATIONS) to a given longitude and latitude
* getPPD: Download PPD data from SILO and store as a data frame
* ppd2apsimMET: Convert downloaded PPD data to APSIM .met file
* getDPIRDstations: Download list of DPIRD weather stations
* getDPIRDdaily: Download DPIRD daily weather data for one year (may contain missing values)
* getDPIRDhourlyByDay: Download DPIRD daily weather data for a period of one day (may contain missing values)
* getDPIRDhourlyByMonth: Download DPIRD daily weather data for a period of one month (may contain missing values)
* getDPIRDhourlyByYear: Download DPIRD daily weather data for a period of one year (may contain missing values)
* getSCIENCEstations: Download list of DPIRD SCIENCE weather stations - contains Patched Point and DPIRD automatic weather stations
* getRTD: Download 'rainfall to date' data using DPIRD SCIENCE API
* getSoilWater: Download  'soil water' data using DPIRD SCIENCE API

Planned future functionality:
* function getClimatology is in development - do not use

Further functions developed by patrickaxe for project-specific purposes to produce .met files for APSIM.
* metProducer: prodcue a .met file all around Australia from closest PPD station data 
* westdale_met: produce a .met file for West Dale (WA) local weather station (https://avachat.info/WDDAILY.txt)
* callington_met: produce a .met file for Callington (SA) local weather station (data downloaded from WildEye API in a XML format)


Note: DPIRD = Department of Primary Industries and Regional Development, Western Australia. See https://www.agric.wa.gov.au/web-apis 

Note: If you want to obtain current weather observation data or forecasts from BoM, see https://github.com/ropensci/bomrang (PS: it may not be working as in May 2021).

