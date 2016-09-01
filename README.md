# climate-anomaly-detection
**Introduction**
This repository contains all the R codes that I have written during my summer internship at IIT Kharagpur.
Project name : Anomaly detection and Summarisation of various extreme weather events over Indian sub-continent
The data that is dealt with is a spatio-temporal climate over the Indian sub-continent.

#Dataset description
The weather data used for the study is collected from the NCEP/NCAR website and was downloaded from [data](www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.pressure.html) in NetCDF format. The atmospheric variables : surface air temperature, uwind, vwind, relative humidity and omega, provided at the pressure level of 925 hPa was downloaded in separate files. The dataset contain records taken from 1 st January,1951 to 31 st December, 2014. It covers the area from 5°N to 40°N and from 65°E to 100°E. Each grid has a spatial coverage of 2.5° x 2.5°.
**Global attributes:**
  1. conventions = “COARDS”
  2. Description = Data is from NMC reanalysis (4 times/day). It consists of most variablesinterpolated to pressure surfaces from model(sigma) surfaces.
