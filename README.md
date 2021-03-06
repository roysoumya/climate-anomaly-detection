# Anomaly detection and Summarisation of various extreme weather events over the Indian sub-continent
## Introduction
This repository contains all the R codes that I have written during my summer internship at IIT Kharagpur. The data is spatio-temporal in nature, with the spatial projection over only the Indian sub-continent. Most of the codes deal with the pre-processing stage(i.e, converting .nc to a 2-D workable .csv format) and application of gating techniques. Somes codes contain implementation of the grid-wise spatio-temporal clustering. The R program containing the cSTAG implementation is intentionally not provided due to privacy issues.

All the uploaded codes can be freely used by everyone. Hope it helps.

## Abstract
Anomalous weather events occur very rarely but they have a huge impact on human life. The aim of this study is to characterise the anomalous weather events over the Indian sub-continent using the climatological variables(surface air temperature, uwind, vwind,relative humidity and omega), so that their period of occurrence and their trajectory can be more accurately predicted, at a nascent stage. In this study, we use the Seasonal Hybrid Extreme Studentized Test to detect outliers during the period 1951 to 2014. It is most appropriate as it considers both the seasonal and trend aspect of the temporal, univariate data and is also highly robust in the presence of anomalies. The naive spatio-temporal clustering is used to aggregate the anomalies, based on their spatial and temporal context, which are later used to evaluate and also validate the anomaly detection technique. The cSTAG algorithm is then used to discover regions of correlated spatio-temporal change, so that wecan first separate and then characterise the events that caused them. The results are visualised over the map of India and we find that it aptly portrays the evolution of a particular event over consecutive days. These results can be used by domain experts to better study the
behavior of these extreme events. Through this project, we have been able to detect extreme weather events like heat waves, rainstorms over the Indian sub-continent as well as validated it with real-time data. Weather events like floods,fog are not detected by the method that we used. Due to lack of data regarding reported cold wave events, a large number of them cannot be validated.

## Dataset description
The weather data used for the study is collected from the NCEP/NCAR website and was downloaded from [data](http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.pressure.html) in NetCDF format. The atmospheric variables : surface air temperature, uwind, vwind, relative humidity and omega, provided at the pressure level of 925 hPa was downloaded in separate files. The dataset contain records taken from 1 st January,1951 to 31 st December, 2014. It covers the area from 5°N to 40°N and from 65°E to 100°E. Each grid has a spatial coverage of 2.5° x 2.5°.
### Global attributes:
  1. conventions = “COARDS”
  2. Description = Data is from NMC reanalysis (4 times/day). It consists of most variablesinterpolated to pressure surfaces from model(sigma) surfaces.
  
## Case Studies
### 1.La Nina event :
We studied this event from 22 nd to 28 th December,1999. Strong effect of La-Nina was seen till January, 2000 around Andaman and Nicobar Islands and Malaysia.

  **Function call :** cstag(temp_data, 71549, 71573, 5, 15, 9, 15, param= 'air_temp')
  
  **Observation :** A large portion is blank because the region was already going through a period of disturbance as stated in one report. The change in red and yellow events shows two simultaneous events developing over time and at the end, the disturbances seem to disappear.
  
![Alt](/Pictures/Event-1/1.1.png "Title")![Alt](/Pictures/Event-1/1.2.png "Title")![Alt](/Pictures/Event-1/1.3.png "Title")
![Alt](/Pictures/Event-1/1.4.png "Title")![Alt](/Pictures/Event-1/1.5.png "Title")![Alt](/Pictures/Event-1/1.6.png "Title")
![Alt](/Pictures/Event-1/legend1.png "Title")

### 2. Tornado :
We studied this event from 26 th October to 1 st November,1999. On 29th October, 1999 a tornado was reported at Midnapore and South 24 Pargana districts, close to Paradeep which injured 80 people.

  **Function call :** cstag(omega_data, 71321, 71352, 7, 11, 7, 12, param = 'omega')
  
  **Observation :** We find that region at the reported location of the tornado is showing uncorrelated behavior with the surrounding areas and also with adjacent time steps. This indicates turbulent behavior and also we see they persist over the specific area during the period of the tornado. Most importantly, all this disturbances disappear at the end, which indicates a strong and positive detection.

![Alt](/Pictures/Event-2/2.1.png "Title")![Alt](/Pictures/Event-2/2.2.png "Title")![Alt](/Pictures/Event-2/2.3.png "Title")
![Alt](/Pictures/Event-2/2.4.png "Title")![Alt](/Pictures/Event-2/2.5.png "Title")![Alt](/Pictures/Event-2/2.6.png "Title")
![Alt](/Pictures/Event-2/legend2.png "Title")
