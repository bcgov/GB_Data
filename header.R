# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


library(sf)
library(dplyr)
library(plyr)
library(readr)
library(raster)
library(bcmaps)
library(fasterize)
library(tidyr)
library(rio)
library(WriteXLS)
library(readxl)
library(openxlsx)
library(rgdal)
library(RColorBrewer)

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')
figsOutDir <- file.path(OutDir,'figures')
DataDir <- 'data'
StrataDir <- file.path(DataDir,'Strata')
BTMDir <- file.path(DataDir,'BTM')
BearsCEDir <- file.path(DataDir,'BearsCE')
LandFormDir <- file.path(DataDir,'LandForm')


dir.create(file.path(OutDir), showWarnings = FALSE)
dir.create(file.path(dataOutDir), showWarnings = FALSE)
dir.create(file.path(spatialOutDir), showWarnings = FALSE)
dir.create(file.path(StrataOutDir), showWarnings = FALSE)
dir.create(file.path(figsOutDir), showWarnings = FALSE)
dir.create(DataDir, showWarnings = FALSE)
dir.create("tmp", showWarnings = FALSE)

#Required packages
options(scipen=3)
require(sp)
gpclibPermit()
require(raster)
require(maptools)
#require(rgdal)
require(shapefiles)
require(gpclib)
require(RColorBrewer) # creates nice color schemes
require(e1071) #package needed for classInt
require(classInt) # finds class intervals for continuous variables
require(png) # next 3 for pulling in google maps
require(RJSONIO)
require(RgoogleMaps)
require(maps) # for scale bar
require(rgeos)
require(GISTools)
require(Hmisc)
library(Hmisc)
require(corrgram)
require(aod)


