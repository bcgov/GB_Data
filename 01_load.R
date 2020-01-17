# Copyright 2019 Province of British Columbia
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

source("header.R")

# Scritp reads in data sources for grizzly bears and stores them in sub-directories of 'data'
# so they can be accessed by other repos in the grizzly bear assessment rep family

#Rasterize the Province for subsequent masking
ProvRast<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5,
                 ymn=173787.5, ymx=1748187.5,
                 crs="+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0",
                 res = c(100,100), vals = 0)

# Generate a raster mask for only the Province
BCr_file <- file.path(StrataDir ,"BCr.tif")
if (!file.exists(BCr_file)) {
  BCr <- fasterize(bcmaps::bc_bound_hres(class='sf'),ProvRast)
  writeRaster(BCr, filename=BCr_file, format="GTiff", overwrite=TRUE)
} else {
  BCr <- raster(BCr_file)
}

# BTM for identifying rock, ice and water
BTM_file <- file.path("tmp/BTM_Brick")
if (!file.exists(BTM_file)) {
  # Link to BTM file download from BCDC:
  # https://catalogue.data.gov.bc.ca/dataset/baseline-thematic-mapping-present-land-use-version-1-spatial-layer
  #Dowload file manually and put *.zip in this script and place file in the data directory
  BTMZip <- 'BCGW_78757263_1520272242999_7572.zip'
  unzip(file.path(BTMDir, BTMZip), exdir = file.path(BTMDir, "BTM"))

  # List feature classes in the geodatabase
  BTM_gdb <- list.files(file.path(BTMDir), pattern = ".gdb", full.names = TRUE)[1]
  fc_list <- st_layers(BTM_gdb)
  BTM <- read_sf(BTM_gdb, layer = "WHSE_BASEMAPPING_BTM_PRESENT_LAND_USE_V1_SVW")

  # Pull out the BTM layers and rasterize
  #BTM[1,] #look at file header
  NonHab <- BTM[BTM$PRESENT_LAND_USE_LABEL %in% c('Fresh Water','Outside B.C.','Salt Water', 'Glaciers and Snow') ,] %>%
    fasterize(ProvRast, background=NA)

  AgricultureR <- BTM[BTM$PRESENT_LAND_USE_LABEL %in% c("Agriculture",'Residential Agriculture Mixtures') ,] %>%
    fasterize(ProvRast, background=0)

  RangeR <- BTM[BTM$PRESENT_LAND_USE_LABEL %in% c('Range Lands') ,] %>%
    fasterize(ProvRast, background=0)

  UrbanR <- BTM[BTM$PRESENT_LAND_USE_LABEL %in% c('Urban') ,] %>%
    fasterize(ProvRast, background=0)

  MiningR <- BTM[BTM$PRESENT_LAND_USE_LABEL %in% c('Mining') ,] %>%
    fasterize(ProvRast, background=0)

  RecR <- BTM[BTM$PRESENT_LAND_USE_LABEL %in% c('Recreation Activities') ,] %>%
    fasterize(ProvRast, background=0)

  # Write out the BTM layers as individual rasters
  writeRaster(AgricultureR, filename=file.path(spatialOutDir,"AgricultureR.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(RangeR, filename=file.path(spatialOutDir,"RangeR.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(UrbanR, filename=file.path(spatialOutDir,"UrbanR.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(MiningR, filename=file.path(spatialOutDir,"MiningR.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(RecR, filename=file.path(spatialOutDir,"RecR.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(NonHab, filename=file.path(StrataDir,"NonHab.tif"), format="GTiff", overwrite=TRUE)

  #Make a raster brick of layers - not using due to raster memory allocation bug
  BTM_Brick<- brick(AgricultureR, RangeR, UrbanR, MiningR, RecR)
  names(BTM_Brick) <- c('AgricultureR','RangeR','UrbanR','MiningR','RecR')
  saveRDS(BTM_Brick, file = BTM_file)

} else {
  BTM_Brick <- readRDS(file = BTM_file)
  NonHab<-raster(file.path(DataDir,"NonHab.tif"))
}

# Read in GB data - compiled through CE, includes latest GBPU, WMU, Security Areas, Human Access, Road Density
GB_file <- file.path("tmp/GB_Brick")
if (!file.exists(GB_file)) {
  #Read layers from threats directory #maybe change to generic diretory?
  GB_gdb <- list.files(file.path(BearsCEDir), pattern = ".gdb", full.names = TRUE)[1]
  fc_list <- st_layers(GB_gdb)

  #Organize strata layers from GB_file
  BEI <- read_sf(GB_gdb, layer = "Final_Grizzly_BEI")
  # Make a BEI raster
  BEI_1_2_r <- BEI[BEI$HIGHCAP %in% c(1,2) ,] %>%
    fasterize(ProvRast, background=NA)
  BEI_1_5_r <- BEI[BEI$HIGHCAP %in% c(1,2,3,4,5) ,] %>%
    fasterize(ProvRast, background=NA)
  BEIr <- fasterize(BEI, ProvRast, background=0, field='HIGHCAP')
  #GBPU
  GBPU <- read_sf(GB_gdb, layer = "GBPU_BC_edits_v2_20150601")
  GBPU_lut <- tidyr::replace_na(data.frame(GRIZZLY_BEAR_POP_UNIT_ID=GBPU$GRIZZLY_BEAR_POP_UNIT_ID, POPULATION_NAME=GBPU$POPULATION_NAME, stringsAsFactors = FALSE), list(POPULATION_NAME = 'extirpated'))
  saveRDS(GBPU, file = 'tmp/GBPU')
  saveRDS(GBPU_lut, file = file.path(StrataDir ,'GBPU_lut'))

  # Save a GBPU shape
  GBPU$GBPU<-GBPU$GRIZZLY_BEAR_POP_UNIT_ID
  st_write(GBPU, file.path(spatialOutDir,'GBPU.shp'), delete_layer = TRUE)

  # Make a GBPU raster
  GBPUr <- fasterize(GBPU, ProvRast, field = 'GRIZZLY_BEAR_POP_UNIT_ID', background=NA)
  #saveRDS(GBPUr, file = 'tmp/GBPUr')

  #WMU
  WMU <- read_sf(GB_gdb, layer = "WMU_grizz_Link")

  #Remove hyphen from WILDLIFE_MGMT_UNIT_ID and add 0 for single digit WMUs
  #First peel off the Region number
  #Second place a 0 if the 'character' is blank (ie single digit)
  #Third concatenate
  WMU$WMU<-as.numeric(paste0(substr(WMU$WILDLIFE_MGMT_UNIT_ID, 0, 1), gsub(" ", "0", sprintf("% 2s", substr(WMU$WILDLIFE_MGMT_UNIT_ID, 3, 4)))))

  # Save a WMU shape
  st_write(WMU, file.path(spatialOutDir,'WMU.shp'), delete_layer = TRUE)

  # Make a WMU raster
  WMUr <- fasterize(WMU, ProvRast, field = 'WMU', background=NA)
  writeRaster(WMUr, filename=file.path(StrataDir ,"WMUr.tif"), format="GTiff", overwrite=TRUE)

  # Make a WMU raster and remove non habitat - rock, ice, water, ocean
  WMUr_NonHab <- overlay(WMUr, NonHab, fun = function(x, y) {
    x[!is.na(y[])] <- NA
    return(x)
  })
  writeRaster(WMUr_NonHab, filename=file.path(StrataDir ,"WMUr_NonHab.tif"), format="GTiff", overwrite=TRUE)

  #set GBPUr to NA where BEI is NonHab
  GBPUr_NonHab <- overlay(GBPUr, NonHab, fun = function(x, y) {
    x[!is.na(y[])] <- NA
    return(x)
  })
  writeRaster(GBPUr_NonHab, filename=file.path(StrataDir ,"GBPUr_NonHab.tif"), format="GTiff", overwrite=TRUE)

  GBPUr_BEI_1_2 <- overlay(GBPUr_NonHab, BEI_1_2_r, fun = function(x, y) {
    x[is.na(y[])] <- NA
    return(x)
  })
  writeRaster(GBPUr_BEI_1_2, filename=file.path(StrataDir,"GBPUr_BEI_1_2.tif"), format="GTiff", overwrite=TRUE)

  GBPUr_BEI_1_5 <- overlay(GBPUr_NonHab, BEI_1_5_r, fun = function(x, y) {
    x[is.na(y[])] <- NA
    return(x)
  })
  writeRaster(GBPUr_BEI_1_5, filename=file.path(StrataDir,"GBPUr_BEI_1_5.tif"), format="GTiff", overwrite=TRUE)

  #Front Country
  FrontCountry <- read_sf(GB_gdb, layer = "FrontCountry_v2_Coastal_DC")
  # Make a Front Country raster, FrontCountry_Class_Coastal_Adj 1,2 & 3 are 'front country', 4 & 5 are 'back country'
  # Adjusts for coastal disconnected roads - setting them to back country
  FrontCountryr <- FrontCountry[FrontCountry$FrontCountry_Class_Coastal_Adj %in% c(1,2,3) ,] %>%
    fasterize(ProvRast, background=0)
  writeRaster(FrontCountryr, filename=file.path(spatialOutDir  ,"FrontCountryr.tif"), format="GTiff", overwrite=TRUE)

  #Secure Habitat
  Secure <- read_sf(GB_gdb, layer = "SecCore_10km2_noDisturb_BEI_Cap_BC")
  # Make a Secure Habitat raster, gridcodes 1,2 & 3 are 'front country', 4 & 5 are 'back country'
  Securer <- Secure[Secure$grid_code == 2 ,] %>%
    fasterize(ProvRast, background=0)
  writeRaster(Securer, filename=file.path(spatialOutDir  ,"Securer.tif"), format="GTiff", overwrite=TRUE)

  #Mortality - see GB_unreported for update
  #Mort <- read_sf(GB_gdb, layer = "COMBINED_Grizzly_PopMort_Allocation_2004_to_2014")
  # Make a Mortality raster - 1-5 fails - what's the spatial - has had a past mort failure?
  # CE used hunted units only as failure as it related to policy
  # Mortr <- Mort[Mort$Pop_Mort_Flag_Hunt == 'Fail' ,] %>%
  #  fasterize(ProvRast, background=0)

  # for status used all mortality
  #Mortr <- Mort[Mort$Pop_Mort_Flag_v1_allAreas == 'Fail' ,] %>%
  #  fasterize(ProvRast, background=0)
  #writeRaster(Mortr, filename=file.path(spatialOutDir,'Mortr.tif'), format="GTiff", overwrite=TRUE)

  #Mid Seral
  MidSeral <- read_sf(GB_gdb, layer = "LU_midSeral_conifer")
  # Make a Mid Seral raster - 1 is low
  MidSeralR <- MidSeral[MidSeral$mid_Seral_Num == 1 ,] %>%
    fasterize(ProvRast, background=0)
  writeRaster(MidSeralR, filename=file.path(spatialOutDir,"MidSeralR.tif"), format="GTiff", overwrite=TRUE)

  # Hunter Day density per km2 LU_hunterDays_annual_per_km2 from old data - replaced by more recent
  #HunterDayD <- read_sf(GB_gdb, layer = "LU_SUMMARY_poly_v5_20160210")
  # Make a Hunter Day density raster
  #HunterDayDr <- HunterDayD %>%
  #  fasterize(ProvRast, field='LU_hunterDays_annual_per_km2', background=0)
  # writeRaster(HunterDayDr, filename=file.path(BearsCEDir, "HunterDayDr.tif"), format="GTiff", overwrite=TRUE)

  SalmonChange <- read_sf(GB_gdb, layer = "LU_SUMMARY_poly_v5_20160210")
  # Make a Salmon raster of per cent negative change - ie a -ve number indicates a positive change
  SalmonChange$SalmonPc<-(SalmonChange$Tot_Salmon_kg_all-SalmonChange$Tot_Salmon_kg_recent)/SalmonChange$Tot_Salmon_kg_all*100

  SalmonChangeR <- SalmonChange %>%
    fasterize(ProvRast, field='SalmonPc', background=0)
  writeRaster(SalmonChangeR, filename=file.path(spatialOutDir, "SalmonChangeR.tif"), format="GTiff", overwrite=TRUE)


  #Read in landcover
  LandCover<-raster(file.path(DataDir,"LandCover/land_cover_n_age_2017.tif"))
  LC_lut<-read_csv(file.path(DataDir,'LandCover/LandCover_lut.csv'), col_names=TRUE)
  Forest<-LandCover
  Forest[!(Forest[] > 0)]<-NA
  GBPUr_Forest <- overlay(GBPUr_NonHab, Forest, fun = function(x, y) {
    x[is.na(y[])] <- NA
    return(x)
  })
  writeRaster(GBPUr_Forest, filename=file.path(StrataDir,"GBPUr_Forest.tif"), format="GTiff", overwrite=TRUE)

  #Landform from AdaptWest
  # Need to pull strata of interest - valley bottom
  LForm_file <- file.path(LandFormDir,"LForm.tif")
  LFormFlat_file <- file.path(LandFormDir,"LFormFlat.tif")
  if (!file.exists(LForm_file)) {
    LForm<-mask(raster(file.path(LandFormDir,"Landform_BCAlbs.tif")) %>%
                  resample(ProvRast, method='ngb'), BCr)

    LF_lut<-read_csv(file.path(LandFormDir,'landform_lut.csv'), col_names=TRUE)
    writeRaster(LForm, filename=file.path(LandFormDir,"LForm.tif"), format="GTiff", overwrite=TRUE)
    saveRDS(LF_lut, file = 'tmp/LF_lut')

    # Pull out just the flat areas - valley bottom (1000) and plains (5000)
    LFormFlat<-LForm
    LFormFlat[!(LFormFlat[] %in% c(1000,5000,6000,7000,8000))]<-NA
    writeRaster(LFormFlat, filename=file.path(StrataDir,"LFormFlat.tif"), format="GTiff", overwrite=TRUE)
    # Pull out really flat areas - valley bottom (1000) and plains (5000)
    LFormFlatFlat<-LForm
    LFormFlatFlat[!(LFormFlatFlat[] %in% c(1000,5000))]<-NA
    writeRaster(LFormFlatFlat, filename=file.path(StrataDir,"LFormFlatFlat.tif"), format="GTiff", overwrite=TRUE)
  } else {
    LForm <- raster(file.path(StrataDir,"LForm.tif"))
    LFormFlat <- raster(file.path(StrataDir,"LFormFlat.tif"))
    LFormFlatFlat <- raster(file.path(StrataDir,"LFormFlatFlat.tif"))
    LF_lut<- readRDS(file = 'tmp/LF_lut')
  }

  GBPUr_LFormFlat <- overlay(GBPUr_NonHab, LFormFlat, fun = function(x, y) {
    x[is.na(y[])] <- NA
    return(x)
  })
  writeRaster(GBPUr_LFormFlat, filename=file.path(StrataDir,"GBPUr_LFormFlat.tif"), format="GTiff", overwrite=TRUE)

  GBPUr_LFormFlatFlat <- overlay(GBPUr_NonHab, LFormFlatFlat, fun = function(x, y) {
    x[is.na(y[])] <- NA
    return(x)
  })
  writeRaster(GBPUr_LFormFlatFlat, filename=file.path(StrataDir,"GBPUr_LFormFlatFlat.tif"), format="GTiff", overwrite=TRUE)


} else {
  NonHab<-raster(file.path(StrataDir,"NonHab.tif"))
  GBPUr<-raster(file.path(StrataDir,"GBPUr.tif"))
  WMUr<-raster(file.path(StrataDir,"WMUr.tif"))
  WMUr_NonHab<-raster(file.path(StrataDir,"WMUr_NonHab.tif"))
  GBPUr_NonHab<-raster(file.path(StrataDir,"GBPUr_NonHab.tif"))
  GBPUr_BEI_1_2<-raster(file.path(StrataDir,"GBPUr_BEI_1_2.tif"))
  GBPUr_BEI_1_5<-raster(file.path(StrataDir,"GBPUr_BEI_1_5.tif"))
  GBPUr_LFormFlat<-raster(file.path(StrataDir,"GBPUr_LFormFlat.tif"))
  GBPUr_LFormFlatFlat<-raster(file.path(StrataDir,"GBPUr_LFormFlatFlat.tif"))
  GBPUr_Forest<-raster(file.path(StrataDir,"GBPUr_Forest.tif"))
  GB_Brick <- readRDS(file = GB_file)
}

#Read in the 2018 population data - extract WMU and GBPU based populion estimates
#save for mortality and NatureServe analysis
GBPop_gdb <- list.files(file.path(GBPDir), pattern = ".gdb", full.names = TRUE)[1]
fc_list <- st_layers(GBPop_gdb)
GBPop <- read_sf(GBPop_gdb, layer = "GBPU_MU_LEH_2015_2018_bear_density_DRAFT")
saveRDS(GBPop, file = 'tmp/GBPop')

#Extract WMU/LEH/NPark data
st_geometry(GBPop)<-NULL
WMUpop<-GBPop %>%
  dplyr::select(Region=REGION_RESPONSIBLE_NAME, GBPU_MU_LEH_uniqueID,MU,LEH_Zone2_fix,
                MAX_ALLOW_MORT_PERC,
                GRIZZLY_BEAR_POP_UNIT_ID, POPULATION_NAME,STATUS,EST_POP_DENSITY_2018,
                EST_POP_2018,EST_POP_2015, AREA_KM2, AREA_KM2_noWaterIce) %>%
         mutate(EST_POP_DENSITY_2018=round(EST_POP_DENSITY_2018,2))

WriteXLS(WMUpop, file.path(dataOutDir,paste('WMUpop.xls',sep='')))

WMUtest <-WMUpop %>%
  dplyr::select(GBPU_MU_LEH_uniqueID,POPULATION_NAME,EST_POP_2018,EST_POP_DENSITY_2018,AREA_KM2,
                AREA_KM2_noWaterIce) %>%
  mutate(densityCalc = round(EST_POP_2018/AREA_KM2*1000,2)) %>%
  mutate(densityCalc_noWaterIce = round(EST_POP_2018/AREA_KM2_noWaterIce*1000,2))

#Extract GBPU scale population data
GBPUpop<-WMUpop %>%
  dplyr::group_by(GRIZZLY_BEAR_POP_UNIT_ID, POPULATION_NAME) %>%
  dplyr::summarise(pop2018 = sum(EST_POP_2018), pop2015=sum(EST_POP_2015),
                   Status=first(STATUS),Area_km2=sum(AREA_KM2),
                   Area_km2_noWaterIce=sum(AREA_KM2_noWaterIce)) %>%
  mutate(Density = round(pop2018/Area_km2*1000,2)) %>%
  mutate(Density_noWaterIce = round(pop2018/Area_km2_noWaterIce*1000,2))

WriteXLS(GBPUpop, file.path(dataOutDir,paste('GBPUpop.xls',sep='')))
