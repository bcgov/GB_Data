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

# Group updated population data by GBPU
GB_GBPUdf <- GB_WMU %>% st_set_geometry(NULL) %>%
  dplyr::group_by(GRIZZLY_BEAR_POP_UNIT_ID, POPULATION_NAME) %>%
  dplyr::summarize(EST_POP_2018 = sum(EST_POP_2018),EST_POP_2015 = sum(EST_POP_2015), AREA_KM2 = sum(AREA_KM2), AREA_KM2_noWaterIce=sum(AREA_KM2_noWaterIce))

WriteXLS(GB_GBPUdf, file.path(dataOutDir,paste('gb2018GBPUpop.xls',sep='')))
