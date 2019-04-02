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

## 
## Caribou disturbance analysis 2018/2019 
## This script prepares the boundary data for use in the disturbance analysis: 
## 
# Load Libraries 
x <- c("dplyr","ggplot2","tidyr","raster","sp","sf","rgdal","lwgeom","mapview","tibble", "bcgovr")   
lapply(x, library, character.only = TRUE) ; rm(x)  # load the required packages

#create_bcgov_project(path = "C:/Temp/Github/Caribou_disturb/Boreal/", coc_email = "genevieve.perkins@gov.bc.ca") 

## set your output directory 
data.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/Boreal/Data/"
out.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/Boreal/Analysis/"
temp.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/Boreal/temp/"

## Set your input geodatabases (this will be where you saved your arcmap exports)
Base  = "Base_data.gdb" # clipped disturb layers
AOI = "AOI_data.gdb"   # AOI

## List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
base_list <- ogrListLayers(paste(data.dir,Base,sep = "")); print(base_list)
aoi_list <- ogrListLayers(paste(data.dir,AOI,sep = "")); print(aoi_list)

##############################################################################################
# Read in herd boundary layers and join to single file 

# calandar
b.cal <- st_read(dsn=paste(data.dir,AOI,sep = ""),layer="AOI_Calandar"); plot(st_geometry(b.cal))
b.cal <- b.cal %>% dplyr::select("Zone","Range")
b.cal <- b.cal %>% dplyr::filter(Zone == "A")
b.cal.c <- st_read(dsn=paste(data.dir,AOI,sep = ""),layer="AOI_Calandar_C"); plot(st_geometry(b.cal.c))
b.cal.c <- b.cal.c %>% mutate(Zone = "C")
b.cal.c <- b.cal.c %>% dplyr::select("Zone","Range")
b.cal.abc <- rbind(b.cal,b.cal.c)
plot(st_geometry(b.cal.abc))

# chinchaga
b.chin <- st_read(dsn=paste(data.dir,AOI,sep = ""),layer="AOI_Chinchaga"); plot(st_geometry(b.chin))
b.chin <- b.chin%>% mutate(Zone = Zone_1,Range = Range_1)
b.chin <- b.chin %>% dplyr::select("Zone","Range")
b.chin <- b.chin %>% dplyr::filter(Zone %in% c("A","B"))
b.chin.c <- st_read(dsn=paste(data.dir,AOI,sep = ""),layer="AOI_Chinchaga_C"); plot(st_geometry(b.chin.c))
b.chin.c <- b.chin.c%>% mutate(Range = Range_1)
b.chin.c<- b.chin.c%>% mutate(Zone = "C")
b.chin.c <- b.chin.c%>% dplyr::select("Zone","Range")
b.chin.abc <- rbind(b.chin,b.chin.c)
plot(st_geometry(b.chin.abc))

#Maxhamish
b.max <- st_read(dsn=paste(data.dir,AOI,sep = ""),layer="AOI_Maxhamish"); plot(st_geometry(b.max))
b.max <- b.max %>% dplyr::select("Zone","Range")
b.max <- b.max %>% dplyr::filter(Zone %in% c("A","B"))
b.max.c <- st_read(dsn=paste(data.dir,AOI,sep = ""),layer="AOI_Maxhamish_C"); plot(st_geometry(b.max.c))
b.max.c <- b.max.c %>% mutate(Zone = "C")
b.max.c <- b.max.c %>% dplyr::select("Zone","Range")
b.max.abc <- rbind(b.max,b.max.c )
plot(st_geometry(b.max.abc))

#SS
b.ss <-  st_read(dsn=paste(data.dir,AOI,sep = ""),layer="AOI_SS"); plot(st_geometry(b.ss))
b.ss <- b.ss %>% dplyr::select("Zone","Range")
b.ss <- b.ss %>% dplyr::filter(Zone %in% c("A","B"))
b.ss.c <- st_read(dsn=paste(data.dir,AOI,sep = ""),layer="AOI_SS_C"); plot(st_geometry(b.ss.c))
b.ss.c <- b.ss.c %>% mutate(Zone = "C")
b.ss.c <- b.ss.c %>% dplyr::select("Zone","Range")
b.ss.abc <- rbind(b.ss,b.ss.c )
plot(st_geometry(b.ss.abc))


#WFN 
b.wfn <-  st_read(dsn=paste(data.dir,AOI,sep = ""),layer="AOI_WFN"); plot(st_geometry(b.wfn))
b.wfn <- b.wfn %>% dplyr::select("Zone","Range")
b.wfn <- b.wfn %>% dplyr::filter(Zone %in% c("A","B"))
b.wfn.c <- st_read(dsn=paste(data.dir,AOI,sep = ""),layer="AOI_WFN_C"); plot(st_geometry(b.wfn.c))
b.wfn.c <- b.wfn.c %>% mutate(Zone = "C")
b.wfn.c <- b.wfn.c %>% dplyr::select("Zone","Range")
b.wfn.abc <- rbind(b.wfn,b.wfn.c )
plot(st_geometry(b.wfn.abc))

b.all <-  rbind(b.cal.abc,b.chin.abc,b.max.abc,b.ss.abc,b.wfn.abc)


# add the areas of interest also 
aoi.nh1<- st_read(dsn=paste(data.dir,AOI,sep = ""),layer="AOI_nonherd1"); plot(st_geometry(aoi.nh1))
aoi.nh1<- aoi.nh1 %>% mutate(Zone = "D")
aoi.nh1<- aoi.nh1%>% dplyr::select("Zone","Range")

b.all <-  rbind(b.all, aoi.nh1)

# format the area columns 
b.all$Area = st_area(b.all)
b.all$Area_ha = round(as.numeric(b.all$Area /10000),0)

#plot(st_geometry(b.all))
#plot(b.all)
#b.all

# write out a shapefile with combined boundary information 
st_write(b.all,paste(data.dir,"Boreal_herd_bdry.shp",sep = ""))

# create a table with the total areas 
Herd_key<- data.frame(b.all) %>% 
  dplyr::select(Range,Zone,Area_ha) %>% 
  group_by(Range,Zone)%>%
  summarise(Area_ha = sum(Area_ha)) 

write.csv(Herd_key,paste(data.dir,"Herd_key.csv",sep = ""),row.names = FALSE)

