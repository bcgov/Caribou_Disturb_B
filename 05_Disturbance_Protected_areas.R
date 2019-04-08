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

## Script to read in ESRI data tables and combine outputs

##https://gis.stackexchange.com/questions/265863/how-to-read-a-feature-class-in-an-esri-personal-geodatabase-using-r
##http://rstudio-pubs-static.s3.amazonaws.com/255550_1c983c8a6a5a45a0aee5a923206f4818.html
#http://www.rspatial.org/spatial/rst/7-vectmanip.html#spatial-queries

# Load Libraries 
x <- c("dplyr","ggplot2","tidyr","raster","sp","sf","rgdal","lwgeom","mapview","tibble", "bcgovr")   
lapply(x, library, character.only = TRUE) ; rm(x)  # load the required packages

#install.packages("doParallel",dep = T)
require(doParallel)
set.seed(123321)
coreNum <- as.numeric(detectCores()-1)
coreNo <- makeCluster(coreNum)
registerDoParallel(coreNo, cores = coreNum)
clusterEvalQ(coreNo, .libPaths())

memory.limit(size = 80000)


# running on local drive
data.dir = "C:/Temp/Boreal/Data/"
out.dir = "C:/Temp/Boreal/Analysis/"
shape.out.dir =  "C:/Temp/Boreal/Analysis/dist_int_layers/"
temp.dir = "C:/Temp/Boreal/temp/"

## Set your input geodatabases (this will be where you saved your arcmap exports)
## edit these to your filepath and name of gdb

Base  = "Base_data.gdb" # clipped disturb layers
#AOI = "AOI_data.gdb"   # AOI

## List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
base_list <- ogrListLayers(paste(data.dir,Base,sep = "")); print(base_list)
#aoi_list <- ogrListLayers(paste(data.dir,AOI,sep = "")); print(aoi_list)

##############################################################################################
# Read in herd boundary layers and join to single file 

Herd_key <- read.csv(paste(data.dir,"Herd_key.csv",sep = ""))
b.aoi <- st_read(paste(data.dir,"Boreal_herd_bdry.shp",sep = ""))
st_crs(b.aoi)<- 3005
#plot(b.aoi)
#plot(st_geometry(b.aoi))

Herd_key$desig = "Total_Area"
Herd_key <- Herd_key %>% dplyr::select(Range, Zone, desig, Area_ha) 
out.tab <- Herd_key 

##############################################################################################
# Read in  Protection layers

# 1) 
pro<- st_read(dsn=paste(data.dir,Base,sep = ""),layer="designatedlands") # reading in as geometry?
pro <-st_transform(pro,3005)
pro.int = st_intersection(b.aoi,pro)   # intersect with ranges

pro.int$Area_ha <- round(as.numeric(st_area(pro.int)/10000),2)
# plot(st_geometry(roads.int)) ## this takes a long time 

pro.int.df = data.frame(pro.int) 
pro.int.df = pro.int.df %>% 
  group_by(Range,Zone,desig) %>% 
  summarise(Area_ha = sum(Area_ha))

#pro.int.df

##OUTPUT 1 calculate the otoal area and Percent protected
out.tab <- bind_rows(out.tab,pro.int.df)   ## add to table Herd key
out.tab <- merge(out.tab,Herd_key, by= c('Range','Zone')) # add the size of the areas 
out.tab <- out.tab %>% mutate(Area_pc = round(Area_ha.x/ Area_ha.y*100,2)) %>% dplyr::select(Range,Zone,desig.x,Area_ha.x,Area_pc)


#loop through the herds
final.data.table <- as.data.frame(unique(out.tab$desig.x))
colnames(final.data.table)= 'desig.x'
# get list of herds
herds <- as.character(unique(out.tab$Range))

  for(i in 1:8){ 
    # i = 1
    h = herds[i]
    tdata <- out.tab %>% dplyr::filter(Range == paste(h))
    tdata.area <- tdata %>% dplyr::select(Range,Zone,desig.x,Area_ha.x) %>% spread(.,key = Zone,value = Area_ha.x)
    tdata.pc<- tdata %>% dplyr::select(Range,Zone,desig.x,Area_pc) %>% spread(.,key = Zone,value = Area_pc)
    tdata.out <- merge(tdata.area,tdata.pc,by=c('desig.x'))
    
    final.table<- left_join(final.data.table,tdata.out)
    write.csv(final.table,paste(out.dir,h,"_prot.sum.csv",sep = ""),row.names = FALSE)
  }
  
  
  
###############################################################################
## Make some pretty plots 

## plot 

pro.ranges <- as.character(unique(pro.int$Range)) 

for (i in 1:length(pro.ranges)){ 
  # i = 1
  herd.oi <-pro.ranges[i] 
  
  tdata<- pro.int %>% filter(Range == paste(herd.oi)) # subset fire data for aeach herd
  tdata.aoi.C <- b.aoi %>%  filter(Range ==paste(herd.oi) & Zone == 'C')
  #tdata.aoi.A <- b.aoi %>%  filter(Range ==paste(herd.oi) & Zone == 'A')
  # tdata.aoi. <- b.aoi %>%  filter(Range ==paste(herd.oi))  
  
  p1 = ggplot() + 
    geom_sf(data =  tdata.aoi.C , fill = "grey94") + 
    #geom_sf(data =  tdata.aoi.A , fill = "grey96") + 
    #geom_sf(data = r.temp, aes(fill =dec.period)) + ggtitle("Natural Disturbance per Herd Range") + theme_bw()
    geom_sf(data = tdata,fill = "dark green",colour = NA) +  
    ggtitle(paste("Protected Areas within the ",herd.oi," herd boundary",sep = "")) #+ 
    #theme_bw() 
  p1
  
  ggsave(paste(out.dir,herd.oi,"_protect_map.jpg",sep = ""),width = 30, height = 15, units = "cm")  
}






