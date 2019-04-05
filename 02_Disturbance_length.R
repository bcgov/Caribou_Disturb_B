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
## Caribou disturbance analysis 2018 
## 
## August 14th 2018 
##
## written by genevieve perkins (genevieve.perkins@gov.bc.ca)
## 
## This requires initial preparation of layers within arcmap. 
## Step 1) 
##
## Assemble layers as per datasheet in arcmap mxd 
## Clip the layers to the range with the largest extent (for example range boundary for Boreal (not core))
## Create a filegeodatabdase and output these to the given data base. 
##
## Step 2)
## Run through the script 01_disturbance layers first! 
## 
## Step 3) 
## Run this script
## You may need to adjust 
## - the names of files to match your arcmap exports 
## - the directory/folder sructure. 
##
## Step 4)
## For road and seismic lines these need to be run in Arcmap. 
## To assemble the "all disturbance layers" this need to be done in ArcMap
## The final assemble of the data tables is done at the end of the script.


## General notes: 
## For each disturbance layers the script will read in, intersect with range and core areas and calculate the length. The peripery area will be calculated for each herd as well. 
## With each layer the compiled disturbance will also be calculated. 



## Associated help files for reference: 
##https://gis.stackexchange.com/questions/265863/how-to-read-a-feature-class-in-an-esri-personal-geodatabase-using-r
##http://rstudio-pubs-static.s3.amazonaws.com/255550_1c983c8a6a5a45a0aee5a923206f4818.html
#http://www.rspatial.org/spatial/rst/7-vectmanip.html#spatial-queries
#https://r-spatial.github.io/sf/articles/sf1.html
#https://github.com/r-spatial/sf/issues/290


## Read in packages and libraries required: 



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

#create_bcgov_project(path = "C:/Temp/Github/Caribou_disturb/Boreal/", coc_email = "genevieve.perkins@gov.bc.ca") 

## set your output directory 
#data.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/Boreal/Data/"
#out.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/Boreal/Analysis/"
#temp.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/Boreal/temp/"

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

Herd_key$ThemeName = "Total_Area"
Herd_key <- Herd_key %>% dplyr::select(Range, Zone, ThemeName, Area_ha) 
out.tab <- Herd_key 
out.tab$length_m2 = 0

##############################################################################################
# Read in individual Disturbance layers:

##linear : level B : "TR_Rail_B" ,   "CU_Trail_B"     
## linear level C:" AOI_anth_C_linear" 

# 1) THEME: Transport ROAD 
roads <- st_read(dsn=paste(data.dir,Base,sep = ""),layer="TR_Road_B") # reading in as geometry?
        #roads  = st_cast(roads,"MULTIPOLYGON")  
        roads.int = st_intersection(b.aoi,roads)   # intersect with ranges
      
        roads.int$length_m = round(as.numeric(st_length(roads.int)),2) 
        roads.int.df = data.frame(roads.int) 
        roads.int.df= roads.int.df %>% 
          group_by(Range,Zone,ThemeName) %>% 
          summarise(length_m2 = sum(length_m))
        
        ## not sure if these values are dissolved or not 
        ## check overlaps
      
        ##OUTPUT 2: Individual dist layer (spatial) to be used to make plots in Arcmap 
        st_write(roads.int,paste(shape.out.dir,"D_roads_int_line.shp")) # generate spatial products

        ##OUTPUT 1 : DATA table
        out.tab <- bind_rows(out.tab,roads.int.df)   ## add to table Herd key
        
        
#### Cut lines: 
seis <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="CU_Cut_B_l") # reading in as geometry?      
        #seis <-st_make_valid(seis)
        seis.int = st_intersection(b.aoi,seis)   # intersect with ranges
        seis.int$length_m = round(as.numeric(st_length(seis.int)),2) 
        #seis.int$Area_ha <- round(as.numeric(st_area(seis.int)/10000),2)
        seis.int.df = data.frame(seis.int) 
        seis.int.df= seis.int.df %>% 
          group_by(Range,Zone,ThemeName) %>% 
          summarise(length_m2 = sum(length_m))
        ##OUTPUT 1 : DATA table
        out.tab <- bind_rows(out.tab,seis.int.df)   ## add to table Herd key 
        st_write(seis.int,paste(shape.out.dir,"D_seis_int_line.shp")) # generate spatial products
        
        #clean up afterwards
        rm(seis,seis.int)

# Trails   
trail <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="CU_Trail_B") # reading in as geometry?      
        
          trail<-st_make_valid(trail)
          trail.int = st_intersection(b.aoi,trail)   # intersect with ranges
          trail.int$length_m = round(as.numeric(st_length(trail.int)),2) 
          
          # plot(st_geometry(seis.int))
          trail.int.df = data.frame(trail.int) 
          trail.int.df = trail.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(length_m2 = sum(length_m))
          
          trail.int.df 
          
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,trail.int.df)   ## add to table Herd key 
          st_write(trail.int,paste(shape.out.dir,"D_trail_int_line.shp")) # generate spatial products
          
          #clean up afterwards
          rm(trail,trail.int)

# rail 
rail <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="TR_Rail_B") # reading in as geometry?      
           #st_is_valid(rail)
          rail <- st_make_valid(rail)
          rail.int = st_intersection(b.aoi,rail)   # intersect with ranges
          
          rail.int$length_m = round(as.numeric(st_length(rail.int)),2) 
          ## fix this 
          rail.int.df = data.frame(rail.int) 
          rail.int.df = rail.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(length_m2 = sum(length_m))
          
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,rail.int.df)   ## add to table Herd key 
          st_write(rail.int,paste(shape.out.dir,"D_rail_int_line.shp")) # generate spatial products
          
          #clean up afterwards
          rm(rail,rail.int)
          
          
write.csv(out.tab,paste(out.dir,"out.length.table.csv",sep = ""),row.names = FALSE)


###################################################################################
## All linear compiled data  as compiled by RSEA level C version 
###################################################################################

## linear level C:" AOI_anth_C_linear" 

# 1) THEME: all linear combined 
anth <- st_read(dsn=paste(data.dir,Base,sep = ""),layer="AOI_anth_C_linear") # reading in as geometry?
      anth.int = st_intersection(b.aoi,anth)   # intersect with ranges


## up to here


#roads  = st_cast(roads,"MULTIPOLYGON")  
roads.int = st_intersection(b.aoi,roads)   # intersect with ranges

roads.int$length_m = round(as.numeric(st_length(roads.int)),2) 
roads.int.df = data.frame(roads.int) 
roads.int.df= roads.int.df %>% 
  group_by(Range,Zone,ThemeName) %>% 
  summarise(length_m2 = sum(length_m))

## not sure if these values are dissolved or not 
## check overlaps

##OUTPUT 2: Individual dist layer (spatial) to be used to make plots in Arcmap 
st_write(roads.int,paste(shape.out.dir,"D_roads_int_line.shp")) # generate spatial products

##OUTPUT 1 : DATA table
out.tab <- bind_rows(out.tab,roads.int.df)   ## add to table Herd key













          
          
#################################################################################################
## Create length and density tables for Core, Periphery and Range for disturbance linear features
#################################################################################################







          
          
out.tab <- read.csv(paste(out.dir,"out.length.table.csv",sep = ""))


## reshape the long version of the table ? now or later 

## calculate the length (km) and density (km2) 





all.outl = reshape2::melt(all.out)
all.outl<- all.outl %>% 
    mutate(Dist_type = gsub(".*R_|_length.*","", variable)) %>%
    mutate(Dist_type = gsub("C_","",Dist_type)) %>%
    mutate(Dist_type = gsub("P_","",Dist_type)) %>%
    mutate(Boundary = substring(variable,1,1)) 
   
  lr = all.outl  %>% gather(key = variable, value = value, 3) 

# output density tables
herds = c('Calendar','Chinchaga','Maxhamish','Snake-Sahtahneh','Westside Fort Nelson')

for (i in herds) { 
  # write out core area details. 
  t.l = lr %>% filter(Range == paste(i))
    # get the total areas (km2 to calculate density)
    t.r = t.l %>% filter(Boundary =='R' & Dist_type == "area_km2") %>%  dplyr::select(value)
    t.p = t.l %>% filter(Boundary =='P' & Dist_type == "area_km2") %>%  dplyr::select(value)
    t.c = t.l %>% filter(Boundary =='C' & Dist_type == "area_km2") %>%  dplyr::select(value)
  # add columns to the totals for RPC  
  t.l <- t.l %>% mutate(Total_area_km2 = ifelse(Boundary =='R',t.r,
                                     ifelse(Boundary =='P',t.p,
                                     ifelse(Boundary == 'C',t.c,0))))
  # calculate the length (km) and density (km2) 
  t.l <- t.l %>% 
    mutate(Length_km = value/1000) %>% 
    mutate(Density_km_km2 = Length_km / as.numeric(Total_area_km2)) %>%
    dplyr::select(-c(variable, value)) 
 
   # this creates a table with the Total areas (Total_area_km2) for eqach of the range/core/periphery
  #t.total <- t.l %>% 
  #  dplyr::filter(Dist_type == "Road") %>%
  #  dplyr::select(-c(Length_km,Density_km_km2,Dist_type)) %>%  
  #  spread(key = Boundary,value = Total_area_km2) 

   # get rid of the area_total rows and random X row
  t.l = t.l[-(grep("area_km2",t.l$Dist_type)),]
  t.l = t.l[-(grep("X",t.l$Dist_type)),]
  t.l = t.l %>% dplyr::select(-c(Total_area_km2))
  
  t.length <- t.l %>% 
  dplyr::select(-c(Density_km_km2)) %>%  
  spread(.,key = Boundary,value = Length_km)
  
  write.csv(t.length, paste(out.dir,i,"_Disturb_RCP_length.csv",sep = ''))
  
  t.density = t.l %>% 
    dplyr::select(-c(Length_km)) %>%  
    spread(.,key = Boundary,value = Density_km_km2)
    
  # write out the csv   
  write.csv(t.density, paste(out.dir,i,"_Disturb_RCP_density.csv",sep = ''))
  
} 
  
  
### Ammalgamate the length and density files and calculate a % of each of the component in excel ### 

 
  
  
  
  
  
  
  
  
  
  
  
 


 
  
  
##################### OLD SCRIPTING  ##########################################  
  

 ## OLD PLOT INFO  
  p = ggplot(t.l, aes(Decade,value)) + try(facet_wrap(~var_name))+ geom_bar(stat = 'identity') 
  p = p + ylab("Percentage") + xlab("Decade") + ggtitle(paste("Percent Cutblock (per area) from 1958 - 2017 (",i," herd)",sep = "" ))
  p = p + theme_bw()
  ggsave(paste(out.dir,i,"_cutblock_temp1.png",sep = ""))  
  
  
  
  
  p1 = ggplot() + geom_sf(data = merged.all, aes(fill = range.all)) + ggtitle("Range") + theme_bw()
  p1  = p1 + guides(fill=guide_legend(title="Density (km/km2"))
  
  p2 = ggplot() + geom_sf(data = merged.all, aes(fill = per.all)) + ggtitle("Periphery") + theme_bw()
  p2  = p2 + guides(fill=guide_legend(title="Density (km/km2"))
  
  p3 = ggplot() + geom_sf(data = merged.all, aes(fill = core.all)) + ggtitle("Core") + theme_bw()
  p3  = p3 + guides(fill=guide_legend(title="Density (km/km2"))
  
  
  
  
  
  

  
  if(length(t.c.c$Range)>0){
    # plot 1
    p = ggplot(t.c.c, aes(Decade,value)) + try(facet_wrap(~var_name))+ geom_bar(stat = 'identity') 
    p = p + ylab("Percentage") + xlab("Decade") + ggtitle(paste("Percent Cutblock (per area) from 1958 - 2017 (",i," herd)",sep = "" ))
    p = p + theme_bw()
    ggsave(paste(out.dir,i,"_cutblock_temp1.png",sep = ""))  
    # plot 2
    p1 = ggplot(t.c.c, aes(var_name,value)) + facet_wrap(~Decade)+ geom_bar(stat = 'identity') 
    p1 = p1 + ylab("Percentage") + xlab("") + ggtitle(paste("Percent Cutblock (per area) from 1958 - 2017 (",i," herd)",sep = "" ))
    p1 = p1 + theme_bw()
    ggsave(paste(out.dir,i,"_cutblock_temp2.png",sep = ""))  
    # plot 3
    p2 = ggplot(t.c.group, aes(var_name,sum)) + facet_wrap(~Age_group)+ geom_bar(stat = 'identity') 
    p2 = p2 + ylab("Percentage") + xlab("") + ggtitle(paste("Percent Cutblock (per area) for harvest (0-40 years) and 41-80 years(",i," herd)",sep = "" ))
    p2 = p2 + theme_bw()
  } 
} 

l.range = read.csv(paste(out.dir,"Boreal_range_disturb_length_km.csv",sep = ''))
#l.range$brdy = 'range'; 
l.range = l.range %>% dplyr::select(-(X))
l.rangel = reshape2::melt(l.range)
l.rangel$range = l.rangel$value
l.core = read.csv(paste(out.dir,"Disturbance_length_core_by_ranges.csv",sep = ""))
l.core = l.core %>% dplyr::select(-(X))
l.core = l.core %>% mutate(Calendar = Calendar/1000)  %>% 
  mutate(Chinchaga = Chinchaga/1000) %>% 
  mutate(Maxhamish = Maxhamish/1000) %>%
  mutate(Snake.Sahtahneh = Snake.Sahtahneh/1000)  %>% 
  mutate(Westside.Fort.Nelson = Westside.Fort.Nelson/1000)
l.corel = reshape2::melt(l.core)
l.corel$core = l.corel$value
out = merge(l.rangel,l.corel, by = c("Type",'variable'))
out = out %>% dplyr::select(-c(value.x,value.y))

out = out %>% dplyr::filter(!(grepl(pattern = 'Dams',Type)))
out$peri = out$range-out$core

out.l = left_join(out,Herd_key, by = c('variable' = 'Range'))
out.l = out.l %>% 
  mutate(range.l = range/ (R_area_ha/100))%>%
  mutate(per.l = peri/ (P_area_ha/100))%>%
  mutate(core.l = core/ (C_area_ha/100))

out.l = out.l %>% dplyr::select(-c(R_area_ha,C_area_ha,P_area_ha))

write.csv(out.l, paste(out.dir,"Disturb_RCP_length_density.csv",sep = ''))





    mutate(Disturbance_km = ifelse(grep("_ha",variable),variable/100,variable*100))
           
# create a table with disturbance length (km) , density (km/km2) and 


# table with total linear 

l.range = l.range %>% 
  mutate(R_Pipe_length_km = R_Pipe_length_m*1000)  %>% 
  mutate(C_Pipe_length_km = C_Pipe_length_m *1000) %>% 
  mutate(Maxhamish = Maxhamish/1000) %>%
  mutate(Snake.Sahtahneh = Snake.Sahtahneh/1000)  %>% 
  mutate(Westside.Fort.Nelson = Westside.Fort.Nelson/1000)

# convert to km/km2  = 1 km2 = 100 ha 
l.rangel = reshape2::melt(l.range)

l.rangel$range = l.rangel$value
l.core = read.csv(paste(out.dir,"Disturbance_length_core_by_ranges.csv",sep = ""))
l.core = l.core %>% dplyr::select(-(X))
l.core = l.core %>% mutate(Calendar = Calendar/1000)  %>% 
  mutate(Chinchaga = Chinchaga/1000) %>% 
  mutate(Maxhamish = Maxhamish/1000) %>%
  mutate(Snake.Sahtahneh = Snake.Sahtahneh/1000)  %>% 
  mutate(Westside.Fort.Nelson = Westside.Fort.Nelson/1000)
l.corel = reshape2::melt(l.core)
l.corel$core = l.corel$value
out = merge(l.rangel,l.corel, by = c("Type",'variable'))
out = out %>% dplyr::select(-c(value.x,value.y))

out = out %>% dplyr::filter(!(grepl(pattern = 'Dams',Type)))
out$peri = out$range-out$core

out.l = left_join(out,Herd_key, by = c('variable' = 'Range'))
out.l = out.l %>% 
  mutate(range.l = range/ (R_area_ha/100))%>%
  mutate(per.l = peri/ (P_area_ha/100))%>%
  mutate(core.l = core/ (C_area_ha/100))

out.l = out.l %>% dplyr::select(-c(R_area_ha,C_area_ha,P_area_ha))

write.csv(out.l, paste(out.dir,"Disturb_RCP_length_density.csv",sep = ''))


##########################################################################
# Join the data to spatial data and map by density 

out.d = out.l %>% dplyr::select(-c(range,core,peri))
out.d$Range = out.d$variable 
out.d$Range = gsub('Snake.Sahtahneh','Snake-Sahtahneh', out.d$Range) 
out.d$Range = gsub('Westside.Fort.Nelson','Westside Fort Nelson', out.d$Range) 

merged = merge(b.range.sf,out.d, by = "Range")
merged <- st_as_sf(merged)

merged.all <- merged %>% group_by(Range) %>% summarise(range.all = sum(range.l), per.all = sum(per.l), core.all = sum(core.l))


p1 = ggplot() + geom_sf(data = merged.all, aes(fill = range.all)) + ggtitle("Range") + theme_bw()
p1  = p1 + guides(fill=guide_legend(title="Density (km/km2"))

p2 = ggplot() + geom_sf(data = merged.all, aes(fill = per.all)) + ggtitle("Periphery") + theme_bw()
p2  = p2 + guides(fill=guide_legend(title="Density (km/km2"))

p3 = ggplot() + geom_sf(data = merged.all, aes(fill = core.all)) + ggtitle("Core") + theme_bw()
p3  = p3 + guides(fill=guide_legend(title="Density (km/km2"))


p <- cowplot::plot_grid(p1,p2,p3, ncol = 3)

ggsave(paste(out.dir,"Linear_density_RPC.png",sep = ""),p)










