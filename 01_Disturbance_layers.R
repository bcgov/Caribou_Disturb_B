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
## 
## Rerunning caribou analysis based on updated boundary and new disturbance layere provided by RSEA project 
## written by genevieve perkins (genevieve.perkins@gov.bc.ca)
## 
## This requires initial preparation of layers within arcmap. 
## Step 1) 
## Assemble layers as listed in documentation in arcmap mxd 
## Clip the layers to the range with the largest extent (AOI_boreal = 1 km buffered widest extent)
## Create a filegeodatabdase and output these to the given data base. 

## Step 2)
## Run the preparation script to clean and generate boundary layers (00_Herd_boundary_prep.R
##
## STep 3 )
## Run through the script below. You may need to adjust 
## - the names of files to match your arcmap exports 
## - the directory/folder sructure. 
##
## General notes: 
## For each disturbance layers the script will read in, intersect with zones ABC and calculate the area and or length. 
## With each layer the compiled disturbance will also be calculated. 

## Associated help files for reference: 
##https://gis.stackexchange.com/questions/265863/how-to-read-a-feature-class-in-an-esri-personal-geodatabase-using-r
##http://rstudio-pubs-static.s3.amazonaws.com/255550_1c983c8a6a5a45a0aee5a923206f4818.html
#http://www.rspatial.org/spatial/rst/7-vectmanip.html#spatial-queries
#https://r-spatial.github.io/sf/articles/sf1.html
#https://github.com/r-spatial/sf/issues/290

## Read in packages and libraries required: 

#install.packages(c("rgdal","sp","dplyr","raster","rgeos","maptools","magrittr","tibble", 
#			"tidyr","sf","lwgeom","mapview"),dep = T )


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

##############################################################################################
# Read in individual Disturbance layers:

##linear : level B : "TR_Rail_B" ,"CU_Cut_B_l"   "TR_Road_B"   "CU_Trail_B"     
## linear level C:" AOI_anth_C_linear" 

## poolygon 
     "designatedlands"              




# 1) THEME: Transport 
roads <- st_read(dsn=paste(data.dir,Base,sep = ""),layer="TR_Road_B_pol") # reading in as geometry?
          roads  = st_cast(roads,"MULTIPOLYGON")  
          roads.int = st_intersection(b.aoi,roads)   # intersect with ranges
          roads.int$Area_ha <- round(as.numeric(st_area(roads.int)/10000),2)
          # plot(st_geometry(roads.int)) ## this takes a long time 
          
          roads.int.df = data.frame(roads.int) 
          roads.int.df = roads.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(Area_ha = sum(Area_ha))
          
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,roads.int.df)   ## add to table Herd key
          ##OUTPUT 2: Individual dist layer (spatial)
          st_write(roads.int,paste(shape.out.dir,"D_roads_int.shp")) # generate spatial products
          ##OUTPUT 3: Spatial layer to add to (cumulative disturbance) 
         
          ## STILL YET TO DO THIS>>>>>>> out.sf <-  roads.int
          
          #clean up afterwards
          rm(roads,roads.int,roads.int.df)
          
rail <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="TR_Rail_B_pol") # reading in as geometry?      
          #st_is_valid(rail)
          rail <- st_make_valid(rail)
          rail.int = st_intersection(b.aoi,rail)   # intersect with ranges
          rail.int$Area_ha <- round(as.numeric(st_area(rail.int)/10000),2)
          rail.int.df = data.frame(rail.int) 
          rail.int.df = rail.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(Area_ha = sum(Area_ha))
         
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,rail.int.df)   ## add to table Herd key 
          ##OUTPUT 2: Individual dist layer (spatial)
          st_write(rail.int,paste(shape.out.dir,"D_rail_int.shp")) # generate spatial products 
          ##OUTPUT 3: Spatial layer to add to (cumulative disturbance) 
          out.sf <-  st_union(out.sf,rail.int)  ; plot(st_geometry(out.sf))
          out.sf = st_union(out.sf); plot(st_geometry(out.sf),add = T)
          out.sf = st_cast(out.sf,"POLYGON")
          
          #clean up afterwards
          rm(rail,rail.int,rail.int.df)
       
### Theme cutlines and trails 

seis <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="CU_Cut_B") # reading in as geometry?      
          seis <-st_make_valid(seis)
          #seis = st_cast(seis,"MULTIPOLYGON")
          seis.int = st_intersection(b.aoi,seis)   # intersect with ranges
          seis.int$Area_ha <- round(as.numeric(st_area(seis.int)/10000),2)
          seis.int.df = data.frame(seis.int) 
          seis.int.df= seis.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(Area_ha = sum(Area_ha))
        
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,seis.int.df)   ## add to table Herd key 
          ##OUTPUT 2: Individual dist layer (spatial)
          st_write(seis.int,paste(shape.out.dir,"D_seis_int.shp")) # generate spatial products 
          
          ##OUTPUT 3: Spatial layer to add to (cumulative disturbance) 
          #out.sf <-  st_union(out.sf,seis.int)  ; plot(st_geometry(out.sf))
          #out.sf = st_union(out.sf); plot(st_geometry(out.sf),add = T)
          #out.sf = st_cast(out.sf,"POLYGON")
          
          #clean up afterwards
          rm(seis,seis.int,seis.int.df)
      
# Trails 
trail <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="CU_Trail_B_pol") # reading in as geometry?      
          trail<-st_make_valid(trail)
          trail.int = st_intersection(b.aoi,trail)   # intersect with ranges
          trail.int$Area_ha <- round(as.numeric(st_area(trail.int)/10000),2)
          # plot(st_geometry(seis.int))
          trail.int.df = data.frame(trail.int) 
          trail.int.df = trail.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(Area_ha = sum(Area_ha))
          
          trail.int.df 
          
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,trail.int.df)   ## add to table Herd key 
          ##OUTPUT 2: Individual dist layer (spatial)
          st_write(trail.int,paste(shape.out.dir,"D_trail_int.shp")) # generate spatial products 
          
          ##OUTPUT 3: Spatial layer to add to (cumulative disturbance) 
          #out.sf <-  st_union(out.sf,trail.int)  ; plot(st_geometry(out.sf))
          #out.sf = st_union(out.sf); plot(st_geometry(out.sf),add = T)
          #out.sf = st_cast(out.sf,"POLYGON")
          
          #clean up afterwards
          rm(trail,trail.int,trail.int.df)
         
          
# Recreation   
rec <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="REC_B") # reading in as geometry?      
          rec.int = st_intersection(b.aoi,rec)   # intersect with ranges
          rec.int$Area_ha <- round(as.numeric(st_area(rec.int)/10000),2)
          # plot(st_geometry(seis.int))
          rec.int.df = data.frame(rec.int) 
          rec.int.df= rec.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(Area_ha = sum(Area_ha))
          
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,rec.int.df)   ## add to table Herd key 
          ##OUTPUT 2: Individual dist layer (spatial)
          st_write(rec.int,paste(shape.out.dir,"D_rec_int.shp")) # generate spatial products 
          
          ##OUTPUT 3: Spatial layer to add to (cumulative disturbance) 
          #out.sf <-  st_union(out.sf,rec.int)  ; plot(st_geometry(out.sf))
          #out.sf = st_union(out.sf); plot(st_geometry(out.sf),add = T)
          #out.sf = st_cast(out.sf,"POLYGON")
          
          #clean up afterwards
          rm(rec,rec.int,rec.int.df)
          
# Aggriculture
agr <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="IN_AGRI_B") # reading in as geometry?      
         agr <- st_make_valid(agr)
         agr.int = st_intersection(b.aoi,agr)   # intersect with ranges
         agr.int$Area_ha <- round(as.numeric(st_area(agr.int)/10000),2)
          # plot(st_geometry(seis.int))
         agr.int.df = data.frame(agr.int) 
         agr.int.df= agr.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(Area_ha = sum(Area_ha))
          
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,agr.int.df)   ## add to table Herd key 
          write.csv(out.tab,paste(out.dir,"out.table.csv",sep = ""),row.names = FALSE)
        
          ##OUTPUT 2: Individual dist layer (spatial)
          st_write(com.int,paste(shape.out.dir,"D_agr_int.shp")) # generate spatial products 
          
          ##OUTPUT 3: Spatial layer to add to (cumulative disturbance) 
          #out.sf <-  st_union(out.sf,agr.int)  ; plot(st_geometry(out.sf))
          #out.sf = st_union(out.sf); plot(st_geometry(out.sf),add = T)
          #out.sf = st_cast(out.sf,"POLYGON")
          
          #clean up afterwards
          rm(agr,agr.int,agr.int.df)
          
# Industry - Mining  
mine <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="IN_MINE_B") # reading in as geometry?      
          #mine <- st_make_valid(mine)
          mine.int = st_intersection(b.aoi,mine)   # intersect with ranges
          mine.int$Area_ha <- round(as.numeric(st_area(mine.int)/10000),2)
          # plot(st_geometry(seis.int))
          mine.int.df = data.frame(mine.int) 
          mine.int.df= mine.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(Area_ha = sum(Area_ha))
          
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,mine.int.df)   ## add to table Herd key 
          write.csv(out.tab,paste(out.dir,"out.table.csv",sep = ""),row.names = FALSE)
          
          ##OUTPUT 2: Individual dist layer (spatial)
          st_write(mine.int,paste(shape.out.dir,"D_mine_int.shp")) # generate spatial products 
          
          ##OUTPUT 3: Spatial layer to add to (cumulative disturbance) 
          #out.sf <-  st_union(out.sf,trail.int)  ; plot(st_geometry(out.sf))
          #out.sf = st_union(out.sf); plot(st_geometry(out.sf),add = T)
          #out.sf = st_cast(out.sf,"POLYGON")
          
          #clean up afterwards
          rm(mine,mine.int,mine.int.df)
          
# Industry -  POwer
po <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="IN_POW_B") # reading in as geometry?      
          po <- st_make_valid(po)
          po.int = st_intersection(b.aoi,po)   # intersect with ranges
          po.int$Area_ha <- round(as.numeric(st_area(po.int)/10000),2)
          # plot(st_geometry(seis.int))
          po.int.df = data.frame(po.int) 
          po.int.df= po.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(Area_ha = sum(Area_ha))
          
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,po.int.df)   ## add to table Herd key 
          write.csv(out.tab,paste(out.dir,"out.table.csv",sep = ""),row.names = FALSE)
          
          ##OUTPUT 2: Individual dist layer (spatial)
          st_write(po.int,paste(shape.out.dir,"D_pow_int.shp")) # generate spatial products 
          
          ##OUTPUT 3: Spatial layer to add to (cumulative disturbance) 
          #out.sf <-  st_union(out.sf,po.int)  ; plot(st_geometry(out.sf))
          #out.sf = st_union(out.sf); plot(st_geometry(out.sf),add = T)
          #out.sf = st_cast(out.sf,"POLYGON")
          
          #clean up afterwards
          rm(po,po.int,po.int.df)

# Industry -  oil and gas
og <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="IN_OG_B") # reading in as geometry?      
          og <- st_make_valid(og)
          og.int = st_intersection(b.aoi,og)   # intersect with ranges
          og.int$Area_ha <- round(as.numeric(st_area(og.int)/10000),2)
          # plot(st_geometry(seis.int))
          og.int.df = data.frame(og.int) 
          og.int.df = og.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(Area_ha = sum(Area_ha))
          
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,og.int.df)   ## add to table Herd key 
          write.csv(out.tab,paste(out.dir,"out.table.csv",sep = ""),row.names = FALSE)
          
          ##OUTPUT 2: Individual dist layer (spatial)
          st_write(og.int,paste(shape.out.dir,"D_og_int.shp")) # generate spatial products 
          
          ##OUTPUT 3: Spatial layer to add to (cumulative disturbance) 
          #out.sf <-  st_union(out.sf,og.int)  ; plot(st_geometry(out.sf))
          #out.sf = st_union(out.sf); plot(st_geometry(out.sf),add = T)
          #out.sf = st_cast(out.sf,"POLYGON")
          
          #clean up afterwards
          rm(og,og.int,og.int.df)
          
          
##################################################################################################
# Natural Disturbance (PEst and Fire (by year))
#          
          
# PEst 
pest <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="PEST_B") # reading in as geometry?      
          pest.int = st_intersection(b.aoi,pest)   # intersect with ranges
          pest.int$Area_ha <- round(as.numeric(st_area(pest.int)/10000),2)
          # plot(st_geometry(seis.int))
          pest.int.df = data.frame(pest.int) 
          pest.int.df = pest.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(Area_ha = sum(Area_ha))
          
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,pest.int.df)   ## add to table Herd key 
          write.csv(out.tab,paste(out.dir,"out.table.csv",sep = ""),row.names = FALSE)
          
          ##OUTPUT 2: Individual dist layer (spatial)
          st_write(pest.int,paste(shape.out.dir,"D_pest_int.shp")) # generate spatial products 
          
          ##OUTPUT 3: Spatial layer to add to (cumulative disturbance) 
          #out.sf <-  st_union(out.sf,po.int)  ; plot(st_geometry(out.sf))
          #out.sf = st_union(out.sf); plot(st_geometry(out.sf),add = T)
          #out.sf = st_cast(out.sf,"POLYGON")
          
          #clean up afterwards
          rm(pest,pest.int,pest.int.df)
          
# FIRE BY YEAR 
fire <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="FIRE_Yr_B") # reading in as geometry?      
         fire.int = st_intersection(b.aoi,fire)   # intersect with ranges
         fire.int$Area_ha <- round(as.numeric(st_area(fire.int)/10000),2)
          # plot(st_geometry(seis.int))
         fire.int.df = data.frame(fire.int) 
         fire.int.df = fire.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(Area_ha = sum(Area_ha))
         
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,fire.int.df)   ## add to table Herd key 
          write.csv(out.tab,paste(out.dir,"out.table.csv",sep = ""),row.names = FALSE)
          
          ##OUTPUT 2: Individual dist layer (spatial)
          st_write(fire.int,paste(shape.out.dir,"D_fire_yr_int.shp")) # generate spatial products 
          
          ##OUTPUT 3: Spatial layer to add to (cumulative disturbance) 
          #out.sf <-  st_union(out.sf,po.int)  ; plot(st_geometry(out.sf))
          #out.sf = st_union(out.sf); plot(st_geometry(out.sf),add = T)
          #out.sf = st_cast(out.sf,"POLYGON")
          
          #clean up afterwards
          rm(fire,fire.int,fire.int.df)
          
###################################################################################
# Calculate total disturb using RSEA cumulative values 
###################################################################################

# ANTROPOGENIC 
anth <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="AOI_anth_C_poly") # reading in as geometry?      
          anth <- st_make_valid(anth)
          anth.int = st_intersection(b.aoi,anth)   # intersect with ranges
          anth.int$Area_ha <- round(as.numeric(st_area(anth.int)/10000),2)
          # plot(st_geometry(seis.int))
          anth.int.df = data.frame(anth.int) 
          anth.int.df = anth.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(Area_ha = sum(Area_ha))
          
          
          
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,anth.int.df)   ## add to table Herd key 
          write.csv(out.tab,paste(out.dir,"out.table.csv",sep = ""),row.names = FALSE)
          
          ##OUTPUT 2: Individual dist layer (spatial)
          st_write(anth.int,paste(shape.out.dir,"D_Anth_int.shp")) # generate spatial products 
          
          #clean up afterwards
          rm(anth,anth.int,anth.int.df)
          
# Matural 
nat <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="AOI_anth_C_poly") # reading in as geometry?      
          
      anth.int = st_intersection(b.aoi,anth)   # intersect with ranges
          anth.int$Area_ha <- round(as.numeric(st_area(anth.int)/10000),2)
          # plot(st_geometry(seis.int))
          anth.int.df = data.frame(anth.int) 
          anth.int.df = anth.int.df %>% 
            group_by(Range,Zone,ThemeName) %>% 
            summarise(Area_ha = sum(Area_ha))
          
          
          
          ##OUTPUT 1 : DATA table
          out.tab <- bind_rows(out.tab,anth.int.df)   ## add to table Herd key 
          write.csv(out.tab,paste(out.dir,"out.table.csv",sep = ""),row.names = FALSE)
          
          ##OUTPUT 2: Individual dist layer (spatial)
          st_write(anth.int,paste(shape.out.dir,"D_Anth_int.shp")) # generate spatial products 
          
          #clean up afterwards
          rm(anth,anth.int,anth.int.df)
          

                     
             
          
          [13] "AOI_anth_C_linear" "AOI_anth_C_poly"   "AOI_nat_C_poly"      
          
 
          
          
          
          
          
          
          OLD STUYFFF
##################################################################################
##################################################################################

#### NATURAL DISTURBANCE   

## BURN:
# Add to Burn information Break down of burns into 0-40 and 40-80 years (and total) 
# Break down burns into fire years (same as cutblocks) 

b.r.0 = st_read(dsn = Intersect, layer ="Burn_combo_int")
b.r.0$TimeSinceBurn = 2018-b.r.0$FIRE_YEAR #; plot(b.r.0$Shape)
b.r.0 <- st_intersection(b.range,b.r.0)

b.r.00 = b.r.0
b.r.00 <- st_cast(b.r.00,"POLYGON")
b.r.00$area = st_area(b.r.00)
head(b.r.00)

#sort(unique(b.r.0$TimeSinceBurn))  # 0 - 40 years range 
# burns 0-40 years 
b.r.0.40 = b.r.0[b.r.0$TimeSinceBurn <41,]; #sort(unique(b.r.0$TimeSinceBurn)) 
b.r.0.40 = st_intersection(b.range,b.r.0.40)
b.r.0.40 <- st_cast(b.r.0.40 ,"POLYGON")
b.r.0.40$area = st_area(b.r.0.40)
#st_write(b.r.0.40,"Dist_R_burn.0.40.shp")       #write out individual dist_layer for Range

all.dis.burn = st_union(b.r.0.40)

# burns 41-80 years 
b.r.40.80 = b.r.0[b.r.0$TimeSinceBurn>40,]; #sort(unique(b.r.40.80$TimeSinceBurn)) 
b.r.40.80 = st_intersection(b.range,b.r.40.80)
b.r.40.80 <- st_cast(b.r.40.80 ,"POLYGON")
b.r.40.80$area = st_area(b.r.40.80)
#st_write(b.r.40.80,"Dist_R_burn.40.80.shp")       #write out individual dist_layer for Range

## All years of burns 
b.r.0 = st_cast(b.r.0,"POLYGON");  
b.r.0 = st_union(b.r.0) ; #plot(b.r.c0.40)
b.r.0 <- st_intersection(b.range,b.r.0)
b.r.0$area = st_area(b.r.0)
#all.burn = sum(st_area(b.r.0)) 
#plot(st_geometry(b.r.0))
#st_write(b.r.0 ,"Dist_R_burn.0.80.shp")       #write out individual dist_layer for Range
      
# work with the data frames for 0-40 years ## RANGE 
          b.r.0.40.df = data.frame(b.r.0.40)        # calculate the length per range 
          b.r.00.df = data.frame(b.r.00)
          
          # add a column to differentiate the age brackets of cutblocks 
          b.r.00.df <- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1958 & FIRE_YEAR <= 1967,1958,0))
          b.r.00.df <- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1968 & FIRE_YEAR <= 1977,1968,dec.period))
          b.r.00.df<- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1978 & FIRE_YEAR <= 1987,1978,dec.period))
          b.r.00.df<- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1988 & FIRE_YEAR <= 1997,1988,dec.period))   
          b.r.00.df<- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1998 & FIRE_YEAR <= 2007,1998,dec.period))   
          b.r.00.df<- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 2008 & FIRE_YEAR <= 2018,2008,dec.period)) 
          #b.r.0.40.df[b.r.0.40.df$dec.period == 0,]
          #head(b.r.0.40.df)
          #unique(b.r.0.40.df$dec.period)
          
          # output the amount of burns by range (all years (0-80))  
          b.r.0.df = data.frame(b.r.0)
          r.burn.df.out  = b.r.0.df  %>% group_by(Range) %>% summarise(R_burn0_80_m2 = sum(area))
          
          # output the amount of burns by range (all years (0 - 40)  
          r.burn.df.out.0.40  = b.r.0.40.df %>% group_by(Range) %>% summarise(R_burn0_40_m2 = sum(area))
          
          # output the amount of burns by range (all years (40-80))  
          b.r.40.80.df = data.frame(b.r.40.80)
          r.burn.df.out.40.80  = b.r.40.80.df %>% group_by(Range) %>% summarise(R_burn40_80_m2 = sum(area))
          
          #output the amount of cutblock per decade (all years) 
          b.r.00.df.temp  =  b.r.00.df %>% group_by(Range,dec.period) %>% summarise(R_burn_dec_m2 = sum(area))
        
          # aggregate to single datset 
          burn.range = left_join(r.burn.df.out,r.burn.df.out.0.40)
          burn.range = left_join(burn.range,r.burn.df.out.40.80 )
         
    #############################
    #### CORE: intersect with core and calculate length per range
    # all years: 
          c.burn = st_intersection(b.core.r,b.r.00)   # intersect with core
          c.burn <- st_cast(c.burn,"POLYGON")
          c.burn$area <- st_area(c.burn)
          
    # 0 - 40 years 
          c.burn0.40 = st_intersection(b.core.r,b.r.0.40)   # intersect with core
          c.burn0.40 <- st_cast(c.burn0.40,"POLYGON")
          c.burn0.40$area <- st_area(c.burn0.40)
          
    # 41 - 80 years
          c.burn40.80 = st_intersection(b.core.r,b.r.40.80)   # intersect with core
          c.burn40.80 <- st_cast(c.burn40.80,"POLYGON")
          c.burn40.80$area <- st_area(c.burn40.80)
      
     ## Working with data frames
          
          c.burn.00.df = data.frame(c.burn)        # calculate the length per range 
          
          # add a column to differentiate the age brackets of cutblocks 
          c.burn.00.df <- mutate(c.burn.00.df ,dec.period = ifelse(FIRE_YEAR >= 1958 & FIRE_YEAR <= 1967,1958,0))
          c.burn.00.df  <- mutate(c.burn.00.df ,dec.period = ifelse(FIRE_YEAR >= 1968 & FIRE_YEAR <= 1977,1968,dec.period))
          c.burn.00.df <- mutate(c.burn.00.df ,dec.period = ifelse(FIRE_YEAR >= 1978 & FIRE_YEAR <= 1987,1978,dec.period))
          c.burn.00.df <- mutate(c.burn.00.df ,dec.period = ifelse(FIRE_YEAR >= 1988 & FIRE_YEAR <= 1997,1988,dec.period))   
          c.burn.00.df <- mutate(c.burn.00.df ,dec.period = ifelse(FIRE_YEAR >= 1998 & FIRE_YEAR <= 2007,1998,dec.period))   
          c.burn.00.df <- mutate(c.burn.00.df ,dec.period = ifelse(FIRE_YEAR >= 2008 & FIRE_YEAR <= 2018,2008,dec.period)) 
          #c.burn0.40.df[c.burn0.40.df$dec.period == 0,]
          #head(c.burn0.40.df)
          #unique(c.burn0.40.df$dec.period)

          # output the amount of burns by range (all years (0-80))  
          c.burn.df = data.frame(c.burn)
          c.burn.df.out  = c.burn.df %>% group_by(Range) %>% summarise(C_burn0_80_m2 = sum(area))
          
          # output the amount of burns by range (all years (0 - 40)  
          c.burn.df.out.0.40  = data.frame(c.burn0.40) %>% group_by(Range) %>% summarise(C_burn0_40_m2 = sum(area))
          
          # output the amount of burns by range (all years (40-80))  
          c.burn40.80.df = data.frame(c.burn40.80)
          c.burn.df.out.40.80  = c.burn40.80.df  %>% group_by(Range) %>% summarise(C_burn40_80_m2 = sum(area))
          
          #output the amount of cutblock per decade (all years) 
          c.burn.00.df.temp  =  c.burn.00.df %>% group_by(Range,dec.period) %>% summarise(C_burn_dec_m2 = sum(area))
          
          # aggregate to single datset 
          burn.core = left_join(c.burn.df.out,c.burn.df.out.0.40)
          burn.core = left_join(burn.core,c.burn.df.out.40.80)
          
          ## Combine to burn summary and write out       
# burn by range 
burn.all = left_join(burn.range,burn.core)
burn.all$P_burn0_80_m2 = burn.all$R_burn0_80_m2 - burn.all$C_burn0_80_m2  
burn.all$P_burn0_40_m2 = burn.all$R_burn0_40_m2 - burn.all$C_burn0_40_m2
burn.all$P_burn40_80_m2 = burn.all$R_burn40_80_m2 - burn.all$C_burn40_80_m2

# burn by range and decade
burn.by.temp = left_join(b.r.00.df.temp,c.burn.00.df.temp) 
burn.by.temp$P_burn_dec_m2 = burn.by.temp$R_burn_dec_m2 - burn.by.temp$C_burn_dec_m2        

###########################################

# join the cutlock with burn decade data. 
all.temp.data = left_join(burn.by.temp,Temp.cut)
all.temp.data[is.na(all.temp.data)]<- 0 

############################################
### PEST 
r.pest <-  st_read(dsn = Intersect, layer ="Pest_r_Select_Intersect")
r.pest <-  st_intersection(b.range,r.pest)
        
        # All disturbance 
        r.pest.0 = st_cast(r.pest,"POLYGON")
        r.pest.0 = st_union(r.pest.0)
        #all.pest = sum(st_area(r.pest.0)) ; plot(st_geometry(r.pest.0))  

        ## RANGE SCALE:   
        # work with the data frames for 0-40 years ## RANGE 
        r.pest = st_cast(r.pest,"POLYGON") 
        r.pest$area.m = st_area(r.pest)
        r.pest.df = data.frame(r.pest)        # calculate the length per range 
        
        # add a column to differentiate the age brackets of pest capture
        r.pest.df<- r.pest.df %>% mutate(dec.period = ifelse(CAPTURE_YEAR >= 1988 & CAPTURE_YEAR <= 1997,1988,0))   
        r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 1998 & CAPTURE_YEAR <= 2007,1998,dec.period))   
        r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 2008 & CAPTURE_YEAR <= 2018,2008,dec.period)) 
        #r.pest.df[r.pest.df$dec.period == 0,]
        #head(r.pest.df)
        #unique(r.pest.df$dec.period)
        
        # output the amount of burns by range (all years (0-80))  
        r.pest.df.out  = r.pest.df %>% group_by(Range) %>% summarise(R_pest_m2 = sum(area.m))
        #output the amount of cutblock per decade (all years) 
        r.pest.df.out.temp  =   r.pest.df %>% group_by(Range,dec.period) %>% summarise(R_pest_dec_m2 = sum(area.m))
        #output the amount of cutblock per decade (all years) 
        r.pest.df.out.type  = r.pest.df %>% group_by(Range,PEST_SPECIES_CODE) %>% summarise(R_pest_type_m2 = sum(area.m))
        
        ########################################
        ## CORE SCALE
        c.pest <-  st_intersection(b.core.r,r.pest)
        # All disturbance 
        c.pest.0 = st_cast(c.pest,"POLYGON")
        c.pest.0= st_union(c.pest.0)
        #all.pest = sum(st_area(r.pest.0)) ; plot(st_geometry(r.pest.0))  
        
        # work with the data frames for 0-40 years ## RANGE 
        c.pest = st_cast(c.pest,"POLYGON") 
        c.pest$area.m = st_area(c.pest)
        c.pest.df = data.frame(c.pest)        # calculate the length per range 
        
        # add a column to differentiate the age brackets of pest capture
        c.pest.df<- c.pest.df %>% mutate(dec.period = ifelse(CAPTURE_YEAR >= 1988 & CAPTURE_YEAR <= 1997,1988,0))   
        c.pest.df<- mutate(c.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 1998 & CAPTURE_YEAR <= 2007,1998,dec.period))   
        c.pest.df<- mutate(c.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 2008 & CAPTURE_YEAR <= 2018,2008,dec.period)) 
        #c.pest.df[c.pest.df$dec.period == 0,]
        #head(r.pest.df)
        #unique(r.pest.df$dec.period)
        
        # output the amount of pests by range (all years (0-80))  
        c.pest.df.out  = c.pest.df %>% group_by(Range) %>% summarise(C_pest_m2 = sum(area.m))
        #output the amount of cutblock per decade (all years) 
        c.pest.df.out.temp  = c.pest.df %>% group_by(Range,dec.period) %>% summarise(C_pest_dec_m2 = sum(area.m))
        #output the amount of cutblock per decade (all years) 
        c.pest.df.out.type  = c.pest.df %>% group_by(Range,PEST_SPECIES_CODE) %>% summarise(C_pest_type_m2 = sum(area.m))
        
        # aggregate range and core
        pest.range = left_join(r.pest.df.out,c.pest.df.out)
        pest.range$P_pest_m2 =  pest.range$R_pest_m2 -  pest.range$C_pest_m2 
        # by temp
        pest.temp = left_join( r.pest.df.out.temp, c.pest.df.out.temp)
        pest.temp[is.na( pest.temp)]<- 0 
        # by type
        pest_type <-  left_join( r.pest.df.out.type, c.pest.df.out.type)
        pest_type$P_pest_type_m2 = pest_type$R_pest_type_m2 - pest_type$C_pest_type_m2
        write.csv(pest_type, paste(out.dir,"Pest_by_type_RPC.csv",sep =""))
        
        
   
############################################################################
# Aggregate natural disturbance    
        
# Pests + Burns (0-40 years combined)
        
############################################################################       
    
# Step 1 aggregate table and output 
        
out.temp = left_join(burn.all, pest.range)   
write.csv(out.temp,paste(out.dir,"Nat_dist_type_RPC.csv",sep =""))    

# write out temporal datasets: 
all.temp.data <-left_join(all.temp.data,pest.temp)           
write.csv(all.temp.data,paste(out.dir,"Temporal_decade_dist_RPC.csv",sep ="")) 

       
# Add disturbance combined: UNION Natural Dist 
r.pest.0 = st_union(r.pest.0) 
out.nat = st_union(all.dis.burn, r.pest.0) ; plot(st_geometry(out.nat))

# write out shape file 
out.nat2 = st_cast(out.nat,"POLYGON")
xnat.area2 = sum(st_area(out.nat2)) 
x = st_simplify(out.nat2)    ;  plot(st_geometry(x)) 
st_write(x, "All_natural_dissolve.shp") # this writes out as single layer   

# calculate the range and core overlaps for "all disturbance" 
out.nat1 = st_union(out.nat);
st_make_valid(out.nat1)

out.nat1 <- st_cast(out.nat1,"POLYGON")

# calcualte the totals for all nat. disturbacne 
out.nat.r = st_intersection(b.range,out.nat1)
out.nat.c = st_intersection(b.core.r,out.nat1)
# add areas
out.nat.r$area.m = st_area(out.nat.r)
out.nat.c$area.m = st_area(out.nat.c)
# conver to DF
out.nat.r.df <-data.frame(out.nat.r)
out.nat.c.df <-data.frame(out.nat.c)
# summarise data 
range.out  <- out.nat.r.df%>%  group_by(Range) %>% summarise(R_allnatdis_m = sum(area.m))
core.out <- out.nat.c.df %>%  group_by(Range) %>% summarise(C_allnatdis_m = sum(area.m))
# 

all.dis<- left_join(range.out,core.out)
all.dis$P_allnatdis_m  = all.dis$R_allnatdis_m  - all.dis$C_allnatdis_m 
write.csv(all.dis,paste(out.dir,"Combines_Nat_dist_RPC.csv",sep ="")) 


###################################################################################

# once this script is run - then use the 04_Disturbance_Data_summary.R script 
# to collate all the csv's generated as part of this script and oputput the main summary table for reporting


