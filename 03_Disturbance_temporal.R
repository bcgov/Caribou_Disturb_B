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
## General notes: 
## This script is used to look at more detailed break-down of cutblocks, fire and pests and to create plots


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


## set your output directory 
data.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/Boreal/Data/"
out.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/Boreal/Analysis/"
temp.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/Boreal/temp/"

# running on local drive
data.dir = "C:/Temp/Boreal/Data/"
out.dir = "C:/Temp/Boreal/Analysis/"
shape.out.dir =  "C:/Temp/Boreal/Analysis/dist_int_layers/"
temp.dir = "C:/Temp/Boreal/temp/"

#out.dir ="C:/Temp/BorealV2/Outputs/"


## Set your input geodatabases (this will be where you saved your arcmap exports)
## edit these to your filepath and name of gdb

Base  = "Base_data.gdb" # clipped disturb layers
#AOI = "AOI_data.gdb"   # AOI

## List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
base_list <- ogrListLayers(paste(data.dir,Base,sep = "")); print(base_list)
#aoi_list <- ogrListLayers(paste(data.dir,AOI,sep = "")); print(aoi_list)

##########################################################################################################

# Read in herd boundary layers and join to single file 

Herd_key <- read.csv(paste(data.dir,"Herd_key.csv",sep = ""))
b.aoi <- st_read(paste(data.dir,"Boreal_herd_bdry.shp",sep = ""))
st_crs(b.aoi)<- 3005
#plot(b.aoi)
#plot(st_geometry(b.aoi))

Herd_key$ThemeName = "Total_Area"
Herd_key <- Herd_key %>% dplyr::select(Range, Zone, ThemeName, Area_ha) 
out.tab <- Herd_key 

########################################################################################################
# Read in temporal data sets 
# FIRE

fire <-  st_read(dsn=paste(data.dir,Base,sep = ""),layer="FIRE_Yr_B") # reading in as geometry?      
fire.int = st_intersection(b.aoi,fire)   # intersect with ranges
fire.int$TimeSinceBurn = 2018-fire.int$FIRE_YEAR  # add time since burn 

# Break up into temporal sections : 

# add a column to differentiate the age brackets of cutblocks 
fire.int<- mutate(fire.int,dec.period = ifelse(FIRE_YEAR >= 1958 & FIRE_YEAR <= 1967,1958,
                                               ifelse(FIRE_YEAR >= 1968 & FIRE_YEAR <= 1977,1968,     
                                                      ifelse(FIRE_YEAR >= 1978 & FIRE_YEAR <= 1987,1978,      
                                                             ifelse(FIRE_YEAR >= 1988 & FIRE_YEAR <= 1997,1988,       
                                                                    ifelse(FIRE_YEAR >= 1998 & FIRE_YEAR <= 2007,1998,
                                                                           ifelse(FIRE_YEAR >= 2008 & FIRE_YEAR <= 2018,2008,0)))))))

##Pl;ot the data to see spread of fires.  Note this is just using the 
#p = ggplot(fire.int,aes(dec.period)) + geom_bar() + facet_wrap(~Range) + scale_x_continuous(limits = c(1940,2018))


# get the fires in the last 40 years: 
fire.040 <- fire.int %>% filter(dec.period >= 1978)

decades <- c(unique(fire.040$dec.period)) 
fire.ranges <- as.character(unique(fire.040$Range)) 

for (i in 1:length(decades)){ 
  #i = 1
  y = decades[i]
  tdata <- fire.040 %>% filter(dec.period == paste(y))
  tdata <- st_union(tdata)
  tdata <- st_intersection(b.aoi,tdata)
  plot(st_geometry(tdata))
  
  tdata$Area_ha <- round(as.numeric(st_area(tdata)/10000),2)
  tdata.df = data.frame(tdata) 
  tdata.df = tdata.df %>% 
    group_by(Range,Zone) %>% 
    summarise(Area_ha = sum(Area_ha))
  tdata.df$Decade = paste(y)
  
  # add each time period to overall table
  out.tab <- left_join(out.tab,tdata.df, by = c('Range','Zone'))
} 

write.csv(out.tab, paste(out.dir,"fire_temp_data.csv",sep = ""),row.names= FALSE)


######################

## Clean up this table 

out.tab <- read.csv(paste(out.dir,"fire_temp_data.csv",sep = ""))
out.tab <- out.tab %>% mutate(Area_pc_1978 = round(Area_ha.y/ Area_ha.x*100,2),
                              Area_pc_1988 = round(Area_ha.x.x/ Area_ha.x*100,2),
                              Area_pc_1998 = round(Area_ha.y.y/ Area_ha.x*100,2),
                              Area_pc_2008 = round(Area_ha/Area_ha.x*100,2),
                              Area_ha_1978 = Area_ha.y,
                              Area_ha_1988 = Area_ha.x.x,
                              Area_ha_1998 = Area_ha.y.y,
                              Area_ha_2008 = Area_ha)


out.tab.ha <-out.tab %>% dplyr::select(Range,Zone,Area_ha.x,Area_ha_1978,Area_ha_1988, Area_ha_1998, Area_ha_2008)
out.tab.pc <-out.tab %>% dplyr::select(Range,Zone,Area_pc_1978,Area_pc_1988, Area_pc_1998, Area_pc_2008)

# format the tabel 
out.tab.ha <- as.data.frame(t(out.tab.ha))
out.tab.pc <- as.data.frame(t(out.tab.pc))
# write out 
write.csv(out.tab.ha,paste(out.dir,"fire_temp_formated_ha.csv",sep = ""))
write.csv(out.tab.pc,paste(out.dir,"fire_temp_formated_pc.csv",sep = ""))

#########################################################################################################################

## CUT BLOCKS 







































#### NATURAL DISTURBANCE  

## BURN:
# Add to Burn information Break down of burns into 0-40 and 40-80 years (and total) 
# Break down burns into fire years (same as cutblocks) 

b.r.0 = st_read(dsn = Intersect, layer ="Burn_combo_int")
b.r.0$TimeSinceBurn = 2018-b.r.0$FIRE_YEAR #; plot(b.r.0$Shape)
b.r.0 <- st_buffer(b.r.0, 250) 
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
#st_write(b.r.0.40,"Dist_R_burn.0.40_500.shp")       #write out individual dist_layer for Range

all.dis.burn = st_union(b.r.0.40)

# burns 41-80 years 
b.r.40.80 = b.r.0[b.r.0$TimeSinceBurn>40,]; #sort(unique(b.r.40.80$TimeSinceBurn)) 
b.r.40.80 = st_intersection(b.range,b.r.40.80)
b.r.40.80 <- st_cast(b.r.40.80 ,"POLYGON")
b.r.40.80$area = st_area(b.r.40.80)
#st_write(b.r.40.80,"Dist_R_burn.40.80_500.shp")       #write out individual dist_layer for Range

## All years of burns 
b.r.0 = st_cast(b.r.0,"POLYGON");  
b.r.0 = st_union(b.r.0) ; #plot(b.r.c0.40)
b.r.0 <- st_intersection(b.range,b.r.0)
b.r.0$area = st_area(b.r.0)
#all.burn = sum(st_area(b.r.0)) 
#plot(st_geometry(b.r.0))
#st_write(b.r.0 ,"Dist_R_burn.0.80_500.shp")       #write out individual dist_layer for Range

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

#st_write(fire.int,paste(shape.out.dir,"D_fire_yr_int.shp")) # generate spatial products

##OUTPUT 3: Spatial layer to add to (cumulative disturbance) 
#out.sf <-  st_union(out.sf,po.int)  ; plot(st_geometry(out.sf))
#out.sf = st_union(out.sf); plot(st_geometry(out.sf),add = T)
#out.sf = st_cast(out.sf,"POLYGON")

#clean up afterwards
rm(fire,fire.int,fire.int.df)









##############################################################################################
# cutblock data 
################################################################################################

## Cutblock ## this is all years of consolidated cutblock layer 
# Split out age classes
b.r.c = st_read(dsn = Intersect , layer = "R_cutblock")
b.r.c$TimeSinceCut = 2018-b.r.c$HARVEST_YEAR; #plot(b.r.c$Shape); plot(b.core.sf$Shape,add = TRUE)
#sort(unique(b.r.c$TimeSinceCut)) # note in the boreal the oldest age cut is 56 years 

# cutblocks 0-80 years
b.r.c0.80 = b.r.c[b.r.c$TimeSinceCut < 81,] ; unique(b.r.c0.80$TimeSinceCut); plot(b.r.c0.80$Shape)
b.r.c0.80.0 = b.r.c0.80
b.r.c0.80 = st_cast(b.r.c0.80,"POLYGON"); #st_is_valid(b.r.c0.80)
#all.cut = sum(st_area(b.r.c0.80))
          
          ###############
          ## RANGE: intersect with range and calculate length per range
          r.cut = st_intersection(b.range,b.r.c0.80.0)     # intersect with ranges
          r.cut <- st_cast(r.cut,"POLYGON")
          r.cut$area <- st_area(r.cut)
          head(r.c)
          
          #working with data frame data 
          r.cut.df = data.frame(r.cut)        # calculate the length per range 
          
          # add a column to differentiate the age brackets of cutblocks 
          r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1958 & HARVEST_YEAR <= 1967,1958,0))
          r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1968 & HARVEST_YEAR <= 1977,1968,dec.period))
          r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1978 & HARVEST_YEAR <= 1987,1978,dec.period))
          r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1988 & HARVEST_YEAR <= 1997,1988,dec.period))   
          r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1998 & HARVEST_YEAR <= 2007,1998,dec.period))   
          r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 2008 & HARVEST_YEAR <= 2017,2008,dec.period)) 
          #r.cut.df[r.cut.df$dec.period == 0,], #unique(r.cut.df$dec.period)
          
          # output the amount of cutblock by range (all years (0-80))  
          r.cut.df.out  = r.cut.df %>% group_by(Range) %>% summarise(R_cut0_80_m2 = sum(area))
          # output the amount of cutblock by range (all years (0-40))  
          r.cut.df.out.0.40 <- r.cut.df %>% filter(dec.period >= 1978) %>% group_by(Range) %>% summarise(R_cut0_40_m2 = sum(area))
          #output the amount of cutblock per decade (all years) 
          r.cut.df.out.temp  = r.cut.df %>% group_by(Range,dec.period) %>%  summarise(R_cut_dec_m2 = sum(area))
          
          ###############
          ### CORE: intersect with core and calculate length per range
          c.cut = st_intersection(b.core.r,b.r.c0.80.0)   # intersect with core
          c.cut <- st_cast(c.cut,"POLYGON")
          c.cut$area <- st_area(c.cut)
          
          ##plot(st_geometry(c.cut))
          #st_write(r.dams,"Dist_C_cutblock0.80.shp.shp")       #write out individual dist_layer for core
          c.cut.df = data.frame(c.cut)        # calculate the area per range 
          
          # set up the age class
          # add a column to differentiate the age brackets of cutblocks 
          c.cut.df <- mutate(c.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1958 & HARVEST_YEAR <= 1967,1958,0))
          c.cut.df <- mutate(c.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1968 & HARVEST_YEAR <= 1977,1968,dec.period))
          c.cut.df <- mutate(c.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1978 & HARVEST_YEAR <= 1987,1978,dec.period))
          c.cut.df <- mutate(c.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1988 & HARVEST_YEAR <= 1997,1988,dec.period))   
          c.cut.df <- mutate(c.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1998 & HARVEST_YEAR <= 2007,1998,dec.period))   
          c.cut.df <- mutate(c.cut.df,dec.period = ifelse(HARVEST_YEAR >= 2008 & HARVEST_YEAR <= 2017,2008,dec.period)) 
          #c.cut.df[c.cut.df$dec.period == 0,]
          #unique(c.cut.df$dec.period)
          
          # all cutblocks (0-80 years)
          c.cut.df.out  = c.cut.df %>% group_by(Range) %>% summarise(C_cut0_80_m2 = sum(area))
          # output the amount of cutblock by range (all years (0-40))  
          c.cut.df.out.0.40 <- c.cut.df %>% filter(dec.period >= 1978) %>% group_by(Range) %>% summarise(C_cut0_40_m2 = sum(area))
          #output the amount of cutblock per decade
          c.cut.df.out.temp  = c.cut.df %>% group_by(Range,dec.period) %>% summarise(C_cut_dec_m2 = sum(area))
          
          # add 0-80 years data 
          out.cut = left_join(r.cut.df.out,c.cut.df.out , all = both)
          out.cut$P_cut0_80_m2 = out.cut$R_cut0_80_m2 - out.cut$C_cut0_80_m2
          #range.layer.cals <- left_join(range.layer.cals,out.cut) ; range.layer.cals [is.na(range.layer.cals )] <- 0
          
          # add 0-40 years data 
          out.cut0.40 = left_join(r.cut.df.out.0.40,c.cut.df.out.0.40 , all = both)
          out.cut0.40$P_cut0_40_m2 =  out.cut0.40$R_cut0_40_m2 -  out.cut0.40$C_cut0_40_m2
          range.layer.cals <- left_join(range.layer.cals,out.cut0.40) ; range.layer.cals [is.na(range.layer.cals )] <- 0

################################################################################################################
# Figure 1: make a plot of temporal and core.p.range for cutblocks

# add a column to differentiate the age brackets of cutblocks 
r.cut <- mutate(r.cut,dec.period = ifelse(HARVEST_YEAR >= 1958 & HARVEST_YEAR <= 1967,1958,0))
r.cut <- mutate(r.cut,dec.period = ifelse(HARVEST_YEAR >= 1968 & HARVEST_YEAR <= 1977,1968,dec.period))
r.cut <- mutate(r.cut,dec.period = ifelse(HARVEST_YEAR >= 1978 & HARVEST_YEAR <= 1987,1978,dec.period))
r.cut <- mutate(r.cut,dec.period = ifelse(HARVEST_YEAR >= 1988 & HARVEST_YEAR <= 1997,1988,dec.period))   
r.cut <- mutate(r.cut,dec.period = ifelse(HARVEST_YEAR >= 1998 & HARVEST_YEAR <= 2007,1998,dec.period))   
r.cut <- mutate(r.cut,dec.period = ifelse(HARVEST_YEAR >= 2008 & HARVEST_YEAR <= 2017,2008,dec.period)) 
#r.cut.df[r.cut.df$dec.period == 0,], #unique(r.cut.df$dec.period)
head(r.cut)

herds = c('Calendar','Chinchaga','Maxhamish','Snake-Sahtahneh','Westside Fort Nelson')

for (i in herds) {
  #ii = herds[4]
  r.temp <- r.cut[r.cut$Range == paste(i),]
  
  if(length(r.temp$Range)>0){
        b.range.temp <-  b.range[b.range$Range == paste(i),]
        b.core.temp <- b.core.r[b.core.r$Range ==paste(i),]
        
        p1 = ggplot() + 
          geom_sf(data = b.range.temp, fill = "grey94") + 
          geom_sf(data = b.core.temp, fill = "grey96") + 
          #geom_sf(data = r.temp, aes(fill =dec.period)) + ggtitle("Natural Disturbance per Herd Range") + theme_bw()
          geom_sf(data = r.temp) + facet_grid(.~dec.period)+  ggtitle(paste("Cutblocks Per Decade within the ",i," herd boundary",sep = "")) + 
          theme_bw() 
        p1
        ggsave(paste(out.dir,i,"_cutblock_temporal.png",sep = ""),width = 30, height = 15, units = "cm")  
                            }
              } 

############################################################################
# Figure 2: make a bar graph of the core/range/periperhy per herd. 
# format data together     

plot.data = left_join(r.cut.df.out.temp,c.cut.df.out.temp) 
plot.data[is.na(plot.data )] <- 0
plot.data$P_cut_dec_m2 = plot.data$R_cut_dec_m2 - plot.data$C_cut_dec_m2
# add the area values for Core/Perip/Range and calculate % 
plot.data <- left_join(plot.data, Herd_key) 
plot.data <- plot.data %>% 
  mutate (R_cutblock_pc = ((R_cut_dec_m2/10000)/R_area_ha) * 100) %>%
  mutate (P_cutblock_pc = ((P_cut_dec_m2/10000)/P_area_ha) * 100) %>%
  mutate (C_cutblock_pc = ((C_cut_dec_m2/10000)/C_area_ha) * 100) 

# output the details per core area: 
herds = c('Calendar','Chinchaga','Maxhamish','Snake-Sahtahneh','Westside Fort Nelson')

for (i in herds) { 
  i = herds[3]
  # write out core area details. 
  t.c = plot.data %>% filter(Range == paste(i)) %>% 
    dplyr:: select(Range,dec.period,R_cutblock_pc,P_cutblock_pc,C_cutblock_pc) %>%
    mutate(Range_ = R_cutblock_pc, Periphery_ = P_cutblock_pc, Core_ = C_cutblock_pc)
  t.c.c = t.c %>% gather(key = var_name, value = value, 6:ncol(t.c)) 
  t.c.c = t.c.c %>% 
    mutate(Decade = ifelse(dec.period == 1958,1958,
                           ifelse(dec.period == 1968,1968,
                                  ifelse(dec.period == 1978,1978,
                                         ifelse(dec.period == 1988,1988,
                                                ifelse(dec.period == 1998,1998,
                                                       ifelse(dec.period == 2008,2008,0)))))))
  
  t.c.group = t.c.c %>% 
    mutate(Age_group = ifelse(Decade >=1978,"0-40","41-80"))%>%
    group_by(Age_group,var_name)%>%
    summarise(sum= sum(value))
  
  if(length(t.c.c$Range)>0){
    # plot 1
    p = ggplot(t.c.c, aes(Decade,value)) + try(facet_wrap(~var_name))+ geom_bar(stat = 'identity') 
    p = p + ylab("Percentage") + xlab("Decade") + ggtitle(paste("Percent Cutblock (per area) from 1958 - 2017 (",i," herd)",sep = "" ))
    p = p + theme_bw()
    ggsave(paste(out.dir,i,"_cutblock_temp1.png",sep = ""))  
    # plot 2
    #p1 = ggplot(t.c.c, aes(var_name,value)) + facet_wrap(~Decade)+ geom_bar(stat = 'identity') 
    #p1 = p1 + ylab("Percentage") + xlab("") + ggtitle(paste("Percent Cutblock (per area) from 1958 - 2017 (",i," herd)",sep = "" ))
    #p1 = p1 + theme_bw()
    #ggsave(paste(out.dir,i,"_cutblock_temp2.png",sep = ""))  
    # plot 3
    p2 = ggplot(t.c.group, aes(var_name,sum)) + facet_wrap(~Age_group)+ geom_bar(stat = 'identity') 
    p2 = p2 + ylab("Percentage") + xlab("") + ggtitle(paste("Percent Cutblock (per area) for harvest (0-40 years) and 41-80 years (",i," herd)",sep = "" ))
    p2 = p2 + theme_bw()
    ggsave(paste(out.dir,i,"_cutblock_RPC_0_40_80.png",sep = "")) 
    
  } 
} 
##################################################################################
##################################################################################
# DATA SET 2:  PEST DATA - IBM vs IBB
##################################################################################
##################################################################################

# Prep spatial data 
r.pest <-  st_read(dsn = Intersect, layer ="Pest_r_Select_Intersect")
r.pest <- st_intersection(b.range,r.pest)

# prep table 
pest.type = read.csv(paste(out.dir,"Pest_by_type_RPC.csv",sep =""))
pest.type <- left_join(pest.type, Herd_key) 
pest.type <- pest.type %>% 
  mutate (Range. = ((R_pest_type_m2/10000)/R_area_ha) * 100) %>%
  mutate (Periphery = ((P_pest_type_m2/10000)/P_area_ha) * 100) %>%
  mutate (Core = ((C_pest_type_m2/10000)/C_area_ha) * 100) 
pest.type<- pest.type %>% 
  dplyr::select(c(X,Range,PEST_SPECIES_CODE,Range.,Periphery,Core))

pest.type.IBM <- pest.type %>% filter(PEST_SPECIES_CODE == "IBM")
pest.type.1 = pest.type.IBM %>% gather(key = PEST_SPECIES_CODE,value = value, 4:ncol(pest.type.IBM) )
pest.type.1$Species = "IBM"
pest.type.IBS <- pest.type %>% filter(PEST_SPECIES_CODE == "IBS")
pest.type.2 = pest.type.IBS %>% gather(key = PEST_SPECIES_CODE,value = value, 4:ncol(pest.type.IBS) )
pest.type.2$Species = "IBS"
pest.out <- rbind(pest.type.1,pest.type.2)


# Loop through each of the herds

herds = c('Calendar','Chinchaga','Maxhamish','Snake-Sahtahneh','Westside Fort Nelson')

for (i in herds) {
  #i = herds[2]
  r.temp <- r.pest[r.pest$Range == paste(i),]
  r.temp <- st_cast(r.temp,"POLYGON")
  
  ## figure 1
  
  # plot takes too long to Run 
  #if(length(r.temp$Range)>0){
  #  b.range.temp <-  b.range[b.range$Range == paste(i),]
  #  b.core.temp <- b.core.r[b.core.r$Range ==paste(i),]
  #  
  #  p1 = ggplot() + 
  #    geom_sf(data = b.range.temp, fill = "grey94") + 
  #    geom_sf(data = b.core.temp, fill = "grey96") +
  #    geom_sf(data = r.temp) + facet_grid(~PEST_SPECIES_CODE)+  ggtitle(paste("Pest Type within the ",i," herd boundary",sep = "")) +
  #    #geom_sf(data = r.temp) #+ggtitle(paste("Pest Type within the ",i," herd boundary",sep = "")) +
  #    theme_bw()
  #  p1
  #  ggsave(paste(out.dir,i,"_pest_type.png",sep = "")) #,width = 25, height = 10, units = "cm")  
                             # }
  
  ## Table 1: 
  t.pest.group = pest.out %>% 
          dplyr::filter(Range == paste(i))
    
  if(length(t.pest.group$Range)>0){
    # plot 1
    p = ggplot(t.pest.group , aes(PEST_SPECIES_CODE,value)) + try(facet_wrap(~Species))+ geom_bar(stat = 'identity') 
    p = p + ylab("Percentage") + xlab("") + ggtitle(paste("Percent Pest (per area) for IBM and IBS species  (",i," herd)",sep = "" ))
    p = p + theme_bw(); p
    ggsave(paste(out.dir,i,"_pest_type.png",sep = ""))  
                                  }
              } 
  
##################################################################################
##################################################################################
# DATA SET 3:  Burns - spatial and dates
##################################################################################
##################################################################################
## Presented for range area; 

# Add to Burn information Break down of burns into 0-40 and 40-80 years (and total) 
# Break down burns into fire years (same as cutblocks) 

b.r.0 = st_read(dsn = Intersect, layer ="Burn_combo_int")
b.r.0$TimeSinceBurn = 2018-b.r.0$FIRE_YEAR #; plot(b.r.0$Shape)

b.r.0 = st_intersection(b.range,b.r.0)
b.r.0 <- st_cast(b.r.0 ,"POLYGON")
b.r.0.40.df <-  b.r.0 

# add a column to differentiate the age brackets of cutblocks 
b.r.0.40.df <- mutate(b.r.0.40.df,dec.period = ifelse(FIRE_YEAR >= 1958 & FIRE_YEAR <= 1967,1958,0))
b.r.0.40.df <- mutate(b.r.0.40.df,dec.period = ifelse(FIRE_YEAR >= 1968 & FIRE_YEAR <= 1977,1968,dec.period))
b.r.0.40.df<- mutate(b.r.0.40.df,dec.period = ifelse(FIRE_YEAR >= 1978 & FIRE_YEAR <= 1987,1978,dec.period))
b.r.0.40.df<- mutate(b.r.0.40.df,dec.period = ifelse(FIRE_YEAR >= 1988 & FIRE_YEAR <= 1997,1988,dec.period))   
b.r.0.40.df<- mutate(b.r.0.40.df,dec.period = ifelse(FIRE_YEAR >= 1998 & FIRE_YEAR <= 2007,1998,dec.period))   
b.r.0.40.df<- mutate(b.r.0.40.df,dec.period = ifelse(FIRE_YEAR >= 2008 & FIRE_YEAR <= 2018,2008,dec.period)) 
b.r.0 = b.r.0.40.df[b.r.0.40.df$dec.period>0,]

for (i in herds) {
  #i = herds[5]
  r.temp <- b.r.0[b.r.0$Range == paste(i),]
  
  if(length(r.temp$Range)>0){
    b.range.temp <-  b.range[b.range$Range == paste(i),]
    b.core.temp <- b.core.r[b.core.r$Range ==paste(i),]
    
    p1 = ggplot() + 
      geom_sf(data = b.range.temp, fill = "grey94") + 
      geom_sf(data = b.core.temp, fill = "grey96") + 
      geom_sf(data = r.temp, col = "red") + facet_grid(.~dec.period)+  ggtitle(paste("Burns Per Decade within the ",i," herd boundary",sep = "")) + 
      theme_bw() 
    p1
    ggsave(paste(out.dir,i,"_burn_temporal.png",sep = ""))  
  }
} 




# output the amount of burns by range (all years (0-80))  
b.r.0.df = data.frame(b.r.0)
r.burn.df.out  = b.r.0.df  %>% group_by(Range) %>% summarise(R_burn0_80_m2 = sum(area))

# output the amount of burns by range (all years (0 - 40)  
r.burn.df.out.0.40  = b.r.0.40.df %>% group_by(Range) %>% summarise(R_burn0_40_m2 = sum(area))

# output the amount of burns by range (all years (40-80))  
b.r.40.80.df = data.frame(b.r.40.80)
r.burn.df.out.40.80  = b.r.40.80.df %>% group_by(Range) %>% summarise(R_burn40_80_m2 = sum(area))

#output the amount of cutblock per decade (all years) 
b.r.0.40.df.temp  =  b.r.0.40.df %>% group_by(Range,dec.period) %>% summarise(R_burn_dec_m2 = sum(area))

# aggregate to single datset 
burn.range = left_join(r.burn.df.out,r.burn.df.out.0.40)
burn.range = left_join(burn.range,r.burn.df.out.40.80 )

#############################
#### CORE: intersect with core and calculate length per range
# all years: 
c.burn = st_intersection(b.core.r,b.r.0)   # intersect with core
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

c.burn0.40.df = data.frame(c.burn0.40)        # calculate the length per range 

# add a column to differentiate the age brackets of cutblocks 
c.burn0.40.df<- mutate(c.burn0.40.df,dec.period = ifelse(FIRE_YEAR >= 1958 & FIRE_YEAR <= 1967,1958,0))
c.burn0.40.df <- mutate(c.burn0.40.df,dec.period = ifelse(FIRE_YEAR >= 1968 & FIRE_YEAR <= 1977,1968,dec.period))
c.burn0.40.df<- mutate(c.burn0.40.df,dec.period = ifelse(FIRE_YEAR >= 1978 & FIRE_YEAR <= 1987,1978,dec.period))
c.burn0.40.df<- mutate(c.burn0.40.df,dec.period = ifelse(FIRE_YEAR >= 1988 & FIRE_YEAR <= 1997,1988,dec.period))   
c.burn0.40.df<- mutate(c.burn0.40.df,dec.period = ifelse(FIRE_YEAR >= 1998 & FIRE_YEAR <= 2007,1998,dec.period))   
c.burn0.40.df<- mutate(c.burn0.40.df,dec.period = ifelse(FIRE_YEAR >= 2008 & FIRE_YEAR <= 2018,2008,dec.period)) 
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
c.r.0.40.df.temp  = c.burn0.40.df %>% group_by(Range,dec.period) %>% summarise(C_burn_dec_m2 = sum(area))

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
burn.by.temp = left_join(b.r.0.40.df.temp,c.r.0.40.df.temp) 
burn.by.temp$P_burn_dec_m2 = burn.by.temp$R_burn_dec_m2 - burn.by.temp$C_burn_dec_m2        
