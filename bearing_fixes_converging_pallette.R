


# Title: ##########################################################################

# Bearing down on a solution for osm matches


# Basic Information ###############################################################


# Author: Aaron Fehir

# Date: 
    # Start: 2019-12-17
    #  End:  NA

# Project: Project improvement; Routing, Single Operator ARAN

# Purpose: To further specific which matches are viable when comparing OSM data to a client .shp
         # using the bearing of the lines to rule out wrong ways and intersections. 

# Inputs: 1) the original client .shp
#         2) the fclass-narrowed shp, which is output by the script: navigation_fixes.R

# Outputs: 1) original client shapefile with bearings included.
        #  2) new crop of the potential osm matches 




##  Libraries  ####################################################################

if (!require("pacman")) install.packages("pacman"); library(pacman)

p_load(tidyverse, sp, geosphere, sf, tmap, leaflet, rgdal, pals)




## Imports/Inputs  #################################################################

# set working dir 
home <- "C:/Users/afehir/Desktop/shapefiles/SingleDriver-checks"

# original shapefile 
og_shp <- st_read(paste0(home, "/Mississauga_Routes_With401.shp"))

# OSM shapefile narrowed down to intersecting roads of the most likely fclasses 
fclass_shp <- st_read(paste0(home, "/results/try_4/try_4.shp"))



## Setup - Functions, and Variables ################################################

                          ## Creating Bearings for each shp ##

# creating original empty bearing df
bearings <- as.data.frame(matrix(ncol = 1, nrow = 0))
names(bearings) <- "bearings"

# duplicating one for each input 
og_bearings_output <- bearings
fclass_bearings_output <- bearings 

# one blank vector for each to collect the bearings
bearing_vctr_og <- get_rhumb_line_bearing(og_shp)
bearing_vctr_fclass <- get_rhumb_line_bearing(fclass_shp)

#TODO: Determine if the 12 lines above can be deleted


                            #  get_rhumb_line_bearing 
# This function is desgined to take an sp object, and return the Rhumb bearing
# between the startpoints and endpoints of each linestring or multilinestring
# geometry contained within
get_rhumb_line_bearing <- function(sp){
  sp_gmtry <- sp$geometry 
  sample <- sp_gmtry[[1]]
  empty_vctr <- c()
  if (isTRUE(is(sample, "MULTILINESTRING"))){
    for (i in 1:length(sp_gmtry)){
      line <- sp_gmtry[[i]][[1]]
      start <- line[1,]
      n <- as.numeric(dim(line)[1])
      end <- line[n,]
      rhumb_bear <- bearing(start, end)
      empty_vctr <- append(empty_vctr, rhumb_bear)
    }
    x <- empty_vctr
    return(x)
  } 
  if (isTRUE(is(sample, "LINESTRING"))){
    for (i in 1:length(sp_gmtry)){
      line <- sp_gmtry[[i]]
      start <- line[1,]
      n <- as.numeric(dim(line)[1])
      end <- line[n,]
      rhumb_bear <- bearing(start, end)
      empty_vctr <- append(empty_vctr, rhumb_bear)
    } 
    x <- empty_vctr
    return(x)
  }
 message("The input needs to be exclusively linstrings or multilinestrings")
}


             # Setting up a palette that makes sense for bearing. 

p <- palette(c(rgb(0,0,0, maxColorValue=255),
               rgb(255,0,0, maxColorValue=255),
               rgb(255,0,0, maxColorValue=255),
               rgb(255,255,255, maxColorValue=255),
               rgb(255,255,255, maxColorValue=255),
               rgb(0,0,255, maxColorValue=255),
               rgb(0,0,255, maxColorValue=255),
               rgb(0,0,0, maxColorValue=255)))

# North = White
# East = Blue 
# South = Black 
# West = Red

# Anything between the cardinal directions will be a combination of the two
# nearest cardinal directions; ei. South west will be  a dark red, while
# south-south-west would be an even darker red. 


## Algorithms/ETL###################################################################


                      # adding the rhumb bearing to both sp objects 
og_shp$bearing <- get_rhumb_line_bearing(og_shp) 
fclass_shp$bearing <- get_rhumb_line_bearing(fclass_shp)


                                    # Creating Check #

# check is an sp object where lines from fclass_shp that deviate from the
# bearing of the RECNNO candidate they've been assigned to by more than 45
# degrees have been filtered out. This should remove the opposite side of
# two-directional roads, as well as orthagonal intersections

og_bearings <- og_shp %>% 
  select(RRECNO, bearing) %>% 
  rename(client_bearing = bearing)

ready_for_bearing_comparison <- as.data.frame(fclass_shp) %>% 
  left_join(as.data.frame(og_bearings), by = "RRECNO") %>% 
  select(RRECNO, osm_id, code, fclass, bearing, client_bearing) 

getting_rid_of_wrong_ways <- ready_for_bearing_comparison %>% 
  filter(bearing <= client_bearing + 45 & bearing >= client_bearing - 45)

check <- fclass_shp %>% 
  semi_join(getting_rid_of_wrong_ways, by = "osm_id")


# figuring out which entire roads from the og_shp are missing in check 
missing_recnos <- anti_join(as.data.frame(fclass_shp), check, by = "RRECNO")


                        # Setup for Bearing visualization 
display1 <- check[["bearing"]]
display2 <- og_shp[["bearing"]]

pal1 <- colorBin(p,  bins = 32, domain = display1, na.color = "green")
pal2 <- colorBin(p, bins = 32, domain = display2, na.color = "green")




## Output############################################################################

st_write(og_shp, paste0(home, "/bearing/original.shp"), delete_dsn = TRUE)
st_write(fclass_shp, paste0(home, "/bearing/fclass.shp"), delete_dsn =  TRUE)


                             # Displaying og_shp bearing ###
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addPolylines(
    weight = 3,
    data = og_shp,
    color =  ~pal2(display2),
    opacity = 1) %>% 
addLegend(pal = pal2, values = display2, title = "Bearing Colouration")


                            # Displaying check_bearing ###
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery)%>%     
    addPolylines(
    data = check,
    weight = 3,
    color = ~pal1(display1),
    opacity = 1) %>% 
  addLegend(pal = pal1, values = display1, title = "Bearing Colouration")
