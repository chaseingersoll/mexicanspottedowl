## --- LOAD PACKAGES ---
install.packages("viridis")
install.packages("spatstat.core")

library(sf)
library(terra)
library(tidyverse)
library(tmap)
library(tigris)
library(viridis)
library(lubridate)
library(dplyr)
library(ggplot2)
library(spatstat.geom)
library(spatstat.core)


options(tigris_use_cache = TRUE)

# Force tmap to show maps in Plots pane (tmap v4 fix)
tmap_options(output.format = "plot")


## --- LOAD DATA ---
hab <- st_read("D:/porj2/geovizproject2/criticalhabitatdata/FCH_Strix_occidentalis_lucida_20040831.shp")
mines <- st_read("D:/porj2/geovizproject2/Permits.gdb", layer = "AllPermits")
co_counties <- counties(state = "CO", year = 2020) |> st_as_sf()
roads <- st_read("D:/porj2/geovizproject2/rd data/rds_data/tl_2019_08_prisecroads.shp")
roads <- st_transform(roads, crsCO)
mines_csv <-  read_csv("D:/porj2/geovizproject2/Historic_Mine/MiningHabiatData/mine_data - Sheet1.csv")



names(mines_csv)
head(mines_csv)

## --- REPROJECT ---
crsCO <- "epsg:26953"

hab <- st_transform(hab, crsCO)
co_counties <- st_transform(co_counties, crsCO)
mines <- st_transform(mines, crsCO)
mines <- suppressWarnings(st_as_sf(mines_csv, wkt = "Shape", crs = crsCO))



## --- CLIP HABITAT TO COLORADO ---
hab_co <- st_intersection(hab, co_counties)



# Get mining operations intersecting critical habitat
mines_hab <- st_intersection(mines, hab_co)

### ---- HABITAT BY COUNTY -------
hab_by_county <- st_intersection(co_counties, hab_co) %>%
  group_by(NAME) %>%
  summarize(hab_area = sum(st_area(geometry)))


###combine habitat data to mining data
mines_hab <- st_intersection(mines, hab_co)
tmap_mode("view")

tm_shape(hab_co) + tm_polygons(col = "forestgreen", alpha = 0.4) +
  tm_shape(mines_hab) +
  tm_symbols(col = "red", size = 0.6, popup.vars = TRUE)

### add more mining da6ta from CSV
mines_hab <- st_intersection(mines_csv, hab_co)



## ---MAKE SURE DATA LOOKS CHILL---- 
View(co_counties)
View(hab_co)
View(hab_by_county)
View(mines)
View(mine_csv)
View (mines_hab)

### -----create road overlap with owl hab-----
roads <- st_transform(roads, st_crs(hab_co))
roads_hab <- st_intersection(roads, hab_co)

roads_hab
## did it work
tm_shape(roads_hab) +
  tm_lines(col = "black")

##YAAAAYYYYYYY YAAAAYYYAAYYY IT WOKRED LETS GOOOO!!!


##okay moving on 




#-----create 2.5 mile (4023.26 meter) buffer for roads and mines -------

buffer_distance <- 4023.36  


mines_buffer <- st_buffer(mines_hab, dist = buffer_distance)


roads_buffer <- st_buffer(roads_hab, dist = buffer_distance)


risk_zone <- st_union(mines_buffer, roads_buffer)



##=========================================================================
##              SOLVING SOME PROBLEMS REALLY QUICK.... 
##========================================================================

## ==========================================================
## 1. CHOROPLETH MAP — HABITAT AREA BY COUNTY
## ==========================================================

hab_co$Habitat <- "Mexican Spotted Owl Critical Habitat"

##### ---------------OPTION ONE-----------------------------------------

tm_shape(co_counties) +
  tm_polygons(col = "lightyellow", border.col = "black") +  # counties with color
  tm_shape(hab_co) +
  tm_polygons(col = "Habitat", alpha = 0.7, border = NA,
              palette = "darkgreen", title = "Owl Habitat") +   # habitat color
  tm_shape(co_counties) +
  tm_text("NAME", size = 0.6, col = "black", shadow = TRUE) +  # slightly larger labels
  tm_layout(main.title = "Mexican Spotted Owl Critical Habitat in Colorado",
            main.title.position = "center",       # centered above map
            legend.outside = TRUE,                
            legend.title.size = 1.2,
            legend.text.size = 0.9,
            frame = TRUE,
            bg.color = "lightblue")

#---------------------OPTION TWO----------------
tm_shape(co_counties) +
  tm_polygons(
    col = "has_habitat",      
    palette = c("lightgreen", "darkgreen"), 
    labels = c("No Owl habitat", "Contains Owl Habitat"),   
    title = "Critical Habitat "    
  ) +
  tm_layout(
    main.title = "Mexican Spotted Owl Critical Habitat by County in Colorado",
    main.title.position = tm_pos_out("center", "top"), 
    legend.outside = TRUE,                             
    legend.outside.position = "right",
    frame = TRUE,                                     
    inner.margins = c(0.1, 0.1, 0.1, 0.1)               
  ) +
  tm_legend(
    legend.format = list(
      FUN = function(x) paste0(round(x, 1), " sq mi")  
    )
  )
## ==========================================================
## 2. At risk habitats
## ==========================================================
tm_shape(hab_co) +
  tm_polygons(col = "forestgreen", alpha = 0.5, border.col = NA) +
  tm_shape(risk_zone) +
  tm_polygons(col = "orange", alpha = 0.3, border.col = "orange",
              title = "10-Mile Risk Zone") +
  tm_shape(roads_hab) +
  tm_lines(col = "black", lwd = 1, title = "Roads in Habitat") +
  tm_shape(mines_hab) +
  tm_symbols(col = "red", size = 0.5,
             title.col = "Mines",
             legend.col.show = TRUE) +
  tm_layout(
    main.title = "Mexican Spotted Owl Habitat Risk Zones (Colorado)",
    main.title.size = 1.2,
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "lightblue"
  )


## help with Chat GPT: "add a frame (with title outside of frame on top left), add a legend to the center right of the frame (roads, buffer, mine points, and habitat) and add the colorado counties ** map is not interactive**"

tmap_mode("plot")   # ensures static map

tm_shape(co_counties) +
  tm_polygons(col = "gray95", border.col = "gray70") +  # Colorado counties (no labels)
  
  # Habitat polygons
  tm_shape(hab_co) +
  tm_polygons(col = "forestgreen", alpha = 0.5, border.col = NA,
              title = "Owl Habitat") +
  
  # 2.5 mile risk buffer
  tm_shape(risk_zone) +
  tm_polygons(col = "orange", alpha = 0.3, border.col = "orange",
              title = "10-Mile Buffer") +
  
  # Roads inside habitat
  tm_shape(roads_hab) +
  tm_lines(col = "black", lwd = 1,
           title = "Roads") +
  
  # Mines inside habitat
  tm_shape(mines_hab) +
  tm_symbols(col = "red", size = 0.5,
             title.col = "Mines") +
  
  tm_layout(
    main.title = "Mexican Spotted Owl Habitat Risk Zones ",
    main.title.size = 1.2,
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "lightblue"
  )
## ==========================================================
## 3. Mining Impact Map
## ========================================================
unique(mines_hab$PermitType)

tm_shape(hab_co) +
  tm_polygons(col = "forestgreen", alpha = 0.3, border.col = "darkgreen") +
  tm_shape(mines_hab) +
  tm_symbols(col = "mine_type", size = 0.7, border.col = "red") +
  tm_layout(
    main.title = "Mining Operations Overlaid on Owl Habitat",
    legend.outside = TRUE,
    frame = TRUE
  )

tm_shape(hab_co) +
  tm_polygons(col = "forestgreen", alpha = 0.3, border.col = "darkgreen") +
  tm_shape(mines_hab) +
  tm_symbols(col = "mine_type", size = "Acres", scale = 1, border.col = "red") +
  tm_layout(
    main.title = "Mining Intensity Overlaid on Owl Habitat",
    legend.outside = TRUE
  )



## ==========================================================
## 3. Mining Impact Map
## ========================================================


#prep mines for map
mine_counts <- mines_hab %>%
  st_drop_geometry() %>% 
  count(mine_type)
ggplot(mine_counts, aes(x = mine_type, y = n, fill = mine_type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Mining Operations within Critical Habitat",
    x = "Mine Type",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


## ==========================================================
## 4. Spatial Distribution of Mining Operations in Colorado
## ========================================================
# Count mines per county
mine_counts <- mines %>%
  st_join(co_counties) %>%
  group_by(NAME) %>%
  summarize(num_mines = n())

mine_counts <- mines %>%
  st_join(co_counties) %>%      # assign each mine to a county
  group_by(NAME) %>%            # group by county name
  summarize(num_mines = n())

# Spatially join the mine counts to counties
co_counties_mines <- st_join(co_counties, mine_counts)

# finihs it up
co_counties_mines$num_mines[is.na(co_counties_mines$num_mines)] <- 0
cities <- data.frame(
  name = c("Denver", "Colorado Springs", "Grand Junction", "Limon"),
  lon = c(-104.9903, -104.8214, -108.5506, -103.6920),
  lat = c(39.7392, 38.8339, 39.0639, 39.2633)
)
cities_sf <- st_as_sf(cities, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(co_counties_mines))
tm_shape(co_counties_mines) +
  tm_polygons(
    col = "num_mines",
    palette = "Greens",    
    alpha = 0.7,
    title = "Number of Mines",
    border.col = "grey50"  
  ) +
  tm_shape(cities_sf) + ### added cities
  tm_symbols(size = 0.5, col = "black") +    
  tm_text("name", size = 1, ymod = 0.5) +    
  tm_layout(
    main.title = "Spatial Distribution of Mining Operations in Colorado",
    legend.outside = TRUE,
    frame = TRUE
  )




#why is there so many goofs omg
st_crs(co_counties_mines)
st_crs(st_crs(st_crs(habitat_mine_fix)

              
              
 ## ==========================================================
##  WE NEEDED SCALE BARS AND NORTH ARROWS!!!!!!!!!!
 ## ========================================================


#map 1:
tm_shape(hab_co) +
  tm_polygons(col = "forestgreen", alpha = 0.4) +
  tm_shape(mines_hab) +
  tm_symbols(col = "red", size = 0.6, popup.vars = TRUE) +
  
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(type = "4star", position = c("right", "top")) +
  tm_layout(frame = TRUE)
#map 2:
tmap_mode("plot")

tm_shape(co_counties) +
  tm_polygons(col = "lightyellow", border.col = "black") +  
  tm_shape(hab_co) +
  tm_polygons(col = "Habitat", alpha = 0.7, border = NA,
              palette = "darkgreen", title = "Owl Habitat") +
  tm_text("NAME", size = 0.6, col = "black", shadow = TRUE) +
  
  tm_layout(main.title = "Mexican Spotted Owl Critical Habitat in Colorado",
            main.title.position = "center",
            legend.outside = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 0.9,
            frame = TRUE,
            bg.color = "lightblue") +
  
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top"))
##map 3:
tm_shape(hab_co) +
  tm_polygons(col = "forestgreen", alpha = 0.5, border.col = NA) +
  tm_shape(risk_zone) +
  tm_polygons(col = "orange", alpha = 0.3, border.col = "orange",
              title = "10-Mile Risk Zone") +
  tm_shape(roads_hab) +
  tm_lines(col = "black", lwd = 1, title = "Roads in Habitat") +
  tm_shape(mines_hab) +
  tm_symbols(col = "red", size = 0.5, title.col = "Mines",
             legend.col.show = TRUE) +
  
  tm_layout(main.title = "Mexican Spotted Owl Habitat Risk Zones (Colorado)",
            main.title.size = 1.2,
            legend.outside = TRUE,
            frame = TRUE,
            bg.color = "lightblue") +
  
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top"))
#map 4:
tm_shape(co_counties) +
  tm_polygons(col = "gray95", border.col = "gray70") +
  tm_text("NAME", size = 0.6, col = "black") +     # ← Added county labels
  
  tm_shape(hab_co) +
  tm_polygons(col = "forestgreen", alpha = 0.5, border.col = NA,
              title = "Owl Habitat") +
  
  tm_shape(risk_zone) +
  tm_polygons(col = "orange", alpha = 0.3, border.col = "orange",
              title = "10-Mile Buffer") +
  
  tm_shape(roads_hab) +
  tm_lines(col = "black", lwd = 1, title = "Roads") +
  
  tm_shape(mines_hab) +
  tm_symbols(col = "red", size = 0.5, title.col = "Mines") +
  
  tm_layout(main.title = "Mexican Spotted Owl Habitat Risk Zones",
            main.title.size = 1.2,
            legend.outside = TRUE,
            frame = TRUE,
            bg.color = "lightblue") +
  
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top"))
###map 5:
tm_shape(co_counties_mines) +
  tm_polygons(
    col = "num_mines",
    palette = "Greens",
    alpha = 0.7,
    title = "Number of Mines",
    border.col = "grey50"
  ) +
  tm_shape(cities_sf) +
  tm_symbols(size = 0.5, col = "black") +
  tm_text("name", size = 1, ymod = 0.5) +
  
  tm_layout(
    main.title = "Spatial Distribution of Mining Operations in Colorado",
    legend.outside = TRUE,
    frame = TRUE
  ) +
  
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top"))
##map 5 change to counties not cities
tm_shape(co_counties_mines) +
  tm_polygons(
    col = "num_mines",
    palette = "Greens",
    alpha = 0.7,
    title = "Number of Mines",
    border.col = "grey50"
  ) +
  tm_shape(co_counties) +
  tm_text("NAME", size = 0.6, col = "black", shadow = TRUE) +  # slightly larger labels
  tm_layout(main.title = "Mexican Spotted Owl Critical Habitat in Colorado",
            main.title.position = "center",       # centered above map
            legend.outside = TRUE,                
            legend.title.size = 1.2,
            legend.text.size = 0.9,
            frame = TRUE,
            bg.color = "lightblue")+
  
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(type = "8star", position = c("right", "top"))
