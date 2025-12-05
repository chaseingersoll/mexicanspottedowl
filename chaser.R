#Load only necessary libraries
install.packages("ggrepel")
library(ggrepel)
library(sf)
library(dplyr)
library(ggplot2)
library(tmap)
##
##data
##critical habitats
habitats <- st_read("data/criticalhabitatdata/FCH_Strix_occidentalis_lucida_20040831.shp")
##data
##roads
roads <- st_read("data/tl_2019_08_prisecroads.shp")
##filter to CO only
habitats_co <- habitats %>% filter(State == "COLORADO")
# Quick plot to verify
plot(st_geometry(habitats_co), col = "blue", main = "Critical Habitat in Colorado")
##check CRS
# Correct CRS check
# Compare CRS via WKT
if (st_crs(roads)$wkt != st_crs(habitats_co)$wkt) {
  roads <- st_transform(roads, st_crs(habitats_co))
}
#intersect habitat and road
roads_in_habitat <- st_intersection(roads, habitats_co)
#overlay map
tmap_mode("plot")  # static map

tm_shape(habitats_co) +
  tm_polygons(col = "lightgreen", alpha = 0.5, border.col = "darkgreen", title = "Owl Habitat") +
  tm_shape(roads_in_habitat) +
  tm_lines(col = "red", lwd = 1, title = "Roads in Habitat") +
  tm_layout(title = "Owl Habitat with Roads Overlap") ##not good vis
# Load Colorado boundary
counties <- st_read("data/cb_2023_us_county_500k/cb_2023_us_county_500k.shp")
# Filter to Colorado only (STATEFP == "08")
colorado <- counties %>% 
  filter(STATEFP == "08")
getwd()

## Load libraries
library(sf)
library(dplyr)
library(tmap)
## Load Data
habitats <- st_read("data/criticalhabitatdata/FCH_Strix_occidentalis_lucida_20040831.shp")
roads <- st_read("data/tl_2019_08_prisecroads.shp")
counties <- st_read("data/cb_2023_us_county_500k/cb_2023_us_county_500k.shp")
## Filter habitats to Colorado only
habitats_co <- habitats %>% filter(State == "COLORADO")
## Filter counties to Colorado
colorado <- counties %>% filter(STATEFP == "08")
if (st_crs(roads)$wkt != st_crs(habitats_co)$wkt) {
  roads <- st_transform(roads, st_crs(habitats_co))
}

if (st_crs(colorado)$wkt != st_crs(habitats_co)$wkt) {
  colorado <- st_transform(colorado, st_crs(habitats_co))
}

## Get FULL road segments that intersect habitat
## (No clipping, keeps entire road geometry)
roads_touching_habitat <- roads[lengths(st_intersects(roads, habitats_co)) > 0, ]
## Final Map
tmap_mode("plot")

tm_shape(colorado) +
  tm_polygons(col = "white", border.col = "black", alpha = 0.3) +
  
  tm_shape(counties) +
  tm_polygons(col = NA, border.col = "gray60", lwd = 0.5) +
  
  tm_shape(habitats_co) +
  tm_polygons(col = "lightgreen", border.col = "darkgreen", alpha = 0.5) +
  
  tm_shape(roads_touching_habitat) +
  tm_lines(col = "red", lwd = 1) +
  
  tm_layout(
    title = "Colorado Mexican Spotted Owl Habitat and Road Crossings",
    frame = FALSE
  ) ## not good vis BUT CLOSE, need to chang orientation, add counties, and change title
##check road
nrow(roads_touching_habitat)
# STEP 1 — find IDs of intersecting road features
touching_ids <- unique(roads$LINEARID[ lengths(st_intersects(roads, habitats_co)) > 0 ])
# STEP 2 — extract the FULL road features using those IDs
roads_full_touching <- roads %>% filter(LINEARID %in% touching_ids)
##check roads
nrow(roads_full_touching)
##confirm geometry
st_is_empty(roads_full_touching)
##plot roads alone
plot(st_geometry(roads_full_touching), col = "red", lwd = 2)
##put habitat on top
plot(st_geometry(habitats_co), border = "green", add = TRUE)
####new map
tmap_mode("plot")

tm_shape(colorado, is.master = TRUE) +
  tm_polygons(col = "white", border.col = "black", alpha = 0.3) +
  
  tm_shape(counties) +
  tm_polygons(col = NA, border.col = "gray60", lwd = 0.5) +
  
  tm_shape(roads_full_touching) +
  tm_lines(col = "red", lwd = 1.3) +
  
  tm_shape(habitats_co) +
  tm_polygons(col = "lightgreen", alpha = 0.5, border.col = "darkgreen") +
  
  tm_layout(
    title = "Owl Habitats and Roads",
    frame = FALSE
  )  ## closer
##improve the map, this part doesnt work?
library(tmap)
tmap_mode("plot")  # Static map

# Optionally get bounding box of habitats and expand a bit for slight zoom-in
bbox <- st_bbox(habitats_co)
expand_factor <- 0.02  # 2% expansion to zoom in slightly
xrange <- bbox$xmax - bbox$xmin
yrange <- bbox$ymax - bbox$ymin
bbox_expanded <- c(
  xmin = bbox$xmin - expand_factor * xrange,
  xmax = bbox$xmax + expand_factor * xrange,
  ymin = bbox$ymin - expand_factor * yrange,
  ymax = bbox$ymax + expand_factor * yrange
)

tm_shape(colorado, bbox = bbox_expanded) +
  tm_polygons(col = "white", border.col = "black", alpha = 0.3) +
  
  tm_shape(counties, bbox = bbox_expanded) +
  tm_polygons(col = NA, border.col = "gray60", lwd = 0.5) +
  
  tm_shape(roads_full_touching, bbox = bbox_expanded) +
  tm_lines(col = "red", lwd = 1.3) +
  tm_text("FULLNAME", col = "black", size = 0.6, remove.overlap = TRUE) +  # Road names
  
  tm_shape(habitats_co, bbox = bbox_expanded) +
  tm_polygons(col = "lightgreen", alpha = 0.5, border.col = "darkgreen") +
  
  tm_layout(
    title = "Roads that Cross Owl Habitat",
    frame = FALSE
  )


##check CRS
st_crs(habitats_co)
st_crs(roads_full_touching)
st_crs(colorado)
st_crs(counties)
##clean working version, lets map again
library(tmap)

tmap_mode("plot")

# Use bbox from habitats and convert to numeric vector for tmap
bbox <- st_bbox(habitats_co)
expand_factor <- 0.02
xrange <- bbox$xmax - bbox$xmin
yrange <- bbox$ymax - bbox$ymin

bbox_expanded <- c(
  xmin = bbox["xmin"] - expand_factor * xrange,
  xmax = bbox["xmax"] + expand_factor * xrange,
  ymin = bbox["ymin"] - expand_factor * yrange,
  ymax = bbox["ymax"] + expand_factor * yrange
)

# Check road name field
names(roads_full_touching)
# replace "FULLNAME" below with actual road name field if different

tm_shape(colorado, bbox = bbox_expanded) +
  tm_polygons(col = "white", border.col = "black", alpha = 0.3) +
  
  tm_shape(counties, bbox = bbox_expanded) +
  tm_polygons(col = NA, border.col = "gray60", lwd = 0.5) +
  
  tm_shape(roads_full_touching, bbox = bbox_expanded) +
  tm_lines(col = "red", lwd = 1.3) +
  tm_text("FULLNAME", col = "black", size = 0.6, remove.overlap = TRUE) +  # road labels
  
  tm_shape(habitats_co, bbox = bbox_expanded) +
  tm_polygons(col = "lightgreen", alpha = 0.5, border.col = "darkgreen") +
  
  tm_layout(
    title = "Roads that Cross Owl Habitat",
    frame = FALSE
  )
##again, don't like, don't keep, but this is getting better
library(tmap)
tmap_mode("plot")  # static map

# Check the correct road name field
names(roads_full_touching)
# Replace "FULLNAME" below with the actual road name field if different

tm_shape(colorado) +
  tm_polygons(col = "white", border.col = "black", alpha = 0.3) +
  
  tm_shape(counties) +
  tm_polygons(col = NA, border.col = "gray60", lwd = 0.5) +
  
  tm_shape(roads_full_touching) +
  tm_lines(col = "red", lwd = 1.3) +
  tm_text("FULLNAME", col = "black", size = 0.6, remove.overlap = TRUE) +  # road names
  
  tm_shape(habitats_co) +
  tm_polygons(col = "lightgreen", alpha = 0.5, border.col = "darkgreen") +
  
  tm_layout(
    title = "Roads that Cross Owl Habitat",
    frame = FALSE
  )
##improve overlap THIS IS THE MAP I LIKE, I WANT TO KEEP
library(ggplot2)
library(sf)
library(dplyr)

#Check road name field
names(roads_full_touching)  # make sure to use the correct column for labels

#Get Colorado bounding box
colorado_bbox <- st_bbox(colorado)

ggplot() +
#Colorado state boundary
  geom_sf(data = colorado, fill = "white", color = "black", alpha = 0.3) +
  
#Counties
  geom_sf(data = counties, fill = NA, color = "gray60", size = 0.5) +
  
#Roads
  geom_sf(data = roads_full_touching, color = "red", size = 0.8) +
  
#Road labels
  geom_sf_text(data = roads_full_touching, 
               aes(label = FULLNAME),  # replace if your road name column is different
               color = "black", 
               size = 2.5, 
               check_overlap = TRUE) +
  
#Owl habitat
  geom_sf(data = habitats_co, fill = "lightgreen", color = "darkgreen", alpha = 0.5) +
  
#Zoom to Colorado extent (slightly expanded)
  coord_sf(xlim = c(colorado_bbox$xmin - 5000, colorado_bbox$xmax + 5000),
           ylim = c(colorado_bbox$ymin - 5000, colorado_bbox$ymax + 5000)) +
  
#Map title
  ggtitle("Roads that Cross Owl Habitat") +
  
#Minimal theme
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
##yay, let's make it gray and take the x and y away
ggplot() +
#Colorado state boundary
  geom_sf(data = colorado, fill = "gray80", color = "black", alpha = 0.3) +
  
#Counties
  geom_sf(data = counties, fill = NA, color = "gray60", size = 0.5) +
  
#Roads
  geom_sf(data = roads_full_touching, color = "red", size = 0.8) +
  
#Road labels
  geom_sf_text(data = roads_full_touching, 
               aes(label = FULLNAME),  # replace if your road name column is different
               color = "black", 
               size = 2.5, 
               check_overlap = TRUE) +
  
#Owl habitat
  geom_sf(data = habitats_co, fill = "lightgreen", color = "darkgreen", alpha = 0.5) +
  
#Zoom to Colorado extent
  coord_sf(xlim = c(colorado_bbox$xmin - 5000, colorado_bbox$xmax + 5000),
           ylim = c(colorado_bbox$ymin - 5000, colorado_bbox$ymax + 5000)) +
  
#Map title
  ggtitle("Roads that Cross Owl Habitat") +
  
#Theme adjustments
  theme_minimal() +
  theme(
    panel.grid = element_blank(),           # remove grid
    axis.text = element_blank(),            # remove numbers
    axis.ticks = element_blank(),           # remove ticks
    axis.title = element_blank(),           # remove x/y titles
    panel.background = element_rect(fill = "gray90")  # set gray background
  )
##save 
#Save as PDF
ggsave("Roads_that_Cross_Owl_Habitat.pdf", plot = owl_map, width = 10, height = 8, units = "in")
##save
library(ggplot2)

owl_map <- ggplot() +
  geom_sf(data = colorado, fill = "gray80", color = "black", alpha = 0.3) +
  geom_sf(data = counties, fill = NA, color = "gray60", size = 0.5) +
  geom_sf(data = roads_full_touching, color = "red", size = 0.8) +
  geom_sf_text(data = roads_full_touching, 
               aes(label = FULLNAME), 
               color = "black", size = 2.5, check_overlap = TRUE) +
  geom_sf(data = habitats_co, fill = "lightgreen", color = "darkgreen", alpha = 0.5) +
  coord_sf(xlim = c(colorado_bbox$xmin - 5000, colorado_bbox$xmax + 5000),
           ylim = c(colorado_bbox$ymin - 5000, colorado_bbox$ymax + 5000)) +
  ggtitle("Roads that Cross Owl Habitat") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "gray90")
  )
##save
ggsave("Roads_that_Cross_Owl_Habitat.pdf", plot = owl_map, width = 10, height = 8, units = "in")


# Load Data 
habitats <- st_read("data/criticalhabitatdata/FCH_Strix_occidentalis_lucida_20040831.shp")
roads <- st_read("data/tl_2019_08_prisecroads.shp")
counties <- st_read("data/cb_2023_us_county_500k/cb_2023_us_county_500k.shp")
#Filter to Colorado
habitats_co <- habitats %>% filter(State == "COLORADO")
colorado <- counties %>% filter(STATEFP == "08")
#Ensure matching CRS
roads <- st_transform(roads, st_crs(habitats_co))
colorado <- st_transform(colorado, st_crs(habitats_co))
#Get roads that intersect habitat
touching_ids <- unique(roads$LINEARID[lengths(st_intersects(roads, habitats_co)) > 0])
roads_full_touching <- roads %>% filter(LINEARID %in% touching_ids)
#Get Colorado bounding box and expand slightly
colorado_bbox <- st_bbox(colorado)
expand_factor <- 0.02
xrange <- colorado_bbox$xmax - colorado_bbox$xmin
yrange <- colorado_bbox$ymax - colorado_bbox$ymin

bbox_expanded <- c(
  xmin = colorado_bbox$xmin - expand_factor * xrange,
  xmax = colorado_bbox$xmax + expand_factor * xrange,
  ymin = colorado_bbox$ymin - expand_factor * yrange,
  ymax = colorado_bbox$ymax + expand_factor * yrange
)
#Create final map
owl_map <- ggplot() +
  geom_sf(data = colorado, fill = "gray80", color = "black", alpha = 0.3) +
  geom_sf(data = counties, fill = NA, color = "gray60", size = 0.5) +
  geom_sf(data = roads_full_touching, color = "red", size = 0.8) +
  geom_sf_text(data = roads_full_touching, 
               aes(label = FULLNAME), 
               color = "black", size = 2.5, check_overlap = TRUE) +
  geom_sf(data = habitats_co, fill = "lightgreen", color = "darkgreen", alpha = 0.5) +
  coord_sf(xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
           ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"])) +
  ggtitle("Roads that Cross Owl Habitat") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "gray90")
  )
#Save as PDF
ggsave("Roads_that_Cross_Owl_Habitat.pdf", plot = owl_map, width = 10, height = 8, units = "in")


## UPDATED ROADS THAT CROSS MAP
library(ggplot2)
library(sf)
library(grid)

# --------- FIX ORIENTATION (REPROJECT) ----------
colorado <- st_transform(colorado, 4326)
counties <- st_transform(counties, 4326)
habitats_co <- st_transform(habitats_co, 4326)
roads_full_touching <- st_transform(roads_full_touching, 4326)

colorado_bbox <- st_bbox(colorado)

# --------- MAIN MAP ----------
owl_map <- ggplot() +
  
  # Colorado outline
  geom_sf(data = colorado, fill = "gray85", color = "black") +
  
  # Counties (added)
  geom_sf(data = counties, fill = NA, color = "gray40", size = 0.4) +
  
  # County labels
  geom_sf_text(
    data = counties,
    aes(label = NAME),   # change NAME to your county name field
    size = 2.5, color = "black", check_overlap = TRUE
  ) +
  
  # Habitat
  geom_sf(data = habitats_co, fill = "lightgreen", alpha = 0.5, color = "darkgreen") +
  
  # Roads + labels
  geom_sf(data = roads_full_touching, color = "red", size = 0.8) +
  geom_sf_text(
    data = roads_full_touching,
    aes(label = FULLNAME),
    size = 2.5, color = "black", check_overlap = TRUE
  ) +
  
  # Proper non-skewed orientation
  coord_sf(
    xlim = c(colorado_bbox$xmin, colorado_bbox$xmax),
    ylim = c(colorado_bbox$ymin, colorado_bbox$ymax),
    expand = FALSE, datum = NA
  ) +
  
  ggtitle("Roads that Cross Owl Habitat") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  
  # -----------------------------------------------------
# CUSTOM NORTH ARROW (styled like ggspatial)
# -----------------------------------------------------
annotation_custom(
  grob = grobTree(
    polygonGrob(
      x = c(0.5, 0.45, 0.55),
      y = c(1, 0.8, 0.8),
      gp = gpar(fill = "black")
    ),
    textGrob("N", y = 1.05, gp = gpar(fontsize = 12, fontface = "bold"))
  ),
  xmin = colorado_bbox$xmax - 0.8, xmax = colorado_bbox$xmax - 0.4,
  ymin = colorado_bbox$ymax - 0.8, ymax = colorado_bbox$ymax - 0.4
) +
  
  # -----------------------------------------------------
# CUSTOM SCALE BAR (clean style similar to annotation_scale)
# -----------------------------------------------------
annotation_custom(
  grob = grobTree(
    rectGrob(
      width = unit(2, "cm"), height = unit(0.25, "cm"),
      gp = gpar(fill = "black")
    ),
    rectGrob(
      width = unit(2, "cm"), height = unit(0.25, "cm"),
      x = unit(2, "cm"),
      gp = gpar(fill = "white")
    ),
    textGrob("20 km", y = unit(-0.3, "cm"), gp = gpar(fontsize = 10))
  ),
  xmin = colorado_bbox$xmax - 1.5,
  xmax = colorado_bbox$xmax - 0.5,
  ymin = colorado_bbox$ymin + 0.2,
  ymax = colorado_bbox$ymin + 0.6
)

# Save
ggsave("Roads_that_Cross_Owl_Habitat.pdf",
       plot = owl_map, width = 10, height = 8, units = "in")


## fix it
library(tmap)
tmap_mode("plot")

tm <- tm_shape(colorado) +
  tm_polygons(col = "gray85", border.col = "black") +
  
  tm_shape(counties) +
  tm_borders(col = "gray40") +
  tm_text("NAME", size = 0.6) +
  
  tm_shape(habitats_co) +
  tm_polygons(col = "lightgreen", border.col = "darkgreen", alpha = 0.5) +
  
  tm_shape(roads_full_touching) +
  tm_lines(col = "red", lwd = 2) +
  tm_text("FULLNAME", size = 0.6, bg.color = "white") +
  
  # EXACT north arrow from your screenshot
  tm_compass(type = "8star", position = c("right", "top"), size = 3) +
  
  # EXACT black/white alternating scale bar
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.6) +
  
  tm_layout(
    frame = TRUE,
    title = "Roads that Cross Owl Habitat",
    title.size = 1.2
  )

tmap_save(tm, "Roads_that_Cross_Owl_Habitat.pdf", width = 10, height = 8, units = "in")

## TRY AGAIN, PERFECT just need to fix the styles


## UPDATED ROADS THAT CROSS MAP
install.packages("ggrepel")
library(ggplot2)
library(sf)
library(grid)
library(ggrepel)   # <-- added for non-overlapping labels

# --------- FIX ORIENTATION (REPROJECT) ----------
colorado <- st_transform(colorado, 4326)
counties <- st_transform(counties, 4326)
habitats_co <- st_transform(habitats_co, 4326)
roads_full_touching <- st_transform(roads_full_touching, 4326)

colorado_bbox <- st_bbox(colorado)

# --------- MAIN MAP ----------
owl_map <- ggplot() +
  
  # Colorado outline
  geom_sf(data = colorado, fill = "gray85", color = "black") +
  
  # Counties
  geom_sf(data = counties, fill = NA, color = "gray40", size = 0.4) +
  
  # County labels
  geom_sf_text(
    data = counties,
    aes(label = NAME),
    size = 2.5,
    color = "black",
    check_overlap = TRUE
  ) +
  
  # Habitat
  geom_sf(data = habitats_co, fill = "lightgreen", alpha = 0.5, color = "darkgreen") +
  
  # Roads
  geom_sf(data = roads_full_touching, color = "red", size = 0.8) +
  
  # ---------- FIXED ROAD LABELS (NO OVERLAP) ----------
geom_text_repel(
  data = st_as_sf(roads_full_touching),
  aes(label = FULLNAME, geometry = geometry),
  stat = "sf_coordinates",
  size = 2.5,
  color = "black",
  box.padding = 0.5,
  point.padding = 0.5,
  min.segment.length = 0,
  segment.color = "gray30"
) +
  # ---------- END FIX ----------

# Proper non-skewed orientation
coord_sf(
  xlim = c(colorado_bbox$xmin, colorado_bbox$xmax),
  ylim = c(colorado_bbox$ymin, colorado_bbox$ymax),
  expand = FALSE, datum = NA
) +
  
  ggtitle("Roads that Cross Owl Habitat") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  
  # -----------------------------------------------------
# CUSTOM NORTH ARROW
# -----------------------------------------------------
annotation_custom(
  grob = grobTree(
    polygonGrob(
      x = c(0.5, 0.45, 0.55),
      y = c(1, 0.8, 0.8),
      gp = gpar(fill = "black")
    ),
    textGrob("N", y = 1.05, gp = gpar(fontsize = 12, fontface = "bold"))
  ),
  xmin = colorado_bbox$xmax - 0.8, xmax = colorado_bbox$xmax - 0.4,
  ymin = colorado_bbox$ymax - 0.8, ymax = colorado_bbox$ymax - 0.4
) +
  
  # -----------------------------------------------------
# CUSTOM SCALE BAR
# -----------------------------------------------------
annotation_custom(
  grob = grobTree(
    rectGrob(
      width = unit(2, "cm"), height = unit(0.25, "cm"),
      gp = gpar(fill = "black")
    ),
    rectGrob(
      width = unit(2, "cm"), height = unit(0.25, "cm"),
      x = unit(2, "cm"),
      gp = gpar(fill = "white")
    ),
    textGrob("20 km", y = unit(-0.3, "cm"), gp = gpar(fontsize = 10))
  ),
  xmin = colorado_bbox$xmax - 1.5,
  xmax = colorado_bbox$xmax - 0.5,
  ymin = colorado_bbox$ymin + 0.2,
  ymax = colorado_bbox$ymin + 0.6
)

# Save final map
ggsave(
  "Roads_that_Cross_Owl_Habitat.pdf",
  plot = owl_map, width = 10, height = 8, units = "in"
)

##STYLE


## UPDATED ROADS THAT CROSS MAP
install.packages("ggrepel")
library(ggplot2)
library(sf)
library(grid)
library(ggrepel)

# --------- FIX ORIENTATION (REPROJECT) ----------
colorado <- st_transform(colorado, 4326)
counties <- st_transform(counties, 4326)
habitats_co <- st_transform(habitats_co, 4326)
roads_full_touching <- st_transform(roads_full_touching, 4326)

colorado_bbox <- st_bbox(colorado)

# --------- MAIN MAP ----------
owl_map <- ggplot() +
  
  # Colorado outline
  geom_sf(data = colorado, fill = "gray85", color = "black") +
  
  # Counties
  geom_sf(data = counties, fill = NA, color = "gray40", size = 0.4) +
  
  # County labels
  geom_sf_text(
    data = counties,
    aes(label = NAME),
    size = 2.5,
    color = "black",
    check_overlap = TRUE
  ) +
  
  # Habitat
  geom_sf(data = habitats_co, fill = "lightgreen", alpha = 0.5, color = "darkgreen") +
  
  # Roads
  geom_sf(data = roads_full_touching, color = "red", size = 0.8) +
  
  # ---------- FIXED ROAD LABELS ----------
geom_text_repel(
  data = st_as_sf(roads_full_touching),
  aes(label = FULLNAME, geometry = geometry),
  stat = "sf_coordinates",
  size = 2.5,
  color = "black",
  box.padding = 0.5,
  point.padding = 0.5,
  min.segment.length = 0,
  segment.color = "gray30"
) +
  
  # Proper non-skewed orientation
  coord_sf(
    xlim = c(colorado_bbox$xmin, colorado_bbox$xmax),
    ylim = c(colorado_bbox$ymin, colorado_bbox$ymax),
    expand = FALSE, datum = NA
  ) +
  
  ggtitle("Roads that Cross Owl Habitat") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  
  # -----------------------------------------------------
# SIMPLE NORTH ARROW (matches your screenshot)
# -----------------------------------------------------
annotation_custom(
  grob = grobTree(
    polygonGrob(
      x = c(0.5, 0.45, 0.55),
      y = c(1, 0.85, 0.85),
      gp = gpar(fill = "black")
    ),
    textGrob("N", y = 1.10, gp = gpar(fontsize = 14, fontface = "bold"))
  ),
  xmin = colorado_bbox$xmax - 1,
  xmax = colorado_bbox$xmax - 0.5,
  ymin = colorado_bbox$ymax - 1,
  ymax = colorado_bbox$ymax - 0.5
) +
  
  # -----------------------------------------------------
# SIMPLE SCALE BAR (matches your screenshot)
# -----------------------------------------------------
annotation_custom(
  grob = grobTree(
    rectGrob(
      width = unit(1, "cm"), height = unit(0.3, "cm"),
      x = unit(0, "cm"), just = "left",
      gp = gpar(fill = "black")
    ),
    rectGrob(
      width = unit(1, "cm"), height = unit(0.3, "cm"),
      x = unit(1, "cm"), just = "left",
      gp = gpar(fill = "white", col = "black")
    ),
    textGrob("10 km", y = unit(-0.4, "cm"), gp = gpar(fontsize = 10))
  ),
  xmin = colorado_bbox$xmax - 2,
  xmax = colorado_bbox$xmax - 0.5,
  ymin = colorado_bbox$ymin + 0.3,
  ymax = colorado_bbox$ymin + 0.8
)

# Save final map
ggsave(
  "Roads_that_Cross_Owl_FINALHabitat.pdf",
  plot = owl_map, width = 10, height = 8, units = "in"
)

##please


## UPDATED ROADS THAT CROSS MAP
install.packages("ggrepel")
library(ggplot2)
library(sf)
library(grid)
library(ggrepel)

# --------- FIX ORIENTATION (REPROJECT) ----------
colorado <- st_transform(colorado, 4326)
counties <- st_transform(counties, 4326)
habitats_co <- st_transform(habitats_co, 4326)
roads_full_touching <- st_transform(roads_full_touching, 4326)

colorado_bbox <- st_bbox(colorado)

# --------- MAIN MAP ----------
owl_map <- ggplot() +
  
  # Colorado outline
  geom_sf(data = colorado, fill = "gray85", color = "black") +
  
  # Counties
  geom_sf(data = counties, fill = NA, color = "gray40", size = 0.4) +
  
  # County labels
  geom_sf_text(
    data = counties,
    aes(label = NAME),
    size = 2.5,
    color = "black",
    check_overlap = TRUE
  ) +
  
  # Habitat
  geom_sf(data = habitats_co, fill = "lightgreen", alpha = 0.5, color = "darkgreen") +
  
  # Roads
  geom_sf(data = roads_full_touching, color = "red", size = 0.8) +
  
  # ---------- FIXED ROAD LABELS ----------
geom_text_repel(
  data = st_as_sf(roads_full_touching),
  aes(label = FULLNAME, geometry = geometry),
  stat = "sf_coordinates",
  size = 2.5,
  color = "black",
  box.padding = 0.5,
  point.padding = 0.5,
  min.segment.length = 0,
  segment.color = "gray30"
) +
  
  coord_sf(
    xlim = c(colorado_bbox$xmin, colorado_bbox$xmax),
    ylim = c(colorado_bbox$ymin, colorado_bbox$ymax),
    expand = FALSE, datum = NA
  ) +
  
  ggtitle("Roads that Cross Owl Habitat") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  
  
  ####################################################
# NORTH ARROW — MATCHING THE SCREENSHOT EXACTLY
####################################################
annotation_custom(
  grob = grobTree(
    # Compass star style (looks like your screenshot)
    linesGrob(
      x = unit(c(0.5, 0.5), "npc"),
      y = unit(c(0.85, 1), "npc"),
      gp = gpar(col = "black", lwd = 2)
    ),
    linesGrob(
      x = unit(c(0.45, 0.55), "npc"),
      y = unit(c(0.92, 0.92), "npc"),
      gp = gpar(col = "black", lwd = 2)
    ),
    textGrob("N", y = 1.05, gp = gpar(fontsize = 14, fontface = "bold"))
  ),
  xmin = colorado_bbox$xmax - 0.7,
  xmax = colorado_bbox$xmax - 0.25,
  ymin = colorado_bbox$ymax - 0.7,
  ymax = colorado_bbox$ymax - 0.25
) +
  
  
  ####################################################
# SCALE BAR — MATCHING THE BLACK/WHITE 20–40–60 KM BAR
####################################################
annotation_custom(
  grob = grobTree(
    # Black bar
    rectGrob(
      x = unit(0, "cm"), y = 0,
      width = unit(1.5, "cm"), height = unit(0.25, "cm"),
      just = "left",
      gp = gpar(fill = "black")
    ),
    # White bar
    rectGrob(
      x = unit(1.5, "cm"), y = 0,
      width = unit(1.5, "cm"), height = unit(0.25, "cm"),
      just = "left",
      gp = gpar(fill = "white", col = "black")
    ),
    # Third black bar
    rectGrob(
      x = unit(3, "cm"), y = 0,
      width = unit(1.5, "cm"), height = unit(0.25, "cm"),
      just = "left",
      gp = gpar(fill = "black")
    ),
    
    # Text labels 20, 40, 60 km
    textGrob("20", x = unit(1.5, "cm"), y = unit(-0.35, "cm"), gp = gpar(fontsize = 9)),
    textGrob("40", x = unit(3, "cm"), y = unit(-0.35, "cm"), gp = gpar(fontsize = 9)),
    textGrob("60 km", x = unit(4.5, "cm"), y = unit(-0.35, "cm"), gp = gpar(fontsize = 9))
  ),
  xmin = colorado_bbox$xmax - 2.8,
  xmax = colorado_bbox$xmax - 0.3,
  ymin = colorado_bbox$ymin + 0.25,
  ymax = colorado_bbox$ymin + 0.9
)


# SAVE MAP
ggsave(
  "Roads_that_Cross_Owl_FINALHabitat2.pdf",
  plot = owl_map, width = 10, height = 8, units = "in"
)


##PLEASE BE THE FINAL

# -------------------------------
# Libraries
# -------------------------------
install.packages("ggspatial")  # only if not installed
library(ggplot2)
library(sf)
library(grid)
library(ggrepel)
library(ggspatial)

# -------------------------------
# Reproject layers
# -------------------------------
colorado <- st_transform(colorado, 4326)
counties <- st_transform(counties, 4326)
habitats_co <- st_transform(habitats_co, 4326)
roads_full_touching <- st_transform(roads_full_touching, 4326)

colorado_bbox <- st_bbox(colorado)

# -------------------------------
# Main Map
# -------------------------------
owl_map <- ggplot() +
  
  # Colorado outline
  geom_sf(data = colorado, fill = "gray85", color = "black") +
  
  # Counties
  geom_sf(data = counties, fill = NA, color = "gray40", size = 0.4) +
  
  # County labels
  geom_sf_text(
    data = counties,
    aes(label = NAME),
    size = 2.5,
    color = "black",
    check_overlap = TRUE
  ) +
  
  # Habitat
  geom_sf(data = habitats_co, fill = "lightgreen", alpha = 0.5, color = "darkgreen") +
  
  # Roads
  geom_sf(data = roads_full_touching, color = "red", size = 0.8) +
  
  # Road labels
  geom_text_repel(
    data = st_as_sf(roads_full_touching),
    aes(label = FULLNAME, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.5,
    color = "black",
    box.padding = 0.5,
    point.padding = 0.5,
    min.segment.length = 0,
    segment.color = "gray30"
  ) +
  
  # Proper non-skewed orientation
  coord_sf(
    xlim = c(colorado_bbox$xmin, colorado_bbox$xmax),
    ylim = c(colorado_bbox$ymin, colorado_bbox$ymax),
    expand = FALSE, datum = NA
  ) +
  
  # Title & theme
  ggtitle("Roads that Cross Owl Habitat") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  
  # -------------------------------
# North arrow (upper right) & scale bar (bottom right)
# -------------------------------
annotation_north_arrow(
  location = "tr",
  which_north = "true",
  style = north_arrow_fancy_orienteering
) +
  annotation_scale(
    location = "br",
    width_hint = 0.25
  )

# -------------------------------
# Save final map
# -------------------------------
ggsave(
  "Roads_that_Cross_Owl_FINALHabitat.pdf",
  plot = owl_map,
  width = 10,
  height = 8,
  units = "in"
)



##wildlife refuge data
library(sf)
library(dplyr)
# Load Wildlife Refuge shapefile
refuge_sf <- st_read("data/FWSBoundaries.shp")
# Make sure CRS matches habitats
refuge_sf <- st_transform(refuge_sf, st_crs(habitats_co))

##2nd vis
library(dplyr)
library(ggplot2)
library(sf)

#Calculate length of each road segment
roads_full_touching$Length_m <- as.numeric(st_length(roads_full_touching))

#Summarize total overlapping length per road
road_name_summary <- roads_full_touching %>%
  st_set_geometry(NULL) %>%
  group_by(FULLNAME) %>%             # Road name column
  summarise(TotalLength_km = sum(Length_m) / 1000) %>%  # convert to km
  arrange(desc(TotalLength_km))      # sort descending

#Optional: show top 20 roads only for clarity
top_roads <- road_name_summary %>% slice_max(TotalLength_km, n = 20)

#Bar chart I WANT TO KEEP THIS!! GOOD vis
ggplot(top_roads, aes(x = reorder(FULLNAME, TotalLength_km), y = TotalLength_km, fill = FULLNAME)) +
  geom_col() +
  coord_flip() +  # horizontal for readability
  labs(title = "Top Roads Overlapping Mexican Spotted Owl Habitat",
       x = "Road Name",
       y = "Total Overlapping Length (km)") +
  theme_minimal() +
  theme(legend.position = "none")
#Save bar chart as PDF
ggsave("Top_Roads_Overlapping_Owl_Habitat.pdf", 
       plot = last_plot(),  # the most recent ggplot object
       width = 10, height = 6, units = "in")
##check CRS
st_crs(habitats_co)
st_crs(roads_full_touching)
st_crs(colorado)
st_crs(counties)
##same crs
counties <- st_transform(counties, st_crs(habitats_co))
##mining data
mining <- read.csv("data/mining_habitat_data.csv")
##urban areas data 2020
urban <- st_read("data/Urban_Areas_-_Adjusted_2020.shp")

##vis 3 NEED TOO ADD COUNTIES PERFECT MAP DONT DELETE
# Libraries
library(sf)
library(dplyr)
library(ggplot2)
library(cartogram)

# 1. Reproject all layers to UTM Zone 13N

target_crs <- 26913

roads_full_touching <- st_transform(roads_full_touching, crs = target_crs)
urban <- st_transform(urban, crs = target_crs)
habitats_co <- st_transform(habitats_co, crs = target_crs)
colorado <- st_transform(colorado, crs = target_crs)

#Ensure valid geometries
roads_full_touching <- st_make_valid(roads_full_touching)
urban <- st_make_valid(urban)
habitats_co <- st_make_valid(habitats_co)
colorado <- st_make_valid(colorado)


#Filter counties to only those overlapping owl habitat

counties_with_habitat <- lengths(st_intersects(colorado, habitats_co)) > 0
relevant_counties <- colorado[counties_with_habitat, ]


#Compute road overlap within relevant counties

roads_by_county <- st_intersection(roads_full_touching, relevant_counties) %>%
  mutate(length_m = as.numeric(st_length(geometry))) %>%
  group_by(NAME) %>%
  summarise(road_length_km = sum(length_m)/1000)


#Compute urban overlap within relevant counties

urban_by_county <- st_intersection(urban, relevant_counties) %>%
  mutate(area_m2 = as.numeric(st_area(geometry))) %>%
  group_by(NAME) %>%
  summarise(urban_area_km2 = sum(area_m2)/1e6)


#Compute habitat area per county

habitat_by_county <- st_intersection(habitats_co, relevant_counties) %>%
  mutate(area_m2 = as.numeric(st_area(geometry))) %>%
  group_by(NAME) %>%
  summarise(habitat_area_km2 = sum(area_m2)/1e6)


#Combine into county-level risk metric
#Use habitat area as baseline to ensure circles for all habitat counties

county_risk <- relevant_counties %>%
  left_join(st_set_geometry(roads_by_county, NULL), by = "NAME") %>%
  left_join(st_set_geometry(urban_by_county, NULL), by = "NAME") %>%
  left_join(st_set_geometry(habitat_by_county, NULL), by = "NAME") %>%
  mutate(
    road_length_km = ifelse(is.na(road_length_km), 0, road_length_km),
    urban_area_km2 = ifelse(is.na(urban_area_km2), 0, urban_area_km2),
    habitat_area_km2 = ifelse(is.na(habitat_area_km2), 0, habitat_area_km2),
    # risk score: habitat area baseline + roads + urban
    risk_score = 0.1 * habitat_area_km2 + road_length_km + urban_area_km2
  )


#Create Dorling cartogram

dorling_map <- cartogram_dorling(county_risk, "risk_score", k = 1)


#Centroids for circles and labels

dorling_centers <- st_centroid(dorling_map)
county_centroids <- st_centroid(colorado)  # all Colorado counties for labels


#Plot

owl_risk_map <- ggplot() +
#Colorado state boundary
  geom_sf(data = colorado, fill = "gray90", color = "black", size = 0.5) +
  
#County outlines
  geom_sf(data = colorado, fill = NA, color = "gray60", size = 0.3) +
  
#Owl habitat boundaries
  geom_sf(data = habitats_co, fill = NA, color = "darkgreen", size = 0.5) +
  
#Dorling circles for habitat counties
  geom_sf(data = dorling_centers, 
          aes(size = risk_score, fill = risk_score), 
          shape = 21, color = "black", alpha = 0.7) +
  
#County labels (all Colorado counties)
  geom_sf_text(data = county_centroids, aes(label = NAME),
               size = 2.5, color = "black") +
  
#Blue → Green gradient for risk score
  scale_fill_gradientn(
    colors = c("#c6dbef", "#41b6c4", "#006400"),  # light blue → teal → dark green
    name = "Risk Score"
  )+
  
#Circle sizes
  scale_size_continuous(range = c(3, 15), name = "Risk Score") +
  
#Title & theme
  ggtitle("How Roads and Urban Areas Affect Owl Habitat by County") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
#Save as PDF
ggsave("Owl_Habitat_Risk_Cartogram.pdf", 
       plot = owl_risk_map, 
       width = 12, height = 9, units = "in")
##fix habitats
library(sf)
library(dplyr)
library(ggplot2)
library(cartogram)

#Reproject all layers to UTM Zone 13N

target_crs <- 26913
roads_full_touching <- st_transform(roads_full_touching, target_crs)
urban <- st_transform(urban, target_crs)
habitats_co <- st_transform(habitats_co, target_crs)
colorado <- st_transform(colorado, target_crs)

#Ensure valid geometries
roads_full_touching <- st_make_valid(roads_full_touching)
urban <- st_make_valid(urban)
habitats_co <- st_make_valid(habitats_co)
colorado <- st_make_valid(colorado)

#Filter counties with habitat

relevant_counties <- colorado[lengths(st_intersects(colorado, habitats_co)) > 0, ]

#Compute risk components

roads_by_county <- st_intersection(roads_full_touching, relevant_counties) %>%
  mutate(length_m = as.numeric(st_length(geometry))) %>%
  group_by(NAME) %>%
  summarise(road_length_km = sum(length_m)/1000, .groups="drop")

urban_by_county <- st_intersection(urban, relevant_counties) %>%
  mutate(area_m2 = as.numeric(st_area(geometry))) %>%
  group_by(NAME) %>%
  summarise(urban_area_km2 = sum(area_m2)/1e6, .groups="drop")

habitat_by_county <- st_intersection(habitats_co, relevant_counties) %>%
  mutate(area_m2 = as.numeric(st_area(geometry))) %>%
  group_by(NAME) %>%
  summarise(habitat_area_km2 = sum(area_m2)/1e6, .groups="drop")


#Combine risk

county_risk <- relevant_counties %>%
  left_join(st_set_geometry(roads_by_county, NULL), by = "NAME") %>%
  left_join(st_set_geometry(urban_by_county, NULL), by = "NAME") %>%
  left_join(st_set_geometry(habitat_by_county, NULL), by = "NAME") %>%
  mutate(
    road_length_km = ifelse(is.na(road_length_km), 0, road_length_km),
    urban_area_km2 = ifelse(is.na(urban_area_km2), 0, urban_area_km2),
    habitat_area_km2 = ifelse(is.na(habitat_area_km2), 0, habitat_area_km2),
    risk_score = 0.1 * habitat_area_km2 + road_length_km + urban_area_km2
  )

#Keep counties with positive risk
county_risk <- county_risk %>% filter(risk_score > 0)


#Dorling cartogram

dorling_map <- cartogram_dorling(county_risk, "risk_score", k = 1)
dorling_centers <- st_centroid(dorling_map)
county_centroids <- st_centroid(colorado)

#Plot with habitats filled

owl_risk_map <- ggplot() +
#Colorado boundary
  geom_sf(data = colorado, fill = "gray90", color = "black", size = 0.5) +
  
#County outlines
  geom_sf(data = colorado, fill = NA, color = "gray60", size = 0.3) +
  
#Owl habitat filled in light green
  geom_sf(data = habitats_co, fill = "lightgreen", color = "darkgreen", alpha = 0.4, size = 0.5) +
  
#Dorling circles
  geom_sf(data = dorling_centers,
          aes(size = risk_score, fill = risk_score),
          shape = 21, color = NA, alpha = 0.7) +
  
#County labels
  geom_sf_text(data = county_centroids, aes(label = NAME), size = 2.5, color = "black") +
  
#Blue - dark green gradient for risk
  scale_fill_gradientn(colors = c("#c6dbef", "#41b6c4", "#006400"), name = "Risk Score") +
  
#Circle sizes
  scale_size_continuous(range = c(3, 15), name = "Risk Score") +
  
#Title & theme
  ggtitle("How Roads and Urban Areas Affect Owl Habitat by County") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

#Save PDF

ggsave("Owl_Habitat_Risk_Cartogram.pdf", plot = owl_risk_map,
       width = 12, height = 9, units = "in")

## UPDATED FINAL PERFECT MAP

# Owl Habitat Risk Map (Dorling Circles with Size Legend)


# Libraries
library(sf)
library(dplyr)
library(ggplot2)
library(cartogram)
library(ggspatial)


#Reproject layers to UTM Zone 13N

target_crs <- 26913

roads_full_touching <- st_transform(roads_full_touching, target_crs)
urban <- st_transform(urban, target_crs)
habitats_co <- st_transform(habitats_co, target_crs)
colorado <- st_transform(colorado, target_crs)

#Ensure valid geometries
roads_full_touching <- st_make_valid(roads_full_touching)
urban <- st_make_valid(urban)
habitats_co <- st_make_valid(habitats_co)
colorado <- st_make_valid(colorado)

#Filter counties with habitat

relevant_counties <- colorado[lengths(st_intersects(colorado, habitats_co)) > 0, ]


#Compute risk components

roads_by_county <- st_intersection(roads_full_touching, relevant_counties) %>%
  mutate(length_m = as.numeric(st_length(geometry))) %>%
  group_by(NAME) %>%
  summarise(road_length_km = sum(length_m)/1000, .groups="drop")

urban_by_county <- st_intersection(urban, relevant_counties) %>%
  mutate(area_m2 = as.numeric(st_area(geometry))) %>%
  group_by(NAME) %>%
  summarise(urban_area_km2 = sum(area_m2)/1e6, .groups="drop")

habitat_by_county <- st_intersection(habitats_co, relevant_counties) %>%
  mutate(area_m2 = as.numeric(st_area(geometry))) %>%
  group_by(NAME) %>%
  summarise(habitat_area_km2 = sum(area_m2)/1e6, .groups="drop")

#Combine into county-level risk

county_risk <- relevant_counties %>%
  left_join(st_set_geometry(roads_by_county, NULL), by = "NAME") %>%
  left_join(st_set_geometry(urban_by_county, NULL), by = "NAME") %>%
  left_join(st_set_geometry(habitat_by_county, NULL), by = "NAME") %>%
  mutate(
    road_length_km = ifelse(is.na(road_length_km), 0, road_length_km),
    urban_area_km2 = ifelse(is.na(urban_area_km2), 0, urban_area_km2),
    habitat_area_km2 = ifelse(is.na(habitat_area_km2), 0, habitat_area_km2),
    risk_score = 0.1 * habitat_area_km2 + road_length_km + urban_area_km2
  ) %>%
  filter(risk_score > 0)


#Create Dorling cartogram

dorling_map <- cartogram_dorling(county_risk, "risk_score", k = 1)
dorling_map <- st_make_valid(dorling_map)

#Centroids for circles
dorling_centers <- st_centroid(dorling_map)

# ounty centroids for labels
county_centroids <- st_centroid(colorado)

#Plot map with adjusted north arrow & scale bar

owl_risk_map <- ggplot() +
  
#Colorado boundary
  geom_sf(data = colorado, fill = "gray90", color = "black", size = 0.5) +
  
#County outlines
  geom_sf(data = colorado, fill = NA, color = "gray60", size = 0.3) +
  
#Owl habitat
  geom_sf(data = habitats_co, fill = "lightgreen", color = "darkgreen",
          alpha = 0.4, size = 0.5) +
  
#Dorling circles (centroids)
  geom_sf(data = dorling_centers,
          aes(size = risk_score, fill = risk_score),
          shape = 21, color = "black", alpha = 0.7) +
  
#County labels
  geom_sf_text(data = county_centroids, aes(label = NAME),
               size = 2.5, color = "black") +
  
#Color + size scales
  scale_fill_gradientn(colors = c("#c6dbef", "#41b6c4", "#006400"),
                       name = "Risk Score") +
  scale_size_continuous(range = c(3, 15), name = "Risk Score") +
  
#North arrow in upper right
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  
#Scale bar in bottom right
  annotation_scale(location = "br", width_hint = 0.25) +
  
#Default coordinates
  coord_sf() +
#Title/Theme
  ggtitle("How Roads and Urban Areas Affect Owl Habitat by County") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

#Save Map
ggsave("Owl_Habitat_Risk_Cartogram_Circles.pdf",
       plot = owl_risk_map, width = 12, height = 9, units = "in")

ggsave("Owl_Habitat_Risk_Cartogram_Circles.png",
       plot = owl_risk_map, width = 12, height = 9, units = "in", dpi = 300)
