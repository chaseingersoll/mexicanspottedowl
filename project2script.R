install.packages("dplyr")
install.packages("tidyverse")
install.packages("reshape2")
install.packages("sf")
install.packages("geojsonf")
install.packages("tmap")
install.packages("ggplot2")
install.packages("cartogram")
install.packages("pals")
install.packages("corrplot")
install.packages("geojsonsf")
install.packages("geojson2")
install.packages("tmap")
install.packages("intersects")
install.packages("cartogram")
install.packages("join")
install.packages("arrange")
install.packages("dcast")
install.packages("data.table")
install.packages("igraph")
install.packages("randomcoloR")
install.packages("purrr")
install.packages("ggplot2")
install.packages("ggspatial")
install.packages("networkD3")
install.packages("tif")
install.packages("raster")   # older but compatible
install.packages("terra")    # faster, modern replacement


library(dplyr)
library(tidyverse)
library(reshape2)
library(sf)
library(geojsonsf)
library(geojson2)
library(tmap)
library(ggplot2)
library(viridis)
library(cartogram)
library(pals)
library(corrplot)
library(tmap)
library(intersects)
library(sf)
library(cartogram_cont)
library(arrange)
library(dcast)
library(spatstat)
library(dplyr)
library(data.table)
library(sf)
library(igraph)
library(randomcoloR)
library(tidyverse)
library(purrr)
library(ggplot2)
library(ggspatial)
library(networkD3)
library("tif")
library("raster")
library("terra")
getwd()




###-------------------------
##MAP FOR NWR AND OWL HABITATS
library(stringr)
library(ggspatial)  # for annotation_scale() and annotation_north_arrow()

# Project all layers using Colorado Central State Plane (EPSG:2876)
colorado_proj <- colorado %>% 
  st_make_valid() %>% 
  st_transform(2876)

nwr_co_proj <- st_transform(nwr_co, 2876)
habitats_co_proj <- st_transform(habitats_co, 2876)

# Add Layer column
nwr_co_proj$Layer <- "NWR"
habitats_co_proj$Layer <- "MSO Habitat"

# Combine layers for plotting
all_layers_proj <- bind_rows(
  st_sf(Layer = nwr_co_proj$Layer, geometry = st_geometry(nwr_co_proj)),
  st_sf(Layer = habitats_co_proj$Layer, geometry = st_geometry(habitats_co_proj))
)

# County labels – remove "County"
colorado_labeled <- colorado_proj %>%
  mutate(
    label_text = coalesce(NAMELSAD, NAME, "Unknown County"),
    label_text = str_remove(label_text, regex("(?i) county$"))
  )

# Compute label points inside counties
county_points <- st_point_on_surface(colorado_labeled)
county_coords <- st_coordinates(county_points)

# Extract labels
county_labels <- colorado_labeled %>%
  st_drop_geometry() %>%
  select(label_text)

# Combine into non-sf frame
label_df <- cbind(county_coords, county_labels)

# Plot
MSO_NWR_Colorado_Map <- ggplot() +
  # Counties – light gray fill, darker borders for contrast
  geom_sf(data = colorado_labeled, fill = "gray90", color = "gray50", size = 0.2) + 
  
  # NWR + MSO layers
  geom_sf(data = all_layers_proj, aes(fill = Layer), color = NA, alpha = 0.5) +
  
  # County labels
  geom_text(
    data = label_df,
    aes(x = X, y = Y, label = label_text),
    size = 2.5,
    color = "black"
  ) +
  
  # Custom fill colors for layers
  scale_fill_manual(
    values = c(
      "NWR" = "darkblue",
      "MSO Habitat" = "forestgreen"
    )
  ) +
  
  # ---- North arrow and scale bar ----
annotation_scale(
  location = "br",        # bottom-left
  width_hint = 0.25,
  text_cex = 0.6
) +
  annotation_north_arrow(
    location = "tr",        # top-right
    which_north = "true",
    pad_x = unit(0.3, "in"),
    pad_y = unit(0.3, "in"),
    height = unit(0.9, "cm"),
    width = unit(0.9, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    plot.background  = element_blank(),
    
    # ---- Center title ----
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  
  labs(
    title = "Mexican Spotted Owl Habitat and National Wildlife Refuges in Colorado",
    fill = "Layer",
    caption = "Data sources: FWS, US Census TIGER"
  )

# 6. Save PDF
ggsave("plot/MSO_NWR_Colorado_Map_Upright22.pdf",
       plot = MSO_NWR_Colorado_Map,
       width = 10,
       height = 8,
       units = "in")




###---------------------------
##THIS IS THE MAP FOR URBAN AREAS AND OWL HABITATS

# Add a column for layer type
habitats_plot <- habitats_proj %>% mutate(Layer = "Owl Habitat")
urban_plot <- urban_proj %>% mutate(Layer = "Urban Areas Greater than 5000")

# Combine both layers
layers_combined <- bind_rows(habitats_plot, urban_plot)

# Make Layer a factor to control legend order
layers_combined$Layer <- factor(
  layers_combined$Layer,
  levels = c("Owl Habitat", "Urban Areas Greater than 5000")
)

Owl_Habitats_and_Urban_Areas_in_Colorado <- ggplot() +
  
  # Colorado counties with light gray fill
  geom_sf(data = colorado_proj, fill = "gray90", color = "gray50", size = 0.2) +
  
  # Owl habitats & urban areas
  geom_sf(data = layers_combined, aes(fill = Layer), color = NA, alpha = 0.5) +
  
  # County labels
  geom_sf_text(data = colorado_proj, aes(label = NAME), size = 2.0, color = "black") +
  
  # Custom fill colors for overlay layers
  scale_fill_manual(
    values = c(
      "Owl Habitat" = "green",
      "Urban Areas Greater than 5000" = "blue"
    )
  ) +
  
  # Map projection
  coord_sf(crs = st_crs(4326)) +
  
  # North arrow (moved farther right and up)
  annotation_north_arrow(
    location = "tr",        # top-right corner reference
    which_north = "true",
    pad_x = unit(.1, "in"),  # move farther right
    pad_y = unit(.1, "in"),  # move farther up
    height = unit(0.9, "cm"),
    width = unit(0.9, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  
  # Scale bar (bottom-right corner)
  annotation_scale(
    location = "br",
    width_hint = 0.2,
    text_cex = 0.6,
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in")
  ) +
  
  # Minimal theme with centered title
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    plot.background  = element_blank(),
    
    # Center the title
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  
  # Map title and legend
  labs(
    title = "Owl Habitats and Urban Areas in Colorado",
    fill = "Layer Type"
  )

## --- Save Map as PDF ---

ggsave(
  "plot/Owl_Habitats_and_Urban_Areas_in_Colorado.pdf",
  plot = Owl_Habitats_and_Urban_Areas_in_Colorado,
  width = 10,
  height = 8,
  units = "in"
)

##----------------------------
##PIE CHART FOR URBAN AREAS/NWR/MSO HABITATS


# Project all layers to the same CRS for accurate area calculation
habitats_proj <- st_transform(habitats_co, 5070)
nwr_proj <- st_transform(nwr_co, 5070)
urban_proj <- st_transform(urban_areas, 5070)

# Calculate total area for each layer in km²
habitat_area <- sum(st_area(habitats_proj)) / 1e6
nwr_area <- sum(st_area(nwr_proj)) / 1e6
urban_area <- sum(st_area(urban_proj)) / 1e6

# Combine into a data frame
pie_df <- data.frame(
  Layer = c("Owl Habitats", "Wildlife Refuges", "Urban Areas"),
  Area_km2 = c(as.numeric(habitat_area), as.numeric(nwr_area), as.numeric(urban_area))
)

# Add percentage labels
pie_df <- pie_df %>%
  mutate(
    Percent = round(Area_km2 / sum(Area_km2) * 100, 1),
    Label = paste0(Layer, " (", Percent, "%)")
  )
# Assign your pie chart to a variable
Land_Cover_Areas_in_Colorado <- ggplot(pie_df, aes(x = "", y = Area_km2, fill = Layer)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c(
    "Owl Habitats" = "lightgreen",
    "Wildlife Refuges" = "darkgreen",
    "Urban Areas" = "lightblue"
  )) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3) +
  theme_void() +
  labs(title = "Land Cover Areas in Colorado")


# Create pie chart
ggplot(pie_df, aes(x = "", y = Area_km2, fill = Layer)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c(
    "Owl Habitats" = "lightgreen",
    "Wildlife Refuges" = "darkgreen",
    "Urban Areas" = "lightblue"
  )) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3) +
  theme_void() +
  labs(title = "Land Cover Areas in Colorado")

pdf("plot/Land_Cover_Areas_in_Colorado.pdf")
Land_Cover_Areas_in_Colorado
dev.off()
