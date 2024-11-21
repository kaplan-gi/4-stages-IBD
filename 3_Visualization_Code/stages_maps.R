# random forest classifier stages map
# Contributor: Lindsay Hracs
# Date of script: 2023-11-27
# Date of map generation: 2024-10-21

if (!require("pacman")) install.packages("pacman")

# Install and load packages
pacman::p_load(
      leaflet,                
      leaflet.esri,
      leaflet.extras,
      sf,
      geojsonsf,
      tidyverse,
      ggplot2,
      dplyr,
      viridis,
      ggiraph,
      ggpubr,
      RColorBrewer,
      htmltools,
      htmlwidgets,
      leafsync,
      leaflegend,
      magick) 

# Prep data 
geofiles <- geojson_sf("https://raw.githubusercontent.com/kaplan-gi/geo_files/main/res50m_230223.geojson")

data <- read.csv("~/file/path/file_name.csv")

# Format names in data to match geofiles
data$country[data$country=="Bosnia and Herzegovina"] <- "Bosnia and Herz."
data$country[data$country=="Czech Republic"] <- "Czech Rep."
data$country[data$country=="Faroe Islands"] <- "Faeroe Is."
data$country[data$country=="Turkiye"] <- "Turkey"

# Regions not in geofiles due to status and will not be on static maps because they are spatial subsets:
    # "Catalonia"
    # "England"
    # "Northern Ireland"
    # "Scotland"
    # "Wales"

colnames(data)[which(names(data) == "country")] <- "name"

dec_data <- merge(geofiles, data, by = "name")

stages1950to1959 <- subset(dec_data, decade == "1950-1959")
stages1960to1969 <- subset(dec_data, decade == "1960-1969")
stages1970to1979 <- subset(dec_data, decade == "1970-1979")
stages1980to1989 <- subset(dec_data, decade == "1980-1989")
stages1990to1999 <- subset(dec_data, decade == "1990-1999")
stages2000to2009 <- subset(dec_data, decade == "2000-2009")
stages2010to2019 <- subset(dec_data, decade == "2010-2019")
stages2020to2024 <- subset(dec_data, decade == "2020-2024")

pre2020_stage3 <- dec_data %>%
    filter(predicted_stage == "stage3" & decade != "2020-2024" & !name %in% unique(stages2020to2024$name)) %>%
    mutate(min_range = as.numeric(gsub("-.*", "", decade))) %>%
    group_by(name) %>%
    slice(which.max(min_range))

pal_factor <- as.factor(dec_data$predicted_stage)       

pal <- colorFactor(palette = c("#7570B3", "#D95F02", "#1B9E77"), domain = dec_data$predicted_stage)

title_panel1 = "<span style = 'font-size: 1000%; font-weight:bold;'>A</span>"
title_panel2 = "<span style = 'font-size: 1000%; font-weight:bold;'>B</span>"
title_panel3 = "<span style = 'font-size: 1000%; font-weight:bold;'>C</span>"
title_panel4 = "<span style = 'font-size: 1000%; font-weight:bold;'>D</span>"
title_panel5 = "<span style = 'font-size: 1000%; font-weight:bold;'>E</span>"
title_panel6 = "<span style = 'font-size: 1000%; font-weight:bold;'>F</span>"
title_panel7 = "<span style = 'font-size: 1000%; font-weight:bold;'>G</span>"
title_panel8 = "<span style = 'font-size: 1000%; font-weight:bold;'>H</span>"

# Create dummy factors for leaflegend legend which allows horizontal orientation
dummy_stages <- c("Stage 1 ", "Stage 2 ", "Stage 3 ", "No Data Available ")
dummy <- as.data.frame(dummy_stages)

# Create palette with dummy data
pal_new <- colorFactor(c("#F0F0ED", "#7570B3", "#D95F02", "#1B9E77"), dummy$dummy_stages)

# Build maps

# 1950 to 1959
stages1950to1959_leaflet <- leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 1.25, zoomControl = FALSE, attributionControl=FALSE)) %>% 
    addProviderTiles("CartoDB.PositronNoLabels") %>% 
    setView(lng = 0, lat = 40, zoom = 2.9) %>%
    addPolygons(data = stages1950to1959, 
                color = "black",
                fillColor = ~pal(predicted_stage),
                weight = 1,
                fillOpacity = 0.85) %>%
    addControl(title_panel1, position = "topleft", className="map-title")

# 1960 to 1969
stages1960to1969_leaflet <- leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 1.25, zoomControl = FALSE, attributionControl=FALSE)) %>% 
    addProviderTiles("CartoDB.PositronNoLabels") %>% 
    setView(lng = 0, lat = 40, zoom = 2.9) %>%
    addPolygons(data = stages1960to1969, 
                color = "black",
                fillColor = ~pal(predicted_stage),
                weight = 1,
                fillOpacity = 0.85) %>%
    addControl(title_panel2, position = "topleft", className="map-title")

# 1970 to 1979
stages1970to1979_leaflet <- leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 1.25, zoomControl = FALSE, attributionControl=FALSE)) %>% 
    addProviderTiles("CartoDB.PositronNoLabels") %>% 
    setView(lng = 0, lat = 40, zoom = 2.9) %>%
    addPolygons(data = stages1970to1979,
                color = "black",
                fillColor = ~pal(predicted_stage),
                weight = 1,
                fillOpacity = 0.85) %>%
    addControl(title_panel3, position = "topleft", className="map-title")

# 1980 to 1989
stages1980to1989_leaflet <- leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 1.25, zoomControl = FALSE, attributionControl=FALSE)) %>% 
    addProviderTiles("CartoDB.PositronNoLabels") %>% 
    setView(lng = 0, lat = 40, zoom = 2.9) %>%
    addPolygons(data = stages1980to1989,
                color = "black",
                fillColor = ~pal(predicted_stage),
                weight = 1,
                fillOpacity = 0.85) %>%
    addControl(title_panel4, position = "topleft", className="map-title")

# 1990 to 1999
stages1990to1999_leaflet <- leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 1.25, zoomControl = FALSE, attributionControl=FALSE)) %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>% 
    setView(lng = 0, lat = 40, zoom = 2.9) %>%
    addPolygons(data = stages1990to1999,
                color = "black",
                fillColor = ~pal(predicted_stage),
                weight = 1,
                fillOpacity = 0.85) %>%
    addControl(title_panel5, position = "topleft", className="map-title")

# 2000 to 2010
stages2000to2009_leaflet <- leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 1.25, zoomControl = FALSE, attributionControl=FALSE)) %>% 
    addProviderTiles("CartoDB.PositronNoLabels") %>% 
    setView(lng = 0, lat = 40, zoom = 2.9) %>%
    addPolygons(data = stages2000to2009,
                color = "black",
                fillColor = ~pal(predicted_stage),
                weight = 1,
                fillOpacity = 0.85) %>%
    addControl(title_panel6, position = "topleft", className="map-title")

# 2010 to 2019
stages2010to2019_leaflet <- leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 1.25, zoomControl = FALSE, attributionControl=FALSE)) %>% 
    addProviderTiles("CartoDB.PositronNoLabels") %>% 
    setView(lng = 0, lat = 40, zoom = 2.9) %>%
    addPolygons(data = stages2010to2019,
                color = "black",
                fillColor = ~pal(predicted_stage),
                weight = 1,
                fillOpacity = 0.85) %>%  
    addControl(title_panel7, position = "topleft", className="map-title")

# 2020 to 2024
stages2020to2024_leaflet <- leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 1.25, zoomControl = FALSE, attributionControl=FALSE)) %>% 
    addProviderTiles("CartoDB.PositronNoLabels") %>% 
    setView(lng = 0, lat = 40, zoom = 2.9) %>%
    addPolygons(data = stages2020to2024,
                color = "black",
                fillColor = ~pal(predicted_stage),
                weight = 1,
                fillOpacity = 0.85) %>%  
    addControl(title_panel8, position = "topleft", className="map-title") %>%
    addLegendFactor(pal = pal_new,
                    values = dummy$dummy_stages,
                    position = "bottomright",
                    title = "",
                    fillOpacity = 1,
                    opacity = 0,
                    labelStyle = "font-size: 50px;",
                    orientation = "horizontal", 
                    height = 55,
                    width = 55)

# 2020 to 2024 alternate
stages2020to2024_leaflet_alternate <- leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 1.25, zoomControl = FALSE, attributionControl=FALSE)) %>% 
    addProviderTiles("CartoDB.PositronNoLabels") %>% 
    setView(lng = 0, lat = 40, zoom = 2.9) %>%
    addPolygons(data = stages2020to2024,
                color = "black",
                fillColor = ~pal(predicted_stage),
                weight = 1,
                fillOpacity = 0.85) %>%  
    addPolygons(data = pre2020_stage3,
                color = "black",
                fillColor = "#1B9E77",
                weight = 0.75,
                fillOpacity = 0.45) %>%
    addControl(title_panel8, position = "topleft", className="map-title") %>%
    addLegendFactor(pal = pal_new,
                    values = dummy$dummy_stages,
                    position = "bottomright",
                    title = "",
                    fillOpacity = 1,
                    opacity = 0,
                    labelStyle = "font-size: 50px;",
                    orientation = "horizontal", 
                    height = 55,
                    width = 55)
stages2020to2024_leaflet_alternate

# Save map as HTML object (change plot name and file name for each output)
htmlwidgets::saveWidget(widget = stages2020to2024_leaflet_alternate,
                        file = "~/file/path/stages2020to2024_alternate.html",
                        selfcontained = TRUE)

webshot::webshot("stages2020to2024_alternate.html", "stages2020to2024_alternate.png", vwidth = 2050, vheight = 1300) # check working directory first

# Read in images
img1 <- image_read("~/file/path/stages1950to1959.png")
img2 <- image_read("~/file/path/stages1960to1969.png")
img3 <- image_read("~/file/path/stages1970to1979.png")
img4 <- image_read("~/file/path/stages1980to1989.png")
img5 <- image_read("~/file/path/stages1990to1999.png")
img6 <- image_read("~/file/path/stages2000to2009.png")
img7 <- image_read("~/file/path/stages2010to2019.png")
# img8 <- image_read("~/file/path/stages2020to2024.png")
img8 <- image_read("~/file/path/stages2020to2024_alternate.png")

# Add borders to each image
img1 <- image_border(img1, color = "black", geometry = "1x1")
img2 <- image_border(img2, color = "black", geometry = "1x1")
img3 <- image_border(img3, color = "black", geometry = "1x1")
img4 <- image_border(img4, color = "black", geometry = "1x1")
img5 <- image_border(img5, color = "black", geometry = "1x1")
img6 <- image_border(img6, color = "black", geometry = "1x1")
img7 <- image_border(img7, color = "black", geometry = "1x1")
img8 <- image_border(img8, color = "black", geometry = "1x1")

# Create three side-by-side panels
panel1 <- image_append(c(img1, img2), stack = FALSE)
panel2 <- image_append(c(img3, img4), stack = FALSE)
panel3 <- image_append(c(img5, img6), stack = FALSE)
panel4 <- image_append(c(img7, img8), stack = FALSE)

panel_map <- image_append(c(panel1, panel2, panel3, panel4), stack = TRUE)
image_write(panel_map, "~/file/path/file_name_stages_panel_map.png")
