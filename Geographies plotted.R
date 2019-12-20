library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", "rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras'))

# capwords = function(s, strict = FALSE) {
#   cap = function(s) paste(toupper(substring(s, 1, 1)),
#                           {s = substring(s, 2); if(strict) tolower(s) else s},sep = "", collapse = " " )
#   sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))}

options(scipen = 999)

github_repo_dir <- "~/Documents/Repositories/primary-care-geographies"

PCN_data <- read_excel(paste0(github_repo_dir, "/GP_PCN_CCG l-up (002).xlsx"), skip = 1) %>% 
  select(`GP code`,`GP list`,`PCN`,`CCG`) %>% 
  filter(CCG %in% c('NHS Coastal West Sussex CCG', 'NHS Crawley CCG', 'NHS Horsham and Mid Sussex CCG'))

LSOA_PCN_data <- read_excel(paste0(github_repo_dir, "/LSOA mapping for CI 11.11.2019 (002).xlsx")) %>% 
  filter(PCN %in% PCN_data$PCN)

first_string <- paste0("LSOA11CD%20like%20'%25", LSOA_PCN_data$`LSOA code in CI`[1],"%25'%20OR%20")
last_string <- paste0("LSOA11CD%20like%20'%25", LSOA_PCN_data$`LSOA code in CI`[nrow(LSOA_PCN_data)],"%25'")

x <- 1;
z <- NULL; 
while (x <= nrow(LSOA_PCN_data)-2)  {  
  x <- x+1  
  z <- c(z,paste(paste0("LSOA11CD%20like%20'%25", LSOA_PCN_data$`LSOA code in CI`[x],"%25'%20OR%20"), collapse = "," ))
}

lsoa_string_for_api <- str_c(c(first_string, z, last_string), collapse = '')

rm(first_string, last_string,x,z)

Overview <- PCN_data %>% 
  group_by(PCN, CCG) %>% 
  summarise(`Number of practices` = n()) %>% 
  ungroup() %>% 
  mutate_all(as.character) 

Overview <- Overview %>% 
  add_column(Colours = c("#a04866","#d74356","#c4705e","#ca572a","#d49445","#526dd6","#37835c","#a2b068","#498a36","#a678e4","#8944b3","#57c39b","#4ab8d2","#658dce","#776e29","#60bf52","#7e5b9e","#afb136","#ce5cc6","#d58ec6","#d44b92"))

PCN_data <- PCN_data %>% 
  left_join(Overview[c('PCN', 'Colours')], by = 'PCN')

Overview_2 <- Overview %>% 
  group_by(CCG) %>% 
  summarise(`Number of Primary Care Networks` = n())

#gp locations

# Download the EPRACCUR file 
download.file('https://files.digital.nhs.uk/assets/ods/current/epraccur.zip', paste0(github_repo_dir,'/epraccur.zip'), mode = 'wb') 
unzip(paste0(github_repo_dir,'/epraccur.zip'), exdir = github_repo_dir) # unzip the folder into the directory

# Great but its got entirely capital letters for everything.
# We can use functions such as tolower() and toupper() but this works on the whole string and we probably want each word capitalised rather than all or nothing.
# Thankfully you can create a function (I did not come up with this myself, it was on the examples for chartr package)
capwords = function(s, strict = FALSE) {
  cap = function(s) paste(toupper(substring(s, 1, 1)),
                          {s = substring(s, 2); if(strict) tolower(s) else s},
                          sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))}

# We can tidy up the data frame
epraccur <- read_csv('/Users/richtyler/Documents/Repositories/primary-care-geographies/epraccur.csv', col_names = c("Code", "Name", "National Grouping", "Health Geography", "Address_1", "Address_2", "Address_3", "Address_4", "Address_5", "Postcode", "Open Date", "Close Date", "Status", "Organisation Sub-type", "Commissioner", "Join Provider date", "Left provider date", "Contact Tel.", "Null_1", "Null_2", "Null_3", "Amended Record Indicator", "Null_4", "Provider_purchaser", "Null_5", "Prescribing setting", "Null_6")) %>% 
  select(-c(`Open Date`, `National Grouping`, `Health Geography`, `Organisation Sub-type`, Null_1, Null_2, Null_3, Null_4, Null_5, Null_6, `Amended Record Indicator`, `Join Provider date`)) %>% 
  mutate(Name = capwords(Name, strict = TRUE),
         Address_1 = capwords(Address_1, strict = TRUE),
         Address_2 = capwords(Address_2, strict = TRUE),
         Address_3 = capwords(Address_3, strict = TRUE),
         Address_4 = capwords(Address_4, strict = TRUE),
         Address_5 = capwords(Address_5, strict = TRUE),
         Status = factor(ifelse(Status == 'A', 'Active', ifelse(Status == 'C', 'Closed', ifelse(Status == 'D', 'Dormant', ifelse(Status == 'P', 'Proposed', NA)))))) %>% 
  mutate(`Close Date` = as.character.Date(`Close Date`)) %>% 
  filter(Commissioner %in% c('09G','09H','09X')) %>% 
  filter(Status == 'Active') %>% 
  filter(`Prescribing setting` == 4)

PCN_data <- PCN_data %>% 
  left_join(epraccur[c('Code', 'Postcode')], by = c('GP code' = 'Code')) %>% 
  mutate(pcd_ns = tolower(gsub(' ', '', Postcode)))

# This is the postcode file from open geography portal for wsx 
postcodes_wsx <- unique(list.files("./GIS/Postcodes/multi")) %>% 
  map_df(~read_csv(paste0("./GIS/Postcodes/multi/",.))) %>% 
  select(pcd, oseast1m, osnrth1m, lat, long) %>% 
  mutate(pcd_ns = tolower(gsub(' ', '', pcd)))

PCN_data <- PCN_data %>% 
  left_join(postcodes_wsx, by = 'pcd_ns')

# Working with Open Geography Portal API to avoid needing to download data locally. 

# LAD_clipped <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD_APR_2019_UK_BFC/FeatureServer/0/query?where=%20(LAD19NM%20like%20'%25ADUR%25'%20OR%20LAD19NM%20like%20'%25ARUN%25'%20OR%20LAD19NM%20like%20'%25CHICHESTER%25'%20OR%20LAD19NM%20like%20'%25CRAWLEY%25'%20OR%20LAD19NM%20like%20'%25HORSHAM%25'%20OR%20LAD19NM%20like%20'%25MID%20SUSSEX%25'%20OR%20LAD19NM%20like%20'%25WORTHING%25')%20&outFields=LAD19CD,LAD19NM,LAD19NMW,BNG_E,BNG_N,LONG,LAT&outSR=4326&f=geojson")
# LAD_fe <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD_APR_2019_UK_BFE/FeatureServer/0/query?where=%20(LAD19NM%20like%20'%25ADUR%25'%20OR%20LAD19NM%20like%20'%25ARUN%25'%20OR%20LAD19NM%20like%20'%25CHICHESTER%25'%20OR%20LAD19NM%20like%20'%25CRAWLEY%25'%20OR%20LAD19NM%20like%20'%25HORSHAM%25'%20OR%20LAD19NM%20like%20'%25MID%20SUSSEX%25'%20OR%20LAD19NM%20like%20'%25WORTHING%25')%20&outFields=LAD19CD,LAD19NM,LAD19NMW,BNG_E,BNG_N,LONG,LAT&outSR=4326&f=geojson")

Chichester_clipped <- as(st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD_APR_2019_UK_BFC/FeatureServer/0/query?where=%20(LAD19NM%20like%20'%25CHICHESTER%25')%20&outFields=LAD19CD,LAD19NM,LAD19NMW,BNG_E,BNG_N,LONG,LAT&outSR=4326&f=geojson"), 'Spatial')
# convert to SpatialPolygonsDataFrame
# Chichester_clipped <- as(Chichester_clipped, "Spatial")
# convert to SpatialPolygons
# Chichester_clipped <- as(st_geometry(Chichester_clipped), "Spatial")

LAD_no_chi_fe <- as(st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD_APR_2019_UK_BFE/FeatureServer/0/query?where=%20(LAD19NM%20like%20'%25ADUR%25'%20OR%20LAD19NM%20like%20'%25ARUN%25'%20OR%20LAD19NM%20like%20'%25CRAWLEY%25'%20OR%20LAD19NM%20like%20'%25HORSHAM%25'%20OR%20LAD19NM%20like%20'%25MID%20SUSSEX%25'%20OR%20LAD19NM%20like%20'%25WORTHING%25')%20&outFields=LAD19CD,LAD19NM,LAD19NMW,BNG_E,BNG_N,LONG,LAT&outSR=4326&f=geojson"), 'Spatial')
# convert to SpatialPolygonsDataFrame
# LAD_no_chi_fe_spdf <- as(LAD_no_chi_fe, "Spatial")
# convert to SpatialPolygons
# LAD_no_chi_fe <- as(st_geometry(LAD_no_chi_fe), "Spatial")

LAD <- rbind(LAD_no_chi_fe, Chichester_clipped)
rm(Chichester_clipped, LAD_no_chi_fe)

LSOA_boundary_fe <- as(st_read(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_DEC_2011_EW_BFE/FeatureServer/0/query?where=%20(LSOA11NM%20like%20'%25ADUR%25'%20OR%20LSOA11NM%20like%20'%25ARUN%25'%20OR%20LSOA11NM%20like%20'%25CRAWLEY%25'%20OR%20LSOA11NM%20like%20'%25HORSHAM%25'%20OR%20LSOA11NM%20like%20'%25LEWES%25'%20OR%20LSOA11NM%20like%20'%25MID%20SUSSEX%25'%20OR%20LSOA11NM%20like%20'%25WORTHING%25')%20&outFields=LSOA11CD,LSOA11NM&outSR=4326&f=geojson")), 'Spatial')

LSOA_boundary_clipped <- as(st_read(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_DEC_2011_EW_BFC/FeatureServer/0/query?where=%20(LSOA11NM%20like%20'%25CHICHESTER%25')%20&outFields=LSOA11CD,LSOA11NM&outSR=4326&f=json")), 'Spatial')

LSOA_boundary <- rbind(LSOA_boundary_fe, LSOA_boundary_clipped)
rm(LSOA_boundary_fe, LSOA_boundary_clipped)

LSOA_boundary = subset(LSOA_boundary, LSOA_boundary@data$LSOA11CD %in% LSOA_PCN_data$`LSOA code in CI`)

# https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_DEC_2011_EW_BFC/FeatureServer/0/query?where=LSOA11CD%20like%20'%25E000%25'&outFields=LSOA11CD,LSOA11NM,Shape__Area,Shape__Length&outSR=4326&f=json

# To this LSOA shapefile we can attach the CCG and Locality data
LSOA_boundary = append_data(LSOA_boundary, LSOA_PCN_data, key.shp = "LSOA11CD", key.data = "LSOA code in CI")

# # For CCG
WSx_PCN = gUnaryUnion(LSOA_boundary, id = LSOA_boundary@data$PCN)

leaflet() %>%  
  addTiles(urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",attribution = "Crown copyright 2019.<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons. Click on an area or GP icon to find out more") %>% 
  addPolygons(data = WSx_PCN, 
              stroke = TRUE, 
              fillColor = Overview$Colours, 
              color = '#000000', 
              weight = 1, 
              opacity = 1,
              fillOpacity = .7,
              group = "Show PCN boundaries") %>% 
  addPolygons(data = LAD,
              stroke = TRUE,
              opacity = 1,
              fill = 0,
              weight = 3,
              label = LAD@data$LAD19NM,
              group = "Show LA district & borough boundaries") %>%
  addCircleMarkers(lng = PCN_data$long,
                   lat = PCN_data$lat,
                   color = '#000000',
                   stroke = TRUE,
                   weight = 1,
                   opacity = 1,
                   fillColor = PCN_data$Colours, 
                   label = PCN_data$PCN,
                   fillOpacity = 1,
                   radius = 6,
                   group = "Show GP practices by PCN") %>%
  # addSearchFeatures(targetGroups = 'Show GP practices by PCN',
  #                   options = searchFeaturesOptions(zoom = 12,
  #                                                   autoType = TRUE,
  #                                                   textPlaceholder = "Search Practice name",
  #                                                   textErr = "Practice not found",
  #                                                   collapsed = FALSE,
  #                                                   position = "topright",
  #                                                   openPopup = TRUE,
  #                                                   firstTipSubmit = TRUE,
  #                                                   autoCollapse = FALSE,
  #                                                   hideMarkerOnCollapse = FALSE)) %>%
  addResetMapButton() %>%
  # addProviderTiles(providers$Stamen.TonerLabels) %>%
  addScaleBar(position = "bottomleft")# %>% 
# addPulseMarkers(lng = WSx_GPs$long, 
#                  lat = WSx_GPs$lat,
#                  icon = makePulseIcon(heartbeat = 1,
#                                       animate = T,  
#                                       color = WSx_GPs$Colours),
#                  popup = paste0("<strong>", WSx_GPs$Practice_label, "</strong>"),
#                  group = "Show GP practices by PCN") 


map_theme = function(){
  theme( 
    legend.position = "none", 
    plot.background = element_blank(), 
    panel.background = element_blank(),  
    panel.border = element_blank(),
    axis.text = element_blank(), 
    plot.title = element_text(colour = "#000000", face = "bold", size = 12), 
    axis.title = element_blank(),     
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    strip.text = element_text(colour = "white"), 
    strip.background = element_rect(fill = "#327d9c"), 
    axis.ticks = element_blank() 
  ) 
} 

# We have to reproject LSOA11_bounday to match the projection coordinate reference system (CRS) for ggplot like the other two boundary files
# CRS("+init=epsg:4326") is WGS84
WSx_PCN_2 <- spTransform(WSx_PCN, CRS("+init=epsg:4326"))
LAD_2 <- spTransform(LAD, CRS("+init=epsg:4326"))

WSx_PCN_3 <- SpatialPolygonsDataFrame(WSx_PCN, data = data.frame(Overview), match.ID = F) 

View(WSx_PCN_2)
rm(coords)

#  This will be a good bounding box for any of our maps.
my_bbox <- matrix(data= c(-1.0518, 50.7080, 0.144380, 51.2047), nrow = 2, ncol = 2, dimnames = list(c("Latitude", "Longitude"), c("min", "max")))

ggplot() +
  coord_fixed(1.5) + 
  map_theme() +
  # scale_y_continuous(limits = c(my_bbox[2] - 0.000100, my_bbox[4] + 0.000100)) + 
  # scale_x_continuous(limits = c(my_bbox[1] - 0.00500, my_bbox[3] + 0.00500)) +
  geom_polygon(data = WSx_PCN_3, aes(x=long, y=lat, group = group, fill = group), color="#000000", size = .5, alpha = .7, show.legend = FALSE) +
  # scale_fill_manual(values = Overview$Colours) +
  geom_point(data = PCN_data, aes(x = long, y = lat, fill = PCN), shape = 21, colour = '#000000', size = 3, alpha = 1) +
  geom_polygon(data = LAD, aes(x=long, y=lat, group = group), color="#3D2EFF", fill = NA, size = 1, show.legend = FALSE) +
  scale_colour_manual(values = PCN_data$Colours) +
  theme(legend.position = "none",
        legend.title = element_text(size = 9, face = "bold"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm")) 
