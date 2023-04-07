# installs packages if needed
list_packages <- c("sf","cartography","here","jsonlite", "dplyr")
new_packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# loads packages
library(sf)
library(cartography)
library(jsonlite)
library(dplyr)

here::i_am('create_GEB_map.R')

# Load all countries data
all_countries <- st_read("https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/countries/geojson/CNTR_RG_20M_2016_3857.geojson",
                   stringsAsFactors = FALSE)
all_countries <- all_countries %>% filter(!(CNTR_NAME == "Antarctica")) # save space
all_countries <- st_transform(all_countries, 
                              "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# Add in ISO information
iso <- fromJSON("https://raw.githubusercontent.com/dieghernan/Country-Codes-and-International-Organizations/master/outputs/Countrycodesfull.json")

all_countries <- merge(all_countries,
                 iso,
                 by.x = "ISO3_CODE",
                 by.y = "ISO_3166_3",
                 all.x = TRUE)

# Load and prep event info for countries of interest (COI)
events <- read.csv(here::here("Raw_Data", "GEB_Data.csv"))

## create img_code column : generates image code
## create filename column : name of file to be used for fill
events <- events %>% rename(region = ï..REGION) %>%
          mutate(img_code = apply(events, 1, function(x)
               paste(colnames(events[-1])[x[2:length(x)] == "1"], collapse = ""))) %>%
          mutate(filename = (here::here("IMG", paste(img_code,".png", sep = "")))) %>%
          mutate(event = 1)

# Merge geography and event data
all_data <- merge(all_countries,
                       events,
                       by = "ISO3_CODE",
                       all.x = TRUE)

# COI subset
coi <- all_data %>% filter(event == 1)

# Function to make map
makeMap <- function(all, sub){
     # Saving as PNG, *******EDIT MAP NAME BELOW*******
     png(here::here("Map","YY-MM-DD_MAP.png"), width = 1000, height = 700,
         units = "px", pointsize = 12, bg = "white", res = NA,
         restoreConsole = TRUE)
     # Plotting all countries
     plot(st_geometry(all), col="gray62", bg = "white")
     plot(st_geometry(sub), bg="white", add=TRUE)
     
     # Grab PNG layers (this will take a minute)
     for (i in 1:nrow(sub)) {
          a = getPngLayer(sub[i,],
                          as.character(st_drop_geometry(sub[i,'filename'])))
          pngLayer(a, add = TRUE)
     }
     
     # Add borders
     plot(st_geometry(all), add = TRUE, col = NA, lwd=0.4)
     
     # Add a legend
     legend("bottom", inset=c(-0.3,0), title="Event Classification",
            c("Health", "Food Security, Shelter & Nutrition",
              "WASH", "Security", "Natural Disaster"), 
            fill=c("#cc0000", "#e69138","#38761d","#ff00ff","#9900ff"), 
            cex=1, ncol=2)
     dev.off()
}

makeMap(all_data, coi)
