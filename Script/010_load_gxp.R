# HEADER ------------------------------------------------------------------
#' PROGRAM NAME: SOURCE.R
#' PROJECTS: CALIFORNIA INTERNATIONAL MARATHON
#' DESCRIPTIONS: LOAD GXP FILES
#' 
#' 
#' PROGRAMMER: LEONARDO PALOMERA
#' DATE 12/27/2019
#' R VERSION: 3.5.0
#' INPUT FILES 
#' OUTPUT FILES
#' SECTION

source("./001_functions.R")

runners_gpx <- list.files("../Input/raw/gxp", full.names = TRUE) %>% 
  as_tibble() %>%
  rename(directory = value) %>%
  mutate(runners = str_remove_all(directory, "../Input/raw/gxp/|.gpx"))

lee <- process_gpx("../Input/raw/gxp/lee.gpx") 
  
palomera <- process_gpx("../Input/raw/gxp/palomera.gpx")

cim <- bind_rows(lee, palomera)

#' NET NUMBER OF ELEVATION GAIN

cim %>% group_by(key) %>%
  summarise_at(c("tm_sec", "distance_miles", "distance_meters"), sum) %>% 
  ungroup() %>%
  mutate(test = seconds_to_period(tm_sec)) %>% kable()

#' MILES SPLITS
#' PER MILE OR KILOMETER SIMPLE STATISTICS
#' (EVELAVATION) EVELVATION SEEMS TO BE DIFFERENT SO INDIVIDALS 
#' WHO RAN THE EXACT SAME COURSE 
#' I'LL EVENTAULLY JUST USE THE NET GAIN AVERAGE FROM EVERYONE


#' FUNCTIONALIZE A SUMMARY TABLE BY THE WANTED DISTANCE

split_summary <- 
  function(
    #' DROP DOWN CORRESPONDING TO ATHLETE
    #' PALOMERA, LEE, PROFFITT, CHU, MARKS, BOYLE,
    split = c("Mile", "KM", "5K", "Half")){
    
    #' STOP IF NOT STATEMENT
    
    if(split == "Mile"){
      xx <- "mile"
    } else if (split == "KM") {
      xx <- "km"
    } else if (split == "5K") {
      xx <- "km_5"
    } else if (split == "Half") {
      xx <- "half"
    } else {
      print("ERROR, CONTACT LEONARDO")
    }
    
    xxx <- cim %>%
      group_by_("key", xx) %>%
      summarise( 
        #' TOTAL / SUMS
        tm_sec = sum(tm_sec, na.rm = TRUE),
        distance_meters = sum(distance_meters, na.rm = TRUE),
        distance_miles = sum(distance_miles, na.rm = TRUE),
        net_elevation = sum(elevation_diff, na.rm = TRUE),
        #' AVERAGES
        cadence = mean(cadence, na.rm = TRUE),
        hrt_rate = mean(hrt_rate, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(net_elevation_ft = net_elevation * 3.281) 
    
    return(xxx)
    
  }


mile_splits <- cim %>%
  group_by(key, mile) %>%
  summarise( 
    #' TOTAL / SUMS
    tm_sec = sum(tm_sec, na.rm = TRUE),
    distance_meters = sum(distance_meters, na.rm = TRUE),
    distance_miles = sum(distance_miles, na.rm = TRUE),
    net_elevation = sum(elevation_diff, na.rm = TRUE),
    #' AVERAGES
    cadence = mean(cadence, na.rm = TRUE),
    hrt_rate = mean(hrt_rate, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(net_elevation_ft = net_elevation * 3.281) 
    
