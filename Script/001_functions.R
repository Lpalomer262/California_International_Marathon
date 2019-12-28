# HEADER ------------------------------------------------------------------
#' PROGRAM NAME: GENERAL FUNCTION
#' PROJECTS: LEONARDO'S RUNNING FORMULA
#' DESCRIPTIONS: 
#' CREATE A DIRECTORY FOR FUNCTION ONLY
#' 
#' 
#' 
#' PROGRAMMER: LEONARDO PALOEMRA
#' DATE: NOVEMBER 1, 2018
#' R VERSION: 3.5.0
#' INPUT FILES: 
#' TOP MARATHON TIMES
#' SUB 2:10:00 FINISHES
#' OUTPUT FILES: NON
#' SECTION




#' # STAT LINE VISULIZATION --------------------------------------------------
#' StatMeanLine <- ggproto("StatMeanLine", Stat,
#'                         compute_group = function(data, scales) {
#'                           transform(data, yintercept=mean(y))
#'                         },
#'                         required_aes = c("x", "y")
#' )
#' 
#' stat_mean_line <- function(mapping = NULL, data = NULL, geom = "hline",
#'                            position = "identity", na.rm = FALSE, show.legend = NA, 
#'                            inherit.aes = TRUE, ...) {
#'   layer(
#'     stat = StatMeanLine, data = data, mapping = mapping, geom = geom, 
#'     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#'     params = list(na.rm = na.rm, ...)
#'   )
#' }



# LOAD GPX ----------------------------------------------------------------

Sys.setenv(TZ = "America/Los_Angeles")

process_gpx <- function(directory){
  #' READS HTML FILES AS LINES
  #' HTML  MUST BE SAVE FROM GARMIN CONNECT =
  xx <-  readLines(con = directory) %>% 
    as_tibble() %>%
    #' ADD FLAGS TO CLASSIFY TIME, ELEVATION, LATITURE & LONGTITUDE ... ETC
    mutate(
      key = str_remove_all(directory, "../Input/raw/gxp/|.gpx"),
      data_type = case_when(
        str_detect(value, "lat\\=|lon\\=") ~ "coordinates_dirty",
        str_detect(value, "\\<time\\>") ~ "time_dirty", 
        str_detect(value, "\\<ele\\>") ~ "elevation_dirty",
        str_detect(value, "\\<time\\>") ~ "date_time_dirty",
        str_detect(value, "\\<ns3\\:hr\\>") ~ "hr_dirty",
        str_detect(value, "\\<ns3\\:cad\\>") ~ "cadence_dirty",
        str_detect(value, "\\<trkseg\\>") ~ "begin"
      ),
      remove_flag = ifelse(row_number() > 16, TRUE, FALSE)) %>%
    #' FILTER OUT EMPTY SETS OF DATA
    filter(!is.na(data_type), remove_flag == TRUE) %>%
    select(-remove_flag) %>%
    #' ADD ROW NUMBER TO EVERY UNIQUE TIME INSTAaNCE
    mutate(rank = cumsum(if_else(data_type == "coordinates_dirty", 1, 0))) %>%
    #' SPREAD DATA WIDE
    spread(data_type, value) 
  
  #' CHECK IF ALL FIELD: CADENCE, LON, LAT, ELEVATION, HEART RATE, DATE, TIME,
  #' YOU'LL HAVE TO ASSIGN THEM NA
  
  
  #' if(!any(names(xx) == "hr_rate")){
  #'  xx <- xx %>% mutate(hr_dirty= NA)
  #' } 
  
  
  xxx <- xx %>%
    #' DATA CLEANING
    mutate(cadence = as.numeric(str_extract(cadence_dirty, "[0-9]{2,}")),
           cadence = if_else(is.na(cadence), 0, cadence),
           #' ANY MISSING CADENCE REPLACE WITH ZERO
           lon = str_extract(coordinates_dirty, "lon=...[0-9]{2,3}\\.[0-9]{2,}"),
           lon = as.numeric(str_replace(lon, "lon..", "")),
           lat = str_extract(coordinates_dirty, "lat...[0-9]{1,3}\\.[0-9]{2,}"),
           lat = as.numeric(str_replace(lat, "lat..", "")),
           elevation = str_extract(elevation_dirty, "[0-9]{1,4}"),
           elevation = as.numeric(elevation),
           elevation_ft = as.numeric(elevation * 3.281),
           #' IT MIGHT BE BENEFICAL TO GRAB THE DECIMAL POINTS IN ELEVATION
           hrt_rate = str_extract(hr_dirty, "[0-9]{2,3}"),
           hrt_rate = as.numeric(hrt_rate),
           #'DOUBLE CHECK TIME DURING SINGLE DIGIT MONTHS
           date_time = ymd_hms(time_dirty),
           date = date(date_time),
           time = hms(paste(hour(date_time), minute(date_time), second(date_time), sep = "-"))
    ) %>% 
    #' ESTIMATE DISTANCE RAN USING EUCLIDIAN DISANCE BETWEEN TO COORDINATE POINTS
    mutate(
      lead_lon = lead(lon),
      lead_lon = if_else(is.na(lead_lon), lon, lead_lon),
      lead_lat = lead(lat),
      lead_lat = if_else(is.na(lead_lat), lat, lead_lat),
      lead_elevation = lead(elevation),
      lead_elevation = if_else(is.na(lead_elevation), elevation, lead_elevation),
      lead_dt_tm = lead(date_time),
      lead_dt_tm = if_else(is.na(lead_dt_tm), date_time, lead_dt_tm)
    ) %>%
    rowwise() %>%
    mutate(distance_meters = distm(x = c(lon, lat),
                                   y = c(lead_lon, lead_lat)),
           #' CONVERT METERS TO MILES
           distance_miles = distance_meters / 1609,
           #' ESTIMATE THE ELEVATION DIFFERENCES THROUGH THE COURSE
           #' PURPOSE, CALCULATE SEGMENTED EVEL
           elevation_diff = lead_elevation - elevation,
           elevation_diff_ft = as.numeric(elevation_diff * 3.281),
           #' CALCULATE THE NUMBER OF SECONDS BETWEEN ACTION
           tm_sec = as.numeric(lead_dt_tm - date_time)) %>%
    ungroup() %>%
    #' ESTIMATE DISTANCES AND TIME TRAVELED THROUGH SEGMENTS
    group_by(key) %>%
    mutate(
      mile = ceiling(cumsum(distance_miles)),
      km = ceiling(cumsum(distance_meters) / 1000),
      km_5 = case_when(
        km <=  5 ~ 5,
        km <= 10 ~ 10,
        km <= 15 ~ 15,
        km <= 20 ~ 20,
        km <= 25 ~ 25,
        km <= 30 ~ 30,
        km <= 35 ~ 35,
        km <= 40 ~ 40,
        #' WE'D LIKE TO SEE A 5K ASSESMENT, 
        #' NOT THE OVERALL PICTURE
        km >  40 ~ NA_real_), 
      km_5 = factor(km_5, levels = c(seq(0,40, 5))),
      half = case_when(
        cumsum(distance_meters) / 1000 <= 21.1 ~ 13.1,
        cumsum(distance_meters) / 1000 > 21.1 ~ 26.2,
        TRUE ~ NA_real_
      ),
      half = factor(half,levels = c(13.1, 26.2))
      ) %>%
    ungroup()  %>%
    select(-matches("_dirty"))
  
  #' ADDITIONAL DATA CLEANING TO ACCOUNT FOR GXP TIME DIFFERENCES
  xxx <- xxx %>%
    mutate(date_time = date_time - hours(8),
           time = time - hours(8))
  
  return(xxx)
  
} #' ENDS PROCESS GPX FILES


# PACE CALCULATOR ---------------------------------------------------------
pace_calculator <- 
  function(data = NA, distance, metric = c("km", "mi"), seconds){ 
    #' CALCULATE MILE & KM PACE AS WELL AS MPH & KMPH
    
    #' TESTTHAT
    
    #' OBJECT ORIANTED PROGRAMMING
    #' IF IT DATA, THEN YOU SHOULD BE USING FIELDS
    #' IF ITS VECTORS, YOU SHOULD NOT CARE THAT A DATA FRAME WASN'T USED
    
    #' EXPORT SHOULD ONLY INCLUDE THOSE FEILDS WANTED 
    
    x <- data %>% as_tibble()
    distance <- x %>% pull(distance)
    seconds <- x %>% pull(seconds)
    
    stopifnot(is.numeric(distance))
    stopifnot(is.numeric(seconds))
    stopifnot(metric %in% c("km", "mi"))
    
    if(metric == "km"){
      std_km <-  seconds / distance #' 1 KM PER X SECONDS
      std_mi <- std_km / (0.62137119)
      mp_pace <- mp_pace <- suppressWarnings(
        as.numeric(paste(trunc(std_mi / 60),
                         ifelse(std_mi %% 60 < 10, 
                                paste0(0, round(std_mi %% 60)), 
                                as.character(round(std_mi %% 60))), sep = ".")))
      
      mph <- 3600 / std_mi
      km_pace <-  suppressWarnings(
        as.numeric(paste(trunc(std_km / 60),
                         ifelse(std_km %% 60 < 10, 
                                paste0(0, round(std_km %% 60)), 
                                as.character(round(std_km %% 60))), sep = ".")))
      kph <- 3600 / std_km
      
      x <- x %>% 
        mutate(mi_pace = mp_pace, mph = mph, km_pace = km_pace, kph = kph)
    } else {
      std_mi<-  seconds / distance #' 1 MI PER X SECONDS
      std_km <- std_mi / (1 / (0.62137119))
      mp_pace <- mp_pace <- suppressWarnings(
        as.numeric(paste(trunc(std_mi / 60),
                         ifelse(std_mi %% 60 < 10, 
                                paste0(0, round(std_mi %% 60)), 
                                as.character(round(std_mi %% 60))), sep = ".")))
      mph <- 3600 / std_mi
      km_pace <-  suppressWarnings(
        as.numeric(paste(trunc(std_km / 60),
                         ifelse(std_km %% 60 < 10, 
                                paste0(0, round(std_km %% 60)), 
                                as.character(round(std_km %% 60))), sep = ".")))
      kph <- 3600 / std_km
      
      x <- x %>% 
        mutate(mi_pace = mp_pace, mph = mph, km_pace = km_pace, kph = kph)
    }
    
    return(x)
  } #' END PACE_CALCULATOR



# VISULIZATION FUNCTIONS --------------------------------------------------
pretty_colors <- 
  #' GENERAL COLOR SCHEMES
  c("#778899", "#C90E17", "#001933", "#08519c", "#6495ED","#B0C4DE", 
    "#999999", "#000000", "#800000", "#B23232", "#691b14")


