# HEADER ------------------------------------------------------------------
#' PROGRAM NAME: SOURCE.R
#' PROJECTS: CALIFORNIA INTERNATIONAL MARATHON
#' DESCRIPTIONS: SOURCE PACKAGES AND LIBRARIES
#' 
#' 
#' PROGRAMMER: LEONARDO PALOMERA
#' DATE 12/27/2019
#' R VERSION:3.5.0
#' INPUT FILES 
#' OUTPUT FILES
#' SECTION

#' MEMORY
rm(list = ls())
gc(reset = TRUE)

#' OPTIONS
options(scipen=30)

# SOURCE & PACKAGE IMPORT -------------------------------------------------
#' INSTALL PACKAGES
#' install.packages("tidyr")
#' install.packages("dplyr")
#' install.packages("readr")
#' install.packages("ggplot2")
#' install.packages("stringr")
#' install.packages("purrr")
#' install.packages("googlesheets")
#' install.packages("lubeidate")
#' install.packages("ggmap")
#' install.packages("sf")
#' install.packages("mapview")
#' install.packages("reshape2")
#' install.packages("pwr")
#' install.packages("sqldf")
#' install.packages("shiny")

#' LOAD LIBRARIES
library(tidyr)
library(tibble)
library(forcats)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(purrr)
library(lubridate)
library(knitr)
library(readxl)
library(sqldf) #' SQL QUERIES
#' library(here)
#' OTHER LIBRARIES
#' library(bit64)
library(reshape2) #' PLEASE TRY AND USE GATHER AND SPREAD
library(stringi) #' STRING R
#library(pwr)
library(geosphere)
library(shiny)
