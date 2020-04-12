# ---------------------------------------------------------------------------------------
# legislatoR
# Sascha Göbel and Simon Munzert
# Script: preparations for spain
# October 2017
# ---------------------------------------------------------------------------------------


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list = ls(all = TRUE))

# set working directory -----------------------------------------------------------------
setwd("D:/Sascha/Projects/legislatoR")

# install and load packages and functions -----------------------------------------------
source("./code/packages.R")
source("./code/functions.R")


#### DATA PREPARATION ===================================================================

# join core data, adjust names, and split into core and political -----------------------
spain <- readRDS("./data/spain")
spain_title <- readRDS("./data/spain_title")
#spain_faces <- readRDS("./data/spain_faces")
spain_sex <- readRDS("./data/spain_sex")
spain_religion <- readRDS("./data/spain_religion")
spain_birth <- readRDS("./data/spain_birth")
spain_death <- readRDS("./data/spain_death")
spain_birthplace <- readRDS("./data/spain_birthplace")
spain_deathplace <-readRDS("./data/spain_deathplace")
spain <- left_join(x = spain, y = spain_title, by = "pageid_unique") %>%
  #left_join(x = ., y = spain_faces[,c(1,2)], by = "pageid") %>% # no ethnicity for spain
  left_join(x = ., y = spain_sex, by = "wikidataid") %>%
  left_join(x = ., y = spain_religion, by = "wikidataid") %>%
  left_join(x = ., y = spain_birth, by = "wikidataid") %>%
  left_join(x = ., y = spain_death, by = "wikidataid") %>%
  left_join(x = ., y = spain_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = spain_deathplace, by = "wikidataid")
colnames(spain)[c(27,31,32)] <- c("wikititle", "birth", "death")
spain <- spain %>% dplyr::select(country, pageid_unique, wikidataid, wikititle, name, sex, 
                                 religion, birth, death, birthplace, deathplace, 
                                 session, party, group, constituency, session_start, session_end, 
                                 service)
spain_core <- spain[!duplicated(spain$pageid_unique), 1:11]
colnames(spain_core)[2] <- "pageid"
spain_core$wikidataid <- ifelse(is.na(spain_core$wikidataid),
                                paste0(spain_core$pageid, "-wd"),
                                spain_core$wikidataid)
spain_political <- spain[c(2, 12:18)]
colnames(spain_political)[1] <- "pageid"
rm(spain, spain_title, spain_sex, spain_religion, spain_birth,
   spain_death, spain_birthplace, spain_deathplace)

# correct some name mismatches, pageids and wikidataids are correct
spain_core[match(c("Q15256012", "Q2748756","Q66663492","Q44409046",
                   "Q44630197", "Q64166732", "Q66663512", "Q11955329",
                   "Q12399503", "Q5997107", "Q66663449", "Q44630268",
                   "Q44409223", "Q44519245", "Q66663490", "Q448547",
                   "Q3189830"),spain_core$wikidataid),]$name <- 
  c("Francisco Ramos Fernández-Torrecilla", "Celestino Corbacho Chaves",
    "Germán Renau Martínez", "José Miguel González Moraga",
    "María Pía Sánchez Fernández", "Héctor Illueca Ballester", 
    "Yolanda Seva Ruiz", "Xavier Tárrega Bernal",
    "Salvador Fernández Moreda", "María Margarita Robles Fernández",
    "Andrés Lorite Lorite", "Mercedes Toledo Silvestre",
    "Pío Pérez Laserna", "María Soledad Sánchez Jódar",
    "Margarita Prohens Rigo", "Pedro Duque",
    "Julio de España Moya")
spain_core <- spain_core %>% filter(!(wikidataid %in% c("1-miss-wd", "20-miss-wd", "21-miss-wd", "4-miss-wd")))

# format traffic data -------------------------------------------------------------------
spain_traffic <- readRDS("./data/spain_traffic")
spain_traffic$date <- spain_traffic$date %>% as.POSIXct(tz = "UTC")

# format history data -------------------------------------------------------------------
spain_history <- readRDS("./data/spain_history")
spain_history <- spain_history %>% select(pageid = pageid_unique, revid, parentid, user, 
                                          userid, timestamp, size, comment)
spain_history$timestamp <- spain_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# format facial data --------------------------------------------------------------------
spain_faces <- readRDS("./data/spain_faces")
spain_faces <- spain_faces[,-1]

# save data -----------------------------------------------------------------------------
spain_social <- readRDS("./data/spain_social")
spain_positions <- readRDS("./data/spain_positions")
spain_occupation <- readRDS("./data/spain_occupation")
spain_id <- readRDS("./data/spain_id")
saveRDS(spain_core, "./package/legislatoR-data-v1.0.0/esp_core")
saveRDS(spain_political, "./package/legislatoR-data-v1.0.0/esp_political")
saveRDS(spain_history, "./package/legislatoR-data-v1.0.0/esp_history")
saveRDS(spain_traffic, "./package/legislatoR-data-v1.0.0/esp_traffic")
saveRDS(spain_social, "./package/legislatoR-data-v1.0.0/esp_social")
saveRDS(spain_faces, "./package/legislatoR-data-v1.0.0/esp_portrait")
saveRDS(spain_positions, "./package/legislatoR-data-v1.0.0/esp_office")
saveRDS(spain_occupation, "./package/legislatoR-data-v1.0.0/esp_profession")
saveRDS(spain_id, "./package/legislatoR-data-v1.0.0/esp_ids")


