# ---------------------------------------------------------------------------------------
# legislatoR
# Sascha Göbel and Simon Munzert
# Script: data
# December 2017
# ---------------------------------------------------------------------------------------


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list = ls(all = TRUE))

# set working directory -----------------------------------------------------------------
setwd("D:/Sascha/Projects/legislatoR")

# install and load packages and functions -----------------------------------------------
source("./code/packages.R")
source("./code/functions.R")


#### DATA COLLECTION ====================================================================

# retrieve core data --------------------------------------------------------------------

# austrian nationalrat
austria_core <- wikiCore(country = "austria", name = TRUE, party = TRUE,
                         constituency = TRUE, term = TRUE, service = TRUE)
saveRDS(austria_core, "./data/austria_core")

# french assemble
france_core <- wikiCore(country = "france", name = TRUE, party = TRUE,
                        constituency = TRUE, term = TRUE, service = FALSE)
saveRDS(france_core, "./data/france_core")

# german bundestag
germany_core <- wikiCore(country = "germany", name = TRUE, party = TRUE,
                         constituency = TRUE, term = TRUE, service = TRUE)
saveRDS(germany_core, "./data/germany_core")

# irish dail
ireland_core <- wikiCore(country = "ireland", name = TRUE, party = TRUE,
                         constituency = TRUE, term = TRUE, service = FALSE)
saveRDS(ireland_core, "./data/ireland_core")

# united states house
usah_core <- wikiCore(country = "usa", chamber = "house", name = TRUE, party = TRUE,
                      constituency = TRUE, term = TRUE, service = TRUE)
saveRDS(usah_core, "./data/usah_core")

# united states senate
usas_core <- wikiCore(country = "usa", chamber = "senate", name = TRUE, party = TRUE,
                      constituency = TRUE, term = TRUE, service = TRUE)
saveRDS(usas_core, "./data/usas_core")

# retrieve wikipedia page and wikidata ids ----------------------------------------------

# austrian nationalrat
austria_ids <- lapply(X = austria_core, FUN = wikiIDs, corp = TRUE)
saveRDS(austria_ids, "./data/austria_ids")

# french assemble
france_ids <- lapply(X = france_core, FUN = wikiIDs, corp = TRUE)
saveRDS(france_ids, "./data/france_ids")

# german bundestag
germany_ids <- lapply(X = germany_core, FUN = wikiIDs, corp = TRUE)
saveRDS(germany_ids, "./data/germany_ids")

# irish dail
ireland_ids <- lapply(X = ireland_core, FUN = wikiIDs, corp = TRUE)
saveRDS(ireland_ids, "./data/ireland_ids")

# united states house
usah_ids <- lapply(X = usah_core, FUN = wikiIDs, corp = TRUE)
saveRDS(usah_ids, "./data/usah_ids")

# united states senate
usas_ids <- lapply(X = usas_core, FUN = wikiIDs, corp = TRUE)
saveRDS(usas_ids, "./data/usas_ids")

# bind ids to core data and collapse ----------------------------------------------------

# austrian nationalrat
austria <- bind_rows(Map(cbind, austria_ids, austria_core))
saveRDS(austria, "./data/austria")

# french assemble
france <- bind_rows(Map(cbind, france_ids, france_core))
saveRDS(france, "./data/france")

# german bundestag
germany <- bind_rows(Map(cbind, germany_ids, germany_core))
saveRDS(germany, "./data/germany")

# irish dail
ireland <- bind_rows(Map(cbind, ireland_ids, ireland_core))
saveRDS(ireland, "./data/ireland")

# united states house
usah <- bind_rows(Map(cbind, usah_ids, usah_core))
saveRDS(usah, "./data/usah")

# united states senate
usas <- bind_rows(Map(cbind, usas_ids, usas_core))
saveRDS(usas, "./data/usas")

# retrieve wikipedia revision histories -------------------------------------------------

# austrian nationalrat
austria_history <- bind_rows(lapply(X = unique(austria$pageid), FUN = wikiHist,
                                    project = "de.wikipedia"))
saveRDS(austria_history, "./data/austria_history")

# french assemble
france_history <- bind_rows(lapply(X = unique(france$pageid), FUN = wikiHist,
                                   project = "fr.wikipedia"))
saveRDS(france_history, "./data/france_history")

# german bundestag
germany_history <- bind_rows(lapply(X = unique(germany$pageid), FUN = wikiHist,
                                    project = "de.wikipedia"))
saveRDS(germany_history, "./data/germany_history")

# irish dail
ireland_history <- bind_rows(lapply(X = unique(ireland$pageid), FUN = wikiHist,
                                    project = "en.wikipedia"))
saveRDS(ireland_history, "./data/ireland_history")

# united states house
usah_history <- bind_rows(lapply(X = unique(usah$pageid), FUN = wikiHist,
                                 project = "en.wikipedia"))
saveRDS(usah_history, "./data/usah_history")

# united states senate
usas_history <- bind_rows(lapply(X = unique(usas$pageid), FUN = wikiHist,
                                 project = "en.wikipedia"))
saveRDS(usas_history, "./data/usas_history")

# retrieve undirected wikipedia urls/titles ---------------------------------------------

# austrian nationalrat
austria_title <- undirectedTitle(pageid = unique(austria$pageid),
                                 project = "de.wikipedia")
saveRDS(austria_title, "./data/austria_title")

# french assemble
france_title <- undirectedTitle(pageid = unique(france$pageid),
                                project = "fr.wikipedia")
saveRDS(france_title, "./data/france_title")

# german bundestag
germany_title <- undirectedTitle(pageid = unique(germany$pageid),
                                 project = "de.wikipedia")
saveRDS(germany_title, "./data/germany_title")

# irish dail
ireland_title <- undirectedTitle(pageid = unique(ireland$pageid),
                                 project = "en.wikipedia")
saveRDS(ireland_title, "./data/ireland_title")

# united states house
usah_title <- undirectedTitle(pageid = unique(usah$pageid), project = "en.wikipedia")
saveRDS(usah_title, "./data/usah_title")

# united states senate
usas_title <- undirectedTitle(pageid = unique(usas$pageid), project = "en.wikipedia")
saveRDS(usas_title, "./data/usas_title")

# retrieve wikipedia user traffic -------------------------------------------------------

# austrian nationalrat
austria_traffic <- wikiTraffic(data = austria_title, project = "de.wikipedia")
saveRDS(austria_traffic, "./data/austria_traffic")

# french assemble
france_traffic <- wikiTraffic(data = france_title, project = "fr.wikipedia")
saveRDS(france_traffic, "./data/france_traffic")

# german bundestag
germany_traffic <- wikiTraffic(data = germany_title, project = "de.wikipedia")
saveRDS(germany_traffic, "./data/germany_traffic")

# irish dail
ireland_traffic <- wikiTraffic(data = ireland_title, project = "en.wikipedia")
saveRDS(ireland_traffic, "./data/ireland_traffic")

# united states house
usah_traffic <- wikiTraffic(data = usah_title, project = "en.wikipedia")
saveRDS(usah_traffic, "./data/usah_traffic")

# united states senate
usas_traffic <- wikiTraffic(data = usas_title, project = "en.wikipedia")
saveRDS(usas_traffic, "./data/usas_traffic")

# retrieve sex --------------------------------------------------------------------------

# austrian nationalrat
austria_sex <- wikiData(item = unique(austria$wikidataid), property = "P21")
austria_sex$sex <- ifelse(austria_sex$männlich == TRUE, "male",
                          ifelse(austria_sex$weiblich == TRUE, "female", NA))
austria_sex <- austria_sex[,-c(2,3)]
austria_sex$sex <- factor(austria_sex$sex)
saveRDS(austria_sex, "./data/austria_sex")

# french assemble
france_sex <- wikiData(item = unique(france$wikidataid), property = "P21")
france_sex$sex <- ifelse(france_sex$male == TRUE, "male",
                         ifelse(france_sex$female == TRUE, "female", NA))
france_sex <- france_sex[,-c(2,3)]
france_sex$sex <- factor(france_sex$sex)
saveRDS(france_sex, "./data/france_sex")

# german bundestag
germany_sex <- wikiData(item = unique(germany$wikidataid), property = "P21")
germany_sex$sex <- ifelse(germany_sex$male == TRUE, "male",
                          ifelse(germany_sex$female == TRUE, "female", NA))
germany_sex <- germany_sex[,-c(2,3)]
germany_sex$sex <- factor(germany_sex$sex)
saveRDS(germany_sex, "./data/germany_sex")

# irish dail
ireland_sex <- wikiData(item = unique(ireland$wikidataid), property = "P21")
ireland_sex$sex <- ifelse(ireland_sex$male == TRUE, "male",
                          ifelse(ireland_sex$female == TRUE, "female", NA))
ireland_sex <- ireland_sex[,-c(2,3)]
ireland_sex$sex <- factor(ireland_sex$sex)
saveRDS(ireland_sex, "./data/ireland_sex")

# united states house
usah_sex <- wikiData(item = unique(usah$wikidataid), property = "P21")
usah_sex$sex <- ifelse(usah_sex$male == TRUE, "male",
                       ifelse(usah_sex$female == TRUE, "female", NA))
usah_sex <- usah_sex[,-c(2,3)]
usah_sex$sex <- factor(usah_sex$sex)
saveRDS(usah_sex, "./data/usah_sex")

# united states senate
usas_sex <- wikiData(item = unique(usas$wikidataid), property = "P21")
usas_sex$sex <- ifelse(usas_sex$male == TRUE, "male",
                          ifelse(usas_sex$female == TRUE, "female", NA))
usas_sex <- usas_sex[,-c(2,3)]
usas_sex$sex <- factor(usas_sex$sex)
saveRDS(usas_sex, "./data/usas_sex")

# retrieve religion ---------------------------------------------------------------------

# austrian nationalrat
aentity <- get_item(id = unique(austria$wikidataid))
austria_religion <- wikiData(item = unique(austria$wikidataid), entity = aentity,
                            property = "P140")
austria_religion$religion <- replace(NA, rowSums(austria_religion[,c(3, 5, 7)]) >= 1,
                                    "catholicism") %>%
  replace(., rowSums(austria_religion[,c(4)]) >= 1,
          "agnosticism") %>%
  replace(., rowSums(austria_religion[,c(6)]) >= 1,
          "atheism")
austria_religion <- austria_religion[,c(1, 8)] %>%
  filter(!is.na(religion))
austria_religion$religion <- factor(austria_religion$religion)
saveRDS(austria_religion, "./data/austria_religion")

# french assemble
frentity <- get_item(id = unique(france$wikidataid))
france_religion <- wikiData(item = unique(france$wikidataid), entity = frentity,
                            property = "P140")
france_religion$religion <- replace(NA, rowSums(france_religion[,c(2, 6, 7, 9)]) >= 1,
                                    "catholicism") %>%
  replace(., rowSums(france_religion[,c(4, 5)]) >= 1,
          "islam") %>%
  replace(., rowSums(france_religion[,c(3)]) >= 1,
          "agnosticism") %>%
  replace(., rowSums(france_religion[,c(8)]) >= 1,
        "judaism")
france_religion <- france_religion[,c(1, 10)]
france_religion$religion <- factor(france_religion$religion)
saveRDS(france_religion, "./data/france_religion")

# germany bundestag
gentity <- get_item(id = unique(germany$wikidataid))
germany_religion <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                            property = "P140")
germany_religion$religion <- replace(NA, rowSums(germany_religion[,c(7, 9, 14)]) >= 1,
                                    "catholicism") %>%
  replace(., rowSums(germany_religion[,c(2, 13)]) >= 1,
          "protestantism reformed") %>%
  replace(., rowSums(germany_religion[,c(5, 15)]) >= 1,
          "protestantism lutheran") %>%
  replace(., rowSums(germany_religion[,c(8, 11)]) >= 1,
          "protestantism evangelical") %>%
  replace(., rowSums(germany_religion[,c(10)]) >= 1,
          "protestantism") %>%
  replace(., rowSums(germany_religion[,c(12)]) >= 1,
          "islam") %>%
  replace(., rowSums(germany_religion[,c(6)]) >= 1,
          "agnosticism") %>%
  replace(., rowSums(germany_religion[,c(4)]) >= 1,
          "buddhism") %>%
  replace(., rowSums(germany_religion[,c(3)]) >= 1,
          "atheism")
germany_religion <- germany_religion[,c(1, 16)] %>%
  filter(!is.na(religion))
germany_religion$religion <- factor(germany_religion$religion)
saveRDS(germany_religion, "./data/germany_religion")

# irish dail
irentity <- get_item(id = unique(ireland$wikidataid))
ireland_religion <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                             property = "P140")
ireland_religion$religion <- replace(NA, rowSums(ireland_religion[,c(3, 7)]) >= 1,
                                     "catholicism") %>%
  replace(., rowSums(ireland_religion[,c(4)]) >= 1,
          "islam") %>%
  replace(., rowSums(ireland_religion[,c(2, 5)]) >= 1,
          "protestant presbyterian") %>%
  replace(., rowSums(ireland_religion[,c(6)]) >= 1,
          "anglicanism")
ireland_religion <- ireland_religion[,c(1, 8)]
ireland_religion$religion <- factor(ireland_religion$religion)
saveRDS(ireland_religion, "./data/ireland_religion")

# united states house
uhentity <- get_item(id = unique(usah$wikidataid))
usah_religion <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                             property = "P140")
usah_religion$religion <- replace(NA, rowSums(usah_religion[,c(9, 30, 39, 54)]) >= 1,
                                     "catholicism") %>%
  replace(., rowSums(usah_religion[,c(3, 5, 11, 13, 28, 29, 31, 58)]) >= 1,
          "protestantism evangelical") %>%
  replace(., rowSums(usah_religion[,c(4)]) >= 1,
          "protestantism proto") %>%
  replace(., rowSums(usah_religion[,c(6)]) >= 1,
          "protestantism quaker") %>%
  replace(., rowSums(usah_religion[,c(7)]) >= 1,
          "protestantism anabaptism") %>%
  replace(., rowSums(usah_religion[,c(42,44, 48)]) >= 1,
          "protestantism lutheran") %>%
  replace(., rowSums(usah_religion[,c(53, 59)]) >= 1,
          "protestantism baptist") %>%
  replace(., rowSums(usah_religion[,c(10, 15, 49)]) >= 1,
          "orthodox eastern") %>%
  replace(., rowSums(usah_religion[,c(19)]) >= 1,
          "orthodox") %>%
  replace(., rowSums(usah_religion[,c(8, 12, 20, 21, 26, 32, 34, 35, 50, 55)]) >= 1,
          "protestantism reformed") %>%
  replace(., rowSums(usah_religion[,c(17, 18, 22, 23, 57)]) >= 1,
          "protestantism methodist") %>%
  replace(., rowSums(usah_religion[,c(14, 33, 40)]) >= 1,
          "protestantism") %>%
  replace(., rowSums(usah_religion[,c(16, 38, 41)]) >= 1,
          "anglicanism") %>%
  replace(., rowSums(usah_religion[,c(24, 25, 37, 45, 46, 51)]) >= 1,
          "nontrinitarianism") %>%
  replace(., rowSums(usah_religion[,c(2, 56)]) >= 1,
          "hindu") %>%
  replace(., rowSums(usah_religion[,c(27)]) >= 1,
          "islam") %>%
  replace(., rowSums(usah_religion[,c(36, 47)]) >= 1,
          "buddhism") %>%
  replace(., rowSums(usah_religion[,c(43)]) >= 1,
          "atheism") %>%
  replace(., rowSums(usah_religion[,c(52)]) >= 1,
          "judaism")
usah_religion <- usah_religion[,c(1, 60)]
usah_religion$religion <- factor(usah_religion$religion)
saveRDS(usah_religion, "./data/usah_religion")

# united states senate
usentity <- get_item(id = unique(usas$wikidataid))
usas_religion <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                          property = "P140")
usas_religion$religion <-  replace(NA, rowSums(usas_religion[,c(5,34,20)]) >= 1,
                                   "catholicism") %>%
  replace(., rowSums(usas_religion[,c(6, 18, 37, 40, 41)]) >= 1,
          "protestantism evangelical") %>%
  replace(., rowSums(usas_religion[,c(3)]) >= 1,
          "protestantism quaker") %>%
  replace(., rowSums(usas_religion[,c(26, 28, 30, 39)]) >= 1,
          "protestantism lutheran") %>%
  replace(., rowSums(usas_religion[,c(33, 38)]) >= 1,
          "protestantism baptist") %>%
  replace(., rowSums(usas_religion[,c(11)]) >= 1,
          "orthodox eastern") %>%
  replace(., rowSums(usas_religion[,c(10)]) >= 1,
          "orthodox") %>%
  replace(., rowSums(usas_religion[,c(4, 12, 13, 17, 19, 21, 35, 36)]) >= 1,
          "protestantism reformed") %>%
  replace(., rowSums(usas_religion[,c(8, 9, 14)]) >= 1,
          "protestantism methodist") %>%
  replace(., rowSums(usas_religion[,c(7, 23)]) >= 1,
          "protestantism") %>%
  replace(., rowSums(usas_religion[,c(22, 25)]) >= 1,
          "anglicanism") %>%
  replace(., rowSums(usas_religion[,c(2, 15, 16, 24, 29, 31)]) >= 1,
          "nontrinitarianism") %>%
  replace(., rowSums(usas_religion[,c(27)]) >= 1,
          "atheism") %>%
  replace(., rowSums(usas_religion[,c(32)]) >= 1,
          "judaism")
usas_religion <- usas_religion[,c(1, 42)]
usas_religion$religion <- factor(usas_religion$religion)
saveRDS(usas_religion, "./data/usas_religion")

# retrieve and format birth dates -------------------------------------------------------

# austrian nationalrat
austria_birth <- wikiData(item = unique(austria$wikidataid), date = TRUE,
                          property = "P569")
austria_birth$date <- austria_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(austria_birth, "./data/austria_birth")

# french assemble
france_birth <- wikiData(item = unique(france$wikidataid), date = TRUE,
                         property = "P569")
france_birth$date <- france_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(france_birth, "./data/france_birth")

# german bundestag
germany_birth <- wikiData(item = unique(germany$wikidataid), date = TRUE,
                          property = "P569")
germany_birth$date <- germany_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(germany_birth, "./data/germany_birth")

# irish dail
ireland_birth <- wikiData(item = unique(ireland$wikidataid), date = TRUE,
                          property = "P569")
ireland_birth$date <- ireland_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(ireland_birth, "./data/ireland_birth")

# united states house
usah_birth <- wikiData(item = unique(usah$wikidataid), date = TRUE, property = "P569")
usah_birth$date <- usah_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(usah_birth, "./data/usah_birth")

# united states senate
usas_birth <- wikiData(item = unique(usas$wikidataid), date = TRUE, property = "P569")
usas_birth$date <- usas_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(usas_birth, "./data/usas_birth")

# retrieve and format death dates -------------------------------------------------------

# austrian nationalrat
austria_death <- wikiData(item = unique(austria$wikidataid), date = TRUE,
                          property = "P570")
austria_death$date <- austria_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(austria_death, "./data/austria_death")

# french assemble
france_death <- wikiData(item = unique(france$wikidataid), date = TRUE,
                         property = "P570")
france_death$date <- france_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(france_death, "./data/france_death")

# german bundestag
germany_death <- wikiData(item = unique(germany$wikidataid), date = TRUE,
                          property = "P570")
germany_death$date <- germany_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(germany_death, "./data/germany_death")

# irish dail
ireland_death <- wikiData(item = unique(ireland$wikidataid), date = TRUE,
                          property = "P570")
ireland_death$date <- ireland_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(ireland_death, "./data/ireland_death")

# united states house
usah_death <- wikiData(item = unique(usah$wikidataid), date = TRUE, entity = uhentity, property = "P570")
usah_death$date <- usah_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(usah_death, "./data/usah_death")

# united states senate
usas_death <- wikiData(item = unique(usas$wikidataid), date = TRUE, property = "P570")
usas_death$date <- usas_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(usas_death, "./data/usas_death")

# retrieve and format birth places ------------------------------------------------------

# austrian nationalrat
austria_birthplace <- wikiData(item = unique(austria$wikidataid), location = TRUE,
                               property = "P19")
austria_birthplace$lat <- round(austria_birthplace$lat, digit = 5)
austria_birthplace$lon <- round(austria_birthplace$lon, digit = 5)
austria_birthplace$birthplace <- str_c(austria_birthplace$lat, ",",
                                       austria_birthplace$lon)
austria_birthplace <- austria_birthplace[,c(1,4)]
saveRDS(austria_birthplace, "./data/austria_birthplace")

# french assemble
france_birthplace <- wikiData(item = unique(france$wikidataid), location = TRUE,
                              property = "P19")
france_birthplace$lat <- round(france_birthplace$lat, digit = 5)
france_birthplace$lon <- round(france_birthplace$lon, digit = 5)
france_birthplace$birthplace <- str_c(france_birthplace$lat, ",",
                                      france_birthplace$lon)
france_birthplace <- france_birthplace[,c(1,4)]
saveRDS(france_birthplace, "./data/france_birthplace")

# german bundestag
germany_birthplace <- wikiData(item = unique(germany$wikidataid), location = TRUE,
                               property = "P19")
germany_birthplace$lat <- round(germany_birthplace$lat, digit = 5)
germany_birthplace$lon <- round(germany_birthplace$lon, digit = 5)
germany_birthplace$birthplace <- str_c(germany_birthplace$lat, ",",
                                       germany_birthplace$lon)
germany_birthplace <- germany_birthplace[,c(1,4)]
saveRDS(germany_birthplace, "./data/germany_birthplace")

# irish dail
ireland_birthplace <- wikiData(item = unique(ireland$wikidataid), location = TRUE,
                               property = "P19")
ireland_birthplace$lat <- round(ireland_birthplace$lat, digit = 5)
ireland_birthplace$lon <- round(ireland_birthplace$lon, digit = 5)
ireland_birthplace$birthplace <- str_c(ireland_birthplace$lat, ",",
                                       ireland_birthplace$lon)
ireland_birthplace <- ireland_birthplace[,c(1,4)]
saveRDS(ireland_birthplace, "./data/ireland_birthplace")

# united states house
usah_birthplace <- wikiData(item = unique(usah$wikidataid), location = TRUE,
                            property = "P19")
usah_birthplace$lat <- round(usah_birthplace$lat, digit = 5)
usah_birthplace$lon <- round(usah_birthplace$lon, digit = 5)
usah_birthplace$birthplace <- str_c(usah_birthplace$lat, ",", usah_birthplace$lon)
usah_birthplace <- usah_birthplace[,c(1,4)]
saveRDS(usah_birthplace, "./data/usah_birthplace")

# united states senate
usas_birthplace <- wikiData(item = unique(usas$wikidataid), location = TRUE,
                            property = "P19")
usas_birthplace$lat <- round(usas_birthplace$lat, digit = 5)
usas_birthplace$lon <- round(usas_birthplace$lon, digit = 5)
usas_birthplace$birthplace <- str_c(usas_birthplace$lat, ",", usas_birthplace$lon)
usas_birthplace <- usas_birthplace[,c(1,4)]
saveRDS(usas_birthplace, "./data/usas_birthplace")

# retrieve and format death places ------------------------------------------------------

# austrian nationalrat
austria_deathplace <- wikiData(item = unique(austria$wikidataid), location = TRUE,
                               property = "P20")
austria_deathplace$lat <- round(austria_deathplace$lat, digit = 5)
austria_deathplace$lon <- round(austria_deathplace$lon, digit = 5)
austria_deathplace$deathplace <- str_c(austria_deathplace$lat, ",",
                                       austria_deathplace$lon)
austria_deathplace <- austria_deathplace[,c(1,4)]
saveRDS(austria_deathplace, "./data/austria_deathplace")

# french assemble
france_deathplace <- wikiData(item = unique(france$wikidataid), location = TRUE,
                              property = "P20")
france_deathplace$lat <- round(france_deathplace$lat, digit = 5)
france_deathplace$lon <- round(france_deathplace$lon, digit = 5)
france_deathplace$deathplace <- str_c(france_deathplace$lat, ",",
                                      france_deathplace$lon)
france_deathplace <- france_deathplace[,c(1,4)]
saveRDS(france_deathplace, "./data/france_deathplace")

# german bundestag
germany_deathplace <- wikiData(item = unique(germany$wikidataid), location = TRUE,
                               property = "P20")
germany_deathplace$lat <- round(germany_deathplace$lat, digit = 5)
germany_deathplace$lon <- round(germany_deathplace$lon, digit = 5)
germany_deathplace$deathplace <- str_c(germany_deathplace$lat, ",",
                                       germany_deathplace$lon)
germany_deathplace <- germany_deathplace[,c(1,4)]
saveRDS(germany_deathplace, "./data/germany_deathplace")

# irish dail
ireland_deathplace <- wikiData(item = unique(ireland$wikidataid), location = TRUE,
                               property = "P20")
ireland_deathplace$lat <- round(ireland_deathplace$lat, digit = 5)
ireland_deathplace$lon <- round(ireland_deathplace$lon, digit = 5)
ireland_deathplace$deathplace <- str_c(ireland_deathplace$lat, ",",
                                       ireland_deathplace$lon)
ireland_deathplace <- ireland_deathplace[,c(1,4)]
saveRDS(ireland_deathplace, "./data/ireland_deathplace")

# united states house
usah_deathplace <- wikiData(item = unique(usah$wikidataid), location = TRUE,
                            property = "P20")
usah_deathplace$lat <- round(usah_deathplace$lat, digit = 5)
usah_deathplace$lon <- round(usah_deathplace$lon, digit = 5)
usah_deathplace$deathplace <- str_c(usah_deathplace$lat, ",", usah_deathplace$lon)
usah_deathplace <- usah_deathplace[,c(1,4)]
saveRDS(usah_deathplace, "./data/usah_deathplace")

# united states senate
usas_deathplace <- wikiData(item = unique(usas$wikidataid), location = TRUE,
                            property = "P20")
usas_deathplace$lat <- round(usas_deathplace$lat, digit = 5)
usas_deathplace$lon <- round(usas_deathplace$lon, digit = 5)
usas_deathplace$deathplace <- str_c(usas_deathplace$lat, ",", usas_deathplace$lon)
usas_deathplace <- usas_deathplace[,c(1,4)]
saveRDS(usas_deathplace, "./data/usas_deathplace")

# retrieve and format social media data -------------------------------------------------

# austrian nationalrat
austria_twitter <- wikiData(item = unique(austria$wikidataid), property = "P2002",
                            serial = TRUE)
austria_twitter <- austria_twitter[-c(1,12,15,20,27,28),]
austria_instagram <- wikiData(item = unique(austria$wikidataid), property = "P2003",
                              serial = TRUE)
austria_instagram$value <- str_replace(austria_instagram$value, "2016", "")
austria_facebook <- wikiData(item = unique(austria$wikidataid), property = "P2013",
                             serial = TRUE)
austria_linkedin <- wikiData(item = unique(austria$wikidataid), property = "P2035",
                             serial = TRUE)
austria_youtube <- wikiData(item = unique(austria$wikidataid), property = "P2397",
                            serial = TRUE)
austria_website <- wikiData(item = unique(austria$wikidataid), property = "P856",
                            serial = TRUE)
austria_social <- full_join(austria_twitter, austria_facebook, by = "wikidataid") %>%
  full_join(., austria_youtube, by = "wikidataid") %>%
  full_join(., austria_instagram, by = "wikidataid") %>%
  full_join(., austria_website, by = "wikidataid")
names(austria_social) <- c("wikidataid", "twitter", "facebook", "youtube", "instagram",
                           "website")
saveRDS(austria_social, "./data/austria_social")

# french assemble
frentity <- get_item(id = unique(france$wikidataid))
france_twitter <- wikiData(item = unique(france$wikidataid), entity = frentity,
                           property = "P2002", serial = TRUE)
france_facebook <- wikiData(item = unique(france$wikidataid), entity = frentity,
                            property = "P2013", serial = TRUE)
france_youtube <- wikiData(item = unique(france$wikidataid), entity = frentity,
                           property = "P2397", serial = TRUE)
france_instagram <- wikiData(item = unique(france$wikidataid), entity = frentity,
                             property = "P2003", serial = TRUE)
france_linkedin <- wikiData(item = unique(france$wikidataid), entity = frentity,
                            property = "P2035", serial = TRUE)
france_website <- wikiData(item = unique(france$wikidataid), entity = frentity,
                           property = "P856", serial = TRUE)
france_social <- full_join(x = france_twitter, y = france_facebook, by = "wikidataid") %>%
  full_join(x = ., y = france_youtube, by = "wikidataid") %>%
  full_join(x = ., y = france_instagram, by = "wikidataid") %>%
  full_join(x = ., y = france_linkedin, by = "wikidataid") %>%
  full_join(x = ., y = france_website, by = "wikidataid")
names(france_social) <- c("wikidataid", "twitter", "facebook", "youtube", "instagram",
                      "linkedin", "website")
saveRDS(france_social, "./data/france_social")

# german bundestag
gentity <- get_item(id = unique(germany$wikidataid))
germany_twitter <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                            property = "P2002", serial = TRUE)
germany_facebook <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                             property = "P2013", serial = TRUE)
germany_youtube <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                            property = "P2397", serial = TRUE)
germany_googlep <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                             property = "P2847", serial = TRUE)
germany_instagram <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                              property = "P2003", serial = TRUE)
germany_linkedin <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                             property = "P2035", serial = TRUE)
germany_website <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                            property = "P856", serial = TRUE)
germany_social <- full_join(x = germany_twitter, y = germany_facebook, by = "wikidataid") %>%
  full_join(x = ., y = germany_youtube, by = "wikidataid") %>%
  full_join(x = ., y = germany_googlep, by = "wikidataid") %>%
  full_join(x = ., y = germany_instagram, by = "wikidataid") %>%
  full_join(x = ., y = germany_linkedin, by = "wikidataid") %>%
  full_join(x = ., y = germany_website, by = "wikidataid")
names(germany_social) <- c("wikidataid", "twitter", "facebook", "youtube", "googlep",
                           "instagram", "linkedin", "website")
saveRDS(germany_social, "./data/germany_social")

# irish dail
irentity <- get_item(id = unique(ireland$wikidataid))
ireland_twitter <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                            property = "P2002", serial = TRUE)
ireland_facebook <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                             property = "P2013", serial = TRUE)
#ireland_youtube <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
#                            property = "P2397", serial = TRUE)
#ireland_googlep <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
#                            property = "P2847", serial = TRUE)
#ireland_instagram <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
#                              property = "P2003", serial = TRUE)
#ireland_linkedin <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
#                             property = "P2035", serial = TRUE)
ireland_website <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                            property = "P856", serial = TRUE)
ireland_social <- full_join(x = ireland_twitter, y = ireland_facebook, by = "wikidataid") %>%
  full_join(x = ., y = ireland_website, by = "wikidataid")
names(ireland_social) <- c("wikidataid", "twitter", "facebook", "website")
saveRDS(ireland_social, "./data/ireland_social")

# united states house
uhentity <- get_item(id = unique(usah$wikidataid))
usah_twitter <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                         property = "P2002", serial = TRUE)
usah_facebook <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                          property = "P2013", serial = TRUE)
usah_youtube <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                         property = "P2397", serial = TRUE)
usah_googlep <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                            property = "P2847", serial = TRUE)
usah_instagram <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                           property = "P2003", serial = TRUE)
usah_linkedin <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                          property = "P2035", serial = TRUE)
usah_website <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                         property = "P856", serial = TRUE)
usah_social <- full_join(x = usah_twitter, y = usah_facebook, by = "wikidataid") %>%
  full_join(x = ., y = usah_youtube, by = "wikidataid") %>%
  full_join(x = ., y = usah_googlep, by = "wikidataid") %>%
  full_join(x = ., y = usah_instagram, by = "wikidataid") %>%
  full_join(x = ., y = usah_linkedin, by = "wikidataid") %>%
  full_join(x = ., y = usah_website, by = "wikidataid")
names(usah_social) <- c("wikidataid", "twitter", "facebook", "youtube", "googlep",
                           "instagram", "linkedin", "website")
saveRDS(usah_social, "./data/usah_social")

# united states senate
usentity <- get_item(id = unique(usas$wikidataid))
usas_twitter <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                         property = "P2002", serial = TRUE)
usas_facebook <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                          property = "P2013", serial = TRUE)
usas_youtube <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                         property = "P2397", serial = TRUE)
usas_googlep <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                         property = "P2847", serial = TRUE)
usas_instagram <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                           property = "P2003", serial = TRUE)
usas_linkedin <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                          property = "P2035", serial = TRUE)
usas_website <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                         property = "P856", serial = TRUE)
usas_social <- full_join(x = usas_twitter, y = usas_facebook, by = "wikidataid") %>%
  full_join(x = ., y = usas_youtube, by = "wikidataid") %>%
  full_join(x = ., y = usas_googlep, by = "wikidataid") %>%
  full_join(x = ., y = usas_instagram, by = "wikidataid") %>%
  full_join(x = ., y = usas_linkedin, by = "wikidataid") %>%
  full_join(x = ., y = usas_website, by = "wikidataid")
names(usas_social) <- c("wikidataid", "twitter", "facebook", "youtube", "googlep",
                        "instagram", "linkedin", "website")
saveRDS(usas_social, "./data/usas_social")

# retrieve and format facial data -------------------------------------------------------

# specify face++ api authentification
auth <- authFacepp(api_key = "",
                   api_secret = "")

# austrian nationalrat
austria_images <- imageUrl(pageid = unique(austria$pageid), project = "de.wikipedia")
austria_images <- austria_images[-c(8,11,15,18,22,26,28,30,37,41,45,48,55,59,62,68,72,74,
                                    75,76,78,79,80,81,83,84,85,86,88,89,91,93,94,95,96,
                                    100,102,103,104,105,106,108,109,110,112,116,117,123,
                                    125,126,128,133,134,137,142,150,157,159,226,229,255,
                                    266,314,315,345,373,383,406),]
austria_images <- austria_images[-c(91,99,124,131,140,196,197,240,244,246,317,341),]
austria_images <- austria_images[-c(290),]
austria_faces <- faceEst(data = austria_images, auth = auth)
austria_faces$ethnicity <- "white"
austria_faces <- austria_faces[,-c(2,17)]
austria_faces <- left_join(austria_faces, austria_images, by = "pageid")
austria_faces$ethnicity <- factor(austria_faces$ethnicity)
saveRDS(austria_faces, "./data/austria_faces")

# french assemble
france_images <- imageUrl(pageid = unique(france$pageid), project = "fr.wikipedia")
france_images <- france_images[!str_detect(france_images$image_url,
                                           "https://upload.wikimedia.org/wikipedia/commons/thumb/8/85/Defaut.svg/50px-Defaut.svg.png"), ]
saveRDS(france_images, "./data/france_images")
black <- france_images$pageid[c(1, 12, 13, 23, 24, 34, 38, 41, 50, 53, 76, 79, 380, 448, 553,
                                 616, 630, 670, 674, 733, 746, 777, 830, 899, 923)]
asian <- france_images$pageid[c(322, 327)]
france_images <- france_images[-c(7, 8, 16, 17, 26, 30, 42, 44, 46, 51, 52, 57, 60, 61,
                                  66, 69, 70, 74, 79, 82, 85, 89, 91, 93, 100, 116, 120,
                                  121, 124, 130, 131, 137, 139, 143, 147, 150, 154, 159,
                                  166, 178, 183, 189, 190, 196, 201, 223, 225, 227,252,
                                  254, 257, 262, 266, 271, 291, 294, 300, 305, 306, 310,
                                  356, 371, 389, 403, 411, 412, 443,  453, 460, 462, 463,
                                  471, 472, 478, 488, 507, 521, 528, 536, 540, 546,  551,
                                  559, 561, 564,  591, 601, 635, 636, 669, 681, 694, 702,
                                  710, 717, 729, 731, 732,750, 753, 783, 800, 31, 56, 92,
                                  135, 419, 527, 531, 595, 835, 895, 902, 922),]
france_faces <- faceEst(data = france_images, auth = auth)
france_faces$ethnicity <- "white"
france_faces$ethnicity[which(france_faces$pageid %in% black)] <- "black"
france_faces$ethnicity[which(france_faces$pageid %in% asian)] <- "asian"
france_faces <- france_faces[,-c(2,17)]
france_faces <- left_join(france_faces, france_images, by = "pageid")
france_faces$ethnicity <- factor(france_faces$ethnicity)
saveRDS(france_faces, "./data/france_faces")

# german bundestag
germany_images <- imageUrl(pageid = unique(germany$pageid), project = "de.wikipedia")
black <- germany_images$pageid[c(1519, 1566)]
asian <- germany_images$pageid[c(1193, 1276, 1425)]
germany_images <- germany_images[-c(2, 5, 7, 15, 16, 17, 19, 20, 21, 23, 26, 27, 29, 31, 33, 35,
                            36, 41, 45, 47, 51, 52, 54, 58, 60, 61, 62, 63, 64, 65, 66,
                            69, 81, 83, 84, 85, 87, 88, 93, 96, 99, 100, 102, 103, 104,
                            113, 114, 124, 125, 126, 127, 129, 138, 139, 142, 146, 149,
                            150, 152, 159, 161, 166, 175, 177, 178, 179, 182, 185, 190,
                            193, 196, 197, 203, 205, 207, 210, 211, 217, 218, 220, 221,
                            232, 235, 241, 243, 247, 250, 252, 258, 260, 264, 265, 273,
                            281, 288, 298, 306, 310, 313, 317, 323, 354, 356, 359, 362,
                            364, 368, 371, 372, 376, 377, 379, 383, 388, 389, 391, 402,
                            404, 405, 408, 410, 415, 416, 417, 427, 430, 432, 433, 437,
                            438, 445, 446, 447, 452, 457, 465, 472, 475, 489, 491, 500,
                            509, 510, 511, 519, 529, 531, 534, 539, 546, 548, 555, 557,
                            560, 567, 569, 572, 585, 596, 597, 598, 621, 629, 633, 656,
                            659, 669, 672, 673, 675, 690, 692, 727, 732, 742, 755, 763,
                            771, 794, 795, 802, 815, 826, 827, 839, 843, 849, 866, 874,
                            881, 886, 890, 893, 905, 906, 908, 949, 964, 977, 991, 994,
                            1000, 1005, 1022, 1024, 1038, 1065, 1067, 1071, 1080, 1084,
                            1112, 1114, 1122, 1135, 1139, 1145, 1146, 1151, 1158, 1165,
                            1166, 1209, 1212, 1216, 1239, 1245, 1270, 1313, 1332, 1333,
                            1340, 1357, 1397, 1406, 1462, 1477, 1478, 1489, 1521, 1583,
                            1627, 1653, 1678)]
germany_faces <- faceEst(data = germany_images[1:20], auth = auth)
germany_faces$ethnicity <- "white"
germany_faces$ethnicity[which(germany_faces$pageid %in% black)] <- "black"
germany_faces$ethnicity[which(germany_faces$pageid %in% asian)] <- "asian"
germany_faces <- germany_faces[,-c(2,17)]
germany_faces <- left_join(germany_faces, germany_images, by = "pageid")
saveRDS(germany_faces, "./data/germany_faces")

# irish dail
ireland_images <- imageUrl(pageid = unique(ireland$pageid), project = "en.wikipedia")
ireland_images <- ireland_images[-c(20, 25, 28, 31, 41, 44, 46, 48, 50, 57, 61, 62, 63,
                                   64, 66, 75, 82, 104, 128, 130, 142, 158, 167, 183,
                                   198, 219, 223, 233)]
ireland_faces <- faceEst(data = ireland_images, auth = auth)
ireland_faces$ethnicity <- "white"
ireland_faces <- ireland_faces[,-c(2,17)]
ireland_faces <- left_join(ireland_faces, ireland_images, by = "pageid")
saveRDS(ireland_faces, "./data/ireland_faces")

# united states house
usah_images <- imageUrl(pageid = unique(usah$pageid), project = "en.wikipedia")
black <- c(1958204, 6097971, 2160334, 1943681, 2160232, 2160425, 6098020, 1184772, 6098067,
           3687428, 5949143, 5906912, 1718954, 6098238, 6098279, 1643874, 1191242, 1030585,
           1183764, 392912, 5627881, 814092, 445306, 501512, 567269, 976884, 718060, 3367757,
           551711, 846945, 2046362, 410375, 1259804, 351329, 181510, 3609714, 1068651,
           1028706, 750683, 378145, 6098320, 6098430, 1198557, 958480, 40305, 6098545, 935823,
           865390, 958517, 6098715, 6098847, 779765, 2807116, 411737, 1401514,964391, 457310,
           1012443, 6098897, 414468, 409013, 2228387, 1028916, 411731, 410969, 784733, 3390635,
           411264, 973257, 961356, 392017, 1028837, 958551, 411889, 964012, 958398, 414629,
           414538, 699361, 814763, 479733, 512956, 411900, 409029, 414574, 412215, 412062,
           30876428, 880750, 30876298, 409000, 458036, 407892, 409070, 411233, 411853, 850143,
           1160411, 517069, 1166150, 2045683, 5437748, 6031063, 8861987, 30205998, 5548283,
           19286605, 8722536, 9959597, 26323292, 27658059, 22136277, 11893373, 13719198,
           35941258, 881669, 13272531, 7358729, 36608661, 6520375, 451590, 26384160, 12668582,
           25741095, 3012558, 16488153, 51189858, 3682711, 37072819, 13715528, 3353264,
           309644, 309588, 309645, 309634, 44332072)
asian <- c(2579305, 382573, 542406, 712550, 152810, 408610, 1198557, 715246, 955773, 964012,
           367774, 699418, 383758, 1133032, 1548355, 715545, 1238360, 20101418, 7197854,
           5173337, 9959597, 14176557, 28670879, 3691615, 19309394, 37017445, 5774818,
           40812967, 45298457, 38318277, 51960710, 51003997, 4661622, 47528335, 160753,
           4665409, 4694873, 4734733, 4827038, 4844473, 4895622, 4917923, 4917923,
           647820, 309612)
hispanic <- c(5296377, 651159, 4463732, 11821575, 1854394, 9850460, 3726174, 9857741, 1854831,
              958928, 2335526, 2519361, 1001109, 30863109, 958417, 375375, 964045,
              411238, 945385, 407922, 408979, 409003, 694313, 958974, 3683858, 171566,
              948525, 838680, 966514, 409037, 409672, 410375, 408987, 409644, 1166091,
              2907738, 1163464, 1602620, 408529, 21708310, 26953920, 28933184, 14932561,
              9475992, 4908339, 2359039, 13605432, 412017, 15908202, 26457766, 35760455,
              16211910, 44279946, 16939161, 10815711, 47384600, 4680057, 37836929,
              48530096, 11088878, 21099348, 51468866, 411276, 27569965, 2118902, 49616753,
              4160300, 719486, 824541, 755932, 2224025, 8281898, 8766929, 399426, 70964,
              963626, 865904, 36781283, 15275851, 952447, 11543108, 11580297, 11578859, 11579212,
              1496669, 11580303, 4661622, 11607694, 1496548, 4895622, 3310711, 4930012, 1035479,
              11607698, 309639, 1134123, 24746943, 1129560, 11313563, 8980772, 2844092,
              11607700, 1805771, 11607705, 11608660, 5768003, 160753, 731734, 7549804,
              4734733, 961912, 11578399, 3312727, 1098881, 99034)
native <- c(2630897, 2160334, 1718954, 92213, 11755294, 11755403, 13325985, 11755521,
            8082064, 8915354, 4846943, 339185, 699370, 962197, 16459229, 37205890)
islander <- c(296716, 26328774, 803643, 667983, 904574, 823954, 309529, 309524, 5876215,
              309603, 44345195, 20224544)
usah_images <- usah_images[-c(4, 6, 8, 9, 12, 22, 23, 41, 45, 59, 73, 74, 78, 80, 82,
                              85, 90, 92, 96, 101, 102, 111, 113, 132, 137, 146, 148,
                              149, 170, 175, 188, 191, 193, 194, 199, 200, 204, 210,
                              220, 224, 226, 238, 240, 242, 248, 249, 286, 295, 313,
                              323, 334, 336, 349, 376, 383, 390, 396, 434, 474, 475,
                              476, 481, 487, 508, 509, 510, 511, 514, 530, 536, 556,
                              566, 569, 603, 622, 630, 648, 653, 655, 678, 697, 728,
                              733, 781, 783, 784, 809, 825, 846, 888, 894, 901, 903,
                              911, 922, 962, 966, 1028, 1042, 1044, 1047, 1050, 1075,
                              1120, 1132, 1136, 1160, 1170, 1188, 1222, 1246, 1277,
                              1296, 1314, 1346, 1474, 1477, 1478, 1483, 1510, 1543,
                              1567, 1581, 1591, 1603, 1618, 1626, 1649, 1652, 1722,
                              1727, 1761, 1774, 1785, 1881, 1901, 1910, 1953, 1954)]
usah_faces <- faceEst(data = usah_images, auth = auth)
usah_faces$ethnicity <- "white"
usah_faces$ethnicity[which(usah_faces$pageid %in% black)] <- "black"
usah_faces$ethnicity[which(usah_faces$pageid %in% asian)] <- "asian"
usah_faces$ethnicity[which(usah_faces$pageid %in% hispanic)] <- "hispanic"
usah_faces$ethnicity[which(usah_faces$pageid %in% native)] <- "native"
usah_faces$ethnicity[which(usah_faces$pageid %in% islander)] <- "islander"
usah_faces <- usah_faces[,-c(2,17)]
usah_faces <- left_join(usah_faces, usah_images, by = "pageid")
saveRDS(usah_faces, "./data/usah_faces")

# united states senate
usas_images <- imageUrl(pageid = unique(usas$pageid), project = "en.wikipedia")
black <- usas_images$pageid[c(1582, 1753, 1824, 1850, 1876, 1885, 1890)]
asian <- usas_images$pageid[c(1560, 1634, 1649, 1808, 1881, 1911, 1910)]
hispanic <- usas_images$pageid[c(221, 375, 466, 1206, 1291, 1564, 1817, 1821, 1822, 1826,
                                 1866, 1893, 1915)]
native <- usas_images$pageid[c(620, 972, 979, 1084, 1750)]
islander <- usas_images$pageid[c(1521, 1730)]
usas_images <- usas_images[-c(3, 13, 16, 25, 30, 31, 34, 35, 37, 38, 41, 49, 64, 74, 83,
                              90, 101, 107, 108, 110, 146, 155, 163, 184, 189, 193, 238,
                              240, 243, 252, 253, 276, 295, 308, 359, 398, 405, 426, 436,
                              437, 439, 450, 573, 662, 740, 817, 1039, 1051, 1130, 1288,
                              1295, 1439, 1463, 1689, 1701, 1718, 1768)]
usas_faces <- faceEst(data = usas_images, auth = auth)
usas_faces$ethnicity <- "white"
usas_faces$ethnicity[which(usas_faces$pageid %in% black)] <- "black"
usas_faces$ethnicity[which(usas_faces$pageid %in% asian)] <- "asian"
usas_faces$ethnicity[which(usas_faces$pageid %in% hispanic)] <- "hispanic"
usas_faces$ethnicity[which(usas_faces$pageid %in% native)] <- "native american"
usas_faces$ethnicity[which(usas_faces$pageid %in% islander)] <- "pacific islander"
usas_faces <- usas_faces[,-c(2,17)]
usas_faces <- left_join(usas_faces, usas_images, by = "pageid")
saveRDS(usas_faces, "./data/usas_faces")

# retrieve and format ids ---------------------------------------------------------------

# austrian nationalrat
aentity <- get_item(id = unique(austria$wikidataid))
austria_parlid <- wikiData(item = unique(austria$wikidataid), entity = aentity,
                           property = "P2280", serial = TRUE)
austria_gndid <- wikiData(item = unique(austria$wikidataid), entity = aentity,
                          property = "P227", serial = TRUE)
austria_libcon <- wikiData(item = unique(austria$wikidataid), entity = aentity,
                           property = "P244", serial = TRUE)
austria_bnfid <- wikiData(item = unique(austria$wikidataid), entity = aentity,
                          property = "P268", serial = TRUE)
austria_freebase <- wikiData(item = unique(austria$wikidataid), entity = aentity,
                             property = "P646", serial = TRUE)
austria_munzinger <- wikiData(item = unique(austria$wikidataid), entity = aentity,
                              property = "P1284", serial = TRUE)
austria_nndb <- wikiData(item = unique(austria$wikidataid), entity = aentity,
                         property = "P1263", serial = TRUE)
austria_imdb <- wikiData(item = unique(austria$wikidataid), entity = aentity,
                         property = "P345", serial = TRUE)
austria_brittanica <- wikiData(item = unique(austria$wikidataid), entity = aentity,
                               property = "P1417", serial = TRUE)
austria_quora <- wikiData(item = unique(austria$wikidataid), entity = aentity,
                          property = "P3417", serial = TRUE)
austria_id <- full_join(x = austria_parlid, y = austria_gndid , by = "wikidataid") %>%
  full_join(x = ., y = austria_libcon, by = "wikidataid") %>%
  full_join(x = ., y = austria_bnfid, by = "wikidataid") %>%
  full_join(x = ., y = austria_freebase, by = "wikidataid") %>%
  full_join(x = ., y = austria_munzinger, by = "wikidataid") %>%
  full_join(x = ., y = austria_nndb, by = "wikidataid") %>%
  full_join(x = ., y = austria_imdb, by = "wikidataid") %>%
  full_join(x = ., y = austria_brittanica, by = "wikidataid") %>%
  full_join(x = ., y = austria_quora, by = "wikidataid")
names(austria_id) <- c("wikidataid", "parlid", "gndid", "libcon",
                       "bnfid", "freebase", "munzinger", "nndb", "imdb",
                       "brittanica", "quora")
saveRDS(austria_id, "./data/austria_id")

# french assemble
frentity <- get_item(id = unique(france$wikidataid))
france_parlid <- wikiData(item = unique(france$wikidataid), entity = frentity,
                          property = "P4123", serial = TRUE)
france_sycomore <- wikiData(item = unique(france$wikidataid), entity = frentity,
                            property = "P1045", serial = TRUE)
france_gndid <- wikiData(item = unique(france$wikidataid), entity = frentity,
                         property = "P227", serial = TRUE)
france_libcon <- wikiData(item = unique(france$wikidataid), entity = frentity,
                          property = "P244", serial = TRUE)
france_bnfid <- wikiData(item = unique(france$wikidataid), entity = frentity,
                         property = "P268", serial = TRUE)
france_freebase <- wikiData(item = unique(france$wikidataid), entity = frentity,
                            property = "P646", serial = TRUE)
france_munzinger <- wikiData(item = unique(france$wikidataid), entity = frentity,
                             property = "P1284", serial = TRUE)
france_nndb <- wikiData(item = unique(france$wikidataid), entity = frentity,
                        property = "P1263", serial = TRUE)
france_imdb <- wikiData(item = unique(france$wikidataid), entity = frentity,
                        property = "P345", serial = TRUE)
france_brittanica <- wikiData(item = unique(france$wikidataid), entity = frentity,
                              property = "P1417", serial = TRUE)
france_quora <- wikiData(item = unique(france$wikidataid), entity = frentity,
                         property = "P3417", serial = TRUE)
france_id <- full_join(x = france_parlid, y = france_sycomore , by = "wikidataid") %>%
  full_join(x = ., y = france_gndid, by = "wikidataid") %>%
  full_join(x = ., y = france_libcon, by = "wikidataid") %>%
  full_join(x = ., y = france_bnfid, by = "wikidataid") %>%
  full_join(x = ., y = france_freebase, by = "wikidataid") %>%
  full_join(x = ., y = france_munzinger, by = "wikidataid") %>%
  full_join(x = ., y = france_nndb, by = "wikidataid") %>%
  full_join(x = ., y = france_imdb, by = "wikidataid") %>%
  full_join(x = ., y = france_brittanica, by = "wikidataid") %>%
  full_join(x = ., y = france_quora, by = "wikidataid")
names(france_id) <- c("wikidataid", "parlid", "sycomore", "gndid", "libcon",
                      "bnfid", "freebase", "munzinger", "nndb", "imdb",
                      "brittanica", "quora")
saveRDS(france_id, "./data/france_id")

# german bundestag
germany_parlid <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                           property = "P1713", serial = TRUE)
germany_gndid <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                          property = "P227", serial = TRUE)
germany_libcon <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                           property = "P244", serial = TRUE)
germany_bnfid <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                          property = "P268", serial = TRUE)
germany_freebase <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                             property = "P646", serial = TRUE)
germany_munzinger <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                              property = "P1284", serial = TRUE)
germany_nndb <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                         property = "P1263", serial = TRUE)
germany_imdb <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                         property = "P345", serial = TRUE)
germany_brittanica <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                               property = "P1417", serial = TRUE)
germany_quora <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                          property = "P3417", serial = TRUE)
germany_id <- full_join(x = germany_parlid, y = germany_gndid , by = "wikidataid") %>%
  full_join(x = ., y = germany_libcon, by = "wikidataid") %>%
  full_join(x = ., y = germany_bnfid, by = "wikidataid") %>%
  full_join(x = ., y = germany_freebase, by = "wikidataid") %>%
  full_join(x = ., y = germany_munzinger, by = "wikidataid") %>%
  full_join(x = ., y = germany_nndb, by = "wikidataid") %>%
  full_join(x = ., y = germany_imdb, by = "wikidataid") %>%
  full_join(x = ., y = germany_brittanica, by = "wikidataid") %>%
  full_join(x = ., y = germany_quora, by = "wikidataid")
names(germany_id) <- c("wikidataid", "parlid", "gndid", "libcon",
                      "bnfid", "freebase", "munzinger", "nndb", "imdb",
                      "brittanica", "quora")
saveRDS(germany_id, "./data/germany_id")

# irish dail
ireland_gndid <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                          property = "P227", serial = TRUE)
ireland_libcon <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                           property = "P244", serial = TRUE)
ireland_bnfid <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                          property = "P268", serial = TRUE)
ireland_freebase <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                             property = "P646", serial = TRUE)
ireland_munzinger <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                              property = "P1284", serial = TRUE)
ireland_nndb <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                         property = "P1263", serial = TRUE)
ireland_imdb <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                         property = "P345", serial = TRUE)
ireland_brittanica <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                               property = "P1417", serial = TRUE)
ireland_quora <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                          property = "P3417", serial = TRUE)
ireland_id <- full_join(x = ireland_gndid, y = ireland_libcon, by = "wikidataid") %>%
  full_join(x = ., y = ireland_bnfid, by = "wikidataid") %>%
  full_join(x = ., y = ireland_freebase, by = "wikidataid") %>%
  full_join(x = ., y = ireland_munzinger, by = "wikidataid") %>%
  full_join(x = ., y = ireland_nndb, by = "wikidataid") %>%
  full_join(x = ., y = ireland_imdb, by = "wikidataid") %>%
  full_join(x = ., y = ireland_brittanica, by = "wikidataid") %>%
  full_join(x = ., y = ireland_quora, by = "wikidataid")
names(ireland_id) <- c("wikidataid", "gndid", "libcon",
                       "bnfid", "freebase", "munzinger", "nndb", "imdb",
                       "brittanica", "quora")
saveRDS(ireland_id, "./data/ireland_id")

# united states house
usah_parlid <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                           property = "P1157", serial = TRUE)
usah_gndid <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                          property = "P227", serial = TRUE)
usah_libcon <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                           property = "P244", serial = TRUE)
usah_bnfid <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                          property = "P268", serial = TRUE)
usah_freebase <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                             property = "P646", serial = TRUE)
usah_munzinger <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                              property = "P1284", serial = TRUE)
usah_nndb <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                         property = "P1263", serial = TRUE)
usah_imdb <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                         property = "P345", serial = TRUE)
usah_brittanica <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                               property = "P1417", serial = TRUE)
usah_quora <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                          property = "P3417", serial = TRUE)
usah_votesmart <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                           property = "P3344", serial = TRUE)
usah_fecid <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                       property = "P1839", serial = TRUE)
usah_ballotpedia <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                             property = "P2390", serial = TRUE)
usah_opensecrets <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                             property = "P2686", serial = TRUE)
usah_genealogists <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                              property = "P1819", serial = TRUE)
usah_politfacts <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                            property = "P2267", serial = TRUE)
usah_id <- full_join(x = usah_parlid, y = usah_gndid , by = "wikidataid") %>%
  full_join(x = ., y = usah_libcon, by = "wikidataid") %>%
  full_join(x = ., y = usah_bnfid, by = "wikidataid") %>%
  full_join(x = ., y = usah_freebase, by = "wikidataid") %>%
  full_join(x = ., y = usah_munzinger, by = "wikidataid") %>%
  full_join(x = ., y = usah_nndb, by = "wikidataid") %>%
  full_join(x = ., y = usah_imdb, by = "wikidataid") %>%
  full_join(x = ., y = usah_brittanica, by = "wikidataid") %>%
  full_join(x = ., y = usah_quora, by = "wikidataid") %>%
  full_join(x = ., y = usah_votesmart, by = "wikidataid") %>%
  full_join(x = ., y = usah_fecid, by = "wikidataid") %>%
  full_join(x = ., y = usah_ballotpedia, by = "wikidataid") %>%
  full_join(x = ., y = usah_opensecrets, by = "wikidataid") %>%
  full_join(x = ., y = usah_genealogists, by = "wikidataid") %>%
  full_join(x = ., y = usah_politfacts, by = "wikidataid")
names(usah_id) <- c("wikidataid", "parlid", "gndid", "libcon",
                    "bnfid", "freebase", "munzinger", "nndb", "imdb",
                    "brittanica", "quora", "votesmart", "fecid", "ballotpedia",
                    "opensecrets", "genealogists", "politfacts")
saveRDS(usah_id, "./data/usah_id")

# united states senate
usas_parlid <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                        property = "P1157", serial = TRUE)
usas_gndid <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                          property = "P227", serial = TRUE)
usas_libcon <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                           property = "P244", serial = TRUE)
usas_bnfid <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                          property = "P268", serial = TRUE)
usas_freebase <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                             property = "P646", serial = TRUE)
usas_munzinger <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                              property = "P1284", serial = TRUE)
usas_nndb <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                         property = "P1263", serial = TRUE)
usas_imdb <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                         property = "P345", serial = TRUE)
usas_brittanica <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                               property = "P1417", serial = TRUE)
usas_quora <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                          property = "P3417", serial = TRUE)
usas_votesmart <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                           property = "P3344", serial = TRUE)
usas_fecid <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                       property = "P1839", serial = TRUE)
usas_ballotpedia <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                             property = "P2390", serial = TRUE)
usas_opensecrets <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                             property = "P2686", serial = TRUE)
usas_genealogists <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                                 property = "P1819", serial = TRUE)
usas_politfacts <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                               property = "P2267", serial = TRUE)
usas_id <- full_join(x = usas_parlid, y = usas_gndid , by = "wikidataid") %>%
  full_join(x = ., y = usas_libcon, by = "wikidataid") %>%
  full_join(x = ., y = usas_bnfid, by = "wikidataid") %>%
  full_join(x = ., y = usas_freebase, by = "wikidataid") %>%
  full_join(x = ., y = usas_munzinger, by = "wikidataid") %>%
  full_join(x = ., y = usas_nndb, by = "wikidataid") %>%
  full_join(x = ., y = usas_imdb, by = "wikidataid") %>%
  full_join(x = ., y = usas_brittanica, by = "wikidataid") %>%
  full_join(x = ., y = usas_quora, by = "wikidataid") %>%
  full_join(x = ., y = usas_votesmart, by = "wikidataid") %>%
  full_join(x = ., y = usas_fecid, by = "wikidataid") %>%
  full_join(x = ., y = usas_ballotpedia, by = "wikidataid") %>%
  full_join(x = ., y = usas_opensecrets, by = "wikidataid") %>%
  full_join(x = ., y = usas_genealogists, by = "wikidataid") %>%
  full_join(x = ., y = usas_politfacts, by = "wikidataid")
names(usas_id) <- c("wikidataid", "parlid", "gndid", "libcon",
                    "bnfid", "freebase", "munzinger", "nndb", "imdb",
                    "brittanica", "quora", "votesmart", "fecid", "ballotpedia",
                    "opensecrets", "genealogists", "politfacts")
saveRDS(usas_id, "./data/usas_id")

# retrieve and format positions ---------------------------------------------------------

# austrian nationalrat
austria_positions <- wikiData(item = unique(austria$wikidataid), entity = aentity,
                              unique = TRUE, property = "P39")
austria_positions <- austria_positions[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(austria_positions, "./data/austria_positions")

# french assemble
france_positions <- wikiData(item = unique(france$wikidataid), entity = frentity,
                            unique = TRUE, property = "P39")
france_positions <- france_positions[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(france_positions, "./data/france_positions")

# german bundestag
germany_positions <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                              unique = TRUE, property = "P39")
germany_positions <- germany_positions[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(germany_positions, "./data/germany_positions")

# irish dail
ireland_positions <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                              unique = TRUE, property = "P39")
ireland_positions <- ireland_positions %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(ireland_positions, "./data/ireland_positions")

# united states house
usah_positions <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                           unique = TRUE, property = "P39")
usah_positions <- usah_positions[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(usah_positions, "./data/usah_positions")

# united states senate
usas_positions <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                           unique = TRUE, property = "P39")
usas_positions <- usas_positions[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(usas_positions, "./data/usas_positions")


# retrieve and format occupation --------------------------------------------------------

# austrian nationalrat
austria_occupation <- wikiData(item = unique(austria$wikidataid), entity = aentity,
                              unique = TRUE, property = "P106")
austria_occupation <- austria_occupation[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(austria_occupation, "./data/austria_occupation")

# french assemble
france_occupation <- wikiData(item = unique(france$wikidataid), entity = frentity,
                              unique = TRUE, property = "P106")
france_occupation <- france_occupation[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(france_occupation, "./data/france_occupation")

# german bundestag
germany_occupation <- wikiData(item = unique(germany$wikidataid), entity = gentity,
                               unique = TRUE, property = "P106")
germany_occupation <- germany_occupation[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(germany_occupation, "./data/germany_occupation")

# irish dail
ireland_occupation <- wikiData(item = unique(ireland$wikidataid), entity = irentity,
                               unique = TRUE, property = "P106")
ireland_occupation <- ireland_occupation %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(ireland_occupation, "./data/ireland_occupation")

# united states house
usah_occupation <- wikiData(item = unique(usah$wikidataid), entity = uhentity,
                               unique = TRUE, property = "P106")
usah_occupation <- usah_occupation[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(usah_occupation, "./data/usah_occupation")

# united states senate
usas_occupation <- wikiData(item = unique(usas$wikidataid), entity = usentity,
                            unique = TRUE, property = "P106")
usas_occupation <- usas_occupation[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(usas_occupation, "./data/usas_occupation")
