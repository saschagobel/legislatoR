# ---------------------------------------------------------------------------------------
# legislatoR
# Sascha Göbel and Simon Munzert
# Script: preparations
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

# austrian nationalrat
austria <- readRDS("./data/austria")
austria_title <- readRDS("./data/austria_title")
austria_faces <- readRDS("./data/austria_faces")
austria_sex <- readRDS("./data/austria_sex")
austria_religion <- readRDS("./data/austria_religion")
austria_birth <- readRDS("./data/austria_birth")
austria_death <- readRDS("./data/austria_death")
austria_birthplace <- readRDS("./data/austria_birthplace")
austria_deathplace <-readRDS("./data/austria_deathplace")
austria <- left_join(x = austria, y = austria_title, by = "pageid") %>%
  left_join(x = ., y = austria_faces[,c(1,2)], by = "pageid") %>%
  left_join(x = ., y = austria_sex, by = "wikidataid") %>%
  left_join(x = ., y = austria_religion, by = "wikidataid") %>%
  left_join(x = ., y = austria_birth, by = "wikidataid") %>%
  left_join(x = ., y = austria_death, by = "wikidataid") %>%
  left_join(x = ., y = austria_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = austria_deathplace, by = "wikidataid")
colnames(austria)[c(10,12,16,17)] <- c("session", "wikititle", "birth", "death")
austria <- austria %>% select(country, pageid, wikidataid, wikititle, name, sex, 
                              ethnicity, religion, birth, death, birthplace, deathplace, 
                              session, party, constituency, session_start, session_end, 
                              service)
austria_core <- austria[!duplicated(austria$pageid), 1:12]
austria_political <- austria[c(2, 13:18)]
austria_core$sex <- as.character(austria_core$sex)
austria_core$religion <- as.character(austria_core$religion)
rm(austria, austria_title, austria_faces, austria_sex, austria_religion, austria_birth,
   austria_death, austria_birthplace, austria_deathplace)

# canadian house of commons
canada <- readRDS("./data/canada")
canada_title <- readRDS("./data/canada_title")
canada_faces <- readRDS("./data/canada_faces")
canada_sex <- readRDS("./data/canada_sex")
canada_religion <- readRDS("./data/canada_religion")
canada_birth <- readRDS("./data/canada_birth")
canada_death <- readRDS("./data/canada_death")
canada_birthplace <- readRDS("./data/canada_birthplace")
canada_deathplace <-readRDS("./data/canada_deathplace")
canada <- left_join(x = canada, y = canada_title, by = "pageid") %>%
  left_join(x = ., y = canada_faces[,c(1,2)], by = "pageid") %>%
  left_join(x = ., y = canada_sex, by = "wikidataid") %>%
  left_join(x = ., y = canada_religion, by = "wikidataid") %>%
  left_join(x = ., y = canada_birth, by = "wikidataid") %>%
  left_join(x = ., y = canada_death, by = "wikidataid") %>%
  left_join(x = ., y = canada_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = canada_deathplace, by = "wikidataid")
colnames(canada)[c(11,13,17,18)] <- c("session", "wikititle", "birth", "death")
canada <- canada %>% select(country, pageid, wikidataid, wikititle, name, sex, 
                            ethnicity, religion, birth, death, birthplace, deathplace, 
                            session, party, constituency, constituency2, session_start, 
                            session_end, service)
canada_core <- canada[!duplicated(canada$pageid), 1:12]
canada_political <- canada[c(2, 13:19)]
canada_core$sex <- as.character(canada_core$sex)
canada_core$religion <- as.character(canada_core$religion)
rm(canada, canada_title, canada_faces, canada_sex, canada_religion, canada_birth,
   canada_death, canada_birthplace, canada_deathplace)

# czech poslanecka snemovna
czech <- readRDS("./data/czech")
czech_title <- readRDS("./data/czech_title")
czech_faces <- readRDS("./data/czech_faces")
czech_sex <- readRDS("./data/czech_sex")
czech_religion <- readRDS("./data/czech_religion")
czech_birth <- readRDS("./data/czech_birth")
czech_death <- readRDS("./data/czech_death")
czech_birthplace <- readRDS("./data/czech_birthplace")
czech_deathplace <-readRDS("./data/czech_deathplace")
czech <- left_join(x = czech, y = czech_title, by = "pageid") %>%
  left_join(x = ., y = czech_faces[,c(1,2)], by = "pageid") %>%
  left_join(x = ., y = czech_sex, by = "wikidataid") %>%
  left_join(x = ., y = czech_religion, by = "wikidataid") %>%
  left_join(x = ., y = czech_birth, by = "wikidataid") %>%
  left_join(x = ., y = czech_death, by = "wikidataid") %>%
  left_join(x = ., y = czech_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = czech_deathplace, by = "wikidataid")
colnames(czech)[c(10,12,16,17)] <- c("session", "wikititle", "birth", "death")
czech <- czech %>% select(country, pageid, wikidataid, wikititle, name, sex, 
                          ethnicity, religion, birth, death, birthplace, deathplace, 
                          session, party, constituency, session_start, 
                          session_end, service)
czech_core <- czech[!duplicated(czech$pageid), 1:12]
czech_political <- czech[c(2, 13:18)]
czech_core$sex <- as.character(czech_core$sex)
czech_core$religion <- as.character(czech_core$religion)
rm(czech, czech_title, czech_faces, czech_sex, czech_religion, czech_birth,
   czech_death, czech_birthplace, czech_deathplace)

# french assemble
france <- readRDS("./data/france")
france_title <- readRDS("./data/france_title")
france_faces <- readRDS("./data/france_faces")
france_sex <- readRDS("./data/france_sex")
france_religion <- readRDS("./data/france_religion")
france_birth <- readRDS("./data/france_birth")
france_death <- readRDS("./data/france_death")
france_birthplace <- readRDS("./data/france_birthplace")
france_deathplace <-readRDS("./data/france_deathplace")
france <- left_join(x = france, y = france_title, by = "pageid") %>%
  left_join(x = ., y = france_faces[,c(1,2)], by = "pageid") %>%
  left_join(x = ., y = france_sex, by = "wikidataid") %>%
  left_join(x = ., y = france_religion, by = "wikidataid") %>%
  left_join(x = ., y = france_birth, by = "wikidataid") %>%
  left_join(x = ., y = france_death, by = "wikidataid") %>%
  left_join(x = ., y = france_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = france_deathplace, by = "wikidataid")
colnames(france)[c(9, 11, 15, 16)] <- c("session", "wikititle", "birth", "death")
france <- france %>% select(country, pageid, wikidataid, wikititle, name, sex, ethnicity,
                            religion, birth, death, birthplace, deathplace, session,
                            party, constituency,session_start, session_end)
france_core <- france[!duplicated(france$pageid), 1:12]
france_political <- france[c(2, 13:17)]
france_core$sex <- as.character(france_core$sex)
france_core$religion <- as.character(france_core$religion)
rm(france, france_title, france_faces, france_sex, france_religion, france_birth,
   france_death, france_birthplace, france_deathplace)

# german bundestag
germany <- readRDS("./data/germany")
germany_title <- readRDS("./data/germany_title")
germany_faces <- readRDS("./data/germany_faces")
germany_sex <- readRDS("./data/germany_sex")
germany_religion <- readRDS("./data/germany_religion")
germany_birth <- readRDS("./data/germany_birth")
germany_death <- readRDS("./data/germany_death")
germany_birthplace <- readRDS("./data/germany_birthplace")
germany_deathplace <-readRDS("./data/germany_deathplace")
germany <- left_join(x = germany, y = germany_title, by = "pageid") %>%
  left_join(x = ., y = germany_faces[,c(1,2)], by = "pageid") %>%
  left_join(x = ., y = germany_sex, by = "wikidataid") %>%
  left_join(x = ., y = germany_religion, by = "wikidataid") %>%
  left_join(x = ., y = germany_birth, by = "wikidataid") %>%
  left_join(x = ., y = germany_death, by = "wikidataid") %>%
  left_join(x = ., y = germany_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = germany_deathplace, by = "wikidataid")
colnames(germany)[c(11, 13, 17, 18)] <- c("session", "wikititle", "birth", "death")
germany <- germany %>% select(country, pageid, wikidataid, wikititle, name, sex, ethnicity,
                              religion, birth, death, birthplace, deathplace, session,
                              party, constituency, constituency2, session_start, 
                              session_end, service)
germany_core <- germany[!duplicated(germany$pageid), 1:12]
germany_political <- germany[c(2, 13:19)]
germany_core$sex <- as.character(germany_core$sex)
germany_core$religion <- as.character(germany_core$religion)
rm(germany, germany_title, germany_faces, germany_sex, germany_religion, germany_birth,
   germany_death, germany_birthplace, germany_deathplace)

# irish dail
ireland <- readRDS("./data/ireland")
ireland_title <- readRDS("./data/ireland_title")
ireland_faces <- readRDS("./data/ireland_faces")
ireland_sex <- readRDS("./data/ireland_sex")
ireland_religion <- readRDS("./data/ireland_religion")
ireland_birth <- readRDS("./data/ireland_birth")
ireland_death <- readRDS("./data/ireland_death")
ireland_birthplace <- readRDS("./data/ireland_birthplace")
ireland_deathplace <-readRDS("./data/ireland_deathplace")
ireland <- left_join(x = ireland, y = ireland_title, by = "pageid") %>%
  left_join(x = ., y = ireland_faces[,c(1,2)], by = "pageid") %>%
  left_join(x = ., y = ireland_sex, by = "wikidataid") %>%
  left_join(x = ., y = ireland_religion, by = "wikidataid") %>%
  left_join(x = ., y = ireland_birth, by = "wikidataid") %>%
  left_join(x = ., y = ireland_death, by = "wikidataid") %>%
  left_join(x = ., y = ireland_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = ireland_deathplace, by = "wikidataid")
colnames(ireland)[c(10, 12, 16, 17)] <- c("session", "wikititle", "birth", "death")
ireland <- ireland %>% select(country, pageid, wikidataid, wikititle, name, sex, ethnicity,
                              religion, birth, death, birthplace, deathplace, session,
                              party, constituency, session_start, session_end, service)
ireland_core <- ireland[!duplicated(ireland$pageid), 1:12]
ireland_political <- ireland[c(2, 13:18)]
ireland_core$sex <- as.character(ireland_core$sex)
ireland_core$religion <- as.character(ireland_core$religion)
rm(ireland, ireland_title, ireland_faces, ireland_sex, ireland_religion, ireland_birth,
   ireland_death, ireland_birthplace, ireland_deathplace)

# scottish parliament
scotland <- readRDS("./data/scotland")
scotland_title <- readRDS("./data/scotland_title")
scotland_faces <- readRDS("./data/scotland_faces")
scotland_sex <- readRDS("./data/scotland_sex")
scotland_religion <- readRDS("./data/scotland_religion")
scotland_birth <- readRDS("./data/scotland_birth")
scotland_death <- readRDS("./data/scotland_death")
scotland_birthplace <- readRDS("./data/scotland_birthplace")
scotland_deathplace <-readRDS("./data/scotland_deathplace")
scotland <- left_join(x = scotland, y = scotland_title, by = "pageid") %>%
  left_join(x = ., y = scotland_faces[,c(1,2)], by = "pageid") %>%
  left_join(x = ., y = scotland_sex, by = "wikidataid") %>%
  left_join(x = ., y = scotland_religion, by = "wikidataid") %>%
  left_join(x = ., y = scotland_birth, by = "wikidataid") %>%
  left_join(x = ., y = scotland_death, by = "wikidataid") %>%
  left_join(x = ., y = scotland_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = scotland_deathplace, by = "wikidataid")
colnames(scotland)[c(11,13,17,18)] <- c("session", "wikititle", "birth", "death")
scotland <- scotland %>% select(country, pageid, wikidataid, wikititle, name, sex, 
                                ethnicity, religion, birth, death, birthplace, deathplace, 
                                session, party, constituency, constituency2, session_start, 
                                session_end, service)
scotland_core <- scotland[!duplicated(scotland$pageid), 1:12]
scotland_political <- scotland[c(2, 13:19)]
scotland_core$sex <- as.character(scotland_core$sex)
scotland_core$religion <- as.character(scotland_core$religion)
rm(scotland, scotland_title, scotland_faces, scotland_sex, scotland_religion, scotland_birth,
   scotland_death, scotland_birthplace, scotland_deathplace)

# united kingdom parliament
uk <- readRDS("./data/uk")
uk_title <- readRDS("./data/uk_title")
uk_faces <- readRDS("./data/uk_faces")
uk_sex <- readRDS("./data/uk_sex")
uk_religion <- readRDS("./data/uk_religion")
uk_birth <- readRDS("./data/uk_birth")
uk_death <- readRDS("./data/uk_death")
uk_birthplace <- readRDS("./data/uk_birthplace")
uk_deathplace <-readRDS("./data/uk_deathplace")
uk <- left_join(x = uk, y = uk_title, by = "pageid") %>%
  left_join(x = ., y = uk_faces[,c(1,2)], by = "pageid") %>%
  left_join(x = ., y = uk_sex, by = "wikidataid") %>%
  left_join(x = ., y = uk_religion, by = "wikidataid") %>%
  left_join(x = ., y = uk_birth, by = "wikidataid") %>%
  left_join(x = ., y = uk_death, by = "wikidataid") %>%
  left_join(x = ., y = uk_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = uk_deathplace, by = "wikidataid")
colnames(uk)[c(10,12,16,17)] <- c("session", "wikititle", "birth", "death")
uk <- uk %>% select(country, pageid, wikidataid, wikititle, name, sex, 
                    ethnicity, religion, birth, death, birthplace, deathplace, 
                    session, party, constituency, session_start, 
                    session_end, service)
uk_core <- uk[!duplicated(uk$pageid), 1:12]
uk_political <- uk[c(2, 13:18)]
uk_core$sex <- as.character(uk_core$sex)
uk_core$religion <- as.character(uk_core$religion)
rm(uk, uk_title, uk_faces, uk_sex, uk_religion, uk_birth,
   uk_death, uk_birthplace, uk_deathplace)

# united states house
usah <- readRDS("./data/usah")
usah_title <- readRDS("./data/usah_title")
usah_faces <- readRDS("./data/usah_faces")
usah_sex <- readRDS("./data/usah_sex")
usah_religion <- readRDS("./data/usah_religion")
usah_birth <- readRDS("./data/usah_birth")
usah_death <- readRDS("./data/usah_death")
usah_birthplace <- readRDS("./data/usah_birthplace")
usah_deathplace <-readRDS("./data/usah_deathplace")
usah <- left_join(x = usah, y = usah_title, by = "pageid") %>%
  left_join(x = ., y = usah_faces[,c(1,2)], by = "pageid") %>%
  left_join(x = ., y = usah_sex, by = "wikidataid") %>%
  left_join(x = ., y = usah_religion, by = "wikidataid") %>%
  left_join(x = ., y = usah_birth, by = "wikidataid") %>%
  left_join(x = ., y = usah_death, by = "wikidataid") %>%
  left_join(x = ., y = usah_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = usah_deathplace, by = "wikidataid")
colnames(usah)[c(10, 12, 16, 17)] <- c("session", "wikititle", "birth", "death")
usah <- usah %>% select(country, pageid, wikidataid, wikititle, name, sex, ethnicity,
                        religion, birth, death, birthplace, deathplace, session,
                        party, constituency, session_start, session_end, service)
usah_core <- usah[!duplicated(usah$pageid), 1:12]
usah_political <- usah[c(2, 13:18)]
usah_core$sex <- as.character(usah_core$sex)
usah_core$religion <- as.character(usah_core$religion)
rm(usah, usah_title, usah_faces, usah_sex, usah_religion, usah_birth,
   usah_death, usah_birthplace, usah_deathplace)

# united states senate
usas <- readRDS("./data/usas")
usas_title <- readRDS("./data/usas_title")
usas_faces <- readRDS("./data/usas_faces")
usas_sex <- readRDS("./data/usas_sex")
usas_religion <- readRDS("./data/usas_religion")
usas_birth <- readRDS("./data/usas_birth")
usas_death <- readRDS("./data/usas_death")
usas_birthplace <- readRDS("./data/usas_birthplace")
usas_deathplace <-readRDS("./data/usas_deathplace")
usas <- left_join(x = usas, y = usas_title, by = "pageid") %>%
  left_join(x = ., y = usas_faces[,c(1,2)], by = "pageid") %>%
  left_join(x = ., y = usas_sex, by = "wikidataid") %>%
  left_join(x = ., y = usas_religion, by = "wikidataid") %>%
  left_join(x = ., y = usas_birth, by = "wikidataid") %>%
  left_join(x = ., y = usas_death, by = "wikidataid") %>%
  left_join(x = ., y = usas_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = usas_deathplace, by = "wikidataid")
colnames(usas)[c(10, 12, 16, 17)] <- c("session", "wikititle", "birth", "death")
usas <- usas %>% select(country, pageid, wikidataid, wikititle, name, sex, ethnicity,
                        religion, birth, death, birthplace, deathplace, session,
                        party, constituency, session_start, session_end, service)
usas_core <- usas[!duplicated(usas$pageid), 1:12]
usas_political <- usas[c(2, 13:18)]
usas_core$country <- "USA-S"
usas_core$sex <- as.character(usas_core$sex)
usas_core$religion <- as.character(usas_core$religion)
rm(usas, usas_title, usas_faces, usas_sex, usas_religion, usas_birth,
   usas_death, usas_birthplace, usas_deathplace)

# format political data -----------------------------------------------------------------

# austrian nationalrat
# SdP - Sozialdemokratische Partei
# CSP - Christlichsoziale Partei
# LBd - Landbund für Österreich
# NWB - Nationaler Wirtschaftsblock
# HB - Heimatblock
# GdP - Großdeutsche Volkspartei
# ÖVP - Österreichische Volkspartei
# SPÖ - Sozialdemokratische Partei Österreichs
# KPÖ - Kommunistische Partei Österreichs
# WdU - Wahlpartei der Unabhängigen
# VO - Wahlgemeinschaft Österreichische Volksopposition
# FPÖ - Freiheitliche Partei Österreichs
# GRÜNE - Die Grüne Alternative
# NEOS - Das Neue Österreich und Liberales Forum
# LIF - Liberales Forum
# BZÖ - Bündnis Zukunft Österreich
# STRONACH - Team Stronach für Österreich
# PILZ - Liste Peter Pilz
# none - independent
austria_political$party <- str_replace(austria_political$party, ", .+|/.+", "")
austria_political$party <- ifelse(austria_political$party == "F", "FPÖ",
                             ifelse(austria_political$party == "L", "LIF",
                             ifelse(austria_political$party == "CSP", "CsP",
                             ifelse(austria_political$party == "Stronach", "STRONACH",
                             ifelse(austria_political$party == "LB" | austria_political$party == "KuL", "KPÖ",
                             ifelse(austria_political$party == "Grüne", "GRÜNE",
                             ifelse(austria_political$party == "fraktionslos" | austria_political$party == "OK", "none",
                             austria_political$party)))))))
austria_political$constituency_id <- str_replace_all(austria_political$constituency, "Bundeswahlvorschlag,| .+", "")
austria_political$constituency_id <- ifelse(austria_political$constituency_id == "Bundeswahlvorschlag", "BWV",
                                            ifelse(austria_political$constituency_id == "", NA, austria_political$constituency_id))
austria_political$constituency <- str_replace_all(austria_political$constituency, 
                ",( )?Bundeswahlvorschlag|Bundeswahlvorschlag,|[[:digit:]][A-Z]{1}|[[:digit:]]{1}| \u2013 | \u2014 | - | \\(bis.+|\\[|\\]", "")
austria_political <- select(austria_political, pageid, session, party, constituency,
                            constituency_id, session_start, session_end, service)
austria_political$session <- as.integer(austria_political$session)
austria_political$session_start <- ymd(austria_political$session_start)
austria_political$session_end <- ymd(austria_political$session_end)
austria_political$service <- as.integer(austria_political$service)

# canadian house of commons
canada_political$party <- str_replace_all(canada_political$party, "^.+then |1|†+|Both |\\*.+|\\*", "")
canada_political$party <- ifelse(canada_political$party == "Liberal"|canada_political$party == "Liberal Reformer"|
       canada_political$party =="Liberal Party"|canada_political$party == "Liberalo", "Liberal Party of Canada",
 ifelse(canada_political$party == "Conservative" | canada_political$party == "conservative", "Conservative Party of Canada",
 ifelse(canada_political$party == "Independent Liberal"|canada_political$party == "Independent Conservative"|
        canada_political$party == "Independent Labour"|canada_political$party == "Independent Progressive"|
        canada_political$party == "Independent C.C.F."|canada_political$party == "Independent Progressive Conservative"|
        canada_political$party == "independent","Independent",
 ifelse(canada_political$party == "Labour", "Labour Party of Canada",
 ifelse(canada_political$party == "Progressive", "Progressive Party of Canada",
 ifelse(canada_political$party == "Unionist", "Unionist Party",
 ifelse(canada_political$party == "United Farmers", "United Farmers of Canada",
 ifelse(canada_political$party == "C.C.F."|canada_political$party == "New Party", "Co-operative Commonwealth Federation",
 ifelse(canada_political$party == "New Democracy"|canada_political$party == "Social Credit"|
        canada_political$party =="Ralliement Créditiste", "Social Credit Party",
 ifelse(canada_political$party == "Unity"|canada_political$party =="Labor Progressive"|
        canada_political$party == "Labor-Progressive", "Communist Party of Canada",
 ifelse(canada_political$party == "NDP"|canada_political$party =="New Democrat"|
        canada_political$party =="New Democrat to November 26, 982","New Democratic Party", 
 ifelse(canada_political$party == "Liberal-Conservative","Liberal-Conservative Party",
 ifelse(canada_political$party == "Anti-Confederate", "Anti-Confederation Party",
 ifelse(canada_political$party == "Reconstruction", "Reconstruction Party of Canada",
 ifelse(canada_political$party == "Reform", "Reform Party of Canada",
 ifelse(canada_political$party == "Green", "Green Party of Canada",
 ifelse(canada_political$party == "PC"|canada_political$party == "Progressive Conservative", "Progressive Conservative Party of Canada",
 ifelse(canada_political$party == "Canadian Alliance", "Canadian Reform Conservative Alliance",
        canada_political$party))))))))))))))))))
canada_political$session <- as.integer(canada_political$session)
canada_political$session_start <- ymd(canada_political$session_start)
canada_political$session_end <- ymd(canada_political$session_end)
canada_political$service <- as.integer(canada_political$service)
parl_meta_can <- readxl::read_xlsx("./data/parl_meta/parl-meta-can.xlsx")
parl_meta_can <- parl_meta_can[,c(1,4:10)]
colnames(parl_meta_can)[1] <- "session"
parl_meta_can$government <- ifelse(parl_meta_can$government == "Conservative", "Conservative Party of Canada",
                             ifelse(parl_meta_can$government == "Liberal", "Liberal Party of Canada",
                              ifelse(parl_meta_can$government == "Liberal|Conservative", "Liberal Party of Canada|Conservative Party of Canada",
                               ifelse(parl_meta_can$government == "Progressive Conservative", "Progressive Conservative Party of Canada",
                                ifelse(parl_meta_can$government == "Unionist",  "Unionist Party", parl_meta_can$government)))))
parl_meta_can$opposition <-  ifelse(parl_meta_can$opposition == "Conservative", "Conservative Party of Canada",
                              ifelse(parl_meta_can$opposition == "Liberal", "Liberal Party of Canada",
                               ifelse(parl_meta_can$opposition == "Liberal-Conservative", "Liberal-Conservative Party",
                                ifelse(parl_meta_can$opposition == "Laurier Liberal", "Laurier Liberals",
                                 ifelse(parl_meta_can$opposition== "Conservative|Liberal", "Conservative Party of Canada|Liberal Party of Canada",
                                  ifelse(parl_meta_can$opposition == "Progressive Conservative", "Progressive Conservative Party of Canada",
                                   ifelse(parl_meta_can$opposition == "Bloc Quebecois", "Bloc Québécois",
                                    ifelse(parl_meta_can$opposition == "Reform",  "Reform Party of Canada",
                                     ifelse(parl_meta_can$opposition == "Canadian Alliance", "Canadian Reform Conservative Alliance",
                                      ifelse(parl_meta_can$opposition == "New Democratic", "New Democratic Party",
                                           parl_meta_can$opposition))))))))))
parl_meta_can$third <- parl_meta_can$third %>%
  str_replace_all(.,"Progressive Conservative", "Progressive Conservative Party of Canada") %>%
  str_replace_all(.,"Liberal-Conservative", "Liberal-Conservative Party") %>% 
  str_replace_all(., "Bloc Quebecois", "Bloc Québécois") %>% 
  str_replace_all(., "^Reform", "Reform Party of Canada") %>%
  str_replace_all(., "New Party", "Co-operative Commonwealth Federation") %>%
  str_replace_all(., "Canadian Alliance", "Canadian Reform Conservative Alliance") %>%
  str_replace_all(., "Unity|Labor Progressive|Labor-Progressiv", "Communist Party of Canada") %>%
  str_replace_all(., "Social Credit|New Democracy|Ralliement creditiste", "Social Credit Party") %>%
  str_replace_all(., "\\|Independent Conservative\\|", "\\|Independent\\|") %>%
  str_replace_all(., "\\|Independent-Conservative$", "\\|Independent\\|") %>%
  str_replace_all(., "\\|Independent Liberal\\|", "\\|Independent\\|") %>%
  str_replace_all(., "\\|Independent Liberal$", "\\|Independent\\|") %>%
  str_replace_all(., "\\|Independent Conservative$", "\\|Independent\\|") %>%
  str_replace_all(., "^Independent Labour\\|", "\\|Independent\\|") %>%
  str_replace_all(., "^Independent Labour\\|", "\\|Independent\\|") %>%
  str_replace_all(., "^Labour$", "\\|Labour Party of Canada\\|") %>%
  str_replace_all(., "\\|Labour\\|", "\\|Labour Party of Canada\\|") %>%
  str_replace_all(., "^Progressive\\|", "Progressive Party of Canada\\|") %>%
  str_replace_all(., "\\|Progressive\\|", "\\|Progressive Party of Canada\\|") %>%
  str_replace_all(., "^Liberal$", "Liberal Party of Canada\\|") %>% 
  str_replace_all(., "^Liberal\\|", "Liberal Party of Canada\\|") %>% 
  str_replace_all(., "\\|Conservative\\|", "\\|Conservative Party of Canada\\|") 
parl_meta_can$unrecognized <- ifelse(parl_meta_can$unrecognized == "Ralliement creditiste, Social Credit"|
                                       parl_meta_can$unrecognized == "Social Credit", "Social Credit Party",
                               ifelse(parl_meta_can$unrecognized == "New Democratic|Progressive Conservative", 
                                  "New Democratic Party|Progressive Conservative Party of Canada",
                                ifelse(parl_meta_can$unrecognized == "Green|Progressive Conservative", 
                                    "Green Party of Canada|Progressive Conservative Party of Canada",
                                 ifelse(parl_meta_can$unrecognized == "Progressive Conservative", "Progressive Conservative Party of Canada",
                                  ifelse(parl_meta_can$unrecognized == "Bloc Quebecois|Green|Strength in Democracy|Progressive Conservative",
                                      "Bloc Québécois|Green Party of Canada|Progressive Conservative Party of Canada",
                                   ifelse(parl_meta_can$unrecognized == "Quebec Debout|Bloc Quebecois|Green|Co-operative Commonwealth|People's",
                                      "Bloc Québécois|Green Party of Canada|Co-operative Commonwealth Federation",parl_meta_can$unrecognized))))))
canada_political <- left_join(x = canada_political, y = parl_meta_can, by = "session")
rm(parl_meta_can)
canada_political$government <- str_detect(canada_political$government, canada_political$party)
canada_political$opposition <- str_detect(canada_political$opposition, canada_political$party)
canada_political$third <- str_detect(canada_political$third, canada_political$party)
canada_political$unrecognized <- str_detect(canada_political$unrecognized, canada_political$party)
canada_political <- left_join(x = canada_political, y = canada_core[,c(2,3)], by = "pageid")
canada_political$prime_minister <- str_detect(canada_political$prime_minister, canada_political$wikidataid)
canada_political$opposition_leader <- str_detect(canada_political$opposition_leader, canada_political$wikidataid)
canada_political$speaker <- str_detect(canada_political$speaker, canada_political$wikidataid)
canada_political$unrecognized[which(is.na(canada_political$unrecognized))] <- FALSE
canada_political <- select(canada_political, -c(wikidataid))
canada_political$session <- as.integer(canada_political$session)

# czech poslanecka snemovna
# ODS - Občanská demokratická strana (Civic Democratic Party)
# ANO - Akce nespokojených občanů (Action of Dissatisfied Citizens)
# Piráti - Česká pirátská strana (Czech Pirate Party)
# SPD - Svoboda a přímá demokracie - Tomio Okamura (Freedom and Direct Democracy – Tomio Okamura)
# KSČM - Komunistická strana Čech a Moravy (Communist Party of Bohemia and Moravia)
# ČSSD - Česká strana sociálně demokratická (Czech Social Democratic Party)
# KDU–ČSL - Křesťanská a demokratická unie – Československá strana lidová (Christian and Democratic Union – Czechoslovak People's Party)
# TOP_09 - Tradice Odpovědnost Prosperita (Tradition Responsibility Prosperity)
# STAN - Starostové a nezávislí (Mayors and Independents)
# Úsvit - Úsvit – Národní koalice (Dawn – National Coalition)
# VV - Věci veřejné (Public Affairs)
# SZ - Strana zelených (Green Party)
# US-DEU - Unie Svobody–Demokratická unie (Freedom Union–Democratic Union)
# LSU - Liberálně sociální unie (Liberal-Social Union)
# HSD–SMS - Hnutí za samosprávnou demokracii–Společnost pro Moravu a Slezsko (Movement for Autonomous Democracy–Party for Moravia and Silesia)
# SPR-RSČ - Sdružení pro republiku - Republikánská strana Československa (Rally for the Republic – Republican Party of Czechoslovakia)
# KDS - Křesťanskodemokratická strana (Christian Democratic Party)
# MNS - Moravská národní strana (Moravian National Party)
# ODA - Občanská demokratická aliance (Civic Democratic Alliance)
# ČSS - Česká strana národně sociální (Czech National Social Party)
# DSP - Demokratická strana práce (Democratic Labour Party)
czech_political$party <- ifelse(czech_political$party == "LB"|czech_political$party == "LA"|
                                str_detect(czech_political$party, "KS"), "KSČM",
                          ifelse(str_detect(czech_political$party, "HRP"), "HSD-SMS",
                           ifelse(czech_political$party == "ZS", "LSU",
                            ifelse(str_detect(czech_political$party, "KDU-"), "KDU-ČSL",
                             ifelse(str_detect(czech_political$party, "SSD"), "ČSSD",
                              ifelse(str_detect(czech_political$party, "SPR"), "SPR-RSČ",
                               ifelse(czech_political$party == "US", "US-DEU",
                                ifelse(czech_political$party == "ANO 2011", "ANO",
                                 czech_political$party))))))))
czech_political$session <- as.integer(czech_political$session)
czech_political$session_start <- ymd(czech_political$session_start)
czech_political$session_end <- ymd(czech_political$session_end)
czech_political$service <- as.integer(czech_political$service)

# french assemble
# EAS - Regroupement national pour l'unité de la République (National Grouping for the Unity of the Republic)
# IPAS - Centre national des indépendants et paysans (National Centre of Independents and Peasants)
# UNR - Union pour la nouvelle République (Union for the New Republic)
# RPCD - Républicains populaires et du Centre démocratique (Popular Republicans and Democratic Center)
# PCF - Parti communiste français (French Communist Party)
# SFIO - Section française de l'Internationale ouvrière (French Section of the Workers' International)
# ED - Entente démocratique (Democratic agreement)
# CD - Centre démocratique (Democratic Center)
# RD - Rassemblement démocratique (Democratic Rally)
# RI - Fédération nationale des républicains indépendants (Independent Republicans)
# UNR-UDT - Union pour la nouvelle République-Union démocratique du travail (Union for the New Democratic-Union of Labor)
# FGDS - Fédération de la gauche démocrate et socialiste (Federation of the Democratic and Socialist Left)
# PDM - Progrès et démocratie moderne (Progress and Modern Democracy)
# PSRG - Parti socialiste et Radicaux de gauche (Socialist Party and Radical Left)
# UC - Union centriste (Centrist Union)
# UDR - Union des démocrates pour la République (Union of Democrats for the Republic)
# PS - Parti socialiste (Socialist Party)
# UDF - Union pour la démocratie française (Union for French Democracy)
# RPR - Rassemblement pour la République (Rally for the Republic)
# FN - Front national (National Rally)
# UDC - Union du centre (Union of the Center)
# RCV - Groupe radical, citoyen et vert (Radical group, citizen and green)
# DL - Démocratie libérale (Liberal Democracy)
# UMP - Union pour un mouvement populaire (Union for a Popular Movement)
# NC - Groupe Nouveau Centre (Group New Center)
# SRC - Socialiste, radical, citoyen et divers gauche (Socialist, radical, citizen and various left)
# GDR - Gauche démocrate et républicaine (Democratic and Republican Left)
# SER - Groupe socialiste, écologiste et républicain (Socialist, Ecologist and Republican Group)
# RRDP - Groupe radical, républicain, démocrate et progressiste (Radical, Republican, Democratic and Progressive)
# UDI - Union des démocrates et indépendants (Union of Democrats and Independent)
# LR - Les Républicains (The Republicans)
# REM - La République en marche (The Republic on the move)
# PaC - Pè a Corsica
# DVG - Divers gauche (Miscellaneous left)
# E! - Ensemble!
# RDM - Rassemblement démocratique pour la Martinique (Martinican Democratic Rally)
# MoDem - Mouvement démocrate et apparentés (Democratic Movement and affiliated group)
# DVD - Divers droite (Miscellaneous right)
# GÉ - Génération écologie (Ecology Generation)
# PÉ - Parti écologiste (Ecologist Party)
# PPM - Parti progressiste martiniquais (Martinican Progressive Party)
# MPR - Pour La Réunion (For Reunion)
# LC - Les Centristes (The Centrists)
# PSG - Parti socialiste guyanais (Guianese Socialist Party)
# FD - Front démocrate (Democratic Front)
# MRC - Mouvement républicain et citoyen (Citizen and Republican Movement)
# MRSL - Mouvement radical, social et libéral (Radical Movement)
# La France insoumise - (Unsubmissive France)
# RE974 - Rézistans Égalité 974
# TH - Tavini huiraatira
# MdP - Mouvement des progressistes (Movement of Progressives)
# MIM - Mouvement indépendantiste martiniquais (Martinican Independence Movement)
# Agir - Agir, la droite constructive (Act, the Constructive Right)
# CE - Calédonie ensemble (Caledonia Together)
# AC - Alliance centriste (Centrist Alliance)
# LP - Les Patriotes (The Patriots)
# PP - Place publique (Public place)
# RDS - ?
# PG - Parti de gauche (Left Party)
# DLF - Debout la France (France Arise)
# Résistons!
# Picardie-Debout
# Génération.s
# LS - Ligue du Sud (Southern League)
# RDS - Réformateurs démocrates sociaux (Social Democratic Reformers)
france_political$party <- str_replace(france_political$party, "a\\.( )?|\\(app\\.\\)| \\(Les Verts\\)|\\*", "")
france_political$party <- str_replace(france_political$party, "Non.+|NI|DIV", "Non-Inscrit")
france_political$party <- str_trim(france_political$party)
france_political$party <- ifelse(france_political$party == "Communiste" | france_political$party == "C", "PCF",
                          ifelse(france_political$party == "S" | france_political$party == "Socialiste", "SFIO", 
                           ifelse(france_political$party == "UD-Ve","UDR",
                            ifelse(france_political$party == "I", "Non-Inscrit",
                                   france_political$party))))
france_political$party[which(france_political$party == "")] <- NA
france_political$constituency <- str_replace(france_political$constituency, "e\\)|re\\)", ")")
france_political$session <- as.integer(france_political$session)
france_political$session_start <- ymd(france_political$session_start)
france_political$session_end <- ymd(france_political$session_end)

# german bundestag
# AfD - Alternative für Deutschland (Alternative for Germany)
# AL - Alternative Liste für Demokratie und Umweltschutz (Alternative List for Democracy and Environmental Protection)
# BP - Bayernpartei (Bavaria Party)
# BÜNDNIS 90/DIE GRÜNEN - (Alliance 90/The Greens)
# CDU - Christlich Demokratische Union Deutschlands (Christian Democratic Union of Germany)
# CSU - Christlich-Soziale Union in Bayern (Christian Social Union in Bavaria)
# CVP - Christliche Volkspartei des Saarlandes (Christian People's Party of the Saarland)
# DIE LINKE - Die Linke (The Left)
# DKP-DRP - Deutsche Konservative Partei – Deutsche Rechtspartei (German Right Party)
# DP - Deutsche Partei (German Party)
# DPS - Demokratische Partei Saar (Democratic Party Saar)
# DSU - Deutsche Soziale Union (German Social Union)
# FDP - Freie Demokratische Partei (Free Democratic Party)
# FDV - Freie Deutsche Volkspartei (Free Germany People's Party)
# GB/BHE - Gesamtdeutscher Block/Bund der Heimatvertriebenen und Entrechteten (All-German Bloc/League of Expellees and Deprived of Rights)
# KPD - Kommunistische Partei Deutschlands (Communist Party of Germany)
# PDS - Partei des Demokratischen Sozialismus (Party of Democratic Socialism)
# SPD - Sozialdemokratische Partei Deutschlands (Social Democratic Party of Germany)
# WAV - Wirtschaftliche Aufbau-Vereinigung (Economic Reconstruction Union)
# ZENTRUM - Deutsche Zentrumspartei (Centre Party)
germany_political$party <- str_replace(germany_political$party, " \\(GDP\\)", "")
germany_political$party <- ifelse(germany_political$party == "Bündnis 90" | germany_political$party == "Die Grünen" |
                                  germany_political$party == "Bündnis 90/Die Grünen" | germany_political$party == "GRÜNE" |
                                    germany_political$party == "parteilos(Grüne DDR)"| germany_political$party == "Grüne DDR", "BÜNDNIS 90/DIE GRÜNEN" ,
                           ifelse(germany_political$party == "SSW", "SPD",
                           ifelse(germany_political$party == "Die Linke"|germany_political$party == "LINKE", "DIE LINKE",
                           ifelse(germany_political$party == "unabhängig" | germany_political$party == "fraktionslos", "none",
                                  germany_political$party))))
germany_political$constituency <- str_replace(germany_political$constituency, "Rheinlald-Pfalz", "Rheinland-Pfalz")
germany_political$constituency <- ifelse(germany_political$constituency == "Baden" |
                                           germany_political$constituency == "Württemberg-Hohenzollern"|
                                          germany_political$constituency == "Württemberg-Baden", "Baden-Württemberg",
                                         germany_political$constituency)
colnames(germany_political)[c(4,5)] <- c("constituency2", "constituency")
germany_political <- select(germany_political, pageid, session, party, constituency, 
                            constituency2, session_start, session_end, service)
germany_political$constituency[which(is.na(germany_political$constituency))] <- "Landesliste"
germany_political$session <- as.integer(germany_political$session)
germany_political$session_start <- ymd(germany_political$session_start)
germany_political$session_end <- ymd(germany_political$session_end)
germany_political$service <- as.integer(germany_political$service)

# irish dail
# Clann na Poblachta (Family/Children of the Republic)
# Clann na Talmhan (Family/Children of the land)
# Cumann na nGaedheal (Society of the Gaels)
# Sinn Féin (We Ourselves)
# Fianna Fáil (Soldiers of Destiny)
# Fine Gael (Tribe of the Irish)
ireland_political$party <- str_replace(ireland_political$party, " \\(Pro-Treaty\\)| \\(Anti-Treaty\\)| \\(Workers' Party\\)", "")
ireland_political$party <- ifelse(ireland_political$party == "AAA\u2013PBP", "Solidarity–People Before Profit",
                            ifelse(ireland_political$party == "Businessmen's Party", "Business and Professional Group",
                             ifelse(str_detect(ireland_political$party, "Fianna"), "Fianna Fáil",
                              ifelse(str_detect(ireland_political$party, "Independent Fianna"), "Independent Fianna Fáil",
                               ifelse(ireland_political$party == "Irish Parliamentary", "Irish Parliamentary Party",
                                ifelse(ireland_political$party == "Labour Unionist"| ireland_political$party =="UUP", "Ulster Unionist Labour Association",
                                 ifelse(ireland_political$party == "Nationalist", "Nationalist Party",
                                        ireland_political$party)))))))
ireland_political$party <- ifelse(str_detect(ireland_political$party, "The Workers' Party"), "Workers' Party",
                                  ireland_political$party)
ireland_political$party <- ifelse(str_detect(ireland_political$party, "Sinn"), "Sinn Féin",
                                  ireland_political$party)
ireland_political$session <- as.integer(ireland_political$session)
ireland_political$session_start <- ymd(ireland_political$session_start)
ireland_political$session_end <- ymd(ireland_political$session_end)
ireland_political$service <- as.integer(ireland_political$service)

# scottish parliament
scotland_political$party <- ifelse(scotland_political$party == "Solidarity", "Solidarity – Scotland's Socialist Movement",
                                   scotland_political$party)
scotland_political$session <- as.integer(scotland_political$session)
scotland_political$session_start <- ymd(scotland_political$session_start)
scotland_political$session_end <- ymd(scotland_political$session_end)
scotland_political$service <- as.integer(scotland_political$service)

# united kingdom parliament
# Plaid Cymru - (Party of Wales)
# Sinn Féin - (We Ourselves)
uk_political$party <- str_replace_all(uk_political$party, " \\&.+|\\(.+|, then.+| a$", "")
uk_political$party <- ifelse(uk_political$party == "L Co-op" | uk_political$party == "Labour Co-op" | 
                               uk_political$party == "Labour Co-operative" | uk_political$party == "Co-op" | 
                               uk_political$party == "Labour/Co-operative", "Labour and Co-operative",
                             uk_political$party)
uk_political$party <- ifelse(uk_political$party == "Ind" | uk_political$party == "Indep. National Liberal" | 
                               uk_political$party == "Indep. Progressive" | uk_political$party == "Independent Conservative" | 
                               uk_political$party == "Independent Labour" | uk_political$party == "Independent Republican" | 
                               uk_political$party == "Independent Ulster Unionist" | uk_political$party =="Independent Unionist" | 
                               uk_political$party == "National" | uk_political$party == "None " | uk_political$party == "None - Speaker" | uk_political$party == "Speaker" | uk_political$party == "Speaker " | uk_political$party == "Deputy Speaker", "Independent", uk_political$party)
uk_political$party <- ifelse(uk_political$party == "Ind. Labour Party", "Independent Labour Party",
                             ifelse(uk_political$party == "Common Wealth", "Common Wealth Party",
                                   ifelse(uk_political$party == "Republican Labour", "Republican Labour Party",
                                          ifelse(uk_political$party == "SNP", "Scottish National Party",
                                                 ifelse(uk_political$party == "Vanguard Progressive Unionist", "Vanguard Unionist Progressive Party",
                                                        ifelse(uk_political$party == "Communist", "Communist Party of Great Britain",
                                                               ifelse(uk_political$party == "United Kingdom Unionist", "UK Unionist",
                                                                      ifelse(uk_political$party == "UPUP", "Ulster Popular Unionist",
                                                                             uk_political$party))))))))
uk_political$party <- ifelse(uk_political$party == "UUP" | uk_political$party == "Official Unionist", "Ulster Unionist", uk_political$party)
uk_political$party <- ifelse(uk_political$party == "Irish Nationalist", "Nationalist Party",
                             ifelse(uk_political$party == "Liberal" | uk_political$party ==  "Liberal and Conservative", "Liberal Party",
                                    ifelse(uk_political$party == "Labour"| uk_political$party == "L" | uk_political$party == "Laboura", "Labour Party",
                                           uk_political$party)))
uk_political$party <- ifelse(uk_political$party == "Liberal National" | uk_political$party == "Nat Lib and Conservative" | uk_political$party == "National Liberal and Conservative" | uk_political$party == "Nat Lib","National Liberal", uk_political$party)
uk_political$party <- ifelse(uk_political$party == "LD"|uk_political$party == "Liberal Democrat", "Liberal Democrats", uk_political$party)
uk_political$party <- ifelse(uk_political$party == "Conservative and Nat Lib" | uk_political$party == "Conservative" | uk_political$party == "C" | uk_political$party == "Conservative " | uk_political$party == "Conservatuve"  , "Conservative Party", uk_political$party)
uk_political$party <- ifelse(uk_political$party == "DUP" | uk_political$party == "Democratic Unionist", "Democratic Unionist Party", uk_political$party)
uk_political$party <- ifelse(uk_political$party == "Social Democratic and Labour"|uk_political$party == "SDLP", "Social Democratic and Labour Party", uk_political$party)
uk_political$party <- ifelse(uk_political$party == "Plaid Cymru/Green" | uk_political$party == "PC", "Plaid Cymru",
                             ifelse(uk_political$party == "SF", "Sinn Féin", uk_political$party))
uk_political$party <- ifelse(str_detect(uk_political$party, "Sinn"), "Sinn Féin", uk_political$party)
uk_political$party <- ifelse(uk_political$party == "Social Democratic" | uk_political$party == "Social Democrat", "Social Democratic Party",
                             ifelse(uk_political$party == "RESPECT" | uk_political$party == "Respect", "Respect Party",
                                    ifelse(uk_political$party =="Health Concern", "Independent Community and Health Concern",
                                           ifelse(uk_political$party == "G"| uk_political$party == "Green", "Green Party of England and Wales",
                                                  ifelse(uk_political$party == "APNI", "Alliance Party of Northern Ireland",
                                                         ifelse(uk_political$party == "UKIP","UK Independence Party",
                                                                uk_political$party))))))
uk_political$session <- as.integer(uk_political$session)
uk_political$session_start <- ymd(uk_political$session_start)
uk_political$session_end <- ymd(uk_political$session_end)
uk_political$service <- as.integer(uk_political$service)

# united states house
# D - Democratic Party
# R - Republican Party
# DFL - Minnesota Democratic–Farmer–Labor Party
# PNP/R - New Progressive Party (Puerto Rico)/Republican
# PNP/D - New Progressive Party (Puerto Rico)/Democratic
# D/PPD - Popular Democratic Party (Puerto Rico)
# DNPL - North Dakota Democratic–Nonpartisan League Party
# DL - Liberal Party of New York
# AL - American Labor Party
# Lib - Liberal Party of New York
# FL - Farmer–Labor Party
# Prog - Progressive Party
# Socialist - Socialist Party (Puerto Rico)
# NaC - Nacionalista Party
# UPR - Union of Puerto Rico
# Soc - Socialist Party of America
# Proh - Prohibition Party
# Unionist - Unionist Party (Puerto Rico)
# Fed - Federalist Party
# Pop - People's Party
# SR = Silver Republican Party
# S = Silver Party
# Home Rule - Hawaiian Independent Party
# L - Socialist Labor Party of America
# NG - Greenback Party
# RA - Readjuster Party
# AMon - Anti-Monopoly Party
# NU - National Union Party
# LR - Liberal Republican Party
# C - COnservative Party
# U - Unionist Party, later Unconditional Unionist Party
# CU - Constitutional Union Party
# O - Opposition Party
# A - Native American Party
# ALD - Lecompton Constitution
# FS - Free Soil Party
# W - Whig Party
# SRi - States' Rights Party
# LO - Law and Order Party of Rhode Island
# AM - Anti-Masonic Party
# N - Nullifier Party
# J - Jacksonian Democracy
# AJ - National Republican Party
# DR - Democratic-Republican Party
# F - Federalist Party (Pro-Administration Party)
# AAP - Anti-Administration Party (DR)
# PAP - Pro-Administration Party (PAP)
usah_political$party <- str_replace_all(usah_political$party, "\\, then.+|until.+|then.+| to.+|[[:digit:]].+|\\.$|^(Territory|Northwest|William|Mississippi|Indiana|Duke|Orleans|Illinois|Missouri|Rufus|Nathaniel|John|Alabama|Arkansas|Michigan|Solomon|Florida|Ambrose|Henry|Minnesota|Utah|Nebraska|Nevada|Dakota|Lewis|Hawaii|Puerto|Philippines|George|Clark|Johnny|Jesse|Benjamin).+|Buck|Bud|Bo|Chip|Jake|Kika|Pete|Tip|Duke", "")
usah_political$party <- ifelse(usah_political$party == "I" | usah_political$party == "Ind" | usah_political$party == "Ind.", "Independent",
                        ifelse(usah_political$party == "ID" | usah_political$party == "Independent D", "Independent Democrat",
                        ifelse(usah_political$party == "congressman", "D",
                        ifelse(usah_political$party == "IW", "Independent Whig",
                        ifelse(usah_political$party == "", NA,
                               usah_political$party)))))
usah_political$party <- ifelse(usah_political$party == "IR" | usah_political$party == "ID" | usah_political$party == "Independent D" | 
                                 usah_political$party == "Independent Democrat" | usah_political$party == "Independent Republican" | 
                                 usah_political$party == "Independent Whig", "Independent",usah_political$party)
usah_political$party <- ifelse(usah_political$party == "D "| usah_political$party == "D-L"| usah_political$party == "States Rights D",
                               "D",usah_political$party)
usah_political$party <- ifelse(usah_political$party == "R-L"| usah_political$party == "Nonpartisan Republican"| 
                                 usah_political$party == "Pat" | usah_political$party == "Rep" | usah_political$party == "CR", "R", usah_political$party)
usah_political$party <- ifelse(usah_political$party == "NPP" | usah_political$party == "D/PNP", "PNP/D",  usah_political$party)
usah_political$party <- ifelse(usah_political$party == "R and PNP"| usah_political$party == "PNP", "PNP/R",  usah_political$party)
usah_political$party <- ifelse(usah_political$party == "D/PPD" | usah_political$party == "PD" | usah_political$party == "PPD/D", "PPD", usah_political$party)
usah_political$party <- ifelse(usah_political$party == "NPL", "DNPL",
                               ifelse(usah_political$party == "Nac", "NaC",
                                      ifelse(usah_political$party == "Fed., R","Fed",
                                             ifelse(usah_political$party == "FS","S",
                                                    ifelse(usah_political$party == "GB","NG",
                                                           ifelse(usah_political$party == "KN","A",
                                                                  ifelse(usah_political$party == "States-Rights Whig", "W",
                                                                         ifelse(usah_political$party == "Anti-J","AJ",
                                                                                ifelse(usah_political$party == "American Labor","AL",
                                                                                ifelse(usah_political$party == "Anti-M","AM",
                                                                                       ifelse(usah_political$party == "Prohibitionist", "Proh",
                                                                                       usah_political$party)))))))))))
usah_political$party <- ifelse(usah_political$party == "Coalitionist", "Socialist", usah_political$party)
usah_political$party <- ifelse(usah_political$party == "S" & usah_political$session > 57, "Socialist", usah_political$party)
usah_political$party <- ifelse(usah_political$party == "J-DR"| usah_political$party == "A-DR"|
                                 usah_political$party == "C-DR", "DR", usah_political$party)
usah_political$party <- ifelse(usah_political$party == "C-F" | usah_political$party == "A-F" |
                                 usah_political$party == "J-F", "F", usah_political$party)
usah_political$party <- ifelse(usah_political$party == "Progressive" | usah_political$party == "Prog. R" | 
                                 usah_political$party == "Prog R.", "Prog", usah_political$party)
usah_political$party <- ifelse(usah_political$party == "P" & usah_political$session > 57, "Prog", usah_political$party)
usah_political$party <- ifelse(usah_political$party == "Populist", "Pop",
                               ifelse(usah_political$party == "P" & usah_political$session <= 57 & usah_political$session > 3, "Pop",
                                      usah_political$party))
usah_political$party <- ifelse(usah_political$party == "U" & usah_political$session > 39, "NU", usah_political$party)
usah_political$party <- ifelse(usah_political$party == "UU", "U", usah_political$party)
usah_political$party <- ifelse(usah_political$party == "SR" & usah_political$session <= 32, "SRi", usah_political$party)
usah_political$party <- ifelse(usah_political$party == "A" & usah_political$session <= 20 & usah_political$session > 3, "AJ", usah_political$party)
usah_political$party <- ifelse(usah_political$party == "P" & usah_political$session <= 3, "PAP", usah_political$party)
usah_political$party <- ifelse(usah_political$party == "A" & usah_political$session <= 3, "AAP", usah_political$party)
usah_political$party <- ifelse(usah_political$party == "AM" & usah_political$session == 48, "AMon", usah_political$party)
usah_political$session <- as.integer(usah_political$session)
usah_political$session_start <- mdy(usah_political$session_start)
usah_political$session_end <- mdy(usah_political$session_end)
usah_political$service <- as.integer(usah_political$service)

parl_meta_usah <- readxl::read_xlsx("./data/parl_meta/parl-meta-usah.xlsx")
parl_meta_usah <- parl_meta_usah[,c(1,5:39)]
colnames(parl_meta_usah)[c(1,2,3)] <- c("session","house_majority","house_speaker")
parl_meta_usah$house_majority <- ifelse(parl_meta_usah$house_majority == "Pro-Administration", "PAP",
                                        ifelse(parl_meta_usah$house_majority == "Anti-Administration", "AAP", 
                                               ifelse(parl_meta_usah$house_majority == "Democratic-Republican", "DR", 
                                                      ifelse(parl_meta_usah$house_majority == "Federalist", "F",
                                                             ifelse(parl_meta_usah$house_majority == "Anti-Jackson", "AJ",
                                                                      ifelse(parl_meta_usah$house_majority == "Jacksonian", "jacksonian",
                                                                               ifelse(parl_meta_usah$house_majority == "Democratic", "democratic",
                                                                                        ifelse(parl_meta_usah$house_majority == "Whig", "W",
                                                                                                 ifelse(parl_meta_usah$house_majority == "none: Democratic plurality", "none",
                                                                                                          ifelse(parl_meta_usah$house_majority == "Opposition coalition", "native|republican|opposition",
                                                                                                                   ifelse(parl_meta_usah$house_majority == "Republican led coalition", "republican",
                                                                                                                            ifelse(parl_meta_usah$house_majority == "Republican", "republican",
                                                                                                                                     ifelse(parl_meta_usah$house_majority == "Democratic coalition", "democratic",
                                                                                                                                              ifelse(parl_meta_usah$house_majority == "Democratic (coalition)", "democratic|Prog|Soc",
                                                                                                                                                     ifelse(parl_meta_usah$house_majority == "Republican|Democratic", "republican|democratic",
                                                                                                                                                            parl_meta_usah$house_majority)))))))))))))))
usah_political$party <- ifelse(usah_political$party == "D", "democratic",
                               ifelse(usah_political$party == "R", "republican",
                                      ifelse(usah_political$party == "J", "jacksonian",
                                             ifelse(usah_political$party == "O", "opposition",
                                                    ifelse(usah_political$party == "A", "native",
                                                           usah_political$party)))))
usah_political <- left_join(x = usah_political, y = parl_meta_usah, by = "session")
rm(parl_meta_usah)
usah_political$house_majority <- str_detect(usah_political$house_majority, usah_political$party)
usah_political <- left_join(x = usah_political, y = usah_core[,c(2,3)], by = "pageid")
usah_political$house_speaker <- str_detect(usah_political$house_speaker, usah_political$wikidataid)
usah_political$house_assistant_to_speaker <- str_detect(usah_political$house_assistant_to_speaker, usah_political$wikidataid)
usah_political$house_republican_conference_chair <- str_detect(usah_political$house_republican_conference_chair, usah_political$wikidataid)
usah_political$house_republican_conference_vice_chair <- str_detect(usah_political$house_republican_conference_vice_chair, usah_political$wikidataid)
usah_political$house_conference_secretary <- str_detect(usah_political$house_conference_secretary, usah_political$wikidataid)
usah_political$house_democratic_caucus_chairman <- str_detect(usah_political$house_democratic_caucus_chairman, usah_political$wikidataid)
usah_political$house_democratic_caucus_vice_chairman <- str_detect(usah_political$house_democratic_caucus_vice_chairman, usah_political$wikidataid)
usah_political$house_steering_committee_chair <- str_detect(usah_political$house_steering_committee_chair, usah_political$wikidataid)
usah_political$house_policy_committee_chair <- str_detect(usah_political$house_policy_committee_chair, usah_political$wikidataid)
usah_political$house_republican_policy_committee_chair <- str_detect(usah_political$house_republican_policy_committee_chair, usah_political$wikidataid)
usah_political$house_campaign_committee_deputy_chair <- str_detect(usah_political$house_campaign_committee_deputy_chair, usah_political$wikidataid)
usah_political$house_organization_study_review_chair <- str_detect(usah_political$house_organization_study_review_chair, usah_political$wikidataid)
usah_political$house_caucus_secretary <- str_detect(usah_political$house_caucus_secretary, usah_political$wikidataid)
usah_political$house_democratic_campaign_committee_chair <- str_detect(usah_political$house_democratic_campaign_committee_chair, usah_political$wikidataid)
usah_political$house_democratic_policy_communications_committee_chair <- str_detect(usah_political$house_democratic_policy_communications_committee_chair, usah_political$wikidataid)
usah_political$house_democratic_policy_communications_committee_cochairs <- str_detect(usah_political$house_democratic_policy_communications_committee_cochairs, usah_political$wikidataid)
usah_political$house_democratic_steering_and_policy_committee_cochairs <- str_detect(usah_political$house_democratic_steering_and_policy_committee_cochairs, usah_political$wikidataid)
usah_political$house_democratic_org_study_review_chair <- str_detect(usah_political$house_democratic_org_study_review_chair, usah_political$wikidataid)
usah_political$house_committee_on_ways_means_chair <- str_detect(usah_political$house_committee_on_ways_means_chair, usah_political$wikidataid)
usah_political$house_republican_campaign_committee_chair <- str_detect(usah_political$house_republican_campaign_committee_chair, usah_political$wikidataid)
usah_political$house_majority_leader <- str_detect(usah_political$house_majority_leader, usah_political$wikidataid)
usah_political$house_majority_whip <- str_detect(usah_political$house_majority_whip, usah_political$wikidataid)
usah_political$house_assistant_majority_whip <- str_detect(usah_political$house_assistant_majority_whip, usah_political$wikidataid)
usah_political$house_senior_chief_deputy_whip <- str_detect(usah_political$house_senior_chief_deputy_whip, usah_political$wikidataid)
usah_political$house_senior_deputy_whip <- str_detect(usah_political$house_senior_deputy_whip, usah_political$wikidataid)
usah_political$house_deputy_whip_team <- str_detect(usah_political$house_deputy_whip_team, usah_political$wikidataid)
usah_political$house_assistant_deputy_whip_team <- str_detect(usah_political$house_assistant_deputy_whip_team, usah_political$wikidataid)
usah_political$house_majority_chief_deputy_whip <- str_detect(usah_political$house_majority_chief_deputy_whip, usah_political$wikidataid)
usah_political$house_minority_leader <- str_detect(usah_political$house_minority_leader, usah_political$wikidataid)
usah_political$house_assistant_minority_leader <- str_detect(usah_political$house_assistant_minority_leader, usah_political$wikidataid)
usah_political$house_minority_whip <- str_detect(usah_political$house_minority_whip, usah_political$wikidataid)
usah_political$house_minority_senior_chief_deputy_whip <- str_detect(usah_political$house_minority_senior_chief_deputy_whip, usah_political$wikidataid)
usah_political$house_minority_deputy_whip_team <- str_detect(usah_political$house_minority_deputy_whip_team, usah_political$wikidataid)
usah_political$house_minority_chief_deputy_whip <- str_detect(usah_political$house_minority_chief_deputy_whip, usah_political$wikidataid)
usah_political <- select(usah_political, -c(wikidataid))
usah_political$party <- ifelse(usah_political$party == "democratic", "D",
                               ifelse(usah_political$party == "republican", "R",
                                      ifelse(usah_political$party == "jacksonian", "J",
                                             ifelse(usah_political$party == "opposition", "O",
                                                    ifelse(usah_political$party == "native", "A",
                                                           usah_political$party)))))

# united states senate
# L - Liberty Party
# IMN - Independence Party of Minnesota
# LR - Liberal Republican Party 
usas_political$party <- str_replace(usas_political$party, "\\[1\\]|, then.+|, changed.+|\\.$|2\\. Gilman.+|William E.+", "")
usas_political$party <- ifelse(usas_political$party == "Unknown"| usas_political$party == "", NA, usas_political$party)
usas_political$party <- ifelse(usas_political$party == "A-DR" | usas_political$party == "J-DR" | usas_political$party == "C-DR",
                               "DR", usas_political$party)
usas_political$party <- ifelse(usas_political$party == "A-F", "F",
                               ifelse(usas_political$party == "Anti-J", "AJ",
                                      ifelse(usas_political$party == "Anti-Monopoly", "AM",
                                             ifelse(usas_political$party == "Conservative", "C",
                                                    ifelse(usas_political$party == "D-NPL", "DNPL",
                                                           ifelse(usas_political$party == "UU", "U", usas_political$party))))))
usas_political$party <- ifelse(usas_political$party == "I" | usas_political$party == "ID", "Independent", usas_political$party)
usas_political$party <- ifelse(usas_political$party == "Scoop" | usas_political$party == "Ted", "D", usas_political$party)
usas_political$party <- ifelse(usas_political$party == "P" & usas_political$session <= 3, "PAP", usas_political$party)
usas_political$party <- ifelse(usas_political$party == "P" & usas_political$session >= 77, "Prog", usas_political$party)
usas_political$party <- ifelse(usas_political$party == "P" & usas_political$session >= 52 & usas_political$session <= 57, "Pop", 
                               usas_political$party)
usas_political$session <- as.integer(usas_political$session)
usas_political$session_start <- mdy(usas_political$session_start)
usas_political$session_end <- mdy(usas_political$session_end)
usas_political$service <- as.integer(usas_political$service)


parl_meta_usas <- readxl::read_xlsx("./data/parl_meta/parl-meta-usas.xlsx")
parl_meta_usas <- parl_meta_usas[,c(1,4,6:42)]
colnames(parl_meta_usas)[c(1,2)] <- c("session","senate_majority")
parl_meta_usas$senate_majority <- ifelse(parl_meta_usas$senate_majority == "Pro-Administration", "PAP",
                                        ifelse(parl_meta_usas$senate_majority == "Jackson Men", "jacksonian", 
                                               ifelse(parl_meta_usas$senate_majority == "Democratic-Republican", "DR", 
                                                      ifelse(parl_meta_usas$senate_majority == "Federalist", "F",
                                                             ifelse(parl_meta_usas$senate_majority == "Anto-Jacksonian", "AJ",
                                                                    ifelse(parl_meta_usas$senate_majority == "Jacksonian", "jacksonian",
                                                                           ifelse(parl_meta_usas$senate_majority == "Democratic", "democratic",
                                                                                  ifelse(parl_meta_usas$senate_majority == "Whig", "W",
                                                                                         ifelse(parl_meta_usas$senate_majority == "Republcan", "republican",
                                                                                                ifelse(parl_meta_usas$senate_majority == "Split", "none",
                                                                                                       ifelse(parl_meta_usas$senate_majority == "Republican(plurality)", "republican",
                                                                                                              ifelse(parl_meta_usas$senate_majority == "Republican", "republican",
                                                                                                                     ifelse(parl_meta_usas$senate_majority == "Democratic|Republican", "democratic|republican",
                                                                                                                            ifelse(parl_meta_usas$senate_majority =="Democratic|Republican|Democratic", "democratic|republican|democratic",
                                                                                                                                   parl_meta_usas$senate_majority))))))))))))))
usas_political$party <- ifelse(usas_political$party == "D", "democratic",
                               ifelse(usas_political$party == "R", "republican",
                                      ifelse(usas_political$party == "J", "jacksonian",
                                             ifelse(usas_political$party == "A", "native",
                                                           usas_political$party))))
usas_political <- left_join(x = usas_political, y = parl_meta_usas, by = "session")
rm(parl_meta_usas)
usas_political$senate_majority <- str_detect(usas_political$senate_majority, usas_political$party)
usas_political <- left_join(x = usas_political, y = usas_core[,c(2,3)], by = "pageid")
usas_political$senate_president <- str_detect(usas_political$senate_president, usas_political$wikidataid)
usas_political$senate_president_pro_tempore <- str_detect(usas_political$senate_president_pro_tempore, usas_political$wikidataid)
usas_political$senate_president_pro_tempore_emeritus <- str_detect(usas_political$senate_president_pro_tempore_emeritus, usas_political$wikidataid)
usas_political$senate_permanent_acting_president_pro_tempore <- str_detect(usas_political$senate_permanent_acting_president_pro_tempore, usas_political$wikidataid)
usas_political$senate_deputy_president_pro_tempore <- str_detect(usas_political$senate_deputy_president_pro_tempore, usas_political$wikidataid)
usas_political$senate_republican_conference_chair <- str_detect(usas_political$senate_republican_conference_chair, usas_political$wikidataid)
usas_political$senate_republican_conference_vice_chair <- str_detect(usas_political$senate_republican_conference_vice_chair, usas_political$wikidataid)
usas_political$senate_democratic_conference_chair <- str_detect(usas_political$senate_democratic_conference_chair, usas_political$wikidataid)
usas_political$senate_democratic_conference_vice_chair <- str_detect(usas_political$senate_democratic_conference_vice_chair, usas_political$wikidataid)
usas_political$senate_democratic_caucus_chair <- str_detect(usas_political$senate_democratic_caucus_chair, usas_political$wikidataid)
usas_political$senate_democratic_caucus_vice_chair <- str_detect(usas_political$senate_democratic_caucus_vice_chair, usas_political$wikidataid)
usas_political$senate_democratic_caucus_secretary <- str_detect(usas_political$senate_democratic_caucus_secretary, usas_political$wikidataid)
usas_political$senate_democratic_conference_secretary <- str_detect(usas_political$senate_democratic_conference_secretary, usas_political$wikidataid)
usas_political$senate_republican_conference_secretary <- str_detect(usas_political$senate_republican_conference_secretary, usas_political$wikidataid)
usas_political$senate_senatorial_committee_chair <- str_detect(usas_political$senate_senatorial_committee_chair, usas_political$wikidataid)
usas_political$senate_national_senatorial_committee_chair <- str_detect(usas_political$senate_national_senatorial_committee_chair, usas_political$wikidataid)
usas_political$senate_policy_committee_chair <- str_detect(usas_political$senate_policy_committee_chair, usas_political$wikidataid)
usas_political$senate_republican_policy_committee_chair <- str_detect(usas_political$senate_republican_policy_committee_chair, usas_political$wikidataid)
usas_political$senate_democratic_policy_committee_chair <- str_detect(usas_political$senate_democratic_policy_committee_chair, usas_political$wikidataid)
usas_political$senate_democratic_policy_committee_vice_chair <- str_detect(usas_political$senate_democratic_policy_committee_vice_chair, usas_political$wikidataid)
usas_political$senate_democratic_steering_outreach_committee_chair <- str_detect(usas_political$senate_democratic_steering_outreach_committee_chair, usas_political$wikidataid)
usas_political$senate_democratic_steering_outreach_committee_vice_chair <- str_detect(usas_political$senate_democratic_steering_outreach_committee_vice_chair, usas_political$wikidataid)
usas_political$senate_democratic_steering_committee_chair <- str_detect(usas_political$senate_democratic_steering_committee_chair, usas_political$wikidataid)
usas_political$senate_democratic_committee_outreach_chair <- str_detect(usas_political$senate_democratic_committee_outreach_chair, usas_political$wikidataid)
usas_political$senate_democratic_rural_outreach_chair <- str_detect(usas_political$senate_democratic_rural_outreach_chair, usas_political$wikidataid)
usas_political$senate_republican_conference_secretary_1 <- str_detect(usas_political$senate_republican_conference_secretary__1, usas_political$wikidataid)
usas_political$senate_republican_campaign_committee_chair <- str_detect(usas_political$senate_republican_campaign_committee_chair, usas_political$wikidataid)
usas_political$senate_democratic_campaign_committee_chair <- str_detect(usas_political$senate_democratic_campaign_committee_chair, usas_political$wikidataid)
usas_political$senate_majority_leader <- str_detect(usas_political$senate_majority_leader, usas_political$wikidataid)
usas_political$senate_majority_whip <- str_detect(usas_political$senate_majority_whip, usas_political$wikidataid)
usas_political$senate_chief_deputy_whip <- str_detect(usas_political$senate_chief_deputy_whip, usas_political$wikidataid)
usas_political$senate_majority_deputy_whips <- str_detect(usas_political$senate_majority_deputy_whips, usas_political$wikidataid)
usas_political$senate_minority_leader <- str_detect(usas_political$senate_minority_leader, usas_political$wikidataid)
usas_political$senate_counselor_to_minority_leader <- str_detect(usas_political$senate_counselor_to_minority_leader, usas_political$wikidataid)
usas_political$senate_minority_assistant_leader <- str_detect(usas_political$senate_minority_assistant_leader, usas_political$wikidataid)
usas_political$senate_minority_whip <- str_detect(usas_political$senate_minority_whip, usas_political$wikidataid)
usas_political$senate_minority_deputy_whip <- str_detect(usas_political$senate_minority_deputy_whip, usas_political$wikidataid)
usas_political$party <- ifelse(usas_political$party == "democratic", "D",
                               ifelse(usas_political$party == "republican", "R",
                                      ifelse(usas_political$party == "jacksonian", "J",
                                             ifelse(usas_political$party == "native", "A",
                                             usas_political$party))))
usas_political <- select(usas_political, -c(wikidataid))

# format traffic data -------------------------------------------------------------------

# austrian nationalrat
austria_traffic <- readRDS("./data/austria_traffic")
#austria_traffic <- austria_traffic[,-2]
austria_traffic$date <- austria_traffic$date %>% as.POSIXct(tz = "UTC")
austria_traffic$pageid <- as.integer(austria_traffic$pageid)

# canadian house of commons
canada_traffic <- readRDS("./data/canada_traffic")
#canada_traffic <- canada_traffic[,-2]
canada_traffic$date <- canada_traffic$date %>% as.POSIXct(tz = "UTC")
canada_traffic$pageid <- as.integer(canada_traffic$pageid)

# czech poslanecka snemovna
czech_traffic <- readRDS("./data/czech_traffic")
#czech_traffic <- czech_traffic[,-2]
czech_traffic$date <- czech_traffic$date %>% as.POSIXct(tz = "UTC")
czech_traffic$pageid <- as.integer(czech_traffic$pageid)

# french assemble
france_traffic <- readRDS("./data/france_traffic")
#france_traffic <- france_traffic[,-2]
france_traffic$date <- france_traffic$date %>% as.POSIXct(tz = "UTC")
france_traffic$pageid <- as.integer(france_traffic$pageid)

# german bundestag
germany_traffic <- readRDS("./data/germany_traffic")
#germany_traffic <- germany_traffic[,-2]
germany_traffic$date <- germany_traffic$date %>% as.POSIXct(tz = "UTC")
germany_traffic$pageid <- as.integer(germany_traffic$pageid)

# irish dail
ireland_traffic <- readRDS("./data/ireland_traffic")
#ireland_traffic <- ireland_traffic[,-2]
ireland_traffic$date <- ireland_traffic$date %>% as.POSIXct(tz = "UTC")
ireland_traffic$pageid <- as.integer(ireland_traffic$pageid)

# scottish parliament
scotland_traffic <- readRDS("./data/scotland_traffic")
#scotland_traffic <- scotland_traffic[,-2]
scotland_traffic$date <- scotland_traffic$date %>% as.POSIXct(tz = "UTC")
scotland_traffic$pageid <- as.integer(scotland_traffic$pageid)

# united kingdom parliament
uk_traffic <- readRDS("./data/uk_traffic")
#uk_traffic <- uk_traffic[,-2]
uk_traffic$date <- uk_traffic$date %>% as.POSIXct(tz = "UTC")
uk_traffic$pageid <- as.integer(uk_traffic$pageid)

# united states house
usah_traffic <- readRDS("./data/usah_traffic")
#usah_traffic <- usah_traffic[,-2]
usah_traffic$date <- usah_traffic$date %>% as.POSIXct(tz = "UTC")
# restrict to data after mid 2009 to match individual file storage restrictions
usah_traffic <- filter(usah_traffic, date >= ymd("2009-07-01"))
usah_traffic$pageid <- as.integer(usah_traffic$pageid)

# united states senate
usas_traffic <- readRDS("./data/usas_traffic")
#usas_traffic <- usas_traffic[,-2]
usas_traffic$date <- usas_traffic$date %>% as.POSIXct(tz = "UTC")
usas_traffic$pageid <- as.integer(usas_traffic$pageid)

# format history data -------------------------------------------------------------------

# austrian nationalrat
austria_history <- readRDS("./data/austria_history")
austria_history <- austria_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
austria_history$timestamp <- austria_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# canadian house of commons
canada_history <- readRDS("./data/canada_history")
canada_history <- canada_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
canada_history$timestamp <- canada_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# czech poslanecka snemovna
czech_history <- readRDS("./data/czech_history")
czech_history <- czech_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
czech_history$timestamp <- czech_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# french assemble
france_history <- readRDS("./data/france_history")
france_history <- france_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
france_history$timestamp <- france_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# german bundestag
germany_history <- readRDS("./data/germany_history")
germany_history <- germany_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
germany_history$timestamp <- germany_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# irish dail
ireland_history <- readRDS("./data/ireland_history")
ireland_history <- ireland_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
ireland_history$timestamp <- ireland_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# scottish parliament
scotland_history <- readRDS("./data/scotland_history")
scotland_history <- scotland_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
scotland_history$timestamp <- scotland_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# united kingdom parliament
uk_history <- readRDS("./data/uk_history")
uk_history <- uk_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
uk_history$timestamp <- uk_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# united states house
usah_history <- readRDS("./data/usah_history")
usah_history <- usah_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
usah_history$timestamp <- usah_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# united states senate
usas_history <- readRDS("./data/usas_history")
usas_history <- usas_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
usas_history$timestamp <- usas_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# format facial data --------------------------------------------------------------------

# austrian nationalrat
austria_faces <- readRDS("./data/austria_faces")
austria_faces <- austria_faces[,-1]

# canadian house of commons
canada_faces <- readRDS("./data/canada_faces")
canada_faces <- canada_faces[,-1]

# czech poslanecka snemovna
czech_faces <- readRDS("./data/czech_faces")
czech_faces <- czech_faces[,-1]

# french assemble
france_faces <- readRDS("./data/france_faces")
france_faces <- france_faces[,-1]

# german bundestag
germany_faces <- readRDS("./data/germany_faces")
germany_faces <- germany_faces[-which(is.na(germany_faces$image_url)),-1]

# irish dail
ireland_faces <- readRDS("./data/ireland_faces")
ireland_faces <- ireland_faces[-which(is.na(ireland_faces$image_url)),-1]

# scottish parliament
scotland_faces <- readRDS("./data/scotland_faces")
scotland_faces <- scotland_faces[-which(is.na(scotland_faces$image_url)),-1]

# united kingdom parliament
uk_faces <- readRDS("./data/uk_faces")
uk_faces <- uk_faces[-which(is.na(uk_faces$image_url)),-1]

# united states house
usah_faces <- readRDS("./data/usah_faces")
usah_faces <- usah_faces[,-1]

# united states senate
usas_faces <- readRDS("./data/usas_faces")
usas_faces <- usas_faces[,-1]

# save data -----------------------------------------------------------------------------

# austrian nationalrat
austria_social <- readRDS("./data/austria_social")
austria_positions <- readRDS("./data/austria_positions")
austria_occupation <- readRDS("./data/austria_occupation")
austria_id <- readRDS("./data/austria_id")
saveRDS(austria_core, "./package/legislatoR-data-v1.0.0/aut_core")
saveRDS(austria_political, "./package/legislatoR-data-v1.0.0/aut_political")
saveRDS(austria_history, "./package/legislatoR-data-v1.0.0/aut_history")
saveRDS(austria_traffic, "./package/legislatoR-data-v1.0.0/aut_traffic")
saveRDS(austria_social, "./package/legislatoR-data-v1.0.0/aut_social")
saveRDS(austria_faces, "./package/legislatoR-data-v1.0.0/aut_portrait")
saveRDS(austria_positions, "./package/legislatoR-data-v1.0.0/aut_office")
saveRDS(austria_occupation, "./package/legislatoR-data-v1.0.0/aut_profession")
saveRDS(austria_id, "./package/legislatoR-data-v1.0.0/aut_ids")

# canadian house of commons
canada_social <- readRDS("./data/canada_social")
canada_positions <- readRDS("./data/canada_positions")
canada_occupation <- readRDS("./data/canada_occupation")
canada_id <- readRDS("./data/canada_id")
saveRDS(canada_core, "./package/legislatoR-data-v1.0.0/can_core")
saveRDS(canada_political, "./package/legislatoR-data-v1.0.0/can_political")
saveRDS(canada_history, "./package/legislatoR-data-v1.0.0/can_history")
saveRDS(canada_traffic, "./package/legislatoR-data-v1.0.0/can_traffic")
saveRDS(canada_social, "./package/legislatoR-data-v1.0.0/can_social")
saveRDS(canada_faces, "./package/legislatoR-data-v1.0.0/can_portrait")
saveRDS(canada_positions, "./package/legislatoR-data-v1.0.0/can_office")
saveRDS(canada_occupation, "./package/legislatoR-data-v1.0.0/can_profession")
saveRDS(canada_id, "./package/legislatoR-data-v1.0.0/can_ids")

# czech poslanecka snemovna
czech_social <- readRDS("./data/czech_social")
czech_positions <- readRDS("./data/czech_positions")
czech_occupation <- readRDS("./data/czech_occupation")
czech_id <- readRDS("./data/czech_id")
saveRDS(czech_core, "./package/legislatoR-data/data/cze_core")
saveRDS(czech_political, "./package/legislatoR-data/data/cze_political")
saveRDS(czech_history, "./package/legislatoR-data/data/cze_history")
saveRDS(czech_traffic, "./package/legislatoR-data-v1.0.0/cze_traffic")
saveRDS(czech_social, "./package/legislatoR-data/data/cze_social")
saveRDS(czech_faces, "./package/legislatoR-data/data/cze_portrait")
saveRDS(czech_positions, "./package/legislatoR-data/data/cze_office")
saveRDS(czech_occupation, "./package/legislatoR-data/data/cze_profession")
saveRDS(czech_id, "./package/legislatoR-data/data/cze_ids")

# french assemble
france_social <- readRDS("./data/france_social")
france_positions <- readRDS("./data/france_positions")
france_occupation <- readRDS("./data/france_occupation")
france_id <- readRDS("./data/france_id")
saveRDS(france_core, "./package/legislatoR-data/data/fra_core")
saveRDS(france_political, "./package/legislatoR-data/data/fra_political")
saveRDS(france_history, "./package/legislatoR-data/data/fra_history")
saveRDS(france_traffic, "./package/legislatoR-data-v1.0.0/fra_traffic")
saveRDS(france_social, "./package/legislatoR-data/data/fra_social")
saveRDS(france_faces, "./package/legislatoR-data/data/fra_portrait")
saveRDS(france_positions, "./package/legislatoR-data/data/fra_office")
saveRDS(france_occupation, "./package/legislatoR-data/data/fra_profession")
saveRDS(france_id, "./package/legislatoR-data/data/fra_ids")

# german bundestag
germany_social <- readRDS("./data/germany_social")
germany_positions <- readRDS("./data/germany_positions")
germany_occupation <- readRDS("./data/germany_occupation")
germany_id <- readRDS("./data/germany_id")
saveRDS(germany_core, "./package/legislatoR-data/data/deu_core")
saveRDS(germany_political, "./package/legislatoR-data/data/deu_political")
saveRDS(germany_history, "./package/legislatoR-data/data/deu_history")
saveRDS(germany_traffic, "./package/legislatoR-data-v1.0.0/deu_traffic")
saveRDS(germany_social, "./package/legislatoR-data/data/deu_social")
saveRDS(germany_faces, "./package/legislatoR-data/data/deu_portrait")
saveRDS(germany_positions, "./package/legislatoR-data/data/deu_office")
saveRDS(germany_occupation, "./package/legislatoR-data/data/deu_profession")
saveRDS(germany_id, "./package/legislatoR-data/data/deu_ids")

# irish dail
ireland_social <- readRDS("./data/ireland_social")
ireland_positions <- readRDS("./data/ireland_positions")
ireland_occupation <- readRDS("./data/ireland_occupation")
ireland_id <- readRDS("./data/ireland_id")
saveRDS(ireland_core, "./package/legislatoR-data-v1.0.0/irl_core")
saveRDS(ireland_political, "./package/legislatoR-data-v1.0.0/irl_political")
saveRDS(ireland_history, "./package/legislatoR-data-v1.0.0/irl_history")
saveRDS(ireland_traffic, "./package/legislatoR-data-v1.0.0/irl_traffic")
saveRDS(ireland_social, "./package/legislatoR-data-v1.0.0/irl_social")
saveRDS(ireland_faces, "./package/legislatoR-data-v1.0.0/irl_portrait")
saveRDS(ireland_positions, "./package/legislatoR-data-v1.0.0/irl_office")
saveRDS(ireland_occupation, "./package/legislatoR-data-v1.0.0/irl_profession")
saveRDS(ireland_id, "./package/legislatoR-data-v1.0.0/irl_ids")

# scottish parliament
scotland_social <- readRDS("./data/scotland_social")
scotland_positions <- readRDS("./data/scotland_positions")
scotland_occupation <- readRDS("./data/scotland_occupation")
scotland_id <- readRDS("./data/scotland_id")
saveRDS(scotland_core, "./package/legislatoR-data/data/sco_core")
saveRDS(scotland_political, "./package/legislatoR-data/data/sco_political")
saveRDS(scotland_history, "./package/legislatoR-data/data/sco_history")
saveRDS(scotland_traffic, "./package/legislatoR-data-v1.0.0/sco_traffic")
saveRDS(scotland_social, "./package/legislatoR-data/data/sco_social")
saveRDS(scotland_faces, "./package/legislatoR-data/data/sco_portrait")
saveRDS(scotland_positions, "./package/legislatoR-data/data/sco_office")
saveRDS(scotland_occupation, "./package/legislatoR-data/data/sco_profession")
saveRDS(scotland_id, "./package/legislatoR-data/data/sco_ids")

# united kingdom parliament
uk_social <- readRDS("./data/uk_social")
uk_positions <- readRDS("./data/uk_positions")
uk_occupation <- readRDS("./data/uk_occupation")
uk_id <- readRDS("./data/uk_id")
saveRDS(uk_core, "./package/legislatoR-data-v1.0.0/gbr_core")
saveRDS(uk_political, "./package/legislatoR-data-v1.0.0/gbr_political")
saveRDS(uk_history, "./package/legislatoR-data-v1.0.0/gbr_history")
saveRDS(uk_traffic, "./package/legislatoR-data-v1.0.0/gbr_traffic")
saveRDS(uk_social, "./package/legislatoR-data-v1.0.0/gbr_social")
saveRDS(uk_faces, "./package/legislatoR-data-v1.0.0/gbr_portrait")
saveRDS(uk_positions, "./package/legislatoR-data-v1.0.0/gbr_office")
saveRDS(uk_occupation, "./package/legislatoR-data-v1.0.0/gbr_profession")
saveRDS(uk_id, "./package/legislatoR-data-v1.0.0/gbr_ids")

# united states house
usah_social <- readRDS("./data/usah_social")
usah_positions <- readRDS("./data/usah_positions")
usah_occupation <- readRDS("./data/usah_occupation")
usah_id <- readRDS("./data/usah_id")
saveRDS(usah_core, "./package/legislatoR-data/data/usa_house_core")
saveRDS(usah_political, "./package/legislatoR-data/data/usa_house_political")
saveRDS(usah_history, "./package/legislatoR-data/data/usa_house_history")
saveRDS(usah_traffic, "./package/legislatoR-data-v1.0.0/usa_house_traffic")
saveRDS(usah_social, "./package/legislatoR-data/data/usa_house_social")
saveRDS(usah_faces, "./package/legislatoR-data/data/usa_house_portrait")
saveRDS(usah_positions, "./package/legislatoR-data/data/usa_house_office")
saveRDS(usah_occupation, "./package/legislatoR-data/data/usa_house_profession")
saveRDS(usah_id, "./package/legislatoR-data/data/usa_house_ids")

# united states senate
usas_social <- readRDS("./data/usas_social")
usas_positions <- readRDS("./data/usas_positions")
usas_occupation <- readRDS("./data/usas_occupation")
usas_id <- readRDS("./data/usas_id")
saveRDS(usas_core, "./package/legislatoR-data/data/usa_senate_core")
saveRDS(usas_political, "./package/legislatoR-data/data/usa_senate_political")
saveRDS(usas_history, "./package/legislatoR-data/data/usa_senate_history")
saveRDS(usas_traffic, "./package/legislatoR-data-v1.0.0/usa_senate_traffic")
saveRDS(usas_social, "./package/legislatoR-data/data/usa_senate_social")
saveRDS(usas_faces, "./package/legislatoR-data/data/usa_senate_portrait")
saveRDS(usas_positions, "./package/legislatoR-data/data/usa_senate_office")
saveRDS(usas_occupation, "./package/legislatoR-data/data/usa_senate_profession")
saveRDS(usas_id, "./package/legislatoR-data/data/usa_senate_ids")

