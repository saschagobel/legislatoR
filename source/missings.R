# ---------------------------------------------------------------------------------------
# legislatoR
# Sascha Göbel and Simon Munzert
# Script: missings
# Part of the code in this script written by Lada Rudnitckaia
# June 2019
# ---------------------------------------------------------------------------------------


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list = ls(all = TRUE))

# set working directory -----------------------------------------------------------------
setwd("D:/Sascha/Projects/legislatoR")

# install and load packages and functions -----------------------------------------------
source("./code/packages.R")
source("./code/functions.R")


#### ADD MISSING FRENCH PERIOD OF SERVICE DATA ==========================================
# import data with adjust start and end date of mandate
fra_adjusted <- readRDS("data/france_service/fra_adjusted")
# extract legislators pageid for matching and join to fra_adjusted
fra_adjusted[[12]]$url[402] <- "https://fr.wikipedia.org/wiki/Philippe_Martin_(homme_politique,_1949)"
fra_pageids <- lapply(fra_adjusted, wikiIDs, .$url)
fra_adjusted <- mapply(cbind, fra_adjusted, fra_pageids, SIMPLIFY = FALSE)
# compute period of service
fra_adjusted <- lapply(fra_adjusted, function(x) {
  mutate(x,
         session = term,
         service = difftime(ymd(session_end),ymd(session_start), units = "days")
         )})
# stack and select pageid, session, and service
fra_adjusted <- bind_rows(fra_adjusted) %>%
  select(pageid, session, service)
# join period of service to existing political data for france
fra_political <- readRDS("./package/legislatoR-data-v0.2.0/data/fra_political")
fra_political <- left_join(fra_political, fra_adjusted, by = c("pageid", "session"))
# remove duplicated rows
fra_political <- distinct(fra_political, .keep_all = TRUE)
# convert service
fra_political$service <- as.integer(fra_political$service)
# correct coding mistakes
fra_political[5844,]$service <- 22
fra_political[5485,]$service <- 1175
fra_political[5670,]$service <- 10
fra_political[5638,]$service <- 602
fra_political[1559,]$service <- 1715
fra_political[4778,]$service <- 985
# save
saveRDS(fra_political, "package/legislatoR-data-v0.2.0/fra_political")


#### ADD MISSING FRENCH LEGISLATORS =====================================================

# import data collected for missings
fra_1_add <- readRDS("data/france_service/fra_1_add")
fra_2_add <- readRDS("data/france_service/fra_2_add")
fra_3_add <- readRDS("data/france_service/fra_3_add")
fra_4_add <- readRDS("data/france_service/fra_4_add")
fra_5_add <- readRDS("data/france_service/fra_5_add")
fra_5_add2 <- readRDS("data/france_service/fra_5_add_extra")
fra_6_add <- readRDS("data/france_service/fra_6_add")
fra_7_add <- readRDS("data/france_service/fra_7_add")
fra_9_add <- readRDS("data/france_service/fra_9_add")
fra_10_add <- readRDS("data/france_service/fra_10_add")
fra_10_add2 <- readRDS("data/france_service/fra_10_add_extra")
fra_11_add <- readRDS("data/france_service/fra_11_add")
fra_11_add2 <- readRDS("data/france_service/fra_11_add_extra")
fra_12_add <- readRDS("data/france_service/fra_12_add")
fra_13_add <- readRDS("data/france_service/fra_13_add")
fra_13_add2 <- readRDS("data/france_service/fra_13_add_extra")
fra_14_add <- readRDS("data/france_service/fra_14_add")
fra_14_add2 <- readRDS("data/france_service/fra_14_add_extra")
fra_15_add <- readRDS("data/france_service/fra_15_add")
# bind together
fra_add <- rbind(fra_1_add, fra_2_add, fra_3_add, fra_4_add, fra_5_add, fra_5_add2,
                 fra_6_add, fra_7_add, fra_9_add, fra_10_add, fra_10_add2, fra_11_add,
                 fra_11_add2, fra_12_add, fra_13_add, fra_13_add2, fra_14_add, 
                 fra_14_add2, fra_15_add)
# add session
fra_add$session <- rep(c(1:5,5,6,7,9,10,10,11,11,12,13,13,14,14,15),
                       times = lapply(list(fra_1_add, fra_2_add, fra_3_add, fra_4_add, 
                                           fra_5_add, fra_5_add2, fra_6_add, fra_7_add, 
                                           fra_9_add, fra_10_add, fra_10_add2, fra_11_add,
                                           fra_11_add2, fra_12_add, fra_13_add, 
                                           fra_13_add2, fra_14_add, fra_14_add2, 
                                           fra_15_add), nrow))
# compute service
fra_add$service <- difftime(ymd(fra_add$session_end), ymd(fra_add$session_start), 
                            units = "days")
fra_add$service[119] <- 3433
fra_add$service[51] <- 1578
fra_add$service <- abs(fra_add$service)

# collect Wikidata and pageid, wikititle and demographics, build core, and add to core
fra_add$url <- as.character(fra_add$url)
fra_add$url[253] <- "https://fr.wikipedia.org/wiki/Jean_Duprat_(homme_politique,_1936)"
fra_add$url[254] <- "https://fr.wikipedia.org/wiki/Jean-Jacques_B%C3%A9neti%C3%A8re"
fra_add$url[323] <-"https://fr.wikipedia.org/wiki/Jean-Fran%C3%A7ois_Lamarque_(homme_politique)"
fra_ids_m <- wikiIDs(fra_add, corp = TRUE)
fra_add <- cbind(fra_ids_m, fra_add)
fra_history_m <- bind_rows(lapply(X = unique(fra_add$pageid), FUN = wikiHist,
                                  project = "fr.wikipedia"))
fra_title_m <- undirectedTitle(pageid = unique(fra_add$pageid),
                              project = "fr.wikipedia")
fra_traffic_m <- wikiTraffic(data = fra_title_m, project = "fr.wikipedia")
fra_entities_m <- get_item(id = unique(fra_add$wikidataid))
fra_sex_m <- wikiData(item = unique(fra_add$wikidataid), entity = fra_entities_m, 
                     property = "P21")
fra_sex_m$sex <- ifelse(fra_sex_m$male == TRUE, "male",
                   ifelse(fra_sex_m$female == TRUE, "female", NA))
fra_sex_m <- fra_sex_m[,-c(2,3)]
fra_religion_m <- wikiData(item = unique(fra_add$wikidataid), entity = fra_entities_m,
                            property = "P140")
fra_religion_m$religion <- replace(NA, rowSums(fra_religion_m[,c(3,6,8)]) >= 1,
                                    "catholicism") %>%
  replace(., rowSums(fra_religion_m[,c(2)]) >= 1,
          "protestantism reformed") %>%
  replace(., rowSums(fra_religion_m[,c(5)]) >= 1,
          "islam") %>%
  replace(., rowSums(fra_religion_m[,c(4)]) >= 1,
          "agnosticism") %>%
  replace(., rowSums(fra_religion_m[,c(7)]) >= 1,
          "judaism")
fra_religion_m <- fra_religion_m[,c(1, 9)]
fra_birth_m <- wikiData(item = unique(fra_add$wikidataid), entity = fra_entities_m, 
                         date = TRUE, property = "P569")
fra_birth_m$date <- fra_birth_m$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
fra_death_m <- wikiData(item = unique(fra_add$wikidataid), entity = fra_entities_m, 
                         date = TRUE, property = "P570")
fra_death_m$date <- fra_death_m$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
fra_birthplace_m <- wikiData(item = unique(fra_add$wikidataid), 
                              entity = fra_entities_m, location = TRUE,
                              property = "P19")
fra_birthplace_m$lat <- round(fra_birthplace_m$lat, digit = 5)
fra_birthplace_m$lon <- round(fra_birthplace_m$lon, digit = 5)
fra_birthplace_m$birthplace <- str_c(fra_birthplace_m$lat, ",",
                                      fra_birthplace_m$lon)
fra_birthplace_m <- fra_birthplace_m[,c(1,4)]
fra_deathplace_m <- wikiData(item = unique(fra_add$wikidataid), 
                              entity = fra_entities_m, location = TRUE,
                              property = "P20")
fra_deathplace_m$lat <- round(fra_deathplace_m$lat, digit = 5)
fra_deathplace_m$lon <- round(fra_deathplace_m$lon, digit = 5)
fra_deathplace_m$deathplace <- str_c(fra_deathplace_m$lat, ",",
                                     fra_deathplace_m$lon)
fra_deathplace_m <- fra_deathplace_m[,c(1,4)]

fra_twitter_m <- wikiData(item = unique(fra_add$wikidataid), 
                           entity = fra_entities_m, property = "P2002", 
                           serial = TRUE)
fra_facebook_m <- wikiData(item = unique(fra_add$wikidataid), 
                            entity = fra_entities_m, property = "P2013", 
                            serial = TRUE)
fra_youtube_m <- wikiData(item = unique(fra_add$wikidataid), 
                           entity = fra_entities_m, property = "P2397", 
                           serial = TRUE)
fra_instagram_m <- wikiData(item = unique(fra_add$wikidataid), 
                             entity = fra_entities_m, property = "P2003", 
                             serial = TRUE)
fra_linkedin_m <- wikiData(item = unique(fra_add$wikidataid), 
                            entity = fra_entities_m, property = "P2035", 
                            serial = TRUE)
fra_website_m <- wikiData(item = unique(fra_add$wikidataid), 
                           entity = fra_entities_m, property = "P856", 
                           serial = TRUE)
fra_social_m <- full_join(x = fra_twitter_m, y = fra_facebook_m, by = "wikidataid") %>%
  full_join(x = ., y = fra_youtube_m, by = "wikidataid") %>%
  full_join(x = ., y = fra_instagram_m, by = "wikidataid") %>%
  full_join(x = ., y = fra_linkedin_m, by = "wikidataid") %>%
  full_join(x = ., y = fra_website_m, by = "wikidataid")
names(fra_social_m) <- c("wikidataid", "twitter", "facebook", "youtube", "instagram",
                          "linkedin", "website")
fra_images_m <- imageUrl(pageid = unique(fra_add$pageid), project = "fr.wikipedia")
fra_images_m <- fra_images_m[!str_detect(fra_images_m$image_url,
                                           "https://upload.wikimedia.org/wikipedia/commons/thumb/8/85/Defaut.svg/50px-Defaut.svg.png"), ]
fra_original <- readRDS("./package/legislatoR-data-v0.1.0/data/fra_core")
fra_images_m <- fra_images_m[which(!(fra_images_m$pageid %in% fra_original$pageid)),]
remove_images <- c(1,2,4,7,9,12,13,18,32,33,39,41,43,48,49,52)
fra_images_m$image_url[remove_images] <- NA
fra_images_m <- as.data.frame(fra_images_m)
fra_images_m$ethnicity <- "white"
fra_images_m$ethnicity <- ifelse(is.na(fra_images_m$image_url), NA, fra_images_m$ethnicity)
fra_images_m$ethnicity[c(26,44)] <- "asian"
fra_parlid_m <- wikiData(item = unique(fra_add$wikidataid), 
                          entity = fra_entities_m, property = "P4123", 
                          serial = TRUE)
fra_sycomore_m <- wikiData(item = unique(fra_add$wikidataid), 
                            entity = fra_entities_m, property = "P1045", 
                            serial = TRUE)
fra_gndid_m <- wikiData(item = unique(fra_add$wikidataid), 
                         entity = fra_entities_m, property = "P227", 
                         serial = TRUE)
fra_libcon_m <- wikiData(item = unique(fra_add$wikidataid), 
                          entity = fra_entities_m, property = "P244", 
                          serial = TRUE)
fra_bnfid_m <- wikiData(item = unique(fra_add$wikidataid), 
                         entity = fra_entities_m, property = "P268", 
                         serial = TRUE)
fra_freebase_m <- wikiData(item = unique(fra_add$wikidataid), 
                            entity = fra_entities_m, property = "P646", 
                            serial = TRUE)
fra_munzinger_m <- wikiData(item = unique(fra_add$wikidataid), 
                             entity = fra_entities_m, property = "P1284", 
                             serial = TRUE)
fra_nndb_m <- wikiData(item = unique(fra_add$wikidataid), 
                        entity = fra_entities_m, property = "P1263", 
                        serial = TRUE)
fra_imdb_m <- wikiData(item = unique(fra_add$wikidataid), 
                        entity = fra_entities_m, property = "P345", 
                        serial = TRUE)
fra_brittanica_m <- wikiData(item = unique(fra_add$wikidataid), 
                              entity = fra_entities_m, property = "P1417", 
                              serial = TRUE)
fra_quora_m <- wikiData(item = unique(fra_add$wikidataid), 
                         entity = fra_entities_m, property = "P3417", 
                         serial = TRUE)
fra_id_m <- full_join(x = fra_parlid_m, y = fra_sycomore_m, by = "wikidataid") %>%
  full_join(x = ., y = fra_gndid_m, by = "wikidataid") %>%
  full_join(x = ., y = fra_libcon_m, by = "wikidataid") %>%
  full_join(x = ., y = fra_bnfid_m, by = "wikidataid") %>%
  full_join(x = ., y = fra_freebase_m, by = "wikidataid") %>%
  full_join(x = ., y = fra_munzinger_m, by = "wikidataid") %>%
  full_join(x = ., y = fra_nndb_m, by = "wikidataid") %>%
  full_join(x = ., y = fra_imdb_m, by = "wikidataid") %>%
  full_join(x = ., y = fra_brittanica_m, by = "wikidataid") %>%
  full_join(x = ., y = fra_quora_m, by = "wikidataid")
names(fra_id_m) <- c("wikidataid", "parlid", "sycomore", "gndid", "libcon",
                      "bnfid", "freebase", "munzinger", "nndb", "imdb",
                      "brittanica", "quora")
fra_positions_m <- wikiData(item = unique(fra_add$wikidataid), 
                             entity = fra_entities_m, unique = TRUE, 
                             property = "P39")
fra_positions_m <- fra_positions_m[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
fra_profession_m <- wikiData(item = unique(fra_add$wikidataid), 
                              entity = fra_entities_m, unique = TRUE, 
                              property = "P106")
fra_profession_m <- fra_profession_m[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())

# assemble data -------------------------------------------------------------------------
fra_m <- left_join(x = fra_add, y = fra_title_m, by = "pageid") %>%
  left_join(x = ., y = fra_images_m[,c(1,3)], by = "pageid") %>%
  left_join(x = ., y = fra_sex_m, by = "wikidataid") %>%
  left_join(x = ., y = fra_religion_m, by = "wikidataid") %>%
  left_join(x = ., y = fra_birth_m, by = "wikidataid") %>%
  left_join(x = ., y = fra_death_m, by = "wikidataid") %>%
  left_join(x = ., y = fra_birthplace_m, by = "wikidataid") %>%
  left_join(x = ., y = fra_deathplace_m, by = "wikidataid")
colnames(fra_m)[c(11,15,16)] <- c("wikititle", "birth", "death")
fra_m$country <- "FRA" 
fra_m <- fra_m %>% select(country, pageid, wikidataid, wikititle, name, sex, 
                          ethnicity, religion, birth, death, birthplace, deathplace, 
                          session, party, constituency, session_start, 
                          session_end, service)
fra_political_m <- fra_m[c(2,13:18)]
fra_core_m <- fra_m[c(1:12)]
  

fra_political_m$party <- str_replace_all(fra_political_m$party, "\\|.+", "")
fra_political_m$party <- str_replace(fra_political_m$party, "a\\.( )?|\\(app\\.\\)| \\(Les Verts\\)|\\*", "")
fra_political_m$party <- str_replace(fra_political_m$party, "Non.+|NI|DIV", "Non-Inscrit")
fra_political_m$party <- str_trim(fra_political_m$party)
fra_political_m$party <- ifelse(fra_political_m$party == "Communiste" | fra_political_m$party == "C", "PCF",
                                 ifelse(fra_political_m$party == "S" | fra_political_m$party == "Socialiste", "SFIO", 
                                        ifelse(fra_political_m$party == "UD-Ve","UDR",
                                               ifelse(fra_political_m$party == "I", "Non-Inscrit",
                                                      fra_political_m$party))))
fra_political_m$party[which(fra_political_m$party == "")] <- NA
fra_political_m$party <- str_replace_all(fra_political_m$party, "CNon-Inscrit", "Non-Inscrit")
fra_political_m$constituency <- str_replace(fra_political_m$constituency, "e\\)|re\\)", ")")
fra_political_m$session_start <- as.character(fra_political_m$session_start)
fra_political_m$session_end <- as.character(fra_political_m$session_end)
fra_political_m[fra_political_m$session == 1,"session_start"] <- "1958-11-30"
fra_political_m[fra_political_m$session == 1,"session_end"] <- "1962-11-25"
fra_political_m[fra_political_m$session == 2,"session_start"] <-  "1962-11-25"
fra_political_m[fra_political_m$session == 2,"session_end"] <-  "1967-03-12"
fra_political_m[fra_political_m$session == 3,"session_start"] <-  "1967-03-12"
fra_political_m[fra_political_m$session == 3,"session_end"] <-  "1968-06-30"
fra_political_m[fra_political_m$session == 4,"session_start"] <-  "1968-06-30"
fra_political_m[fra_political_m$session == 4,"session_end"] <-"1973-03-11"
fra_political_m[fra_political_m$session == 5,"session_start"] <-  "1973-03-11"
fra_political_m[fra_political_m$session == 5,"session_end"] <-  "1978-03-19"
fra_political_m[fra_political_m$session == 6,"session_start"]  <-  "1978-03-19"
fra_political_m[fra_political_m$session == 6,"session_end"] <- "1981-06-21"
fra_political_m[fra_political_m$session == 7,"session_start"]  <-"1981-06-21"
fra_political_m[fra_political_m$session == 7,"session_end"] <- "1986-03-16"
fra_political_m[fra_political_m$session == 9,"session_start"]  <- "1988-06-12"
fra_political_m[fra_political_m$session == 9,"session_end"] <- "1993-03-28"
fra_political_m[fra_political_m$session == 10,"session_start"]  <- "1993-03-28"
fra_political_m[fra_political_m$session == 10,"session_end"] <-  "1997-06-01"
fra_political_m[fra_political_m$session == 11,"session_start"]  <- "1997-06-01"
fra_political_m[fra_political_m$session == 11,"session_end"] <- "2002-06-16"
fra_political_m[fra_political_m$session == 12,"session_start"]  <- "2002-06-16"
fra_political_m[fra_political_m$session == 12,"session_end"] <- "2007-06-17"
fra_political_m[fra_political_m$session == 13,"session_start"]  <- "2007-06-17"
fra_political_m[fra_political_m$session == 13,"session_end"] <-"2012-06-17"
fra_political_m[fra_political_m$session == 14,"session_start"]  <-"2012-06-17"
fra_political_m[fra_political_m$session == 14,"session_end"] <-"2017-06-18"
fra_political_m[fra_political_m$session == 15,"session_start"] <- "2017-06-18"
fra_political_m[fra_political_m$session == 15,"session_end"] <- "2022-06-01"
fra_political_m$session <- as.integer(fra_political_m$session)
fra_political_m$session_start <- ymd(fra_political_m$session_start)
fra_political_m$session_end <- ymd(fra_political_m$session_end)
fra_political_m$service <- as.integer(fra_political_m$service)
fra_traffic_m <- fra_traffic_m[,-2]
fra_traffic_m$date <- fra_traffic_m$date %>% as.POSIXct(tz = "UTC")
fra_traffic_m$pageid <- as.integer(fra_traffic_m$pageid)
fra_history_m <- fra_history_m %>% select(pageid, revid, parentid, user, userid,
                                        timestamp, size, comment)
fra_history_m$timestamp <- fra_history_m$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")
fra_portrait_m <- filter(fra_images_m, !is.na(image_url)) %>%
  dplyr::select(-ethnicity)

# import current legislator data --------------------------------------------------------
fra_core <- readRDS("./package/legislatoR-data-v0.1.0/data/fra_core")
fra_political <- readRDS("./package/legislatoR-data-v0.2.0/fra_political")
fra_traffic <- readRDS("./package/legislatoR-data-v0.1.0/data/fra_traffic")
fra_office <- readRDS("./package/legislatoR-data-v0.1.0/data/fra_office")
fra_portrait <- readRDS("./package/legislatoR-data-v0.1.0/data/fra_portrait")
fra_profession <- readRDS("./package/legislatoR-data-v0.1.0/data/fra_profession")
fra_history <- readRDS("./package/legislatoR-data-v0.1.0/data/fra_history")
fra_ids <- readRDS("./package/legislatoR-data-v0.1.0/data/fra_ids")

# add update ----------------------------------------------------------------------------
fra_core_m <- fra_core_m[!(fra_core_m$wikidataid %in% fra_core$wikidataid),]
fra_core_new <- rbind(fra_core, fra_core_m)
fra_political_new <- rbind(fra_political, fra_political_m) %>%
  arrange(session)
fra_traffic_m <- fra_traffic_m[!(fra_traffic_m$pageid %in% fra_traffic$pageid),]
fra_traffic_new <- rbind(fra_traffic, fra_traffic_m) %>%
  arrange(pageid, date)
fra_profession_m <- fra_profession_m[!(fra_profession_m$wikidataid %in% fra_profession$wikidataid),]
fra_profession_new <- rbind.fill(fra_profession, fra_profession_m)
fra_profession_new <- fra_profession_new %>%
  setcolorder(order(names(.))) %>% 
  select(wikidataid, everything())
fra_profession_new[is.na(fra_profession_new)] <- FALSE
fra_positions_m <- fra_positions_m[!(fra_positions_m$wikidataid %in% fra_office$wikidataid),]
fra_office_new <- rbind.fill(fra_office, fra_positions_m)
fra_office_new <- fra_office_new %>%
  setcolorder(order(names(.))) %>% 
  select(wikidataid, everything())
fra_office_new[is.na(fra_office_new)] <- FALSE
fra_portrait_m <- fra_portrait_m[!(fra_portrait_m$pageid %in% fra_portrait$pageid),]
fra_portrait_new <- rbind(fra_portrait, fra_portrait_m)
fra_id_m <- fra_id_m[!(fra_id_m$wikidataid %in% fra_ids$wikidataid),]
fra_ids_new <- rbind.fill(fra_ids, fra_id_m)
fra_history_m <- fra_history_m[!(fra_history_m$pageid %in% fra_history$pageid),]
fra_history_m <- fra_history_m %>%
  select(pageid, revid, parentid, user, userid,
         timestamp, size, comment)
fra_history_m$timestamp <- fra_history_m$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")
fra_history_new <- rbind(fra_history, fra_history_m)

# save to disk
saveRDS(fra_core_new, "./package/legislatoR-data-v0.2.0/fra_core")
saveRDS(fra_political_new, "./package/legislatoR-data-v0.2.0/fra_political")
saveRDS(fra_history_new, "./package/legislatoR-data-v0.2.0/fra_history")
saveRDS(fra_ids_new, "./package/legislatoR-data-v0.2.0/fra_ids")
saveRDS(fra_traffic_new, "./package/legislatoR-data-v0.2.0/fra_traffic")
saveRDS(fra_office_new, "./package/legislatoR-data-v0.2.0/fra_office")
saveRDS(fra_profession_new, "./package/legislatoR-data-v0.2.0/fra_profession")
saveRDS(fra_portrait_new, "./package/legislatoR-data-v0.2.0/fra_portrait")


#### ADD DATA ON MISSING UK LEGISLATIVE PERIODS =========================================

# get core data for 1-11,16,17,20-37 parliaments ----------------------------------------
uk_core_m <- collectorUk(source = "./data/htmls/uk/")
uk_core_m <- uk_core_m[1:31]

# get wikidatids and pageids ------------------------------------------------------------
uk_ids_m <- lapply(X = uk_core_m, FUN = wikiIDs, corp = TRUE)

# bind together -------------------------------------------------------------------------
uk_m <- bind_rows(Map(cbind, uk_ids_m, uk_core_m))

# replace Q224438 with NA on wikidata id, if NA on wikidataid also NA on pageid and URL -
uk_m$wikidataid <- ifelse(uk_m$wikidataid == "Q224438", NA, uk_m$wikidataid)
uk_m$pageid <- ifelse(is.na(uk_m$wikidataid), NA, uk_m$pageid)
uk_m$url <- ifelse(is.na(uk_m$wikidataid), NA, uk_m$url)
uk_m$party <- ifelse(uk_m$party == "", NA, uk_m$party)

# import missing service times and legislatures -----------------------------------------
services_files <- list.files("./data/htmls/uk_csvs")
for (i in 1:length(services_files)) {
  a <- read.csv(str_c("./data/htmls/uk_csvs/", services_files[i]))
  a$term <- str_replace_all(services_files[i], "th.+|st.+|nd.+|rd.+", "")
  if (i == 1) {
    services_m <- a
  } else {
    services_m <- rbind(services_m, a)
  }
}
rm(i)

# fill missing service times ------------------------------------------------------------
services_m$item <- str_extract(services_m$item, "Q.+")
services_m$start <- as.character(services_m$start) %>%
  str_replace("T.+", "") %>%
  ymd()
services_m$end <- as.character(services_m$end) %>%
  str_replace("T.+", "") %>%
  ymd()
services_m$term <- as.numeric(services_m$term)
services_m$service <- difftime(time1 = services_m$end, time2 = services_m$start, 
                               units = "days")
colnames(services_m)[1] <- "wikidataid"
merger_1 <- str_c(uk_m$wikidataid, "_", uk_m$term)
merger_2 <- str_c(services_m$wikidataid, "_", services_m$term)
match_1 <- match(merger_2, merger_1)
match_1 <- match_1[which(!is.na(match_1))]
uk_m$service2[match_1] <- services_m$service[which(!is.na(match(merger_2, merger_1)))]
uk_m$service <- ifelse(is.na(uk_m$service), uk_m$service2, uk_m$service)
uk_m <- dplyr::select(uk_m, -service2)

# fill missing periods ------------------------------------------------------------------
periods_m <- services_m %>%
  filter(term %in% c(12,13,14,15,18,19))
periods_m$pageid <- NA
periods_m$url <- NA
periods_m$country <- "GBR"
periods_m$session_start <- NA
periods_m$session_end <- NA
colnames(periods_m)[c(2,3,4)] <- c("name", "constituency", "party")
periods_m <- periods_m %>%
  dplyr::select(pageid, wikidataid, url, name, party, constituency, service, session_start,
                session_end, term, country)

# get urls and pageids of legislators in missing periods ------------------------------------------
urls_m <- data.frame(url = rep(as.integer(NA), length(periods_m$wikidataid)))
for (i in 1:length(periods_m$wikidataid)) {
  cat(".")
  url <- NULL
  while (is.null(url)) {
    try(
      url <- as.character(RETRY("POST", str_c("https://www.wikidata.org/w/api.php?"),
                               query = list(action= "wbgetentities",
                                            ids = periods_m$wikidataid[i],
                                            format = "json",
                                            props = "sitelinks/urls",
                                            sitefilter = "enwiki"),
                               times = 1000)),
      silent = TRUE)
  }
  check <- fromJSON(url)$entities[[1]]$sitelink$enwiki$url
  if (!is.null(check)) {
    urls_m[i,1] <- check
  }
}
periods_m$url <- urls_m$url
periods_m_ids <- wikiIDs(url = periods_m$url)
periods_m$pageid <- periods_m_ids$pageid
periods_m$pageid <- ifelse(is.na(periods_m$url), NA, periods_m$pageid)

# find wikidata ids in services_m -------------------------------------------------------
uk_m2 <- filter(uk_m, is.na(wikidataid))
services_m2 <- services_m[which(!(services_m$wikidataid %in% uk_m$wikidataid)),]
services_m2 <- filter(services_m2, term %in% c(1:11, 16, 17, 20:36))
services_m2$name2 <- str_c(services_m2$itemLabel, "_", services_m2$term)
uk_m2$name2 <- str_c(uk_m2$name, "_", uk_m2$term)
match_2 <- match(services_m2$name2, uk_m2$name2)
match_2 <- match_2[which(!is.na(match_2))]
uk_m2$wikidataid2[match_2] <- services_m2$wikidataid[which(!is.na(match(services_m2$name2, uk_m2$name2)))]
uk_m <- as.data.frame(uk_m)
uk_m$wikidataid2 <- NA
uk_m[is.na(uk_m$wikidataid),"wikidataid2"] <- uk_m2$wikidataid2
uk_m$wikidataid <- ifelse(is.na(uk_m$wikidataid), uk_m$wikidataid2, uk_m$wikidataid)
uk_m <- dplyr::select(uk_m, -wikidataid2)

# bind all together and order by term
uk_m <- rbind(uk_m, periods_m)
uk_m <- arrange(uk_m, term)
# remove complete duplicates within legislative session
uk_m <- distinct(uk_m, .keep_all = TRUE)

# collect other data as usual -----------------------------------------------------------
uk_history_m <- bind_rows(lapply(X = na.omit(unique(uk_m$pageid)), FUN = wikiHist,
                                 project = "en.wikipedia"))
saveRDS(uk_history_m, "./package/legislatoR-data-v0.2.0/temp/uk_history_m")
uk_title_m <- undirectedTitle(pageid = na.omit(unique(uk_m$pageid)),
                              project = "en.wikipedia")
saveRDS(uk_title_m, "./package/legislatoR-data-v0.2.0/temp/uk_title_m")

uk_traffic_m <- wikiTraffic(data = uk_title_m, project = "en.wikipedia")
saveRDS(uk_traffic_m, "./package/legislatoR-data-v0.2.0/temp/uk_traffic_m")

uk_entities_m <- get_item(id = na.omit(unique(uk_m$wikidataid)))
saveRDS(uk_entities_m, "./package/legislatoR-data-v0.2.0/temp/uk_entities_m")

uk_sex_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), entity = uk_entities_m, 
                     property = "P21")
uk_sex_m$sex <- ifelse(uk_sex_m$male == TRUE, "male",
                  ifelse(uk_sex_m$female == TRUE, "female", NA))
uk_sex_m <- uk_sex_m[,-c(2,3)]
uk_sex_m$sex <- factor(uk_sex_m$sex)
uk_religion_m <- wikiData(item = unique(uk_m$wikidataid), 
                        entity = uk_entities_m, property = "P140")
uk_religion_m$religion <- replace(NA, rowSums(uk_religion_m[,c(5,6,17,19,22)]) >= 1,
                                "catholicism") %>%
  replace(., rowSums(uk_religion_m[,c(2,9,14,21,25)]) >= 1,
          "protestantism reformed") %>%
  replace(., rowSums(uk_religion_m[,c(3,11)]) >= 1,
          "protestantism methodist") %>%
  replace(., rowSums(uk_religion_m[,c(7,13)]) >= 1,
          "protestantism nontrinitarian") %>%
  replace(., rowSums(uk_religion_m[,c(16)]) >= 1,
          "protestantism baptist") %>%
  replace(., rowSums(uk_religion_m[,c(23)]) >= 1,
          "protestantism") %>%
  replace(., rowSums(uk_religion_m[,c(4)]) >= 1,
          "islam") %>%
  replace(., rowSums(uk_religion_m[,c(8,12)]) >= 1,
          "anglicanism") %>%
  replace(., rowSums(uk_religion_m[,c(10,24)]) >= 1,
          "atheism") %>%
  replace(., rowSums(uk_religion_m[,c(15)]) >= 1,
          "judaism") %>%
  replace(., rowSums(uk_religion_m[,c(20)]) >= 1,
          "protestantism quaker") %>%
  replace(., rowSums(uk_religion_m[,c(18)]) >= 1,
          "zoroastrianism") 
uk_religion_m <- uk_religion_m[,c(1, 26)]
uk_religion_m$religion <- factor(uk_religion_m$religion)
uk_birth_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), entity = uk_entities_m, date = TRUE,
                     property = "P569")
uk_birth_m$date <- uk_birth_m$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% replace(930, "1749-02-28") %>% 
  as.POSIXct(tz = "UTC")
uk_death_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), entity = uk_entities_m, date = TRUE, 
                     property = "P570")
uk_death_m$date <- uk_death_m$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
uk_birthplace_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), entity = uk_entities_m, 
                          location = TRUE, property = "P19")
uk_birthplace_m$lat <- round(uk_birthplace_m$lat, digit = 5)
uk_birthplace_m$lon <- round(uk_birthplace_m$lon, digit = 5)
uk_birthplace_m$birthplace <- str_c(uk_birthplace_m$lat, ",",
                                  uk_birthplace_m$lon)
uk_birthplace_m <- uk_birthplace_m[,c(1,4)]
uk_deathplace_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                          entity = uk_entities_m, location = TRUE,
                          property = "P20")
uk_deathplace_m$lat <- round(uk_deathplace_m$lat, digit = 5)
uk_deathplace_m$lon <- round(uk_deathplace_m$lon, digit = 5)
uk_deathplace_m$deathplace <- str_c(uk_deathplace_m$lat, ",",
                                  uk_deathplace_m$lon)
uk_deathplace_m <- uk_deathplace_m[,c(1,4)]
uk_images_m <- imageUrl(pageid = na.omit(unique(uk_m$pageid)), project = "en.wikipedia")
remove_images <- c(43126483,10122739,54697281,42954814,1486780,230169,38760217,55466846
                  ,13073706,6718755,44030382,9129183,41420338,2364715,29447703,30691382
                  ,27620847,7323374,33216443,3038233,7257425,50608163,9443812,43078347
                  ,30678634,35816374,21082269,60039943,14316407,40564873,7515960,9897809
                  ,9462186,43089490,34881368,11502987,55178708,7825583,8605763,663752
                  ,26118066,43026494,36106017,4005289,10778170,17607322,12837493,10444288
                  ,43104747,21546444,1366608,11905480,25140327,40369004,29330564,27672875
                  ,11454390,663691,18561679,55498416,24643044,22773747,704401,35046408
                  ,35109399,2572858,1940730,38732126,20684780,6889637,12608228,4221162
                  ,51238641,38013437,16196615,34433503,60181857,8718669,9462823,4772700
                  ,11859981,28215086,23095957,3071040,8133218,743919,42629807,9581891
                  ,23107125,21425588,2313412,20351714,9910476,29358403,55459038,21376312
                  ,1205409,35972754,3718953,5270138,612960,12513565,39043277,22944182
                  ,5270205,38518231,18661762,26680490,28259383,14513828,33643398,30761489
                  ,49170178,2399083,8246448,19011370,1162391,10871214,51498154,23141435
                  ,26522472,32660124,1510838,6245085,11541284,20832865,11859093,3139874
                  ,38881154,43146296,9538391,42999404,60046774,46496254,28143877,59556135
                  ,31174591,12264645,4806044,33797985,28695568,25214367,57219698,7363218
                  ,40093451,9444641,11815108,40076810,28387297,54233594,2297573,34642948
                  ,23103907,7061186,10517541,19261216,60010404,30255861,9646190,21547200
                  ,39155972,1285920,12598635,12290747,579838,1510349,3721655,16521767
                  ,20597006,7403413,17847615,29329902,560899,2357312,5800237,43160414
                  ,18400588,16055610,34937257,2420847,48064371,11798116,42744933,58022321
                  ,16866606,386460,32485206,22872090,5348081,2317936,1429915,4195131
                  ,25599519,26980631,22210202,38670109,5589713,12153974,9465559,19584650
                  ,5000083,16926223,9247486,43135864,22629575,28225141,2234898,29180144
                  ,4129005,53382635,30852693,8775309,3580483,28232627,1553931,28244863
                  ,21031984,627159,16856387,16654685,6482129,48710686,22927818,22177836
                  ,5735754,7322078,28296836,8590283,29607324,9324692,51994578,6829746
                  ,11803517,38537091,28110918,20696318,22980304,1940702,31873251,35557779
                  ,22716547,29902599,22774919,15901201,29717766,3895545,896847,7363105
                  ,22323298,29609623,42730523,44600963,43144718,25618229,1444871,41575819
                  ,16654942,33878037,38225964,24134333,16024639,28467772,56353966,1214682
                  ,9365437,2897353,57415376,11816809,29540781,1486295,2297605,2526291
                  ,28344867,29389096,3636262,12116020,5232622,17810549,2571051,28336918
                  ,2909487,12423910,46778063,12316078,7934753,22772818,21412272,28339110
                  ,26304456,14431453,22774021,56543901,58832998,29134693,33196834,2819122
                  ,21760410,9482298,10657485,27416926,9019458,4994623,19074488,19076275
                  ,28271306,14745189,24505725,29407914,49554867,14087450,12022414,26642281
                  ,2022563,12839431,7868935,20399302,28585445,12328365,4566171,972912
                  ,9593830,14120445,12011944,33752728,17283483,29007313,872368,27929587
                  ,26451509,29248730,11504169,1002187,7937819,1699232,10189099,8590102
                  ,26626900,872322,24309369,16968578,14465737,19851958,29553575,29554021
                  ,29288553,29676416,29571335,22539936,22798627,23063491,22796581,7480416
                  ,2249206,1220475,21167514,3152194,56169948,55984416,21895045,25925954
                  ,26325017,56352865,45088687,2358429,39894325,22524756,7269020,2848520
                  ,1972646,44601432,28416923,20912560,14415459,23272704,28660020,28664501
                  ,19074600,25599205,25177848,1996983,12392207,12134236,24234375,7046667
                  ,9411559,28824041,15628450,11342835,8933214,8935080,24519642,4118054
                  ,8815673,22691401,19649913,2025595,8252229,8718385,22631517,19189970
                  ,251088,8286126,19173723,6894445,14480979,9097958,5440697,32593402
                  ,8935199,5561856,34017069,13843017,25445599,6217891,3135736,36117504
                  ,14850616,2213933,25150292,14499354,14477019,940578,10067309,13416032
                  ,16725764,1537598,11121666,25361732,2686469,3023210,22877995,13905594
                  ,20920255,8933196,4481626,20152023,3950787,42178,29652555,12378669
                  ,25450968,20193905,1218628,3045108,262829,9649277,10815431,30676894
                  ,12023312,16654494,4670803,12840134,7605385,25154028,2841150,4659378
                  ,17258063,27578986,29208137,1997856,22564378,3656936,6935841,9539337
                  ,2906659,25749869,22520331,1191035,7344324,13611045,1219804,34513708
                  ,2069180,2393421,11895936,22787378,3875845,8130884,3606445,12724452
                  ,34492043,9345852,9345223,5691326,3980414,4770371,5613564,3879103
                  ,5465068,461481,8353754,7189684)
uk_images_m$image_url[which(uk_images_m$pageid %in% remove_images)] <- NA
uk_images_m <- as.data.frame(uk_images_m)
uk_images_m$ethnicity <- "white"
uk_images_m$ethnicity <- ifelse(is.na(uk_images_m$image_url), NA, uk_images_m$ethnicity)

uk_parlthesaurus_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                             entity = uk_entities_m, property = "P4527", 
                             serial = TRUE)
uk_rush_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                    entity = uk_entities_m, property = "P4471", 
                    serial = TRUE)
uk_national_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                        entity = uk_entities_m, property = "P3029", 
                        serial = TRUE)
uk_hansard_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                       entity = uk_entities_m, property = "P2015", 
                       serial = TRUE)
uk_theyworkforyou_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                              entity = uk_entities_m, property = "P2171", 
                              serial = TRUE)
uk_gndid_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                     entity = uk_entities_m, property = "P227", 
                     serial = TRUE)
uk_libcon_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                      entity = uk_entities_m, property = "P244", 
                      serial = TRUE)
uk_freebase_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                        entity = uk_entities_m, property = "P646", 
                        serial = TRUE)
uk_munzinger_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                         entity = uk_entities_m, property = "P1284", 
                         serial = TRUE)
uk_nndb_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                    entity = uk_entities_m, property = "P1263", 
                    serial = TRUE)
uk_imdb_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                    entity = uk_entities_m, property = "P345", 
                    serial = TRUE)
uk_brittanica_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                          entity = uk_entities_m, property = "P1417", 
                          serial = TRUE)
uk_quora_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                     entity = uk_entities_m, property = "P3417", 
                     serial = TRUE)
uk_id_m <- full_join(x = uk_parlthesaurus_m, y = uk_rush_m, by = "wikidataid") %>%
  full_join(x = ., y = uk_national_m, by = "wikidataid") %>%
  full_join(x = ., y = uk_hansard_m, by = "wikidataid") %>%
  full_join(x = ., y = uk_theyworkforyou_m, by = "wikidataid") %>%
  full_join(x = ., y = uk_gndid_m, by = "wikidataid") %>%
  full_join(x = ., y = uk_libcon_m, by = "wikidataid") %>%
  full_join(x = ., y = uk_freebase_m, by = "wikidataid") %>%
  full_join(x = ., y = uk_munzinger_m, by = "wikidataid") %>%
  full_join(x = ., y = uk_nndb_m, by = "wikidataid") %>%
  full_join(x = ., y = uk_imdb_m, by = "wikidataid") %>%
  full_join(x = ., y = uk_brittanica_m, by = "wikidataid") %>%
  full_join(x = ., y = uk_quora_m, by = "wikidataid")
names(uk_id_m) <- c("wikidataid", "parlthesaurus",
                  "rush", "national", "hansard", "theyworkforyou",
                  "gndid", "libcon", "freebase", "munzinger", "nndb", "imdb", 
                  "brittanica", "quora")
uk_positions_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                         entity = uk_entities_m, unique = TRUE, 
                         property = "P39")
uk_positions_m <- uk_positions_m %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
uk_occupation_m <- wikiData(item = na.omit(unique(uk_m$wikidataid)), 
                          entity = uk_entities_m, unique = TRUE, 
                          property = "P106")
uk_occupation_m <- uk_occupation_m %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())

# assemble data -------------------------------------------------------------------------
uk_m3 <- left_join(x = uk_m, y = uk_title_m, by = "pageid") %>%
  left_join(x = ., y = uk_images_m[,c(1,3)], by = "pageid") %>%
  left_join(x = ., y = uk_sex_m, by = "wikidataid") %>%
  left_join(x = ., y = uk_religion_m, by = "wikidataid") %>%
  left_join(x = ., y = uk_birth_m, by = "wikidataid") %>%
  left_join(x = ., y = uk_death_m, by = "wikidataid") %>%
  left_join(x = ., y = uk_birthplace_m, by = "wikidataid") %>%
  left_join(x = ., y = uk_deathplace_m, by = "wikidataid")
colnames(uk_m3)[c(10,12,16,17)] <- c("session", "wikititle", "birth", "death")
uk_m3 <- uk_m3 %>% select(country, pageid, wikidataid, wikititle, name, sex, 
                    ethnicity, religion, birth, death, birthplace, deathplace, 
                    session, party, constituency, session_start, 
                    session_end, service)
uk_m3$unique_id <- str_c(as.integer(
  as.factor(ifelse(is.na(uk_m3$pageid), str_c(uk_m3$name, uk_m3$session), NA))), "miss")
uk_m3$pageid <- ifelse(is.na(uk_m3$pageid), uk_m3$unique_id, uk_m3$pageid)
uk_m3$unique_id2 <- str_c(as.integer(as.factor(ifelse(!is.na(uk_m3$wikidataid) & is.na(uk_m3$wikititle),
                           uk_m3$wikidataid, NA))), "miss2")
uk_m3$pageid <- ifelse(!is.na(uk_m3$wikidataid) & is.na(uk_m3$wikititle), uk_m3$unique_id2,
                       uk_m3$pageid)
uk_core_m <- uk_m3[!duplicated(uk_m3$pageid), 1:12]
uk_political_m <- uk_m3[c(2, 13:18)]
uk_core_m$sex <- as.character(uk_core_m$sex)
uk_core_m$religion <- as.character(uk_core_m$religion)
remove_ids <- uk_m3[which(uk_m3$wikidataid %in% uk_pol$wikidataid),]$wikidataid
uk_core_m <- uk_core_m[!(uk_core_m$wikidataid %in% remove_ids),]
uk_political_m$party2 <- uk_political_m$party
uk_political_m$party2 <- str_replace_all(uk_political_m$party2, "\\*|K\\[edit\\]| \\&.+|\\(.+|, then.+| a$|>.+|>.+|Foxite |Pittite\\/|--|-|–|\\||\\[.\\]", "") %>%
  str_trim %>%
  str_replace_all("Tory.+|Canningite.+|Canningite", "Tory") %>%
  str_replace_all("Whig.+|cWhig|Irish Whig", "Whig") %>%
  str_replace_all("Radical.+|Radical", "Radicals") %>%
  str_replace_all("UltraTory", "Ultra-Tory") %>%
  str_replace_all("Speaker of the House of Commons", "Independent") %>%
  str_replace_all("Home Rule", "Home Rule League") %>%
  str_replace_all("AllforIreland|AllforIreland", "All-for-Ireland") %>% 
  str_replace_all("AntiParnellite", "Anti-Parnellite") %>%
  str_replace_all("AntiWaste", "Anti-Waste") %>%
  str_replace_all("League", "") %>%
  str_trim %>%
  str_replace_all("Co\\. |Coalition|\\/Unionist", "") %>%
  str_replace_all("Independent.+", "Independent") %>%
  ifelse(. == "Irish parliamentary" | . == "Irish Parliamentary", "Irish Parliamentary Party", .) %>%
  ifelse(. == "UUP" | . == "Unionist", "Ulster Unionist", .) %>%
  ifelse(. == "Irish Unionist", "Irish Unionist Party", .) %>%
  str_replace_all("Liberal.+", "Liberal") %>%
  str_replace_all("liberal|LibLab", "Liberal") %>%
  ifelse(. == "NI Labour" | . == "Labour Unionist", "Labour", .) %>%
  str_replace_all("NDP", "National Democratic Party") %>%
  str_replace_all("SNP", "Scottish National Party") %>%
  ifelse(. == "NADSS", "National Association of Discharged Sailors and Soldiers", .) %>%
  ifelse(. == "Labour Coop", "Labour and Co-operative", .) %>%
  str_trim 
uk_political_m$party2 <- ifelse(uk_political_m$party2 == "Ind" | uk_political_m$party2 == "Ind. Conservative" | 
                                  uk_political_m$party2 == "Indep Conservative" | uk_political_m$party2 == "Ind. Unionist" | 
                                  uk_political_m$party2 == "Speaker",
                                "Independent", uk_political_m$party2)
uk_political_m$party2 <- ifelse(uk_political_m$party2 == "Irish", "Independent Irish Party", uk_political_m$party2)

uk_political_m[uk_political_m$session == 1,"session_start"] <- "1801-01-01"
uk_political_m[uk_political_m$session == 1,"session_end"] <- "1802-07-05"
uk_political_m[uk_political_m$session == 2,"session_start"] <- "1802-07-05"
uk_political_m[uk_political_m$session == 2,"session_end"] <- "1806-10-29"
uk_political_m[uk_political_m$session == 3,"session_start"] <- "1806-10-29"
uk_political_m[uk_political_m$session == 3,"session_end"] <- "1807-05-04"
uk_political_m[uk_political_m$session == 4,"session_start"] <- "1807-05-04"
uk_political_m[uk_political_m$session == 4,"session_end"] <- "1812-10-05"
uk_political_m[uk_political_m$session == 5,"session_start"] <-  "1812-10-05"
uk_political_m[uk_political_m$session == 5,"session_end"] <- "1818-08-04"
uk_political_m[uk_political_m$session == 6,"session_start"]  <- "1818-08-04"
uk_political_m[uk_political_m$session == 6,"session_end"] <- "1820-03-06"
uk_political_m[uk_political_m$session == 7,"session_start"]  <- "1820-03-06"
uk_political_m[uk_political_m$session == 7,"session_end"] <- "1826-06-07"
uk_political_m[uk_political_m$session == 8,"session_start"]  <- "1826-06-07"
uk_political_m[uk_political_m$session == 8,"session_end"] <- "1830-07-29"
uk_political_m[uk_political_m$session == 9,"session_start"]  <- "1830-07-29" 
uk_political_m[uk_political_m$session == 9,"session_end"] <- "1831-04-28"
uk_political_m[uk_political_m$session == 10,"session_start"]  <- "1831-04-28"
uk_political_m[uk_political_m$session == 10,"session_end"] <-  "1832-12-08"
uk_political_m[uk_political_m$session == 11,"session_start"]  <- "1832-12-08" 
uk_political_m[uk_political_m$session == 11,"session_end"] <- "1835-01-06"
uk_political_m[uk_political_m$session == 12,"session_start"]  <- "1835-01-06"
uk_political_m[uk_political_m$session == 12,"session_end"] <- "1837-07-24"
uk_political_m[uk_political_m$session == 13,"session_start"]  <- "1837-07-24"
uk_political_m[uk_political_m$session == 13,"session_end"] <- "1841-06-29"
uk_political_m[uk_political_m$session == 14,"session_start"]  <- "1841-06-29" 
uk_political_m[uk_political_m$session == 14,"session_end"] <- "1847-07-29" 
uk_political_m[uk_political_m$session == 15,"session_start"] <- "1847-07-29" 
uk_political_m[uk_political_m$session == 15,"session_end"] <- "1852-07-07"
uk_political_m[uk_political_m$session == 16,"session_start"]  <- "1852-07-07"
uk_political_m[uk_political_m$session == 16,"session_end"] <-  "1857-03-27"
uk_political_m[uk_political_m$session == 17,"session_start"]  <-  "1857-03-27"
uk_political_m[uk_political_m$session == 17,"session_end"] <- "1859-04-28"
uk_political_m[uk_political_m$session == 18,"session_start"] <- "1859-04-28"
uk_political_m[uk_political_m$session == 18,"session_end"] <- "1865-07-11"
uk_political_m[uk_political_m$session == 19,"session_start"] <- "1865-07-11"  
uk_political_m[uk_political_m$session == 19,"session_end"] <- "1868-11-17"
uk_political_m$session <- as.integer(uk_political_m$session)
uk_political_m$session_start <- ymd(uk_political_m$session_start)
uk_political_m$session_end <- ymd(uk_political_m$session_end)
uk_political_m$service <- as.integer(uk_political_m$service)
uk_political_m <- dplyr::select(uk_political_m, -party2)
uk_traffic_m <- uk_traffic_m[,-2]
uk_traffic_m$date <- uk_traffic_m$date %>% as.POSIXct(tz = "UTC")
uk_traffic_m$pageid <- as.integer(uk_traffic_m$pageid)
uk_history_m <- uk_history_m %>% select(pageid, revid, parentid, user, userid,
                                        timestamp, size, comment)
uk_history_m$timestamp <- uk_history_m$timestamp %>% str_replace("T", " ") %>%
                          as.POSIXct(tz = "UTC")
uk_portrait_m <- filter(uk_images_m, !is.na(image_url)) %>%
  dplyr::select(-ethnicity)

# import current legislator data --------------------------------------------------------
gbr_core <- readRDS("./package/legislatoR-data-v0.1.0/data/gbr_core")
gbr_political <- readRDS("./package/legislatoR-data-v0.1.0/data/gbr_political")
gbr_traffic <- readRDS("./package/legislatoR-data-v0.1.0/data/gbr_traffic")
gbr_office <- readRDS("./package/legislatoR-data-v0.1.0/data/gbr_office")
gbr_portrait <- readRDS("./package/legislatoR-data-v0.1.0/data/gbr_portrait")
gbr_profession <- readRDS("./package/legislatoR-data-v0.1.0/data/gbr_profession")
gbr_history <- readRDS("./package/legislatoR-data-v0.1.0/data/gbr_history")
gbr_ids <- readRDS("./package/legislatoR-data-v0.1.0/data/gbr_ids")

# add update ----------------------------------------------------------------------------
gbr_core_new <- rbind(gbr_core, uk_core_m)
gbr_political_new <- rbind(gbr_political, uk_political_m) %>%
  arrange(session)
gbr_traffic_new
uk_traffic_m <- uk_traffic_m[!(uk_traffic_m$pageid %in% gbr_traffic$pageid),]
gbr_traffic_new <- rbind(gbr_traffic, uk_traffic_m) %>%
  arrange(pageid, date)
uk_occupation_m <- uk_occupation_m[!(uk_occupation_m$wikidataid %in% gbr_profession$wikidataid),]
gbr_profession_new <- rbind.fill(gbr_profession, uk_occupation_m)
gbr_profession_new <- gbr_profession_new %>%
  dplyr::select(-unknown) %>%
  setcolorder(order(names(.))) %>% 
  select(wikidataid, everything())
gbr_profession_new[is.na(gbr_profession_new)] <- FALSE
uk_positions_m <- uk_positions_m[!(uk_positions_m$wikidataid %in% gbr_office$wikidataid),]
gbr_office_new <- rbind.fill(gbr_office, uk_positions_m)
gbr_office_new <- gbr_office_new %>%
  setcolorder(order(names(.))) %>% 
  select(wikidataid, everything())
gbr_office_new[is.na(gbr_office_new)] <- FALSE
uk_portrait_m <- uk_portrait_m[!(uk_portrait_m$pageid %in% gbr_portrait$pageid),]
gbr_portrait_new <- rbind(gbr_portrait, uk_portrait_m)
uk_id_m <- uk_id_m[!(uk_id_m$wikidataid %in% gbr_ids$wikidataid),]
gbr_ids_new <- rbind.fill(gbr_ids, uk_id_m)
uk_history_m <- uk_history_m[!(uk_history_m$pageid %in% gbr_history$pageid),]
uk_history_m <- uk_history_m %>%
  select(pageid, revid, parentid, user, userid,
         timestamp, size, comment)
uk_history_m$timestamp <- uk_history_m$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")
gbr_history_new <- rbind(gbr_history, uk_history_m)

# save to disk --------------------------------------------------------------------------
saveRDS(gbr_core_new, "./package/legislatoR-data-v0.2.0/gbr_core")
saveRDS(gbr_political_new, "./package/legislatoR-data-v0.2.0/gbr_political")
saveRDS(gbr_history_new, "./package/legislatoR-data-v0.2.0/gbr_history")
saveRDS(gbr_ids_new, "./package/legislatoR-data-v0.2.0/gbr_ids")
saveRDS(gbr_traffic_new, "./package/legislatoR-data-v0.2.0/gbr_traffic")
saveRDS(gbr_office_new, "./package/legislatoR-data-v0.2.0/gbr_office")
saveRDS(gbr_profession_new, "./package/legislatoR-data-v0.2.0/gbr_profession")
saveRDS(gbr_portrait_new, "./package/legislatoR-data-v0.2.0/gbr_portrait")


#### UPDATE TWITTER HANDLES OF CURRENT US CONGRESS ======================================

# import new ids ------------------------------------------------------------------------
house_twitter <- readRDS("D:/Sascha/projects/legislatoR/data/missings_1/twitter_us/house_output")
house_twitter <- house_twitter[,c(1,3)]
senate_twitter <- readRDS("D:/Sascha/projects/legislatoR/data/missings_1/twitter_us/senate_output")
senate_twitter <- senate_twitter[,c(1,3)]

# import old id -------------------------------------------------------------------------
usa_house_social <- readRDS("./package/legislatoR-data-v0.1.0/data/usa_house_social")
usa_senate_social <- readRDS("./package/legislatoR-data-v0.1.0/data/usa_senate_social")

# merge ---------------------------------------------------------------------------------
usa_house_social <- full_join(usa_house_social, house_twitter, by = "wikidataid")
usa_house_social$ODU.WSDL <- ifelse(is.na(usa_house_social$ODU.WSDL), 
                                    usa_house_social$twitter, usa_house_social$ODU.WSDL)
# 256 house Twitter handles added, plenty updated
usa_senate_social <- full_join(usa_senate_social, senate_twitter, by = "wikidataid")
usa_senate_social$ODU.WSDL <- ifelse(is.na(usa_senate_social$ODU.WSDL), 
                                    usa_senate_social$twitter, usa_senate_social$ODU.WSDL)
# 37 house Twitter handles added, plenty updated
saveRDS(usa_house_social, "./package/legislatoR-data-v0.2.0/usa_house_social")
saveRDS(usa_senate_social, "./package/legislatoR-data-v0.2.0/usa_senate_social")


#### UPDATE RELIGIOUS AFFILIATION OF MEMBERS OF US CONGRESS =============================

# import new ----------------------------------------------------------------------------
house_religion <- readRDS("D:/Sascha/projects/legislatoR/data/missings_1/religion_us/house_output") %>%
  filter(!is.na(religion_new))
senate_religion <- readRDS("D:/Sascha/projects/legislatoR/data/missings_1/religion_us/senate_output") %>%
  filter(!is.na(religion_new))

# import old ----------------------------------------------------------------------------
usa_house_core <- readRDS("./package/legislatoR-data-v0.1.0/data/usa_house_core")
usa_senate_core <- readRDS("./package/legislatoR-data-v0.1.0/data/usa_senate_social")

# recode
key_religion <- c("Congregationalist" = "protestantism baptist", 
                  "Congregationalist; Episcopalian" = "protestantism baptist", 
                  "unknown" = NA,
                  "Episcopalian" = "anglicanism",
                  "Presbyterian" = "protestantism reformed",
                  "Catholic" = "catholicism",
                  "Calvinist"  = "protestantism reformed",
                  "Unitarian" = "protestantism unitarian",
                  "Quaker" = "protestantism quaker",
                  "Dutch Reformed Church"  = "protestantism reformed",
                  "Lutheran" = "protestantism lutheran",                       
                  "Quaker, Episcopalian" = "protestantism quaker",                  
                  "German Reformed Church" =  "protestantism reformed",
                  "Huguenot"  =  "protestantism reformed",                  
                  "Jewish"   =  "judaism",            
                  "Baptist"  =  "protestantism baptist",
                  "Methodist"   = "protestantism methodist",       
                  "\"Protestant\""   = "protestantism",
                  "United Church of Christ"  =  "protestantism reformed",
                  "African Methodist Episcopal Zion"  = "protestantism methodist",
                  "Church of Christ"  = "protestantism restorationism",
                  "Unitarian-Universalist"  = "protestantism unitarian", 
                  "Seventh-day Adventist"  = "protestantism restorationism",
                  "Christian Church (Disciples of Christ)"  = "protestantism restorationism",
                  "Greek Orthodox" = "orthodox eastern",
                  "Christian Scientist"  = "protestantism christian science",
                  "Latter-day Saint"  = "protestantism restorationism",
                  "Christian Churches and Churches of Christ (CC/CC)" = "protestantism restorationism",
                  "\"Christian\""  = "catholicism",
                  "unknown; Freemason" = NA,
                  "Eastern Orthodox" = "orthodox eastern",
                  "Christian Reformed"  =  "protestantism reformed",
                  "Presbyerian"  = "protestantism reformed",
                  "African Methodist Episcopal" = "protestantism methodist",
                  "unspecified"  = NA,
                  "Protestant; Objectivist" = "protestantism", 
                  "Independent Christian (probably Stone-Campbell)"  = "protestantism restorationism",
                  "Christian Reformed Church" =  "protestantism reformed",
                  "Christian" = "catholicism",
                  "Presbyterian (PCA)" =  "protestantism reformed",
                  "Catholic; Scientologist"  = "catholicism",
                  "United Brethren in Christ" = "protestantism anabaptism",
                  "General Baptist" =  "protestantism baptist",
                  "Assemblies of God" =  "protestantism pentecostal",
                  "Southern Baptist" = "protestantism baptist",
                  "Christian Missionary Alliance"  =  "protestantism evangelical",
                  "International Church of the Foursquare Gospel"  = "protestantism pentecostal",
                  "Existentialist?"  = "existentialism",
                  "Evangelical Methodist"  = "protestantism evangelical",
                  "Nazarene" = NA,
                  "Congregationalist Baptist"  = "protestantism baptist",
                  "Protestant; Scientologist"  = "protestantism", 
                  "United Methodist" = "protestantism methodist",
                  "Latter-day Saint (RLDS)" = "protestantism restorationism",  
                  "McLean Bible Church" = "protestantism", 
                  "Moravian" = "protestantism proto",
                  "Christian Church (Stone-Campbell)"  = "protestantism restorationism",  
                  "GLBT (unspecified)" = NA,
                  "Protestant" = "protestantism",
                  "Community Church" = "protestantism",
                  "Church of Christ (Stone-Campbell)"  = "protestantism restorationism",  
                  "Evangelical (Grace Evangelical Free Church)" = "protestantism evangelical",
                  "??" = NA,
                  "Episcopalian; Presbyterian" = "protestantism reformed",
                  "Evangelical" = "protestantism evangelical",
                  "Catholic; Native American" = "catholicism")
senate_religion$religion_new <- recode(senate_religion$religion_new, !!!key_religion)
house_religion$religion_new <- recode(house_religion$religion_new, !!!key_religion)
house_religion <- house_religion %>%
  filter(!is.na(religion_new))
senate_religion <- senate_religion %>%
  filter(!is.na(religion_new))

# merge ---------------------------------------------------------------------------------
usa_house_core <- left_join(usa_house_core, house_religion[,c(1,4)], by = "wikidataid")
usa_house_core$religion <- ifelse(is.na(usa_house_core$religion), usa_house_core$religion_new, usa_house_core$religion)
usa_house_core <- select(usa_house_core, -religion_new)
# religion of 429 house mebers added
usa_senate_core <- left_join(usa_senate_core, senate_religion[,c(1,4)], by = "wikidataid")
usa_senate_core$religion <- ifelse(is.na(usa_senate_core$religion), usa_senate_core$religion_new, usa_senate_core$religion)
usa_senate_core <- select(usa_senate_core, -religion_new)
# religion of 68 senate members added
saveRDS(usa_house_core, "./package/legislatoR-data-v0.2.0/usa_house_core")
saveRDS(usa_senate_core, "./package/legislatoR-data-v0.2.0/usa_senate_core")

#### UPDATE RELIGIOUS AFFILIATION OF MEMBERS OF CANADIAN PARL ===========================
# import new
canada_religion <- readRDS("D:/Sascha/projects/legislatoR/data/missings_1/religion_canada/Canada_jew_output")

# import old
can_core <- readRDS("./package/legislatoR-data-v0.1.0/data/can_core")

# merge
can_core <- left_join(can_core, canada_religion[,c(1,4)], by = "wikidataid")
can_core$religion <- ifelse(is.na(can_core$religion), can_core$religion_new, can_core$religion)
can_core <- select(can_core, -religion_new)
# 38 added
saveRDS(can_core, "./package/legislatoR-data-v0.2.0/can_core")

#### UPDATE ETHNICITY OF MEMBERS OF CANADIAN PARL =======================================

# import new
canada_ethnicity <- readRDS("D:/Sascha/projects/legislatoR/data/missings_1/ethnicity_canada/Canada_ind_output")
# merge
can_core <- left_join(can_core, canada_ethnicity[,c(1,4)], by = "wikidataid")
can_core$ethnicity <- ifelse(is.na(can_core$ethnicity), can_core$ethnicity_new, can_core$ethnicity)
can_core <- select(can_core, -ethnicity_new)
# 28 added
saveRDS(can_core, "./package/legislatoR-data-v0.2.0/can_core")

#### UPDATE ETHNICITY OF MEMBERS OF UK PARL =============================================

# import new
uk_ethnicity <- readRDS("D:/Sascha/projects/legislatoR/data/missings_1/ethnicity_uk/UK_min_output")

# import old
gbr_core <- readRDS("./package/legislatoR-data-v0.2.0/gbr_core")

# adjust key and recode
key_ethn <- c("Anglo-Indian" = "asian",
              "Black British/White British (Mixed Race)" = "black",
              "Black British" = "black",
              "British Indian" = "asian",                    
              "British Sri Lankan" = "asian",                   
              "African American/White British (Mixed Race)" = "black", 
              "British Pakistani" = "asian",
              "Somali/White British (Mixed Race)" = "black",
              "British Bangladeshi" = "asian",
              "British Iraqi" = "asian",              
              "British Chinese" = "asian",                    
              "British Pakistani/White British (Mixed Race)" = "asian",
              "British Iranian/White British (Mixed Race)"  = "asian",
              "British Cypriot" = "white",
              "British Arab" = "asian")
uk_ethnicity$ethnicity_new <- recode(uk_ethnicity$ethnicity_new, !!!key_ethn)

# merge
gbr_core <- left_join(gbr_core, uk_ethnicity[,c(1,4)], by = "wikidataid")
gbr_core$ethnicity <- ifelse(is.na(gbr_core$ethnicity_new), gbr_core$ethnicity, gbr_core$ethnicity_new)
gbr_core <- select(gbr_core, -ethnicity_new)
# 8 added, many corrected
saveRDS(gbr_core, "./package/legislatoR-data-v0.2.0/gbr_core")