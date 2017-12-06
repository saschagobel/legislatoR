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

# import data ---------------------------------------------------------------------------
for (i in 1:length(list.files("./data/"))) {
  object <- readRDS(file = paste0("./data/", list.files("./data/")[i]))
  assign(x = list.files("./data/")[i], value = object)
}
rm(object, i)


#### DATA PREPARATION ===================================================================

# join core data, adjust names, and split into core and political -----------------------

# austrian nationalrat
austria <- left_join(x = austria, y = austria_title, by = "pageid") %>%
  left_join(x = ., y = austria_faces[,c(1,17)], by = "pageid") %>%
  left_join(x = ., y = austria_sex, by = "wikidataid") %>%
  left_join(x = ., y = austria_religion, by = "wikidataid") %>%
  left_join(x = ., y = austria_birth, by = "wikidataid") %>%
  left_join(x = ., y = austria_death, by = "wikidataid") %>%
  left_join(x = ., y = austria_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = austria_deathplace, by = "wikidataid")
colnames(austria)[c(7, 8, 9, 11, 15, 16)] <- c("session", "session_start", "session_end",
                                               "wikititle", "birth", "death")
austria <- austria %>% select(pageid, wikidataid, wikititle, name, sex, ethnicity,
                              religion, birth, death, birthplace, deathplace, session,
                              party, constituency, session_start, session_end, service)
austria_core <- austria[!duplicated(austria$pageid), 1:11]
austria_political <- austria[c(1, 12:17)]

# french assemble
france <- left_join(x = france, y = france_title, by = "pageid") %>%
  left_join(x = ., y = france_faces[,c(1,2)], by = "pageid") %>%
  left_join(x = ., y = france_sex, by = "wikidataid") %>%
  left_join(x = ., y = france_religion, by = "wikidataid") %>%
  left_join(x = ., y = france_birth, by = "wikidataid") %>%
  left_join(x = ., y = france_death, by = "wikidataid") %>%
  left_join(x = ., y = france_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = france_deathplace, by = "wikidataid")
colnames(france)[c(7, 8, 12, 13)] <- c("session", "wikititle", "birth", "death")
france <- france %>% select(pageid, wikidataid, wikititle, name, sex, ethnicity,
                            religion, birth, death, birthplace, deathplace, session,
                            party, constituency)
france_core <- france[!duplicated(france$pageid), 1:11]
france_political <- france[c(1, 12:14)]

# german bundestag
germany <- left_join(x = germany, y = germany_title, by = "pageid") %>%
  left_join(x = ., y = germany_faces[,c(1,17)], by = "pageid") %>%
  left_join(x = ., y = germany_sex, by = "wikidataid") %>%
  left_join(x = ., y = germany_religion, by = "wikidataid") %>%
  left_join(x = ., y = germany_birth, by = "wikidataid") %>%
  left_join(x = ., y = germany_death, by = "wikidataid") %>%
  left_join(x = ., y = germany_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = germany_deathplace, by = "wikidataid")
colnames(germany)[c(8, 9, 10, 12, 16, 17)] <- c("session", "session_start", "session_end",
                                              "wikititle", "birth", "death")
germany <- germany %>% select(pageid, wikidataid, wikititle, name, sex, ethnicity,
                              religion, birth, death, birthplace, deathplace, session,
                              party, constituency, constituency2, session_start, session_end, service)
germany_core <- germany[!duplicated(germany$pageid), 1:11]
germany_political <- germany[c(1, 12:18)]

# irish dail
ireland <- left_join(x = ireland, y = ireland_title, by = "pageid") %>%
  left_join(x = ., y = ireland_faces[,c(1,17)], by = "pageid") %>%
  left_join(x = ., y = ireland_sex, by = "wikidataid") %>%
  left_join(x = ., y = ireland_religion, by = "wikidataid") %>%
  left_join(x = ., y = ireland_birth, by = "wikidataid") %>%
  left_join(x = ., y = ireland_death, by = "wikidataid") %>%
  left_join(x = ., y = ireland_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = ireland_deathplace, by = "wikidataid")
colnames(ireland)[c(7, 8, 12, 13)] <- c("session", "wikititle", "birth", "death")
ireland <- ireland %>% select(pageid, wikidataid, wikititle, name, sex, ethnicity,
                              religion, birth, death, birthplace, deathplace, session,
                              party, constituency)
ireland_core <- ireland[!duplicated(ireland$pageid), 1:11]
ireland_political <- ireland[c(1, 12:14)]

# united states house
usah <- left_join(x = usah, y = usah_title, by = "pageid") %>%
  left_join(x = ., y = usah_faces[,c(1,17)], by = "pageid") %>%
  left_join(x = ., y = usah_sex, by = "wikidataid") %>%
  left_join(x = ., y = usah_religion, by = "wikidataid") %>%
  left_join(x = ., y = usah_birth, by = "wikidataid") %>%
  left_join(x = ., y = usah_death, by = "wikidataid") %>%
  left_join(x = ., y = usah_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = usah_deathplace, by = "wikidataid")
colnames(usah)[c(7, 8, 9, 11, 15, 16)] <- c("session", "session_start", "session_end",
                                               "wikititle", "birth", "death")
usah <- usah %>% select(pageid, wikidataid, wikititle, name, sex, ethnicity,
                        religion, birth, death, birthplace, deathplace, session,
                        party, constituency, session_start, session_end, service)
usah_core <- usah[!duplicated(usah$pageid), 1:11]
usah_political <- usah[c(1, 12:17)]

# united states senate
usas <- left_join(x = usas, y = usas_title, by = "pageid") %>%
  left_join(x = ., y = usas_faces[,c(1,17)], by = "pageid") %>%
  left_join(x = ., y = usas_sex, by = "wikidataid") %>%
  left_join(x = ., y = usas_religion, by = "wikidataid") %>%
  left_join(x = ., y = usas_birth, by = "wikidataid") %>%
  left_join(x = ., y = usas_death, by = "wikidataid") %>%
  left_join(x = ., y = usas_birthplace, by = "wikidataid") %>%
  left_join(x = ., y = usas_deathplace, by = "wikidataid")
colnames(usas)[c(7, 8, 9, 11, 15, 16)] <- c("session", "session_start", "session_end",
                                            "wikititle", "birth", "death")
usas <- usas %>% select(pageid, wikidataid, wikititle, name, sex, ethnicity,
                        religion, birth, death, birthplace, deathplace, session,
                        party, constituency, session_start, session_end, service)
usas_core <- usas[!duplicated(usas$pageid), 1:11]
usas_political <- usas[c(1, 12:17)]

# format political data -----------------------------------------------------------------

# austrian nationalrat
austria_political$party <- str_replace(austria_political$party, ", OK|, LBd|, L|, F|/LIF", "")
austria_political$party <- ifelse(austria_political$party == "F", "FPÖ",
                             ifelse(austria_political$party == "L", "LIF",
                             ifelse(austria_political$party == "CSP", "CsP",
                             ifelse(austria_political$party == "Stronach", "STRONACH",
                             ifelse(austria_political$party == "LB" | austria_political$party == "KuL", "KPÖ",
                             ifelse(austria_political$party == "Grüne", "GRÜNE",
                             ifelse(austria_political$party == "fraktionslos" | austria_political$party == "OK", "none",
                             austria_political$party)))))))
austria_political$constituency <- str_replace(austria_political$constituency, "Bundeswahlvorschlag, | .+| -.+", "")
austria_political$constituency <- ifelse(austria_political$constituency == "Bundeswahlvorschlag", "BWV",
                                         ifelse(austria_political$constituency == "", NA, austria_political$constituency))

# french assemble
france_political$party <- str_replace(france_political$party, "a\\.( )?|\\(app\\.\\)| \\(Les Verts\\)|\\*", "")
france_political$party <- str_replace(france_political$party, "Non.+|NI", "Non-Inscrit")
france_political$party <- str_trim(france_political$party)
france_political$party <- ifelse(france_political$party == "Communiste" | france_political$party == "C", "PCF",
                          ifelse(france_political$party == "S" | france_political$party == "Socialiste", "SFIO", france_political$party))
france_political$party[which(france_political$party == "")] <- NA
france_political$constituency <- str_replace(france_political$constituency, "e\\)|re\\)", ")")

# german bundestag
germany_political$party <- str_replace(germany_political$party, " \\(GDP\\)", "")
germany_political$party <- ifelse(germany_political$party == "Bündnis 90" | germany_political$party == "Die Grünen" |
                                  germany_political$party == "Bündnis 90/Die Grünen" | germany_political$party == "GRÜNE", "BÜNDNIS 90/DIE GRÜNEN" ,
                           ifelse(germany_political$party == "SSW", "SPD",
                           ifelse(germany_political$party == "Die Linke", "DIE LINKE",
                           ifelse(germany_political$party == "unabhängig" | germany_political$party == "fraktionslos", "none",
                                  germany_political$party))))
germany_political$constituency <- str_replace(germany_political$constituency, "Rheinlald-Pfalz", "Rheinland-Pfalz")

# irish dail
ireland_political$party <- str_replace(ireland_political$party, " \\(Pro-Treaty\\)| \\(Anti-Treaty\\)| \\(Workers' Party\\)", "")

# united states house
usah_political$party <- str_replace_all(usah_political$party, "\\, then.+|until.+|then.+| to.+|[[:digit:]].+|\\.$|^(Territory|Northwest|William|Mississippi|Indiana|Duke|Orleans|Illinois|Missouri|Rufus|Nathaniel|John|Alabama|Arkansas|Michigan|Solomon|Florida|Ambrose|Henry|Minnesota|Utah|Nebraska|Nevada|Dakota|Lewis|Hawaii|Puerto|Philippines|George|Clark|Johnny|Jesse|Benjamin).+|Buck|Bud|Bo|Chip|Jake|Kika|Pete|Tip|Duke", "")
usah_political$party <- ifelse(usah_political$party == "I" | usah_political$party == "Ind" | usah_political$party == "Ind.", "Independent",
                        ifelse(usah_political$party == "ID" | usah_political$party == "Independent D", "Independent Democrat",
                        ifelse(usah_political$party == "IR", "Independent Republican",
                        ifelse(usah_political$party == "Proh",  "Prohibitionist",
                        ifelse(usah_political$party == "Prog" | usah_political$party == "Prog R.", "Progressive",
                        ifelse(usah_political$party == "AL", "American Labor",
                        ifelse(usah_political$party == "congressman", "D",
                        ifelse(usah_political$party == "IW", "Independent Whig",
                        ifelse(usah_political$party == "", NA,
                               usah_political$party)))))))))

# united states senate
usas_political$party <- str_replace(usas_political$party, "\\[1\\]|, then.+|, changed.+|\\.$|2\\. Gilman.+|William E.+", "")
usas_political$party <- ifelse(usas_political$party == "Unknown"| usas_political$party == "", NA, usas_political$party)


# format traffic data -------------------------------------------------------------------

# austrian nationalrat
austria_traffic <- austria_traffic[,-2]
austria_traffic$date <- austria_traffic$date %>% as.POSIXct(tz = "UTC")
austria_traffic$pageid <- as.integer(austria_traffic$pageid)

# french assemble
france_traffic <- france_traffic[,-2]
france_traffic$date <- france_traffic$date %>% as.POSIXct(tz = "UTC")
france_traffic$pageid <- as.integer(france_traffic$pageid)

# german bundestag
germany_traffic <- germany_traffic[,-2]
germany_traffic$date <- germany_traffic$date %>% as.POSIXct(tz = "UTC")
germany_traffic$pageid <- as.integer(germany_traffic$pageid)

# irish dail
ireland_traffic <- ireland_traffic[,-2]
ireland_traffic$date <- ireland_traffic$date %>% as.POSIXct(tz = "UTC")
ireland_traffic$pageid <- as.integer(ireland_traffic$pageid)

# united states house
usah_traffic <- usah_traffic[,-2]
usah_traffic$date <- usah_traffic$date %>% as.POSIXct(tz = "UTC")
usah_traffic$pageid <- as.integer(usah_traffic$pageid)

# united states senate
usas_traffic <- usas_traffic[,-2]
usas_traffic$date <- usas_traffic$date %>% as.POSIXct(tz = "UTC")
usas_traffic$pageid <- as.integer(usas_traffic$pageid)

# format history data -------------------------------------------------------------------

# austrian nationalrat
austria_history <- austria_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
austria_history$timestamp <- austria_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# french assemble
france_history <- france_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
france_history$timestamp <- france_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# german bundestag
germany_history <- germany_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
germany_history$timestamp <- germany_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# irish dail
ireland_history <- ireland_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
ireland_history$timestamp <- ireland_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# united states house
usah_history <- usah_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
usah_history$timestamp <- usah_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# united states senate
usas_history <- usas_history %>% select(pageid, revid, parentid, user, userid,
                                              timestamp, size, comment)
usas_history$timestamp <- usas_history$timestamp %>% str_replace("T", " ") %>%
  as.POSIXct(tz = "UTC")

# format social data --------------------------------------------------------------------

# format facial data --------------------------------------------------------------------

# austrian nationalrat
austria_faces <- austria_faces[,-1]
names(austria_faces)[8] <- "emo_happiness"
austria_faces <- austria_faces %>% select(pageid, image_url, smile_intensity, emo_sadness,
                                          emo_neutral, emo_disgust, emo_anger, emo_surprise,
                                          emo_fear, emo_happiness, beauty_female, beauty_male,
                                          skin_dark_circles, skin_stain, skin_acne, skin_health,
                                          image_quality)

# french assemble
france_faces <- france_faces[,-2]
names(france_faces)[9] <- "emo_happiness"
france_faces <- france_faces %>% select(pageid, image_url, smile_intensity, emo_sadness,
                                        emo_neutral, emo_disgust, emo_anger, emo_surprise,
                                        emo_fear, emo_happiness, beauty_female, beauty_male,
                                        skin_dark_circles, skin_stain, skin_acne, skin_health,
                                        image_quality)

# german bundestag
germany_faces <- germany_faces[,-1]
names(germany_faces)[8] <- "emo_happiness"
germany_faces <- germany_faces %>% select(pageid, image_url, smile_intensity, emo_sadness,
                                          emo_neutral, emo_disgust, emo_anger, emo_surprise,
                                          emo_fear, emo_happiness, beauty_female, beauty_male,
                                          skin_dark_circles, skin_stain, skin_acne, skin_health,
                                          image_quality)

# irish dail
ireland_faces <- ireland_faces[,-1]
names(ireland_faces)[8] <- "emo_happiness"
ireland_faces <- ireland_faces %>% select(pageid, image_url, smile_intensity, emo_sadness,
                                          emo_neutral, emo_disgust, emo_anger, emo_surprise,
                                          emo_fear, emo_happiness, beauty_female, beauty_male,
                                          skin_dark_circles, skin_stain, skin_acne, skin_health,
                                          image_quality)

# united states house
usah_faces <- usah_faces[,-1]
names(usah_faces)[8] <- "emo_happiness"
usah_faces <- usah_faces %>% select(pageid, image_url, smile_intensity, emo_sadness,
                                    emo_neutral, emo_disgust, emo_anger, emo_surprise,
                                    emo_fear, emo_happiness, beauty_female, beauty_male,
                                    skin_dark_circles, skin_stain, skin_acne, skin_health,
                                    image_quality)

# united states senate
usas_faces <- usas_faces[,-1]
names(usas_faces)[8] <- "emo_happiness"
usas_faces <- usas_faces %>% select(pageid, image_url, smile_intensity, emo_sadness,
                                    emo_neutral, emo_disgust, emo_anger, emo_surprise,
                                    emo_fear, emo_happiness, beauty_female, beauty_male,
                                    skin_dark_circles, skin_stain, skin_acne, skin_health,
                                    image_quality)

# save data -----------------------------------------------------------------------------

# austrian nationalrat
saveRDS(austria_core, "./package/legislatoR/data-raw/austria_core")
saveRDS(austria_political, "./package/legislatoR/data-raw/austria_political")
saveRDS(austria_history, "./package/legislatoR/data-raw/austria_history")
saveRDS(austria_traffic, "./package/legislatoR/data-raw/austria_traffic")
saveRDS(austria_social, "./package/legislatoR/data-raw/austria_social")
saveRDS(austria_faces, "./package/legislatoR/data-raw/austria_facial")
saveRDS(austria_positions, "./package/legislatoR/data-raw/austria_office")
saveRDS(austria_occupation, "./package/legislatoR/data-raw/austria_occupation")
saveRDS(austria_id, "./package/legislatoR/data-raw/austria_ids")

# french assemble
saveRDS(france_core, "./package/legislatoR/data-raw/france_core")
saveRDS(france_political, "./package/legislatoR/data-raw/france_political")
saveRDS(france_history, "./package/legislatoR/data-raw/france_history")
saveRDS(france_traffic, "./package/legislatoR/data-raw/france_traffic")
saveRDS(france_social, "./package/legislatoR/data-raw/france_social")
saveRDS(france_faces, "./package/legislatoR/data-raw/france_facial")
saveRDS(france_positions, "./package/legislatoR/data-raw/france_office")
saveRDS(france_occupation, "./package/legislatoR/data-raw/france_occupation")
saveRDS(france_id, "./package/legislatoR/data-raw/france_ids")

# german bundestag
saveRDS(germany_core, "./package/legislatoR/data-raw/germany_core")
saveRDS(germany_political, "./package/legislatoR/data-raw/germany_political")
saveRDS(germany_history, "./package/legislatoR/data-raw/germany_history")
saveRDS(germany_traffic, "./package/legislatoR/data-raw/germany_traffic")
saveRDS(germany_social, "./package/legislatoR/data-raw/germany_social")
saveRDS(germany_faces, "./package/legislatoR/data-raw/germany_facial")
saveRDS(germany_positions, "./package/legislatoR/data-raw/germany_office")
saveRDS(germany_occupation, "./package/legislatoR/data-raw/germany_occupation")
saveRDS(germany_id, "./package/legislatoR/data-raw/germany_ids")

# irish dail
saveRDS(ireland_core, "./package/legislatoR/data-raw/ireland_core")
saveRDS(ireland_political, "./package/legislatoR/data-raw/ireland_political")
saveRDS(ireland_history, "./package/legislatoR/data-raw/ireland_history")
saveRDS(ireland_traffic, "./package/legislatoR/data-raw/ireland_traffic")
saveRDS(ireland_social, "./package/legislatoR/data-raw/ireland_social")
saveRDS(ireland_faces, "./package/legislatoR/data-raw/ireland_facial")
saveRDS(ireland_positions, "./package/legislatoR/data-raw/ireland_office")
saveRDS(ireland_occupation, "./package/legislatoR/data-raw/ireland_occupation")
saveRDS(ireland_id, "./package/legislatoR/data-raw/ireland_ids")

# united states house
saveRDS(usah_core, "./package/legislatoR/data-raw/usah_core")
saveRDS(usah_political, "./package/legislatoR/data-raw/usah_political")
saveRDS(usah_history, "./package/legislatoR/data-raw/usah_history")
saveRDS(usah_traffic, "./package/legislatoR/data-raw/usah_traffic")
saveRDS(usah_social, "./package/legislatoR/data-raw/usah_social")
saveRDS(usah_faces, "./package/legislatoR/data-raw/usah_facial")
saveRDS(usah_positions, "./package/legislatoR/data-raw/usah_office")
saveRDS(usah_occupation, "./package/legislatoR/data-raw/usah_occupation")
saveRDS(usah_id, "./package/legislatoR/data-raw/usah_ids")

# united states senate
saveRDS(usas_core, "./package/legislatoR/data-raw/usas_core")
saveRDS(usas_political, "./package/legislatoR/data-raw/usas_political")
saveRDS(usas_history, "./package/legislatoR/data-raw/usas_history")
saveRDS(usas_traffic, "./package/legislatoR/data-raw/usas_traffic")
saveRDS(usas_social, "./package/legislatoR/data-raw/usas_social")
saveRDS(usas_faces, "./package/legislatoR/data-raw/usas_facial")
saveRDS(usas_positions, "./package/legislatoR/data-raw/usas_office")
saveRDS(usas_occupation, "./package/legislatoR/data-raw/usas_occupation")
saveRDS(usas_id, "./package/legislatoR/data-raw/usas_ids")

