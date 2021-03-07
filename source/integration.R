# ---------------------------------------------------------------------------------------
# legislatoR
# Sascha Göbel and Simon Munzert
# Script: integration
# Part of the code in this script written by Johana Sperlova
# April 2019
# ---------------------------------------------------------------------------------------


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list = ls(all = TRUE))

# set working directory -----------------------------------------------------------------
setwd("D:/Sascha/Projects/legislatoR")

# install and load packages and functions -----------------------------------------------
source("./code/packages.R")
source("./code/functions.R")


#### INTEGRATE SIEBERER ET AL BUNDESTAG ROLL CALL VOTE DATA WITH LEGISLATOR =============
# full data and codebook available at:
# https://dataverse.harvard.edu/dataverse/btvote
# the mp_characteristics data set to which legislatoR was integrated is here
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/QSFXLQ

# import, join, and process legislatoR data with necessary columns ----------------------
ger <- readRDS("./data/germany")[,c("pageid","wikidataid","name","term")]
ger2 <- readRDS("./data/germany_birth")
ger <- left_join(ger, ger2, by = "wikidataid")
ger <- select(ger, pageid, name, date, term)
ger$date <- as.character(ger$date)
ger$name <- tolower(ger$name)
rm(ger2)

# import, join, process btvote data with necessary columns ------------------------------
bt_vote <- read_dta("./data/pol_sci_data/mp_characteristics.dta")
bt_vote$name <- str_c(bt_vote$firstname, " ", bt_vote$lastname)
bt_vote <- select(bt_vote, mp_id, name, lastname, elecper, date_birth)
bt_vote$date_birth <- as.character(bt_vote$date_birth)
bt_vote$name <- tolower(bt_vote$name)
bt_vote$lastname <- tolower(bt_vote$lastname)

# build unique match string (full name, term, birthdate) --------------------------------
ger$match_string <- str_c(ger$name, "_", ger$term, "_", ger$date)
bt_vote$match_string <- str_c(bt_vote$name, "_", bt_vote$elecper, "_",
                              bt_vote$date_birth)

# initialise empty columns --------------------------------------------------------------
bt_vote$match_name <- NA
bt_vote$match_birth <- NA
bt_vote$pageid <- NA

# match bt_vote and legislatoR data based on unique match string ------------------------
bt_vote$match_name[match(ger$match_string, bt_vote$match_string, nomatch = 0)] <- ger$name[ger$match_string %in% bt_vote$match_string]
bt_vote$match_birth[match(ger$match_string, bt_vote$match_string, nomatch = 0)] <- ger$date[ger$match_string %in% bt_vote$match_string]
bt_vote$pageid[match(ger$match_string, bt_vote$match_string, nomatch = 0)] <- ger$pageid[ger$match_string %in% bt_vote$match_string]
bt_vote_matched <- distinct(bt_vote, mp_id, .keep_all = TRUE)
bt_vote_matched$pageid[which(bt_vote_matched$date_birth != bt_vote_matched$match_birth)] <- NA
matched1 <- bt_vote_matched[which(!is.na(bt_vote_matched$pageid)),]

# match unmatched bt_vote and legislatoR data based on non-duplicated birth dates -------
matched_ids <- bt_vote_matched$pageid[which(!is.na(bt_vote_matched$pageid))]
ger3 <- ger[-which(ger$pageid %in% matched_ids),]
ger3 <- distinct(ger3, pageid, .keep_all = TRUE)
ger3 <- ger3[!(duplicated(ger3$date) | duplicated(ger3$date, fromLast = TRUE)), ]
bt_vote_unmatched1 <- bt_vote_matched[which(is.na(bt_vote_matched$pageid)),]
bt_vote_unmatched1 <- bt_vote_unmatched1[!(duplicated(bt_vote_unmatched1$date_birth) | duplicated(bt_vote_unmatched1$date_birth, fromLast = TRUE)), ]
bt_vote_unmatched1$match_birth[match(ger3$date, bt_vote_unmatched1$date_birth, nomatch = 0)] <- ger3$date[ger3$date %in% bt_vote_unmatched1$date_birth]
bt_vote_unmatched1$match_name[match(ger3$date, bt_vote_unmatched1$date_birth, nomatch = 0)] <- ger3$name[ger3$date %in% bt_vote_unmatched1$date_birth]
bt_vote_unmatched1$pageid[match(ger3$date, bt_vote_unmatched1$date_birth, nomatch = 0)] <- ger3$pageid[ger3$date %in% bt_vote_unmatched1$date_birth]
matched2 <- bt_vote_unmatched1[which(!is.na(bt_vote_unmatched1$pageid)),]
matched <- rbind(matched1, matched2)
rm(matched_ids,bt_vote_matched,matched1,matched2)

# manually match remaining unmatched bt_vote and legislatoR data ------------------------
unmatched3 <- bt_vote[which(!(bt_vote$mp_id %in% matched$mp_id)),]
unmatched3 <- distinct(unmatched3, mp_id, .keep_all = TRUE)
matched_ids2 <- bt_vote_unmatched1$pageid[which(!is.na(bt_vote_unmatched1$pageid))]
ger4 <- ger3[-which(ger3$pageid %in% matched_ids2),]
matched3 <- data.frame(mp_id = c(37,59,489,1143,1900,1902,2070,2521,2948,3114,3300,4752,4778,5172,
                                 5183,5821,5916,6331,7917,7940,8594,9158,9476,9623,9811,9817,10068,
                                 10090,71309,80435,90561,90985,100481,110342,110887,111693,120426,
                                 120532,120732,121208,122027,122556,132083,136472,138434,141888,
                                 141953,142773,151852,152506,152558,153372,161568,161663,162207,
                                 162949,162950,163021,163431,171853,172083,172840,180292,180870,
                                 181758,182623,182773,111731,121863,120753,111384,143826,1814,81291,
                                 152050,121478,151513,131585,121701,131337,143475,152132,121293,
                                 130924,120739,182771,2571,141990,181997,142045,7138,90156,6718,
                                 5355,90095,71794,143508,70305,132189,90970,90923,110847,182471,
                                 181217,121173,8505,173107,2528,151147,5299,2898,123098,111471,
                                 131760,181753,1173,2576,3898,111870,142331,71196,122214,488,
                                 180744,120781,152012,120958,121211,172038,101096,6676,130909,
                                 153065,182683,10483,122302,162511,161654,144343,141915,90199,
                                 111358,122401,131178,121794,181426,2650,182387,90991),
                       pageid = c(171955,171031,1735815,73442,59646,275654,461545,1107969,384715,
                                  306741,249207,662444,236401,73028,671031,72707,249182,2458535,
                                  268250,3529205,175594,68496,396004,249589,44167,3139967,492786,
                                  929399,2453129,984854,385674,309188,6102529,4237239,1198825,
                                  314710,4371301,67204,6105368,655561,1640511,528146,4330989,
                                  175482,6676676,587829,454894,381427,667865,984976,517256,
                                  984828,984862,182032,955303,6211362,4449876,977008,516094,
                                  4763942,3849486,4754626,1640511,528146,67204,6964599,655561,
                                  174000,6585374,173925,92453,4764351,73446,6099304,985060,
                                  145,412478,173786,6098250,293347,908175,2882304,6585379,448542,
                                  466451,3883230,256024,544256,4411342,397042,366157,236382,
                                  2568922,2756753,1999142,174340,422346,1116341,173801,2907199,
                                  5105098,1549269,1045238,805760,72217,3111614,4541620,271490,
                                  937009,68600,281241,1073329,174118,984868,1156743,241226,
                                  278972,1636023,67208,663326,261246,286279,174379,6574489,
                                  6574489,1027175,10541132,3793999,985385,237748,109100,
                                  175484,667926,44691,44691,219996,984963,4750832,4802838,
                                  292724,413127,952741,174349,964075,2171574,175490,1113748,
                                  6069711,174368))
matched3 <- left_join(matched3, unmatched3, by = "mp_id")
matched3$pageid <- matched3$pageid.x
matched3 <- select(matched3,-c(pageid.x,pageid.y))
matched <- rbind(matched,matched3)
rm(bt_vote_unmatched1,ger3,ger4,matched3,unmatched3,matched_ids2)

# check multiple matches on same pageid -------------------------------------------------
# mostly this is because in bt_vote one person holds more than one mp_id across terms
# if not, they are matched to the correct pageid
#View(matched[(duplicated(matched$pageid) |
#                duplicated(matched$pageid, fromLast = TRUE)), ])
matched$pageid[which(matched$mp_id == 131746)] <- 4764692
bt_vote_id <- select(matched, pageid, mp_id) # join to deu_ids
rm(bt_vote,ger,matched)

# import legislator IDs and Core datasets for germany -----------------------------------
deu_core <- readRDS("./package/legislatoR-data-v0.1.0/data/deu_core")[,c(2,3)]
deu_ids <- readRDS("./package/legislatoR-data-v0.1.0/data/deu_ids")

# join matches to wikidata ids ----------------------------------------------------------
bt_vote_id <- left_join(x = bt_vote_id, y =  deu_core, by = "pageid")
rm(deu_core)
saveRDS(bt_vote_id, "./data/pol_sci_data/bt_vote_id")

# join matches to IDs dataset -----------------------------------------------------------
bt_vote_id <- readRDS("./data/pol_sci_data/bt_vote_id")
deu_ids <- full_join(x = deu_ids, y = bt_vote_id, by = "wikidataid")

# clean up ------------------------------------------------------------------------------
deu_ids <- deu_ids[-which(is.na(deu_ids$wikidataid)),]
deu_ids <- select(deu_ids, -pageid)
colnames(deu_ids)[12] <- "btvote"
saveRDS(deu_ids, "./package/legislatoR-data-v0.2.0/deu_ids")


#### INTEGRATE RAUH ET AL BUNDESTAG PARL SPEECH DATA V1 WITH LEGISLATOR =================
# full data and codebook available at:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/E4RSP9
# note: ParlSpeech covers 12:17, "1991-03-12"-"2013-09-03" sessions only and includes 
# speakers that were not MPs of the national assembly

# import, join, and process legislatoR data with necessary columns ----------------------
deu_core <- readRDS("./package/legislatoR-data/data/deu_core")
deu_political <- filter(readRDS("./package/legislatoR-data/data/deu_political"), 
                        session %in% 12:17)
leg_ger <- semi_join(x = deu_core, y = deu_political, by = "pageid")

# import, join, process btvote data with necessary columns ------------------------------
load("./data/pol_sci_data/Corp_Bundestag.RData")

# match legislator and parlspeech
origin <- leg_ger$name
destin <- unique(bt.corpus$speaker)
#length(unique(str_c(bt.corpus$speaker, bt.corpus$party)))
remainder1 <- origin[which(is.na(match(origin, destin)))]
remainder2 <- destin[which(is.na(match(destin, origin)))]
remainder1_last <- str_replace(remainder1, "-", " ") %>%
  str_replace("^.+ ", "")
remainder2_last <- str_replace(remainder2, "-", " ") %>%
  str_replace("^.+ ", "")
remainder1_2 <- remainder1[-which(duplicated(remainder1_last))]
remainder2_2 <- remainder2[-which(duplicated(remainder2_last))]
replace_1 <- remainder1_2[which(!is.na(match(unique(remainder1_last), unique(remainder2_last))))] # orig
replace_2 <- na.omit(remainder2_2[match(unique(remainder1_last), unique(remainder2_last))]) # dest
# replace in id orig with dest
id <- leg_ger$name
id[match(replace_1[-c(15)], origin)] <- replace_2[-c(15)]
# remove those matched from destin, repeat with first names, remove from destin, 
origin <- leg_ger$name[-which(!is.na(match(leg_ger$name, unique(bt.corpus$speaker))))] # minus these matched from the start
destin <- unique(bt.corpus$speaker)[-which(!is.na(match(unique(bt.corpus$speaker), leg_ger$name)))] # minus these matched from the start
destin <- sort(destin[-match(replace_2[-c(15)], destin)])
origin <- sort(origin[-match(replace_1[-c(15)], origin)])
id[match(origin[c(4,6,7)], leg_ger$name)] <- destin[c(18,26,31)]
# Brigitte Traupe == Brigitte Schulte
# Christian von Stetten == Christian Freiherr von Stetten
# Cornelia von Teichman und Logischen == Cornelia von Teichman
leg_ger$parl_speech <- id
leg_ger <- leg_ger[which(id %in% unique(bt.corpus$speaker)),]
# 50 MPs from legislator in the respective period are left unmatched.
leg_ger <- select(leg_ger, wikidataid, parl_speech)
saveRDS(leg_ger, "./data/pol_sci_data/leg_ger")

# import legislator IDs dataset for germany ---------------------------------------------
deu_ids <- readRDS("./package/legislatoR-data-v0.2.0/deu_ids")

# join matches to IDs dataset -----------------------------------------------------------
leg_ger <- readRDS("./data/pol_sci_data/leg_ger")
deu_ids <- full_join(x = deu_ids, y = leg_ger, by = "wikidataid")

# clean up ------------------------------------------------------------------------------
colnames(deu_ids)[13] <- "parlspeech"
saveRDS(deu_ids, "./package/legislatoR-data-v0.2.0/deu_ids")

#### INTEGRATE ANDREW EGGERS AND ARTHUR SPIRLING DATABASE WITH LEGISLATOR ===============
# full data and codebook available at:
# http://andy.egge.rs/eggers_spirling_database.html
# this part of the code was written by Johana Sperlova

# load core dataset (core) --------------------------------------------------------------
gbr_core <- readRDS('./data/pol_sci_data/gbr_core.rds')  %>%
  group_by(pageid,wikidataid,name,birth) %>%
  count(name = "record_count_in_group") %>%
  ungroup() %>%
  as.data.frame()
gbr_core$birth <- as.Date(gbr_core$birth)
gbr_core['birthyear'] <-format(as.Date(gbr_core$birth, format="%d/%m/%Y"),"%Y")
gbr_core$birthyear<-as.numeric(as.character(gbr_core$birthyear))

# load external dataset -----------------------------------------------------------------
gbr_mp_characteristics <- read.csv('mps.csv') %>%
  group_by(member.id,mp.name,mp.sname,mp.fname) %>%
  ungroup() %>%
  as.data.frame()
gbr_mp_characteristics$mp.dob <- as.Date(gbr_mp_characteristics$mp.dob)
gbr_mp_characteristics['birthyear'] <-format(as.Date(gbr_mp_characteristics$mp.dob, format="%d/%m/%Y"),"%Y")
gbr_mp_characteristics$birthyear<-as.numeric(as.character(gbr_mp_characteristics$birthyear))

## extract first name and last name -----------------------------------------------------
#first name through regex
gbr_core['firstname'] <- trimws(unlist(str_extract(gbr_core$name,
                                                   '^([\\w\\-]+)')))
gbr_core['firstname'] <- tolower(gbr_core$firstname)
#last name through regex
gbr_core['lastname'] <- trimws(unlist(str_extract(gbr_core$name,
                                                  '\\s(\\w+)$')))
gbr_core['lastname'] <- tolower(gbr_core$lastname)
gbr_mp_characteristics['lastname'] <- tolower(gbr_mp_characteristics$mp.sname)
gbr_mp_characteristics['firstname'] <- tolower(gbr_mp_characteristics$mp.fname)

# first merge; merging the core dataset with the exterenal database ---------------------
merge1 <- gbr_core %>%
  left_join(gbr_mp_characteristics,by= c("firstname" = "firstname",
                                         "lastname"="lastname",
                                         "birthyear"="birthyear")) %>%
  select(pageid, wikidataid, member.id, firstname, lastname, birthyear)
# filter for when there are no NAs (to later join)
finaljoin1 <- merge1 %>%
  filter(merge1$member.id != "NA")
# create a dataset with all the unmatched observations
nas1 <- merge1 %>%
  filter( is.na(merge1$member.id))
names(nas1)[names(nas1) == 'member.id'] <- 'nacolumn'

# second merge; merging the NAs1 with the external database ----------------------------
merge2 <- nas1 %>%
  left_join(gbr_mp_characteristics,by= c("lastname"="lastname",
                                         "birthyear"="birthyear")) %>%
  select(pageid, wikidataid, member.id,firstname.x,lastname, birthyear)
# filter for when there are no NAs (to later join)
finaljoin2 <- merge2 %>%
  filter(merge2$member.id != "NA")
names(finaljoin2)[4]<-paste("firstname")
# create a dataset with all the unmatched observations
nas2 <-merge2 %>%
  filter( is.na(merge2$member.id))
names(nas2)[names(nas2) == 'member.id'] <- 'nacolumn'
names(nas2)[4]<-paste("firstname")

# finalize ------------------------------------------------------------------------------
#join the two left_joined files:
totaljoin<-rbind(finaljoin1,finaljoin2)
#extract unmatched NAs to csv (to be sorted manually):
write.csv(nas2, "gbr_nas2.csv")
write.csv(gbr_mp_characteristics, "gbr_external_data.csv")
write.csv(totaljoin,"totaljoin_gbr.csv")
#the rest of the observations were edited manually in excel (around 900)

# import legislator IDs dataset for gbr -------------------------------------------------
gbr_ids <- readRDS("./package/legislatoR-data-v0.2.0/gbr_ids")

# join matches to IDs dataset -----------------------------------------------------------
gbr_eggers <- read.csv("./data/pol_sci_data/gbr_wikidataid_eggers_key.csv", 
                       stringsAsFactors = FALSE)
gbr_eggers <- select(gbr_eggers, wikidataid, member.id)
gbr_eggers <- gbr_eggers[!is.na(gbr_eggers$member.id),]
gbr_ids <- full_join(x = gbr_ids, y = gbr_eggers, by = "wikidataid")

# clean up ------------------------------------------------------------------------------
colnames(gbr_ids)[18] <- "eggersspirling"
saveRDS(gbr_ids, "./package/legislatoR-data-v0.2.0/gbr_ids")


## Czech ParlSpeech V1 ==================================================================
cze_output <- readRDS("./data/pol_sci_data/cze_output")
czech_ids <- readRDS("./package/legislatoR-data-v0.1.0/data/cze_ids")
czech_ids <- full_join(czech_ids, cze_output, by = "wikidataid")
colnames(czech_ids)[11] <- "parlspeech"
#czech_ids <- czech_ids[-which(rowSums(is.na(czech_ids)) == ncol(czech_ids)-1),]
saveRDS(czech_ids, "./package/legislatoR-data-v1.0.0/czech_ids")

## UK ParlSpeech V1 =====================================================================
uk_output <- readRDS("./data/pol_sci_data/uk_output")
uk_ids <- readRDS("./package/legislatoR-data-v1.0.0/gbr_ids")
uk_ids <- full_join(uk_ids, uk_output, by = "wikidataid")
colnames(uk_ids)[19] <- "parlspeech"
uk_ids <- uk_ids[-which(rowSums(is.na(uk_ids)) == ncol(uk_ids)-1),]
saveRDS(uk_ids, "./package/legislatoR-data-v1.0.0/gbr_ids")

## Spain  ParlSpeech V2 =================================================================
# names in parlSpeech are unique identifiers
# and they match perfectly because legislator gathered official data
# from the same source used by parlspeech
spain_corp <- readRDS("./data/pol_sci_data/Corp_Congreso_V2")
spain_corp$date <- ymd(as.character(spain_corp$date))
spain_corp$speaker <- as.character(spain_corp$speaker)
spain_corp$speaker <- spain_corp$speaker %>% str_trim()
spain_corp <- spain_corp %>% tidyr::drop_na(party) # president etc. not mps
spain_corp$first <- str_trim(str_replace(str_extract(spain_corp$speaker, ",.+"), "^,", ""))
spain_corp$last <- str_replace(spain_corp$speaker, ",.+", "")
spain_corp$name <- str_c(spain_corp$first, " ", spain_corp$last)
spain_corp <- spain_corp %>% 
  distinct(speaker, .keep_all = TRUE)
spain_core <- readRDS("./package/legislatoR-data-v1.0.0/esp_core")
spain_core$name[which(duplicated(spain_core$name) | duplicated(spain_core$name, fromLast=TRUE))] <- NA
spain_core <- spain_core %>% tidyr::drop_na(name)
spain_ids <- left_join(spain_corp[,c("party","name", "speaker")], spain_core[,c("name", "wikidataid")], by = "name")
spain_core <- readRDS("./package/legislatoR-data-v1.0.0/esp_core")
# In the following NAs are persons to which an ID was already assigned, these are duplicates in parlspeech
# with the name written slightly differently, or persons which are not in legislatoR, could be non-mp speakers
spain_ids[is.na(spain_ids$wikidataid),]$wikidataid <- c(NA,NA,NA, "Q455748",NA, "Q701362",NA, "Q3109576","Q1366326",NA,
                                                        NA, "Q3154831", NA, NA, NA, NA, NA, NA, NA, NA,
                                                        NA, NA, NA, NA, NA, "Q4890993", NA, "Q292095", NA, NA,
                                                        NA, NA, "Q3327534", "Q2749833", NA, NA, NA, "Q3187630", NA, "Q270883",
                                                        NA, NA, NA, NA, NA, "Q1311580", NA, "Q1690734", NA, "Q2986637",
                                                        NA, NA, NA, "Q2893019", NA, NA, NA, NA, NA, NA,
                                                        NA, NA, NA, "Q1336629", NA, NA, NA, NA, "Q16297359", NA,
                                                        NA, NA, "Q271658", NA, NA, NA, NA, NA, NA, "Q9009373",
                                                        NA, NA, NA)
spain_ids <- spain_ids %>% tidyr::drop_na(wikidataid) %>%
  dplyr::select(speaker, wikidataid)
colnames(spain_ids)[1] <- "parlspeech"
spain_ids_full <- readRDS("./package/legislatoR-data-v1.0.0/esp_ids")
spain_ids_full <- full_join(spain_ids_full, spain_ids[,c("wikidataid","parlspeech")], by = "wikidataid")
#spain_ids_full <- spain_ids_full[-which(rowSums(is.na(spain_ids_full)) == ncol(spain_ids_full)-1),]
saveRDS(spain_ids_full, "./package/legislatoR-data-v1.0.0/esp_ids")

# Austria V2 ============================================================================
austria_corp <- readRDS("./data/pol_sci_data/Corp_Nationalrat_V2")
austria_corp <- austria_corp %>% 
  distinct(speaker, .keep_all = TRUE)
austria_corp$speaker_original <- austria_corp$speaker
austria_corp$speaker <- austria_corp$speaker %>% str_remove("Bundesminister(in)?|Präsident(in)?|Bundeskanzler(in)?|Schriftführer(in)?")
austria_corp$speaker <- austria_corp$speaker %>% str_remove(".+Abg\\.|Vizekanzler(in)?")
austria_corp$speaker <- austria_corp$speaker %>% str_remove(".+Forschung |\\. Redezeit|.+Verbraucherschutz |.+Bundesminister")
office_regex <- unique(str_extract(austria_corp$speaker, "für .+"))
office_regex <- gsub(" [^ ]*$", "", office_regex)
office_regex <- gsub(" [^ ]*$", "", office_regex)
office_regex <- na.omit(unique(office_regex))
for (i in 1:length(office_regex)) {
  austria_corp$speaker <- austria_corp$speaker %>% str_remove(office_regex[i])
}
austria_corp$speaker <- str_trim(austria_corp$speaker)
austria_corp$session <- NA
austria_corp$date <- ymd(austria_corp$date)
austria_corp$party <- austria_corp$party %>% str_replace("Jetzt – Liste ", "")

aut <- readRDS("./package/legislatoR-data-v1.0.0/aut_core")
aut_pol <- readRDS("./package/legislatoR-data-v1.0.0/aut_political")
aut_pol <- aut_pol %>% filter(session != 27)
for (i in 1:length(unique(aut_pol$session_start))) {
  in_session <- austria_corp$date %within% lubridate::interval(unique(aut_pol$session_start)[i], unique(aut_pol$session_end)[i])
  austria_corp$session <- ifelse(in_session == TRUE, i, austria_corp$session)
}
aut_pol <- aut_pol %>% filter(session %in% 20:26)
aut <- semi_join(aut, aut_pol, by = "pageid")
aut <- left_join(aut, aut_pol, by = "pageid")
aut$speaker <- aut$wikititle %>% str_replace_all("_|\\(.+", " ") %>% str_trim()
aut$matchid_1 <- str_c(aut$speaker, aut$session)
austria_corp$matchid_1 <- str_c(austria_corp$speaker, austria_corp$session)
ids_1 <- left_join(austria_corp[,c("speaker_original", "session", "party", "matchid_1")], 
                   aut[,c("speaker", "session", "party", "wikidataid", "matchid_1")], by = "matchid_1") %>%
  dplyr::distinct(wikidataid, .keep_all = TRUE) %>%
  tidyr::drop_na(wikidataid)
# inspection did not yield any mismatches
# manually match remaining
aut <- aut %>% filter(!(wikidataid %in% ids_1$wikidataid)) %>%
  distinct(wikidataid, .keep_all = TRUE)
ids_2 <- data.frame(speaker_original = c("Erich L. Schreiner", "Willi Brauneder", " Eva Glawischnig-Piesczek",
                                         "Herbert L. Graf", "Beate Hartinger", "Evelin Lichtenberger",
                                         "Susanne Riess-Passer", "Ulrike Sima", "Karl-Heinz Dernoscheg",
                                         "Dagmar Belakowitsch-Jenewein", "Adelheid Irina Fürntrath-Moretti",
                                         "Johann Georg Schelling", "Johannes Hahn", "Sonja Steßl-Mühlbacher",
                                         "Nikolaus Alm", "Aygül Berivan Aslan", "Angelika Rosa Mlinar",
                                         "Josef Schellhorn", "Ulrike Weigerstorfer", "Ricarda Berger",
                                         "Douglas Hoyos-Trauttmansdorff", "Barbara Krenn", "Maria Theresia Niss",
                                         "Bundesminister für Land- und Forstwirtschaft, Umwelt und Wasserwirtschaft Andrä Rupprechter",
                                         "Birgit Silvia Sandler", "Andrea Michaela Schartel", "Gabriela Schwarz"),
                    wikidataid = c("Q1353204", "Q1237043", "Q93870", "Q28919536",
                                   "Q813144", "Q79073", "Q78904", "Q2477473", "Q1731521",
                                   "Q90426", "Q354661", "Q1580606", "Q78647", "Q2301742",
                                   "Q964070", "Q15792244", "Q524225", "Q17352992", "Q94004",
                                   "Q46013387", "Q19278490", "Q42313276", "Q42304258", "Q15428608",
                                   "Q43231483", "Q19501935", "Q36808597")) 
ids <- rbind(ids_1[,c("speaker_original", "wikidataid")], ids_2)
colnames(ids)[1] <- "parlspeech"
aut_ids <- readRDS("./package/legislatoR-data-v1.0.0/aut_ids")
aut_ids <- full_join(aut_ids, ids, by = "wikidataid")
saveRDS(aut_ids, "./package/legislatoR-data-v1.0.0/aut_ids")

  
#### INTEGRATE SILVA/PROKSCH TWITTER DATA WITH LEGISLATOR ===============================

# import twitter data -------------------------------------------------------------------
sp_twitter <- read.csv("./data/twitter handles/politicians_working_JAN2019_2.csv", 
                       na.strings = "",
                       stringsAsFactors = FALSE)

# filter relevant countries and columns -------------------------------------------------
sp_twitter_austria <- filter(sp_twitter, country == "austria")
sp_twitter_austria <- select(sp_twitter_austria, wikidata, twitter, facebook)
sp_twitter_austria <- sp_twitter_austria[!is.na(sp_twitter_austria$twitter),]
colnames(sp_twitter_austria) <- c("wikidataid", "twitter_new", "facebook_new")
sp_twitter_czech <- filter(sp_twitter, country == "czechia")
sp_twitter_czech <- select(sp_twitter_czech, wikidata, twitter, facebook)
sp_twitter_czech <- sp_twitter_czech[!is.na(sp_twitter_czech$twitter),]
colnames(sp_twitter_czech) <- c("wikidataid", "twitter_new", "facebook_new")
sp_twitter_france <- filter(sp_twitter, country == "france")
sp_twitter_france <- select(sp_twitter_france, wikidata, twitter, facebook)
sp_twitter_france <- sp_twitter_france[!(is.na(sp_twitter_france$twitter) & 
                                                is.na(sp_twitter_france$facebook)),]
colnames(sp_twitter_france) <- c("wikidataid", "twitter_new", "facebook_new")
sp_twitter_germany <- filter(sp_twitter, country == "germany")
sp_twitter_germany <- select(sp_twitter_germany, wikidata, twitter, facebook)
sp_twitter_germany <- sp_twitter_germany[!(is.na(sp_twitter_germany$twitter) & 
                                           is.na(sp_twitter_germany$facebook)),]
colnames(sp_twitter_germany) <- c("wikidataid", "twitter_new", "facebook_new")
sp_twitter_ireland <- filter(sp_twitter, country == "ireland")
sp_twitter_ireland <- select(sp_twitter_ireland, wikidata, twitter, facebook)
sp_twitter_ireland <- sp_twitter_ireland[!is.na(sp_twitter_ireland$twitter),]
colnames(sp_twitter_ireland) <- c("wikidataid", "twitter_new", "facebook_new")
sp_twitter_gbr <- filter(sp_twitter, country == "uk")
sp_twitter_gbr <- select(sp_twitter_gbr, wikidata, twitter, facebook)
sp_twitter_gbr <- sp_twitter_gbr[!(is.na(sp_twitter_gbr$twitter) & 
                                             is.na(sp_twitter_gbr$facebook)),]
colnames(sp_twitter_gbr) <- c("wikidataid", "twitter_new", "facebook_new")

# import legislator social datasets -----------------------------------------------------
aut_social <- readRDS("./package/legislatoR-data-v0.1.0/data/aut_social")
cze_social <- readRDS("./package/legislatoR-data-v0.1.0/data/cze_social")
fra_social <- readRDS("./package/legislatoR-data-v0.1.0/data/fra_social")
deu_social <- readRDS("./package/legislatoR-data-v0.1.0/data/deu_social")
irl_social <- readRDS("./package/legislatoR-data-v0.1.0/data/irl_social")
gbr_social <- readRDS("./package/legislatoR-data-v0.1.0/data//gbr_social")

# join twitter and legislatoR social datasets -------------------------------------------
aut_social <- full_join(x = aut_social, y = sp_twitter_austria, by = "wikidataid")
aut_social <- aut_social[!is.na(aut_social$wikidataid),]
aut_social$twitter_full <- ifelse(is.na(aut_social$twitter), aut_social$twitter_new, 
                                  aut_social$twitter)
length(which(!is.na(aut_social$twitter_full)))-length(which(!is.na(aut_social$twitter)))
# 61 twitter handles added
aut_social <- select(aut_social, wikidataid, twitter = twitter_full, facebook:googlep)
cze_social <- full_join(x = cze_social, y = sp_twitter_czech, by = "wikidataid")
cze_social <- cze_social[!is.na(cze_social$wikidataid),]
cze_social$twitter_full <- ifelse(is.na(cze_social$twitter), cze_social$twitter_new, 
                                  cze_social$twitter)
length(which(!is.na(cze_social$twitter_full)))-length(which(!is.na(cze_social$twitter)))
# 58 twitter handles added
cze_social <- select(cze_social, wikidataid, twitter = twitter_full, facebook:linkedin)
fra_social <- full_join(x = fra_social, y = sp_twitter_france, by = "wikidataid")
fra_social <- fra_social[!is.na(fra_social$wikidataid),]
fra_social$twitter_full <- ifelse(is.na(fra_social$twitter), fra_social$twitter_new, 
                                  fra_social$twitter)
fra_social$facebook_full <- ifelse(is.na(fra_social$facebook), fra_social$facebook_new, 
                                  fra_social$facebook)
length(which(!is.na(fra_social$twitter_full)))-length(which(!is.na(fra_social$twitter)))
# 26 twitter handles added
length(which(!is.na(fra_social$facebook_full)))-length(which(!is.na(fra_social$facebook)))
# 0 facebool handles added
fra_social <- select(fra_social, wikidataid, twitter = twitter_full, facebook:website)
deu_social <- full_join(x = deu_social, y = sp_twitter_germany, by = "wikidataid")
deu_social <- deu_social[!is.na(deu_social$wikidataid),]
deu_social$twitter_full <- ifelse(is.na(deu_social$twitter), deu_social$twitter_new, 
                                  deu_social$twitter)
deu_social$facebook_full <- ifelse(is.na(deu_social$facebook), deu_social$facebook_new, 
                                   deu_social$facebook)
length(which(!is.na(deu_social$twitter_full)))-length(which(!is.na(deu_social$twitter)))
# 232 twitter handles added
length(which(!is.na(deu_social$facebook_full)))-length(which(!is.na(deu_social$facebook)))
# 6 facebook handles added
deu_social <- select(deu_social, wikidataid, twitter = twitter_full, 
                     facebook = facebook_full, youtube:website)
irl_social <- full_join(x = irl_social, y = sp_twitter_ireland, by = "wikidataid")
irl_social <- irl_social[!is.na(irl_social$wikidataid),]
irl_social$twitter_full <- ifelse(is.na(irl_social$twitter), irl_social$twitter_new, 
                                  irl_social$twitter)
irl_social$facebook_full <- ifelse(is.na(irl_social$facebook), irl_social$facebook_new, 
                                   irl_social$facebook)
length(which(!is.na(irl_social$twitter_full)))-length(which(!is.na(irl_social$twitter)))
# 129 twitter handles added
length(which(!is.na(irl_social$facebook_full)))-length(which(!is.na(irl_social$facebook)))
# 0 facebook handles added
irl_social <- select(irl_social, wikidataid, twitter = twitter_full, facebook:website)
gbr_social <- full_join(x = gbr_social, y = sp_twitter_gbr, by = "wikidataid")
gbr_social <- gbr_social[!is.na(gbr_social$wikidataid),]
gbr_social$twitter_full <- ifelse(is.na(gbr_social$twitter), gbr_social$twitter_new, 
                                  gbr_social$twitter)
gbr_social$facebook_full <- ifelse(is.na(gbr_social$facebook), gbr_social$facebook_new, 
                                   gbr_social$facebook)
length(which(!is.na(gbr_social$twitter_full)))-length(which(!is.na(gbr_social$twitter)))
# 15 twitter handles added
length(which(!is.na(gbr_social$facebook_full)))-length(which(!is.na(gbr_social$facebook)))
# 4 facebook handles added
gbr_social <- select(gbr_social, wikidataid, twitter = twitter_full, 
                     facebook = facebook_full, youtube:googlep)
# overall 521 missing twitter handles added
# overall 10 missing facebook handles added
saveRDS(aut_social, "./package/legislatoR-data-v0.2.0/aut_social")
saveRDS(cze_social, "./package/legislatoR-data-v0.2.0/cze_social")
saveRDS(fra_social, "./package/legislatoR-data-v0.2.0/fra_social")
saveRDS(deu_social, "./package/legislatoR-data-v0.2.0/deu_social")
saveRDS(irl_social, "./package/legislatoR-data-v0.2.0/irl_social")
saveRDS(gbr_social, "./package/legislatoR-data-v0.2.0/gbr_social")



#### INTEGRATE VOTEVIEW DATABASE WITH LEGISLATOR ========================================
# full data and codebook available at:
# https://www.voteview.com/
# this part of the code was written by Johana Sperlova

# house
# import core dataset for house members (core) ------------------------------------------
usa_house_core <- readRDS('./data/pol_sci_data/usa_house_core.rds')  %>%
  group_by(pageid,wikidataid,name,birth) %>%
  count(name = "record_count_in_group") %>%
  ungroup() %>%
  as.data.frame()
usa_house_core$birth <- as.Date(usa_house_core$birth)
usa_house_core['birthyear'] <-format(as.Date(usa_house_core$birth, format="%d/%m/%Y"),"%Y")
usa_house_core$birthyear<-as.numeric(as.character(usa_house_core$birthyear))

# import external dataset (voteview) ----------------------------------------------------
us_house_mp_characteristics <- read.csv('./data/pol_sci_data/us_house_mp_characteristics.csv') %>%
  group_by(bioname,bioguide_id, born, died) %>%
  count(name = "record_count_in_group") %>%
  ungroup() %>%
  as.data.frame()

# extract politicians names (external dataset) ------------------------------------------
# last name
# regex pattern to extract only part of the name that's before the comma (lastname)
us_house_mp_characteristics['lastname'] <- trimws(unlist(str_extract(us_house_mp_characteristics$bioname,
                                                                     '^([^,])+')))
# change last name to lowercase
us_house_mp_characteristics['lastname'] <- tolower(us_house_mp_characteristics$lastname)
# first name
# regex pattern to extract only part of the last name that's before the bracket
us_house_mp_characteristics['firstname'] <- trimws(unlist(str_extract(us_house_mp_characteristics$bioname,
                                                                      '(?<=\\,\\s)(\\w+)')))
us_house_mp_characteristics['firstname'] <- tolower(us_house_mp_characteristics$firstname)

# extract politicians names (core dataset) ----------------------------------------------
# first name through regex
usa_house_core['firstname'] <- trimws(unlist(str_extract(usa_house_core$name,
                                                         '^([\\w\\-]+)')))
usa_house_core['firstname'] <- tolower(usa_house_core$firstname)
# last name through regex
usa_house_core['lastname'] <- trimws(unlist(str_extract(usa_house_core$name,
                                                        '\\s(\\w+)$')))
usa_house_core['lastname'] <- tolower(usa_house_core$lastname)

# merge legislator with voteview Level 1 ------------------------------------------------
#first merge; merging the core dataset with the exterenal database
merge1 <- usa_house_core %>%
  left_join(us_house_mp_characteristics,by= c("firstname" = "firstname",
                                              "lastname"="lastname",
                                              "birthyear"="born")) %>%
  select(pageid, wikidataid, bioguide_id, firstname, lastname, birthyear)
#filter for when there are no NAs (to later join)
finaljoin1 <- merge1 %>%
  filter(merge1$bioguide_id != "NA")
#create a dataset with all the unmatched observations
nas1 <- merge1 %>%
  filter(is.na(merge1$bioguide_id))
names(nas1)[names(nas1) == 'bioguide_id'] <- 'nacolumn'

## merge legislator and voteview Level 2 ------------------------------------------------
# second merge; merging the NAs1 with the external database
merge2 <- nas1 %>%
  left_join(us_house_mp_characteristics,by= c("lastname"="lastname",
                                              "birthyear"="born")) %>%
  select(pageid, wikidataid, bioguide_id,firstname.x,lastname, birthyear)
#filter for when there are no NAs (to later join)
finaljoin2 <- merge2 %>%
  filter(merge2$bioguide_id != "NA")
names(finaljoin2)[4]<-paste("firstname")
#create a dataset with all the unmatched observations
nas2 <-merge2 %>%
  filter( is.na(merge2$bioguide_id))
names(nas2)[names(nas2) == 'bioguide_id'] <- 'nacolumn'
names(nas2)[4]<-paste("firstname")
#join the two left_joined files:
totaljoin<-rbind(finaljoin1,finaljoin2)
#extract unmatched NAs to csv (to be sorted manually):
write.csv(nas2, "us_nas2.csv")
write.csv(us_house_mp_characteristics, "us_external_data.csv")
write.csv(totaljoin,"./data/pol_sci_data/totaljoin.csv")
# the rest of the observations were edited manually in excel (around 900)!

# senate
# import core dataset for senate members (core) -----------------------------------------
usa_senate_core <- readRDS('./data/pol_sci_data/usa_senate_core.rds')  %>%
  group_by(pageid,wikidataid,name,birth) %>%
  count(name = "record_count_in_group") %>%
  ungroup() %>%
  as.data.frame()
usa_senate_core$birth <- as.Date(usa_senate_core$birth)
usa_senate_core['birthyear'] <-format(as.Date(usa_senate_core$birth, format="%d/%m/%Y"),"%Y")
usa_senate_core$birthyear<-as.numeric(as.character(usa_senate_core$birthyear))

# import external dataset (voteview) ----------------------------------------------------
us_senate_mp_characteristics <- read.csv('./data/pol_sci_data/us_senate_mp_characteristics.csv') %>%
  group_by(bioname,bioguide_id, born, died) %>%
  count(name = "record_count_in_group") %>%
  ungroup() %>%
  as.data.frame()

# extract politicians names (external dataset) ------------------------------------------
# first name
# regex pattern to extract only part of the name that's before the comma (lastname)
us_senate_mp_characteristics['lastname'] <- trimws(unlist(str_extract(us_senate_mp_characteristics$bioname,
                                                                      '^([^,])+')))
# change last name to lowercase
us_senate_mp_characteristics['lastname'] <- tolower(us_senate_mp_characteristics$lastname)
# last name
# regex pattern to extract only part of the last name that's before the bracket
us_senate_mp_characteristics['firstname'] <- trimws(unlist(str_extract(us_senate_mp_characteristics$bioname,
                                                                       '(?<=\\,\\s)(\\w+)')))
us_senate_mp_characteristics['firstname'] <- tolower(us_senate_mp_characteristics$firstname)

# extract politicians names (core dataset) ----------------------------------------------
#first name through regex
usa_senate_core['firstname'] <- trimws(unlist(str_extract(usa_senate_core$name,
                                                          '^([\\w\\-]+)')))
usa_senate_core['firstname'] <- tolower(usa_senate_core$firstname)
#last name through regex
usa_senate_core['lastname'] <- trimws(unlist(str_extract(usa_senate_core$name,
                                                         '\\s(\\w+)$')))
usa_senate_core['lastname'] <- tolower(usa_senate_core$lastname)

# merge legislator with voteview Level 1 ------------------------------------------------
#first merge; merging the core dataset with the exterenal database
merge1 <- usa_senate_core %>%
  left_join(us_senate_mp_characteristics,by= c("firstname" = "firstname",
                                               "lastname"="lastname",
                                               "birthyear"="born")) %>%
  select(pageid, wikidataid, bioguide_id, firstname, lastname, birthyear)
#filter for when there are no NAs (to later join)
finaljoin1 <- merge1 %>%
  filter(merge1$bioguide_id != "NA")
#create a dataset with all the unmatched observations
nas1 <- merge1 %>%
  filter( is.na(merge1$bioguide_id))
names(nas1)[names(nas1) == 'bioguide_id'] <- 'nacolumn'

# merge legislator with voteview Level 1 ------------------------------------------------
#second merge; merging the NAs1 with the exterenal database
merge2 <- nas1 %>%
  left_join(us_senate_mp_characteristics,by= c("lastname"="lastname",
                                               "birthyear"="born")) %>%
  select(pageid, wikidataid, bioguide_id,firstname.x,lastname, birthyear)
#filter for when there are no NAs (to later join)
finaljoin2 <- merge2 %>%
  filter(merge2$bioguide_id != "NA")
names(finaljoin2)[4]<-paste("firstname")
#create a dataset with all the unmatched observations
nas2 <-merge2 %>%
  filter( is.na(merge2$bioguide_id))
names(nas2)[names(nas2) == 'bioguide_id'] <- 'nacolumn'
names(nas2)[4]<-paste("firstname")
#join the two left_joined files:
totaljoin<-rbind(finaljoin1,finaljoin2)
#extract unmatched NAs to csv (to be sorted manually):
write.csv(nas2, "./data/pol_sci_data/us_senate_nas2.csv")
write.csv(us_senate_mp_characteristics, "./data/pol_sci_data/us_senate_external_data.csv")
write.csv(totaljoin,"./data/pol_sci_data/totaljoin_senate.csv")
#the rest of the observations were edited manually in excel (around 380)!

# join house an senate ------------------------------------------------------------------
# load usa_nominate dataset
usa_nominate <- read_excel('./data/pol_sci_data/US_Nominate_Voteview_Total_Key.xlsx')  %>%
  as.data.frame()
# load voteview original dataset
usa_nominate_original <- read.csv('./data/pol_sci_data/Voteview_scpsr.csv') %>%
  as.data.frame()

# merge on icpsr & bioguide id ----------------------------------------------------------
#first merge; merging the core dataset with the exterenal database
merge1 <- usa_nominate %>%
  left_join(usa_nominate_original,by= c("bioguide_id" = "bioguide_id")) %>%
  select(pageid, wikidataid, bioguide_id, firstname, lastname, birthyear, icpsr)
#drop duplicate rows
merge2 <- merge1 %>%
  distinct(pageid, .keep_all = TRUE)
#extract document into csv:
write.csv(merge2, "./data/pol_sci_data/usa_congress_wikidataid_icspr_bioguideid_final_key.csv")

# import legislator core and IDs dataset for usa house and senate -----------------------
usa_house_core <- readRDS("./package/legislatoR-data-v0.1.0/data/usa_house_core")
usa_senate_core <- readRDS("./package/legislatoR-data-v0.1.0/data/usa_senate_core")
usa_house_ids <- readRDS("./package/legislatoR-data-v0.1.0/data/usa_house_ids")
usa_senate_ids <- readRDS("./package/legislatoR-data-v0.1.0/data/usa_senate_ids")

# join matches to IDs dataset -----------------------------------------------------------
usa_voteview <- read.csv("./data/pol_sci_data/usa_congress_wikidataid_icspr_bioguideid_final_key.csv", 
                       stringsAsFactors = FALSE)
usa_voteview <- usa_voteview[!is.na(usa_voteview$wikidataid),]
usa_voteview_house <- filter(usa_voteview, wikidataid %in% 
                               unique(usa_house_core$wikidataid))
usa_voteview_senate <- filter(usa_voteview, wikidataid %in% 
                                unique(usa_senate_core$wikidataid))
# select appropriate columns
usa_voteview_house <- select(usa_voteview_house, wikidataid, icpsr, bioguide_id)
usa_voteview_senate <- select(usa_voteview_senate, wikidataid, icpsr, bioguide_id)
# full join
usa_house_ids <- full_join(x = usa_house_ids, y = usa_voteview_house, by = "wikidataid")
usa_senate_ids <- full_join(x = usa_senate_ids, y = usa_voteview_senate, by = "wikidataid")
usa_house_ids$bioguide_id <- ifelse(is.na(usa_house_ids$bioguide_id), usa_house_ids$parlid, 
                                    usa_house_ids$bioguide_id)
usa_senate_ids$bioguide_id <- ifelse(is.na(usa_senate_ids$bioguide_id), usa_senate_ids$parlid, 
                                    usa_senate_ids$bioguide_id)

# clean up ------------------------------------------------------------------------------
usa_house_ids <- select(usa_house_ids, wikidataid, bioguide_id, icpsr, gndid:politfacts)
usa_senate_ids <- select(usa_senate_ids, wikidataid, bioguide_id, icpsr, gndid:politfacts)
usa_house_ids <- usa_house_ids[-which(rowSums(is.na(usa_house_ids)) == ncol(usa_house_ids)-1),]
usa_senate_ids <- usa_senate_ids[-which(rowSums(is.na(usa_senate_ids)) == ncol(usa_senate_ids)-1),]
colnames(usa_house_ids)[2] <- "bioguide"
colnames(usa_senate_ids)[2] <- "bioguide"

saveRDS(usa_house_ids, "./package/legislatoR-data-v0.2.0/usa_house_ids")
saveRDS(usa_senate_ids, "./package/legislatoR-data-v0.2.0/usa_senate_ids")


#### INTEGRATE DATABASE OF PARLIAMENTARY SPEECHES IN IRELAND TH LEGISLATOR ==============
library(vroom)
library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
dpsi <- vroom::vroom(file = "./data/pol_sci_data/Dail_debates_1919-2013/Dail_debates_1919-2013.tab",
                     col_select = c(memberID,date,member_name,party_name))
dpsi <- dpsi %>% distinct(memberID, party_name, .keep_all = TRUE)
irl <- readRDS("./package/legislatoR-data-v1.0.0/irl_core")
irl_pol <- readRDS("./package/legislatoR-data-v1.0.0/irl_political")
#dpsi$member_name <- str_remove(dpsi$member_name, "^.+?\\.|General|Professor") %>%
#  str_trim()
dpsi$last_name <- dpsi$member_name %>%
  stringi::stri_extract_last_words(1) %>%
  tolower()
dpsi$session <- NA
for (i in 1:length(unique(irl_pol$session_start))) {
  in_session <- dpsi$date %within% lubridate::interval(unique(irl_pol$session_start)[i], unique(irl_pol$session_end)[i])
  dpsi$session <- ifelse(in_session == TRUE, i, dpsi$session)
}
irl <- left_join(irl_pol, irl, by = "pageid")
irl$last_name <- irl$name %>%
  stringi::stri_extract_last_words(1) %>%
  tolower()
irl2 <- irl %>%
  group_by(session) %>%
  distinct(last_name, .keep_all = TRUE) %>%
  ungroup() %>%
  group_by_all() %>%
  filter(n() == 1) %>%
  mutate(matchID = str_c(last_name, session))
dpsi2 <- dpsi %>%
  group_by(session) %>%
  distinct(last_name, .keep_all = TRUE) %>%
  ungroup() %>%
  group_by_all() %>%
  filter(n() == 1) %>%
  mutate(matchID = str_c(last_name, session))
irl2 <- left_join(irl2, dpsi2, by = "matchID")
dpsi <- left_join(dpsi, irl2[,c("pageid", "name", "memberID")], by = "memberID")
# correct mismatches
dpsi$pageid[c(13,30,76,79,180,185,
              197,206,223,261,265,
              274,275,292,307,310,
              334,357,366,391,399,
              406,421,425,426,453,
              457,471,476,497,499,
              504,512,513,518,536,
              545,550,560,564,567,
              568,571,573,579,597,
              600,615,616,625,629,
              630,631,633,635,663,
              670,671,682,683,707,
              715,720,721,731,740,
              750,758,773,781,784,
              791,795,803,817,844,
              852,858,865,888,899,
              901,923,933,945,951,
              967,968,969,985,990,
              999,1002,1004,1012,
              1015,1016,1025,1059,
              1074,1075,1085,1091,
              1110,1113,1115,1121,
              1123,1146,1149)] <- as.integer(c(12962224,10204324,268250,559763,15316591,19887055,
                                         18493722,5076916,21003759,21117442,21056219,
                                         15694619, 262963, 20675980,4923955,20162801,
                                         21279436,25165814,11571030,21445057,20723993,
                         8313964,4951236,155297,19886919,18993945,
                         5211624,8839393,3311885,1749922,4980676,
                         16762121,4951611,15525105,563162,18399837,
                         390173,19553185,11562473,5209531,20423340,
                         18401494,2051931,5230730,17419286,1076658,
                         17767979,1080484,11591809,5211949,21056316,
                         4944644,9563739,9509723,5031154,5223847,
                         30872323,254418,15472425,17748676,533405,
                         5220051,148009,1786645,8896919,2363705,
                         15878770,1087220,2780061,482923,9039250,
                         2671345,969786,1508578,5940560,9719032,
                         15085414,1479176,5226017,316088,1095335,
                         30872401,3322765,4069955,4972475,5160886,
                         9631623,1479223,1387412,1326887,3422685,
                         1508486,3422946,30865860,4892571,3191097,
                         3193024,1479223,3199789,11451134,11435896,
                         11437479,4150590,11416881,31026346,22690975,
                         31022071,7268221,1508492,31022086))
# manually match NAs
dpsi[is.na(dpsi$pageid),]$pageid <- as.integer(c(10519936, 15849075, 97509, 12960359, 2218736,
                                                 20480488, 11329755, 20640403, 147390, 13067392,
                                                 15330744, 16654206, 4115669, 20654463, 20723667,
                                                 18709803, 1683145, 11591480, 20251372, 2126221,
                                                 4976440, 21103761, 5238527, 17696717, 18630820,
                                                 21131499,8089293,21321425, 20356044,8166418,
                                                 20580168,1076125,4931235,18301731,478888,
                                                 18504589,18301893,3172374,5230730,15734696,
                                                 5170107,20680931,15614093,983143,5040122,13755540,
                                                 4930848,1085153,2329531,3774806,1087209,
                                                 15755004,2560179,19285376,316094,1474357,15850298,
                                                 30875049,1724217,3313007,15766118,172298,
                                                 556748, 5160977,22450394,3214938,4983885,
                                                 3422568,1474327,3191118,1374439,2199319,
                                                 2695519,11436629,1508611,11417803,
                                                 12529585,23121623,31029261,31022106,31021272,
                                                 1508492,31022071,31023259))
dpsi$pageid[which(duplicated(dpsi$pageid) | duplicated(dpsi$pageid, fromLast=TRUE))] <- NA
dpsi[is.na(dpsi$pageid),]$pageid %>% length
dpsi[match(c(835,229,405,798,137,
             2093,93,1032,267,1033,
             800,896,127,1021,1017,
             138,612,91,611,404,
             128,228,268,203,652,
             895,804,654,799,840,
             144,400,145,1315,408,
             1013,2028,2292,2293,2004,
             2323,410,199),dpsi$memberID),]$pageid <- 
  c(10204324,1796630,20527274,20675980,4155405,
    25166139,4916567,20958411,4976440,20971662,
    18913597,1089066,2371351,21278998,11572276,
    18407583,896958,5010169,18444718,20527274,
    18914011,262966,4976287,20601375,146950,
    272755,5230730,5165717,18570544,10519936,
    15129693,1076644,3378954,5174095,3191282,
    1479223, 984957,22751226,31025105,1508492,
    31022071,1080636,271593)
irl <- left_join(irl, dpsi[,c("pageid", "memberID")], by = "pageid") %>%
  dplyr::distinct(wikidataid, .keep_all = TRUE)
ireland_ids <- readRDS("./package/legislatoR-data-v1.0.0/irl_ids")
ireland_ids <- full_join(ireland_ids, irl[,c("wikidataid","memberID")], by = "wikidataid")
colnames(ireland_ids)[11] <- "dpsi"
ireland_ids <- ireland_ids[-which(rowSums(is.na(ireland_ids)) == ncol(ireland_ids)-1),]
saveRDS(ireland_ids, "./package/legislatoR-data-v1.0.0/irl_ids")


#### INTEGRATE parlScot WITH LEGISLATOR =================================================
parlScot <- vroom("./data/pol_sci_data/parlScotch_CLD.csv")
scotland_ids <- readRDS("./package/legislatoR-data-v2.0.0/sco_ids")
scotland_ids <- full_join(scotland_ids, parlScot[,c("parl_id", "wikidataid")], by = "wikidataid")
colnames(scotland_ids)[9] <- "parlScot"
saveRDS(scotland_ids, "./package/legislatoR-data-v2.0.0/sco_ids")




