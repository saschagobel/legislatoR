# ---------------------------------------------------------------------------------------
# legislatoR
# Sascha Göbel and Simon Munzert
# Script: integration with czech parlspeech
# This script was written by Lada Rudnitckaia
# March 2020
# ---------------------------------------------------------------------------------------


# devtools::install_github("saschagobel/legislatoR")
library(legislatoR)
library(xlsx)
setwd("")

if (.Platform$OS.type == 'windows') {
  Sys.setlocale(category = 'LC_ALL','English_United States.1250')
} else {
  Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
}

cze <- get_core(legislature = "cze")
cze_pol <- get_political(legislature = "cze")
load("Corp_PSP.Rdata") # parl speech data

## Retrieve year from session
cze_pol$start_year <- format(cze_pol$session_start,"%Y")
psp.corpus$start_year <- substr(psp.corpus$session, start=1, stop=4)

## Filter by session 1-7 (as in ParlSpeech)
cze_pol_1_7 <- cze_pol[cze_pol$start_year == 1992 |
                      cze_pol$start_year == 1996 |
                      cze_pol$start_year == 1998 |
                      cze_pol$start_year == 2002 |
                      cze_pol$start_year == 2006 |
                      cze_pol$start_year == 2010 |
                      cze_pol$start_year == 2013,]

### PARTY NAMES
### Since we will use party name for matching, they should be similar in cze_pol and ParlSpeech
## Check party names in cze_pol
fr <- data.frame(table(as.matrix(cze_pol_1_7$party, useNA = "always")))
fr
## Check party names in ParlSpeech
fr_ParlSpeech <- data.frame(table(as.matrix(psp.corpus$party, useNA = "always")))
fr_ParlSpeech

## Change party names in ParlSpeech to respective name from cze_pol
# to change in cze_pol:
# 1      ANO     - ANO
# 2      CSS     - ?
# 3     CSSD     - CSSD
# 4      DSP     - ?
# 5  HSD-SMS     - HSD-SMS
# 6      KDS     - KDS
# 7  KDU-CSL     - KDU-CSL
# 8     KSCM     - KSCM
# 9      LSU     - LSU
# 10     MNS     - ?
# 11     ODA     - ODA
# 12     ODS     - ODS
# 13 SPR-RSC     - SPR-RSC
# 14      SZ     - SZ
# 15  TOP 09     - TOP09
# 16  US-DEU     - US-DEU
# 17   Usvit     - Usvit
# 18      VV     - VV

# to change in ParlSpeech:
# 19                CMSS
# 20                CMUS
# 21               HSDMS
# 22                  LB
# 23                LSNS
# 24             Nez.-SZ
# 25          Nezarazeni
# 26                 ONH
# 27               other
# 28 TOP 09 a Starostove
# 29                  US

cze_pol_1_7$party <- gsub("CSS", "other", cze_pol_1_7$party)
cze_pol_1_7$party <- gsub("DSP", "other", cze_pol_1_7$party)
cze_pol_1_7$party <- gsub("MNS", "other", cze_pol_1_7$party)
cze_pol_1_7$party <- gsub("TOP 09", "TOP09", cze_pol_1_7$party)

psp.corpus$party <- gsub("CMSS", "other", psp.corpus$party)
psp.corpus$party <- gsub("CMUS", "other", psp.corpus$party)
psp.corpus$party <- gsub("HSDMS", "other", psp.corpus$party)
psp.corpus$party <- gsub("LB", "other", psp.corpus$party)
psp.corpus$party <- gsub("LSNS", "other", psp.corpus$party)
psp.corpus$party <- gsub("Nez.-SZ", "other", psp.corpus$party)
psp.corpus$party <- gsub("Nezarazeni", "other", psp.corpus$party)
psp.corpus$party <- gsub("ONH", "other", psp.corpus$party)
psp.corpus$party <- gsub("TOP 09 a Starostove", "other", psp.corpus$party)
psp.corpus$party <- gsub("US_DEU", "1", psp.corpus$party)
psp.corpus$party <- gsub("Usvit", "2", psp.corpus$party)
psp.corpus$party <- gsub("US", "other", psp.corpus$party)
psp.corpus$party <- gsub("1", "US_DEU", psp.corpus$party)
psp.corpus$party <- gsub("2", "Usvit", psp.corpus$party)


## Add wikidataids and names to cze_pol from cze
cze_pol_1_7$wikidataid <- cze$wikidataid[match(cze_pol_1_7$pageid, cze$pageid)]
cze_pol_1_7$name <- cze$name[match(cze_pol_1_7$pageid, cze$pageid)]


### NAMES
## Some names include middle names that are not mentioned in ParlSpeech. Let's remove them 
## and add a new column 'name1'
cze_pol_1_7$name_split = strsplit(cze_pol_1_7$name, ' ')
cze_pol_1_7$firstname = sapply(cze_pol_1_7$name_split, function(x) x[1])
cze_pol_1_7$lastname = sapply(cze_pol_1_7$name_split, function(x) x[length(x)])
cze_pol_1_7$name1 <- paste(cze_pol_1_7$firstname, cze_pol_1_7$lastname, sep = " ", collapse = NULL)

## Concatenate name and party to match both by name and party
cze_pol_1_7$name_party <- paste(cze_pol_1_7$name, cze_pol_1_7$party, sep = " ", collapse = NULL)
cze_pol_1_7$name1_party <- paste(cze_pol_1_7$name1, cze_pol_1_7$party, sep = " ", collapse = NULL)



################################### Session 1 ###################################
### Filter by 1st session
cze_pol_1 <- cze_pol_1_7[cze_pol_1_7$start_year == 1992,]
ParlSpeech_1 <- psp.corpus[psp.corpus$start_year == 1993,]

### NAMES
ParlSpeech_1$name_party <- paste(ParlSpeech_1$speaker, ParlSpeech_1$party, 
                                  sep = " ", collapse = NULL)
### FREQUENCY
## Name
fr <- data.frame(table(as.matrix(cze_pol_1$name, useNA = "always")))
cze_pol_1$uniquename <- fr$Freq[match(cze_pol_1$name, fr$Var1)]
fr_ <- fr[fr$Freq != 1,]
fr_

## Name1
fr <- data.frame(table(as.matrix(cze_pol_1$name1, useNA = "always")))
cze_pol_1$uniquename1 <- fr$Freq[match(cze_pol_1$name1, fr$Var1)]
fr1_ <- fr[fr$Freq != 1,]
fr1_

## Name + party
fr <- data.frame(table(as.matrix(cze_pol_1$name_party, useNA = "always")))
cze_pol_1$uniquename_party <- fr$Freq[match(cze_pol_1$name_party, fr$Var1)]
fr_p <- fr[fr$Freq != 1,]
fr_p

## Name1 + party
fr <- data.frame(table(as.matrix(cze_pol_1$name1_party, useNA = "always")))
cze_pol_1$uniquename1_party <- fr$Freq[match(cze_pol_1$name1_party, fr$Var1)]
fr1_p <- fr[fr$Freq != 1,]
fr1_p

### MATCHING
### Match only if:
### 1. the desired value is still missing
### 2. the pair name+party is unique in uk_pol (we assume that it's always true in ParlSpeech)
### 3. if possible, match manually

## Create empty columns 
cze_pol_1$speaker <- NA

## Match by name
cze_pol_1$speaker <- ifelse(is.na(cze_pol_1$speaker) & cze_pol_1$uniquename == 1,
                            ParlSpeech_1$speaker[match(cze_pol_1$name, ParlSpeech_1$speaker)],
                            cze_pol_1$speaker)
## Match by name1
cze_pol_1$speaker <- ifelse(is.na(cze_pol_1$speaker) & cze_pol_1$uniquename1 == 1,
                            ParlSpeech_1$speaker[match(cze_pol_1$name1, ParlSpeech_1$speaker)],
                            cze_pol_1$speaker)
## Match by name and party
cze_pol_1$speaker <- ifelse(is.na(cze_pol_1$speaker) & cze_pol_1$uniquename_party == 1,
                            ParlSpeech_1$speaker[match(cze_pol_1$name_party, ParlSpeech_1$name_party)],
                            cze_pol_1$speaker)
## Match by name1 and party
cze_pol_1$speaker <- ifelse(is.na(cze_pol_1$speaker) & cze_pol_1$uniquename1_party == 1,
                            ParlSpeech_1$speaker[match(cze_pol_1$name1_party, ParlSpeech_1$name_party)],
                            cze_pol_1$speaker)
# Check the number of still missing data
sum(is.na(cze_pol_1$speaker))


## Match manually (check which existing in ParlSpeech values are not in cze_pol)
# Create csv with unmatched data for manual matching (the code is commented since it is 
# just preparation for manual matching)
# ParlSpeech_1$unname <- cze_pol_1$uniquename_party[match(ParlSpeech_1$name_party,cze_pol_1$name_party)]
# ParlSpeech_1$unname1 <- cze_pol_1$uniquename1_party[match(ParlSpeech_1$name_party,cze_pol_1$name1_party)]
# ParlSpeech_1$match <- NA
# ParlSpeech_1$match <- ifelse(is.na(ParlSpeech_1$match) & ParlSpeech_1$unname == 1,
#                            "matched", ParlSpeech_1$match)
# ParlSpeech_1$match <- ifelse(is.na(ParlSpeech_1$match) & ParlSpeech_1$unname1 == 1,
#                            "matched", ParlSpeech_1$match)
# unmatched_ParlSpeech_1 <- ParlSpeech_1[is.na(ParlSpeech_1$match),]
# unmatched_cze_pol_1 <- cze_pol_1[is.na(cze_pol_1$speaker),]
# unmatched_cze_pol_1 <- unmatched_cze_pol_1[, !(colnames(unmatched_cze_pol_1) %in% c("name_split"))]
# write.xlsx(unmatched_ParlSpeech_1, file = "unmatched_ParlSpeech_1.xlsx", row.names=FALSE)
# write.xlsx(unmatched_cze_pol_1, file = "unmatched_cze_pol_1.xlsx", row.names=FALSE)

# Match manually
# no matches


### OUTPUT
## Add ParlSpeech speaker's id to uk from uk_pol by wikidataid
cze$ParlSpeech_speaker <- cze_pol_1$speaker[match(cze$wikidataid, cze_pol_1$wikidataid)]




################################### Session 2 ###################################
### Filter by 2d session
cze_pol_2 <- cze_pol_1_7[cze_pol_1_7$start_year == 1996,]
ParlSpeech_2 <- psp.corpus[psp.corpus$start_year == 1996,]

### NAMES
ParlSpeech_2$name_party <- paste(ParlSpeech_2$speaker, ParlSpeech_2$party, 
                                 sep = " ", collapse = NULL)
### FREQUENCY
## Name
fr <- data.frame(table(as.matrix(cze_pol_2$name, useNA = "always")))
cze_pol_2$uniquename <- fr$Freq[match(cze_pol_2$name, fr$Var1)]
fr_ <- fr[fr$Freq != 1,]
fr_

## Name1
fr <- data.frame(table(as.matrix(cze_pol_2$name1, useNA = "always")))
cze_pol_2$uniquename1 <- fr$Freq[match(cze_pol_2$name1, fr$Var1)]
fr1_ <- fr[fr$Freq != 1,]
fr1_

## Name + party
fr <- data.frame(table(as.matrix(cze_pol_2$name_party, useNA = "always")))
cze_pol_2$uniquename_party <- fr$Freq[match(cze_pol_2$name_party, fr$Var1)]
fr_p <- fr[fr$Freq != 1,]
fr_p

## Name1 + party
fr <- data.frame(table(as.matrix(cze_pol_2$name1_party, useNA = "always")))
cze_pol_2$uniquename1_party <- fr$Freq[match(cze_pol_2$name1_party, fr$Var1)]
fr1_p <- fr[fr$Freq != 1,]
fr1_p

### MATCHING
## Create empty columns 
cze_pol_2$speaker <- NA

## Match by name
cze_pol_2$speaker <- ifelse(is.na(cze_pol_2$speaker) & cze_pol_2$uniquename == 1,
                            ParlSpeech_2$speaker[match(cze_pol_2$name, ParlSpeech_2$speaker)],
                            cze_pol_2$speaker)
## Match by name1
cze_pol_2$speaker <- ifelse(is.na(cze_pol_2$speaker) & cze_pol_2$uniquename1 == 1,
                            ParlSpeech_2$speaker[match(cze_pol_2$name1, ParlSpeech_2$speaker)],
                            cze_pol_2$speaker)
## Match by name and party
cze_pol_2$speaker <- ifelse(is.na(cze_pol_2$speaker) & cze_pol_2$uniquename_party == 1,
                            ParlSpeech_2$speaker[match(cze_pol_2$name_party, ParlSpeech_2$name_party)],
                            cze_pol_2$speaker)
## Match by name1 and party
cze_pol_2$speaker <- ifelse(is.na(cze_pol_2$speaker) & cze_pol_2$uniquename1_party == 1,
                            ParlSpeech_2$speaker[match(cze_pol_2$name1_party, ParlSpeech_2$name_party)],
                            cze_pol_2$speaker)
# Check the number of still missing data
sum(is.na(cze_pol_2$speaker))


## Match manually (check which existing in ParlSpeech values are not in cze_pol)
# Create csv with unmatched data for manual matching (the code is commented since it is 
# just preparation for manual matching)
# ParlSpeech_2$unname <- cze_pol_2$uniquename_party[match(ParlSpeech_2$name_party,cze_pol_2$name_party)]
# ParlSpeech_2$unname1 <- cze_pol_2$uniquename1_party[match(ParlSpeech_2$name_party,cze_pol_2$name1_party)]
# ParlSpeech_2$match <- NA
# ParlSpeech_2$match <- ifelse(is.na(ParlSpeech_2$match) & ParlSpeech_2$unname == 1,
#                              "matched", ParlSpeech_2$match)
# ParlSpeech_2$match <- ifelse(is.na(ParlSpeech_2$match) & ParlSpeech_2$unname1 == 1,
#                              "matched", ParlSpeech_2$match)
# unmatched_ParlSpeech_2 <- ParlSpeech_2[is.na(ParlSpeech_2$match),]
# cze_pol_2$speaker <- ifelse(is.na(cze_pol_2$speaker),
#                           cze$ParlSpeech_speaker[match(cze_pol_2$wikidataid, cze$wikidataid)],
#                           cze_pol_2$speaker)
# unmatched_cze_pol_2 <- cze_pol_2[is.na(cze_pol_2$speaker),]
# unmatched_cze_pol_2 <- unmatched_cze_pol_2[, !(colnames(unmatched_cze_pol_2) %in% c("name_split"))]
# write.xlsx(unmatched_ParlSpeech_2, file = "unmatched_ParlSpeech_2.xlsx", row.names=FALSE)
# write.xlsx(unmatched_cze_pol_2, file = "unmatched_cze_pol_2.xlsx", row.names=FALSE)

# Match manually
cze_pol_2$speaker[cze_pol_2$wikidataid == "Q10861579"] <- "Zuzka Bebarova Rujbrova"


### OUTPUT
## Add ParlSpeech speaker's id to uk from uk_pol by wikidataid
cze$ParlSpeech_speaker <- ifelse(is.na(cze$ParlSpeech_speaker),
                                 cze_pol_2$speaker[match(cze$wikidataid, cze_pol_2$wikidataid)],
                                 cze$ParlSpeech_speaker)



################################### Session 3 ###################################
### Filter by 3d session
cze_pol_3 <- cze_pol_1_7[cze_pol_1_7$start_year == 1998,]
ParlSpeech_3 <- psp.corpus[psp.corpus$start_year == 1998,]

### NAMES
ParlSpeech_3$name_party <- paste(ParlSpeech_3$speaker, ParlSpeech_3$party, 
                                 sep = " ", collapse = NULL)
### FREQUENCY
## Name
fr <- data.frame(table(as.matrix(cze_pol_3$name, useNA = "always")))
cze_pol_3$uniquename <- fr$Freq[match(cze_pol_3$name, fr$Var1)]
fr_ <- fr[fr$Freq != 1,]
fr_

## Name1
fr <- data.frame(table(as.matrix(cze_pol_3$name1, useNA = "always")))
cze_pol_3$uniquename1 <- fr$Freq[match(cze_pol_3$name1, fr$Var1)]
fr1_ <- fr[fr$Freq != 1,]
fr1_

## Name + party
fr <- data.frame(table(as.matrix(cze_pol_3$name_party, useNA = "always")))
cze_pol_3$uniquename_party <- fr$Freq[match(cze_pol_3$name_party, fr$Var1)]
fr_p <- fr[fr$Freq != 1,]
fr_p

## Name1 + party
fr <- data.frame(table(as.matrix(cze_pol_3$name1_party, useNA = "always")))
cze_pol_3$uniquename1_party <- fr$Freq[match(cze_pol_3$name1_party, fr$Var1)]
fr1_p <- fr[fr$Freq != 1,]
fr1_p

### MATCHING
## Create empty columns 
cze_pol_3$speaker <- NA

## Match by name
cze_pol_3$speaker <- ifelse(is.na(cze_pol_3$speaker) & cze_pol_3$uniquename == 1,
                            ParlSpeech_3$speaker[match(cze_pol_3$name, ParlSpeech_3$speaker)],
                            cze_pol_3$speaker)
## Match by name1
cze_pol_3$speaker <- ifelse(is.na(cze_pol_3$speaker) & cze_pol_3$uniquename1 == 1,
                            ParlSpeech_3$speaker[match(cze_pol_3$name1, ParlSpeech_3$speaker)],
                            cze_pol_3$speaker)
## Match by name and party
cze_pol_3$speaker <- ifelse(is.na(cze_pol_3$speaker) & cze_pol_3$uniquename_party == 1,
                            ParlSpeech_3$speaker[match(cze_pol_3$name_party, ParlSpeech_3$name_party)],
                            cze_pol_3$speaker)
## Match by name1 and party
cze_pol_3$speaker <- ifelse(is.na(cze_pol_3$speaker) & cze_pol_3$uniquename1_party == 1,
                            ParlSpeech_3$speaker[match(cze_pol_3$name1_party, ParlSpeech_3$name_party)],
                            cze_pol_3$speaker)
# Check the number of still missing data
sum(is.na(cze_pol_3$speaker))


## Match manually (check which existing in ParlSpeech values are not in cze_pol)
# Create csv with unmatched data for manual matching (the code is commented since it is 
# just preparation for manual matching)
# ParlSpeech_3$unname <- cze_pol_3$uniquename_party[match(ParlSpeech_3$name_party,cze_pol_3$name_party)]
# ParlSpeech_3$unname1 <- cze_pol_3$uniquename1_party[match(ParlSpeech_3$name_party,cze_pol_3$name1_party)]
# ParlSpeech_3$match <- NA
# ParlSpeech_3$match <- ifelse(is.na(ParlSpeech_3$match) & ParlSpeech_3$unname == 1,
#                              "matched", ParlSpeech_3$match)
# ParlSpeech_3$match <- ifelse(is.na(ParlSpeech_3$match) & ParlSpeech_3$unname1 == 1,
#                              "matched", ParlSpeech_3$match)
# unmatched_ParlSpeech_3 <- ParlSpeech_3[is.na(ParlSpeech_3$match),]
# cze_pol_3$speaker <- ifelse(is.na(cze_pol_3$speaker),
#                             cze$ParlSpeech_speaker[match(cze_pol_3$wikidataid, cze$wikidataid)],
#                             cze_pol_3$speaker)
# unmatched_cze_pol_3 <- cze_pol_3[is.na(cze_pol_3$speaker),]
# unmatched_cze_pol_3 <- unmatched_cze_pol_3[, !(colnames(unmatched_cze_pol_3) %in% c("name_split"))]
# write.csv(unmatched_ParlSpeech_3, file = "unmatched_ParlSpeech_3.csv", row.names=FALSE)
# write.xlsx(unmatched_cze_pol_3, file = "unmatched_cze_pol_3.xlsx", row.names=FALSE)

# Match manually
cze_pol_3$speaker[cze_pol_3$wikidataid == "Q12037465"] <- "Miloslav Kucera ml"
# for older one: Miloslav Kucera or Miloslav Kucera st?
# Check the number of still missing data
sum(is.na(cze_pol_3$speaker))


### OUTPUT
## Add ParlSpeech speaker's id to uk from uk_pol by wikidataid
cze$ParlSpeech_speaker <- ifelse(is.na(cze$ParlSpeech_speaker),
                                 cze_pol_3$speaker[match(cze$wikidataid, cze_pol_3$wikidataid)],
                                 cze$ParlSpeech_speaker)



################################### Session 4 ###################################
### Filter by 4th session
cze_pol_4 <- cze_pol_1_7[cze_pol_1_7$start_year == 2002,]
ParlSpeech_4 <- psp.corpus[psp.corpus$start_year == 2002,]

### NAMES
ParlSpeech_4$name_party <- paste(ParlSpeech_4$speaker, ParlSpeech_4$party, 
                                 sep = " ", collapse = NULL)
cze_pol_4$name2 <- paste(cze_pol_4$lastname, cze_pol_4$firstname, 
                                   sep = " ", collapse = NULL)
cze_pol_4$name2_party <- paste(cze_pol_4$name2, cze_pol_4$party, 
                         sep = " ", collapse = NULL)

### FREQUENCY
## Name
fr <- data.frame(table(as.matrix(cze_pol_4$name, useNA = "always")))
cze_pol_4$uniquename <- fr$Freq[match(cze_pol_4$name, fr$Var1)]
fr_ <- fr[fr$Freq != 1,]
fr_

## Name1
fr <- data.frame(table(as.matrix(cze_pol_4$name1, useNA = "always")))
cze_pol_4$uniquename1 <- fr$Freq[match(cze_pol_4$name1, fr$Var1)]
fr1_ <- fr[fr$Freq != 1,]
fr1_

## Name2
fr <- data.frame(table(as.matrix(cze_pol_4$name2, useNA = "always")))
cze_pol_4$uniquename2 <- fr$Freq[match(cze_pol_4$name2, fr$Var1)]
fr2_ <- fr[fr$Freq != 1,]
fr2_

## Name + party
fr <- data.frame(table(as.matrix(cze_pol_4$name_party, useNA = "always")))
cze_pol_4$uniquename_party <- fr$Freq[match(cze_pol_4$name_party, fr$Var1)]
fr_p <- fr[fr$Freq != 1,]
fr_p

## Name1 + party
fr <- data.frame(table(as.matrix(cze_pol_4$name1_party, useNA = "always")))
cze_pol_4$uniquename1_party <- fr$Freq[match(cze_pol_4$name1_party, fr$Var1)]
fr1_p <- fr[fr$Freq != 1,]
fr1_p

## Name2 + party
fr <- data.frame(table(as.matrix(cze_pol_4$name2_party, useNA = "always")))
cze_pol_4$uniquename2_party <- fr$Freq[match(cze_pol_4$name2_party, fr$Var1)]
fr2_p <- fr[fr$Freq != 1,]
fr2_p


### MATCHING
## Create empty columns 
cze_pol_4$speaker <- NA

## Match by name and party
cze_pol_4$speaker <- ifelse(is.na(cze_pol_4$speaker) & cze_pol_4$uniquename_party == 1,
                            ParlSpeech_4$speaker[match(cze_pol_4$name_party, ParlSpeech_4$name_party)],
                            cze_pol_4$speaker)
## Match by name1 and party
cze_pol_4$speaker <- ifelse(is.na(cze_pol_4$speaker) & cze_pol_4$uniquename1_party == 1,
                            ParlSpeech_4$speaker[match(cze_pol_4$name1_party, ParlSpeech_4$name_party)],
                            cze_pol_4$speaker)
## Match by name
cze_pol_4$speaker <- ifelse(is.na(cze_pol_4$speaker) & cze_pol_4$uniquename == 1,
                            ParlSpeech_4$speaker[match(cze_pol_4$name, ParlSpeech_4$speaker)],
                            cze_pol_4$speaker)
## Match by name1
cze_pol_4$speaker <- ifelse(is.na(cze_pol_4$speaker) & cze_pol_4$uniquename1 == 1,
                            ParlSpeech_4$speaker[match(cze_pol_4$name1, ParlSpeech_4$speaker)],
                            cze_pol_4$speaker)
# Check the number of still missing data
sum(is.na(cze_pol_4$speaker))
## Match by name2 and party
cze_pol_4$speaker <- ifelse(is.na(cze_pol_4$speaker) & cze_pol_4$uniquename2_party == 1,
                            ParlSpeech_4$speaker[match(cze_pol_4$name2_party, ParlSpeech_4$name_party)],
                            cze_pol_4$speaker)
## Match by name2
cze_pol_4$speaker <- ifelse(is.na(cze_pol_4$speaker) & cze_pol_4$uniquename2 == 1,
                            ParlSpeech_4$speaker[match(cze_pol_4$name2, ParlSpeech_4$speaker)],
                            cze_pol_4$speaker)
# Check the number of still missing data
sum(is.na(cze_pol_4$speaker))


## Match manually (check which existing in ParlSpeech values are not in cze_pol)
# Create csv with unmatched data for manual matching (the code is commented since it is 
# just preparation for manual matching)
# ParlSpeech_4$unname <- cze_pol_4$uniquename_party[match(ParlSpeech_4$name_party,cze_pol_4$name_party)]
# ParlSpeech_4$unname1 <- cze_pol_4$uniquename1_party[match(ParlSpeech_4$name_party,cze_pol_4$name1_party)]
# ParlSpeech_4$match <- NA
# ParlSpeech_4$match <- ifelse(is.na(ParlSpeech_4$match) & ParlSpeech_4$unname == 1,
#                              "matched", ParlSpeech_4$match)
# ParlSpeech_4$match <- ifelse(is.na(ParlSpeech_4$match) & ParlSpeech_4$unname1 == 1,
#                              "matched", ParlSpeech_4$match)
# unmatched_ParlSpeech_4 <- ParlSpeech_4[is.na(ParlSpeech_4$match),]
# cze_pol_4$speaker <- ifelse(is.na(cze_pol_4$speaker),
#                             cze$ParlSpeech_speaker[match(cze_pol_4$wikidataid, cze$wikidataid)],
#                             cze_pol_4$speaker)
# unmatched_cze_pol_4 <- cze_pol_4[is.na(cze_pol_4$speaker),]
# unmatched_cze_pol_4 <- unmatched_cze_pol_4[, !(colnames(unmatched_cze_pol_4) %in% c("name_split"))]

# Match manually
# no matches

### OUTPUT
## Add ParlSpeech speaker's id to uk from uk_pol by wikidataid
cze$ParlSpeech_speaker <- ifelse(is.na(cze$ParlSpeech_speaker),
                                 cze_pol_4$speaker[match(cze$wikidataid, cze_pol_4$wikidataid)],
                                 cze$ParlSpeech_speaker)



################################### Session 5 ###################################
### Filter by 5th session
cze_pol_5 <- cze_pol_1_7[cze_pol_1_7$start_year == 2006,]
ParlSpeech_5 <- psp.corpus[psp.corpus$start_year == 2006,]

### NAMES
ParlSpeech_5$name_party <- paste(ParlSpeech_5$speaker, ParlSpeech_5$party, 
                                 sep = " ", collapse = NULL)
cze_pol_5$name2 <- paste(cze_pol_5$lastname, cze_pol_5$firstname, 
                         sep = " ", collapse = NULL)
cze_pol_5$name2_party <- paste(cze_pol_5$name2, cze_pol_5$party, 
                               sep = " ", collapse = NULL)

### FREQUENCY
## Name
fr <- data.frame(table(as.matrix(cze_pol_5$name, useNA = "always")))
cze_pol_5$uniquename <- fr$Freq[match(cze_pol_5$name, fr$Var1)]
fr_ <- fr[fr$Freq != 1,]
fr_

## Name1
fr <- data.frame(table(as.matrix(cze_pol_5$name1, useNA = "always")))
cze_pol_5$uniquename1 <- fr$Freq[match(cze_pol_5$name1, fr$Var1)]
fr1_ <- fr[fr$Freq != 1,]
fr1_

## Name + party
fr <- data.frame(table(as.matrix(cze_pol_5$name_party, useNA = "always")))
cze_pol_5$uniquename_party <- fr$Freq[match(cze_pol_5$name_party, fr$Var1)]
fr_p <- fr[fr$Freq != 1,]
fr_p

## Name1 + party
fr <- data.frame(table(as.matrix(cze_pol_5$name1_party, useNA = "always")))
cze_pol_5$uniquename1_party <- fr$Freq[match(cze_pol_5$name1_party, fr$Var1)]
fr1_p <- fr[fr$Freq != 1,]
fr1_p

## Name2
fr <- data.frame(table(as.matrix(cze_pol_5$name2, useNA = "always")))
cze_pol_5$uniquename2 <- fr$Freq[match(cze_pol_5$name2, fr$Var1)]
fr2_ <- fr[fr$Freq != 1,]
fr2_

## Name2 + party
fr <- data.frame(table(as.matrix(cze_pol_5$name2_party, useNA = "always")))
cze_pol_5$uniquename2_party <- fr$Freq[match(cze_pol_5$name2_party, fr$Var1)]
fr2_p <- fr[fr$Freq != 1,]
fr2_p


### MATCHING
## Create empty columns 
cze_pol_5$speaker <- NA

## Match by name and party
cze_pol_5$speaker <- ifelse(is.na(cze_pol_5$speaker) & cze_pol_5$uniquename_party == 1,
                            ParlSpeech_5$speaker[match(cze_pol_5$name_party, ParlSpeech_5$name_party)],
                            cze_pol_5$speaker)
## Match by name
cze_pol_5$speaker <- ifelse(is.na(cze_pol_5$speaker) & cze_pol_5$uniquename == 1,
                            ParlSpeech_5$speaker[match(cze_pol_5$name, ParlSpeech_5$speaker)],
                            cze_pol_5$speaker)
## Match by name1 and party
cze_pol_5$speaker <- ifelse(is.na(cze_pol_5$speaker) & cze_pol_5$uniquename1_party == 1,
                            ParlSpeech_5$speaker[match(cze_pol_5$name1_party, ParlSpeech_5$name_party)],
                            cze_pol_5$speaker)
## Match by name1
cze_pol_5$speaker <- ifelse(is.na(cze_pol_5$speaker) & cze_pol_5$uniquename1 == 1,
                            ParlSpeech_5$speaker[match(cze_pol_5$name1, ParlSpeech_5$speaker)],
                            cze_pol_5$speaker)
# Check the number of still missing data
sum(is.na(cze_pol_5$speaker))
## Match by name2 and party
cze_pol_5$speaker <- ifelse(is.na(cze_pol_5$speaker) & cze_pol_5$uniquename2_party == 1,
                            ParlSpeech_5$speaker[match(cze_pol_5$name2_party, ParlSpeech_5$name_party)],
                            cze_pol_5$speaker)
## Match by name2
cze_pol_5$speaker <- ifelse(is.na(cze_pol_5$speaker) & cze_pol_5$uniquename2 == 1,
                            ParlSpeech_5$speaker[match(cze_pol_5$name2, ParlSpeech_5$speaker)],
                            cze_pol_5$speaker)
# Check the number of still missing data
sum(is.na(cze_pol_5$speaker))


## Match manually (check which existing in ParlSpeech values are not in cze_pol)
# Create csv with unmatched data for manual matching (the code is commented since it is 
# just preparation for manual matching)
ParlSpeech_5$unname <- cze_pol_5$uniquename_party[match(ParlSpeech_5$name_party,cze_pol_5$name_party)]
ParlSpeech_5$unname1 <- cze_pol_5$uniquename1_party[match(ParlSpeech_5$name_party,cze_pol_5$name1_party)]
ParlSpeech_5$match <- NA
ParlSpeech_5$match <- ifelse(is.na(ParlSpeech_5$match) & ParlSpeech_5$unname == 1,
                             "matched", ParlSpeech_5$match)
ParlSpeech_5$match <- ifelse(is.na(ParlSpeech_5$match) & ParlSpeech_5$unname1 == 1,
                             "matched", ParlSpeech_5$match)
unmatched_ParlSpeech_5 <- ParlSpeech_5[is.na(ParlSpeech_5$match),]
cze_pol_5$speaker <- ifelse(is.na(cze_pol_5$speaker),
                            cze$ParlSpeech_speaker[match(cze_pol_5$wikidataid, cze$wikidataid)],
                            cze_pol_5$speaker)
unmatched_cze_pol_5 <- cze_pol_5[is.na(cze_pol_5$speaker),]
unmatched_cze_pol_5 <- unmatched_cze_pol_5[, !(colnames(unmatched_cze_pol_5) %in% c("name_split"))]

# Match manually
# no matches


### OUTPUT
## Add ParlSpeech speaker's id to uk from uk_pol by wikidataid
cze$ParlSpeech_speaker <- ifelse(is.na(cze$ParlSpeech_speaker),
                                 cze_pol_5$speaker[match(cze$wikidataid, cze_pol_5$wikidataid)],
                                 cze$ParlSpeech_speaker)



################################### Session 6 ###################################
### Filter by 6th session
cze_pol_6 <- cze_pol_1_7[cze_pol_1_7$start_year == 2010,]
ParlSpeech_6 <- psp.corpus[psp.corpus$start_year == 2010,]

### NAMES
ParlSpeech_6$name_party <- paste(ParlSpeech_6$speaker, ParlSpeech_6$party, 
                                 sep = " ", collapse = NULL)
### FREQUENCY
## Name
fr <- data.frame(table(as.matrix(cze_pol_6$name, useNA = "always")))
cze_pol_6$uniquename <- fr$Freq[match(cze_pol_6$name, fr$Var1)]
fr_ <- fr[fr$Freq != 1,]
fr_

## Name1
fr <- data.frame(table(as.matrix(cze_pol_6$name1, useNA = "always")))
cze_pol_6$uniquename1 <- fr$Freq[match(cze_pol_6$name1, fr$Var1)]
fr1_ <- fr[fr$Freq != 1,]
fr1_

## Name + party
fr <- data.frame(table(as.matrix(cze_pol_6$name_party, useNA = "always")))
cze_pol_6$uniquename_party <- fr$Freq[match(cze_pol_6$name_party, fr$Var1)]
fr_p <- fr[fr$Freq != 1,]
fr_p

## Name1 + party
fr <- data.frame(table(as.matrix(cze_pol_6$name1_party, useNA = "always")))
cze_pol_6$uniquename1_party <- fr$Freq[match(cze_pol_6$name1_party, fr$Var1)]
fr1_p <- fr[fr$Freq != 1,]
fr1_p

### MATCHING
## Create empty columns 
cze_pol_6$speaker <- NA

## Match by name
cze_pol_6$speaker <- ifelse(is.na(cze_pol_6$speaker) & cze_pol_6$uniquename == 1,
                            ParlSpeech_6$speaker[match(cze_pol_6$name, ParlSpeech_6$speaker)],
                            cze_pol_6$speaker)
## Match by name1
cze_pol_6$speaker <- ifelse(is.na(cze_pol_6$speaker) & cze_pol_6$uniquename1 == 1,
                            ParlSpeech_6$speaker[match(cze_pol_6$name1, ParlSpeech_6$speaker)],
                            cze_pol_6$speaker)
## Match by name and party
cze_pol_6$speaker <- ifelse(is.na(cze_pol_6$speaker) & cze_pol_6$uniquename_party == 1,
                            ParlSpeech_6$speaker[match(cze_pol_6$name_party, ParlSpeech_6$name_party)],
                            cze_pol_6$speaker)
## Match by name1 and party
cze_pol_6$speaker <- ifelse(is.na(cze_pol_6$speaker) & cze_pol_6$uniquename1_party == 1,
                            ParlSpeech_6$speaker[match(cze_pol_6$name1_party, ParlSpeech_6$name_party)],
                            cze_pol_6$speaker)
# Check the number of still missing data
sum(is.na(cze_pol_6$speaker))


## Match manually (check which existing in ParlSpeech values are not in cze_pol)
# Create csv with unmatched data for manual matching (the code is commented since it is 
# just preparation for manual matching)
# ParlSpeech_6$unname <- cze_pol_6$uniquename_party[match(ParlSpeech_6$name_party,cze_pol_6$name_party)]
# ParlSpeech_6$unname1 <- cze_pol_6$uniquename1_party[match(ParlSpeech_6$name_party,cze_pol_6$name1_party)]
# ParlSpeech_6$match <- NA
# ParlSpeech_6$match <- ifelse(is.na(ParlSpeech_6$match) & ParlSpeech_6$unname == 1,
#                              "matched", ParlSpeech_6$match)
# ParlSpeech_6$match <- ifelse(is.na(ParlSpeech_6$match) & ParlSpeech_6$unname1 == 1,
#                              "matched", ParlSpeech_6$match)
# unmatched_ParlSpeech_6 <- ParlSpeech_6[is.na(ParlSpeech_6$match),]
# cze_pol_6$speaker <- ifelse(is.na(cze_pol_6$speaker),
#                             cze$ParlSpeech_speaker[match(cze_pol_6$wikidataid, cze$wikidataid)],
#                             cze_pol_6$speaker)
# unmatched_cze_pol_6 <- cze_pol_6[is.na(cze_pol_6$speaker),]
# unmatched_cze_pol_6 <- unmatched_cze_pol_6[, !(colnames(unmatched_cze_pol_6) %in% c("name_split"))]

# Match manually
cze_pol_6$speaker[cze_pol_6$wikidataid == "Q12026431"] <- "Josef Novotny ml"
cze_pol_6$speaker[cze_pol_6$wikidataid == "Q12026433"] <- "Josef Novotny st"


### OUTPUT
## Add ParlSpeech speaker's id to uk from uk_pol by wikidataid
cze$ParlSpeech_speaker <- ifelse(is.na(cze$ParlSpeech_speaker),
                                 cze_pol_6$speaker[match(cze$wikidataid, cze_pol_6$wikidataid)],
                                 cze$ParlSpeech_speaker)



################################### Session 7 ###################################
### Filter by 7th session
cze_pol_7 <- cze_pol_1_7[cze_pol_1_7$start_year == 2013,]
ParlSpeech_7 <- psp.corpus[psp.corpus$start_year == 2013,]

### NAMES
ParlSpeech_7$name_party <- paste(ParlSpeech_7$speaker, ParlSpeech_7$party, 
                                 sep = " ", collapse = NULL)
### FREQUENCY
## Name
fr <- data.frame(table(as.matrix(cze_pol_7$name, useNA = "always")))
cze_pol_7$uniquename <- fr$Freq[match(cze_pol_7$name, fr$Var1)]
fr_ <- fr[fr$Freq != 1,]
fr_

## Name1
fr <- data.frame(table(as.matrix(cze_pol_7$name1, useNA = "always")))
cze_pol_7$uniquename1 <- fr$Freq[match(cze_pol_7$name1, fr$Var1)]
fr1_ <- fr[fr$Freq != 1,]
fr1_

## Name + party
fr <- data.frame(table(as.matrix(cze_pol_7$name_party, useNA = "always")))
cze_pol_7$uniquename_party <- fr$Freq[match(cze_pol_7$name_party, fr$Var1)]
fr_p <- fr[fr$Freq != 1,]
fr_p

## Name1 + party
fr <- data.frame(table(as.matrix(cze_pol_7$name1_party, useNA = "always")))
cze_pol_7$uniquename1_party <- fr$Freq[match(cze_pol_7$name1_party, fr$Var1)]
fr1_p <- fr[fr$Freq != 1,]
fr1_p

### MATCHING
## Create empty columns
cze_pol_7$speaker <- NA

## Match by name
cze_pol_7$speaker <- ifelse(is.na(cze_pol_7$speaker) & cze_pol_7$uniquename == 1,
                            ParlSpeech_7$speaker[match(cze_pol_7$name, ParlSpeech_7$speaker)],
                            cze_pol_7$speaker)
## Match by name1
cze_pol_7$speaker <- ifelse(is.na(cze_pol_7$speaker) & cze_pol_7$uniquename1 == 1,
                            ParlSpeech_7$speaker[match(cze_pol_7$name1, ParlSpeech_7$speaker)],
                            cze_pol_7$speaker)
## Match by name and party
cze_pol_7$speaker <- ifelse(is.na(cze_pol_7$speaker) & cze_pol_7$uniquename_party == 1,
                            ParlSpeech_7$speaker[match(cze_pol_7$name_party, ParlSpeech_7$name_party)],
                            cze_pol_7$speaker)
## Match by name1 and party
cze_pol_7$speaker <- ifelse(is.na(cze_pol_7$speaker) & cze_pol_7$uniquename1_party == 1,
                            ParlSpeech_7$speaker[match(cze_pol_7$name1_party, ParlSpeech_7$name_party)],
                            cze_pol_7$speaker)
# Check the number of still missing data
sum(is.na(cze_pol_7$speaker))


## Match manually (check which existing in ParlSpeech values are not in cze_pol)
# Create csv with unmatched data for manual matching (the code is commented since it is 
# just preparation for manual matching)
ParlSpeech_7$unname <- cze_pol_7$uniquename_party[match(ParlSpeech_7$name_party,cze_pol_7$name_party)]
ParlSpeech_7$unname1 <- cze_pol_7$uniquename1_party[match(ParlSpeech_7$name_party,cze_pol_7$name1_party)]
ParlSpeech_7$match <- NA
ParlSpeech_7$match <- ifelse(is.na(ParlSpeech_7$match) & ParlSpeech_7$unname == 1,
                             "matched", ParlSpeech_7$match)
ParlSpeech_7$match <- ifelse(is.na(ParlSpeech_7$match) & ParlSpeech_7$unname1 == 1,
                             "matched", ParlSpeech_7$match)
unmatched_ParlSpeech_7 <- ParlSpeech_7[is.na(ParlSpeech_7$match),]
cze_pol_7$speaker <- ifelse(is.na(cze_pol_7$speaker),
                            cze$ParlSpeech_speaker[match(cze_pol_7$wikidataid, cze$wikidataid)],
                            cze_pol_7$speaker)
unmatched_cze_pol_7 <- cze_pol_7[is.na(cze_pol_7$speaker),]
unmatched_cze_pol_7 <- unmatched_cze_pol_7[, !(colnames(unmatched_cze_pol_7) %in% c("name_split"))]

# Match manually
cze_pol_7$speaker[cze_pol_7$wikidataid == "Q13721260"] <- "Tomas Jan Podivinsky"


### OUTPUT
## Add ParlSpeech speaker's id to cze from cze_pol by wikidataid
cze$ParlSpeech_speaker <- ifelse(is.na(cze$ParlSpeech_speaker),
                                 cze_pol_7$speaker[match(cze$wikidataid, cze_pol_7$wikidataid)],
                                 cze$ParlSpeech_speaker)



################################### THE OUTPUT DATAFRAME ################################### 
# output
cze_output <- subset(cze, select = c('wikidataid', 'ParlSpeech_speaker'))
sum(!is.na(cze_output$ParlSpeech_speaker))
# We found 880 out of 1096 (according pdf to ParlSpeech data set)
cze_output <- cze_output[!is.na(cze_output$ParlSpeech_speaker),]
saveRDS(cze_output, "./data/pol_sci_data/cze_output")




