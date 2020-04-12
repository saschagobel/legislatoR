# ---------------------------------------------------------------------------------------
# legislatoR
# Sascha Göbel and Simon Munzert
# Script: integration with uk parlspeech
# This script was written by Lada Rudnitckaia
# March 2020
# ---------------------------------------------------------------------------------------

# devtools::install_github("saschagobel/legislatoR")
library(legislatoR)
library(xlsx)
setwd("C:/Users/User/Desktop/Student job/P.Selb/integration")

uk <- get_core(legislature = "gbr")
uk_pol <- get_political(legislature = "gbr")
load("Corp_HouseOfCommons.Rdata")
# ParlSpeech_sessions <- data.frame(table(as.matrix(hoc.corpus$session, useNA = "always")))
# ParlSpeech_sessions


################################### Session 50-56 ###################################
## Retrieve year from 'session_start' column
uk_pol$start_year <- format(uk_pol$session_start,"%Y")

## Filter by session 50-56 (as in ParlSpeech)
uk_pol_5056 <- uk_pol[uk_pol$start_year == 1987 |
                      uk_pol$start_year == 1992 |
                      uk_pol$start_year == 1997 |
                      uk_pol$start_year == 2001 |
                      uk_pol$start_year == 2005 |
                      uk_pol$start_year == 2010 |
                      uk_pol$start_year == 2015,]


### PARTY NAMES
### Since we will use party name for matching, they should be similar in uk_pol and ParlSpeech
## Check party names in uk_pol
# fr <- data.frame(table(as.matrix(uk_pol_5056$party, useNA = "always")))
## Check party names in ParlSpeech
# fr_ParlSpeech <- data.frame(table(as.matrix(hoc.corpus$party, useNA = "always")))

## Change party names in uk_pol to respective name from ParlSpeech
# 1        Alliance Party of Northern Ireland - APNI
# 2                        Conservative Party - Con
# 3                 Democratic Unionist Party - DUP
# 4          Green Party of England and Wales - GPEW
# 5                               Independent - other
# 6  Independent Community and Health Concern - other
# 7                   Labour and Co-operative - Lab
# 8                              Labour Party - Lab
# 9                         Liberal Democrats - LibDem
# 10                            Liberal Party - LibDem
# 11                              Plaid Cymru - PlaidCymru
# 12                            Respect Party - Respect
# 13                  Scottish National Party - SNP
# 14                                Sinn Fein - other
# 15       Social Democratic and Labour Party - SDLP
# 16                  Social Democratic Party - SDP
# 17                    UK Independence Party - UKIP
# 18                              UK Unionist - UKUP
# 19                  Ulster Popular Unionist - UPUP
# 20                          Ulster Unionist - UUP

uk_pol_5056$party <- gsub("Alliance Party of Northern Ireland", "APNI", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Conservative Party", "Con", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Democratic Unionist Party", "DUP", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Green Party of England and Wales", "GPEW", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Independent Community and Health Concern", "other", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Labour and Co-operative", "Lab", uk_pol_5056$party) # not sure, but according to data after names matching
uk_pol_5056$party <- gsub("Liberal Democrats", "LibDem", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Liberal Party", "LibDem", uk_pol_5056$party) # not sure, but according to data after names matching
uk_pol_5056$party <- gsub("Plaid Cymru", "PlaidCymru", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Respect Party", "Respect", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Scottish National Party", "SNP", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Sinn Fein", "other", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Social Democratic and Labour Party", "SDLP", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Social Democratic Party", "SDP", uk_pol_5056$party)
uk_pol_5056$party <- gsub("UK Independence Party", "UKIP", uk_pol_5056$party)
uk_pol_5056$party <- gsub("UK Unionist", "UKUP", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Ulster Popular Unionist", "UPUP", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Ulster Unionist", "UUP", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Labour Party", "Lab", uk_pol_5056$party)
uk_pol_5056$party <- gsub("Independent", "other", uk_pol_5056$party)


## Add wikidataids and names to uk_pol from uk
uk_pol_5056$wikidataid <- uk$wikidataid[match(uk_pol_5056$pageid, uk$pageid)]
uk_pol_5056$name <- uk$name[match(uk_pol_5056$pageid, uk$pageid)]

### NAMES
## In uk_pol, some names contain "Sir", "Hon.", "Lord", "Dr". Since in ParlSpeech contains only names,
## let's remove this from uk_pol and create new column 'name1'
uk_pol_5056$name1 <- uk_pol_5056$name
uk_pol_5056$name1 <- gsub("Hon. ", "", uk_pol_5056$name1)
uk_pol_5056$name1 <- gsub("Sir ", "", uk_pol_5056$name1)
uk_pol_5056$name1 <- gsub("Lord ", "", uk_pol_5056$name1)
uk_pol_5056$name1 <- gsub("Dr ", "", uk_pol_5056$name1)
uk_pol_5056$name1 <- gsub(", Bt", "", uk_pol_5056$name1)

## Also some names include middle names that are not mentioned in ParlSpeech. Let's remove them 
## and add a new column 'name2'
uk_pol_5056$name_split = strsplit(uk_pol_5056$name1, ' ')
uk_pol_5056$firstname = sapply(uk_pol_5056$name_split, function(x) x[1])
uk_pol_5056$lastname = sapply(uk_pol_5056$name_split, function(x) x[length(x)])
uk_pol_5056$name2 <- paste(uk_pol_5056$firstname, uk_pol_5056$lastname, sep = " ", collapse = NULL)

## Concatenate name and party to match both by name and party
uk_pol_5056$name_party <- paste(uk_pol_5056$name, uk_pol_5056$party, sep = " ", collapse = NULL)
uk_pol_5056$name1_party <- paste(uk_pol_5056$name1, uk_pol_5056$party, sep = " ", collapse = NULL)
uk_pol_5056$name2_party <- paste(uk_pol_5056$name2, uk_pol_5056$party, sep = " ", collapse = NULL)



################################### Session 50 ###################################
### Filter by 50th session
uk_pol_50 <- uk_pol_5056[uk_pol_5056$start_year == 1987,]
ParlSpeech_50 <- hoc.corpus[hoc.corpus$session == "1988-89" |
                              hoc.corpus$session == "1989-90" |
                              hoc.corpus$session == "1990-91" |
                              hoc.corpus$session == "1991-92",]

### NAMES
ParlSpeech_50$name_party <- paste(ParlSpeech_50$speaker, ParlSpeech_50$party, 
                                  sep = " ", collapse = NULL)
### FREQUENCY
## Name
fr <- data.frame(table(as.matrix(uk_pol_50$name_party, useNA = "always")))
uk_pol_50$uniquename_party <- fr$Freq[match(uk_pol_50$name_party, fr$Var1)]
fr_ <- fr[fr$Freq != 1,]
fr_

## Name1
fr <- data.frame(table(as.matrix(uk_pol_50$name1_party, useNA = "always")))
uk_pol_50$uniquename1_party <- fr$Freq[match(uk_pol_50$name1_party, fr$Var1)]
fr1_ <- fr[fr$Freq != 1,]
fr1_

## Name2
fr <- data.frame(table(as.matrix(uk_pol_50$name2_party, useNA = "always")))
uk_pol_50$uniquename2_party <- fr$Freq[match(uk_pol_50$name2_party, fr$Var1)]
fr2_ <- fr[fr$Freq != 1,]
fr2_


### MATCHING
### Match only if:
### 1. the desired value is still missing
### 2. the pair name+party is unique in uk_pol (we assume that it's always true in ParlSpeech)
### 3. if possible, match manually

## Create empty columns 
uk_pol_50$speaker <- NA

## Match by name and party
uk_pol_50$speaker <- ifelse(is.na(uk_pol_50$speaker) & uk_pol_50$uniquename_party == 1,
                            ParlSpeech_50$speaker[match(uk_pol_50$name_party, ParlSpeech_50$name_party)],
                            uk_pol_50$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_50$speaker))

## Match by name1 and party
uk_pol_50$speaker <- ifelse(is.na(uk_pol_50$speaker) & uk_pol_50$uniquename1_party == 1,
                            ParlSpeech_50$speaker[match(uk_pol_50$name1_party, ParlSpeech_50$name_party)],
                            uk_pol_50$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_50$speaker))

## Match by name2 and party
uk_pol_50$speaker <- ifelse(is.na(uk_pol_50$speaker) & uk_pol_50$uniquename2_party == 1,
                            ParlSpeech_50$speaker[match(uk_pol_50$name2_party, ParlSpeech_50$name_party)],
                            uk_pol_50$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_50$speaker))


## Match manually (check which existing in ParlSpeech values are not in uk_pol)
# Create csv with unmatched data for manual matching (the code is commented since it is 
# just preparation for manual matching)
#ParlSpeech_50$unname <- uk_pol_50$uniquename_party[match(ParlSpeech_50$name_party,uk_pol_50$name_party)]
#ParlSpeech_50$unname1 <- uk_pol_50$uniquename1_party[match(ParlSpeech_50$name_party,uk_pol_50$name1_party)]
#ParlSpeech_50$unname2 <- uk_pol_50$uniquename2_party[match(ParlSpeech_50$name_party,uk_pol_50$name2_party)]
#ParlSpeech_50$match <- NA
#ParlSpeech_50$match <- ifelse(is.na(ParlSpeech_50$match) & ParlSpeech_50$unname == 1,
#                            "matched", ParlSpeech_50$match)
#ParlSpeech_50$match <- ifelse(is.na(ParlSpeech_50$match) & ParlSpeech_50$unname1 == 1,
#                            "matched", ParlSpeech_50$match)
#ParlSpeech_50$match <- ifelse(is.na(ParlSpeech_50$match) & ParlSpeech_50$unname2 == 1,
#                            "matched", ParlSpeech_50$match)
#unmatched_ParlSpeech_50 <- ParlSpeech_50[is.na(ParlSpeech_50$match),]
#unmatched_uk_pol_50 <- uk_pol_50[is.na(uk_pol_50$speaker),]
#unmatched_uk_pol_50 <- unmatched_uk_pol_50[, !(colnames(unmatched_uk_pol_50) %in% c("name_split"))]
#library(xlsx)
#write.xlsx(unmatched_ParlSpeech_50, file = "unmatched_ParlSpeech_50.xlsx", row.names=FALSE)
#write.xlsx(unmatched_uk_pol_50, file = "unmatched_uk_pol_50.xlsx", row.names=FALSE)

# Match manually
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q4716943"] <- "Alexander Eadie"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q4777250"] <- "Anthony Speller"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q333722"] <- "Betty Boothroyd"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q332950"] <- "Charles Kennedy"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q5106053"] <- "Christopher Butler"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q5229455"] <- "Dave Nellist"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q279314"] <- "Doug Hoyle"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q5344222"] <- "Eddie Loyden"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q2399855"] <- "Ted Rowlands"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q5534714"] <- "Geoffrey Johnson Smith"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q5552654"] <- "Gerry Bermingham"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q750219"] <- "Greg Knight"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q5640864"] <- "Hilary Miller"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q1585945"] <- "Harold Walker"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q6198115"] <- "Jim Sillars"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q6201626"] <- "Jimmy Wray"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q6196395"] <- "James Lester"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q6197404"] <- "James Pawsey"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q6208398"] <- "Joe Ashton"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q6204390"] <- "Jo Richardson"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q579412"] <- "Kenneth Hargreaves"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q1738633"] <- "Ken Maginnis"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q1866867"] <- "Llin Golding"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q262727"] <- "Mo Mowlam"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q6849323"] <- "Michael Woodcock"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q334578"] <- "Paul Dean"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q333650"] <- "Peter Blaker"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q13530012"] <- "Philip Oppenheim"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q4932182"] <- "Bob Cryer"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q269005"] <- "Ronnie Campbell"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q333689"] <- "Ronnie Fearn"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q7364072"] <- "Ron Leighton"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q7025093"] <- "Nicholas Bonsor"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q16294802"] <- "Stan Orme"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q14946689"] <- "Edward Garrett"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q2405597"] <- "Terry Fields"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q271935"] <- "Tom Clarke"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q7819456"] <- "Tommy Graham"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q7803427"] <- "Timothy Devlin"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q2434248"] <- "Timothy Renton"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q7804223"] <- "Tim Sainsbury"
uk_pol_50$speaker[uk_pol_50$wikidataid == "Q7822270"] <- "Anthony Durant"
# Check the number of still missing data
sum(is.na(uk_pol_50$speaker))


### OUTPUT
## Add ParlSpeech speaker's id to uk from uk_pol by wikidataid
uk$ParlSpeech_speaker <- uk_pol_50$speaker[match(uk$wikidataid, uk_pol_50$wikidataid)]



################################### Session 51 ###################################
### Filter by 51th session
uk_pol_51 <- uk_pol_5056[uk_pol_5056$start_year == 1992,]
ParlSpeech_51 <- hoc.corpus[hoc.corpus$session == "1992-93" |
                              hoc.corpus$session == "1993-94" |
                              hoc.corpus$session == "1994-95" |
                              hoc.corpus$session == "1995-96" |
                              hoc.corpus$session == "1996-97",]

### NAMES
ParlSpeech_51$name_party <- paste(ParlSpeech_51$speaker, ParlSpeech_51$party, 
                                  sep = " ", collapse = NULL)
### FREQUENCY
## Name
fr <- data.frame(table(as.matrix(uk_pol_51$name_party, useNA = "always")))
uk_pol_51$uniquename_party <- fr$Freq[match(uk_pol_51$name_party, fr$Var1)]
fr_ <- fr[fr$Freq != 1,]
fr_

## Name1
fr <- data.frame(table(as.matrix(uk_pol_51$name1_party, useNA = "always")))
uk_pol_51$uniquename1_party <- fr$Freq[match(uk_pol_51$name1_party, fr$Var1)]
fr1_ <- fr[fr$Freq != 1,]
fr1_

## Name2
fr <- data.frame(table(as.matrix(uk_pol_51$name2_party, useNA = "always")))
uk_pol_51$uniquename2_party <- fr$Freq[match(uk_pol_51$name2_party, fr$Var1)]
fr2_ <- fr[fr$Freq != 1,]
fr2_


### MATCHING
## Create empty columns
uk_pol_51$speaker <- NA

## Match by name and party
uk_pol_51$speaker <- ifelse(is.na(uk_pol_51$speaker) & uk_pol_51$uniquename_party == 1,
                            ParlSpeech_51$speaker[match(uk_pol_51$name_party, ParlSpeech_51$name_party)],
                            uk_pol_51$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_51$speaker))

## Match by name1 and party
uk_pol_51$speaker <- ifelse(is.na(uk_pol_51$speaker) & uk_pol_51$uniquename1_party == 1,
                            ParlSpeech_51$speaker[match(uk_pol_51$name1_party, ParlSpeech_51$name_party)],
                            uk_pol_51$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_51$speaker))

## Match by name2 and party
uk_pol_51$speaker <- ifelse(is.na(uk_pol_51$speaker) & uk_pol_51$uniquename2_party == 1,
                            ParlSpeech_51$speaker[match(uk_pol_51$name2_party, ParlSpeech_51$name_party)],
                            uk_pol_51$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_51$speaker))


## Match manually (check which existing in ParlSpeech values are not in uk_pol)
# Create csv with unmatched data for manual matching (the code is commented since it is 
# just preparation for manual matching)
ParlSpeech_51$unname <- uk_pol_51$uniquename_party[match(ParlSpeech_51$name_party,uk_pol_51$name_party)]
ParlSpeech_51$unname1 <- uk_pol_51$uniquename1_party[match(ParlSpeech_51$name_party,uk_pol_51$name1_party)]
ParlSpeech_51$unname2 <- uk_pol_51$uniquename2_party[match(ParlSpeech_51$name_party,uk_pol_51$name2_party)]
ParlSpeech_51$match <- NA
ParlSpeech_51$match <- ifelse(is.na(ParlSpeech_51$match) & ParlSpeech_51$unname == 1,
                           "matched", ParlSpeech_51$match)
ParlSpeech_51$match <- ifelse(is.na(ParlSpeech_51$match) & ParlSpeech_51$unname1 == 1,
                           "matched", ParlSpeech_51$match)
ParlSpeech_51$match <- ifelse(is.na(ParlSpeech_51$match) & ParlSpeech_51$unname2 == 1,
                           "matched", ParlSpeech_51$match)
unmatched_ParlSpeech_51 <- ParlSpeech_51[is.na(ParlSpeech_51$match),]
unmatched_uk_pol_51 <- uk_pol_51[is.na(uk_pol_51$speaker),]
unmatched_uk_pol_51 <- unmatched_uk_pol_51[, !(colnames(unmatched_uk_pol_51) %in% c("name_split"))]
write.csv(unmatched_ParlSpeech_51, file = "unmatched_ParlSpeech_51.csv", row.names=FALSE)
write.csv(unmatched_uk_pol_51, file = "unmatched_uk_pol_51.csv", row.names=FALSE)

# Match manually
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q4708112"] <- "Williams"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q4757839"] <- "Andrew MacKinlay"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q4777232"] <- "Tony Marlow"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q333068"] <- "Tony Newton"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q333722"] <- "Betty Boothroyd"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q1494468"] <- "Robert Maclennan"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q300308"] <- "Brian H Donohoe"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q279314"] <- "Doug Hoyle"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q5344222"] <- "Eddie Loyden"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q2399855"] <- "Ted Rowlands"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q5534714"] <- "Geoffrey Johnson Smith"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q1503024"] <- "Geoffrey Lofthouse"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q5552654"] <- "Gerry Bermingham"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q750219"] <- "Greg Knight"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q6130070"] <- "Jimmy Boyce"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q6201626"] <- "Jimmy Wray"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q222985"] <- "Janet Fookes"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q6196395"] <- "James Lester"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q6197404"] <- "James Pawsey"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q6210280"] <- "Joseph Hendron"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q6208398"] <- "Joe Ashton"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q262872"] <- "Joe Benton"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q1738633"] <- "Ken Maginnis"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q6470476"] <- "Olga Maitland"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q1866867"] <- "Llin Golding"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q262727"] <- "Mo Mowlam"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q1928654"] <- "Michael Morris"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q13530012"] <- "Philip Oppenheim"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q7190233"] <- "Piara S Khabra"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q4932182"] <- "Bob Cryer"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q269005"] <- "Ronnie Campbell"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q7364072"] <- "Ron Leighton"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q16294802"] <- "Stan Orme"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q271935"] <- "Tom Clarke"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q7819456"] <- "Tommy Graham"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q7803427"] <- "Timothy Devlin"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q2434248"] <- "Timothy Renton"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q7804223"] <- "Tim Sainsbury"
uk_pol_51$speaker[uk_pol_51$wikidataid == "Q7822270"] <- "Anthony Durant"


# Check the number of still missing data
sum(is.na(uk_pol_51$speaker))


### OUTPUT
## Add ParlSpeech speaker's id to uk from uk_pol by wikidataid
uk$ParlSpeech_speaker <- ifelse(is.na(uk$ParlSpeech_speaker),
                                uk_pol_51$speaker[match(uk$wikidataid, uk_pol_51$wikidataid)],
                                uk$ParlSpeech_speaker)



################################### Session 52 ###################################
### Filter by 52th session
uk_pol_52 <- uk_pol_5056[uk_pol_5056$start_year == 1997,]
ParlSpeech_52 <- hoc.corpus[hoc.corpus$session == "1997-98" |
                              hoc.corpus$session == "1998-99" |
                              hoc.corpus$session == "1999-00" |
                              hoc.corpus$session == "2000-01",]

### NAMES
ParlSpeech_52$name_party <- paste(ParlSpeech_52$speaker, ParlSpeech_52$party, 
                                  sep = " ", collapse = NULL)
### FREQUENCY
## Name
fr <- data.frame(table(as.matrix(uk_pol_52$name_party, useNA = "always")))
uk_pol_52$uniquename_party <- fr$Freq[match(uk_pol_52$name_party, fr$Var1)]
fr_ <- fr[fr$Freq != 1,]
fr_

## Name1
fr <- data.frame(table(as.matrix(uk_pol_52$name1_party, useNA = "always")))
uk_pol_52$uniquename1_party <- fr$Freq[match(uk_pol_52$name1_party, fr$Var1)]
fr1_ <- fr[fr$Freq != 1,]
fr1_

## Name2
fr <- data.frame(table(as.matrix(uk_pol_52$name2_party, useNA = "always")))
uk_pol_52$uniquename2_party <- fr$Freq[match(uk_pol_52$name2_party, fr$Var1)]
fr2_ <- fr[fr$Freq != 1,]
fr2_


### MATCHING
## Create empty columns 
uk_pol_52$speaker <- NA


## Match by name and party
uk_pol_52$speaker <- ifelse(is.na(uk_pol_52$speaker) & uk_pol_52$uniquename_party == 1,
                            ParlSpeech_52$speaker[match(uk_pol_52$name_party, ParlSpeech_52$name_party)],
                            uk_pol_52$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_52$speaker))

## Match by name1 and party
uk_pol_52$speaker <- ifelse(is.na(uk_pol_52$speaker) & uk_pol_52$uniquename1_party == 1,
                            ParlSpeech_52$speaker[match(uk_pol_52$name1_party, ParlSpeech_52$name_party)],
                            uk_pol_52$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_52$speaker))

## Match by name2 and party
uk_pol_52$speaker <- ifelse(is.na(uk_pol_52$speaker) & uk_pol_52$uniquename2_party == 1,
                            ParlSpeech_52$speaker[match(uk_pol_52$name2_party, ParlSpeech_52$name_party)],
                            uk_pol_52$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_52$speaker))


## Match manually (check which existing in ParlSpeech values are not in uk_pol)
# Create csv with unmatched data for manual matching (the code is commented since it is 
# just preparation for manual matching)
# ParlSpeech_52$unname <- uk_pol_52$uniquename_party[match(ParlSpeech_52$name_party,uk_pol_52$name_party)]
# ParlSpeech_52$unname1 <- uk_pol_52$uniquename1_party[match(ParlSpeech_52$name_party,uk_pol_52$name1_party)]
# ParlSpeech_52$unname2 <- uk_pol_52$uniquename2_party[match(ParlSpeech_52$name_party,uk_pol_52$name2_party)]
# ParlSpeech_52$match <- NA
# ParlSpeech_52$match <- ifelse(is.na(ParlSpeech_52$match) & ParlSpeech_52$unname == 1,
#                            "matched", ParlSpeech_52$match)
# ParlSpeech_52$match <- ifelse(is.na(ParlSpeech_52$match) & ParlSpeech_52$unname1 == 1,
#                            "matched", ParlSpeech_52$match)
# ParlSpeech_52$match <- ifelse(is.na(ParlSpeech_52$match) & ParlSpeech_52$unname2 == 1,
#                            "matched", ParlSpeech_52$match)
# unmatched_ParlSpeech_52 <- ParlSpeech_52[is.na(ParlSpeech_52$match),]
# 
# uk_pol_52$speaker <- ifelse(is.na(uk_pol_52$speaker),
#                             uk$ParlSpeech_speaker[match(uk_pol_52$wikidataid, uk$wikidataid)],
#                             uk_pol_52$speaker)
# unmatched_uk_pol_52 <- uk_pol_52[is.na(uk_pol_52$speaker),]
# unmatched_uk_pol_52 <- unmatched_uk_pol_52[, !(colnames(unmatched_uk_pol_52) %in% c("name_split"))]
# 
# write.csv(unmatched_ParlSpeech_52, file = "unmatched_ParlSpeech_52.csv", row.names=FALSE)
# write.csv(unmatched_uk_pol_52, file = "unmatched_uk_pol_52.csv", row.names=FALSE)

# Match manually
uk_pol_52$speaker[uk_pol_52$wikidataid == "Q695217"] <- "Andrew Love"
uk_pol_52$speaker[uk_pol_52$wikidataid == "Q337556"] <- "Anthony D Wright"
uk_pol_52$speaker[uk_pol_52$wikidataid == "Q4966571"] <- "Chris McCafferty"
uk_pol_52$speaker[uk_pol_52$wikidataid == "Q263882"] <- "Dave Watts"
uk_pol_52$speaker[uk_pol_52$wikidataid == "Q5263565"] <- "Desmond Turner"
uk_pol_52$speaker[uk_pol_52$wikidataid == "Q300292"] <- "Jeffrey M. Donaldson"
uk_pol_52$speaker[uk_pol_52$wikidataid == "Q694695"] <- "John Martin McDonnell"
uk_pol_52$speaker[uk_pol_52$wikidataid == "Q672124"] <- "Jonathan R Shaw"
uk_pol_52$speaker[uk_pol_52$wikidataid == "Q1055918"] <- "Lembit Opik"
uk_pol_52$speaker[uk_pol_52$wikidataid == "Q334603"] <- "Mark Oaten"
uk_pol_52$speaker[uk_pol_52$wikidataid == "Q3412180"] <- "Steve Pound"

# Check the number of still missing data
sum(is.na(uk_pol_52$speaker))


### OUTPUT
## Add ParlSpeech speaker's id to uk from uk_pol by wikidataid
uk$ParlSpeech_speaker <- ifelse(is.na(uk$ParlSpeech_speaker),
                                uk_pol_52$speaker[match(uk$wikidataid, uk_pol_52$wikidataid)],
                                uk$ParlSpeech_speaker)



################################### Session 53 ###################################
### Filter by 53th session
uk_pol_53 <- uk_pol_5056[uk_pol_5056$start_year == 2001,]
ParlSpeech_53 <- hoc.corpus[hoc.corpus$session == "2001-02" |
                              hoc.corpus$session == "2002-03" |
                              hoc.corpus$session == "2003-04" |
                              hoc.corpus$session == "2004-05",]

### NAMES
ParlSpeech_53$name_party <- paste(ParlSpeech_53$speaker, ParlSpeech_53$party, 
                                  sep = " ", collapse = NULL)
### FREQUENCY
## Name
fr <- data.frame(table(as.matrix(uk_pol_53$name_party, useNA = "always")))
uk_pol_53$uniquename_party <- fr$Freq[match(uk_pol_53$name_party, fr$Var1)]
fr_ <- fr[fr$Freq != 1,]
fr_

## Name1
fr <- data.frame(table(as.matrix(uk_pol_53$name1_party, useNA = "always")))
uk_pol_53$uniquename1_party <- fr$Freq[match(uk_pol_53$name1_party, fr$Var1)]
fr1_ <- fr[fr$Freq != 1,]
fr1_

## Name2
fr <- data.frame(table(as.matrix(uk_pol_53$name2_party, useNA = "always")))
uk_pol_53$uniquename2_party <- fr$Freq[match(uk_pol_53$name2_party, fr$Var1)]
fr2_ <- fr[fr$Freq != 1,]
fr2_


### MATCHING
## Create empty columns 
uk_pol_53$speaker <- NA

## Match by name and party
uk_pol_53$speaker <- ifelse(is.na(uk_pol_53$speaker) & uk_pol_53$uniquename_party == 1,
                            ParlSpeech_53$speaker[match(uk_pol_53$name_party, ParlSpeech_53$name_party)],
                            uk_pol_53$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_53$speaker))

## Match by name1 and party
uk_pol_53$speaker <- ifelse(is.na(uk_pol_53$speaker) & uk_pol_53$uniquename1_party == 1,
                            ParlSpeech_53$speaker[match(uk_pol_53$name1_party, ParlSpeech_53$name_party)],
                            uk_pol_53$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_53$speaker))

## Match by name2 and party
uk_pol_53$speaker <- ifelse(is.na(uk_pol_53$speaker) & uk_pol_53$uniquename2_party == 1,
                            ParlSpeech_53$speaker[match(uk_pol_53$name2_party, ParlSpeech_53$name_party)],
                            uk_pol_53$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_53$speaker))


## Match manually (check which existing in ParlSpeech values are not in uk_pol)
# Create csv with unmatched data for manual matching (the code is commented since it is 
# just preparation for manual matching)
# ParlSpeech_53$unname <- uk_pol_53$uniquename_party[match(ParlSpeech_53$name_party,uk_pol_53$name_party)]
# ParlSpeech_53$unname1 <- uk_pol_53$uniquename1_party[match(ParlSpeech_53$name_party,uk_pol_53$name1_party)]
# ParlSpeech_53$unname2 <- uk_pol_53$uniquename2_party[match(ParlSpeech_53$name_party,uk_pol_53$name2_party)]
# ParlSpeech_53$match <- NA
# ParlSpeech_53$match <- ifelse(is.na(ParlSpeech_53$match) & ParlSpeech_53$unname == 1,
#                            "matched", ParlSpeech_53$match)
# ParlSpeech_53$match <- ifelse(is.na(ParlSpeech_53$match) & ParlSpeech_53$unname1 == 1,
#                            "matched", ParlSpeech_53$match)
# ParlSpeech_53$match <- ifelse(is.na(ParlSpeech_53$match) & ParlSpeech_53$unname2 == 1,
#                            "matched", ParlSpeech_53$match)
# unmatched_ParlSpeech_53 <- ParlSpeech_53[is.na(ParlSpeech_53$match),]
# 
# uk_pol_53$speaker <- ifelse(is.na(uk_pol_53$speaker),
#                             uk$ParlSpeech_speaker[match(uk_pol_53$wikidataid, uk$wikidataid)],
#                             uk_pol_53$speaker)
# unmatched_uk_pol_53 <- uk_pol_53[is.na(uk_pol_53$speaker),]
# unmatched_uk_pol_53 <- unmatched_uk_pol_53[, !(colnames(unmatched_uk_pol_53) %in% c("name_split"))]
# write.csv(unmatched_ParlSpeech_53, file = "unmatched_ParlSpeech_53.csv", row.names=FALSE)
# write.csv(unmatched_uk_pol_53, file = "unmatched_uk_pol_53.csv", row.names=FALSE)

# Match manually
uk_pol_53$speaker[uk_pol_53$wikidataid == "Q392461"] <- "Jim Sheridan"
uk_pol_53$speaker[uk_pol_53$wikidataid == "Q261675"] <- "Pete Wishart"
uk_pol_53$speaker[uk_pol_53$wikidataid == "Q7533492"] <- "Si&#244n Simon"

# Check the number of still missing data
sum(is.na(uk_pol_53$speaker))


### OUTPUT
## Add ParlSpeech speaker's id to uk from uk_pol by wikidataid
uk$ParlSpeech_speaker <- ifelse(is.na(uk$ParlSpeech_speaker),
                                uk_pol_53$speaker[match(uk$wikidataid, uk_pol_53$wikidataid)],
                                uk$ParlSpeech_speaker)




################################### Session 54 ###################################
### Filter by 54th session
uk_pol_54 <- uk_pol_5056[uk_pol_5056$start_year == 2005,]
ParlSpeech_54 <- hoc.corpus[hoc.corpus$session == "2005-06" |
                              hoc.corpus$session == "2006-07" |
                              hoc.corpus$session == "2007-08" |
                              hoc.corpus$session == "2008-09" |
                              hoc.corpus$session == "2009-10",]

### NAMES
ParlSpeech_54$name_party <- paste(ParlSpeech_54$speaker, ParlSpeech_54$party, 
                                  sep = " ", collapse = NULL)
### FREQUENCY
## Name
fr <- data.frame(table(as.matrix(uk_pol_54$name_party, useNA = "always")))
uk_pol_54$uniquename_party <- fr$Freq[match(uk_pol_54$name_party, fr$Var1)]
fr_ <- fr[fr$Freq != 1,]
fr_

## Name1
fr <- data.frame(table(as.matrix(uk_pol_54$name1_party, useNA = "always")))
uk_pol_54$uniquename1_party <- fr$Freq[match(uk_pol_54$name1_party, fr$Var1)]
fr1_ <- fr[fr$Freq != 1,]
fr1_

## Name2
fr <- data.frame(table(as.matrix(uk_pol_54$name2_party, useNA = "always")))
uk_pol_54$uniquename2_party <- fr$Freq[match(uk_pol_54$name2_party, fr$Var1)]
fr2_ <- fr[fr$Freq != 1,]
fr2_


### MATCHING
## Create empty columns
uk_pol_54$speaker <- NA

## Match by name and party
uk_pol_54$speaker <- ifelse(is.na(uk_pol_54$speaker) & uk_pol_54$uniquename_party == 1,
                            ParlSpeech_54$speaker[match(uk_pol_54$name_party, ParlSpeech_54$name_party)],
                            uk_pol_54$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_54$speaker))

## Match by name1 and party
uk_pol_54$speaker <- ifelse(is.na(uk_pol_54$speaker) & uk_pol_54$uniquename1_party == 1,
                            ParlSpeech_54$speaker[match(uk_pol_54$name1_party, ParlSpeech_54$name_party)],
                            uk_pol_54$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_54$speaker))

## Match by name2 and party
uk_pol_54$speaker <- ifelse(is.na(uk_pol_54$speaker) & uk_pol_54$uniquename2_party == 1,
                            ParlSpeech_54$speaker[match(uk_pol_54$name2_party, ParlSpeech_54$name_party)],
                            uk_pol_54$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_54$speaker))


## Match manually (check which existing in ParlSpeech values are not in uk_pol)
# Create csv with unmatched data for manual matching (the code is commented since it is 
# just preparation for manual matching)
# ParlSpeech_54$unname <- uk_pol_54$uniquename_party[match(ParlSpeech_54$name_party,uk_pol_54$name_party)]
# ParlSpeech_54$unname1 <- uk_pol_54$uniquename1_party[match(ParlSpeech_54$name_party,uk_pol_54$name1_party)]
# ParlSpeech_54$unname2 <- uk_pol_54$uniquename2_party[match(ParlSpeech_54$name_party,uk_pol_54$name2_party)]
# ParlSpeech_54$match <- NA
# ParlSpeech_54$match <- ifelse(is.na(ParlSpeech_54$match) & ParlSpeech_54$unname == 1,
#                            "matched", ParlSpeech_54$match)
# ParlSpeech_54$match <- ifelse(is.na(ParlSpeech_54$match) & ParlSpeech_54$unname1 == 1,
#                            "matched", ParlSpeech_54$match)
# ParlSpeech_54$match <- ifelse(is.na(ParlSpeech_54$match) & ParlSpeech_54$unname2 == 1,
#                            "matched", ParlSpeech_54$match)
# unmatched_ParlSpeech_54 <- ParlSpeech_54[is.na(ParlSpeech_54$match),]
# uk_pol_54$speaker <- ifelse(is.na(uk_pol_54$speaker),
#                            uk$ParlSpeech_speaker[match(uk_pol_54$wikidataid, uk$wikidataid)],
#                            uk_pol_54$speaker)
# unmatched_uk_pol_54 <- uk_pol_54[is.na(uk_pol_54$speaker),]
# unmatched_uk_pol_54 <- unmatched_uk_pol_54[, !(colnames(unmatched_uk_pol_54) %in% c("name_split"))]
# write.csv(unmatched_ParlSpeech_54, file = "unmatched_ParlSpeech_54.csv", row.names=FALSE)
# write.csv(unmatched_uk_pol_54, file = "unmatched_uk_pol_54.csv", row.names=FALSE)

# Match manually
uk_pol_54$speaker[uk_pol_54$wikidataid == "Q391341"] <- "Andrew Slaughter"
uk_pol_54$speaker[uk_pol_54$wikidataid == "Q332540"] <- "Christopher Huhne"
uk_pol_54$speaker[uk_pol_54$wikidataid == "Q727933"] <- "Diana R. Johnson"
uk_pol_54$speaker[uk_pol_54$wikidataid == "Q260464"] <- "Edward Balls"
uk_pol_54$speaker[uk_pol_54$wikidataid == "Q262335"] <- "Jennifer Willott"
uk_pol_54$speaker[uk_pol_54$wikidataid == "Q390549"] <- "Rob Flello"
uk_pol_54$speaker[uk_pol_54$wikidataid == "Q676383"] <- "Hermon"
uk_pol_54$speaker[uk_pol_54$wikidataid == "Q260049"] <- "William Bain"

# Check the number of still missing data
sum(is.na(uk_pol_54$speaker))


### OUTPUT
## Add ParlSpeech speaker's id to uk from uk_pol by wikidataid
uk$ParlSpeech_speaker <- ifelse(is.na(uk$ParlSpeech_speaker),
                                uk_pol_54$speaker[match(uk$wikidataid, uk_pol_54$wikidataid)],
                                uk$ParlSpeech_speaker)




################################### Session 55 ###################################
### Filter by 55th session
uk_pol_55 <- uk_pol_5056[uk_pol_5056$start_year == 2010,]
ParlSpeech_55 <- hoc.corpus[hoc.corpus$session == "2010-11" |
                              hoc.corpus$session == "2011-12" |
                              hoc.corpus$session == "2012-13" |
                              hoc.corpus$session == "2013-14" |
                              hoc.corpus$session == "2014-15",]

### NAMES
ParlSpeech_55$name_party <- paste(ParlSpeech_55$speaker, ParlSpeech_55$party, 
                                  sep = " ", collapse = NULL)
### FREQUENCY
## Name
fr <- data.frame(table(as.matrix(uk_pol_55$name_party, useNA = "always")))
uk_pol_55$uniquename_party <- fr$Freq[match(uk_pol_55$name_party, fr$Var1)]
fr_ <- fr[fr$Freq != 1,]
fr_

## Name1
fr <- data.frame(table(as.matrix(uk_pol_55$name1_party, useNA = "always")))
uk_pol_55$uniquename1_party <- fr$Freq[match(uk_pol_55$name1_party, fr$Var1)]
fr1_ <- fr[fr$Freq != 1,]
fr1_

## Name2
fr <- data.frame(table(as.matrix(uk_pol_55$name2_party, useNA = "always")))
uk_pol_55$uniquename2_party <- fr$Freq[match(uk_pol_55$name2_party, fr$Var1)]
fr2_ <- fr[fr$Freq != 1,]
fr2_


### MATCHING
## Create empty columns 
uk_pol_55$speaker <- NA

## Match by name and party
uk_pol_55$speaker <- ifelse(is.na(uk_pol_55$speaker) & uk_pol_55$uniquename_party == 1,
                            ParlSpeech_55$speaker[match(uk_pol_55$name_party, ParlSpeech_55$name_party)],
                            uk_pol_55$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_55$speaker))

## Match by name1 and party
uk_pol_55$speaker <- ifelse(is.na(uk_pol_55$speaker) & uk_pol_55$uniquename1_party == 1,
                            ParlSpeech_55$speaker[match(uk_pol_55$name1_party, ParlSpeech_55$name_party)],
                            uk_pol_55$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_55$speaker))

## Match by name2 and party
uk_pol_55$speaker <- ifelse(is.na(uk_pol_55$speaker) & uk_pol_55$uniquename2_party == 1,
                            ParlSpeech_55$speaker[match(uk_pol_55$name2_party, ParlSpeech_55$name_party)],
                            uk_pol_55$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_55$speaker))


## Match manually (check which existing in ParlSpeech values are not in uk_pol)
# Create csv with unmatched data for manual matching (the code is commented since it is 
# just preparation for manual matching)
# ParlSpeech_55$unname <- uk_pol_55$uniquename_party[match(ParlSpeech_55$name_party,uk_pol_55$name_party)]
# ParlSpeech_55$unname1 <- uk_pol_55$uniquename1_party[match(ParlSpeech_55$name_party,uk_pol_55$name1_party)]
# ParlSpeech_55$unname2 <- uk_pol_55$uniquename2_party[match(ParlSpeech_55$name_party,uk_pol_55$name2_party)]
# ParlSpeech_55$match <- NA
# ParlSpeech_55$match <- ifelse(is.na(ParlSpeech_55$match) & ParlSpeech_55$unname == 1,
#                            "matched", ParlSpeech_55$match)
# ParlSpeech_55$match <- ifelse(is.na(ParlSpeech_55$match) & ParlSpeech_55$unname1 == 1,
#                            "matched", ParlSpeech_55$match)
# ParlSpeech_55$match <- ifelse(is.na(ParlSpeech_55$match) & ParlSpeech_55$unname2 == 1,
#                            "matched", ParlSpeech_55$match)
# unmatched_ParlSpeech_55 <- ParlSpeech_55[is.na(ParlSpeech_55$match),]
# uk_pol_55$speaker <- ifelse(is.na(uk_pol_55$speaker),
#                            uk$ParlSpeech_speaker[match(uk_pol_55$wikidataid, uk$wikidataid)],
#                            uk_pol_55$speaker)
# unmatched_uk_pol_55 <- uk_pol_55[is.na(uk_pol_55$speaker),]
# unmatched_uk_pol_55 <- unmatched_uk_pol_55[, !(colnames(unmatched_uk_pol_55) %in% c("name_split"))]
# write.csv(unmatched_ParlSpeech_55, file = "unmatched_ParlSpeech_55.csv", row.names=FALSE)
# write.csv(unmatched_uk_pol_55, file = "unmatched_uk_pol_55.csv", row.names=FALSE)

# Match manually
uk_pol_55$speaker[uk_pol_55$wikidataid == "Q576208"] <- "Chi Onwurah"
uk_pol_55$speaker[uk_pol_55$wikidataid == "Q575239"] <- "Daniel Poulter"
uk_pol_55$speaker[uk_pol_55$wikidataid == "Q575881"] <- "Ian Paisley"
uk_pol_55$speaker[uk_pol_55$wikidataid == "Q291785"] <- "Nicholas Dakin"
uk_pol_55$speaker[uk_pol_55$wikidataid == "Q391328"] <- "Stephen Gilbert"

# Check the number of still missing data
sum(is.na(uk_pol_55$speaker))


### OUTPUT
## Add ParlSpeech speaker's id to uk from uk_pol by wikidataid
uk$ParlSpeech_speaker <- ifelse(is.na(uk$ParlSpeech_speaker),
                                uk_pol_55$speaker[match(uk$wikidataid, uk_pol_55$wikidataid)],
                                uk$ParlSpeech_speaker)




################################### Session 56 ###################################
### Filter by 56th session
uk_pol_56 <- uk_pol_5056[uk_pol_5056$start_year == 2010,]
ParlSpeech_56 <- hoc.corpus[hoc.corpus$session == "2010-11" |
                              hoc.corpus$session == "2011-12" |
                              hoc.corpus$session == "2012-13" |
                              hoc.corpus$session == "2013-14" |
                              hoc.corpus$session == "2014-15",]

### NAMES
ParlSpeech_56$name_party <- paste(ParlSpeech_56$speaker, ParlSpeech_56$party, 
                                  sep = " ", collapse = NULL)
### FREQUENCY
## Name
fr <- data.frame(table(as.matrix(uk_pol_56$name_party, useNA = "always")))
uk_pol_56$uniquename_party <- fr$Freq[match(uk_pol_56$name_party, fr$Var1)]
fr_ <- fr[fr$Freq != 1,]
fr_

## Name1
fr <- data.frame(table(as.matrix(uk_pol_56$name1_party, useNA = "always")))
uk_pol_56$uniquename1_party <- fr$Freq[match(uk_pol_56$name1_party, fr$Var1)]
fr1_ <- fr[fr$Freq != 1,]
fr1_

## Name2
fr <- data.frame(table(as.matrix(uk_pol_56$name2_party, useNA = "always")))
uk_pol_56$uniquename2_party <- fr$Freq[match(uk_pol_56$name2_party, fr$Var1)]
fr2_ <- fr[fr$Freq != 1,]
fr2_


### MATCHING
## Create empty columns 
uk_pol_56$speaker <- NA

## Match by name and party
uk_pol_56$speaker <- ifelse(is.na(uk_pol_56$speaker) & uk_pol_56$uniquename_party == 1,
                            ParlSpeech_56$speaker[match(uk_pol_56$name_party, ParlSpeech_56$name_party)],
                            uk_pol_56$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_56$speaker))

## Match by name1 and party
uk_pol_56$speaker <- ifelse(is.na(uk_pol_56$speaker) & uk_pol_56$uniquename1_party == 1,
                            ParlSpeech_56$speaker[match(uk_pol_56$name1_party, ParlSpeech_56$name_party)],
                            uk_pol_56$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_56$speaker))

## Match by name2 and party
uk_pol_56$speaker <- ifelse(is.na(uk_pol_56$speaker) & uk_pol_56$uniquename2_party == 1,
                            ParlSpeech_56$speaker[match(uk_pol_56$name2_party, ParlSpeech_56$name_party)],
                            uk_pol_56$speaker)
# Check the number of still missing data
sum(is.na(uk_pol_56$speaker))


## Match manually (check which existing in ParlSpeech values are not in uk_pol)
# Create csv with unmatched data for manual matching (the code is commented since it is 
# just preparation for manual matching)
# ParlSpeech_56$unname <- uk_pol_56$uniquename_party[match(ParlSpeech_56$name_party,uk_pol_56$name_party)]
# ParlSpeech_56$unname1 <- uk_pol_56$uniquename1_party[match(ParlSpeech_56$name_party,uk_pol_56$name1_party)]
# ParlSpeech_56$unname2 <- uk_pol_56$uniquename2_party[match(ParlSpeech_56$name_party,uk_pol_56$name2_party)]
# ParlSpeech_56$match <- NA
# ParlSpeech_56$match <- ifelse(is.na(ParlSpeech_56$match) & ParlSpeech_56$unname == 1,
#                            "matched", ParlSpeech_56$match)
# ParlSpeech_56$match <- ifelse(is.na(ParlSpeech_56$match) & ParlSpeech_56$unname1 == 1,
#                            "matched", ParlSpeech_56$match)
# ParlSpeech_56$match <- ifelse(is.na(ParlSpeech_56$match) & ParlSpeech_56$unname2 == 1,
#                            "matched", ParlSpeech_56$match)
# unmatched_ParlSpeech_56 <- ParlSpeech_56[is.na(ParlSpeech_56$match),]
# uk_pol_56$speaker <- ifelse(is.na(uk_pol_56$speaker),
#                            uk$ParlSpeech_speaker[match(uk_pol_56$wikidataid, uk$wikidataid)],
#                            uk_pol_56$speaker)
# unmatched_uk_pol_56 <- uk_pol_56[is.na(uk_pol_56$speaker),]
# unmatched_uk_pol_56 <- unmatched_uk_pol_56[, !(colnames(unmatched_uk_pol_56) %in% c("name_split"))]
# write.csv(unmatched_ParlSpeech_56, file = "unmatched_ParlSpeech_56.csv", row.names=FALSE)
# write.csv(unmatched_uk_pol_56, file = "unmatched_uk_pol_56.csv", row.names=FALSE)

# Match manually
# no matches


### OUTPUT
## Add ParlSpeech speaker's id to uk from uk_pol by wikidataid
uk$ParlSpeech_speaker <- ifelse(is.na(uk$ParlSpeech_speaker),
                                uk_pol_56$speaker[match(uk$wikidataid, uk_pol_56$wikidataid)],
                                uk$ParlSpeech_speaker)



################################### THE OUTPUT DATAFRAME ################################### 
# output
uk_output <- subset(uk, select = c('wikidataid', 'ParlSpeech_speaker'))
View(uk_output)
sum(!is.na(uk_output$ParlSpeech_speaker))
# We found 1542 out of 1994 (according pdf to ParlSpeech data set)
uk_output <- uk_output[!is.na(uk_output$ParlSpeech_speaker),]
saveRDS(uk_output, "./data/pol_sci_data/uk_output")
