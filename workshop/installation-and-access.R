# ---------------------------------------------------------------------------------------
# legislatoR: Installation and Data Access
# ---------------------------------------------------------------------------------------



#### INSTALLATION =======================================================================

# install devtools from CRAN and load ---------------------------------------------------
install.packages("devtools")
library(devtools)

# install legislatoR from GitHub and load -----------------------------------------------
devtools::install_github("saschagobel/legislatoR")
library(legislatoR)

# install and load additional packages --------------------------------------------------
install.packages(c("dplyr", "tibble"))
library(dplyr)
library(tibble)



#### BASIC DATA ACCESS ==================================================================

# access and inspect "Core" table for German Bundestag ----------------------------------
core_bt <- get_core(legislature = "deu")
glimpse(core_bt)

# access and inspect "Political" table for German Bundestag -----------------------------
political_bt <- get_political(legislature = "deu")
glimpse(political_bt)

# access and inspect "Offices" table for German Bundestag -------------------------------
office_bt <- get_office(legislature = "deu")
glimpse(office_bt)

# access and inspect "Professions" table for German Bundestag ---------------------------
profession_bt <- get_profession(legislature = "deu")
glimpse(profession_bt)

# access and inspect "Social" table for German Bundestag --------------------------------
social_bt <- get_social(legislature = "deu")
glimpse(social_bt)

# access and inspect "Portrait" table for German Bundestag ------------------------------
portrait_bt <- get_portrait(legislature = "deu")
glimpse(portrait_bt)

# access and inspect "Wikipedia History" table for German Bundestag ---------------------
history_bt <- get_history(legislature = "deu")
glimpse(history_bt)

# access and inspect "Wikipedia Traffic" table for German Bundestag ---------------------
traffic_bt <- get_traffic(legislature = "deu")
glimpse(traffic_bt)

# access and inspect "IDs" table for German Bundestag -----------------------------------
ids_bt <- get_ids(legislature = "deu")
glimpse(ids_bt)


# Further country codes:
# "aut" - Austria
# "can" - Canada
# "cze" - Czech Republic
# "fra" - France
# "irl" - Ireland
# "sco" - Scotland
# "gbr" - United Kingdom
# "usa_house" - United States (House)
# "usa_senate" - United States (Senate)