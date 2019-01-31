# ---------------------------------------------------------------------------------------
# legislatoR
# Sascha Göbel, Simon Munzert
# Script: Packages
# December 2017
# ---------------------------------------------------------------------------------------


#### INSTALL AND LOAD PACKAGES ==========================================================

# install pacman package if not installed -----------------------------------------------
suppressWarnings(if (!require("pacman")) install.packages("pacman"))

# load packages and install if not installed --------------------------------------------
pacman::p_load(stringr, lubridate, magrittr, plyr, dplyr, eeptools, RSelenium, httr,
               rvest, toOrdinal, mpoly, data.table, zoo, jsonlite, R.utils,
               WikidataR, tibble, pageviews, wikipediatrend, padr, gtools,readxl,
               install = TRUE,
               update = FALSE)

# show loaded packages ------------------------------------------------------------------
cat("loaded packages\n")
print(pacman::p_loaded())
