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

# retrieve basic data from Wikipedia tables ---------------------------------------------

# austrian nationalrat
austria_core <- collectorAustria(source = "./data/htmls/austria/")
saveRDS(austria_core, "./data/austria_core")

# canadian house of commons
canada_core <- collectorCanada(source = "./data/htmls/canada")
saveRDS(canada_core, "./data/canada_core")

# czech poslanecka snemovna
czech_core <- collectorCzech(source = "./data/htmls/czech/")
saveRDS(czech_core, "./data/czech_core")

# french assemble
france_core <- collectorFrance(source = "./data/htmls/france/")
saveRDS(france_core, "./data/france_core")

# german bundestag
germany_core <- collectorGermany(source = "./data/htmls/germany/")
saveRDS(germany_core, "./data/germany_core")

# irish dail
ireland_core <- collectorIreland(source = "./data/htmls/ireland/")
saveRDS(ireland_core, "./data/ireland_core")

# scottish parliament
scotland_core <- collectorScotland(source = "./data/htmls/scotland/")
saveRDS(scotland_core, "./data/scotland_core")

# united kingdom parliament
uk_core <- collectorUk(source = "./data/htmls/uk/")
saveRDS(uk_core, "./data/uk_core")

# united states house
usah_core <- collectorUSA(source = "./data/htmls/usa/", chamber = "house")
saveRDS(usah_core, "./data/usah_core")

# united states senate
usas_core <- collectorUSA(source = "./data/htmls/usa/", chamber = "senate")
saveRDS(usas_core, "./data/usas_core")

# retrieve wikipedia page and wikidata ids ----------------------------------------------

# austrian nationalrat
austria_ids <- lapply(X = austria_core, FUN = wikiIDs, corp = TRUE)
saveRDS(austria_ids, "./data/austria_ids")

# canadian house of commons
canada_ids <- lapply(X = canada_core, FUN = wikiIDs, corp = TRUE)
saveRDS(canada_ids, "./data/canada_ids")

# czech poslanecka snemovna
czech_ids <- lapply(X = czech_core, FUN = wikiIDs, corp = TRUE)
saveRDS(czech_ids, "./data/czech_ids")

# french assemble
france_ids <- lapply(X = france_core, FUN = wikiIDs, corp = TRUE)
saveRDS(france_ids, "./data/france_ids")

# german bundestag
germany_ids <- lapply(X = germany_core, FUN = wikiIDs, corp = TRUE)
saveRDS(germany_ids, "./data/germany_ids")

# irish dail
ireland_ids <- lapply(X = ireland_core, FUN = wikiIDs, corp = TRUE)
saveRDS(ireland_ids, "./data/ireland_ids")

# scottish parliament
scotland_ids <- lapply(X = scotland_core, FUN = wikiIDs, corp = TRUE)
saveRDS(scotland_ids, "./data/scotland_ids")

# united kingdom parliament
uk_ids <- lapply(X = uk_core, FUN = wikiIDs, corp = TRUE)
saveRDS(uk_ids, "./data/uk_ids")

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

# canadian house of commons
canada <- bind_rows(Map(cbind, canada_ids, canada_core))
saveRDS(canada, "./data/canada")

# czech poslanecka snemovna
czech <- bind_rows(Map(cbind, czech_ids, czech_core))
saveRDS(czech, "./data/czech")

# french assemble
france <- bind_rows(Map(cbind, france_ids, france_core))
saveRDS(france, "./data/france")

# german bundestag
germany <- bind_rows(Map(cbind, germany_ids, germany_core))
saveRDS(germany, "./data/germany")

# irish dail
ireland <- bind_rows(Map(cbind, ireland_ids, ireland_core))
saveRDS(ireland, "./data/ireland")

# scottish parliament
scotland <- bind_rows(Map(cbind, scotland_ids, scotland_core))
saveRDS(scotland, "./data/scotland")

# united kingdom parliament
uk <- bind_rows(Map(cbind, uk_ids, uk_core))
saveRDS(uk, "./data/uk")

# united states house
usah <- bind_rows(Map(cbind, usah_ids, usah_core))
saveRDS(usah, "./data/usah")

# united states senate
usas <- bind_rows(Map(cbind, usas_ids, usas_core))
saveRDS(usas, "./data/usas")

# import official data (where applicable) and merge with core and ids -------------------

# german bundestag - religion
# ### the 'religion' field was completely removed in the 2018 edition of the official xml
# ### hence the officially listed religion is only appended for politicians in office in
# ### or prior to the 17th legislative period
mdbs <- read_html("./data/htmls/germany_official/MDB_STAMMDATEN.xml")
last_name <- html_nodes(x = mdbs, xpath = "//mdb//namen") %>%
  html_node(., "nachname") %>%
  html_text()
first_name <- html_nodes(x = mdbs, xpath = "//mdb//namen") %>%
  html_node(., "vorname") %>%
  html_text()
birth_date <- html_nodes(x = mdbs, xpath = "//mdb//geburtsdatum") %>%
  html_text()
religion <- html_nodes(x = mdbs, xpath = "//mdb//religion") %>%
  html_text()
id <- html_nodes(x = mdbs, xpath = "//mdb/id") %>%
  html_text()
mdbs <- data.frame(first_name = first_name, last_name = last_name, religion = religion, id = id, birth = birth_date, 
                   stringsAsFactors = FALSE)
mdbs$name <- str_c(mdbs$first_name, " ", mdbs$last_name)
mdbs <- arrange(mdbs, name)
remainder1 <- germany[which(is.na(match(germany$name, mdbs$name))),]
remainder2 <- mdbs[which(is.na(match(mdbs$name, germany$name))),]
remainder1$last_name <- str_replace(remainder1$name, "^.+ ", "")
remainder1$first_name <- str_replace(remainder1$name, " .+$", "")
remainder1$comb1 <- str_c(remainder1$first_name, remainder1$birth)
remainder1$comb2 <- str_c(remainder1$last_name, remainder1$birth)
remainder2$birth <- dmy(str_replace_all(as.character(remainder2$birth), "\\.", "-"))
remainder2$comb1 <- str_c(remainder2$first_name, remainder2$birth)
remainder2$comb2 <- str_c(remainder2$last_name, remainder2$birth)
remainder2$replace1 <- remainder1$name[match(remainder2$comb1, remainder1$comb1)]
remainder2$replace2 <- remainder1$name[match(remainder2$comb2, remainder1$comb2)]
remainder2$replaced <- ifelse(!is.na(remainder2$replace1), remainder2$replace1, 
                              ifelse(!is.na(remainder2$replace2), remainder2$replace2,
                                     remainder2$name))
mdbs$name[which(mdbs$id %in% remainder2$id)] <- remainder2$replaced
mdbs_name_repl1 <- str_replace(mdbs$name[which(!(mdbs$name %in% germany$name))], " +?", " von ")
mdbs_name_idx1 <- which(!(mdbs$name %in% germany$name))[which(mdbs_name_repl1 %in% germany$name)]          
mdbs$name[mdbs_name_idx1] <- mdbs_name_repl1[which(mdbs_name_repl1 %in% germany$name)]   
mdbs_name_repl2 <- str_replace(mdbs$name[which(!(mdbs$name %in% germany$name))], " .+(?=[A-Z].+$)", " ")
mdbs_name_idx2 <- which(!(mdbs$name %in% germany$name))[which(mdbs_name_repl2 %in% germany$name)]          
mdbs$name[mdbs_name_idx2] <- mdbs_name_repl2[which(mdbs_name_repl2 %in% germany$name)]   
mdbs$name <- mdbs$name %>%
  replace(list = which(!(mdbs$name %in% germany$name)), # germany must include legislators until and including the 18th period
          values = c("Adelheid D. Tröscher", "Adolf Freiherr Spies von Büllesheim", "Agnieszka Brugger", 
                     "Alexander Graf Lambsdorff", "Alois Graf von Waldburg-Zeil", "Anke Martiny-Glotz",
                     "Armin-Paul Hampel", "Axel de Vries", "Bernard Povel",
                     "Bertold Mathias Reinartz", "Botho Prinz zu Sayn-Wittgenstein-Hohenstein", 
                     "Carl-Detlev Freiherr von Hammerstein", "Christian Haase", "Christoph de Vries",
                     "Claire Marienfeld", "Cornelia von Teichman und Logischen", "Dietrich Rollmann",
                     "Dorothea Marx", "Dora Lösche", "Dorothée Menzner",
                     "Egon Wilhelm Ramms", "Eike Götz", "Elfriede Klemmert",
                     "Elimar Freiherr von Fürstenberg", "Elke Leonhard", "Ernst Paul Dörfler",
                     "Volker Schemmel", "Ernst Ulrich von Weizsäcker", "Eugen Fürst zu Oettingen-Wallerstein",
                     "Eugen von der Wiesche", "Eva Bulling-Schröter", "Eva Schreiber",
                     "Eva Gräfin Finck von Finckenstein", "Sibylle Engel", "Frank Schmidt",
                     "Franz Peter Basten", "Franz Ludwig Schenk Graf von Stauffenberg", "Fritz Mensing",
                     "Fritz Henßler", "Gabi Molitor", "Georg Graf Henckel von Donnersmarck",
                     "Georg Baron Manteuffel-Szoege", "Gerd Bauer", "Trude Unruh",
                     "Gila Altmann", "Günter Schlichting-von Rönn", "Günter Henle",
                     "Gustav Freiherr von Gemmingen-Hornberg", "Hakki Keskin", "Halo Saibold",
                     "Hans-Adolf de Terra", "Hans Egon Engell", "Hans-Georg von der Marwitz",
                     "Hajo Hoffmann", "Hans Albrecht Freiherr von Rechenberg", "Hans Böhm",
                     "Hans Graf Huyn", "Hans Paul Hermann Schuster", "Hans With",
                     "Heidi Lippmann", "Heinrich Graf von Einsiedel", "Heinrich Leonhard Kolb",
                     "Heinrich Stooß", "Heinz Dieter Eßmann", "Heinz-Dieter Hackel",
                     "Evrim Sommer", "Horst Ludwig Riemer", "Hubertus Prinz zu Löwenstein-Wertheim-Freudenberg",
                     "Inge Höger", "Volrad Deneke", "Jan van Aken",
                     "Joachim Graf von Schönburg-Glauchau", "Jochen van Aerssen", "Johann Segitz",
                     "Diether Dehm", "Jörg van Essen", "Joseph-Ernst Fugger von Glött",
                     "Joseph Baumgartner", "Joschka Fischer", "Julia Obermeier",
                     "Karl-Theodor zu Guttenberg", "Fred Zander", "Karl-Heinz Schröter",
                     "Karl Graf von Spreti", "Karl-Theodor zu Guttenberg", "Katharina Willkomm",
                     "Katrin Göring-Eckardt", "Kees de Vries", "Klaus-Dieter Uelhoff",
                     "Klaus Freiherr von Mühlen", "Klaus-Werner Jonas", "Konstantin Prinz von Bayern",
                     "Lothar de Maizière", "Ludolf von Wartenberg", "Ludwig Bergsträsser",
                     "Margareta Wolf", "Mariana Harder-Kühnel", "Marlis Gräfin vom Hagen",
                     "Matern von Marschall", "Maximilian Lehmer", "Max Freiherr Riederer von Paar",
                     "Memet Kiliç", "Michael Georg Link", "Michaela Engelmeier-Heite",
                     "Michaela Noll", "Milan Horácek", "Nadine Schön",
                     "Olaf in der Beek", "Olaf Baron von Wrangel", "Otto Fürst von Bismarck",
                     "Otto Freiherr von Feury", "Otto Freiherr von Fircks", "Otto Graf Lambsdorff",
                     "Peter Harry Carstensen", "Peter von der Heydt Freiherr von Massenbach", "Petra Bläss",
                     "Philipp Graf von und zu Lerchenfeld", "Martin Schmidt", "Werner Schuster",
                     "Rembert van Delden", "Richard Tamblé", "Robert Jaffé",
                     "Ronja Kemmer", "Rudi Adams", "Thomas de Maizière",
                     "Ulrike Höfken", "Uschi Eid", "Ursula von der Leyen",
                     "Uwe Lühr", "Alexander Menne", "Walter Franz Altherr",
                     "Wilderich Freiherr Ostman von der Leye", "Wilhelm Fischer", "Wilhelm Knothe",
                     "Wolfgang Neškovic"))
mdbs$religion <- ifelse(mdbs$religion == "katholisch" | mdbs$religion == "alt-katholisch", "catholicism",
                        ifelse(mdbs$religion == "römisch-katholisch", "catholicism",
                               ifelse(mdbs$religion == "evangelisch", "protestantism evangelical",
                                      ifelse(mdbs$religion == "evangelisch-lutherisch", "protestantism lutheran",
                                             ifelse(mdbs$religion == "evangelisch-reformiert" | 
                                                      mdbs$religion == "evangelisch-freikirchlich", "protestantism evangelical",
                                                    ifelse(mdbs$religion == "konfessionslos" | mdbs$religion == "Atheistin"|
                                                             mdbs$religion == "religionslos" | mdbs$religion == "Atheist" |
                                                             mdbs$religion == "freireligiös", "atheism",
                                                           ifelse(mdbs$religion == "protestantisch", "protestantism",
                                                                  ifelse(mdbs$religion == "muslimisch"| mdbs$religion == "Islam" |
                                                                           mdbs$religion == "alevitisch", "islam",
                                                                         ifelse(mdbs$religion == "griechisch-orthodox" | 
                                                                                  mdbs$religion == "russisch-orthodox", "orthodox eastern",
                                                                                ifelse(mdbs$religion == "orthodox", "orthodox",
                                                                                ifelse(mdbs$religion == "neuapostolisch", "protestantism apostolic",
                                                                                       NA)
                                                                         ))))))))))

# retrieve wikipedia revision histories -------------------------------------------------

# austrian nationalrat
austria_history <- bind_rows(lapply(X = unique(austria$pageid), FUN = wikiHist,
                                    project = "de.wikipedia"))
saveRDS(austria_history, "./data/austria_history")

# canadian house of commons
canada_history <- bind_rows(lapply(X = unique(canada$pageid), FUN = wikiHist,
                                    project = "en.wikipedia"))
saveRDS(canada_history, "./data/canada_history")

# czech poslanecka snemovna
czech_history <- bind_rows(lapply(X = unique(czech$pageid), FUN = wikiHist,
                                   project = "cz.wikipedia"))
saveRDS(czech_history, "./data/czech_history")

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

# scottish parliament
scotland_history <- bind_rows(lapply(X = unique(scotland$pageid), FUN = wikiHist,
                                    project = "en.wikipedia"))
saveRDS(scotland_history, "./data/scotland_history")

# united kingdom parliament
uk_history <- bind_rows(lapply(X = unique(uk$pageid), FUN = wikiHist,
                                     project = "en.wikipedia"))
saveRDS(uk_history, "./data/uk_history")

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

# canadian house of commons
canada_title <- undirectedTitle(pageid = unique(canada$pageid),
                                 project = "en.wikipedia")
saveRDS(canada_title, "./data/canada_title")

# czech poslanecka snemovna
czech_title <- undirectedTitle(pageid = unique(czech$pageid),
                                project = "cz.wikipedia")
saveRDS(czech_title, "./data/czech_title")

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

# scottish parliament
scotland_title <- undirectedTitle(pageid = unique(scotland$pageid),
                                 project = "en.wikipedia")
saveRDS(scotland_title, "./data/scotland_title")

# united kingdom parliament
uk_title <- undirectedTitle(pageid = unique(uk$pageid),
                                  project = "en.wikipedia")
saveRDS(uk_title, "./data/uk_title")

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

# canadian house of commons
canada_traffic <- wikiTraffic(data = canada_title, project = "en.wikipedia")
saveRDS(canada_traffic, "./data/canada_traffic")

# czech poslanecka snemovna
czech_traffic <- wikiTraffic(data = czech_title, project = "cs.wikipedia")
saveRDS(czech_traffic, "./data/czech_traffic")

# french assemble
france_title <- readRDS("./data/france_title")
france_traffic <- wikiTraffic(data = france_title, project = "fr.wikipedia")
saveRDS(france_traffic, "./data/france_traffic")

# german bundestag
germany_title <- readRDS("./data/germany_title")
germany_traffic <- wikiTraffic(data = germany_title, project = "de.wikipedia")
saveRDS(germany_traffic, "./data/germany_traffic")

# irish dail
ireland_title <- readRDS("./data/ireland_title")
ireland_traffic <- wikiTraffic(data = ireland_title, project = "en.wikipedia")
saveRDS(ireland_traffic, "./data/ireland_traffic")

# scottish parliament
scotland_title <- readRDS("./data/scotland_title")
scotland_traffic <- wikiTraffic(data = scotland_title, project = "en.wikipedia")
saveRDS(scotland_traffic, "./data/scotland_traffic")

# uk parliament
uk_title <- readRDS("./data/uk_title")
uk_traffic <- wikiTraffic(data = uk_title, project = "en.wikipedia")
saveRDS(uk_traffic, "./data/uk_traffic")

# united states house
usah_title <- readRDS("./data/usah_title")
usah_traffic <- wikiTraffic(data = usah_title, project = "en.wikipedia")
saveRDS(usah_traffic, "./data/usah_traffic")

# united states senate
usas_title <- readRDS("./data/usas_title")
usas_traffic <- wikiTraffic(data = usas_title, project = "en.wikipedia")
saveRDS(usas_traffic, "./data/usas_traffic")

# retrieve Wikidata entities ------------------------------------------------------------

# austrian nationalrat
austria_entities <- get_item(id = unique(austria$wikidataid))
saveRDS(austria_entities, "./data/austria_entities")

# canadian house of commons
canada_entities <- get_item(id = unique(canada$wikidataid))
saveRDS(canada_entities, "./data/canada_entities")

# czech poslanecka snemovna
czech_entities <- get_item(id = unique(czech$wikidataid))
saveRDS(czech_entities, "./data/czech_entities")

# french assemble
france_entities <- get_item(id = unique(france$wikidataid))
saveRDS(france_entities, "./data/france_entities")

# german bundestag
germany_entities <- get_item(id = unique(germany$wikidataid))
saveRDS(germany_entities, "./data/germany_entities")

# irish dail
ireland_entities <- get_item(id = unique(ireland$wikidataid))
saveRDS(ireland_entities, "./data/ireland_entities")

# scottish parliament
scotland_entities <- get_item(id = unique(scotland$wikidataid))
saveRDS(scotland_entities, "./data/scotland_entities")

# united kingdom parliament
uk_entities <- get_item(id = unique(uk$wikidataid))
saveRDS(uk_entities, "./data/uk_entities")

# united states house
usah_entities <- get_item(id = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))])
saveRDS(usah_entities, "./data/usah_entities")

# united states senate
usas_entities <- get_item(id = unique(usas$wikidataid))
saveRDS(usas_entities, "./data/usas_entities")

# retrieve sex --------------------------------------------------------------------------

# austrian nationalrat
austria_sex <- wikiData(item = unique(austria$wikidataid), entity = austria_entities, 
                        property = "P21")
austria_sex$sex <- ifelse(austria_sex$male == TRUE, "male",
                          ifelse(austria_sex$female == TRUE, "female", NA))
austria_sex <- austria_sex[,-c(2,3)]
austria_sex$sex <- factor(austria_sex$sex)
saveRDS(austria_sex, "./data/austria_sex")

# canadian house of commons
canada_sex <- wikiData(item = unique(canada$wikidataid), entity = canada_entities, 
                       property = "P21")
canada_sex$sex <- ifelse(canada_sex$male == TRUE, "male",
                          ifelse(canada_sex$female == TRUE, "female", NA))
canada_sex <- canada_sex[,-c(2,3)]
canada_sex$sex <- factor(canada_sex$sex)
saveRDS(canada_sex, "./data/canada_sex")

# czech poslanecka snemovna
czech_sex <- wikiData(item = unique(czech$wikidataid), entity = czech_entities, 
                      property = "P21")
czech_sex$sex <- ifelse(czech_sex$male == TRUE, "male",
                         ifelse(czech_sex$female == TRUE, "female", NA))
czech_sex <- czech_sex[,-c(2,3)]
czech_sex$sex <- factor(czech_sex$sex)
saveRDS(czech_sex, "./data/czech_sex")

# french assemble
france_sex <- wikiData(item = unique(france$wikidataid), entity = france_entities, 
                       property = "P21")
france_sex$sex <- ifelse(france_sex$male == TRUE, "male",
                         ifelse(france_sex$female == TRUE, "female", NA))
france_sex <- france_sex[,-c(2,3)]
france_sex$sex <- factor(france_sex$sex)
saveRDS(france_sex, "./data/france_sex")

# german bundestag
germany_sex <- wikiData(item = unique(germany$wikidataid), entity = germany_entities, 
                        property = "P21")
germany_sex$sex <- ifelse(germany_sex$male == TRUE, "male",
                          ifelse(germany_sex$female == TRUE, "female", NA))
germany_sex <- germany_sex[,-c(2,3)]
germany_sex$sex <- factor(germany_sex$sex)
saveRDS(germany_sex, "./data/germany_sex")

# irish dail
ireland_sex <- wikiData(item = unique(ireland$wikidataid), entity = ireland_entities, 
                        property = "P21")
ireland_sex$sex <- ifelse(ireland_sex$male == TRUE, "male",
                          ifelse(ireland_sex$female == TRUE, "female", NA))
ireland_sex <- ireland_sex[,-c(2,3)]
ireland_sex$sex <- factor(ireland_sex$sex)
saveRDS(ireland_sex, "./data/ireland_sex")

# scottish parliament
scotland_sex <- wikiData(item = unique(scotland$wikidataid), entity = scotland_entities, 
                         property = "P21")
scotland_sex$sex <- ifelse(scotland_sex$male == TRUE, "male",
                          ifelse(scotland_sex$female == TRUE, "female", NA))
scotland_sex <- scotland_sex[,-c(2,3)]
scotland_sex$sex <- factor(scotland_sex$sex)
saveRDS(scotland_sex, "./data/scotland_sex")

# united kingdom parliament
uk_sex <- wikiData(item = unique(uk$wikidataid), entity = uk_entities, 
                   property = "P21")
uk_sex$sex <- ifelse(uk_sex$male == TRUE, "male",
                          ifelse(uk_sex$female == TRUE, "female", NA))
uk_sex <- uk_sex[,-c(2,3)]
uk_sex$sex <- factor(uk_sex$sex)
saveRDS(uk_sex, "./data/uk_sex")

# united states house
usah_sex <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], entity = usah_entities, 
                     property = "P21")
usah_sex$sex <- ifelse(usah_sex$male == TRUE, "male",
                       ifelse(usah_sex$female == TRUE, "female", NA))
usah_sex <- usah_sex[,-c(2,3)]
usah_sex$sex <- factor(usah_sex$sex)
saveRDS(usah_sex, "./data/usah_sex")

# united states senate
usas_sex <- wikiData(item = unique(usas$wikidataid), entity = usas_entities, 
                     property = "P21")
usas_sex$sex <- ifelse(usas_sex$male == TRUE, "male",
                          ifelse(usas_sex$female == TRUE, "female", NA))
usas_sex <- usas_sex[,-c(2,3)]
usas_sex$sex <- factor(usas_sex$sex)
saveRDS(usas_sex, "./data/usas_sex")

# retrieve religion ---------------------------------------------------------------------

# austrian nationalrat
austria_religion <- wikiData(item = unique(austria$wikidataid), 
                             entity = austria_entities, property = "P140")
austria_religion$religion <- replace(NA, rowSums(austria_religion[,c(3, 6, 9)]) >= 1,
                                    "catholicism") %>%
  replace(., rowSums(austria_religion[,c(8)]) >= 1,
          "protestantism lutheran") %>%
  replace(., rowSums(austria_religion[,c(5)]) >= 1,
          "islam") %>%
  replace(., rowSums(austria_religion[,c(4)]) >= 1,
          "agnosticism") %>%
  replace(., rowSums(austria_religion[,c(2,7)]) >= 1,
          "atheism")
austria_religion <- austria_religion[,c(1, 10)] %>%
  filter(!is.na(religion))
austria_religion$religion <- factor(austria_religion$religion)
saveRDS(austria_religion, "./data/austria_religion")

# canadian house of commons
canada_religion <- wikiData(item = unique(canada$wikidataid), 
                            entity = canada_entities, property = "P140")
canada_religion$religion <- replace(NA, rowSums(canada_religion[,c(6,7,18,20)]) >= 1,
                                     "catholicism") %>%
  replace(., rowSums(canada_religion[,c(9)]) >= 1,
          "protestantism evangelical") %>%
  replace(., rowSums(canada_religion[,c(14)]) >= 1,
          "protestantism anabaptism") %>%
  replace(., rowSums(canada_religion[,c(11)]) >= 1,
          "protestantism lutheran") %>%
  replace(., rowSums(canada_religion[,c(17)]) >= 1,
          "protestantism baptist") %>%
  replace(., rowSums(canada_religion[,c(3)]) >= 1,
          "orthodox eastern") %>%
  replace(., rowSums(canada_religion[,c(4,19)]) >= 1,
          "protestantism reformed") %>%
  replace(., rowSums(canada_religion[,c(2,25)]) >= 1,
          "protestantism methodist") %>%
  replace(., rowSums(canada_religion[,c(22,23,24)]) >= 1,
          "protestantism") %>%
  replace(., rowSums(canada_religion[,c(8,10,21)]) >= 1,
          "protestantism anglicanism") %>%
  replace(., rowSums(canada_religion[,c(13)]) >= 1,
          "protestantism unitarian") %>%
  replace(., rowSums(canada_religion[,c(12)]) >= 1,
          "hindu") %>%
  replace(., rowSums(canada_religion[,c(16)]) >= 1,
          "sikhism") %>%
  replace(., rowSums(canada_religion[,c(5)]) >= 1,
          "islam") %>%
  replace(., rowSums(canada_religion[,c(15)]) >= 1,
          "judaism")
canada_religion <- canada_religion[,c(1, 26)] %>%
  filter(!is.na(religion))
canada_religion$religion <- factor(canada_religion$religion)
saveRDS(canada_religion, "./data/canada_religion")

# czech poslanecka snemovna
czech_religion <- wikiData(item = unique(czech$wikidataid), 
                          entity = czech_entities, property = "P140")
czech_religion$religion <- replace(NA, rowSums(czech_religion[,c(2,7)]) >= 1,
                                    "catholicism") %>%
  replace(., rowSums(czech_religion[,c(5)]) >= 1,
          "atheism") %>%
  replace(., rowSums(czech_religion[,c(6)]) >= 1,
          "protestantism evangelical") %>%
  replace(., rowSums(czech_religion[,c(4)]) >= 1,
          "protestantism") %>%
  replace(., rowSums(czech_religion[,c(3)]) >= 1,
          "protestantism reformed")
czech_religion <- czech_religion[,c(1, 8)] %>%
  filter(!is.na(religion))
czech_religion$religion <- factor(czech_religion$religion)
saveRDS(czech_religion, "./data/czech_religion")

# french assemble
france_religion <- wikiData(item = unique(france$wikidataid), entity = france_entities,
                            property = "P140")
france_religion$religion <- replace(NA, rowSums(france_religion[,c(3,7,8,10)]) >= 1,
                                    "catholicism") %>%
  replace(., rowSums(france_religion[,c(2)]) >= 1,
          "protestantism reformed") %>%
  replace(., rowSums(france_religion[,c(5,6)]) >= 1,
          "islam") %>%
  replace(., rowSums(france_religion[,c(4)]) >= 1,
          "agnosticism") %>%
  replace(., rowSums(france_religion[,c(9)]) >= 1,
        "judaism")
france_religion <- france_religion[,c(1, 11)]
france_religion$religion <- factor(france_religion$religion)
saveRDS(france_religion, "./data/france_religion")

# germany bundestag
germany_religion <- wikiData(item = unique(germany$wikidataid), 
                             entity = germany_entities, property = "P140")
germany_religion$religion <- replace(NA, rowSums(germany_religion[,c(3,12,15,20)]) >= 1,
                                    "catholicism") %>%
  replace(., rowSums(germany_religion[,c(19)]) >= 1,
          "protestantism reformed") %>%
  replace(., rowSums(germany_religion[,c(9,11,4)]) >= 1,
          "protestantism lutheran") %>%
  replace(., rowSums(germany_religion[,c(2,13,14,17)]) >= 1,
          "protestantism evangelical") %>%
  replace(., rowSums(germany_religion[,c(16)]) >= 1,
          "protestantism") %>%
  replace(., rowSums(germany_religion[,c(6)]) >= 1,
          "orthodox eastern") %>%
  replace(., rowSums(germany_religion[,c(18)]) >= 1,
          "islam") %>%
  replace(., rowSums(germany_religion[,c(8)]) >= 1,
          "buddhism") %>%
  replace(., rowSums(germany_religion[,c(7,10,5)]) >= 1,
          "atheism")
germany_religion <- germany_religion[,c(1,21)] %>%
  filter(!is.na(religion))
germany_religion <- full_join(x = germany_religion, y = data.frame(wikidataid = germany$wikidataid[match(mdbs$name,germany$name)], 
                                               religion_2 = mdbs$religion),
          by = "wikidataid")
germany_religion$religion_2 <- as.character(germany_religion$religion_2)
germany_religion$religion <- ifelse(is.na(germany_religion$religion), germany_religion$religion_2, germany_religion$religion)
germany_religion <- germany_religion[,-3]
germany_religion <- germany_religion[!is.na(germany_religion$religion),]
germany_religion$religion <- factor(germany_religion$religion)
saveRDS(germany_religion, "./data/germany_religion")

# irish dail
ireland_religion <- wikiData(item = unique(ireland$wikidataid), 
                             entity = ireland_entities, property = "P140")
ireland_religion$religion <- replace(NA, rowSums(ireland_religion[,c(3, 8)]) >= 1,
                                     "catholicism") %>%
  replace(., rowSums(ireland_religion[,c(4)]) >= 1,
          "islam") %>%
  replace(., rowSums(ireland_religion[,c(2, 5, 6)]) >= 1,
          "protestant reformed") %>%
  replace(., rowSums(ireland_religion[,c(7)]) >= 1,
          "anglicanism")
ireland_religion <- ireland_religion[,c(1, 9)]
ireland_religion$religion <- factor(ireland_religion$religion)
saveRDS(ireland_religion, "./data/ireland_religion")

# scottish parliament
scotland_religion <- wikiData(item = unique(scotland$wikidataid), 
                              entity = scotland_entities, property = "P140")
scotland_religion$religion <- replace(NA, rowSums(scotland_religion[,c(4,8)]) >= 1,
                                     "catholicism") %>%
  replace(., rowSums(scotland_religion[,c(5)]) >= 1,
          "islam") %>%
  replace(., rowSums(scotland_religion[,c(3,6)]) >= 1,
          "protestant reformed") %>%
  replace(., rowSums(scotland_religion[,c(7)]) >= 1,
          "protestantism baptist") %>%
  replace(., rowSums(scotland_religion[,c(2)]) >= 1,
          "anglicanism")
scotland_religion <- scotland_religion[,c(1, 9)]
scotland_religion$religion <- factor(scotland_religion$religion)
saveRDS(scotland_religion, "./data/scotland_religion")

# united kingdom parliament
uk_religion <- wikiData(item = unique(uk$wikidataid), 
                        entity = uk_entities, property = "P140")
uk_religion$religion <- replace(NA, rowSums(uk_religion[,c(4,16,20)]) >= 1,
                                      "catholicism") %>%
  replace(., rowSums(uk_religion[,c(2,13,17,7,12,19)]) >= 1,
          "protestantism reformed") %>%
  replace(., rowSums(uk_religion[,c(3)]) >= 1,
          "protestantism evangelical") %>%
  replace(., rowSums(uk_religion[,c(23,24)]) >= 1,
          "islam") %>%
  replace(., rowSums(uk_religion[,c(6,10)]) >= 1,
          "anglicanism") %>%
  replace(., rowSums(uk_religion[,c(5,8,21)]) >= 1,
          "atheism") %>%
  replace(., rowSums(uk_religion[,c(9)]) >= 1,
          "buddhism") %>%
  replace(., rowSums(uk_religion[,c(11)]) >= 1,
          "hindu") %>%
  replace(., rowSums(uk_religion[,c(15)]) >= 1,
          "sikhism") %>%
  replace(., rowSums(uk_religion[,c(14)]) >= 1,
          "judaism") %>%
replace(., rowSums(uk_religion[,c(18)]) >= 1,
        "protestantism quaker") %>%
  replace(., rowSums(uk_religion[,c(22)]) >= 1,
          "protestantism methodist")
uk_religion <- uk_religion[,c(1, 25)]
uk_religion$religion <- factor(uk_religion$religion)
saveRDS(uk_religion, "./data/uk_religion")

# united states house
usah_religion <- wikiData(item = unique(usah$wikidataid), entity = usah_entities,
                             property = "P140")
usah_religion$religion <- replace(NA, rowSums(usah_religion[,c(11,33,42,61)]) >= 1,
                                     "catholicism") %>%
  replace(., rowSums(usah_religion[,c(7,14,23,31,32,35,37,38,48)]) >= 1,
          "protestantism evangelical") %>%
  replace(., rowSums(usah_religion[,c(5)]) >= 1,
          "protestantism proto") %>%
  replace(., rowSums(usah_religion[,c(8)]) >= 1,
          "protestantism quaker") %>%
  replace(., rowSums(usah_religion[,c(9)]) >= 1,
          "protestantism anabaptism") %>%
  replace(., rowSums(usah_religion[,c(45,51)]) >= 1,
          "protestantism lutheran") %>%
  replace(., rowSums(usah_religion[,c(3,60)]) >= 1,
          "protestantism baptist") %>%
  replace(., rowSums(usah_religion[,c(12,17,52)]) >= 1,
          "orthodox eastern") %>%
  replace(., rowSums(usah_religion[,c(21)]) >= 1,
          "orthodox") %>%
  replace(., rowSums(usah_religion[,c(10,15,22,24,36,53,62)]) >= 1,
          "protestantism reformed") %>%
  replace(., rowSums(usah_religion[,c(4,13,27,34,49,54,65)]) >= 1,
          "protestantism restorationism") %>%
  replace(., rowSums(usah_religion[,c(19,20,25,26,64)]) >= 1,
          "protestantism methodist") %>%
  replace(., rowSums(usah_religion[,c(16,28,43)]) >= 1,
          "protestantism") %>%
  replace(., rowSums(usah_religion[,c(6,46,56)]) >= 1,
          "protestantism unitarian") %>%
  replace(., rowSums(usah_religion[,c(40)]) >= 1,
          "protestantism christian science") %>%
  replace(., rowSums(usah_religion[,c(18,41,44)]) >= 1,
          "anglicanism") %>%
  replace(., rowSums(usah_religion[,c(2,58,63)]) >= 1,
          "hindu") %>%
  replace(., rowSums(usah_religion[,c(29,30)]) >= 1,
          "islam") %>%
  replace(., rowSums(usah_religion[,c(50)]) >= 1,
          "buddhism") %>%
  replace(., rowSums(usah_religion[,c(47,55)]) >= 1,
          "atheism") %>%
  replace(., rowSums(usah_religion[,c(57,59)]) >= 1,
          "judaism") %>%
  replace(., rowSums(usah_religion[,c(39)]) >= 1,
          "soka gakkai")
usah_religion <- usah_religion[,c(1, 66)]
usah_religion$religion <- factor(usah_religion$religion)
saveRDS(usah_religion, "./data/usah_religion")

# united states senate
usas_religion <- wikiData(item = unique(usas$wikidataid), entity = usas_entities,
                          property = "P140")
usas_religion$religion <-  replace(NA, rowSums(usas_religion[,c(7,25,43)]) >= 1,
                                   "catholicism") %>%
  replace(., rowSums(usas_religion[,c(4,8,20,21,22)]) >= 1,
          "protestantism evangelical") %>%
  replace(., rowSums(usas_religion[,c(5)]) >= 1,
          "protestantism quaker") %>%
  replace(., rowSums(usas_religion[,c(30,33,36,48)]) >= 1,
          "protestantism lutheran") %>%
  replace(., rowSums(usas_religion[,c(42,47)]) >= 1,
          "protestantism baptist") %>%
  replace(., rowSums(usas_religion[,c(14)]) >= 1,
          "orthodox eastern") %>%
  replace(., rowSums(usas_religion[,c(12)]) >= 1,
          "orthodox") %>%
  replace(., rowSums(usas_religion[,c(6,13,14,17,19,23,26,44,45,15)]) >= 1,
          "protestantism reformed") %>%
  replace(., rowSums(usas_religion[,c(2,18,32,34,37,46,49)]) >= 1,
          "protestantism restorationism") %>%
  replace(., rowSums(usas_religion[,c(10,11,16)]) >= 1,
          "protestantism methodist") %>%
  replace(., rowSums(usas_religion[,c(3,24,40)]) >= 1,
          "protestantism unitarian") %>%
  replace(., rowSums(usas_religion[,c(9,28)]) >= 1,
          "protestantism") %>%
  replace(., rowSums(usas_religion[,c(27,29)]) >= 1,
          "anglicanism") %>%
  replace(., rowSums(usas_religion[,c(31,38,39)]) >= 1,
          "atheism") %>%
  replace(., rowSums(usas_religion[,c(35,41)]) >= 1,
          "judaism")
usas_religion <- usas_religion[,c(1, 50)]
usas_religion$religion <- factor(usas_religion$religion)
saveRDS(usas_religion, "./data/usas_religion")

# retrieve and format birth dates -------------------------------------------------------

# austrian nationalrat
austria_birth <- wikiData(item = unique(austria$wikidataid), entity = austria_entities, 
                          date = TRUE, property = "P569")
austria_birth$date <- austria_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(austria_birth, "./data/austria_birth")

# canadian house of commons
canada_birth <- wikiData(item = unique(canada$wikidataid), entity = canada_entities, 
                         date = TRUE, property = "P569")
canada_birth$date <- canada_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(canada_birth, "./data/canada_birth")

# czech poslanecka snemovna
czech_birth <- wikiData(item = unique(czech$wikidataid), entity = czech_entities, 
                        date = TRUE, property = "P569")
czech_birth$date <- czech_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(czech_birth, "./data/czech_birth")

# french assemble
france_birth <- wikiData(item = unique(france$wikidataid), entity = france_entities, 
                         date = TRUE, property = "P569")
france_birth$date <- france_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(france_birth, "./data/france_birth")

# german bundestag
germany_birth <- wikiData(item = unique(germany$wikidataid), entity = germany_entities, 
                          date = TRUE, property = "P569")
germany_birth$date <- germany_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(germany_birth, "./data/germany_birth")

# irish dail
ireland_birth <- wikiData(item = unique(ireland$wikidataid), entity = ireland_entities, 
                          date = TRUE, property = "P569")
ireland_birth$date <- ireland_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(ireland_birth, "./data/ireland_birth")

# scottish parliament
scotland_birth <- wikiData(item = unique(scotland$wikidataid), 
                           entity = scotland_entities, date = TRUE, property = "P569")
scotland_birth$date <- scotland_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(scotland_birth, "./data/scotland_birth")

# united kingdom parliament
uk_birth <- wikiData(item = unique(uk$wikidataid), entity = uk_entities, date = TRUE,
                           property = "P569")
uk_birth$date <- uk_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(uk_birth, "./data/uk_birth")

# united states house
usah_birth <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], entity = usah_entities, 
                       date = TRUE, property = "P569")
usah_birth$date <- usah_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(usah_birth, "./data/usah_birth")

# united states senate
usas_birth <- wikiData(item = unique(usas$wikidataid), entity = usas_entities, 
                       date = TRUE, property = "P569")
usas_birth$date <- usas_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(usas_birth, "./data/usas_birth")

# retrieve and format death dates -------------------------------------------------------

# austrian nationalrat
austria_death <- wikiData(item = unique(austria$wikidataid), entity = austria_entities, 
                          date = TRUE, property = "P570")
austria_death$date <- austria_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(austria_death, "./data/austria_death")

# canadian house of commons
canada_death <- wikiData(item = unique(canada$wikidataid), entity = canada_entities, 
                          date = TRUE, property = "P570")
canada_death$date <- canada_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(canada_death, "./data/canada_death")

# czech poslanecka snemovna
czech_death <- wikiData(item = unique(czech$wikidataid), entity = czech_entities, 
                          date = TRUE, property = "P570")
czech_death$date <- czech_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(czech_death, "./data/czech_death")

# french assemble
france_death <- wikiData(item = unique(france$wikidataid), entity = france_entities, 
                         date = TRUE, property = "P570")
france_death$date <- france_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(france_death, "./data/france_death")

# german bundestag
germany_death <- wikiData(item = unique(germany$wikidataid), entity = germany_entities, 
                          date = TRUE, property = "P570")
germany_death$date <- germany_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(germany_death, "./data/germany_death")

# irish dail
ireland_death <- wikiData(item = unique(ireland$wikidataid), entity = ireland_entities, 
                          date = TRUE, property = "P570")
ireland_death$date <- ireland_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(ireland_death, "./data/ireland_death")

# scottish parliament
scotland_death <- wikiData(item = unique(scotland$wikidataid), 
                           entity = scotland_entities, date = TRUE, property = "P570")
scotland_death$date <- scotland_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(scotland_death, "./data/scotland_death")

# united kingdom parliament
uk_death <- wikiData(item = unique(uk$wikidataid), entity = uk_entities, date = TRUE, 
                     property = "P570")
uk_death$date <- uk_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(uk_death, "./data/uk_death")

# united states house
usah_death <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                       date = TRUE, entity = usah_entities, 
                       property = "P570")
usah_death$date <- usah_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(usah_death, "./data/usah_death")

# united states senate
usas_death <- wikiData(item = unique(usas$wikidataid), date = TRUE, 
                       entity = usas_entities, property = "P570")
usas_death$date <- usas_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(usas_death, "./data/usas_death")

# retrieve and format birth places ------------------------------------------------------

# austrian nationalrat
austria_birthplace <- wikiData(item = unique(austria$wikidataid), 
                               entity = austria_entities, location = TRUE, 
                               property = "P19")
austria_birthplace$lat <- round(austria_birthplace$lat, digit = 5)
austria_birthplace$lon <- round(austria_birthplace$lon, digit = 5)
austria_birthplace$birthplace <- str_c(austria_birthplace$lat, ",",
                                       austria_birthplace$lon)
austria_birthplace <- austria_birthplace[,c(1,4)]
saveRDS(austria_birthplace, "./data/austria_birthplace")

# canadian house of commons
canada_birthplace <- wikiData(item = unique(canada$wikidataid), 
                               entity = canada_entities, location = TRUE, 
                               property = "P19")
canada_birthplace$lat <- round(canada_birthplace$lat, digit = 5)
canada_birthplace$lon <- round(canada_birthplace$lon, digit = 5)
canada_birthplace$birthplace <- str_c(canada_birthplace$lat, ",",
                                       canada_birthplace$lon)
canada_birthplace <- canada_birthplace[,c(1,4)]
saveRDS(canada_birthplace, "./data/canada_birthplace")

# czech poslanecka snemovna
czech_birthplace <- wikiData(item = unique(czech$wikidataid), 
                              entity = czech_entities, location = TRUE, 
                              property = "P19")
czech_birthplace$lat <- round(czech_birthplace$lat, digit = 5)
czech_birthplace$lon <- round(czech_birthplace$lon, digit = 5)
czech_birthplace$birthplace <- str_c(czech_birthplace$lat, ",",
                                     czech_birthplace$lon)
czech_birthplace <- czech_birthplace[,c(1,4)]
saveRDS(czech_birthplace, "./data/czech_birthplace")

# french assemble
france_birthplace <- wikiData(item = unique(france$wikidataid), 
                              entity = france_entities, location = TRUE,
                              property = "P19")
france_birthplace$lat <- round(france_birthplace$lat, digit = 5)
france_birthplace$lon <- round(france_birthplace$lon, digit = 5)
france_birthplace$birthplace <- str_c(france_birthplace$lat, ",",
                                      france_birthplace$lon)
france_birthplace <- france_birthplace[,c(1,4)]
saveRDS(france_birthplace, "./data/france_birthplace")

# german bundestag
germany_birthplace <- wikiData(item = unique(germany$wikidataid), 
                               entity = germany_entities, location = TRUE,
                               property = "P19")
germany_birthplace$lat <- round(germany_birthplace$lat, digit = 5)
germany_birthplace$lon <- round(germany_birthplace$lon, digit = 5)
germany_birthplace$birthplace <- str_c(germany_birthplace$lat, ",",
                                       germany_birthplace$lon)
germany_birthplace <- germany_birthplace[,c(1,4)]
saveRDS(germany_birthplace, "./data/germany_birthplace")

# irish dail
ireland_birthplace <- wikiData(item = unique(ireland$wikidataid), 
                               entity = ireland_entities, location = TRUE,
                               property = "P19")
ireland_birthplace$lat <- round(ireland_birthplace$lat, digit = 5)
ireland_birthplace$lon <- round(ireland_birthplace$lon, digit = 5)
ireland_birthplace$birthplace <- str_c(ireland_birthplace$lat, ",",
                                       ireland_birthplace$lon)
ireland_birthplace <- ireland_birthplace[,c(1,4)]
saveRDS(ireland_birthplace, "./data/ireland_birthplace")

# scottish parliament
scotland_birthplace <- wikiData(item = unique(scotland$wikidataid), 
                                entity = scotland_entities, location = TRUE,
                                property = "P19")
scotland_birthplace$lat <- round(scotland_birthplace$lat, digit = 5)
scotland_birthplace$lon <- round(scotland_birthplace$lon, digit = 5)
scotland_birthplace$birthplace <- str_c(scotland_birthplace$lat, ",",
                                        scotland_birthplace$lon)
scotland_birthplace <- scotland_birthplace[,c(1,4)]
saveRDS(scotland_birthplace, "./data/scotland_birthplace")

# united kingdom parliament
uk_birthplace <- wikiData(item = unique(uk$wikidataid), entity = uk_entities, 
                          location = TRUE, property = "P19")
uk_birthplace$lat <- round(uk_birthplace$lat, digit = 5)
uk_birthplace$lon <- round(uk_birthplace$lon, digit = 5)
uk_birthplace$birthplace <- str_c(uk_birthplace$lat, ",",
                                        uk_birthplace$lon)
uk_birthplace <- uk_birthplace[,c(1,4)]
saveRDS(uk_birthplace, "./data/uk_birthplace")

# united states house
usah_birthplace <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                            entity = usah_entities, location = TRUE, property = "P19")
usah_birthplace$lat <- round(usah_birthplace$lat, digit = 5)
usah_birthplace$lon <- round(usah_birthplace$lon, digit = 5)
usah_birthplace$birthplace <- str_c(usah_birthplace$lat, ",", usah_birthplace$lon)
usah_birthplace <- usah_birthplace[,c(1,4)]
saveRDS(usah_birthplace, "./data/usah_birthplace")

# united states senate
usas_birthplace <- wikiData(item = unique(usas$wikidataid), entity = usas_entities, 
                            location = TRUE, property = "P19")
usas_birthplace$lat <- round(usas_birthplace$lat, digit = 5)
usas_birthplace$lon <- round(usas_birthplace$lon, digit = 5)
usas_birthplace$birthplace <- str_c(usas_birthplace$lat, ",", usas_birthplace$lon)
usas_birthplace <- usas_birthplace[,c(1,4)]
saveRDS(usas_birthplace, "./data/usas_birthplace")

# retrieve and format death places ------------------------------------------------------

# austrian nationalrat
austria_deathplace <- wikiData(item = unique(austria$wikidataid), 
                               entity = austria_entities, location = TRUE,
                               property = "P20")
austria_deathplace$lat <- round(austria_deathplace$lat, digit = 5)
austria_deathplace$lon <- round(austria_deathplace$lon, digit = 5)
austria_deathplace$deathplace <- str_c(austria_deathplace$lat, ",",
                                       austria_deathplace$lon)
austria_deathplace <- austria_deathplace[,c(1,4)]
saveRDS(austria_deathplace, "./data/austria_deathplace")

# canadian house of commons
canada_deathplace <- wikiData(item = unique(canada$wikidataid), 
                               entity = canada_entities, location = TRUE,
                               property = "P20")
canada_deathplace$lat <- round(canada_deathplace$lat, digit = 5)
canada_deathplace$lon <- round(canada_deathplace$lon, digit = 5)
canada_deathplace$deathplace <- str_c(canada_deathplace$lat, ",",
                                       canada_deathplace$lon)
canada_deathplace <- canada_deathplace[,c(1,4)]
saveRDS(canada_deathplace, "./data/canada_deathplace")

# czech poslanecka snemovna
czech_deathplace <- wikiData(item = unique(czech$wikidataid), 
                              entity = czech_entities, location = TRUE,
                              property = "P20")
czech_deathplace$lat <- round(czech_deathplace$lat, digit = 5)
czech_deathplace$lon <- round(czech_deathplace$lon, digit = 5)
czech_deathplace$deathplace <- str_c(czech_deathplace$lat, ",",
                                     czech_deathplace$lon)
czech_deathplace <- czech_deathplace[,c(1,4)]
saveRDS(czech_deathplace, "./data/czech_deathplace")

# french assemble
france_deathplace <- wikiData(item = unique(france$wikidataid), 
                              entity = france_entities, location = TRUE,
                              property = "P20")
france_deathplace$lat <- round(france_deathplace$lat, digit = 5)
france_deathplace$lon <- round(france_deathplace$lon, digit = 5)
france_deathplace$deathplace <- str_c(france_deathplace$lat, ",",
                                      france_deathplace$lon)
france_deathplace <- france_deathplace[,c(1,4)]
saveRDS(france_deathplace, "./data/france_deathplace")

# german bundestag
germany_deathplace <- wikiData(item = unique(germany$wikidataid), 
                               entity = germany_entities, location = TRUE,
                               property = "P20")
germany_deathplace$lat <- round(germany_deathplace$lat, digit = 5)
germany_deathplace$lon <- round(germany_deathplace$lon, digit = 5)
germany_deathplace$deathplace <- str_c(germany_deathplace$lat, ",",
                                       germany_deathplace$lon)
germany_deathplace <- germany_deathplace[,c(1,4)]
saveRDS(germany_deathplace, "./data/germany_deathplace")

# irish dail
ireland_deathplace <- wikiData(item = unique(ireland$wikidataid), 
                               entity = ireland_entities, location = TRUE,
                               property = "P20")
ireland_deathplace$lat <- round(ireland_deathplace$lat, digit = 5)
ireland_deathplace$lon <- round(ireland_deathplace$lon, digit = 5)
ireland_deathplace$deathplace <- str_c(ireland_deathplace$lat, ",",
                                       ireland_deathplace$lon)
ireland_deathplace <- ireland_deathplace[,c(1,4)]
saveRDS(ireland_deathplace, "./data/ireland_deathplace")

# scottish parliament
scotland_deathplace <- wikiData(item = unique(scotland$wikidataid), 
                               entity = scotland_entities, location = TRUE,
                               property = "P20")
scotland_deathplace$lat <- round(scotland_deathplace$lat, digit = 5)
scotland_deathplace$lon <- round(scotland_deathplace$lon, digit = 5)
scotland_deathplace$deathplace <- str_c(scotland_deathplace$lat, ",",
                                        scotland_deathplace$lon)
scotland_deathplace <- scotland_deathplace[,c(1,4)]
saveRDS(scotland_deathplace, "./data/scotland_deathplace")

# united kingdom parliament
uk_deathplace <- wikiData(item = unique(uk$wikidataid), 
                                entity = uk_entities, location = TRUE,
                                property = "P20")
uk_deathplace$lat <- round(uk_deathplace$lat, digit = 5)
uk_deathplace$lon <- round(uk_deathplace$lon, digit = 5)
uk_deathplace$deathplace <- str_c(uk_deathplace$lat, ",",
                                  uk_deathplace$lon)
uk_deathplace <- uk_deathplace[,c(1,4)]
saveRDS(uk_deathplace, "./data/uk_deathplace")

# united states house
usah_deathplace <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                            entity = usah_entities,
                            location = TRUE, property = "P20")
usah_deathplace$lat <- round(usah_deathplace$lat, digit = 5)
usah_deathplace$lon <- round(usah_deathplace$lon, digit = 5)
usah_deathplace$deathplace <- str_c(usah_deathplace$lat, ",", usah_deathplace$lon)
usah_deathplace <- usah_deathplace[,c(1,4)]
saveRDS(usah_deathplace, "./data/usah_deathplace")

# united states senate
usas_deathplace <- wikiData(item = unique(usas$wikidataid), entity = usas_entities, 
                            location = TRUE, property = "P20")
usas_deathplace$lat <- round(usas_deathplace$lat, digit = 5)
usas_deathplace$lon <- round(usas_deathplace$lon, digit = 5)
usas_deathplace$deathplace <- str_c(usas_deathplace$lat, ",", usas_deathplace$lon)
usas_deathplace <- usas_deathplace[,c(1,4)]
saveRDS(usas_deathplace, "./data/usas_deathplace")

# retrieve and format social media data -------------------------------------------------

# austrian nationalrat
austria_twitter <- wikiData(item = unique(austria$wikidataid), 
                            entity = austria_entities, property = "P2002",
                            serial = TRUE)
austria_instagram <- wikiData(item = unique(austria$wikidataid), 
                              entity = austria_entities, property = "P2003",
                              serial = TRUE)
austria_instagram$value <- str_replace(austria_instagram$value, "2016", "")
austria_facebook <- wikiData(item = unique(austria$wikidataid), 
                             entity = austria_entities, property = "P2013",
                             serial = TRUE)
austria_youtube <- wikiData(item = unique(austria$wikidataid), 
                            entity = austria_entities, property = "P2397",
                            serial = TRUE)
austria_googlep <- wikiData(item = unique(austria$wikidataid), 
                            entity = austria_entities, property = "P2847", 
                            serial = TRUE)
austria_website <- wikiData(item = unique(austria$wikidataid), 
                            entity = austria_entities, property = "P856",
                            serial = TRUE)
austria_social <- full_join(austria_twitter, austria_facebook, by = "wikidataid") %>%
  full_join(., austria_youtube, by = "wikidataid") %>%
  full_join(., austria_instagram, by = "wikidataid") %>%
  full_join(., austria_website, by = "wikidataid") %>%
  full_join(., austria_googlep, by = "wikidataid")
names(austria_social) <- c("wikidataid", "twitter", "facebook", "youtube", "instagram",
                           "website", "googlep")
saveRDS(austria_social, "./data/austria_social")

# canadian house of commons
canada_twitter <- wikiData(item = unique(canada$wikidataid), 
                           entity = canada_entities, property = "P2002",
                           serial = TRUE)
canada_instagram <- wikiData(item = unique(canada$wikidataid), 
                              entity = canada_entities, property = "P2003",
                              serial = TRUE)
canada_facebook <- wikiData(item = unique(canada$wikidataid), 
                             entity = canada_entities, property = "P2013",
                             serial = TRUE)
canada_youtube <- wikiData(item = unique(canada$wikidataid), 
                            entity = canada_entities, property = "P2397",
                            serial = TRUE)
canada_googlep <- wikiData(item = unique(canada$wikidataid), 
                            entity = canada_entities, property = "P2847", 
                            serial = TRUE)
canada_website <- wikiData(item = unique(canada$wikidataid), 
                            entity = canada_entities, property = "P856",
                            serial = TRUE)
canada_social <- full_join(canada_twitter, canada_facebook, by = "wikidataid") %>%
  full_join(., canada_youtube, by = "wikidataid") %>%
  full_join(., canada_instagram, by = "wikidataid") %>%
  full_join(., canada_website, by = "wikidataid") %>%
  full_join(., canada_googlep, by = "wikidataid")
names(canada_social) <- c("wikidataid", "twitter", "facebook", "youtube", "instagram",
                           "website", "googlep")
saveRDS(canada_social, "./data/canada_social")

# czech poslanecka snemovna
czech_twitter <- wikiData(item = unique(czech$wikidataid), 
                          entity = czech_entities, property = "P2002",
                          serial = TRUE)
#czech_instagram <- wikiData(item = unique(czech$wikidataid), 
#                             entity = czech_entities, property = "P2003",
#                             serial = TRUE)
czech_facebook <- wikiData(item = unique(czech$wikidataid), 
                            entity = czech_entities, property = "P2013",
                            serial = TRUE)
#czech_youtube <- wikiData(item = unique(czech$wikidataid), 
#                           entity = czech_entities, property = "P2397",
#                           serial = TRUE)
#czech_googlep <- wikiData(item = unique(czech$wikidataid), 
#                           entity = czech_entities, property = "P2847", 
#                           serial = TRUE)
czech_linkedin <- wikiData(item = unique(czech$wikidataid), 
                           entity = czech_entities, property = "P2035", 
                           serial = TRUE)
czech_website <- wikiData(item = unique(czech$wikidataid), 
                           entity = czech_entities, property = "P856",
                           serial = TRUE)
czech_social <- full_join(czech_twitter, czech_facebook, by = "wikidataid") %>%
#  full_join(., czech_youtube, by = "wikidataid") %>%
#  full_join(., czech_instagram, by = "wikidataid") %>%
  full_join(., czech_website, by = "wikidataid") %>%
#  full_join(., czech_googlep, by = "wikidataid")
  full_join(., czech_linkedin, by = "wikidataid")
names(czech_social) <- c("wikidataid", "twitter", "facebook", "website", "linkedin")
saveRDS(czech_social, "./data/czech_social")

# french assemble
france_twitter <- wikiData(item = unique(france$wikidataid), 
                           entity = france_entities, property = "P2002", 
                           serial = TRUE)
france_facebook <- wikiData(item = unique(france$wikidataid), 
                            entity = france_entities, property = "P2013", 
                            serial = TRUE)
france_youtube <- wikiData(item = unique(france$wikidataid), 
                           entity = france_entities, property = "P2397", 
                           serial = TRUE)
france_instagram <- wikiData(item = unique(france$wikidataid), 
                             entity = france_entities, property = "P2003", 
                             serial = TRUE)
france_linkedin <- wikiData(item = unique(france$wikidataid), 
                            entity = france_entities, property = "P2035", 
                            serial = TRUE)
france_googlep <- wikiData(item = unique(france$wikidataid), 
                           entity = france_entities, property = "P2847", 
                           serial = TRUE)
france_website <- wikiData(item = unique(france$wikidataid), 
                           entity = france_entities, property = "P856", 
                           serial = TRUE)
france_social <- full_join(x = france_twitter, y = france_facebook, by = "wikidataid") %>%
  full_join(x = ., y = france_youtube, by = "wikidataid") %>%
  full_join(x = ., y = france_instagram, by = "wikidataid") %>%
  full_join(x = ., y = france_linkedin, by = "wikidataid") %>%
  full_join(x = ., y = france_googlep, by = "wikidataid") %>%
  full_join(x = ., y = france_website, by = "wikidataid")
names(france_social) <- c("wikidataid", "twitter", "facebook", "youtube", "instagram",
                      "linkedin", "googlep", "website")
saveRDS(france_social, "./data/france_social")

# german bundestag
germany_twitter <- wikiData(item = unique(germany$wikidataid), 
                            entity = germany_entities,
                            property = "P2002", serial = TRUE)
germany_facebook <- wikiData(item = unique(germany$wikidataid), 
                             entity = germany_entities,
                             property = "P2013", serial = TRUE)
germany_youtube <- wikiData(item = unique(germany$wikidataid), 
                            entity = germany_entities,
                            property = "P2397", serial = TRUE)
germany_googlep <- wikiData(item = unique(germany$wikidataid), 
                            entity = germany_entities,
                            property = "P2847", serial = TRUE)
germany_instagram <- wikiData(item = unique(germany$wikidataid), 
                              entity = germany_entities,
                              property = "P2003", serial = TRUE)
germany_linkedin <- wikiData(item = unique(germany$wikidataid), 
                             entity = germany_entities,
                             property = "P2035", serial = TRUE)
germany_website <- wikiData(item = unique(germany$wikidataid), 
                            entity = germany_entities,
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
ireland_twitter <- wikiData(item = unique(ireland$wikidataid), 
                            entity = ireland_entities,
                            property = "P2002", serial = TRUE)
ireland_facebook <- wikiData(item = unique(ireland$wikidataid), 
                             entity = ireland_entities,
                             property = "P2013", serial = TRUE)
#ireland_youtube <- wikiData(item = unique(ireland$wikidataid), 
#                            entity = ireland_entities,
#                            property = "P2397", serial = TRUE)
#ireland_googlep <- wikiData(item = unique(ireland$wikidataid), 
#                            entity = ireland_entities,
#                            property = "P2847", serial = TRUE)
#ireland_instagram <- wikiData(item = unique(ireland$wikidataid), 
#                              entity = ireland_entities,
#                              property = "P2003", serial = TRUE)
#ireland_linkedin <- wikiData(item = unique(ireland$wikidataid), 
#                             entity = ireland_entities,
#                             property = "P2035", serial = TRUE)
ireland_website <- wikiData(item = unique(ireland$wikidataid), 
                            entity = ireland_entities,
                            property = "P856", serial = TRUE)
ireland_social <- full_join(x = ireland_twitter, y = ireland_facebook, by = "wikidataid") %>%
  full_join(x = ., y = ireland_website, by = "wikidataid")
names(ireland_social) <- c("wikidataid", "twitter", "facebook", "website")
saveRDS(ireland_social, "./data/ireland_social")

# scottish parliament
scotland_twitter <- wikiData(item = unique(scotland$wikidataid), 
                          entity = scotland_entities, property = "P2002",
                          serial = TRUE)
#scotland_instagram <- wikiData(item = unique(scotland$wikidataid), 
#                            entity = scotland_entities, property = "P2003",
#                            serial = TRUE)
scotland_facebook <- wikiData(item = unique(scotland$wikidataid), 
                           entity = scotland_entities, property = "P2013",
                           serial = TRUE)
#scotland_linkedin <- wikiData(item = unique(scotland$wikidataid), 
#                              entity = scotland_entities, property = "P2035",
#                              serial = TRUE)
#scotland_youtube <- wikiData(item = unique(scotland$wikidataid), 
#                          entity = scotland_entities, property = "P2397",
#                          serial = TRUE)
#scotland_googlep <- wikiData(item = unique(scotland$wikidataid), 
#                          entity = scotland_entities, property = "P2847", 
#                          serial = TRUE)
scotland_website <- wikiData(item = unique(scotland$wikidataid), 
                          entity = scotland_entities, property = "P856",
                          serial = TRUE)
scotland_social <- full_join(scotland_twitter, scotland_facebook, by = "wikidataid") %>%
#  full_join(., scotland_youtube, by = "wikidataid") %>%
#  full_join(., scotland_instagram, by = "wikidataid") %>%
  full_join(., scotland_website, by = "wikidataid")
#  full_join(., scotland_googlep, by = "wikidataid")
names(scotland_social) <- c("wikidataid", "twitter", "facebook", "website")
saveRDS(scotland_social, "./data/scotland_social")

# united kingdom parliament
uk_twitter <- wikiData(item = unique(uk$wikidataid), 
                             entity = uk_entities, property = "P2002",
                             serial = TRUE)
uk_instagram <- wikiData(item = unique(uk$wikidataid), 
                               entity = uk_entities, property = "P2003",
                               serial = TRUE)
uk_facebook <- wikiData(item = unique(uk$wikidataid), 
                              entity = uk_entities, property = "P2013",
                              serial = TRUE)
uk_youtube <- wikiData(item = unique(uk$wikidataid), 
                             entity = uk_entities, property = "P2397",
                             serial = TRUE)
uk_linkedin <- wikiData(item = unique(uk$wikidataid), 
                        entity = uk_entities, property = "P2035", 
                        serial = TRUE)
uk_googlep <- wikiData(item = unique(uk$wikidataid), 
                             entity = uk_entities, property = "P2847", 
                             serial = TRUE)
uk_website <- wikiData(item = unique(uk$wikidataid), 
                             entity = uk_entities, property = "P856",
                             serial = TRUE)
uk_social <- full_join(uk_twitter, uk_facebook, by = "wikidataid") %>%
  full_join(., uk_youtube, by = "wikidataid") %>%
  full_join(., uk_linkedin, by = "wikidataid") %>%
  full_join(., uk_instagram, by = "wikidataid") %>%
  full_join(., uk_website, by = "wikidataid") %>%
  full_join(., uk_googlep, by = "wikidataid")
names(uk_social) <- c("wikidataid", "twitter", "facebook", "youtube", "linkedin", 
                      "instagram", "website", "googlep")
saveRDS(uk_social, "./data/uk_social")

# united states house
usah_twitter <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                         entity = usah_entities,
                         property = "P2002", serial = TRUE)
usah_facebook <- wikiData(item =  unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                          entity = usah_entities,
                          property = "P2013", serial = TRUE)
usah_youtube <- wikiData(item =  unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                         entity = usah_entities,
                         property = "P2397", serial = TRUE)
usah_googlep <- wikiData(item =  unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                         entity = usah_entities,
                            property = "P2847", serial = TRUE)
usah_instagram <- wikiData(item =  unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                           entity = usah_entities,
                           property = "P2003", serial = TRUE)
usah_linkedin <- wikiData(item =  unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                          entity = usah_entities,
                          property = "P2035", serial = TRUE)
usah_website <- wikiData(item =  unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                         entity = usah_entities,
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
usas_twitter <- wikiData(item = unique(usas$wikidataid), 
                         entity = usas_entities,
                         property = "P2002", serial = TRUE)
usas_facebook <- wikiData(item = unique(usas$wikidataid), 
                          entity = usas_entities,
                          property = "P2013", serial = TRUE)
usas_youtube <- wikiData(item = unique(usas$wikidataid), 
                         entity = usas_entities,
                         property = "P2397", serial = TRUE)
usas_googlep <- wikiData(item = unique(usas$wikidataid), 
                         entity = usas_entities,
                         property = "P2847", serial = TRUE)
usas_instagram <- wikiData(item = unique(usas$wikidataid), 
                           entity = usas_entities,
                           property = "P2003", serial = TRUE)
usas_linkedin <- wikiData(item = unique(usas$wikidataid), 
                          entity = usas_entities,
                          property = "P2035", serial = TRUE)
usas_website <- wikiData(item = unique(usas$wikidataid), 
                         entity = usas_entities,
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
auth <- authFacepp(api_key = "cdg976ZVSpX0soMLSGXzB8m1-T5CP-yB",
                   api_secret = "13qnSJ1w3xoOlqiLuzt3hw9Lwr3YOPVI")

# austrian nationalrat
austria_images <- imageUrl(pageid = unique(austria$pageid), project = "de.wikipedia")
austria_faces <- faceEst(data = austria_images, auth = auth)
austria_faces <- left_join(austria_faces, austria_images, by = "pageid")
austria_faces$ethnicity <- factor(austria_faces$ethnicity)
saveRDS(austria_faces, "./data/austria_faces")

# canadian house of commons
canada_images <- imageUrl(pageid = unique(canada$pageid), project = "en.wikipedia")
canada_faces <- faceEst(data = canada_images, auth = auth)
canada_faces <- left_join(canada_faces,canada_images, by = "pageid")
white <- c(5981,517118,4048461,6881930,5880203,350064,366637,4822418,5254718,6182806,
           3389901,10481740,14227282,9901917,2011723,10888595,4940571,10195551,3494655,
           21506058,21527349,416972,16315162,19662879,5246882,4510484,14561395,21749712,
           21598314,21652173,5409579,19843478,4521561,5005931,12570739,13237708,21900891,
           21857888,15124093,14811906,2597441,14416445,12439208,8603178,19849500,
           21546537,14488521,11654151,13872310,9185341,6143142,14811628,14207881,
           13205654,2317748,3204306,13353450,13502002,6396689,12555504,13987147,6219837,
           18867923,538756,2182348,15087517,3222236,10048701,12777514,9995998,6539514,
           15007916,23998492,520793,1028825,23024637,2228037,13422751,1097519,1459136,
           394346,383856,544558,3072846,543685,6921711,3651681,479215,1974048,488037,
           479100,428687,321333,238104,477245,383853,440981,1993991,1345356,762985,
           1415770,1482981,48303984,174454,3867018,3837052,42082626,4302105,19789695,
           19925529,31674576,31676854,31678434,48304513,31677984,48298501,48298169,
           2410103,6809945,2850632,2352178,3095633,3549235,12238907,5235455,1696800,
           387479,8030855,18577270,6018709,2615373,21758141,3162207,14890996,1996572,
           21633345,21871506,6127491,15731101,18308945,13443948,17019367,21919275,
           15040248,1720639,14762513,5583206,19240615,12266439,884292,12835928,13400064,
           13429483,22276185,13290370,22976186,12577205,13075320,12857120,13079577,
           13290780,13297577,13291346,13184270,2805025,12854202,12880127,6249354,
           9448097,881638,3156045,23469041,23771212,9419054,6429401,517036,6571597,
           23661779,19659565,17971620,23443339,18294252,12671974,6583088,23621928,
           7209573,16015316,75626,1123065,55610012,20608608,48304363,25026199,31679900,
           2134116,31680477,1042952,1110393,31676144,1984887,692732,48308620,2612299,
           3423964,6391151,19790606,48310343,48303886,374579,2565249,18664,15220561,
           12480672,5880402,10542660,23555657,11552763,23417457,12908920,11337802,
           22945067,520818,23428522,10368168,13339541,14621645,778507,11983357,
           3391689,48304293,298032,899061,1676565,2539711,540613,31668905,31674405,
           31682073,31668751,31678434,31681842,7707790,3481063,41194855)
asian <- c(1160505,747952,48298448,19192345,31536121,48304996,48303823,48298681, 
           48298079,1026459,439312,1910160,19361927,969744,1925289,3418160,16290296,
           31662280,43186413,31666758,11985198,1415656,13318872)
black <- c(48304576,48298524,912635,477835,31678287)
arab <- c(48303810,48304871,385673)
hispanic <- c(31673987)
native <- c(31668081,414870)
remove_image <- c(190456,8340262,1717559,5254718,6182806,13976437,17321138,24141445,
                  1036179,12821703,13422751,1097519,543247,6921711,1459824,226074,
                  161072,3713104,123462,45146623,48304240,36885523)
canada_faces$ethnicity[which(canada_faces$pageid %in% white)] <- "white"
canada_faces$ethnicity[which(canada_faces$pageid %in% asian)] <- "asian"
canada_faces$ethnicity[which(canada_faces$pageid %in% black)] <- "black"
canada_faces$ethnicity[which(canada_faces$pageid %in% arab)] <- "arab"
canada_faces$ethnicity[which(canada_faces$pageid %in% hispanic)] <- "hispanic"
canada_faces$ethnicity[which(canada_faces$pageid %in% native)] <- "native"
canada_faces <- canada_faces[-which(canada_faces$pageid %in% remove_image),]
canada_faces$ethnicity <- canada_faces$ethnicity %>% tolower
canada_faces$ethnicity[which(canada_faces$ethnicity == "india")] <- "asian"
saveRDS(canada_faces, "./data/canada_faces")

# czech poslanecka snemovna
czech_images <- imageUrl(pageid = unique(czech$pageid), project = "cs.wikipedia")
czech_faces <- faceEst(data = czech_images, auth = auth)
czech_faces <- left_join(czech_faces, czech_images, by = "pageid")
czech_faces$ethnicity <- "white"
remove_image <- c(105983,848041,27721,570694,568056,932888,1335737)
czech_faces <- czech_faces[-which(czech_faces$pageid %in% remove_image),]
saveRDS(czech_faces, "./data/czech_faces")

# french assemble
france_images <- imageUrl(pageid = unique(france$pageid), project = "fr.wikipedia")
france_images <- france_images[!str_detect(france_images$image_url,
                                           "https://upload.wikimedia.org/wikipedia/commons/thumb/8/85/Defaut.svg/50px-Defaut.svg.png"), ]
france_faces <- faceEst(data = france_images, auth = auth)
france_faces <- left_join(france_faces, france_images, by = "pageid")
france_faces$ethnicity <- factor(france_faces$ethnicity)
saveRDS(france_faces, "./data/france_faces")

# german bundestag
germany_images <- imageUrl(pageid = unique(germany$pageid), project = "de.wikipedia")
germany_faces <- faceEst(data = germany_images, auth = auth)
germany_faces <- left_join(germany_faces, germany_images, by = "pageid")
germany_faces$ethnicity <- factor(germany_faces$ethnicity)
saveRDS(germany_faces, "./data/germany_faces")

# irish dail
ireland_images <- imageUrl(pageid = unique(ireland$pageid), project = "en.wikipedia")
ireland_faces <- faceEst(data = ireland_images, auth = auth)
ireland_faces <- left_join(ireland_faces, ireland_images, by = "pageid")
saveRDS(ireland_faces, "./data/ireland_faces")

# scottish parliament
scotland_images <- imageUrl(pageid = unique(scotland$pageid), project = "en.wikipedia")
scotland_faces <- faceEst(data = scotland_images, auth = auth)
scotland_faces <- left_join(scotland_faces, scotland_images, by = "pageid")
white <- c(1365557,1520683,4117726,4127895,358155,1808142,11416906,31692442,
           31776575,50442511)
asian <- c(31697583,31697554,19139923)
remove_image <- c(523544,1577736,4127895,11052705)
scotland_faces$ethnicity[which(scotland_faces$pageid %in% white)] <- "white"
scotland_faces$ethnicity[which(scotland_faces$pageid %in% asian)] <- "asian"
scotland_faces$image_url[which(scotland_faces$pageid %in% remove_image)] <- NA
scotland_faces$ethnicity <- scotland_faces$ethnicity %>% tolower
saveRDS(scotland_faces, "./data/scotland_faces")

# united kingdom parliament
uk_images <- imageUrl(pageid = unique(uk$pageid), project = "en.wikipedia")
uk_faces <- faceEst(data = uk_images, auth = auth)
uk_faces <- left_join(uk_faces, uk_images, by = "pageid")
white <- c(153641,592121,5379963,414612,414632,434611,1873640,27264484,1371430,26363008,
           26119658,57318769,3338644,528488,353642,601370,17658242,2056358,7379374,
           7317072,4346083,287825,3312313,654386,812314,594003,961816,1294919,3636707,
           435653,176026,177585,5892379,6040618,723808,356907,359553,523511,430859,
           3558478,450370,550767,359355,413944,26598741,352623,204498,414678,354520,
           8065844,723744,416956,415641,723769,417484,415091,449464,413358,419398,420600,
           435622,450506,415139,414704,404502,1114223,416923,883609,1872014,1872049,
           1872804,1867577,1873707,27318945,27332025,46642381,46644326,46643848,46648167,
           46646901,9939550,46214034,54258178,1876802)
asian <- c(354040,417462,434736,420184,735450,1287136,1873069,1847198,19360784,27286521,
           27316155,19139923,27265339,27282341,5603691,34070960,46644039,46642705,37796971,
           46643872,42592035,46622934,46642575,50835552,54258069,53956138,5289586,54257250,
           54259537)
black <- c(434634,27334232,46646833,46651792)
remove_images <- c(57318769,12724452,13611045,4770371,22177965,2056358,3875845,22119193,
                   1393347,612403,961816,11895936,2231981,2080109,8353754,435653,6769021,
                   4672743,472378,3655061,5892379,8424853,5961783,5380735,186797,9192644,
                   3687222,523544,413126,5692886,19592864,5379963,414612,415091,414469,
                   417518,449373,419405,1855077,1876802,17750010,27274243)
uk_faces$ethnicity[which(uk_faces$pageid %in% white)] <- "white"
uk_faces$ethnicity[which(uk_faces$pageid %in% asian)] <- "asian"
uk_faces$ethnicity[which(uk_faces$pageid %in% black)] <- "black"
uk_faces$image_url[which(uk_faces$pageid %in% remove_image)] <- NA
uk_faces$ethnicity <- uk_faces$ethnicity %>% tolower
saveRDS(uk_faces, "./data/uk_faces")

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
usah_faces <- faceEst(data = usah_images, auth = auth)
usah_faces$ethnicity[which(usah_faces$pageid %in% black)] <- "black"
usah_faces$ethnicity[which(usah_faces$pageid %in% asian)] <- "asian"
usah_faces$ethnicity[which(usah_faces$pageid %in% hispanic)] <- "hispanic"
usah_faces$ethnicity[which(usah_faces$pageid %in% native)] <- "native"
usah_faces$ethnicity[which(usah_faces$pageid %in% islander)] <- "islander"
usah_faces <- left_join(usah_faces, usah_images, by = "pageid")
saveRDS(usah_faces, "./data/usah_faces")

# united states senate
usas_images <- imageUrl(pageid = unique(usas$pageid), project = "en.wikipedia")
usas_faces <- faceEst(data = usas_images, auth = auth)
usas_faces <- left_join(usas_faces, usas_images, by = "pageid")
saveRDS(usas_faces, "./data/usas_faces")

# retrieve and format ids ---------------------------------------------------------------

# austrian nationalrat
austria_parlid <- wikiData(item = unique(austria$wikidataid), 
                           entity = austria_entities, property = "P2280", 
                           serial = TRUE)
austria_gndid <- wikiData(item = unique(austria$wikidataid), 
                          entity = austria_entities, property = "P227", 
                          serial = TRUE)
austria_libcon <- wikiData(item = unique(austria$wikidataid), 
                           entity = austria_entities, property = "P244", 
                           serial = TRUE)
austria_bnfid <- wikiData(item = unique(austria$wikidataid), 
                          entity = austria_entities, property = "P268", 
                          serial = TRUE)
austria_freebase <- wikiData(item = unique(austria$wikidataid), 
                             entity = austria_entities, property = "P646", 
                             serial = TRUE)
austria_munzinger <- wikiData(item = unique(austria$wikidataid), 
                              entity = austria_entities, property = "P1284", 
                              serial = TRUE)
austria_nndb <- wikiData(item = unique(austria$wikidataid), 
                         entity = austria_entities, property = "P1263", 
                         serial = TRUE)
austria_imdb <- wikiData(item = unique(austria$wikidataid), 
                         entity = austria_entities, property = "P345", 
                         serial = TRUE)
austria_brittanica <- wikiData(item = unique(austria$wikidataid), 
                               entity = austria_entities, property = "P1417", 
                               serial = TRUE)
austria_quora <- wikiData(item = unique(austria$wikidataid), 
                          entity = austria_entities, property = "P3417", 
                          serial = TRUE)
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

# canadian house of commons
canada_gndid <- wikiData(item = unique(canada$wikidataid), 
                          entity = canada_entities, property = "P227", 
                          serial = TRUE)
canada_libcon <- wikiData(item = unique(canada$wikidataid), 
                           entity = canada_entities, property = "P244", 
                           serial = TRUE)
canada_bnfid <- wikiData(item = unique(canada$wikidataid), 
                          entity = canada_entities, property = "P268", 
                          serial = TRUE)
canada_freebase <- wikiData(item = unique(canada$wikidataid), 
                             entity = canada_entities, property = "P646", 
                             serial = TRUE)
canada_munzinger <- wikiData(item = unique(canada$wikidataid), 
                              entity = canada_entities, property = "P1284", 
                              serial = TRUE)
canada_nndb <- wikiData(item = unique(canada$wikidataid), 
                         entity = canada_entities, property = "P1263", 
                         serial = TRUE)
canada_imdb <- wikiData(item = unique(canada$wikidataid), 
                         entity = canada_entities, property = "P345", 
                         serial = TRUE)
canada_brittanica <- wikiData(item = unique(canada$wikidataid), 
                               entity = canada_entities, property = "P1417", 
                               serial = TRUE)
canada_quora <- wikiData(item = unique(canada$wikidataid), 
                          entity = canada_entities, property = "P3417", 
                          serial = TRUE)
canada_id <- full_join(x = canada_gndid, y = canada_libcon, by = "wikidataid") %>%
  full_join(x = ., y = canada_bnfid, by = "wikidataid") %>%
  full_join(x = ., y = canada_freebase, by = "wikidataid") %>%
  full_join(x = ., y = canada_munzinger, by = "wikidataid") %>%
  full_join(x = ., y = canada_nndb, by = "wikidataid") %>%
  full_join(x = ., y = canada_imdb, by = "wikidataid") %>%
  full_join(x = ., y = canada_brittanica, by = "wikidataid") %>%
  full_join(x = ., y = canada_quora, by = "wikidataid")
names(canada_id) <- c("wikidataid", "gndid", "libcon",
                       "bnfid", "freebase", "munzinger", "nndb", "imdb",
                       "brittanica", "quora")
saveRDS(canada_id, "./data/canada_id")

# czech poslanecka snemovna
czech_gndid <- wikiData(item = unique(czech$wikidataid), 
                          entity = czech_entities, property = "P227", 
                          serial = TRUE)
czech_libcon <- wikiData(item = unique(czech$wikidataid), 
                           entity = czech_entities, property = "P244", 
                           serial = TRUE)
czech_freebase <- wikiData(item = unique(czech$wikidataid), 
                             entity = czech_entities, property = "P646", 
                             serial = TRUE)
czech_munzinger <- wikiData(item = unique(czech$wikidataid), 
                              entity = czech_entities, property = "P1284", 
                              serial = TRUE)
czech_nndb <- wikiData(item = unique(czech$wikidataid), 
                         entity = czech_entities, property = "P1263", 
                         serial = TRUE)
czech_imdb <- wikiData(item = unique(czech$wikidataid), 
                         entity = czech_entities, property = "P345", 
                         serial = TRUE)
czech_brittanica <- wikiData(item = unique(czech$wikidataid), 
                               entity = czech_entities, property = "P1417", 
                               serial = TRUE)
czech_quora <- wikiData(item = unique(czech$wikidataid), 
                          entity = czech_entities, property = "P3417", 
                          serial = TRUE)
czech_nkcr <- wikiData(item = unique(czech$wikidataid), 
                           entity = czech_entities, property = "P691", 
                           serial = TRUE)
czech_id <- full_join(x = czech_gndid, y = czech_libcon , by = "wikidataid") %>%
  full_join(x = ., y = czech_freebase, by = "wikidataid") %>%
  full_join(x = ., y = czech_munzinger, by = "wikidataid") %>%
  full_join(x = ., y = czech_nndb, by = "wikidataid") %>%
  full_join(x = ., y = czech_imdb, by = "wikidataid") %>%
  full_join(x = ., y = czech_brittanica, by = "wikidataid") %>%
  full_join(x = ., y = czech_quora, by = "wikidataid") %>%
  full_join(x = ., y = czech_nkcr, by = "wikidataid")
names(czech_id) <- c("wikidataid", "gndid", "libcon",
                       "freebase", "munzinger", "nndb", "imdb",
                       "brittanica", "quora", "nkcr")
saveRDS(czech_id, "./data/czech_id")

# french assemble
france_parlid <- wikiData(item = unique(france$wikidataid), 
                          entity = france_entities, property = "P4123", 
                          serial = TRUE)
france_sycomore <- wikiData(item = unique(france$wikidataid), 
                            entity = france_entities, property = "P1045", 
                            serial = TRUE)
france_gndid <- wikiData(item = unique(france$wikidataid), 
                         entity = france_entities, property = "P227", 
                         serial = TRUE)
france_libcon <- wikiData(item = unique(france$wikidataid), 
                          entity = france_entities, property = "P244", 
                          serial = TRUE)
france_bnfid <- wikiData(item = unique(france$wikidataid), 
                         entity = france_entities, property = "P268", 
                         serial = TRUE)
france_freebase <- wikiData(item = unique(france$wikidataid), 
                            entity = france_entities, property = "P646", 
                            serial = TRUE)
france_munzinger <- wikiData(item = unique(france$wikidataid), 
                             entity = france_entities, property = "P1284", 
                             serial = TRUE)
france_nndb <- wikiData(item = unique(france$wikidataid), 
                        entity = france_entities, property = "P1263", 
                        serial = TRUE)
france_imdb <- wikiData(item = unique(france$wikidataid), 
                        entity = france_entities, property = "P345", 
                        serial = TRUE)
france_brittanica <- wikiData(item = unique(france$wikidataid), 
                              entity = france_entities, property = "P1417", 
                              serial = TRUE)
france_quora <- wikiData(item = unique(france$wikidataid), 
                         entity = france_entities, property = "P3417", 
                         serial = TRUE)
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
germany_parlid <- wikiData(item = unique(germany$wikidataid), 
                           entity = germany_entities, property = "P1713", 
                           serial = TRUE)
germany_gndid <- wikiData(item = unique(germany$wikidataid), 
                          entity = germany_entities, property = "P227", 
                          serial = TRUE)
germany_libcon <- wikiData(item = unique(germany$wikidataid), 
                           entity = germany_entities, property = "P244", 
                           serial = TRUE)
germany_bnfid <- wikiData(item = unique(germany$wikidataid), 
                          entity = germany_entities, property = "P268", 
                          serial = TRUE)
germany_freebase <- wikiData(item = unique(germany$wikidataid), 
                             entity = germany_entities, property = "P646", 
                             serial = TRUE)
germany_munzinger <- wikiData(item = unique(germany$wikidataid), 
                              entity = germany_entities, property = "P1284", 
                              serial = TRUE)
germany_nndb <- wikiData(item = unique(germany$wikidataid), 
                         entity = germany_entities, property = "P1263", 
                         serial = TRUE)
germany_imdb <- wikiData(item = unique(germany$wikidataid), 
                         entity = germany_entities, property = "P345", 
                         serial = TRUE)
germany_brittanica <- wikiData(item = unique(germany$wikidataid), 
                               entity = germany_entities, property = "P1417", 
                               serial = TRUE)
germany_quora <- wikiData(item = unique(germany$wikidataid), 
                          entity = germany_entities, property = "P3417", 
                          serial = TRUE)
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
ireland_gndid <- wikiData(item = unique(ireland$wikidataid), 
                          entity = ireland_entities, property = "P227", 
                          serial = TRUE)
ireland_libcon <- wikiData(item = unique(ireland$wikidataid), 
                           entity = ireland_entities, property = "P244", 
                           serial = TRUE)
ireland_bnfid <- wikiData(item = unique(ireland$wikidataid), 
                          entity = ireland_entities, property = "P268", 
                          serial = TRUE)
ireland_freebase <- wikiData(item = unique(ireland$wikidataid), 
                             entity = ireland_entities, property = "P646", 
                             serial = TRUE)
ireland_munzinger <- wikiData(item = unique(ireland$wikidataid), 
                              entity = ireland_entities, property = "P1284", 
                              serial = TRUE)
ireland_nndb <- wikiData(item = unique(ireland$wikidataid), 
                         entity = ireland_entities, property = "P1263", 
                         serial = TRUE)
ireland_imdb <- wikiData(item = unique(ireland$wikidataid), 
                         entity = ireland_entities, property = "P345", 
                         serial = TRUE)
ireland_brittanica <- wikiData(item = unique(ireland$wikidataid), 
                               entity = ireland_entities, property = "P1417", 
                               serial = TRUE)
ireland_quora <- wikiData(item = unique(ireland$wikidataid), 
                          entity = ireland_entities, property = "P3417", 
                          serial = TRUE)
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

# scottish parliament
scotland_gndid <- wikiData(item = unique(scotland$wikidataid), 
                          entity = scotland_entities, property = "P227", 
                          serial = TRUE)
scotland_libcon <- wikiData(item = unique(scotland$wikidataid), 
                           entity = scotland_entities, property = "P244", 
                           serial = TRUE)
scotland_freebase <- wikiData(item = unique(scotland$wikidataid), 
                             entity = scotland_entities, property = "P646", 
                             serial = TRUE)
scotland_munzinger <- wikiData(item = unique(scotland$wikidataid), 
                              entity = scotland_entities, property = "P1284", 
                              serial = TRUE)
scotland_nndb <- wikiData(item = unique(scotland$wikidataid), 
                         entity = scotland_entities, property = "P1263", 
                         serial = TRUE)
#scotland_imdb <- wikiData(item = unique(scotland$wikidataid), 
#                         entity = scotland_entities, property = "P345", 
#                         serial = TRUE)
scotland_brittanica <- wikiData(item = unique(scotland$wikidataid), 
                               entity = scotland_entities, property = "P1417", 
                               serial = TRUE)
scotland_quora <- wikiData(item = unique(scotland$wikidataid), 
                          entity = scotland_entities, property = "P3417", 
                          serial = TRUE)
scotland_id <- full_join(x = scotland_gndid, y = scotland_libcon, by = "wikidataid") %>%
  full_join(x = ., y = scotland_freebase, by = "wikidataid") %>%
  full_join(x = ., y = scotland_munzinger, by = "wikidataid") %>%
  full_join(x = ., y = scotland_nndb, by = "wikidataid") %>%
#  full_join(x = ., y = scotland_imdb, by = "wikidataid") %>%
  full_join(x = ., y = scotland_brittanica, by = "wikidataid") %>%
  full_join(x = ., y = scotland_quora, by = "wikidataid")
names(scotland_id) <- c("wikidataid", "gndid", "libcon",
                       "freebase", "munzinger", "nndb",
                       "brittanica", "quora")
saveRDS(scotland_id, "./data/scotland_id")

# united kingdom parliament
uk_parlid <- wikiData(item = unique(uk$wikidataid), 
                      entity = uk_entities, property = "P6213", 
                      serial = TRUE)
uk_parlbio <- wikiData(item = unique(uk$wikidataid), 
                       entity = uk_entities, property = "P1996", 
                       serial = TRUE)
uk_parlthesaurus <- wikiData(item = unique(uk$wikidataid), 
                         entity = uk_entities, property = "P4527", 
                         serial = TRUE)
uk_rush <- wikiData(item = unique(uk$wikidataid), 
                         entity = uk_entities, property = "P4471", 
                         serial = TRUE)
uk_national <- wikiData(item = unique(uk$wikidataid), 
                         entity = uk_entities, property = "P3029", 
                         serial = TRUE)
uk_hansard <- wikiData(item = unique(uk$wikidataid), 
                         entity = uk_entities, property = "P2015", 
                         serial = TRUE)
uk_publicwhip <- wikiData(item = unique(uk$wikidataid), 
                       entity = uk_entities, property = "P2169", 
                       serial = TRUE)
uk_theyworkforyou <- wikiData(item = unique(uk$wikidataid), 
                       entity = uk_entities, property = "P2171", 
                       serial = TRUE)
uk_gndid <- wikiData(item = unique(uk$wikidataid), 
                           entity = uk_entities, property = "P227", 
                           serial = TRUE)
uk_libcon <- wikiData(item = unique(uk$wikidataid), 
                            entity = uk_entities, property = "P244", 
                            serial = TRUE)
uk_freebase <- wikiData(item = unique(uk$wikidataid), 
                              entity = uk_entities, property = "P646", 
                              serial = TRUE)
uk_munzinger <- wikiData(item = unique(uk$wikidataid), 
                               entity = uk_entities, property = "P1284", 
                               serial = TRUE)
uk_nndb <- wikiData(item = unique(uk$wikidataid), 
                          entity = uk_entities, property = "P1263", 
                          serial = TRUE)
uk_imdb <- wikiData(item = unique(uk$wikidataid), 
                         entity = uk_entities, property = "P345", 
                         serial = TRUE)
uk_brittanica <- wikiData(item = unique(uk$wikidataid), 
                                entity = uk_entities, property = "P1417", 
                                serial = TRUE)
uk_quora <- wikiData(item = unique(uk$wikidataid), 
                           entity = uk_entities, property = "P3417", 
                           serial = TRUE)
uk_id <- full_join(x = uk_parlid, y = uk_parlbio, by = "wikidataid") %>%
  full_join(x = ., y = uk_parlthesaurus, by = "wikidataid") %>%
  full_join(x = ., y = uk_rush, by = "wikidataid") %>%
  full_join(x = ., y = uk_national, by = "wikidataid") %>%
  full_join(x = ., y = uk_hansard, by = "wikidataid") %>%
  full_join(x = ., y = uk_publicwhip, by = "wikidataid") %>%
  full_join(x = ., y = uk_theyworkforyou, by = "wikidataid") %>%
  full_join(x = ., y = uk_gndid, by = "wikidataid") %>%
  full_join(x = ., y = uk_libcon, by = "wikidataid") %>%
  full_join(x = ., y = uk_freebase, by = "wikidataid") %>%
  full_join(x = ., y = uk_munzinger, by = "wikidataid") %>%
  full_join(x = ., y = uk_nndb, by = "wikidataid") %>%
  full_join(x = ., y = uk_imdb, by = "wikidataid") %>%
  full_join(x = ., y = uk_brittanica, by = "wikidataid") %>%
  full_join(x = ., y = uk_quora, by = "wikidataid")
names(uk_id) <- c("wikidataid", "parlid", "parlbio", "parlthesaurus",
                  "rush", "national", "hansard", "publicwhip", "theyworkforyou",
                  "gndid", "libcon", "freebase", "munzinger", "nndb", "imdb", 
                  "brittanica", "quora")
saveRDS(uk_id, "./data/uk_id")

# united states house
usah_parlid <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                        entity = usah_entities, property = "P1157", 
                        serial = TRUE)
usah_gndid <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                       entity = usah_entities, property = "P227", 
                       serial = TRUE)
usah_libcon <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                        entity = usah_entities, property = "P244", 
                        serial = TRUE)
usah_bnfid <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                       entity = usah_entities, property = "P268", 
                       serial = TRUE)
usah_freebase <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                          entity = usah_entities, property = "P646", 
                          serial = TRUE)
usah_munzinger <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                           entity = usah_entities, property = "P1284", 
                           serial = TRUE)
usah_nndb <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                      entity = usah_entities, property = "P1263", 
                      serial = TRUE)
usah_imdb <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                      entity = usah_entities, property = "P345", 
                      serial = TRUE)
usah_brittanica <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                            entity = usah_entities, property = "P1417", 
                            serial = TRUE)
usah_quora <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                       entity = usah_entities, property = "P3417", 
                       serial = TRUE)
usah_votesmart <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                           entity = usah_entities, property = "P3344", 
                           serial = TRUE)
usah_fecid <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                       entity = usah_entities, property = "P1839", 
                       serial = TRUE)
usah_ballotpedia <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                             entity = usah_entities, property = "P2390", 
                             serial = TRUE)
usah_opensecrets <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                             entity = usah_entities, property = "P2686", 
                             serial = TRUE)
usah_genealogists <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                              entity = usah_entities, property = "P1819", 
                              serial = TRUE)
usah_politfacts <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                            entity = usah_entities, property = "P2267", 
                            serial = TRUE)
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
usas_parlid <- wikiData(item = unique(usas$wikidataid), 
                        entity = usas_entities, property = "P1157", 
                        serial = TRUE)
usas_gndid <- wikiData(item = unique(usas$wikidataid), 
                       entity = usas_entities, property = "P227", 
                       serial = TRUE)
usas_libcon <- wikiData(item = unique(usas$wikidataid), 
                        entity = usas_entities, property = "P244", 
                        serial = TRUE)
usas_bnfid <- wikiData(item = unique(usas$wikidataid), 
                       entity = usas_entities, property = "P268", 
                       serial = TRUE)
usas_freebase <- wikiData(item = unique(usas$wikidataid), 
                          entity = usas_entities, property = "P646", 
                          serial = TRUE)
usas_munzinger <- wikiData(item = unique(usas$wikidataid), 
                           entity = usas_entities, property = "P1284", 
                           serial = TRUE)
usas_nndb <- wikiData(item = unique(usas$wikidataid), 
                      entity = usas_entities, property = "P1263", 
                      serial = TRUE)
usas_imdb <- wikiData(item = unique(usas$wikidataid), 
                      entity = usas_entities, property = "P345", 
                      serial = TRUE)
usas_brittanica <- wikiData(item = unique(usas$wikidataid), 
                            entity = usas_entities, property = "P1417", 
                            serial = TRUE)
usas_quora <- wikiData(item = unique(usas$wikidataid), 
                       entity = usas_entities, property = "P3417", 
                       serial = TRUE)
usas_votesmart <- wikiData(item = unique(usas$wikidataid), 
                           entity = usas_entities, property = "P3344", 
                           serial = TRUE)
usas_fecid <- wikiData(item = unique(usas$wikidataid), 
                       entity = usas_entities, property = "P1839", 
                       serial = TRUE)
usas_ballotpedia <- wikiData(item = unique(usas$wikidataid), 
                             entity = usas_entities, property = "P2390", 
                             serial = TRUE)
usas_opensecrets <- wikiData(item = unique(usas$wikidataid), 
                             entity = usas_entities, property = "P2686", 
                             serial = TRUE)
usas_genealogists <- wikiData(item = unique(usas$wikidataid), 
                              entity = usas_entities, property = "P1819", 
                              serial = TRUE)
usas_politfacts <- wikiData(item = unique(usas$wikidataid), 
                            entity = usas_entities, property = "P2267", 
                            serial = TRUE)
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
austria_positions <- wikiData(item = unique(austria$wikidataid), 
                              entity = austria_entities, unique = TRUE, 
                              property = "P39")
austria_positions <- austria_positions[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(austria_positions, "./data/austria_positions")

# canadian house of commons
canada_positions <- wikiData(item = unique(canada$wikidataid), 
                              entity = canada_entities, unique = TRUE, 
                             property = "P39")
canada_positions <- canada_positions[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(canada_positions, "./data/canada_positions")

# czech poslanecka snemovna
czech_positions <- wikiData(item = unique(czech$wikidataid), 
                              entity = czech_entities, unique = TRUE, 
                            property = "P39")
czech_positions <- czech_positions[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(czech_positions, "./data/czech_positions")

# french assemble
france_positions <- wikiData(item = unique(france$wikidataid), 
                             entity = france_entities, unique = TRUE, 
                             property = "P39")
france_positions <- france_positions[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(france_positions, "./data/france_positions")

# german bundestag
germany_positions <- wikiData(item = unique(germany$wikidataid), 
                              entity = germany_entities, unique = TRUE, 
                              property = "P39")
germany_positions <- germany_positions[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(germany_positions, "./data/germany_positions")

# irish dail
ireland_positions <- wikiData(item = unique(ireland$wikidataid), 
                              entity = ireland_entities, unique = TRUE, 
                              property = "P39")
ireland_positions <- ireland_positions %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(ireland_positions, "./data/ireland_positions")

# scottish parliament
scotland_positions <- wikiData(item = unique(scotland$wikidataid), 
                              entity = scotland_entities, unique = TRUE, 
                              property = "P39")
scotland_positions <- scotland_positions %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(scotland_positions, "./data/scotland_positions")

# united kingdom parliament
uk_positions <- wikiData(item = unique(uk$wikidataid), 
                               entity = uk_entities, unique = TRUE, 
                               property = "P39")
uk_positions <- uk_positions %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(uk_positions, "./data/uk_positions")

# united states house
usah_positions <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                           entity = usah_entities, unique = TRUE, 
                           property = "P39")
usah_positions <- usah_positions[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(usah_positions, "./data/usah_positions")

# united states senate
usas_positions <- wikiData(item = unique(usas$wikidataid), 
                           entity = usas_entities, unique = TRUE, 
                           property = "P39")
usas_positions <- usas_positions[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(usas_positions, "./data/usas_positions")

# retrieve and format occupation --------------------------------------------------------

# austrian nationalrat
austria_occupation <- wikiData(item = unique(austria$wikidataid), 
                               entity = austria_entities, unique = TRUE, 
                               property = "P106")
austria_occupation <- austria_occupation[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(austria_occupation, "./data/austria_occupation")

# canadian house of commons
canada_occupation <- wikiData(item = unique(canada$wikidataid), 
                               entity = canada_entities, unique = TRUE, 
                               property = "P106")
canada_occupation <- canada_occupation %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(canada_occupation, "./data/canada_occupation")

# czech poslanecka snemovna
czech_occupation <- wikiData(item = unique(czech$wikidataid), 
                               entity = czech_entities, unique = TRUE, 
                               property = "P106")
czech_occupation <- czech_occupation[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(czech_occupation, "./data/czech_occupation")

# french assemble
france_occupation <- wikiData(item = unique(france$wikidataid), 
                              entity = france_entities, unique = TRUE, 
                              property = "P106")
france_occupation <- france_occupation[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(france_occupation, "./data/france_occupation")

# german bundestag
germany_occupation <- wikiData(item = unique(germany$wikidataid), 
                               entity = germany_entities, unique = TRUE, 
                               property = "P106")
germany_occupation <- germany_occupation[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(germany_occupation, "./data/germany_occupation")

# irish dail
ireland_occupation <- wikiData(item = unique(ireland$wikidataid), 
                               entity = ireland_entities, unique = TRUE, 
                               property = "P106")
ireland_occupation <- ireland_occupation %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(ireland_occupation, "./data/ireland_occupation")

# scottish parliament
scotland_occupation <- wikiData(item = unique(scotland$wikidataid), 
                               entity = scotland_entities, unique = TRUE, 
                               property = "P106")
scotland_occupation <- scotland_occupation %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(scotland_occupation, "./data/scotland_occupation")

# united kingdom parliament
uk_occupation <- wikiData(item = unique(uk$wikidataid), 
                                entity = uk_entities, unique = TRUE, 
                                property = "P106")
uk_occupation <- uk_occupation %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(uk_occupation, "./data/uk_occupation")

# united states house
usah_occupation <- wikiData(item = unique(usah$wikidataid)[!is.na(unique(usah$wikidataid))], 
                            entity = usah_entities, unique = TRUE, 
                            property = "P106")
usah_occupation <- usah_occupation[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(usah_occupation, "./data/usah_occupation")

# united states senate
usas_occupation <- wikiData(item = unique(usas$wikidataid), 
                            entity = usas_entities, unique = TRUE, 
                            property = "P106")
usas_occupation <- usas_occupation[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(usas_occupation, "./data/usas_occupation")


#### DATA UPDATE ========================================================================

# the following requires data from the previous version (0.0.0.9000) available on GitHub

# update social media data --------------------------------------------------------------
# If the current version lists NA for an observation for which the previous version lists
# an entry, this entry is transfered to the current version

# austrian nationalrat
austria_social <- readRDS("./data/austria_social")
current <- updateSocial(previous = "./data_v0.0.0.9/austria_social", 
                        current = "./data/austria_social")
diff_data <- setdiff(current, austria_social)
if(dim(diff_data)[1] > 0) {
  saveRDS(current, "./data/austria_social")
}

# canadian house of commons (not available in version 0.0.0.9, hence grayed out)
#canada_social <- readRDS("./data/canada_social")
#current <- updateSocial(previous = "./data_v0.0.0.9/canada_social", 
#                        current = "./data/canada_social")
#diff_data <- setdiff(current, canada_social)
#if(dim(diff_data)[1] > 0) {
#  saveRDS(current, "./data/canada_social")
#}

# czech poslanecka snemovna (not available in version 0.0.0.9, hence grayed out)
#czech_social <- readRDS("./data/czech_social")
#current <- updateSocial(previous = "./data_v0.0.0.9/czech_social", 
#                        current = "./data/czech_social")
#diff_data <- setdiff(current, czech_social)
#if(dim(diff_data)[1] > 0) {
#  saveRDS(current, "./data/czech_social")
#}

# french assemble
france_social <- readRDS("./data/france_social")
current <- updateSocial(previous = "./data_v0.0.0.9/france_social", 
                        current = "./data/france_social")
diff_data <- setdiff(current, france_social)
if(dim(diff_data)[1] > 0) {
  saveRDS(current, "./data/france_social")
}

# german bundestag
germany_social <- readRDS("./data/germany_social")
current <- updateSocial(previous = "./data_v0.0.0.9/germany_social", 
                        current = "./data/germany_social")
diff_data <- setdiff(current, germany_social)
if(dim(diff_data)[1] > 0) {
  saveRDS(current, "./data/germany_social")
}

# irish dail
ireland_social <- readRDS("./data/ireland_social")
current <- updateSocial(previous = "./data_v0.0.0.9/ireland_social", 
                        current = "./data/ireland_social")
diff_data <- setdiff(current, ireland_social)
if(dim(diff_data)[1] > 0) {
  saveRDS(current, "./data/ireland_social")
}

# scottish parliament (not available in version 0.0.0.9, hence grayed out)
#scotland_social <- readRDS("./data/scotland_social")
#current <- updateSocial(previous = "./data_v0.0.0.9/scotland_social", 
#                        current = "./data/scotland_social")
#diff_data <- setdiff(current, scotland_social)
#if(dim(diff_data)[1] > 0) {
#  saveRDS(current, "./data/scotland_social")
#}

# united kingdom parliament (not available in version 0.0.0.9, hence grayed out)
#uk_social <- readRDS("./data/uk_social")
#current <- updateSocial(previous = "./data_v0.0.0.9/uk_social", 
#                        current = "./data/uk_social")
#diff_data <- setdiff(current, uk_social)
#if(dim(diff_data)[1] > 0) {
#  saveRDS(current, "./data/uk_social")
#}

# united states house
usah_social <- readRDS("./data/usah_social")
current <- updateSocial(previous = "./data_v0.0.0.9/usah_social", 
                        current = "./data/usah_social")
diff_data <- setdiff(current, usah_social)
if(dim(diff_data)[1] > 0) {
  saveRDS(current, "./data/usah_social")
}

# united states senate
usas_social <- readRDS("./data/usas_social")
current <- updateSocial(previous = "./data_v0.0.0.9/usas_social", 
                        current = "./data/usas_social")
diff_data <- setdiff(current, usas_social)
if(dim(diff_data)[1] > 0) {
  saveRDS(current, "./data/usas_social")
}


# update portrait data ------------------------------------------------------------------
# If a pageid is present in a previous version, the ethnicity value is transfered to the
# current version. I a pageid is not present in the previous version, the ethnicity value
# will be checked and adjusted in the current version.

# austrian nationalrat
austria_faces <- readRDS("./data/austria_faces")[,c("ethnicity", "pageid", "image_url")]
austria_faces$ethnicity <- as.character(austria_faces$ethnicity)
previous <- readRDS("./data_v0.0.0.9/austria_faces")[,c("ethnicity", "pageid", 
                                                        "image_url")]
previous$ethnicity <- as.character(previous$ethnicity)
match(previous$pageid, austria_faces$pageid)
austria_faces[match(previous$pageid, austria_faces$pageid),] <- previous[previous$pageid 
                                                                         %in% austria_faces$pageid,]
austria_faces$ethnicity[str_detect(austria_faces$ethnicity, "WHITE|BLACK|ASIAN|INDIA")] <- NA
white <- c(4483781,9706131,5339528,2778152,643891,355550,3488919,1335173,2997586,273422,
           643595,319104,3959392,3817011,3165013,2030294,3158444,265582,1035345,3773146,  
           8990887,8307028,7949059,7769254,7404842,4087622,9454760,10091266,10083529,
           7404872,4735227,10025905,8496878,7659405,3468829,5500449,10083641,10089180,
           8351591,9700326,10082169,8902246,10108954,838966,10083553,8011339,10118579,
           10364965,8614972,10108733,10106514,4532253,2975879,10118553,10085669,10080503,
           76308,2781395,3785550,3836043,10103318,10080547,7404769,4825734,8359044,
           8539615,5033960,10087696,273485,9907067,4010302)
austria_faces$ethnicity[which(austria_faces$pageid %in% white)] <- "white"
austria_faces <- austria_faces[-which(is.na(austria_faces$ethnicity)|is.na(austria_faces$image_url)),]
saveRDS(austria_faces, "./data/austria_faces")

# canadian house of commons (not available in version 0.0.0.9)

# czech poslanecka snemovna (not available in version 0.0.0.9)

# french assemble
france_faces <- readRDS("./data/france_faces")[,c("ethnicity", "pageid", "image_url")]
france_faces$ethnicity <- as.character(france_faces$ethnicity)
previous <- readRDS("./data_v0.0.0.9/france_faces")[,c("ethnicity", "pageid", 
                                                        "image_url")]
previous$ethnicity <- as.character(previous$ethnicity)
match(previous$pageid, france_faces$pageid)
france_faces[na.omit(match(previous$pageid, france_faces$pageid)),] <- previous[previous$pageid 
                                                                         %in% france_faces$pageid,]
france_faces$ethnicity[str_detect(france_faces$ethnicity, "WHITE|BLACK|ASIAN|INDIA")] <- NA
black <- c(543016,937306,207347,10933712,10933368,10934755,9161,10934893,3825555,10934442,
           10934899,10933773,10936196,10933401,10934860,10936683,10937469,10935304,10934595,
           10934757,10934182,10934798)
asian <- c(9216,10883494,9015132)
arab <- c(10935317,10934613)
hispanic <- c(634584)
remove_image <- c(937306,3536220,86463,3517968,319709,1106945,543066,1624369,3024135,
                  4214376,219905,225422,567998,48294,171243,10600,9204,9350,91399,9146,
                  2495512,263746,5541604,9355,9777,296204,7135564,4289806,3714696,1077745, 
                  2488102,7553350,62598,126719,5796815,117504,9641,1656530,242830,9590,
                  9601,6262088,494219,1175020,10616,731657,1116637,6368156,6370383,9565,
                  1365906,9284,1683516,1293447,10353,10677,9525,1701448,1693969,1711917,
                  1710967,1699385,4796397,6368444,8258785,6368352,10934732,10936502,
                  10935229,10937049)
france_faces$ethnicity[which(france_faces$pageid %in% black)] <- "black"
france_faces$ethnicity[which(france_faces$pageid %in% asian)] <- "asian"
france_faces$ethnicity[which(france_faces$pageid %in% arab)] <- "arab"
france_faces$ethnicity[which(france_faces$pageid %in% hispanic)] <- "hispanic"
france_faces <- france_faces[-which(france_faces$pageid %in% remove_image),]
france_faces$ethnicity[which(is.na(france_faces$ethnicity))] <- "white"
saveRDS(france_faces, "./data/france_faces")

# german bundestag
germany_faces <- readRDS("./data/germany_faces")[,c("ethnicity", "pageid", "image_url")]
germany_faces$ethnicity <- as.character(germany_faces$ethnicity)
previous <- readRDS("./data_v0.0.0.9/germany_faces")[,c("ethnicity", "pageid")]
previous$ethnicity <- as.character(previous$ethnicity)
match(previous$pageid, germany_faces$pageid)
germany_faces[na.omit(match(previous$pageid, germany_faces$pageid)),c("ethnicity","pageid")] <- previous[previous$pageid 
                                                                                %in% germany_faces$pageid,]
germany_faces$ethnicity[str_detect(germany_faces$ethnicity, "WHITE|BLACK|ASIAN|INDIA")] <- NA
remove_image <- c(172137,263400,281110,284651,224726,130000,306438,328142,288159,
                  96334,68670,72974,8726,234629,171182,99320,806904,706778,68677,
                  833256,361107,68863,2561320,3529188,2941739,8729,171029,318136,
                  2458535,466039,225610,953539,932179,465996,1064262,958043,71381,
                  143781,83287,982221,56169,71195,412023,173918,894892,1016579,
                  8018834,810572,56169,1119143,1406147,186972,72412,172103,1045180,
                  3994280,175609,261254,117950,274741,274756,268323,245034,72564,
                  246044,277242,255230,277737,282405,284583,308531,64611,268205,850561,
                  302133,302139,302150,68663,343013,446219,416356,284313,1015282,242011,
                  372783,314181,292584,2961077,23882,68682,120125,237989,581883,
                  582910,241441,603184,1159080,1254574,665980,73028,685376,59445,
                  72917,181687,735922,237783,68676,751056,1165903,70889,1352226,
                  3529188,245071,929399,2976058,2954300,172098,237994,19907,
                  1285948,119254,2805640,71380,5984651,2084514,2203947,2452645,
                  301076,1016476,1823263,20188,237767,217661,2369935,468941,
                  2971909,1903099,5894401,1119143,147181,6090531,4483787,385674,
                  1882666,2690771,186942,175623,238014,1156670,244791,518274,
                  6224966,2477,999493,288747,75631,261254,2498776,1139068,174104,
                  1449977,173801,176247,1919815,544256,939956,770570,179861,
                  950171,984997,4764351,7220616,175467,237793,6070382,173791,
                  273911,118080,237794,1063545,2918247,900857,593558,393070,544239,
                  237874,487009,666204,667926,191661,67608,60076,938901,517250,4775998,
                  4753037,4777169,4764334,7886996,10061481,9484917,4492780,10078959)
germany_faces$image_url[which(germany_faces$pageid %in% remove_image)] <- NA
germany_faces$ethnicity[which(is.na(germany_faces$ethnicity))] <- "white"
saveRDS(germany_faces, "./data/germany_faces")

# irish dail
ireland_faces <- readRDS("./data/ireland_faces")[,c("ethnicity", "pageid", "image_url")]
ireland_faces$ethnicity <- as.character(ireland_faces$ethnicity)
previous <- readRDS("./data_v0.0.0.9/ireland_faces")[,c("ethnicity", "pageid")]
previous$ethnicity <- as.character(previous$ethnicity)
match(previous$pageid, ireland_faces$pageid)
ireland_faces[na.omit(match(previous$pageid, ireland_faces$pageid)),c("ethnicity","pageid")] <- previous[previous$pageid 
                                                                                                         %in% ireland_faces$pageid,]
ireland_faces$ethnicity[str_detect(ireland_faces$ethnicity, "WHITE|BLACK|ASIAN|INDIA")] <- NA
remove_image <- c(640693,1371698,1034456,16725764,16654494,27589987,13247299,24046831,
                  1090222,18727407,18782192,2113433,479136,18570544,2780061)
ireland_faces$image_url[which(ireland_faces$pageid %in% remove_image)] <- NA
ireland_faces$ethnicity[which(is.na(ireland_faces$ethnicity))] <- "white"
saveRDS(ireland_faces, "./data/ireland_faces")

# scottish parliament (not available in version 0.0.0.9, hence grayed out)

# united kingdom parliament (not available in version 0.0.0.9, hence grayed out)

# united states house
usah_faces <- readRDS("./data/usah_faces")[,c("ethnicity", "pageid", "image_url")]
usah_faces$ethnicity <- as.character(usah_faces$ethnicity)
previous <- readRDS("./data_v0.0.0.9/usah_faces")[,c("ethnicity", "pageid")]
previous$ethnicity <- as.character(previous$ethnicity)
match(previous$pageid, usah_faces$pageid)
usah_faces[na.omit(match(previous$pageid, usah_faces$pageid)),c("ethnicity","pageid")] <- previous[previous$pageid 
                                                                                                         %in% usah_faces$pageid,]
usah_faces$ethnicity[str_detect(usah_faces$ethnicity, "WHITE|BLACK|ASIAN|INDIA")] <- NA
remove_image <- c(1189542,2684294,3138674,1183529,1135174,920701,3025047,3142352,3185656,
                  1189545,1198513,3723285,3805906,1188786,7575833,5646237,10243632,9597584,
                  7674419,7665582,1393101,31816597,7674521,6033161,11600194,8529354,1362688,
                  10074277,6857341,7603699,11579480,12767474,11544000,10662390,13722998,
                  11593687,11631734,470247,11594459,11630573,710474,4673147,11804468,4305129,
                  11600229,11629652,6615421,8302085,11629219,11594896,10956300,10533102,
                  11630974,11587236,11588217,2633524,8418020,11629260,11630729,11597569,
                  11578806,11729182,13364095,2393578,11948738)
black <- c(58093755,44301554,57479640,58986352,26750955,51289996,58833433,14312712,
           318104,57489931)
asian <- c(51289996)
white <- c(8131370)
islander <- c(4695345)
hispanic <- c(57019825,44518105,57548040,57809437,54885332)
usah_faces <- usah_faces[-which(usah_faces$pageid %in% remove_image),]
usah_faces$ethnicity[which(usah_faces$pageid %in% asian)] <- "asian"
usah_faces$ethnicity[which(usah_faces$pageid %in% black)] <- "black"
usah_faces$ethnicity[which(usah_faces$pageid %in% islander)] <- "islander"
usah_faces$ethnicity[which(usah_faces$pageid %in% white)] <- "white"
usah_faces$ethnicity[which(usah_faces$pageid %in% hispanic)] <- "hispanic"
usah_faces$ethnicity[which(is.na(usah_faces$ethnicity))] <- "white"
saveRDS(usah_faces, "./data/usah_faces")

# united states senate
usas_faces <- readRDS("./data/usas_faces")[,c("ethnicity", "pageid", "image_url")]
usas_faces$ethnicity <- as.character(usas_faces$ethnicity)
previous <- readRDS("./data_v0.0.0.9/usas_faces")[,c("ethnicity", "pageid")]
previous$ethnicity <- as.character(previous$ethnicity)
match(previous$pageid, usas_faces$pageid)
usas_faces[na.omit(match(previous$pageid, usas_faces$pageid)),c("ethnicity","pageid")] <- previous[previous$pageid 
                                                                                                   %in% usas_faces$pageid,]
usas_faces$ethnicity[str_detect(usas_faces$ethnicity, "WHITE|BLACK|ASIAN|INDIA")] <- NA
remove_image <- c(502726,3025047,710474)
usas_faces <- usas_faces[-which(usas_faces$pageid %in% remove_image),]
usas_faces$ethnicity[which(is.na(usas_faces$ethnicity))] <- "white"
saveRDS(usas_faces, "./data/usas_faces")
