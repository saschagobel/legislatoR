# ---------------------------------------------------------------------------------------
# legislatoR
# Sascha Göbel and Simon Munzert
# Script: data for spain
# August 2019
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

# retrieve basic data from Wikipedia ----------------------------------------------------
spain_wiki <- collectorSpain(source = "./data/htmls/spain")

# retrieve official data and merge with data from Wikipedia -----------------------------
spain_official <- collectorSpainOfficial(source = "./data/htmls/spain_official")
a <- spain_official
b <- spain_wiki

# name matching
for (k in 1:length(b)) {
  b[[k]]$index <- NA 
  b[[k]]$index2 <- NA 
  a[[k]]$first <- str_trim(str_replace(str_extract(a[[k]]$name, ",.+"), "^,", ""))
  a[[k]]$end <- str_replace(a[[k]]$name, ",.+", "")
  a[[k]]$full <- str_c(a[[k]]$first, " ", a[[k]]$end)
  a[[k]]$name_wiki <- NA
  a[[k]]$url_wiki <- NA
  for (i in 1:nrow(b[[k]])) {
    # try to find full string
    find <- str_detect(a[[k]]$full, b[[k]]$name[i])
    if (length(find) == 1) {
      b[[k]]$index[i] <- find
      next
    }
    #try to find name components
    for (j in 1:4) {
      search <- str_c(" ", word(b[[k]]$name[i], -j), " ") 
      find <- which(str_detect(str_c(" ", a[[k]]$full, " "), search) == TRUE)
      if (length(find) == 1) {
        b[[k]]$index2[i] <- find
        break
      }
    }
    # try to find combinations of name components
    search <- str_c(word(b[[k]]$name[i], -2), " ", word(b[[k]]$name[i], -(1)))
    find <- which(str_detect(a[[k]]$full, search) == TRUE)
    if (length(find) == 1) {
      b[[k]]$index[i] <- find
      next
    } else {
      search <- str_c(word(b[[k]]$name[i], -(3)), " ", word(b[[k]]$name[i], -(2)))
      find <- which(str_detect(a[[k]]$full, search) == TRUE)
      if (length(find) == 1) {
        b[[k]]$index[i] <- find
        next
      }
    }
  }
  b[[k]][duplicated(b[[k]]$index2)| duplicated(b[[k]]$index2, fromLast = TRUE),]$index2 <- NA
  b[[k]]$index <- ifelse(is.na(b[[k]]$index), b[[k]]$index2, b[[k]]$index)
  b[[k]][duplicated(b[[k]]$index)| duplicated(b[[k]]$index, fromLast = TRUE),]$index <- NA
  d <- b[[k]][which(!is.na(b[[k]]$index)),]
  a[[k]][d$index,]$name_wiki <- d$name
  a[[k]][d$index,]$url_wiki <- d$url
}

# collect missings on Wikipedia ---------------------------------------------------------

# check for mismatches in a, if wrong, replace with NA in a and b
b[[14]][b[[14]]$index %in% c(47,206,243),]$index <- NA
a[[14]][c(47,206,243),c("url_wiki", "name_wiki")] <- NA
b[[13]][b[[13]]$index %in% c(1,32,221,254),]$index <- NA
a[[13]][c(1,32,221,254),c("url_wiki", "name_wiki")] <- NA
b[[12]][b[[12]]$index %in% c(387),]$index <- NA
a[[12]][c(387),c("url_wiki", "name_wiki")] <- NA

# manually fill remaining NAs in b
b[[14]]$index3 <- NA
b[[14]][c(3,13,19,32,103,108,119,120,130,
          133,138,154,173,182,183,199,212,
          223,227), ]$index3 <- c(7,19,31,47,156,172,
                                  189,190,208,211,221,243,273,
                                  288,289,310,328,342,347)
a[[14]][na.omit(b[[14]]$index3),]$name_wiki <- b[[14]][which(!is.na(b[[14]]$index3)),]$name
a[[14]][na.omit(b[[14]]$index3),]$url_wiki <- b[[14]][which(!is.na(b[[14]]$index3)),]$url

b[[13]]$index3 <- NA
b[[13]][c(3,21,56,97,103,108,123,125,129,
          140,162,163,192,204), ]$index3 <- c(7,32,89,177,191,197,56,227,236,254,195,
                                              296,340,359)
a[[13]][na.omit(b[[13]]$index3),]$name_wiki <- b[[13]][which(!is.na(b[[13]]$index3)),]$name
a[[13]][na.omit(b[[13]]$index3),]$url_wiki <- b[[13]][which(!is.na(b[[13]]$index3)),]$url

b[[12]]$index3 <- NA
b[[12]][c(20,25,51,98,119,124,126,224), ]$index3 <- c(26,32,76,168,202,214,217,81)
a[[12]][na.omit(b[[12]]$index3),]$name_wiki <- b[[12]][which(!is.na(b[[12]]$index3)),]$name
a[[12]][na.omit(b[[12]]$index3),]$url_wiki <- b[[12]][which(!is.na(b[[12]]$index3)),]$url

b[[11]]$index3 <- NA
b[[11]][c(22,26,94,107,126,129,176), ]$index3 <- c(38,44,155,177,219,223,318)
a[[11]][na.omit(b[[11]]$index3),]$name_wiki <- b[[11]][which(!is.na(b[[11]]$index3)),]$name
a[[11]][na.omit(b[[11]]$index3),]$url_wiki <- b[[11]][which(!is.na(b[[11]]$index3)),]$url

b[[10]]$index3 <- NA
b[[10]][c(29,65,89,111,115), ]$index3 <- c(43,101,154,187,198)
a[[10]][na.omit(b[[10]]$index3),]$name_wiki <- b[[10]][which(!is.na(b[[10]]$index3)),]$name
a[[10]][na.omit(b[[10]]$index3),]$url_wiki <- b[[10]][which(!is.na(b[[10]]$index3)),]$url

b[[9]]$index3 <- NA
b[[9]][c(11,55,70,111,225,227), ]$index3 <- c(14,87,111,181,394,400)
a[[9]][na.omit(b[[9]]$index3),]$name_wiki <- b[[9]][which(!is.na(b[[9]]$index3)),]$name
a[[9]][na.omit(b[[9]]$index3),]$url_wiki <- b[[9]][which(!is.na(b[[9]]$index3)),]$url

b[[8]]$index3 <- NA
b[[8]][c(29,56,86,237,240), ]$index3 <- c(37,71,119,396,401)
a[[8]][na.omit(b[[8]]$index3),]$name_wiki <- b[[8]][which(!is.na(b[[8]]$index3)),]$name
a[[8]][na.omit(b[[8]]$index3),]$url_wiki <- b[[8]][which(!is.na(b[[8]]$index3)),]$url

b[[7]]$index3 <- NA
b[[7]][c(129, 223, 243, 262), ]$index3 <- c(165, 297, 340,382)
a[[7]][na.omit(b[[7]]$index3),]$name_wiki <- b[[7]][which(!is.na(b[[7]]$index3)),]$name
a[[7]][na.omit(b[[7]]$index3),]$url_wiki <- b[[7]][which(!is.na(b[[7]]$index3)),]$url

b[[6]]$index3 <- NA
b[[6]][c(7,64,86,116,129,130,199,246,269), ]$index3 <- c(8,85,111,157,184,186,283,354,395)
a[[6]][na.omit(b[[6]]$index3),]$name_wiki <- b[[6]][which(!is.na(b[[6]]$index3)),]$name
a[[6]][na.omit(b[[6]]$index3),]$url_wiki <- b[[6]][which(!is.na(b[[6]]$index3)),]$url

b[[5]]$index3 <- NA
b[[5]][c(23,89,143,263,280), ]$index3 <- c(28,116,190,367,392)
a[[5]][na.omit(b[[5]]$index3),]$name_wiki <- b[[5]][which(!is.na(b[[5]]$index3)),]$name
a[[5]][na.omit(b[[5]]$index3),]$url_wiki <- b[[5]][which(!is.na(b[[5]]$index3)),]$url

b[[4]]$index3 <- NA
b[[4]][c(131,193,202,270,271), ]$index3 <- c(151,216,228,302,303)
a[[4]][na.omit(b[[4]]$index3),]$name_wiki <- b[[4]][which(!is.na(b[[4]]$index3)),]$name
a[[4]][na.omit(b[[4]]$index3),]$url_wiki <- b[[4]][which(!is.na(b[[4]]$index3)),]$url

b[[3]]$index3 <- NA
b[[3]][c(6,224,235,309,310), ]$index3 <- c(6,240,252,336,337)
a[[3]][na.omit(b[[3]]$index3),]$name_wiki <- b[[3]][which(!is.na(b[[3]]$index3)),]$name
a[[3]][na.omit(b[[3]]$index3),]$url_wiki <- b[[3]][which(!is.na(b[[3]]$index3)),]$url

b[[2]]$index3 <- NA
b[[2]][c(64,121,140,198), ]$index3 <- c(96,196,225,319)
a[[2]][na.omit(b[[2]]$index3),]$name_wiki <- b[[2]][which(!is.na(b[[2]]$index3)),]$name
a[[2]][na.omit(b[[2]]$index3),]$url_wiki <- b[[2]][which(!is.na(b[[2]]$index3)),]$url

b[[1]]$index3 <- NA
b[[1]][c(108,195,196,197,198,199,200), ]$index3 <- c(110,326,327,330,331,333,86)
a[[1]][na.omit(b[[1]]$index3),]$name_wiki <- b[[1]][which(!is.na(b[[1]]$index3)),]$name
a[[1]][na.omit(b[[1]]$index3),]$url_wiki <- b[[1]][which(!is.na(b[[1]]$index3)),]$url

# fill remaining missing Wikipedia/Wikidata information in a
a[[14]]$wikidata <- NA
a[[14]]$wikidata[c(4,22,29,40,41,68,81,
                   93,94,98,108,110,120,
                   128,132,145,167,174,177,
                   182,183,185,187,188,
                   195,197,203,209,215,
                   220,224,237,251,254,
                   255,256,259,261,272,
                   277,283,285,297,312,
                   317,321,323,329,332,
                   333,350,351,355,357,
                   360,364,366,367,373,
                   376,389,392,370,151,
                   23)] <- c("Q44307467","Q43841293","Q16170012","Q44106802","Q16175063",
                             "Q32946555","Q11909011", "Q17420698", "Q11912457", "Q16190203",
                             "Q16191436", "Q20004818", "Q12399503", "Q11905597",
                             "Q20533325", "Q20605104", "Q12390751", "Q43654865", "Q44628675",
                             "Q11940995", "Q12263209","Q11955787","Q43840826","Q44383687",
                             "Q44629750","Q44408730","Q43368332","Q43841192","Q44629913",
                             "Q11929168","Q11905897", "Q20004413", "Q44299150", "Q11922995",
                             "Q44307413", "Q11928846","Q44307180","Q44408673",
                             "Q12391120", "Q12391095","Q20004727","Q12391271", "Q20006775", "Q44629806",
                             "Q17195288", "Q44307671", "Q11944641", "Q44409009", "Q12393318", 
                             "Q9207377", "Q5483964", "Q11954630", "Q12390810","Q44408779",
                             "Q11935014", "Q11927710", "Q12390971","Q11929430", "Q20003376",
                             "Q20535892", "Q12397587", "Q20001648","Q11928965","Q20000694","Q13635303")
a[[14]]$url_wiki[c(10,11,17,28,30,32,33,
                   74,86,92,96,100,112,
                   121,152,169,170,192,206,
                   229,233,250,264,304,346,
                   391)] <- c("/wiki/Josep_Lluís_Albiñana", "/wiki/Felipe_Alcaraz",
                              "/wiki/José_Luis_Álvarez_y_Álvarez", "/wiki/Rafael_Arias-Salgado",
                              "/wiki/Josep_Arnau_i_Figuerola", "/wiki/Xabier_Arzalluz",
                              "/wiki/Emilio_Attard", "/wiki/Ignacio_Camuñas_Solís",
                              "/wiki/Íñigo_Cavero","/wiki/Salvador_Clotas", "/wiki/Asunción_Cruañes",
                              "/wiki/José_Ángel_Cuerda", "/wiki/Rafael_Escuredo",
                              "/wiki/Francisco_Fernández_Ordóñez", "/wiki/Joaquín_Garrigues_Walker",
                              "/wiki/Luis_de_Grandes", "/wiki/Felipe_Guardiola_Sellés",
                              "/wiki/Ernest_Lluch", "/wiki/Rodolfo_Martín_Villa",
                              "/wiki/Elena_María_Moreno_González", "/wiki/Arturo_Moya_Moreno",
                              "/wiki/José_Manuel_Otero", "/wiki/Ángel_Manuel_Perera_Calle",
                              "/wiki/Agustín_Rodríguez_Sahagún","/wiki/Carlos_Solchaga",
                              "/wiki/Virgilio_Zapatero")
a[[14]]$available <- ifelse(is.na(a[[14]]$url_wiki) & is.na(a[[14]]$wikidata), FALSE, TRUE)

a[[13]]$wikidata <- NA
subst_14_13 <- match(a[[13]][which(is.na(a[[13]]$name_wiki)),]$full, a[[14]]$full)
a[[13]][which(is.na(a[[13]]$name_wiki)),]$url_wiki <- a[[14]][subst_14_13,]$url_wiki
a[[13]][which(is.na(a[[13]]$name_wiki)),]$wikidata <- a[[14]][subst_14_13,]$wikidata
a[[13]]$wikidata[c(2,5,9,19,21,31,
                   55,59,67,79,81,
                   92,98,99,101,102,109,
                   120,131,132,133,139,151,
                   153,154,159,160,163,164,
                   170,185,186,189,208,212,
                   213,215,219,243,246,249,
                   255,262,265,279,284,287,
                   303,325,326,335,337,353,
                   354,366,368,376,380,390,
                   258,233,148)] <- c("Q44383597","Q16107185","Q44409085","Q44587502",
                                      "Q12390852","Q11919377","Q11928512","Q11926448",
                                      "Q11955786","Q6274123","Q44587165","Q43654539",
                                      "Q17206080","Q17420729","Q12382659","Q44628742",
                                      "Q1710559","Q11928092","Q6752595","Q3755516",
                                      "Q12388393", "Q6700636", "Q12388394", "Q19997788", 
                                      "Q5483410", "Q44628638", "Q44587226", "Q19257926", 
                                      "Q44298724", "Q44212959", "Q14087526", "Q29608631",
                                      "Q44604126", "Q7406425", "Q44522746","Q5042323", "Q43368145", 
                                      "Q44104873", "Q20004369", "Q44298945", "Q8210595",
                                      "Q43654051", "Q44628793", "Q44299121", "Q20534680", 
                                      "Q12383103", "Q17414258", "Q17036473", "Q43840854", 
                                      "Q44629837", "Q11928921", "Q11926850", "Q11955329", 
                                      "Q12403085", "Q44519458", "Q19301727", "Q11935764", 
                                      "Q19998255", "Q44298833","Q11037961","Q8965058","Q17420743")
a[[13]]$url_wiki[c(1,16,30,80,107,126,
                   202,294,350)] <- c("/wiki/Ana_María_Abascal_y_Calabria","/wiki/Jaume_Antich_i_Balada",
                                      "/wiki/Pablo_Beltrán_de_Heredia","/wiki/José_Luis_Corcuera",
                                      "/wiki/Iñaki_Esnaola", "/wiki/Francisco_Fuentes_Gallardo",
                                      "/wiki/Alberto_López_Fernández", "/wiki/Miguel_Ramón_Izquierdo",
                                      "/wiki/Fernando_Suárez_González")
a[[13]]$available <- ifelse(is.na(a[[13]]$url_wiki) & is.na(a[[13]]$wikidata), FALSE, TRUE)

a[[12]]$wikidata <- NA
a[[12]]$available <- ifelse(is.na(a[[12]]$url_wiki) & is.na(a[[12]]$wikidata), FALSE, TRUE)
subst_12_13 <- match(a[[12]][which(a[[12]]$available == FALSE),]$full, a[[13]]$full)
a[[12]][which(a[[12]]$available == FALSE),]$url_wiki <- a[[13]][subst_12_13,]$url_wiki
a[[12]][which(a[[12]]$available == FALSE),]$wikidata <- a[[13]][subst_12_13,]$wikidata
a[[12]]$available <- ifelse(is.na(a[[12]]$url_wiki) & is.na(a[[12]]$wikidata), FALSE, TRUE)
subst_12_14 <- match(a[[12]][which(a[[12]]$available == FALSE),]$full, a[[14]]$full)
a[[12]][which(a[[12]]$available == FALSE),]$url_wiki <- a[[14]][subst_12_14,]$url_wiki
a[[12]][which(a[[12]]$available == FALSE),]$wikidata <- a[[14]][subst_12_14,]$wikidata
a[[12]]$available <- ifelse(is.na(a[[12]]$url_wiki) & is.na(a[[12]]$wikidata), FALSE, TRUE)
a[[12]]$wikidata[c(7,10,34,39,44,48,52,55,58,68,
                   70,80,85,87,98,104,107,109,120,
                   129,131,132,133,134,138,140,
                   141,154,157,160,166,167,170,
                   171,175,177,178,186,193,194,
                   198,207,209,212,213,218,
                   219,220,221,223,224,225,
                   227,228,229,231,234,236,
                   239,244,245,246,252,256,
                   259,260,266,267,271,278,
                   279,
                   285,286,290,291,295,297,
                   299,300,303,304,311,314,
                   316,324,325,329,333,335,
                   336,337,
                   338,340,342,346,347,356,
                   363,364,365,366,367,368,
                   371,373,375,377,382,383,
                   385,387,392)] <- c("Q17024012", "Q20005080", "Q44634202", 
                                      "Q44521917", "Q11928280", "Q21001123", 
                                      "Q12390855", "Q44408487", "Q11697862",
                                      "Q11946749", "Q20100402", "Q16188621", "Q11928533", 
                                      "Q20006182", "Q12390857", "Q12392711", 
                                      "Q17297221", "Q19301658", "Q19300955", 
                                      "Q44213099", "Q43655493", "Q44409247", 
                                      "Q44299055", "Q44307228", "Q17420743",
                                      "Q44383627",
                                      "Q44408594","Q44298646","Q44588081","Q20003253",
                                      "Q44520484","Q44588636","Q14086254","Q44623244",
                                      "Q43841061","Q11913370","Q44298805","Q44307698",
                                      "Q44408532","Q44604309","Q44383733","Q12382497",
                                      "Q43654944","Q44298695","Q43841144","Q11944431",
                                      "Q20100796","Q43841368","Q44409212","Q44307772","Q44307274","Q44383701",
                                      "Q44107367","Q44522056","Q44588719","Q28082067","Q44306843","Q44307794",
                                      "Q11927846","Q44307384","Q44298592","Q12393070","Q5676003","Q44299028",
                                      "Q11039661","Q5942240","Q44307727","Q44307080","Q44623146","Q43841088",
                                      "Q44623567",
                                      "Q44630333","Q11919346","Q44603668","Q44307895","Q44299090","Q44298864",
                                      "Q44307247","Q30117955","Q43840332","Q20533102","Q44307950","Q44522999",
                                      "Q44629208","Q20004436","Q44307973","Q44629867","Q44383655","Q44519431",
                                      "Q11922838","Q43655522",
                                      "Q44408863","Q5938108","Q11946798","Q44603770","Q44104757","Q44523025",
                                      "Q44298620","Q44588189","Q44518424","Q44520385","Q44298670","Q42961668",
                                      "Q44298569","Q12392741","Q12390976","Q44307356","Q11941074","Q44603913",
                                      "Q44409700","Q4750451","Q44408745")
a[[12]]$url_wiki[c(9,19,72,164,204,216)] <- c("/wiki/Iñaki_Aldekoa", "/wiki/Mario_Amilivia", "/wiki/Jordi_Casas", 
                                              "/wiki/Francesc_Homs_i_Ferret", "/wiki/José_María_Maravall", 
                                              "/wiki/Manuel_Martínez_Núñez")
a[[12]]$available <- ifelse(is.na(a[[12]]$url_wiki) & is.na(a[[12]]$wikidata), FALSE, TRUE)
na_names_12 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/3rd") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_text()
na_urls_12 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/3rd") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_attr("href") %>%
  str_replace("/wiki/", "")
subst_12_na <- match(a[[12]][which(a[[12]]$available == FALSE),]$full, na_names_12)
a[[12]][which(a[[12]]$available == FALSE),]$wikidata <- na_urls_12[subst_12_na]
a[[12]]$available <- ifelse(is.na(a[[12]]$url_wiki) & is.na(a[[12]]$wikidata), FALSE, TRUE)

na_names_14 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/1st") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_text()
na_urls_14 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/1st") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_attr("href") %>%
  str_replace("/wiki/", "")
subst_14_na <- match(a[[14]][which(a[[14]]$available == FALSE),]$full, na_names_14)
a[[14]][which(a[[14]]$available == FALSE),]$wikidata <- na_urls_14[subst_14_na]
a[[14]]$available <- ifelse(is.na(a[[14]]$url_wiki) & is.na(a[[14]]$wikidata), FALSE, TRUE)

na_names_13 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/2nd") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_text()
na_urls_13 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/2nd") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_attr("href") %>%
  str_replace("/wiki/", "")
subst_13_na <- match(a[[13]][which(a[[13]]$available == FALSE),]$full, na_names_13)
a[[13]][which(a[[13]]$available == FALSE),]$wikidata <- na_urls_13[subst_13_na]
a[[13]]$available <- ifelse(is.na(a[[13]]$url_wiki) & is.na(a[[13]]$wikidata), FALSE, TRUE)

a[[11]]$wikidata <- NA
a[[11]]$available <- ifelse(is.na(a[[11]]$url_wiki) & is.na(a[[11]]$wikidata), FALSE, TRUE)
na_names_11 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/4th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_text()
na_urls_11 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/4th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_attr("href") %>%
  str_replace("/wiki/", "")
subst_11_na <- match(a[[11]][which(a[[11]]$available == FALSE),]$full, na_names_11)
a[[11]][which(a[[11]]$available == FALSE),]$wikidata <- na_urls_11[subst_11_na]
a[[11]]$available <- ifelse(is.na(a[[11]]$url_wiki) & is.na(a[[11]]$wikidata), FALSE, TRUE)
subst_11_12 <- match(a[[11]][which(a[[11]]$available == FALSE),]$full, a[[12]]$full)
a[[11]][which(a[[11]]$available == FALSE),]$wikidata <- a[[12]][subst_11_12,]$wikidata
a[[11]]$available <- ifelse(is.na(a[[11]]$url_wiki) & is.na(a[[11]]$wikidata), FALSE, TRUE)
a[[11]]$wikidata[c(10,34,45,128,152,
                   185,190,232,234,
                   249,277,281,289,372)] <- c("Q3025982", "Q11927771","Q11922699","Q43655548","Q16518059",
                                              "Q11919319","Q12257301","Q14119414","Q19300649","Q5943223",
                                              "Q6293673","Q5667095","Q20534680","Q12393103")
a[[11]]$available <- ifelse(is.na(a[[11]]$url_wiki) & is.na(a[[11]]$wikidata), FALSE, TRUE)

a[[10]]$wikidata <- NA
a[[10]]$available <- ifelse(is.na(a[[10]]$url_wiki) & is.na(a[[10]]$wikidata), FALSE, TRUE)
na_names_10 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/5th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_text()
na_urls_10 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/5th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_attr("href") %>%
  str_replace("/wiki/", "")
subst_10_na <- match(a[[10]][which(a[[10]]$available == FALSE),]$full, na_names_10)
a[[10]][which(a[[10]]$available == FALSE),]$wikidata <- na_urls_10[subst_10_na]

a[[10]]$available <- ifelse(is.na(a[[10]]$url_wiki) & is.na(a[[10]]$wikidata), FALSE, TRUE)
subst_10_11 <- match(a[[10]][which(a[[10]]$available == FALSE),]$full, a[[11]]$full)
a[[10]][which(a[[10]]$available == FALSE),]$wikidata <- a[[11]][subst_10_11,]$wikidata
a[[10]]$available <- ifelse(is.na(a[[10]]$url_wiki) & is.na(a[[10]]$wikidata), FALSE, TRUE)
a[[10]]$wikidata[c(21,61,66,90,107,113,114,
                   117,123,157,161,174,183,
                   250,278,285,298,343,359,
                   370,371,405)] <- c("Q5415002","Q12392682","Q16183688","Q3186359","Q12390857",
                                      "Q43653983","Q3189830","Q12402633","Q42911182","Q20000703",
                                      "Q11172477","Q17030992","Q20876038","Q2831417","Q12391310",
                                      "Q20005674","Q11904826","Q20873248","Q25509305","Q19300967",
                                      "Q11927749","Q44523238")
a[[10]]$available <- ifelse(is.na(a[[10]]$url_wiki) & is.na(a[[10]]$wikidata), FALSE, TRUE)

a[[9]]$wikidata <- NA
a[[9]]$available <- ifelse(is.na(a[[9]]$url_wiki) & is.na(a[[9]]$wikidata), FALSE, TRUE)
na_names_9 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/6th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_text()
na_urls_9 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/6th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_attr("href") %>%
  str_replace("/wiki/", "")
subst_9_na <- match(a[[9]][which(a[[9]]$available == FALSE),]$full, na_names_9)
a[[9]][which(a[[9]]$available == FALSE),]$wikidata <- na_urls_9[subst_9_na]

a[[9]]$available <- ifelse(is.na(a[[9]]$url_wiki) & is.na(a[[9]]$wikidata), FALSE, TRUE)
subst_9_10 <- match(a[[9]][which(a[[9]]$available == FALSE),]$full, a[[10]]$full)
a[[9]][which(a[[9]]$available == FALSE),]$wikidata <- a[[10]][subst_9_10,]$wikidata
a[[9]]$available <- ifelse(is.na(a[[9]]$url_wiki) & is.na(a[[9]]$wikidata), FALSE, TRUE)
a[[9]]$wikidata[c(23,80,105,118,126,130,
                  147,148,158,172,193,250,
                  269,288,297,298,306,308,
                  311,332,374,385,397)] <- c("Q5415002","Q16019323","Q16189681","Q5638508","Q43653983",
                                             "Q240104","Q12390896","Q11918052","Q9209242","Q5993152",
                                             "Q14086254","Q6119979","Q2984396","Q6293673","Q19289556",
                                             "Q11904826","Q20534680","Q43654344","Q11916139","Q942949",
                                             "Q2939561","Q44409659","Q12393103")
a[[9]]$available <- ifelse(is.na(a[[9]]$url_wiki) & is.na(a[[9]]$wikidata), FALSE, TRUE)

a[[8]]$wikidata <- NA
a[[8]]$available <- ifelse(is.na(a[[8]]$url_wiki) & is.na(a[[8]]$wikidata), FALSE, TRUE)
na_names_8 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/7th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_text()
na_urls_8 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/7th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_attr("href") %>%
  str_replace("/wiki/", "")
subst_8_na <- match(a[[8]][which(a[[8]]$available == FALSE),]$full, na_names_8)
a[[8]][which(a[[8]]$available == FALSE),]$wikidata <- na_urls_8[subst_8_na]

a[[8]]$available <- ifelse(is.na(a[[8]]$url_wiki) & is.na(a[[8]]$wikidata), FALSE, TRUE)
subst_8_9 <- match(a[[8]][which(a[[8]]$available == FALSE),]$full, a[[9]]$full)
a[[8]][which(a[[8]]$available == FALSE),]$wikidata <- a[[9]][subst_8_9,]$wikidata
a[[8]]$available <- ifelse(is.na(a[[8]]$url_wiki) & is.na(a[[8]]$wikidata), FALSE, TRUE)
a[[8]]$wikidata[c(11,33,77,97,107,125,137,
                  145,182,233,239,253,255,
                  284,318,332,337,346,367,
                  368,374,383,395)] <- c("Q21001068","Q12385329","Q14119558","Q11951585",
                                         "Q12385870","Q11906140","Q11905379","Q14087112","Q20003253",
                                         "Q11944493","Q19998156","Q2749842","Q14039521",
                                         "Q6782095","Q11936748","Q44630370","Q6161311",
                                         "Q44634013","Q11927931","Q20968342", "Q5751710",
                                         "Q3108429","Q14072455")
a[[8]]$available <- ifelse(is.na(a[[8]]$url_wiki) & is.na(a[[8]]$wikidata), FALSE, TRUE)

a[[7]]$wikidata <- NA
a[[7]]$available <- ifelse(is.na(a[[7]]$url_wiki) & is.na(a[[7]]$wikidata), FALSE, TRUE)
na_names_7 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/8th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_text()
na_urls_7 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/8th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_attr("href") %>%
  str_replace("/wiki/", "")
subst_7_na <- match(a[[7]][which(a[[7]]$available == FALSE),]$full, na_names_7)
a[[7]][which(a[[7]]$available == FALSE),]$wikidata <- na_urls_7[subst_7_na]

a[[7]]$available <- ifelse(is.na(a[[7]]$url_wiki) & is.na(a[[7]]$wikidata), FALSE, TRUE)
subst_7_8 <- match(a[[7]][which(a[[7]]$available == FALSE),]$full, a[[8]]$full)
a[[7]][which(a[[7]]$available == FALSE),]$wikidata <- a[[7]][subst_7_8,]$wikidata
a[[7]]$available <- ifelse(is.na(a[[7]]$url_wiki) & is.na(a[[7]]$wikidata), FALSE, TRUE)
a[[7]]$wikidata[c(6,39,67,81,98,99,
                  113,114,115,128,133,
                  149,163,170,201,217,
                  239,255,257,272,275,
                  283,299,304,310,311,
                  328,335,337,352,370,
                  389)] <- c("Q14070625", "Q3163131", "Q11922720", "Q2973310", "Q12385870", "Q6136317",
                             "Q19290544", "Q2880992", "Q11919307", "Q11905379", "Q44634097", 
                             "Q44634058", "Q11928699", "Q20003253", "Q12383145", "Q11921210", 
                             "Q6119979", "Q11935442", "Q6173697", "Q6782095", "Q12391013",
                             "Q20533203", "Q20534958", "Q9012034", "Q11928352", "Q6781915",
                             "Q12393225", "Q11955692","Q12393033","Q6280531", "Q44409659",
                             "Q12388975")
a[[7]]$available <- ifelse(is.na(a[[7]]$url_wiki) & is.na(a[[7]]$wikidata), FALSE, TRUE)

a[[6]]$wikidata <- NA
a[[6]]$available <- ifelse(is.na(a[[6]]$url_wiki) & is.na(a[[6]]$wikidata), FALSE, TRUE)
na_names_6 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/9th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_text()
na_urls_6 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/9th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_attr("href") %>%
  str_replace("/wiki/", "")
subst_6_na <- match(a[[6]][which(a[[6]]$available == FALSE),]$full, na_names_6)
a[[6]][which(a[[6]]$available == FALSE),]$wikidata <- na_urls_6[subst_6_na]

a[[6]]$available <- ifelse(is.na(a[[6]]$url_wiki) & is.na(a[[6]]$wikidata), FALSE, TRUE)
subst_6_7 <- match(a[[6]][which(a[[6]]$available == FALSE),]$full, a[[7]]$full)
a[[6]][which(a[[6]]$available == FALSE),]$wikidata <- a[[6]][subst_6_7,]$wikidata
a[[6]]$available <- ifelse(is.na(a[[6]]$url_wiki) & is.na(a[[6]]$wikidata), FALSE, TRUE)
a[[6]]$wikidata[c(4,26,35,36,37,
                  43,56,58,68,76,
                  78,84,109,128,129,
                  130,137,142,143,164,
                  175,179,203,218,227,
                  252,266,279,288,299,
                  324,333,335,347,350,
                  362,368,397)] <- c("Q32848820", "Q17024032", "Q4714487", "Q453730", "Q6035010",
                                     "Q3163131", "Q5445554", "Q43986871", "Q522506", "Q11922720",
                                     "Q16184300", "Q20532788", "Q12385870", "Q11919307", "Q11912494",
                                     "Q14092781", "Q11929374", "Q11905379", "Q19289667", "Q44634058", 
                                     "Q6048650", "Q12402589", "Q11923367", "Q12383145", "Q14067511", 
                                     "Q11935417", "Q11935442", "Q11949944", "Q8775568", "Q11936741", 
                                     "Q5950803", "Q30117955", "Q20005786", "Q17036720", "Q11955692", 
                                     "Q6280531", "Q3108429", "Q14083376")
a[[6]]$available <- ifelse(is.na(a[[6]]$url_wiki) & is.na(a[[6]]$wikidata), FALSE, TRUE)

a[[5]]$wikidata <- NA
a[[5]]$available <- ifelse(is.na(a[[5]]$url_wiki) & is.na(a[[5]]$wikidata), FALSE, TRUE)
na_names_5 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/10th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_text()
na_urls_5 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/10th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_attr("href") %>%
  str_replace("/wiki/", "")
subst_5_na <- match(a[[5]][which(a[[5]]$available == FALSE),]$full, na_names_5)
a[[5]][which(a[[5]]$available == FALSE),]$wikidata <- na_urls_5[subst_5_na]

a[[5]]$available <- ifelse(is.na(a[[5]]$url_wiki) & is.na(a[[5]]$wikidata), FALSE, TRUE)
subst_5_6 <- match(a[[5]][which(a[[5]]$available == FALSE),]$full, a[[6]]$full)
a[[5]][which(a[[5]]$available == FALSE),]$wikidata <- a[[5]][subst_5_6,]$wikidata
a[[5]]$available <- ifelse(is.na(a[[5]]$url_wiki) & is.na(a[[5]]$wikidata), FALSE, TRUE)
a[[5]]$wikidata[c(27,40,60,64,67,
                  78,83,99,132,134,
                  138,145,147,160,162,
                  178,214,269,297,302,
                  307,371,379,390,432)] <- c("Q12262875", "Q14026688", "Q43986871", "Q14085945", "Q14024903", 
                                             "Q16184300", "Q976844", "Q12390253", "Q19290544", "Q11919307", 
                                             "Q5493388", "Q11905379", "Q11906863", "Q20963353", "Q14077251", 
                                             "Q14087284", "Q12397261", "Q6119979", "Q25510057", "Q14076095", 
                                             "Q12399258", "Q20968342", "Q6280531", "Q2904994", "Q14089173")
a[[5]]$available <- ifelse(is.na(a[[5]]$url_wiki) & is.na(a[[5]]$wikidata), FALSE, TRUE)

a[[4]]$wikidata <- NA
a[[4]]$available <- ifelse(is.na(a[[4]]$url_wiki) & is.na(a[[4]]$wikidata), FALSE, TRUE)
na_names_4 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/11th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_text()
na_urls_4 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/11th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_attr("href") %>%
  str_replace("/wiki/", "")
subst_4_na <- match(a[[4]][which(a[[4]]$available == FALSE),]$full, na_names_4)
a[[4]][which(a[[4]]$available == FALSE),]$wikidata <- na_urls_4[subst_4_na]

a[[4]]$available <- ifelse(is.na(a[[4]]$url_wiki) & is.na(a[[4]]$wikidata), FALSE, TRUE)
subst_4_5 <- match(a[[4]][which(a[[4]]$available == FALSE),]$full, a[[5]]$full)
a[[4]][which(a[[4]]$available == FALSE),]$wikidata <- a[[4]][subst_4_5,]$wikidata
a[[4]]$available <- ifelse(is.na(a[[4]]$url_wiki) & is.na(a[[4]]$wikidata), FALSE, TRUE)
a[[4]]$wikidata[c(8,23,47,48,87,
                  109,132,174,229,285,
                  321)] <- c("Q14121319", "Q22809692", "Q43986871", "Q21816265", "Q14085307", 
                             "Q11936733", "Q14089321", "Q5683166", "Q27756507", "Q25509971", 
                             "Q19289058")
a[[4]]$available <- ifelse(is.na(a[[4]]$url_wiki) & is.na(a[[4]]$wikidata), FALSE, TRUE)

a[[3]]$wikidata <- NA
a[[3]]$available <- ifelse(is.na(a[[3]]$url_wiki) & is.na(a[[3]]$wikidata), FALSE, TRUE)
na_names_3 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/12th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_text()
na_urls_3 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/12th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_attr("href") %>%
  str_replace("/wiki/", "")
subst_3_na <- match(a[[3]][which(a[[3]]$available == FALSE),]$full, na_names_3)
a[[3]][which(a[[3]]$available == FALSE),]$wikidata <- na_urls_3[subst_3_na]

a[[3]]$available <- ifelse(is.na(a[[3]]$url_wiki) & is.na(a[[3]]$wikidata), FALSE, TRUE)
subst_3_4 <- match(a[[3]][which(a[[3]]$available == FALSE),]$full, a[[4]]$full)
a[[3]][which(a[[3]]$available == FALSE),]$wikidata <- a[[3]][subst_3_4,]$wikidata
a[[3]]$available <- ifelse(is.na(a[[3]]$url_wiki) & is.na(a[[3]]$wikidata), FALSE, TRUE)
a[[3]]$wikidata[c(13,20,89,123,127,
                  239,250,253,287,318,
                  350,353,379)] <- c("Q14121319", "Q22809692", "Q2973310", "Q11955718", "Q11936733", 
                                     "Q10949778", "Q6119979", "Q27756507", "Q44588109", "Q25509971", 
                                     "Q5939451", "Q43992146", "Q56072343")
a[[3]]$available <- ifelse(is.na(a[[3]]$url_wiki) & is.na(a[[3]]$wikidata), FALSE, TRUE)

a[[2]]$wikidata <- NA
a[[2]]$available <- ifelse(is.na(a[[2]]$url_wiki) & is.na(a[[2]]$wikidata), FALSE, TRUE)
na_names_2 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/13th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_text()
na_urls_2 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/13th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_attr("href") %>%
  str_replace("/wiki/", "")
subst_2_na <- match(a[[2]][which(a[[2]]$available == FALSE),]$full, na_names_2)
a[[2]][which(a[[2]]$available == FALSE),]$wikidata <- na_urls_2[subst_2_na]

a[[2]]$available <- ifelse(is.na(a[[2]]$url_wiki) & is.na(a[[2]]$wikidata), FALSE, TRUE)
subst_2_3 <- match(a[[2]][which(a[[2]]$available == FALSE),]$full, a[[3]]$full)
a[[2]][which(a[[2]]$available == FALSE),]$wikidata <- a[[2]][subst_2_3,]$wikidata
a[[2]]$available <- ifelse(is.na(a[[2]]$url_wiki) & is.na(a[[2]]$wikidata), FALSE, TRUE)
a[[2]]$wikidata[c(6,7,10,21,56,
                  57,65,99,101,113,
                  116,138,142,151,153,
                  154,223,229,238,255,
                  317,320,322,327,331,
                  332)] <- c("Q12263472", "Q62054182", "Q65275420", "Q63861758", "Q66116008", 
                             "Q62092107", "Q63436578", "Q14084147", "Q65275034", "Q2223490", 
                             "Q12255661", "Q21479993", "Q21857951", "Q28872545", "Q63372854", 
                             "Q62631116", "Q6119979", "Q12401411", "Q14089251", "Q65275067", 
                             "Q14124051", "Q11954626", "Q12267336", "Q19826434", "Q4684282", 
                             "Q27755620")
a[[2]]$available <- ifelse(is.na(a[[2]]$url_wiki) & is.na(a[[2]]$wikidata), FALSE, TRUE)

a[[1]]$wikidata <- NA
a[[1]]$available <- ifelse(is.na(a[[1]]$url_wiki) & is.na(a[[1]]$wikidata), FALSE, TRUE)
na_names_1 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/14th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_text()
na_urls_1 <- read_html("https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician/Spain/data/Deputies/14th") %>%
  html_nodes(xpath = "//tbody/tr/td[1]/a") %>%
  html_attr("href") %>%
  str_replace("/wiki/", "")
subst_1_na <- match(a[[1]][which(a[[1]]$available == FALSE),]$full, na_names_1)
a[[1]][which(a[[1]]$available == FALSE),]$wikidata <- na_urls_1[subst_1_na]

a[[1]]$available <- ifelse(is.na(a[[1]]$url_wiki) & is.na(a[[1]]$wikidata), FALSE, TRUE)
subst_1_2 <- match(a[[1]][which(a[[1]]$available == FALSE),]$full, a[[2]]$full)
a[[1]][which(a[[1]]$available == FALSE),]$wikidata <- a[[1]][subst_1_2,]$wikidata
a[[1]]$available <- ifelse(is.na(a[[1]]$url_wiki) & is.na(a[[1]]$wikidata), FALSE, TRUE)
a[[1]]$wikidata[c(6,8,27,30,56,
                  64,65,81,104,106,
                  115,141,148,150,163,
                  167,175,183,185,188,
                  204,217,227,266,292,
                  312,334)] <- c("Q12263472", "Q62054182", "Q14022564", "Q66498531", "Q80082826", 
                                 "Q66116008", "Q62092107", "Q74579112", "Q14084147", "Q65275034", 
                                 "Q44307441", "Q74579135", "Q63372854", "Q62631116", "Q5698658", 
                                 "Q20877110" ,"Q63862217", "Q62018287", "Q66547767", "Q14117863", 
                                 "Q27755511", "Q6119979", "Q14089251", "Q5941643","Q74579104",
                                 "Q11954626", "Q19301601")
a[[1]]$available <- ifelse(is.na(a[[1]]$url_wiki) & is.na(a[[1]]$wikidata), FALSE, TRUE)

# get Wikipedia URLs for those with wikidataid only, for various langcodes
a <- a %>% # spanish Wikipedia
  purrr::map(~ {
    .x$url_wiki2 <-  wikiURLS(ids = .x$wikidata, langcode = "eswiki")
    .x$url_wiki <- ifelse(is.na(.x$url_wiki), .x$url_wiki2, .x$url_wiki)
    .x <- .x %>% select(-url_wiki2)
  })
a <- a %>% # Catalan Wikipedia
  purrr::map(~ {
    .x$url_wiki2 <-  wikiURLS(ids = .x$wikidata, langcode = "cawiki")
    .x$url_wiki <- ifelse(is.na(.x$url_wiki), .x$url_wiki2, .x$url_wiki)
    .x <- .x %>% select(-url_wiki2)
  })
a <- a %>% # Galician Wikipedia
  purrr::map(~ {
    .x$url_wiki2 <-  wikiURLS(ids = .x$wikidata, langcode = "glwiki")
    .x$url_wiki <- ifelse(is.na(.x$url_wiki), .x$url_wiki2, .x$url_wiki)
    .x <- .x %>% select(-url_wiki2)
  })
a <- a %>% # English Wikipedia
  purrr::map(~ {
    .x$url_wiki2 <-  wikiURLS(ids = .x$wikidata, langcode = "enwiki")
    .x$url_wiki <- ifelse(is.na(.x$url_wiki), .x$url_wiki2, .x$url_wiki)
    .x <- .x %>% select(-url_wiki2)
  })
a <- a %>% # Portuguese Wikipedia
  purrr::map(~ {
    .x$url_wiki2 <-  wikiURLS(ids = .x$wikidata, langcode = "ptwiki")
    .x$url_wiki <- ifelse(is.na(.x$url_wiki), .x$url_wiki2, .x$url_wiki)
    .x <- .x %>% select(-url_wiki2)
  })
a <- a %>% # French Wikipedia
  purrr::map(~ {
    .x$url_wiki2 <-  wikiURLS(ids = .x$wikidata, langcode = "frwiki")
    .x$url_wiki <- ifelse(is.na(.x$url_wiki), .x$url_wiki2, .x$url_wiki)
    .x <- .x %>% select(-url_wiki2)
  })
a <- a %>% # Esperanto Wikipedia
  purrr::map(~ {
    .x$url_wiki2 <-  wikiURLS(ids = .x$wikidata, langcode = "eowiki")
    .x$url_wiki <- ifelse(is.na(.x$url_wiki), .x$url_wiki2, .x$url_wiki)
    .x <- .x %>% select(-url_wiki2)
  })
a <- a %>%
  purrr::map(~ {
  .x$url_wiki <- ifelse(is.na(.x$wikidata) & .x$available == TRUE, paste0("https://es.wikipedia.org", .x$url_wiki), .x$url_wiki)
  return(.x)
})
a[[3]][20,]$url_wiki <- paste0("https://es.wikipedia.org", a[[3]][20,]$url_wiki)
a[[12]][21,]$url_wiki <- "https://ca.wikipedia.org/wiki/Jaume_Antich_i_Balada"
a[[13]][16,]$url_wiki <- "https://ca.wikipedia.org/wiki/Jaume_Antich_i_Balada"

# retrieve wikipedia page and wikidata ids ----------------------------------------------
ids <- a %>%
  purrr::map(~{ 
    ids <- wikiIDs(.x$url_wiki)
    ids$project <- str_extract(.x$url_wiki, "(?<=//)[[:alpha:]]{2}")
    ids$wikidata <- .x$wikidata
    return(ids)
  })
spain_ids <- ids %>%
  purrr::map(~{
    .x$wikidataid <- ifelse(is.na(.x$wikidataid), .x$wikidata, .x$wikidataid)
    .x <- .x %>% select(-wikidata)
  })
saveRDS(spain_ids, "./data/spain_ids")

# format ------------------------------------------------
spain_core <- a %>%
  purrr::imap(~{
    .x$name <- .x$full
    .x$group <- str_extract(.x$group, "(?<=\\().+(?=\\))") %>%
      str_remove("\\).+")
    .x$constituency <- str_remove(.x$constituency, "Diputad(o|a) por ")
    .x$start_time <- str_extract(.x$start_time, "[[:digit:]]{2}/[[:digit:]]{2}/[[:digit:]]{4}") %>%
      lubridate::dmy()
    .x$end_time <- str_extract(.x$end_time, "[[:digit:]]{2}/[[:digit:]]{2}/[[:digit:]]{4}") %>%
      lubridate::dmy()
    .x$service <- .x$end_time %>%
      subtract(.x$start_time) %>%
      as.integer
    .x$session <- as.integer(14:1)[.y]
    .x$country <- "ESP"
    .x$session_start <- c("2019-12-03", "2019-05-21", "2016-07-19", "2016-01-13", "2011-12-13", 
                          "2008-04-01", "2004-04-02", "2000-04-05", "1996-03-27", "1993-06-29", 
                          "1989-11-21", "1986-07-15", "1982-11-18", "1979-03-23")[.y]
    .x$session_end <- c(NA, "2019-12-03", "2019-05-21", "2016-07-19", "2016-01-13", "2011-12-13", 
                        "2008-04-01", "2004-04-02", "2000-04-05", "1996-03-27", "1993-06-29", 
                        "1989-11-21", "1986-07-15", "1982-11-18")[.y]
    return(.x)
  })
saveRDS(spain_core, "./data/spain_core")

# bind ids to core data and collapse ----------------------------------------------------
spain <- bind_rows(Map(cbind, spain_ids, spain_core))
saveRDS(spain, "./data/spain")

# retrieve wikipedia revision histories -------------------------------------------------
spain$pageid_unique <- str_c(spain$pageid, "-", spain$project)
spain_wikipedia <- spain %>%
  dplyr::select(pageid, project, pageid_unique) %>%
  dplyr::distinct(pageid_unique, .keep_all = TRUE) %>%
  tidyr::drop_na()
saveRDS(spain_wikipedia, "./data/spain_wikipedia") 
spain_history <- bind_rows(mapply(FUN = wikiHist, 
                                  pageid = spain_wikipedia$pageid,
                                  project = paste0(spain_wikipedia$project, ".wikipedia"), 
                                  uniqueid = spain_wikipedia$pageid_unique, SIMPLIFY = FALSE))
saveRDS(spain_history, "./data/spain_history")

# retrieve undirected wikipedia urls/titles ---------------------------------------------
spain_title <- undirectedTitle(pageid = spain_wikipedia$pageid,
                               project = paste0(spain_wikipedia$project, ".wikipedia"),
                               uniqueid = spain_wikipedia$pageid_unique)
saveRDS(spain_title, "./data/spain_title")


# retrieve wikipedia user traffic -------------------------------------------------------
spain_traffic <- wikiTrafficNew(data = spain_title, 
                                project = spain_wikipedia$project,
                                uniqueid = TRUE)
saveRDS(spain_traffic, "./data/spain_traffic")

# retrieve Wikidata entities ------------------------------------------------------------
spain_entities <- get_item(id = na.omit(unique(spain$wikidataid)))
saveRDS(austria_entities, "./data/austria_entities")

# retrieve sex --------------------------------------------------------------------------
spain_sex <- wikiData(item = na.omit(unique(spain$wikidataid)), entity = spain_entities, 
                      property = "P21")
spain_sex$sex <- ifelse(spain_sex$male == TRUE, "male",
                  ifelse(spain_sex$female == TRUE, "female", NA))
spain_sex <- spain_sex[,-c(2,3)]
saveRDS(spain_sex, "./data/spain_sex")

# retrieve religion ---------------------------------------------------------------------
spain_religion <- wikiData(item = na.omit(unique(spain$wikidataid)), entity = spain_entities, 
                      property = "P140")
spain_religion$religion <- replace(NA, rowSums(spain_religion[,c(2,6,8)]) >= 1,
                                  "catholicism") %>%
  replace(., rowSums(spain_religion[,c(3)]) >= 1,
          "protestantism evangelical") %>% 
  replace(., rowSums(spain_religion[,c(4)]) >= 1,
          "agnosticism") %>%
  replace(., rowSums(spain_religion[,c(5)]) >= 1,
          "atheism") %>%
  replace(., rowSums(spain_religion[,c(7)]) >= 1,
          "judaism")
spain_religion <- spain_religion[,c(1,9)]
saveRDS(spain_religion, "./data/spain_religion")

# retrieve and format birth dates -------------------------------------------------------
spain_birth <- wikiData(item = na.omit(unique(spain$wikidataid)), entity = spain_entities, 
                        date = TRUE, property = "P569")
spain_birth$date <- spain_birth$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(spain_birth, "./data/spain_birth")

# retrieve and format death dates -------------------------------------------------------
spain_death <- wikiData(item = na.omit(unique(spain$wikidataid)), entity = spain_entities, 
                          date = TRUE, property = "P570")
spain_death$date <- spain_death$date %>% str_replace_all("\\+|T.+", "") %>%
  str_replace_all("-00", "-01") %>% as.POSIXct(tz = "UTC")
saveRDS(spain_death, "./data/spain_death")

# retrieve and format birth places ------------------------------------------------------
spain_birthplace <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                             entity = spain_entities, location = TRUE, 
                             property = "P19")
spain_birthplace$lat <- round(spain_birthplace$lat, digit = 5)
spain_birthplace$lon <- round(spain_birthplace$lon, digit = 5)
spain_birthplace$birthplace <- str_c(spain_birthplace$lat, ",",
                                       spain_birthplace$lon)
spain_birthplace <- spain_birthplace[,c(1,4)]
saveRDS(spain_birthplace, "./data/spain_birthplace")

# retrieve and format death places ------------------------------------------------------
spain_deathplace <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                               entity = spain_entities, location = TRUE,
                               property = "P20")
spain_deathplace$lat <- round(spain_deathplace$lat, digit = 5)
spain_deathplace$lon <- round(spain_deathplace$lon, digit = 5)
spain_deathplace$deathplace <- str_c(spain_deathplace$lat, ",",
                                     spain_deathplace$lon)
spain_deathplace <- spain_deathplace[,c(1,4)]
saveRDS(spain_deathplace, "./data/spain_deathplace")

# retrieve and format social media data -------------------------------------------------
spain_twitter <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                         entity = spain_entities,
                         property = "P2002", serial = TRUE)
spain_facebook <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                          entity = spain_entities,
                          property = "P2013", serial = TRUE)
spain_youtube <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                         entity = spain_entities,
                         property = "P2397", serial = TRUE)
spain_instagram <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                           entity = spain_entities,
                           property = "P2003", serial = TRUE)
#spain_linkedin <- wikiData(item = na.omit(unique(spain$wikidataid)), 
#                          entity = spain_entities,
#                          property = "P2035", serial = TRUE)
spain_website <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                         entity = spain_entities,
                         property = "P856", serial = TRUE)
spain_social <- full_join(x = spain_twitter, y = spain_facebook, by = "wikidataid") %>%
  full_join(x = ., y = spain_youtube, by = "wikidataid") %>%
  full_join(x = ., y = spain_instagram, by = "wikidataid") %>%
  full_join(x = ., y = spain_website, by = "wikidataid")
names(spain_social) <- c("wikidataid", "twitter", "facebook", "youtube",
                        "instagram", "website")
saveRDS(spain_social, "./data/spain_social")

# retrieve and format portrait data -------------------------------------------------------
spain_images <- imageUrl(pageid = spain_wikipedia$pageid, 
                         project = paste0(spain_wikipedia$project, ".wikipedia"),
                         uniqueid = spain_wikipedia$pageid_unique)
colnames(spain_images)[1] <- "pageid_unique"
# give unique missing-pageid to those who do not have a pageid
spain$miss1 <-  as.integer(as.factor(ifelse(is.na(spain$wikidataid), spain$name, NA)))  
spain$miss2 <- as.integer(as.factor(ifelse(is.na(spain$pageid), 
                                           paste0(spain$wikidataid, spain$miss1), NA)))
spain$pageid_unique <- ifelse(is.na(spain$pageid_unique), str_c(spain$miss2, "-miss"), spain$pageid_unique)
saveRDS(spain, "./data/spain")
# add official portait urls
spain_images <- left_join(spain[,c(24,10)], spain_images, by = "pageid_unique")
colnames(spain_images)[c(1,2)] <- c("pageid", "official_url")
spain_images <- spain_images %>%
  dplyr::distinct(pageid, .keep_all = TRUE)
saveRDS(spain_images, "./data/spain_faces")

# retrieve and format ids ---------------------------------------------------------------
spain_parlid <- data.frame(wikidataid = ifelse(is.na(spain$wikidataid),
                                               paste0(spain$pageid_unique, "-wd"),
                                               spain$wikidataid),
                           value = str_c("http://www.congreso.es/portal/page/portal/Congreso/Congreso/Diputados/BusqForm?_piref73_1333155_73_1333154_1333154.next_page=/wc/fichaDiputado?idDiputado=",
                                         str_extract(spain$portrait, "[[:digit:]]+(?=_)"),
                                         "&idLegislatura=",
                                         spain$session))
spain_gndid <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                         entity = spain_entities, property = "P227", 
                         serial = TRUE)
spain_libcon <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                          entity = spain_entities, property = "P244", 
                          serial = TRUE)
spain_bnfid <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                         entity = spain_entities, property = "P268", 
                         serial = TRUE)
spain_freebase <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                            entity = spain_entities, property = "P646", 
                            serial = TRUE)
spain_munzinger <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                             entity = spain_entities, property = "P1284", 
                             serial = TRUE)
spain_nndb <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                        entity = spain_entities, property = "P1263", 
                        serial = TRUE)
spain_imdb <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                        entity = spain_entities, property = "P345", 
                        serial = TRUE)
spain_brittanica <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                              entity = spain_entities, property = "P1417", 
                              serial = TRUE)
spain_quora <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                         entity = spain_entities, property = "P3417", 
                         serial = TRUE)
spain_id <- full_join(x = spain_parlid, y = spain_gndid , by = "wikidataid") %>%
  full_join(x = ., y = spain_libcon, by = "wikidataid") %>%
  full_join(x = ., y = spain_bnfid, by = "wikidataid") %>%
  full_join(x = ., y = spain_freebase, by = "wikidataid") %>%
  full_join(x = ., y = spain_munzinger, by = "wikidataid") %>%
  full_join(x = ., y = spain_nndb, by = "wikidataid") %>%
  full_join(x = ., y = spain_imdb, by = "wikidataid") %>%
  full_join(x = ., y = spain_brittanica, by = "wikidataid") %>%
  full_join(x = ., y = spain_quora, by = "wikidataid")
names(spain_id) <- c("wikidataid", "parlid", "gndid", "libcon",
                      "bnfid", "freebase", "munzinger", "nndb", "imdb",
                      "brittanica", "quora")
spain_id <- spain_id %>%
  dplyr::distinct(wikidataid, .keep_all = TRUE)
saveRDS(spain_id, "./data/spain_id")

# retrieve and format positions ---------------------------------------------------------
spain_positions <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                              entity = spain_entities, unique = TRUE, 
                              property = "P39")
spain_positions <- spain_positions[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(spain_positions, "./data/spain_positions")

# retrieve and format occupation --------------------------------------------------------
spain_occupation <- wikiData(item = na.omit(unique(spain$wikidataid)), 
                             entity = spain_entities, unique = TRUE, 
                             property = "P106")
spain_occupation <- spain_occupation[,-c("unknown")] %>% data.frame %>%
  setcolorder(order(names(.))) %>% select(wikidataid, everything())
saveRDS(spain_occupation, "./data/spain_occupation")

