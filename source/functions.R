# ---------------------------------------------------------------------------------------
# legislatoR
# Sascha Göbel and Simon Munzert
# Script: functions
# December 2017
# ---------------------------------------------------------------------------------------


##### LANGUAGE CONVERSION OF MONTHS =====================================================
convMonth <- function(string, patterns, replacements) {
  for (i in seq_along(patterns))
    string <- gsub(patterns[i], replacements[i], string, perl=TRUE)
  string
}
months_foreign <- c("Januar", "Jänner", "Februar", "März", "Mai", "Juni", "Juli",
                    "Oktober", "Dezember")
months_english <- c("January", "January", "February", "March", "May", "June", "July",
                    "October", "December")


##### ENCODING DEBUGGING ================================================================
debugEnc <- function(string) {
  debugtab <- read_html("http://www.i18nqa.com/debug/utf8-debug.html") %>%
    html_table(fill = TRUE)
  replacements <- c(debugtab[[1]][-1,9]) %>%
    c("\U0153", "-", "i", "g", "s", "c", "c", "c", "\u2013", ., "'", "\"")
  patterns <- str_replace_all(debugtab[[1]][-1,11], "  |%C3", "") %>%
    c("%C5%93", "_%E2%80%A0_", "%C4%B1", "%C4%9F", "%C5%A1", "%C4%87", "%C4%8D", "%C4%87", "%E2%80%93", ., "%27", "%22")
  string <- str_replace_all(string, "%C3", "")
  for (i in seq_along(patterns)) {
    #string <- gsub(patterns[i], replacements[i], string, perl=TRUE)
    string <- str_replace_all(string, patterns[i], replacements[i])
  }
  string
}


#### RETRIEVAL OF POLITICIANS' WIKIPEDIA URL AND SOME BASIC DATA ========================

# notes ---------------------------------------------------------------------------------
# description: this function provides Wikipedia URLs of legislators for several countries
#              and years. It also allows for additional data to be requested.
# arguments: country, chamber, session, name, party, constituency, term, service
# application: wikiUrl(country = "usa",
#                      chamber = "senate",
#                      session = 1:115,
#                      name = TRUE,
#                      party = TRUE,
#                      constituency = TRUE,
#                      term = TRUE,
#                      service = TRUE)
wikiCore <- function(country, chamber = NULL, session = NULL, name = NULL,
                           party = NULL, constituency = NULL, term = NULL,
                           service = NULL) {
  if (length(country) > 1)
    stop ("Please select only a single country")
  if (!(country %in% c("austria", "france", "germany", "ireland", "usa")))
    stop ("urlWikiProfile does not cover this country yet. Please try 'austria',
          'france', 'germany', 'ireland', or 'usa'")
  if (country == "austria") {
    source <- str_c("https://de.wikipedia.org/wiki/Liste_der_Abgeordneten_zum_%C3%96sterreichischen_Nationalrat_(",
                    as.roman(1:25), "._Gesetzgebungsperiode)")
    langcode <- "de"
    query <- rep("//table/tr/td[1]/a", times = 25)
    query_party <- c(rep("//table/tr/td[4]", times = 24),
                     "//table/tr/td[6]")
    query_multiplier <- NA
    query_multiplier2 <- NA
    regex_constituency <- NA
    query_constituency <- c(rep(NA, times = 22),
                            rep("//table/tr/td[5]", times = 2),
                            "//table/tr/td[7]")
    regex_term <- "[[:upper:]]+(?=\\._)"
    query_service <- c(rep("//table/tr/td[5]", times = 22),
                       rep("//table/tr/td[6]", times = 2),
                       "//table/tr/td[8]")
    duration <- list(c("1920-11-10", "1923-11-20"), c("1923-11-20", "1927-05-18"),
                     c("1927-05-18", "1930-10-01"), c("1930-12-02", "1934-05-02"),
                     c("1945-12-19", "1949-11-08"), c("1949-11-08", "1953-03-18"),
                     c("1953-11-08", "1956-06-08"), c("1956-06-08", "1959-06-09"),
                     c("1959-06-09", "1962-12-14"), c("1962-12-14", "1966-03-30"),
                     c("1966-03-30", "1970-03-30"), c("1970-03-31", "1971-11-04"),
                     c("1971-11-04", "1975-11-04"), c("1975-11-04", "1979-06-04"),
                     c("1979-06-05", "1983-05-18"), c("1983-05-19", "1986-12-16"),
                     c("1986-12-17", "1990-11-04"), c("1990-11-05", "1994-11-06"),
                     c("1994-11-07", "1996-01-14"), c("1996-01-15", "1999-10-29"),
                     c("1999-10-29", "2002-12-19"), c("2002-12-20", "2006-10-29"),
                     c("2006-10-30", "2008-10-27"), c("2008-10-28", "2013-10-28"),
                     c("2013-10-29", "2017-10-15"))
    fill_query <- c(rep(NA, times = 21),
                    "//body/div[3]/div[3]/div[4]/div/table/tr",
                    rep(NA, times = 3))
  }
  if (country == "france") {
    source <- str_c("https://fr.wikipedia.org/wiki/Liste_des_d%C3%A9put%C3%A9s_de_la_",
                    str_c(as.roman(1:14), c("re", rep("e", times = 13))),
                    "_l%C3%A9gislature_de_la_Cinqui%C3%A8me_R%C3%A9publique")
    langcode <- "fr"
    condition <- str_c("[", str_c("@id = '", LETTERS, "' or ", collapse = ""),
                       "@id = 'Liste_des_d.C3.A9put.C3.A9s']")
    query <- c(str_c("//h2/span", condition,
                     "/ancestor::h2/following-sibling::table[1]/tr/td[2]/a"),
               str_c("//h2/span", condition,
                     "/ancestor::h2/following-sibling::table[1]/tr/td[1]/a"),
               str_c("//h2/span", str_replace(condition, " or @id = 'U'", ""),
                     "/ancestor::h2/following-sibling::table[1]/tr/td[1]/a"),
               rep(str_c("//h2/span", condition,
                         "/ancestor::h2/following-sibling::table[1]/tr/td[1]/a"),
                   times = 10),
               str_c("//h2/span", condition,
                     "/ancestor::h2/following-sibling::table[1]/tr/td[1]/a|//h2/span", condition,
                     "/ancestor::h2/following-sibling::table[1]/tr/td[1]/s/a"))
    query_party <- c(rep(str_c("//h2/span", condition,
                               "/ancestor::h2/following-sibling::table[1]/tr/td[3]"),
                         times = 13),
                     str_c("//h2/span", condition,
                               "/ancestor::h2/following-sibling::table[1]/tr/td[4]/a"))
    query_constituency <- c(rep(str_c("//h2/span", condition,
                                      "/ancestor::h2/following-sibling::table[1]/tr/td[4]"),
                                times = 13),
                            str_c("//h2/span", condition,
                                  "/ancestor::h2/following-sibling::table[1]/tr/td[2]"))
    query_multiplier <- NA
    query_multiplier2 <- NA
    regex_constituency <- NA
    regex_term <- "[[:upper:]]+(?=re_l|e_l)"
    query_service <- NA
    duration <- NA
    fill_query <- NA
  }
  if (country == "germany") {
    source <- str_c("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Deutschen_Bundestages_(",
                    1:18, "._Wahlperiode)")
    langcode <- "de"
    query <- c(rep("//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tr/td[1]/a[1]",
                 times = 11),
               "//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tr/td[1]/i/a[1]|
               //h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tr/td[1]/a[1]",
               rep("//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tr/td[1]/a[1]",
                   times = 5),
               "//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tr/td[1]/a[1]|
               //h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tr/td[1]/i/a[1]")
    query_party <- rep("//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tr/td[3]",
                       times = 18)
    query_constituency <- rep("//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tr/td[4]",
                              times = 18)
    query_constituency2 <- rep("//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tr/td[5]",
                              times = 18)
    query_multiplier <- NA
    query_multiplier2 <- NA
    regex_constituency <- NA
    regex_term <- "[[:digit:]]+(?=\\._)"
    query_service <- rep("//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tr/td[7]",
                         times = 18)
    duration <- list(c("1949-09-07", "1953-10-06"), c("1953-10-06", "1957-10-15"),
                     c("1957-10-15", "1961-10-17"), c("1961-10-17", "1965-10-19"),
                     c("1965-10-19", "1969-10-20"), c("1969-10-20", "1972-12-13"),
                     c("1972-12-13", "1976-12-14"), c("1976-12-14", "1980-10-04"),
                     c("1980-10-04", "1983-03-29"), c("1983-03-29", "1987-02-18"),
                     c("1987-02-18", "1990-12-20"), c("1990-12-20", "1994-11-10"),
                     c("1994-11-10", "1998-10-26"), c("1998-10-26", "2002-10-17"),
                     c("2002-10-17", "2005-10-18"), c("2005-10-18", "2009-10-27"),
                     c("2009-10-27", "2013-10-22"), c("2013-10-22", "2017-10-24"))
    fill_query <- NA
  }
  if (country == "ireland") {
    source <- str_c("https://en.wikipedia.org/wiki/Members_of_the_",
                    sapply(1:32, toOrdinal), "_D%C3%A1il")
    langcode <- "en"
    query <- c("//h2/span[contains(text(), 'Members by constituency')]/ancestor::h2/
               following-sibling::table[1]/tr/td[2]/a",
               "//h2/span[contains(text(), 'Members by constituency')]/ancestor::h2/
               following-sibling::table[1]/tr/td[position() < last()]/
               a[not(contains(@title, 'constituency'))]",
               rep("//h2/span[contains(text(), 'TDs by constituency')]/ancestor::h2/
                   following-sibling::table[1]/tr/td[position() < last()]/
                   a[not(contains(@title, 'constituency'))]", times = 26),
               rep("//h2/span[contains(text(), 'TDs by party')]/ancestor::h2/
                   following-sibling::table[1]/tr/td[not(@rowspan) and position() <
                   last()]/a[not(contains(@title, 'Unemployed Action'))]",
                   times = 4))
    query_party <- c(rep("//h2/span[contains(text(), 'Members by constituency')]/
                         ancestor::h2/following-sibling::table[1]/tr/
                         td[last()]/a", times = 2),
                     rep("//h2/span[contains(text(), 'TDs by constituency')]/
                         ancestor::h2/following-sibling::table[1]/tr/
                         td[last()]/a", times = 26),
                     rep("//h2/span[contains(text(), 'TDs by party')]/
                         ancestor::h2/following-sibling::table[1]/tr/
                         td[contains(text(), ')')]/a", times = 4)
                     )
    query_constituency <- c(rep("//h2/span[contains(text(), 'Members by constituency')]/
                                ancestor::h2/following-sibling::table[1]/tr/td/
                                a[contains(@title, 'constituency')]", times = 2),
                            rep("//h2/span[contains(text(), 'TDs by constituency')]/
                                ancestor::h2/following-sibling::table[1]/tr/td/
                                a[contains(@title, 'constituency')]", times = 26),
                            rep("//h2/span[contains(text(), 'TDs by party')]/
                                ancestor::h2/following-sibling::table[1]/tr/td/
                                a[contains(@title, 'constituency')]", times = 4))
    query_multiplier <- c(rep("//h2/span[contains(text(), 'Members by constituency')]/
                              ancestor::h2/following-sibling::table[1]/tr/td/
                              a[contains(@title, 'constituency')]/parent::td", times = 2),
                          rep("//h2/span[contains(text(), 'TDs by constituency')]/
                              ancestor::h2/following-sibling::table[1]/tr/td/
                              a[contains(@title, 'constituency')]/parent::td", times = 26),
                          rep("//h2/span[contains(text(), 'TDs by party')]/
                              ancestor::h2/following-sibling::table[1]/tr/td/
                              a[contains(@title, 'constituency')]/parent::td", times = 4))
    query_multiplier2 <- c(rep(NA, times = 28),
                           rep("//h2/span[contains(text(), 'TDs by party')]/
                               ancestor::h2/following-sibling::table[1]/tr/
                               td[contains(text(), ')')]", times = 4))
    regex_term <- "[[:digit:]]+"
    regex_constituency <- NA
    query_service <- NA
    duration <- NA
    fill_query <- NA
  }
  if (country == "usa") {
    if (missing(chamber))
      stop ("Please specify chamber, i.e., 'house' or 'senate'")
    if (chamber == "house" | chamber == "senate") {
      source <- str_c("https://en.wikipedia.org/wiki/",
                      sapply(1:115, toOrdinal), "_United_States_Congress")
      langcode <- "en"
      if (chamber == "house") {
      query_a <- "(//table[@class='multicol'])[2]//ul/li/a[not(contains(@title, 'district'))
                  and not(contains(@title, 'Party'))
                  and not(contains(@title, 'Commissioner of Puerto'))
                  and not(contains(@title, 'at-large'))
                  and not(contains(@title, 'Independent politician'))
                  and not(contains(@title, 'Free Silver'))
                  and not(contains(@title, 'Southwest Territory'))
                  and not(contains(@title, 'Know Nothing'))
                  and not(contains(@title, 'Independent Democrat'))
                  and not(contains(@title, 'Independent (politics)'))
                  and not(contains(@title, 'Independent (United States)'))
                  and not(contains(@title, 'Independent (politician)'))
                  and not(contains(@title, 'congressional delegations'))"
      query_b <- " and not(contains(@title, 'Tennessee'))"
      query_c <- "(//table[@class='multicol'])[2]//ul/li/dl/dd/a[not(contains(@title, 'district'))
                  and not(contains(@title, 'Party'))
                  and not(contains(@title, 'Commissioner of Puerto'))
                  and not(contains(@title, 'at-large'))
                  and not(contains(@title, 'Independent politician'))
                  and not(contains(@title, 'Free Silver'))
                  and not(contains(@title, 'Southwest Territory'))
                  and not(contains(@title, 'Know Nothing'))
                  and not(contains(@title, 'Independent Democrat'))
                  and not(contains(@title, 'Independent (politics)'))
                  and not(contains(@title, 'Independent (United States)'))
                  and not(contains(@title, 'Independent (politician)'))
                  and not(contains(@title, 'congressional delegations'))"
      query_d <- " and not(contains(@title, 'William Wilkins'))"
      query_e <- " and not(contains(@title, 'States Rights party'))"
      query_f <- " and not(contains(@title, 'States-Rights Whig'))"
      query_g <- str_replace(query_a, "@class='multicol'", "@width='75%'")
      query_h <- str_replace(query_c, "@class='multicol'", "@width='75%'")
      query <- c(rep(str_c(query_a, query_b, "]|", query_c, query_b, "]"), times = 16),
                 rep(str_c(query_a, "]|", query_c, "]"), times = 4),
                 rep(str_c(query_a, query_d, "]|", query_c, query_d, "]"), times = 1),
                 rep(str_c(query_a, "]|", query_c, "]"), times = 2),
                 rep(str_c(query_a, query_e, "]|", query_c, query_e, "]"), times = 1),
                 rep(str_c(query_a, "]|", query_c, "]"), times = 2),
                 rep(str_c(query_a, query_f, "]|", query_c, query_f, "]"), times = 1),
                 rep(str_c(query_a, "]|", query_c, "]"), times = 63),
                 str_c(query_g, "]|", query_h, "]"),
                 rep(str_c(query_a, "]|", query_c, "]"), times = 24)
                 )
      query_constituency <- c(rep("(//table[@class='multicol'])[2]//ul/li/a[1]", 90),
                              "(//table[@width='75%'])[2]//ul/li/a[1]",
                              rep("(//table[@class='multicol'])[2]//ul/li/a[1]", 24))
      query_party <- c(rep("(//table[@class='multicol'])[2]//ul/li", 90),
                       "(//table[@width='75%'])[2]//ul/li",
                       rep("(//table[@class='multicol'])[2]//ul/li", 24))
      query_multiplier <- NA
      query_multiplier2 <- NA
      query_service <- NA
      duration <- NA
      regex_term <- "[[:digit:]]+"
      regex_exception1 <- "\n.+|\\(Resident Commissioner\\)|\\(.Southwest Territory.+?\\)|\\(politician\\)"
      regex_exception2 <- ".acant(,)? (until|to|admitted|moved)|^Vacant.+thereafter$|newly admitted|2\\. and 3\\. Joint|.acant(,)? from|^Vacant|Vacant$|^4\\. vacant"
      regex_exception3 <- "^Vacant|^[[:digit:]]([[:digit:]])?(\\.|\\:) Vacant|At-large..(v|V)acant|2. and 3. Joint district with two seats\\.|, vacant until|\\. vacant to|(\\.|\\:) Vacant until|^[[:digit:]]([[:digit:]])?(\\.|\\:) vacant"
      regex_constituency <- NA
      query_ttime <- "//table[@class = 'infobox vevent']/tr[4]"
      fill_query <- NA
      }
      if (chamber == "senate") {
      query <- c(rep("(//table[@class='multicol'])[1]//ul/li/a[1]", 42),
                 "(//table[@class='multicol'])[1]//ul/li/a[not(contains(@title, 'Senatorial'))
                 and not(contains(@title, 'Liberal'))]",
                 rep("(//table[@class='multicol'])[1]//ul/li/a[1]", 7),
                 "(//table[@class='multicol'])[1]//ul/li/a[not(contains(@title, 'Senatorial'))]",
                 rep("(//table[@class='multicol'])[1]//ul/li/a[1]", 39),
                 "//table[2]//ul/li/a[1]",
                 rep("(//table[@class='multicol'])[1]//ul/li/a[1]", 24))
      query_constituency <- c(rep("(//table[@class='multicol'])[1]//ul/li", 90),
                                "//table[2]//ul/li",
                              rep("(//table[@class='multicol'])[1]//ul/li", 24))
      query_party <- c(rep("(//table[@class='multicol'])[1]//ul/li", times = 90), "//table[2]//ul/li",
                       rep("(//table[@class='multicol'])[1]//ul/li", times = 24))
      query_multiplier <- NA
      query_multiplier2 <- NA
      regex_term <- "[[:digit:]]+"
      regex_exception1 <- "\n.+|\\(Resident Commissioner\\)|\\(.Southwest Territory.+?\\)|\\(politician\\)| until December 3, 1928.+"
      regex_exception2 <- ".acant(,)? (until|to|admitted|moved)|^Vacant.+thereafter$|newly admitted|2\\. and 3\\. Joint|.acant(,)? from|^Vacant|Vacant$|(\\.|\\:) .acant"
      regex_exception3 <- "^Vacant|^[[:digit:]]([[:digit:]])?(\\.|\\:) Vacant|At-large..(v|V)acant|2. and 3. Joint district with two seats\\.|, vacant until|\\. vacant to|(\\.|\\:) Vacant until|^[[:digit:]]([[:digit:]])?(\\.|\\:) vacant"
      regex_constituency <- c(rep("h4/span[1]/a", times = 12), "h4/span[1]",
                              rep("h4/span[1]/a", times = 76), "h4/span[1]", "p/b",
                              rep("h4/span[1]/a", times = 24))
      query_ttime <- "//table[@class = 'infobox vevent']/tr[4]"
      query_service <- NA
      duration <- NA
      fill_query <- NA
      }
    } else stop ("Please specify chamber, i.e., 'house' or 'senate'")
  }
  if (!missing(session)) {
    source <- source[session]
    query <- query[session]
    query_multiplier <- query_multiplier[session]
    query_constituency <- query_constituency[session]
    query_party <- query_party[session]
    query_multiplier2 <- query_multiplier2[session]
    regex_constituency <- regex_constituency[session]
    query_service <- query_service[session]
    duration <- duration[session]
    fill_query <- fill_query[session]
  }
  if (length(source) > 1) {
    urls <- rep(list(NA), times = length(source))
    htmls <- rep(list(NA), times = length(source))
    for (i in 1:length(urls)) {
      htmls[[i]] <-  read_html(source[i], encoding = "UTF-8")
      urls[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
        html_attr("href") %>%
        str_c("https://", langcode,".wikipedia.org", .)
    }
    if (isTRUE(name | party | constituency | term)) {
      urls <- lapply(urls, data.table) %>%
        lapply(rename, url = V1)
    }
    if (isTRUE(name)) {
      names <- rep(list(NA), times = length(source))
      for (k in 1:length(names)) {
        names[[k]] <- html_nodes(x = htmls[[k]], xpath = query[k]) %>%
          html_text()
      }
      names <- lapply(names, data.table) %>%
        lapply(rename, name = V1)
      urls <- mapply(cbind, urls, names, SIMPLIFY = FALSE)
    }
    if (isTRUE(party)) {
      parties <- rep(list(NA), times = length(source))
      multiplier <- rep(list(NA), times = length(source))
      for (l in 1:length(parties)) {
        if (country == "usa") {
          parties[[l]] <- html_nodes(x = htmls[[l]], xpath = query_party[l]) %>%
            html_text() %>%
            str_replace_all(regex_exception1, "") %>%
            str_replace_all("^.+?(?=\\()|(?<=\\)).+", "") %>%
            str_replace_all("\\(|\\)|\n", "") %>%
            extract(!str_detect(., regex_exception2))
        } else {
          parties[[l]] <- html_nodes(x = htmls[[l]], xpath = query_party[l]) %>%
            html_text()
          if (!equals(length(parties[[l]]), dim(urls[[l]])[1])) {
            multiplier[[l]] <- html_nodes(x = htmls[[l]], xpath = query_multiplier2[l]) %>%
              html_attrs() %>%
              as.numeric %>%
              na.replace(1)
            parties[[l]] <- rep(parties[[l]], times = multiplier[[l]])
          }
        }
      }
      parties <- lapply(parties, data.table) %>%
        lapply(rename, party = V1)
      urls <- mapply(cbind, urls, parties, SIMPLIFY = FALSE)
    }
    if (isTRUE(constituency)) {
      constituencies <- rep(list(NA), times = length(source))
      multiplier <- rep(list(NA), times = length(source))
      vacant <- rep(list(NA), times = length(source))
      for (m in 1:length(constituencies)) {
        if (country == "usa") {
          if (chamber == "house") {
            vacant[[m]] <- html_nodes(x = htmls[[m]], xpath = query_party[m]) %>%
              html_text() %>%
              str_replace_all(regex_exception1, "") %>%
              str_replace_all("^.+?(?=\\()|(?<=\\)).+", "") %>%
              str_replace_all("\\(|\\)|\n", "") %>%
              str_detect(., regex_exception2)
            constituencies[[m]] <- html_nodes(x = htmls[[m]], xpath = query_constituency[m]) %>%
              html_attr("title") %>%
              replace(., !str_detect(., "district|At-large"), NA) %>%
              na.locf()
            if (equals(length(vacant[[m]]), length(constituencies[[m]]))) {
              constituencies[[m]] <- constituencies[[m]] %>%
                extract(!vacant[[m]])
            }
          }
          if (chamber == "senate") {
            vacant[[m]] <- html_nodes(x = htmls[[m]], xpath = query_party[m]) %>%
              html_text() %>%
              str_replace_all(regex_exception1, "") %>%
              str_replace_all("^.+?(?=\\()|(?<=\\)).+", "") %>%
              str_replace_all("\\(|\\)|\n", "") %>%
              str_detect(., regex_exception2)
            constituency <- html_nodes(x = htmls[[m]], xpath = str_replace(query_constituency[m], "ul/li", regex_constituency[m])) %>%
              html_text()
            replicate <- html_nodes(x = htmls[[m]], xpath = query_constituency[m]) %>%
              html_text %>%
              str_extract("[[:digit:]](?=\\.|\\:)") %>%
              as.numeric
            constituencies[[m]] <- replace(replicate, which(!is.na(replicate))[-seq(from = 2,
                                                                             to = length(which(!is.na(replicate))), by = 2)],
                                    constituency) %>%
              replace(., str_detect(., "[[:digit:]]"), NA) %>%
              na.locf() %>%
              extract(!vacant[[m]])
          }
        } else {
          if (!is.na(query_constituency[[m]])) {
            constituencies[[m]] <- html_nodes(x = htmls[[m]], xpath = query_constituency[m]) %>%
              html_text()
            if (!equals(length(constituencies[[m]]), dim(urls[[m]])[1])) {
              multiplier[[m]] <- html_nodes(x = htmls[[m]], xpath = query_multiplier[m]) %>%
                html_attrs() %>%
                as.numeric %>%
                na.replace(1)
              constituencies[[m]] <- rep(constituencies[[m]], times = multiplier[[m]])
            }
          } else {
            constituencies[[m]] <- NA
          }
        }
      }
      constituencies <- lapply(constituencies, data.table) %>%
        lapply(rename, constituency = V1)
      urls <- mapply(cbind, urls, constituencies, SIMPLIFY = FALSE)
      if (country == "germany") {
        constituencies2 <- rep(list(NA), times = length(source))
        for (m in 1:length(constituencies)) {
          fill <- html_nodes(x = htmls[[m]], xpath = "//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tr") %>%
            lapply(xml_children) %>%
            lapply(length) %>%
            unlist %>%
            extract(-1) %>%
            as.numeric %>%
            is_less_than(5) %>%
            which()
          constituencies2[[m]] <- html_nodes(x = htmls[[m]], xpath = query_constituency2[m]) %>%
            html_text() %>%
            inset(.=="", NA)
          constituencies2[[m]] <- numeric(length(constituencies2[[m]]) + length(fill)) %>%
            replace(fill, NA) %>%
            replace(!is.na(.), constituencies2[[m]])
        }
        constituencies2 <- lapply(constituencies2, data.table) %>%
          lapply(rename, constituency2 = V1)
        urls <- mapply(cbind, urls, constituencies2, SIMPLIFY = FALSE)
      }
    }
    if (isTRUE(term)) {
      urls <-  as.numeric(as.roman(str_extract(source, regex_term))) %>%
        mapply(function(urls, x) mutate(urls, term = x), urls, ., SIMPLIFY=FALSE)
    }
    if (isTRUE(service)) {
      if (!(country %in% c("austria", "usa", "germany")))
        stop ("Period of service data is not available for this country. Please try 'austria', 'usa', or 'germany'.")
      Sys.setlocale("LC_TIME","C")
      term_times <- rep(list(NA), times = length(source))
      beginnings <- rep(list(NA), times = length(source))
      ends <- rep(list(NA), times = length(source))
      services <- rep(list(NA), times = length(source))
      if (country == "austria") {
        for (m in 1:length(source)) {
          term_times[[m]] <- duration[[m]]
          beginnings[[m]] <- html_nodes(x = htmls[[m]], xpath = query_service[m]) %>%
            html_text() %>%
            str_trim() %>%
            str_replace("(Wechsel von der SPÖ zum Team Stronach)|Nachfolger von Reinhold|und von 26. April 2011|und ab 20. September 2011 als |und Vorgänger von Gerald Klug| Bundesministerin für Gesundheit und Frauen|Nachfolger und Vorgänger jeweils| und seit 1. Juli 2016.+|Nachfolgerin von Monika Lindner.+", "") %>%
            str_replace_all("(Bis|bis).+[[:digit:]]{4}( )?|Wechselte am .+|(A|a)m .+ (übergetreten|übergetreten|ausgetreten|ausgeschlossen|ausgeschieden)|bis 14. Oktober 2012 BZÖ,|.+Ruhendstellung wieder aufgehoben|Bis 27. Dezember 2009 BZÖ, seit 23.|Ab 28. April 2006 BZÖ|Bis 29. Dezember 2009 BZÖ|Bis 1. August 2011 FPÖ,|bis 27. Dezember 2009 BZÖ|.+als Abgeordneter der Landesliste Wien|bis 15. September 2011 BZÖ|.+vom Bundeswahlvorschlag auf|^.+Klubobfrau|.+seit 11. August 2017 Mitglied im FPÖ-Klub|.+bis 23. Dezember 2015 fraktionslos|Nach Mandatsverzicht.+|.+ohne Klubzugehörigkeit|Nationalratspräsident.+|.+Klubobmann der Grünen", "") %>%
            str_extract("([[:digit:]])?[[:digit:]].+?[[:digit:]]{4}|ab 10. April für|ab April 2017") %>%
            str_replace("ab 10. April für", "10. April 2007") %>%
            str_replace("^ab", "01.") %>%
            convMonth(patterns = months_foreign, replacements = months_english) %>%
            dmy() %>%
            na.replace(duration[[m]][1])
          ends[[m]] <- html_nodes(x = htmls[[m]], xpath = query_service[m]) %>%
            html_text() %>%
            str_trim()    %>%
            str_replace("(Wechsel von der SPÖ zum Team Stronach)|Nachfolger von Reinhold|und von 26. April 2011|und ab 20. September 2011 als |und Vorgänger von Gerald Klug| Bundesministerin für Gesundheit und Frauen|Nachfolger und Vorgänger jeweils| und seit 1. Juli 2016.+", "") %>%
            str_replace_all("(Vom|vom|ab|Ab|Von|von|seit).+?[[:digit:]]{4}( )?|^([[:digit:]])?[[:digit:]].+?[[:digit:]]{4}|.+? und|Wechselte am .+|(A|a)m .+ (angelobt|nachgerückt|übergetreten|übergetreten|ausgetreten|ausgeschlossen)|bis 14. Oktober 2012 BZÖ,|.+Ruhendstellung wieder aufgehoben|Bis 27. Dezember 2009 BZÖ, seit 23.|Bis 29. Dezember 2009 BZÖ|Bis 1. August 2011 FPÖ,|bis 27. Dezember 2009 BZÖ|.+als Abgeordneter der Landesliste Wien|bis 15. September 2011 BZÖ|.+vom Bundeswahlvorschlag auf|^.+Klubobfrau|.+seit 11. August 2017 Mitglied im FPÖ-Klub|.+bis 23. Dezember 2015 fraktionslos", "") %>%
            str_extract("([[:digit:]])?[[:digit:]].+?[[:digit:]]{4}|Bis September 2008|bis April 2017") %>%
            str_replace("^(B|b)is", "01.") %>%
            convMonth(patterns = months_foreign, replacements = months_english) %>%
            dmy() %>%
            na.replace(duration[[m]][2])
          if (!is.na(fill_query[m])) {
            fill <- html_nodes(x = htmls[[m]], xpath = fill_query[m]) %>%
              lapply(xml_children) %>%
              lapply(length) %>%
              unlist %>%
              extract(-1) %>%
              as.numeric %>%
              is_less_than(5) %>%
              which
            beginnings[[m]] <- numeric(length(beginnings[[m]]) + length(fill)) %>%
              replace(fill, NA) %>%
              replace(!is.na(.), beginnings[[m]]) %>%
              as.Date() %>%
              na.replace(duration[[m]][1])
            ends[[m]] <- numeric(length(ends[[m]]) + length(fill)) %>%
              replace(fill, NA) %>%
              replace(!is.na(.), ends[[m]]) %>%
              as.Date() %>%
              na.replace(duration[[m]][2])
          }
          services[[m]] <- difftime(time1 = ends[[m]], time2 = beginnings[[m]], units = "days")
        }
      }
      if (country == "germany") {
        Sys.setlocale("LC_TIME","C")
        term_times <- rep(list(NA), times = length(source))
        beginnings <- rep(list(NA), times = length(source))
        ends <- rep(list(NA), times = length(source))
        services <- rep(list(NA), times = length(source))
        for (m in 1:length(source)) {
          term_times[[m]] <- duration[[m]]
          fill <- html_nodes(x = htmls[[m]], xpath = "//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tr") %>%
            lapply(xml_children) %>%
            lapply(length) %>%
            unlist %>%
            extract(-1) %>%
            as.numeric %>%
            is_less_than(7) %>%
            which()
          beginnings[[m]] <- html_nodes(x = htmls[[m]], xpath = query_service[m]) %>%
            html_text() %>%
            str_extract_all("(eingetreten am|nachgewählt am|nachgerückt am).+?[[:digit:]]{4}?") %>%
            lapply(., tail, n=1) %>%
            lapply(., function(x) if(identical(x, character(0))) NA_character_ else x) %>%
            unlist %>%
            str_extract("([[:digit:]])?[[:digit:]].+?[[:digit:]]{4}") %>%
            convMonth(patterns = months_foreign, replacements = months_english) %>%
            dmy() %>%
            na.replace(duration[[m]][1])
          beginnings[[m]] <- numeric(length(beginnings[[m]]) + length(fill)) %>%
            replace(fill, NA) %>%
            replace(!is.na(.), beginnings[[m]]) %>%
            as.Date() %>%
            na.replace(duration[[m]][1])
          ends[[m]] <- html_nodes(x = htmls[[m]], xpath = query_service[m]) %>%
            html_text() %>%
            str_extract_all("((A|a)usgeschieden am|verstorben am).+?[[:digit:]]{4}?") %>%
            lapply(., function(x) if(identical(x, character(0))) NA_character_ else x) %>%
            unlist %>%
            str_extract("([[:digit:]])?[[:digit:]].+?[[:digit:]]{4}") %>%
            convMonth(patterns = months_foreign, replacements = months_english) %>%
            dmy() %>%
            na.replace(duration[[m]][2])
          ends[[m]] <- numeric(length(ends[[m]]) + length(fill)) %>%
            replace(fill, NA) %>%
            replace(!is.na(.), ends[[m]]) %>%
            as.Date() %>%
            na.replace(duration[[m]][2])
          services[[m]] <- difftime(time1 = ends[[m]], time2 = beginnings[[m]], units = "days")
        }
      }
      if (country == "usa") {
        for (m in 1:length(source)) {
          term_times[[m]] <- html_nodes(x = htmls[[m]], xpath = query_ttime) %>%
            html_text() %>%
            str_replace_all(".\\(.+?\\)", "") %>%
            str_split(pattern = "(?<=[[:digit:]]) ", n = 2) %>%
            unlist %>%
            str_replace_all(pattern = "^.+(?=[[:upper:]])|\\n", replacement = "")
          beginnings[[m]] <- html_nodes(x = htmls[[m]], xpath = query_party[m]) %>%
            html_text() %>%
            str_replace_all("\n.+|\n", "") %>%
            extract(!str_detect(., regex_exception3)) %>%
            str_replace_all("([^[[:digit:]]]+$)|\\[.(.\\])?|, changed.+", "") %>%
            replace(.,list = !str_detect(., "[[:digit:]]{4}"), "") %>%
            str_replace_all("(?<=[[:digit:]]{4} ).+| until.+| resigned.+| died.+|.\\?\\?(\\?)?(\\?)?", "") %>%
            str_extract("[[:upper:]][[:alpha:]]+ [[:digit:]]([[:digit:]])?,.+") %>%
            na.replace(term_times[[m]][1]) %>%
            mdy()
          ends[[m]] <- html_nodes(x = htmls[[m]], xpath = query_party[m]) %>%
            html_text() %>%
            str_replace_all("\n.+|\n", "") %>%
            extract(!str_detect(., regex_exception3)) %>%
            str_replace(" - End", str_c(" - ", term_times[[m]][2])) %>%
            str_replace_all("([^[[:digit:]]]+$)| before April 6|, changed.+", "") %>%
            replace(.,list = !str_detect(., "[[:digit:]]{4}"), "") %>%
            str_replace_all("^.+[[:digit:]]{4} |( )?from.+?[[:digit:]]{4}|( )?seated.+|.\\?\\?(\\?)?(\\?)?(,)?", "") %>%
            str_extract("([[:upper:]][[:alpha:]]+)?(,)? ([[:digit:]]([[:digit:]])?, )?[[:digit:]]{4}") %>%
            str_trim %>%
            str_replace("^(?=[[:digit:]]{4})", "January 1, ") %>%
            str_replace("(?<=[[:alpha:]])((,)? )(?=[[:digit:]]{4})", " 1, ") %>%
            na.replace(term_times[[m]][2]) %>%
            mdy()
          services[[m]] <- difftime(time1 = ends[[m]], time2 = beginnings[[m]], units = "days")
        }
      }
      beginnings <- lapply(beginnings, data.table) %>%
        lapply(rename, beginning = V1)
      ends <- lapply(ends, data.table) %>%
        lapply(rename, ends = V1)
      services <- lapply(services, data.table) %>%
        lapply(rename, service = V1)
      urls <- mapply(cbind, urls, beginnings, ends, services, SIMPLIFY = FALSE)
    }
  } else {
      html <- read_html(source, encoding = "UTF-8")
      urls <- html_nodes(x = html, xpath = query) %>%
        html_attr("href") %>%
        str_c("https://de.wikipedia.org", .)
      if (isTRUE(name)| isTRUE(party) | isTRUE(constituency)) {
        urls <- data.table(url = urls)
      }
      if (isTRUE(name)) {
        name <- html_nodes(x = html, xpath = query) %>%
          html_text()
        urls <- data.table(urls, name)
      }
      if (isTRUE(party)) {
        if (country == "usa") {
          party <- html_nodes(x = html, xpath = query_party) %>%
            html_text() %>%
            str_replace_all(regex_exception1, "") %>%
            str_replace_all("^.+?(?=\\()|(?<=\\)).+", "") %>%
            str_replace_all("\\(|\\)|\n", "") %>%
            extract(!str_detect(., regex_exception2))
        } else {
          party <- html_nodes(x = html, xpath = query_party) %>%
            html_text()
          if (!equals(length(party), dim(urls)[1])) {
            multiplier <- html_nodes(x = html, xpath = query_multiplier2) %>%
              html_attrs() %>%
              as.numeric %>%
              na.replace(1)
            party <- rep(party, times = multiplier)
          }
        }
        urls <- data.table(urls, party)
      }
      if (isTRUE(constituency)) {
        if (country == "usa") {
          if (chamber == "house") {
            vacant <- html_nodes(x = html, xpath = query_party) %>%
              html_text() %>%
              str_replace_all(regex_exception1, "") %>%
              str_replace_all("^.+?(?=\\()|(?<=\\)).+", "") %>%
              str_replace_all("\\(|\\)|\n", "") %>%
              str_detect(., regex_exception2)
            constituency <- html_nodes(x = html, xpath = query_constituency) %>%
              html_attr("title") %>%
              replace(., !str_detect(., "district|At-large"), NA) %>%
              na.locf()
            if (equals(length(vacant), length(constituency))) {
              constituency <- constituency %>%
                extract(!vacant)
            }
          }
          if (chamber == "senate") {
            vacant <- html_nodes(x = html, xpath = query_party) %>%
              html_text() %>%
              str_replace_all(regex_exception1, "") %>%
              str_replace_all("^.+?(?=\\()|(?<=\\)).+", "") %>%
              str_replace_all("\\(|\\)|\n", "") %>%
              str_detect(., regex_exception2)
            constituency <- html_nodes(x = html, xpath = str_replace(query_constituency, "ul/li", regex_constituency)) %>%
              html_text
            replicate <- html_nodes(x = html, xpath = query_constituency) %>%
              html_text %>%
              str_extract("[[:digit:]](?=\\.|\\:)") %>%
              as.numeric
            constituency <- replace(replicate, which(!is.na(replicate))[-seq(from = 2,
                                                                             to = length(which(!is.na(replicate))), by = 2)],
                    constituency) %>%
              replace(., str_detect(., "[[:digit:]]"), NA) %>%
              na.locf() %>%
              extract(!vacant)
          }
        } else {
          if (!is.na(query_constituency)) {
            constituency <- html_nodes(x = html, xpath = query_constituency) %>%
              html_text()
            if (!equals(length(constituency), dim(urls)[1])) {
              multiplier <- html_nodes(x = html, xpath = query_multiplier) %>%
                html_attrs() %>%
                as.numeric %>%
                na.replace(1)
              constituency <- rep(constituency, times = multiplier)
            }
          } else {
            constituency <- NA
          }
        }
        urls <- data.table(urls, constituency)
      }
      if (isTRUE(term)) {
        term <- as.numeric(as.roman(str_extract(source, regex_term)))
        urls <- data.table(urls, term)
      }
      if (isTRUE(service)) {
        if (country != "usa")
          stop ("sry, this feature is only available for the USA")
        Sys.setlocale("LC_TIME","C")
        term_time <- html_nodes(x = html, xpath = query_ttime) %>%
          html_text() %>%
          str_replace_all(".\\(.+?\\)", "") %>%
          str_split(pattern = "(?<=[[:digit:]]) ", n = 2) %>%
          unlist %>%
          str_replace_all(pattern = "^.+(?=[[:upper:]])|\\n", replacement = "")
        beginning <- html_nodes(x = html, xpath = query_party) %>%
          html_text() %>%
          str_replace_all("\n.+|\n", "") %>%
          extract(!str_detect(., regex_exception3)) %>%
          str_replace_all("([^[[:digit:]]]+$)|\\[.(.\\])?|, changed.+", "") %>%
          replace(.,list = !str_detect(., "[[:digit:]]{4}"), "") %>%
          str_replace_all("(?<=[[:digit:]]{4} ).+| until.+| resigned.+| died.+|.\\?\\?(\\?)?(\\?)?", "") %>%
          str_extract("[[:upper:]][[:alpha:]]+ [[:digit:]]([[:digit:]])?,.+") %>%
          na.replace(term_time[1]) %>%
          mdy()
        end <- html_nodes(x = html, xpath = query_party) %>%
          html_text() %>%
          str_replace_all("\n.+|\n", "") %>%
          extract(!str_detect(., regex_exception3)) %>%
          str_replace(" - End", str_c(" - ", term_time[2])) %>%
          str_replace_all("([^[[:digit:]]]+$)| before April 6|, changed.+", "") %>%
          replace(.,list = !str_detect(., "[[:digit:]]{4}"), "") %>%
          str_replace_all("^.+[[:digit:]]{4} |( )?from.+?[[:digit:]]{4}|( )?seated.+|.\\?\\?(\\?)?(\\?)?(,)?", "") %>%
          str_extract("([[:upper:]][[:alpha:]]+)?(,)? ([[:digit:]]([[:digit:]])?, )?[[:digit:]]{4}") %>%
          str_trim %>%
          str_replace("^(?=[[:digit:]]{4})", "January 1, ") %>%
          str_replace("(?<=[[:alpha:]])((,)? )(?=[[:digit:]]{4})", " 1, ") %>%
          na.replace(term_time[2]) %>%
          mdy()
        service <- difftime(time1 = end, time2 = beginning, units = "days")
        urls <- data.table(urls, beginning, end, service)
      }
  }
  return(urls)
}


#### RETRIEVAL OF POLITICIANS' WIKIDATA AND PAGE IDS ====================================
wikiIDs <- function(url, corp = NULL) {
  if (!missing(corp)) {
    url <- url$url
  }
  url <- debugEnc(string = url)
  title <- str_extract(url, "(?<=/)[[:upper:]].+")
  langcode <- str_extract(url[1], "(?<=//)[[:alpha:]]{2}")
  ids <- data.table(pageid = rep(as.integer(NA), length(title)),
                    wikidataid = rep(as.character(NA), length(title)))
  for (i in 1:length(url)) {
  # for (i in 201:300) {
    id <- NULL
    while (is.null(id)) {
      try(
        id <- as.character(RETRY("POST", str_c("https://", langcode,".wikipedia.org/w/api.php"),
                                  query = list(action  = "query",
                                               titles = title[i],
                                               format = "json",
                                               prop = "info|pageprops",
                                               ppprop = "wikibase_item",
                                               redirects = 1),
                                  times = 1000)),
        silent = TRUE)
    }
    ids[i,1] <- fromJSON(id)$query$pages[[1]]$pageid
    ids[i,2] <- fromJSON(id)$query$pages[[1]]$pageprops$wikibase_item
  }
  return(ids)
}


#### RETRIEVAL OF WIKIPEDIA PAGE REVISIONS ==============================================
pageRev <- function(data, from, to, interval, fill = NULL, download = NULL, folder = NULL) {
  if (!(from %in% 2001:2017)|!(to %in% 2001:2017))
    stop ("Please select a year between 2001 and 2017", call. = FALSE)
  if (from > to)
    stop ("The year specified in 'from' must occur before or equal the year specified in 'to'", call. = FALSE)
  if (!(interval %in% c("monthly", "weekly")))
    stop ("The specified 'interval' must be either ''monthly'' or ''weekly''", call. = FALSE)
  run <- 1
  for (z in 1:length(data)) {
    cat("\nDownloading data for legislature", z, "of", length(data), "\n")
    if (interval == "monthly") {
      begin <- seq(as.Date(str_c(from, "-01-01")), length = (to - from + 1) * 12, by = "1 month")
      end <- seq(as.Date(str_c(from, "-02-01")), length = (to - from + 1) * 12, by = "1 month") -1
      ids <- data.table(oldid = as.integer(NA), timestamp = as.character(NA), month = rep(month(begin), times = dim(data[[z]])[1]),
                        year = rep(year(begin), times = dim(data[[z]])[1]), pageid = rep(data[[z]]$pageid, each = length(begin)))
      #ids <- rep(list(data.table(oldid = as.integer(NA), timestamp = as.character(NA), month = month(begin), year = year(begin))), times = length(url))
      #names(ids) <- url
      }
    if (interval == "weekly") {
      begin <- floor_date(seq(as.Date(str_c(from, "-01-01")), length = (to - from + 1) * 53, by="1 week"), unit = "week") + 1
      end <- as.Date(ceiling_date(seq(as.Date(str_c(from, "-01-01")), length = (to - from + 1) * 53, by="1 week"), unit = "week"))
      ids <- data.table(oldid = as.integer(NA), timestamp = as.character(NA), week = rep(week(begin), times = dim(data[[z]])[1]),
                        year = rep(year(begin), times = dim(data[[z]])[1]), pageid = rep(data[[z]]$pageid, each = length(begin)))
      #ids <- rep(list(data.table(oldid = as.integer(NA), timestamp = as.character(NA), week = week(begin), year = year(begin))), times = length(url))
      #names(ids) <- url
    }
    pageid <- data[[z]]$pageid
    #title <- str_extract(url, "(?<=/)([[:upper:]]|%).+")
    langcode <- str_extract(data[[z]]$url[1], "(?<=//)[[:alpha:]]{2}")
    #for (i in 1:length(url)) {
    for (i in 1:dim(data[[z]])[1]) {
      for (k in 1:length(begin)) {
        rev <- NULL
        while (is.null(rev)) {
          try(
            rev <- as.character(RETRY("POST", str_c("https://", langcode,".wikipedia.org/w/api.php"),
                                      query = list(action  = "query",
                                                   pageids = pageid[i],
                                                   format = "json",
                                                   prop = "revisions",
                                                   rvprop = "ids|timestamp",
                                                   rvlimit = "1",
                                                   rvstart = str_c(begin[k], "T00:00:00Z"),
                                                   rvend = str_c(end[k], "T24:59:59Z"),
                                                   rvdir = "newer"),
                                      times = 1000)),
            silent = TRUE)
        }
        oldid <- fromJSON(rev)$query$pages[[1]]$revisions[,c(1,3)]
        ids[ids$pageid == pageid[i],][k, 1:2] <- if(is.null(oldid)) NA else oldid
        # ids[[1]][k,1:2] <- if(is.null(oldid)) NA else oldid
        if (i > 1) {
          cat(".")
        }
      }
      if (i == 1) {
        cat("---- MP ", i, " ")
        } else {
          cat(" MP ", i, " ")
        }
    }
    #ids <- if(isTRUE(fill)) lapply(ids, na.locf) else ids
    ids <- if(isTRUE(fill)) na.locf(ids) else ids
    if (isTRUE(download)) {
      if(!dir.exists(folder)) dir.create(path = folder)
      #for (i in 1:length(url)) {
      for (i in 1:dim(data[[z]])[1]) {
        #if(!dir.exists(str_c(folder, "/", title[i]))) dir.create(path = str_c(folder, "/", title[i]))
        if(!dir.exists(str_c(folder, "/", pageid[i]))) dir.create(path = str_c(folder, "/", pageid[i]))
        #link <- na.omit(str_c("https://en.wikipedia.org/w/index.php?title=", title, "&oldid=",
        #                      ids[[i]][[1]]))
        link <- na.omit(str_c("https://", langcode, ".wikipedia.org/w/index.php?curid=", pageid[i], "&oldid=",
                              ids[ids$pageid == pageid[i],oldid]))
        #dest <- na.omit(str_c(folder, "/", title[i], "/", title[i], "_", ids[[i]][[1]], ".html"))
        dest <- na.omit(str_c(folder, "/", pageid[i], "/", pageid[i], "_", ids[ids$pageid == pageid[i],oldid], ".html"))
        invisible(mapply(function(x, y) {
          download.file(x, y)
          # Sys.sleep(sample(seq(0, 1, by=0.001), 1))
        }, link, dest))
      }
    }
    if (run == 1) {
      ids_list <- list(ids)
    } else {
      ids_list <- c(ids_list, list(ids))
      cat("\nLegislature", z, "finished\n")
    }
    run <- run + 1
  }
  return(ids_list)
}


#### DATA EXTRACTION FROM WIKIDATA ======================================================
wikiData <- function (item, entity = NULL, property, date = FALSE, location = FALSE, serial = FALSE,
                      qualifier = NULL, qualifier_names = NULL, qualifier_class = NULL, unique = NULL) {
  if (is.null(entity)) {
    entity <- get_item(id = item)
  }
  claims <- extract_claims(items = entity, claims = property)
  item <- item[!unlist(lapply(claims, is.na))]
  claims <- claims[!unlist(lapply(claims, is.na))]
  if (!isTRUE(date) & !isTRUE(location) & !isTRUE(serial)) {
    property_ids <- rep(list(NA), length(item))
    for (i in 1:length(item)) {
      property_ids[[i]] <- claims[[i]][[1]]$mainsnak$datavalue$value$id
    }
    if (!isTRUE(unique)) {
      idscount <- lapply(property_ids, table) %>%
        mapply(`[`, .,  lapply(., `>`, 1)) %>%
        unlist %>%
        subtract(1)
      property_ids_unique <- sort(c(unique(unlist(property_ids)), rep(names(idscount), idscount)))
    } else {
      property_ids_unique <- unique(unlist(property_ids))
    }
    property_entities_unique <- get_item(id = property_ids_unique)
    property_values_unique <- lapply(property_entities_unique, `$.data.frame`, "labels") %>%
      lapply(`$.data.frame`, "en") %>%
      lapply(`$.data.frame`, "value") %>%
      lapply(., function(x) ifelse(is.null(x), "unknown", x)) %>%
      unlist
    property_values <- rep(list(NA), length(property_ids_unique))
    for (i in 1:length(property_ids_unique)) {
      idx <- lapply(property_ids ,is.element, property_ids_unique[i])
      property_values[[i]] <- unlist(lapply(idx, any))
      property_values[[i]] <- ifelse(property_values[[i]], property_values_unique[i], NA)
    }
    property_values <- data.table(do.call(cbind, property_values))
    if (!missing(qualifier)) {
      qualifier_values <- rep(list(rep(list(NA), length(item))), length(qualifier))
      for (k in 1:length(qualifier)) {
        for (i in 1:length(item)) {
          if (is.null(claims[[i]][[1]]$qualifiers) | !(qualifier[k] %in% names(claims[[i]][[1]]$qualifiers))) {
            qualifier_values[[k]][[i]] <- NA
          } else {
            qualifier_values[[k]][[i]] <- lapply(claims[[i]][[1]]$qualifiers[qualifier[k]][[1]],
                                                 `$.data.frame`, "datavalue") %>%
              lapply(`$.data.frame`, 1) %>%
              lapply(`$.data.frame`, 1) %>%
              lapply(., function(x) ifelse(is.null(x), NA, x)) %>%
              unlist
          }
        }
        if (!missing(qualifier_class)) {
          if (qualifier_class == "Date") {
            qualifier_values[[k]] <- lapply(X = qualifier_values[[k]], FUN = str_replace_all,
                                            pattern = "\\+", replacement = "") %>%
              lapply(X = ., FUN = str_replace_all,
                     pattern = "T", replacement = " ") %>%
              lapply(X = ., FUN = str_replace_all,
                     pattern = "-00", replacement = "-01")
            }
        }
      }
      qualifier_columns <- rep(list(rep(list(NA), length(property_ids_unique))), length(qualifier))
      for (k in 1:length(qualifier)) {
        qualifier_values[[k]] <- lapply(qualifier_values[[k]], `length<-`, max(lengths(qualifier_values[[k]])))
        property_ids2 <- property_ids
        for (i in 1:length(property_ids_unique)) {
          qualifier_pos <- lapply(property_ids2, function(x)
            which(x %in% property_ids_unique[i])) %>%
            lapply(., `[`, 1)
          qualifier_columns[[k]][[i]] <- mapply(`[`, qualifier_values[[k]], qualifier_pos) %>%
            lapply(., function(x) ifelse(!length(x), NA, x)) %>%
            unlist
          property_ids2 <- mapply(replace, property_ids2, qualifier_pos, NA)
        }
        qualifier_columns[[k]] <- data.table(do.call(cbind, qualifier_columns[[k]]))
      }
      qualifier_columns <- do.call(cbind, qualifier_columns)
      if (!missing(qualifier_class)) {
        if (qualifier_class == "Date") {
          qualifier_columns[,  1:dim(qualifier_columns)[2]] <- lapply(qualifier_columns[, 1:dim(qualifier_columns)[2]], as.POSIXct)
        }
      }
      data <- cbind(ifelse(is.na(property_values), FALSE, TRUE), qualifier_columns)
      col_order <- order(names(data))
      col_names <- c(str_replace_all(tolower(property_values_unique), " ", "_"),
                     unlist(lapply(qualifier_names, str_c, "_", seq_along(property_ids_unique))))
      colnames(data) <- col_names
      setcolorder(data, col_order)
      data <- cbind(wikidataid = item, data)
    } else {
      data <- as.data.table(ifelse(is.na(property_values), FALSE, TRUE))
      col_order <- order(names(data))
      col_names <- str_replace_all(tolower(property_values_unique), " ", "_")
      colnames(data) <- col_names
      setcolorder(data, col_order)
      data <- cbind(wikidataid = item, data)
    }
  } else {
    if (isTRUE(date)) {
      property_ids <- rep(NA, length(item))
      for (i in 1:length(item)) {
        property_ids[[i]] <- ifelse(is.null(claims[[i]][[1]]$mainsnak$datavalue$value$time[1]),
                                    NA, claims[[i]][[1]]$mainsnak$datavalue$value$time[1])
      }
      data <- as.data.table(cbind(wikidataid = item, date = property_ids))
    }
    if (isTRUE(serial)) {
      property_ids <- rep(NA, length(item))
      for (i in 1:length(item)) {
        property_ids[[i]] <- ifelse(is.null(claims[[i]][[1]]$mainsnak$datavalue$value[1]),
                                    NA, claims[[i]][[1]]$mainsnak$datavalue$value[1])
      }
      data <- as.data.table(cbind(wikidataid = item, value = property_ids))
    }
    if (isTRUE(location)) {
      property_ids <- rep(list(NA), length(item))
      for (i in 1:length(item)) {
        property_ids[[i]] <- claims[[i]][[1]]$mainsnak$datavalue$value$id
      }
      property_ids <- unlist(lapply(property_ids, `[[`, 1))
            item <- item[!unlist(lapply(property_ids, is.na))]
      property_ids <- property_ids[!unlist(lapply(property_ids, is.na))]
            property_entities <- get_item(id = property_ids)
      claims2 <- extract_claims(items = property_entities, claims = "P625")
      item <- item[!unlist(lapply(claims2, is.na))]
      claims2 <- claims2[!unlist(lapply(claims2, is.na))]
      coordinates <- cbind(lat = rep(NA, length(item)), lon = rep(NA, length(item)))
      for (i in 1:length(item)) {
        coordinates[i,"lon"] <- claims2[[i]][[1]]$mainsnak$datavalue$value$longitude[1]
        coordinates[i,"lat"] <- claims2[[i]][[1]]$mainsnak$datavalue$value$latitude[1]
      }
      data <- cbind(wikidataid = item, as.data.table(coordinates))
    }
  }
  return(data)
}


#### RETRIEVAL OF WIKIPEDIA REVISION HISTORIES ==========================================
wikiHist <- function(pageid, project) {
  nms <- c("revid", "parentid", "user", "userid", "userhidden", "anon",
           "timestamp", "size", "minor", "comment", "commenthidden", "tags")
  cat("|")
  revision <- NULL
  while(is.null(revision)) {
    try(
      revision <- as.character(httr::RETRY("POST", str_c("https://", project,".org/w/api.php"),
                                           query = list(action  = "query",
                                                        pageids = pageid,
                                                        prop = "revisions",
                                                        rvprop = "ids|flags|timestamp|user|userid|size|comment|tags",
                                                        rvlimit = "500",
                                                        format = "json",
                                                        redirects = ""),
                                           times = 1000)),
      silent = TRUE
    )
  }
  revisions_full <- fromJSON(revision)$query$pages[[1]]$revisions
  missing <- setdiff(nms, names(revisions_full))
  revisions_full[missing] <- NA
  revisions_full <- revisions_full[nms]
  continue <- str_detect(revision, "rvcontinue")
  rvcontinue <- str_extract(revision, "[[:digit:]]+\\|[[:digit:]]+")
  cat(".")
  Sys.sleep(1)
  while(isTRUE(continue)) {
    revision <- NULL
    while(is.null(revision)) {
      try(
        revision <-as.character(RETRY("POST", str_c("https://", project,".org/w/api.php"),
                                      query = list(action  = "query",
                                                   pageids = pageid,
                                                   prop = "revisions",
                                                   rvprop = "ids|flags|timestamp|user|userid|size|comment|tags",
                                                   rvlimit = "500",
                                                   format = "json",
                                                   redirects = "",
                                                   rvcontinue = rvcontinue),
                                      times = 1000)),
        silent = TRUE
      )
    }
    continue <- str_detect(revision, "rvcontinue")
    rvcontinue <- str_extract(revision, "[[:digit:]]+\\|[[:digit:]]+")
    revision <- fromJSON(revision)$query$pages[[1]]$revisions
    missing <- setdiff(nms, names(revision))
    revision[missing] <- NA
    revision <- revision[nms]
    revisions_full <- rbind(revisions_full, revision)
    Sys.sleep(1)
    cat(".")
  }
  revisions_full <- revisions_full %>% mutate(pageid = pageid)
  return(revisions_full)
}


#### RETRIEVAL OF WIKIPEDIA PAGE TRAFFIC ================================================
wikiTraffic <- function(data, project) {
  title <- data$title
  pageid <- data$pageid
  traffic <- NULL
  for (i in 1:length(title)) {
    cat(".")
    while(is.null(traffic)) {
      traffic <- try(article_pageviews(project = project,
                                       article = title[1],
                                       user_type = "user",
                                       start = "2015070100",
                                       end = "2017103100") %>%
                          dplyr::select(article, agent, date, views),
                        silent = TRUE
      )
      if (class(traffic) == "try-error") {
        traffic <- NULL
      }
    }
    traffic$article <- pageid[i]
    names(traffic)[c(1,4)] <- c("pageid", "traffic")
    traffic <- suppressMessages(pad(traffic, interval = "day"))
    traffic[,1:3] <- na.locf(traffic[,1:3])
    traffic[is.na(traffic[4]), 4] <- 0
    if (i == 1) {
      traffic_list <- list(traffic)
    } else {
      traffic_list <- c(traffic_list, list(traffic))
    }
    traffic <- NULL
  }
  traffic <- bind_rows(traffic_list)
  Sys.sleep(1)
  return(traffic)
}


#### RETRIEVAL OF WIKIPEDIA IMAGE URLS ==================================================
imageUrl <- function(pageid, project) {
  image <- NULL
  urls <- rep(NA, times = length(pageid))
  for (i in 1:length(urls)) {
    while(is.null(image)) {
      try(
        image <- as.character(httr::RETRY("POST", str_c("https://", project,".org/w/api.php"),
                                          query = list(action  = "query",
                                                       pageids = pageid[i],
                                                       prop = "pageimages",
                                                       format = "json",
                                                       redirects = ""),
                                          times = 1000)),
        silent = TRUE
      )
    }
    image <- fromJSON(image)$query$pages[[1]]$thumbnail$source
    urls[i] <- if(is.null(image)) NA else image
    if (!is.na(urls[i])) {
      urls[i] <- str_replace(urls[i], pattern = "jpg\\/[[:digit:]].+?px",
                             replacement = "jpg\\/600px") %>%
        str_replace(., pattern = "JPG\\/[[:digit:]].+?px",
                    replacement = "JPG\\/600px") %>%
        str_replace(., pattern = "png\\/[[:digit:]].+?px",
                    replacement = "png\\/600px") %>%
        str_replace(., pattern = "jpeg\\/[[:digit:]].+?px",
                    replacement = "jpeg\\/600px")
    }
    image <- NULL
    cat(".")
  }
  urls <- na.omit(data.table(pageid = pageid, image_url = urls))
  return(urls)
}


#### RETRIEVAL OF UNDIRECTED WIKIPEDIA PAGE TITLES ======================================
undirectedTitle <- function(pageid, project) {
  revision <- NULL
  urls <- rep(NA, times = length(pageid))
  for (i in 1:length(urls)) {
    while(is.null(revision)) {
      try(
        revision <- as.character(httr::RETRY("POST", str_c("https://", project,".org/w/api.php"),
                                             query = list(action  = "query",
                                                          pageids = pageid[i],
                                                          prop = "info",
                                                          inprop= "url",
                                                          format = "json",
                                                          redirects = ""),
                                             times = 1000)),
        silent = TRUE
      )
    }
    urls[i] <- str_extract(fromJSON(revision)$query$pages[[1]]$fullurl, "(?<=\\/wiki\\/).+")
    revision <- NULL
    cat(".")
  }
  urls <- debugEnc(urls)
  urls <- data.table(title = urls, pageid = pageid)
  return(urls)
}


#### FACE++ API BINDING =================================================================
authFacepp <- function(api_key, api_secret){
  auth <- structure(list(api_key = api_key, api_secret = api_secret), class="FaceppProxy")
}

faceEst <- function(data, auth) {
  url <- data$image_url
  pageid <- data$pageid
  face <- NULL
  faces <- data.table(ethnicity = as.character(NA), smile_threshold = as.numeric(NA),
                      smile_intensity = as.numeric(NA), emo_sadness = as.numeric(NA),
                      emo_neutral = as.numeric(NA), emo_disgust = as.numeric(NA),
                      emo_anger = as.numeric(NA), emo_surprise = as.numeric(NA),
                      emo_fear = as.numeric(NA), emo_hapiness = as.numeric(NA),
                      beauty_female = as.numeric(NA), beauty_male = as.numeric(NA),
                      skin_dark_circles = as.numeric(NA), skin_stain = as.numeric(NA),
                      skin_acne = as.numeric(NA), skin_health = as.numeric(NA),
                      image_quality_threshold = as.numeric(NA), image_quality = as.numeric(NA),
                      pageid = data$pageid)
  run <- 0
  for (i in 1:length(url)) {
    run <- run + 1
    cat(run)
    while(is.null(face)) {
      try(
        face <- as.character(httr::RETRY("POST", "https://api-us.faceplusplus.com/facepp/v3/detect",
                                         query = list(api_key  = auth$api_key,
                                                      api_secret = auth$api_secret,
                                                      image_url = url[i],
                                                      return_landmark = 0,
                                                      return_attributes = "smiling,emotion,ethnicity,beauty,skinstatus,facequality"),
                                         times = 6, encode = "json")),
        silent = TRUE
      )
    }
    if (length(fromJSON(face)$faces) != 0) {
      ethnicity <- fromJSON(face)$faces[[1]]$ethnicity
      smile <- fromJSON(face)$faces[[1]]$smile
      emotion <- fromJSON(face)$faces[[1]]$emotion
      beauty <- fromJSON(face)$faces[[1]]$beauty
      skin <- fromJSON(face)$faces[[1]]$skinstatus
      quality <- fromJSON(face)$faces[[1]]$facequality
      faces[faces$pageid == pageid[i],][,1:18] <- c(ethnicity,smile,emotion,beauty,skin,quality)
      face <- NULL
      Sys.sleep(2)
    } else {
      face <- NULL
      Sys.sleep(2)
    }
  }
  return(faces)
}
