# ---------------------------------------------------------------------------------------
# legislatoR
# Sascha Göbel and Simon Munzert
# Script: functions
# August 2019
# ---------------------------------------------------------------------------------------


##### LANGUAGE CONVERSION OF MONTHS =====================================================
convMonth <- function(string, patterns, replacements) {
  for (i in seq_along(patterns))
    string <- gsub(patterns[i], replacements[i], string, perl=TRUE)
  string
}
months_foreign <- c("Januar", "J\u00E4nner", "Februar", "M\u00E4rz", "Mai", "Juni", "Juli",
                    "Oktober", "Dezember")
months_english <- c("January", "January", "February", "March", "May", "June", "July",
                    "October", "December")


##### ENCODING DEBUGGING ================================================================
debugEnc <- function(string) {
  debugtab <- read_html("http://www.i18nqa.com/debug/utf8-debug.html") %>%
    html_table(fill = TRUE)
  replacements <- c(debugtab[[1]][-1,9]) %>%
    c("\u010d", "\U0153", "-", "i", "g", "\u0161", "c", "c", "c", "\u0159",
      "\u011b", "\u017d", "\u017e", "\u0160", "\u0148", "\u010c", "\u0158", "\u016f", "\u0165", "\u010f", "\u0164",
      "\u2013", "\u2014", ., "'", "\"")
  patterns <- str_replace_all(debugtab[[1]][-1,11], "  |%C3", "") %>%
    c("%C4%8D", "%C5%93", "_%E2%80%A0_", "%C4%B1", "%C4%9F", "%C5%A1", "%C4%87", "%C4%8D", "%C4%87", "%C5%99",
      "%C4%9B", "%C5%BD", "%C5%BE", "%C5%A0", "%C5%88", "%C4%8C", "%C5%98", "%C5%AF", "%C5%A5", "%C4%8F", "%C5%A4",
      "%E2%80%93", "%E2%80%94", ., "%27", "%22")
  string <- str_replace_all(string, "%C3", "")
  for (i in seq_along(patterns)) {
    #string <- gsub(patterns[i], replacements[i], string, perl=TRUE)
    string <- str_replace_all(string, patterns[i], replacements[i])
  }
  string
}


##### DOWNLOAD OF HTMLS =================================================================
getHtml <- function(source, folder, country) {
  for(i in 1:length(source)) {
    if (!dir.exists(folder)) dir.create(path = folder)
    download.file(source[i], destfile = str_c(folder, country, "-", i, ".html"))
    Sys.sleep(1)
  }
}


#### RETRIEVAL OF POLITICIANS' WIKIDATA AND PAGE IDS ====================================
wikiIDs <- function(url, corp = NULL) {
  if (!missing(corp)) {
    url <- url$url
  }
  url <- debugEnc(string = url)
  title <- str_extract(url, "(?<=/)[[:upper:]].+")
  langcode <- str_extract(url, "(?<=//)[[:alpha:]]{2}") #  str_extract(url[1], "(?<=//)[[:alpha:]]{2}")
  ids <- data.table(pageid = rep(as.integer(NA), length(title)),
                    wikidataid = rep(as.character(NA), length(title)))
  k <- 1
  cat("|")
   #cat(k) # for debug purposes
  for (i in 1:length(url)) {
    if (is.na(url[i])) {
      next
    }
    # for (i in 201:300) {
    id <- NULL
    while (is.null(id)) {
      try(
        id <- as.character(RETRY("POST", str_c("https://", langcode[i],".wikipedia.org/w/api.php"), # langcode
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
   #  ids[i,2] <- fromJSON(id)$query$pages[[1]]$pageprops$wikibase_item # for debug purposes
    check <- fromJSON(id)$query$pages[[1]]$pageprops$wikibase_item
    if (!is.null(check)) {
       ids[i,2] <- check
    }
    k <- k + 1   # for debug purposes
    cat(k, ", ")
    cat(".")
  }
  return(ids)
}

#### RETRIEVAL OF POLITICIANS' WIKIPEDIA URLS ===========================================
wikiURLS <- function(ids, langcode) { # wikidataid
  cat("|")
  urls <- rep(NA, length(ids))
  for (i in 1:length(ids)) {
    cat(".")
    if (is.na(ids[i])) {
      next
    }
    url <- as.character(RETRY("POST", "https://www.wikidata.org/w/api.php",
                              query = list(action  = "wbgetentities",
                              format = "json",
                              props = "sitelinks/urls",
                              ids = ids[i],
                              sitefilter = langcode),
                              times = 1000))
    Sys.sleep(1)
    check <- fromJSON(url)$entities[[1]]$sitelinks
    if (length(check) > 0) {
      urls[i] <- fromJSON(url)$entities[[1]]$sitelinks[[1]][[4]]
    }
  }
  return(urls)
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
      property_ids_unique <- na.omit(unique(unlist(property_ids))) # remove na.omit
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
        test_lon <- claims2[[i]][[1]]$mainsnak$datavalue$value$longitude[1]
        test_lat <- claims2[[i]][[1]]$mainsnak$datavalue$value$latitude[1]
        if (length(test_lon == 1) & length(test_lat == 1)) {
          coordinates[i,"lon"] <- claims2[[i]][[1]]$mainsnak$datavalue$value$longitude[1]
          coordinates[i,"lat"] <- claims2[[i]][[1]]$mainsnak$datavalue$value$latitude[1]
        }
      }
      data <- cbind(wikidataid = item, as.data.table(coordinates))
    }
  }
  return(data)
}


#### RETRIEVAL OF WIKIPEDIA REVISION HISTORIES ==========================================
wikiHist <- function(pageid, project, uniqueid = NULL) {
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
  if (!is.null(uniqueid)) {
    revisions_full <- revisions_full %>% mutate(pageid_unique = uniqueid)
  }
  return(revisions_full)
}


#### RETRIEVAL OF WIKIPEDIA PAGE TRAFFIC ================================================
wikiTraffic <- function(data, project) {
  title <- data$title
  pageid <- data$pageid
  traffic <- NULL
  for (i in 1:length(title)) {
    cat(".")
    er <- FALSE
    while(is.null(traffic)) {
      traffic <- try(article_pageviews(project = project,
                                       article = title[i],
                                       user_type = "user",
                                       start = "2015070100",
                                       end = "2018123100") %>%
                       dplyr::select(article, agent, date, views),
                     silent = TRUE
      )
      if (class(traffic) == "try-error") {
        traffic <- data.frame(article = NA, agent = NA, date = NA, views = NA, stringsAsFactors = FALSE)
        er <- TRUE
      }
    }
    if (!isTRUE(er)) {
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
    }
    traffic <- NULL
  }
  traffic <- bind_rows(traffic_list)
  return(traffic)
}

wikiTrafficNew <- function(data, project, uniqueid = NULL) {
  title <- data$title
  if (is.null(uniqueid)) {
    pageid <- data$pageid
  } else {
    pageid_unique <- data$pageid_unique
  }
  traffic <- NULL
  for (i in 1:length(title)) {
    cat(".")
    while(is.null(traffic)) {
      traffic <- try(wp_trend(page = title[i],
                              lang = project[i],
                              from = "2007-12-01",
                              to = Sys.Date()) %>%
                       dplyr::select(date, views) %>%
                       filter(views > 0) %>%
                       mutate(date = lubridate::as_date(date)),
                     silent = TRUE
      )
      if (class(traffic) == "try-error") {
        traffic <- try(wp_trend(page = title[i],
                                lang = project[i],
                                from = "2016-01-01",
                                to = Sys.Date()) %>%
                         dplyr::select(date, views) %>%
                         filter(views > 0) %>%
                         mutate(date = lubridate::as_date(date)),
                       silent = TRUE
        )
        if (class(traffic) == "try-error") {
          next
        }
      }
    }
    if (is.null(uniqueid)) {
      traffic$pageid <- pageid[i]
      traffic <- traffic %>%
        dplyr::select(pageid, date, traffic = views)
    } else {
      traffic$pageid_unique <- pageid_unique[i]
      traffic <- traffic %>%
        dplyr::select(pageid = pageid_unique, date, traffic = views)
    }
    traffic <- suppressMessages(padr::pad(traffic, interval = "day"))
    traffic[,1:2] <- zoo::na.locf(traffic[,1:2])
    traffic[is.na(traffic[3]), 3] <- 0
    if (i == 1) {
      traffic_list <- list(traffic)
    } else {
      traffic_list <- c(traffic_list, list(traffic))
    }
    traffic <- NULL
  }
  traffic <- bind_rows(traffic_list)
  return(traffic)
}


#### RETRIEVAL OF WIKIPEDIA IMAGE URLS ==================================================
imageUrl <- function(pageid, project, uniqueid = NULL) {
  image <- NULL
  urls <- rep(NA, times = length(pageid))
  for (i in 1:length(urls)) {
    while(is.null(image)) {
      try(
        image <- as.character(httr::RETRY("POST", str_c("https://", project,".org/w/api.php"), # project[i]
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
  urls <- na.omit(data.table(pageid = pageid, image_url = urls)) # pageid = uniqueid
  return(urls)
}


#### RETRIEVAL OF UNDIRECTED WIKIPEDIA PAGE TITLES ======================================
undirectedTitle <- function(pageid, project, uniqueid = NULL) {
  revision <- NULL
  urls <- rep(NA, times = length(pageid))
  for (i in 1:length(urls)) {
    while(is.null(revision)) {
      try(
        revision <- as.character(httr::RETRY("POST", str_c("https://", project,".org/w/api.php"), # project[i] - adjust if error
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
  if (is.null(uniqueid)) {
    urls <- data.table(title = urls, pageid = pageid)
  } else {
    urls <- data.table(title = urls, pageid = pageid, pageid_unique = uniqueid)
  }
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
  faces <- data.table(ethnicity = as.character(NA), 
                      #smile_threshold = as.numeric(NA),
                      #smile_intensity = as.numeric(NA), emo_sadness = as.numeric(NA),
                      #emo_neutral = as.numeric(NA), emo_disgust = as.numeric(NA),
                      #emo_anger = as.numeric(NA), emo_surprise = as.numeric(NA),
                      #emo_fear = as.numeric(NA), emo_hapiness = as.numeric(NA),
                      #beauty_female = as.numeric(NA), beauty_male = as.numeric(NA),
                      #skin_dark_circles = as.numeric(NA), skin_stain = as.numeric(NA),
                      #skin_acne = as.numeric(NA), skin_health = as.numeric(NA),
                      #image_quality_threshold = as.numeric(NA), image_quality = as.numeric(NA),
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
                                                      return_attributes = "ethnicity"),
                                         times = 20, encode = "json")),
        silent = TRUE
      )
    }
    if (length(fromJSON(face)$faces) != 0) {
      ethnicity <- fromJSON(face)$faces[[1]]$ethnicity
      #smile <- fromJSON(face)$faces[[1]]$smile
      #emotion <- fromJSON(face)$faces[[1]]$emotion
      #beauty <- fromJSON(face)$faces[[1]]$beauty
      #skin <- fromJSON(face)$faces[[1]]$skinstatus
      #quality <- fromJSON(face)$faces[[1]]$facequality
      #faces[faces$pageid == pageid[i],][,1:18] <- c(ethnicity,smile,emotion,beauty,skin,quality)
      faces[faces$pageid == pageid[i],][,1] <- c(ethnicity)
      face <- NULL
      Sys.sleep(5)
    } else {
      face <- NULL
      Sys.sleep(5)
    }
  }
  return(faces)
}


#### UPDATE SOCIAL MEDIA INFORMATION ====================================================
updateSocial <- function(previous,current) {
  previous <- readRDS(previous)
  current <- readRDS(current)
  #missing_1 <- which(!(previous$wikidataid %in% current$wikidataid))
  #current <- rbind(current, previous[missing_1,])
  cols <- colnames(previous)
  for (i in 2:length(cols)) {
    na_curr <- current$wikidataid[which(is.na(current[,cols[i]]))]
    na_prev <- previous$wikidataid[which(previous$wikidataid %in% na_curr)]
    na_prev_idx <- which(!is.na(previous[previous$wikidataid %in% na_prev, cols[i]]))
    if(length(na_prev_idx) > 0) {
      current[current$wikidataid %in% na_prev[na_prev_idx],cols[i]] <- previous[previous$wikidataid %in% na_prev[na_prev_idx],cols[i]]
      }
    print(i)
    }
  return(current)
}


#### CANADA WIKIPEDIA INFORMATION EXTRACTION ============================================
collectorCanada <- function(source) {
  #source <- str_c("https://en.wikipedia.org/wiki/List_of_House_members_of_the_",
  #                sapply(1:42, toOrdinal), "_Parliament_of_Canada")
  source <- mixedsort(list.files(source, full.names = TRUE), decreasing = TRUE)
  langcode <- "en"
  duration <- list(c("1867-09-20", "1872-10-11"), c("1872-10-12", "1874-01-21"),
                   c("1874-01-22", "1878-09-16"), c("1878-09-17", "1882-06-19"),
                   c("1882-06-20", "1887-02-21"), c("1887-02-21", "1891-03-04"),
                   c("1891-03-05", "1896-06-22"), c("1896-06-23", "1900-11-06"),
                   c("1900-11-07", "1904-11-02"), c("1904-11-03", "1908-10-25"),
                   c("1908-10-26", "1911-09-20"), c("1911-09-21", "1917-12-16"),
                   c("1917-12-17", "1921-12-05"), c("1921-12-06", "1925-10-28"),
                   c("1925-10-29", "1926-09-13"), c("1926-09-14", "1930-07-27"),
                   c("1930-07-28", "1935-10-13"), c("1935-10-14", "1940-03-25"),
                   c("1940-03-26", "1945-06-10"), c("1945-06-11", "1949-06-26"),
                   c("1949-06-27", "1953-08-09"), c("1953-08-10", "1957-06-09"),
                   c("1957-06-10", "1958-03-30"), c("1958-03-31", "1962-06-17"),
                   c("1962-06-18", "1963-04-07"), c("1963-04-08", "1965-11-07"),
                   c("1965-11-08", "1968-06-24"), c("1968-06-25", "1972-10-29"),
                   c("1972-10-30", "1974-07-07"), c("1974-07-08", "1979-05-21"),
                   c("1979-05-22", "1980-02-17"), c("1980-02-18", "1984-09-03"),
                   c("1984-09-04", "1988-11-20"), c("1988-11-21", "1993-10-24"),
                   c("1993-10-25", "1997-06-01"), c("1997-06-02", "2000-11-26"),
                   c("2000-11-27", "2004-06-27"), c("2004-06-28", "2006-01-22"),
                   c("2006-01-23", "2008-10-13"), c("2008-10-14", "2011-05-01"),
                   c("2011-05-02", "2015-10-18"), c("2015-10-19", "2019-10-20"),
                   c("2019-10-21", "2023-10-16"))
  condition1 <- "contains(@title, 'Bloc')"
  condition2 <- "contains(., 'members') or contains(., 'membership') or contains(., 'Membership') or contains(., 'Standings')"
  condition3 <- "contains(@title, 'Lieutenant') or contains(@title, 'Minister') or 
  contains(@title, 'Superior') or contains(@title, 'Supreme') or 
  contains(@title, 'Honourable') or contains(@title, 's Land') or 
  contains(@title, 'Welland Canal') or contains(@title, 'Template')"
  condition4 <- "contains(@title, 'Andrew McNaughton') or contains(@title, 'Arthur Meighen')"
  condition5 <- "contains(@href, 'cite') or contains(@href, 'endnote')"
  condition6 <- "contains(@title, 'Elmer MacKay') or contains(@title, 'Yvon L') or 
  contains(@title, 'opold Corriveau') or contains(@title, 'Maurice Dupras') or 
  contains(@title, 'Claude G. Lajoie') or contains(@title, 'Derek Blackburn') or 
  contains(@title, 'Jack Murta') or contains(@title, 'Doug Rowland') or 
  contains(@title, 'Bill Knight') or contains(@title, 'Thomas Speakman Barnett') or 
  contains(@title, 'Tommy Douglas')"
  condition7 <- "contains(@title, 'By-election')"
  query <- c(rep(str_c("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                       td[position()>=2 and position() <= (last() - 1)]//a[not(", condition3,
                       " or ", condition5, ")]"), times = 18),
             str_c("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                   td[position()>=2 and position() <= (last() - 1)]//a[not(", condition3,
                   " or ", condition4, " or ", condition5, ")]"),
             rep(str_c("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                       td[position()>=2 and position() <= (last() - 1)]//a[not(", condition3,
                       " or ", condition5, ")]"), times = 8),
             str_c("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                   td[position()>=2 and position() <= (last() - 1)]//a[not(", condition3,
                   " or ", condition6, " or ", condition5, ")]" ),
             str_c("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                   td[position()>=2 and position() <= (last() - 1)]//a[not(", condition3,
                   " or ", condition5, ")]"),
             str_c("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                   td[(last() - 1)]//a[1][not(", condition3, " or ", condition7, " or ",
                   condition5, ")]"),
             rep(str_c("//h3[not(", condition2, ")]", "/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                       td[3]/ancestor::tr/td[3]//a[1][not(", condition3, " or ", condition7, " or ",
                       condition5, ")]"), times = 2),
             str_c("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                   td[2]//a[1][not(", condition3, " or ", condition7, " or ", condition5, ")]"),
             rep(str_c("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                       td[(last() - 1)]//a[1][not(", condition3, " or ", condition7, " or ",
                       condition5, ")]"), times = 3),
             rep(str_c("//h3[not(", condition2, ")]", "/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                       td[3]/ancestor::tr/td[2]//a[1][not(", condition3, " or ", condition7, " or ",
                       condition5, ")]"), times = 7))
  query_party <- c(rep("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                       td[last()]", times = 30),
                   rep(str_c("//h3[not(", condition2, ")]", "/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                             td[3]/ancestor::tr/td[last()]"), times = 2),
                   "//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                   td[3]",
                   str_c("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                         td[last()]//a[not(", condition1, ")]"),
                   str_c("//h3[not(", condition2, ")]", "/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                         td[3]/ancestor::tr/td[last()]"),
                   "//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                   td[last()]",
                   rep(str_c("//h3[not(", condition2, ")]", "/span/ancestor::h3/following-sibling::table[1]/tbody/tr/
                             td[3]/ancestor::tr/td[3]"), times = 7)
                   )
  regex_term <- "[[:digit:]]+"
  query_constituency <- c(rep("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[4]/ancestor::tr/td[1]/a", times = 2),
                          "//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[4]/ancestor::tr/td[1]",
                          rep("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[4]/ancestor::tr/td[1]/a", times = 24),
                          rep("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[3]/ancestor::tr/td[1]/a", times = 2),
                          rep("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[3]/ancestor::tr/td[position()>=1 and position() <= (last() - 2)]/a",
                              times = 3),
                          str_c("//h3[not(", condition2, ")]/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[4]/ancestor::tr/td[last()]/a"),
                          rep("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[4]/ancestor::tr/td[2]/a", times = 2),
                          "//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[3]/ancestor::tr/td[1]/a",
                          rep(str_c("//h3[not(", condition2, ")]/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[4]/ancestor::tr/td[last()]/a"), times = 7))
  query_multiplier <- c(rep("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[4]/ancestor::tr/td[1]/a/parent::td", times = 2),
                        "//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[4]/ancestor::tr/td[1]",
                        rep("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[4]/ancestor::tr/td[1]/a/parent::td", times = 24),
                        rep("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[3]/ancestor::tr/td[1]/a/parent::td", times = 2),
                        rep("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[3]/ancestor::tr/td[position()>=1 and position() <= (last() - 2)]/a/parent::td", 
                            times = 3),
                        str_c("//h3[not(", condition2, ")]/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[4]/ancestor::tr/td[last()]/a/parent::td"),
                        rep("//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[4]/ancestor::tr/td[2]/a/parent::td", times = 2),
                        "//h3/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[3]/ancestor::tr/td[1]/a/parent::td",
                        rep(str_c("//h3[not(", condition2, ")]/span/ancestor::h3/following-sibling::table[1]/tbody/tr/td[4]/ancestor::tr/td[last()]/a/parent::td"),
                            times = 7))
  replace_idx <- c(list(56), rep(list(0), times = 10), list(54), rep(list(0), times = 18), list(71), list(280), list(0), 
                   list(c(12, 37, 57, 62, 71, 76, 87, 91, 94, 99, 100, 107, 247)), list(c(113, 153, 204, 240)), list(0),
                   list(c(1:6,7,8:12,14,15:26,28:51,52,55,60:62,64,65,66,68,78,82,83,87,90,91, 93, 97, 98, 100, 104,124,
                          142,161,169,173,224,228,262,264,269,275,278,285:287,290,291,293:296,298)),
                   list(c(14, 56, 67, 149, 155, 159, 290)), list(c(10, 89, 97, 107, 124,134, 137, 158, 198, 301, 305)), 
                   list(c(2, 51, 189, 238)), list(c(11,15,42,89,110,153,172,177,194,197,219,238,244,260,271,272,283,284,290)), 
                   list(c(6,7,8,13,27,33,51,71,102,114,121,127,170,174,175,198,202,210,233,250,253,271,283,289,291,302,
                          306,310,311,316,322,328,337)), list(0))
  replacement <- c(list(1), rep(list(0), times = 10), list(1), rep(list(0), times = 18), list(1), list(1), list(0),
                   list(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), list(c(1, 1, 1, 1)), list(0),
                   list(c(rep(1,6),2,rep(1,5),1,rep(1,12),rep(1,24),1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,2,
                          1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1)),
                   list(c(1,1,1,1,1,1,1)), list(c(1,1,1,1,1,1,1,1,1,1,1)), list(c(1,1,1,1)), 
                   list(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)), list(1), list(0))
  query_constituency2 <- c(rep("//h3[not(contains(., 'members') or contains(., 'membership') or contains(., 'Membership') or contains(., 'Standings'))]/span[@id]", times = 9),
                           rep("//h3[not(contains(., 'members') or contains(., 'membership') or contains(., 'Membership') or contains(., 'Standings'))]/span[@id]/a/ancestor::span", times = 18),
                           rep("//h3[not(contains(., 'members') or contains(., 'membership') or contains(., 'Membership') or contains(., 'Standings'))]/span[@id]", times = 16))
  query_multiplier2_1 <- unlist(lapply(str_split(query, "/tbody"), `[`, 1))
  query_multiplier2_2 <- unlist(lapply(str_split(query, "table\\[1\\]"), `[`, 2)) %>%
    str_replace("^", "\\.")
  query_service <- str_c(query, "/ancestor::td")
  replace_idx2 <- c(list(0),list(0),list(c(107,11)),list(c(204,162,117)),
                    list(c(25,111,112,166, 171, 172, 175,177,184,204, 205, 239, 244)),
                    list(c(261, 162, 159, 160, 142, 142, 141, 124, 125, 118, 119, 107, 106, 86, 
                           82, 71, 72, 69, 70, 67, 68, 50, 51, 61, 63, 48, 49, 45, 46, 1)),
                    list(c(294, 295, 285, 286, 284, 282, 283, 273, 254, 251, 252, 242, 243, 224, 218, 
                           219, 192, 188, 189, 186, 180, 175, 176, 173, 174, 169, 165, 166, 167, 168, 
                           160, 157, 155, 153, 147, 139, 137, 138, 135, 118, 119, 108, 109, 102, 103,
                           95, 96, 88, 84, 82, 83, 74, 75, 71, 72, 69, 70, 64, 65, 58, 59, 60, 61,
                           55, 56, 52, 53, 54, 48, 37, 32, 22, 18, 19, 15, 13, 14, 10, 6, 7)),
                    list(c(241,242,231,232,229,230,225,226,204,205,189,190,185,186,176,173,
                           174,163,164,142,142,130,131,51,52,42,7)),
                    list(c(251,252,231,232,210,211,198,199,191,192,177,178,175,172,165,
                           166,141,130,129,123,30,31,32,33)),
                    list(c(252,253,245,246,190,191,170,168,166,167,163,144,133,130,125,116,117,67,68,41,42)),
                    list(c(185,186,171,142,143,115,66)),
                    list(c(242,243,229,230,224,225,219,220,196,197,192,193,188,189,181,
                           182,156,157,150,151,152,153,145,140,87,88,81,82,25,26,23,24,
                           21,22,15,16)),
                    list(c(146,135,122,61,62,27,24,25)),
                    list(c(270,271,249,250,230,231,226,227,208,209,191,192,180,181,177
                           ,170,148,149,150,151,96,97,93,94,76,77,78,79,73,74,56,57,41,
                           42)),
                    list(c(161,110,111)),
                    list(c(269,270,266,267,262,263,249,250,238,239,229,230,225,226,196,197,
                           192,193,179,163,164,143,125,126,112,113,86,87,54,55,34,35,21,22)),
                    list(c(261,262,251,252,239,240,227,228,210,211,190,191,173,170,171,
                           153,154,155,156,135,133,134,120,121,95,96,61,62,58,59,39,40,7,8)),
                    list(c(148,136,92,93)), list(c(161,151)), list(c(136,131,64)), list(c(169, 142)),
                    list(c(169,142,262,251,66,194,196,208,213,215,217,223,231,237,246,249)),
                    list(c(4,49,61,141,154,156,161,185)),list(c(267)), list(c(164,154)),
                    list(c(0)),list(c(159,62)), list(c(16,42,56,58,102,108,198,202,208,244,250)),
                    list(c(0)), list(c(243,206,27,5,23,112,117,131,156,160,168,194,74,95,109,13,66,75,
                                       103,104,107,8,155,32,58)), list(c(223,2)),
                    list(c(273,17,212,115,151,194,186,59,71,155,8,141)),
                    list(c(63,6,138,287,251,100,64,7,139,288,252,101)),
                    list(c(23,167,68,46,240,24,168,69,47,241)),
                    list(c(4,6,137,65,85,103,176,47,98,5,7,138,66,86,104,177,48,99)),
                    list(c(17,57,76,100,206,210,234,279,281)),
                    list(c(206,283,68,87,7,90,294,246,225,173,288,255,165,285,128,63,
                           207,284,69,88,8,91,295,247,226,174,289,256,232)),
                    list(c(92,56,291,93)),
                    list(c(88,155,89,156,81,98,205,213,310,94,238,95,82,99,111,138,128,239,311,214,206)),
                    list(c(44,101,267,244,102,45,268,245,78,206,69,243,52,2,207,79,70)),
                    list(c(0)),list(c(0)), list(c(0))) 
  replacement2 <- c(list(0),list(0),list(c(0,0)),list(c(1036,552,1292)),
                    list(c(1707, 524, 428, 310, 1168, 538, 73, 863, 1500, 492, 537, 406, 56)),
                    list(c(1472, 162, 1472, 1472, 1167, 1167, 304, 1167, 1167, 1167, 1167, 197, 
                           1255, 1126, 1231, 1472, 1472, 1472, 1472, 1174, 1174, 963, 963, 1472, 
                           1472, 508, 508, 541, 541, 831)),
                    list(c(1475, 1475, 1600, 1600, 335, 1935, 1935, 312, 371, 1935, 1935, 1935, 1935, 
                           530, 1935, 1935, 421, 1935, 1935, 343, 357, 1935, 1935, 1935, 1935, 371, 1935, 
                           1935, 1935, 1935, 1008, 352, 1743, 376, 373, 358, 1935, 1935, 336, 1935, 1935, 
                           1935, 1935, 1935, 1935, 1935, 1935, 1755, 343, 1935, 1935, 1935, 1935, 1935,
                           1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
                           1935, 1797, 1633, 357, 628, 1935, 1935, 993, 1935, 1935,498, 1935, 1935)),
                    list(c(1596,1596,1596,1596,1596,1596,1596,1596,1596,1596,1596,1596,1596,
                           1596,904,1596,1596,1596,1596,1596,1596,1596,1596,1596,1596,
                           301,157)),
                    list(c(1456,1456,1456,1456,1456,1456,1178,1178,1456,1456,1456,1456,
                           427,756,1456,1456,894,1456,1456,853,1456,1456,1456,1456)),
                    list(c(1452,1452,1452,1452,1452,1452,384,1145,1452,1452,1452,1090,
                           705,222,1145,1090,889,1452,1452,1452,1452)),
                    list(c(1059,1059,738,1059,1059,460,119)),
                    list(c(1125,1125,2278,2278,2278,2278,1125,1125,893,893,1476,1476,
                           2278,2278,2278,2278,2278,2278,2278,2278,2278,2278,263,
                           154,2278,2278,2278,2278,2278,2278,2278,2278,1890,1890,2278,2278)),
                    list(c(842,1148,722,1449,1449,1071,1449,1449)),
                    list(c(1422,1422,1422,1422,1422,1422,1422,1422,1422,1422,1422,1422,
                           1422,1422,84,1422,1422,1422,1422,1422,1422,1422,1422,407,
                           1422,1422,1422,1422,491,491,1422,1422,1422,1422)),
                    list(c(39,319,319)),
                    list(c(1412,1412,1412,1412,1412,1412,1412,1412,1412,1412,1412,1412,1412,
                           1412,1412,1412,1154,1154,1231,1412,1412,1049,1412,1412,1412,1412,
                           1412,1412,1412,1412,1203,1203,1412,1412)),
                    list(c(1903,1903,1903,1903,1903,1903,1903,1903,1903,1903,1903,1903,
                           1519,1903,1903,1903,1903,1519,1519,1358,1903,1903,1903,1903,
                           1903,1903,1903,1903,1903,1903,1903,1903,1903,1903)),
                    list(c(539,378,1624,1624)), list(c(685,146)), list(c(497,1093,861)),
                    list(c(1064, 1064)),
                    list(c(1064,1064,1027,439,439,439,1146,1384,1404,1040,1384,1384,1384,306,306,1027)),
                    list(c(737,943,621,224,805,455,455,777)),list(c(189)), list(c(259,553)),
                    list(c(0)),list(c(581,581)),
                    list(c(1070,875,1044,864,911,1070,792,657,1231,287,230)), list(c(0)),
                    list(c(1561,1561,1561,1561,1561,1561,1561,1561,1561,1561,1561,1561,1561,1561,
                           1561,1051,1051,1051,1051,1051,1051,833,833,463,463)), list(c(181,120)),
                    list(c(1288,1288,1191,967,967,967,546,546,441,420,420,203)),
                    list(c(1385,1049,1049,1049,755,755,152,488,488,488,782,782)),
                    list(c(749,630,630,448,112,1048,1167,1167,1349,1685)),
                    list(c(882,882,882,882,882,882,476,476,476,432,432,432,432,432,432,838,838,838)),
                    list(c(943,578,578,578,213,578,578,578,213)),
                    list(c(413,413,414,424,430,484,484,526,537,683,837,840,1016,
                           1110,1110,1256,894,894,893,883,877,823,823,781,770,624,470,
                           467,197)),
                    list(c(171,376,499,401)),
                    list(c(217,240,685,685,370,394,525,535,550,552,585,391,391,391,732,805,921,209,209,209,209)),
                    list(c(181,198,219,337,591,591,591,591,563,688,688,738,741,761,152,152,152)),
                    list(c(0)), list(c(0)), list(c(0)))
  regex_service1 <- c(rep("(from|acclaimed in|re-elected).+?[[:digit:]]{4}?", times = 4),
                      rep("(from|acclaimed in|re-elected|by-election|declared).+?[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}?", times = 7),
                      rep(c("(from|acclaimed in|re-elected|by-election|declared).+?[[:digit:]]{4}?",
                            "(from|acclaimed in|re-elected|by-election|declared).+?[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}?"), times = 6),
                      rep("(from|acclaimed in|re-elected|by-election|declared).+?[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}?",times =4),
                      rep(".+", times =13),
                      rep("(from|acclaimed in|re-elected|after).+?[[:digit:]]{4}?", times = 2))
  regex_service2 <- c(rep("(to |died|until|overturned|defeated in|ended term|resigned).+?[[:digit:]]{4}?", times = 4),
                      rep("(to |died|until|overturned|defeated in|ended term|resigned|appointed|unseated|voided|expelled).+?[[:digit:]]{4}?", times = 23),
                      rep(".+", times =13),
                      rep("(to |died|until|overturned|defeated in|ended term|resigned).+?[[:digit:]]{4}?", times = 2))
  append_names <- c(rep(list(0), 41), 
                    list(c("Bob Benzen", "Stephanie Kusie", "Glen Motz", "Dane Lloyd",
                           "Churence Rogers", "Michael Barrett", "Mary Ng", "Mona Fortier",
                           "Jean Yip", "Emmanuella Lambropoulos", "Rosemarie Falk")))
  append_urls <- c(rep(list(0), 41),
                   list(c("https://en.wikipedia.org/wiki/Bob_Benzen",
                          "https://en.wikipedia.org/wiki/Stephanie_Kusie",
                          "https://en.wikipedia.org/wiki/Glen_Motz",
                          "https://en.wikipedia.org/wiki/Dane_Lloyd",
                          "https://en.wikipedia.org/wiki/Churence_Rogers",
                          "https://en.wikipedia.org/wiki/Michael_Barrett_(Canadian_politician)",
                          "https://en.wikipedia.org/wiki/Mary_Ng",
                          "https://en.wikipedia.org/wiki/Mona_Fortier",
                          "https://en.wikipedia.org/wiki/Jean_Yip",
                          "https://en.wikipedia.org/wiki/Emmanuella_Lambropoulos",
                          "https://en.wikipedia.org/wiki/Rosemarie_Falk")))
  append_parties <- c(rep(list(0), 41), 
                      list(c("Conservative", "Conservative", "Conservative", "Conservative", 
                             "Liberal", "conservative", "Liberal",
                             "Liberal", "Liberal", "Liberal", "Conservative")))
  append_constituencies <- c(rep(list(0), 41), 
                             list(c("Calgary Heritage", "Calgary Midnapore", "Medicine Hat—Cardston—Warner", "Sturgeon River—Parkland",
                                    "Bonavista—Burin—Trinity", "Leeds—Grenville—Thousand Islands and Rideau Lakes",
                                    "Markham—Thornhill", "Ottawa—Vanier", "Scarborough—Agincourt", "Saint-Laurent",
                                    "Battlefords—Lloydminster")))
  append_constituencies2 <- c(rep(list(0), 41), 
                             list(c("Alberta", "Alberta","Alberta", "Alberta", "Newfoundland and Labrador",
                                    "Ontario", "Ontario", "Ontario", "Ontario", "Quebec", "Saskatchewan")))
  append_services <- c(rep(list(0), 41), 
                       list(c(930,930,1091,727,678,321,920,920,678,930,678)))
  append_session_start <- c(rep(list(0), 41), 
                            list(rep("2015-10-19", times = 11)))
  append_session_end <- c(rep(list(0), 41), 
                          list(rep("2019-10-20", times = 11)))
  urls <- rep(list(NA), times = length(source))
  htmls <- rep(list(NA), times = length(source))
  names <- rep(list(NA), times = length(source))
  parties <- rep(list(NA), times = length(source))
  constituencies <- rep(list(NA), times = length(source))
  constituencies2 <- rep(list(NA), times = length(source))
  multipliers1 <- rep(list(NA), times = length(source))
  multipliers2 <- rep(list(NA), times = length(source))
  beginnings <- rep(list(NA), times = length(source))
  endings <- rep(list(NA), times = length(source))
  services <- rep(list(NA), times = length(source))
  session_start <- rep(list(NA), times = length(source))
  session_end <- rep(list(NA), times = length(source))
  for (i in 1:length(urls)) {
    htmls[[i]] <-  read_html(source[i], encoding = "UTF-8")
    urls[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_attr("href") %>%
      str_c("https://", langcode,".wikipedia.org", .)
    names[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_text()
    parties[[i]] <- html_nodes(x = htmls[[i]], xpath = query_party[i]) %>%
      html_text() %>%
      str_replace_all("\\n|Vacant|\\r", "") %>%
      extract(. !="")
    constituencies[[i]] <- html_nodes(x = htmls[[i]], xpath = query_constituency[i]) %>%
      html_text() %>%
      str_replace("\n", "")
    multipliers1[[i]] <- html_nodes(x = htmls[[i]], xpath = query_multiplier[i]) %>%
      html_attr("rowspan") %>%
      as.numeric() %>%
      replace(replace_idx[[i]], replacement[[i]]) %>%
      na.replace(1)
    constituencies[[i]] <- rep(constituencies[[i]], times = multipliers1[[i]])
    constituencies2[[i]] <- html_nodes(x = htmls[[i]], xpath = query_constituency2[i]) %>%
      html_text()
    multipliers2[[i]] <- html_nodes(x = htmls[[i]], xpath = query_multiplier2_1[i]) %>%
      lapply(., html_nodes, xpath = query_multiplier2_2[i]) %>% 
      lengths
    constituencies2[[i]] <- rep(constituencies2[[i]], times = multipliers2[[i]])
    beginnings[[i]] <- html_nodes(x = htmls[[i]], xpath = query_service[i]) %>%
      html_text() %>%
      str_replace_all("\n", "") %>%
      str_extract(regex_service1[i]) %>%
      str_replace_all("from |acclaimed in |re-elected in |by-election |,|of |declared elected |after ", "")
    if (i %in% c(1,2,3,20,41,42,43)) {
      beginnings[[i]] <- beginnings[[i]] %>%
        str_replace("^(?=[[:digit:]]{4})", "January 2, ") %>% 
        str_replace("(?<=[[:alpha:]])((,)? )(?=[[:digit:]]{4})", " 2, ") %>%
        mdy() %>%
        na.replace(duration[[i]][1]) 
    } else if (i %in% c(4,12,14,16,18,22)) {
      if (i %in% c(22)) {
        beginnings[[i]] <- str_replace(beginnings[[i]],"^(?=[[:digit:]]{4})", "1 January ") %>%
          str_replace("^(?=[[:alpha:]])", "1 ") 
      }
      beginnings[[i]] <- beginnings[[i]] %>%
        dmy() %>%
        na.replace(duration[[i]][1]) 
    } else if (i %in% c(28:40)) {
      beginnings[[i]][1:length(beginnings[[i]])] <- NA 
      beginnings[[i]] <- na.replace(beginnings[[i]], duration[[i]][1])
    } else {
      beginnings[[i]] <- beginnings[[i]] %>%
        ymd() %>%
        na.replace(duration[[i]][1]) 
    }
    endings[[i]] <- html_nodes(x = htmls[[i]], xpath = query_service[i]) %>%
      html_text() %>%
      str_replace_all("\n", "") %>%
      str_extract(regex_service2[i]) %>%
      str_replace_all("to |,|died |until |overturned in |defeated in |by-election |ended term |resigned |
                      unseated |Quebec legislative council |appointment |election voided |appointed |New Brunswick's |
                      Lieutenant-Governor |Manitoba |voided |expelled |by petition |Secretary of State |unseated |House of Commons Clerk on ", "")
    if (i %in% c(1,2,3,20,41,42,43)) {
      endings[[i]] <- endings[[i]] %>%
        str_replace("^(?=[[:digit:]]{4})", "January 1, ") %>%
        str_replace("(?<=[[:alpha:]])((,)? )(?=[[:digit:]]{4})", " 1, ") %>%
        mdy() %>%
        na.replace(duration[[i]][2])
    } else if (i %in% c(4:19,21,22,23,24,25,26,27)) {
      if (i %in% c(5:19,21,22,23,24,25,26,27)) {
        endings[[i]] <- str_replace(endings[[i]],"^(?=[[:digit:]]{4})", "1 January ") %>%
          str_replace("^(?=[[:alpha:]])", "1 ") 
      }
      endings[[i]] <- endings[[i]] %>%
        dmy() %>%
        na.replace(duration[[i]][2]) 
    } else if (i %in% c(28:40)) {
      endings[[i]][1:length(endings[[i]])] <- NA 
      endings[[i]] <- na.replace(endings[[i]], duration[[i]][2])
    }
    services[[i]] <- difftime(time1 = endings[[i]], time2 = beginnings[[i]], units = "days")
    services[[i]] <- ifelse(services[[i]] < 0, 0, services[[i]]) %>%
      replace(replace_idx2[[i]], replacement2[[i]]) %>%
      round()
    session_start[[i]] <- duration[[i]][1]
    session_end[[i]] <- duration[[i]][2]
    if (i %in% c(42)) {
      urls[[i]] <- append(urls[[i]], append_urls[[i]])
      names[[i]] <- append(names[[i]], append_names[[i]])
      parties[[i]] <- append(parties[[i]], append_parties[[i]])
      constituencies[[i]] <- append(constituencies[[i]], append_constituencies[[i]])
      constituencies2[[i]] <- append(constituencies2[[i]], append_constituencies2[[i]])
      services[[i]] <- append(services[[i]], append_services[[i]])
   #   session_start[[i]] <- append(session_start[[i]], append_session_start[[i]])
  #    session_end[[i]] <- append(session_end[[i]], append_session_end[[i]])
    }
  }
  urls <- lapply(urls, data.table) %>%
    lapply(rename, url = V1)
  names <- lapply(names, data.table) %>%
    lapply(rename, name = V1)
  parties <- lapply(parties, data.table) %>%
    lapply(rename, party = V1)
  constituencies <- lapply(constituencies, data.table) %>%
    lapply(rename, constituency = V1)
  constituencies2 <- lapply(constituencies2, data.table) %>%
    lapply(rename, constituency2 = V1)
  services <- lapply(services, data.table) %>%
    lapply(rename, service = V1)
  session_start <- lapply(session_start, data.table) %>%
    lapply(rename, session_start = V1)
  session_end <- lapply(session_end, data.table) %>%
    lapply(rename, session_end = V1)
  urls <- mapply(cbind, urls, names, parties, constituencies, 
                 constituencies2, services, session_start,
                 session_end, SIMPLIFY = FALSE)
  urls <-  as.numeric(as.roman(str_extract(source, regex_term))) %>%
    mapply(function(urls, x) mutate(urls, term = x), urls, ., SIMPLIFY=FALSE) %>%
    mapply(function(., x) mutate(., country = x), ., rep("CAN", times = 43), SIMPLIFY=FALSE)
}


#### AUSTRIA WIKIPEDIA INFORMATION EXTRACTION ===========================================
collectorAustria <- function(source) {
  # read and sort filepaths to Wikipedia htmls
  source <- mixedsort(list.files(source, full.names = TRUE), decreasing = TRUE)
  # assign start and end date for each legislative period
  duration <- list(c("1920-11-10", "1923-11-20"), c("1923-11-20", "1927-05-18"),
                   c("1927-05-18", "1930-10-01"), c("1930-12-02", "1934-05-02"),
                   c("1945-12-19", "1949-11-08"), c("1949-11-08", "1953-03-18"),
                   c("1953-03-18", "1956-06-08"), c("1956-06-08", "1959-06-09"),
                   c("1959-06-09", "1962-12-14"), c("1962-12-14", "1966-03-30"),
                   c("1966-03-30", "1970-03-30"), c("1970-03-31", "1971-11-04"),
                   c("1971-11-04", "1975-11-04"), c("1975-11-04", "1979-06-04"),
                   c("1979-06-05", "1983-05-18"), c("1983-05-19", "1986-12-16"),
                   c("1986-12-17", "1990-11-04"), c("1990-11-05", "1994-11-06"),
                   c("1994-11-07", "1996-01-14"), c("1996-01-15", "1999-10-29"),
                   c("1999-10-29", "2002-12-19"), c("2002-12-20", "2006-10-29"),
                   c("2006-10-30", "2008-10-27"), c("2008-10-28", "2013-10-28"),
                   c("2013-10-29", "2017-10-15"), c("2017-10-16", "2019-09-29"),
                   c("2019-09-30", "2024-10-01"))
  # assign session specific XPath query for URLs of legislators 
  query <- rep("//table/tbody/tr/td[1]/a", times = 27)
  # assign session specific XPath query for party affiliation of legislators
  query_party <- c(rep("//table/tbody/tr/td[4]", times = 24),
                   rep("//table/tbody/tr/td[6]", times = 3)) ##########
  # assign session specific XPath query for constituency of legislators
  query_constituency <- c(rep(NA, times = 22),
                          rep("//table/tbody/tr/td[5]", times = 2),
                          rep("//table/tbody/tr/td[7]", times = 3))
  # assign session specific XPath query for constituency of legislators
  query_service <- c(rep("//table/tbody/tr/td[5]", times = 22),
                     rep("//table/tbody/tr/td[6]", times = 2),
                     rep("//table/tbody/tr/td[8]", times = 3)) 
  # assign empty lists to store session speficic htmls
  htmls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator URLs
  urls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator names 
  names <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator party affiliations
  parties <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator constituencies
  constituencies <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator beginning of services
  beginnings <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator end of services
  endings <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator service in days
  services <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic beginning dates
  session_start <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic ending dates
  session_end <- rep(list(NA), times = length(source))
  # assign session specific manual replacement index for services
  replace_idx <- c(rep(list(0), times = 21), list(c(c(65))),
                   rep(list(0), times = 5))
  # assign session specific manual replacement for services
  replacement <- c(rep(list(0), times = 21), list(c(c(429))),
                   rep(list(0), times = 5))
  # loop through all html files
  for (i in 1:length(htmls)) {
    # read html file
    htmls[[i]] <-  read_html(source[i], encoding = "UTF-8")
    # locate legislator biography URLs in html via XPath query, extract and complete URL
    urls[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_attr("href") %>%
      str_c("https://", "de",".wikipedia.org", .)
    # locate legislator names in html via query, extract names
    names[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_text()
    # locate legislator party affiliations in html via query_party, extract and format party affiliation
    parties[[i]] <- html_nodes(x = htmls[[i]], xpath = query_party[i]) %>%
      html_text() %>%
      str_replace_all("\\r|\\n", "")
    # if a query was set for the respective session
    if (!is.na(query_constituency[[i]])) {
      # locate legislator constituencies in html via query_constituency and format constituencies
      constituencies[[i]] <- html_nodes(x = htmls[[i]], xpath = query_constituency[i]) %>%
        html_text() %>%
        str_replace_all("\\r|\\n", "")
    }
    beginnings[[i]] <- html_nodes(x = htmls[[i]], xpath = query_service[i]) %>%
      html_text() %>%
      str_trim() %>%
      str_replace("(Wechsel von der SPÖ zum Team Stronach)|Nachfolger von Reinhold|und von 26. April 2011|und ab 20. September 2011 als |und Vorgänger von Gerald Klug| Bundesministerin für Gesundheit und Frauen|Nachfolger und Vorgänger jeweils| und seit 1. Juli 2016.+|Nachfolgerin von Monika Lindner.+|7 – Tirol; ab |9 – Wien, ab ", "") %>%
      str_replace_all("(Bis|bis).+[[:digit:]]{4}( )?|Wechselte am .+|(A|a)m .+ (übergetreten|übergetreten|ausgetreten|ausgeschlossen|ausgeschieden)|bis 14. Oktober 2012 BZÖ,|.+Ruhendstellung wieder aufgehoben|Bis 27. Dezember 2009 BZÖ, seit 23.|Ab 28. April 2006 BZÖ|Bis 29. Dezember 2009 BZÖ|Bis 1. August 2011 FPÖ,|bis 27. Dezember 2009 BZÖ|.+als Abgeordneter der Landesliste Wien|bis 15. September 2011 BZÖ|.+vom Bundeswahlvorschlag auf|^.+Klubobfrau|.+seit 11. August 2017 Mitglied im FPÖ-Klub|.+bis 23. Dezember 2015 fraktionslos|Nach Mandatsverzicht.+|.+ohne Klubzugehörigkeit|Nationalratspräsident.+|.+Klubobmann der Grünen", "") %>%
      str_extract("([[:digit:]])?[[:digit:]].+?[[:digit:]]{4}|ab 10. April für|ab April 2017") %>%
      str_replace("ab 10. April für", "10. April 2007") %>%
      str_replace("^ab", "01.") %>%
      convMonth(patterns = months_foreign, replacements = months_english) %>%
      dmy() %>%
      na.replace(duration[[i]][1])
    endings[[i]] <- html_nodes(x = htmls[[i]], xpath = query_service[i]) %>%
      html_text() %>%
      str_trim()    %>%
      str_replace("(Wechsel von der SPÖ zum Team Stronach)|Nachfolger von Reinhold|und von 26. April 2011|und ab 20. September 2011 als |und Vorgänger von Gerald Klug| Bundesministerin für Gesundheit und Frauen|Nachfolger und Vorgänger jeweils| und seit 1. Juli 2016.+", "") %>%
      str_replace_all("(Vom|vom|ab|Ab|Von|von|seit).+?[[:digit:]]{4}( )?|^([[:digit:]])?[[:digit:]].+?[[:digit:]]{4}|.+? und|Wechselte am .+|(A|a)m .+ (angelobt|nachgerückt|übergetreten|übergetreten|ausgetreten|ausgeschlossen)|bis 14. Oktober 2012 BZÖ,|.+Ruhendstellung wieder aufgehoben|Bis 27. Dezember 2009 BZÖ, seit 23.|Bis 29. Dezember 2009 BZÖ|Bis 1. August 2011 FPÖ,|bis 27. Dezember 2009 BZÖ|.+als Abgeordneter der Landesliste Wien|bis 15. September 2011 BZÖ|.+vom Bundeswahlvorschlag auf|^.+Klubobfrau|.+seit 11. August 2017 Mitglied im FPÖ-Klub|.+bis 23. Dezember 2015 fraktionslos", "") %>%
      str_extract("([[:digit:]])?[[:digit:]].+?[[:digit:]]{4}|Bis September 2008|bis April 2017") %>%
      str_replace("^(B|b)is", "01.") %>%
      convMonth(patterns = months_foreign, replacements = months_english) %>%
      dmy() %>%
      na.replace(duration[[i]][2])
    if (i == 22) {
      beginnings[[i]] <- append(beginnings[[i]], ymd(duration[[i]][1]), after = 196)
      endings[[i]] <- append(endings[[i]], ymd(duration[[i]][2]), after = 196)
    }
    services[[i]] <- difftime(time1 = endings[[i]], time2 = beginnings[[i]], units = "days")
    if (i == 22) {
      services[[i]] <- replace(services[[i]], replace_idx[[i]], replacement[[i]])
    }
    session_start[[i]] <- duration[[i]][1]
    session_end[[i]] <- duration[[i]][2]
  }
  urls <- lapply(urls, data.table) %>%
    lapply(rename, url = V1)
  names <- lapply(names, data.table) %>%
    lapply(rename, name = V1)
  parties <- lapply(parties, data.table) %>%
    lapply(rename, party = V1)
  constituencies <- lapply(constituencies, data.table) %>%
    lapply(rename, constituency = V1)
  services <- lapply(services, data.table) %>%
    lapply(rename, service = V1)
  session_start <- lapply(session_start, data.table) %>%
    lapply(rename, session_start = V1)
  session_end <- lapply(session_end, data.table) %>%
    lapply(rename, session_end = V1)
  urls <- mapply(cbind, urls, names, parties, constituencies, 
                 services, session_start, session_end, SIMPLIFY = FALSE)
  urls <-  as.numeric(as.roman(str_extract(source, "[[:digit:]]+"))) %>%
    mapply(function(urls, x) mutate(urls, term = x), urls, ., SIMPLIFY=FALSE) %>%
    mapply(function(., x) mutate(., country = x), ., rep("AUT", times = 27), SIMPLIFY=FALSE)
}


#### GERMANY WIKIPEDIA INFORMATION EXTRACTION ===========================================
collectorGermany <- function(source) {
  # read and sort filepaths to Wikipedia htmls
  source <- mixedsort(list.files(source, full.names = TRUE), decreasing = TRUE)
  duration <- list(c("1949-09-07", "1953-10-06"), c("1953-10-06", "1957-10-15"),
                   c("1957-10-15", "1961-10-17"), c("1961-10-17", "1965-10-19"),
                   c("1965-10-19", "1969-10-20"), c("1969-10-20", "1972-12-13"),
                   c("1972-12-13", "1976-12-14"), c("1976-12-14", "1980-10-04"),
                   c("1980-10-04", "1983-03-29"), c("1983-03-29", "1987-02-18"),
                   c("1987-02-18", "1990-12-20"), c("1990-12-20", "1994-11-10"),
                   c("1994-11-10", "1998-10-26"), c("1998-10-26", "2002-10-17"),
                   c("2002-10-17", "2005-10-18"), c("2005-10-18", "2009-10-27"),
                   c("2009-10-27", "2013-10-22"), c("2013-10-22", "2017-10-24"),
                   c("2017-10-24", "2021-10-24"))
  query <- c(rep("//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/a[1]",
                 times = 11),
             "//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/i/a[1]|
             //h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/a[1]",
             rep("//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/a[1]",
                 times = 5),
             "//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/a[1]|
             //h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/i/a[1]",
             "//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[2]/a[1]")
  query_party <- c(rep("//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[3]",
                       times = 18),
                   "//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[4]")
  query_constituency <- c(rep("//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[4]",
                              times = 18),
                          "//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[5]")
  query_constituency2 <- c(rep("//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[5]",
                               times = 18),
                           "//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[6]")
  query_service <- c(rep("//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[7]",
                         times = 18),
                     "//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[8]")
  # assign empty lists to store session speficic htmls
  htmls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator URLs
  urls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator names 
  names <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator party affiliations
  parties <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator constituencies
  constituencies <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator constituencies2
  constituencies2 <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator beginning of services
  beginnings <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator end of services
  endings <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator service in days
  services <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic beginning dates
  session_start <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic ending dates
  session_end <- rep(list(NA), times = length(source))
  # assign session specific manual replacement index for services
  replace_idx <- c(rep(list(0), times = 18), list(c(710)))
  # assign session specific manual replacement for services
  replacement <- c(rep(list(0), times = 18), list(c(373)))
  # assign removal index
  removal_idx <- c(rep(list(0), times = 18), list(c(711)))
  # loop through all html files
  for (i in 1:length(htmls)) {
    # read html file
    htmls[[i]] <-  read_html(source[i], encoding = "UTF-8")
    # locate legislator biography URLs in html via XPath query, extract and complete URL
    urls[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_attr("href") %>%
      str_c("https://", "de",".wikipedia.org", .)
    # locate legislator names in html via query, extract names
    names[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_text()
    # locate legislator party affiliations in html via query_party, extract and format party affiliation
    parties[[i]] <- html_nodes(x = htmls[[i]], xpath = query_party[i]) %>%
      html_text() %>%
      str_replace_all("\\r|\\n", "")
    constituencies[[i]] <- html_nodes(x = htmls[[i]], xpath = query_constituency[i]) %>%
      html_text() %>%
      str_replace_all("\\r|\\n", "")
    constituencies2[[i]] <- html_nodes(x = htmls[[i]], xpath = query_constituency2[i]) %>%
      html_text() %>%
      str_replace_all("\\r|\\n", "") %>%
      inset(.=="", NA)
    fill <- html_nodes(x = htmls[[i]], xpath = "//h2/span[contains(text(), 'Abgeordnete')]/ancestor::h2/following-sibling::table[1]/tbody/tr") %>%
      lapply(xml_children) %>%
      lapply(length) %>%
      unlist %>%
      extract(-1) %>%
      as.numeric %>%
      is_less_than(7) %>%
      which()
    beginnings[[i]] <- html_nodes(x = htmls[[i]], xpath = query_service[i]) %>%
      html_text() %>%
      str_extract_all("(eingetreten am|nachgewählt am|nachgerückt am).+?[[:digit:]]{4}?") %>%
      lapply(., tail, n=1) %>%
      lapply(., function(x) if(identical(x, character(0))) NA_character_ else x) %>%
      unlist %>%
      str_extract("([[:digit:]])?[[:digit:]].+?[[:digit:]]{4}") %>%
      convMonth(patterns = months_foreign, replacements = months_english) %>%
      dmy() %>%
      na.replace(duration[[i]][1])
    beginnings[[i]] <- numeric(length(beginnings[[i]]) + length(fill)) %>%
      replace(fill, NA) %>%
      replace(!is.na(.), beginnings[[i]]) %>%
      as.Date() %>%
      na.replace(duration[[i]][1])
    endings[[i]] <- html_nodes(x = htmls[[i]], xpath = query_service[i]) %>%
      html_text() %>%
      str_extract_all("((A|a)usgeschieden am|verstorben am).+?[[:digit:]]{4}?") %>%
      lapply(., function(x) if(identical(x, character(0))) NA_character_ else x) %>%
      unlist %>%
      str_extract("([[:digit:]])?[[:digit:]].+?[[:digit:]]{4}") %>%
      convMonth(patterns = months_foreign, replacements = months_english) %>%
      dmy() %>%
      na.replace(duration[[i]][2])
    endings[[i]] <- numeric(length(endings[[i]]) + length(fill)) %>%
      replace(fill, NA) %>%
      replace(!is.na(.), endings[[i]]) %>%
      as.Date() %>%
      na.replace(duration[[i]][2])
    services[[i]] <- difftime(time1 = endings[[i]], time2 = beginnings[[i]], units = "days")
    if (i == 19) {
      services[[i]] <- replace(services[[i]], replace_idx[[i]], replacement[[i]])
    }
    if (i %in% 19) {
      parties[[i]] <- parties[[i]][-removal_idx[[i]]]
      constituencies[[i]] <- constituencies[[i]][-removal_idx[[i]]]
      constituencies2[[i]] <- constituencies2[[i]][-removal_idx[[i]]]
      services[[i]] <- services[[i]][-removal_idx[[i]]]
    }
    if (i %in% 9) {
      constituencies2[[i]] <- append(constituencies2[[i]], NA, after = 522)
    }
    session_start[[i]] <- duration[[i]][1]
    session_end[[i]] <- duration[[i]][2]
  }
  urls <- lapply(urls, data.table) %>%
    lapply(rename, url = V1)
  names <- lapply(names, data.table) %>%
    lapply(rename, name = V1)
  parties <- lapply(parties, data.table) %>%
    lapply(rename, party = V1)
  constituencies <- lapply(constituencies, data.table) %>%
    lapply(rename, constituency = V1)
  constituencies2 <- lapply(constituencies2, data.table) %>%
    lapply(rename, constituency2 = V1)
  services <- lapply(services, data.table) %>%
    lapply(rename, service = V1)
  session_start <- lapply(session_start, data.table) %>%
    lapply(rename, session_start = V1)
  session_end <- lapply(session_end, data.table) %>%
    lapply(rename, session_end = V1)
  urls <- mapply(cbind, urls, names, parties, constituencies, 
                 constituencies2, services, session_start, 
                 session_end, SIMPLIFY = FALSE)
  urls <-  as.numeric(as.roman(str_extract(source, "[[:digit:]]+"))) %>%
    mapply(function(urls, x) mutate(urls, term = x), urls, ., SIMPLIFY=FALSE) %>%
    mapply(function(., x) mutate(., country = x), ., rep("DEU", times = 19), SIMPLIFY=FALSE)
}


#### IRELAND WIKIPEDIA INFORMATION EXTRACTION ===========================================
collectorIreland <- function(source) {
  # read and sort filepaths to Wikipedia htmls
  source <- mixedsort(list.files(source, full.names = TRUE), decreasing = TRUE)
  duration <- list(c("1918-12-14", "1921-05-24"), c("1921-05-24", "1922-06-16"),
                   c("1922-06-16", "1923-08-27"), c("1923-08-27", "1927-06-09"),
                   c("1927-06-09", "1927-09-15"), c("1927-09-15", "1932-02-16"),
                   c("1932-02-16", "1933-01-24"), c("1933-01-24", "1937-07-01"),
                   c("1937-07-01", "1938-06-17"), c("1938-06-17", "1943-06-23"),
                   c("1943-06-23", "1944-05-30"), c("1944-05-30", "1948-02-04"),
                   c("1948-02-04", "1951-05-30"), c("1951-05-30", "1954-05-18"),
                   c("1954-05-18", "1957-03-05"), c("1957-03-05", "1961-10-04"),
                   c("1961-10-04", "1965-04-07"), c("1965-04-07", "1969-06-18"),
                   c("1969-06-18", "1973-02-28"), c("1973-02-28", "1977-06-16"),
                   c("1977-06-16", "1981-06-11"), c("1981-06-11", "1982-02-18"),
                   c("1982-02-18", "1982-11-24"), c("1982-11-24", "1987-02-17"),
                   c("1987-02-17", "1989-06-15"), c("1989-06-15", "1992-11-25"),
                   c("1992-11-25", "1997-06-06"), c("1997-06-06", "2002-05-17"),
                   c("2002-05-17", "2007-05-24"), c("2007-05-24", "2011-02-25"),
                   c("2011-02-25", "2016-02-26"), c("2016-02-26", "2020-02-08"),
                   c("2020-02-08", "2025-02-20"))
  query <- c("//h2/span[contains(text(), 'Members by constituency')]/ancestor::h2/
             following-sibling::table[1]/tbody/tr/td[2]/a",
             "//h2/span[contains(text(), 'Members by constituency')]/ancestor::h2/
             following-sibling::table[1]/tbody/tr/td[position() < last()]/
             a[not(contains(@title, 'constituency'))]",
             rep("//h2/span[contains(text(), 'TDs by constituency')]/ancestor::h2/
                 following-sibling::table[1]/tbody/tr/td[position() < last()]/
                 a[not(contains(@title, 'constituency'))]", times = 26),
             rep("//h2/span[contains(text(), 'TDs by party')]/ancestor::h2/
                 following-sibling::table[1]/tbody/tr/td[not(@rowspan) and position() <
                 last()]/a[not(contains(@title, 'Unemployed Action'))]",
                 times = 4),
             rep("//h2/span[contains(text(), 'List of TDs')]/ancestor::h2/
                 following-sibling::table[1]/tbody/tr/td[not(@rowspan) and position() <
                 last()]/a[not(contains(@title, 'Unemployed Action'))]",
                 times = 1))
  query_party <- c(rep("//h2/span[contains(text(), 'Members by constituency')]/
                       ancestor::h2/following-sibling::table[1]/tbody/tr/
                       td[last()]/a", times = 2),
                   rep("//h2/span[contains(text(), 'TDs by constituency')]/
                       ancestor::h2/following-sibling::table[1]/tbody/tr/
                       td[last()]/a", times = 26),
                   rep("//h2/span[contains(text(), 'TDs by party')]/
                       ancestor::h2/following-sibling::table[1]/tbody/tr/
                       td[contains(text(), ')')]/a", times = 4),
                   rep("//h2/span[contains(text(), 'List of TDs')]/
                       ancestor::h2/following-sibling::table[1]/tbody/tr/
                       td[contains(text(), ')')]/a", times = 1)
                   )
  query_constituency <- c(rep("//h2/span[contains(text(), 'Members by constituency')]/
                              ancestor::h2/following-sibling::table[1]/tbody/tr/td/
                              a[contains(@title, 'constituency')]", times = 2),
                          rep("//h2/span[contains(text(), 'TDs by constituency')]/
                              ancestor::h2/following-sibling::table[1]/tbody/tr/td/
                              a[contains(@title, 'constituency')]", times = 26),
                          rep("//h2/span[contains(text(), 'TDs by party')]/
                              ancestor::h2/following-sibling::table[1]/tbody/tr/td/
                              a[contains(@title, 'constituency')]", times = 4),
                          rep("//h2/span[contains(text(), 'List of TDs')]/
                              ancestor::h2/following-sibling::table[1]/tbody/tr/td/
                              a[contains(@title, 'constituency')]", times = 1))
  query_multiplier <- c(rep("//h2/span[contains(text(), 'Members by constituency')]/
                            ancestor::h2/following-sibling::table[1]/tbody/tr/td/
                            a[contains(@title, 'constituency')]/parent::td", times = 2),
                        rep("//h2/span[contains(text(), 'TDs by constituency')]/
                            ancestor::h2/following-sibling::table[1]/tbody/tr/td/
                            a[contains(@title, 'constituency')]/parent::td", times = 26),
                        rep("//h2/span[contains(text(), 'TDs by party')]/
                            ancestor::h2/following-sibling::table[1]/tbody/tr/td/
                            a[contains(@title, 'constituency')]/parent::td", times = 4),
                        rep("//h2/span[contains(text(), 'List of TDs')]/
                            ancestor::h2/following-sibling::table[1]/tbody/tr/td/
                            a[contains(@title, 'constituency')]/parent::td", times = 1))
  query_multiplier2 <- c(rep(NA, times = 28),
                         rep("//h2/span[contains(text(), 'TDs by party')]/
                             ancestor::h2/following-sibling::table[1]/tbody/tr/
                             td[contains(text(), ')')]", times = 4),
                         rep("//h2/span[contains(text(), 'List of TDs')]/
                             ancestor::h2/following-sibling::table[1]/tbody/tr/
                             td[contains(text(), ')')]", times = 1))
  replace_idx <- c(list(c(25,29,92)), list(c(0)), list(c(0)), 
                   list(c(59,129,57,42,102,24,113,17,39,3,8,50,53,61,86,91,115,132,43,97)),
                   list(c(62,48)), list(c(1,53,49,89,108,42,83)), list(0),
                   list(c(65,46,75,71,147)), list(0), list(c(53,69)),list(0), list(c(76,12,
                                                                                     51,74,103,132,19,44,125,129)), list(c(31,29,35)), list(c(92,109,139,50,20,145,
                                                                                                                                              73,18,105)), list(c(97,77,45,87,18,5,62)), list(c(42,71,56,10,61,115,3,124)),
                   list(c(45,16,86,120,71,18)), list(c(81,137,14,97,10,143,96)),
                   list(c(69,90,104,34,46,23)), list(c(119,25,78,80,114,36,70)), list(c(16,25,32)),
                   list(0),list(c(79,89)),list(c(38,42,109)),list(0),list(0), list(c(68,131,21,28,162,35,82)),
                   list(c(114,45,28,69,149,147)),list(c(50,84)),list(c(14,162,34,25,49,7)),
                   list(c(122,49,50,153,38,41,76)),list(c(158, 132, 22, 69, 133, 37)), list(c(0)))
  replacement <- c(list(c(681,601,82)), list(c(0)), list(c(0)), 
                   list(c(61,68,198,205,275,449,449,450,451,562,562,562,562,562,562,562,562,562,906,906)),
                   list(c(76,76)), list(c(49,201,546,631,1002,1181,1383)),list(0),
                   list(c(262,874,876,1297,1301)),list(0),list(c(354,713)),list(0),list(c(164,553,553,
                                                                                          553,553,553,745,1247,1247,1247)), list(c(307,497,651)), list(c(393,393,393,532,750,750,
                                                                                                                                                         814,1008,1008)), list(c(574,652,713,713,807,911,911)), list(c(254,451,477,869,869,869,
                                                                                                                                                                                                                       1206,1092)), list(c(603,868,868,1008,1156,1253)), list(c(609,609,946,946,1072,1072,1141)),
                   list(c(259,300,300,532,532,410)), list(c(272,613,734,734,987,1198,1198)),
                   list(c(874,874,1239)),list(0), list(c(96,152)),list(c(170,364,568)),list(0),list(0),
                   list(c(561,561,715,715,946,1224,1224)), list(c(113,136,281,626,1019,1202)),list(c(898,898)),
                   list(c(412,589,746,991,1034,1258)),list(c(105,665,1124,1186,1186,1343,1784)),list(c(13,1221,1221,1221,1221,1376)),list(0))
  # assign session specific manual addition
  append_urls <- c(list(0), list(0), list(0), 
                   list(c("https://en.wikipedia.org/wiki/Hugh_Kennedy",
                          "https://en.wikipedia.org/wiki/Patrick_McGilligan",
                          "https://en.wikipedia.org/wiki/James_O%27Mara",
                          "https://en.wikipedia.org/wiki/Batt_O%27Connor",
                          "https://en.wikipedia.org/wiki/Richard_O%27Connell_(politician)",
                          "https://en.wikipedia.org/wiki/Michael_K._Noonan",
                          "https://en.wikipedia.org/wiki/Se%C3%A1n_Lemass",
                          "https://en.wikipedia.org/wiki/John_Madden_(Irish_politician)",
                          "https://en.wikipedia.org/wiki/Michael_Egan_(Irish_politician)",
                          "https://en.wikipedia.org/wiki/Denis_McCullough",
                          "https://en.wikipedia.org/wiki/Thomas_Bolger",
                          "https://en.wikipedia.org/wiki/John_Joe_O%27Reilly",
                          "https://en.wikipedia.org/wiki/Patrick_Leonard_(politician)",
                          "https://en.wikipedia.org/wiki/Oscar_Traynor",
                          "https://en.wikipedia.org/wiki/Thomas_Hennessy",
                          "https://en.wikipedia.org/wiki/Samuel_Holt",
                          "https://en.wikipedia.org/wiki/Martin_Roddy",
                          "https://en.wikipedia.org/wiki/Michael_Tierney_(politician)",
                          "https://en.wikipedia.org/wiki/Martin_Conlon",
                          "https://en.wikipedia.org/wiki/William_Norton",
                          "https://en.wikipedia.org/wiki/James_Dwyer_(politician)")),
                   list(c("https://en.wikipedia.org/wiki/Thomas_Hennessy",
                          "https://en.wikipedia.org/wiki/Gear%C3%B3id_O%27Sullivan")),
                   list(c("https://en.wikipedia.org/wiki/Denis_Gorey",
                          "https://en.wikipedia.org/wiki/Vincent_Rice",
                          "https://en.wikipedia.org/wiki/Thomas_F._O%27Higgins",
                          "https://en.wikipedia.org/wiki/Se%C3%A1n_Mac_Eoin",
                          "https://en.wikipedia.org/wiki/James_Geoghegan",
                          "https://en.wikipedia.org/wiki/Thomas_Finlay_(Cumann_na_nGaedheal_politician)",
                          "https://en.wikipedia.org/wiki/Thomas_Harris_(Irish_politician)")),
                   list(0), list(c("https://en.wikipedia.org/wiki/Robert_Rowlette",
                                   "https://en.wikipedia.org/wiki/Cecil_Lavery",
                                   "https://en.wikipedia.org/wiki/Eamon_Corbett",
                                   "https://en.wikipedia.org/wiki/Martin_Neilan",
                                   "https://en.wikipedia.org/wiki/Denis_Allen_(politician)"
                   )),list(0), list(c("https://en.wikipedia.org/wiki/John_McCann_(Irish_politician)",
                                      "https://en.wikipedia.org/wiki/John_J._Keane_(politician)")),
                   list(0), list(c("https://en.wikipedia.org/wiki/Donal_O%27Donoghue",
                                   "https://en.wikipedia.org/wiki/Patrick_Shanahan_(politician)",
                                   "https://en.wikipedia.org/wiki/Vivion_de_Valera",
                                   "https://en.wikipedia.org/wiki/Honor_Crowley",
                                   "https://en.wikipedia.org/wiki/Bernard_Commons",
                                   "https://en.wikipedia.org/wiki/Brendan_Corish",
                                   "https://en.wikipedia.org/wiki/Patrick_McGrath_(Irish_politician)",
                                   "https://en.wikipedia.org/wiki/Se%C3%A1n_MacBride",
                                   "https://en.wikipedia.org/wiki/Patrick_Kinane",
                                   "https://en.wikipedia.org/wiki/John_Ormonde")),
                   list(c("https://en.wikipedia.org/wiki/Neil_Blaney",
                          "https://en.wikipedia.org/wiki/William_J._Murphy_(Labour_politician)",
                          "https://en.wikipedia.org/wiki/Patrick_O%27Donnell_(Irish_politician)")),
                   list(c("https://en.wikipedia.org/wiki/John_Carew_(Irish_politician)",
                          "https://en.wikipedia.org/wiki/Phelim_Calleary",
                          "https://en.wikipedia.org/wiki/William_Kenneally",
                          "https://en.wikipedia.org/wiki/Thomas_Byrne_(Dublin_politician)",
                          "https://en.wikipedia.org/wiki/Richard_Barry_(Irish_politician)",
                          "https://en.wikipedia.org/wiki/Mark_Deering",
                          "https://en.wikipedia.org/wiki/Robert_Lahiffe",
                          "https://en.wikipedia.org/wiki/Stephen_Barrett_(Irish_politician)",
                          "https://en.wikipedia.org/wiki/George_Coburn")),
                   list(c("https://en.wikipedia.org/wiki/Michael_Colbert",
                          "https://en.wikipedia.org/wiki/Kathleen_O%27Connor",
                          "https://en.wikipedia.org/wiki/Patrick_Byrne_(Irish_politician)",
                          "https://en.wikipedia.org/wiki/Kieran_Egan_(politician)",
                          "https://en.wikipedia.org/wiki/John_Galvin_(Irish_politician)",
                          "https://en.wikipedia.org/wiki/Martin_Medlar",
                          "https://en.wikipedia.org/wiki/Noel_Lemass")),
                   list(c("https://en.wikipedia.org/wiki/Frank_Sherwin",
                          "https://en.wikipedia.org/wiki/Anthony_Millar",
                          "https://en.wikipedia.org/wiki/Patrick_Cummins_(politician)",
                          "https://en.wikipedia.org/wiki/Se%C3%A1n_%C3%93_Ceallaigh_(Clare_politician)",
                          "https://en.wikipedia.org/wiki/Richie_Ryan_(politician)",
                          "https://en.wikipedia.org/wiki/Henry_Johnston_(Irish_politician)",
                          "https://en.wikipedia.org/wiki/Patrick_Teehan",
                          "https://en.wikipedia.org/wiki/Joseph_McLoughlin")),
                   list(c("https://en.wikipedia.org/wiki/Paddy_Belton",
                          "https://en.wikipedia.org/wiki/Sheila_Galvin",
                          "https://en.wikipedia.org/wiki/Terence_Boylan_(politician)",
                          "https://en.wikipedia.org/wiki/Joan_Burke",
                          "https://en.wikipedia.org/wiki/John_Donnellan",
                          "https://en.wikipedia.org/wiki/Eileen_Desmond")),
                   list(c("https://en.wikipedia.org/wiki/John_O%27Leary_(Kerry_politician)",
                          "https://en.wikipedia.org/wiki/Fad_Browne",
                          "https://en.wikipedia.org/wiki/Se%C3%A1n_French_(1931%E2%80%932011)",
                          "https://en.wikipedia.org/wiki/Gerry_Collins_(politician)",
                          "https://en.wikipedia.org/wiki/Sylvester_Barrett",
                          "https://en.wikipedia.org/wiki/Godfrey_Timmins",
                          "https://en.wikipedia.org/wiki/Desmond_O%27Malley")),
                   list(c("https://en.wikipedia.org/wiki/Se%C3%A1n_Sherwin",
                          "https://en.wikipedia.org/wiki/Patrick_Malone_(Irish_politician)",
                          "https://en.wikipedia.org/wiki/Patrick_Cooney",
                          "https://en.wikipedia.org/wiki/Patrick_Delap",
                          "https://en.wikipedia.org/wiki/Larry_McMahon",
                          "https://en.wikipedia.org/wiki/Gene_Fitzgerald")),
                   list(c("https://en.wikipedia.org/wiki/Brendan_Toal",
                          "https://en.wikipedia.org/wiki/Se%C3%A1n_Brosnan",
                          "https://en.wikipedia.org/wiki/Michael_P._Kitt",
                          "https://en.wikipedia.org/wiki/M%C3%A1ire_Geoghegan-Quinn",
                          "https://en.wikipedia.org/wiki/Enda_Kenny",
                          "https://en.wikipedia.org/wiki/Paddy_Keaveney",
                          "https://en.wikipedia.org/wiki/Brendan_Halligan")),
                   list(c("https://en.wikipedia.org/wiki/Liam_Burke",
                          "https://en.wikipedia.org/wiki/Myra_Barry",
                          "https://en.wikipedia.org/wiki/Clement_Coughlan")),list(0),
                   list(c("https://en.wikipedia.org/wiki/Liam_Skelly",
                          "https://en.wikipedia.org/wiki/Noel_Treacy")),
                   list(c("https://en.wikipedia.org/wiki/Cathal_Coughlan_(politician)",
                          "https://en.wikipedia.org/wiki/Tom_Leonard_(Irish_politician)",
                          "https://en.wikipedia.org/wiki/Brian_Cowen")),list(0),list(0),
                   list(c("https://en.wikipedia.org/wiki/Eric_Byrne",
                          "https://en.wikipedia.org/wiki/Michael_Ring",
                          "https://en.wikipedia.org/wiki/Kathleen_Lynch_(politician)",
                          "https://en.wikipedia.org/wiki/Hugh_Coveney",
                          "https://en.wikipedia.org/wiki/Mildred_Fox",
                          "https://en.wikipedia.org/wiki/Cecilia_Keaveney",
                          "https://en.wikipedia.org/wiki/Brian_Lenihan_Jnr")),
                   list(c("https://en.wikipedia.org/wiki/Jan_O%27Sullivan",
                          "https://en.wikipedia.org/wiki/Se%C3%A1n_Ryan_(politician)",
                          "https://en.wikipedia.org/wiki/Simon_Coveney",
                          "https://en.wikipedia.org/wiki/Mary_Upton",
                          "https://en.wikipedia.org/wiki/S%C3%A9amus_Healy",
                          "https://en.wikipedia.org/wiki/Tom_Hayes_(Irish_politician)")),
                   list(c("https://en.wikipedia.org/wiki/Shane_McEntee",
                          "https://en.wikipedia.org/wiki/Catherine_Murphy_(politician)")),
                   list(c("https://en.wikipedia.org/wiki/George_Lee_(journalist)",
                          "https://en.wikipedia.org/wiki/Maureen_O%27Sullivan_(politician)",
                          "https://en.wikipedia.org/wiki/Pearse_Doherty")),
                   list(c("https://en.wikipedia.org/wiki/Patrick_Nulty",
                          "https://en.wikipedia.org/wiki/Helen_McEntee",
                          "https://en.wikipedia.org/wiki/Ruth_Coppinger",
                          "https://en.wikipedia.org/wiki/Gabrielle_McFadden",
                          "https://en.wikipedia.org/wiki/Paul_Murphy_(Irish_politician)",
                          "https://en.wikipedia.org/wiki/Michael_Fitzmaurice_(politician)",
                          "https://en.wikipedia.org/wiki/Bobby_Aylward")),
                   list(c("https://en.wikipedia.org/wiki/Mark_Ward_(politician)",
                          "https://en.wikipedia.org/wiki/P%C3%A1draig_O%27Sullivan",
                          "https://en.wikipedia.org/wiki/Malcolm_Byrne",
                          "https://en.wikipedia.org/wiki/Joe_O%27Brien_(politician)")))
  # assign session specific manual addition for names
  append_names <- c(list(0), list(0), list(0), 
                    list(c("Hugh Kennedy", "Patrick McGilligan",
                           "James O'Mara", "Batt O'Connor",
                           "Richard O'Connell", "Michael Noonan",
                           "Seán Lemass", "John Madden", "Michael Egan",
                           "Denis McCullough", "Thomas Bolger", "John Joe O'Reilly",
                           "Patrick Leonard", "Oscar Traynor", "Thomas Hennessy",
                           "Samuel Holt", "Martin Roddy", "Michael Tierney",
                           "Martin Conlon", "William Norton", "James Dwyer")),
                    list(c("Thomas Hennessy", "Gearóid O'Sullivan")),
                    list(c("Denis Gorey", "Vincent Rice", "Thomas F. O'Higgins",
                           "Seán Mac Eoin", "James Geoghegan", "Thomas Finlay",
                           "Thomas Harris")), list(0),list(c("Robert Rowlette",
                                                             "Cecil Lavery", "Eamon Corbett", "Martin Neilan",
                                                             "Denis Allen")),list(0), list(c("John McCann", "John J. Keane")),
                    list(0), list(c("Donal O'Donoghue", "Patrick Shanahan", "Vivion de Valera",
                                    "Honor Crowley", "Bernard Commons", "Brendan Corish", "Patrick McGrath",
                                    "Seán MacBride","Patrick Kinane", "John Ormonde")),
                    list(c("Neil Blaney", "William J. Murphy", "Patrick O'Donnell")),
                    list(c("John Carew", "Phelim Calleary", "William Kenneally", "Thomas Byrne",
                           "Richard Barry", "Mark Deering", "Robert Lahiffe", "Stephen Barrett",
                           "George Coburn")), list(c("Michael Colbert", "Kathleen O'Connor",
                                                     "Patrick Byrne", "Kieran Egan", "John Galvin", "Martin Medlar",
                                                     "Noel Lemass")),list(c("Frank Sherwin", "Anthony Millar", "Patrick Cummins", 
                                                                            "Seán Ó Ceallaigh", "Richie Ryan", "Henry Johnston", "Patrick Teehan", "Joseph McLoughlin")),
                    list(c("Paddy Belton","Sheila Galvin","Terence Boylan","Joan Burke","John Donnellan","Eileen Desmond")),
                    list(c("John O'Leary","Fad Browne","Seán French","Gerry Collins","Sylvester Barrett","Godfrey Timmins",
                           "Desmond O'Malley")), list(c("Seán Sherwin","Patrick Malone","Patrick Cooney","Patrick Delap",
                                                        "Larry McMahon","Gene Fitzgerald")), list(c("Brendan Toal","Seán Brosnan","Michael P. Kitt",
                                                                                                    "Máire Geoghegan-Quinn","Enda Kenny","Paddy Keaveney","Brendan Halligan")),
                    list(c("Liam Burke","Myra Barry","Clement Coughlan")),list(0), list(c("Liam Skelly","Noel Treacy")),
                    list(c("Cathal Coughlan","Tom Leonard","Brian Cowen")),list(0),list(0),
                    list(c("Eric Byrne","Michael Ring","Kathleen Lynch","Hugh Coveney","Mildred Fox","Cecilia Keaveney",
                           "Brian Lenihan Jnr")), list(c("Jan O'Sullivan","Seán Ryan","Simon Coveney","Mary Upton",
                                                         "Séamus Healy","Tom Hayes")),list(c("Shane McEntee","Catherine Murphy")), list(c("George Lee",
                                                                                                                                          "Maureen O'Sullivan","Pearse Doherty")), list(c("Patrick Nulty","Helen McEntee","Ruth Coppinger",
                                                                                                                                                                                          "Gabrielle McFadden","Paul Murphy","Michael Fitzmaurice","Bobby Aylward")),
                    list(c("Mark Ward", "Pádraig O'Sullivan", "Malcolm Byrne", "Joe O'Brien")))
  # assign session specific manual addition for parties
  append_parties <- c(list(0), list(0), list(0), 
                      list(c(rep("Cumann na nGaedheal", times = 6), 
                             "Republican","Republican", rep("Cumann na nGaedheal", times = 5),
                             "Republican", "Cumann na nGaedheal","Republican",
                             "Cumann na nGaedheal","Cumann na nGaedheal","Cumann na nGaedheal",
                             "Labour Party", "Cumann na nGaedheal")),
                      list(c("Cumann na nGaedheal", "Cumann na nGaedheal")),
                      list(c(rep("Cumann na nGaedheal", times = 4), "Fianna Fáil",
                             "Cumann na nGaedheal", "Fianna Fáil")),list(0),list(c("Independent",
                                                                                   "Fine Gael", "Fianna Fáil", "Fianna Fáil", "Fianna Fáil")),list(0),
                      list(c("Fianna Fáil", "Fianna Fáil")),
                      list(0), list(c(rep("Fianna Fáil", times = 4), "Clann na Talmhan",
                                      "Labour Party", "Fianna Fáil", "Clann na Poblachta",
                                      "Clann na Poblachta", "Fianna Fáil")), list(c("Fianna Fáil", "Labour Party",
                                                                                    "Fine Gael")), list(c("Fine Gael","Fianna Fáil","Fianna Fáil",
                                                                                                          "Independent", "Fine Gael", "Fine Gael", "Fianna Fáil", "Fine Gael",
                                                                                                          "Fine Gael")), list(c("Fianna Fáil","Clann na Poblachta","Independent",
                                                                                                                                rep("Fianna Fáil", times = 4))), list(c("Independent","Fianna Fáil","Fianna Fáil",
                                                                                                                                                                        "Fianna Fáil","Fine Gael","Fianna Fáil","Fianna Fáil","Fine Gael")),
                      list(c("Fine Gael","Fianna Fáil","Fianna Fáil","Fine Gael","Fine Gael","Labour Party")),
                      list(c(rep("Fianna Fáil", times = 5),"Fine Gael","Fianna Fáil")), list(c("Fianna Fáil","Fine Gael",
                                                                                               "Fine Gael","Fianna Fáil","Fine Gael","Fianna Fáil")),list(c("Fine Gael",
                                                                                                                                                            rep("Fianna Fáil",times=3),"Fine Gael","Independent Fianna Fáil","Labour Party")),
                      list(c("Fine Gael","Fine Gael","Fianna Fáil")),list(0), list(c("Fine Gael","Fianna Fáil")),
                      list(c(rep("Fianna Fáil", times = 3))),list(0),list(0), list(c("Democratic Left","Fine Gael",
                                                                                     "Democratic Left","Fine Gael","Independent","Fianna Fáil","Fianna Fáil")),
                      list(c("Labour Party","Labour Party","Fine Gael","Labour Party","Independent","Fine Gael")),
                      list(c("Fine Gael","Independent")),list(c("Fine Gael","Independent","Sinn Féin")),
                      list(c("Labour Party","Fine Gael","Socialist Party","Fine Gael","Anti-Austerity Alliance",
                             "Independent","Fianna Fáil")),
                      list(c("Sinn Féin", "Fianna Fáil", "Fianna Fáil", "Green Party")))
  # assign session specific manual addition for constituencies
  append_constituencies <- c(list(0), list(0), list(0), 
                             list(c("Dublin South", "National University of Ireland", "Dublin South",
                                    "Dublin County", "Limerick", "Cork East", "Dublin South",
                                    "Mayo North", "Cork Borough", "Donegal", "Carlow–Kilkenny", "Cavan",
                                    "Dublin North", "Dublin North", "Dublin South", "Leitrim–Sligo",
                                    "Leitrim–Sligo", "Mayo North", "Roscommon", "Dublin County",
                                    "Leix–Offaly")),
                             list(c("Dublin South", "Dublin County")),
                             list(c("Carlow–Kilkenny", "Dublin North", "Dublin North",
                                    "Leitrim–Sligo", "Longford–Westmeath", "Dublin County",
                                    "Kildare")),list(0),list(c("Dublin University",
                                                               "Dublin County", "Galway", "Galway", "Wexford")),list(0),
                             list(c("Dublin South", "Galway West")),
                             list(0), list(c("Kerry South", "Clare", "Dublin North-West",
                                             "Kerry South", "Mayo South", "Wexford", "Cork Borough", "Dublin County",
                                             "Tipperary", "Waterford")), list(c("Donegal East", "Cork West", "Donegal West")),
                             list(c("Limerick East", "Mayo North", "Waterford", "Dublin North-West",
                                    "Cork East", "Wicklow", "Galway South", "Cork Borough", "Louth")),
                             list(c("Limerick West", "Kerry North", "Dublin North-East", "Leix–Offaly",
                                    "Cork Borough", "Carlow–Kilkenny","Dublin South-West")),
                             list(c("Dublin North-Central","Galway South","Dublin South-Central","Clare",
                                    "Dublin South-West","Meath","Carlow–Kilkenny","Sligo–Leitrim")),
                             list(c("Dublin North-East","Cork Borough","Kildare","Roscommon","Galway East","Cork Mid")),
                             list(c("Kerry South","Waterford","Cork Borough","Limerick West","Clare","Wicklow",
                                    "Limerick East")), list(c("Dublin South-West","Kildare","Longford–Westmeath",
                                                              "Donegal–Leitrim","Dublin County South","Cork Mid")),list(c("Monaghan",
                                                                                                                          "Cork North-East","Galway North-East","Galway West","Mayo West","Donegal North-East",
                                                                                                                          "Dublin South-West")), list(c("Cork City","Cork North-East","Donegal")),list(0),
                             list(c("Dublin West","Galway East")),list(c("Donegal South-West","Dublin Central",
                                                                         "Laois–Offaly")),list(0),list(0), list(c("Dublin South-Central","Mayo West",
                                                                                                                  "Cork North-Central","Cork South-Central","Wicklow","Donegal North-East",
                                                                                                                  "Dublin West")),list(c("Limerick East","Dublin North","Cork South-Central",
                                                                                                                                         "Dublin South-Central","Tipperary South","Tipperary South")),list(c("Meath",
                                                                                                                                                                                                             "Kildare North")),list(c("Dublin South","Dublin Central","Donegal South-West")),
                             list(c("Dublin West","Meath East","Dublin West","Longford–Westmeath","Dublin South-West",
                                    "Roscommon–South Leitrim","Carlow–Kilkenny")),
                             list(c("Dublin Mid-West", "Cork North-Central", "Wexford", "Dublin Fingal")))
  # assign session specific manual addition for services
  append_services <- c(list(0), list(0), list(0), 
                       list(c(388,1313,1183,1176,1106,932,932,932,931,930,819,819,819,819,819,819,
                              819,819,819,475,475)),
                       list(c(22,22)),list(c(1565,1413,1068,983,612,433,231)),list(0),
                       list(c(1356,744,742,321,317)),list(0), list(c(1477,1118)),
                       list(0), list(c(1180,791,791,791,791,791,599,97,97,97)),
                       list(c(903,713,559)), list(c(690,690,690,551,333,333,269,75,75)),
                       list(c(447,369,308,308,214,110,110)), list(c(1419,1222,1196,804,804,
                                                                    804,467,581)),list(c(677,412,412,272,124,27)),list(c(923,923,
                                                                                                                         586,586,460,460,391)), list(c(1091,1050,1050,818,818,940)),
                       list(c(1296,945,834,834,581,370,370)), list(c(581,581,216)),list(0),
                       list(c(182,126)), list(c(1375,1181,977)),list(0),list(0),
                       list(c(1092,1092,938,938,707,429,429)),list(c(1527,1527,1301,932,
                                                                     693,319)), list(c(803,803)),list(c(247,628,90)),list(c(875,1065,643,643,
                                                                                                                            502,502,278)),
                       list(c(70,70,70,70)))
  # assign session specific manual addition for session_star
  append_session_start <- c(list(0), list(0), list(0), 
                            list(rep("1923-08-27", times = 21)),
                            list(c("1927-06-09","1927-06-09")),
                            list(rep("1927-09-15", times = 7)),list(0),
                            list(rep("1933-01-24", times = 5)),list(0),
                            list(c("1938-06-17","1938-06-17")),list(0),
                            list(rep("1944-05-30", times = 10)), list(c("1948-02-04", "1948-02-04",
                                                                        "1948-02-04")), list(rep("1951-05-30", times = 9)),
                            list(rep("1954-05-18", times = 7)),list(rep("1957-03-05", times = 8)),
                            list(rep("1961-10-04", times = 6)),list(rep("1965-04-07", times = 7)),
                            list(rep("1969-06-18", times = 6)),list(rep("1973-02-28", times = 7)),
                            list(rep("1977-06-16", times = 3)),list(0),
                            list(c("1982-02-18","1982-02-18")),list(rep("1982-11-24", times = 3)),
                            list(0),list(0),list(rep("1992-11-25",times =7)),
                            list(rep("1997-06-06",times = 6)),list(c("2002-05-17","2002-05-17")),
                            list(rep("2007-05-24",times = 3)),list(rep("2011-02-25",times = 7)),
                            list(rep("2016-02-26",times = 4)))
  # assign session specific manual addition for session_end
  append_session_end <- c(list(0), list(0), list(0), 
                          list(rep("1927-06-09", times = 21)),
                          list(c("1927-09-15", "1927-09-15")),
                          list(rep("1932-02-16", times = 7)),list(0),
                          list(rep("1937-07-01", times = 5)),list(0),
                          list(c("1943-06-23","1943-06-23")),list(0),
                          list(rep("1948-02-04", times = 10)), list(c("1951-05-30","1951-05-30",
                                                                      "1951-05-30")), list(rep("1954-05-18", times = 9)),
                          list(rep("1957-03-05", times = 7)),list(rep("1961-10-04", times = 8)),
                          list(rep("1965-04-07", times = 6)),list(rep("1969-06-18", times = 7)),
                          list(rep("1973-02-28", times = 6)),list(rep("1977-06-16", times = 7)),
                          list(rep("1981-06-11", times = 3)),list(0),
                          list(c("1982-11-24","1982-11-24")),list(rep("1987-02-17", times = 3)),
                          list(0),list(0),list(rep("1997-06-06",times = 7)),
                          list(rep("2002-05-17",times = 6)),list(c("2007-05-24","2007-05-24")),
                          list(rep("2011-02-25",times = 3)),list(rep("2016-02-26",times = 7)),
                          list(rep("2020-02-08",times = 4)))
  # assign empty lists to store session speficic htmls
  htmls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator URLs
  urls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator names 
  names <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator party affiliations
  parties <- rep(list(NA), times = length(source))
  multiplier <- rep(list(NA), times = length(source))
  multiplier2 <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator constituencies
  constituencies <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic beginning dates
  session_start <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic ending dates
  session_end <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator service in days
  services <- rep(list(NA), times = length(source))
  # loop through all html files
  for (i in 1:length(htmls)) {
    # read html file
    htmls[[i]] <-  read_html(source[i], encoding = "UTF-8")
    # locate legislator biography URLs in html via XPath query, extract and complete URL
    urls[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_attr("href") %>%
      str_c("https://", "en",".wikipedia.org", .)
    # locate legislator names in html via query, extract names
    names[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_text()
    # locate legislator party affiliations in html via query_party, extract and format party affiliation
    parties[[i]] <- html_nodes(x = htmls[[i]], xpath = query_party[i]) %>%
      html_text() %>%
      str_trim()
    if (!equals(length(parties[[i]]), length(urls[[i]]))) {
      multiplier[[i]] <- html_nodes(x = htmls[[i]], xpath = query_multiplier2[i]) %>%
        html_attrs() %>%
        as.numeric %>%
        na.replace(1)
      parties[[i]] <- rep(parties[[i]], times = multiplier[[i]])
    }
    constituencies[[i]] <- html_nodes(x = htmls[[i]], xpath = query_constituency[i]) %>%
      html_text() 
    if (!equals(length(constituencies[[i]]), length(urls[[i]]))) {
      multiplier2[[i]] <- html_nodes(x = htmls[[i]], xpath = query_multiplier[i]) %>%
        html_attrs() %>%
        as.numeric %>%
        na.replace(1)
      constituencies[[i]] <- rep(constituencies[[i]], times = multiplier2[[i]])
    }
    session_start[[i]] <- rep(duration[[i]][1], times = length(urls[[i]]))
    session_end[[i]] <- rep(duration[[i]][2], times = length(urls[[i]]))
    services[[i]] <- round(rep(difftime(time1 = session_end[[i]][1], time2 = session_start[[i]][1], units = "days"),
                               length(urls[[i]])))
    services[[i]] <- replace(services[[i]], replace_idx[[i]], replacement[[i]])
    if (i %in% c(4:6,8,10,12:21,23:24,27:32)) {
      urls[[i]] <- append(urls[[i]], append_urls[[i]])
      names[[i]] <- append(names[[i]], append_names[[i]])
      parties[[i]] <- append(parties[[i]], append_parties[[i]])
      constituencies[[i]] <- append(constituencies[[i]], append_constituencies[[i]])
      services[[i]] <- append(services[[i]], append_services[[i]])
      session_start[[i]] <- append(session_start[[i]], append_session_start[[i]])
      session_end[[i]] <- append(session_end[[i]], append_session_end[[i]])
    }
  }
  urls <- lapply(urls, data.table) %>%
    lapply(rename, url = V1)
  names <- lapply(names, data.table) %>%
    lapply(rename, name = V1)
  parties <- lapply(parties, data.table) %>%
    lapply(rename, party = V1)
  constituencies <- lapply(constituencies, data.table) %>%
    lapply(rename, constituency = V1)
  services <- lapply(services, data.table) %>%
    lapply(rename, service = V1)
  session_start <- lapply(session_start, data.table) %>%
    lapply(rename, session_start = V1)
  session_end <- lapply(session_end, data.table) %>%
    lapply(rename, session_end = V1)
  urls <- mapply(cbind, urls, names, parties, constituencies, 
                 services, session_start,session_end, SIMPLIFY = FALSE)
  urls <-  as.numeric(as.roman(str_extract(source, "[[:digit:]]+"))) %>%
    mapply(function(urls, x) mutate(urls, term = x), urls, ., SIMPLIFY=FALSE) %>%
    mapply(function(., x) mutate(., country = x), ., rep("IRL", times = 33), SIMPLIFY=FALSE)
}


#### FRANCE WIKIPEDIA INFORMATION EXTRACTION ============================================
collectorFrance <- function(source) {
  source <- mixedsort(list.files(source, full.names = TRUE), decreasing = TRUE)
  duration <- list(c("1958-11-30", "1962-11-25"), c("1962-11-25", "1967-03-12"),
                   c("1967-03-12", "1968-06-30"), c("1968-06-30", "1973-03-11"),
                   c("1973-03-11", "1978-03-19"), c("1978-03-19", "1981-06-21"),
                   c("1981-06-21", "1986-03-16"), c("1986-03-16", "1988-06-12"),
                   c("1988-06-12", "1993-03-28"), c("1993-03-28", "1997-06-01"),
                   c("1997-06-01", "2002-06-16"), c("2002-06-16", "2007-06-17"),
                   c("2007-06-17", "2012-06-17"), c("2012-06-17", "2017-06-18"),
                   c("2017-06-18", "2022-06-01"))
  condition <- str_c("[", str_c("@id = '", LETTERS, "' or ", collapse = ""),
                     "@id = 'Liste_des_d.C3.A9put.C3.A9s']")
  query <- c(str_c("//h2/span", condition,
                   "/ancestor::h2/following-sibling::table[1]/tbody/tr/td[2]/a"),
             str_c("//h2/span", condition,
                   "/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/a"),
             str_c("//h2/span", str_replace(condition, " or @id = 'U'", ""),
                   "/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/a"),
             rep(str_c("//h2/span", condition,
                       "/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/a"),
                 times = 10),
             str_c("//h2/span", condition,
                   "/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/a|//h2/span", condition,
                   "/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/s/a"),
             str_c("//h2/span", condition,
                   "/ancestor::h2/following-sibling::table[1]/tbody/tr/td[2]/a|//h2/span", condition,
                   "/ancestor::h2/following-sibling::table[1]/tbody/tr/td[2]/s/a"))
  query_party <- c(rep(str_c("//h2/span", condition,
                             "/ancestor::h2/following-sibling::table[1]/tbody/tr/td[3]"),
                       times = 13),
                   str_c("//h2/span", condition,
                         "/ancestor::h2/following-sibling::table[1]/tbody/tr/td[4]/a"),
                   str_c("//h2/span", condition,
                         "/ancestor::h2/following-sibling::table[1]/tbody/tr/td[9]/a[1]"))
  query_constituency <- c(rep(str_c("//h2/span", condition,
                                    "/ancestor::h2/following-sibling::table[1]/tbody/tr/td[4]"),
                              times = 13),
                          str_c("//h2/span", condition,
                                "/ancestor::h2/following-sibling::table[1]/tbody/tr/td[2]"),
                          str_c("//h2/span", condition,
                                "/ancestor::h2/following-sibling::table[1]/tbody/tr/td[3]"))
  # assign empty lists to store session speficic htmls
  htmls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator URLs
  urls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator names 
  names <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator party affiliations
  parties <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator constituencies
  constituencies <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic beginning dates
  session_start <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic ending dates
  session_end <- rep(list(NA), times = length(source))
  for (i in 1:length(htmls)) {
    # read html file
    htmls[[i]] <-  read_html(source[i], encoding = "UTF-8")
    # locate legislator biography URLs in html via XPath query, extract and complete URL
    urls[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_attr("href") %>%
      str_c("https://", "fr",".wikipedia.org", .)
    # locate legislator names in html via query, extract names
    names[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_text()
    # locate legislator party affiliations in html via query_party, extract and format party affiliation
    parties[[i]] <- html_nodes(x = htmls[[i]], xpath = query_party[i]) %>%
      html_text() %>%
      str_replace_all("\\r|\\n", "")
    constituencies[[i]] <- html_nodes(x = htmls[[i]], xpath = query_constituency[i]) %>%
      html_text() %>%
      str_replace_all("\\r|\\n", "")
    session_start[[i]] <- duration[[i]][1]
    session_end[[i]] <- duration[[i]][2]
  }
  urls <- lapply(urls, data.table) %>%
    lapply(rename, url = V1)
  names <- lapply(names, data.table) %>%
    lapply(rename, name = V1)
  parties <- lapply(parties, data.table) %>%
    lapply(rename, party = V1)
  constituencies <- lapply(constituencies, data.table) %>%
    lapply(rename, constituency = V1)
  session_start <- lapply(session_start, data.table) %>%
    lapply(rename, session_start = V1)
  session_end <- lapply(session_end, data.table) %>%
    lapply(rename, session_end = V1)
  urls <- mapply(cbind, urls, names, parties, constituencies, 
                 session_start, session_end, SIMPLIFY = FALSE)
  urls <-  as.numeric(as.roman(str_extract(source, "[[:digit:]]+"))) %>%
    mapply(function(urls, x) mutate(urls, term = x), urls, ., SIMPLIFY=FALSE) %>%
    mapply(function(., x) mutate(., country = x), ., rep("FRA", times = 15), SIMPLIFY=FALSE)
}


#### USA WIKIPEDIA INFORMATION EXTRACTION ===============================================
collectorUSA <- function(source,chamber) {
  source <- mixedsort(list.files(source, full.names = TRUE), decreasing = TRUE)
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
               rep(str_c(query_a, "]|", query_c, "]"), times = 25)
    )
    query_constituency <- c(rep("(//table[@class='multicol'])[2]//ul/li/a[1]", 90),
                            "(//table[@width='75%'])[2]//ul/li/a[1]",
                            rep("(//table[@class='multicol'])[2]//ul/li/a[1]", 25))
    query_party <- c(rep("(//table[@class='multicol'])[2]//ul/li", 90),
                     "(//table[@width='75%'])[2]//ul/li",
                     rep("(//table[@class='multicol'])[2]//ul/li", 25))
    regex_exception1 <- "\n.+|\\(Resident Commissioner\\)|\\(.Southwest Territory.+?\\)|\\(politician\\)"
    regex_exception2 <- ".acant(,)? (until|to|admitted|moved)|^Vacant.+thereafter$|newly admitted|2\\. and 3\\. Joint|.acant(,)? from|^Vacant|Vacant$|^4\\. vacant"
    regex_exception3 <- "^Vacant|^[[:digit:]]([[:digit:]])?(\\.|\\:) Vacant|At-large..(v|V)acant|2. and 3. Joint district with two seats\\.|, vacant until|\\. vacant to|(\\.|\\:) Vacant until|^[[:digit:]]([[:digit:]])?(\\.|\\:) vacant"
    query_ttime <- "//table[@class = 'infobox vevent']/tbody/tr[4]"
  }
  if (chamber == "senate") {
    query <- c(rep("(//table[@class='multicol'])[1]//ul/li/a[1]", 42),
               "(//table[@class='multicol'])[1]//ul/li/a[not(contains(@title, 'Senatorial'))
               and not(contains(@title, 'Liberal'))]",
               rep("(//table[@class='multicol'])[1]//ul/li/a[1]", 7),
               "(//table[@class='multicol'])[1]//ul/li/a[not(contains(@title, 'Senatorial'))]",
               rep("(//table[@class='multicol'])[1]//ul/li/a[1]", 39),
               "//table[2]//ul/li/a[1]",
               rep("(//table[@class='multicol'])[1]//ul/li/a[1]", 25))
    query_constituency <- c(rep("(//table[@class='multicol'])[1]//ul/li", 90),
                            "//table[2]//ul/li",
                            rep("(//table[@class='multicol'])[1]//ul/li", 25))
    query_party <- c(rep("(//table[@class='multicol'])[1]//ul/li", times = 90), "//table[2]//ul/li",
                     rep("(//table[@class='multicol'])[1]//ul/li", times = 25))
    regex_exception1 <- "\n.+|\\(Resident Commissioner\\)|\\(.Southwest Territory.+?\\)|\\(politician\\)| until December 3, 1928.+"
    regex_exception2 <- ".acant(,)? (until|to|admitted|moved)|^Vacant.+thereafter$|newly admitted|2\\. and 3\\. Joint|.acant(,)? from|^Vacant|Vacant$|(\\.|\\:) .acant"
    regex_exception3 <- "^Vacant|^[[:digit:]]([[:digit:]])?(\\.|\\:) Vacant|At-large..(v|V)acant|2. and 3. Joint district with two seats\\.|, vacant until|\\. vacant to|(\\.|\\:) Vacant until|^[[:digit:]]([[:digit:]])?(\\.|\\:) vacant"
    regex_constituency <- c(rep("h4/span[1]/a", times = 12), "h4/span[1]",
                            rep("h4/span[1]/a", times = 76), "h4/span[1]", "h4/span[1]",
                            rep("h4/span[1]/a", times = 25))
    query_ttime <- "//table[@class = 'infobox vevent']/tbody/tr[4]"
  }
  # assign empty lists to store session speficic htmls
  htmls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator URLs
  urls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator names 
  names <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator party affiliations
  parties <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator constituencies
  constituencies <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator beginning of services
  beginnings <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator end of services
  endings <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator service in days
  services <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic beginning dates
  session_start <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic ending dates
  session_end <- rep(list(NA), times = length(source))
  vacant <- rep(list(NA), times = length(source))
  term_times <- rep(list(NA), times = length(source))
  replace_idx <- c(rep(list(0), times = 4),list(c(86)),rep(list(0), times = 8),list(c(35,82)),
                   rep(list(0), times = 2),list(c(27)),list(c(64,113)),list(c(0)),list(c(11)),
                   list(c(156,158,209)),list(c(13)),rep(list(0), times = 8),list(c(223)))
  replacement <- c(rep(list(0), times = 4),list(c(178)),rep(list(0), times = 8),list(c(0,275)),
                   rep(list(0), times = 2),list(c(0)),list(c(461,48)),list(c(0)),list(c(0)),
                   list(c(0,0,327)),list(c(0)),rep(list(0), times = 8),list(c(274)))
  replace_idx2 <- c(list(c(0)),list(c(23)),list(c(0)),list(c(7)),rep(list(0), times = 47),
                    list(c(14)),rep(list(0), times = 62),list(c(2)))
  replacement2 <- c(list(c(0)),list(c(0)),list(c(0)),list(c(257)),rep(list(0), times = 47),
                    list(c(648)),rep(list(0), times = 62),list(c(328)))
  for (i in 1:116) {
    # read html file
    htmls[[i]] <-  read_html(source[i], encoding = "UTF-8")
    # locate legislator biography URLs in html via XPath query, extract and complete URL
    urls[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_attr("href") %>%
      str_c("https://", "en",".wikipedia.org", .)
    # locate legislator names in html via query, extract names
    names[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_text()
    parties[[i]] <- html_nodes(x = htmls[[i]], xpath = query_party[i]) %>%
      html_text() %>%
      str_replace_all(regex_exception1, "") %>%
      str_replace_all("^.+?(?=\\()|(?<=\\)).+", "") %>%
      str_replace_all("\\(|\\)|\n|\r", "") %>%
      extract(!str_detect(., regex_exception2))
    if (chamber == "house") {
      vacant[[i]] <- html_nodes(x = htmls[[i]], xpath = query_party[i]) %>%
        html_text() %>%
        str_replace_all(regex_exception1, "") %>%
        str_replace_all("^.+?(?=\\()|(?<=\\)).+", "") %>%
        str_replace_all("\\(|\\)|\n", "") %>%
        str_detect(., regex_exception2)
      constituencies[[i]] <- html_nodes(x = htmls[[i]], xpath = query_constituency[i]) %>%
        html_attr("title") %>%
        replace(., !str_detect(., "district|At-large"), NA) %>%
        na.locf()
      if (equals(length(vacant[[i]]), length(constituencies[[i]]))) {
        constituencies[[i]] <- constituencies[[i]] %>%
          extract(!vacant[[i]])
      }
    }
    if (chamber == "senate") {
      vacant[[i]] <- html_nodes(x = htmls[[i]], xpath = query_party[i]) %>%
        html_text() %>%
        str_replace_all(regex_exception1, "") %>%
        str_replace_all("^.+?(?=\\()|(?<=\\)).+", "") %>%
        str_replace_all("\\(|\\)|\n", "") %>%
        str_detect(., regex_exception2)
      constituency <- html_nodes(x = htmls[[i]], xpath = str_replace(query_constituency[i], "ul/li", regex_constituency[i])) %>%
        html_text()
      replicate <- html_nodes(x = htmls[[i]], xpath = query_constituency[i]) %>%
        html_text %>%
        str_extract("[[:digit:]](?=\\.|\\:)") %>%
        as.numeric
      constituencies[[i]] <- replace(replicate, which(!is.na(replicate))[-seq(from = 2,
                                                                              to = length(which(!is.na(replicate))), by = 2)],
                                     constituency) %>%
        replace(., str_detect(., "[[:digit:]]"), NA) %>%
        na.locf() %>%
        extract(!vacant[[i]])
    }
    term_times[[i]] <- html_nodes(x = htmls[[i]], xpath = query_ttime) %>%
      html_text() %>%
      str_replace_all(".\\(.+?\\)", "") %>%
      str_split(pattern = "(?<=[[:digit:]]) ", n = 2) %>%
      unlist %>%
      str_replace_all(pattern = "^.+(?=[[:upper:]])|\\n", replacement = "")
    beginnings[[i]] <- html_nodes(x = htmls[[i]], xpath = query_party[i]) %>%
      html_text() %>%
      str_replace_all("\n.+|\n", "") %>%
      extract(!str_detect(., regex_exception3)) %>%
      str_replace_all("([^[[:digit:]]]+$)|\\[.(.\\])?|, changed.+", "") %>%
      replace(.,list = !str_detect(., "[[:digit:]]{4}"), "") %>%
      str_replace_all("(?<=[[:digit:]]{4} ).+| until.+| resigned.+| died.+|.\\?\\?(\\?)?(\\?)?", "") %>%
      str_extract("[[:upper:]][[:alpha:]]+ [[:digit:]]([[:digit:]])?,.+") %>%
      na.replace(term_times[[i]][1]) %>%
      mdy()
    endings[[i]] <- html_nodes(x = htmls[[i]], xpath = query_party[i]) %>%
      html_text() %>%
      str_replace_all("\n.+|\n", "") %>%
      extract(!str_detect(., regex_exception3)) %>%
      str_replace(" - End", str_c(" - ", term_times[[i]][2])) %>%
      str_replace_all("([^[[:digit:]]]+$)| before April 6|, changed.+", "") %>%
      replace(.,list = !str_detect(., "[[:digit:]]{4}"), "") %>%
      str_replace_all("^.+[[:digit:]]{4} |( )?from.+?[[:digit:]]{4}|( )?seated.+|.\\?\\?(\\?)?(\\?)?(,)?", "") %>%
      str_extract("([[:upper:]][[:alpha:]]+)?(,)? ([[:digit:]]([[:digit:]])?, )?[[:digit:]]{4}") %>%
      str_trim %>%
      str_replace("^(?=[[:digit:]]{4})", "January 1, ") %>%
      str_replace("(?<=[[:alpha:]])((,)? )(?=[[:digit:]]{4})", " 1, ") %>%
      na.replace(term_times[[i]][2]) %>%
      mdy()
    services[[i]] <- difftime(time1 = endings[[i]], time2 = beginnings[[i]], units = "days")
    if (i %in% c(5,14,17,18,20:22,31) & chamber == "house") {
      services[[i]] <- replace(services[[i]], replace_idx[[i]], replacement[[i]])
    }
    if (i %in% c(2,4,52,115) & chamber == "senate") {
      services[[i]] <- replace(services[[i]], replace_idx2[[i]], replacement2[[i]])
    }
    session_start[[i]] <- term_times[[i]][1]
    session_end[[i]] <- term_times[[i]][2]
  }
  urls <- lapply(urls, data.table) %>%
    lapply(rename, url = V1)
  names <- lapply(names, data.table) %>%
    lapply(rename, name = V1)
  parties <- lapply(parties, data.table) %>%
    lapply(rename, party = V1)
  constituencies <- lapply(constituencies, data.table) %>%
    lapply(rename, constituency = V1)
  services <- lapply(services, data.table) %>%
    lapply(rename, service = V1)
  session_start <- lapply(session_start, data.table) %>%
    lapply(rename, session_start = V1)
  session_end <- lapply(session_end, data.table) %>%
    lapply(rename, session_end = V1)
  urls <- mapply(cbind, urls, names, parties, constituencies, 
                 services, session_start, session_end, SIMPLIFY = FALSE)
  urls <-  as.numeric(as.roman(str_extract(source, "[[:digit:]]+"))) %>%
    mapply(function(urls, x) mutate(urls, term = x), urls, ., SIMPLIFY=FALSE) %>%
    mapply(function(., x) mutate(., country = x), ., rep("USA-H", times = 116), SIMPLIFY=FALSE)
}


#### CZECH REPUBLIC WIKIPEDIA INFORMATION EXTRACTION ====================================
collectorCzech <- function(source,chamber) {
  source <- mixedsort(list.files(source, full.names = TRUE), decreasing = TRUE)
  duration <- list(c("1992-06-06","1996-06-01"),c("1996-06-01","1998-06-20"),
                   c("1998-06-20","2002-06-15"),c("2002-06-15","2006-06-03"),
                   c("2006-06-03","2010-05-29"), c("2010-05-29", "2013-10-26"),
                   c("2013-10-26", "2017-10-21"), c("2017-10-21", "2021-10-21"))
  query <- c("//h2/span[contains(text(), 'seznam')]/ancestor::h2/following-sibling::ul/li/a[1]",
             "//h2/span[contains(text(), 'seznam')]/ancestor::h2/following-sibling::ul/li/a[1]",
             "//h2/span[contains(text(), 'seznam')]/ancestor::h2/following-sibling::ul/li/a[1]",
             "//h2/span[contains(text(), 'Seznam')]/ancestor::h2/following-sibling::table/tbody/tr/td[1]/b/a[1]",
             "//h2/span[contains(text(), 'Seznam')]/ancestor::h2/following-sibling::table/tbody/tr/td[1]//a[1]",
             "//h2/span[contains(text(), 'Seznam')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[1]/a[1]|
             //h2/span[contains(text(), 'Seznam')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[1]/span[@class = 'vcard fn']/a[1]",
             rep("//h2/span[contains(text(), 'Seznam')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/a[1]|
                 //h2/span[contains(text(), 'Seznam')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/span[@class = 'vcard fn']/a[1]",
                 times = 2))
  query_party <- c("//h2/span[contains(text(), 'seznam')]/ancestor::h2/following-sibling::ul/li/a[2]","","",
                   "//h2/span[contains(text(), 'Seznam')]/ancestor::h2/following-sibling::table/tbody/tr/td[3]/a",
                   "",
                   "//h2/span[contains(text(), 'Seznam')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[2]",
                   "//h2/span[contains(text(), 'Seznam')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[2]//a",
                   "//h2/span[contains(text(), 'Seznam')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[2]")
  query_constituency <- c("","","",
                          "//h2/span[contains(text(), 'Seznam')]/ancestor::h2/following-sibling::table/tbody/tr/td[6]/a",
                          "",
                          "//h2/span[contains(text(), 'Seznam')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[4]",
                          rep("//h2/span[contains(text(), 'Seznam')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[4]",
                              times = 2))
  # assign empty lists to store session speficic htmls
  htmls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator URLs
  urls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator names 
  names <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator party affiliations
  parties <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator constituencies
  constituencies <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic beginning dates
  session_start <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic ending dates
  session_end <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator service in days
  services <- rep(list(NA), times = length(source))
  # assign session specific manual replacement index for services
  replace_idx <- c(list(c(0)),
                   list(c(0)),
                   list(c(0)),
                   list(c(5,7,11,15,18,22,41,83,120,141,158,161,228,236,25,33,39,44,49,55,70,71,84,86,89,
                          95,100,111,116,119,121,129,151,152,155,160,162,164,171,173,196,201,216,219,220,
                          221,224,231)),
                   list(c(0)),
                   list(c(13,15,19,24,33,42,50,51,52,63,66,76,77,81,86,94,101,106,107,110,119,142,144,145,146,174,177,186,187,188,189,196,218)),
                   list(c(10,23,26,50,52,58,77,79,80,94,100,113,137,138,143,146,148,149,150,164,167,174,176,179,190,194,212)),
                   list(c(32,60,88,115,133,137,138,144,147,152,166,168,172,188)))
  # assign session specific manual replacement for services
  replacement <- c(list(c(0)),
                   list(c(0)),
                   list(c(0)),
                   list(c(695,695,754,1149,13,766,766,766,766,766,766,766,766,766,58,0,832,695,
                          142,326,1055,149,771,225,707,653,188,195,1366,632,146,632,631,6,695,
                          695,1196,312,1109,695,695,695,695,1105,623,188,996,107)),
                   list(c(0)),
                   list(c(131,352,352,875,1,875,185,893,276,367,1048,619,267,875,203,184,367,1042,969,875,197,300,352,626,1060,1098,944,147,367,1114,893,893,367)),
                   list(c(32,1235,1299,1269,1047,186,1322,111,360,247,1417,1208,306,290,1208,408,1165,247,237,220,6,1149,1095,1344,133,1218,156)),
                   list(c(1219,34,409,1426,1426,241,1051,34,1426,34,1299,161,345,1115)))
  # assign removal index
  for (i in 1:length(htmls)) {
    # read html file
    htmls[[i]] <-  read_html(source[i], encoding = "UTF-8")
    # locate legislator biography URLs in html via XPath query, extract and complete URL
    urls[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_attr("href") %>%
      str_c("https://", "cs",".wikipedia.org", .)
    # locate legislator names in html via query, extract names
    names[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_text() %>%
      str_squish()
    session_start[[i]] <- duration[[i]][1]
    session_end[[i]] <- duration[[i]][2]
    if (i %in% c(4,6:8)) {
      # locate legislator party affiliations in html via query_party, extract and format party affiliation
      parties[[i]] <- html_nodes(x = htmls[[i]], xpath = query_party[i]) %>%
        html_text() %>%
        str_replace_all("\\r|\\n", "")
      if (i  %in% 6) { # check whether this ndex is correct
        parties[[i]] <- str_replace(parties[[i]], ".+(?= [[:alpha:]])", "") %>%
          str_squish()
      }
      constituencies[[i]] <- html_nodes(x = htmls[[i]], xpath = query_constituency[i]) %>%
        html_text() %>%
        str_replace_all("\\r|\\n", "")
      services[[i]] <- round(rep(difftime(time1 = session_end[[i]][1], time2 = session_start[[i]][1], units = "days"),
                                 length(urls[[i]])))
      services[[i]] <- replace(services[[i]], replace_idx[[i]], replacement[[i]])
      services[[i]] <- as.numeric(services[[i]])
    }
    if (i %in% c(1,2,3,5)) {
      if (i %in% 5) {
        data2 <- data.frame(urls = urls[[i]], names = names[[i]], nameso = names[[i]], stringsAsFactors = FALSE)
        data2$names <- str_c(str_replace(data2$names, ".+? ", ""), " ", str_extract(data2$names, ".+? ")) %>%
          str_replace(" \\(politik\\)", "")
        data2 <- arrange(data2, names)
        redist <- data2[c(29,30,58,88,99,160,162,163,166,168,169,170,173,174,175,178,179,182,188,189,191,192,193,198),]
        data2 <- data2[-c(4,29,30,33,58,69,80,88,99,120,140,160,162,163,166,168,169,170,171,173,174,175,178,179,182,184,188,189,191,192,193,197,198,210,225,232,234),]
        rownames(data2) <- seq(length=nrow(data2)) 
        data2 <- rbind(data2[1:54,],redist[3,],data2[55:62,],redist[1:2,],data2[63:81,],redist[4,],data2[82:92,],redist[5,],data2[93:151,],
                       redist[6:8,],data2[152:153,],redist[9,],data2[154:168,],redist[10:24,],data2[169:197,])
        urls[[i]] <- data2$urls
        names[[i]] <- data2$nameso
      }
      if (i %in% c(1,2,3)) {
        urls[[i]] <- urls[[i]][-length(urls[[i]])]
        names[[i]] <- names[[i]][-length(names[[i]])]
      }
      if (i %in% 1) {
        urls[[i]] <-  urls[[i]][-c(57,64)]
        names[[i]] <- names[[i]][-c(57,64)]
        parties[[i]] <- html_nodes(x = htmls[[i]], xpath = query_party[i]) %>%
          html_text() %>%
          extract(-c(57,64))
      }
      htmls2 <- lapply(mixedsort(list.files(str_c("./data/htmls/czech_official/",i,"/"), full.names = TRUE)), read_html)
      dates <- lapply(htmls2, html_nodes, xpath = "(//li[@class='previous'])[1]") %>%
        lapply(., html_text) %>% unlist %>% str_squish
      beginnings <- dates %>% str_replace(".+?od", "") %>% str_split("do") %>% lapply(`[`,1) %>% unlist %>% str_replace_all(" ", "") %>% dmy
      endings <- dates %>% str_replace(".+?od", "") %>% str_split("do") %>% lapply(`[`,2) %>% unlist %>% str_replace_all(" ", "") %>% dmy
      services[[i]] <- as.numeric(round(difftime(time1 = endings, time2 = beginnings, units = "days")))
      parcon <- lapply(htmls2, html_nodes, xpath = "//div[@class = 'figcaption']/p/text()[preceding-sibling::br]") %>% lapply(html_text)
      constituencies[[i]] <- lapply(parcon, `[`, 1) %>% unlist %>% str_squish %>% str_replace(".+?: ", "")
      if (i %in% c(2,3,5)) {
        parties[[i]] <- lapply(parcon, `[`, 2) %>% unlist %>% str_squish %>% str_replace(".+?: ", "")
      }
    }
  }
  urls <- lapply(urls, data.table) %>%
    lapply(rename, url = V1)
  names <- lapply(names, data.table) %>%
    lapply(rename, name = V1)
  parties <- lapply(parties, data.table) %>%
    lapply(rename, party = V1)
  constituencies <- lapply(constituencies, data.table) %>%
    lapply(rename, constituency = V1)
  services <- lapply(services, data.table) %>%
    lapply(rename, service = V1)
  session_start <- lapply(session_start, data.table) %>%
    lapply(rename, session_start = V1)
  session_end <- lapply(session_end, data.table) %>%
    lapply(rename, session_end = V1)
  urls <- mapply(cbind, urls, names, parties, constituencies, 
                 services, session_start, 
                 session_end, SIMPLIFY = FALSE)
  urls <-  as.numeric(as.roman(str_extract(source, "[[:digit:]]+"))) %>%
    mapply(function(urls, x) mutate(urls, term = x), urls, ., SIMPLIFY=FALSE) %>%
    mapply(function(., x) mutate(., country = x), ., rep("CZE", times = 8), SIMPLIFY=FALSE)
}


#### SCOTLAND WIKIPEDIA INFORMATION EXTRACTION ==========================================
collectorScotland <- function(source) {
  source <- mixedsort(list.files(source, full.names = TRUE), decreasing = TRUE)
  duration <- list(c("1999-05-06","2003-05-01"),c("2003-05-01","2007-05-03"),
                   c("2007-05-03","2011-05-05"),c("2011-05-05","2016-05-05"),
                   c("2016-05-05","2021-05-06"))
  query <- rep("//h2/span[contains(text(), 'MSPs')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[1]//a[1]|
               //h3/span[contains(text(), 'Former MSPs')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[1]//a[1]",
               times = 5)
  query_party <- rep("//h2/span[contains(text(), 'MSPs')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[5]//a[1]|
                     //h3/span[contains(text(), 'Former MSPs')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[5]//a[1]",
                     times = 5)
  query_constituency <- rep("//h2/span[contains(text(), 'MSPs')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[3]//a[1]|
                            //h3/span[contains(text(), 'Former MSPs')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[3]//a[1]",
                            times = 5)
  query_constituency2 <- rep("//h2/span[contains(text(), 'MSPs')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[4]|
                             //h3/span[contains(text(), 'Former MSPs')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[4]",
                             times = 5)
  # assign empty lists to store session speficic htmls
  htmls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator URLs
  urls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator names 
  names <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator party affiliations
  parties <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator constituencies
  constituencies <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator constituencies2
  constituencies2 <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic beginning dates
  session_start <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic ending dates
  session_end <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator service in days
  services <- rep(list(NA), times = length(source))
  # assign session specific manual replacement index for services
  replace_idx <- c(list(c(133,105,130,9,131,134,114,29,132,30)),list(c(131,4,132,15,130,42,133,62,125,134,100)),
                   list(c(130,114,131,80)),list(c(129,7,130,84,4,131,17,132,54,133,106,134)),
                   list(c(130,12,131,8,132,53,133,85)))
  # assign session specific manual replacement for services
  replacement <- c(list(c(229,1140,524,888,739,739,692,692,827,628)),list(c(620,842,778,684,854,580,1055,1443,390,1072,390)),
                   list(c(120,1342,645,817)),list(c(582,1244,721,1791,1086,830,996,856,970,919,907,1065)),
                   list(c(216,1610,364,1462,404,1422,404,1422)))
  # loop through all html files
  for (i in 1:length(htmls)) {
    # read html file
    htmls[[i]] <-  read_html(source[i], encoding = "UTF-8")
    # locate legislator biography URLs in html via XPath query, extract and complete URL
    urls[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_attr("href") %>%
      str_c("https://", "en",".wikipedia.org", .)
    # locate legislator names in html via query, extract names
    names[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
      html_text()
    # locate legislator party affiliations in html via query_party, extract and format party affiliation
    parties[[i]] <- html_nodes(x = htmls[[i]], xpath = query_party[i]) %>%
      html_text() %>%
      str_replace_all("\\r|\\n", "")
    constituencies[[i]] <- html_nodes(x = htmls[[i]], xpath = query_constituency[i]) %>%
      html_text() %>%
      str_replace_all("\\r|\\n", "")
    constituencies2[[i]] <- html_nodes(x = htmls[[i]], xpath = query_constituency2[i]) %>%
      html_text() %>%
      str_replace_all("\\r|\\n", "")
    session_start[[i]] <- duration[[i]][1]
    session_end[[i]] <- duration[[i]][2]
    services[[i]] <- round(rep(difftime(time1 = session_end[[i]][1], time2 = session_start[[i]][1], units = "days"),
                               length(urls[[i]])))
    services[[i]] <- replace(services[[i]], replace_idx[[i]], replacement[[i]])
  }
  urls <- lapply(urls, data.table) %>%
    lapply(rename, url = V1)
  names <- lapply(names, data.table) %>%
    lapply(rename, name = V1)
  parties <- lapply(parties, data.table) %>%
    lapply(rename, party = V1)
  constituencies <- lapply(constituencies, data.table) %>%
    lapply(rename, constituency = V1)
  constituencies2 <- lapply(constituencies2, data.table) %>%
    lapply(rename, constituency2 = V1)
  services <- lapply(services, data.table) %>%
    lapply(rename, service = V1)
  session_start <- lapply(session_start, data.table) %>%
    lapply(rename, session_start = V1)
  session_end <- lapply(session_end, data.table) %>%
    lapply(rename, session_end = V1)
  urls <- mapply(cbind, urls, names, parties, constituencies, 
                 constituencies2, services, session_start, 
                 session_end, SIMPLIFY = FALSE)
  urls <-  as.numeric(as.roman(str_extract(source, "[[:digit:]]+"))) %>%
    mapply(function(urls, x) mutate(urls, term = x), urls, ., SIMPLIFY=FALSE) %>%
    mapply(function(., x) mutate(., country = x), ., rep("SCO", times = 5), SIMPLIFY=FALSE)
}


#### UNITED KINGDOM WIKIPEDIA INFORMATION EXTRACTION ====================================
collectorUk <- function(source) {
  source <- mixedsort(list.files(source, full.names = TRUE), decreasing = TRUE)
  duration <- list(c("1801-01-01", "1802-07-05"), c("1802-07-05", "1806-10-29"),
                   c("1806-10-29", "1807-05-04"), c("1807-05-04", "1812-10-05"),
                   c("1812-10-05", "1818-08-04"), c("1818-08-04", "1820-03-06"),
                   c("1820-03-06", "1826-06-07"), c("1826-06-07", "1830-07-29"),
                   c("1830-07-29", "1831-04-28"), c("1831-04-28", "1832-12-08"),
                   c("1832-12-08", "1835-01-06"),
                   c("1852-07-07", "1857-03-27"), c("1857-03-27", "1859-04-28"),
                   c("1868-11-17", "1874-01-31"), c("1874-01-31", "1880-03-31"),
                   c("1880-03-31", "1885-11-24"), c("1885-11-24", "1886-07-01"),
                   c("1886-07-01", "1892-07-04"), c("1892-07-04", "1895-07-13"),
                   c("1895-07-13", "1900-09-26"), c("1900-09-26", "1906-01-12"),
                   c("1906-01-12", "1910-01-15"), c("1910-01-15", "1910-12-03"),
                   c("1910-12-03", "1918-12-14"), c("1918-12-14", "1922-11-15"),
                   c("1922-11-15", "1923-12-06"), c("1923-12-06", "1924-10-29"),
                   c("1924-10-29", "1929-05-30"), c("1929-05-30", "1931-10-27"),
                   c("1931-10-27", "1935-11-14"), c("1935-11-14", "1945-07-05"),
                   c("1945-07-05","1950-02-23"), c("1950-02-23","1951-10-25"),
                   c("1951-10-25","1955-05-26"), c("1955-05-26","1959-10-08"),
                   c("1959-10-08","1964-10-15"), c("1964-10-15","1966-03-31"),
                   c("1966-03-31","1970-06-18"), c("1970-06-18","1974-02-28"),
                   c("1974-02-28","1974-10-10"), c("1974-10-10","1979-05-03"),
                   c("1979-05-03","1983-06-09"), c("1983-06-09","1987-06-11"),
                   c("1987-06-11","1992-04-09"), c("1992-04-09","1997-05-01"),
                   c("1997-05-01","2001-06-07"), c("2001-06-07","2005-05-05"),
                   c("2005-05-05","2010-05-06"), c("2010-05-06","2015-05-07"),
                   c("2015-05-07","2017-06-08"), c("2017-06-08","2019-12-12"),
                   c("2019-12-12","2024-05-02"))
  query <- c("", "", "", "", "", "", "", "", "", "",
             "//h2/span[contains(text(), 'MPs')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[7]/b/span/a[1]|
             //h2/span[contains(text(), 'MPs')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[7]/b/a[1]",
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1][not(contains(., 'Independent'))]|
             //table[@class = 'wikitable']/tbody/tr/td[2]/i/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[1]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[2]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[3]/td[1]//a[1]",
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1][not(contains(., 'Independent'))]|
             //table[@class = 'wikitable']/tbody/tr/td[2]/i/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[1]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[2]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[3]/td[1]//a[1]",
             "//table[@class = 'wikitable']/tbody/tr/td[2]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[1]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[2]/td[1]//a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[3]/td[1]//a[1]",
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[2]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[3]/td[1]/a[1]",
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[2]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[3]/td[1]/a[1]",
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1][not(contains(., 'Party'))] |
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]", 
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1][not(contains(., 'Party'))] |
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]", 
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1][not(contains(., 'Federation'))] |
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]", 
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1][not(contains(., 'Parnellite'))] |
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]", 
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1][not(contains(., 'Party'))] |
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]", 
             "//h2/span[contains(text(), 'MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[2]/a[1][not(contains(., 'Party'))] |
             //h2/span[contains(text(), 'MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]",
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1][not(contains(., 'Party'))] |
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]",
             "//h2/span[contains(text(), 'MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[2]//a[1]",
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[1]/a[1]", # 31 -
             "//table[3]/tbody/tr/td[2]/a[1]|
             //table[3]/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[3]/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[3]/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[1]/a[1]",
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[1]/a[1]",
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[1]/a[1]",
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[1]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]/ancestor::tr/td[2]/a[1]|
             //h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[1]/a[1]", # 36
             "//table[@class = 'wikitable']/tbody/tr/td[2]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[1]/a[1]", # 37
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]/ancestor::tr/td[2]/a[1]|
             //h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[1]/a[1]|
             //h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[1]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[2]/a[1]",
             "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[5]/a[1]",
             "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[5]/span//a[1]",
             "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[3]/b/a[1]",
             "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[3]/b/a[1]")
  query_party <- c("", "", "", "", "", "", "", "", "", "",
                   "//h2/span[contains(text(), 'MPs')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[8]/a[1]",
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[2]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[3]/td[2]",
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[2]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[3]/td[2]",
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[2]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[3]/td[2]",
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[2]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[3]/td[2]",
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[2]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='4']/ancestor::tr/following-sibling::tr[3]/td[2]",
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]", 
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]", 
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]", 
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]",
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]",
                   "//h2/span[contains(text(), 'MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[3]|
                   //h2/span[contains(text(), 'MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]",
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]",
                   "//h2/span[contains(text(), 'MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[3]",
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[2]",
                   "//table[3]/tbody/tr/td[3]|
                   //table[3]/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[3]/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[3]/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[2]",
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[2]",
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[2]",
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[2]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]/ancestor::tr/td[3]|
                   //h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[2]",
                   "//table[@class = 'wikitable']/tbody/tr/td[3]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //table[@class = 'wikitable']/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[2]", # 37
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]/ancestor::tr/td[3]|
                   //h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[@rowspan='2']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[1]/td[2]|
                   //h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[@rowspan='3']/ancestor::tr/following-sibling::tr[2]/td[2]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[3]",
                   "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[5]/a[2]",
                   "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[5]/a[1]",
                   "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[2]/a[1]",
                   "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[5]/a[1]")
  query_constituency <- c("", "", "", "", "", "", "", "", "", "",
                          "//h2/span[contains(text(), 'MPs')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[1]/b/a[1]|
                          //h2/span[contains(text(), 'MPs')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[1]/b/span/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[3]/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]", # 37
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]/ancestor::tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[2]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[1]/a[1]",
                          "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/th[1]/a[1]",
                          "//h2/span[contains(text(), 'List of MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/th[1]/a[1]")
  query_multiplier <- c("", "", "", "", "", "", "", "", "", "",
                        "",
                        "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]", 
                        "//table[3]/tbody/tr/td[3]/ancestor::tr/td[1]", 
                        "//table[3]/tbody/tr/td[3]/ancestor::tr/td[1]", 
                        "//table[3]/tbody/tr/td[3]/ancestor::tr/td[1]", 
                        "//table[3]/tbody/tr/td[3]/ancestor::tr/td[1]", 
                        "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]",
                        "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]",
                        "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]",
                        "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]",
                        "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]", 
                        "//h2/span[contains(text(), 'MPs')]/ancestor::h2/following-sibling::table[1]/tbody/tr/td[3]/ancestor::tr/td[1]", 
                        "//table[3]/tbody/tr/td[3]/ancestor::tr/td[1]", "",
                        "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]", 
                        "//table[3]/tbody/tr/td[3]/ancestor::tr/td[1]", 
                        "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]", 
                        "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]", 
                        "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]", 
                        "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]/ancestor::tr/td[1]", 
                        "//table[@class = 'wikitable']/tbody/tr/td[3]/ancestor::tr/td[1]", 
                        "//h2/span[contains(text(), 'Composition')]/ancestor::h2/following-sibling::table[3]/tbody/tr/td[3]/ancestor::tr/td[1]")
  query_be <- c("", "", "", "", "", "", "", "", "", "",
                "", "", "", "", "", "", "", "", "", "",
                "//h3/span/a[contains(text(), '27th')]/ancestor::tr/following-sibling::tr/td[6]/a[preceding::a[contains(text(), '27th')]]",
                "//h3/span/a[contains(text(), '28th')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '27th')]]",
                "//h3/span/a[contains(text(), '29th')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '28th')]]",
                "//h3/span/a[contains(text(), '30th')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '29th')]]",
                "//h3/span/a[contains(text(), '31st')]/ancestor::tr/following-sibling::tr/td[6]/a[preceding::a[contains(text(), '31st')]]",
                "//h3/span/a[contains(text(), '32nd')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '31st')]]",
                "//h3/span/a[contains(text(), '33rd')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '32nd')]]",
                "//h3/span/a[contains(text(), '34th')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '33rd')]]",
                "//h3/span/a[contains(text(), '35th')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '34th')]]",
                "//h3/span/a[contains(text(), '36th')]/ancestor::tr/following-sibling::tr/td[6]/a[preceding::a[contains(text(), '36th')]]",
                "//h3/span/a[contains(text(), '37th')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '36th')]]",
                "//h3/span/a[contains(text(), '38th')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '37th')]]",
                "//h3/span/a[contains(text(), '39th')]/ancestor::tr/following-sibling::tr/td[6]/a[preceding::a[contains(text(), '39th')]]",
                "//h3/span/a[contains(text(), '40th')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '39th')]]",
                "//h3/span/a[contains(text(), '41st')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '40th')]]",
                "//h3/span/a[contains(text(), '42nd')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '41st')]]",
                "//h3/span/a[contains(text(), '43rd')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '42nd')]]",
                "//h3/span/a[contains(text(), '44th')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '43rd')]]",
                "//h3/span/a[contains(text(), '45th')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '44th')]]",
                "//h3/span/a[contains(text(), '46th')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '45th')]]",
                "//h3/span/a[contains(text(), '47th')]/ancestor::tr/following-sibling::tr/td[6]/a[following::a[contains(text(), '46th')]]",
                "//h3/span[contains(text(), '1983 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[6]/a",
                "//h3/span[contains(text(), '1987 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[6]/a",
                "//h3/span[contains(text(), '1992 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[6]/a",
                "//h3/span[contains(text(), '1997 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[6]/a",
                "//h3/span[contains(text(), '2001 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[6]/a",
                "//h3/span[contains(text(), '2005 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[6]/a",
                "//h3/span[contains(text(), '2010 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[6]/a",
                "//h3/span[contains(text(), '15 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[6]/a",
                "//h3/span[contains(text(), '17 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[6]/a",
                "//h3/span[contains(text(), 'present Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[6]/a")
  query_be_party <- c("", "", "", "", "", "", "", "", "", "",
                      "", "", "", "", "", "", "", "", "", "",
                      "//h3/span/a[contains(text(), '27th')]/ancestor::tr/following-sibling::tr/td[8]/a[preceding::a[contains(text(), '27th')]]",
                      "//h3/span/a[contains(text(), '28th')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '27th')]]",
                      "//h3/span/a[contains(text(), '29th')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '28th')]]",
                      "//h3/span/a[contains(text(), '30th')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '29th')]]",
                      "//h3/span/a[contains(text(), '31st')]/ancestor::tr/following-sibling::tr/td[8]/a[preceding::a[contains(text(), '31st')]]",
                      "//h3/span/a[contains(text(), '32nd')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '31st')]]",
                      "//h3/span/a[contains(text(), '33rd')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '32nd')]]",
                      "//h3/span/a[contains(text(), '34th')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '33rd')]]",
                      "//h3/span/a[contains(text(), '35th')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '34th')]]",
                      "//h3/span/a[contains(text(), '36th')]/ancestor::tr/following-sibling::tr/td[8]/a[preceding::a[contains(text(), '36th')]]",
                      "//h3/span/a[contains(text(), '37th')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '36th')]]",
                      "//h3/span/a[contains(text(), '38th')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '37th')]]",
                      "//h3/span/a[contains(text(), '39th')]/ancestor::tr/following-sibling::tr/td[8]/a[preceding::a[contains(text(), '39th')]]",
                      "//h3/span/a[contains(text(), '40th')]/ancestor::tr/following-sibling::tr/td[8]/a[1][following::a[contains(text(), '39th')]]",
                      "//h3/span/a[contains(text(), '41st')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '40th')]]",
                      "//h3/span/a[contains(text(), '42nd')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '41st')]]",
                      "//h3/span/a[contains(text(), '43rd')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '42nd')]]",
                      "//h3/span/a[contains(text(), '44th')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '43rd')]]",
                      "//h3/span/a[contains(text(), '45th')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '44th')]]",
                      "//h3/span/a[contains(text(), '46th')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '45th')]]",
                      "//h3/span/a[contains(text(), '47th')]/ancestor::tr/following-sibling::tr/td[8]/a[following::a[contains(text(), '46th')]]",
                      "//h3/span[contains(text(), '1983 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[8]/a",
                      "//h3/span[contains(text(), '1987 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[8]/a",
                      "//h3/span[contains(text(), '1992 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[8]/a",
                      "//h3/span[contains(text(), '1997 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[8]/a",
                      "//h3/span[contains(text(), '2001 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[8]/a",
                      "//h3/span[contains(text(), '2005 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[8]/a",
                      "//h3/span[contains(text(), '2010 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[8]/a",
                      "//h3/span[contains(text(), '15 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[8]/a",
                      "//h3/span[contains(text(), '17 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[8]/a",
                      "//h3/span[contains(text(), 'present Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[8]/a")
  query_be_constituency <- c("", "", "", "", "", "", "", "", "", "",
                             "", "", "", "", "", "", "", "", "", "",
                             "//h3/span/a[contains(text(), '27th')]/ancestor::tr/following-sibling::tr/td[1]/a[preceding::a[contains(text(), '27th')]]",
                             "//h3/span/a[contains(text(), '28th')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '27th')]]",
                             "//h3/span/a[contains(text(), '29th')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '28th')]]",
                             "//h3/span/a[contains(text(), '30th')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '29th')]]",
                             "//h3/span/a[contains(text(), '31st')]/ancestor::tr/following-sibling::tr/td[1]/a[preceding::a[contains(text(), '31st')]]",
                             "//h3/span/a[contains(text(), '32nd')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '31st')]]",
                             "//h3/span/a[contains(text(), '33rd')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '32nd')]]",
                             "//h3/span/a[contains(text(), '34th')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '33rd')]]",
                             "//h3/span/a[contains(text(), '35th')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '34th')]]",
                             "//h3/span/a[contains(text(), '36th')]/ancestor::tr/following-sibling::tr/td[1]/a[preceding::a[contains(text(), '36th')]]",
                             "//h3/span/a[contains(text(), '37th')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '36th')]]",
                             "//h3/span/a[contains(text(), '38th')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '37th')]]",
                             "//h3/span/a[contains(text(), '39th')]/ancestor::tr/following-sibling::tr/td[1]/a[preceding::a[contains(text(), '39th')]]",
                             "//h3/span/a[contains(text(), '40th')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '39th')]]",
                             "//h3/span/a[contains(text(), '41st')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '40th')]]",
                             "//h3/span/a[contains(text(), '42nd')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '41st')]]",
                             "//h3/span/a[contains(text(), '43rd')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '42nd')]]",
                             "//h3/span/a[contains(text(), '44th')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '43rd')]]",
                             "//h3/span/a[contains(text(), '45th')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '44th')]]",
                             "//h3/span/a[contains(text(), '46th')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '45th')]]",
                             "//h3/span/a[contains(text(), '47th')]/ancestor::tr/following-sibling::tr/td[1]/a[following::a[contains(text(), '46th')]]",
                             "//h3/span[contains(text(), '1983 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[1]/a",
                             "//h3/span[contains(text(), '1987 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[1]/a",
                             "//h3/span[contains(text(), '1992 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[1]/a",
                             "//h3/span[contains(text(), '1997 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[1]/a",
                             "//h3/span[contains(text(), '2001 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[1]/a",
                             "//h3/span[contains(text(), '2005 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[1]/a",
                             "//h3/span[contains(text(), '2010 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[1]/a",
                             "//h3/span[contains(text(), '15 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[1]/a",
                             "//h3/span[contains(text(), '17 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[1]/a",
                             "//h3/span[contains(text(), 'present Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[1]/a")
  query_be_service <- c("", "", "", "", "", "", "", "", "", "",
                        "", "", "", "", "", "", "", "", "", "",
                        "//h3/span/a[contains(text(), '27th')]/ancestor::tr/following-sibling::tr/td[2][preceding::a[contains(text(), '27th')]]",
                        "//h3/span/a[contains(text(), '28th')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '27th')]]",
                        "//h3/span/a[contains(text(), '29th')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '28th')]]",
                        "//h3/span/a[contains(text(), '30th')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '29th')]]",
                        "//h3/span/a[contains(text(), '31st')]/ancestor::tr/following-sibling::tr/td[2][preceding::a[contains(text(), '31st')]]",
                        "//h3/span/a[contains(text(), '32nd')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '31st')]]",
                        "//h3/span/a[contains(text(), '33rd')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '32nd')]]",
                        "//h3/span/a[contains(text(), '34th')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '33rd')]]",
                        "//h3/span/a[contains(text(), '35th')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '34th')]]",
                        "//h3/span/a[contains(text(), '36th')]/ancestor::tr/following-sibling::tr/td[2][preceding::a[contains(text(), '36th')]]",
                        "//h3/span/a[contains(text(), '37th')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '36th')]]",
                        "//h3/span/a[contains(text(), '38th')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '37th')]]",
                        "//h3/span/a[contains(text(), '39th')]/ancestor::tr/following-sibling::tr/td[2][preceding::a[contains(text(), '39th')]]",
                        "//h3/span/a[contains(text(), '40th')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '39th')]]",
                        "//h3/span/a[contains(text(), '41st')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '40th')]]",
                        "//h3/span/a[contains(text(), '42nd')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '41st')]]",
                        "//h3/span/a[contains(text(), '43rd')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '42nd')]]",
                        "//h3/span/a[contains(text(), '44th')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '43rd')]]",
                        "//h3/span/a[contains(text(), '45th')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '44th')]]",
                        "//h3/span/a[contains(text(), '46th')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '45th')]]",
                        "//h3/span/a[contains(text(), '47th')]/ancestor::tr/following-sibling::tr/td[2][following::a[contains(text(), '46th')]]",
                        "//h3/span[contains(text(), '1983 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[2]",
                        "//h3/span[contains(text(), '1987 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[2]",
                        "//h3/span[contains(text(), '1992 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[2]",
                        "//h3/span[contains(text(), '1997 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[2]",
                        "//h3/span[contains(text(), '2001 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[2]",
                        "//h3/span[contains(text(), '2005 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[2]",
                        "//h3/span[contains(text(), '2010 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[2]",
                        "//h3/span[contains(text(), '15 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[2]",
                        "//h3/span[contains(text(), '17 Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[2]",
                        "//h3/span[contains(text(), 'present Parliament')]/ancestor::h3/following-sibling::table[1]/tbody/tr/td[2]")
  # assign empty lists to store session speficic htmls
  htmls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator URLs
  urls <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator names 
  names <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator party affiliations
  parties <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator constituencies
  constituencies <- rep(list(NA), times = length(source))
  multipliers1 <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic beginning dates
  session_start <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic ending dates
  session_end <- rep(list(NA), times = length(source))
  # assign empty lists to store session speficic legislator service in days
  services <- rep(list(NA), times = length(source))
  # assign session specific manual replacement index for services
  replace_idx <- c(rep(list(c(0)),31),list(c(88,347,540,506,273,38,249,212,548,247,537,92,163,18,618,453,244,216,303,
                                             261,323,364,324,430,331,2,156,4,451,492,243,39,466,55,187,441,153,283,291,
                                             245,24,473,579,328,83,101,145,407,208,20,528)),
                   list(c(595,614,377,285,444,109,7,108,51,66,447,256,354,101,195,514)),
                   list(c(621,541,213,570,445,425,323,19,375,409,554,2,11,575,517,163,410,211,
                          282,20,90,287,276,319,448,303,444,162,113,63,8,552,188,289,544,33,326,
                          126,228,71,50,622,16,150,196,351,529,89)),
                   list(c(604,457,244,430,285,50,531,520,5,465,142,411,18,603,606,503,201,
                          332,570,255,485,371,358,327,263,184,310,204,215,423,45,588,109,
                          132,590,363,398,139,141,425,568,585,563,243,296,350,80,569,271,
                          246,576)),
                   list(c(373,231,610,498,178,118,540,553,505,394,196,384,343,52,484,
                          549,595,172,352,560,491,154,433,427,262,185,144,356,598,177,
                          404,409,173,545,464,449,403,78,368,248,450,234,393,108,587,
                          454,72,621,303,153,123,81,208,567,461,383,133,84,47,213,286,
                          101)),
                   list(c(318,222,148,313,66,7,493,501,509,203,12,442,366)),
                   list(c(28,97,593,380,562,452,419,332,252,68,142,577,605,585,103,416,39,120,
                          415,514,446,590,398,190,10,337,177,391,358,279,586,122,99,307,481,442,
                          257,132)),
                   list(c(312,252,212,57,482,327,390,603,597,367,194,141,575,555,483,532,341,400,384,
                          607,538,271,289,265,112,526,20,372,218,503)),
                   list(c(439)),
                   list(c(383,357,157,474,58,466,406,285,632,230,358,255,485,90,68,510,277,21,74,153,
                          125,628,590,431,577,500,620,563,165,165)),
                   list(c(175,538,262,540,70,270,159,415,47,257,56,170,173,601,241,253,533,312,404,134,491)),
                   list(c(348,367,580,281,349,420,495,184,252,427,584,83,233,468,562,533,172,135,458)),
                   list(c(354,354,304,379,408,414,478,453,452,79,214,350,74,588,532,591,261,590,480,464,234,263,341)),
                   list(c(397,188,191,628,24,536,303,372,195,459,334,198,406,423,217,173,80,22,490,141,418)),
                   list(c(637,224,621,464,241,520,586,481,125,314,635,269,204,327,637,35,450,595)),
                   list(c(278,49,333,80,440,307)),
                   list(c(238,422,242,236,257,275,152,490,184,94,61,180,340,126)),
                   list(c(284,385,646,520,366,204,158,368,477,114,150,354,77,226,298,38,327,23,435)),
                   list(c(436,495,433,576,30,631,468,503,149,549)),
                   list(c(619,331,392,447,79)), list(c(0)))
  # assign session specific manual replacement for services
  replacement <- c(rep(list(c(0)),31),list(c(1617,1477,1350,1344,1330,1323,1239,1227,1190,1183,1029,993,980,974,973,958,937,882,875,
                                             874,812,798,671,586,518,518,510,509,503,502,420,385,383,382,336,334,256,232,231,
                                             222,217,210,161,138,133,132,118,118,90,89,88)),
                   list(c(483,476,450,422,406,357,280,280,279,266,252,244,217,70,61,41)),
                   list(c(1239,1197,1190,1188,1183,1176,1153,1122,1120,1106,1106,1099,1099,1092,1092,
                          1071,902,896,868,866,847,840,840,832,770,756,749,749,693,616,614,566,538,
                          524,523,523,504,476,399,399,376,376,368,364,266,105,104,104)),
                   list(c(1484,1477,1414,1399,1393,1393,1344,1281,1274,1264,1260,1260,1113,
                          1113,1113,1113,1113,1085,1036,1022,993,924,917,882,840,763,735,735,
                          734,665,665,651,651,644,644,630,573,539,490,407,378,280,264,264,264,
                          259,252,203,196,195,77)),
                   list(c(1708,1701,1680,1680,1680,1680,1526,1519,1519,1519,1505,1491,
                          1491,1475,1412,1407,1365,1365,1351,1267,1267,1260,1141,1141,
                          1141,1141,1141,1008,980,972,972,950,922,910,896,888,888,887,
                          882,770,763,763,762,574,560,560,532,525,525,525,525,413,406,
                          405,405,405,405,405,405,224,161,161)),
                   list(c(469,392,385,280,203,168,160,159,112,112,112,98,98)),
                   list(c(1449,1442,1344,1344,1309,1309,1309,1309,1309,1183,1148,1113,1092,1092,
                          1092,952,945,840,819,805,805,728,728,728,728,714,602,581,581,581,
                          539,539,392,350,343,343,343,105)),
                   list(c(1239,1239,1239,1239,1134,1134,1105,1071,1071,987,987,987,903,903,861,686,686,
                          665,469,462,455,385,364,343,343,343,287,287,154,126)),
                   list(c(84)),
                   list(c(1631,1603,1603,1477,1477,1372,1372,1329,1295,1295,1288,1281,1239,1141,1043,1001,
                          931,931,903,868,784,756,756,756,644,623,518,518,511,259)),
                   list(c(1421,1393,1309,1274,1274,1232,1148,1127,1120,1057,1036,938,903,805,707,420,315,224,147,1379,1417)),
                   list(c(1411,1417,1372,1358,1253,1134,1064,1064,1036,959,910,756,553,371,329,329,329,266,49)),
                   list(c(1610,1610,1610,1484,1435,1393,1365,1267,1267,1246,1225,1204,1078,1071,1015,735,735,693,623,623,553,518,399)),
                   list(c(1740,1743,1842,1785,1708,1463,1393,1204,1162,1141,1043,980,812,791,791,791,791,791,756,476,392)),
                   list(c(158,1330,1302,1302,1302,1239,1148,1099,1008,938,875,875,812,770,203,203,189,91)),
                   list(c(1211,1134,1134,833,252,168)),
                   list(c(1652,1540,1281,1176,1162,1148,1113,805,805,420,420,280,147,70)),
                   list(c(1617,1491,1379,1092,1036,1029,938,938,938,924,924,924,693,588,420,399,364,301,252)),
                   list(c(167,273,321,368,406,494,537,547,627,627)),
                   list(c(329,372,665,728,784)), list(c(0)))
  replace_idx2 <- c(rep(list(c(0)), 20), list(c(75)), list(c(0)), list(c(0)), list(c(246)),
                    list(c(83,105)), list(c(0)), list(c(0)), list(c(35)), list(c(0)), list(c(6)), list(c(54)))
  replacement2 <- c(rep(list(c(0)), 20), list(c("18 November 1902")), list(c(0)), list(c(0)), list(c("11 February 1911")),
                    list(c("27 March 1920", "19 March 1919")), list(c(0)), list(c(0)), list(c("26 April 1927")),
                    list(c(0)), list(c("17 June 1935")), list(c("10 June 1942")))
  remove_table <- c(list(c(1,32,45,51,90,93,164,211,232,241,244,267,300,307,314,331,336,391,427,470,
                           484,509,514,535,591,618,677)), 
                    list(c(1,32,89,90,138,164,211,232,241,267,300,307,314,336,391,427,428,470,484,509,
                           514,535,591,618,677)), 
                    list(c(1,32,89,92,158,161,208,229,238,264,297,304,311,328,333,388,424,462,476,501,
                           506,528,561,584,611,676)),
                    list(c(1,32,88,89,135,161,208,229,238,264,297,304,311,333,388,424,425,
                           462,476,494,501,506,528,584,611,676)), 
                    list(c(1,32,89,92,158,161,208,229,238,264,297,304,311,328,333,388,424,462,
                           476, 501,506,528,584,611,676)), 
                    list(c(1,32,88,89,135,161,208,229,238,264,297,304,311,333,388,424,425,462,476,
                           501,506,522,528,584,611,630,676)), 
                    list(c(1,2,33,34,91,92,95,161,164,165,211,212,233,234,243,244,270,271,
                           310,311,318,319,336,341,342,397,398,434,435,473,474,488,489,514,515,519,
                           520,542,543,599,600,627,628,633,646,693,694)), 
                    list(c(1,2,32,33,89,90,91,99,135,141,162,163,209,210,218,231,232,241,242,266,267,
                           308,309,317,332,338,339,385,394,395,432,433,434,445,470,471,485,486,511,
                           512,517,518,541,542,598,599,626,627,690,691,694)), 
                    list(c(1,2,32,33,90,91,94,99,141,159,162,163,209,210,218,231,232,241,242,266,267,
                           308,309,316,317,332,333,338,339,394,395,432,433,445,470,471,485,486,511,
                           512,517,518,541,542,598,599,626,627,690,691,694)), 
                    list(c(1,2,32,33,89,90,91,135,141,162,163,209,210,218,231,232,241,242,266,267,
                           308,309,316,317,332,338,339,394,395,432,433,434,445,470,471,485,486,511,
                           512,517,518,541,542,598,599,626,627,690,691,694)))
  ext_by_table <- c("//table[@class = 'wikitable'][1]","//table[@class = 'wikitable'][2]","//table[@class = 'wikitable'][1]","//table[@class = 'wikitable'][2]","//table[@class = 'wikitable'][3]",
                    "//table[@class = 'wikitable'][1]","//table[@class = 'wikitable'][2]","//table[@class = 'wikitable'][3]","//table[@class = 'wikitable'][4]","//table[@class = 'wikitable'][5]",
                    "//table[@class = 'wikitable'][1]","//table[@class = 'wikitable'][1]","//table[@class = 'wikitable'][1]","//table[@class = 'wikitable'][1]","//table[@class = 'wikitable'][1]",
                    "//table[@class = 'wikitable'][1]","//table[@class = 'wikitable'][1]","//table[@class = 'wikitable'][1]","//table[@class = 'wikitable'][1]","//table[@class = 'wikitable'][1]")
  cut_by_table <- c(list(c(1:71)),list(c(1:219)),list(c(1:54)),list(c(1:214)),list(c(1:211)),
                    list(c(1:67)),list(c(1:178)),list(c(1:165)),list(c(1:61)),list(c(1:64)),
                    list(c(444:502)),list(c(3:221)),list(c(371:460)),list(c(396:573)),list(c(199:393)),
                    list(c(3:196)),list(c(407:444)),list(c(225:403)),list(c(119:221)),list(c(3:115)))
  col_by_table_1 <- c(rep(5, 10), rep(6, 10))
  col_by_table_2 <- c(rep(0, 10), rep(8, 10))
  col_by_table_3 <- c(rep(2, 10), rep(1, 10))
  col_by_table_4 <- c(rep(1, 10), rep(2, 10))
  for (i in 1:length(htmls)) {
    if (i %in% c(11:52)) {
      # read html file
      htmls[[i]] <-  read_html(source[i], encoding = "UTF-8")
      # locate legislator biography URLs in html via XPath query, extract and complete URL
      urls[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
        html_attr("href") %>%
        str_c("https://", "en",".wikipedia.org", .)
      if (i == 15) {
        urls[[i]] <- c(urls[[i]][1:368], "", urls[[i]][369:651])
      }
      if (i == 12) {
        urls[[i]] <- c(urls[[i]][1:302], "", urls[[i]][303:653])
      }
      # locate legislator names in html via query, extract names
      names[[i]] <- html_nodes(x = htmls[[i]], xpath = query[i]) %>%
        html_text()
      if (i == 15) {
        names[[i]] <- c(names[[i]][1:368], "George Errington", names[[i]][369:651])
      }
      if (i == 12) {
        names[[i]] <- c(names[[i]][1:302], "John Green", names[[i]][303:653])
      }
      # locate legislator party affiliations in html via query_party, extract and format party affiliation
      parties[[i]] <- html_nodes(x = htmls[[i]], xpath = query_party[i]) %>%
        html_text() %>%
        str_replace_all("\\r|\\n", "")
      constituencies[[i]] <- html_nodes(x = htmls[[i]], xpath = query_constituency[i]) %>%
        html_text() %>%
        str_replace_all("\\r|\\n", "")
      if (i %in% c(1:31)) {
        services[[i]] <- rep(NA, length(urls[[i]]))
        session_start[[i]] <- duration[[i]][1]
        session_end[[i]] <- duration[[i]][2]
      } else {
        session_start[[i]] <- duration[[i]][1]
        session_end[[i]] <- duration[[i]][2]
        services[[i]] <- round(rep(difftime(time1 = session_end[[i]][1], time2 = session_start[[i]][1], units = "days"),
                                   length(urls[[i]])))
        services[[i]] <- replace(services[[i]], replace_idx[[i]], replacement[[i]])
      }
      if (i %in% c(12:23,25:32)) {# 31:38
        if (i == 27) {
          constituencies[[i]] <- c(constituencies[[i]][1:105], "Cambridge", constituencies[[i]][106:594])
        }
        multipliers1[[i]] <- html_nodes(x = htmls[[i]], xpath = query_multiplier[i]) %>%
          html_attr("rowspan") %>%
          as.numeric() %>%
          na.replace(1)
        constituencies[[i]] <- rep(constituencies[[i]], times = multipliers1[[i]])
      }
    }
    if (i %in% c(1:10)) {
      html_table2 <- trace(rvest:::html_table.xml_node, quote({ 
        values      <- lapply(lapply(cells, html_node, "a"), html_attr, name = "href")
        values[[1]] <- html_text(cells[[1]])
      }), at = 14)
      htmls[[i]] <-  read_html(source[i], encoding = "UTF-8")
      table_a <- html_nodes(x = htmls[[i]], xpath = "//table[@class = 'wikitable']") %>%
        html_table(fill = TRUE) %>%
        extract2(1)
      table_a <- table_a[-remove_table[[i]],]
      urls[[i]] <- table_a$X2 %>%
        str_c("https://", "en",".wikipedia.org", .)
      untrace(rvest:::html_table.xml_node)
      table_b <- html_nodes(x = htmls[[i]], xpath = "//table[@class = 'wikitable']") %>%
        html_table(fill = TRUE) %>%
        extract2(1)
      table_b <- table_b[-remove_table[[i]],]
      names[[i]] <- table_b$X2 %>%
        str_replace_all("\\[.+| died.+| Died.+| appointed.+| Appointed.+|seat Vacant|seat vacant| elected.+| resigned.+| Resigned.+| became.+| void.+|  Void.+| sat for.+| Sat for.+| election.+| Election.+| 18.+| took office.+| raised to.+| unseated on.+|( )?Replaced.+| succeeded.+| expelled.+| ennobled.+| Ennobled.+| Expelled.+| assassinated.+| –", "") %>%
        str_trim
      parties[[i]] <- table_b$X3
      constituencies[[i]] <- table_b$X1 %>%
        str_replace_all("\\(two.+|\\(three.+|\\(four.+|\\(seat.+", "") %>%
        str_trim
      services[[i]] <- rep(NA, length(urls[[i]]))
      session_start[[i]] <- duration[[i]][1]
      session_end[[i]] <- duration[[i]][2]
    }
    if (i %in% c(1:2)) {
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections14.html")
    }
    if (i %in% c(3:5)) {
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections13.html")
    }
    if (i %in% c(6:10)) {
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections12.html")
    }
    if (i %in% c(11)) {
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections11.html")
    }
    if (i %in% c(12)) {
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections10.html")
    }
    if (i %in% c(13)) {
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections9.html")
    }
    if (i %in% c(14:16)) {
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections8.html")
    }
    if (i %in% c(17:20)) {
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections7.html")
    }
    if (i %in% c(21:24)) {
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections6.html")
    }
    if (i %in% c(25:29)) {# 31-35
      if (i == 25) {
        urls[[i]] <- urls[[i]][-168]
        names[[i]] <- names[[i]][-168]
        services[[i]] <- services[[i]][-168]
      }
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections1.html")
    }
    # add legislators elected at by-elections
    if (i %in% c(30:32)) {#38
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections2.html")
    }
    if (i %in% c(33:41)) {#39-
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections3.html")
    }
    if (i %in% c(42:48)) {#48-
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections4.html")
    }
    if (i %in% c(49:50)) {#48-
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections5.html")
    }
    if (i %in% c(51)) {#48-
      by_elections <- read_html("./data/htmls/uk_by_elections/by_elections15.html")
    }
    if (i %in% c(21:52)) {
      be_urls <- html_nodes(x = by_elections, xpath = query_be[i]) %>%
        html_attr("href") %>%
        str_c("https://", "en",".wikipedia.org", .)
      be_names <- html_nodes(x = by_elections, xpath = query_be[i]) %>%
        html_text()
      # locate legislator party affiliations in html via query_party, extract and format party affiliation
      be_parties <- html_nodes(x = by_elections, xpath = query_be_party[i]) %>%
        html_text() %>%
        # remove the two additionals, for 37
        str_replace_all("\\r|\\n", "")
      if (i %in% 31) {
        be_parties <- be_parties[-c(57,194)]
      }
      be_constituencies <- html_nodes(x = by_elections, xpath = query_be_constituency[i]) %>%
        html_text() %>%
        str_replace_all("\\r|\\n", "")
      if (i %in% 31) {
        be_constituencies <- be_constituencies[-c(57,194)]
      }
      # remove the two additionals, for 37
      be_services <- html_nodes(x = by_elections, xpath = query_be_service[i]) %>%
        html_text()
      if (i %in% c(21,24,25,28,30,31)) {
        be_services <- replace(be_services, replace_idx2[[i]], replacement2[[i]])
      }
      be_services <- be_services %>%
        str_extract("^.+[[:digit:]]{4}") %>%
        dmy() %>%
        difftime(time1 = duration[[i]][2], time2 = .)
      if (i %in% 31) {
        be_services <- be_services[-c(57,194)]
      }
    } else {
      html_table2 <- trace(rvest:::html_table.xml_node, quote({ 
        values      <- lapply(lapply(cells, html_node, "a"), html_attr, name = "href")
        values[[1]] <- html_text(cells[[1]])
      }), at = 14)
      table_a <- html_nodes(x = by_elections, xpath = ext_by_table[i]) %>%
        html_table(fill = TRUE) %>%
        extract2(1)
      table_a <- table_a[cut_by_table[[i]],]
      be_urls <- table_a[,col_by_table_1[i]] %>%
        str_c("https://", "en",".wikipedia.org", .)
      untrace(rvest:::html_table.xml_node)
      table_b <- html_nodes(x = by_elections, xpath = ext_by_table[i]) %>%
        html_table(fill = TRUE) %>%
        extract2(1)
      table_b <- table_b[cut_by_table[[i]],]
      be_names <- table_b[,col_by_table_1[i]] %>%
        str_trim
      if (i %in% c(11:20)) {
        be_parties <- table_b[,col_by_table_2[i]] 
      } else {
        be_parties <- rep(NA, length(be_urls))
      }
      be_constituencies <- table_b[,col_by_table_3[i]]  %>%
        str_replace_all("[[:digit:]]", "") %>%
        str_trim
      be_services <- table_b[,col_by_table_4[i]] %>%
        str_extract("^.+ [[:digit:]]{4}") %>%
        str_replace_all("–[[:digit:]] |–[[:digit:]][[:digit:]] ", "") %>%
        dmy() %>%
        difftime(time1 = duration[[i]][2], time2 = .)
    }
    if (!(i %in% 52)) {
      urls[[i]] <- c(urls[[i]], be_urls)
      names[[i]] <- c(names[[i]], be_names)
      parties[[i]] <- c(parties[[i]], be_parties)
      constituencies[[i]] <- c(constituencies[[i]], be_constituencies)
      services[[i]] <- c(services[[i]], be_services)
    }
  }
  urls <- lapply(urls, data.table) %>%
    lapply(rename, url = V1)
  names <- lapply(names, data.table) %>%
    lapply(rename, name = V1)
  parties <- lapply(parties, data.table) %>%
    lapply(rename, party = V1)
  constituencies <- lapply(constituencies, data.table) %>%
    lapply(rename, constituency = V1)
  services <- lapply(services, data.table) %>%
    lapply(rename, service = V1)
  session_start <- lapply(session_start, data.table) %>%
    lapply(rename, session_start = V1)
  session_end <- lapply(session_end, data.table) %>%
    lapply(rename, session_end = V1)
  urls <- mapply(cbind, urls, names, parties, constituencies, 
                 services, session_start, 
                 session_end, SIMPLIFY = FALSE)
  urls <-  c(1:11,16,17,20:58) %>%
    mapply(function(urls, x) mutate(urls, term = x), urls, ., SIMPLIFY=FALSE) %>%
    mapply(function(., x) mutate(., country = x), ., rep("GBR", times = 52), SIMPLIFY=FALSE)
}


#### SPAIN WIKIPEDIA INFORMATION EXTRACTION =============================================
collectorSpain <- function(source) {
  source <- mixedsort(list.files(source, full.names = TRUE), decreasing = TRUE)
  for (i in 1:length(source)) {
    subsource <- mixedsort(list.files(source[i], full.names = TRUE), decreasing = FALSE)
    cat("|")
    for (j in 1:length(subsource)) {
      urls <- read_html(subsource[j]) %>%
        html_nodes(xpath = "//div[@class = 'mw-category']//ul/li/a") %>%
        html_attr("href")
      names <- read_html(subsource[j]) %>%
        html_nodes(xpath = "//div[@class = 'mw-category']//ul/li/a") %>%
        html_text()
      if (j == 1) {
        urls_all <- urls
        names_all <- names
      } else {
        urls_all <- c(urls_all, urls)
        names_all <- c(names_all, names)
      }
      cat(".")
    }
    if (i %in% c(2:5)) {
      urls_all <- urls_all[-1]
      names_all <- names_all[-1]
    }
    if (i == 1) {
      output <- list(data.frame(name = names_all, url = urls_all,
                                stringsAsFactors = FALSE))
    } else {
      output <- c(output,
                  list(data.frame(name = names_all, url = urls_all,
                                  stringsAsFactors = FALSE)))
    }
  }
  return(output)
}

#### SPAIN OFFICIAL INFORMATION EXTRACTION ==============================================
collectorSpainOfficial <- function(source) {
  source <- mixedsort(list.files(source, full.names = TRUE), decreasing = TRUE)
  for (i in 1:length(source)) {
    subsource <- mixedsort(list.files(source[i], full.names = TRUE), decreasing = TRUE)
    hull <- data.frame(name = rep(NA, length(subsource)), 
                       party = rep(NA, length(subsource)),
                       group = rep(NA, length(subsource)),
                       constituency = rep(NA, length(subsource)),
                       start_time = rep(NA, length(subsource)),
                       end_time = rep(NA, length(subsource)),
                       portrait = rep(NA, length(subsource))) 
    cat("|")
    for (j in 1:length(subsource)) {
      parsed <- read_html(subsource[j])
      hull$name[j] <- html_node(parsed, xpath = "//div[@class = 'nombre_dip']") %>%
        html_text()
      hull$party[j] <- html_node(parsed, xpath = "//p[@class = 'nombre_grupo']") %>%
        html_text()
      hull$group[j] <- html_node(parsed, xpath = "//div[@class = 'dip_rojo']/a") %>%
        html_text()
      hull$constituency[j] <- html_node(parsed, xpath = "//div[@class = 'dip_rojo'][1]") %>%
        html_text() %>%
        str_replace_all("\\\r|\\\n|\\.", "") %>%
        str_squish()
      if (i %in% c(1,2)) {
        hull$start_time[j] <-  html_node(parsed, xpath = "//div[@class = 'texto_dip'][2]/ul/li//div[contains(text(), 'Condici')]") %>%
          html_text()
      } else {
        hull$start_time[j] <-  html_node(parsed, xpath = "//div[@class = 'texto_dip'][2]/ul/li//div[contains(text(), 'Fecha')]") %>%
          html_text()
      }
      hull$end_time[j] <- html_node(parsed, xpath = "//div[@class = 'texto_dip'][2]/ul/li//div[contains(text(), 'Caus')]") %>%
        html_text()
      hull$portrait[j] <- html_node(parsed, xpath = "//img[@name = 'foto']") %>%
        html_attr(name = "src") %>%
        str_c("http://www.congreso.es", .)
      cat(".")
    }
    if (i == 1) {
      output <- list(hull) 
    } else {
      output <- c(output, list(hull))
    }
  }
  return(output)
}
