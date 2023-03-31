#' Fetch 'Core' table
#'
#' Fetches sociodemographic data of legislators for the specified legislature. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the three-letter country code of the legislature for which data shall be fetched. Currently one of \sQuote{aut}, \sQuote{can}, \sQuote{cze}, \sQuote{esp}, \sQuote{fra}, \sQuote{deu}, \sQuote{irl}, \sQuote{sco}, \sQuote{gbr}, \sQuote{usa_house}, or \sQuote{usa_senate}.
#' @return A data frame with columns as specified above.
#' @format Data frame with columns (varies by legislature):
#' \itemize{
#' \item{country: ISO 3166-1 alpha-3 three-letter country code (of class \sQuote{character}).}
#' \item{pageid: Wikipedia page ID identifying a legislator's Wikipedia biography (of class \sQuote{integer} or \sQuote{character}).}
#' \item{wikidataid: Wikidata ID identifying a legislator's Wikidata entry (of class \sQuote{character}).}
#' \item{wikititle: A legislator's undirected Wikipedia title (of class \sQuote{character}).}
#' \item{name: A legislator's full name (of class \sQuote{character}).}
#' \item{sex: A legislator's sex (of class \sQuote{character}).}
#' \item{ethnicity: A legislator's ethnicity (of class \sQuote{character}).}
#' \item{religion: A legislator's religious denomination (of class \sQuote{character}).}
#' \item{birth: A legislator's date of birth (of class \sQuote{POSIXct}).}
#' \item{death: A legislator's date of death (of class \sQuote{POSIXct}).}
#' \item{birthplace: Comma separated latitude and longitude of a legislator's place of birth (of class \sQuote{character}).}
#' \item{deathplace: Comma separated latitude and longitude of a legislator's place of death (of class \sQuote{character}).}
#' }
#' @examples
#' \donttest{# Get entire 'Core' table for the German Bundestag
#' deu_core <- get_core(legislature = "deu")
#' tibble::glimpse(deu_core)
#'
#' # Get 'Core' table for 16th session of the German Bundestag
#' deu_core_subset <- dplyr::semi_join(x = deu_core,
#'                                     y = dplyr::filter(get_political(legislature = "deu"),
#'                                                       session == 16),
#'                                     by = "pageid")
#' tibble::glimpse(deu_core_subset)
#' }
#' @source
#' Wikipedia, \url{https://www.wikipedia.org/} \cr
#' Wikipedia API, \url{https://en.wikipedia.org/w/api.php} \cr
#' Wikidata API, \url{https://www.wikidata.org/wiki/Wikidata:Main_Page} \cr
#' Wikimedia Commons, \url{https://commons.wikimedia.org/wiki/Main_Page} \cr
#' Face++ Cognitive Services API, \url{https://www.faceplusplus.com/} \cr
#' Germany Bundestag Open Data, \url{https://www.bundestag.de/services/opendata}
#' @export
#' @importFrom curl nslookup
#' @import dplyr
get_core <- function(legislature) {
  if (length(legislature) > 1) {
    stop ("\n\nNo more than one legislature can be called at once. Please provide only one valid three-letter country code.")
  }
  if (!(legislature %in% c("aut", "bra", "can", "cze",
                           "deu", "esp", "fra", "gbr",
                           "irl", "isr", "ita_house", "ita_senate",
                           "jpn", "nld", "sco", "tur",
                           "usa_house", "usa_senate"))) {
    stop (paste0("\n\nPlease provide a valid three-letter country code. legislatoR does not recognize the country code or does not contain data for ",
                 paste0(
                   paste0("\"", legislature, "\""),
                   collapse = ", "),
                 ". Use `legislatoR::cld_content()` to see country codes of available legislatures."))
  }
  if (is.null(curl::nslookup("www.harvard.edu", error = FALSE))) {
    stop ("\n\nlegislatoR cannot establish a connection to Harvard Dataverse. Please check your Internet connection and whether Harvard Dataverse is online.")
  }
  endpoint <- "https://dataverse.harvard.edu/api/access/datafile/"
  file_id <- sysdata %>% filter(.data$table == "core" & .data$country == legislature)
  dvurl <- paste0(endpoint, file_id$id)
  connect <- url(dvurl)
  on.exit(close(connect))
  dataset <- readRDS(connect)
  return(dataset)
}
