#' Fetch 'Core' dataset
#'
#' Fetches basic sociodemographic data for legislators of the specified legislature. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the legislature for which data shall be fetched. Currently one of \sQuote{aut}, \sQuote{can}, \sQuote{cze}, \sQuote{fra}, \sQuote{deu}, \sQuote{irl}, \sQuote{sco}, \sQuote{gbr}, \sQuote{usa_house}, or \sQuote{usa_senate}.
#' @return A data frame with columns as specified above.
#' @format Data frame with columns (might vary by legislature):
#' \itemize{
#' \item{country: ISO 3166-1 alpha-3 three-letter country code (of class \sQuote{character}).}
#' \item{pageid: Wikipedia page ID identifying a legislator's Wikipedia biography (of class \sQuote{integer}).}
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
#' \donttest{## assign entire core dataset into the environment
#' aut_politicians <- get_core(legislature = "aut")
#'
#' ## assign only data for the 12th legislative session into the environment
#' aut_politicians_subset <- dplyr::semi_join(x = get_core(legislature = "aut"),
#'                                            y = dplyr::filter(get_political(legislature = "aut"),
#'                                                                      session == 8),
#'                                            by = "pageid")
#'
#' ## join aut_politicians_subset with respective history dataset
#' aut_politicians_history <- dplyr::left_join(x = aut_politicians_subset,
#'                                             y = get_history(legislature = "aut"),
#'                                             by = "pageid")
#'
#' ## assign only birthdate for members of the political party 'SdP' into the environment
#' aut_birthdates_SdP <- dplyr::semi_join(x = dplyr::select(get_core(legislature = "aut"),
#'                                                          pageid, birth),
#'                                        y = dplyr::filter(get_political(legislature = "aut"),
#'                                                                        party == "SdP"),
#'                                        by = "pageid")$birth
#' }
#' @source
#' Wikipedia, \url{https://wikipedia.org/} \cr
#' Wikipedia API, \url{https://wikipedia.org/w/api.php} \cr
#' Wikidata API, \url{https://www.wikidata.org/} \cr
#' Wikimedia Commons, \url{https://commons.wikimedia.org/} \cr
#' Face++ Cognitive Services API, \url{https://www.faceplusplus.com/}
#' Germany Bundestag Open Data, \url{https://www.bundestag.de/service/opendata}
#' @export
#' @importFrom curl nslookup
#' @import dplyr
get_core <- function(legislature) {
  if (!(legislature %in% c("aut", "can", "cze", "fra", "deu", "irl", "sco", "gbr", "usa_house", "usa_senate")))
    stop ("legislatoR does not contain data for this legislature at the moment. Please try one of 'aut', 'can', 'cze', 'fra', 'deu', 'irl', 'usa_house', or 'usa_senate'.")
  if (is.null(curl::nslookup("www.github.com", error = FALSE)))
    stop ("legislatoR failed to establish a connection to GitHub. Please check your Internet connection and whether GitHub is online.")
  ghurl <- base::paste0("https://github.com/saschagobel/legislatoR-data/blob/master/data/", legislature, "_core?raw=true")
  connect <- base::url(ghurl)
  on.exit(close(connect))
  dataset <- base::readRDS(connect)
  return(dataset)
}
