#' Fetch 'Professions' table
#'
#' Fetches occupational data of legislators for the specified legislature. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the three-letter country code of the legislature for which data shall be fetched. Currently one of \sQuote{aut}, \sQuote{can}, \sQuote{cze}, \sQuote{esp}, \sQuote{fra}, \sQuote{deu}, \sQuote{irl}, \sQuote{sco}, \sQuote{gbr}, \sQuote{usa_house}, or \sQuote{usa_senate}.
#' @return A data frame with columns as specified above.
#' @format Data frame in wide format with columns (varies by legislature):
#' \itemize{
#' \item{wikidataid: Wikidata ID identifying a legislator's Wikidata entry (of class \sQuote{character}).}
#' \item{occupation_1: occupation a legislator practiced or was trained in (of class \sQuote{logical}).}
#' \item{occupation_2: ... (of class \sQuote{logical}).}
#' \item{...}
#' }
#' @examples
#' \donttest{# Get entire 'Professions' table for the United States House
#' usa_offices <- get_office(legislature = "usa_house")
#' tibble::glimpse(usa_offices)
#'
#' # Get 'Professions' table for female members of the United States House
#' usa_offices_subset <- dplyr::semi_join(x = usa_offices,
#'                                        y = dplyr::filter(get_core(legislature = "usa_house"),
#'                                                          sex == "female"),
#'                                        by = "wikidataid")
#' tibble::glimpse(usa_offices_subset)
#' }
#' @source
#' Wikidata API, \url{https://www.wikidata.org/wiki/Wikidata:Main_Page}
#' @export
#' @importFrom curl nslookup
#' @import dplyr
get_profession <- function(legislature) {
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
  file_id <- sysdata %>% filter(.data$table == "profession" & .data$country == legislature)
  dvurl <- paste0(endpoint, file_id$id)
  connect <- url(dvurl)
  on.exit(close(connect))
  dataset <- readRDS(connect)
  return(dataset)
}
