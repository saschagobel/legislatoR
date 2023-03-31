#' Fetch Wikipedia 'Traffic' table
#'
#' Fetches daily user traffic on legislators' Wikipedia biographies for the specified legislature. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the three-letter country code of the legislature for which data shall be fetched. Currently one of \sQuote{aut}, \sQuote{can}, \sQuote{cze}, \sQuote{esp}, \sQuote{fra}, \sQuote{deu}, \sQuote{irl}, \sQuote{sco}, \sQuote{gbr}, \sQuote{usa_house}, or \sQuote{usa_senate}.
#' @return A data frame with columns as specified above.
#' @format Data frame with columns:
#' \itemize{
#' \item{pageid: Wikipedia page ID identifying a legislator's Wikipedia biography (of class \sQuote{integer}).}
#' \item{date: Date for which user traffic is recorded, from 2015-07-01 to 2018-12-31 UTC (of class \sQuote{POSIXct}).}
#' \item{traffic: Daily non-unique user visits (of class \sQuote{numeric}).}
#' }
#' @examples
#' \donttest{# Get entire 'Traffic' table for the Scottish Parliament
#' sco_traffic <- get_traffic(legislature = "sco")
#' tibble::glimpse(sco_traffic)
#'
#' # Add Wikidataid to 'Traffic' table for the Scottish Parliament
#' sco_traffic_subset <- dplyr::inner_join(x = dplyr::select(get_core(legislature = "sco"),
#'                                                           pageid, wikidataid),
#'                                         y = sco_traffic,
#'                                         by = "pageid")
#' tibble::glimpse(sco_traffic_subset)
#' }
#' @source
#' Wikimedia API, \url{https://wikimedia.org/api/rest_v1/} \cr
#' \url{http://petermeissner.de:8880/}
#' @export
#' @importFrom curl nslookup
#' @import dplyr
get_traffic <- function(legislature) {
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
  file_id <- sysdata %>% filter(.data$table == "traffic" & .data$country == legislature)
  dvurl <- paste0(endpoint, file_id$id)
  connect <- url(dvurl)
  on.exit(close(connect))
  dataset <- readRDS(connect)
  return(dataset)
}
