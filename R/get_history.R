#' Fetch Wikipedia 'History' table
#'
#' Fetches full revision histories of legislators' Wikipedia biographies for the specified legislature. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the three-letter country code of the legislature for which data shall be fetched. Currently one of \sQuote{aut}, \sQuote{can}, \sQuote{cze}, \sQuote{esp}, \sQuote{fra}, \sQuote{deu}, \sQuote{irl}, \sQuote{sco}, \sQuote{gbr}, \sQuote{usa_house}, or \sQuote{usa_senate}.
#' @return A data frame with columns as specified above.
#' @format Data frame with columns:
#' \itemize{
#' \item{pageid: Wikipedia page ID identifying a legislator's Wikipedia biography (of class \sQuote{integer}).}
#' \item{revid: Wikipedia edit ID (of class \sQuote{integer}).}
#' \item{parentid: Wikipedia edit ID of the previous revision (of class \sQuote{integer}).}
#' \item{user: Username of registered user responsible for the revision, IP address in case of anonymous revision (of class \sQuote{character}).}
#' \item{userid: ID of registered user responsible for the revision, 0 in case of anonymous revision (of class \sQuote{integer}).}
#' \item{timestamp: Date and time of the revision (of class \sQuote{POSIXct}).}
#' \item{size: Revision size in bytes (of class \sQuote{integer}).}
#' \item{comment: Revision comment (of class \sQuote{character}).}
#' }
#' @examples
#' \donttest{# Get entire 'History' table for the Austrian Nationalrat
#' aut_history <- get_history(legislature = "aut")
#' tibble::glimpse(aut_history)
#'
#' # Get 'History' table for NEOS party members of the Austrian Nationalrat
#' aut_history_subset <- dplyr::semi_join(x = aut_history,
#'                                        y = dplyr::filter(get_political(legislature = "aut"),
#'                                                          party == "NEOS"),
#'                                        by = "pageid")
#' tibble::glimpse(aut_history_subset)
#' }
#' @source
#' Wikipedia API, \url{https://wikipedia.org/w/api.php}
#' @export
#' @importFrom curl nslookup
#' @import dplyr
get_history <- function(legislature) {
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
  file_id <- sysdata %>% filter(.data$table == "history" & .data$country == legislature)
  dvurl <- paste0(endpoint, file_id$id)
  connect <- url(dvurl)
  on.exit(close(connect))
  dataset <- readRDS(connect)
  return(dataset)
}
