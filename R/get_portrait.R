#' Fetch 'Portrait' table
#'
#' Fetches portrait urls of legislators for the specified legislature. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the three-letter country code of the legislature for which data shall be fetched. Currently one of \sQuote{aut}, \sQuote{can}, \sQuote{cze}, \sQuote{esp}, \sQuote{fra}, \sQuote{deu}, \sQuote{irl}, \sQuote{sco}, \sQuote{gbr}, \sQuote{usa_house}, or \sQuote{usa_senate}.
#' @return A data frame with columns as specified above.
#' @format Data frame with columns (varies by legislature):
#' \itemize{
#' \item{pageid: Wikipedia page ID identifying a legislator's Wikipedia biography (of class \sQuote{integer}).}
#' \item{image_url: URL linking to a legislator's portrait on Wikimedia Commons (of class \sQuote{character}).}
#' }
#' @examples
#' \donttest{# Get entire 'Portraits' table for the United States Senate
#' usa_portraits <- get_portrait(legislature = "usa_senate")
#' tibble::glimpse(usa_portraits)
#'
#' # Get 'Portraits' table for Democratic members of the United States Senate
#' usa_port_subset <- dplyr::semi_join(x = usa_portraits,
#'                                     y = dplyr::filter(get_political(legislature = "usa_senate"),
#'                                                                     party == "D"),
#'                                     by = "pageid")
#' tibble::glimpse(usa_port_subset)
#' }
#' @source
#' Wikipedia API, \url{https://en.wikipedia.org/w/api.php} \cr
#' Wikimedia Commons, \url{https://commons.wikimedia.org/wiki/Main_Page}
#' @export
#' @importFrom curl nslookup
#' @import dplyr
get_portrait <- function(legislature) {
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
  file_id <- sysdata %>% filter(.data$table == "portrait" & .data$country == legislature)
  dvurl <- paste0(endpoint, file_id$id)
  connect <- url(dvurl)
  on.exit(close(connect))
  dataset <- readRDS(connect)
  return(dataset)
}
