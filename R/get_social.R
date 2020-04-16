#' Fetch 'Social' table
#'
#' Fetches social media handles and website URLs of legislators for the specified legislature. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the three-letter country code of the legislature for which data shall be fetched. Currently one of \sQuote{aut}, \sQuote{can}, \sQuote{cze}, \sQuote{esp}, \sQuote{fra}, \sQuote{deu}, \sQuote{irl}, \sQuote{sco}, \sQuote{gbr}, \sQuote{usa_house}, or \sQuote{usa_senate}.
#' @return A data frame with columns as specified above.
#' @format Data frame with columns (varies by legislature):
#' \itemize{
#' \item{wikidataid: Wikidata ID identifying a legislator's Wikidata entry (of class \sQuote{character}).}
#' \item{twitter: Twitter handle (of class \sQuote{character}).}
#' \item{facebook: Facebook handle (of class \sQuote{character}).}
#' \item{youtube: Youtube ID (of class \sQuote{character}).}
#' \item{googlep: Google Plus ID (of class \sQuote{character}).}
#' \item{instagram: Instagram handle (of class \sQuote{character}).}
#' \item{linkedin: LinkedIn ID (of class \sQuote{character}).}
#' \item{website: Personal website URL (of class \sQuote{character}).}
#' }
#' @examples
#' # Get entire 'Social' table forthe UK House of Commons
#' gbr_social <- get_social(legislature = "gbr")
#' tibble::glimpse(gbr_social)
#'
#' # Get 'Social' table for members of the UK House of Commons with available TheyWorkForYou ID
#' gbr_social_subset <- dplyr::semi_join(x = gbr_social,
#'                                       y = dplyr::filter(get_ids(legislature = "gbr"),
#'                                                         !is.na(theyworkforyou)),
#'                                       by = "wikidataid")
#' tibble::glimpse(gbr_social_subset)
#' @source
#' Wikidata API, \url{https://www.wikidata.org/}
#' @export
#' @importFrom curl nslookup
#' @import dplyr
get_social <- function(legislature) {
  if (length(legislature) > 1) {
    stop ("\n\nNo more than one legislature can be called at once. Please provide only one valid three-letter country code.")
  }
  if (!(legislature %in% c("aut", "can", "cze",
                           "esp", "fra", "deu",
                           "irl", "sco", "gbr",
                           "usa_house", "usa_senate"))) {
    stop (paste0("\n\nPlease provide a valid three-letter country code. legislatoR does not recognize the country code or does not contain data for ",
                 paste0(
                   paste0("\"", legislature, "\""),
                   collapse = ", "),
                 ". Use `legislatoR::cld_content()` to see country codes of available legislatures."))
  }
  if (is.null(curl::nslookup("www.github.com", error = FALSE))) {
    stop ("\n\nlegislatoR cannot establish a connection to GitHub. Please check your Internet connection and whether GitHub is online.")
  }
  ghurl <- paste0("https://github.com/saschagobel/legislatoR-data/blob/master/data/", legislature, "_social?raw=true")
  connect <- url(ghurl)
  on.exit(close(connect))
  dataset <- readRDS(connect)
  return(dataset)
}
