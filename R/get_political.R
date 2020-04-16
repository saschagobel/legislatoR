#' Fetch 'Political' table
#'
#' Fetches political data of legislators for the specified legislature. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the three-letter country code of the legislature for which data shall be fetched. Currently one of \sQuote{aut}, \sQuote{can}, \sQuote{cze}, \sQuote{esp}, \sQuote{fra}, \sQuote{deu}, \sQuote{irl}, \sQuote{sco}, \sQuote{gbr}, \sQuote{usa_house}, or \sQuote{usa_senate}.
#' @return A data frame with columns as specified above.
#' @format Data frame in long format with columns (varies by legislature):
#' \itemize{
#' \item{pageid: Wikipedia page ID identifying a legislator's Wikipedia biography (of class \sQuote{integer}).}
#' \item{session: Legislative period (of class \sQuote{integer}).}
#' \item{party: A legislator's party affiliation (of class \sQuote{character}). See \url{https://github.com/saschagobel/legislatoR} for the full form of abbreviated party names and english translations of non-english party names}
#' \item{constituency: A legislator's constituency (of class \sQuote{character}).}
#' \item{constituency2: A legislator's constituency (upper level, if applicable, of class \sQuote{character}).}
#' \item{constituency_id: ID of a legislator's constituency (of class \sQuote{character}).}
#' \item{session_start: Date the legislative period started (of class \sQuote{Date}).}
#' \item{session_end: Date the legislative period ended (of class \sQuote{Date}).}
#' \item{service: A legislator's period of service in days during the respective session (of class \sQuote{integer})}
#' \item{government (or similar): Indicator of a legislator's majority status in parliament (of class \sQuote{logical}). Further columns with extensions of this might exist.}
#' \item{leader (or similar): Indicator of a legislator's leader status in parliament (of class \sQuote{logical}). Further columns with extensions of this might exist.}
#' }
#' @examples
#' \donttest{# Get entire 'Political' table for the Czech Poslanecka Snemovna
#' cze_political <- get_political(legislature = "cze")
#' tibble::glimpse(cze_political)
#'
#' # Get 'Political' table for female DSP party members of the Czech Poslanecka Snemovna
#' cze_political_subset <- dplyr::semi_join(x = dplyr::filter(cze_political,
#'                                                            party == "ODS"),
#'                                          y = dplyr::filter(get_core(legislature = "cze"),
#'                                                            sex == "female"),
#'                                          by = "pageid")
#' tibble::glimpse(cze_political_subset)
#' }
#' @source
#' Wikipedia, \url{https://wikipedia.org/} \cr
#' Czech Republic Parliamentary Members Archive \url{http://public.psp.cz/sqw/fsnem.sqw?zvo=1} \cr
#' Spain Parliamentary Members Archive \url{http://www.congreso.es/portal/page/portal/Congreso/Congreso/Diputados}
#' @export
#' @importFrom curl nslookup
#' @import dplyr
get_political <- function(legislature) {
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
  ghurl <- paste0("https://github.com/saschagobel/legislatoR-data/blob/master/data/", legislature, "_political?raw=true")
  connect <- url(ghurl)
  on.exit(close(connect))
  dataset <- readRDS(connect)
  return(dataset)
}
