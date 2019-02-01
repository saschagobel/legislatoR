#' Fetch 'Portrait' dataset
#'
#' Fetches portrait urls of legislators for the specified legislature. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the legislature for which data shall be fetched. Currently one of \sQuote{aut}, \sQuote{can}, \sQuote{cze}, \sQuote{fra}, \sQuote{deu}, \sQuote{irl}, \sQuote{sco}, \sQuote{gbr}, \sQuote{usa_house}, or \sQuote{usa_senate}.
#' @return A data frame with columns as specified above.
#' @format Data frame with columns:
#' \itemize{
#' \item{pageid: Wikipedia page ID identifying a legislator's Wikipedia biography (of class \sQuote{integer}).}
#' \item{image_url: URL linking to a legislator's portrait on Wikimedia Commons (of class \sQuote{character}).}
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
#' Wikipedia API, \url{https://wikipedia.org/w/api.php} \cr
#' Wikimedia Commons, \url{https://commons.wikimedia.org/}
#' @export
#' @importFrom curl nslookup
#' @import dplyr
get_portrait <- function(legislature) {
  if (!(legislature %in% c("aut", "can", "cze", "fra", "deu", "irl", "sco", "gbr", "usa_house", "usa_senate")))
    stop ("legislatoR does not contain data for this legislature at the moment. Please try one of 'aut', 'can', 'cze', 'fra', 'deu', 'irl', 'usa_house', or 'usa_senate'.")
  if (is.null(curl::nslookup("www.github.com", error = FALSE)))
    stop ("legislatoR failed to establish a connection to GitHub. Please check your Internet connection and whether GitHub is online.")
  ghurl <- base::paste0("https://github.com/saschagobel/legislatoR-data/blob/master/data/", legislature, "_portrait?raw=true")
  connect <- base::url(ghurl)
  on.exit(close(connect))
  dataset <- base::readRDS(connect)
  return(dataset)
}
