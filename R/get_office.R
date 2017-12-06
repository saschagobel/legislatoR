#' Legislators political and other offices data.
#'
#' Fetches political and other offices of legislators' for the specified legislature from the GitHub repository. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the legislature for which data shall be fetched from the GitHub repository. Currently one of \sQuote{austria}, \sQuote{france}, \sQuote{germany}, \sQuote{ireland}, \sQuote{usah}, or \sQuote{usas}.
#' @return A data frame with columns as specified above.
#' @format Data frame in wide format with columns (might vary by legislature):
#' \itemize{
#' \item{pageid: Wikipedia page ID identifying a legislator's Wikipedia biography (of class \sQuote{integer}).}
#' \item{office_1: political or other office held by a legislator (of class \sQuote{logical}).}
#' \item{office_2: ... (of class \sQuote{logical}).}
#' \item{...}
#' }
#' @examples
#' \donttest{## assign entire core dataset into the environment
#' at_elites <- get_core(legislature = "austria")
#'
#' ## assign only data for the 12th legislative session into the environment
#' at_elites_subset <- dplyr::semi_join(x = get_core(legislature = "austria"),
#'                                      y = dplyr::filter(get_political(legislature = "austria"),
#'                                                                      session == 8),
#'                                      by = "pageid")
#'
#' ## join at_elites_subset with respective history dataset
#' at_history <- dplyr::left_join(x = at_elites_subset,
#'                                y = get_history(legislature = "austria"),
#'                                by = "pageid")
#'
#' ## assign only birthdate for members of the political party 'SdP' into the environment
#' at_birthdates_SdP <- dplyr::semi_join(x = dplyr::select(get_core(legislature = "austria"),
#'                                                         pageid, birth),
#'                                       y = dplyr::filter(get_political(legislature = "austria"),
#'                                                                       party == "SdP"),
#'                                       by = "pageid")$birth
#' }
#' @source
#' Wikipedia API, \url{https://wikipedia.org/w/api.php}
#' @export
#' @importFrom curl nslookup
#' @import dplyr
get_office <- function(legislature) {
  if (!(legislature %in% c("austria", "france", "germany", "ireland", "usah", "usas")))
    stop ("legislatoR does not contain data for this legislature at the moment. Please try one of 'austria', 'france', 'germany', 'ireland', 'usah', or 'usas'.")
  if (is.null(curl::nslookup("www.github.com", error = FALSE)))
    stop ("legislatoR failed to establish a connection to GitHub. Please check your Internet connection and whether GitHub is online.")
  ghurl <- base::paste0("https://github.com/saschagobel/legislatoR-data/blob/master/data/", legislature, "_office?raw=true")
  connect <- base::url(ghurl)
  on.exit(close(connect))
  dataset <- base::readRDS(connect)
  return(dataset)
}
