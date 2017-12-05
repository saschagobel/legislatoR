#' Legislators IDs data.
#'
#' Fetches a range of IDs of legislators for the specified legislature from the GitHub repository. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the legislature for which data shall be fetched from the GitHub repository. Currently one of \sQuote{austria}, \sQuote{france}, \sQuote{germany}, \sQuote{ireland}, \sQuote{ushouse}, or \sQuote{ussenate}.
#' @return A data frame with columns as specified above.
#' @format Data frame with columns (might vary by legislature):
#' \itemize{
#' \item{wikidataid: Wikidata ID identifying a legislator's Wikidata entry (of class \sQuote{character}).}
#' \item{parlid: Parliamentary website ID  (of class \sQuote{character}).}
#' \item{sycomore: Sycomore database of French MPs (of class \sQuote{character}).}
#' \item{libcon: Library of Congress ID (of class \sQuote{character}).}
#' \item{gnd: German National Library ID (of class \sQuote{character}).}
#' \item{bnf: French National Library ID (of class \sQuote{character}).}
#' \item{freebase: Freebase ID (of class \sQuote{character}).}
#' \item{munzinger:  Munzinger archive ID (of class \sQuote{character}).}
#' \item{nndb: Notable Names Database ID (of class \sQuote{character}).}
#' \item{imdb: Internet Movie Database ID (of class \sQuote{character}).}
#' \item{brittanica: Encyclopedia Brittanica ID (of class \sQuote{character}).}
#' \item{quora: Quora ID (of class \sQuote{character}).}
#' \item{votesmart: Project Votesmart ID (of class \sQuote{character}).}
#' \item{fec: Federal Election Commission ID (of class \sQuote{character}).}
#' \item{ballotpedia: Ballotpedia ID (of class \sQuote{character}).}
#' \item{opensecrets: Opensecrets ID (of class \sQuote{character}).}
#' \item{genealogists: Genealogists ID (of class \sQuote{character}).}
#' \item{politfacts: Politfacts ID (of class \sQuote{character}).}
#' }
#' @examples
#' \dontrun{
#' ## load data into memory
#' at <- austria
#'
#' ## load only data for members of the 12th legislative period into memory
#' at_12 <- austria[austria$session == 12,]
#'
#' ## load only birth dates for members of the political party 'SdP' into memory
#' at_sdp_birth <- austria[austria$party == "SdP", "birthdate"]
#'
#' ## add traffic data to at_12 (requires package 'dplyr')
#' at_12_traffic <- left_join(x = at_12, y = austria_traffic, by = "pageid")
#'
#' ## add revision history data to at_12 (requires package 'dplyr')
#' at_12_history <- left_join(x = at_12, y = austria_history, by = "pageid")
#'
#' ## retrieve locations from birthplace coordinates (requires package 'ggmap')
#' birth_lat <- str_split(austria_birthplace$birthplace, pattern = ",") %>%
#'   lapply(., `[`, 1) %>%
#'   unlist %>%
#'   as.numeric
#' birth_lon <- str_split(austria_birthplace$birthplace, pattern = ",") %>%
#'   lapply(., `[`, 2) %>%
#'   unlist %>%
#'   as.numeric
#' birth_location <- rep(NA, times = length(birth_lat))
#' for (i in 1:length(birth_lat)) {
#'   birth_location[i] <- revgeocode(c(birth_lon[i], birth_lat[i]))
#'   Sys.sleep(4)
#' }
#' }
#' @source
#' Wikipedia API, \url{https://de.wikipedia.org/w/api.php}
#' @export
#' @importFrom curl nslookup
get_ids <- function(legislature) {
  if (!(legislature %in% c("austria", "france", "germany", "ireland", "usah", "usas")))
    stop ("legislatoR does not contain data for this legislature at the moment. Please try one of 'austria', 'france', 'germany', 'ireland', 'usah', or 'usas'.")
  if (is.null(curl::nslookup("www.github.com", error = FALSE)))
    stop ("legislatoR failed to establish a connection to GitHub. Please check your Internet connection and whether GitHub is online.")
  ghurl <- base::paste0("https://github.com/saschagobel/legislatoR/blob/master/data-raw/", legislature, "_ids?raw=true")
  connect <- base::url(ghurl)
  on.exit(close(connect))
  dataset <- base::readRDS(connect)
  return(dataset)
}
