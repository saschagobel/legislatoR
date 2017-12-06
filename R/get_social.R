#' Legislators social media data.
#'
#' Fetches social media handles and website URLs of legislators for the specified legislature from the GitHub repository. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the legislature for which data shall be fetched from the GitHub repository. Currently one of \sQuote{austria}, \sQuote{france}, \sQuote{germany}, \sQuote{ireland}, \sQuote{ushouse}, or \sQuote{ussenate}.
#' @return A data frame with columns as specified above.
#' @format Data frame with columns (might vary by legislature):
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
get_social <- function(legislature) {
  if (!(legislature %in% c("austria", "france", "germany", "ireland", "usah", "usas")))
    stop ("legislatoR does not contain data for this legislature at the moment. Please try one of 'austria', 'france', 'germany', 'ireland', 'usah', or 'usas'.")
  if (is.null(curl::nslookup("www.github.com", error = FALSE)))
    stop ("legislatoR failed to establish a connection to GitHub. Please check your Internet connection and whether GitHub is online.")
  ghurl <- base::paste0("https://github.com/saschagobel/legislatoR-data/blob/master/data/", legislature, "_social?raw=true")
  connect <- base::url(ghurl)
  on.exit(close(connect))
  dataset <- base::readRDS(connect)
  return(dataset)
}
