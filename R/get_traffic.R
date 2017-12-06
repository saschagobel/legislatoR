#' Legislators Wikipedia traffic data.
#'
#' Fetches daily user traffic on legislators' Wikipedia biographies for the specified legislature from the GitHub repository. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the legislature for which data shall be fetched from the GitHub repository. Currently one of \sQuote{austria}, \sQuote{france}, \sQuote{germany}, \sQuote{ireland}, \sQuote{ushouse}, or \sQuote{ussenate}.
#' @return A data frame with columns as specified above.
#' @format Data frame with columns (might vary by legislature):
#' \itemize{
#' \item{pageid: Wikipedia page ID identifying a legislator's Wikipedia biography (of class \sQuote{integer}).}
#' \item{date: Date for which user traffic is recorded, from 2015-07-01 to 2017-10-31 UTC (of class \sQuote{POSIXct}).}
#' \item{traffic: Daily non-unique user visits (of class \sQuote{numeric}).}
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
get_traffic <- function(legislature) {
  if (!(legislature %in% c("austria", "france", "germany", "ireland", "usah", "usas")))
    stop ("legislatoR does not contain data for this legislature at the moment. Please try one of 'austria', 'france', 'germany', 'ireland', 'usah', or 'usas'.")
  if (is.null(curl::nslookup("www.github.com", error = FALSE)))
    stop ("legislatoR failed to establish a connection to GitHub. Please check your Internet connection and whether GitHub is online.")
  ghurl <- base::paste0("https://github.com/saschagobel/legislatoR-data/blob/master/data/", legislature, "_traffic?raw=true")
  connect <- base::url(ghurl)
  on.exit(close(connect))
  dataset <- base::readRDS(connect)
  return(dataset)
}
