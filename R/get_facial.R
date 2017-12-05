#' Legislators facial data.
#'
#' Fetches facial data on legislators for the specified legislature from the GitHub repository. Requires a working Internet connection. Facial data are Face++ estimates based on portraits from Wikipedia Biographies.
#'
#' @param legislature A character string specifying the legislature for which data shall be fetched from the GitHub repository. Currently one of \sQuote{austria}, \sQuote{france}, \sQuote{germany}, \sQuote{ireland}, \sQuote{ushouse}, or \sQuote{ussenate}.
#' @return A data frame with columns as specified above.
#' @format Data frame with columns (might vary by legislature):
#' \itemize{
#' \item{pageid: Wikipedia page ID identifying a legislator's Wikipedia biography (of class \sQuote{integer}).}
#' \item{image_url: URL linking to a legislator's portrait on Wikimedia Commons (of class \sQuote{character}).}
#' \item{smile_intensity: Smile intensity. A smiling face can be confirmed if this number goes beyond the threshold value of 30.1. (of class \sQuote{numeric}).}
#' \item{emo_sadness: Sadness expressed, a bigger value indicates greater confidence of the emotion. The values of all emo variables is 100 (of class \sQuote{numeric}).}
#' \item{emo_neutral: Neutrality expressed, a bigger value indicates greater confidence of the emotion. The values of all emo variables is 100 (of class \sQuote{numeric}).}
#' \item{emo_disgust: Disgust expressed, a bigger value indicates greater confidence of the emotion. The values of all emo variables is 100 (of class \sQuote{numeric}).}
#' \item{emo_anger: Anger expressed, a bigger value indicates greater confidence of the emotion. The values of all emo variables is 100 (of class \sQuote{numeric}).}
#' \item{emo_surprise: Surprise expressed, a bigger value indicates greater confidence of the emotion. The values of all emo variables is 100 (of class \sQuote{numeric}).}
#' \item{emo_fear: Fear expressed, a bigger value indicates greater confidence of the emotion. The values of all emo variables is 100 (of class \sQuote{numeric}).}
#' \item{emo_happiness: Happiness expressed, a bigger value indicates greater confidence of the emotion. The values of all emo variables is 100 (of class \sQuote{numeric}).}
#' \item{beauty_female: A higher score indicates that the face is more beautiful from a female perspective (of class \sQuote{numeric}).}
#' \item{beauty_male: A higher score indicates that the face is more beautiful from a male perspective (of class \sQuote{numeric}).}
#' \item{skin_dark_circles: A higher score indicates greater confidence of dark circles (of class \sQuote{numeric}).}
#' \item{skin_stain: A higher score indicates greater confidence of spots (of class \sQuote{numeric}).}
#' \item{skin_acne: A higher score indicates greater confidence of acne (of class \sQuote{numeric}).}
#' \item{skin_health: A higher score indicates greater confidence of a healthy skin (of class \sQuote{numeric}).}
#' \item{image_quality: Suitability of the image quality for image comparison. Estimates are comparable if this number goes beyond the threshold value of 70.1 (of class \sQuote{numeric}).}
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
#' Wikipedia API, \url{https://de.wikipedia.org/w/api.php} \cr
#' Wikimedia Commons, \url{https://commons.wikimedia.org/} \cr
#' Face++ Cognitive Services API, \url{https://www.faceplusplus.com/}
#' @export
#' @importFrom curl nslookup
get_facial <- function(legislature) {
  if (!(legislature %in% c("austria", "france", "germany", "ireland", "usah", "usas")))
    stop ("legislatoR does not contain data for this legislature at the moment. Please try one of 'austria', 'france', 'germany', 'ireland', 'usah', or 'usas'.")
  if (is.null(curl::nslookup("www.github.com", error = FALSE)))
    stop ("legislatoR failed to establish a connection to GitHub. Please check your Internet connection and whether GitHub is online.")
  ghurl <- base::paste0("https://github.com/saschagobel/legislatoR/blob/master/data-raw/", legislature, "_facial?raw=true")
  connect <- base::url(ghurl)
  on.exit(close(connect))
  dataset <- base::readRDS(connect)
  return(dataset)
}
