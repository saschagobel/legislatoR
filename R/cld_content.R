#' List content of the CLD
#'
#' Returns a named list of legislatures and sessions available in the CLD. This provides a quick overview of the CLD's scope and valid three-letter country codes, and helps to conventiently loop/map over legislatures and sessions.
#'
#' @param legislature An optional character string specifying one or more legislatures. Currently one of \sQuote{aut}, \sQuote{can}, \sQuote{cze}, \sQuote{esp}, \sQuote{fra}, \sQuote{deu}, \sQuote{irl}, \sQuote{sco}, \sQuote{gbr}, \sQuote{usa_house}, or \sQuote{usa_senate}. If NULL (the default), a list with all legislatures and sessions available in the CLD is returned.
#' @return A list with names being three-letter country codes and with each element containing a vector that shows the sessions available for a legislature.
#' @examples
#' # Get a list of three-letter country codes and available sessions for all countries
#' overview <- cld_content()
#' tibble::glimpse(overview)
#'
#' # Get a list of available sessions for the French Assemblée and the Irish Dail
#' sessions <- cld_content(legislature = c("fra", "irl"))
#' tibble::glimpse(sessions)
#' @export
cld_content <- function(legislature = NULL) {
  output <-  list(aut = 1:27, can = 1:43, cze = 1:8,
                  esp = 1:15, fra = 1:15, deu = 1:19,
                  irl = 1:33, sco = 1:5, gbr = 1:58,
                  usa_house = 1:116, usa_senate = 1:116)
  if (is.null(legislature)) {
    return(output)
  } else {
    if (any(!(legislature %in% c("aut", "can", "cze",
                                 "esp", "fra", "deu",
                                 "irl", "sco", "gbr",
                                 "usa_house", "usa_senate")))) {
      legislature <- legislature[which(!(legislature %in% c("aut", "can", "cze",
                                                            "esp", "fra", "deu",
                                                            "irl", "sco", "gbr",
                                                            "usa_house", "usa_senate")))]
      stop (paste0("\n\nPlease provide valid three-letter country codes. legislatoR does not recognize the country code or does not contain data for ",
                   paste0(
                     paste0("\"", legislature, "\""),
                     collapse = ", "),
                   ". Use `legislatoR::cld_content()` to see country codes of available legislatures."))
    }
    output <- output[legislature]
  }
  return(output)
}
