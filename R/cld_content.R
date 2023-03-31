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
  output <-  list(aut = 1:27, bra = 38:57, can = 1:44, cze = 1:9,
                  deu = 1:20, esp = 1:14, fra = 1:16, gbr = 1:58,
                  irl = 1:33, isr = 1:25, ita_house = 1:19, ita_senate = 1:19,
                  jpn = 1:49, nld = 1:65, sco = 1:6, tur = 1:27,
                  usa_house = 1:117, usa_senate = 1:117)
  if (is.null(legislature)) {
    return(output)
  } else {
    if (any(!(legislature %in% c("aut", "bra", "can", "cze",
                                 "deu", "esp", "fra", "gbr",
                                 "irl", "isr", "ita_house", "ita_senate",
                                 "jpn", "nld", "sco", "tur",
                                 "usa_house", "usa_senate")))) {
      legislature <- legislature[which(!(legislature %in% c("aut", "bra", "can", "cze",
                                                            "deu", "esp", "fra", "gbr",
                                                            "irl", "isr", "ita_house", "ita_senate",
                                                            "jpn", "nld", "sco", "tur",
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
