#' List content of the CLD
#'
#' Returns a named list of legislatures and sessions available in the CLD. This provides a quick overview of the CLD's scope and valid three-letter country codes, and helps to conventiently loop/map over legislatures and sessions.
#'
#' @param legislature An optional character string specifying one or more legislatures. Currently one of \sQuote{aut}, \sQuote{can}, \sQuote{cze}, \sQuote{esp}, \sQuote{fra}, \sQuote{deu}, \sQuote{irl}, \sQuote{sco}, \sQuote{gbr}, \sQuote{usa_house}, or \sQuote{usa_senate}. If NULL (the default), a list with all legislatures and sessions available in the CLD is returned.
#' @return A list with names being three-letter country codes and with each element containing a vector that shows the sessions available for a legislature.
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
