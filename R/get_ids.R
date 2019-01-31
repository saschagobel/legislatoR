#' Fetch 'IDs' dataset
#'
#' Fetches a range of IDs of legislators for the specified legislature. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the legislature for which data shall be fetched. Currently one of \sQuote{aut}, \sQuote{can}, \sQuote{cze}, \sQuote{fra}, \sQuote{deu}, \sQuote{irl}, \sQuote{sco}, \sQuote{gbr}, \sQuote{usa_house}, or \sQuote{usa_senate}.
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
#' \item{nkcr: Czech National Library ID (of class \sQuote{character}).}
#' \item{parlbio: parliament.uk biography ID (of class \sQuote{character}).}
#' \item{parlthesaurus: UK Parliament thesaurus ID (of class \sQuote{character}).}
#' \item{national: UK National Archives ID (of class \sQuote{character}).}
#' \item{hansard: Hansard (1803-2005) ID (of class \sQuote{character}).}
#' \item{publicwhip: PublicWhip ID (of class \sQuote{character}).}
#' \item{theyworkforyou: TheyWorkForYou ID (of class \sQuote{character}).}
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
#' Wikidata API, \url{https://www.wikidata.org/}
#' @export
#' @importFrom curl nslookup
#' @import dplyr
get_ids <- function(legislature) {
  if (!(legislature %in% c("aut", "can", "cze", "fra", "deu", "irl", "sco", "gbr", "usa_house", "usa_senate")))
    stop ("legislatoR does not contain data for this legislature at the moment. Please try one of 'aut', 'can', 'cze', 'fra', 'deu', 'irl', 'usa_house', or 'usa_senate'.")
  if (is.null(curl::nslookup("www.github.com", error = FALSE)))
    stop ("legislatoR failed to establish a connection to GitHub. Please check your Internet connection and whether GitHub is online.")
  ghurl <- base::paste0("https://github.com/saschagobel/legislatoR-data/blob/master/data/", legislature, "_ids?raw=true")
  connect <- base::url(ghurl)
  on.exit(close(connect))
  dataset <- base::readRDS(connect)
  return(dataset)
}
