#' Fetch 'IDs' table
#'
#' Fetches a range of IDs of legislators for the specified legislature. Requires a working Internet connection.
#'
#' @param legislature A character string specifying the three-letter country code of the legislature for which data shall be fetched. Currently one of \sQuote{aut}, \sQuote{can}, \sQuote{cze}, \sQuote{esp}, \sQuote{fra}, \sQuote{deu}, \sQuote{irl}, \sQuote{sco}, \sQuote{gbr}, \sQuote{usa_house}, or \sQuote{usa_senate}.
#' @return A data frame with columns as specified above.
#' @format Data frame with columns (varies by legislature):
#' \itemize{
#' \item{wikidataid: Wikidata ID identifying a legislator's Wikidata entry (of class \sQuote{character}).}
#' \item{parlid: Parliamentary website or website ID  (of class \sQuote{character}).}
#' \item{btvote: ID for BTVote data on all roll call votes taken in the German Bundestag from 1949 to 2013 and for Reelection Prospects data. The respective ID in BTVote and Reelection Prospects data is 'mp_id' (of class \sQuote{character}).}
#' \item{parlspeech: ID for ParlSpeech datasets containing full-text vectors of plenary speeches. The respective ID in ParlSpeech data is 'speaker' (of class \sQuote{character}).}
#' \item{dpsi: ID for Database of Parliamentary Speeches in Ireland. The respective ID in the Database of Parliamentary Speeches in Ireland is 'memberID' (of class \sQuote{character}).}
#' \item{eggersspirling: ID for Eggers and Spirling British political development database. The respective ID in Eggers and Spirling data is 'member.id' (of class \sQuote{character}).}
#' \item{bioguide: ID for the Voteview congressional roll-call votes database. The respective ID in Voteview data is 'bioguide_id' (of class \sQuote{character}).}
#' \item{icpsr: ID for Congressional Bills Project database. The respective ID in Congressional Bills Project data is 'icpsr' (of class \sQuote{character}).}
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
#' # Get entire 'IDs' table for the Austrian Nationalrat
#' aut_ids <- get_ids(legislature = "aut")
#' tibble::glimpse(aut_ids)
#'
#' # Get ParlSpeech IDs and add 'Offices' table for the Austrian Nationalrat
#' aut_ids_subset <- dplyr::inner_join(x = dplyr::filter(aut_ids,
#'                                                       !is.na(parlspeech)),
#'                                     y = get_office(legislature = "aut"),
#'                                     by = "wikidataid")
#' tibble::glimpse(aut_ids_subset)
#' @source
#' Wikidata API, \url{https://www.wikidata.org/}
#' @export
#' @importFrom curl nslookup
#' @import dplyr
get_ids <- function(legislature) {
  if (length(legislature) > 1) {
    stop ("\n\nNo more than one legislature can be called at once. Please provide only one valid three-letter country code.")
  }
  if (!(legislature %in% c("aut", "can", "cze",
                           "esp", "fra", "deu",
                           "irl", "sco", "gbr",
                           "usa_house", "usa_senate"))) {
    stop (paste0("\n\nPlease provide a valid three-letter country code. legislatoR does not recognize the country code or does not contain data for ",
                 paste0(
                   paste0("\"", legislature, "\""),
                   collapse = ", "),
                 ". Use `legislatoR::cld_content()` to see country codes of available legislatures."))
  }
  if (is.null(curl::nslookup("www.github.com", error = FALSE))) {
    stop ("\n\nlegislatoR cannot establish a connection to GitHub. Please check your Internet connection and whether GitHub is online.")
  }
  ghurl <- paste0("https://github.com/saschagobel/legislatoR-data/blob/master/data/", legislature, "_ids?raw=true")
  connect <- url(ghurl)
  on.exit(close(connect))
  dataset <- readRDS(connect)
  return(dataset)
}
