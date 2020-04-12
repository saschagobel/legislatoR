# legislatoR 1.0
  
* All legislative periods of Spain's Congreso de los Diputados added to the database (14 in total).
* Ongoing session of the Austrian Nationalrat added to the database (27th) and concluded session updated (26th).
* Ongoing session of the Canadian House of Commons added to the database (43th) and concluded session updated (42th).
* Ongoing session of the Irish Dáil added to the database (33th) and concluded session updated (32th).
* Ongoing session of the UK House of Commons added to the database (58th) and concluded session updated (57th).
* Irish Dáil data integrated with Database of Parliamentary Speeches in Ireland.
* Czech Poslanecka Snemovna data integrated with ParlSpeech V1.
* UK House of Commons data integrated with ParlSpeech V1.
* Spanish Congreso de los Diputados data integrated with ParlSpeech V2.
* Austrian Nationalrat data integrated with ParlSpeech V2.
* Daily Wikipedia page traffic now goes back to December 2007 (except for the US House, where file storage restrictions currently limit traffic to range back to July 2009 only).
* New `cld_content()` function that offers a quick overview of the CLD's scope and valid three-letter country codes, and helps to conventiently loop/map over legislatures and sessions.
* "Introducing legislatoR" Vignette added.
* New Logo/Hexsticker.


# legislatoR 0.2.0

* 1st to 37th legislative periods of UK's House of Commons added to the database (37 in total).
* Missing legislators and services variable added to all sessions of the French Assemblée.
* German Bundestag data integrated with BTVote and ParlSpeech data.
* UK House of Commons data integrated with Eggers and Spirling british political development database (starting from 38th session).
* United States House and Senate data integrated with Voteview and congressional bills project data.
* Missing Twitter handles added for the Austrian Nationalrat, Czech Poslanecka Snemovna, French Assemblée, German Bundestag, Irish Dail, UK House of Commons, and United States House and Senate (841 in total).
* Missing religious affilition added for United States House and Senate and Canada's House of Commons.
* Missing ethnicity added for Canada and UK's House of Commons.

# legislatoR 0.1.0

* Changed some function calls: `get_facial` is now `get_portrait` and `get_occupation` is now `get_profession`.
* Changed valid legislature arguments for `get_` functions. Full legislature names are no longer accepted as legislature codes. Instead, three-letter country codes such as `aut`, `can`, `cze`, `fra`, `deu`, `irl`, `sco`, `gbr`, `usa_house`, `usa_senate` must be used.
* All legislative periods of Canada's House of Commons added to the database (42 in total).
* All legislative periods of Czech Republic's Poslanecka Snemovna added to the database (8 in total).
* All legislative periods of Scotland's Parliament added to the database (5 in total).
* 38th - 57th legislative periods of UK's House of Commons added to the database (20 in total).
* Current session of the Austrian Nationalrat added to the database (26th).
* Current session of the German Bundestag added to the database (19th).
* Current session of the French Assemblée added to the database (15th).
* Current session of the United States House of representatives added to the database (116th).
* Current session of the United States Senate added to the database (116th).
* ISO 3166-1 alpha-3 three-letter country codes added to all core datasets.
* Start- and end date of sessions added to all political datasets.
* Missing legislators and services variable added to all sessions of the Irish Dáil.
* Error in United States Senate Wikipedia revision records corrected.
* Missing religious affiliation added from 1st to 18th German Bundestag using official data.
* Erroneous information on United States Senate and House members' period of service corrected.
* Additional variables on government/opposition status and leader positions added to all sessions of the Canadian House of Commons and United States Congress.
* Correction and alignment of party names across legislative periods for all parliaments.
* Emotion estimates removed from the portraits dataset.
