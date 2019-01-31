# legislatoR 0.1.0

* Changed some function calls: `get_facial` is now `get_portrait` and `get_occupation` is now `get_profession`.
* Changed valid legislature arguments for `get_` functions. Full legislature names are no longer accepted as legislature codes. Instead, three-letter country codes such as `aut`, `can`, `cze`, `fra`, `deu`, `irl`, `sco`, `gbr`, `usa_house`, `usa_senate` must be used.
* All legislative periods of Canada's House of Commons added to the database (42 in total).
* All legislative periods of Czech Republic's Poslanecka Snemovna added to the database (8 in total).
* All legislative periods of Scotland's Parliament added to the database (5 in total).
* 38th - 57th legislative periods of UK's House of Commons added to the database (20 in total).
* Current session of the Austrian Nationalrat added to the database (26th).
* Current session of the German Bundestag added to the database (19th).
* Current session of the French Assemble added to the database (15th).
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
