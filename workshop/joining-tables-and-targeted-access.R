# ---------------------------------------------------------------------------------------
# legislatoR: Joining Tables and Targeted Access
# ---------------------------------------------------------------------------------------



#### ILLUSTRATING TABLE JOINS ===========================================================

# join "Core" and "Political" tables for German Bundestag -------------------------------
join_bt <- left_join(x = get_core(legislature = "deu"), 
                     y = get_political(legislature = "deu"), 
                     by = "pageid")
glimpse(join_bt)

# add "Social" table for German Bundestag -----------------------------------------------
join2_bt <- left_join(x = join_bt, 
                      y = get_social(legislature = "deu"), 
                      by = "wikidataid")
glimpse(join2_bt)

# join "Political" and "Wikipedia Traffic" ----------------------------------------------
join3_bt <- left_join(x = get_political(legislature = "deu"), 
                     y = get_traffic(legislature = "deu"), 
                     by = "pageid")
glimpse(join3_bt)



#### EXAMPLES OF TARGETED DATA ACCESS ===================================================

# catholic politicians from the German Bundestag ----------------------------------------
catholic_bt <- filter(get_core(legislature = "deu"), religion == "catholicism")
glimpse(catholic_bt)

# ``Core'' table with politicians from CDU parties in the German Bundestag -------------- 
partysub_bt <- semi_join(x = get_core(legislature = "deu"),
                         y = filter(get_political(legislature = "deu"), 
                                    party == "CDU"),
                         by = "pageid")
glimpse(partysub_bt)

# Politicians from CDU, CSU, and AfD parties in the German Bundestag -------------------- 
partysub2_bt <- inner_join(x = get_core(legislature = "deu"),
                           y = filter(get_political(legislature = "deu"), 
                                      party %in% c("CDU", "CSU", "AfD")),
                           by = "pageid")
glimpse(partysub2_bt)

# female legislators from the 7th German Bundestag --------------------------------------
female_bt7 <- inner_join(x = filter(get_core(legislature = "deu"), sex == "female"), 
                         y = filter(get_political(legislature = "deu"), session == 7), 
                         by = "pageid")
glimpse(female_bt7)

# birthplaces of politicians from German Bundestag --------------------------------------
birthplaces_bt <- pull(get_core(legislature = "deu"), "birthplace")
glimpse(birthplaces_bt)

# all available Twitter handles of politicians from German Bundestag --------------------
twitter_bt <- pull(filter(get_social(legislature = "deu"), !is.na(twitter)), 
                   "twitter")
glimpse(twitter_bt)

# Further country codes:
# "aut" - Austria
# "can" - Canada
# "cze" - Czech Republic
# "fra" - France
# "irl" - Ireland
# "sco" - Scotland
# "gbr" - United Kingdom
# "usa_house" - United States (House)
# "usa_senate" - United States (Senate)

