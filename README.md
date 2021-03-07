# legislatoR: Interface to the Comparative <img src="images/sticker.jpg" width="160" align="right" /> <br /> Legislators Database

[![Travis-CI Build Status](https://travis-ci.org/saschagobel/legislatoR.svg?branch=master)](https://travis-ci.org/saschagobel/legislatoR)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/legislatoR)](https://cran.r-project.org/package=legislatoR)
[![GitHub release version](https://img.shields.io/github/release/saschagobel/legislatoR.svg?style=flat)](https://github.com/saschagobel/legislatoR/releases)
[![CRAN_Download_Badge](https://cranlogs.r-pkg.org/badges/grand-total/legislatoR)](https://cran.rstudio.com/web/packages/legislatoR/index.html)

legislatoR is a package for the software environment R that facilitates access to the Comparative Legislators Database (CLD). The CLD includes political, sociodemographic, career, online presence, public attention, and visual information for over 45,000 contemporary and historical politicians from ten countries.

## Content and data structure
The CLD covers the following countries and time periods:

| Country                              | Legislative sessions        | Politicians (unique) | Integrated with    |
| ------------------------------------ | --------------------------- | -------------------- | ------------------ |
| Austria (Nationalrat)                | all 27<br /> (1920-2019)    | 1,923                | [ParlSpeech V2](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/L4OAKN) (Rauh/Schwalbach 2020)      |
| Canada (House of Commons)            | all 43<br /> (1867-2019)    | 4,515                |                    |
| Czech Republic (Poslanecka Snemovna) | all 8<br /> (1992-2017)     | 1,020                | [ParlSpeech V1](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/E4RSP9) (Rauh et al. 2017)          |
| France (Assemblée)                   | all 15<br /> (1958-2017)    | 3,933                |                    |
| Germany (Bundestag)                  | all 19<br /> (1949-2017)    | 4,075                | [BTVote data](https://dataverse.harvard.edu/dataverse/btvote) (Bergmann et al. 2018),<br /> [ParlSpeech V1](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/E4RSP9) (Rauh et al. 2017),<br /> [Reelection Prospects data](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/EBEDPI) (Stoffel/Sieberer 2017)   |
| Ireland (Dail)                       | all 33<br /> (1918-2020)          | 1,408                |	[Database of Parliamentary Speeches in Ireland](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/6MZN76) (Herzog/Mikhaylov 2017)	|
| Scotland (Parliament)                | all 5<br /> (1999-2016)           | 305                  | [ParlScot](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/EQ9WBE) (Braby/Fraser 2021)      			 |
| Spain (Congreso de los Diputados)    | all 14<br /> (1979-2019)          | 2634           | [ParlSpeech V2](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/L4OAKN) (Rauh/Schwalbach 2020)      |        
| United Kingdom (House of Commons)    | all 58<br /> (1801-2019)          | 13,215               | [EggersSpirling data](https://github.com/ArthurSpirling/EggersSpirlingDatabase) (starting from <br /> 38th session, Eggers/Spirling 2014),<br /> [ParlSpeech V1](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/E4RSP9) (Rauh et al. 2017) | 
| United States (House and Senate)     | all 116<br /> (1789-2019)         | 12,512               | [Voteview data](https://voteview.com/data) (Lewis et al. 2019), <br /> [Congressional Bills Project data](http://www.congressionalbills.org/) (Adler/Wilkserson 2018) |
| **10**                                | **338**                     | **45,540**           | **12** 		       |

For each legislature, the CLD holds nine tables:

1.  *Core* (sociodemographic data)
2.  *Political* (political data)
3.  *History* (full revision records of individual Wikipedia biographies)
4.  *Traffic* (daily user traffic on individual Wikipedia biographies starting from July 2007)
5.  *Social* (social media handles and personal website URLs)
6.  *Portraits* (URLs to portraits)
7.  *Offices* (public offices)
8.  *Professions* (professions)
9.  *IDs* (identifiers linking politicians to other files, databases, or websites)
 
The tables contain the following variables (see respective R help files for further details):

- *Core*: Country, Wikipedia page ID, Wikidata ID, Wikipedia Title, full name, sex, ethnicity, religion, date of birth and death, place of birth and death.
- *Political*: Wikipedia page ID, legislative session, party affiliation, lower constituency, upper constituency, constituency ID, start and end date of legislative session, period of service, majority status, leader positions.
- *History*: Wikipedia page ID, Wikipedia revision and previous revision ID, editor name/IP and ID, revision date and time, revision size, revision comment.
- *Traffic*: Wikipedia page ID, date, user traffic.
- *Social*: Wikidata ID, Twitter handle, Facebook handle, Youtube ID, Google Plus ID, Instagram handle, LinkedIn ID, personal website URL.
- *Portraits*: Wikipedia page ID, Wikipedia portrait URL.
- *Offices*: Wikidata ID, a range of offices such as attorney general, chief justice, mayor, party chair, secretary of state, etc.
- *Professions*: Wikidata ID, a range of professions such as accountant, farmer, historian, judge, mechanic, police officer, salesperson, teacher, etc.
- *IDs*: Wikidata ID, IDs for integration with various political science datsets as well as a range of other IDs such as parliamentary website IDs, Library of Congress or German National Library IDs, Notable Names Database or Project Vote Smart IDs, etc.

Note that for some legislatures or legislative periods, tables may only hold information for a subset of politicians or variables. In successive versions of the CLD, we fill some of these gaps.

The CLD comes as a relational database. This means that all tables can be joined with the *Core* table via one of two keys - the Wikipedia page ID or the Wikidata ID. These keys uniquely identify individual politicians. The figure below illustrates this structure and the CLD's content.

<p align="center">
  <img width="500" src="images/data-structure.png">
</p>

## Installation
legislatoR is available through CRAN and GitHub. To install the package from CRAN, type:

```r
install.packages("legislatoR")
```

To install the package from Github, type:

```r
devtools::install_github("saschagobel/legislatoR")
```

## Usage

A working Internet connection is required to access the CLD in R. This is because the data are stored online and not installed together with the package. The package provides table-specific function calls. These functions are named after the respective table (see [above](#content-and-data-structure)) and preceded by `get_`. To fetch  the *Core* table, use the `get_core()` function, for the *Political* table, use the `get_political()` function. Call the package help file via `?legislatoR()` to get an overview of all function calls. Tables are legislature-specific, so a three-letter country code must be passed as an argument to the function. Here is a breakdown of all country codes. You can also call the `cld_content()` function to get an overview of the CLD's scope and valid country codes.

| Legislature               | Code        | Legislature      | Code       | Legislature            | Code                     |
| ------------------------- |:-----------:| ---------------- |:----------:| ---------------------- |:------------------------:|
| Austrian Nationalrat      | `aut`       | German Bundestag |  `deu`     | UK House of Commons    | `gbr`                    |
| Canadian House of Commons | `can`       | Irish Dail       | `irl`      | United States Congress | `usa_house`/`usa_senate` |
| Czech Poslanecka Snemovna | `cze`       | Scottish Parliament | `sco`   |                        |                          | 
| French Assemblée          | `fra`       | Spanish Congreso | `esp`      |                        |                          |

Tables can be joined and subsetted while being fetched and memory is only allocated by the parts of a table assigned into the environment. Basic fetching, joining, and subsetting of data are illustrated below. See the Vignette [Introducing legislatoR](http://htmlpreview.github.com/?https://github.com/saschagobel/legislatoR/blob/master/vignettes/legislatoR.html) for a detailed tutorial.

```r
# load and attach legislatoR and dplyr
library(legislatoR)
library(dplyr)

# assign entire Core table for the German Bundestag into the environment
deu_politicians <- get_core(legislature = "deu")

# assign data for the 8th legislative session into the environment
deu_politicians_subset <- semi_join(x = get_core(legislature = "deu"),
				                            y = filter(get_political(legislature = "deu"), session == 8), 
			                              by = "pageid")

# join deu_politicians_subset with respective traffic on Wikipedia biographies
deu_traffic <- left_join(x = deu_politicians_subset, 
               		       y = get_traffic(legislature = "deu"), 
		                     by = "pageid")

# assign birthdate for members of the political party 'SPD' into the environment
deu_birthdates_SPD <- semi_join(x = select(get_core(legislature = "deu"), pageid, birth),
                                y = filter(get_political(legislature = "deu"), party == "SPD"),
                                by = "pageid")$birth
```

## News
See [here](NEWS.md) for details on updates.

## Glossary
See [here](GLOSSARY.md) for the full form of abbreviated country codes and party names and English translations of non-English party names.

## Sources
The CLD was predominantly built using automated data extraction techniques. See the [source code](source) and [this list](SOURCES.md) of Web sources for more details.

## Citation
Thank you for using the CLD and legislatoR! Please consider citing:

Göbel, Sascha and Simon Munzert. "The Comparative Legislators Database". Forthcoming at *British Journal of Political Science*.

## Support
The work on this package was in part funded by the Daimler and Benz Foundation (Funding period 2017/18; project "Citizen and Elite Activity on the Wikipedia Market Place of Political Information").

Many thanks to Anna Wunderling for designing legislatoR's logo.

## Author information

**Sascha Göbel** (corresponding author and repository maintainer) <br />
University of Konstanz <br />
Graduate School of Decision Sciences and Center for Data and Methods <br />
Box 85 <br />
D-78457 Konstanz, Germany <br />
Email: sascha.goebel@uni-konstanz.de 

**Simon Munzert** <br />
Hertie School of Governance <br />
Quartier 110 - Friedrichstrasse 180 <br />
D-10117 Berlin, Germany <br />
Email: munzert@hertie-school.org
