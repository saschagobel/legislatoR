# legislatoR: Political, sociodemographic, and <img src="images/logo.jpg" width="160" align="right" /> <br /> Wikipedia-related data on political elites 

[![Travis-CI Build Status](https://travis-ci.org/saschagobel/legislatoR.svg?branch=master)](https://travis-ci.org/saschagobel/legislatoR)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/legislatoR)](https://cran.r-project.org/package=legislatoR)
[![GitHub release version](https://img.shields.io/github/release/saschagobel/legislatoR.svg?style=flat)](https://github.com/saschagobel/legislatoR/releases)
[![Tweet](https://img.shields.io/twitter/url/http/shields.io.svg?style=social)](https://twitter.com/intent/tweet?text=R%20data%20package%20with%20political,%20sociodemographic,%20and%20Wikipedia-related%20data%20for%20more%20than%2030K%20current%20and%20former%20elected%20politicians%20from%20nine%20countries'%20legislatures&url=https://github.com/saschagobel/legislatoR&hashtags=rpackage,legislators,politicians,wikipedia,rstats)

legislatoR is a data package for the software environment R. It comprises political, sociodemographic, and Wikipedia-related data on elected politicians across the globe. This version (0.2.0) includes 42,534 current and former elected politicians from nine countries' legislatures.

## Motivation
Researchers, students, analysts, journalists, and the public continue to rely on individual-level data on political elites for various kinds of analyses, whether theory-driven or motivated by real-world problems. As a consequence, the past has likely seen recurrent data collection efforts with the same purpose. Financial limitations or time restrictions often force analysts to limit their analyses to a subset of politicians, though. The frequent compromise is between many politicians but few variables or few politicians but many variables. Existing data structures are often limited in scope, hidden behind paywalls, or simply not accessible to those who are not super tech-savvy. legislatoR is an open-source, targeted, fast, and easily accessible one stop shop for comprehensive data on political elites. Data comes in a rectangular/spreadsheet format familiar to social scientists and ready for immediate analysis. It facilitates data integration, and supports replication efforts.

## Content and data structure
The data package covers the following countries and time periods:

| Country                              | Legislative sessions        | Politicians (unique) | Integrated with    |
| ------------------------------------ | --------------------------- | -------------------- | ------------------ |
| Austria (Nationalrat)                | all 26<br /> (1920-2017)          | 1,853                |                    |
| Canada (House of Commons)            | all 42<br /> (1867-2015)          | 4,410                |                    |
| Czech Republic (Poslanecka Snemovna) | all 8<br /> (1992-2017)           | 1,020                |                    |
| France (Assemblée)                   | all 15<br /> (1958-2017)          | 3,933                |                    |
| Germany (Bundestag)                  | all 19<br /> (1949-2017)          | 4,075                | [BTVote data](https://dataverse.harvard.edu/dataverse/btvote) (Bergmann et al. 2018),<br /> [ParlSpeech data](https://dataverse.harvard.edu/dataverse/ParlSpeech) (Rauh et al. 2017) |
| Ireland (Dail)                       | all 32<br /> (1918-2016)          | 1,355                |			 |
| Scotland (Parliament)                | all 5<br /> (1999-2016)           | 305                  | 			 |
| United Kingdom (House of Commons)    | all 57<br /> (1801-2017)          | 13,071               | [EggersSpirling data](https://github.com/ArthurSpirling/EggersSpirlingDatabase) (starting from <br /> 38th session, Eggers/Spirling 2014) | 
| United States (House and Senate)     | all 116<br /> (1789-2019)         | 12,512               | [Voteview data](https://voteview.com/data) (Lewis et al. 2019), <br /> [Congressional Bills Project data](http://www.congressionalbills.org/) (Adler/Wilkserson 2018) |
| **9**                                | **320**                     | **42,534**           | **5** 		 |

For each legislature, the package holds nine datasets: 

1.  *Core* (sociodemographic data)
2.  *Political* (political data)
3.  *History* (full revision records of individual Wikipedia biographies)
4.  *Traffic* (daily user traffic on individual Wikipedia biographies from July 2015 to December 2018)
5.  *Social* (social media handles and personal website URLs)
6.  *Portrait* (URLs to individual Wikipedia portraits)
7.  *Office* (public offices)
8.  *Profession* (professions)
9.  *ID* (a range of identifiers linking a politician to another file, database, or website)
 
The datasets contain the following variables (see the respective R help files for further details):

- *Core*: Country, Wikipedia page ID, Wikidata ID, Wikipedia Title, full name, sex, ethnicity, religion, date of birth and death, place of birth and death.
- *Political*: Wikipedia page ID, legislative session, party affiliation, lower constituency, upper constituency, constituency ID, start and end date of legislative session, period of service, majority status, leader positions.
- *History*: Wikipedia page ID, Wikipedia revision and previous revision ID, editor name/IP and ID, revision date and time, revision size, revision comment.
- *Traffic*: Wikipedia page ID, date, user traffic.
- *Social*: Wikidata ID, Twitter handle, Facebook handle, Youtube ID, Google Plus ID, Instagram handle, LinkedIn ID, personal website URL.
- *Portraits*: Wikipedia page ID, Wikipedia portrait URL.
- *Offices*: Wikidata ID, a range of offices such as attorney general, chief justice, mayor, party chair, secretary of state, etc.
- *Professions*: Wikidata ID, a range of professions such as accountant, farmer, historian, judge, mechanic, police officer, salesperson, teacher, etc.
- *IDs*: Wikidata ID, IDs for integration with various political science datsets as well as a range of other IDs such as parliamentary website IDs, Library of Congress or German National Library IDs, Notable Names Database or Project Vote Smart IDs, etc.


Note that for some legislatures or legislative periods, datasets may only hold data on a subset of observations. In successive versions of legislatoR, we try to fill some of these gaps.

legislatoR comes as a relational database. This means that all datasets can be joined with the Core dataset via one of two keys - the Wikipedia page ID or the Wikidata ID. These keys uniquely identify individual politicians. The figure below illustrates this structure and some of the package's content.

<p align="center">
  <img width="500" src="images/data-structure.png">
</p>

## Installation
legislatoR is available through GitHub. To install the package in `R`, type:

```r
devtools::install_github("saschagobel/legislatoR")
```

## Usage

After having installed the package, a working Internet connection is required in order to access the data in R. This is because the data are not installed with the package, but are stored on legislatoR's GitHub repository. The package provides dataset-specific function calls to fetch the data from the repository. These functions are named after the datasets and preceded by `get_`. To fetch  the *Core* dataset, use the `get_core` function, for the *Political* dataset, use the `get_political` function, and so on (see [above](#content-and-data-structure) for dataset names). The datasets are all legislature-specific. To access a dataset in R the legislature's code must be passed as an argument to the respective dataset's function call. The legislature codes are:

| Legislature               | Code        | Legislature      | Code       | Legislature            | Code                     |
| ------------------------- |:-----------:| ---------------- |:----------:| ---------------------- |:------------------------:|
| Austrian Nationalrat      | `aut`       | French Assemblée | `fra`      | Scottish Parliament    | `sco`                    |
| Canadian House of Commons | `can`       | German Bundestag | `deu`      | UK House of Commons    | `gbr`                    |
| Czech Poslanecka Snemovna | `cze`       | Irish Dail       | `irl`      | United States Congress | `usa_house`/`usa_senate` |        | 

Data can be joined and subsetted while being fetched from the repository and memory is only allocated by the parts of a dataset assigned into the environment. The data fetching, joining and subsetting stages are illustrated in the code below.

```r
# load and attach legislatoR and dplyr packages
library(legislatoR)
library(dplyr)

# assign entire Core dataset for the German Bundestag into the environment
deu_politicians <- get_core(legislature = "deu")

# assign only data for the 8th legislative session into the environment
deu_politicians_subset <- semi_join(x = get_core(legislature = "deu"),
				    y = filter(get_political(legislature = "deu"), session == 8), 
			            by = "pageid")

# join deu_politicians_subset with respective History dataset
deu_history <- left_join(x = deu_politicians_subset, 
               		 y = get_history(legislature = "deu"), 
		         by = "pageid")

# assign only birthdate for members of the political party 'SPD' into the environment
deu_birthdates_SPD <- semi_join(x = select(get_core(legislature = "deu"), pageid, birth),
                                y = filter(get_political(legislature = "deu"), party == "SPD"),
                                by = "pageid")$birth
```

For each dataset, there is a help file with details on content and usage examples.

```r
# call help file for legislatoR package to get an overview of the function calls
?legislatoR

# call help file for the 'History' dataset 
?get_history
```

## News
See [here](NEWS.md) for details on package updates.

## Glossary
See [here](GLOSSARY.md) for the full form of abbreviated country codes and party names and English translations of non-English party names.

## Sources
legislatoR was predominantly built using automated data extraction techniques. See the [source code](source) and [this list](SOURCES.md) of Web sources for more details.

## Citation
Thank you for using legislatoR! Please consider citing:

Göbel, Sascha and Simon Munzert. (2019). legislatoR: Political, sociodemographic, and Wikipedia-related data on political elites. Source: https://github.com/saschagobel.

## Support
The work on this package was in part funded by the Daimler and Benz Foundation (Funding period 2017/18; project "Citizen and Elite Activity on the Wikipedia Market Place of Political Information").

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
