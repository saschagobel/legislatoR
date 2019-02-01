# legislatoR: Political, sociodemographic, and <img src="images/logo.jpg" width="160" align="right" /> <br /> Wikipedia-related data on political elites 

[![Travis-CI Build Status](https://travis-ci.org/saschagobel/legislatoR.svg?branch=master)](https://travis-ci.org/saschagobel/legislatoR)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/legislatoR)](https://cran.r-project.org/package=legislatoR)
[![GitHub release version](https://img.shields.io/github/release/saschagobel/legislatoR.svg?style=flat](https://github.com/saschagobel/legislatoR/releases)

legislatoR is a data package for the software environment R. It comprises political, sociodemographic, and Wikipedia-related data on elected politicians across the globe. This version (0.1.0) includes 32,533 current and former elected politicians from nine countries' legislatures.

## Motivation
Researchers, students, analysts, journalists, and the public continue to rely on individual-level data on political elites for various kinds of analyses, whether theory-driven or motivated by real-world problems. As a consequence, the past has likely seen recurrent data collection efforts with the same purpose. This practice is highly inefficient. Student assistants, interns, or volunteers work on tedious data collection tasks likely accomplished by others already. Financial limitations or time restrictions often force analysts to limit their analyses to a subset of politicians. The frequent compromise is between broad (many politicians) but shallow (few variables) or narrow (few politicians) but deep (many variables) data. Existing data structures are either limited in scope, hidden behind paywalls, or simply not accessible to those who are not super tech-savvy. legislatoR is a free, efficient, and accessible one stop shop for broad and deep data on political elites, facilitates data integration, and supports replication efforts.

## Content and data structure
The data package covers the following countries and time periods:

| Country                              | Legislative sessions        | Politicians (unique) |
| ------------------------------------ | --------------------------- | -------------------- |
| Austria (Nationalrat)                | all 26 (1920-2017)         | 1,853                | 
| Canada (House of Commons)            | all 42 (1867-2015)         | 4,410                |
| Czech Republic (Poslanecka Snemovna) | all 8 (1992-2017)          | 1,020                |
| France (Assemblée)                   | all 15 (1958-2017)         | 3,603                |
| Germany (Bundestag)                  | all 19 (1949-2017)         | 4,075                |
| Ireland (Dail)                       | all 32 (1918-2016)         | 1,355                |
| Scotland (Parliament)                | all 5 (1999-2016)          | 305                  |
| United Kingdom (House of Commons)    | 38-57 (1945-2017)          | 3,400                |
| United States (House and Senate)     | all 116 (1789-2019)        | 12,512               |
| **9**                                | **283**                     | **32,533**           |

For each legislature, the package currently holds nine datasets: 

1.  *Core* (basic sociodemographic data)
2.  *Political* (basic political data)
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
- *IDs*: Wikidata ID, a range of IDs such as parliamentary website IDs, Library of Congress or German National Library IDs, Notable Names Database or Project Vote Smart IDs, etc.
 
Please note that for some legislatures or legislative periods, datasets may only hold data on a small subset of politicians, yield a substantial amount of missings for specific variables, or lack specific variables altogether. In successive versions of legislatoR, we try to fill some of these gaps.

legislatoR comes as a relational database, which means that all datasets can be joined with the *Core* dataset via one of two keys - the Wikipedia page ID or the Wikidata ID, which uniquely identify individual politicians. The figure below illustrates this structure and some of the package's content.

<p align="center">
  <img width="500" src="images/data-structure.png">
</p>

## Installation
legislatoR is currently only available through GitHub. To install the package in `R`, type:

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

## Sources
legislatoR was predominantly built using automated data extraction techniques. See the [source code](source) for more details. The package was assembled using the following Web sources or tools:

[Face++ Cognitive Services API](https://www.faceplusplus.com/) <br />
[Czech Republic Parliamentary Members Archive](http://public.psp.cz/sqw/fsnem.sqw?zvo=1) <br />
[Germany Bundestag Open Data](https://www.bundestag.de/service/opendata) <br />
[Wikimedia Commons](https://commons.wikimedia.org/) <br />
[Wikimedia API](https://wikimedia.org/) <br />
[Wikidata API](https://www.wikidata.org/) <br />
[Wikipedia](https://de.wikipedia.org/) <br />
[Wikipedia API](https://en.wikipedia.org/w/api.php) 

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
