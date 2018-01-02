## Read in and preprocess SLCC dataset
## DBM, 12/14/17


###### SET UP THE PACKAGES ####
rm(list = ls())
## Get the necessary packages for the analyses
get.package<- function(package){
  if (!package %in% installed.packages()){
    install.packages(package, repos = "http://cran.rstudio.com/")
  } 
  invisible(library(package, character.only = TRUE))
}

## packages required
packages.needed <- c('dplyr', 'rapportools','readr', 'readxl', 'ggplot2', 'tidyr', 'broom', 'captioner','knitr','extrafont', 'lubridate', 'lettercase') 
invisible(sapply(packages.needed, get.package))

## source ggplot template
source('~/Documents/git/research/gates/ggplot2theme_min.R')


## Read in SLCC data
SLCCpersistence <- read_csv('~/Box Sync/SLCC History/oer_data.csv')

######## Preprocess dataset for modeling
### Convert date fields to standardized format
### Compute Age
### Mark academic year
### Mark semesters
### Round off grades
### Mark passing grades (>C)
### Mark before/After OER
### The Open Ed program was started in the summer of 2014

SLCCpersistenceProc <- SLCCpersistence %>% 
  mutate(studentId = factor(id), 
         instructorId = factor(INSTRUCTOR_PIDM),
         birthDate = dmy(SLCCpersistence$BIRTH_DATE),
         termStartDate = dmy(SLCCpersistence$TERM_START_DATE),
         termEndDate = dmy(SLCCpersistence$TERM_END_DATE)) %>% 
  mutate(roundedGrade = gsub("\\+|\\-","", FINAL_GRADE),
         age = as.numeric(round(difftime(termStartDate, birthDate, units = "weeks")/52)),
         year = year(termStartDate), 
         semester = if_else(month(termStartDate) == 8, "Fall",
                           ifelse(month(termStartDate) == 5, "Summer", "Spring"))) %>% 
  unite(yrSem, year, semester, sep = " ") %>% 
  mutate(oer = if_else(termStartDate >= "2014-05-19" & COURSE_SUBJECT == "HIST", 1, 0),
         pass = if_else(roundedGrade >= "C",1,0),
         academicYear = if_else(yrSem %in% c("2011 Fall", "2012 Spring", "2012 Summer"), "2011-2012",
                                if_else(yrSem %in% c("2012 Fall", "2013 Spring", "2013 Summer"), "2012-2013",
                                        if_else(yrSem %in% c("2013 Fall", "2014 Spring", "2014 Summer"), "2013-2014",
                                                if_else(yrSem %in% c("2014 Fall", "2015 Spring", "2015 Summer"), "2014-2015",
                                                        if_else(yrSem %in% c("2015 Fall", "2016 Spring", "2016 Summer"), "2015-2016",
                                                                if_else(yrSem %in% c("2016 Fall", "2017 Spring", "2017 Summer"), "2016-2017", "Check"))))))) %>% 
  select(-TERM_START_DATE,-TERM_END_DATE,-BIRTH_DATE)

## Convert names to camel case
## First conver to lower case
names(SLCCpersistenceProc)[is_upper(names(SLCCpersistenceProc))] <- tolower(names(SLCCpersistenceProc)[is_upper(names(SLCCpersistenceProc))])
## Then select names with underscores (_)
names(SLCCpersistenceProc)[grepl("_", names(SLCCpersistenceProc))] <- lapply(names(SLCCpersistenceProc)[grepl("_", names(SLCCpersistenceProc))], function(nm){
  nmNo_ <- unlist(strsplit(nm, "_"))
  ## Select the words other than the first word, and convert the first letter to uppercase
  followingWords <- paste0(unlist(lapply(strsplit(nmNo_[-1],""), function(w){
    tmpChar <- unlist(w)
    tmpChar[1] <- toupper(tmpChar[1])
    paste0(tmpChar, collapse = "")
  })), collapse = "")
  paste0(nmNo_[1], followingWords, collapse = "")
})