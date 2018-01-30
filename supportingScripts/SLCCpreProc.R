## Read in and preprocess SLCC dataset
## DBM, 12/14/17


# Setup the necessary packages --------------------------------------------
rm(list = ls())
## Get the necessary packages for the analyses
get.package<- function(package){
  if (!package %in% installed.packages()){
    install.packages(package, repos = "http://cran.rstudio.com/")
  } 
  invisible(library(package, character.only = TRUE))
}

## packages required
packages.needed <- c('dplyr', 'rapportools','readr', 'prophet','sjPlot', 'optimx', 'Rcpp','lme4', 'kableExtra','lmerTest','readxl', 'ggplot2', 'tidyr', 'broom', 'captioner','knitr','extrafont', 'lubridate', 'lettercase') 
suppressMessages(sapply(packages.needed, get.package))

## source ggplot template
source('~/Documents/oer-history/supportingScripts/ggplot2theme_min.R')


## Read in SLCC data
SLCCpersistence <- read_csv('~/Box Sync/SLCC History/oer_data.csv')

# Preprocess dataset for modeling------------------------------------------------------
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
         birthDate = dmy(BIRTH_DATE),
         termStartDate = dmy(TERM_START_DATE),
         termEndDate = dmy(TERM_END_DATE)) %>% 
  mutate(roundedGrade = gsub("\\+|\\-","", FINAL_GRADE),
         age = as.numeric(round(difftime(termStartDate, birthDate, units = "weeks")/52)),
         year = year(termStartDate), 
         semester = if_else(month(termStartDate) == 8, "Fall",
                           if_else(month(termStartDate) == 5, "Summer", "Spring")),
         courseGrade = if_else(FINAL_GRADE %in% c("E","F"), "0",
                               if_else(FINAL_GRADE  %in% c("D", "D-"), "1",
                                       if_else(FINAL_GRADE == "D+", "1.3",
                                               if_else(FINAL_GRADE == "C-","1.6",
                                                       if_else(FINAL_GRADE == "C", "2",
                                                               if_else(FINAL_GRADE == "C+", "2.3",
                                                                       if_else(FINAL_GRADE == "B-","2.6",
                                                                               if_else(FINAL_GRADE == "B", "3",
                                                                                       if_else(FINAL_GRADE == "B+", "3.3",
                                                                                               if_else(FINAL_GRADE == "A-", "3.6",
                                                                                                       if_else(FINAL_GRADE == "A","4",
                                                                                                               if_else(is.na(FINAL_GRADE), "NA", 
                                                                                                                       if_else(FINAL_GRADE == "A+", "4.3", "-1")))))))))))))) %>% 
  unite(yrSem, semester, year, sep = " ", remove = FALSE) %>% 
  mutate(courseGrade = as.numeric(courseGrade),
         termDuration = difftime(termEndDate, termStartDate, units = "days"),
    oer = if_else(termStartDate >= "2014-05-19" & COURSE_SUBJECT == "HIST", 1, 0),
             pass = if_else(roundedGrade <= "C",1,0),
         dfw = if_else(pass == 1, 0, 1),
         academicYear = if_else(yrSem %in% c("Fall 2011", "Spring 2012", "Summer 2012"), "2011-2012",
                                if_else(yrSem %in% c("Fall 2012", "Spring 2013", "Summer 2013"), "2012-2013",
                                        if_else(yrSem %in% c("Fall 2013", "Spring 2014", "Summer 2014"), "2013-2014",
                                                if_else(yrSem %in% c("Fall 2014", "Spring 2015", "Summer 2015"), "2014-2015",
                                                        if_else(yrSem %in% c("Fall 2015", "Spring 2016", "Summer 2016"), "2015-2016",
                                                                if_else(yrSem %in% c("Fall 2016", "Spring 2017", "Summer 2017"), "2016-2017", "Check"))))))) %>% 
  select(-TERM_START_DATE,-TERM_END_DATE,-BIRTH_DATE, -INSTRUCTOR_PIDM)
  


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

## Add in semester since implementation variable
## Filter out students younger than 18
SLCCpersistenceProc <- SLCCpersistenceProc %>% 
  filter(age>=18) %>% 
  mutate(semSinceImplementation = 
           if_else(yrSem == "Summer 2014" & courseSubject == "HIST", 1, 
                   if_else(yrSem == "Fall 2014" & courseSubject == "HIST",2,
                           if_else(yrSem == "Spring 2015" & courseSubject == "HIST",3,
                                   if_else(yrSem == "Summer 2015" & courseSubject == "HIST",4,
                                           ifelse(yrSem == "Fall 2015" & courseSubject == "HIST",5,
                                                  if_else(yrSem == "Spring 2016" & courseSubject == "HIST",6,
                                                          if_else(yrSem == "Summer 2016" & courseSubject == "HIST",7, 
                                                                  if_else(yrSem == "Fall 2016" & courseSubject == "HIST",8,
                                                                          if_else(yrSem == "Spring 2017" & courseSubject == "HIST", 9, 0)))))))))) %>% 
  select(-id)

