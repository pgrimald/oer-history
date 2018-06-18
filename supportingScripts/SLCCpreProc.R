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
packages.needed <- c('tidyverse', 'rapportools', 'prophet','scales','sjPlot', 'optimx', 'Rcpp','lme4', 'kableExtra','lmerTest','readxl', 'captioner','knitr','extrafont', 'lubridate', 'lettercase') 
suppressMessages(sapply(packages.needed, get.package))

## source ggplot template
source('~/Documents/oer-history/supportingScripts/ggplot2theme_min.R')


# Read in SLCC data -------------------------------------------------------

SLCCpersistence <- read_csv('~/Box Sync/SLCC History/oer_data.csv')
SLCC_11_18 <- read_csv('Box Sync/SLCC History/oer_data_update_201140-201820.csv')
teacher_sections <- read_csv("~/Box Sync/SLCC History/Instructor Sections.csv")

SLCCperc11_18 <- SLCCperc11_18 %>% 
  left_join(teacher_sections)
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
         year = gsub("20","",year),
         semester = if_else(month(termStartDate) == 8, "F",
                           if_else(month(termStartDate) == 5, "Su", "Sp")),
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
  mutate(yrSem = factor(yrSem), courseGrade = as.numeric(courseGrade),
         termDuration = difftime(termEndDate, termStartDate, units = "days"),
    oer = factor(if_else(termStartDate >= "2015-08-26" & COURSE_SUBJECT == "HIST", 1, 
                         if_else(termStartDate == "2016-01-11" & COURSE_SECTION  %in% c(paste0("0",c(12,14,18,22,23,28,35,76))) & COURSE_SUBJECT == "HIST",1,0))),
    pass = factor(if_else(roundedGrade <= "C",1,0)),
    dfw = factor(if_else(pass == 1, 0, 1)),
    academicYear = factor(if_else(yrSem %in% c("F 11", "Sp 12", "Su 12"), "11-12",
                                if_else(yrSem %in% c("F 12", "Sp 13", "Su 13"), "12-13",
                                        if_else(yrSem %in% c("F 13", "Sp 14", "Su 14"), "13-14",
                                                if_else(yrSem %in% c("F 14", "Sp 15", "Su 15"), "14-15",
                                                        if_else(yrSem %in% c("F 15", "Sp 16", "Su 16"), "15-16",
                                                                if_else(yrSem %in% c("F 16", "Sp 17", "Su 17"), "16-17", "Check")))))))) %>% 
  select(-TERM_START_DATE,-TERM_END_DATE,-BIRTH_DATE, -INSTRUCTOR_PIDM)
  
## Folding in new data from academic year 2017-2018
SLCCpersistenceProc17_18 <- SLCCperc17_18 %>% 
  mutate(studentId = factor(id), 
         instructorId = factor(INSTRUCTOR_PIDM),
         birthDate = dmy(BIRTH_DATE),
         termStartDate = TERM_START_DATE,
         termEndDate = TERM_END_DATE, 
         roundedGrade = gsub("\\+|\\-","", FINAL_GRADE),
         age = as.numeric(round(difftime(termStartDate, birthDate, units = "weeks")/52)),
         year = year(termStartDate), 
         year = gsub("20","",year),
         semester = if_else(month(termStartDate) == 8, "F",
                            if_else(month(termStartDate) == 5, "Su", "Sp")),
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
  mutate(yrSem = factor(yrSem), courseGrade = as.numeric(courseGrade),
         termDuration = difftime(termEndDate, termStartDate, units = "days"),
         oer = factor(if_else(termStartDate >= "2015-08-26" & COURSE_SUBJECT == "HIST", 1, 
                              if_else(termStartDate == "2016-01-11" & COURSE_SECTION  %in% c(paste0("0",c(12,14,18,22,23,28,35,76))) & COURSE_SUBJECT == "HIST",1,0))),
         pass = factor(if_else(roundedGrade <= "C",1,0)),
         dfw = factor(if_else(pass == 1, 0, 1)),
         academicYear = factor(if_else(yrSem %in% c("F 11", "Sp 12", "Su 12"), "11-12",
                                       if_else(yrSem %in% c("F 12", "Sp 13", "Su 13"), "12-13",
                                               if_else(yrSem %in% c("F 13", "Sp 14", "Su 14"), "13-14",
                                                       if_else(yrSem %in% c("F 14", "Sp 15", "Su 15"), "14-15",
                                                               if_else(yrSem %in% c("F 15", "Sp 16", "Su 16"), "15-16",
                                                                       if_else(yrSem %in% c("F 16", "Sp 17", "Su 17"), "16-17", "Check")))))))) %>% 
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
  mutate(ethnicity = factor(ethnicity), 
         semSinceImplementation = 
           ordered(factor(if_else(yrSem == "Su 14", 1, 
                   if_else(yrSem == "F 14",2,
                           if_else(yrSem == "Sp 15",3,
                                   if_else(yrSem == "Su 15",4,
                                           ifelse(yrSem == "F 15",5,
                                                  if_else(yrSem == "Sp 16",6,
                                                          if_else(yrSem == "Su 16",7, 
                                                                  if_else(yrSem == "F 16",8,
                                                                          if_else(yrSem == "Sp 17", 9, 0)))))))))))) %>% 
  select(-id)


## Order the year semester combination
#levels(SLCCpersistenceProc$yrSem)
## Summer 2011 is added. It needs to be removed
yrSemOrdered <- paste(rep(c("F", "Sp", "Su"), 6), c(11,rep(seq(12, 16, by = 1),each = 3), 17))
yrSemOrdered <- yrSemOrdered[-length(yrSemOrdered)]

SLCCpersistenceProc$yrSem <- factor(SLCCpersistenceProc$yrSem, levels = yrSemOrdered)

# Some students appear more than once in the dataset
dupeIDs <- SLCCpersistenceProc %>% 
  filter(duplicated(studentId)) %>% 
  distinct(studentId) %>% 
  unlist()

SLCCpersistenceProc <- SLCCpersistenceProc %>% 
  mutate(rep = if_else(studentId %in% dupeIDs,1,0))

oerPilotSections <- paste0("0", c(12,14,18,22,23, 28, 35, 76))

## We are trying to locate the pilot sections for fall 2015
## We know the  instructors for fall 2015 pilot sections
instructorsSpring  <- SLCCpersistenceProc %>% 
  filter(yrSem == "Sp 16" & courseSection %in% oerPilotSections) %>% 
  distinct(instructorId) %>% unlist()

instructorCourseCountSp16  <- SLCCpersistenceProc %>% 
  filter(yrSem == "Sp 16" & courseSection %in% oerPilotSections) %>% 
  group_by(instructorId) %>% 
  summarise(secTaught = n_distinct(courseSection)) %>%  
  arrange(instructorId)


# Find whether the instructors who taught the pilot sections in Spring 2016, also taught Fall 2015 
instructorFall <- SLCCpersistenceProc %>% 
  filter(yrSem == "F 15" & instructorId %in% instructorsSpring) %>% 
  distinct(instructorId) %>% 
  unlist()

instructorCourseCountF15 <- SLCCpersistenceProc %>% 
  filter(yrSem == "F 15" & instructorId %in% instructorsSpring) %>% 
  group_by(instructorId) %>% 
  summarise(secTaught = n_distinct(courseSection)) %>%  
  arrange(instructorId)
  
  

# remove original dataset
rm(SLCCpersistence)
