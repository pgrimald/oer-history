## Read in and preprocess SLCC dataset
## DBM, 06/14/17


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
packages.needed <- c('tidyverse', 'rapportools', 'RColorBrewer', 'prophet','scales','sjPlot', 'optimx', 'Rcpp','lme4', 'kableExtra','lmerTest','readxl', 'captioner','knitr','extrafont', 'lubridate', 'lettercase') 
suppressMessages(sapply(packages.needed, get.package))

## source ggplot template
source('~/Documents/oer-history/supportingScripts/ggplot2theme_min.R')


# Read in SLCC data -------------------------------------------------------
SLCC_11_18 <- read_excel('~/Box Sync/SLCC History/oer_data_update_201140-201820.xlsx')

# Read in legend
SLCC_legend <- read_csv('~/Box Sync/SLCC History/SLCC legend.csv')

# Preprocess dataset for modeling------------------------------------------------------
### Convert date fields to standardized format
### Compute Age
### Mark academic year
### Mark semesters
### Round off grades
### Mark passing grades (>C)
### Mark before/After OER
### The Open Ed program was started in the summer of 2014

SLCC_11_18_proc <- SLCC_11_18 %>% 
  mutate(studentId = factor(PIDM), 
         instructorId = factor(INSTRUCTOR_PIDM),
         birthDate = ymd(BIRTH_DATE),
         termStartDate = ymd(TERM_START_DATE),
         termEndDate = ymd(TERM_END_DATE)) %>% 
  mutate(roundedGrade = gsub("\\+|\\-","", FINAL_GRADE),
         age = as.numeric(round(difftime(termStartDate, birthDate, units = "weeks")/52)),
         year = year(termStartDate), 
         year = gsub("20","",year),
         semester = if_else(month(termStartDate) == 8, "F",
                            if_else(month(termStartDate) == 5, "Su", "Sp"))) %>% 
  unite(yrSem, semester, year, sep = " ", remove = FALSE) %>% 
  left_join(SLCC_legend[ ,c("Final Grade", "Points")], by = c("FINAL_GRADE" = "Final Grade")) %>% 
  left_join(SLCC_legend[ ,c("yrSem", "academicYear")], by = "yrSem") %>% 
  mutate(yrSem = factor(yrSem), courseGrade = as.numeric(Points), 
         termDuration = difftime(termEndDate, termStartDate, units = "days"),
                                 pass = factor(if_else(roundedGrade <= "C",1,0)),
                                 completion = factor(if_else(FINAL_GRADE == "I",0,1))) %>% 
  select(-TERM_START_DATE,-TERM_END_DATE,-BIRTH_DATE, -INSTRUCTOR_PIDM, -PIDM, -Points) 

# Convert names to camel case --------------------------------------------- 

## First conver to lower case
names(SLCC_11_18_proc)[is_upper(names(SLCC_11_18_proc))] <- tolower(names(SLCC_11_18_proc)[is_upper(names(SLCC_11_18_proc))])
## Then select names with underscores (_)
names(SLCC_11_18_proc)[grepl("_", names(SLCC_11_18_proc))] <- lapply(names(SLCC_11_18_proc)[grepl("_", names(SLCC_11_18_proc))], function(nm){
  nmNo_ <- unlist(strsplit(nm, "_"))
  ## Select the words other than the first word, and convert the first letter to uppercase
  followingWords <- paste0(unlist(lapply(strsplit(nmNo_[-1],""), function(w){
    tmpChar <- unlist(w)
    tmpChar[1] <- toupper(tmpChar[1])
    paste0(tmpChar, collapse = "")
  })), collapse = "")
  paste0(nmNo_[1], followingWords, collapse = "")
})



# Order the year semester combination -------------------------------------

#levels(SLCC_11_18_proc$yrSem)
## Summer 2011 is added. It needs to be removed
yrSemOrdered <- paste(rep(c("F", "Sp", "Su"), 7), c(11,rep(12:17, each = 3), 18))
yrSemOrdered <- yrSemOrdered[-length(yrSemOrdered)]

SLCC_11_18_proc$yrSem <- factor(SLCC_11_18_proc$yrSem, levels = yrSemOrdered)


# Duplicate students ------------------------------------------------------

# Some students appear more than once in the dataset
# They could have taken more than one course, or the same course more than once, or both
dupeIDs <- SLCC_11_18_proc %>% 
  filter(duplicated(studentId)) %>% 
  distinct(studentId) %>% 
  unlist()

# Add in a column to flag students who appear more than once in the dataset
SLCC_11_18_proc <- SLCC_11_18_proc %>% 
  mutate(rep = if_else(studentId %in% dupeIDs,1,0))

# Check the course attributes column by year
# SLCC_11_18_proc %>% 
#   group_by(courseAttributes) %>% 
#   distinct(yrSem) %>% View()

# Locate pilot instructors/sections/courses ------------------------------------------------
oerPilotSections <- paste0("0", c(12,14,18,22,23, 28, 35,76))

## We are trying to locate the pilot sections for fall 2015
## We know the course sections for the Sp 16 pilots
## Get instructor IDs from the sections
instructorsSpring  <- SLCC_11_18_proc %>% 
  filter(yrSem == "Sp 16" & courseSection %in% oerPilotSections) %>% 
  distinct(instructorId) %>% unlist()

## Look at how many courses were taught by each of these instructors
instructorCourseCountSp16  <- SLCC_11_18_proc %>% 
  filter(yrSem == "Sp 16" & courseSection %in% oerPilotSections) %>% 
  group_by(instructorId) %>% 
  summarise(secTaught = n_distinct(courseSection)) %>%  
  arrange(instructorId)


# Find whether the instructors who taught the pilot sections in Spring 2016, also taught Fall 2015 
# 5/6 instructors for Sp 16 taught F15
instructorFall <- SLCC_11_18_proc %>% 
  filter(yrSem == "F 15" & instructorId %in% instructorsSpring) %>% 
  distinct(instructorId) %>% 
  unlist()

## Found how many sections were taught by these instructors
## One of the Pilot instructors in F15 also taught 4 sections in F15
## Instructors c(3763, 651843) taught 4 sections each in F15 and are likely to be the pilot instructors
instructorCourseCountF15 <- SLCC_11_18_proc %>% 
  filter(yrSem == "F 15" & instructorId %in% instructorsSpring) %>% 
  group_by(instructorId) %>% 
  summarise(secTaught = n_distinct(courseSection)) %>%  
  arrange(instructorId) 

instructorF15 <- instructorCourseCountF15  %>% 
  filter(secTaught == 4) %>% 
  select(instructorId) %>% 
  unlist()

# Add in OER field --------------------------------------------------------
fullOERSems <- c("F 16", "Sp 17", "Su 17", "F 17", "Sp 18")
SLCC_11_18_proc <- SLCC_11_18_proc %>% 
  mutate(oer = if_else(courseSubject == "HIST" & yrSem == "Sp 16" & courseSection %in% oerPilotSections, 1,
                       if_else(courseSubject == "HIST" & yrSem == "F 15" & instructorId %in% instructorF15, 1, 
                               if_else(courseSubject == "HIST" & yrSem  %in% fullOERSems, 1, 
                                       if_else(courseSubject == "POLS" & !is.na(courseAttributes), 1, 0)))))



# remove original dataset
rm(SLCC_11_18)

