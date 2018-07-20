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
packages.needed <- c('tidyverse', 'rapportools', 'RColorBrewer', 'emmeans' ,'scales','sjPlot', 
                     'optimx', 'Rcpp','lme4', 'kableExtra','lme4','readxl', 'captioner',
                     'knitr','extrafont', 'lubridate', 'lettercase', 'effsize') 
suppressMessages(sapply(packages.needed, get.package))

# Read in SLCC data -------------------------------------------------------
SLCC_11_18 <- read_excel('~/Box Sync/oer-history/oer_data_update_201140-201820.xlsx')

# Read in legend
SLCC_legend <- read_csv('~/Box Sync/oer-history/SLCC legend.csv')

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
         completion = factor(if_else(FINAL_GRADE == "I",0,1)),
         onlineInd = factor(if_else(ONLINE_IND == "Y", 1, 0))) %>% 
  select(-TERM_START_DATE,-TERM_END_DATE,-BIRTH_DATE, -INSTRUCTOR_PIDM, -PIDM, -Points, -ONLINE_IND) 

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

SLCC_11_18_proc$yrSem <- factor(SLCC_11_18_proc$yrSem, levels = yrSemOrdered, ordered = TRUE)

# Duplicate students ------------------------------------------------------

# Some students appear more than once in the dataset
# They could have taken more than one course, or the same course more than once, or both
dupeIDs <- SLCC_11_18_proc %>% 
  group_by(courseSubject) %>% 
  filter(duplicated(studentId)) %>% 
  distinct(courseSubject, studentId) %>% 
  mutate(rep = 1)

# Add in a column to flag students who appear more than once in the dataset
# The rep variable tell us which students took the same course more than once
SLCC_11_18_proc <- SLCC_11_18_proc %>% 
  left_join(dupeIDs, by = c("studentId","courseSubject")) 

## Some students have taken a course more than once
## Compute number of attempts
SLCC_11_18_proc <- SLCC_11_18_proc %>% 
  arrange(studentId) %>% 
  group_by(rep, courseSubject, studentId) %>% 
  mutate(numAttempts = if_else(rep == 1, order(yrSem) - 1, 0)) 


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
  filter(courseSubject == "HIST" & yrSem == "Sp 16" & courseSection %in% oerPilotSections) %>% 
  ungroup() %>% 
  distinct(instructorId) %>% unlist()

## Look at how many courses were taught by each of these instructors
instructorCourseCountSp16  <- SLCC_11_18_proc %>% 
  filter(courseSubject == "HIST" & yrSem == "Sp 16" & courseSection %in% oerPilotSections) %>% 
  group_by(instructorId) %>% 
  summarise(secTaught = n_distinct(courseSection)) %>%  
  arrange(instructorId)



# From an inspection of the F15 syllabi we found that there were 
# no OER sections in Fall 15 -----------------------------------
# Find whether the instructors who taught the pilot sections in Spring 2016, also taught Fall 2015 
# 5/6 instructors for Sp 16 taught F15
# instructorFall <- SLCC_11_18_proc %>% 
#   filter(courseSubject == "HIST" & yrSem == "F 15" & instructorId %in% instructorsSpring) %>% 
#   ungroup() %>% 
#   distinct(instructorId) %>% 
#   unlist()

## Found how many sections were taught by these instructors
## One of the Pilot instructors in F15 also taught 4 sections in F15
## Instructors c(3763, 651843) taught 4 sections each in F15 and are likely to be the pilot instructors
# instructorCourseCountF15 <- SLCC_11_18_proc %>% 
#   filter(courseSubject == "HIST" & yrSem == "F 15" & instructorId %in% instructorsSpring) %>% 
#   group_by(instructorId) %>% 
#   summarise(secTaught = n_distinct(courseSection)) %>%  
#   arrange(instructorId) 
# 
# instructorF15 <- instructorCourseCountF15  %>% 
#   filter(secTaught == 4) %>% 
#   select(instructorId) %>% 
#   unlist()

# Add in OER field --------------------------------------------------------
fullOERSems <- c("F 16", "Sp 17", "Su 17", "F 17", "Sp 18")
# F15 has no oer sections
## Reorder course subjects
SLCC_11_18_proc <- SLCC_11_18_proc %>% 
  ungroup() %>% 
  mutate(oer = factor(if_else(courseSubject == "HIST" & yrSem == "Sp 16" & courseSection %in% oerPilotSections, "OER",
                       #if_else(courseSubject == "HIST" & yrSem == "F 15" & instructorId %in% instructorF15, 1, 
                               if_else(courseSubject == "HIST" & yrSem  %in% fullOERSems, "OER", 
                                       if_else(courseSubject == "POLS" & !is.na(courseAttributes), "OER", "No OER")))),
         `Textbook Type` = factor(if_else(oer == "OER", "Open", "Traditional"), levels = c("Traditional", "Open")),
         courseSubject = if_else(courseSubject == "HIST", "History", 
                                 if_else(courseSubject == "POLS", "Political Science", "Economics")),
         courseSubject = factor(courseSubject, levels = c("History", "Economics", "Political Science")),
         everPellEligibleInd = factor(if_else(everPellEligibleInd == "Y", "Pell Eligible", 
                                              if_else(everPellEligibleInd == "N", "Non-Pell Eligible", "NA"))),
         firstGenerationIndNA = as.numeric(if_else(firstGenerationInd == "U", "NA", 
                                                   if_else(firstGenerationInd == "Y", "1", "0"))),
         cpt1 = as.numeric(cpt1))


# Plot to check oer and number of students
# SLCC_11_18_proc %>%
#   ggplot(aes(x = yrSem, y = oer)) + geom_count(alpha = .2) +
#   theme_minimal() + facet_wrap(~courseSubject)

# remove original dataset
rm(SLCC_11_18)

# Write out processed dataset
# SLCC_11_18_proc %>% 
#   write_csv("~/Box Sync/oer-history/SLCC History 2011 - 2018.csv")

## POLS had OER starting from Spring 2018
## Since we are focusing on History OER, we will take POLS out
SLCC_11_18_proc <- SLCC_11_18_proc %>% 
  filter(yrSem != "Sp 18")



# Read in instructor info -------------------------------------------------

# instructorInfo <- read_excel("~/Box Sync/oer-history/Copy of Openstax HIST Instructor Info.xlsx")
# instructorInfo <- instructorInfo %>% 
#   mutate(instructorNm =  trimws(tolower(paste0(INSTRUCTOR_FIRST_NAME, INSTRUCTOR_LAST_NAME))))
# 
# instructorInfo %>% 
#   group_by(instructorNm, INSTRUCTOR_UID) %>% 
#   count() %>% View()

# Student Counts ----------------------------------------------------------
# count the number of distinct students
# sCount <- SLCC_11_18_proc %>% 
#   summarize(`unique students` = n_distinct(studentId)) %>% 
#   unlist()

# Get count of underage students
# underAge <- SLCC_11_18_proc %>% 
#   filter(age<18)

# Filter out underage students from main dataset
SLCC_11_18_proc <- SLCC_11_18_proc %>% 
  filter(age >= 18)

# Check if stuents repeated a class or had taken more than one courses or both
# sCourseRep <- SLCC_11_18_proc %>% 
#   filter(rep == 1) %>% 
#   group_by(studentId, courseSubject) %>% 
#   count() %>% 
#   group_by(studentId) %>% 
#   count() %>% 
#   ungroup() %>% 
#   count(nn)