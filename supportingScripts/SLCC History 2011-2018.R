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
packages.needed <- c('tidyverse', 'rapportools', 'RColorBrewer', 'lsmeans' ,'scales','sjPlot', 
                     'optimx', 'Rcpp','lme4', 'kableExtra','lmerTest','readxl', 'captioner',
                     'knitr','extrafont', 'lubridate', 'lettercase', 'effsize') 
suppressMessages(sapply(packages.needed, get.package))


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

SLCC_11_18_proc$yrSem <- factor(SLCC_11_18_proc$yrSem, levels = yrSemOrdered)

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
  mutate(oer = factor(if_else(courseSubject == "HIST" & yrSem == "Sp 16" & courseSection %in% oerPilotSections, 1,
                       if_else(courseSubject == "HIST" & yrSem == "F 15" & instructorId %in% instructorF15, 1, 
                               if_else(courseSubject == "HIST" & yrSem  %in% fullOERSems, 1, 
                                       if_else(courseSubject == "POLS" & !is.na(courseAttributes), 1, 0))))))



# remove original dataset
rm(SLCC_11_18)

## Set up labels for plots
subjLab <- c(HIST = "History", POLS = "Political Science", ECON = "Economics")
oerLab <- c(`0` = "Traditional", `1` = "Open")
onlineLab <- c(`0` = "Classroom", `1` = "Online")


# Plot functions ----------------------------------------------------------
## Linear trends -----------------------------------------------------------

linearTrend <- function(df, v1 = "yrSem", v2 = "passRate", v3 = "oer", v4 = "courseSubject", v5 = "semPass", 
                        labx = "Year-Semester", laby = "Pass Rate", colLab = "Textbook type", colVal = oerLab, 
                        facetLab = subjLab, greyStart = .4, greyStop = .8, limy = c(0,1), legPos = "bottom", ltype = 2, 
                        lwidth = .25, wEbar = .1, xtickfsz = 7, ytickfsz = 7){
  # Create the upper and lower bounds for ebar plots
  mutate_call1 <- lazyeval::interp(~ a + b, a = as.name(v2), b = as.name(v5))
  mutate_call2 <- lazyeval::interp(~ a - b, a = as.name(v2), b = as.name(v5))
  # Plot 
  df %>%
    mutate_(.dots = setNames(list(mutate_call1, mutate_call2), nm = c("uB", "lB"))) %>% 
    ggplot(aes_string(x = v1, y = v2, color = v3, group = v3)) + 
    geom_line() + geom_point() + 
    geom_errorbar(aes_(ymin = ~lB, ymax = ~uB), width = wEbar) + 
    scale_colour_grey(start = greyStart, end = greyStop, labels = colVal) + theme_minimal() + 
    labs(x = labx, y = laby, colour = colLab) + scale_y_continuous(limits = limy) + 
    theme(legend.position = legPos, axis.text.x = element_text(size = xtickfsz), axis.text.y = element_text(size = ytickfsz)) +
    facet_wrap(v4) 
}

## Aggregated bar plots ----------------------------------------------------
## x = yrSem, facet = courseSubject, fill = oer
 
barPlotWEbar <- function(df, v1 = "semester", v2 = "passRate", v3 = "factor(oer)", v4 = "courseSubject", v5 = "semPass",
                         labx = "Semester", laby = "Pass Rate", colLab = "Textbook type", colVal = oerLab,
                         facetLab = subjLab, greyStart = .4, greyStop = .8, limy = c(0,1), legPos = "bottom",
                         ltype = 2, lwidth = .25, wEbar = .1, colPos = "dodge"){
  # Create the upper and lower bounds for ebar plots
  mutate_call1 <- lazyeval::interp(~ a + b, a = as.name(v2), b = as.name(v5))
  mutate_call2 <- lazyeval::interp(~ a - b, a = as.name(v2), b = as.name(v5))
  df %>%
    mutate_(.dots = setNames(list(mutate_call1, mutate_call2), nm = c("uB", "lB"))) %>%
    ggplot(aes_string(x = v1, y = v2, fill = v3)) +
    geom_col(position = colPos) +
    geom_errorbar(aes_(ymin = ~lB, ymax = ~uB),
                  width = wEbar, position = position_dodge(.9)) +
    theme_minimal() +
    scale_fill_grey(start = greyStart, end = greyStop, labels = colVal) +
    labs(x = labx, y = laby, fill = colLab) +
    theme(legend.position = legPos) +
    facet_wrap(v4) +
    scale_y_continuous(limits = limy)
}

## Aggregated line plots ---------------------------------------------------


