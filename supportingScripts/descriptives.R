
# Source necessary files/packages -----------------------------------------
source("~/Documents/oer-history/supportingScripts/SLCC History 2011-2018.R")
source("~/Documents/oer-history/supportingScripts/SLCC Plot Functions.R")

# Demographic Variables ---------------------------------------------------

participantTable <- SLCC_11_18_proc %>% 
  group_by(courseSubject, `Textbook Type`, everPellEligibleInd) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(courseSubject, desc(`Textbook Type`)) %>% 
  select(everPellEligibleInd, n)


## Ethnicity ---------------------------------------------------------------

### Ethnicity x Course Subject x OER ----------------------------------------
# Ethnicity count
ethnicityCount <- SLCC_11_18_proc %>%
  distinct(studentId, .keep_all = TRUE) %>%
  count(ethnicity) %>%
  mutate(percRace = round(n / sum(n) * 100)) %>% 
  ggplot(aes(x = reorder(ethnicity, percRace), y = percRace)) +
  geom_col() + theme_minimal() + coord_flip() +
  labs(x = "Ethnicity", y = "Percentage of Students") 

# Ethnicity Pass
ethnicityPass <- SLCC_11_18_proc %>% 
#  distinct(studentId, .keep_all = TRUE) %>% 
  add_count(ethnicity, `Textbook Type`, courseSubject) %>% 
  group_by(ethnicity, `Textbook Type`, courseSubject, n) %>% 
  summarise(numPass = sum(pass == 1), 
            passRate = numPass/unique(n))
            # sdPass = sd(passRate, na.rm = TRUE),
            # semPass = sdPass/sqrt(unique(n))) %>% View()

# Ethnicity Grade
ethnicityAvgGrade <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(ethnicity, `Textbook Type`, courseSubject) %>% 
  summarise(mGrade = mean(courseGrade, na.rm = TRUE),
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n))

### Plot

#### Pass (this plot is particularly uninformative)
ethnicitypass <- ethnicityPass %>% 
  ggplot(aes(x = reorder(ethnicity, passRate), y = passRate, fill = `Textbook Type`)) + 
  geom_col(position = "dodge") +
  facet_wrap(~courseSubject) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_grey(start = .4, end = .8, na.value = "blue") +
  labs(x = "Ethnicity", y = "Pass rate", fill = "Textbook Type") +
  # geom_errorbar(aes(ymin = mGrade - semGrade, ymax = mGrade + semGrade), 
  #               width = .1, position = position_dodge(.9)) +
  coord_flip()

#### Avg Grade
ethnicityAvgGradePlt <- ethnicityAvgGrade %>% 
  ggplot(aes(x = reorder(ethnicity, mGrade), y = mGrade, fill = `Textbook Type`)) + 
  geom_col(position = "dodge") +
  facet_wrap(~courseSubject) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_grey(start = .4, end = .8, na.value = "blue") +
  labs(x = "Ethnicity", y = "Mean Grade", fill = "Textbook Type") +
  geom_errorbar(aes(ymin = mGrade - semGrade, ymax = mGrade + semGrade), 
                width = .1, position = position_dodge(.9)) +
  coord_flip()



# Gender ------------------------------------------------------------------

gender <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  count(gender) %>% 
  mutate(genderPerc = round(n/sum(n) * 100, 2))


## Pell Status -------------------------------------------------------------

### Pell Status x Course Subject x OER ----------------------------------------

pell <- SLCC_11_18_proc %>%
  distinct(studentId, .keep_all = TRUE) %>%
  count(everPellEligibleInd) %>% 
  mutate(pell = round(n/sum(n) * 100))

pellStatusPass <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(everPellEligibleInd, pass, `Textbook Type`, courseSubject) %>% 
  count() 

pellStatusAvgGrade <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(everPellEligibleInd, `Textbook Type`, courseSubject) %>% 
  summarise(mGrade = mean(courseGrade, na.rm = TRUE),
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n))

### Plot

#### Pass (this plot is particularly uninformative)

#### Avg Grade
(pellStatusAvgGradeBarPlt <- pellStatusAvgGrade %>% 
  ggplot(aes(x = reorder(everPellEligibleInd, -mGrade), y = mGrade, fill = `Textbook Type`)) + 
  geom_col(position = "dodge") +
  facet_wrap(~courseSubject) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  scale_fill_grey(start = .4, end = .7, na.value = "blue") +
  labs(x = "Pell Status", y = "Mean Grade", fill = "Textbook Type") +
  geom_errorbar(aes(col = `Textbook Type`, ymin = mGrade - semGrade, ymax = mGrade + semGrade), 
                width = .1, position = position_dodge(.9)) +
  scale_color_grey(start = .4, end = .7, na.value = "blue") +  
  scale_y_continuous(limits = c(0,4)) +
  guides(label = "none", color = "none") +  
  geom_text(aes(label = paste0("n = ",n), y = 0.1), 
            col = "white", fontface = "bold",lwd = 3.5, position = position_dodge(.9)))
# Point plot

# (pellStatusAvgGradePtPlt <- pellStatusAvgGrade %>% 
#     ggplot(aes(x = reorder(everPellEligibleInd, -mGrade), y = mGrade, col = `Textbook Type`)) + 
#     geom_point(aes(size = n)) +
#     #geom_col(position = "dodge") +
#     facet_wrap(~courseSubject) +
#     theme_minimal() +
#     theme(legend.position = "bottom") +
#     scale_color_grey(start = .4, end = .8, na.value = "blue") +
#     labs(x = "Pell Status", y = "Mean Grade", fill = "OER") +
#     geom_errorbar(aes(ymin = mGrade - semGrade, ymax = mGrade + semGrade), 
#                   width = .1))

#grid.arrange(pellStatusAvgGradeBarPlt, pellStatusAvgGradePtPlt)
## Registration Status (FT/PT) ---------------------------------------------------------------

### Registration Status x Course Subject x OER ----------------------------------------
# Reg status count
regStatus <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  count(fullTime) %>% 
  mutate(enroll = round(n/sum(n) * 100))
# summarise(FullTime = round(sum(fullTime == "Full-Time")/n() * 100), 
#          PartTime = round(sum(fullTime == "Part-Time")/n() * 100))

# Reg status pass
regStatusPass <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(fullTime, pass, `Textbook Type`, courseSubject) %>% 
  count() 

# Reg status grade
regStatusAvgGrade <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(fullTime, `Textbook Type`, courseSubject) %>% 
  summarise(mGrade = mean(courseGrade, na.rm = TRUE),
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n))


### Plot

#### Pass (this plot is particularly uninformative)

#### Avg Grade
(regStatusAvgGradePlt <- regStatusAvgGrade %>% 
    ggplot(aes(x = reorder(fullTime, -mGrade), y = mGrade, fill = `Textbook Type`)) + 
    geom_col(position = "dodge") +
    facet_wrap(~courseSubject) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_fill_grey(start = .4, end = .7, na.value = "blue") +
    labs(x = "Registration Status", y = "Mean Grade", fill = "Textbook Type") +
    geom_errorbar(aes(col = `Textbook Type`, ymin = mGrade - semGrade, ymax = mGrade + semGrade), 
                  width = .1, position = position_dodge(.9)) +
    scale_color_grey(start = .4, end = .7, na.value = "blue") +  
    scale_y_continuous(limits = c(0,4)) +
    guides(label = "none", color = "none") +  
    geom_text(aes(label = paste0("n = ",n), y = 0.1), 
              col = "white", fontface = "bold",lwd = 3.5, position = position_dodge(.9)))


## First Generation --------------------------------------------------------

### First Gen Status x Course Subject x OER ----------------------------------------

# First gen count
firstGen <- SLCC_11_18_proc %>%
  distinct(studentId, .keep_all = TRUE) %>%
  count(firstGenerationInd) %>% 
  mutate(firstGen = round(n/sum(n) * 100))
# summarise(yes = round(sum(firstGenerationInd == "Y")/n() * 100), 
#           no = round(sum(firstGenerationInd == "N")/n() * 100),
# unReported = round(sum(firstGenerationInd == "U")/n() * 100))

# First gen pass
firstGenPass <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(firstGenerationInd, pass, `Textbook Type`, courseSubject) %>% 
  count() 

# # First gen grade
firstGenAvgGrade <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(firstGenerationInd, `Textbook Type`, courseSubject) %>% 
  summarise(mGrade = mean(courseGrade, na.rm = TRUE),
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n))

### Plot

#### Pass (this plot is particularly uninformative)

#### Avg Grade
(firstGenAvgGradePlt <- firstGenAvgGrade %>% 
    ggplot(aes(x = reorder(firstGenerationInd, -mGrade), y = mGrade, fill = `Textbook Type`)) + 
    geom_col(position = "dodge") +
    facet_wrap(~courseSubject) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(x = "First Generation", y = "Mean Grade", fill = "Textbook Type") +
    geom_errorbar(aes(col = `Textbook Type`, ymin = mGrade - semGrade, ymax = mGrade + semGrade), 
                  width = .1, position = position_dodge(.9)) +
    scale_fill_grey(start = .4, end = .7, na.value = "blue") +  
    scale_color_grey(start = .4, end = .7, na.value = "blue") +  
    scale_y_continuous(limits = c(0,4)) +
    guides(label = "none", color = "none") +  
    geom_text(aes(label = paste0("n = ",n), y = 0.1), 
              col = "white", fontface = "bold",lwd = 3.5, position = position_dodge(.9)))

# Prior Performance -------------------------------------------------------

### Prior Performance x Course Subject x OER ----------------------------------------

### Cumulative UG GPA
(gpaAvgGrade <- SLCC_11_18_proc %>%
  distinct(studentId, .keep_all = TRUE) %>%
  filter(!is.na(cumUgGpa)) %>% 
  #group_by(cumUgGpa, `Textbook Type`, courseSubject) %>%
  ggplot(aes(x = cumUgGpa, y = courseGrade, colour = `Textbook Type`)) + 
    geom_point(alpha = .5) + 
    scale_colour_grey(start = .4, end = .8) + 
    theme_minimal() + 
    facet_wrap(~courseSubject+`Textbook Type`) + 
    theme(legend.position = "bottom") +
    labs(x = "Cumulative undergraduate GPA (rounded)", 
         y = "Course Grade"))
  
# summarise(mGrade = mean(courseGrade, na.rm = TRUE),
#             n = sum(!is.na(unique(studentId))),
#             semGrade = sd(courseGrade)/sqrt(n))



# Course Factors ----------------------------------------------------------

### Online x Course Subject x OER ----------------------------------------

onlinePass <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(onlineInd, pass, `Textbook Type`, courseSubject) %>% 
  count() 

onlineAvgGrade <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(onlineInd, `Textbook Type`, courseSubject) %>% 
  summarise(mGrade = mean(courseGrade, na.rm = TRUE),
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n))


### Plot

#### Pass (this plot is particularly uninformative)

#### Avg Grade
(onlineAvgGradePlt <- onlineAvgGrade %>% 
    ggplot(aes(x = onlineInd, y = mGrade, fill = `Textbook Type`)) + 
    geom_col(position = "dodge") +
    facet_wrap(~courseSubject) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(x = "Course Delivery", y = "Mean Grade", fill = "Textbook Type") +
    scale_x_discrete(labels = c("Classroom", "Online")) +
    geom_errorbar(aes(ymin = mGrade - semGrade, ymax = mGrade + semGrade, col = `Textbook Type`), 
                  width = .1, position = position_dodge(.9)) +
    scale_fill_grey(start = .4, end = .7, na.value = "blue") +  
    scale_color_grey(start = .4, end = .7, na.value = "blue") +  
    scale_y_continuous(limits = c(0,4)) +
    guides(label = "none", color = "none") +  
    geom_text(aes(label = paste0("n = ",n), y = 0.1), 
              col = "white", fontface = "bold",lwd = 3.5, position = position_dodge(.9)))

# Semester x Subject ------------------------------------------------------
# Pass x Subject
(passAggOer <- SLCC_11_18_proc %>% 
   add_count(courseSubject, `Textbook Type`) %>% 
   group_by(courseSubject, `Textbook Type`, n) %>% 
   summarise(numPass = sum(pass == 1), passRate = numPass/unique(n)) %>% 
   #mutate(semPass = sd(passRate)/sqrt(unique(n))) %>% 
   ggplot(aes(x = courseSubject, y = passRate, fill = `Textbook Type`)) + 
   geom_col(position = "dodge") + 
   #geom_errorbar(aes(col = oer, ymin = passRate - semPass, ymax = passRate + semPass), width = .1, position = position_dodge(.9)) +
   theme_minimal() + 
   theme(legend.position = "bottom") + 
   guides(color = "none") +
   scale_color_grey(start = .4, end = .8) +
   scale_fill_grey(start = .4, end = .8) + 
   scale_y_continuous(limits = c(0,1)) +
   labs(x = "Course Subject", fill = "Textbook Type", y = "Pass Rate"))

# Sem x Pass
# passAggSem <- SLCC_11_18_proc %>% 
#   add_count(courseSubject, oer, semester) %>% 
#   group_by(courseSubject, oer, semester, n) %>% 
#   summarise(numPass = sum(pass == 1), passRate = numPass/unique(n), semPass = passRate/unique(n)) 
# 
# (passAggSemPlot <- barPlotWEbar(df = passAggSem, v2 = "passRate", v5 = "semPass", 
#                              laby = "Pass Rate", colLab = "Textbook Type", limy = c(0,1)))
# 

# Grade x Subject
# pass rate aggregate plot, fig.cap="Average grades across textbook types, semesters, subject."}
avgGradeAgg <- SLCC_11_18_proc %>% 
  group_by(courseSubject, `Textbook Type`, semester) %>%
  summarise(mGrade = mean(courseGrade, na.rm = TRUE), 
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n)) 

# Grade x Subject across semesters
(avgGradeAggPlt <- avgGradeAgg %>% 
  ggplot(aes(x = semester, y = mGrade, fill = `Textbook Type`)) + 
    geom_col(position = "dodge") + 
  geom_errorbar(aes(col = `Textbook Type`, ymin = mGrade - semGrade, ymax = mGrade + semGrade), width = 0.1, position = position_dodge(.9)) + 
  facet_wrap(~courseSubject) +
  theme_minimal() +
  scale_fill_grey(start = .4, end = .8, labels = c("Traditional", "Open")) +  
  scale_color_grey(start = .4, end = .8) +  guides(color = "none") +
  labs(x = "Semester", y = "Mean Grade", fill = "Textbook Type") + theme(legend.position = "bottom") +  
    geom_text(aes(label = paste0("n = ",n), y = 0.1), 
              col = "white", fontface = "bold",lwd = 3.5, position = position_dodge(.9)))


# Pilot period ------------------------------------------------------------

# pass rate pilot period, fig.cap="Difference in pass rate between courses using traditional and open textbooks over the pilot semesters."}
# History was piloted during Sp 16
pilotSems <- "Sp 16" #c("F 15", "Sp 16")
# Change OER labels
labels <- c(`1` = "Used Open textbook", `0` = "Used Traditional Textbook")

# (histPilotPassPlot <- passRate %>% 
#     filter(courseSubject == "History" & yrSem %in% pilotSems) %>% 
#     ggplot(aes(x = oer, y = passRate, fill = oer)) + 
#     geom_col() + 
#     theme_minimal() + 
#     scale_x_discrete(labels = c("Traditional Textbook", "Open Textbook")) +
#     labs(y = "Pass Rate", x = "Textbook Type") + 
#     theme(legend.position = "bottom") +
#     geom_errorbar(aes(col = oer, ymin = passRate - semPass, ymax = passRate + semPass), position = position_dodge(0.9), width = .1) +
#     scale_fill_grey(start = .4, end = 0.8, na.value = "blue") +
#     scale_colour_grey(start = .4, end = 0.8, na.value = "blue") +
#     scale_y_continuous(limits = c(0,1)) + 
#     geom_text(aes(label = paste0("n = ",n), y = 0.1), 
#               col = "white", fontface = "bold",lwd = 3.5, position = position_dodge(.9)))
# 


# Mode of course delivery: Online vs Classroom ----------------------------
## Pass rate ---------------
# passRateOnline <- SLCC_11_18_proc %>% 
#   add_count(courseSubject, `Textbook Type`, onlineInd) %>% 
#   ungroup() %>% 
#   group_by(courseSubject, `Textbook Type`, onlineInd, n) %>%
#   summarise(numPass = sum(pass == 1), passRate = numPass/unique(n)) #, semPass = passRate/unique(n)
# 
# passRateOnlinePlt <- passRateOnline %>% 
#   ggplot(aes(x = onlineInd, y = passRate, fill = `Textbook Type`)) +  
#   geom_col(position = "dodge") + theme_minimal() + 
#   facet_wrap(~courseSubject) + 
#   labs(x = "Course Type", y = "Pass Rate", fill = "Textbook Type") +
# #  geom_errorbar(aes(ymin = passRate - semPass, ymax = passRate + semPass, colour = oer), 
# #                width = .1, position = position_dodge(0.9)) + 
#   scale_y_continuous(limits = c(0,1)) +
#   scale_x_discrete(labels = c("Classroom", "Online")) +
#   scale_fill_grey(start = .4, end = .8, labels = c("Traditional", "Open")) + 
#   scale_color_grey(start = .4, end = .8) + 
#   theme(legend.position = "bottom") + guides(color = "none") +  
#   geom_text(aes(label = paste0("n = ",n), y = 0.1), 
#             col = "white", fontface = "bold",lwd = 3.5, position = position_dodge(.9))


# Avg Grade
# (avgGradeOnline <- SLCC_11_18_proc %>% 
#     group_by(courseSubject, `Textbook Type`,onlineInd) %>%
#     summarise(mGrade = mean(courseGrade, na.rm = TRUE), 
#               n = sum(!is.na(unique(studentId))),
#               semGrade = sd(courseGrade)/sqrt(n)) %>% 
#     ggplot(aes(x = onlineInd, y = mGrade, fill = `Textbook Type`)) + 
#     geom_col(position = "dodge") + 
#     theme_minimal() + 
#     facet_wrap(~courseSubject) + 
#     labs(x = "Course Type", y = "Mean Grade", fill = "Textbook Type") +
#     geom_errorbar(aes(ymin = mGrade - semGrade, ymax = mGrade + semGrade, colour = `Textbook Type`), 
#                   width = .2, position = position_dodge(0.9)) + 
#     scale_y_continuous(limits = c(0,4)) +
#     scale_x_discrete(labels = c("Classroom", "Online")) +
#     scale_fill_grey(start = .4, end = .8) + 
#     scale_color_grey(start = .4, end = .8) + theme(legend.position = "bottom") + 
#     guides(color = "none") +  
#     geom_text(aes(label = paste0("n = ",n), y = 0.1), 
#               col = "white", fontface = "bold",lwd = 3.5, position = position_dodge(.9)))


# Year-Semester -----------------------------------------------------------

### Pass rate ---------------------------------------------------------------
# passRateYrSem <- SLCC_11_18_proc %>% 
#   add_count(courseSubject, oer, yrSem) %>% 
#   ungroup() %>% 
#   group_by(courseSubject, oer, yrSem, n) %>%
#   summarise(numPass = sum(pass == 1), passRate = numPass/unique(n))  #, semPass = passRate/unique(n)
# 
# 
# 
# # Effect size
# idx <- passRateYrSem$courseSubject == "History"
# dPassOER <- cohen.d(passRateYrSem$passRate[idx],passRate$oer[idx], na.rm = TRUE)
# 
# ## Pass plot
# passLPlot <- linearTrend(df = passRateYrSem, v1 = "yrSem", v2 = "passRate", v3 = "oer", v4 = "courseSubject", v5 = "semPass", labx = "Year-Semester", laby = "Pass Rate") + 
#     geom_vline(aes(xintercept = which(levels(yrSem) %in% "Sp 16")), lty = 2, lwd = .25)


### Average Grade -----------------------------------------------------------

# r oer pass, fig.cap="Overall effect of OER on average grade across different courses"
(gradeOer <- SLCC_11_18_proc %>% 
    group_by(courseSubject, `Textbook Type`) %>% 
    summarise(mGrade = mean(courseGrade, na.rm = TRUE), 
              n = sum(!is.na(unique(studentId))),
              semGrade = sd(courseGrade)/sqrt(n)) %>%  
    ggplot(aes(x = courseSubject, y = mGrade, fill = `Textbook Type`)) + geom_col(position = "dodge") + 
    geom_errorbar(aes(col = `Textbook Type`, ymin = mGrade - semGrade, ymax = mGrade + semGrade), width = .1, position = position_dodge(.9)) +
    theme_minimal() + theme(legend.position = "bottom") + 
    scale_colour_grey(start = .4, end = .8) +
    scale_fill_grey(start = .4, end = .8) + 
    scale_y_continuous(limits = c(0,4)) +
    guides(col = "none") +
    labs(x = "Course Subject", fill = "Textbook Type", 
         y = "Mean Grade"))


# Grade x yr x Sem
avgGrade <- SLCC_11_18_proc %>% 
  group_by(courseSubject, `Textbook Type`, yrSem) %>%
  summarise(mGrade = mean(courseGrade, na.rm = TRUE), 
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n)) 

# Effect size
gIdx <- avgGrade$courseSubject == "History"
dAvgGradeOER <- cohen.d(avgGrade$mGrade[gIdx],avgGrade$`Textbook Type`[gIdx], na.rm = TRUE)

# Line Plot

(agLPlot <- linearTrend(df = avgGrade, v1 = "yrSem", v2 = "mGrade",
                        v3 = "`Textbook Type`", v4 = "courseSubject", 
                        v5 = "semGrade", colVal = oerLab,labx = "Year-Semester", 
                        laby = "Mean grade", limy = c(0,4)) +
    geom_vline(aes(xintercept = which(levels(yrSem) %in% "Sp 16")), lty = 2, lwd = .25))


# Pilot Period ------------------------------------------------------------

(histAvgGrade <- avgGrade %>% 
   filter(courseSubject == "History" & yrSem == "Sp 16") %>% 
   ggplot(aes(x = yrSem, y = mGrade, fill = `Textbook Type`, colour = `Textbook Type`)) + 
   geom_col(position = "dodge") + guides(color = "none") +
   theme_minimal() + theme(legend.position = "bottom") +
   scale_fill_grey(start = .4, end = .8) + 
   scale_color_grey(start = .4, end = .8) +
   labs(x = "Semester Year", y = "Mean Grade", fill = "Textbook Type") + 
   geom_errorbar(aes(ymin = mGrade - semGrade, ymax = mGrade + semGrade, 
                     colour = `Textbook Type`), width = .1, position = position_dodge(.9)) + 
   scale_y_continuous(limits = c(0,4)))

# Course Completion -------------------------------------------------------

# Completion is near ceiling
completion <- SLCC_11_18_proc %>% 
  group_by(courseSubject) %>% 
  summarize(completion = sum(completion == 1), n = n(studentId), 
            completionrate = completion/n)

# Incomplete grades by course
incomplete <- SLCC_11_18_proc %>% 
  filter(finalGrade == "I") %>% 
  group_by(courseSubject) %>% 
  count() 



# Omnibus Models ------------------------------------------------------------------

## Pass Rate Logistic Models -----------------------------------------------
### Base Model
passM0 <- SLCC_11_18_proc %>% 
  lme4::glmer(pass ~ (1|courseSubject/instructorId) + (1|academicYear) + (1|semester) + 
          everPellEligibleInd + firstGenerationInd + fullTime + 
          cumUgGpa + onlineInd, family = binomial, data=., 
        control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(passM0)
#emmeans(passM0, ~onlineInd)

#emmip(passM0, ~onlineInd)

### Additive treatment model
passM1 <- SLCC_11_18_proc %>% 
  lme4::glmer(pass ~ (1|courseSubject/instructorId) + (1|academicYear) + (1|semester) + 
          everPellEligibleInd +  firstGenerationInd + fullTime + 
          cumUgGpa + onlineInd + `Textbook Type`, family = binomial, 
        data=., control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

summary(passM1)

# Find the least squares mean
passPmmeans <- emmeans(passM1, ~`Textbook Type`)
# emmip(passM1, onlineInd ~ `Textbook Type`)

# Anova
passAnova <- anova(passM1)


# Model comparison
passComp <- anova(passM0, passM1)

# Plot random effects
#passM1RE <- sjp.glmer(passM1, type = "re") + theme_minimal()

### Interactive treatment model
passM2 <- SLCC_11_18_proc %>% 
  lme4::glmer(pass ~ (1|courseSubject/instructorId) + (1|academicYear) + (1|semester) + 
        everPellEligibleInd + firstGenerationInd + fullTime +
        cumUgGpa + onlineInd + `Textbook Type` + 
          `Textbook Type`:everPellEligibleInd + `Textbook Type`:onlineInd, family = binomial, 
        control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)), data=.)

summary(passM2)

# Anova
passAnova2 <- anova(passM2)

# passM2means <- emmeans(passM2, ~ `Textbook Type` * everPellEligibleInd)
# emmip(passM2, everPellEligibleInd ~ `Textbook Type`)
# Plot random effects
# passM2RE <- sjp.glmer(passM2, type = "re") + theme_minimal()


passAIC <- AIC(passM0, passM1, passM2)

# Comparing additive and interactive models
anova(passM1, passM2)


## Average Grade Models -----------------------------------------------
# emm_options(pbkrtest.limit = 3000, lmerTest.limit = 3000)


### Base Model
gradeM0 <- SLCC_11_18_proc %>% 
  lme4::lmer(courseGrade ~ (1|courseSubject/instructorId) + (1|academicYear) + (1|semester) + 
          everPellEligibleInd + fullTime + 
          cumUgGpa + onlineInd, data=.)
#, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
# optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))
summary(gradeM0)
# emmeans(gradeM0, ~onlineInd)
# 
# emmip(gradeM0, ~onlineInd)
# ref_grid(passM0) @ grid %>% View()

### Additive treatment model
gradeM1 <- SLCC_11_18_proc %>% 
  lme4::lmer(courseGrade ~ (1|courseSubject/instructorId) + (1|academicYear) + (1|semester) + 
         everPellEligibleInd + fullTime +
         cumUgGpa + onlineInd + `Textbook Type`, data=.)
# , control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
#                         optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))


summary(gradeM1)



# Find the least squares mean
gradepmM1 <- emmeans(gradeM1, ~`Textbook Type`)



emmip(gradeM1,~`Textbook Type`) +
  theme_minimal() + scale_color_grey(start = .4, end = .8)

# Anova
gradeAnova <- anova(gradeM1)


# Model comparison
gradeComp <- anova(gradeM0, gradeM1)

# Plot random effects
#gradeM1RE <- sjp.glmer(gradeM1, type = "re") + theme_minimal()



# grade interactive model
# Interactive treatment model
gradeM2 <- SLCC_11_18_proc %>% 
  lme4::lmer(courseGrade ~ (1|courseSubject/instructorId) + (1|academicYear) + (1|semester) + 
         everPellEligibleInd + fullTime  + 
         cumUgGpa + onlineInd + `Textbook Type` + 
         `Textbook Type`:everPellEligibleInd, data=.)

        # control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
        #                        optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

summary(gradeM2)




# Anova
gradeAnova2 <- anova(gradeM2)

gradeM2means <- as.data.frame(emmeans(gradeM2, ~ `Textbook Type`* everPellEligibleInd)) %>% 
  mutate(stat = "Adjusted Means")

names(gradeM2means)[3:4] <- c("mGrade", "semGrade")

# Create plot with emm and raw marginal means
mMaster <- pellStatusAvgGrade %>% 
  filter(courseSubject == "History") %>% 
  mutate(stat = "Unadjusted Means") %>% 
  bind_rows(gradeM2means) %>% 
  mutate(courseSubject = "History", 
         stat = factor(stat, levels = c("Unadjusted Means", "Adjusted Means")))
  
# Contrasting unadjusted vs. adjusted marginal means
(mMasterPlot <- mMaster %>% 
  ggplot(aes(x = reorder(everPellEligibleInd, -mGrade), y = mGrade, fill = `Textbook Type`)) + 
    geom_col(position = "dodge", col = "black") +
    facet_wrap(~stat) +
    theme_bw() +
    theme(legend.position = "bottom", strip.text.x = element_text(size = 12), 
          axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 12),
          axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 12), legend.title = element_text(size = 14)) + 
    scale_fill_manual(values = c("grey60","white")) +
    labs(x = "Pell Status", y = "Mean Grade", fill = "Textbook Type") +
    geom_errorbar(aes(ymin = mGrade - semGrade, ymax = mGrade + semGrade), 
                  width = .1, position = position_dodge(.9)) +
    #scale_color_grey(start = .1, end = .7, na.value = "blue") +  
    scale_y_continuous(limits = c(0,4)) +
  scale_x_discrete(labels = c("Non-Pell Eligible", "Pell Eligible")) +
    guides(label = "none", color = "none"))


emmip(gradeM2, `Textbook Type` ~ everPellEligibleInd) +
  theme_minimal() + 
  scale_color_grey(start = .4, end = .8) + 
  scale_y_continuous(limits = c(0,4)) + 
  theme(legend.position = "bottom")

# Plot random effects
#gradeM2RE <- sjp.glmer(gradeM2, type = "re") + theme_minimal()


# grade model comparison
AIC(gradeM0, gradeM1, gradeM2)
anova(gradeM1, gradeM2)




# History Models ------------------------------------------------------------------
## Filter out only history data
history <- SLCC_11_18_proc %>% 
  filter(courseSubject == "History")
# Effect sizes ------------------------------------------------------------
# function to extract effect size estimate and confidence intervals
theD <- function(d){
  data.frame(`Effect Size *` = round(d$estimate,2),
             `Confidence Interval` = paste0("[",formatC(round(d$conf.int[1],2), format='f', digits=2),",",formatC(round(d$conf.int[2],2), format='f', digits=2),"]"), check.names = FALSE)    
  #paste0(round(d$estimate,2)," [",round(d$conf.int[1],2),",",round(d$conf.int[2],2),"]")
}
# Create effect size table
create.es.table <- function(es.table){
  es.table %>% 
    kable(., format = "latex", linesep = "",caption = paste0("Effect size estimates (Cohen's d) showing non-significant group differences between students using traditional and open textbooks in History."), booktabs = TRUE,
          col.names = c("","Tutor","Control","Cohen's d[note]", "95% Conf. Int."),
          align = c("l", rep("r", 3))) %>%
    kable_styling(full_width = FALSE) %>%
    kableExtra::add_footnote(label = c("Standardized mean difference."),notation = "symbol") #95% confidence interval in brackets.
}

cGrade.d <- cohen.d(history$courseGrade, history$`Textbook Type`, na.rm = TRUE)
pell.d <- cohen.d(as.numeric(history$everPellEligibleInd), history$`Textbook Type`, na.rm = TRUE)
cumUg.d <- cohen.d(history$cumUgGpa, history$`Textbook Type`, na.rm = TRUE)
courseTyp.d <- cohen.d(as.numeric(history$onlineInd), history$`Textbook Type`, na.rm = TRUE)
regStat.d <- cohen.d(as.numeric(as.factor(history$fullTime)), history$`Textbook Type`, na.rm = TRUE)

effS <-bind_rows(theD(cGrade.d), theD(pell.d), theD(cumUg.d), theD(courseTyp.d), theD(regStat.d))
effS <- data.frame(check.names = FALSE, 
                   Factor = c("Course Grade", "Pell Eligibility", "Cumulative UG GPA", "Course Type", 
                              "Registration Status"), effS)
## Pass Rate Logistic Models -----------------------------------------------



### Base Model
passHM0 <- history %>% 
  lme4::glmer(pass ~ (1|instructorId) + (1|academicYear) + (1|semester) + 
          everPellEligibleInd + firstGenerationInd + fullTime + 
          cumUgGpa + onlineInd, family = binomial, 
          REML = FALSE, data=.)
#,control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
#                       optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))
summary(passHM0)
# emmeans(passHM0, ~everPellEligibleInd)
# 
# emmip(passHM0, ~onlineInd)

### Additive treatment model
passHM1 <- history %>% 
  lme4::glmer(pass ~ (1|instructorId) + (1|academicYear) + (1|semester) + 
          everPellEligibleInd + firstGenerationInd + fullTime + 
          cumUgGpa + onlineInd + `Textbook Type`, family = binomial, 
        REML = FALSE, data=.)
# , 
#         control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
#                                optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(passHM1)

# Find the least squares mean
# passHPmmeans <- emmeans(passHM1, ~`Textbook Type`)
# emmip(passHM1, onlineInd ~ `Textbook Type`)

# Anova
passAnova <- anova(passHM1)


# Model comparison
passHComp <- anova(passHM0, passHM1)

# Plot random effects
#passHM1RE <- sjp.glmer(passHM1, type = "re") + theme_minimal()

### Interactive treatment model
passHM2 <- history %>% 
  lme4::glmer(pass ~ (1|instructorId) + (1|academicYear) + (1|semester) + 
           everPellEligibleInd + firstGenerationInd + fullTime + 
           cumUgGpa + onlineInd + `Textbook Type` + 
          `Textbook Type`:everPellEligibleInd + `Textbook Type`:onlineInd, 
        family = binomial, 
        REML = FALSE, data=.)
        # , control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
        #                        optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

summary(passHM2)

# Anova
passHAnova2 <- anova(passHM2)

# passHM2means <- emmeans(passHM2, ~ `Textbook Type` * everPellEligibleInd)
# emmip(passHM2, `Textbook Type` ~ everPellEligibleInd)
# Plot random effects
#passM2RE <- sjp.glmer(passM2, type = "re") + theme_minimal()

## Average Grade Models -----------------------------------------------
### Base Model
gradeHM0 <- history %>% 
  lme4::lmer(courseGrade ~ (1|instructorId) + (1|academicYear) + (1|semester) + 
         everPellEligibleInd + firstGenerationInd + fullTime + 
         cumUgGpa + onlineInd, 
         REML = FALSE, data=.)
       # , 
       # control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
       #                                           optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(gradeHM0)
# emmeans(gradeHM0, ~`Textbook Type`)

# emmip(gradeHM0, ~`Textbook Type`)
# ref_grid(passM0) @ grid %>% View()

### Additive treatment model
gradeHM1 <- history %>% 
  lme4::lmer(courseGrade ~ (1|instructorId) + (1|academicYear) + (1|semester) + 
         everPellEligibleInd +  firstGenerationInd + fullTime + 
         cumUgGpa + onlineInd + `Textbook Type`, 
       REML = FALSE, data=.)
#, 
       # control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
       #                                                            optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


summary(gradeHM1)

# Find the least squares mean
# gradepmHM1 <- emmeans(gradeHM1, ~`Textbook Type`)
# emmip(gradeHM1, everPellEligibleInd ~ `Textbook Type`)

# Anova
gradeHAnova <- anova(gradeHM1)


# Model comparison
gradeHComp <- anova(gradeHM0, gradeHM1)

# Plot random effects
#gradeHM1RE <- sjp.glmer(gradeHM1, type = "re") + theme_minimal()

# History grade interactive model
# Interactive treatment model
gradeHM2 <- history %>% 
  lme4::lmer(courseGrade ~ (1|instructorId) + (1|academicYear) + (1|semester) + 
         everPellEligibleInd + firstGenerationInd + fullTime +
         cumUgGpa + onlineInd + `Textbook Type` + 
         `Textbook Type`:onlineInd + `Textbook Type`:everPellEligibleInd, 
       REML = FALSE, data=.)

       # control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
       #                       optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

summary(gradeHM2)

# Anova
gradeHAnova2 <- anova(gradeHM2)

# gradeHM2means <- emmeans(gradeHM2, ~ `Textbook Type` * everPellEligibleInd)
# emmip(gradeHM2, everPellEligibleInd ~ `Textbook Type`) + 
#   theme_minimal()
# Plot random effects
# gradeHM2RE <- sjp.glmer(gradeHM2, type = "re") + 
#   theme_minimal()


# grade model comparison
AIC(gradeHM0, gradeHM1, gradeHM2)
anova(gradeHM1, gradeHM2)
