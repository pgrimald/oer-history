
# Source necessary files/packages -----------------------------------------
source("~/Documents/oer-history/supportingScripts/SLCC History 2011-2018.R")
source("~/Documents/oer-history/supportingScripts/SLCC Plot Functions.R")

# Demographic Variables ---------------------------------------------------

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
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(ethnicity, pass, oer, courseSubject) %>% 
  count() 

# Ethnicity Grade
ethnicityAvgGrade <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(ethnicity, oer, courseSubject) %>% 
  summarise(mGrade = mean(courseGrade, na.rm = TRUE),
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n))

### Plot

#### Pass (this plot is particularly uninformative)

#### Avg Grade
ethnicityAvgGradePlt <- ethnicityAvgGrade %>% 
  ggplot(aes(x = reorder(ethnicity, -mGrade), y = mGrade, fill = oer)) + 
  geom_col(position = "dodge") +
  facet_wrap(~courseSubject) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_grey(start = .4, end = .8, na.value = "blue") +
  labs(x = "Ethnicity", y = "Mean Grade", fill = "OER") +
  geom_errorbar(aes(ymin = mGrade - semGrade, ymax = mGrade + semGrade), 
                width = .1, position = position_dodge(.9)) +
  coord_flip()



# Gender ------------------------------------------------------------------

gender <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  count(gender)


## Pell Status -------------------------------------------------------------

### Pell Status x Course Subject x OER ----------------------------------------

pellStatusPass <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(everPellEligibleInd, pass, oer, courseSubject) %>% 
  count() 

pellStatusAvgGrade <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(everPellEligibleInd, oer, courseSubject) %>% 
  summarise(mGrade = mean(courseGrade, na.rm = TRUE),
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n))

### Plot

#### Pass (this plot is particularly uninformative)

#### Avg Grade
(pellStatusAvgGradeBarPlt <- pellStatusAvgGrade %>% 
  ggplot(aes(x = reorder(everPellEligibleInd, -mGrade), y = mGrade, fill = oer)) + 
  geom_col(position = "dodge") +
  facet_wrap(~courseSubject) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  scale_fill_grey(start = .4, end = .7, na.value = "blue") +
  labs(x = "Pell Status", y = "Mean Grade", fill = "OER") +
  geom_errorbar(aes(col = oer, ymin = mGrade - semGrade, ymax = mGrade + semGrade), 
                width = .1, position = position_dodge(.9)) +
  scale_color_grey(start = .4, end = .7, na.value = "blue") +  
  scale_y_continuous(limits = c(0,4)) +
  guides(label = "none", color = "none") +  
  geom_text(aes(label = paste0("n = ",n), y = 0.1), 
            col = "white", fontface = "bold",lwd = 3.5, position = position_dodge(.9)))
# Point plot

# (pellStatusAvgGradePtPlt <- pellStatusAvgGrade %>% 
#     ggplot(aes(x = reorder(everPellEligibleInd, -mGrade), y = mGrade, col = oer)) + 
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
  group_by(fullTime, pass, oer, courseSubject) %>% 
  count() 

# Reg status grade
regStatusAvgGrade <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(fullTime, oer, courseSubject) %>% 
  summarise(mGrade = mean(courseGrade, na.rm = TRUE),
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n))


### Plot

#### Pass (this plot is particularly uninformative)

#### Avg Grade
(regStatusAvgGradePlt <- regStatusAvgGrade %>% 
    ggplot(aes(x = reorder(fullTime, -mGrade), y = mGrade, fill = oer)) + 
    geom_col(position = "dodge") +
    facet_wrap(~courseSubject) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_fill_grey(start = .4, end = .7, na.value = "blue") +
    labs(x = "Registration Status", y = "Mean Grade", fill = "OER") +
    geom_errorbar(aes(col = oer, ymin = mGrade - semGrade, ymax = mGrade + semGrade), 
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
  group_by(firstGenerationInd, pass, oer, courseSubject) %>% 
  count() 

# # First gen grade
firstGenAvgGrade <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(firstGenerationInd, oer, courseSubject) %>% 
  summarise(mGrade = mean(courseGrade, na.rm = TRUE),
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n))

### Plot

#### Pass (this plot is particularly uninformative)

#### Avg Grade
(firstGenAvgGradePlt <- firstGenAvgGrade %>% 
    ggplot(aes(x = reorder(firstGenerationInd, -mGrade), y = mGrade, fill = oer)) + 
    geom_col(position = "dodge") +
    facet_wrap(~courseSubject) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(x = "First Generation", y = "Mean Grade", fill = "OER") +
    geom_errorbar(aes(col = oer, ymin = mGrade - semGrade, ymax = mGrade + semGrade), 
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
gpaAvgGrade <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(cumUgGpa, oer, courseSubject) %>% 
  summarise(mGrade = mean(courseGrade, na.rm = TRUE),
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n))

# Course Factors ----------------------------------------------------------

### Online x Course Subject x OER ----------------------------------------

onlinePass <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(onlineInd, pass, oer, courseSubject) %>% 
  count() 

onlineAvgGrade <- SLCC_11_18_proc %>% 
  distinct(studentId, .keep_all = TRUE) %>% 
  group_by(onlineInd, oer, courseSubject) %>% 
  summarise(mGrade = mean(courseGrade, na.rm = TRUE),
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n))


### Plot

#### Pass (this plot is particularly uninformative)

#### Avg Grade
(onlineAvgGradePlt <- onlineAvgGrade %>% 
    ggplot(aes(x = onlineInd, y = mGrade, fill = oer)) + 
    geom_col(position = "dodge") +
    facet_wrap(~courseSubject) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(x = "Course Delivery", y = "Mean Grade", fill = "OER") +
    scale_x_discrete(labels = c("Classroom", "Online")) +
    geom_errorbar(aes(ymin = mGrade - semGrade, ymax = mGrade + semGrade, col = oer), 
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
   add_count(courseSubject, oer) %>% 
   group_by(courseSubject, oer, n) %>% 
   summarise(numPass = sum(pass == 1), passRate = numPass/unique(n), semPass = passRate/unique(n)) %>% 
   ggplot(aes(x = courseSubject, y = passRate, fill = oer)) + geom_col(position = "dodge") + 
   geom_errorbar(aes(ymin = passRate - semPass, ymax = passRate + semPass), width = .1, position = position_dodge(.9)) +
   theme_minimal() + theme(legend.position = "bottom") + 
   scale_fill_grey(start = .4, end = .8, labels = oerLab) + scale_y_continuous(limits = c(0,1)) +
   labs(x = "Course Subject", fill = "Textbook Type", y = "Pass Rate"))

# Sem x Pass
passAggSem <- SLCC_11_18_proc %>% 
  add_count(courseSubject, oer, semester) %>% 
  group_by(courseSubject, oer, semester, n) %>% 
  summarise(numPass = sum(pass == 1), passRate = numPass/unique(n), semPass = passRate/unique(n)) 

(passAggSemPlot <- barPlotWEbar(df = passAggSem, v2 = "passRate", v5 = "semPass", 
                             laby = "Pass Rate", colLab = "Textbook type", limy = c(0,1)))


# Grade x Subject
# pass rate aggregate plot, fig.cap="Average grades across textbook types, semesters, subject."}
avgGradeAgg <- SLCC_11_18_proc %>% 
  group_by(courseSubject, oer, semester) %>%
  summarise(mGrade = mean(courseGrade, na.rm = TRUE), 
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n)) 

# Grade x Subject across semesters
avgGradeAggPlt <- avgGradeAgg %>% 
  ggplot(aes(x = semester, y = mGrade, fill = oer)) + geom_col(position = "dodge") + 
  geom_errorbar(aes(col = oer, ymin = mGrade - semGrade, ymax = mGrade + semGrade), width = 0.1, position = position_dodge(.9)) + 
  facet_wrap(~courseSubject) +
  theme_minimal() +
  scale_fill_grey(start = .4, end = .8, labels = c("Traditional", "Open")) +  
  scale_color_grey(start = .4, end = .8) +  guides(color = "none") +
  labs(x = "Semester", y = "Average Grade", fill = "Textbook Type") + theme(legend.position = "bottom")


# Pilot period ------------------------------------------------------------

# pass rate pilot period, fig.cap="Difference in pass rate between courses using traditional and open textbooks over the pilot semesters."}
# History was piloted during Sp 16
pilotSems <- "Sp 16" #c("F 15", "Sp 16")
# Change OER labels
labels <- c(`1` = "Used Open textbook", `0` = "Used Traditional Textbook")

(histPilotPassPlot <- passRate %>% 
    filter(courseSubject == "History" & yrSem %in% pilotSems) %>% 
    ggplot(aes(x = oer, y = passRate, fill = oer)) + 
    geom_col() + 
    theme_minimal() + 
    scale_x_discrete(labels = c("Traditional Textbook", "Open Textbook")) +
    labs(y = "Pass Rate", x = "Textbook Type") + 
    theme(legend.position = "bottom") +
    geom_errorbar(aes(col = oer, ymin = passRate - semPass, ymax = passRate + semPass), position = position_dodge(0.9), width = .1) +
    scale_fill_grey(start = .4, end = 0.8, na.value = "blue") +
    scale_colour_grey(start = .4, end = 0.8, na.value = "blue") +
    scale_y_continuous(limits = c(0,1)) + 
    geom_text(aes(label = paste0("n = ",n), y = 0.1), 
              col = "white", fontface = "bold",lwd = 3.5, position = position_dodge(.9)))



# Mode of course delivery: Online vs Classroom ----------------------------
# Pass rate
passRateOnline <- SLCC_11_18_proc %>% 
  add_count(courseSubject, oer, onlineInd) %>% 
  ungroup() %>% 
  group_by(courseSubject, oer, onlineInd, n) %>%
  summarise(numPass = sum(pass == 1), passRate = numPass/unique(n), semPass = passRate/unique(n))

passRateOnlinePlt <- passRateOnline %>% 
  ggplot(aes(x = onlineInd, y = passRate, fill = oer)) +  
  geom_col(position = "dodge") + theme_minimal() + 
  facet_wrap(~courseSubject) + 
  labs(x = "Course Type", y = "Pass Rate", fill = "Textbook Type") +
  geom_errorbar(aes(ymin = passRate - semPass, ymax = passRate + semPass, colour = oer), 
                width = .1, position = position_dodge(0.9)) + 
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(labels = c("Classroom", "Online")) +
  scale_fill_grey(start = .4, end = .8, labels = c("Traditional", "Open")) + 
  scale_color_grey(start = .4, end = .8) + 
  theme(legend.position = "bottom") + guides(color = "none")


# Avg Grade
(avgGradeOnline <- SLCC_11_18_proc %>% 
    group_by(courseSubject, oer,onlineInd) %>%
    summarise(mGrade = mean(courseGrade, na.rm = TRUE), 
              n = sum(!is.na(unique(studentId))),
              semGrade = sd(courseGrade)/sqrt(n)) %>% 
    ggplot(aes(x = onlineInd, y = mGrade, fill = oer)) + 
    geom_col(position = "dodge") + theme_minimal() + 
    facet_wrap(~courseSubject) + labs(x = "Course Type", y = "Mean Grade", fill = "Textbook Type") +
    geom_errorbar(aes(ymin = mGrade - semGrade, ymax = mGrade + semGrade, colour = oer), 
                  width = .2, position = position_dodge(0.9)) + 
    scale_y_continuous(limits = c(0,4)) +
    scale_x_discrete(labels = c("Traditional", "Online")) +
    scale_fill_grey(start = .4, end = .8, labels = c("Traditional", "Open")) + 
    scale_color_grey(start = .4, end = .8) + theme(legend.position = "bottom") + guides(color = "none"))


# Year-Semester -----------------------------------------------------------

### Pass rate ---------------------------------------------------------------
passRate <- SLCC_11_18_proc %>% 
  add_count(courseSubject, oer, yrSem) %>% 
  ungroup() %>% 
  group_by(courseSubject, oer, yrSem, n) %>%
  summarise(numPass = sum(pass == 1), passRate = numPass/unique(n), semPass = passRate/unique(n)) 



# Effect size
idx <- passRate$courseSubject == "History"
dPassOER <- cohen.d(passRate$passRate[idx],passRate$oer[idx], na.rm = TRUE)

## Pass plot
passLPlot <- linearTrend(df = passRate, v1 = "yrSem", v2 = "passRate", v3 = "oer", v4 = "courseSubject", v5 = "semPass", labx = "Year-Semester", laby = "Pass Rate") + 
    geom_vline(aes(xintercept = which(levels(yrSem) %in% "Sp 16")), lty = 2, lwd = .25)


### Average Grade -----------------------------------------------------------

# r oer pass, fig.cap="Overall effect of OER on average grade across different courses"
(gradeOer <- SLCC_11_18_proc %>% 
    group_by(courseSubject, oer) %>% 
    summarise(mGrade = mean(courseGrade, na.rm = TRUE), 
              n = sum(!is.na(unique(studentId))),
              semGrade = sd(courseGrade)/sqrt(n)) %>%  
    ggplot(aes(x = courseSubject, y = mGrade, fill = oer)) + geom_col(position = "dodge") + 
    geom_errorbar(aes(col = oer, ymin = mGrade - semGrade, ymax = mGrade + semGrade), width = .1, position = position_dodge(.9)) +
    theme_minimal() + theme(legend.position = "bottom") + 
    scale_colour_grey(start = .4, end = .8, labels = oerLab) +
    scale_fill_grey(start = .4, end = .8, labels = oerLab) + 
    scale_y_continuous(limits = c(0,4)) +
    guides(col = "none") +
    labs(x = "Course Subject", fill = "Textbook Type", y = "Average Grade"))


# Grade x yr x Sem
avgGrade <- SLCC_11_18_proc %>% 
  group_by(courseSubject, oer, yrSem) %>%
  summarise(mGrade = mean(courseGrade, na.rm = TRUE), 
            n = sum(!is.na(unique(studentId))),
            semGrade = sd(courseGrade)/sqrt(n)) 

# Effect size
gIdx <- avgGrade$courseSubject == "History"
dAvgGradeOER <- cohen.d(avgGrade$mGrade[gIdx],avgGrade$oer[gIdx], na.rm = TRUE)

# Line Plot

(agLPlot <- linearTrend(df = avgGrade, v1 = "yrSem", v2 = "mGrade", v3 = "oer", v4 = "courseSubject", v5 = "semGrade", colVal = oerLab,labx = "Year-Semester", laby = "Average grade", limy = c(0,4)) +
    geom_vline(aes(xintercept = which(levels(yrSem) %in% "Sp 16")), lty = 2, lwd = .25))


# Pilot Period ------------------------------------------------------------

(histAvgGrade <- avgGrade %>% 
   filter(courseSubject == "History" & yrSem == "Sp 16") %>% 
   ggplot(aes(x = yrSem, y = mGrade, fill = oer, colour = oer)) + geom_col(position = "dodge") + guides(color = "none") +
   theme_minimal() + theme(legend.position = "bottom") +
   scale_fill_grey(start = .4, end = .8, labels = c("Traditional Textbook", "Open Textbook")) + 
   scale_color_grey(start = .4, end = .8, labels = c("Traditional Textbook", "Open Textbook")) +
   labs(x = "Semester Year", y = "Average Grade", fill = "Textbook Type") + 
   geom_errorbar(aes(ymin = mGrade - semGrade, ymax = mGrade + semGrade, colour = oer), width = .1, position = position_dodge(.9)) + 
   scale_y_continuous(limits = c(0,4)))

# Course Completion -------------------------------------------------------

# Completion is near ceiling
completion <- SLCC_11_18_proc %>% 
  group_by(courseSubject) %>% 
  summarize(completion = sum(completion == 1), n = n(studentId), completionrate = completion/n)

# Incomplete grades by course
incomplete <- SLCC_11_18_proc %>% 
  filter(finalGrade == "I") %>% 
  group_by(courseSubject) %>% 
  count() 







# Omnibus Models ------------------------------------------------------------------

## Pass Rate Logistic Models -----------------------------------------------
### Base Model
passM0 <- SLCC_11_18_proc %>% 
  glmer(pass ~ (1|courseSubject/instructorId) + (1|academicYear) + (1|semester) + onlineInd + everPellEligibleInd + firstGenerationInd + fullTime + cumUgGpa, family = binomial, data=., 
        control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(passM0)
emmeans(passM0, ~onlineInd)

emmip(passM0, ~onlineInd)

### Additive treatment model
passM1 <- SLCC_11_18_proc %>% 
  glmer(pass ~ (1|courseSubject/instructorId) + (1|academicYear) + (1|semester) + onlineInd + everPellEligibleInd +  firstGenerationInd + fullTime + cumUgGpa + oer, family = binomial, data=., 
        control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

summary(passM1)

# Find the least squares mean
passPmmeans <- emmeans(passM1, ~oer)
emmip(passM1, onlineInd ~ oer)

# Anova
passAnova <- anova(passM1)


# Model comparison
passComp <- anova(passM0, passM1)

# Plot random effects
passM1RE <- sjp.glmer(passM1, type = "re") + theme_minimal()

### Interactive treatment model
passM2 <- SLCC_11_18_proc %>% 
  glmer(pass ~ (1|courseSubject/instructorId) + (1|academicYear) + (1|semester) + 
        everPellEligibleInd + everPellEligibleInd:oer + fullTime + firstGenerationInd +
        cumUgGpa + onlineInd + onlineInd:oer, family = binomial, 
        control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)), data=.)

summary(passM2)

# Anova
passAnova2 <- anova(passM2)

passM2means <- emmeans(passM2, ~ oer * onlineInd)
emmip(passM2, onlineInd ~ oer)
# Plot random effects
passM2RE <- sjp.glmer(passM2, type = "re") + theme_minimal()


passAIC <- AIC(passM0, passM1, passM2)

# Comparing additive and interactive models
anova(passM1, passM2)


## Average Grade Models -----------------------------------------------
### Base Model
gradeM0 <- SLCC_11_18_proc %>% 
  lmer(courseGrade ~ (1|courseSubject/instructorId) + (1|academicYear) + (1|semester) + 
          onlineInd + everPellEligibleInd + firstGenerationInd + fullTime + 
          cumUgGpa, data=., control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(gradeM0)
emmeans(gradeM0, ~onlineInd)

emmip(gradeM0, ~onlineInd)
# ref_grid(passM0) @ grid %>% View()

### Additive treatment model
gradeM1 <- SLCC_11_18_proc %>% 
  lmer(courseGrade ~ (1|courseSubject/instructorId) + (1|academicYear) + (1|semester) + 
         onlineInd + everPellEligibleInd +  firstGenerationInd + 
         fullTime + cumUgGpa + oer, data=., control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


summary(gradeM1)

# Find the least squares mean
gradepmM1 <- emmeans(gradeM1, ~oer)
emmip(gradeM1, onlineInd ~ oer)

# Anova
gradeAnova <- anova(gradeM1)


# Model comparison
passComp <- anova(gradeM0, gradeM1)

# Plot random effects
gradeM1RE <- sjp.glmer(gradeM1, type = "re") + theme_minimal()

# History Models ------------------------------------------------------------------

## Pass Rate Logistic Models -----------------------------------------------
## Filter out only history data
history <- SLCC_11_18_proc %>% 
  filter(courseSubject == "History")
### Base Model
passHM0 <- history %>% 
  glmer(pass ~ (1|instructorId) + (1|academicYear) + (1|semester) + onlineInd + everPellEligibleInd + firstGenerationInd + fullTime + cumUgGpa, family = binomial, data=., 
        control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(passHM0)
emmeans(passHM0, ~onlineInd)

emmip(passHM0, ~onlineInd)

### Additive treatment model
passHM1 <- history %>% 
  glmer(pass ~ (1|instructorId) + (1|academicYear) + (1|semester) + onlineInd + everPellEligibleInd +  firstGenerationInd + fullTime + cumUgGpa + oer, family = binomial, data=., 
        control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(passHM1)

# Find the least squares mean
passHPmmeans <- emmeans(passHM1, ~oer)
emmip(passHM1, onlineInd ~ oer)

# Anova
passAnova <- anova(passM1)


# Model comparison
passComp <- anova(passM0, passM1)

# Plot random effects
passM1RE <- sjp.glmer(passM1, type = "re") + theme_minimal()

### Interactive treatment model
passM2 <- SLCC_11_18_proc %>% 
  glmer(pass ~ (1|courseSubject/instructorId) + (1|academicYear) + (1|semester) + oer*everPellEligibleInd + fullTime + 
          firstGenerationInd + cumUgGpa + onlineInd, family = binomial, 
        control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)), data=.)

summary(passM2)

# Anova
passAnova2 <- anova(passM2)

passM2means <- emmeans(passM2, ~ oer * onlineInd)
emmip(passM2, onlineInd ~ oer)
# Plot random effects
passM2RE <- sjp.glmer(passM2, type = "re") + theme_minimal()

## Average Grade Models -----------------------------------------------
### Base Model
gradeM0 <- SLCC_11_18_proc %>% 
  lmer(courseGrade ~ (1|courseSubject/instructorId) + (1|academicYear) + (1|semester) + 
         onlineInd + everPellEligibleInd + firstGenerationInd + fullTime + 
         cumUgGpa, data=., control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                 optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(gradeM0)
emmeans(gradeM0, ~onlineInd)

emmip(gradeM0, ~onlineInd)
# ref_grid(passM0) @ grid %>% View()

### Additive treatment model
gradeM1 <- SLCC_11_18_proc %>% 
  lmer(courseGrade ~ (1|courseSubject/instructorId) + (1|academicYear) + (1|semester) + 
         onlineInd + everPellEligibleInd +  firstGenerationInd + 
         fullTime + cumUgGpa + oer, data=., control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                  optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


summary(gradeM1)

# Find the least squares mean
gradepmM1 <- emmeans(gradeM1, ~oer)
emmip(gradeM1, onlineInd ~ oer)

# Anova
gradeAnova <- anova(gradeM1)


# Model comparison
passComp <- anova(gradeM0, gradeM1)

# Plot random effects
gradeM1RE <- sjp.glmer(gradeM1, type = "re") + theme_minimal()