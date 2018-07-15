# Exploratory Data Analysis -----------------------------------------------
# Just exploring the data from different perspectives

# First gen x Pell
SLCC_11_18_proc %>% 
  ggplot(aes(x = fullTime, y = everPellEligibleInd)) + 
  geom_count() + theme_minimal() + 
  theme(legend.position = "bottom") +
  labs(x = "First Generation Individual", y = "Ethnicity", size = "Number of Respondents")

# course grade by subject
(eda1 <- SLCC_11_18_proc %>% 
    ggplot(aes(x = courseSubject, y = courseGrade)) + geom_boxplot() + 
    theme_minimal() + labs(x = "Subject", y = "Course Grade"))

# course grade by subject by semester, include=FALSE}
(eda2 <- SLCC_11_18_proc %>% 
    ggplot(aes(x = courseSubject, y = courseGrade)) + geom_boxplot() + 
    facet_wrap(~semester) + theme_minimal() + labs(x = "Subject", y = "Course Grade"))

# pell x grade
(eda3 <- SLCC_11_18_proc %>% 
    ggplot(aes(x = everPellEligibleInd, y = courseGrade)) + geom_boxplot() + 
    theme_minimal() + labs(x = "Pell Eligibility", y = "Course Grade") + 
    facet_wrap(~courseSubject))

# gpa x grade
(eda4 <- SLCC_11_18_proc %>% 
    ggplot(aes(y = cumUgGpa, x = factor(courseGrade))) + geom_boxplot(alpha = .2) +
    theme_minimal() + labs(y = "UG GPA", x = "Course Grade") + facet_wrap(~courseSubject) + coord_flip())

# Pass x grade, fig.cap="", include = FALSE}
(eda5 <- SLCC_11_18_proc %>% 
    ggplot(aes(x = pass, y = courseSubject)) + geom_count(alpha = .4) +
    theme_minimal() + labs(x = "Pass", y = "Subject", size = "Number of Students") + theme(legend.position = "bottom"))
