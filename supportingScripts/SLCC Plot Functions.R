# Plot functions ----------------------------------------------------------
library(tidyverse)

## Set up labels for plots
subjLab <- c(HIST = "History", POLS = "Political Science", ECON = "Economics")
oerLab <- c(`0` = "Traditional", `1` = "Open")
onlineLab <- c(`0` = "Classroom", `1` = "Online")

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


