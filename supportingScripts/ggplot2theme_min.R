## ggplot2 theme
library(ggplot2)
#osColorPalette <- c('#9A9A9B','#212E66','#0DC0DC','#0C9372','#77AF43','#F4D019','#FDBD3E','#F37642')
quartzFonts(avenir = c("Avenir Book", "Avenir Black", "Avenir Book Oblique", 
                       "Avenir Black Oblique"))
osColorPalette <- c('#F37642','#77AF43','#212E66','#5E6062','#0DC0DC','#9A9A9B','#C22032','#0C9372','#FDBD3E','#F4D019', '#BEBADA')
theme_min <- function (ff = 'avenir', aTextsz = 12, ...) {
  theme(plot.title = element_text(hjust = 0.5, family = ff, size = 16),
        axis.text = element_text(family = ff, size = 12),
        axis.text.x = element_text(family = ff, size = aTextsz),
        axis.text.y = element_text(family = ff, size = aTextsz),
        #axis.ticks = element_line(margin=margin(0,0,0,0)),
        #axis.ticks.length = element_line(size = unit(2, "points")),
        axis.title = element_text(family = ff, size = 16),
        panel.background = element_rect(fill = 'white'),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_line(colour = NA),
        panel.ontop = FALSE,
        plot.caption = element_text(family = ff, hjust = 0.0, size = 12, lineheight = 1, margin = margin(rep(10,4))),
        legend.text = element_text(family = ff, size = 12),
        legend.title = element_text(family = ff, size = 16, hjust = 0.5),
        legend.title.align = 0.5,
        legend.box = 'horizontal',
        legend.box.background = element_rect(fill = NULL, colour = 'grey50'),
        legend.position = "bottom",
        legend.margin = margin(10,10,10,10),
        legend.spacing.x = unit(3, "bigpts"),
        legend.background = element_rect(fill = NA),
        strip.background = element_rect(fill = 'grey95'),
        strip.text = element_text(size = 12, family = ff))
}