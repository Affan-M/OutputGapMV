#--- Theme: Output gap ---#
library(showtext)
library(extrafont)
library(cowplot)

font_add("Fira", "fonts/FiraSans-Light.ttf")
font_add("Roboto", "fonts/Roboto-Medium.ttf")
# font_add("Myriad", "fonts/MYRIADPRO-REGULAR.OTF")

# Font selection
font <- "Fira"
font_h <- "Roboto"
font_sub <- "Roboto"

showtext_auto()

line_col <- "#131f2c"

cols <- c("#213063",
          "#EEA91F",
          "#61BDCD",
          "#CD506A",
          "#0A6BB0",
          "#E9633F",
          "#0B818A",
          "#F7A9B2",
          "#75A8DA",
          "#C7DB8F",
          "#C3E8F8",
          "#B5C7E4")

headings_color <- "#57585a"

font_title_size <- 11
font_sub_size <- 8
font_size <- 6+1

theme_mma <- function(){
  theme(plot.title = element_text(size = font_title_size, family = font_h, color = headings_color),
        plot.subtitle = element_text(size = font_sub_size, family = font_sub, color = headings_color),
        plot.caption = element_text(size = font_sub_size, family = font_sub, hjust = 0, color = headings_color),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(family = font, size = font_size, margin = margin(r=0.3, l=-0.15, unit="cm")),
        legend.box.margin = margin(t = 0, b = 0.2, unit = "cm"),
        legend.margin = margin(t = 0, b = -0.3, unit = "cm"),
        legend.key.width = unit(0.5, "line"),
        legend.key.height = unit(0.75, "line"),
        legend.direction = "horizontal",
        legend.box.just = "left",
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = "transparent"),
        axis.text = element_text(family = font, color = "black", size = font_size),
        axis.title = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "#e9e9e9", size = 0.15),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin(t = 0)),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "#e9e9e9", size = 0.15),
        plot.margin = margin(0.1,0.1,0.1,0.1, "cm"),
        plot.title.position = "plot",
        plot.caption.position = "plot")
}

## Chart dimensions
.width <- 4.50
.height <- 3.50
