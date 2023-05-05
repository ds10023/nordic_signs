library(dplyr)
library(tidyr)
library(ggplot2)
library(waffle)
library(cowplot)

setwd("<insert_path>")

swadesh <- read.csv("swadesh.csv")

swadesh_list_wa <- swadesh %>% 
  mutate(nor_swe = ifelse(norwegian == "same" & swedish == "same", 1,  0),
         nor_den = ifelse(norwegian == "same" & danish == "same", 1,  0),
         den_swe = ifelse(danish == "same" & swedish == "same", 1,  0),
         nor_den_swe = ifelse(norwegian == "same" & (danish == "same" & swedish == "same"), 1, 0))

norswe_table <- table(swadesh_list_wa$nor_swe)
norden_table <- table(swadesh_list_wa$nor_den)
denswe_table <- table(swadesh_list_wa$den_swe)
nordenswe_table <- table(swadesh_list_wa$nor_den_swe)

create_plots <- function(a) {
  waffle(
    a[order(a)],
    rows = 5,
    flip = TRUE,
    colors = c("#5f115d", "#CCCCCC")) +
    theme_light() +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          axis.text = element_blank())
}

norswe <- waffle(
  norswe_table[order(norswe_table, decreasing = TRUE)],
  rows = 5,
  flip = TRUE,
  colors = c("#5f115d", "#CCCCCC")) +
  theme_light() +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank())

norswe
norden <- create_plots(norden_table)
sweden <- create_plots(sweden_table)
denswe <- create_plots(denswe_table)
nordenswe <- create_plots(nordenswe_table)

first_row <- plot_grid(norswe, NULL, norden, NULL, denswe, NULL, nordenswe,
                       hjust = 0,
                       rel_widths = c(1, -.4,1, -.4, 1, -.4, 1),
                       nrow = 1)

empty_plot <- ggplot() + theme_void()

grid_with_space <- plot_grid(empty_plot, first_row, empty_plot, nrow = 3, rel_heights = c(2, 10, 1))

fo_fam = "Skia"
fo_size = 8

ggdraw(grid_with_space) +
  draw_image('norswe.png', x = -.315, y = -0.435, scale = .19) +
  draw_image('norden.png', x = -.1, y = -0.436, scale = .17) +
  draw_image('sweden.png', x = .115, y = -0.435, scale = .18) +
  draw_image('scandin.png', x = .328, y = -0.43, scale = .18) +
  annotate("label", x = .182, y = .7, label = "53/100", size = fo_size, family = fo_fam, label.padding=unit(.4, "lines")) +
  annotate("label", x = .395, y = .7, label = "49/100", size = fo_size, family = fo_fam, label.padding=unit(.4, "lines")) +
  annotate("label", x = .61, y = .7, label = "41/100", size = fo_size, family = fo_fam, label.padding=unit(.4, "lines")) +
  annotate("label", x = .825, y = .7, label = "36/100", size = fo_size, family = fo_fam, label.padding=unit(.4, "lines")) +
  annotate("text", x = .5, y = .93, size = 6, family = fo_fam,
           label = "Results of a comparison of signs representing 100 different\nconcepts in NTS, STS, and DTS. Purple color = same sign.") +
  annotate("text", x = .5, y = .857, size = 4, family = fo_fam,
           label = "https://github.com/ds10023/scandinavia_signs")
