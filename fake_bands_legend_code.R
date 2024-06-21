#-----------------------------------------------------
# Make fake plots to generate legend waterbands plots and waterboxplots
#-----------------------------------------------------
library(ggplot2)
library(ggpubr)
library(gridExtra)

full_data = data.frame(month = 1:6,
                       val = 5:10,
                       val2 = 4:9,
                       var = 
                         factor(c("Historic range", "Hist. 95% range", "Hist. 50% range",
                                  "Hist. median", "Upper WQ Threshold", 
                                  "Lower WQ Threshold"),
                                levels = c("Historic range", "Hist. 95% range", "Hist. 50% range",
                                           "Hist. median", 
                                           "Upper WQ Threshold", 
                                           "Lower WQ Threshold")),
                       grp = c(rep('g1', 6)))

point_data = data.frame(month = c(1, 3),
                        val = c(5, 7),
                        var = as.factor(c("Current value", "Poor WQ value")),
                        type = 'point', 
                        grp = as.factor(c("Current value", "Poor WQ value")),
                        grp2 = c(rep("Historic outlier", 2)))
fake_bands <-
  ggplot(full_data[1:4,], aes(x = month, y = val))+
  labs(y = NULL, x = NULL, title = NULL) + 
  geom_ribbon(aes(ymax = val, ymin = val2, fill = var))+
  geom_line(aes(color = var), linewidth = 1)+
  scale_color_manual(values = 
                       c("Historic range" = "#E4F0F8", 
                         "Hist. 95% range" = "#B8D8ED", 
                         "Hist. 50% range" = "#7FB9DD", 
                         "Hist. median" = "#1378b5"),
                     breaks = 
                       c("Historic range", 
                         "Hist. 95% range", 
                         "Hist. 50% range", 
                         "Hist. median"),
                     name = NULL)+
  scale_fill_manual(values = 
                       c("Historic range" = "#E4F0F8", 
                         "Hist. 95% range" = "#B8D8ED", 
                         "Hist. 50% range" = "#7FB9DD", 
                         "Hist. median" = "white"),
                     breaks = 
                       c("Historic range", 
                         "Hist. 95% range", 
                         "Hist. 50% range", 
                         "Hist. median"),
                     name = NULL)+
  theme(legend.position = 'bottom',
        axis.title.y = element_text(size = 10),
        legend.text = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = '#696969', fill = 'white', linewidth = 0.4),
        axis.line.x = element_line(color = "#696969", linewidth = 0.4),
        axis.line.y = element_line(color = "#696969", linewidth = 0.4),
        axis.ticks = element_line(color = "#696969", linewidth = 0.4),
        legend.key = element_blank(),
        legend.key.width = unit(0.8, 'cm'),
        legend.margin = margin(t = 0, unit = "cm"))

fake_bands

fake_lines <-
  ggplot(full_data[5:6,], aes(x = month, y = val))+
  labs(y = NULL, x = NULL, title = NULL) + 
  geom_line(aes(color = var, linetype = var, linetype = var), linewidth = 1, color = 'black')+
  scale_linetype_manual(values =
                          c("Lower WQ Threshold" = 'dashed', 
                            "Upper WQ Threshold" = 'solid'),
                        name = NULL)+
  theme(legend.position = 'bottom',
        axis.title.y = element_text(size = 10),
        legend.text = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = '#696969', fill = 'white', linewidth = 0.4),
        axis.line.x = element_line(color = "#696969", linewidth = 0.4),
        axis.line.y = element_line(color = "#696969", linewidth = 0.4),
        axis.ticks = element_line(color = "#696969", linewidth = 0.4),
        legend.key = element_blank(),
        legend.key.width = unit(0.8, 'cm'),
        legend.margin = margin(t = 0, unit = "cm"))#+

fake_lines  
# guides(color = guide_legend(nrow = 2, byrow = TRUE),
  #        fill = guide_legend(nrow = 2, byrow = TRUE),
  #        shape = guide_legend(nrow = 2, byrow = TRUE),
  #        linetype = guide_legend(nrow = 2, byrow = TRUE))

fake_points <-
  ggplot(point_data, aes(x = month, y = val, color = var))+
  geom_point()+
  labs(y = NULL, x = NULL, title = NULL) + 
  scale_color_manual(values = 
                       c("Current value" = 'black', 
                         "Poor WQ value" = "orange"),
                     breaks = 
                       c("Current value", 
                         "Poor WQ value"),
                     name = NULL)+
  theme(legend.position = "bottom", #'right',
        axis.title.y = element_text(size = 10),
        legend.text = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = '#696969', fill = 'white', linewidth = 0.4),
        axis.line.x = element_line(color = "#696969", linewidth = 0.4),
        axis.line.y = element_line(color = "#696969", linewidth = 0.4),
        axis.ticks = element_line(color = "#696969", linewidth = 0.4),
        legend.key = element_blank(),
        legend.margin = margin(t = 0, unit = "cm"))

fake_points

leg_bands_pb <- as_ggplot(get_legend(fake_bands))
leg_lines_pb <- as_ggplot(get_legend(fake_lines))
leg_points_pb <- as_ggplot(get_legend(fake_points))

legg <- as_ggplot(
  grid.arrange(grobs = list(leg_bands_pb, leg_points_pb, leg_lines_pb),
               layout_matrix = rbind(c(1, 1),
                                     c(2, 3))
  ))
legg
#legg <- as_ggplot(leg)

#ggsave("./rmd/waterband_leg.svg", as_ggplot(leg), width = 12, height = 0.25)
#legimg2 <- magick::image_read_pdf("./rmd/waterband_leg.pdf", density = 600)

#ggsave("./rmd/waterband_leg.pdf", as_ggplot(leg), width = 12, height = 0.25)
#ggsave("./rmd/waterband_leg.png", as_ggplot(leg), width = 12, height = 0.3)



