# -----------------------------------------------------------------------------
# Script Name: Manuscript figure creation
# Description: This script loads environmental data and 
# generates visualizations for manuscript figures.
# Author: Marco Girardello
# Date: 2024-07-23
# Version: 1.0
# -----------------------------------------------------------------------------

# load required packages
library(tidyverse)
library(sf)
library(terra)
library(viridis)
library(scales)
library(patchwork)
library(factoextra)
library(ggrepel)
library(ggcorrplot)

# data files --------------------------------------------------------------

# vector files
coast <- st_read("./data/coastline.gpkg")

# vector mask i.e eu countries
eu <- st_read("./data/landmask.gpkg")

# datasets
dat10km <- readRDS("./data/dat10km.rds")
dat5km <- readRDS("./data/dat5km.rds")
dat1km <- readRDS("./data/dat1km.rds")

# variable selection
vars_10km <- readRDS("./data/vars_10km.rds")
vars_5km <- readRDS("./data/vars_5km.rds")
vars_1km <- readRDS("./data/vars_1km.rds")

# predictor lookup table
pred_all <- readRDS("./data/var_all_groups.rds")

# response lookup table
resp <- readRDS("./data/response_labels.rds")

#### climate data
envdat10km <- readRDS("./data/envdat10km.rds")
envdat5km <- readRDS("./data/envdat5km.rds")
envdat1km <- readRDS("./data/envdat1km.rds")

dat10kmenv <- inner_join(envdat10km,dat10km)
dat5kmenv <- inner_join(envdat5km,dat5km)
dat1kmenv <- inner_join(envdat1km,dat1km)

# R2 - model performance
validation_results <- readRDS("./data/validation_results.rds")

# Fig. 2 maps --------------------------------------------------------------
# 5km 
shan <- map_create(data = dat5km,
                  col = "shannon_5km",
                  title = expression("Shannon Entropy" ~ (tau[SW])),
                  invec = eu,
                  palette = "plasma",
                  plow = 0.02,
                  pupp = 0.98,
                  buff_low = 0.1,
                  buff_up = 0.09)


rao <- map_create(data = dat5km,
                   col = "rao_5km",
                   title = expression("Rao Quadratic Entropy" ~ (tau[RAO])),
                   invec = eu,
                   palette = "plasma",
                  buff_low = 0.1,
                  buff_up = 0.09)

sd <- map_create(data = dat5km,
                  col = "rh98_5km",
                  title = expression(paste(sigma, " of Canopy Height", " " ~ (tau[CH]))),
                  invec = eu,
                  palette = "plasma",
                 buff_low = 0.1,
                 buff_up = 0.09)

skew <- map_create(data = dat5km,
                 col = "skew_5km",
                 title = expression("Skewness of vertical profile" ~ (tau[SK])),
                 invec = eu,
                 palette = "plasma",
                 buff_low = 0.1,
                 buff_up = 0.09)


hull <- map_create(data = dat5km,
                   col = "hull_5km",
                   title = expression("Convex hull volume" ~ (tau[CHV])),
                   invec = eu,
                   palette = "plasma",
                   buff_low = 0.1,
                   buff_up = 0.09)

cover <- map_create(data = dat5km,
                   col = "cover_5km" ,
                   title = expression(paste(sigma, " of Canopy Cover", " " ~ (tau[CC]))),
                   invec = eu,
                   palette = "plasma",
                   rounding_fact = 2,
                   correct_fact = 0.01,
                   prop = 0.1,
                   buff_low = 0.1,
                   buff_up = 0.09)

cv <- map_create(data =  dat5km,
                    col = "cv_5km" ,
                    title = expression("CV of vertical profile" ~ (tau[CV])),
                    invec = eu,
                    palette = "plasma",
                 buff_low = 0.1,
                 buff_up = 0.09)


kurt <- map_create(data = dat5km,
                   col = "kurt_5km",
                   title = expression("Kurtosis of vertical profile" ~ (tau[KU])),
                   invec = eu,
                   palette = "plasma",
                   buff_low = 0.1,
                   buff_up = 0.09)

# combine plots together
plotcombined <- (cv | skew) / 
  (kurt | sd) / 
  (cover  | shan) /
  (rao |  hull)

combined5km <- plotcombined &
  theme(plot.margin = unit(c(1, 1, 1, 1), "mm"))



# Print the combined plot
# ggsave(combined5km, filename = "./results/strmodels/paper_figures/Fig1/Predictions5kmv2.png",
#        width = 11, height = 12, dpi = 400, bg = "white")

ggsave(combined5km, filename = "./results/strmodels/paper_figures/Fig1/Predictions5km.eps", bg = "white",
       width = 11, height = 12)



# 10 km
shan <- map_create(data = dat10km,
                   col = "shannon_10km",
                   title = expression("Shannon Entropy" ~ (tau[SW])),
                   invec = eu,
                   palette = "plasma",
                   plow = 0.02,
                   pupp = 0.98,
                   buff_low = 0.1,
                   buff_up = 0.09)


rao <- map_create(data = dat10km,
                  col = "rao_10km",
                  title = expression("Rao Quadratic Entropy" ~ (tau[RAO])),
                  invec = eu,
                  palette = "plasma",
                  buff_low = 0.1,
                  buff_up = 0.09)

sd <- map_create(data = dat10km,
                 col = "rh98_10km",
                 title = expression(paste(sigma, " of Canopy Height", " " ~ (tau[CH]))),
                 invec = eu,
                 palette = "plasma",
                 buff_low = 0.1,
                 buff_up = 0.09)

skew <- map_create(data = dat10km,
                   col = "skew_10km",
                   title = expression("Skewness of vertical profile" ~ (tau[SK])),
                   invec = eu,
                   palette = "plasma",
                   buff_low = 0.1,
                   buff_up = 0.09)


hull <- map_create(data = dat10km,
                   col = "hull_10km",
                   title = expression("Convex hull volume" ~ (tau[CHV])),
                   invec = eu,
                   palette = "plasma",
                   buff_low = 0.1,
                   buff_up = 0.09)

cover <- map_create(data = dat10km,
                    col = "cover_10km" ,
                    title = expression(paste(sigma, " of Canopy Cover", " " ~ (tau[CC]))),
                    invec = eu,
                    palette = "plasma",
                    rounding_fact = 2,
                    correct_fact = 0.01,
                    prop = 0.1,
                    buff_low = 0.1,
                    buff_up = 0.09)

cv <- map_create(data =  dat10km,
                 col = "cv_10km" ,
                 title = expression("CV of vertical profile" ~ (tau[CV])),
                 invec = eu,
                 palette = "plasma",
                 buff_low = 0.1,
                 buff_up = 0.09)


kurt <- map_create(data = dat10km,
                   col = "kurt_10km",
                   title = expression("Kurtosis of vertical profile" ~ (tau[KU])),
                   invec = eu,
                   palette = "plasma",
                   buff_low = 0.1,
                   buff_up = 0.09)


plotcombined <- (cv | skew) / 
  (kurt | sd) / 
  (cover  | shan) /
  (rao |  hull)

combined10km <- plotcombined &
  theme(plot.margin = unit(c(1, 1, 1, 1), "mm"))

# Print the combined plot
# ggsave(combined10km, filename = "./results/strmodels/paper_figures/Fig1/Predictions10kmv1.png",
#        width = 11, height = 12, dpi = 400, bg = "white")

ggsave(combined10km, filename = "./results/strmodels/paper_figures/Fig1/Predictions10km.eps", bg = "white",
       width = 11, height = 12)


# 1 km 
shan <- map_create(data = dat1km,
                   col = "shannon_1km",
                   title = expression("Shannon Entropy" ~ (tau[SW])),
                   invec = eu,
                   palette = "plasma",
                   plow = 0.02,
                   pupp = 0.98,
                   buff_low = 0.1,
                   buff_up = 0.09)


rao <- map_create(data = dat1km,
                  col = "rao_1km",
                  title = expression("Rao Quadratic Entropy" ~ (tau[RAO])),
                  invec = eu,
                  palette = "plasma",
                  buff_low = 0.1,
                  buff_up = 0.09)

sd <- map_create(data = dat1km,
                 col = "rh98_1km",
                 title = expression(paste(sigma, " of Canopy Height", " " ~ (tau[CH]))),
                 invec = eu,
                 palette = "plasma",
                 buff_low = 0.1,
                 buff_up = 0.09)

skew <- map_create(data = dat1km,
                   col = "skew_1km",
                   title = expression("Skewness of vertical profile" ~ (tau[SK])),
                   invec = eu,
                   palette = "plasma",
                   buff_low = 0.1,
                   buff_up = 0.09)




cover <- map_create(data = dat1km,
                    col = "cover_1km" ,
                    title = expression(paste(sigma, " of Canopy Cover", " " ~ (tau[CC]))),
                    invec = eu,
                    palette = "plasma",
                    rounding_fact = 2,
                    correct_fact = 0.01,
                    prop = 0.1,
                    buff_low = 0.1,
                    buff_up = 0.09)

cv <- map_create(data =  dat1km,
                 col = "cv_1km" ,
                 title = expression("CV of vertical profile" ~ (tau[CV])),
                 invec = eu,
                 palette = "plasma",
                 buff_low = 0.1,
                 buff_up = 0.09)


kurt <- map_create(data = dat1km,
                   col = "kurt_1km",
                   title = expression("Kurtosis of vertical profile" ~ (tau[KU])),
                   invec = eu,
                   palette = "plasma",
                   buff_low = 0.1,
                   buff_up = 0.09)

# Calculate the quantile limits

plotcombined <- (cv | skew) / 
  (kurt | sd) / 
  (cover  | shan) /
  (rao |plot_spacer())

combined1km <- plotcombined &
  theme(plot.margin = unit(c(1, 1, 1, 1), "mm"))




ggsave(combined1km, filename = "./results/strmodels/paper_figures/Fig1/Predictions1km.eps", bg = "white",
       width = 11, height = 12)


# Fig. 3 climate spaces  ---------------------------------------------------------------------
shan_10km <- heatmap_create(data = dat10kmenv,
                            col = "shannon_10km",
                            title = expression("Shannon Entropy" ~ (tau[SW])),
                            xlab = "Annual Mean Temperature [°C]",
                            ylab = "Annual accumulated precipitation [mm/y]")

rao_10km <- heatmap_create(data = dat10kmenv,
                           col = "rao_10km",
                           title = expression("Rao Quadratic Entropy" ~ (tau[RAO])),
                           xlab = "Annual Mean Temperature [°C]",
                           ylab = "Annual accumulated precipitation [mm/y]")


sd_10km <- heatmap_create(data = dat10kmenv,
                          col = "rh98_10km",
                          title = expression(paste(sigma, " of Canopy Height", " " ~ (tau[CH]))),
                          xlab = "Annual Mean Temperature [°C]",
                          ylab = "Annual accumulated precipitation [mm/y]")

skew_10km <- heatmap_create(data = dat10kmenv,
                            col = "skew_10km",
                            title = expression("Skewness of vertical profile" ~ (tau[SK])),
                            xlab = "Annual Mean Temperature [°C]",
                            ylab = "Annual accumulated precipitation [mm/y]")

hull_10km <- heatmap_create(data = dat10kmenv,
                            col = "hull_10km",
                            title = expression("Convex hull volume" ~ (tau[CHV])),
                            xlab = "Annual Mean Temperature [°C]",
                            ylab = "Annual accumulated precipitation [mm/y]")

cover_10km <- heatmap_create(data = dat10kmenv,
                             col = "cover_10km",
                             title = expression(paste(sigma, " of Canopy Cover", " " ~ (tau[CC]))),
                             xlab = "Annual Mean Temperature [°C]",
                             ylab = "Annual accumulated precipitation [mm/y]")

cv_10km <- heatmap_create(data = dat10kmenv,
                          col = "cv_10km",
                          title = expression("CV of vertical profile" ~ (tau[CV])),
                          xlab = "Annual Mean Temperature [°C]",
                          ylab = "Annual accumulated precipitation [mm/y]")

kurt_10km <- heatmap_create(data = dat10kmenv,
                            col = "kurt_10km",
                            title = expression("Kurtosis of vertical profile" ~ (tau[KU])),
                            xlab = "Annual Mean Temperature [°C]",
                            ylab = "Annual accumulated precipitation [mm/y]")



combined_10km <- cv_10km + skew_10km+ kurt_10km + 
  sd_10km+ cover_10km+shan_10km + rao_10km+hull_10km+
  plot_layout(ncol =2, axis_titles = "collect")+
  theme(plot.margin = unit(c(1, 1, 1, 1), "mm"))

# Print the combined plot
ggsave(combined_10km, filename = "./results/strmodels/paper_figures/Fig2/Climate_space_10km.eps",
       width = 9.5, height = 12, dpi = 400, bg = "white")

# 5km

shan_5km <- heatmap_create(data = dat5kmenv,
                           col = "shannon_5km",
                           title = expression("Shannon Entropy" ~ (tau[SW])),
                           xlab = "Annual Mean Temperature [°C]",
                           ylab = "Annual accumulated precipitation [mm/y]")

rao_5km <- heatmap_create(data = dat5kmenv,
                          col = "rao_5km",
                          title = expression("Rao Quadratic Entropy" ~ (tau[RAO])),
                          xlab = "Annual Mean Temperature [°C]",
                          ylab = "Annual accumulated precipitation [mm/y]")


sd_5km <- heatmap_create(data = dat5kmenv,
                         col = "rh98_5km",
                         title = expression(paste(sigma, " of Canopy Height", " " ~ (tau[CH]))),
                         xlab = "Annual Mean Temperature [°C]",
                         ylab = "Annual accumulated precipitation [mm/y]")

skew_5km <- heatmap_create(data = dat5kmenv,
                           col = "skew_5km",
                           title = expression("Skewness of vertical profile" ~ (tau[SK])),
                           xlab = "Annual Mean Temperature [°C]",
                           ylab = "Annual accumulated precipitation [mm/y]")

hull_5km <- heatmap_create(data = dat5kmenv,
                           col = "hull_5km",
                           title = expression("Convex hull volume" ~ (tau[CHV])),
                           xlab = "Annual Mean Temperature [°C]",
                           ylab = "Annual accumulated precipitation [mm/y]")

cover_5km <- heatmap_create(data = dat5kmenv,
                            col = "cover_5km",
                            title = expression(paste(sigma, " of Canopy Cover", " " ~ (tau[CC]))),
                            xlab = "Annual Mean Temperature [°C]",
                            ylab = "Annual accumulated precipitation [mm/y]")

cv_5km <- heatmap_create(data = dat5kmenv,
                         col = "cv_5km",
                         title = expression("CV of vertical profile" ~ (tau[CV])),
                         xlab = "Annual Mean Temperature [°C]",
                         ylab = "Annual accumulated precipitation [mm/y]")

kurt_5km <- heatmap_create(data = dat5kmenv,
                           col = "kurt_5km",
                           title = expression("Kurtosis of vertical profile" ~ (tau[KU])),
                           xlab = "Annual Mean Temperature [°C]",
                           ylab = "Annual accumulated precipitation [mm/y]")



combined_5km <- cv_5km + skew_5km+ kurt_5km + 
  sd_5km+ cover_5km+shan_5km + rao_5km+hull_5km+
  plot_layout(ncol =2, axis_titles = "collect")+
  theme(plot.margin = unit(c(1, 1, 1, 1), "mm"))

# Print the combined plot
ggsave(combined_5km, filename = "./results/strmodels/paper_figures/Supplementary/FigS2climatespace/Climate_space_5km.eps",
       width = 9.5, height = 12, dpi = 400, bg = "white")

# 1km
shan_1km <- heatmap_create(data = dat1kmenv,
                           col = "shannon_1km",
                           title = expression("Shannon Entropy" ~ (tau[SW])),
                           xlab = "Annual Mean Temperature [°C]",
                           ylab = "Annual accumulated precipitation [mm/y]")

rao_1km <- heatmap_create(data = dat1kmenv,
                          col = "rao_1km",
                          title = expression("Rao Quadratic Entropy" ~ (tau[RAO])),
                          xlab = "Annual Mean Temperature [°C]",
                          ylab = "Annual accumulated precipitation [mm/y]")+
                          ylab("")


sd_1km <- heatmap_create(data = dat1kmenv,
                         col = "rh98_1km",
                         title = expression(paste(sigma, " of Canopy Height", " " ~ (tau[CH]))),
                         xlab = "Annual Mean Temperature [°C]",
                         ylab = "Annual accumulated precipitation [mm/y]")

skew_1km <- heatmap_create(data = dat1kmenv,
                           col = "skew_1km",
                           title = expression("Skewness of vertical profile" ~ (tau[SK])),
                           xlab = "Annual Mean Temperature [°C]",
                           ylab = "Annual accumulated precipitation [mm/y]")

cover_1km <- heatmap_create(data = dat1kmenv,
                            col = "cover_1km",
                            title = expression(paste(sigma, " of Canopy Cover", " " ~ (tau[CC]))),
                            xlab = "Annual Mean Temperature [°C]",
                            ylab = "Annual accumulated precipitation [mm/y]")

cv_1km <- heatmap_create(data = dat1kmenv,
                         col = "cv_1km",
                         title = expression("CV of vertical profile" ~ (tau[CV])),
                         xlab = "Annual Mean Temperature [°C]",
                         ylab = "Annual accumulated precipitation [mm/y]")

kurt_1km <- heatmap_create(data = dat1kmenv,
                           col = "kurt_1km",
                           title = expression("Kurtosis of vertical profile" ~ (tau[KU])),
                           xlab = "Annual Mean Temperature [°C]",
                           ylab = "Annual accumulated precipitation [mm/y]")



combined_1km <- cv_1km + skew_1km+ kurt_1km + 
  sd_1km+ cover_1km+shan_1km + rao_1km+plot_spacer()+
  plot_layout(ncol =2, axis_titles = "collect")+
  theme(plot.margin = unit(c(1, 1, 1, 1), "mm"))

# Print the combined plot
ggsave(combined_1km, filename = "./results/strmodels/paper_figures/Supplementary/FigS2climatespace/Climate_space_1km.eps",
       width = 9.5, height = 12, dpi = 400, bg = "white")


# Fig 4 multipanel -------------------------------------------------------------------

# PCA 
pca_10km <- pca_wrap(indat = dat10km, arrow_scale = 1.9)

# variable selection 
opt_rad_10km <- bar_optrad(indat = vars_10km,
           pred_lab =  pred_all,
           resp_lab = resp)

specific_10km <- bar_specific(indat = vars_10km,
                           pred_lab =  pred_all,
                           resp_lab = resp)
# model performance
r2_10km <- bar_perf(indat = validation_results,
         resolution = "10km")

# combine everything together  
combined_10km <- (opt_rad_10km | specific_10km) / 
  (r2_10km | pca_10km )+
  plot_annotation(tag_levels = c('a'),tag_prefix = '(', tag_suffix = ')') & 
    theme(plot.tag.position = c(0, 1),
          plot.tag = element_text(size = 14, hjust = 0, vjust = 1),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))
  
ggsave(combined_10km, filename = "./results/strmodels/paper_figures/Fig3/Fig3_10km.eps",
       width = 8 , height =9, dpi = 400, bg = "white")








# Supporting Information --------------------------------------------------

# ------ Section 1: Correlation matrices  #

# Using rename with a named vector
cor10km <- dat10km %>%
           dplyr::select(-c(x,y)) %>%
           rename(CC = cover_10km,
                  CV = cv_10km,
                  CVH = hull_10km,
                  KU = kurt_10km,
                  RAO = rao_10km,
                  CH = rh98_10km,
                  SW = shannon_10km,
                  SK = skew_10km) %>%
  cor() %>%
  ggcorrplot(., type = "lower",
             lab = TRUE)+
  scale_x_discrete(labels = c(
    "CV" = expression(tau[CV]),
    "SK" = expression(tau[SK]),
    "KU" = expression(tau[KU]),
    "CH" = expression(tau[CH]),
    "CC" = expression(tau[CC]),
    "SW" = expression(tau[SW]),
    "RAO" = expression(tau[RAO]),
    "CVH" = expression(tau[CVH])
  ))+
  scale_y_discrete(labels = c(
    "CV" = expression(tau[CV]),
    "SK" = expression(tau[SK]),
    "KU" = expression(tau[KU]),
    "CH" = expression(tau[CH]),
    "CC" = expression(tau[CC]),
    "SW" = expression(tau[SW]),
    "RAO" = expression(tau[RAO]),
    "CVH" = expression(tau[CVH])
  ))+
  theme_bw()+
  xlab("")+
  ylab("")+
  theme(axis.text = element_text(size = 15),
        legend.title = element_blank(),
        plot.title = element_text(size = 20, color = "black", hjust = 0.5,
        ))
  
  
cor5km <- dat5km %>%
  dplyr::select(-c(x,y)) %>%
  rename(CC = cover_5km,
         CV = cv_5km,
         CVH = hull_5km,
         KU = kurt_5km,
         RAO = rao_5km,
         CH = rh98_5km,
         SW = shannon_5km,
         SK = skew_5km) %>%
  cor() %>%
  ggcorrplot(., type = "lower",
             lab = TRUE)+
  scale_x_discrete(labels = c(
    "CV" = expression(tau[CV]),
    "SK" = expression(tau[SK]),
    "KU" = expression(tau[KU]),
    "CH" = expression(tau[CH]),
    "CC" = expression(tau[CC]),
    "SW" = expression(tau[SW]),
    "RAO" = expression(tau[RAO]),
    "CVH" = expression(tau[CVH])
  ))+
  scale_y_discrete(labels = c(
    "CV" = expression(tau[CV]),
    "SK" = expression(tau[SK]),
    "KU" = expression(tau[KU]),
    "CH" = expression(tau[CH]),
    "CC" = expression(tau[CC]),
    "SW" = expression(tau[SW]),
    "RAO" = expression(tau[RAO]),
    "CVH" = expression(tau[CVH])
  ))+
  theme_bw()+
  xlab("")+
  ylab("")+
  theme(axis.text = element_text(size = 15),
        legend.title = element_blank(),
        plot.title = element_text(size = 20, color = "black", hjust = 0.5,
        ))  

cor1km <- dat1km %>%
  dplyr::select(-c(x,y)) %>%
  rename(CC = cover_1km,
         CV = cv_1km,
         KU = kurt_1km,
         RAO = rao_1km,
         CH = rh98_1km,
         SW = shannon_1km,
         SK = skew_1km) %>%
  cor() %>%
  ggcorrplot(., type = "lower",
             lab = TRUE)+
  scale_x_discrete(labels = c(
    "CV" = expression(tau[CV]),
    "SK" = expression(tau[SK]),
    "KU" = expression(tau[KU]),
    "CH" = expression(tau[CH]),
    "CC" = expression(tau[CC]),
    "SW" = expression(tau[SW]),
    "RAO" = expression(tau[RAO])
  ))+
  scale_y_discrete(labels = c(
    "CV" = expression(tau[CV]),
    "SK" = expression(tau[SK]),
    "KU" = expression(tau[KU]),
    "CH" = expression(tau[CH]),
    "CC" = expression(tau[CC]),
    "SW" = expression(tau[SW]),
    "RAO" = expression(tau[RAO])
  ))+
  theme_bw()+
  xlab("")+
  ylab("")+
  theme(axis.text = element_text(size = 15),
        legend.title = element_blank(),
        plot.title = element_text(size = 20, color = "black", hjust = 0.5,
        ))  


cor_combined <- cor10km + cor5km + cor1km + plot_spacer()+plot_annotation(tag_levels = 'A')+theme(plot.tag = element_text(size = 40))

ggsave(cor_combined, filename = "./results/strmodels/paper_figures/Supplementary/FigS3_correlationmatrix/cormat.eps",
       height = 10, width = 10, dpi = 400,
       bg = "white")


# ------ Section 2: variable selection  #

# predictor selection at multiple resolutions

selected_vars_heat <- bind_rows(
  vars_1km %>% mutate(Selected_1_km = TRUE),
  vars_5km %>% mutate(Selected_5_km = TRUE),
  vars_10km %>% mutate(Selected_10_km = TRUE)
) %>%
  group_by(predictor, response) %>%
  summarize(
    Selected_1_km = any(Selected_1_km, na.rm = TRUE),
    Selected_5_km = any(Selected_5_km, na.rm = TRUE),
    Selected_10_km = any(Selected_10_km, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Selection_Pattern = case_when(
      Selected_1_km & Selected_5_km & Selected_10_km ~ "Selected at 1 km, 5 km, and 10 km",
      Selected_1_km & Selected_5_km & !Selected_10_km ~ "Selected at 1 km and 5 km",
      Selected_1_km & !Selected_5_km & Selected_10_km ~ "Selected at 1 km and 10 km",
      !Selected_1_km & Selected_5_km & Selected_10_km ~ "Selected at 5 km and 10 km",
      Selected_1_km & !Selected_5_km & !Selected_10_km ~ "Selected at 1 km only",
      !Selected_1_km & Selected_5_km & !Selected_10_km ~ "Selected at 5 km only",
      !Selected_1_km & !Selected_5_km & Selected_10_km ~ "Selected at 10 km only",
      TRUE ~ "Not Selected"
    )
  ) %>%
  mutate(
    Selection_Pattern = factor(Selection_Pattern, levels = c(
      "Selected at 1 km, 5 km, and 10 km",
      "Selected at 1 km only",
      "Selected at 5 km only",
      "Selected at 10 km only",
      "Selected at 1 km and 5 km",
      "Selected at 1 km and 10 km",
      "Selected at 5 km and 10 km"
    ))
  ) %>%
  inner_join(resp) %>% {.->> tmp}  %>%
  # create plot
  ggplot(., aes(x = label1, y = predictor, fill = Selection_Pattern)) +
  geom_tile(color = "white", size = 0.1) +  # Adding white borders for clarity
  scale_fill_manual(values = c(
    "Selected at 1 km only" = "#E6735C",     # darker shade of soft peach
    "Selected at 5 km only" = "#729ECC",     # darker shade of light sky blue
    "Selected at 10 km only" = "#99CC99",    # darker shade of very light green
    "Selected at 1 km and 5 km" = "#BDA6CC", # darker shade of pale lavender
    "Selected at 1 km and 10 km" = "#FFCC66",# darker shade of light lemon
    "Selected at 5 km and 10 km" = "#FFFF99",# darker shade of very pale pink
    "Selected at 1 km, 5 km, and 10 km" = "#C2B280"  # darker shade of pale blue gray
  )) +
  xlab("Diversity Metric")+
  ylab("Predictor")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,
                                   colour = "black",
                                   size = 13),
              legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size = 13,
                                   colour = "black"))+
  scale_x_discrete(labels = c(
    "CC" = expression(tau[CC]),
    "CV" = expression(tau[CV]),
    "CVH" = expression(tau[CVH]),
    "KU" = expression(tau[KU]),
    "RAO" = expression(tau[RAO]),
    "CH" = expression(tau[CH]),
    "SW" = expression(tau[SW]),
    "SK" = expression(tau[SK])))+
  geom_tile(color = "blue", size = 0.3)



ggsave(selected_vars_heat,filename = "./results/strmodels/paper_figures/Supplementary/FigS4_variableselection/selected_variables.eps",
       width = 10, height = 12, dpi = 400, bg = "white")

# ------ Section 3 multipanel plots 5km and 10 km 

# PCAs
pca_5km <- pca_wrap(indat = dat5km, arrow_scale = 1.8)
pca_1km <- pca_wrap(indat = dat1km,res_flag = TRUE, arrow_scale = 1.8)

# barplot radar vs. optical
opt_rad_5km <- bar_optrad(indat = vars_5km,
                          pred_lab =  pred_all,
                          resp_lab = resp)

opt_rad_1km <- bar_optrad(indat = vars_1km,
                          pred_lab =  pred_all,
                          resp_lab = resp)


specific_5km <- bar_specific(indat = vars_5km,
                              pred_lab =  pred_all,
                              resp_lab = resp)


specific_1km <- bar_specific(indat = vars_1km,
                             pred_lab =  pred_all,
                             resp_lab = resp)

r2_5km <- bar_perf(indat = validation_results,
                    resolution = "5km")

r2_1km <- bar_perf(indat = validation_results,
                   resolution = "1km")
# combine everything together  
combined_5km <- (opt_rad_5km | specific_5km) / 
  (r2_5km | pca_5km )+
  plot_annotation(tag_levels = c('a'), tag_prefix = '(',tag_suffix = ')') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 14, hjust = 0, vjust = 1),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))

ggsave(combined_5km, filename = "./results/strmodels/paper_figures/Fig3/Fig3_5km.eps",
       width = 8 , height =9, dpi = 400, bg = "white")

# combine everything together  
combined_1km <- (opt_rad_1km | specific_1km) / 
  (r2_1km | pca_1km )+
  plot_annotation(tag_levels = c('a'), tag_prefix = '(',tag_suffix = ')') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 14, hjust = 0, vjust = 1),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))

ggsave(combined_1km, filename = "./results/strmodels/paper_figures/Fig3/Fig3_1km.eps",
       width = 8 , height =9, dpi = 400, bg = "white")

# ------ Section 4 standard error 5km and 10 km 
# 10km 
filel <- list.files("./data/structure/output_models/se",
                    full.names = TRUE,pattern = "_10km")

dat10km_pred <-map(filel,
           ~se_create(.x,mask = eu,colname = "pred")) %>%
         reduce(inner_join)

dat10km_se <-map(filel,
                 ~se_create(.x,mask = eu,colname = "se")) %>%
  reduce(inner_join) %>%
  mutate(hull_10km = hull_10km/1000)


shan <- map_create(data = dat10km_se,
                   col = "shannon_10km",
                   title = expression("Shannon Entropy" ~ (tau[SW])),
                   invec = eu,
                   palette = "plasma",
                   plow = 0.01,
                   pupp = 0.99,
                   buff_low = 0.1,
                   buff_up = 0.09)


rao <- map_create(data = dat10km_se,
                  col = "rao_10km",
                  title = expression("Rao Quadratic Entropy" ~ (tau[RAO])),
                  invec = eu,
                  palette = "plasma",
                  buff_low = 0.1,
                  buff_up = 0.09)

sd <- map_create(data = dat10km_se,
                 col = "rh98_10km",
                 title = expression(paste(sigma, " of Canopy Height", " " ~ (tau[CH]))),
                 invec = eu,
                 palette = "plasma",
                 buff_low = 0.1,
                 buff_up = 0.09)

skew <- map_create(data = dat10km_se,
                   col = "skew_10km",
                   title = expression("Skewness of vertical profile" ~ (tau[SK])),
                   invec = eu,
                   palette = "plasma",
                   buff_low = 0.1,
                   buff_up = 0.09)


hull <- map_create(data = dat10km_se,
                   col = "hull_10km",
                   title = expression("Convex hull volume" ~ (tau[CHV])),
                   invec = eu,
                   palette = "plasma",
                   buff_low = 0.1,
                   buff_up = 0.09)

cover <- map_create(data = dat10km_se,
                    col = "cover_10km" ,
                    title = expression(paste(sigma, " of Canopy Cover", " " ~ (tau[CC]))),
                    invec = eu,
                    palette = "plasma",
                    rounding_fact = 2,
                    correct_fact = 0.01,
                    prop = 0.1,
                    plow = 0.1,
                    pupp = 1,
                    buff_low = 0.1,
                    buff_up = 0.09,
                    manual_breaks = c(0.01,0.02,0.03,0.04))


cv <- map_create(data =  dat10km_se,
                 col = "cv_10km" ,
                 title = expression("CV of vertical profile" ~ (tau[CV])),
                 invec = eu,
                 palette = "plasma",
                 buff_low = 0.1,
                 buff_up = 0.9,
                 correct_fact = 0.03)


kurt <- map_create(data = dat10km_se,
                   col = "kurt_10km",
                   title = expression("Kurtosis of vertical profile" ~ (tau[KU])),
                   invec = eu,
                   palette = "plasma",
                   buff_low = 0.1,
                   buff_up = 0.09)

# combine plots together
plotcombined <- (cv | skew) / 
  (kurt | sd) / 
  (cover  | shan) /
  (rao |  hull)

combined10km <- plotcombined &
  theme(plot.margin = unit(c(1, 1, 1, 1), "mm"))


ggsave(combined10km, filename = "./results/strmodels/paper_figures/Fig1/SE10km.eps", bg = "white",
       width = 11, height = 12)


# 5km 
filel <- list.files("./data/structure/output_models/se",
                    full.names = TRUE,pattern = "_5km")

dat5km_pred <-map(filel,
                   ~se_create(.x,mask = eu,colname = "pred")) %>%
  reduce(inner_join)

dat5km_se <-map(filel,
                 ~se_create(.x,mask = eu,colname = "se")) %>%
  reduce(inner_join) %>%
  mutate(hull_5km = hull_5km/1000)


shan <- map_create(data = dat5km_se,
                   col = "shannon_5km",
                   title = expression("Shannon Entropy" ~ (tau[SW])),
                   invec = eu,
                   palette = "plasma",
                   plow = 0.01,
                   pupp = 0.99,
                   buff_low = 0.1,
                   buff_up = 0.09)


rao <- map_create(data = dat5km_se,
                  col = "rao_5km",
                  title = expression("Rao Quadratic Entropy" ~ (tau[RAO])),
                  invec = eu,
                  palette = "plasma",
                  buff_low = 0.1,
                  buff_up = 0.09)

sd <- map_create(data = dat5km_se,
                 col = "rh98_5km",
                 title = expression(paste(sigma, " of Canopy Height", " " ~ (tau[CH]))),
                 invec = eu,
                 palette = "plasma",
                 buff_low = 0.1,
                 buff_up = 0.09)

skew <- map_create(data = dat5km_se,
                   col = "skew_5km",
                   title = expression("Skewness of vertical profile" ~ (tau[SK])),
                   invec = eu,
                   palette = "plasma",
                   buff_low = 0.1,
                   buff_up = 0.09)


hull <- map_create(data = dat5km_se,
                   col = "hull_5km",
                   title = expression("Convex hull volume" ~ (tau[CHV])),
                   invec = eu,
                   palette = "plasma",
                   buff_low = 0.1,
                   buff_up = 0.09)

cover <- map_create(data = dat5km_se,
                    col = "cover_5km" ,
                    title = expression(paste(sigma, " of Canopy Cover", " " ~ (tau[CC]))),
                    invec = eu,
                    palette = "plasma",
                    rounding_fact = 2,
                    correct_fact = 0.01,
                    prop = 0.1,
                    plow = 0.1,
                    pupp = 1,
                    buff_low = 0.1,
                    buff_up = 0.09,
                    manual_breaks = c(0.01,0.02,0.03,0.04))


cv <- map_create(data =  dat5km_se,
                 col = "cv_5km" ,
                 title = expression("CV of vertical profile" ~ (tau[CV])),
                 invec = eu,
                 palette = "plasma",
                 buff_low = 0.1,
                 buff_up = 0.9,
                 correct_fact = 0.03)


kurt <- map_create(data = dat5km_se,
                   col = "kurt_5km",
                   title = expression("Kurtosis of vertical profile" ~ (tau[KU])),
                   invec = eu,
                   palette = "plasma",
                   buff_low = 0.1,
                   buff_up = 0.09)

# combine plots together
plotcombined <- (cv | skew) / 
  (kurt | sd) / 
  (cover  | shan) /
  (rao |  hull)

combined5km <- plotcombined &
  theme(plot.margin = unit(c(1, 1, 1, 1), "mm"))


ggsave(combined5km, filename = "./results/strmodels/paper_figures/Fig1/SE5km.eps", bg = "white",
       width = 11, height = 12)

