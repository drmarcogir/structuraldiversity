# functions.R
# 
# This script contains a collection of R functions used in the manuscript titled 
# 'A dataset on the structural diversity of European forests' by Marco Girardello et al. 
# (under review). The functions provided here are designed to assist in the data 
# preparation, analysis, and visualization processes as described in the manuscript.
#
# Usage:
# This script is automatically sourced via the .Rprofile file in the repository, so 
# there is no need to manually source it in your R environment. The functions will be 
# available for use throughout your analysis scripts.
#
# Functions included:
# A variety of functions are included to handle different aspects of the data analysis 
# and visualization tasks. Due to the complexity and number of functions, brief 
# descriptions are not provided here. Please refer to the function definitions and 
# inline comments for details on usage and parameters.
#
# Dependencies:
# Ensure that the following R packages are installed and loaded in your environment:
# - tidyverse
# - sf
# - terra
# - viridis
# - scales
# - patchwork
# - factoextra
# - ggrepel
# - ggcorrplot
#
# Authors: Marco Girardello
# Date: 2024-07-23
#
# License:
# This script is released under the [License Name] license. For more details, see the 
# LICENSE file in the repository.
#
# Note:
# Please report any issues or bugs on the GitHub repository issues page.


# PCA plot function
pca_wrap <- function(indat,arrow_scale,res_flag = FALSE) {
  
  # Compute PCA, excluding 'x' and 'y' columns
  pca_obj <- indat %>%
    dplyr::select(-c(x, y)) %>%
    prcomp(center = TRUE, scale. = TRUE)
  
  if (res_flag) {
    corrs <- pca_obj$rotation %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "name") %>%
      dplyr::mutate(label = c(
        "tau[CC]", "tau[CV]", "tau[KU]","tau[RAO]",
        "tau[CH]","tau[SW]","tau[SK]"
      ))
  } else {
    # Prepare correlations for plot arrows (based on PCA rotation)
    corrs <- pca_obj$rotation %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "name") %>%
      dplyr::mutate(label = c(
        "tau[CC]", "tau[CV]", "tau[CVH]", "tau[KU]",
        "tau[RAO]", "tau[CH]", "tau[SW]", "tau[SK]"
      ))
  }  
  
  # Extracting and tidying the PCA loadings
  var_coords <- pca_obj$rotation %>%
    dplyr::as_tibble()
  
  # Scale scores to be between -1 and 1, if necessary
  scores_df <- pca_obj$x[, 1:2] %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(max_score = max(abs(c(PC1, PC2)))) %>%
    dplyr::mutate(
      PC1 = dplyr::if_else(max_score > 1, PC1 / max_score, PC1),
      PC2 = dplyr::if_else(max_score > 1, PC2 / max_score, PC2)
    ) %>%
    dplyr::select(-max_score)  # Optionally drop the max_score column if no longer needed
  
  # Create PCA biplot
  ggplot(data = scores_df, aes(x = PC1, y = PC2)) +
    xlim(c(-1, 1)) + ylim(c(-1, 1)) +  # Set limits to standard -1 to 1 range
    theme_bw() +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 9, color = "black"),
      axis.ticks = element_line(color = "black")
    ) +
    coord_fixed() +
    geom_text_repel(
      data = corrs, aes(x = PC1, y = PC2, label = label),
      color = "red", size = 4, parse = TRUE
    ) +
    geom_segment(
      data = corrs, aes(x = 0, y = 0, xend = PC1 * arrow_scale, yend = PC2 * arrow_scale),
      arrow = arrow(type = "closed", length = unit(0.05, "inches")),
      size = 0.6, color = "steelblue"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray")
}



map_create <- function(data, col, title, invec, plow = 0.01, pupp = 0.99, palette = "magma", num_breaks = 4,
                       rounding_fact  = 2, correct_fact = 0.1,
                       prop = 0.1, buff_low = NULL, buff_up = NULL,
                       manual_breaks = NULL) {
  # Calculate the quantile limits
  quant_limits <- quantile(data %>% pull(!!sym(col)), prob = c(plow, pupp), na.rm = TRUE)
  
  quant_limits[1] <- quant_limits[1]-(quant_limits[2] - quant_limits[1]) * buff_low
  quant_limits[2] <- quant_limits[2]+(quant_limits[2] - quant_limits[1]) * buff_up
  
  quant_limits1 <- c(quantile(data %>% pull(!!sym(col)), prob = c(plow, pupp), na.rm = TRUE)[1],
                     quant_limits[2])
  
  # Generate evenly spaced breaks within the quantile limits
  if (!is.null(manual_breaks)) {
    breaks <- manual_breaks
  } else {
    breaks <- floor(seq(quant_limits1[1], quant_limits1[2], length.out = 4) * 100) / 100
  }
  
  plot <- data %>%
    select(x, y, !!sym(col)) %>%
    ggplot() +
    geom_sf(data = invec, fill = "grey90") +
    geom_tile(aes(x = x, y = y, fill = !!sym(col))) +
    geom_sf(data = invec, fill = "NA", colour = "black", linewidth = 0.1) +
    coord_sf(xlim = c(2485489,7465739), ylim = c(1203755,4021836)) +
    theme_bw() +
    theme(legend.position = "right",
          legend.text = element_text(size = 8),
          plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt"),
          legend.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_fill_viridis(option = palette, limits = quant_limits, oob = scales::squish, breaks = breaks) +
    guides(fill = guide_colourbar(barwidth = 1.5, barheight = 6, title.position = "top", title.hjust = 0.5)) +
    ggtitle(title)
  
  return(plot)
  
}  



# optical vs. radar barchart
bar_optrad <- function(indat, pred_lab, resp_lab) {
  # Data processing steps
  indat %>%
    inner_join(pred_lab) %>%  
    group_by(response, type_general) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(response) %>%
    mutate(total = sum(count)) %>%
    ungroup() %>%
    inner_join(resp_lab) %>%  
    mutate(proportion = count / total) %>%
    # Generate plot
    ggplot(aes(x = proportion, y = label1, fill = type_general)) +
    geom_bar(stat = "identity", position = "stack", width = 0.8) +
    theme_bw() +
    xlab("Proportion selected") +
    ylab("") +
    scale_y_discrete(labels = c(
      "CV" = expression(tau[CV]),
      "SK" = expression(tau[SK]),
      "KU" = expression(tau[KU]),
      "CH" = expression(tau[CH]),
      "CC" = expression(tau[CC]),
      "SW" = expression(tau[SW]),
      "RAO" = expression(tau[RAO]),
      "CVH" = expression(tau[CVH])
    )) +
    scale_fill_manual(values = c("steelblue", "sandybrown")) +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_text(size = 10, colour = "black"),
      axis.title = element_text(size = 14),
      axis.text.y = element_text(size = 10, colour = "black"),
      legend.text = element_text(size = 8),
      title = element_text(hjust = 0.5, size = 21),
      legend.position = "bottom"
    ) +
    geom_vline(xintercept = 0.5, linetype = "dashed", size = 1)
}

# specific types barchart
bar_specific <- function(indat, pred_lab, resp_lab) {
  # Data processing steps
  indat %>%
    inner_join(pred_lab) %>%  
    group_by(response, type_specific) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(response) %>%
    mutate(total = sum(count)) %>%
    ungroup() %>%
    inner_join(resp_lab) %>%  
    mutate(proportion = count / total) %>%
    # Generate plot
    ggplot(aes(x = proportion, y = label1, fill = type_specific)) +
    geom_bar(stat = "identity", position = "stack", width = 0.8) +
    theme_bw() +
    xlab("Proportion selected") +
    ylab("") +
    scale_y_discrete(labels = c(
      "CV" = expression(tau[CV]),
      "SK" = expression(tau[SK]),
      "KU" = expression(tau[KU]),
      "CH" = expression(tau[CH]),
      "CC" = expression(tau[CC]),
      "SW" = expression(tau[SW]),
      "RAO" = expression(tau[RAO]),
      "CVH" = expression(tau[CVH])
    )) +
    scale_fill_manual(values=c("#c89595", "#9ea487", "#7393B3", "#F0DC82")) +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = 10, colour = "black"),
          axis.title = element_text(size = 14, hjust = 0.5),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.text = element_text(size = 8),
          legend.position = "bottom")+
    geom_vline(xintercept = 0.5, linetype = "dashed", size = 1)+
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
}


rename_cols <- function(df, suffix) {
  df <- df %>%
    dplyr::select(-c(x,y))
  
  base_mapping <- c(
    CC = "cover",
    CV = "cv",
    CH = "hull",
    KU = "kurt",
    RAO = "rao",
    RH98 = "rh98",
    SW = "shannon",
    SK = "skew"
  )
  # Generate the renaming vector with suffix
  rename_vector <- setNames(
    paste0(base_mapping, suffix),  # Concatenate base part with suffix
    names(base_mapping)            # New names as specified
  )
  
  # Filter out rename pairs where the column does not exist in the dataframe
  existing_columns <- names(df)
  rename_vector <- rename_vector[as.character(rename_vector) %in% existing_columns]
  
  # Rename columns using the generated rename vector
  df %>% rename(!!!rename_vector)
}

heatmap_create <- function(data, col, title, xlab = NULL, ylab  = NULL) {
  data %>%
    mutate(temperature_mean = temperature_mean/10) %>%
    ggplot(aes(x = temperature_mean, y = precipitation_mean, z = !!sym(col))) +
    stat_summary_2d(
      fun = function(x) {
        if (length(x[!is.infinite(x)]) >= 5) {
          mean(x)
        } else {
          NA
        }
      },
      bins = 30,
      na.rm = FALSE
    ) +
    scale_fill_viridis(option = "plasma") +
    theme_bw() +
    theme(
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 16),
      plot.title = element_text(size = 14, hjust = 0.5),
      legend.title=element_blank()) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(title)
}



bar_perf <- function(indat, resolution) {
  
  # Handle data resolutions of 10km or 5km
  if (resolution == "10km" || resolution == "5km") {
    
    # Define a lookup table using a tibble
    lookup_r2 <- tibble(
      Metric = c("skew", "shannon", "rh98", "rao", "kurt", "hull", "cv", "cover"),
      label1 = factor(
        c("SK", "SW", "CH","RAO","KU", "CVH","CV","CC"),
        levels = c("SK", "SW", "CH","RAO","KU", "CVH","CV","CC"))) %>%
      mutate(label1 = fct_relevel(label1,
                                  rev(c("CV","SK",
                                        "KU","CH",
                                        "CC","SW",
                                        "RAO","CVH"))))
    
    # Create a plot
    indat %>%
      filter(!!resolution == resolution) %>%
      inner_join(lookup_r2) %>%
      ggplot(aes(x = R2, y = label1, fill = type)) +
      geom_bar(position = "dodge", stat = "identity") +
      theme_bw() +
      ylab("") +
      scale_y_discrete(labels = c(
        "CV" = expression(tau[CV]),
        "SK" = expression(tau[SK]),
        "KU" = expression(tau[KU]),
        "CH" = expression(tau[CH]),
        "CC" = expression(tau[CC]),
        "SW" = expression(tau[SW]),
        "RAO" = expression(tau[RAO]),
        "CVH" = expression(tau[CVH])
      )) +
      scale_fill_manual(values = c("darkolivegreen", "rosybrown")) +
      xlab(expression('R'^2)) +
      theme(
        legend.title = element_blank(),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.text = element_text(size = 8),
        title = element_text(hjust = 0.5, size = 21),
        legend.position = "bottom"
      )
    
  } else {
    # Define a lookup table using a tibble
    lookup_r2 <- tibble(
      Metric = c("skew", "shannon", "rh98", "rao", "kurt", "cv", "cover"),
      label1 = factor(
        c("SK", "SW", "CH","RAO","KU", "CV","CC"),
        levels = c("SK", "SW", "CH","RAO","KU","CV","CC"))) %>%
      mutate(label1 = fct_relevel(label1,
                                  rev(c("CV","SK",
                                        "KU","CH",
                                        "CC","SW",
                                        "RAO"))))
    
    indat %>%
      filter(!!resolution == resolution) %>%
      inner_join(lookup_r2) %>%
      ggplot(aes(x = R2, y = label1, fill = type)) +
      geom_bar(position = "dodge", stat = "identity") +
      theme_bw() +
      ylab("") +
      scale_y_discrete(labels = c(
        "CV" = expression(tau[CV]),
        "SK" = expression(tau[SK]),
        "KU" = expression(tau[KU]),
        "CH" = expression(tau[CH]),
        "CC" = expression(tau[CC]),
        "SW" = expression(tau[SW]),
        "RAO" = expression(tau[RAO])
      )) +
      scale_fill_manual(values = c("darkolivegreen", "rosybrown")) +
      xlab(expression('R'^2)) +
      theme(
        legend.title = element_blank(),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.text = element_text(size = 8),
        title = element_text(hjust = 0.5, size = 21),
        legend.position = "bottom"
      )
  }
}


se_create <- function(file_paths, colname, mask = NULL) {
  
  # Extract the base name and remove file extension to get a variable name
  var_name <- tools::file_path_sans_ext(basename(file_paths))
  
  # Read data from CSV
  data <- read_csv(file_paths)
  
  
  # Create a raster from the specified column
  rast_var <- terra::rast(data, type = "xyz", crs = "EPSG:3035")
  
  # Apply mask if provided
  rast_var <- mask(rast_var, mask)
  
  # Convert raster to a data frame and rename the column
  var_df <- as.data.frame(rast_var, xy = TRUE) %>%
    as_tibble() %>%
    dplyr::select(!!c("x","y",colname)) %>%
    rename_with(~var_name, colname)
}
