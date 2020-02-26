
# Load Packages -----------------------------------------------------------

library(data.table)
library(dplyr)
library(ggplot2)
library(grid)
library(patchwork)

# Save variables from Global Environment ----------------------------------

variables_to_keep <- ls()

# Initialize Kaggle KHS Feature Data --------------------------------------

if(adjusted_weekly_daily_frequency == TRUE) {
  load_path <- paste0(khs_feat_save_path, "adjusted/")
} else {
  load_path <- paste0(khs_feat_save_path, "original/")
}

# Load KHS features
all_features <- rbindlist(lapply(list.files(load_path, full.names = T), readRDS))

# Adjust ID's
all_features[, id := dplyr::case_when(id == "favorita-grocery-sales-forecasting" ~ "Corporacion Favorita",
                                      id == "recruit-restaurant-visitor-forecasting" ~ "Recruit Restaurant",
                                      id == "rossmann-store-sales" ~ "Rossmann",
                                      id == "walmart-recruiting-store-sales-forecasting" ~ "Walmart",
                                      id == "walmart-storm-weather-competition" ~ "Walmart Stormy Weather",
                                      id == "web-traffic-time-series-forecasting" ~ "Wikipedia",
                                      TRUE ~ id)]

# Investigate % of rows that contain NA's
all_features[, .("% of rows without NA" = scales::percent(sum(complete.cases(.SD))/.N)), id]

# Fill NA's & NaN's with 0
num_cols <- names(all_features)[which(sapply(all_features, is.numeric))]

for(col_name in num_cols) {
  set(all_features, all_features[, which(is.na(eval(str2lang(col_name))))], col_name, 0)
  set(all_features, all_features[, which(is.nan(eval(str2lang(col_name))))], col_name, 0)
}

# Sort data by ID
setkey(all_features, "id")


# Extract Principal Components from KHS Features --------------------------

# Make PCA
pc_data <- all_features[complete.cases(all_features), -"Frequency"]
pc_obj <- prcomp(pc_data[, -c("N", "Period", "id")], scale=TRUE)

# Pull PC1 & PC2 for all features and bind ID
plot.data <- pc_obj$x %>% 
  as.data.table() %>%
  cbind(id = pc_data$id)

# Define x and y plot limits
plot.xlims <- c(min(plot.data$PC1) %>% floor(),
                max(plot.data$PC1) %>% ceiling())

plot.ylims <- c(min(plot.data$PC2) %>% floor(),
                max(plot.data$PC2) %>% ceiling())

# Get Unique IDS and Define Color Scheme
unique_ids <- unique(plot.data$id)
color_scheme <- ggthemes::ptol_pal()(length(unique_ids))
unique_cols <- sapply(unique_ids, function(x) {
  color_scheme[grep(eval(x), unique_ids)][1]
})

# Generate PCA Plots ------------------------------------------------------

#### POINT ####
pc_point_plots <- sapply(unique_ids, function(x) {
  if( x == "M4") {
    bg.plot.data <- plot.data
  } else {
    bg.plot.data <- plot.data[id == "M4", ]
  }
  
  tmp <- plot.data[id == eval(x), ]
  
  ggplot() +
    geom_point(aes(x=PC1, y=PC2), colour = "grey92", data = bg.plot.data, na.rm = T) +
    geom_point(aes(x=PC1, y=PC2), colour = unique_cols[x], data = tmp, na.rm = T) +
    theme_bw(base_size = 12) +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    xlim(plot.xlims) + ylim(plot.ylims) +
    facet_wrap(~eval(x))
  
}, USE.NAMES = TRUE, simplify = FALSE)


#### HEXAGON ####
pc_hex_plots <- sapply(unique_ids, function(x) {
  if( x == "M4") {
    bg.plot.data <- plot.data
  } else {
    bg.plot.data <- plot.data[id == "M4", ]
  }
  
  tmp <- plot.data[id == eval(x), ]
  
  ggplot() +
    geom_hex(aes(x=PC1, y=PC2), fill = "grey92", bins = 20, data = bg.plot.data, na.rm = T) +
    geom_hex(aes(x=PC1, y=PC2), data = tmp, bins = 20, na.rm = T) +
    scale_fill_gradient(low = "grey50", high = "navyblue", trans = "log10") +
    theme_bw(base_size = 12) +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none") +
    xlim(plot.xlims) + ylim(plot.ylims) +
    facet_wrap(~eval(x))
  
}, USE.NAMES = TRUE, simplify = FALSE)


#### DENSITY ####
pc_density_plots <- sapply(unique_ids, function(x) {
  if( x == "M4") {
    bg.plot.data <- plot.data
  } else {
    bg.plot.data <- plot.data[id == "M4", ]
  }
  
  tmp <- plot.data[id == eval(x), ]
  
  ggplot() +
    stat_density2d(aes(x=PC1, y=PC2), geom = "polygon", data = bg.plot.data, na.rm = T) +
    stat_density2d(aes(x=PC1, y=PC2, fill = ..level..), geom = "polygon", data = tmp, na.rm = T) +
    theme_bw(base_size = 12) +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none") +
    xlim(plot.xlims) + ylim(plot.ylims) +
    facet_wrap(~eval(x))
}, USE.NAMES = TRUE, simplify = FALSE)


# Calculate and Plot PCA Loadings -----------------------------------------

# Pull PC1 & PC2 Loading Data
loading.data <- pc_obj$rotation %>%
  as.data.table(keep.rownames = "rn")

# Define x and y plot limits
loading.xlims <- c(min(loading.data$PC1*3) %>% floor(),
                   max(loading.data$PC1*3) %>% ceiling())

loading.ylims <- c(min(loading.data$PC2*3) %>% floor(),
                   max(loading.data$PC2*3) %>% ceiling())

#### POINT ####
pc_point_plots[["loadings"]] <- loading.data %>%
  ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = PC1*2.5, yend = PC2*2.5),
               arrow = grid::arrow(length = grid::unit(8, "points"), type = "closed"),
               col = "firebrick") +
  geom_text(aes(x = PC1*3, y = PC2*3, label = rn), col = "firebrick") +
  xlim(loading.xlims) + ylim(loading.ylims) +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~"Loadings")

#### HEXAGON ####
pc_hex_plots[["loadings"]] <- loading.data %>%
  ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = PC1*2.5, yend = PC2*2.5),
               arrow = grid::arrow(length = grid::unit(8, "points"), type = "closed"),
               col = "firebrick") +
  geom_text(aes(x = PC1*3, y = PC2*3, label = rn), col = "firebrick") +
  xlim(loading.xlims) + ylim(loading.ylims) +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~"Loadings")

#### DENSITY ####
pc_density_plots[["loadings"]] <- loading.data %>%
  ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = PC1*2.5, yend = PC2*2.5),
               arrow = grid::arrow(length = grid::unit(8, "points"), type = "closed"),
               col = "firebrick") +
  geom_text(aes(x = PC1*3, y = PC2*3, label = rn), col = "firebrick") +
  xlim(loading.xlims) + ylim(loading.ylims) +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~"Loadings")


# Structure Final Plot ----------------------------------------------------

# Define Layout for Plot (Loadings [e] will be placed in the middle)
layout <- 'yabc \n ydef \n yghi \n xxxx'

# Calculate Variance Explained by PC's
var_exp <- pc_obj$sdev^2/sum(pc_obj$sdev^2)
var_exp <- var_exp[c(1,2)]

# Concatenate Variance Explained with 
labs <- paste0(paste0("PC", 1:2), " (", scales::percent(var_exp), ")")
x_lab <- grid::textGrob(labs[1], hjust = 0.5)
y_lab <- grid::textGrob(labs[2], rot = 90, vjust = 0.5)

#### POINT ####
point_patchwork <- wrap_plots(a=pc_point_plots$M3,                    
                              b=pc_point_plots$M4,                       
                              c=pc_point_plots$`Corporacion Favorita`,
                              d=pc_point_plots$`Recruit Restaurant`,  
                              e=pc_point_plots$loadings,                 
                              f=pc_point_plots$Rossmann,
                              g=pc_point_plots$Walmart,               
                              h=pc_point_plots$`Walmart Stormy Weather`, 
                              i=pc_point_plots$Wikipedia,
                              x = x_lab, y = y_lab) +
  plot_layout(ncol = 4, nrow = 4,
              widths = c(0.1, 1, 1, 1),
              heights = c(1, 1, 1, 0.1), 
              design = layout)

#### HEXAGON ####
hex_patchwork <- wrap_plots(a=pc_hex_plots$M3,                    
                            b=pc_hex_plots$M4,                       
                            c=pc_hex_plots$`Corporacion Favorita`,
                            d=pc_hex_plots$`Recruit Restaurant`,  
                            e=pc_hex_plots$loadings,                 
                            f=pc_hex_plots$Rossmann,
                            g=pc_hex_plots$Walmart,               
                            h=pc_hex_plots$`Walmart Stormy Weather`, 
                            i=pc_hex_plots$Wikipedia,
                            x = x_lab, y = y_lab) +
  plot_layout(ncol = 4, nrow = 4,
              widths = c(0.1, 1, 1, 1),
              heights = c(1, 1, 1, 0.1), 
              design = layout)

#### DENSITY ####
density_patchwork <- wrap_plots(a=pc_density_plots$M3,                    
                                b=pc_density_plots$M4,                       
                                c=pc_density_plots$`Corporacion Favorita`,
                                d=pc_density_plots$`Recruit Restaurant`,  
                                e=pc_density_plots$loadings,                 
                                f=pc_density_plots$Rossmann,
                                g=pc_density_plots$Walmart,               
                                h=pc_density_plots$`Walmart Stormy Weather`, 
                                i=pc_density_plots$Wikipedia,
                                x = x_lab, y = y_lab) +
  plot_layout(ncol = 4, nrow = 4,
              widths = c(0.1, 1, 1, 1),
              heights = c(1, 1, 1, 0.1), 
              design = layout)


# Save Final Plots --------------------------------------------------------

if(adjusted_weekly_daily_frequency == TRUE) {
  save_path <- paste0(khs_plot_save_path, "adjusted/")
  if(!dir.exists(save_path)) {
    dir.create(save_path)
  }
  ggsave(paste0(save_path, "khs_feature_point_adj_with_freq.png"), point_patchwork, width = 12, height = 10)
  ggsave(paste0(save_path, "khs_feature_hexagons_adj_with_freq.png"), hex_patchwork, width = 12, height = 10)
  ggsave(paste0(save_path, "khs_feature_densities_adj_with_freq.png"), density_patchwork, width = 12, height = 10)
} else {
  save_path <- paste0(khs_plot_save_path, "original/")
  if(!dir.exists(save_path)) {
    dir.create(save_path)
  }
  ggsave(paste0(save_path, "khs_feature_point_orig_with_freq.png"), point_patchwork, width = 12, height = 10)
  ggsave(paste0(save_path, "khs_feature_hexagons_orig_with_freq.png"), hex_patchwork, width = 12, height = 10)
  ggsave(paste0(save_path, "khs_feature_densities_orig_with_freq.png"), density_patchwork, width = 12, height = 10)
}



# Remove variables created in script --------------------------------------

rm(list = ls()[!ls() %in% variables_to_keep]) ; invisible(gc())
