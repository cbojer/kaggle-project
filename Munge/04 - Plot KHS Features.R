
# Load Packages -----------------------------------------------------------

library(data.table)
library(qs)
library(dplyr)
library(ggplot2)
library(grid)
library(patchwork)

# Initialize Kaggle KHS Feature Data --------------------------------------

if(isTRUE(adjusted_weekly_daily_frequency)) {
  load_path <- paste0(khs_feat_save_path, "adjusted/")
  save_path <- paste0(khs_plot_save_path, "adjusted/")
  if(!dir.exists(save_path)) {
    dir.create(save_path)
  }
} else {
  load_path <- paste0(khs_feat_save_path, "original/")
  save_path <- paste0(khs_plot_save_path, "original/")
  if(!dir.exists(save_path)) {
    dir.create(save_path)
  }
}

# Load KHS features
all_features <- rbindlist(lapply(list.files(load_path, full.names = T), qread))

# Adjust ID's
all_features[, id := dplyr::case_when(id == "favorita-grocery-sales-forecasting" ~ "Corporacion Favorita",
                                      id == "recruit-restaurant-visitor-forecasting" ~ "Recruit Restaurant",
                                      id == "rossmann-store-sales" ~ "Rossmann",
                                      id == "walmart-recruiting-store-sales-forecasting" ~ "Walmart Store Sales",
                                      id == "walmart-recruiting-sales-in-stormy-weather" ~ "Walmart Stormy Weather",
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
if (isTRUE(include_frequency)) {
  pc_data <- all_features[complete.cases(all_features)]
} else {
  pc_data <- all_features[complete.cases(all_features), -"Frequency"]
}

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
point_plots <- sapply(unique_ids, function(x) {
  pc_plot(.data = plot.data, target = x, id_col = "id", bg_id = "M4", 
          x_lims = plot.xlims, y_lims = plot.ylims,
          color_scheme = unique_cols, geom_type = "point")
}, USE.NAMES = TRUE, simplify = FALSE)


#### HEXAGON ####
hexbin_plots <- sapply(unique_ids, function(x) {
  pc_plot(.data = plot.data, target = x, id_col = "id", bg_id = "M4", 
          x_lims = plot.xlims, y_lims = plot.ylims,
          color_scheme = unique_cols, geom_type = "hexbin")
}, USE.NAMES = TRUE, simplify = FALSE)


#### DENSITY ####
density_plots <- sapply(unique_ids, function(x) {
  pc_plot(.data = plot.data, target = x, id_col = "id", bg_id = "M4", 
          x_lims = plot.xlims, y_lims = plot.ylims,
          color_scheme = unique_cols, geom_type = "density")
}, USE.NAMES = TRUE, simplify = FALSE)


# Calculate and Plot PCA Loadings -----------------------------------------

# Pull PC1 & PC2 Loading Data
loading.data <- pc_obj$rotation %>%
  as.data.table(keep.rownames = "rn")

# Define x and y limits
loading.xlims <- c(min(loading.data$PC1*3) %>% floor(),
                   max(loading.data$PC1*3) %>% ceiling())

loading.ylims <- c(min(loading.data$PC2*3) %>% floor(),
                   max(loading.data$PC2*3) %>% ceiling())

# Plot Loadings
loadings <- loading.data %>%
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
labs <- paste0(paste0("PC", 1:2), " (", scales::percent(var_exp, accuracy = 0.01), ")")
x_lab <- grid::textGrob(labs[1], hjust = 0.5)
y_lab <- grid::textGrob(labs[2], rot = 90, vjust = 0.5)

# Construct Patchworks
patchworks <- lapply(list("Point" = point_plots,
                          "Hex" = hexbin_plots,
                          "Density" = density_plots),
                     function(x) {
                       wrap_plots(a=x$M3,                    
                                  b=x$M4,                       
                                  c=x$`Corporacion Favorita`,
                                  d=x$`Recruit Restaurant`,  
                                  e=loadings,                 
                                  f=x$Rossmann,
                                  g=x$`Walmart Store Sales`,               
                                  h=x$`Walmart Stormy Weather`, 
                                  i=x$Wikipedia,
                                  x = x_lab, y = y_lab) +
                         plot_layout(ncol = 4, nrow = 4,
                                     widths = c(0.1, 1, 1, 1),
                                     heights = c(1, 1, 1, 0.1), 
                                     design = layout)
                     })

# Save Final Plots --------------------------------------------------------

if(isTRUE(adjusted_weekly_daily_frequency)) {
  if(isTRUE(include_frequency)) {
    ggsave(paste0(save_path, "khs_feature_point_adj_with_freq.png"), patchworks$Point, width = 12, height = 10)
    ggsave(paste0(save_path, "khs_feature_hexagons_adj_with_freq.png"), patchworks$Hex, width = 12, height = 10)
    ggsave(paste0(save_path, "khs_feature_densities_adj_with_freq.png"), patchworks$Density, width = 12, height = 10)
  } else {
    ggsave(paste0(save_path, "khs_feature_point_adj.png"), patchworks$Point, width = 12, height = 10)
    ggsave(paste0(save_path, "khs_feature_hexagons_adj.png"), patchworks$Hex, width = 12, height = 10)
    ggsave(paste0(save_path, "khs_feature_densities_adj.png"), patchworks$Density, width = 12, height = 10)
  }
} else {
  if(isTRUE(include_frequency)) {
    ggsave(paste0(save_path, "khs_feature_point_orig_with_freq.png"), patchworks$Point, width = 12, height = 10)
    ggsave(paste0(save_path, "khs_feature_hexagons_orig_with_freq.png"), patchworks$Hex, width = 12, height = 10)
    ggsave(paste0(save_path, "khs_feature_densities_orig_with_freq.png"), patchworks$Density, width = 12, height = 10)
  } else {
    ggsave(paste0(save_path, "khs_feature_point_orig.png"), patchworks$Point, width = 12, height = 10)
    ggsave(paste0(save_path, "khs_feature_hexagons_orig.png"), patchworks$Hex, width = 12, height = 10)
    ggsave(paste0(save_path, "khs_feature_densities_orig.png"), patchworks$Density, width = 12, height = 10)
  }
}
