
# Plot principal components -----------------------------------------------

pc_plot <- function(.data, target, id_col = "id", bg_id = "M4", x_lims = NULL, y_lims = NULL, 
                    color_scheme = NULL, geom_type = c("point", "hexbin", "density")) {
  
  type <- match.arg(geom_type)
  
  if (target == bg_id) {
    bg.data <- .data
  } else {
    bg.data <- .data[.data[[id_col]] == bg_id, ]
  }
  
  if(!is.null(color_scheme)) {
    col <- color_scheme[target]
  } else {
    col <- NULL
  }
  
  if(type == "point") {
    p <- ggplot() +
      geom_point(aes(x=PC1, y=PC2), 
                 color = "grey92",
                 data = bg.data, 
                 na.rm = T) +
      geom_point(aes(x=PC1, y=PC2),
                 color = col, 
                 data = .data[.data[[id_col]] == eval(target), ],
                 na.rm = T) +
      theme_bw(base_size = 12) +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      xlim(x_lims) + ylim(y_lims) +
      facet_wrap(~eval(target))
  } else if(type == "hexbin") {
    p <- ggplot() +
      geom_hex(aes(x=PC1, y=PC2),
               fill = "grey92",
               bins = 20, 
               data = bg.data, 
               na.rm = T) +
      geom_hex(aes(x=PC1, y=PC2), 
               data = .data[.data[[id_col]] == eval(target), ], 
               bins = 20, 
               na.rm = T) +
      scale_fill_gradient(low = "grey50", 
                          high = "navyblue",
                          trans = "log10") +
      theme_bw(base_size = 12) +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      xlim(x_lims) + ylim(y_lims) +
      facet_wrap(~eval(target))
  } else if(type == "density") {
    p <- ggplot() +
      stat_density2d(aes(x=PC1, y=PC2),
                     geom = "polygon", 
                     data = bg.data, 
                     na.rm = T) +
      stat_density2d(aes(x=PC1, y=PC2, fill = ..level..),
                     geom = "polygon", 
                     data = .data[.data[[id_col]] == eval(target), ], 
                     na.rm = T) +
      theme_bw(base_size = 12) +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      xlim(x_lims) + ylim(y_lims) +
      facet_wrap(~eval(target))
  }
  
  p
}
