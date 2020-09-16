
# Plot principal components -----------------------------------------------

pc_plot <- function(.data, target, id_col = "id", bg_id = "M4", x_lims = NULL, y_lims = NULL, include_legend = TRUE,
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
      geom_hex(aes(x=PC1, y=PC2, fill = ..ndensity..),
               fill = "grey92",
               bins = 20, 
               data = bg.data, 
               na.rm = T) +
      geom_hex(aes(x=PC1, y=PC2, fill = ..ndensity..), 
               bins = 20,
               data = .data[.data[[id_col]] == eval(target), ], 
               na.rm = T) +
      scale_fill_gradient(low = "grey50",
                          high = "navyblue",
                          breaks = seq(0, 1, 0.2)) +
      theme_bw(base_size = 12) +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      xlim(x_lims) + ylim(y_lims) +
      facet_wrap(~eval(target)) + 
      labs(fill = "Normalized\nDensity")
    
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
            plot.title = element_text(hjust = 0.5)) +
      xlim(x_lims) + ylim(y_lims) +
      facet_wrap(~eval(target))
  }
  
  if (!isTRUE(include_legend)) {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}


make_patchwork <- function(x, layout, x_lab, y_lab, loadings, guide = NULL, type = c("point", "hex", "density")) {
  type <- match.arg(type)
  
  if (type == "hex") {
    return(
      wrap_plots(a=x$M3,                    
                 b=x$M4,                       
                 c=x$`Corporacion Favorita`,
                 d=x$`Recruit Restaurant`,  
                 e=loadings,                 
                 f=x$Rossmann,
                 g=x$`Walmart Store Sales`,               
                 h=x$`Walmart Stormy Weather`, 
                 i=x$Wikipedia,
                 x = x_lab, 
                 y = y_lab,
                 z = guide,
                 ncol = 5, nrow = 4,
                 widths = c(0.1, 1, 1, 1, 0.3),
                 heights = c(1, 1, 1, 0.1), 
                 design = layout) +
        plot_annotation(theme = theme(legend.position = "none"))
    )
  } else {
    return(
      wrap_plots(a=x$M3,                    
                 b=x$M4,                       
                 c=x$`Corporacion Favorita`,
                 d=x$`Recruit Restaurant`,  
                 e=loadings,                 
                 f=x$Rossmann,
                 g=x$`Walmart Store Sales`,               
                 h=x$`Walmart Stormy Weather`, 
                 i=x$Wikipedia,
                 x = x_lab, 
                 y = y_lab,
                 ncol = 5, nrow = 4,
                 widths = c(0.1, 1, 1, 1),
                 heights = c(1, 1, 1, 0.1), 
                 design = layout) +
        plot_annotation(theme = theme(legend.position = "none"))
    )
  }
}