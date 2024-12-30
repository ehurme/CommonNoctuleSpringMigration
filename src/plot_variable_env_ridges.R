## ggridges plot of environmental variables across days
# plot_variable_env_ridges(data, env_name, hours = seq(-24*4, 24*4, by = 24))
# https://r-graph-gallery.com/42-colors-names.html

# scales::seq_gradient_pal("#e5f5f9", "#2ca25f")

plot_variable_env_ridges <- function(data, env_name, env_component, hours,
                                     adjust_divide = 1, adjust_add = 0, main_color = "cornflowerblue") {
  require(ggplot2)
  require(ggridges)
  require(reshape2)
  require(RColorBrewer)
  require(scales) # for rescale function

  # Check if required columns are present
  required_cols <- paste0(env_name, "_", hours)
  missing_cols <- setdiff(required_cols, names(data))
  if(length(missing_cols) > 0){
    stop("Missing columns: ", paste(missing_cols, collapse = ", "),
         ". Please check the data and try again.")
  }

  # Filter data based on selected relevant columns for melting
  data_subset <- data[, required_cols, drop = FALSE]

  # Melt the data into long format for plotting
  long_data <- reshape2::melt(data_subset, variable.name = "time", value.name = "value")
  long_data <- long_data[is.finite(long_data$value),]

  # Convert hours to 'days from zero' and map to factor with correct levels
  days_from_zero <- as.numeric(gsub(paste0(env_name, "_"), "", names(data_subset)))/24
  names_from_zero <- days_from_zero #paste0("Day ", days_from_zero)
  levels_with_days <- setNames(names_from_zero, required_cols)
  long_data$time <- factor(long_data$time, levels = required_cols, labels = levels_with_days)

  # Create a color palette with white for the zero hour
  color_values <- scales::rescale(days_from_zero, c(-1, 1))
  color_palette <- c("white", main_color, "white")
  colors <- colorRampPalette(color_palette)(length(unique(days_from_zero)))
  colors_vector <- setNames(colors, levels_with_days)

  # Create the ridgeline plot
  p <- ggplot(long_data, aes(x = value/adjust_divide+adjust_add, y = time, fill = time)) +
    geom_density_ridges(scale = 1.5, size = 0.3, #rel_min_height = 0.01,
                                 quantile_lines = TRUE, quantiles = 2, alpha = 1,
                                 vline_size = 0.5,
                                 vline_color = "black") +
    # stat_density_ridges()+
    #stat_summary(fun.y = median, geom = "line", aes(group = time, color = time), size = 0.5) +
    scale_fill_manual(values = colors_vector) +
    scale_color_manual(values = colors_vector) + # Use the same color for the median line
    theme_ridges(font_size = 11, font_family = "Helvetica") +
    theme(legend.position = "none") + # remove legend
    ylab("Days to migration") +
    xlab(env_component) +
    ggtitle("") + # remove title
    theme(plot.title = element_blank()) # Ensure the title space is also removed if needed

  return(p)
}
