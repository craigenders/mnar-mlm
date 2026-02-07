#------------------------------------------------------------------------------#
# FUNCTION TO PLOT AVERAGE IMPUTATIONS BY TIME OR TIME/GROUP ----
#------------------------------------------------------------------------------#

plot_means <- function(formula, model, ylab = NULL, title = NULL, 
                       group_labels = NULL, fs = 14) {
  
  # Parse formula: outcome ~ x_var | group_var (group_var optional)
  formula_parts <- as.character(formula)
  outcome <- formula_parts[2]
  rhs <- formula_parts[3]
  
  # Check if there's a grouping variable
  has_group <- grepl("\\|", rhs)
  
  if (has_group) {
    rhs_split <- strsplit(rhs, "\\|")[[1]]
    x_var <- trimws(rhs_split[1])
    group_var <- trimws(rhs_split[2])
    
    # Aggregate data
    agg_formula <- as.formula(paste(outcome, "~", x_var, "+", group_var))
    plot_data <- aggregate(agg_formula, data = model@average_imp, mean)
    
    # Set default labels
    if (is.null(ylab)) ylab <- outcome
    if (is.null(title)) title <- paste(outcome, "by", x_var, "and", group_var)
    
    # Create plot with grouping
    p <- ggplot(plot_data, aes(x = .data[[x_var]], 
                               y = .data[[outcome]], 
                               linetype = factor(.data[[group_var]]),
                               group = factor(.data[[group_var]]))) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_linetype_manual(values = c("0" = "solid", "1" = "dashed")) +
      labs(title = title, x = x_var, y = ylab, linetype = group_var)
    
    # Apply custom group labels if provided
    if (!is.null(group_labels)) {
      p <- p + scale_linetype_manual(values = c("0" = "solid", "1" = "dashed"),
                                     labels = group_labels)
    }
    
  } else {
    x_var <- trimws(rhs)
    
    # Aggregate data
    agg_formula <- as.formula(paste(outcome, "~", x_var))
    plot_data <- aggregate(agg_formula, data = model@average_imp, mean)
    
    # Set default labels
    if (is.null(ylab)) ylab <- outcome
    if (is.null(title)) title <- paste(outcome, "by", x_var)
    
    # Create plot without grouping
    p <- ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[outcome]])) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(title = title, x = x_var, y = ylab)
  }
  
  p <- p + 
    theme_classic(base_size = fs) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"
    )
  
  return(p)
}