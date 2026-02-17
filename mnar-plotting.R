#------------------------------------------------------------------------------#
# FUNCTION TO PLOT AVERAGE IMPUTATIONS BY TIME OR TIME/GROUP ----
#------------------------------------------------------------------------------#
#
# Default behavior:
#   - Aggregates the requested outcome in model@average_imp and plots means by x
#     (and by group if supplied).
#
# Optional latent-growth behavior (use_latent_growth = TRUE):
#   - Constructs strictly linear predictions from person-level posterior means:
#       yhat_it = alpha.latent_i + beta.latent_i * time_t
#     where alpha.latent_i and beta.latent_i are first collapsed to ONE value per
#     person (and group) by averaging across any repeated rows.
#   - Then aggregates yhat_it by time (and group) and plots those means.
#
# This is useful when plotting "model-implied" growth curves that must be linear
# in time within group, even when row-level y.predicted or imputed values show
# slight curvature due to averaging/selection across imputations.
#
plot_means <- function(formula, model, ylab = NULL, title = NULL,
                       group_labels = NULL, fs = 14,
                       use_latent_growth = FALSE,
                       id_var = "id",
                       alpha_var = "alpha.latent",
                       beta_var  = "beta.latent") {
  
  # Parse formula: outcome ~ x_var | group_var (group_var optional)
  formula_parts <- as.character(formula)
  outcome <- formula_parts[2]
  rhs <- formula_parts[3]
  
  has_group <- grepl("\\|", rhs)
  
  if (has_group) {
    rhs_split <- strsplit(rhs, "\\|")[[1]]
    x_var <- trimws(rhs_split[1])
    group_var <- trimws(rhs_split[2])
  } else {
    x_var <- trimws(rhs)
    group_var <- NULL
  }
  
  dat <- model@average_imp
  
  # ---- Optional: build strictly linear yhat from person-level alpha/beta latents
  if (use_latent_growth) {
    
    need <- c(id_var, x_var, alpha_var, beta_var)
    if (!is.null(group_var)) need <- c(need, group_var)
    
    miss <- setdiff(need, names(dat))
    if (length(miss) > 0) {
      stop("plot_means(use_latent_growth=TRUE): missing columns: ",
           paste(miss, collapse = ", "))
    }
    
    # Collapse alpha/beta to ONE value per person (and group), in case they repeat over time rows
    if (is.null(group_var)) {
      re <- aggregate(dat[, c(alpha_var, beta_var)],
                      by = list(id = dat[[id_var]]),
                      FUN = mean)
      names(re)[1] <- id_var
    } else {
      re <- aggregate(dat[, c(alpha_var, beta_var)],
                      by = list(id = dat[[id_var]], group = dat[[group_var]]),
                      FUN = mean)
      names(re)[1:2] <- c(id_var, group_var)
    }
    
    # Build person-by-time grid
    times <- unique(dat[[x_var]])
    times_num <- suppressWarnings(as.numeric(as.character(times)))
    if (all(is.finite(times_num))) {
      times <- sort(unique(times_num))
    } else {
      # Fall back to lexical ordering if not numeric-like
      times <- sort(unique(as.character(times)))
    }
    
    time_df <- data.frame(tmp_time = times, stringsAsFactors = FALSE)
    names(time_df) <- x_var
    
    pred <- merge(re, time_df, by = NULL)
    
    # Ensure numeric time for multiplication when possible
    x_num <- suppressWarnings(as.numeric(as.character(pred[[x_var]])))
    if (all(is.finite(x_num))) pred[[x_var]] <- x_num
    
    pred[[".plot_outcome"]] <- pred[[alpha_var]] + pred[[beta_var]] * pred[[x_var]]
    
    dat <- pred
    outcome <- ".plot_outcome"
    
    if (is.null(ylab)) ylab <- "E[Y] (alpha.latent + beta.latent*time)"
    if (is.null(title)) title <- "Model-implied means from person-level alpha/beta latents"
  } else {
    if (is.null(ylab)) ylab <- outcome
    if (is.null(title)) {
      if (has_group) title <- paste(outcome, "by", x_var, "and", group_var)
      else title <- paste(outcome, "by", x_var)
    }
  }
  
  # ---- Aggregate + plot
  if (has_group) {
    
    agg_formula <- as.formula(paste(outcome, "~", x_var, "+", group_var))
    plot_data <- aggregate(agg_formula, data = dat, mean)
    
    p <- ggplot(plot_data, aes(x = .data[[x_var]],
                               y = .data[[outcome]],
                               linetype = factor(.data[[group_var]]),
                               group = factor(.data[[group_var]]))) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_linetype_manual(values = c("0" = "solid", "1" = "dashed")) +
      labs(title = title, x = x_var, y = ylab, linetype = group_var)
    
    if (!is.null(group_labels)) {
      p <- p + scale_linetype_manual(values = c("0" = "solid", "1" = "dashed"),
                                     labels = group_labels)
    }
    
  } else {
    
    agg_formula <- as.formula(paste(outcome, "~", x_var))
    plot_data <- aggregate(agg_formula, data = dat, mean)
    
    p <- ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[outcome]])) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(title = title, x = x_var, y = ylab)
  }
  
  p + theme_classic(base_size = fs) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "right")
}
