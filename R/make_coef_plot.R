#' Make coefficient plot from models
#' 
#' Plot treatment effects with confidence intervals against a dotted line at zero
#'
#' This function takes the output from several models and plots their estimated treatment effects with confidence intervals horizontally, across a dotted line at zero.
#' 
#' @param model_list A list of models
#' @param model_names A character vector with a name for each model
#' @param ci_level The confidence level for the intervals to be plotted, expressed as a probability
#' @param treat_vars A character vector of the main treatment variable in each regression. If only one is provided, will be assumed to apply to all regressions.
#' @param plot_title A title for the plot
#' @param plot_subtitle A subtitle for the plot
#' @param plot_caption A caption for the plot
#' @param return_ci_table A logical; if \code{TRUE}, returns the table of confidence intervals underlying the plot
#' @param ... Other arguments to be passed to \code{theme_lab()}

#' @return A ggplot object
#' @import dplyr ggplot2
#' @export

make_coef_plot <- function(model_list = list(sample_model_1, sample_model_2, sample_model_3),
                           model_names = c("Outcome 1", "Outcome 2","Outcome 3"),
                           ci_level = .95,
                           treat_vars = c("sample_treatment1", "sample_treatment1", "sample_treatment1"),
                           plot_title = "Treatment Effects",
                           plot_subtitle = "95% Confidence Intervals",
                           plot_caption = "The Lab @ DC. Regression results from a randomized evaluation.",
                           return_ci_table = FALSE,
                           ...) {

  n_models <- length(model_list)
  
  if(length(treat_vars) == 1){
    treat_vars <- rep(treat_vars, length(model_list))
  }

  # Create an empty storage data frame 
  plotting_df <- tibble(term = rep(NA, n_models),
                        estimate = rep(NA, n_models),
                        upper = rep(NA, n_models),
                        lower = rep(NA, n_models),
                        model = 1:n_models)

  # Iterate through each model in the input list
  for (i in 1:length(model_list)) {
    current_model <- model_list[[i]]

    # Perform operations on the current model
    # Extract coefficients
    plotting_df[i, "term"] <- names(coef(current_model)[treat_vars[i]])
    plotting_df[i, "estimate"] <- coef(current_model)[treat_vars[i]]
    
    # Calculate CI
    plotting_df[i, "upper"] <- coef(current_model)[treat_vars[i]] +
      (qt(1 - (1 - ci_level) / 2, df = current_model$df.residual) * 
         coef(summary(current_model))[, "Std. Error"][treat_vars[i]])
    plotting_df[i, "lower"] <- coef(current_model)[treat_vars[i]] -
      (qt(1 - (1 - ci_level) / 2, df = current_model$df.residual) * 
         coef(summary(current_model))[, "Std. Error"][treat_vars[i]])

    if(exists("model_names")){
      plotting_df$model <- model_names
    }else(
      plotting_df$model <- 1:length(model)
    )
    coef_plot <- ggplot(plotting_df) + 
      geom_point(aes(x = model, y = estimate,  color = factor(model)), stat = "identity") +
      geom_errorbar(data = plotting_df, 
                    aes(x = model, ymin = lower, ymax = upper, color = factor(model)), 
                    width = .75) +
      geom_hline(aes(yintercept = 0), linetype = "dotted") +
      coord_flip() +
      theme_lab(...) +
      theme(legend.position = "none") +
      labs(caption = plot_caption,
           title = plot_title,
           subtitle = plot_subtitle,
           x = "",
           y = "Treatment Effect")
    }

  # Return the collected results
  if(return_ci_table == TRUE){
    return(plotting_df)
  }else
    return(coef_plot)

}
