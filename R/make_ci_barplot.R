#' Make Barplot with Confidence Intervals around Treatment Effects
#'
#' Take regression output and make a bar plot which compares a control group with a treatment group, adding confidence intervals to the treatment groups.
#'
#' @param model The regression whose results you want to plot
#' @param treat_arms A character vector for the treatment arms, as reported in the regression
#' @param ci_level The confidence level for the bar plot's intervals, expressed as a probability
#' @param plot_caption A character string with a caption for the plot
#' @param ... Other arguments passed to \code{theme_lab()}
#' 
#' @return A bar plot with a confidence interval on the treatment arms
#' @import dplyr ggplot2 tibble
#' @export

make_ci_barplot <- function(model = sample_model_1, 
                            treat_arms = c("sample_treatment1"),
                            ci_level = .95,
                            plot_caption = "Sample Plot Caption",
                            ...){
  
  # Use sample data if needed
  if(!exists("model")){
    print("Warning: This output uses sample data. Make sure to submit a model for the model parameter.")
    model <- sample_model_1
  }
  tidy_model <- broom::tidy(model)
  
  if(!exists("treat_arms")){
    print("Warning: Make sure to submit the treatment names for the treat_arms parameter.")
    treat_arms <- "sample_treatment1"
  }
  
  # set key terms using the parameters
  intercept <- as.numeric(tidy_model[tidy_model$term == "(Intercept)", "estimate"])
  n_obs <- nobs(model)

  if(!exists("plot_caption")){
    plot_caption <- paste("The Lab @ DC. Results from a randomized evaluation using ", n_obs, "observations.")
  }
  
  # Make table of relevant linear combinations
  
  coef_table <- tibble(
    term = c("Control", treat_arms),
    estimate = as.numeric(unlist(tidy_model[tidy_model$term == "(Intercept)" | tidy_model$term %in% treat_arms, "estimate"])),
    se = as.numeric(unlist(tidy_model[tidy_model$term == "(Intercept)" | tidy_model$term %in% treat_arms, "std.error"]))) |>
    mutate(
      linear_combination = case_when(
        term == "Control" ~ estimate,
        term != "Control" ~ estimate + intercept)
    ) |>
    mutate(
      ci_upper = ifelse(term == "Control", 
                        NA, 
                        linear_combination + (qt(1 - (1 - ci_level) / 2, df = model$df.residual) * se)),
      ci_lower = ifelse(term == "Control", 
                        NA, 
                        linear_combination - (qt(1 - (1 - ci_level) / 2, df = model$df.residual) * se))
    )
  
  # Plot
  
  coef_plot <- ggplot(coef_table) + 
    geom_bar(aes(x = term, y = linear_combination, fill = term), stat = "identity") +
    geom_errorbar(data = coef_table, 
                  aes(x = term, ymin = ci_lower, ymax = ci_upper), 
                  width = .75) +
    theme_lab(...) +
    labs(caption = plot_caption,
         title = "Treatment Effect",
         subtitle = "95% Confidence Intervals",
         x = "",
         y = "Estimated Outcome",
         fill = "")
  
  return(coef_plot)
  
}
