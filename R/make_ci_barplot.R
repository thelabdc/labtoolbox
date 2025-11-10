#' make_ci_barplot
#'
#' This function takes regression output and makes a bar plot which compares a control group with a treatment group, adding confidence intervals to the treatment groups.
#'
#' @param model The regression whose results you want to plot
#' @param treat_arms A character vector for the treatment arms, as reported in the regression
#' @param ci_level = .95
#' @return A bar plot with a confidence interval on the treatment arms
#' @import tidyverse
#' @export
# Function to return linear combinations of coefficients, with confidence intervals
make_ci_barplot <- function(model = sample_model_1, treat_arms = c("sample_treatment1"),
                            ci_level = .95,
                            plot_caption = "Sample Plot Caption",
                            ...){
  # Make sample data in case it's necessary

  if(!exists("model")){
    print("Warning: This output uses sample data. Make sure to submit a model for the model parameter.")
    model <- sample_model_1
  }
  tidy_model <- broom::tidy(model)

  if(!exists("treat_arms")){
    print("Warning: Make sure to submit the treatment names for the treat_arms parameter.")
    treat_arms <- c("sample_treatment1")
  }

  # set key terms using the parameters
  intercept <- as.numeric(tidy_model[tidy_model$term == "(Intercept)", "estimate"])
  n_obs <- nobs(model)

  if(!exists("ci_level")){
    print("Reporting 95% confidence intervals.")
    ci_level = .95
  }

  if(!exists("plot_caption")){
    plot_caption <- paste("The Lab @ DC. Results from a randomized evaluation using ", n_obs, "observations.")
  }

  # make a table of the relevant linear combinations

  coef_table <-
    tibble::tibble(
    term = c("Control", treat_arms),
    estimate = as.numeric(unlist(tidy_model[tidy_model$term == "(Intercept)"|tidy_model$term %in% treat_arms,"estimate"])),
    se = as.numeric(unlist(tidy_model[tidy_model$term == "(Intercept)"|tidy_model$term %in% treat_arms,"std.error"]))) |>
    dplyr::mutate(
      linear_combination = dplyr::case_when(
        term == "Control" ~  estimate,
        term != "Control" ~ estimate + intercept)
      )|>
    dplyr::mutate(ci_upper = ifelse(term == "Control", NA, linear_combination + ( qnorm(1- (1-ci_level)/2)*se)),
           ci_lower = ifelse(term == "Control", NA, linear_combination - ( qnorm(1- (1-ci_level)/2)*se))
    )

  # make the plot
  coef_plot <- ggplot2::ggplot(coef_table) + ggplot2::geom_bar(ggplot2::aes(x = term, y = linear_combination, fill = term), stat = "identity")+
    ggplot2::geom_errorbar(data = coef_table, ggplot2::aes(x = term, ymin =ci_lower,ymax = ci_upper), width = .75) +
    theme_lab(...)+
    ggplot2::labs(caption = plot_caption,
         title = "Treatment Effect",
         subtitle = "95% Confidence Intervals",
         x = "",
         y = "Estimated outcome",
         fill = "")
  return(coef_plot)

}
