#' make_ci_barplot
#'
#' This function takes regression output and makes a bar plot which compares a control group with a treatment group, adding confidence intervals to the treatment groups.
#'
#' @param model The regression whose results you want to plot
#' @param treat_arms A character vector for the treatment arms, as reported in the regression
#' @param ci_level = .95
#' @return A bar plot with a confidence interval on the treatment arms
#' @export
# Function to return linear combinations of coefficients, with confidence intervals
make_ci_barplot <- function(model = sample_model, treat_arms = c("sample_treatment1"),
                            ci_level = .95,
                            ...){
  # Make sample data in case it's necessary

  set.seed(955)  # for reproducibility

  sample_data <- data.frame(
    x = rnorm(50, mean = 2, sd = 0.15),  # smaller sample size (50 instead of 100)
    sample_treatment = factor(sample(c(1, 0), 50, replace = TRUE))
  ) |>
    mutate(
      sample_outcome = ifelse(
        sample_treatment == 1,
        x + rnorm(50, mean = 1.5, sd = 2.5) + rnorm(50, mean = 1.5, sd = 5),  # increased noise
        x + rnorm(50, mean = 1, sd = 2.5) + rnorm(50, mean = 1.5, sd = 5)
      )
    )

  sample_model <- lm(sample_outcome ~ sample_treatment, sample_data)
  if(!exists("model")){
    print("Warning: This output uses sample data. Make sure to submit a model for the model parameter.")
    model <- sample_model
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
    plot_caption <- paste("Results from a randomized evaluation using ", n_obs, "observations.")
  }

  # make a table of the relevant linear combinations

  coef_table <-
    tibble(
    term = c("Control", treat_arms),
    estimate = as.numeric(unlist(tidy_model[tidy_model$term == "(Intercept)"|tidy_model$term %in% treat_arms,"estimate"])),
    se = as.numeric(unlist(tidy_model[tidy_model$term == "(Intercept)"|tidy_model$term %in% treat_arms,"std.error"]))) |>
    mutate(
      linear_combination = case_when(
        term == "Control" ~  estimate,
        term != "Control" ~ estimate + intercept)
      )|>
    mutate(ci_upper = ifelse(term == "Control", NA, linear_combination + ( qnorm(1- (1-ci_level)/2)*se)),
           ci_lower = ifelse(term == "Control", NA, linear_combination - ( qnorm(1- (1-ci_level)/2)*se))
    )

  # make the plot
  coef_plot <- ggplot(coef_table) + geom_bar(aes(x = term, y = linear_combination, fill = term), stat = "identity")+
    geom_errorbar(data = coef_table, aes(x = term, ymin =ci_lower,ymax = ci_upper), width = .75) +
    theme_lab(...)+
    labs(caption = plot_caption,
         title = "Treatment Effect",
         subtitle = "95% Confidence Intervals",
         x = "",
         y = "Estimated outcome",
         fill = "")
  return(coef_plot)

}


