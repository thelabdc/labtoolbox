set.seed(955)  # for reproducibility

samp_size <- 50
  
sample_reg_data <- tibble::tibble(
  
  x = rnorm(samp_size, mean = 2, sd = 0.15),  
  sample_treatment = factor(sample(c(1, 0), samp_size, replace = TRUE))
) |>
  dplyr::mutate(
    sample_outcome_1 = ifelse(
      sample_treatment == 1,
      x + rnorm(samp_size, mean = 1.5, sd = 2.5) + rnorm(samp_size, mean = 1.5, sd = 5),
      x + rnorm(samp_size, mean = 1, sd = 2.5) + rnorm(samp_size, mean = 1.5, sd = 5)
    ),
    sample_outcome_2 = ifelse(
      sample_treatment == 1,
      x + rnorm(samp_size, mean = 2, sd = 3) + rnorm(samp_size, mean = 1.5, sd = 5),
      x + rnorm(samp_size, mean = 2.25, sd = 3.5) + rnorm(samp_size, mean = 1.5, sd = 5)
    ),
    sample_outcome_3 =
      x + rnorm(samp_size, mean = 2, sd = 3) + rnorm(samp_size, mean = 1.5, sd = 5)
  )

sample_model_1 <- lm(sample_outcome_1 ~ sample_treatment, sample_reg_data)
sample_model_2 <- lm(sample_outcome_2 ~ sample_treatment, sample_reg_data)
sample_model_3 <- lm(sample_outcome_3 ~ sample_treatment, sample_reg_data)
