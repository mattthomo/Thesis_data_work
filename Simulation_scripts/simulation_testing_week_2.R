# This code is from my second week of simulation testing
# I make adaptations to the simulation function in week 1 in line with the notes from previous meetings

# Adaptations to original code:
    # 1.

sim_function_moments2 <- function(par, n, target_coefs, weights){


    params <- c(
        beta_0_educ   = par[1],
        alpha_educ    = par[2],
        beta_0_income = par[3],
        gamma_income  = par[4],
        alpha_income  = par[5]
       # u_consc       = par[6],
       # u_educ        = par[7],
       # u_income      = par[8]
    )



    library(tidyverse)

    set.seed(123456)



    #### Variables #####

    # educ <- rnorm(n = 100, mean = 10, sd = 1)

    consc <- rnorm(n, mean = 0, sd = 1)



    ##### Parameters ####

    # Set in function arguments

    ##### Equations #####

    e_consc  <- rnorm(n, mean = 0, sd = 1)
    e_educ   <- rnorm(n, mean = 0, sd = 1)
    e_income <- rnorm(n, mean = 0, sd = 1)

    consc_measure <- consc + e_consc # Conscientiousness equation

    educ   <- as.numeric(params["beta_0_educ"]) + as.numeric(params["alpha_educ"]) * consc_measure + e_educ

    log_income <- as.numeric(params["beta_0_income"]) +
            as.numeric(params["gamma_income"]) * educ +
            as.numeric(params["alpha_income"]) * consc_measure +
            e_income


    sim_df <- data.frame(
        consc        = consc,
        consc_meas   = consc_measure,
        educ         = educ,
        log_income       = log_income
    )


    simulated_moments_df <- sim_df %>%
        summarise(
            mean_loginc = mean(log_income, na.rm = T),
            sd_loginc = sd(log_income, na.rm = T),
            mean_educ = mean(educ, na.rm = T),
            sd_educ = sd(educ, na.rm = T),
            mean_consc = mean(consc, na.rm = T),
            sd_consc = sd(consc, na.rm = T)
        )


    observed_moments_df <- readRDS('/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/observed_tab.rds')


    comparison_df <- observed_moments_df %>%
        bind_rows(simulated_moments_df) %>%
        mutate(source = c("observed_data", "simulated_data")) %>%
        pivot_longer(cols = -source,
                     names_to = "parameter",
                     values_to = "value") %>%
        pivot_wider(names_from = source, values_from = value) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
        mutate(diff = observed_data - simulated_data) %>%
        mutate(squared_diff = diff^2)

    sim_coefs <- comparison_df$simulated_data

    loss <- sum(weights*(sim_coefs - target_coefs)^2)
    return(loss)


}

target_coefs_moments <- observed_moments_df

init_par <- c(
    12,       # beta_0_educ
    1.5,      # alpha_educ
    20,       # beta_0_income
    2.0,      # gamma_income
    1.0      # alpha_income
    #log(1),   # u_consc SD (exp(0) = 1)
   # log(1),   # u_educ SD
    #log(1)    # u_income SD
)

# optimise on moments

result <- optim(
    par          = init_par,
    fn           = sim_function_moments2,
    target_coefs = target_coefs_moments,
    n            = 10000,
    method       = "Nelder-Mead",
    weights = weights
)

opt_params_moments <- result$par        # optimal parameter values

opt_params_tab_moments <- opt_params_moments %>%
    as.data.frame() %>%
    rename("Optimal value" = 1)

opt_params_tab_moments$Parameter <- c("beta_0_educ",
                                      "alpha_educ",
                                      "beta_0_income",
                                      "gamma_income",
                                      "alpha_income",
                                      "u_consc",
                                      "u_educ",
                                      "u_income"
)

opt_params_tab_moments$Parameter_meaning <- c(
    "Education reg intercept",
    "Effect of consc on educ",
    "Income reg intercept",
    "Effect of educ on income",
    "Effect of consc on income",
    "Std dev of consc error",
    "Std dev of educ error",
    "Std dev of income error"
)


# WEIGHTING

# run your observed lm models and extract standard errors
obs_lm1 <- lm(w4_best_edu ~ consc_flipped,
              data = test_ols)
obs_lm2 <- lm(l_total_inc ~ w4_best_edu + consc_flipped,
              data = test_ols)

se1 <- summary(obs_lm1)$coefficients[, "Std. Error"] %>%
    as.data.frame() %>%
    rownames_to_column("id") %>%
    mutate(id = recode(id,
                       "(Intercept)" = "beta_0_educ",
                       "consc_flipped" = "alpha_educ"))

se2 <- summary(obs_lm2)$coefficients[, "Std. Error"] %>%
    as.data.frame() %>%
    rownames_to_column("id") %>%
    mutate(id = recode(id,
                       "(Intercept)" = "beta_0_income",
                       "w4_best_edu" = "gamma_income",
                       "consc_flipped" = "alpha_income"))

se_combo <- se1 %>%
    bind_rows(se2) %>%
    rename("Std_error" = ".") %>%
    mutate(weights = 1/(Std_error^2)) %>%
    mutate(weights_scaled = weights/sum(weights))

weights <- se_combo$weights_scaled


optim()