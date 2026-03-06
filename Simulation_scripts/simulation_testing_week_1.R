# This script houses the code from my first week of simulations for my thesis

# SIMPLE TESTING
# Here I want to test simulations on the simplest data set possible



#### Setup ####
## Minimising difference between lm results ##

sim_function_reg <- function(par, n, target_coefs){


    params <- c(
        beta_0_educ   = par[1],
        alpha_educ    = par[2],
        beta_0_income = par[3],
        gamma_income  = par[4],
        alpha_income  = par[5],
        u_consc       = par[6],
        u_educ        = par[7],
        u_income      = par[8]
    )



library(tidyverse)

set.seed(123456)



#### Variables #####

# educ <- rnorm(n = 100, mean = 10, sd = 1)

consc <- rnorm(n, mean = 0, sd = 1)



##### Parameters ####

# Set in function arguments

##### Equations #####

e_consc  <- rnorm(n, mean = 0, sd = exp(params["u_consc"]))
e_educ   <- rnorm(n, mean = 0, sd = exp(params["u_educ"]))
e_income <- rnorm(n, mean = 0, sd = exp(params["u_income"]))

consc_measure <- consc + e_consc # Conscientiousness equation

educ   <- as.numeric(params["beta_0_educ"]) + as.numeric(params["alpha_educ"]) * consc_measure + e_educ

income <- log(
    as.numeric(params["beta_0_income"]) +
        as.numeric(params["gamma_income"]) * educ +
        as.numeric(params["alpha_income"]) * consc_measure +
        e_income
)

sim_df <- data.frame(
    consc        = consc,
    consc_meas   = consc_measure,
    educ         = educ,
    income       = income
)


simulated_moments_df <- sim_df %>%
    summarise(
        mean_loginc = mean(income, na.rm = T),
        sd_loginc = sd(income, na.rm = T),
        mean_educ = mean(educ, na.rm = T),
        sd_educ = sd(educ, na.rm = T),
        mean_consc = mean(consc, na.rm = T),
        sd_consc = sd(consc, na.rm = T)
    )

lm1 <- lm(educ ~ consc_measure,
          data = sim_df)

lm2 <- lm(income ~ educ + consc_measure,
          data = sim_df)

lm1_summary <- summary(lm1)
lm2_summary <- summary(lm2)

reg_results1 <- as.data.frame(lm1$coefficients) %>%
    pivot_wider(names_from = "lm1$coefficients", values_from = "lm1$coefficients") %>%
    rename("educ_reg_Intercept" = 1,
           "educ_reg_Conscientiousness" = 2) %>%
    mutate(educ_reg_r_squared = lm1_summary$r.squared)

reg_results2 <- as.data.frame(lm2$coefficients) %>%
    pivot_wider(names_from = "lm2$coefficients",
                values_from = "lm2$coefficients") %>%
    rename("inc_reg_intercept" = 1,
           "inc_reg_educ" = 2,
           "inc_reg_consc" = 3) %>%
    mutate(inc_reg_r_squared = lm2_summary$r.squared)

sim_results <- simulated_moments_df %>%
    cbind(reg_results1, reg_results2)

observed_moments_df <- readRDS('/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/observed_tab.rds')


comparison_df <- observed_moments_df %>%
    bind_rows(sim_results) %>%
    mutate(source = c("observed_data", "simulated_data")) %>%
    pivot_longer(cols = -source,
                 names_to = "parameter",
                 values_to = "value") %>%
    pivot_wider(names_from = source, values_from = value) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    mutate(diff = observed_data - simulated_data) %>%
    mutate(squared_diff = diff^2) %>%
    filter(parameter %in% c("inc_reg_intercept",
                            "inc_reg_educ",
                            "inc_reg_consc"))

sim_coefs <- comparison_df$simulated_data

loss <- sum((sim_coefs - target_coefs)^2)
return(loss)


}

## Minimising difference between simulated and observed moments ##

sim_function_moments <- function(par, n, target_coefs){


    params <- c(
        beta_0_educ   = par[1],
        alpha_educ    = par[2],
        beta_0_income = par[3],
        gamma_income  = par[4],
        alpha_income  = par[5],
        u_consc       = par[6],
        u_educ        = par[7],
        u_income      = par[8]
    )



    library(tidyverse)

    set.seed(123456)



    #### Variables #####

    # educ <- rnorm(n = 100, mean = 10, sd = 1)

    consc <- rnorm(n, mean = 0, sd = 1)



    ##### Parameters ####

    # Set in function arguments

    ##### Equations #####

    e_consc  <- rnorm(n, mean = 0, sd = exp(params["u_consc"]))
    e_educ   <- rnorm(n, mean = 0, sd = exp(params["u_educ"]))
    e_income <- rnorm(n, mean = 0, sd = exp(params["u_income"]))

    consc_measure <- consc + e_consc # Conscientiousness equation

    educ   <- as.numeric(params["beta_0_educ"]) + as.numeric(params["alpha_educ"]) * consc_measure + e_educ

    income <- log(
        as.numeric(params["beta_0_income"]) +
            as.numeric(params["gamma_income"]) * educ +
            as.numeric(params["alpha_income"]) * consc_measure +
            e_income
    )

    sim_df <- data.frame(
        consc        = consc,
        consc_meas   = consc_measure,
        educ         = educ,
        income       = income
    )


    simulated_moments_df <- sim_df %>%
        summarise(
            mean_loginc = mean(income, na.rm = T),
            sd_loginc = sd(income, na.rm = T),
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

    loss <- sum((sim_coefs - target_coefs)^2)
    return(loss)


}


# first attempt

sim_df1 <- sim_function(n = 1000,
                        params = c(
                            beta_0_educ = 12,
                            alpha_educ = 1.5,
                            beta_0_income = 10,
                            gamma_income = 2.0,
                            alpha_income = 1.0,
                            u_consc = 0.5,
                            u_educ = 1.0,
                            u_income = 1.5))


# second attempt

sim_df2 <- sim_function(n = 10000,
             params = c(beta_0_educ = 12,
                        alpha_educ = 1.5,
                        beta_0_income = 10,
                        gamma_income = 2.0,
                        alpha_income = 1.0,
                        u_consc = 0.5,
                        u_educ = 1.0,
                        u_income = 1.5))


sim_df3 <- sim_function(n = 100000,
                        params = c(beta_0_educ = 12,
                                   alpha_educ = 1.5,
                                   beta_0_income = 10,
                                   gamma_income = 2.0,
                                   alpha_income = 1.0,
                                   u_consc = 0.5,
                                   u_educ = 1.0,
                                   u_income = 1.5))



sim_df4 <- sim_function(n = 100000,
                        params = c(beta_0_educ = 12,
                                   alpha_educ = 1.5,
                                   beta_0_income = 10,
                                   gamma_income = 2.0,
                                   alpha_income = 1.0,
                                   u_consc = 0.75,
                                   u_educ = 1.0,
                                   u_income = 1.5))


sim_df5 <- sim_function(n = 100000,
                        params = c(beta_0_educ = 12,
                                   alpha_educ = 1.5,
                                   beta_0_income = 17,
                                   gamma_income = 2.0,
                                   alpha_income = 1.0,
                                   u_consc = 0.75,
                                   u_educ = 1.0,
                                   u_income = 3))

sim_df6 <- sim_function(n = 100000,
                        params = c(beta_0_educ = 12.339,
                                   alpha_educ = 1.5,
                                   beta_0_income = 6.369,
                                   gamma_income = 0.085,
                                   alpha_income = 0.126,
                                   u_consc = 0.75,
                                   u_educ = 1.0,
                                   u_income = 0.063))



##### NUMERICAL OPTIMISATION FUNCTION #####

# Here, I want to create a function which minimises the squared difference between coefficients from the simulated and observed dfs

# Your target coefficients from real data

target_coefs_moments <- observed_moments_df  # e.g. intercept, educ, consc_meas

target_coefs_reg <- coef(ols_results)

# loss_function <- function(par, n = 100, target_coefs) {
#
#     params <- c(
#         beta_0_educ   = par[1],
#         alpha_educ    = par[2],
#         beta_0_income = par[3],
#         gamma_income  = par[4],
#         alpha_income  = par[5],
#         u_consc       = exp(par[6]),  # exp() ensures always positive
#         u_educ        = exp(par[7]),
#         u_income      = exp(par[8])
#     )
#
#     sim_df <- sim_function(n = n, params = params)
#     sim_model <- lm(income ~ educ + consc_meas, data = sim_df)
#     sim_coefs <- coef(sim_model)
#
#     loss <- sum((sim_coefs - target_coefs)^2)
#     return(loss)
# }



init_par <- c(
    12,       # beta_0_educ
    1.5,      # alpha_educ
    20,       # beta_0_income
    2.0,      # gamma_income
    1.0,      # alpha_income
    log(1),   # u_consc SD (exp(0) = 1)
    log(1),   # u_educ SD
    log(1)    # u_income SD
)

## Optimise on moments

result <- optim(
    par          = init_par,
    fn           = sim_function_moments,
    target_coefs = target_coefs_moments,
    n            = 100,
    method       = "Nelder-Mead"
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

opt_params_tab_moments

write_rds(opt_params_tab, file = "./output/opt_params_tab.rds")


result$value      # final loss (how close you got)


## Optimise on reg results ##


result <- optim(
    par          = init_par,
    fn           = sim_function_reg,
    target_coefs = target_coefs_reg,
    n            = 100,
    method       = "Nelder-Mead"
)

opt_params <- result$par        # optimal parameter values

opt_params_tab <- opt_params %>%
    as.data.frame() %>%
    rename("Optimal value" = 1)

opt_params_tab$Parameter <- c("beta_0_educ",
                              "alpha_educ",
                              "beta_0_income",
                              "gamma_income",
                              "alpha_income",
                              "u_consc",
                              "u_educ",
                              "u_income"
)

opt_params_tab$Parameter_meaning <- c(
    "Education reg intercept",
    "Effect of consc on educ",
    "Income reg intercept",
    "Effect of educ on income",
    "Effect of consc on income",
    "Std dev of consc error",
    "Std dev of educ error",
    "Std dev of income error"
)

opt_params_tab



## Simulate with controls ##


sim_function_controls <- function(par, n, target_coefs){


    params <- c(
        beta_0_educ   = par[1],
        alpha_educ    = par[2],
        beta_0_income = par[3],
        gamma_income  = par[4],
        alpha_income  = par[5],
        u_consc       = par[6],
        u_educ        = par[7],
        u_income      = par[8],
        delta_income_white = par[9],
        delta_income_coloured = par[10],
        delta_income_asian = par[11],
        mu_income = par[12]
    )



    library(tidyverse)

    set.seed(123456)






    #### Variables #####

    # Add race

    # simulate a factor variable, e.g. race with 4 levels
    race <- factor(sample(c("black", "coloured", "asian", "white"), n, replace = TRUE,
                   prob = c(0.8, 0.13, 0.02, 0.05)),
                   levels = c("black", "coloured", "asian", "white"))  # set probs to match your true data

    race_coloured <- as.numeric(race == "coloured")
    race_asian <- as.numeric(race == "asian")
    race_white <- as.numeric(race == "white")



    # Add gender

    gender <- factor(sample(c("male", "female"), n, replace = T,
                     prob = c(0.47, 0.53)),
                     levels = c("male", "female"))

    gender_female <- as.numeric(gender == "female")



    # educ <- rnorm(n = 100, mean = 10, sd = 1)

    consc <- rnorm(n, mean = 0, sd = 1)

    age <- runif(n, min = 14, max = 23)

    wealth_sim <- rnorm(n, mean = 0, sd = 1.8)


    ##### Parameters ####

    # Set in function arguments

    ##### Equations #####

    e_consc  <- rnorm(n, mean = 0, sd = exp(params["u_consc"]))
    e_educ   <- rnorm(n, mean = 0, sd = exp(params["u_educ"]))
    e_income <- rnorm(n, mean = 0, sd = exp(params["u_income"]))

    consc_measure <- consc + e_consc # Conscientiousness equation

    educ   <- as.numeric(params["beta_0_educ"]) + as.numeric(params["alpha_educ"]) * consc_measure + e_educ

    income <- log(
        as.numeric(params["beta_0_income"]) +
            as.numeric(params["gamma_income"]) * educ +
            as.numeric(params["alpha_income"]) * consc_measure +
            as.numeric(params["delta_income_educ_squared"]) * educ^2 +
            as.numeric(params["delta_income_age"]) * age +
            as.numeric(params["delta_income_age_squared"]) * age^2 +
            as.numeric(params["delta_income_coloured"]) * race_coloured +
            as.numeric(params["delta_income_asian"]) * race_asian +
            as.numeric(params["delta_income_white"]) * race_white +
            as.numeric(params["delta_income_female"]) * gender_female +
            as.numeric(params["delta_income_wealth"]) * wealth_sim +
            e_income
    )

    sim_df <- data.frame(
        consc        = consc,
        consc_meas   = consc_measure,
        educ         = educ,
        income       = income,
        age          = age,
        race_coloured = race_coloured,
        race_asian = race_asian,
        race_white = race_white,
        gender_female = gender_female,
        age_squared = age^2,
        educ_squared = educ^2,
        wealth_sim = wealth_sim
    )


    simulated_moments_df <- sim_df %>%
        summarise(
            mean_loginc = mean(income, na.rm = T),
            sd_loginc = sd(income, na.rm = T),
            mean_educ = mean(educ, na.rm = T),
            sd_educ = sd(educ, na.rm = T),
            mean_consc = mean(consc, na.rm = T),
            sd_consc = sd(consc, na.rm = T)
        )

    lm1 <- lm(educ ~ consc_measure,
              data = sim_df)

    lm2 <- lm(income ~ educ + consc_measure + educ_squared + age + age_squared + gender_female + race_coloured + race_asian + race_white + wealth_sim,
              data = sim_df)

    lm1_summary <- summary(lm1)
    lm2_summary <- summary(lm2)

    reg_results1 <- as.data.frame(lm1$coefficients) %>%
        pivot_wider(names_from = "lm1$coefficients", values_from = "lm1$coefficients") %>%
        rename("educ_reg_Intercept" = 1,
               "educ_reg_Conscientiousness" = 2) %>%
        mutate(educ_reg_r_squared = lm1_summary$r.squared)

    reg_results2 <- as.data.frame(lm2$coefficients) %>%
        pivot_wider(names_from = "lm2$coefficients",
                    values_from = "lm2$coefficients") %>%
        rename("inc_reg_intercept" = 1,
               "inc_reg_educ" = 2,
               "inc_reg_consc" = 3,
               "inc_reg_educ_squared" = 4,
               "inc_reg_age" = 5,
               "inc_reg_age_sq" = 6,
               "inc_reg_female" = 7,
               "inc_reg_coloured" = 8,
               "inc_reg_asian" = 9,
               "inc_reg_white" = 10,
               "inc_reg_wealth" = 11) %>%
        mutate(inc_reg_r_squared = lm2_summary$r.squared)

    sim_results <- simulated_moments_df %>%
        cbind(reg_results1, reg_results2)

    observed_moments_df <- readRDS('/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/observed_tab2.rds')


    comparison_df <- observed_moments_df %>%
        bind_rows(sim_results) %>%
        mutate(source = c("observed_data", "simulated_data")) %>%
        pivot_longer(cols = -source,
                     names_to = "parameter",
                     values_to = "value") %>%
        pivot_wider(names_from = source, values_from = value) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
        mutate(diff = observed_data - simulated_data) %>%
        mutate(squared_diff = diff^2) %>%
        filter(parameter %in% c("inc_reg_intercept",
                                "inc_reg_educ",
                                "inc_reg_consc",
                                "inc_reg_educ_squared",
                                "inc_reg_age",
                                "inc_reg_age_sq",
                                "inc_reg_female",
                                "inc_reg_coloured",
                                "inc_reg_asian",
                                "inc_reg_white",
                                "inc_reg_wealth"))

    sim_coefs <- comparison_df$simulated_data

    loss <- sum((sim_coefs - target_coefs)^2)
    return(loss)


}




init_par <- c(
    12,       # beta_0_educ
    1.5,      # alpha_educ
    20,       # beta_0_income
    2.0,      # gamma_income
    1.0,      # alpha_income
    log(1),   # u_consc SD (exp(0) = 1)
    log(1),   # u_educ SD
    log(1),   # u_income SD
    1,
    0.5,
    1.1,
    1
)


target_coefs2 <- coef(sim_ols_controls)

result <- optim(
    par          = init_par,
    fn           = sim_function_controls,
    target_coefs = target_coefs2,
    n            = 100,
    method       = "Nelder-Mead"
)

result$par        # optimal parameter values
result$value












#### LAVAAN PACKAGE #######

library(lavaan)


true_df <- test_ols %>%
    mutate(
        race_coloured = as.integer(w4_best_race == "Coloured"),
        race_asian    = as.integer(w4_best_race == "Asian/Indian"),
        race_white    = as.integer(w4_best_race == "White"), # black = reference
        gender_female = as.integer(w4_best_gen  == "Female")   # male = reference
    )

sem_model <- '
  # Measurement model: latent conscientiousness
  consc_lat =~ 1 * consc_flipped

  # Fix residual variance of indicator to 0 (perfect measurement), can play around with this to indicate measurement error
  # or set to a known value if you have a reliability estimate
  consc_flipped ~~ 0.8 * consc_flipped

  # Structural model
  w4_best_edu   ~ consc_lat
  l_total_inc ~ w4_best_edu + consc_lat + w1_age + race_coloured + race_asian + race_white + gender_female + wealth_index_famd
'


fit <- sem(
    model       = sem_model,
    data        = true_df,
    meanstructure = TRUE,            # estimates intercepts (your beta_0 terms)
    estimator   = "ML"
)

summary(fit, fit.measures = TRUE, standardized = TRUE, estimates = TRUE)

# ── 3. Extract key parameter estimates ────────────────────────────────────────
parameterEstimates(fit) %>%
    filter(op %in% c("~", "~1", "=~")) %>%
    select(lhs, op, rhs, est, se, pvalue) %>%
    print()










df %>% ggplot2::ggplot(aes(x = income)) +
    geom_density()


