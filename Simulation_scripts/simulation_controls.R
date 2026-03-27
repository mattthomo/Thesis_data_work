
### Create simulation function #####

sim_function_controls <- function(par, n, target_coefs, weights){


    params <- c(
        beta_0_educ            = par[1],
        alpha_educ             = par[2],
        beta_0_income          = par[3],
        gamma_income           = par[4],
        alpha_income           = par[5],
        u_consc                = par[6],
        u_educ                 = par[7],
        u_income               = par[8],
        delta_income_white     = par[9],
        delta_income_coloured  = par[10],
        delta_income_asian     = par[11],
        delta_income_female    = par[12],
        delta_income_age       = par[13],
        delta_income_age_squared = par[14],
        delta_income_educ_squared = par[15],
        delta_income_wealth    = par[16]
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

    consc_measure <- consc + e_consc # Conscientiousness PCA is noisy measure of conscientiousness


    educ   <- as.numeric(params["beta_0_educ"]) + as.numeric(params["alpha_educ"]) * consc_measure + e_educ # educ varies by level of conscientiousness


    income <-
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
        e_income # wage equation


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

# create df of simulated moments
    simulated_moments_df <- sim_df %>%
        summarise(
            mean_loginc = mean(income, na.rm = T),
            sd_loginc = sd(income, na.rm = T),
            mean_educ = mean(educ, na.rm = T),
            sd_educ = sd(educ, na.rm = T),
            mean_consc = mean(consc, na.rm = T),
            sd_consc = sd(consc, na.rm = T),
            mean_age = mean(age, na.rm = T),
            sd_age = sd(age, na.rm = T),
            mean_wealth = mean(wealth_sim, na.rm = T),
            sd_wealth = sd(wealth_sim, na.rm = T)
        )

    # run linear models on simulated data to get coefficients

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


    observed_moments_df <- readRDS('/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/observed_moments_controls.rds') # call in the df with the target values of the observed moments and coefficients


    loss_df <- sim_results %>%
        bind_rows(observed_moments_df) %>%
        mutate(source = c("simulated", "observed")) %>% # create this column to distinguish between what's simulated and what's observed
        pivot_longer(cols = -source,     # everything but the source column
                     names_to = "Parameter",
                     values_to = "Value")  %>%  # new column gets made with all the names and is called "Parameter"
        pivot_wider(names_from = source,
                    values_from = Value)

    loss_df

    sim_values <- loss_df$simulated

    target_values <- target_coefs

    loss <- sum(weights * ((sim_values - target_values)^2))

    return(loss)

    # sim_coefs <- comparison_df$simulated_data
    #
    # loss <- sum((sim_coefs - target_coefs)^2)
    # return(loss)


}


init_par <- c(
    # education equation
    10,     # beta_0_educ: intercept (mean years of education)
    0.5,    # alpha_educ: positive effect of conscientiousness on education

    # income equation
    5,      # beta_0_income: intercept
    0.3,    # gamma_income: positive return to education
    0.2,    # alpha_income: positive effect of conscientiousness on income

    # error SDs (on log scale because you use exp() inside sim)
    log(1), # u_consc: SD of conscientiousness error
    log(1), # u_educ: SD of education error
    log(1), # u_income: SD of income error

    # race effects on income (relative to black = reference)
    0.3,    # delta_income_white: white wage premium
    0.1,    # delta_income_coloured: coloured wage premium
    0.2,    # delta_income_asian: asian wage premium

    # other controls
    0.1,    # delta_income_female: gender wage gap
    0.05,   # delta_income_age: age effect
    -0.001, # delta_income_age_squared: diminishing age returns
    -0.01,  # delta_income_educ_squared: diminishing education returns
    0.2     # delta_income_wealth: wealth effect
)

weights_vec_controls <- c(
    # distributional moments - use inverse variance (1/sd^2)
    mean_loginc  = 1 / sd(test_ols$l_total_inc, na.rm = T)^2,
    sd_loginc    = 1 / (sd(test_ols$l_total_inc, na.rm = T)^2 / (2 * nrow(test_ols))),
    mean_educ    = 1 / sd(test_ols$w4_best_edu, na.rm = T)^2,
    sd_educ      = 1 / (sd(test_ols$w4_best_edu, na.rm = T)^2 / (2 * nrow(test_ols))),
    mean_consc   = 1 / sd(test_ols$consc_flipped, na.rm = T)^2,
    sd_consc     = 1 / (sd(test_ols$consc_flipped, na.rm = T)^2 / (2 * nrow(test_ols))),
    mean_age     = 1 / sd(test_ols$w1_age, na.rm = T)^2,
    sd_age       = 1 / (sd(test_ols$w1_age, na.rm = T)^2 / (2 * nrow(test_ols))),
    mean_wealth  = 1 / sd(test_ols$wealth_index_famd, na.rm = T)^2,
    sd_wealth    = 1 / (sd(test_ols$wealth_index_famd, na.rm = T)^2 / (2 * nrow(test_ols))),

    # regression coefficients - use inverse SE^2 from observed models
    educ_reg_Intercept         = 1 / summary(educ_ols_simp)$coefficients["(Intercept)", "Std. Error"]^2,
    educ_reg_Conscientiousness = 1 / summary(educ_ols_simp)$coefficients["consc_flipped", "Std. Error"]^2,

    # r squared - assign low weight since it's a fit statistic not a moment
    educ_reg_r_squared = 0.025,

    # income regression coefficients
    inc_reg_intercept = 1 / summary(sim_ols_controls)$coefficients["(Intercept)", "Std. Error"]^2,
    inc_reg_educ      = 1 / summary(sim_ols_controls)$coefficients["w4_best_edu",  "Std. Error"]^2,
    inc_reg_consc     = 1 / summary(sim_ols_controls)$coefficients["consc_flipped","Std. Error"]^2,
    inc_reg_educ_squared     = 1 / summary(sim_ols_controls)$coefficients["w4_educ_squared","Std. Error"]^2,
    inc_reg_age     = 1 / summary(sim_ols_controls)$coefficients["w1_age","Std. Error"]^2,
    inc_reg_age_sq     = 1 / summary(sim_ols_controls)$coefficients["age_sq","Std. Error"]^2,
    inc_reg_female     = 1 / summary(sim_ols_controls)$coefficients["w4_best_genFemale","Std. Error"]^2,
    inc_reg_coloured     = 1 / summary(sim_ols_controls)$coefficients["w4_best_raceColoured","Std. Error"]^2,
    inc_reg_asian     = 1 / summary(sim_ols_controls)$coefficients["w4_best_raceAsian/Indian","Std. Error"]^2,
    inc_reg_white    = 1 / summary(sim_ols_controls)$coefficients["w4_best_raceWhite","Std. Error"]^2,
    inc_reg_wealth     = 1 / summary(sim_ols_controls)$coefficients["wealth_index_famd","Std. Error"]^2,

    # r squared - low weight
    inc_reg_r_squared = 0.025
)

weights_vec_controls <- weights_vec_controls / sum(weights_vec_controls)



result <- optim(
    par          = init_par,
    fn           = sim_function_controls,
    target_coefs = target_values_controls,
    n            = 10000,
    method       = "Nelder-Mead",
    weights      = weights_vec_controls
)

result$par


### Using mlrMBO package ######

par_set_controls <- makeParamSet(
    makeNumericParam("beta_0_educ",   lower = 0,  upper = 20),
    makeNumericParam("alpha_educ",    lower = 0,  upper = 10),
    makeNumericParam("beta_0_income", lower = -10,  upper = 20),
    makeNumericParam("gamma_income",  lower = 0,  upper = 10),
    makeNumericParam("alpha_income",  lower = 0,  upper = 10),
    makeNumericParam("u_consc",       lower = -5,   upper = 5),
    makeNumericParam("u_educ",        lower = -5,   upper = 5),
    makeNumericParam("u_income",      lower = -5,   upper = 5),
    makeNumericParam("delta_income_white",   lower = 0, upper = 10),
    makeNumericParam("delta_income_coloured",   lower = -5, upper = 10),
    makeNumericParam("delta_income_asian",   lower = -5, upper = 10),
    makeNumericParam("delta_income_female",   lower = -5, upper = 0),
    makeNumericParam("delta_income_age",   lower = -5, upper = 10),
    makeNumericParam("delta_income_age_squared",   lower = -5, upper = 0),
    makeNumericParam("delta_income_educ_squared",   lower = -5, upper = 0),
    makeNumericParam("delta_income_wealth",   lower = 0, upper = 10)
)

obj_fun <- makeSingleObjectiveFunction(
    name = "loss",
    fn = function(x) {

        x <- as.numeric(x)


        sim_function_controls(par = x, n = 50000,
                              target_coefs = target_values_controls,
                              weights = weights_vec_controls)
    },
    par.set = par_set_controls,
    minimize = TRUE
)

# 3. Configure optimisation

ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 1000)  # number of iterations

# 4. run the optimisation

result <- mbo(obj_fun, control = ctrl)

# 5. extract results

result$x        # optimal parameter values
result$y        # final loss value

mbo_results_control <- as.data.frame(result$x) %>%
    pivot_longer(cols = everything(),
                 names_to = "Parameter",
                 values_to = "Value")

write_rds(mbo_results_control, file = '/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/mbo_control.rds')
