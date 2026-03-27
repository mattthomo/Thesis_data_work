# Main aim of this script is to have a more clean script showing my simulation results

sim_function_reg <- function(par, n, target_coefs, weights){


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

    consc_measure <-
        consc +
        e_consc # Conscientiousness equation

    educ   <-
        as.numeric(params["beta_0_educ"]) +
        as.numeric(params["alpha_educ"]) * consc_measure +
        e_educ

    income <-
        as.numeric(params["beta_0_income"]) +
        as.numeric(params["gamma_income"]) * educ +
        as.numeric(params["alpha_income"]) * consc_measure +
        e_income


    sim_df <- data.frame(
        consc        = consc,
        consc_meas   = consc_measure,
        educ         = educ,
        income       = income
    )

    moments_df <- data.frame(
        mean_loginc = mean(sim_df$income),
        sd_loginc = sd(sim_df$income),
        mean_educ = mean(sim_df$educ),
        sd_educ = sd(sim_df$educ),
        mean_consc = mean(sim_df$consc_meas),
        sd_consc = sd(sim_df$consc_meas)
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

    sim_results <-
        cbind(reg_results1, reg_results2)

    comparison_df <- moments_df %>%
    bind_cols(sim_results)


    observed_moments_df <- readRDS('/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/observed_tab.rds')

    loss_df <- comparison_df %>%
        bind_rows(observed_moments_df) %>%
        mutate(source = c("simulated", "observed")) %>% # create this column to distinguish between what's simulated and what's observed
        pivot_longer(cols = -source,     # everything but the source column
                     names_to = "Parameter",
                     values_to = "Value")  %>%  # new column gets made with all the names and is called "Parameter"
        pivot_wider(names_from = source,
                    values_from = Value) # gets simmulated and observed data next to each other

    loss_df

    sim_values <- loss_df$simulated

    target_values <- target_coefs

    loss <- sum(weights * ((sim_values - target_values)^2))

    return(loss)
    # comparison_df <- observed_moments_df %>%
    #     bind_rows(sim_results) %>%
    #     mutate(source = c("observed_data", "simulated_data")) %>%
    #     pivot_longer(cols = -source,
    #                  names_to = "parameter",
    #                  values_to = "value") %>%
    #     pivot_wider(names_from = source, values_from = value) %>%
    #     mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    #     mutate(diff = observed_data - simulated_data) %>%
    #     mutate(squared_diff = diff^2) %>%
    #     filter(parameter %in% c("inc_reg_intercept",
    #                             "inc_reg_educ",
    #                             "inc_reg_consc"))
    #
    # sim_coefs <- comparison_df$simulated_data
    #
    # sim_coefs
    # loss <- sum(weights*(sim_coefs - target_coefs)^2)
    # return(loss)


}
sim_function_reg(par = init_par, n = 100, target_coefs = target_values_simple)


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
    fn           = sim_function_reg,
    target_coefs = target_values_simple,
    n            = 100,
    method       = "Nelder-Mead",
    weights      = weights_vec
)


weights_vec <- c(
    # distributional moments - use inverse variance (1/sd^2)
    mean_loginc  = 1 / sd(test_ols$l_total_inc, na.rm = T)^2,
    sd_loginc    = 1 / (sd(test_ols$l_total_inc, na.rm = T)^2 / (2 * nrow(test_ols))),
    mean_educ    = 1 / sd(test_ols$w4_best_edu, na.rm = T)^2,
    sd_educ      = 1 / (sd(test_ols$w4_best_edu, na.rm = T)^2 / (2 * nrow(test_ols))),
    mean_consc   = 1 / sd(test_ols$consc_flipped, na.rm = T)^2,
    sd_consc     = 1 / (sd(test_ols$consc_flipped, na.rm = T)^2 / (2 * nrow(test_ols))),

    # regression coefficients - use inverse SE^2 from observed models
    educ_reg_Intercept         = 1 / summary(educ_ols_simp)$coefficients["(Intercept)", "Std. Error"]^2,
    educ_reg_Conscientiousness = 1 / summary(educ_ols_simp)$coefficients["consc_flipped", "Std. Error"]^2,

    # r squared - assign low weight since it's a fit statistic not a moment
    educ_reg_r_squared = 0.1,

    # income regression coefficients
    inc_reg_intercept = 1 / summary(sim_ols)$coefficients["(Intercept)", "Std. Error"]^2,
    inc_reg_educ      = 1 / summary(sim_ols)$coefficients["w4_best_edu",  "Std. Error"]^2,
    inc_reg_consc     = 1 / summary(sim_ols)$coefficients["consc_flipped","Std. Error"]^2,

    # r squared - low weight
    inc_reg_r_squared = 0.1
)

weights_vec <- weights_vec / sum(weights_vec)


##### Using mlrMBO package #######

par_set_simple <- makeParamSet(
    makeNumericParam("beta_0_educ",   lower = 0,  upper = 20),
    makeNumericParam("alpha_educ",    lower = -10,  upper = 10),
    makeNumericParam("beta_0_income", lower = 0,  upper = 20),
    makeNumericParam("gamma_income",  lower = -10,  upper = 10),
    makeNumericParam("alpha_income",  lower = -10,  upper = 10),
    makeNumericParam("u_consc",       lower = -5,   upper = 5),
    makeNumericParam("u_educ",        lower = -5,   upper = 5),
    makeNumericParam("u_income",      lower = -5,   upper = 5)
)

# 2. wrap your loss function for mlrMBO
obj_fun <- makeSingleObjectiveFunction(
    name = "loss",
    fn = function(x) {

        x <- as.numeric(x)


        sim_function_reg(par = x, n = 100000,
                              target_coefs = target_values_simple,
                              weights = weights_vec)
    },
    par.set = par_set_simple,
    minimize = TRUE
)

# 3. configure the optimisation
ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 500)  # number of iterations

# 4. run the optimisation
result_simple <- mbo(obj_fun, control = ctrl)

# 5. extract results
result_simple$x        # optimal parameter values
result_simple$y        # final loss value


mbo_results_simple <- as.data.frame(result$x) %>%
    pivot_longer(cols = everything(),
                 names_to = "Parameter",
                 values_to = "Value")

write_rds(mbo_results_simple, file = '/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/mbo_simple.rds')
