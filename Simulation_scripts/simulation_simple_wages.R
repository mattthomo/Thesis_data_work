# This script houses the simulation code for the full sample without controls, using the optimisation method found in the simulation_simple_black_males.R, and for the log of wages outcome


### Simulation function ####


sim_function_reg_wages <- function(par, n, target_coefs, weights){


    params <- c(
        beta_0_educ   = par[1],
        alpha_educ    = par[2],
        beta_0_wages = par[3],
        gamma_wages  = par[4],
        alpha_wages  = par[5],
        u_consc       = par[6],
        u_educ        = par[7],
        u_wages      = par[8]
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
    e_wages <- rnorm(n, mean = 0, sd = exp(params["u_wages"]))

    consc_measure <-
        consc +
        e_consc # Conscientiousness equation

    educ   <-
        as.numeric(params["beta_0_educ"]) +
        as.numeric(params["alpha_educ"]) * consc_measure +
        e_educ

    log_wages <-
        as.numeric(params["beta_0_wages"]) +
        as.numeric(params["gamma_wages"]) * educ +
        as.numeric(params["alpha_wages"]) * consc_measure +
        e_wages


    sim_df <- data.frame(
        consc        = consc,
        consc_meas   = consc_measure,
        educ         = educ,
        log_wages        = log_wages
    )

    moments_df <- data.frame(
        mean_log_wages = mean(sim_df$log_wages),
        sd_log_wages = sd(sim_df$log_wages),
        mean_educ = mean(sim_df$educ),
        sd_educ = sd(sim_df$educ),
        mean_consc = mean(sim_df$consc_meas),
        sd_consc = sd(sim_df$consc_meas)
    )

    lm1 <- lm(educ ~ consc_measure,
              data = sim_df)

    lm2 <- lm(log_wages ~ educ + consc_measure,
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


    observed_moments_df <- readRDS('/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/observed_tab_wages.rds')

    loss_df <- comparison_df %>%
        bind_rows(observed_moments_df) %>%
        mutate(source = c("simulated", "observed")) %>% # create this column to distinguish between what's simulated and what's observed
        pivot_longer(cols = -source,     # everything but the source column
                     names_to = "Parameter",
                     values_to = "Value")  %>%  # new column gets made with all the names and is called "Parameter"
        pivot_wider(names_from = source,
                    values_from = Value) # gets simulated and observed data next to each other

    loss_df

    sim_values <- loss_df$simulated

    target_values <- target_coefs

    loss <- sum(weights * ((sim_values - target_values)^2))

    return(loss)
}


### Weights vector ####


weights_vec <- c(
    # distributional moments - use inverse variance (1/sd^2)
    mean_log_wages  = 1 / sd(test_ols_wages$l_w4_wages, na.rm = T)^2,
    sd_log_wages    = 1 / (sd(test_ols_wages$l_w4_wages, na.rm = T)^2 / (2 * nrow(test_ols_wages))),
    mean_educ    = 1 / sd(test_ols_wages$w4_best_edu, na.rm = T)^2,
    sd_educ      = 1 / (sd(test_ols_wages$w4_best_edu, na.rm = T)^2 / (2 * nrow(test_ols_wages))),
    mean_consc   = 1 / sd(test_ols_wages$consc_flipped, na.rm = T)^2,
    sd_consc     = 1 / (sd(test_ols_wages$consc_flipped, na.rm = T)^2 / (2 * nrow(test_ols_wages))),

    # regression coefficients - use inverse SE^2 from observed models
    educ_reg_Intercept         = 1 / summary(educ_ols_simp)$coefficients["(Intercept)", "Std. Error"]^2,
    educ_reg_Conscientiousness = 1 / summary(educ_ols_simp)$coefficients["consc_flipped", "Std. Error"]^2,

    # r squared - assign low weight since it's a fit statistic not a moment
    educ_reg_r_squared = 0.1,

    # income regression coefficients
    inc_reg_intercept = 1 / summary(sim_ols_wages)$coefficients["(Intercept)", "Std. Error"]^2,
    inc_reg_educ      = 1 / summary(sim_ols_wages)$coefficients["w4_best_edu",  "Std. Error"]^2,
    inc_reg_consc     = 1 / summary(sim_ols_wages)$coefficients["consc_flipped","Std. Error"]^2,

    # r squared - low weight
    inc_reg_r_squared = 0.1
)

weights_vec <- weights_vec / sum(weights_vec)


### Define parameter space ####

par_set_simple <- makeParamSet(
    makeNumericParam("beta_0_educ",   lower = 0,  upper = 20),
    makeNumericParam("alpha_educ",    lower = -10,  upper = 10),
    makeNumericParam("beta_0_wages", lower = 0,  upper = 20),
    makeNumericParam("gamma_wages",  lower = -10,  upper = 10),
    makeNumericParam("alpha_wages",  lower = -10,  upper = 10),
    makeNumericParam("u_consc",       lower = -5,   upper = 5),
    makeNumericParam("u_educ",        lower = -5,   upper = 5),
    makeNumericParam("u_wages",      lower = -5,   upper = 5)
)

### Wrap objective function ####


obj_fun <- makeSingleObjectiveFunction(
    name = "loss",
    fn = function(x) {

        x <- as.numeric(x)


        sim_function_reg_wages(par = x,
                               n = 100000,
                               target_coefs = target_values_simple_wages,
                               weights = weights_vec)
    },
    par.set = par_set_simple,
    minimize = TRUE
)

### Configure Bayesian optimisation ####

mbo_ctrl <- makeMBOControl()
mbo_ctrl <- setMBOControlInfill(mbo_ctrl, crit = crit.ei)      # expected improvement
mbo_ctrl <- setMBOControlTermination(mbo_ctrl, iters = 1000, max.evals = 1000L) # iterations


### Initial values and random design generation ####

init_par <- c(
    12,       # beta_0_educ
    0.5,      # alpha_educ
    5,       # beta_0_wages
    0.3,      # gamma_wages
    0.2,      # alpha_wages
    log(1),   # u_consc SD (exp(0) = 1)
    log(1),   # u_educ SD
    log(1)    # u_wages SD
)

random_design <- generateRandomDesign(n = 100, par.set = par_set_simple)

design_mat <- rbind(init_par, random_design)

# create y values to add to design mat
y_vals <- apply(design_mat, 1, function(row) {
    sim_function_reg_wages(
        par          = as.numeric(row),
        n            = 100000,
        target_coefs = target_values_simple_wages,
        weights      = weights_vec
    )
})

# Add y column to design_mat
design_mat$y <- y_vals

### Run optimisation ####

# run in parallel
parallelStartSocket(cpus = parallel::detectCores() - 1)

parallelExport(
    "sim_function_reg_wages",
    "target_values_simple_wages",
    "weights_vec"
)

# also load required packages on each worker
parallelLibrary("tidyverse")
parallelLibrary("mlrMBO")

set.seed(427292, "L'Ecuyer")
result <- mbo(
    fun     = obj_fun,
    design  = design_mat,
    control = mbo_ctrl,
    show.info = TRUE
)

parallelStop()

# extract results
result$x        # optimal parameter values
result$y        # final loss value


opdf <- as.data.frame(result$opt.path) %>%
    arrange(y)

# full history
plot(opdf$dob, opdf$y)

# This result once again showed that while the optimisation code was run, the optimal values were still the initial parameters provided
# This prompts the use of the local optimiser to be input as the initial parameters in the global optimising function

optim_result <- optim(
    par     = init_par,  # rough starting guess
    fn      = function(par) {
        sim_function_reg_wages(
            par          = par,
            n            = 10000,
            target_coefs = target_values_simple_wages,
            weights      = weights_vec
        )
    },
    method  = "Nelder-Mead",
    control = list(maxit = 1000, reltol = 1e-8)
)

optim_init_par <- optim_result$par

random_design <- generateRandomDesign(n = 100, par.set = par_set_simple)

design_mat <- rbind(optim_init_par, random_design)

# create y values to add to design mat
y_vals <- apply(design_mat, 1, function(row) {
    sim_function_reg_wages(
        par          = as.numeric(row),
        n            = 100000,
        target_coefs = target_values_simple_wages,
        weights      = weights_vec
    )
})

# Add y column to design_mat
design_mat$y <- y_vals

### Run optimisation ####

# run in parallel
parallelStartSocket(cpus = parallel::detectCores() - 1)

parallelExport(
    "sim_function_reg_wages",
    "target_values_simple_wages",
    "weights_vec"
)

# also load required packages on each worker
parallelLibrary("tidyverse")
parallelLibrary("mlrMBO")

set.seed(427292, "L'Ecuyer")
result <- mbo(
    fun     = obj_fun,
    design  = design_mat,
    control = mbo_ctrl,
    show.info = TRUE
)

parallelStop()

# extract results
result$x        # optimal parameter values
result$y        # final loss value


opdf <- as.data.frame(result$opt.path) %>%
    arrange(y)

# full history
plot(opdf$dob, opdf$y)

mbo_results_simple_wages <- as.data.frame(result$x) %>%
    pivot_longer(cols = everything(),
                 names_to = "Parameter",
                 values_to = "Value")

# save optimisation results

write_rds(mbo_results_simple_wages, file = '/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/mbo_simple_wages.rds')


## get SE results from bootstrapping

mbo_results_simple_wages <- readRDS(file = '/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/mbo_simple_wages.rds') # in cases where session was closed and don't want to redo simulations

n_boots <- 500
boot_params <- matrix(NA, nrow = n_boots, ncol = 8)
colnames(boot_params) <- c(
    "beta_0_educ", "alpha_educ", "beta_0_wages",
    "gamma_wages", "alpha_wages",
    "u_consc", "u_educ", "u_wages"
)

# use optimal parameters from mbo as starting point
optimal_par <- as.numeric(unlist(mbo_results_simple_wages$Value))

for (i in 1:n_boots) {

    # resample true data with replacement
    boot_df <- test_ols_wages[sample(nrow(test_ols_wages), replace = TRUE), ]

    # recompute target moments from bootstrapped data
    boot_lm1 <- lm(w4_best_edu ~ consc_flipped, data = boot_df)
    boot_lm2 <- lm(l_w4_wages ~ w4_best_edu + consc_flipped,
                   data = boot_df)

    # recompute bootstrap target moments (means, SDs, coefficients)
    boot_target <- c(
        mean(boot_df$l_w4_wages,        na.rm = TRUE),
        sd(boot_df$l_w4_wages,          na.rm = TRUE),
        mean(boot_df$w4_best_edu,        na.rm = TRUE),
        sd(boot_df$w4_best_edu,          na.rm = TRUE),
        mean(boot_df$consc_flipped,      na.rm = TRUE),
        sd(boot_df$consc_flipped,        na.rm = TRUE),
        coef(boot_lm1),
        coef(boot_lm2)
    )

    # rerun optim from optimal mbo values
    boot_result <- optim(
        par          = optimal_par,
        fn           = sim_function_reg_wages,
        n            = 10000,           # smaller n for speed
        target_coefs = boot_target,
        weights      = weights_vec,
        method       = "Nelder-Mead",
        control      = list(maxit = 500)
    )

    boot_params[i, ] <- boot_result$par
    cat("Bootstrap iteration", i, "of", n_boots, "complete\n")
}

# bootstrap standard errors (on original estimated scale)
boot_se <- apply(boot_params, 2, sd)
boot_means <- colMeans(boot_params)

# apply delta method to transformed parameters

pacman::p_load(msm)

# covariance matrix from bootstrap
boot_cov <- cov(boot_params)

# delta method for each error SD
# g(x) = exp(x), so SE(exp(x)) = exp(x) * SE(x)
# x6 = u_consc, x7 = u_educ, x8 = u_income
se_u_consc  <- deltamethod(~ exp(x6), mean = boot_means, cov = boot_cov)
se_u_educ   <- deltamethod(~ exp(x7), mean = boot_means, cov = boot_cov)
se_u_wages <- deltamethod(~ exp(x8), mean = boot_means, cov = boot_cov)

results_final_simple <- data.frame(
    parameter = c(
        "beta_0_educ", "alpha_educ", "beta_0_wages",
        "gamma_wages", "alpha_wages",
        "u_consc", "u_educ", "u_wages"
    ),
    estimate = c(
        optimal_par[1:5],
        exp(optimal_par[6]),    # transform back from log scale
        exp(optimal_par[7]),
        exp(optimal_par[8])
    ),
    se = c(
        boot_se[1:5],           # direct bootstrap SEs
        se_u_consc,             # delta method SEs for transformed params
        se_u_educ,
        se_u_wages
    )
) %>%
    mutate(
        t_stat  = estimate / se,
        p_value = 2 * pnorm(-abs(t_stat)),
        stars   = case_when(
            p_value < 0.001 ~ "$^{***}$",
            p_value < 0.01  ~ "$^{**}$",
            p_value < 0.05  ~ "$^{*}$",
            TRUE            ~ ""
        ),
        estimate_stars = paste0(round(estimate, 3), stars),
        se = paste0("(",round(se, 3),")")
    ) %>%
    select(parameter, estimate_stars, se) %>%
    mutate(row = row_number()) %>%
    pivot_longer(
        cols = c(estimate_stars, se),
        names_to = "Type",
        values_to = "Value"
    ) %>%
    mutate(
        parameter = ifelse(Type == "se", "", parameter),
        Type = factor(Type, levels = c("Estimate", "se"))
    ) %>%
    fill(parameter, .direction = "down") %>%
    mutate(is_error_sd = parameter %in% c("u_consc", "u_educ", "u_wages")) %>%
    filter(!(row %in% c(6,7,8) & is_error_sd == FALSE)) %>%
    mutate(order = ifelse(is_error_sd, 1, 0)) %>%
    arrange(order, row) %>%
    select(-order, -is_error_sd, -row, -Type)


print(results_final_simple)

# save results
write_rds(results_final_simple,
          '/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/simple_results_with_se_wages.rds')

