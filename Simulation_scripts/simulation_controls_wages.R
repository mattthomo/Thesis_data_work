# This script houses the simulation code for the full sample with controls, using the optimisation method found in the simulation_simple_black_males.R, for the outcome with log wages



### Simulation Function ###

sim_function_controls_wages <- function(par, n, target_coefs, weights){


    params <- c(
        beta_0_educ            = par[1],
        alpha_educ             = par[2],
        beta_0_wages          = par[3],
        gamma_wages           = par[4],
        alpha_wages           = par[5],
        u_consc                = par[6],
        u_educ                 = par[7],
        u_wages               = par[8],
        delta_wages_white     = par[9],
        delta_wages_coloured  = par[10],
        delta_wages_asian     = par[11],
        delta_wages_female    = par[12],
        delta_wages_age       = par[13],
        delta_wages_age_squared = par[14],
        delta_wages_educ_squared = par[15],
        delta_wages_wealth    = par[16]
    )



    library(tidyverse)

    set.seed(123456)






    #### Variables #####

    # Add race

    # simulate a factor variable, e.g. race with 4 levels
    race <- factor(sample(c("black", "coloured", "asian", "white"), n, replace = TRUE,
                          prob = c(0.8, 0.13, 0.02, 0.05)),
                   levels = c("black", "coloured", "asian", "white"))  # set probabilities to match true data

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
    e_wages <- rnorm(n, mean = 0, sd = exp(params["u_wages"]))

    consc_measure <- consc + e_consc # Conscientiousness PCA is noisy measure of conscientiousness


    educ   <- as.numeric(params["beta_0_educ"]) + as.numeric(params["alpha_educ"]) * consc_measure + e_educ # educ varies by level of conscientiousness


    log_wages <-
        as.numeric(params["beta_0_wages"]) +
        as.numeric(params["gamma_wages"]) * educ +
        as.numeric(params["alpha_wages"]) * consc_measure +
        as.numeric(params["delta_wages_educ_squared"]) * educ^2 +
        as.numeric(params["delta_wages_age"]) * age +
        as.numeric(params["delta_wages_age_squared"]) * age^2 +
        as.numeric(params["delta_wages_coloured"]) * race_coloured +
        as.numeric(params["delta_wages_asian"]) * race_asian +
        as.numeric(params["delta_wages_white"]) * race_white +
        as.numeric(params["delta_wages_female"]) * gender_female +
        as.numeric(params["delta_wages_wealth"]) * wealth_sim +
        e_wages # wage equation


    sim_df <- data.frame(
        consc        = consc,
        consc_meas   = consc_measure,
        educ         = educ,
        log_wages       = log_wages,
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
            mean_log_wages = mean(log_wages, na.rm = T),
            sd_log_wages = sd(log_wages, na.rm = T),
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

    lm2 <- lm(log_wages ~ educ + consc_measure + educ_squared + age + age_squared + gender_female + race_coloured + race_asian + race_white + wealth_sim,
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


    observed_moments_df <- readRDS('/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/observed_moments_controls_wages.rds') # call in the df with the target values of the observed moments and coefficients


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

}


### Weights vector ###


weights_vec_controls <- c(
    # distributional moments - use inverse variance (1/sd^2)
    mean_log_wages  = 1 / sd(test_ols_wages$l_w4_wages, na.rm = T)^2,
    sd_log_wages    = 1 / (sd(test_ols_wages$l_w4_wages, na.rm = T)^2 / (2 * nrow(test_ols_wages))),
    mean_educ    = 1 / sd(test_ols_wages$w4_best_edu, na.rm = T)^2,
    sd_educ      = 1 / (sd(test_ols_wages$w4_best_edu, na.rm = T)^2 / (2 * nrow(test_ols_wages))),
    mean_consc   = 1 / sd(test_ols_wages$consc_flipped, na.rm = T)^2,
    sd_consc     = 1 / (sd(test_ols_wages$consc_flipped, na.rm = T)^2 / (2 * nrow(test_ols_wages))),
    mean_age     = 1 / sd(test_ols_wages$w1_age, na.rm = T)^2,
    sd_age       = 1 / (sd(test_ols_wages$w1_age, na.rm = T)^2 / (2 * nrow(test_ols_wages))),
    mean_wealth  = 1 / sd(test_ols_wages$wealth_index_famd, na.rm = T)^2,
    sd_wealth    = 1 / (sd(test_ols_wages$wealth_index_famd, na.rm = T)^2 / (2 * nrow(test_ols_wages))),

    # regression coefficients - use inverse SE^2 from observed models
    educ_reg_Intercept         = 1 / summary(educ_ols_simp)$coefficients["(Intercept)", "Std. Error"]^2,
    educ_reg_Conscientiousness = 1 / summary(educ_ols_simp)$coefficients["consc_flipped", "Std. Error"]^2,

    # r squared - assign low weight since it's a fit statistic not a moment
    educ_reg_r_squared = 0.025,

    # wages regression coefficients
    inc_reg_intercept = 1 / summary(sim_ols_controls_wages)$coefficients["(Intercept)", "Std. Error"]^2,
    inc_reg_educ      = 1 / summary(sim_ols_controls_wages)$coefficients["w4_best_edu",  "Std. Error"]^2,
    inc_reg_consc     = 1 / summary(sim_ols_controls_wages)$coefficients["consc_flipped","Std. Error"]^2,
    inc_reg_educ_squared     = 1 / summary(sim_ols_controls_wages)$coefficients["w4_educ_squared","Std. Error"]^2,
    inc_reg_age     = 1 / summary(sim_ols_controls_wages)$coefficients["w1_age","Std. Error"]^2,
    inc_reg_age_sq     = 1 / summary(sim_ols_controls_wages)$coefficients["age_sq","Std. Error"]^2,
    inc_reg_female     = 1 / summary(sim_ols_controls_wages)$coefficients["w4_best_genFemale","Std. Error"]^2,
    inc_reg_coloured     = 1 / summary(sim_ols_controls_wages)$coefficients["w4_best_raceColoured","Std. Error"]^2,
    inc_reg_asian     = 1 / summary(sim_ols_controls_wages)$coefficients["w4_best_raceAsian/Indian","Std. Error"]^2,
    inc_reg_white    = 1 / summary(sim_ols_controls_wages)$coefficients["w4_best_raceWhite","Std. Error"]^2,
    inc_reg_wealth     = 1 / summary(sim_ols_controls_wages)$coefficients["wealth_index_famd","Std. Error"]^2,

    # r squared - low weight
    inc_reg_r_squared = 0.025
)

weights_vec_controls <- weights_vec_controls / sum(weights_vec_controls)

### Define parameter space ###

par_set_controls <- makeParamSet(
    makeNumericParam("beta_0_educ",   lower = 0,  upper = 20),
    makeNumericParam("alpha_educ",    lower = 0,  upper = 10),
    makeNumericParam("beta_0_wages", lower = -10,  upper = 20),
    makeNumericParam("gamma_wages",  lower = 0,  upper = 10),
    makeNumericParam("alpha_wages",  lower = 0,  upper = 10),
    makeNumericParam("u_consc",       lower = -5,   upper = 5),
    makeNumericParam("u_educ",        lower = -5,   upper = 5),
    makeNumericParam("u_wages",      lower = -5,   upper = 5),
    makeNumericParam("delta_wages_white",   lower = 0, upper = 10),
    makeNumericParam("delta_wages_coloured",   lower = -5, upper = 10),
    makeNumericParam("delta_wages_asian",   lower = -5, upper = 10),
    makeNumericParam("delta_wages_female",   lower = -5, upper = 0),
    makeNumericParam("delta_wages_age",   lower = -5, upper = 10),
    makeNumericParam("delta_wages_age_squared",   lower = -5, upper = 0),
    makeNumericParam("delta_wages_educ_squared",   lower = -5, upper = 0),
    makeNumericParam("delta_wages_wealth",   lower = 0, upper = 10)
)


### Wrap objective function ###


obj_fun <- makeSingleObjectiveFunction(
    name = "loss",
    fn = function(x) {

        x <- as.numeric(x)


        sim_function_controls_wages(par = x,
                              n = 50000,
                              target_coefs = target_values_controls_wages,
                              weights = weights_vec_controls)
    },
    par.set = par_set_controls,
    minimize = TRUE
)

### Configure Bayesian optimisation ###

mbo_ctrl <- makeMBOControl()
mbo_ctrl <- setMBOControlInfill(mbo_ctrl, crit = crit.ei)      # expected improvement
mbo_ctrl <- setMBOControlTermination(mbo_ctrl, iters = 1000, max.evals = 1000L) # iterations


### Initial values and random design generation ###


init_par <- c(
    # education equation
    10,     # beta_0_educ: intercept (mean years of education)
    0.5,    # alpha_educ: positive effect of conscientiousness on education

    # wages equation
    5,      # beta_0_wages: intercept
    0.3,    # gamma_wages: positive return to education
    0.2,    # alpha_wages: positive effect of conscientiousness on wages

    # error SDs (on log scale because you use exp() inside sim)
    log(1), # u_consc: SD of conscientiousness error
    log(1), # u_educ: SD of education error
    log(1), # u_wages: SD of wages error

    # race effects on wages (relative to black = reference)
    0.3,    # delta_wages_white: white wage premium
    0.1,    # delta_wages_coloured: coloured wage premium
    0.2,    # delta_wages_asian: asian wage premium

    # other controls
    -0.1,    # delta_wages_female: gender wage gap
    0.05,   # delta_wages_age: age effect
    -0.001, # delta_wages_age_squared: diminishing age returns
    -0.01,  # delta_wages_educ_squared: diminishing education returns
    0.2     # delta_wages_wealth: wealth effect
)

random_design <- generateRandomDesign(n = 100, par.set = par_set_controls)

design_mat <- rbind(init_par, random_design)

# create y values to add to design mat
y_vals <- apply(design_mat, 1, function(row) {
    sim_function_controls_wages(
        par          = as.numeric(row),
        n            = 100000,
        target_coefs = target_values_controls_wages,
        weights      = weights_vec_controls
    )
})

# Add y column to design_mat
design_mat$y <- y_vals

### Run optimisation ###

# run in parallel
parallelStartSocket(cpus = parallel::detectCores() - 1)

parallelExport(
    "sim_function_controls_wages",
    "target_values_controls_wages",
    "weights_vec_controls"
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


### similar result to what was obtained previously
### try now with optim function

optim_result <- optim(
    par     = init_par,  # rough starting guess
    fn      = function(par) {
        sim_function_controls_wages(
            par          = par,
            n            = 10000,
            target_coefs = target_values_controls_wages,
            weights      = weights_vec_controls
        )
    },
    method  = "Nelder-Mead",
    control = list(maxit = 1000, reltol = 1e-8)
)


optim_result$value
optim_result$par


### try simulation again with opt_result values as new initial parameter values

init_par <- optim_result$par

random_design <- generateRandomDesign(n = 100, par.set = par_set_controls)

design_mat <- rbind(init_par, random_design)

# Add y column to design_mat
design_mat$y <- y_vals

### Run optimisation ###

# run in parallel
parallelStartSocket(cpus = parallel::detectCores() - 1)

parallelExport(
    "sim_function_controls_wages",
    "target_values_controls_wages",
    "weights_vec_controls"
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


opdf <- as.data.frame(result$opt.path)  # full history
plot(opdf$dob, opdf$y)


mbo_results_control <- as.data.frame(result$x) %>%
    pivot_longer(cols = everything(),
                 names_to = "Parameter",
                 values_to = "Value")

write_rds(mbo_results_control, file = '/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/mbo_control_wages.rds')

mbo_results_control <- readRDS(file = '/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/mbo_control_wages.rds')

## Get standard errors through bootstrapping ##

### Bootstrap to get standard errors ###

n_boots <- 500
boot_params <- matrix(NA, nrow = n_boots, ncol = 16)
colnames(boot_params) <- c(
    "beta_0_educ", "alpha_educ", "beta_0_wages",
    "gamma_wages", "alpha_wages",
    "u_consc", "u_educ", "u_wages",
    "delta_wages_white", "delta_wages_coloured", "delta_wages_asian",
    "delta_wages_female", "delta_wages_age", "delta_wages_age_squared",
    "delta_wages_educ_squared", "delta_wages_wealth"
)

# use optimal parameters from mbo as starting point
optimal_par <- as.numeric(unlist(mbo_results_control$Value))

for (i in 1:n_boots) {

    # resample true data with replacement
    boot_df <- test_ols_wages[sample(nrow(test_ols_wages), replace = TRUE), ]

    # recompute target moments from bootstrapped data
    boot_lm1 <- lm(w4_best_edu ~ consc_flipped, data = boot_df)
    boot_lm2 <- lm(l_w4_wages ~ w4_best_edu + consc_flipped + w4_educ_squared +
                       w1_age + age_sq + gender_female + race_coloured +
                       race_asian + race_white + wealth_index_famd,
                   data = boot_df)

    # recompute bootstrap target moments (means, SDs, coefficients)
    boot_target <- c(
        mean(boot_df$l_w4_wages,        na.rm = TRUE),
        sd(boot_df$l_w4_wages,          na.rm = TRUE),
        mean(boot_df$w4_best_edu,        na.rm = TRUE),
        sd(boot_df$w4_best_edu,          na.rm = TRUE),
        mean(boot_df$consc_flipped,      na.rm = TRUE),
        sd(boot_df$consc_flipped,        na.rm = TRUE),
        mean(boot_df$w1_age,             na.rm = TRUE),
        sd(boot_df$w1_age,               na.rm = TRUE),
        mean(boot_df$wealth_index_famd,  na.rm = TRUE),
        sd(boot_df$wealth_index_famd,    na.rm = TRUE),
        coef(boot_lm1),
        coef(boot_lm2)
    )

    # rerun optim from optimal mbo values
    boot_result <- optim(
        par          = optimal_par,
        fn           = sim_function_controls_wages,
        n            = 10000,           # smaller n for speed
        target_coefs = boot_target,
        weights      = weights_vec_controls,
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
# x6 = u_consc, x7 = u_educ, x8 = u_wages
se_u_consc  <- deltamethod(~ exp(x6), mean = boot_means, cov = boot_cov)
se_u_educ   <- deltamethod(~ exp(x7), mean = boot_means, cov = boot_cov)
se_u_wages <- deltamethod(~ exp(x8), mean = boot_means, cov = boot_cov)

results_final_controls <- data.frame(
    parameter = c(
        "beta_0_educ", "alpha_educ", "beta_0_wages",
        "gamma_wages", "alpha_wages",
        "u_consc", "u_educ", "u_wages",
        "delta_wages_white", "delta_wages_coloured", "delta_wages_asian",
        "delta_wages_female", "delta_wages_age", "delta_wages_age_squared",
        "delta_wages_educ_squared", "delta_wages_wealth"
    ),
    estimate = c(
        optimal_par[1:5],
        exp(optimal_par[6]),    # transform back from log scale
        exp(optimal_par[7]),
        exp(optimal_par[8]),
        optimal_par[9:16]
    ),
    se = c(
        boot_se[1:5],           # direct bootstrap SEs
        se_u_consc,             # delta method SEs for transformed params
        se_u_educ,
        se_u_wages,
        boot_se[9:16]           # direct bootstrap SEs
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
    # flag which rows are error SD parameters
    mutate(is_error_sd = parameter %in% c("u_consc", "u_educ", "u_wages")) %>%
    # remove SE rows for error SD parameters
    filter(!(row %in% c(6,7,8) & is_error_sd == FALSE)) %>%
    # push error SD rows to bottom
    mutate(order = ifelse(is_error_sd, 1, 0)) %>%
    arrange(order, row) %>%
    select(-order, -is_error_sd, -row, -Type)


print(results_final_controls)

# save results
write_rds(results_final_controls,
          '/Users/matthewthompson/Documents/Stellenbosch University/Masters/Research Assignment/Data_work/output/results_with_se_wages.rds')
