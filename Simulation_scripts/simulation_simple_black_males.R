# Main aim of this script is to have a more clean script showing my simulation results

# Read in objects which will be used in function, which were created in Readme.rmd
target_values_simple_black_males <- read_rds(file = "./output/target_black_males.rds")

test_ols <- read_rds(file = "./output/test_ols.rds")

observed_moments_df <- read_rds(file = "./output/observed_tab_black_males.rds")

sim_ols_black_males <- read_rds(file = "./output/sim_ols_black_males.rds")

educ_ols_simp_black_males <- read_rds(file = "./output/educ_ols_simp_black_males.rds")




##### SIMULATION FUNCTION ############


sim_function_reg <- function(par, n, target_coefs, weights, observed_moments_df){

# these are the parameters
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

# read in df of observed moments from real data, constructed in README.rmd
    observed_moments_df <- observed_moments_df

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


weights_vec <- c(
    # distributional moments - use inverse variance (1/sd^2)
    mean_loginc  = 1 / sd(test_ols %>%
                              filter(w1_best_gen == 1,
                                     w4_best_race == "African") %>%
                              pull(l_total_inc), na.rm = T)^2,
    sd_loginc    = 1 / (sd(test_ols %>%
                               filter(w1_best_gen == 1,
                                      w4_best_race == "African") %>%
                               pull(l_total_inc), na.rm = T)^2 / (2 * nrow(test_ols %>%
                                                                               filter(w1_best_gen == 1,
                                                                                      w4_best_race == "African")))),
    mean_educ    = 1 / sd(test_ols %>%
                              filter(w1_best_gen == 1,
                                     w4_best_race == "African") %>%
                              pull(w4_best_edu), na.rm = T)^2,
    sd_educ      = 1 / (sd(test_ols %>%
                               filter(w1_best_gen == 1,
                                      w4_best_race == "African") %>%
                               pull(w4_best_edu), na.rm = T)^2 / (2 * nrow(test_ols %>%
                                                                               filter(w1_best_gen == 1,
                                                                                      w4_best_race == "African")))),
    mean_consc   = 1 / sd(test_ols %>%
                              filter(w1_best_gen == 1,
                                     w4_best_race == "African") %>%
                              pull(consc_flipped), na.rm = T)^2,
    sd_consc     = 1 / (sd(test_ols %>%
                               filter(w1_best_gen == 1,
                                      w4_best_race == "African") %>%
                               pull(consc_flipped), na.rm = T)^2 / (2 * nrow(test_ols %>%
                                                                               filter(w1_best_gen == 1,
                                                                                      w4_best_race == "African")))),

    # regression coefficients - use inverse SE^2 from observed models
    educ_reg_Intercept         = 1 / summary(educ_ols_simp_black_males)$coefficients["(Intercept)", "Std. Error"]^2,
    educ_reg_Conscientiousness = 1 / summary(educ_ols_simp_black_males)$coefficients["consc_flipped", "Std. Error"]^2,

    # r squared - assign low weight since it's a fit statistic not a moment
    educ_reg_r_squared = 0.1,

    # income regression coefficients
    inc_reg_intercept = 1 / summary(sim_ols_black_males)$coefficients["(Intercept)", "Std. Error"]^2,
    inc_reg_educ      = 1 / summary(sim_ols_black_males)$coefficients["w4_best_edu",  "Std. Error"]^2,
    inc_reg_consc     = 1 / summary(sim_ols_black_males)$coefficients["consc_flipped","Std. Error"]^2,

    # r squared - low weight
    inc_reg_r_squared = 0.1
)

weights_vec <- weights_vec / sum(weights_vec)


##### Using mlrMBO package #######
### THIS CODE WAS BEFORE USING DESIGNMAT ######
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
#
# # 2. wrap your loss function for mlrMBO
obj_fun <- makeSingleObjectiveFunction(
    name = "loss",
    fn = function(x) {

        x <- as.numeric(x)


        sim_function_reg(par = x,
                         n = 100000,
                         target_coefs = target_values_simple_black_males,
                         weights = weights_vec,
                         observed_moments_df = observed_moments_df)
    },
    par.set = par_set_simple,
    minimize = TRUE
)


# After meeting on 27 March, want to work on getting results for structural model which are closer to true estimates
# Start with removing harsh parameter constraints and using code from Tutorial 12 to set grid design for mlrMBO code


library(mlrMBO)
library(ParamHelpers)
library(parallelMap)



# configure Bayesian optimisation
mbo_ctrl <- makeMBOControl()
mbo_ctrl <- setMBOControlInfill(mbo_ctrl, crit = crit.ei)      # expected improvement
mbo_ctrl <- setMBOControlTermination(mbo_ctrl, iters = 1000, max.evals = 1000L) # iterations

## Attempt 1
# generate initial values and random design

init_par <- data.frame(
    beta_0_educ   = 10,
    alpha_educ    = 0.5,
    beta_0_income = 5,
    gamma_income  = 0.3,
    alpha_income  = 0.2,
    u_consc       = 0,
    u_educ        = 0,
    u_income      = 0
)


random_design <- generateRandomDesign(n = 50, par.set = par_set_simple)

design_mat <- rbind(init_par, random_design)

y_vals <- apply(design_mat, 1, function(row) {
    sim_function_reg(
        par          = as.numeric(row),
        n            = 10000,
        target_coefs = target_values_simple_black_males,
        weights      = weights_vec,
        observed_moments_df = observed_moments_df
    )
})

# Add y column to design_mat
design_mat$y <- y_vals

# run in parallel
parallelStartSocket(cpus = parallel::detectCores() - 1)

parallelExport(
    "sim_function_reg",
    "target_values_simple_black_males",
    "weights_vec",
    "observed_moments_df"
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


opt_path <- as.data.frame(result$opt.path)
print(opt_path)

# check if y values have any variation
summary(opt_path$y)
table(opt_path$y)



# try checking for local optimisers in the global optimisation function
optim_result <- optim(
    par     = c(10, 0.5, 5, 0.3, 0.2, 0, 0, 0),  # rough starting guess
    fn      = function(par) {
        sim_function_reg(
            par          = par,
            n            = 10000,
            target_coefs = target_values_simple_black_males,
            weights      = weights_vec,
            observed_moments_df = observed_moments_df
        )
    },
    method  = "Nelder-Mead",
    control = list(maxit = 1000, reltol = 1e-8)
)



## Attempt 2 with different init vals

init_par2 <- data.frame(
    beta_0_educ               = 1,
    alpha_educ                = 0.2,
    beta_0_income             = 8,
    gamma_income              = 0.4,
    alpha_income              = 0.5,
    u_consc                   = 1,
    u_educ                    = 1,
    u_income                  = 1
)

# result of optim function
init_par3 <- data.frame(
    beta_0_educ               = 11.50333,
    alpha_educ                = -0.02113,
    beta_0_income             = 6.361638,
    gamma_income              = 0.084179,
    alpha_income              = 0.04725592,
    u_consc                   = -0.4193541,
    u_educ                    = 1.310216,
    u_income                  = 0.03502108
)


# Try using a more narrow parameter space

par_set_simple_narrow <- makeParamSet(
    makeNumericParam("beta_0_educ",   lower = 0,  upper = 15),
    makeNumericParam("alpha_educ",    lower = -5,  upper = 5),
    makeNumericParam("beta_0_income", lower = 0,  upper = 15),
    makeNumericParam("gamma_income",  lower = -5,  upper = 5),
    makeNumericParam("alpha_income",  lower = -5,  upper = 5),
    makeNumericParam("u_consc",       lower = -5,   upper = 5),
    makeNumericParam("u_educ",        lower = -5,   upper = 5),
    makeNumericParam("u_income",      lower = -5,   upper = 5)
)


opdf <- as.data.frame(result$opt.path)  # full history
plot(opdf$dob, opdf$y)               # convergence plot: iteration vs objective value
