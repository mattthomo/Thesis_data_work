# having now completed the functions for simulation testing with a simple model and for a model with controls
# I now move on to running simulations with `mlrMBO`


library(mlrMBO)
library(smoof)
library(ParamHelpers)

# 1. define your parameter space
par_set1 <- makeParamSet(
    makeNumericParam("beta_0_educ",   lower = -50,  upper = 100),
    makeNumericParam("alpha_educ",    lower = -10,  upper = 10),
    makeNumericParam("beta_0_income", lower = -50,  upper = 100),
    makeNumericParam("gamma_income",  lower = -10,  upper = 10),
    makeNumericParam("alpha_income",  lower = -10,  upper = 10),
    makeNumericParam("u_consc",       lower = -5,   upper = 5),
    makeNumericParam("u_educ",        lower = -5,   upper = 5),
    makeNumericParam("u_income",      lower = -5,   upper = 5)
)

par_set2 <- makeParamSet(
    makeNumericParam("beta_0_educ",   lower = 0,  upper = 20),
    makeNumericParam("alpha_educ",    lower = -10,  upper = 10),
    makeNumericParam("beta_0_income", lower = -10,  upper = 20),
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


        sim_function_moments2(par = x, n = 10000,
                      target_coefs = target_coefs_moments,
                      weights = weights)
    },
    par.set = par_set2,
    minimize = TRUE
)

# 3. configure the optimisation
ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 400)  # number of iterations

# 4. run the optimisation
result <- mbo(obj_fun, control = ctrl)

# 5. extract results
result$x        # optimal parameter values
result$y        # final loss value


mbo_results_simple <- as.data.frame(result$x) %>%
    pivot_longer(cols = everything(),
                 names_to = "Parameter",
                 values_to = "Value")


## Model with controls


par_set3 <- makeParamSet(
    makeNumericParam("beta_0_educ",   lower = 0,  upper = 20),
    makeNumericParam("alpha_educ",    lower = -10,  upper = 10),
    makeNumericParam("beta_0_income", lower = -10,  upper = 20),
    makeNumericParam("gamma_income",  lower = -10,  upper = 10),
    makeNumericParam("alpha_income",  lower = -10,  upper = 10),
    makeNumericParam("u_consc",       lower = -5,   upper = 5),
    makeNumericParam("u_educ",        lower = -5,   upper = 5),
    makeNumericParam("u_income",      lower = -5,   upper = 5),
    makeNumericParam("delta_income_white",   lower = -5, upper = 10),
    makeNumericParam("delta_income_coloured",   lower = -5, upper = 10),
    makeNumericParam("delta_income_asian",   lower = -5, upper = 10),
    makeNumericParam("delta_income_female",   lower = -5, upper = 10),
    makeNumericParam("delta_income_age",   lower = -5, upper = 10),
    makeNumericParam("delta_income_age_squared",   lower = -5, upper = 10),
    makeNumericParam("delta_income_educ_squared",   lower = -5, upper = 10),
    makeNumericParam("delta_income_wealth",   lower = -5, upper = 10)
)


obj_fun <- makeSingleObjectiveFunction(
    name = "loss",
    fn = function(x) {

        x <- as.numeric(x)


        sim_function_controls(par = x, n = 10000,
                              target_coefs = target_coefs2)
    },
    par.set = par_set3,
    minimize = TRUE
)

# 3. configure the optimisation
ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 400)  # number of iterations

# 4. run the optimisation
result <- mbo(obj_fun, control = ctrl)

# 5. extract results
result$x        # optimal parameter values
result$y        # final loss value


## Add weights