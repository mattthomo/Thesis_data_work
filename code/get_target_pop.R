# this function extracts the ids of the target population (age 14-23)

get_target_pop <- function(df, lower_age, upper_age){

    target_df <- df %>%
        filter(w1_best_age_yrs >= lower_age & w1_best_age_yrs <= upper_age) %>%
        select(pid)

    target_df
}


