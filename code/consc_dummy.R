# this function creates a dummy variable from the column provided which shows if a proxy for conscientiousness is missing or not
# requires following packages
# library(dplyr)


consc_dummy <- function(data, vars) {

    for (v in vars) {
        dummy_name <- paste0(v, "_miss")
        data[[dummy_name]] <- ifelse(data[[v]] %in% c(-9, -8, -5, -3) | is.na(data[[v]]), 1, 0)
    }

    return(data)
}


