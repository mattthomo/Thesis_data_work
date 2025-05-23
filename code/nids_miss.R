# This function codes values which are entered as "don't know", "refused to answer" etc., from NIDS as missing

nids_miss <- function(data, vars){

    for (v in vars){

        data <- data %>%
            mutate(!!v := ifelse(data[[v]] %in% c(-9, -8, -5, -3), NA, data[[v]]))
    }

    return(data)
}

