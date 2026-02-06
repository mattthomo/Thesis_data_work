# This function creates df which can then be saved and piped into kable functions when ready for the write-up

create_df_for_table <- function(results, path) {
    # Function to add significance stars
    add_stars <- function(p_value) {
        if (p_value < 0.001) return("$^{***}$")
        if (p_value < 0.01) return("$^{**}$")
        if (p_value < 0.05) return("$^{*}$")
        return("")
    }

    # Separate coefficient data and statistics data
    coef_data <- lapply(seq_along(results), function(i) {
        coefs <- summary(results[[i]])$coefficients

        tibble(
            term = rep(rownames(coefs), each = 2),
            stat_type = rep(c("coef", "se"), length(rownames(coefs))),
            value = c(rbind(
                paste0(sprintf("%.3f", coefs[, 1]),
                       sapply(coefs[, 4], add_stars)),
                paste0("(", sprintf("%.3f", coefs[, 2]), ")")
            )),
            model = paste0("Model_", i)
        )
    })

    stat_data <- lapply(seq_along(results), function(i) {
        summ <- summary(results[[i]])

        n_obs <- if (!is.null(results[[i]]$nobs)) {
            results[[i]]$nobs  # lm_robust stores it here
        } else if (!is.null(summ$df) && length(summ$df) >= 2) {
            summ$df[1] + summ$df[2]  # lm: residual df + params
        } else {
            length(results[[i]]$residuals)  # fallback
        }


        tibble(
            term = c("N", "R-squared", "Adj. R-squared"),
            stat_type = c("stat", "stat", "stat"),
            value = c(
                as.character(n_obs),
                sprintf("%.3f", summ$r.squared),
                sprintf("%.3f", summ$adj.r.squared)
            ),
            model = paste0("Model_", i)
        )
    })

    # Pivot coefficients
    coef_table <- bind_rows(coef_data) %>%
        unite("row_id", term, stat_type, sep = "_", remove = FALSE) %>%
        select(-stat_type) %>%
        pivot_wider(names_from = model, values_from = value, id_cols = c(row_id, term)) %>%
        select(-row_id) %>%
        mutate(term = ifelse(row_number() %% 2 == 0, "", term),
               across(everything(), ~replace_na(., "")))

    # Pivot statistics
    stat_table <- bind_rows(stat_data) %>%
        unite("row_id", term, stat_type, sep = "_", remove = FALSE) %>%
        select(-stat_type) %>%
        pivot_wider(names_from = model, values_from = value, id_cols = c(row_id, term)) %>%
        select(-row_id)

    # Combine with stats at the bottom
    finished_table <- bind_rows(coef_table, stat_table)

    write_rds(finished_table, file = path)

    finished_table
}
