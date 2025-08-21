# this function creates the code to run the simple tables with the OLS estimates

make_simple_tables <- function(model, output_path){

    library(broom)

    table <- tidy(model) %>%
        filter(!is.na(estimate)) %>%  # remove dropped coefficients
        mutate(
            stars = case_when(
                p.value < 0.001 ~ "***",
                p.value < 0.01  ~ "**",
                p.value < 0.05  ~ "*",
                p.value < 0.1   ~ ".",
                TRUE            ~ ""
            ),
            estimate = round(estimate, 3),
            estimate = paste0(estimate, stars),
            std.error = round(std.error, 3),
            p.value = signif(p.value, 3),
            std.error = paste0("(", std.error, ")")
        ) %>%
        select(term, estimate, std.error, p.value)

    write_rds(table, output_path)


}
