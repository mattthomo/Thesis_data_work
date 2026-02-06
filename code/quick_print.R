# This script prints a quick view of regression results

quick_print <- function(output){

    print(summary(output),
          signif.stars = T)
}
