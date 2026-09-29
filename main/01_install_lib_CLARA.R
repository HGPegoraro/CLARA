options(timeout = 300)

# Core packages used by the app
install.packages(c(
    "shiny", "shinyjs", "readxl", "DT", "ggplot2", "dplyr",
    "jsonlite", "ggtext", "shinyWidgets", "digest", "tibble",
    "tidyr", "ggpattern", "emmeans", "multcomp", "multcompView",
    "sortable", "commonmark", "fBasics", "afex", "rstatix",
    "dunn.test"
), repos = "https://cran.rstudio.com/", dependencies = TRUE)

# Additional packages to extend statistical functionality
install.packages(c(
    "car",         # ANOVA diagnostics, Levene's test
    "DescTools",   # effect sizes and extra tests
    "effectsize",  # comprehensive effect size calculations
    "effsize",     # alternative effect size implementations (Cohen's d)
    "ggpubr",      # plotting with stat tests helpers
    "pwr",         # power analysis
    "drc",         # dose-response curve fitting (BCA enhancements)
    "corrplot",    # correlation visualization
    "coin",        # exact non-parametric tests
    "broom",       # tidy model outputs
    "performance", # model diagnostics
    "parameters"   # helper for model parameters and effect sizes
), repos = "https://cran.rstudio.com/", dependencies = TRUE)
# Color palettes
install.packages(c("RColorBrewer", "viridis", "colourpicker", "knitr"), repos = "https://cran.rstudio.com/", dependencies = TRUE)
install.packages(c("plotly"), repos = "https://cran.rstudio.com/", dependencies = TRUE)