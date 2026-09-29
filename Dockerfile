# CLARA - containerised Shiny app
# Build from the repo root:  docker build -t clara .
FROM rocker/r-ver:4.5.1

# System libraries needed by ggtext / ggpattern (sf) / curl / xml packages
RUN apt-get update && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev libssl-dev libxml2-dev \
      libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
      libpng-dev libtiff-dev libjpeg-dev \
      libgdal-dev libgeos-dev libproj-dev libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# R packages (same list as 01_install_lib_CLARA.R, plus knitr which the ELISA module calls).
# rocker/r-ver installs pre-compiled Linux binaries, so no Rtools/compiling is needed.
RUN Rscript -e ' \
  options(Ncpus = 4); \
  pkgs <- c("shiny","shinyjs","readxl","DT","ggplot2","dplyr","jsonlite","ggtext", \
            "shinyWidgets","digest","tibble","tidyr","ggpattern","emmeans","multcomp", \
            "multcompView","sortable","commonmark","fBasics","afex","rstatix","dunn.test","knitr"); \
  install.packages(pkgs); \
  miss <- setdiff(pkgs, rownames(installed.packages())); \
  if (length(miss)) stop("Failed to install: ", paste(miss, collapse = ", "))'

# Packages added later (loaded by the ELISA/BCA modules at startup). Kept as a separate layer
# so Docker reuses the cached layer above. The final loop loads every package the app needs,
# so a missing package or system library fails the BUILD instead of crashing the app at launch.
RUN Rscript -e ' \
  options(Ncpus = 4); \
  extra <- c("colourpicker","car","DescTools","effectsize","effsize","ggpubr","pwr", \
             "RColorBrewer","viridis","plotly"); \
  install.packages(extra); \
  all <- c("shiny","shinyjs","readxl","DT","ggplot2","dplyr","jsonlite","ggtext","shinyWidgets", \
           "digest","tibble","tidyr","ggpattern","emmeans","multcomp","multcompView","sortable", \
           "commonmark","fBasics","afex","rstatix","dunn.test","knitr", extra); \
  bad <- all[!vapply(all, function(p) suppressWarnings(requireNamespace(p, quietly = TRUE)), logical(1))]; \
  if (length(bad)) stop("Failed to install/load: ", paste(bad, collapse = ", "))'

# Run as a normal user so files written to mounted folders aren't root-owned
RUN useradd -m -u 1000 clara
WORKDIR /app
COPY --chown=clara:clara main/ /app/
RUN mkdir -p /app/normality_diagnosis && chown clara:clara /app/normality_diagnosis
USER clara

EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('/app/05_app.R', host = '0.0.0.0', port = 3838)"]
