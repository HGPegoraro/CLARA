library(readxl)

get_well_names_row_wise_vector <- function() {
  as.vector(t(outer(LETTERS[1:8], 1:12, paste0))) 
}

get_well_names_column_wise_vector <- function() {
  as.vector(outer(LETTERS[1:8], 1:12, paste0))
}

read_excel_format_LAB3 <- function(filepath) {
  tryCatch({
    dat_raw_matrix <- as.matrix(readxl::read_excel(filepath, range = "C23:N30", col_names = FALSE))
    
    if (nrow(dat_raw_matrix) != 8 || ncol(dat_raw_matrix) != 12) {
      stop(paste0("Range C23:N30 não retornou uma matriz 8x12. Dimensões: ", 
                  nrow(dat_raw_matrix), "x", ncol(dat_raw_matrix)))
    }
   
    values_grid <- as.vector(t(dat_raw_matrix)) 
    
    if (!is.numeric(values_grid)) {
      values_grid_numeric <- suppressWarnings(as.numeric(as.character(values_grid)))
      if (sum(is.na(values_grid_numeric)) > sum(is.na(values_grid))) {
        warning("Valores não numéricos convertidos para NA LAB 3.")
      }
      values_grid <- values_grid_numeric
    }
    if (length(values_grid) != 96) {
      stop("Formato LAB 3 esperava 96 valores, mas obteve ", length(values_grid))
    }
    
    return(setNames(values_grid, get_well_names_row_wise_vector()))
    
  }, error = function(e) {
    shiny::showNotification(paste("Erro LAB 3 '", basename(filepath), "': ", e$message), type = "error", duration = 10)
    return(NULL)
  })
}

read_excel_format_LAB6 <- function(filepath) {
  tryCatch({
    dat_raw_list <- readxl::read_excel(filepath, sheet = "Results", range = "D3:D98", col_names = FALSE)
    
    if (is.null(dat_raw_list) || nrow(dat_raw_list) == 0) {
      stop("Nenhum dado lido da Planilha LAB 6.")
    }
    values_grid <- as.vector(as.matrix(dat_raw_list))
    
    if (length(values_grid) != 96) {
      stop(paste0("Formato LAB 6 esperava 96 valores, mas obteve ", length(values_grid)))
    }
    
    if (!is.numeric(values_grid)) {
      values_grid_numeric <- suppressWarnings(as.numeric(as.character(values_grid)))
      if (sum(is.na(values_grid_numeric)) > sum(is.na(values_grid))) {
        warning("Valores não numéricos convertidos para NA LAB 6.")
      }
      values_grid <- values_grid_numeric
    }
    
    return(setNames(values_grid, get_well_names_row_wise_vector()))
    
  }, error = function(e) {
    shiny::showNotification(paste("Erro LAB 6 '", basename(filepath), "': ", e$message), type = "error", duration = 10)
    return(NULL)
  })
}

read_excel_format_LAB7 <- function(filepath) {
  tryCatch({
    dat_raw_list <- readxl::read_excel(filepath, range = "E2:E97", col_names = FALSE)
    
    if (is.null(dat_raw_list) || nrow(dat_raw_list) == 0) {
      stop("Nenhum dado lido da Planilha LAB 7.")
    }
    values_grid <- as.vector(as.matrix(dat_raw_list))
    
    if (length(values_grid) != 96) {
      stop(paste0("Formato LAB 7 esperava 96 valores, mas obteve ", length(values_grid)))
    }
    
    if (!is.numeric(values_grid)) {
      values_grid_numeric <- suppressWarnings(as.numeric(as.character(values_grid)))
      if (sum(is.na(values_grid_numeric)) > sum(is.na(values_grid))) {
        warning("Valores não numéricos convertidos para NA (formato LAB. 7).")
      }
      values_grid <- values_grid_numeric
    }
    
    return(setNames(values_grid, get_well_names_column_wise_vector()))
    
  }, error = function(e) {
    shiny::showNotification(paste("Erro '", basename(filepath), "': ", e$message), type = "error", duration = 10)
    return(NULL)
  })
}

GLOBAL_EXCEL_READERS <- list(
  "lab_3" = read_excel_format_LAB3,
  "lab_6" = read_excel_format_LAB6,
  "lab_7" = read_excel_format_LAB7
)