library(shiny)
library(shinyjs)
library(readxl)
library(DT)
library(ggplot2)
library(dplyr)
library(jsonlite)
library(ggtext)
library(shinyWidgets)
library(digest)
library(tibble)
library(tidyr)

bca_tabPanel <- function(id, name = "Curva BCA") {
  ns <- NS(id)
  
  selected_wells_input_id <- ns("selected_wells")
  toggled_std_well_input_id <- ns("toggled_std_well") 
  
  estilo_js_bca <- sprintf("
    (function() { 
      let isMouseDown_bca = false, isSelecting_bca = true;
      let standardWells_bca = []; 
      const targetInputId_bca = '%s'; 
      const toggledStdWellInputId_bca = '%s';

      Shiny.addCustomMessageHandler('%s_updateStandardWells', function(newStandardWells) {
        standardWells_bca = newStandardWells;
        $('.bca-module-tab .well-cell').removeClass('standard-well non-selectable-standard');
        standardWells_bca.forEach(function(well_id) {
          $('.bca-module-tab .well-cell[data-well=\"' + well_id + '\"]').addClass('standard-well non-selectable-standard');
        });
      });

      $(document).on('mousedown', '.bca-module-tab .well-cell', function(e) {
        e.preventDefault(); 
        const well_id = $(this).data('well');

        if (standardWells_bca.includes(well_id)) {
            Shiny.setInputValue(toggledStdWellInputId_bca, { well: well_id, ts: new Date().getTime() }, {priority: 'event'});
            return false; 
        } else {
            isMouseDown_bca = true;
            isSelecting_bca = !$(this).hasClass('selected');
            $(this).toggleClass('selected', isSelecting_bca); 
            return false;
        }
      }).on('mouseover', '.bca-module-tab .well-cell', function() {
        const well_id = $(this).data('well');
        if (standardWells_bca.includes(well_id)) return; 

        if (isMouseDown_bca) $(this).toggleClass('selected', isSelecting_bca);
      });

      $(document).on('mouseup', function(e) {
        if (isMouseDown_bca) { 
            isMouseDown_bca = false;
            let selected_bca = [];
            $('.bca-module-tab .well-cell.selected:not(.standard-well)').each(function() {
              selected_bca.push($(this).data('well'));
            });
            Shiny.setInputValue(targetInputId_bca, selected_bca, {priority: 'event'});
        }
      }).on('mouseleave', '.bca-module-tab', function () { 
          if(isMouseDown_bca) {
            isMouseDown_bca = false; 
          }
      });
    })(); 
  ", selected_wells_input_id, toggled_std_well_input_id, ns("handler")) 
  
  tabPanel(name, class = "bca-module-tab", 
           useShinyjs(),
           tags$head(
             tags$style(HTML("
                .well-plate { border-collapse: collapse; margin: 10px 0; }
                .well-cell {
                  border: 1px solid #ccc; width: 65px; height: 60px;
                  text-align: center; vertical-align: middle;
                  font-size: 11px;
                  font-weight: bold; user-select: none;
                  padding: 2px; white-space: normal;
                }
                .well-cell small { font-size: 9px; font-weight: normal; }
                .selected { background-color: lightblue !important; }
                .standard-well {
                    border: 2px solid orange !important;
                    background-color: #FFF3E0 !important; 
                }
                .well-cell.non-selectable-standard { 
                    cursor: pointer !important; 
                }
                .well-cell.standard-ignored {
                    background-color: #FFCDD2 !important; 
                    text-decoration: line-through;
                }
                .well-cell.non-selectable-standard.selected, .well-cell.standard-ignored.selected {
                    background-color: inherit;
                }
                table.well-plate td { cursor: pointer; }
                .std-point-row { margin-bottom: 8px; }
                #bca_module-toggle_std_config { cursor: pointer; }
                #bca_module-toggle_std_config:hover { color: #007bff; }
              ")),
             tags$script(HTML(estilo_js_bca))
           ),
           sidebarLayout(
             sidebarPanel(
               fileInput(ns("files"), "Importar Excel", multiple = TRUE, accept = ".xlsx, .xls"),
               selectInput(ns("selected_plate"), "Selecionar Placa", choices = list("Nenhuma Placa disponível" = "NA")),
               hr(),
               
               div(id = ns("toggle_std_config"), 
                   h4(strong("Definição da Curva Padrão"), icon("chevron-down"))
               ),
               div(id = ns("std_config_content"),
                   uiOutput(ns("standards_config_ui")),
                   fluidRow(
                     column(6, actionButton(ns("add_std_point"), "Adicionar Ponto", icon = icon("plus"), width = "100%")),
                     column(6, actionButton(ns("clear_ignored_stds"), "Restaurar Ignorados", width = "100%"))
                   )
               ),
               
               hr(),
               h4(strong("Atribuição de Amostras")),
               textInput(ns("sample_name"), "Nome da Amostra", placeholder = "Ex: PLD"),
               actionButton(ns("add_sample"), "Atribuir Amostra aos Selecionados"),
               actionButton(ns("remove_from_sample"), "Remover Atribuição", icon = icon("eraser")),
               hr(),
               fluidRow(
                 column(8, fileInput(ns("load_state"), "Carregar Projeto Salvo (.rds)", accept = ".rds")),
                 column(4, downloadButton(ns("save_state"), "Salvar Projeto"))
               )
             ),
             mainPanel(
               h4(strong("Placa de 96 poços")),
               uiOutput(ns("plate_ui")),
               hr(),
               h4(strong("Curva Padrão BCA")),
               plotOutput(ns("std_curve_plot")),
               verbatimTextOutput(ns("std_curve_summary")),
               hr(),
               h4("Valores Interpolados (Por Amostra)"),
               DTOutput(ns("interpolated_values_table"))
             )
           )
  )
}


bca_server <- function(id, global_excel_format_reactive) { 
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    all_data <- reactiveValues(files = list())
    
    standards_config <- reactiveVal(data.frame(id=character(), concentration=numeric(), wells=I(list())))
    ignored_standard_wells_all_plates <- reactiveVal(list()) 
    
    shinyjs::hide(id = "std_config_content", anim = FALSE) 
    
    shinyjs::onclick("toggle_std_config", {
      shinyjs::toggle(id = "std_config_content", anim = TRUE, animType = "fade")
      runjs(sprintf("
          var icon = $('#%s').find('i');
          if (icon.hasClass('fa-chevron-down')) {
            icon.removeClass('fa-chevron-down').addClass('fa-chevron-up');
          } else {
            icon.removeClass('fa-chevron-up').addClass('fa-chevron-down');
          }
        ", ns("toggle_std_config")))
    })
    
    current_file <- reactive({ input$selected_plate })
    
    create_default_standards_config <- function() {
      ids <- replicate(9, digest::digest(runif(1), algo="crc32"))
      concentrations = c(2.000, 1.500, 1.000, 0.750, 0.500, 0.250, 0.125, 0.025, 0)
      wells <- I(list(
        c("A1", "A2"), c("B1", "B2"), c("C1", "C2"), c("D1", "D2"),
        c("E1", "E2"), c("F1", "F2"), c("G1", "G2"), c("H1", "H2"),
        c("A3", "A4")
      ))
      return(tibble(id = ids, concentration = concentrations, wells = wells))
    }
    
    output$standards_config_ui <- renderUI({
      config_df <- standards_config()
      if (nrow(config_df) == 0) return(p("Clique em 'Adicionar Ponto' para começar."))
      
      tagList(
        lapply(1:nrow(config_df), function(i) {
          row <- config_df[i, ]
          point_id <- row$id
          
          fluidRow(class="std-point-row",
                   column(4, numericInput(ns(paste0("conc_", point_id)), NULL, value = row$concentration, step = 0.1)),
                   column(5, actionButton(ns(paste0("assign_", point_id)), "Atribuir Poços", icon = icon("arrow-left"), width = "100%")),
                   column(3, actionButton(ns(paste0("remove_", point_id)), NULL, icon = icon("trash"), class="btn-danger", width="100%")),
                   column(12, tags$p(
                     tags$small(paste("Poços:", paste(row$wells[[1]], collapse=", ") %||% "Nenhum"))
                   ))
          )
        })
      )
    })
    
    observe({
      config_df <- standards_config()
      req(nrow(config_df) > 0)
      
      lapply(config_df$id, function(point_id) {
        observeEvent(input[[paste0("assign_", point_id)]], {
          selected <- req(input$selected_wells)
          
          current_config <- standards_config()
          current_config$wells[current_config$id == point_id] <- list(selected)
          standards_config(current_config)
          
          runjs(sprintf("$('.well-cell[data-well]').removeClass('selected'); Shiny.setInputValue('%s', [], {priority: 'event'});", ns("selected_wells")))
          showNotification(paste(length(selected), "poços atribuídos."), type="message")
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
      
      lapply(config_df$id, function(point_id) {
        debounced_conc <- debounce(reactive({input[[paste0("conc_", point_id)]]}), 1000)
        
        observeEvent(debounced_conc(), {
          req(!is.null(debounced_conc()))
          isolate({
            current_config <- standards_config()
            if (point_id %in% current_config$id) {
              current_config$concentration[current_config$id == point_id] <- debounced_conc()
              standards_config(current_config)
            }
          })
        }, ignoreInit = TRUE)
      })
      
      lapply(config_df$id, function(point_id) {
        observeEvent(input[[paste0("remove_", point_id)]], {
          current_config <- standards_config()
          standards_config(current_config[current_config$id != point_id, ])
          showNotification("Ponto da curva removido.", type="message")
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    })
    
    observeEvent(input$add_std_point, {
      new_id <- digest::digest(runif(1), algo="crc32")
      new_row <- tibble(id = new_id, concentration = 0, wells = I(list(character(0))))
      standards_config(bind_rows(standards_config(), new_row))
    })
    
    current_standard_wells_list_all <- reactive({
      config_df <- standards_config()
      unique(unlist(config_df$wells))
    })
    
    observeEvent(input$selected_plate, {
      req(input$selected_plate, input$selected_plate != "NA")
      fname <- input$selected_plate
      
      if (!is.null(all_data$files[[fname]]$standards_config)) {
        standards_config(all_data$files[[fname]]$standards_config)
      } else {
        standards_config(create_default_standards_config())
      }
    }, ignoreInit = TRUE)
    
    observe({
      session$sendCustomMessage(
        type = paste0(ns("handler"), "_updateStandardWells"), 
        message = current_standard_wells_list_all()
      )
    })
    
    observeEvent(input$toggled_std_well, {
      req(current_file(), current_file() != "NA", input$toggled_std_well$well)
      fname <- current_file()
      toggled_well <- input$toggled_std_well$well
      
      current_ignored_list <- ignored_standard_wells_all_plates()
      
      if (is.null(current_ignored_list[[fname]])) {
        current_ignored_list[[fname]] <- character(0)
      }
      
      if (toggled_well %in% current_ignored_list[[fname]]) {
        current_ignored_list[[fname]] <- setdiff(current_ignored_list[[fname]], toggled_well)
        showNotification(paste("Poço padrão", toggled_well, "restaurado para curva."), type="message", duration=2)
      } else {
        current_ignored_list[[fname]] <- unique(c(current_ignored_list[[fname]], toggled_well))
        showNotification(paste("Poço padrão", toggled_well, "ignorado da curva."), type="warning", duration=2)
      }
      ignored_standard_wells_all_plates(current_ignored_list)
    })
    
    observeEvent(input$clear_ignored_stds, {
      req(current_file(), current_file() != "NA")
      fname <- current_file()
      
      current_ignored_list <- ignored_standard_wells_all_plates()
      if (!is.null(current_ignored_list[[fname]]) && length(current_ignored_list[[fname]]) > 0) {
        current_ignored_list[[fname]] <- character(0)
        ignored_standard_wells_all_plates(current_ignored_list)
        showNotification(paste("Todos os padrões ignorados para a placa", fname, "foram restaurados."), type="message")
      } else {
        showNotification("Nenhum padrão ignorado para restaurar nesta placa.", type="info")
      }
    })
    
    well_names <- function() { 
      rows <- LETTERS[1:8]
      cols <- 1:12
      outer(rows, cols, paste0)
    }
    
    plate_vals <- reactive({ 
      req(current_file(), current_file() != "NA")
      fname <- current_file()
      if (!fname %in% names(all_data$files) || is.null(all_data$files[[fname]]) || is.null(all_data$files[[fname]]$vals)) {
        return(NULL)
      }
      all_data$files[[fname]]$vals
    })
    
    generate_std_curve_logic <- eventReactive({
      standards_config()
      ignored_standard_wells_all_plates()
    }, {
      fname_logic <- current_file()
      req(fname_logic, fname_logic != "NA")
      
      current_plate_values_logic <- plate_vals()
      req(current_plate_values_logic)
      
      current_standard_def <- standards_config()
      req(nrow(current_standard_def) > 0)
      
      ignored_stds_for_plate <- ignored_standard_wells_all_plates()[[fname_logic]] %||% character(0)
      
      points_data <- current_standard_def %>%
        mutate(
          wells_to_use = lapply(wells, function(w) setdiff(w, ignored_stds_for_plate)),
          abs_values = lapply(wells_to_use, function(w) current_plate_values_logic[w]),
          abs_mean = sapply(abs_values, function(v) if(length(v) > 0) mean(v, na.rm=T) else NA_real_)
        )
      
      std_curve_data_for_fit <- points_data %>%
        transmute(
          Concentracao = concentration,
          AbsorbanciaMedia = abs_mean
        ) %>%
        filter(!is.na(AbsorbanciaMedia))
      
      if(nrow(std_curve_data_for_fit) < 2) { 
        showNotification("Pontos padrão insuficientes (mínimo 2) para gerar a curva.", type="error")
        output$std_curve_plot <- renderPlot({ plot.new(); title("Pontos insuficientes para curva") })
        output$std_curve_summary <- renderText("Erro: Pontos insuficientes.")
        all_data$files[[fname_logic]]$standard_curve_fit <- NULL
        all_data$files[[fname_logic]]$standard_curve_data_points <- NULL
        return()
      }
      
      fit <- lm(AbsorbanciaMedia ~ Concentracao, data = std_curve_data_for_fit)
      
      all_data$files[[fname_logic]]$standard_curve_fit <- fit
      all_data$files[[fname_logic]]$standard_curve_data_points <- std_curve_data_for_fit
      
      output$std_curve_plot <- renderPlot({
        req(fit) 
        ggplot(std_curve_data_for_fit, aes(x = Concentracao, y = AbsorbanciaMedia)) + 
          geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) + 
          geom_point(color = "blue", size = 3, na.rm = TRUE) + 
          labs(title = paste("Curva Padrão BCA - Placa:", fname_logic),
               x = "Concentração (µg/µL)",
               y = "Absorbância Média") + 
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(hjust = 0.5))
      })
      
      output$std_curve_summary <- renderPrint({ 
        req(fit)
        summary_fit <- summary(fit)
        coefs <- coef(fit)
        cat("Sumário da Regressão Linear:\n")
        print(summary_fit$coefficients)
        cat(sprintf("\nR-quadrado (R²): %.4f\n", summary_fit$r.squared))
        cat(sprintf("Equação: Absorbância = %.4f * Concentração + %.4f\n", coefs[2], coefs[1]))
        cat(sprintf("Número de pontos padrão usados na curva: %d\n", nrow(std_curve_data_for_fit)))
      })
      
      all_data$files[[fname_logic]]$standards_config <- current_standard_def
    })
    
    observe({
      generate_std_curve_logic()
    })
    
    interpolate_samples_logic <- function(fname) { 
      if (!fname %in% names(all_data$files) || is.null(all_data$files[[fname]])) return(NULL) 
      fit_model <- all_data$files[[fname]]$standard_curve_fit
      samples_on_plate_individual_wells <- all_data$files[[fname]]$wells
      if (is.null(fit_model)) return(NULL)
      if (is.null(samples_on_plate_individual_wells) || nrow(samples_on_plate_individual_wells) == 0) {
        all_data$files[[fname]]$interpolated_data <- NULL 
        return(data.frame()) 
      }
      coefs <- coef(fit_model)
      if (is.null(coefs) || !("Concentracao" %in% names(coefs))) return(NULL)
      slope <- coefs["Concentracao"] 
      intercept <- coefs["(Intercept)"]
      samples_grouped <- samples_on_plate_individual_wells %>%
        filter(!is.na(Valor)) %>% group_by(Amostra) %>%
        summarise(
          Mean_Abs_Raw = mean(Valor, na.rm = TRUE), SD_Abs_Raw = sd(Valor, na.rm = TRUE),
          N_Replicas = n(), Poços_Replicas = paste(Poço_Original, collapse = ", "),
          .groups = 'drop'
        ) %>%
        mutate(
          Mean_Abs_Corrigida = Mean_Abs_Raw - intercept,
          Concentracao_Interpolada = Mean_Abs_Corrigida / slope 
        ) %>%
        mutate(
          Concentracao_Interpolada = ifelse(is.na(Concentracao_Interpolada) | Concentracao_Interpolada < 0, 0, round(Concentracao_Interpolada, 3)),
          SD_Abs_Raw = ifelse(N_Replicas < 2, NA_real_, round(SD_Abs_Raw, 4)) 
        )
      all_data$files[[fname]]$interpolated_data <- samples_grouped
      return(samples_grouped) 
    }
    
    observe({ 
      fname <- current_file()
      if (is.null(fname) || fname == "NA") return()
      if (fname %in% names(all_data$files)) { 
        interpolated_df <- interpolate_samples_logic(fname) 
        if(!is.null(interpolated_df) && nrow(interpolated_df) > 0){
          output$interpolated_values_table <- renderDT({
            datatable(interpolated_df[, c("Amostra", "Mean_Abs_Raw", "SD_Abs_Raw", "N_Replicas", "Mean_Abs_Corrigida", "Concentracao_Interpolada", "Poços_Replicas")],
                      options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE,
                      colnames = c("Amostra", "Abs Média (Raw)", "DP Abs (Raw)", "Nº Réplicas", "Abs Média (Corrigida)", "Conc. Interpolada  (µg/µL)", "Poços"))
          }, server = FALSE) 
        } else {
          output$interpolated_values_table <- renderDT({
            datatable(data.frame(Mensagem="Nenhuma amostra para interpolar."), options=list(dom='t', searching = FALSE, paging=FALSE), rownames=FALSE)
          }, server = FALSE)
        }
      } else {
        output$interpolated_values_table <- renderDT(NULL)
      }
    })
    
    observeEvent(input$files, {
      req(input$files)
      current_global_format_id <- global_excel_format_reactive()
      req(current_global_format_id)
      
      loaded_files_temp <- list()
      reader_function <- GLOBAL_EXCEL_READERS[[current_global_format_id]]
      if (is.null(reader_function)) {
        showNotification(paste("Formato de leitor inválido:", current_global_format_id), type = "error")
        return()
      }
      
      withProgress(message = "Carregando arquivos...", value = 0, {
        for (i in seq_len(nrow(input$files))) {
          incProgress(1 / nrow(input$files), detail = input$files$name[i])
          filepath <- input$files$datapath[i]
          original_fname <- input$files$name[i]
          plate_values_named_vector <- reader_function(filepath)
          if (is.null(plate_values_named_vector)) {
            showNotification(paste("Falha ao processar:", original_fname), type = "error")
            next
          }
          fname_sanitized <- gsub("[^A-Za-z0-9_.-]", "_", original_fname)
          loaded_files_temp[[fname_sanitized]] <- list(
            vals = plate_values_named_vector,
            wells = data.frame(Poço_Original=character(), Poço_Prefixo=character(), Valor=numeric(), Amostra=character(), stringsAsFactors=FALSE),
            standards_config = create_default_standards_config(),
            standard_curve_fit = NULL,
            standard_curve_data_points = NULL,
            interpolated_data = NULL
          )
        }
      })
      
      if (length(loaded_files_temp) > 0) {
        for(fname in names(loaded_files_temp)) { all_data$files[[fname]] <- loaded_files_temp[[fname]] }
        placas_disponiveis <- names(all_data$files)
        updateSelectInput(session, "selected_plate", choices = placas_disponiveis, selected = placas_disponiveis[1])
      }
    })
    
    observeEvent(input$add_sample, {  
      req(input$sample_name, current_file(), current_file() != "NA") 
      fname <- current_file()
      if (trimws(input$sample_name) == "") {
        showNotification("Insira um nome válido para a amostra.", type = "error"); return()
      }
      selected_wells_current <- input$selected_wells 
      if (is.null(selected_wells_current) || length(selected_wells_current) == 0) {
        showNotification("Nenhum poço selecionado.", type = "warning"); return()
      }
      vals_current_plate <- plate_vals()
      req(vals_current_plate)
      
      std_wells_on_plate <- current_standard_wells_list_all() 
      actual_selected_wells <- selected_wells_current[!selected_wells_current %in% std_wells_on_plate]
      
      if (length(actual_selected_wells) == 0) {
        showNotification("Nenhum poço válido (não padrão) selecionado.", type = "warning"); return()
      }
      
      new_sample_rows <- data.frame(
        Poço_Original = actual_selected_wells,
        Poço_Prefixo = paste0(fname, "_", actual_selected_wells),
        Valor = vals_current_plate[actual_selected_wells],
        Amostra = input$sample_name,
        stringsAsFactors = FALSE
      )
      
      existing_sample_data <- all_data$files[[fname]]$wells
      existing_sample_data_filtered <- existing_sample_data[!existing_sample_data$Poço_Prefixo %in% new_sample_rows$Poço_Prefixo, ]
      all_data$files[[fname]]$wells <- rbind(existing_sample_data_filtered, new_sample_rows)
      
      runjs(sprintf("$('.well-cell[data-well]').removeClass('selected'); Shiny.setInputValue('%s', [], {priority: 'event'});", ns("selected_wells")))
      showNotification(paste("Amostra", input$sample_name, "adicionada."), type = "message")
      updateTextInput(session, "sample_name", value = "")
    })
    
    observeEvent(input$remove_from_sample, {  
      req(input$selected_wells, current_file(), current_file() != "NA")
      fname <- current_file()
      
      current_assigned_samples <- all_data$files[[fname]]$wells
      if(is.null(current_assigned_samples) || nrow(current_assigned_samples) == 0) return()
      
      poços_prefix_to_remove <- paste0(fname, "_", input$selected_wells)
      all_data$files[[fname]]$wells <- current_assigned_samples[!current_assigned_samples$Poço_Prefixo %in% poços_prefix_to_remove, ]
      
      runjs(sprintf("var wells_to_clear = %s; wells_to_clear.forEach(function(w) { $(\".well-cell[data-well='\"+w+\"']\").removeClass('selected'); }); Shiny.setInputValue('%s', [], {priority: 'event'});", 
                    jsonlite::toJSON(input$selected_wells, auto_unbox = TRUE), ns("selected_wells")))
      showNotification("Atribuição removida.", type = "message")
    })
    
    output$plate_ui <- renderUI({  
      req(current_file(), current_file() != "NA")
      fname <- current_file()
      req(fname %in% names(all_data$files))
      
      vals_para_placa <- plate_vals()
      req(vals_para_placa)
      
      std_config_df <- standards_config()
      std_lookup_table <- if (nrow(std_config_df) > 0 && "wells" %in% names(std_config_df)) {
        std_config_df %>%
          dplyr::select(concentration, wells) %>% 
          tidyr::unnest(cols = c(wells))
      } else {
        data.frame(concentration=numeric(), wells=character())
      }
      
      std_wells_on_plate <- current_standard_wells_list_all() 
      ignored_stds_this_plate <- ignored_standard_wells_all_plates()[[fname]] %||% character(0) 
      
      plate_layout_matrix <- well_names()
      assigned_samples_df <- all_data$files[[fname]]$wells
      
      html_table_placa <- "<table class='well-plate'>"
      for (r_idx in 1:nrow(plate_layout_matrix)) {
        html_table_placa <- paste0(html_table_placa, "<tr>")
        for (c_idx in 1:ncol(plate_layout_matrix)) {
          well_id_atual <- plate_layout_matrix[r_idx, c_idx]
          well_value_atual <- vals_para_placa[well_id_atual]
          well_value_display_atual <- if(is.na(well_value_atual)) "_" else as.character(round(well_value_atual, 3))
          
          cell_class_atual <- "well-cell"
          is_standard <- well_id_atual %in% std_wells_on_plate
          is_ignored <- well_id_atual %in% ignored_stds_this_plate
          
          if(is_standard){
            cell_class_atual <- paste(cell_class_atual, "standard-well non-selectable-standard") 
            if(is_ignored) cell_class_atual <- paste(cell_class_atual, "standard-ignored")
          }
          
          cell_content <- sprintf("%s<br><small>%s</small>", well_id_atual, well_value_display_atual)
          
          if(!is_standard && !is.null(assigned_samples_df) && nrow(assigned_samples_df) > 0){ 
            idx_match <- match(paste0(fname, "_", well_id_atual), assigned_samples_df$Poço_Prefixo)
            if(!is.na(idx_match)) { 
              sample_name_display <- assigned_samples_df[idx_match, ]$Amostra[1]
              cell_content <- sprintf("%s<br><small>%s</small><br><small><i>%s</i></small>", well_id_atual, well_value_display_atual, htmltools::htmlEscape(sample_name_display))
            }
          } else if (is_standard) {
            conc_value <- std_lookup_table$concentration[std_lookup_table$wells == well_id_atual]
            conc_display <- if (length(conc_value) == 1) sprintf("%.3f", conc_value) else "Padrão"
            
            label <- if(is_ignored) sprintf("<s>%s</s>", conc_display) else sprintf("<b>%s</b>", conc_display)
            cell_content <- sprintf("%s<br><small>%s</small><br><small>%s</small>", well_id_atual, well_value_display_atual, label)
          }
          
          html_table_placa <- paste0(html_table_placa, sprintf("<td class='%s' data-well='%s'>%s</td>", cell_class_atual, well_id_atual, cell_content))
        }
        html_table_placa <- paste0(html_table_placa, "</tr>")
      }
      html_table_placa <- paste0(html_table_placa, "</table>"); HTML(html_table_placa)
    })
    
    output$save_state <- downloadHandler(  
      filename = function() { paste0("Projeto_BCA_", Sys.Date(), ".rds") },
      content = function(file) {
        fname <- current_file()
        if (!is.null(fname) && fname != "NA") {
          all_data$files[[fname]]$standards_config <- standards_config()
        }
        bca_state_to_save <- list(
          files = all_data$files,
          ignored_stds_saved = ignored_standard_wells_all_plates() 
        )
        saveRDS(bca_state_to_save, file)
      }
    )
    
    observeEvent(input$load_state, {  
      req(input$load_state)
      estado_carregado <- tryCatch(readRDS(input$load_state$datapath), error = function(e) { NULL })
      req(estado_carregado, is.list(estado_carregado), "files" %in% names(estado_carregado))
      
      all_data$files <- estado_carregado$files
      
      if(!is.null(estado_carregado$ignored_stds_saved) && is.list(estado_carregado$ignored_stds_saved)){
        ignored_standard_wells_all_plates(estado_carregado$ignored_stds_saved)
      } else {
        ignored_standard_wells_all_plates(list()) 
      }
      
      placas_carregadas <- names(all_data$files)
      if (length(placas_carregadas) > 0) {
        updateSelectInput(session, "selected_plate", choices = placas_carregadas, selected = placas_carregadas[1])
        showNotification("Projeto BCA carregado.", type = "message")
      } else {
        updateSelectInput(session, "selected_plate", choices = list("Nenhuma Placa disponível" = "NA"), selected = "NA")
        showNotification("Arquivo RDS carregado, mas sem placas válidas.", type = "warning")
      }
    })
  }) 
}