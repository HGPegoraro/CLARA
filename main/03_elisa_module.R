library(shiny)
library(shinyjs)
library(readxl)
library(DT)
library(ggplot2)
library(ggpattern) 
library(dplyr)
library(emmeans)
library(multcomp)
library(multcompView)
library(jsonlite)
library(sortable)
library(ggtext)
library(commonmark) 
library(shinyWidgets)
library(digest) 
library(fBasics) 
library(afex) 
library(rstatix)
library(dunn.test)

elisa_tabPanel <- function(id, name = "Resposta Humoral") {
  ns <- NS(id)
  
  selected_wells_input_id_elisa <- ns("selected_wells") 
  
  estilo_js_elisa <- sprintf("
    (function() { 
      let isMouseDown_elisa = false, isSelecting_elisa = true;
      const targetInputId_elisa = '%s';

      $(document).on('mousedown', '.elisa-module-tab .well-cell', function(e) {
        e.preventDefault(); isMouseDown_elisa = true;
        isSelecting_elisa = !$(this).hasClass('selected');
        $(this).toggleClass('selected', isSelecting_elisa); return false;
      }).on('mouseover', '.elisa-module-tab .well-cell', function() {
        if (isMouseDown_elisa) $(this).toggleClass('selected', isSelecting_elisa);
      });

      $(document).on('mouseup', function(e) {
        if (isMouseDown_elisa) {
            isMouseDown_elisa = false;
            let selected_elisa = [];
            $('.elisa-module-tab .well-cell.selected').each(function() {
              selected_elisa.push($(this).data('well'));
            });
            Shiny.setInputValue(targetInputId_elisa, selected_elisa, {priority: 'event'});
        }
      }).on('mouseleave', '.elisa-module-tab', function () { 
          if(isMouseDown_elisa) {
            isMouseDown_elisa = false;
          }
      });
    })(); 
  ", selected_wells_input_id_elisa)
  
  tabPanel(name, class = "elisa-module-tab", 
           useShinyjs(),
           tags$head(
             tags$style(HTML("
        .elisa-module-tab .well-plate { border-collapse: collapse; margin: 10px 0; }
        .elisa-module-tab .well-cell {
          border: 1px solid #ccc; 
          width: 65px; 
          height: 60px; 
          text-align: center; 
          vertical-align: middle;
          font-size: 11px; 
          font-weight: bold; 
          user-select: none;
          padding: 2px; 
          white-space: normal; 
        }
        .elisa-module-tab .well-cell small { 
          font-size: 9px; 
          font-weight: normal; 
        }
        .elisa-module-tab .selected { background-color: lightblue !important; }
        .elisa-module-tab .blank { background-color: lightgray !important; }
        .elisa-module-tab table.well-plate td { cursor: pointer; }
      ")),
             tags$script(HTML(estilo_js_elisa))
           ),
           sidebarLayout(
             sidebarPanel(
               fileInput(ns("files"), "Importar Excel", multiple = TRUE, accept = ".xlsx"),
               selectInput(ns("selected_plate"), "Selecionar Placa", choices = list("Nenhuma Placa disponível" = "NA")),
               hr(),
               radioButtons(ns("replica_mode"), "Modo de Réplica:", 
                            choices = list("Duplicata" = "duo", "Triplicata" = "trio"), 
                            selected = "duo"),
               hr(),
               tags$b("Configuração dos Dias"),
               textInput(ns("dias_config"), label = NULL, placeholder = "Ex: Dia 0, Dia 21, Pós-desafio"),
               actionButton(ns("update_dias"), "Atualizar Lista de Dias", icon = icon("sync")),
               uiOutput(ns("dias_radio_ui")),
               hr(),
               textInput(ns("test_type"), "Nome do Teste", placeholder = "Ex: IgG Total"),
               textInput(ns("group_name"), "Grupo", placeholder = "Ex: Grupo A"),
               actionButton(ns("add_group"), "Adicionar Grupo"),
               actionButton(ns("set_blank"), "Marcar como Branco"),
               actionButton(ns("remove_from_group"), "Remover do Grupo Selecionado", icon = icon("eraser")),
               hr(),
               fluidRow(
                 column(8, fileInput(ns("load_state"), "Carregar Projeto Salvo", accept = ".rds")),
                 column(4, downloadButton(ns("save_state"), "Salvar Projeto"))
               )
             ),
             mainPanel(
               h4(strong("Placa de 96 poços")),
               uiOutput(ns("plate_ui")),
               DTOutput(ns("group_table")),
               fluidRow(
                 h4(strong("Gráfico Dinâmico")),
                 column(10, 
                        plotOutput(ns("grafico_dinamico"))
                 ),
                 column(1, 
                        tags$div(style = "padding-top: 20px;", 
                                 numericInput(ns("ymax_dynamic_input"), "Escala", value = NA_real_, min = 0.01, step = 0.1, width = "100px")
                        )
                 )
               ),
               hr(),
               br(),
               h4(strong("Gráfico Final")),
               uiOutput(ns("graficos_ui")), 
               fluidRow(
                 column(6,
                        uiOutput(ns("filtro_teste_ui")),
                        textInput(ns("grafico_nome"), "Nome do Gráfico", placeholder = "Nome do Gráfico"),
                        actionButton(ns("adicionar_grafico"), "Adicionar Gráfico"),
                        hr(),
                        shinyWidgets::pickerInput(
                          inputId = ns("plot_groups"), label = "Selecionar Grupos para Gráfico",
                          choices = list("Nenhum grupo disponível" = "NA"),
                          selected = NULL, multiple = TRUE,
                          options = list(`live-search` = TRUE), width = '100%'
                        )
                 ),
                 column(6,
                        selectInput(ns("grafico_selecionado"), "Selecionar Gráfico para Exibição", choices = list("Nenhum gráfico disponível" = "NA")),
                        actionButton(ns("remover_grafico"), "Remover Gráfico Selecionado", icon = icon("trash"), class = "btn-danger"),
                        uiOutput(ns("group_order_ui")),
                        hr(),
                        radioButtons(ns("analysis_method"), "Método de Análise Estatística:",
                                     choices = list("Análise Integrada (ANOVA de Duas Vias)" = "mixed_anova",
                                                    "Análise por Dia (ANOVA de Uma Via)" = "anova_per_day",
                                                    "Análise por Dia (Kruskal-Wallis)" = "kruskal_per_day"
                                     ),
                                     selected = "mixed_anova"),
                        hr(),
                        radioButtons(ns("signif_style"), "Estilo de Significância:",
                                     choices = list("Letras (Tukey/Dunn)" = "letters",
                                                    "Asteriscos e Barras" = "asterisks"),
                                     selected = "letters"),
                        hr(),
                        h4(strong("Diagnóstico de Normalidade")),
                        uiOutput(ns("normality_controls_ui"))
                 )
               ),
               numericInput(ns("plot_width"), "Largura (pixels)", value = 1200, min = 1000),
               numericInput(ns("plot_height"), "Altura (pixels)", value = 600, min = 500),
               downloadButton(ns("download_plot_png"), "Baixar Gráfico (PNG)"),
               downloadButton(ns("download_plot_pdf"), "Baixar Gráfico (PDF)"),
               downloadButton(ns("download_plot_tiff"), "Baixar Gráfico (TIFF)")
             )
           )
  )
}

elisa_server <- function(id, global_excel_format_reactive) { 
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    well_names_fixed <- function() {
      rows <- LETTERS[1:8]
      cols <- 1:12
      return(outer(rows, cols, paste0))
    }
    
    all_data <- reactiveValues(files = list(), export_data = NULL)
    group_colors <- reactiveVal(list()) 
    graficos_config <- reactiveVal(list())
    data_for_normality_check <- reactiveVal(NULL)
    
    dias_analise <- reactiveVal(c("Dia 0", "Dia 21", "Dia 42"))
    
    output$dias_radio_ui <- renderUI({
      dias <- dias_analise()
      req(dias)
      radioButtons(ns("selected_day"), "Selecione o Dia para Atribuição:", 
                   choices = dias, 
                   selected = dias[1])
    })
    
    observe({
      updateTextInput(session, "dias_config", value = paste(dias_analise(), collapse = ", "))
    })
    
    observeEvent(input$update_dias, {
      req(input$dias_config)
      novos_dias_bruto <- strsplit(input$dias_config, ",")[[1]]
      novos_dias_limpo <- trimws(novos_dias_bruto)
      novos_dias_final <- novos_dias_limpo[novos_dias_limpo != ""]
      
      if (length(novos_dias_final) > 0) {
        dias_analise(novos_dias_final)
        showNotification("Lista de dias atualizada com sucesso.", type = "message")
      } else {
        showNotification("A lista de dias não pode estar vazia.", type = "error")
      }
    })
    
    grafico_dinamico_params <- reactiveVal(list(grupos = NULL, ordem = NULL, teste = NULL, ymax = NULL, replica_mode = "duo", analysis_method = "mixed_anova", signif_style = "letters"))
    
    italicize_markdown <- function(text) {
      sapply(text, function(t) {
        if (is.null(t) || is.na(t)) return(NA_character_)
        if (grepl("\\*(.*?)\\*", t)) {
          gsub("\\*(.*?)\\*", "<em>\\1</em>", t)
        } else {
          as.character(t) 
        }
      }, USE.NAMES = FALSE)
    }
    
    nomes_dos_grupos <- function() {
      todos <- do.call(rbind, lapply(all_data$files, function(f) f$wells))
      if (is.null(todos) || nrow(todos) == 0) return(character(0))
      grupos <- todos$Grupo
      grupos <- grupos[!is.na(grupos) & grupos != ""]
      unique(grupos)
    }
    
    nomes_dos_testes <- function() {
      todos <- do.call(rbind, lapply(all_data$files, function(f) f$wells))
      if (is.null(todos) || nrow(todos) == 0) return(character(0))
      testes <- todos$Teste
      testes <- testes[!is.na(testes) & testes != ""]
      unique(testes)
    }
    
    remover_html <- function(text) {
      if(is.null(text)) return(NULL)
      sapply(text, function(t) if(is.null(t) || is.na(t)) NA_character_ else gsub("<[^>]+>", "", t), USE.NAMES = FALSE)
    }
    
    remover_asteriscos <- function(text) {
      if(is.null(text)) return(NULL)
      sapply(text, function(t) if(is.null(t) || is.na(t)) NA_character_ else gsub("\\*(.*?)\\*", "\\1", t), USE.NAMES = FALSE)
    }
    
    observeEvent(input$remover_grafico, {
      req(input$grafico_selecionado, input$grafico_selecionado != "NA")
      confs <- graficos_config()
      confs[[input$grafico_selecionado]] <- NULL
      graficos_config(confs)
      if (length(confs) > 0) {
        novo_id_selecionado <- names(confs)[1]
        updateSelectInput(session, "grafico_selecionado", 
                          choices = setNames(sapply(confs, function(x) x$id), lapply(sapply(confs, function(x) x$nome), function(name) HTML(italicize_markdown(name)))),
                          selected = novo_id_selecionado)
      } else {
        updateSelectInput(session, "grafico_selecionado", choices = list("Nenhum gráfico disponível" = "NA"), selected = "NA")
      }
      showNotification("Gráfico removido com sucesso.", type = "message")
    })
    
    output$filtro_teste_ui <- renderUI({
      testes <- nomes_dos_testes()
      if (length(testes) == 0) return(NULL)
      selectInput(ns("filtro_teste"), "Filtrar por Tipo de Teste", choices = c("Todos", testes), selected = "Todos")
    })
    
    output$save_state <- downloadHandler(
      filename = function() { paste0("Projeto_", Sys.Date(), ".rds") },
      content = function(file) {
        estado <- list(
          files = all_data$files,    
          cores = group_colors(),
          ordem = input$group_order, 
          graficos = graficos_config(),
          replica_mode_saved = input$replica_mode,
          dias = dias_analise(),
          analysis_method_saved = input$analysis_method,
          signif_style_saved = input$signif_style
        )
        saveRDS(estado, file)
      }
    )
    
    safe_update_plot_groups <- function(session, choices_list_picker, selected_values_picker, pure_group_names_available) {
      if (is.null(choices_list_picker) || length(choices_list_picker) == 0) {
        shinyWidgets::updatePickerInput(session, "plot_groups", choices = list("Nenhum grupo disponível" = "NA"), selected = character(0))
        return()
      }
      selected_valid <- intersect(selected_values_picker, pure_group_names_available)
      shinyWidgets::updatePickerInput(session, "plot_groups", choices = choices_list_picker, choicesOpt = list(content = names(choices_list_picker)), selected = selected_valid)
    }
    
    plot_groups_choices_val <- reactiveVal(NULL) 
    
    atualiza_plot_groups <- function() {
      grupos_orig_names <- nomes_dos_grupos() 
      grupos_pure_names <- remover_asteriscos(remover_html(grupos_orig_names)) 
      selected_currently_pure <- input$plot_groups 
      if (length(grupos_pure_names) == 0) {
        safe_update_plot_groups(session, NULL, NULL, character(0)); return()
      }
      labels_for_display <- lapply(italicize_markdown(grupos_orig_names), HTML)
      choices_list_new <- stats::setNames(grupos_pure_names, labels_for_display)
      prev_choices_val <- plot_groups_choices_val()
      if (!identical(sort(names(prev_choices_val)), sort(names(choices_list_new))) ||
          !identical(sort(unname(prev_choices_val)), sort(unname(choices_list_new))) ) {
        plot_groups_choices_val(choices_list_new)
        safe_update_plot_groups(session, choices_list_new, selected_currently_pure, grupos_pure_names)
      }
    }
    
    assigned_well_colors_keys <- reactiveVal(list()) 
    
    get_new_well_color <- function(key) {
      current_assigned <- assigned_well_colors_keys()
      h <- runif(1, 0, 1)    
      s <- runif(1, 0.25, 0.45) 
      v <- runif(1, 0.9, 0.98) 
      new_color <- hsv(h,s,v)
      max_tries <- 30 
      tries <- 0
      while(new_color %in% unname(unlist(current_assigned)) && tries < max_tries){
        h <- runif(1, 0, 1)
        s <- runif(1, 0.25, 0.45) 
        v <- runif(1, 0.9, 0.98)
        new_color <- hsv(h,s,v)
        tries <- tries + 1
      }
      current_assigned[[key]] <- new_color 
      assigned_well_colors_keys(current_assigned) 
      return(new_color)
    }
    
    observe({ if (length(all_data$files) > 0) { atualiza_plot_groups() } else { safe_update_plot_groups(session, NULL, NULL, character(0)) } })
    
    current_file <- reactive({ input$selected_plate })
    
    plate_vals <- reactive({
      req(current_file(), current_file() != "NA")
      if (!current_file() %in% names(all_data$files) || is.null(all_data$files[[current_file()]])) return(NULL)
      all_data$files[[current_file()]]$vals
    })
    
    observeEvent(input$files, {
      req(input$files)
      current_global_format_id <- global_excel_format_reactive() 
      req(current_global_format_id) 
      
      loaded_files_temp <- list()
      
      reader_function <- GLOBAL_EXCEL_READERS[[current_global_format_id]]
      if (is.null(reader_function)) {
        showNotification(paste("Formato de leitor de Excel global ('", current_global_format_id, "') é inválido."), type = "error", duration=7)
        return()
      }
      
      withProgress(message = "Carregando arquivos Excel...", value = 0, {
        for (i in seq_len(nrow(input$files))) {
          incProgress(1 / nrow(input$files), detail = input$files$name[i])
          filepath <- input$files$datapath[i]
          original_fname <- input$files$name[i]
          
          plate_values_named_vector <- reader_function(filepath)
          
          if (is.null(plate_values_named_vector)) {
            showNotification(paste("Falha ao processar (ELISA):", original_fname), type = "error")
            next 
          }
          
          fname_sanitized <- gsub("[^A-Za-z0-9_.-]", "_", original_fname)
          loaded_files_temp[[fname_sanitized]] <- list(
            vals = plate_values_named_vector,
            wells = data.frame(Poço=character(), Valor=numeric(), Grupo=character(), Dia=character(), Teste=character(), Graficos=character(), stringsAsFactors=FALSE),
            blank = character()
          )
        }
      })
      
      if (length(loaded_files_temp) > 0) {
        for(fname in names(loaded_files_temp)) { 
          all_data$files[[fname]] <- loaded_files_temp[[fname]] 
        }
        
        placas_disponiveis_modulo <- names(all_data$files)
        current_selected_plate_elisa <- input$selected_plate
        
        new_selected_plate_elisa <- if (!is.null(current_selected_plate_elisa) && current_selected_plate_elisa %in% placas_disponiveis_modulo) {
          current_selected_plate_elisa
        } else if (length(placas_disponiveis_modulo) > 0) {
          names(loaded_files_temp)[1]
        } else { "NA" }
        
        updateSelectInput(session, "selected_plate", 
                          choices = if(length(placas_disponiveis_modulo) > 0) setNames(placas_disponiveis_modulo, placas_disponiveis_modulo) else list("Nenhuma Placa disponível" = "NA"), 
                          selected = new_selected_plate_elisa)
      }
    })
    
    observeEvent(input$selected_plate, {}, ignoreInit = TRUE, ignoreNULL = TRUE) 
    
    observeEvent(input$add_group, {
      req(input$group_name, input$selected_wells, current_file(), current_file() != "NA", input$selected_day)
      if (trimws(input$group_name) == "") { showNotification("Insira um nome válido.", type = "error"); return() }
      fname <- current_file(); vals_current_plate <- plate_vals()
      if(is.null(vals_current_plate)) { showNotification("Valores da placa não carregados.", type="error"); return() }
      dia_selecionado <- input$selected_day
      valid_wells_selected <- input$selected_wells[input$selected_wells %in% names(vals_current_plate)]
      if (length(valid_wells_selected) == 0) { showNotification("Nenhum poço válido selecionado.", type = "warning"); return() }
      new_rows_to_add <- data.frame(Poço = paste0(fname, "_", valid_wells_selected), Valor = vals_current_plate[valid_wells_selected],
                                    Grupo = input$group_name, Dia = dia_selecionado, Teste = input$test_type, Graficos = NA_character_, stringsAsFactors = FALSE )
      existing_wells_df_for_plate <- all_data$files[[fname]]$wells
      existing_wells_df_for_plate <- existing_wells_df_for_plate[!existing_wells_df_for_plate$Poço %in% new_rows_to_add$Poço,]
      all_data$files[[fname]]$wells <- rbind(existing_wells_df_for_plate, new_rows_to_add)
      
      pure_group_name_for_color <- remover_asteriscos(remover_html(input$group_name))
      chave_cor_nova <- paste(pure_group_name_for_color, input$selected_day, sep = "_") 
      
      current_group_cols <- group_colors() 
      if (!(chave_cor_nova %in% names(current_group_cols))) { 
        current_group_cols[[chave_cor_nova]] <- get_new_well_color(chave_cor_nova) 
        group_colors(current_group_cols) 
      }
      
      runjs(sprintf("$('.well-cell[data-well]').removeClass('selected'); Shiny.setInputValue('%s', [], {priority: 'event'});", ns("selected_wells")))
      atualiza_plot_groups(); showNotification(paste("Grupo", input$group_name, "adicionado."), type = "message")
    })
    
    output$group_order_ui <- renderUI({
      todos_pocos_combinados <- do.call(rbind, lapply(all_data$files, function(f) f$wells))
      if (is.null(todos_pocos_combinados) || nrow(todos_pocos_combinados) == 0) return(NULL)
      grupos_unicos_originais <- unique(todos_pocos_combinados$Grupo)
      grupos_unicos_originais <- grupos_unicos_originais[!is.na(grupos_unicos_originais) & grupos_unicos_originais != ""]
      if (length(grupos_unicos_originais) == 0) return(NULL)
      final_labels_for_bucket <- sort(grupos_unicos_originais)
      bucket_list(header = "Arraste os grupos para definir a ordem no gráfico:", group_name = ns("plot_groups_bucket_shared"), orientation = "horizontal",
                  add_rank_list(text = NULL, labels = lapply(italicize_markdown(final_labels_for_bucket), HTML), input_id = ns("group_order")))
    })
    
    observeEvent(input$remove_from_group, {
      req(input$selected_wells, current_file(), current_file() != "NA"); fname <- current_file()
      current_wells_df <- all_data$files[[fname]]$wells
      if (is.null(current_wells_df) || nrow(current_wells_df) == 0) return()
      poços_prefixados_para_remover_from_groups <- paste0(fname, "_", input$selected_wells)
      all_data$files[[fname]]$wells <- current_wells_df[!(current_wells_df$Poço %in% poços_prefixados_para_remover_from_groups),]
      all_data$files[[fname]]$blank <- setdiff(all_data$files[[fname]]$blank, input$selected_wells)
      runjs(sprintf("var wells = %s; wells.forEach(w => $(\".elisa-module-tab .well-cell[data-well='\"+w+\"']\").removeClass('selected blank').css('background-color', '')); Shiny.setInputValue('%s', [], {priority: 'event'});", 
                    jsonlite::toJSON(input$selected_wells, auto_unbox = TRUE), ns("selected_wells")))
      atualiza_plot_groups(); showNotification("Poços removidos.", type = "message")
    })
    
    observeEvent(input$set_blank, {
      req(input$selected_wells, current_file(), current_file() != "NA"); fname <- current_file()
      all_data$files[[fname]]$blank <- unique(c(all_data$files[[fname]]$blank, input$selected_wells))
      current_wells_df <- all_data$files[[fname]]$wells
      if (!is.null(current_wells_df) && nrow(current_wells_df) > 0) {
        poços_prefixados_blanked <- paste0(fname, "_", input$selected_wells)
        all_data$files[[fname]]$wells <- current_wells_df[!current_wells_df$Poço %in% poços_prefixados_blanked,]
      }
      runjs(sprintf("var wells = %s; wells.forEach(w => $(\".elisa-module-tab .well-cell[data-well='\"+w+\"']\").removeClass('selected').addClass('blank').css('background-color', '')); Shiny.setInputValue('%s', [], {priority: 'event'});", 
                    jsonlite::toJSON(input$selected_wells, auto_unbox = TRUE), ns("selected_wells")))
      atualiza_plot_groups(); showNotification("Poços marcados como branco.", type = "message")
    })
    
    observeEvent(input$group_order, { req(input$group_order); runjs(sprintf("Shiny.setInputValue('%s', new Date().getTime());", ns("plot_trigger"))) }, ignoreNULL = TRUE, ignoreInit = TRUE) 
    
    output$group_table <- renderDT({
      todos_combinados_df <- do.call(rbind, lapply(all_data$files, function(f) f$wells))
      if (is.null(todos_combinados_df) || nrow(todos_combinados_df) == 0) return(NULL)
      display_df_group_table <- todos_combinados_df
      display_df_group_table$Grupo <- sapply(display_df_group_table$Grupo, function(g) { if (is.na(g)) NA_character_ else italicize_markdown(g) })
      datatable(display_df_group_table, escape = FALSE, options = list(pageLength = 5, scrollX=TRUE))
    })
    
    current_dynamic_plot_parameters <- reactive({
      
      input$plot_trigger
      
      
      list(
        grupos = input$plot_groups, 
        ordem = input$group_order, 
        teste = if (!is.null(input$filtro_teste) && input$filtro_teste != "Todos") input$filtro_teste else NULL, 
        ymax = if (is.na(input$ymax_dynamic_input) || !is.numeric(input$ymax_dynamic_input) || input$ymax_dynamic_input <= 0) NULL else input$ymax_dynamic_input, 
        replica_mode = input$replica_mode, 
        analysis_method = input$analysis_method,
        signif_style = input$signif_style
      )
    })
    
    observe({
      params <- current_dynamic_plot_parameters()
      grafico_dinamico_params(params)
    })
    
    observeEvent(input$adicionar_grafico, {
      req(input$grafico_nome); if (trimws(input$grafico_nome) == "") { showNotification("Insira um nome para o gráfico.", type = "error"); return() }
      params_to_save <- grafico_dinamico_params() 
      req(params_to_save$grupos, length(params_to_save$grupos) > 0, msg = "Selecione grupos para o gráfico.")
      grafico_id_new <- paste0("grafico_", digest::digest(list(input$grafico_nome, Sys.time(), runif(1)), algo = "crc32"))
      nova_config_grafico <- list(
        nome = input$grafico_nome, id = grafico_id_new, grupos = params_to_save$grupos, ordem = params_to_save$ordem, 
        teste = params_to_save$teste, ymax = params_to_save$ymax, replica_mode = params_to_save$replica_mode, 
        dias_ordem = dias_analise(),
        analysis_method = params_to_save$analysis_method,
        signif_style = params_to_save$signif_style
      )
      confs_atuais <- graficos_config(); confs_atuais[[grafico_id_new]] <- nova_config_grafico; graficos_config(confs_atuais)
      updateSelectInput(session, "grafico_selecionado",
                        choices = setNames(sapply(confs_atuais, function(x) x$id), lapply(sapply(confs_atuais, function(x) x$nome), function(name) HTML(italicize_markdown(name)))),
                        selected = grafico_id_new )
      showNotification(paste("Gráfico", input$grafico_nome, "adicionado!"), type="message"); updateTextInput(session, "grafico_nome", value="")
    })
    
    output$graficos_ui <- renderUI({ req(input$grafico_selecionado, input$grafico_selecionado != "NA"); plotOutput(outputId = ns(input$grafico_selecionado), height = "400px") })
    
    observeEvent(input$load_state, {
      req(input$load_state)
      estado_carregado <- tryCatch(readRDS(input$load_state$datapath), error = function(e) { showNotification(paste("Erro ao ler RDS:", e$message), type="error"); NULL })
      
      expected_fields <- c("files", "cores", "ordem", "graficos") 
      if(is.null(estado_carregado) || !is.list(estado_carregado) || !all(expected_fields %in% names(estado_carregado))) {
        showNotification("Arquivo RDS inválido ou incompleto.", type="error"); return()
      }
      
      if (!is.null(estado_carregado$dias) && is.character(estado_carregado$dias) && length(estado_carregado$dias) > 0) {
        dias_analise(estado_carregado$dias)
      } else { dias_analise(c("Dia 0", "Dia 21", "Dia 42")) }
      
      loaded_replica_mode <- estado_carregado$replica_mode_saved %||% "duo"
      if (!loaded_replica_mode %in% c("duo", "trio")) { loaded_replica_mode <- "duo" }
      
      loaded_analysis_method <- estado_carregado$analysis_method_saved %||% "mixed_anova"
      if (!loaded_analysis_method %in% c("kruskal_per_day", "anova_per_day", "mixed_anova")) { loaded_analysis_method <- "mixed_anova" }
      
      loaded_signif_style <- estado_carregado$signif_style_saved %||% "letters"
      if (!loaded_signif_style %in% c("letters", "asterisks")) { loaded_signif_style <- "letters" }
      
      expected_wells_cols <- c("Poço"="character", "Valor"="numeric", "Grupo"="character", "Dia"="character", "Teste"="character", "Graficos"="character")
      if (!is.null(estado_carregado$files)) {
        for (fname_loaded in names(estado_carregado$files)) {
          if (!is.null(estado_carregado$files[[fname_loaded]]$wells)) {
            current_wells_df <- estado_carregado$files[[fname_loaded]]$wells
            for (col_name in names(expected_wells_cols)) {
              if (!(col_name %in% colnames(current_wells_df))) {
                current_wells_df[[col_name]] <- switch(expected_wells_cols[col_name], "character" = NA_character_, "numeric" = NA_real_, NULL) 
              }
            }
            estado_carregado$files[[fname_loaded]]$wells <- current_wells_df[, names(expected_wells_cols), drop = FALSE]
          }
        }
      }
      all_data$files <- estado_carregado$files 
      
      if(!is.null(estado_carregado$cores) && is.list(estado_carregado$cores)) {
        group_colors(estado_carregado$cores)
        assigned_well_colors_keys(estado_carregado$cores) 
      } else { group_colors(list()); assigned_well_colors_keys(list()) }
      
      cleaned_graficos_config <- list()
      if (!is.null(estado_carregado$graficos) && is.list(estado_carregado$graficos)) {
        for (graf_item_id in names(estado_carregado$graficos)) {
          graf_item <- estado_carregado$graficos[[graf_item_id]]
          if (is.list(graf_item) && !is.null(graf_item$id) && !is.na(graf_item$id) && !is.null(graf_item$nome)) {
            graf_item$ymax <- graf_item$ymax %||% NULL
            graf_item$replica_mode <- graf_item$replica_mode %||% loaded_replica_mode
            graf_item$dias_ordem <- graf_item$dias_ordem %||% c("Dia 0", "Dia 21", "Dia 42") 
            graf_item$analysis_method <- graf_item$analysis_method %||% loaded_analysis_method
            graf_item$signif_style <- graf_item$signif_style %||% loaded_signif_style
            cleaned_graficos_config[[graf_item$id]] <- graf_item
          }
        }
      }
      graficos_config(cleaned_graficos_config) 
      
      updateRadioButtons(session, "replica_mode", selected = loaded_replica_mode)
      updateRadioButtons(session, "analysis_method", selected = loaded_analysis_method)
      updateRadioButtons(session, "signif_style", selected = loaded_signif_style)
      
      placas_carregadas <- if(!is.null(estado_carregado$files)) names(estado_carregado$files) else character(0)
      updateSelectInput(session, "selected_plate", choices = if(length(placas_carregadas) > 0) placas_carregadas else list("Nenhuma Placa disponível" = "NA"),
                        selected = if(length(placas_carregadas) > 0) placas_carregadas[1] else "NA")
      
      confs_graficos_carregados <- graficos_config() 
      if (!is.null(confs_graficos_carregados) && length(confs_graficos_carregados) > 0) {
        updateSelectInput(session, "grafico_selecionado", choices = setNames(sapply(confs_graficos_carregados, function(x) x$id), lapply(sapply(confs_graficos_carregados, function(x) x$nome), function(name) HTML(italicize_markdown(name)))),
                          selected = names(confs_graficos_carregados)[1])
      } else { updateSelectInput(session, "grafico_selecionado", choices = list("Nenhum gráfico disponível" = "NA"), selected = "NA") }
      
      atualiza_plot_groups()
      delay(500, runjs(sprintf("Shiny.setInputValue('%s', new Date().getTime());", ns("plot_trigger")))); 
      showNotification("Projeto carregado.", type = "message")
    })
    
    observe({ 
      confs_para_plotar <- graficos_config()
      for (graf_id_curto in names(confs_para_plotar)) {
        local({ 
          current_graf_id_curto <- graf_id_curto; config_item_plot <- confs_para_plotar[[current_graf_id_curto]]
          output[[current_graf_id_curto]] <- renderPlot({
            p_desenhado <- gerar_plot_personalizado(
              grupos_pure_names = config_item_plot$grupos, ordem_orig_names = config_item_plot$ordem,   
              titulo = config_item_plot$nome, teste_filtro = config_item_plot$teste,
              custom_ymax = config_item_plot$ymax, custom_replica_mode = config_item_plot$replica_mode %||% "duo",
              dias_ordem_param = config_item_plot$dias_ordem %||% dias_analise(),
              analysis_method_param = config_item_plot$analysis_method %||% "mixed_anova",
              signif_style_param = config_item_plot$signif_style %||% "letters"
            )
            if (is.null(p_desenhado)) { plot.new(); text(0.5, 0.5, "Nenhum dado para plotar.", cex=1.2) } else { print(p_desenhado) }
          }, height = 400) 
        })
      }
    }) 
    
    output$plate_ui <- renderUI({
      req(current_file(), current_file() != "NA")
      fname <- current_file() 
      if (!fname %in% names(all_data$files) || is.null(all_data$files[[fname]])) return(p("Dados não encontrados.")) 
      vals_para_placa <- plate_vals(); 
      if(is.null(vals_para_placa)) return(p("Valores da placa não disponíveis."))
      
      plate_layout_matrix <- well_names_fixed(); 
      blank_wells_para_placa <- all_data$files[[fname]]$blank
      wells_info_df_para_placa <- all_data$files[[fname]]$wells
      current_group_colors_list <- group_colors()
      
      html_table_placa <- "<table class='well-plate'>"
      for (r_idx in 1:nrow(plate_layout_matrix)) {
        html_table_placa <- paste0(html_table_placa, "<tr>")
        for (c_idx in 1:ncol(plate_layout_matrix)) {
          well_id_atual <- plate_layout_matrix[r_idx, c_idx]
          well_value_atual <- vals_para_placa[well_id_atual] 
          well_value_display_atual <- if(is.na(well_value_atual)) "_" else as.character(round(well_value_atual, 3))
          cell_class_atual <- "well-cell"
          if (well_id_atual %in% blank_wells_para_placa) cell_class_atual <- paste(cell_class_atual, "blank") 
          
          bg_color_style_atual <- ""
          cell_content <- sprintf("%s<br><small>%s</small>", well_id_atual, well_value_display_atual) 
          
          prefixed_well_id_atual <- paste0(fname, "_", well_id_atual)
          well_group_info_row <- NULL
          if(!is.null(wells_info_df_para_placa) && nrow(wells_info_df_para_placa) > 0){
            idx_match <- match(prefixed_well_id_atual, wells_info_df_para_placa$Poço)
            if(!is.na(idx_match)) well_group_info_row <- wells_info_df_para_placa[idx_match, ]
          }
          
          if (!is.null(well_group_info_row) && nrow(well_group_info_row) == 1) {
            group_name_orig_atual <- well_group_info_row$Grupo[1]
            group_day_atual <- well_group_info_row$Dia[1]
            cell_content <- sprintf("%s<br><small>%s</small><br><small><i>%s (%s)</i></small>", 
                                    well_id_atual, well_value_display_atual,
                                    htmltools::htmlEscape(group_name_orig_atual), htmltools::htmlEscape(group_day_atual))
            
            pure_group_name_atual <- remover_asteriscos(remover_html(group_name_orig_atual))
            color_key_atual <- paste(pure_group_name_atual, group_day_atual, sep = "_")
            if (color_key_atual %in% names(current_group_colors_list)) { 
              bg_color_style_atual <- sprintf("style='background-color:%s;'", current_group_colors_list[[color_key_atual]]) 
            }
          }
          html_table_placa <- paste0(html_table_placa, sprintf("<td class='%s' data-well='%s' %s>%s</td>", 
                                                               cell_class_atual, well_id_atual, bg_color_style_atual, cell_content))
        }
        html_table_placa <- paste0(html_table_placa, "</tr>")
      }
      html_table_placa <- paste0(html_table_placa, "</table>"); HTML(html_table_placa)
    })
    
    internal_plot_generation <- function(all_well_data_input,
                                         selected_pure_group_names_input,
                                         group_order_orig_names_input,
                                         plot_title_input,
                                         test_type_filter_input,
                                         custom_ymax = NULL,
                                         replica_mode_param = "duo",
                                         dias_ordem_param,
                                         analysis_method_param = "mixed_anova",
                                         signif_style_param = "letters") {
      
      cat("\n\n=========================================================\n")
      cat("--- INICIANDO GERAÇÃO DE GRÁFICO:", plot_title_input, "---\n")
      cat("--- MÉTODO DE ANÁLISE:", toupper(analysis_method_param), "| ESTILO:", toupper(signif_style_param), "---\n")
      
      if (is.null(all_well_data_input) || nrow(all_well_data_input) == 0) return(NULL)
      if (is.null(selected_pure_group_names_input) || length(selected_pure_group_names_input) < 1) {
        return(NULL)
      }
      
      plot_data_filtered <- all_well_data_input
      if (!is.null(test_type_filter_input) && test_type_filter_input != "Todos" && test_type_filter_input != "") {
        plot_data_filtered <- plot_data_filtered %>% dplyr::filter(Teste == test_type_filter_input)
      }
      
      plot_data_processed_initial <- plot_data_filtered %>%
        mutate(GrupoPuro = remover_asteriscos(remover_html(Grupo))) %>%
        dplyr::filter(GrupoPuro %in% selected_pure_group_names_input)
      
      n_replicas_tecnicas <- ifelse(replica_mode_param == "trio", 3, 2)
      
      plot_data_observations <- plot_data_processed_initial %>%
        arrange(Poço) %>% 
        group_by(Grupo, Dia, Teste, GrupoPuro) %>% 
        mutate(replica_set_id = ceiling(row_number() / n_replicas_tecnicas)) %>%
        ungroup() %>%
        group_by(Grupo, Dia, Teste, GrupoPuro, replica_set_id) %>%
        summarise(ValorObservacao = mean(Valor, na.rm = TRUE), N_in_obs = n(), .groups = 'drop') %>%
        filter(N_in_obs >= n_replicas_tecnicas) %>%
        group_by(GrupoPuro) %>%
        mutate(ID_Animal = paste0(GrupoPuro, "_", replica_set_id)) %>%
        ungroup()
      
      if (nrow(plot_data_observations) == 0) {
        showNotification("Nenhuma observação biológica completa encontrada.", type = "warning", duration = 10)
        return(NULL)
      }
      
      final_order_levels <- selected_pure_group_names_input
      if (!is.null(group_order_orig_names_input) && length(group_order_orig_names_input) > 0) {
        ordered_pure_names <- remover_asteriscos(remover_html(group_order_orig_names_input))
        valid_ordered_pure_names <- ordered_pure_names[ordered_pure_names %in% selected_pure_group_names_input]
        if (length(valid_ordered_pure_names) > 0) {
          missing_from_order <- setdiff(selected_pure_group_names_input, valid_ordered_pure_names)
          final_order_levels <- c(valid_ordered_pure_names, missing_from_order)
        }
      } else {
        final_order_levels <- sort(selected_pure_group_names_input)
      }
      
      plot_data_for_analysis <- plot_data_observations %>%
        dplyr::select(ID_Animal, Grupo, Dia, Teste, GrupoPuro, Valor = ValorObservacao) %>%
        dplyr::filter(!is.na(Valor)) %>% 
        mutate(
          Dia = factor(Dia, levels = dias_ordem_param),
          GrupoPuro = factor(GrupoPuro, levels = final_order_levels)
        )
      
      stat.test <- NULL
      
      if (length(final_order_levels) > 1 && nrow(plot_data_for_analysis) > 0) {
        tryCatch({
          if (analysis_method_param == "mixed_anova") {
            
            model <- afex::mixed(Valor ~ GrupoPuro * Dia + (1|ID_Animal), data = plot_data_for_analysis)
            
            cat("\n--- TABELA DA ANOVA DE DUAS VIAS (Modelo Misto) ---\n"); print(knitr::kable(anova(model)))
            
            if (signif_style_param == "letters") {
              emm <- emmeans(model, pairwise ~ Dia * GrupoPuro)
              cld <- multcomp::cld(emm, Letters = letters, alpha = 0.05, adjust = "tukey")
              stat.test <- as.data.frame(cld) %>%
                dplyr::select(Dia, GrupoPuro, .group) %>% dplyr::rename(Signif = .group) %>%
                mutate(Signif = trimws(Signif))
            } else { 
              emm <- emmeans(model, pairwise ~ GrupoPuro | Dia)
              stat.test <- as.data.frame(summary(emm$contrasts)) %>%
                dplyr::select(Dia, contrast, p.value) %>%
                tidyr::separate(contrast, into = c("group1", "group2"), sep = " - ", remove = TRUE)
            }
            
          } else { 
            stat.test <- plot_data_for_analysis %>%
              group_by(Dia) %>%
              do(
                if (analysis_method_param == "anova_per_day") {
                  rstatix::tukey_hsd(aov(Valor ~ GrupoPuro, data = .))
                } else { 
                  dunn_result <- dunn.test::dunn.test(.$Valor, .$GrupoPuro, method = "bh", list = TRUE, alpha = 0.05)
                  data.frame(
                    group1 = sapply(strsplit(dunn_result$comparisons, " - "), `[`, 1),
                    group2 = sapply(strsplit(dunn_result$comparisons, " - "), `[`, 2),
                    p.adj = dunn_result$P.adjusted,
                    stringsAsFactors = FALSE
                  )
                }
              ) %>% ungroup()
          }
        }, error = function(e) {
          showNotification(paste("Falha na análise estatística:", e$message), type = "error", duration = 10)
          cat("\nERRO na Análise:", e$message, "\n")
        })
      }
      
      summary_stats_for_plot <- plot_data_for_analysis %>%
        group_by(Dia, GrupoPuro, Grupo) %>%
        summarise(MeanValue = mean(Valor, na.rm = TRUE), SDValue = sd(Valor, na.rm = TRUE), N_obs = n(), .groups = 'drop') %>%
        mutate(MeanValue = ifelse(is.na(MeanValue), 0, MeanValue), SDValue = ifelse(is.na(SDValue) | N_obs < 2, 0, SDValue),
               UpperError = MeanValue + SDValue) %>%
        mutate(
          Dia = factor(Dia, levels = dias_ordem_param),
          GrupoPuro = factor(GrupoPuro, levels = final_order_levels)
        )
      
      data_for_normality_check(plot_data_for_analysis)
      
      legend_labels_map_plot <- summary_stats_for_plot %>%
        distinct(GrupoPuro, Grupo) %>% arrange(factor(GrupoPuro, levels = final_order_levels)) %>%
        mutate(DisplayLabel = italicize_markdown(Grupo)) %>%
        dplyr::select(GrupoPuro, DisplayLabel) %>% tibble::deframe()
      
      dodge_width <- 0.9
      p_final <- ggplot(summary_stats_for_plot, aes(x = Dia, y = MeanValue, fill = GrupoPuro)) +
        geom_col(position = position_dodge(width = dodge_width), color = "black", width = 0.8) +
        geom_errorbar(aes(ymin = MeanValue, ymax = UpperError, group = GrupoPuro),
                      position = position_dodge(width = dodge_width), width = 0.25, na.rm = TRUE) +
        scale_fill_manual(name = "", values = setNames(grey.colors(length(final_order_levels), start = 0.9, end = 0.2), final_order_levels),
                          breaks = final_order_levels, labels = legend_labels_map_plot) +
        scale_y_continuous(limits = c(0, custom_ymax), expand = expansion(mult = c(0, 0.15))) +
        theme_classic(base_size = 14) +
        theme(plot.title = element_markdown(hjust = 0, size = 20), legend.position = "right", legend.text = element_markdown(size = 12),
              axis.text = element_text(size = 12, color = "black"), axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
              panel.grid.major.y = element_line(color = "grey85"), panel.grid.minor.y = element_line(color = "grey90"),
              panel.grid.major.x = element_line(color = "grey85", linetype = "dotted"), panel.grid.minor.x = element_line(color = "grey90", linetype = "dotted"))
      
      signif_caption <- if (signif_style_param == "asterisks" && !is.null(stat.test) && nrow(stat.test) > 0) {
        p_col_name <- if("p.adj" %in% names(stat.test)) "p.adj" else "p.value"
        if(any(stat.test[[p_col_name]] < 0.05, na.rm=TRUE)) {
          "Significância: * p < 0.05, ** p < 0.01, *** p < 0.001"
        } else { "" }
      } else { "" }
      
      p_final <- p_final + labs(title = if (!is.null(plot_title_input)) italicize_markdown(plot_title_input) else "",
                                y = "Absorbância (492 nm)", x = NULL,
                                caption = signif_caption)
      
      if (!is.null(stat.test) && nrow(stat.test) > 0) {
        if (signif_style_param == "letters") {
          
          signif_letters <- if (analysis_method_param == "mixed_anova") {
            stat.test %>%
              mutate(
                Dia = as.character(Dia),
                GrupoPuro = as.character(GrupoPuro)
              )
          } else {
            stat.test %>%
              group_by(Dia) %>%
              do({
                p_values <- .$p.adj
                names(p_values) <- paste(.$group1, .$group2, sep = "-")
                letters_res <- multcompLetters(p_values)
                data.frame(GrupoPuro = names(letters_res$Letters), Signif = letters_res$Letters, stringsAsFactors = FALSE)
              }) %>% ungroup()
          }
          
          if(nrow(signif_letters) > 0) {
            summary_with_signif <- summary_stats_for_plot %>%
              mutate(Dia = as.character(Dia), GrupoPuro = as.character(GrupoPuro)) %>%
              left_join(signif_letters, by = c("Dia", "GrupoPuro")) %>%
              mutate(Signif = ifelse(is.na(Signif), "", Signif)) %>%
              mutate(
                Dia = factor(Dia, levels = dias_ordem_param),
                GrupoPuro = factor(GrupoPuro, levels = final_order_levels)
              )
            
            p_final <- p_final + geom_text(data = summary_with_signif, aes(label = Signif, y = UpperError, group = GrupoPuro),
                                           position = position_dodge(width = dodge_width),
                                           vjust = -0.5, size = 5, fontface = "bold", na.rm = TRUE, show.legend = FALSE)
          }
          
        } else { 
          
          p_col_name <- if("p.adj" %in% names(stat.test)) "p.adj" else "p.value"
          
          stat.test_filtered <- stat.test %>%
            filter(.data[[p_col_name]] < 0.05) 
          
          if(nrow(stat.test_filtered) > 0) {
            
            n_grupos <- length(final_order_levels)
            
            x_coords <- summary_stats_for_plot %>%
              mutate(
                dia_num = as.numeric(Dia),
                grupo_num = as.numeric(factor(GrupoPuro, levels = final_order_levels))
              ) %>%
              mutate(
                x_pos = dia_num - (dodge_width/2) + ((grupo_num - 0.5) * dodge_width / n_grupos)
              ) %>%
              dplyr::select(Dia, GrupoPuro, x_pos)
            
            y_max_overall <- max(summary_stats_for_plot$UpperError, na.rm = TRUE)
            y_step <- y_max_overall * 0.08
            if (is.na(y_step) || y_step == 0 || y_step < 0.05) y_step <- 0.05
            
            y_positions <- summary_stats_for_plot %>%
              group_by(Dia) %>%
              summarise(max_y = max(UpperError, na.rm = TRUE)) %>%
              ungroup()
            
            signif_final <- stat.test_filtered %>%
              left_join(x_coords, by = c("Dia", "group1" = "GrupoPuro")) %>% rename(xmin = x_pos) %>%
              left_join(x_coords, by = c("Dia", "group2" = "GrupoPuro")) %>% rename(xmax = x_pos) %>%
              left_join(y_positions, by = "Dia") %>%
              filter(!is.na(xmin), !is.na(xmax)) %>%
              arrange(Dia, xmax - xmin) %>%
              group_by(Dia) %>%
              mutate(
                y.position = max_y + (row_number() * y_step),
                label = case_when(
                  .data[[p_col_name]] < 0.001 ~ "***",
                  .data[[p_col_name]] < 0.01  ~ "**",
                  TRUE ~ "*"
                )
              ) %>%
              ungroup()
            
            if(nrow(signif_final)>0){
              p_final <- p_final +
                geom_segment(data = signif_final,
                             aes(x = xmin, xend = xmax, y = y.position, yend = y.position),
                             inherit.aes = FALSE) +
                geom_segment(data = signif_final,
                             aes(x = xmin, xend = xmin, y = y.position, yend = y.position - (y_step * 0.1)),
                             inherit.aes = FALSE) +
                geom_segment(data = signif_final,
                             aes(x = xmax, xend = xmax, y = y.position, yend = y.position - (y_step * 0.1)),
                             inherit.aes = FALSE) +
                geom_text(data = signif_final,
                          aes(x = (xmin + xmax) / 2, y = y.position, label = label),
                          vjust = -0.3,
                          size = 6,
                          inherit.aes = FALSE)
            }
          }
        }
      }
      
      return(p_final)
    }
    
    output$normality_controls_ui <- renderUI({
      data <- data_for_normality_check()
      req(data)
      dias_disponiveis <- unique(as.character(data$Dia))
      
      tagList(
        selectInput(ns("day_for_normality"), "Selecionar Dia para Diagnóstico:", choices = dias_disponiveis),
        actionButton(ns("run_normality_check"), "Gerar e Salvar Diagnósticos", icon = icon("save"))
      )
    })
    
    observeEvent(input$run_normality_check, {
      req(input$day_for_normality)
      all_data <- data_for_normality_check()
      req(all_data)
      
      output_dir <- "diagnostico_normalidade"
      dir.create(output_dir, showWarnings = FALSE)
      
      day_data <- all_data %>% filter(Dia == input$day_for_normality)
      groups_in_day <- unique(as.character(day_data$GrupoPuro))
      
      cat(paste("\n\n--- INICIANDO DIAGNÓSTICO DE NORMALIDADE PARA O DIA:", input$day_for_normality, "---\n"))
      
      for (group_name in groups_in_day) {
        group_values <- day_data %>% filter(GrupoPuro == group_name) %>% pull(Valor)
        
        cat(paste("\n  --- Grupo:", group_name, "---\n"))
        
        if (length(group_values) < 3) {
          cat("    Amostra pequena demais para testes e plots (N < 3).\n")
          next
        }
       
        shapiro_p_value <- NA
        tryCatch({
          shapiro_result <- shapiro.test(group_values)
          shapiro_p_value <- shapiro_result$p.value
          cat(sprintf("    Shapiro-Wilk: W = %.4f, p = %.4f %s\n", 
                      shapiro_result$statistic, shapiro_p_value,
                      ifelse(shapiro_p_value < 0.05, "-> (Não Normal)", "-> (Normal)")))
        }, error = function(e) { cat(paste("    Shapiro-Wilk: Erro -", e$message, "\n")) })
        
        mean_val <- mean(group_values, na.rm = TRUE)
        sd_val <- sd(group_values, na.rm = TRUE)
        
        plot_df <- data.frame(Valor = group_values)
        p_norm <- ggplot(plot_df, aes(x = Valor)) +
          geom_histogram(aes(y = after_stat(density)), bins = 10, fill = "lightblue", color = "black", alpha = 0.7) +
          stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color = "red", linewidth = 1.2) +
          labs(
            title = paste("Diagnóstico de Normalidade:", group_name),
            subtitle = paste("Dia:", input$day_for_normality, "| Shapiro p-valor =", round(shapiro_p_value, 4)),
            x = "Valor Observado",
            y = "Densidade"
          ) +
          theme_bw(base_size = 14)
        
        safe_group_name <- gsub("[^A-Za-z0-9_.-]", "_", group_name)
        safe_day_name <- gsub("[^A-Za-z0-9_.-]", "_", input$day_for_normality)
        
        file_name <- paste0("Normalidade_Dia-", safe_day_name, "_Grupo-", safe_group_name, ".png")
        file_path <- file.path(output_dir, file_name)
        
        tryCatch({
          ggsave(filename = file_path, plot = p_norm, width = 8, height = 6, units = "in", dpi = 150)
          cat(paste("    -> Gráfico salvo em:", file_path, "\n"))
        }, error = function(e) {
          cat(paste("    ! ERRO ao salvar o gráfico:", e$message, "\n"))
        })
      }
      
      showNotification(paste("Gráficos de diagnóstico salvos na pasta:", output_dir), type = "message", duration = 8)
    })
    
    gerar_plot_dinamico <- function(selected_pure_group_names_dyn, group_order_orig_names_dyn, plot_title_dyn, test_type_filter_dyn, custom_ymax_dyn = NULL, custom_replica_mode_dyn = "duo", analysis_method_param_dyn = "mixed_anova", signif_style_param_dyn = "letters") {
      cat("\n\n--- CORREÇÃO PELO BRANCO (Gráfico Dinâmico) ---\n")
      corrected_data_list <- lapply(names(all_data$files), function(fname) {
        plate_data <- all_data$files[[fname]]
        if (is.null(plate_data$wells) || nrow(plate_data$wells) == 0) return(NULL)
        mean_blank_value <- 0
        if (length(plate_data$blank) > 0 && !is.null(plate_data$vals)) {
          blank_values <- plate_data$vals[plate_data$blank]
          if(any(!is.na(blank_values))) mean_blank_value <- mean(blank_values, na.rm = TRUE)
        }
        corrected_wells_df <- plate_data$wells
        corrected_wells_df$Valor <- as.numeric(corrected_wells_df$Valor) - mean_blank_value
        return(corrected_wells_df)
      })
      all_data_combined_dyn <- do.call(rbind, corrected_data_list)
      
      internal_plot_generation(all_data_combined_dyn, selected_pure_group_names_dyn, group_order_orig_names_dyn, plot_title_dyn, test_type_filter_dyn, custom_ymax_dyn, custom_replica_mode_dyn, dias_ordem_param = dias_analise(), analysis_method_param = analysis_method_param_dyn, signif_style_param = signif_style_param_dyn)
    }
    
    gerar_plot_personalizado <- function(grupos_pure_names_pers, ordem_orig_names_pers, titulo_pers, teste_filtro_pers, custom_ymax = NULL, custom_replica_mode = "duo", dias_ordem_param, analysis_method_param = "mixed_anova", signif_style_param = "letters") {
      cat("\n\n--- CORREÇÃO PELO BRANCO (Gráfico Salvo) ---\n")
      corrected_data_list <- lapply(names(all_data$files), function(fname) {
        plate_data <- all_data$files[[fname]]
        if (is.null(plate_data$wells) || nrow(plate_data$wells) == 0) return(NULL)
        mean_blank_value <- 0
        if (length(plate_data$blank) > 0 && !is.null(plate_data$vals)) {
          blank_values <- plate_data$vals[plate_data$blank]
          if(any(!is.na(blank_values))) mean_blank_value <- mean(blank_values, na.rm = TRUE)
        }
        corrected_wells_df <- plate_data$wells
        corrected_wells_df$Valor <- as.numeric(corrected_wells_df$Valor) - mean_blank_value
        return(corrected_wells_df)
      })
      all_data_combined_pers <- do.call(rbind, corrected_data_list)
      
      internal_plot_generation(all_data_combined_pers, grupos_pure_names_pers, ordem_orig_names_pers, titulo_pers, teste_filtro_pers, custom_ymax, custom_replica_mode, dias_ordem_param = dias_ordem_param, analysis_method_param = analysis_method_param, signif_style_param = signif_style_param)
    }
    
    output$grafico_dinamico <- renderPlot({
      params_plot_atual <- grafico_dinamico_params()
      if(is.null(params_plot_atual$grupos) || length(params_plot_atual$grupos) == 0){
        return(plot.new() + text(0.5, 0.5, "Nenhum grupo selecionado.", cex = 1.2))
      }
      p_desenhado_dyn <- gerar_plot_dinamico(
        selected_pure_group_names_dyn = params_plot_atual$grupos, 
        group_order_orig_names_dyn = params_plot_atual$ordem, 
        plot_title_dyn = input$grafico_nome, 
        test_type_filter_dyn = params_plot_atual$teste, 
        custom_ymax_dyn = params_plot_atual$ymax, 
        custom_replica_mode_dyn = params_plot_atual$replica_mode,
        analysis_method_param_dyn = params_plot_atual$analysis_method,
        signif_style_param_dyn = params_plot_atual$signif_style
      )
      if (is.null(p_desenhado_dyn)) plot.new() + text(0.5, 0.5, "Nenhum dado para plotar.", cex = 1.2) else print(p_desenhado_dyn)
    }, height=450)  
    
    observeEvent(input$grafico_selecionado, {})
    
    create_download_plot <- function(format_type = "png") {
      downloadHandler(
        filename = function() {
          req(input$grafico_selecionado, input$grafico_selecionado != "NA")
          conf_dl <- graficos_config()[[input$grafico_selecionado]]
          req(conf_dl)
          name_base_dl <- gsub("[^A-Za-z0-9_-]", "_", conf_dl$nome)
          paste0(name_base_dl %||% "grafico_salvo", "_", Sys.Date(), ".", format_type)
        },
        content = function(file) {
          req(input$grafico_selecionado, input$grafico_selecionado != "NA")
          conf_dl_content <- graficos_config()[[input$grafico_selecionado]]
          req(conf_dl_content)
          p_dl <- gerar_plot_personalizado(
            conf_dl_content$grupos, conf_dl_content$ordem, conf_dl_content$nome, conf_dl_content$teste,
            conf_dl_content$ymax, conf_dl_content$replica_mode %||% "duo", 
            dias_ordem_param = conf_dl_content$dias_ordem %||% dias_analise(),
            analysis_method_param = conf_dl_content$analysis_method %||% "mixed_anova",
            signif_style_param = conf_dl_content$signif_style %||% "letters"
          )
          req(p_dl)
          plot_w_px <- input$plot_width %||% 1000; plot_h_px <- input$plot_height %||% 500
          ggsave(file, plot = p_dl, device = format_type, 
                 width = plot_w_px / 96, height = plot_h_px / 96, dpi = 300, units = "in", bg = "white") 
        }
      )
    }
    output$download_plot_png  <- create_download_plot(format_type = "png")
    output$download_plot_pdf  <- create_download_plot(format_type = "pdf")
    output$download_plot_tiff <- create_download_plot(format_type = "tiff")
  }) 
}