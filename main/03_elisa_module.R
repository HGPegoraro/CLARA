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
library(colourpicker)
library(digest)
library(fBasics)
library(afex)
library(rstatix)
library(dunn.test)
library(car)
library(DescTools)
library(effectsize)
library(effsize)
library(ggpubr)
library(pwr)
library(RColorBrewer)
library(viridis)
library(plotly)

elisa_tabPanel <- function(id, name = "Humoral Response") {
  ns <- NS(id)
  selected_wells_input_id_elisa <- ns("selected_wells")
  
  estilo_js_elisa <- sprintf("
    (function() {
      let isMouseDown_elisa = false, isSelecting_elisa = true;
      const targetInputId_elisa = '%s';
      $(document).on('mousedown', '.elisa-module-tab .well-cell', function(e) {
        if ($(e.target).closest('.well-plate').length > 0) {
          e.preventDefault(); isMouseDown_elisa = true;
          isSelecting_elisa = !$(this).hasClass('selected');
          $(this).toggleClass('selected', isSelecting_elisa); return false;
        }
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
          if(isMouseDown_elisa) isMouseDown_elisa = false;
      });
    })();
  ", selected_wells_input_id_elisa)
  
  tabPanel(name, class = "elisa-module-tab",
           useShinyjs(),
           tags$head(
             tags$style(HTML("
        .elisa-outer-wrapper { display: flex; flex-direction: row; align-items: flex-start; width: 100%; margin: 0; padding: 0; }
        .sidebar-tabs-container { width: 50px; min-width: 50px; background: #f0f0f0; border-right: 1px solid #ddd; display: flex; flex-direction: column; z-index: 1000; height: 100vh; position: sticky; top: 0; }
        .sidebar-tab-button { width: 50px; height: 85px; background: #007bff; color: white; border: 1px solid #0056b3; cursor: pointer; font-weight: bold; writing-mode: vertical-rl; text-orientation: mixed; display: flex; align-items: center; justify-content: center; padding: 5px; font-size: 18px; margin-bottom: 5px; }
        .sidebar-tab-button:hover { background: #0056b3; }
        .sidebar-tab-button i { font-size: 24px; }
        #toggle_graph_sidebar i { transform: rotate(-90deg) scaleX(-1); display: inline-block; }
        #elisa_module-hex_code_input { font-family: monospace; }
        .left-sidebar-container { width: 320px; min-width: 320px; max-height: 100vh; background: #f5f5f5; border-right: 1px solid #ddd; overflow-y: auto; z-index: 999; padding: 15px; box-sizing: border-box; display: none; position: sticky; top: 0; }
        .left-sidebar-container.open { display: block; }
        .sidebar-title { font-size: 18px; font-weight: 700; margin-bottom: 15px; }
        .left-sidebar-container .sidebar-title { font-size: 18px; font-weight: 700; }
        .left-sidebar-container label, .left-sidebar-container .form-group label { font-size: 13px; }
        .elisa-main-content { flex-grow: 1; padding: 15px; min-width: 0; }
        .elisa-module-tab .well-plate { border-collapse: collapse; margin: 10px 0; }
        .elisa-module-tab .well-cell { border: 1px solid #ccc; width: 65px; height: 60px; text-align: center; vertical-align: middle; font-size: 11px; font-weight: bold; user-select: none; background: white; }
        .elisa-module-tab .selected { background-color: lightblue !important; }
        .elisa-module-tab .blank { background-color: lightgray !important; }
        .elisa-content-layout { display: flex; flex-direction: column; gap: 12px; }
      ")),
             tags$script(HTML(estilo_js_elisa))
           ),
           div(class = "elisa-outer-wrapper",
               div(class = "sidebar-tabs-container", id = NS(id, "sidebar_tabs_container"),
                   actionButton(NS(id, "toggle_data_sidebar"), icon("table"), class = "sidebar-tab-button", title = "Data Options"),
                   actionButton(NS(id, "toggle_graph_sidebar"), icon("chart-bar"), class = "sidebar-tab-button", title = "Graph Settings")
               ),
               div(class = "left-sidebar-container", id = NS(id, "data_sidebar_panel"),
                   div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom: 15px;", 
                       h5("Data Options", class = "sidebar-title"), actionButton(NS(id, "close_data_sidebar"), "✕", class = "btn-sm")),
                   fileInput(NS(id, "files"), "Import Excel", multiple = TRUE, accept = ".xlsx"),
                   selectInput(NS(id, "selected_plate"), "Select Plate", choices = list("No Plate available" = "NA")),
                   hr(),
                   radioButtons(NS(id, "replica_mode"), "Replica Mode:", choices = list("Duplicate" = "duo", "Triplicate" = "trio"), selected = "duo"),
                   hr(),
                   tags$b("Days Configuration"),
                   textInput(NS(id, "days_config"), label = NULL, placeholder = "Ex: Day 0, Day 21"),
                   actionButton(NS(id, "update_days"), "Update Days List", icon = icon("sync")),
                   uiOutput(NS(id, "days_radio_ui")),
                   hr(),
                   textInput(NS(id, "test_type"), "Test Name"),
                   textInput(NS(id, "group_name"), "Group"),
                   actionButton(NS(id, "add_group"), "Add Group"),
                   actionButton(NS(id, "set_blank"), "Set Blank"),
                   actionButton(NS(id, "remove_from_group"), "Remove Selected Group", icon = icon("eraser")),
                   hr(),
                   fluidRow(
                     column(6, fileInput(NS(id, "load_state"), "Load Project", accept = ".rds")),
                     column(6, downloadButton(NS(id, "save_state"), "Save Project", style = "margin-top: 25px;"))
                   )
               ),
               div(class = "left-sidebar-container", id = NS(id, "graph_sidebar_panel"),
                   div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom: 15px;", 
                       h5("Graph Settings", class = "sidebar-title"), actionButton(NS(id, "close_graph_sidebar"), "✕", class = "btn-sm")),
                   uiOutput(NS(id, "test_filter_ui")),
                   hr(),
                   actionButton(NS(id, "refresh_graph_conditions"), "Refresh Graph Settings", icon = icon("sync"), class = "btn-sm", width = "100%"),
                   hr(),
                   textInput(NS(id, "plot_name"), "Graph Name"),
                   actionButton(NS(id, "add_plot"), "Add Graph"),
                   hr(),
                   shinyWidgets::pickerInput(NS(id, "plot_groups"), "Select Groups", choices = list("NA"), multiple = TRUE, options = list(`live-search` = TRUE), width = '100%'),
                   hr(),
                   selectInput(NS(id, "selected_plot"), "Select Graph for Display", choices = list("NA")),
                   actionButton(NS(id, "remove_plot"), "Remove Graph", icon = icon("trash"), class = "btn-danger", width = "100%"),
                   hr(),
                   uiOutput(NS(id, "group_order_ui")),
                   hr(),
                   radioButtons(NS(id, "analysis_method"), "Statistical Method:",
                                choices = list("Integrated (Twoway-ANOVA)" = "mixed_anova", "Daily (Oneway-ANOVA)" = "anova_per_day", "Daily (Kruskal-Wallis)" = "kruskal_per_day", "t-test" = "t_test", "Welch" = "welch", "Mann-Whitney" = "mann_whitney"), selected = "mixed_anova"),
                   hr(),
                   radioButtons(NS(id, "signif_style"), "Significance Style:", choices = list("Letters" = "letters", "Asterisks" = "asterisks", "P-values" = "pvalues", "None" = "none"), selected = "letters"),
                   hr(),
                   # Axis label inputs and snapshot capture live in Graph Settings
                   textInput(NS(id, "x_axis_label"), "X Axis Label", value = "Day"),
                   textInput(NS(id, "y_axis_label"), "Y Axis Label", value = "Absorbance (492 nm)"),
                   actionButton(NS(id, "capture_snapshot"), "Capture Snapshot as Final (Static)", class = "btn-primary", width = "100%"),
                   hr(),
                   # color picker inline for dynamic graph (moved from main area)
                   uiOutput(NS(id, "plotly_color_picker_inline")),
                   hr(),
                   uiOutput(NS(id, "normality_controls_ui"))
               ),
               div(class = "elisa-main-content", id = NS(id, "main_content"),
                   h4(strong("96 Well Plate & Analysis")),
                   div(class = "elisa-content-layout",
                       div(class = "plate-section", h5("Plate Editor"), uiOutput(ns("plate_ui")), DTOutput(ns("group_table"))),
                       div(class = "graph-section",
                           h5("Dynamic Graph"), uiOutput(ns("dynamic_plot_ui")),
                           br(),
                           h5("Final Graph"), uiOutput(ns("plots_ui")),
                           # place for the captured static final plot
                           uiOutput(ns("final_static_plot_ui")),
                             # Downloads for the captured static final plot (if any)
                             downloadButton(ns("download_final_png"), "Final PNG"),
                             downloadButton(ns("download_final_pdf"), "Final PDF"),
                             downloadButton(ns("download_final_tiff"), "Final TIFF")
                       )
                   )
               )
           ),
           tags$script(HTML(sprintf("
             $(document).ready(function() {
               var dataPanel = $('#%s'), graphPanel = $('#%s');
               function reset() { dataPanel.removeClass('open'); graphPanel.removeClass('open'); }
               $('#%s').on('click', function() { let o = dataPanel.hasClass('open'); reset(); if(!o) dataPanel.addClass('open'); });
               $('#%s').on('click', function() { let o = graphPanel.hasClass('open'); reset(); if(!o) graphPanel.addClass('open'); });
               $('#%s, #%s').on('click', reset);
             });
           ", ns("data_sidebar_panel"), ns("graph_sidebar_panel"), ns("toggle_data_sidebar"), ns("toggle_graph_sidebar"), ns("close_data_sidebar"), ns("close_graph_sidebar"))))
  )
}

elisa_server <- function(id, global_excel_format_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    plotly_group_colors <- reactiveVal(list())
    unsaved_changes <- reactiveVal(FALSE)
    project_file_path <- reactiveVal(NULL)
    
    well_names_fixed <- function() {
      rows <- LETTERS[1:8]
      cols <- 1:12
      return(outer(rows, cols, paste0))
    }

    all_data <- reactiveValues(files = list(), export_data = NULL)
    group_colors <- reactiveVal(list())
    plots_config <- reactiveVal(list())
    data_for_normality_check <- reactiveVal(NULL)

    selected_groups_rv <- reactiveVal(character(0))

    analysis_days <- reactiveVal(c("Day 0", "Day 21", "Day 42"))

    output$days_radio_ui <- renderUI({
      days <- analysis_days()
      req(days)
      radioButtons(ns("selected_day"), "Select the Day for Assignment:",
                   choices = days,
                   selected = days[1])
    })

    observe({
      updateTextInput(session, "days_config", value = paste(analysis_days(), collapse = ", "))
    })

    observeEvent(input$update_days, {
      req(input$days_config)
      new_days_raw <- strsplit(input$days_config, ",")[[1]]
      new_days_clean <- trimws(new_days_raw)
      new_days_final <- new_days_clean[new_days_clean != ""]

      if (length(new_days_final) > 0) {
        analysis_days(new_days_final)
        showNotification("List of Days Updated Successfully.", type = "message")
      } else {
        showNotification("The List of Days cannot be empty.", type = "error")
      }
    })

    dynamic_plot_params <- reactive({
      list(
        groups = selected_groups_rv(),
        order = input$group_order,
        test = if (!is.null(input$test_filter) && input$test_filter != "All") input$test_filter else NULL,
        replica_mode = input$replica_mode,
        analysis_method = input$analysis_method,
        signif_style = input$signif_style,
        color_scheme = input$color_scheme
      )
    })

    plot_group_colors <- reactiveVal(list())
    # token to force re-render of plots when interactive plotly colors change
    plot_color_token <- reactiveVal(0)
    # reactive container to hold a user-captured static ggplot snapshot
    final_plot_gg <- reactiveVal(NULL)

    observeEvent(input$plot_groups, {
      selected_groups_rv(input$plot_groups)
    }, ignoreNULL = FALSE)

    output$color_custom_ui <- renderUI({
      # Custom color scheme removed - only color wheel/picker is available
      return(NULL)
    })

    

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

    get_group_names <- function() {
      all_wells <- do.call(rbind, lapply(all_data$files, function(f) f$wells))
      if (is.null(all_wells) || nrow(all_wells) == 0) return(character(0))
      groups <- all_wells$Group
      groups <- groups[!is.na(groups) & groups != ""]
      unique(groups)
    }

    get_test_names <- function() {
      all_wells <- do.call(rbind, lapply(all_data$files, function(f) f$wells))
      if (is.null(all_wells) || nrow(all_wells) == 0) return(character(0))
      tests <- all_wells$Test
      tests <- tests[!is.na(tests) & tests != ""]
      unique(tests)
    }

    remove_html <- function(text) {
      if(is.null(text)) return(NULL)
      sapply(text, function(t) if(is.null(t) || is.na(t)) NA_character_ else gsub("<[^>]+>", "", t), USE.NAMES = FALSE)
    }

    remove_asterisks <- function(text) {
      if(is.null(text)) return(NULL)
      sapply(text, function(t) if(is.null(t) || is.na(t)) NA_character_ else gsub("\\*(.*?)\\*", "\\1", t), USE.NAMES = FALSE)
    }

    observeEvent(input$remove_plot, {
      req(input$selected_plot, input$selected_plot != "NA")
      configs <- plots_config()
      configs[[input$selected_plot]] <- NULL
      plots_config(configs)
      if (length(configs) > 0) {
        new_selected_id <- names(configs)[1]
        updateSelectInput(session, "selected_plot",
                          choices = setNames(sapply(configs, function(x) x$id), lapply(sapply(configs, function(x) x$name), function(name) HTML(italicize_markdown(name)))),
                          selected = new_selected_id)
      } else {
        updateSelectInput(session, "selected_plot", choices = list("No Graph Available" = "NA"), selected = "NA")
      }
      showNotification("Graph Removed Successfully.", type = "message")
    })

    output$test_filter_ui <- renderUI({
      tests <- get_test_names()
      if (length(tests) == 0) return(NULL)
      selectInput(ns("test_filter"), "Filter by Test Type", choices = c("All", tests), selected = "All")
    })

    output$save_state <- downloadHandler(
      filename = function() { paste0("Project_", Sys.Date(), ".rds") },
      content = function(file) {
        state <- list(
          files = all_data$files,
          colors = group_colors(),
          order = input$group_order,
          plots = plots_config(),
          replica_mode_saved = input$replica_mode,
          days = analysis_days(),
          analysis_method_saved = input$analysis_method,
          signif_style_saved = input$signif_style
        )
        saveRDS(state, file)
      }
    )

    safe_update_plot_groups <- function(session, choices_list_picker, selected_values_picker, pure_group_names_available) {
      if (is.null(choices_list_picker) || length(choices_list_picker) == 0) {
        shinyWidgets::updatePickerInput(session, "plot_groups", choices = list("No Group Available" = "NA"), selected = character(0))
        return()
      }
      selected_valid <- intersect(selected_values_picker, pure_group_names_available)
      shinyWidgets::updatePickerInput(session, "plot_groups", choices = choices_list_picker, choicesOpt = list(content = names(choices_list_picker)), selected = selected_valid)
    }

    update_plot_groups <- function() {
      orig_group_names <- get_group_names()
      pure_group_names <- remove_asterisks(remove_html(orig_group_names))

      if (length(pure_group_names) == 0) {
        safe_update_plot_groups(session, NULL, NULL, character(0))
        selected_groups_rv(character(0))
        return()
      }

      selection_to_use <- pure_group_names

      labels_for_display <- lapply(italicize_markdown(orig_group_names), HTML)
      choices_list_new <- stats::setNames(pure_group_names, labels_for_display)

      safe_update_plot_groups(session, choices_list_new, selection_to_use, pure_group_names)
      selected_groups_rv(selection_to_use)
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
        showNotification(paste("Global Excel Reader Format ('", current_global_format_id, "') is invalid."), type = "error", duration=7)
        return()
      }

      withProgress(message = "Loading Excel Files...", value = 0, {
        for (i in seq_len(nrow(input$files))) {
          incProgress(1 / nrow(input$files), detail = input$files$name[i])
          filepath <- input$files$datapath[i]
          original_fname <- input$files$name[i]

          plate_values_named_vector <- reader_function(filepath)

          if (is.null(plate_values_named_vector)) {
            showNotification(paste("Fail to Process(ELISA):", original_fname), type = "error")
            next
          }

          fname_sanitized <- gsub("[^A-Za-z0-9_.-]", "_", original_fname)
          loaded_files_temp[[fname_sanitized]] <- list(
            vals = plate_values_named_vector,
            wells = data.frame(Well=character(), Value=numeric(), Group=character(), Day=character(), Test=character(), Plots=character(), stringsAsFactors=FALSE),
            blank = character()
          )
        }
      })

      if (length(loaded_files_temp) > 0) {
        for(fname in names(loaded_files_temp)) {
          all_data$files[[fname]] <- loaded_files_temp[[fname]]
        }

        available_plates_in_module <- names(all_data$files)
        current_selected_plate <- input$selected_plate

        new_selected_plate <- if (!is.null(current_selected_plate) && current_selected_plate %in% available_plates_in_module) {
          current_selected_plate
        } else if (length(available_plates_in_module) > 0) {
          names(loaded_files_temp)[1]
        } else { "NA" }

        updateSelectInput(session, "selected_plate",
                          choices = if(length(available_plates_in_module) > 0) setNames(available_plates_in_module, available_plates_in_module) else list("No Plate available" = "NA"),
                          selected = new_selected_plate)

        update_plot_groups()
      }
    })

    observeEvent(input$selected_plate, {}, ignoreInit = TRUE, ignoreNULL = TRUE)

    observeEvent(input$add_group, {
      req(input$group_name, input$selected_wells, current_file(), current_file() != "NA", input$selected_day)
      if (trimws(input$group_name) == "") { showNotification("Use a Valid Name.", type = "error"); return() }
      fname <- current_file(); vals_current_plate <- plate_vals()
      if(is.null(vals_current_plate)) { showNotification("Plate Values not loaded.", type="error"); return() }
      selected_day <- input$selected_day
      valid_wells_selected <- input$selected_wells[input$selected_wells %in% names(vals_current_plate)]
      if (length(valid_wells_selected) == 0) { showNotification("No Valid Wells Selected.", type = "warning"); return() }
      new_rows_to_add <- data.frame(Well = paste0(fname, "_", valid_wells_selected), Value = vals_current_plate[valid_wells_selected],
                                    Group = input$group_name, Day = selected_day, Test = input$test_type, Plots = NA_character_, stringsAsFactors = FALSE )
      existing_wells_df_for_plate <- all_data$files[[fname]]$wells
      existing_wells_df_for_plate <- existing_wells_df_for_plate[!existing_wells_df_for_plate$Well %in% new_rows_to_add$Well,]
      all_data$files[[fname]]$wells <- rbind(existing_wells_df_for_plate, new_rows_to_add)

      pure_group_name_for_color <- remove_asterisks(remove_html(input$group_name))
      new_color_key <- paste(pure_group_name_for_color, input$selected_day, sep = "_")

      current_group_cols <- group_colors()
      if (!(new_color_key %in% names(current_group_cols))) {
        current_group_cols[[new_color_key]] <- get_new_well_color(new_color_key)
        group_colors(current_group_cols)
      }

      runjs(sprintf("$('.well-cell[data-well]').removeClass('selected'); Shiny.setInputValue('%s', [], {priority: 'event'});", ns("selected_wells")))
      update_plot_groups(); showNotification(paste("Group", input$group_name, "added."), type = "message")
    })

    output$group_order_ui <- renderUI({
      all_combined_wells <- do.call(rbind, lapply(all_data$files, function(f) f$wells))
      if (is.null(all_combined_wells) || nrow(all_combined_wells) == 0) return(NULL)
      unique_original_groups <- unique(all_combined_wells$Group)
      unique_original_groups <- unique_original_groups[!is.na(unique_original_groups) & unique_original_groups != ""]
      if (length(unique_original_groups) == 0) return(NULL)
      final_labels_for_bucket <- sort(unique_original_groups)
      bucket_list(header = "Drag the groups to define the order in the graph:", group_name = ns("plot_groups_bucket_shared"), orientation = "horizontal",
                  add_rank_list(text = NULL, labels = lapply(italicize_markdown(final_labels_for_bucket), HTML), input_id = ns("group_order")))
    })

    observeEvent(input$remove_from_group, {
      req(input$selected_wells, current_file(), current_file() != "NA"); fname <- current_file()
      current_wells_df <- all_data$files[[fname]]$wells
      if (is.null(current_wells_df) || nrow(current_wells_df) == 0) return()
      prefixed_wells_to_remove_from_groups <- paste0(fname, "_", input$selected_wells)
      all_data$files[[fname]]$wells <- current_wells_df[!(current_wells_df$Well %in% prefixed_wells_to_remove_from_groups),]
      all_data$files[[fname]]$blank <- setdiff(all_data$files[[fname]]$blank, input$selected_wells)
      runjs(sprintf("var wells = %s; wells.forEach(w => $(\".elisa-module-tab .well-cell[data-well='\"+w+\"']\").removeClass('selected blank').css('background-color', '')); Shiny.setInputValue('%s', [], {priority: 'event'});",
                    jsonlite::toJSON(input$selected_wells, auto_unbox = TRUE), ns("selected_wells")))
      update_plot_groups(); showNotification("Wells Removed.", type = "message")
    })

    observeEvent(input$set_blank, {
      req(input$selected_wells, current_file(), current_file() != "NA"); fname <- current_file()
      all_data$files[[fname]]$blank <- unique(c(all_data$files[[fname]]$blank, input$selected_wells))
      current_wells_df <- all_data$files[[fname]]$wells
      if (!is.null(current_wells_df) && nrow(current_wells_df) > 0) {
        prefixed_blanked_wells <- paste0(fname, "_", input$selected_wells)
        all_data$files[[fname]]$wells <- current_wells_df[!current_wells_df$Well %in% prefixed_blanked_wells,]
      }
      runjs(sprintf("var wells = %s; wells.forEach(w => $(\".elisa-module-tab .well-cell[data-well='\"+w+\"']\").removeClass('selected').addClass('blank').css('background-color', '')); Shiny.setInputValue('%s', [], {priority: 'event'});",
                    jsonlite::toJSON(input$selected_wells, auto_unbox = TRUE), ns("selected_wells")))
      update_plot_groups(); showNotification("Wells Set as Blank.", type = "message")
    })

    output$group_table <- renderDT({
      all_combined_df <- do.call(rbind, lapply(all_data$files, function(f) f$wells))
      if (is.null(all_combined_df) || nrow(all_combined_df) == 0) return(NULL)
      display_df_group_table <- all_combined_df
      display_df_group_table$Group <- sapply(display_df_group_table$Group, function(g) { if (is.na(g)) NA_character_ else italicize_markdown(g) })
      datatable(display_df_group_table, escape = FALSE, options = list(pageLength = 5, scrollX=TRUE))
    })

    observeEvent(input$add_plot, {
      req(input$plot_name); if (trimws(input$plot_name) == "") { showNotification("Insert a Name for the Graph.", type = "error"); return() }
      params_to_save <- dynamic_plot_params()
      req(params_to_save$groups, length(params_to_save$groups) > 0, msg = "Select Groups for the Graph.")
      new_plot_id <- paste0("plot_", digest::digest(list(input$plot_name, Sys.time(), runif(1)), algo = "crc32"))
      new_plot_config <- list(
        name = input$plot_name, id = new_plot_id, groups = params_to_save$groups, order = params_to_save$order,
        test = params_to_save$test, replica_mode = params_to_save$replica_mode,
        days_order = analysis_days(),
        analysis_method = params_to_save$analysis_method,
        signif_style = params_to_save$signif_style
      )
      current_configs <- plots_config(); current_configs[[new_plot_id]] <- new_plot_config; plots_config(current_configs)
      updateSelectInput(session, "selected_plot",
                        choices = setNames(sapply(current_configs, function(x) x$id), lapply(sapply(current_configs, function(x) x$name), function(name) HTML(italicize_markdown(name)))),
                        selected = new_plot_id )
      showNotification(paste("Graph", input$plot_name, "added!"), type="message"); updateTextInput(session, "plot_name", value="")
    })

    output$plots_ui <- renderUI({ req(input$selected_plot, input$selected_plot != "NA"); plotly::plotlyOutput(outputId = ns(input$selected_plot), height = "600px") })

    observeEvent(input$load_state, {
      req(input$load_state)
      loaded_state <- tryCatch(readRDS(input$load_state$datapath), error = function(e) { showNotification(paste("Error reading RDS:", e$message), type="error"); NULL })

      expected_fields <- c("files", "colors", "order", "plots")
      if(is.null(loaded_state) || !is.list(loaded_state) || !all(expected_fields %in% names(loaded_state))) {
        showNotification("RDS file invalid or incomplete.", type="error"); return()
      }

      if (!is.null(loaded_state$days) && is.character(loaded_state$days) && length(loaded_state$days) > 0) {
        analysis_days(loaded_state$days)
      } else { analysis_days(c("Day 0", "Day 21", "Day 42")) }

      loaded_replica_mode <- loaded_state$replica_mode_saved %||% "duo"
      if (!loaded_replica_mode %in% c("duo", "trio")) { loaded_replica_mode <- "duo" }

      loaded_analysis_method <- loaded_state$analysis_method_saved %||% "mixed_anova"
      if (!loaded_analysis_method %in% c("kruskal_per_day", "anova_per_day", "mixed_anova")) { loaded_analysis_method <- "mixed_anova" }

      loaded_signif_style <- loaded_state$signif_style_saved %||% "letters"
      if (!loaded_signif_style %in% c("letters", "asterisks", "pvalues", "none")) { loaded_signif_style <- "letters" }

      expected_wells_cols <- c("Well"="character", "Value"="numeric", "Group"="character", "Day"="character", "Test"="character", "Plots"="character")
      if (!is.null(loaded_state$files)) {
        for (fname_loaded in names(loaded_state$files)) {
          if (!is.null(loaded_state$files[[fname_loaded]]$wells)) {
            current_wells_df <- loaded_state$files[[fname_loaded]]$wells
            for (col_name in names(expected_wells_cols)) {
              if (!(col_name %in% colnames(current_wells_df))) {
                current_wells_df[[col_name]] <- switch(expected_wells_cols[col_name], "character" = NA_character_, "numeric" = NA_real_, NULL)
              }
            }
            loaded_state$files[[fname_loaded]]$wells <- current_wells_df[, names(expected_wells_cols), drop = FALSE]
          }
        }
      }
      all_data$files <- loaded_state$files

      if(!is.null(loaded_state$colors) && is.list(loaded_state$colors)) {
        group_colors(loaded_state$colors)
        assigned_well_colors_keys(loaded_state$colors)
      } else { group_colors(list()); assigned_well_colors_keys(list()) }

      cleaned_plots_config <- list()
      if (!is.null(loaded_state$plots) && is.list(loaded_state$plots)) {
        for (plot_item_id in names(loaded_state$plots)) {
          plot_item <- loaded_state$plots[[plot_item_id]]
          if (is.list(plot_item) && !is.null(plot_item$id) && !is.na(plot_item$id) && !is.null(plot_item$name)) {
            plot_item$ymax <- plot_item$ymax %||% NULL
            plot_item$replica_mode <- plot_item$replica_mode %||% loaded_replica_mode
            plot_item$days_order <- plot_item$days_order %||% c("Day 0", "Day 21", "Day 42")
            plot_item$analysis_method <- plot_item$analysis_method %||% loaded_analysis_method
            plot_item$signif_style <- plot_item$signif_style %||% loaded_signif_style
            cleaned_plots_config[[plot_item$id]] <- plot_item
          }
        }
      }
      plots_config(cleaned_plots_config)

      updateRadioButtons(session, "replica_mode", selected = loaded_replica_mode)
      updateRadioButtons(session, "analysis_method", selected = loaded_analysis_method)
      updateRadioButtons(session, "signif_style", selected = loaded_signif_style)

      loaded_plates <- if(!is.null(loaded_state$files)) names(loaded_state$files) else character(0)
      updateSelectInput(session, "selected_plate", choices = if(length(loaded_plates) > 0) loaded_plates else list("No Plate available" = "NA"),
                        selected = if(length(loaded_plates) > 0) loaded_plates[1] else "NA")

      loaded_plots_configs <- plots_config()
      if (!is.null(loaded_plots_configs) && length(loaded_plots_configs) > 0) {
        updateSelectInput(session, "selected_plot", choices = setNames(sapply(loaded_plots_configs, function(x) x$id), lapply(sapply(loaded_plots_configs, function(x) x$name), function(name) HTML(italicize_markdown(name)))),
                          selected = names(loaded_plots_configs)[1])
      } else { updateSelectInput(session, "selected_plot", choices = list("No Graph Available" = "NA"), selected = "NA") }

      update_plot_groups()
      showNotification("Project Loaded.", type = "message")
    })
    
    # Refresh graph conditions when button clicked
    observeEvent(input$refresh_graph_conditions, {
      update_plot_groups()
      plot_color_token(plot_color_token() + 1)
      showNotification("Graph settings refreshed", type = "message", duration = 2)
    })

    observe({
      plot_color_token()  # Dependency on color token to trigger re-renders when colors change
      configs_to_plot <- plots_config()
      for (current_plot_id_short in names(configs_to_plot)) {
        local({
          current_plot_id_local <- current_plot_id_short
          config_item_plot <- configs_to_plot[[current_plot_id_local]]
          output[[current_plot_id_local]] <- plotly::renderPlotly({
            p_drawn <- generate_custom_plot(
              groups_pure_names_custom = config_item_plot$groups, order_orig_names_custom = config_item_plot$order,
              title_custom = config_item_plot$name, test_filter_custom = config_item_plot$test,
              custom_replica_mode = config_item_plot$replica_mode %||% "duo",
              days_order_param = config_item_plot$days_order %||% analysis_days(),
              analysis_method_param = config_item_plot$analysis_method %||% "mixed_anova",
              signif_style_param = config_item_plot$signif_style %||% "letters"
            )
            if (is.null(p_drawn)) return(NULL)
            ggplotly(p_drawn, tooltip = c("x", "y", "fill")) %>% 
              layout(legend = list(orientation = "v"), hovermode = "closest", dragmode = "zoom") %>%
              config(displayModeBar = TRUE, modeBarButtonsToRemove = list("pan2d", "lasso2d", "resetScale2d"))
          })
        })
      }
    })

    output$plate_ui <- renderUI({
      req(current_file(), current_file() != "NA")
      fname <- current_file()
      if (!fname %in% names(all_data$files) || is.null(all_data$files[[fname]])) return(p("Data not found."))
      vals_for_plate <- plate_vals();
      if(is.null(vals_for_plate)) return(p("Plate Values Unavailable."))

      plate_layout_matrix <- well_names_fixed();
      blank_wells_for_plate <- all_data$files[[fname]]$blank
      wells_info_df_for_plate <- all_data$files[[fname]]$wells
      current_group_colors_list <- group_colors()

      plate_html_table <- "<table class='well-plate'>"
      for (r_idx in 1:nrow(plate_layout_matrix)) {
        plate_html_table <- paste0(plate_html_table, "<tr>")
        for (c_idx in 1:ncol(plate_layout_matrix)) {
          well_id_current <- plate_layout_matrix[r_idx, c_idx]
          well_value_current <- vals_for_plate[well_id_current]
          well_value_display_current <- if(is.na(well_value_current)) "_" else as.character(round(well_value_current, 3))
          cell_class_current <- "well-cell"
          if (well_id_current %in% blank_wells_for_plate) cell_class_current <- paste(cell_class_current, "blank")

          bg_color_style_current <- ""
          cell_content <- sprintf("%s<br><small>%s</small>", well_id_current, well_value_display_current)

          prefixed_well_id_current <- paste0(fname, "_", well_id_current)
          well_group_info_row <- NULL
          if(!is.null(wells_info_df_for_plate) && nrow(wells_info_df_for_plate) > 0){
            idx_match <- match(prefixed_well_id_current, wells_info_df_for_plate$Well)
            if(!is.na(idx_match)) well_group_info_row <- wells_info_df_for_plate[idx_match, ]
          }

          if (!is.null(well_group_info_row) && nrow(well_group_info_row) == 1) {
            group_name_orig_current <- well_group_info_row$Group[1]
            group_day_current <- well_group_info_row$Day[1]
            cell_content <- sprintf("%s<br><small>%s</small><br><small><i>%s (%s)</i></small>",
                                    well_id_current, well_value_display_current,
                                    htmltools::htmlEscape(group_name_orig_current), htmltools::htmlEscape(group_day_current))

            pure_group_name_current <- remove_asterisks(remove_html(group_name_orig_current))
            color_key_current <- paste(pure_group_name_current, group_day_current, sep = "_")
            if (color_key_current %in% names(current_group_colors_list)) {
              bg_color_style_current <- sprintf("style='background-color:%s;'", current_group_colors_list[[color_key_current]])
            }
          }
          plate_html_table <- paste0(plate_html_table, sprintf("<td class='%s' data-well='%s' %s>%s</td>",
                                                               cell_class_current, well_id_current, bg_color_style_current, cell_content))
        }
        plate_html_table <- paste0(plate_html_table, "</tr>")
      }
      plate_html_table <- paste0(plate_html_table, "</table>"); HTML(plate_html_table)
    })

    internal_plot_generation <- function(all_well_data_input,
                                         selected_pure_group_names_input,
                                         group_order_orig_names_input,
                                         plot_title_input,
                                         test_type_filter_input,
                                         custom_ymax = NULL,
                                         replica_mode_param = "duo",
                                         days_order_param,
                                         analysis_method_param = "mixed_anova",
                                         signif_style_param = "letters",
                                         custom_colors = list()) {

      cat("\n\n=========================================================\n")
      cat("--- GRAPH GENERATION:", plot_title_input, "---\n")
      cat("--- ANALYSIS METHOD:", toupper(analysis_method_param), "| STYLE:", toupper(signif_style_param), "---\n")

      if (is.null(all_well_data_input) || nrow(all_well_data_input) == 0) return(NULL)
      if (is.null(selected_pure_group_names_input) || length(selected_pure_group_names_input) < 1) {
        return(NULL)
      }

      # reference color token so this function re-executes when interactive colors change
      plot_color_token()

      plot_data_filtered <- all_well_data_input
      if (!is.null(test_type_filter_input) && test_type_filter_input != "All" && test_type_filter_input != "") {
        plot_data_filtered <- plot_data_filtered %>% dplyr::filter(Test == test_type_filter_input)
      }

      plot_data_processed_initial <- plot_data_filtered %>%
        mutate(PureGroup = remove_asterisks(remove_html(Group))) %>%
        dplyr::filter(PureGroup %in% selected_pure_group_names_input)

      n_technical_replicates <- ifelse(replica_mode_param == "trio", 3, 2)

      plot_data_observations <- plot_data_processed_initial %>%
        arrange(Well) %>%
        group_by(Group, Day, Test, PureGroup) %>%
        mutate(replica_set_id = ceiling(row_number() / n_technical_replicates)) %>%
        ungroup() %>%
        group_by(Group, Day, Test, PureGroup, replica_set_id) %>%
        summarise(ObservationValue = mean(Value, na.rm = TRUE), N_in_obs = n(), .groups = 'drop') %>%
        filter(N_in_obs >= n_technical_replicates) %>%
        group_by(PureGroup) %>%
        mutate(AnimalID = paste0(PureGroup, "_", replica_set_id)) %>%
        ungroup()

      if (nrow(plot_data_observations) == 0) {
        showNotification("No complete biological observation found.", type = "warning", duration = 10)
        return(NULL)
      }

      final_order_levels <- selected_pure_group_names_input
      if (!is.null(group_order_orig_names_input) && length(group_order_orig_names_input) > 0) {
        ordered_pure_names <- remove_asterisks(remove_html(group_order_orig_names_input))
        valid_ordered_pure_names <- ordered_pure_names[ordered_pure_names %in% selected_pure_group_names_input]
        if (length(valid_ordered_pure_names) > 0) {
          missing_from_order <- setdiff(selected_pure_group_names_input, valid_ordered_pure_names)
          final_order_levels <- c(valid_ordered_pure_names, missing_from_order)
        }
      } else {
        final_order_levels <- sort(selected_pure_group_names_input)
      }

      plot_data_for_analysis <- plot_data_observations %>%
        dplyr::select(AnimalID, Group, Day, Test, PureGroup, Value = ObservationValue) %>%
        dplyr::filter(!is.na(Value)) %>%
        mutate(
          Day = factor(Day, levels = days_order_param),
          PureGroup = factor(PureGroup, levels = final_order_levels)
        )

      stat.test <- NULL

      if (length(final_order_levels) > 1 && nrow(plot_data_for_analysis) > 0) {
        tryCatch({
          if (analysis_method_param == "mixed_anova") {
            model <- afex::mixed(Value ~ PureGroup * Day + (1|AnimalID), data = plot_data_for_analysis)
            cat("\n--- TABLE OF TWOWAY ANOVA ---\n"); print(knitr::kable(anova(model)))
            emm <- emmeans(model, pairwise ~ PureGroup | Day)
            if (signif_style_param == "letters") {
              cld <- multcomp::cld(emm$emmeans, Letters = letters, alpha = 0.05, adjust = "tukey")
              stat.test <- as.data.frame(cld) %>%
                dplyr::select(Day, PureGroup, .group) %>% dplyr::rename(Signif = .group) %>%
                mutate(Signif = trimws(Signif))
            } else {
              stat.test <- as.data.frame(summary(emm$contrasts)) %>%
                dplyr::select(Day, contrast, p.value) %>%
                tidyr::separate(contrast, into = c("group1", "group2"), sep = " - ", remove = TRUE)
            }
          } else {
            stat.test <- plot_data_for_analysis %>%
              group_by(Day) %>%
              do(
                if (analysis_method_param == "anova_per_day") {
                  rstatix::tukey_hsd(aov(Value ~ PureGroup, data = .))
                } else {
                  dunn_result <- dunn.test::dunn.test(.$Value, .$PureGroup, method = "bh", list = TRUE, alpha = 0.05)
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
          showNotification(paste("Statistical Analysis Failed:", e$message), type = "error", duration = 10)
          cat("\nERROR in Analysis:", e$message, "\n")
        })
      }

      summary_stats_for_plot <- plot_data_for_analysis %>%
        group_by(Day, PureGroup, Group) %>%
        summarise(MeanValue = mean(Value, na.rm = TRUE), SDValue = sd(Value, na.rm = TRUE), N_obs = n(), .groups = 'drop') %>%
        mutate(MeanValue = ifelse(is.na(MeanValue), 0, MeanValue), SDValue = ifelse(is.na(SDValue) | N_obs < 2, 0, SDValue),
               UpperError = MeanValue + SDValue) %>%
        mutate(
          Day = factor(Day, levels = days_order_param),
          PureGroup = factor(PureGroup, levels = final_order_levels)
        )

      data_for_normality_check(plot_data_for_analysis)

      legend_labels_map_plot <- summary_stats_for_plot %>%
        distinct(PureGroup, Group) %>% arrange(factor(PureGroup, levels = final_order_levels)) %>%
        mutate(DisplayLabel = italicize_markdown(Group)) %>%
        dplyr::select(PureGroup, DisplayLabel) %>% tibble::deframe()

      dodge_width <- 0.9

      # Determine colors for plot fill (use custom_colors if available, else fall back)
      pgcols <- plot_group_colors() # Existing reactive
      cat("\n--- INTERNAL PLOT GENERATION ---\n")
      cat("Custom colors passed:", paste(names(custom_colors), "=", unlist(custom_colors), collapse=", "), "\n")
      cat("Final order levels:", paste(final_order_levels, collapse=", "), "\n")
      
      if (length(custom_colors) > 0) {
        # This combines your chosen wheel colors with the defaults
        pgcols_tmp <- sapply(final_order_levels, function(k) {
          color_val <- if (!is.null(custom_colors[[k]])) custom_colors[[k]] else (if (!is.null(pgcols[[k]])) pgcols[[k]] else NA_character_)
          cat("  Group", k, "->", color_val, "\n")
          color_val
        }, USE.NAMES = FALSE)
        names(pgcols_tmp) <- final_order_levels
        pgcols <- pgcols_tmp
        cat("Colors after merge:", paste(names(pgcols), "=", unlist(pgcols), collapse=", "), "\n")
      }
      
      if (is.null(pgcols) || length(pgcols) == 0) {
        # fallback to group_colors keyed by PureGroup_Day or default greys
        gc <- group_colors()
        pgcols_tmp <- sapply(final_order_levels, function(k) {
          if (!is.null(gc[[k]])) return(gc[[k]])
          m <- grep(paste0('^', k, '(_|$)'), names(gc), value = TRUE)
          if (length(m) > 0) return(gc[[m[1]]])
          return(NA_character_)
        }, USE.NAMES = FALSE)
        if (all(is.na(pgcols_tmp))) pgcols_tmp <- grDevices::grey.colors(length(final_order_levels), start = 0.9, end = 0.2)
        names(pgcols_tmp) <- final_order_levels
        pgcols <- pgcols_tmp
      } else {
        cols_vec <- sapply(final_order_levels, function(k) {
          if (!is.null(pgcols[[k]])) pgcols[[k]] else NA_character_
        }, USE.NAMES = FALSE)
        if (all(is.na(cols_vec))) {
          cols_vec <- grDevices::grey.colors(length(final_order_levels), start = 0.9, end = 0.2)
        }
        names(cols_vec) <- final_order_levels
        pgcols <- cols_vec
      }

      # Calculate auto y-max to accommodate significance labels
      y_max_data <- max(summary_stats_for_plot$UpperError, na.rm = TRUE)
      n_sig_lines <- if (!is.null(stat.test) && nrow(stat.test) > 0) ceiling(nrow(stat.test) / length(unique(summary_stats_for_plot$Day))) else 0
      # Increased margins: base 25% + 12% per significance line + 5% buffer for line rendering
      y_margin <- y_max_data * (0.25 + (n_sig_lines * 0.12) + 0.05)
      auto_ymax <- y_max_data + y_margin
      if (is.na(auto_ymax) || auto_ymax <= y_max_data) auto_ymax <- y_max_data * 1.2

      p_final_plot <- ggplot(summary_stats_for_plot, aes(x = Day, y = MeanValue, fill = PureGroup)) +
        geom_col(position = position_dodge(width = dodge_width), color = "black", width = 0.8) +
        geom_errorbar(aes(ymin = MeanValue, ymax = UpperError, group = PureGroup),
                      position = position_dodge(width = dodge_width), width = 0.25, na.rm = TRUE) +
        scale_fill_manual(name = "", values = pgcols,
                          breaks = final_order_levels, labels = legend_labels_map_plot) +
        scale_y_continuous(limits = c(0, auto_ymax), expand = expansion(mult = c(0, 0))) +
        theme_classic(base_size = 14) +
        theme(plot.title = element_markdown(hjust = 0, size = 20), legend.position = "right", legend.text = element_markdown(size = 12),
              axis.text = element_text(size = 12, color = "black"), axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
              panel.grid.major.y = element_line(color = "grey85"), panel.grid.minor.y = element_line(color = "grey90"),
              panel.grid.major.x = element_line(color = "grey85", linetype = "dotted"), panel.grid.minor.x = element_line(color = "grey90", linetype = "dotted"))

      signif_caption <- if (signif_style_param == "asterisks" && !is.null(stat.test) && nrow(stat.test) > 0) {
        p_col_name <- if("p.adj" %in% names(stat.test)) "p.adj" else "p.value"
        if(any(stat.test[[p_col_name]] < 0.05, na.rm=TRUE)) {
          "Significance: * p < 0.05, ** p < 0.01, *** p < 0.001"
        } else { "" }
      } else { "" }

      p_final_plot <- p_final_plot + labs(title = if (!is.null(plot_title_input)) italicize_markdown(plot_title_input) else "",
                                          y = "Absorbance (492 nm)", x = NULL,
                                          caption = signif_caption)

      # Calculate y_step early for both letter and asterisk styles
      y_max_data_for_step <- max(summary_stats_for_plot$UpperError, na.rm = TRUE)
      y_step <- y_max_data_for_step * 0.22
      if (is.na(y_step) || y_step == 0 || y_step < 0.08) y_step <- 0.08

      if (!is.null(stat.test) && nrow(stat.test) > 0) {
        if (signif_style_param == "letters") {
          signif_letters <- if ("Signif" %in% names(stat.test)) {
            stat.test %>%
              mutate(
                Day = as.character(Day),
                PureGroup = as.character(PureGroup)
              )
          } else {
            stat.test %>%
              group_by(Day) %>%
              do({
                p_col_name <- if("p.adj" %in% names(.)) "p.adj" else "p.value"
                p_values <- .[[p_col_name]]
                names(p_values) <- paste(.$group1, .$group2, sep = "-")
                letters_res <- multcompLetters(p_values)
                data.frame(PureGroup = names(letters_res$Letters), Signif = letters_res$Letters, stringsAsFactors = FALSE)
              }) %>% ungroup()
          }

          if(nrow(signif_letters) > 0) {
            summary_with_signif <- summary_stats_for_plot %>%
              mutate(Day = as.character(Day), PureGroup = as.character(PureGroup)) %>%
              left_join(signif_letters, by = c("Day", "PureGroup")) %>%
              mutate(Signif = ifelse(is.na(Signif), "", Signif)) %>%
              mutate(
                Day = factor(Day, levels = days_order_param),
                PureGroup = factor(PureGroup, levels = final_order_levels)
              )

            p_final_plot <- p_final_plot + geom_text(data = summary_with_signif, aes(label = Signif, y = UpperError + (y_step * 0.4), group = PureGroup),
                                                     position = position_dodge(width = dodge_width),
                                                     vjust = 0, size = 5, fontface = "bold", na.rm = TRUE, show.legend = FALSE)
          }

        } else {
          # handle non-letter significance styles: asterisks, p-values, or none
          if (signif_style_param != "none") {
            p_col_name <- if("p.adj" %in% names(stat.test)) "p.adj" else "p.value"
            stat.test_filtered <- stat.test %>% filter(.data[[p_col_name]] < 0.05)

            if(nrow(stat.test_filtered) > 0) {
              n_groups <- length(final_order_levels)
              x_coords <- summary_stats_for_plot %>%
                mutate(
                  day_num = as.numeric(Day),
                  group_num = as.numeric(factor(PureGroup, levels = final_order_levels))
                ) %>%
                mutate(
                  x_pos = day_num - (dodge_width/2) + ((group_num - 0.5) * dodge_width / n_groups)
                ) %>%
                dplyr::select(Day, PureGroup, x_pos)

              # y_step already calculated at the beginning for consistent spacing

              y_positions <- summary_stats_for_plot %>%
                group_by(Day) %>%
                summarise(max_y = max(UpperError, na.rm = TRUE)) %>%
                ungroup()

              signif_final <- stat.test_filtered %>%
                left_join(x_coords, by = c("Day", "group1" = "PureGroup")) %>% rename(xmin = x_pos) %>%
                left_join(x_coords, by = c("Day", "group2" = "PureGroup")) %>% rename(xmax = x_pos) %>%
                left_join(y_positions, by = "Day") %>%
                filter(!is.na(xmin), !is.na(xmax)) %>%
                arrange(Day, xmax - xmin) %>%
                group_by(Day) %>%
                mutate(
                  y.position = max_y + (row_number() * y_step),
                  label = if (signif_style_param == "pvalues") sprintf("% .3g", .data[[p_col_name]]) else case_when(
                    .data[[p_col_name]] < 0.001 ~ "***",
                    .data[[p_col_name]] < 0.01  ~ "**",
                    TRUE ~ "*"
                  )
                ) %>%
                ungroup()

              if(nrow(signif_final)>0){
                p_final_plot <- p_final_plot +
                  geom_segment(data = signif_final, aes(x = xmin, xend = xmax, y = y.position, yend = y.position), inherit.aes = FALSE, size = 0.6) +
                  geom_segment(data = signif_final, aes(x = xmin, xend = xmin, y = y.position, yend = y.position - (y_step * 0.25)), inherit.aes = FALSE, size = 0.6) +
                  geom_segment(data = signif_final, aes(x = xmax, xend = xmax, y = y.position, yend = y.position - (y_step * 0.25)), inherit.aes = FALSE, size = 0.6) +
                  geom_text(data = signif_final, aes(x = (xmin + xmax) / 2, y = y.position + (y_step * 0.45), label = label), vjust = 0, size = 4, inherit.aes = FALSE)
              }
            }
          }
        }
      }

      return(p_final_plot)
    }

    output$normality_controls_ui <- renderUI({
      data <- data_for_normality_check()
      req(data)
      available_days <- unique(as.character(data$Day))

      tagList(
        selectInput(ns("day_for_normality"), "Select Day for Diagnosis:", choices = available_days),
        actionButton(ns("run_normality_check"), "Generate Diagnosis", icon = icon("check-circle"))
      )
    })

    observeEvent(input$run_normality_check, {
      req(input$day_for_normality)
      all_data_norm <- data_for_normality_check()
      req(all_data_norm)

      output_dir <- "normality_diagnosis"
      dir.create(output_dir, showWarnings = FALSE)

      day_data <- all_data_norm %>% filter(Day == input$day_for_normality)
      groups_in_day <- unique(as.character(day_data$PureGroup))

      cat(paste("\n\n--- NORMALITY DIAGNOSIS FOR DAY:", input$day_for_normality, "---\n"))

      for (group_name in groups_in_day) {
        group_values <- day_data %>% filter(PureGroup == group_name) %>% pull(Value)

        cat(paste("\n  --- Group:", group_name, "---\n"))

        if (length(group_values) < 3) {
          cat("    Sample too small for tests and plots (N < 3).\n")
          next
        }

        shapiro_p_value <- NA
        tryCatch({
          shapiro_result <- shapiro.test(group_values)
          shapiro_p_value <- shapiro_result$p.value
          cat(sprintf("    Shapiro-Wilk: W = %.4f, p = %.4f %s\n",
                      shapiro_result$statistic, shapiro_p_value,
                      ifelse(shapiro_p_value < 0.05, "-> (Non Normal)", "-> (Normal)")))
        }, error = function(e) { cat(paste("    Shapiro-Wilk: Error -", e$message, "\n")) })

        mean_val <- mean(group_values, na.rm = TRUE)
        sd_val <- sd(group_values, na.rm = TRUE)

        plot_df <- data.frame(Value = group_values)
        p_norm <- ggplot(plot_df, aes(x = Value)) +
          geom_histogram(aes(y = after_stat(density)), bins = 10, fill = "lightblue", color = "black", alpha = 0.7) +
          stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color = "red", linewidth = 1.2) +
          labs(
            title = paste("Normality Diagnosis:", group_name),
            subtitle = paste("Day:", input$day_for_normality, "| Shapiro p-value =", round(shapiro_p_value, 4)),
            x = "Observed Value",
            y = "Density"
          ) +
          theme_bw(base_size = 14)

        safe_group_name <- gsub("[^A-Za-z0-9_.-]", "_", group_name)
        safe_day_name <- gsub("[^A-Za-z0-9_.-]", "_", input$day_for_normality)

        file_name <- paste0("Normality_Day-", safe_day_name, "_Group-", safe_group_name, ".png")
        file_path <- file.path(output_dir, file_name)

        tryCatch({
          ggsave(filename = file_path, plot = p_norm, width = 8, height = 6, units = "in", dpi = 150)
          cat(paste("    -> Graph saved in:", file_path, "\n"))
        }, error = function(e) {
          cat(paste("    ! ERROR saving Graph:", e$message, "\n"))
        })
      }

      showNotification(paste("Diagnostic graphs saved in the folder:", output_dir), type = "message", duration = 8)
    })

    get_corrected_data <- function() {
      corrected_data_list <- lapply(names(all_data$files), function(fname) {
        plate_data <- all_data$files[[fname]]
        if (is.null(plate_data$wells) || nrow(plate_data$wells) == 0) return(NULL)

        mean_blank_value <- 0
        if (length(plate_data$blank) > 0 && !is.null(plate_data$vals)) {
          blank_values <- plate_data$vals[plate_data$blank]
          if(any(!is.na(blank_values))) mean_blank_value <- mean(blank_values, na.rm = TRUE)
        }

        corrected_wells_df <- plate_data$wells
        corrected_wells_df$Value <- as.numeric(corrected_wells_df$Value) - mean_blank_value
        corrected_wells_df$Value[corrected_wells_df$Value < 0] <- 0
        return(corrected_wells_df)
      })
      do.call(rbind, corrected_data_list)
    }

    generate_dynamic_plot <- function(selected_pure_group_names_dyn, group_order_orig_names_dyn, plot_title_dyn, test_type_filter_dyn, custom_replica_mode_dyn = "duo", analysis_method_param_dyn = "mixed_anova", signif_style_param_dyn = "letters") {
      all_data_combined_dynamic <- get_corrected_data()
      internal_plot_generation(all_data_combined_dynamic, selected_pure_group_names_dyn, group_order_orig_names_dyn, plot_title_dyn, test_type_filter_dyn, custom_ymax = NULL, custom_replica_mode_dyn, days_order_param = analysis_days(), analysis_method_param = analysis_method_param_dyn, signif_style_param = signif_style_param_dyn, custom_colors = plotly_group_colors())
    }

    generate_custom_plot <- function(groups_pure_names_custom, order_orig_names_custom, title_custom, test_filter_custom, custom_replica_mode = "duo", days_order_param, analysis_method_param = "mixed_anova", signif_style_param = "letters") {
      all_data_combined_custom <- get_corrected_data()
      internal_plot_generation(all_data_combined_custom, groups_pure_names_custom, order_orig_names_custom, title_custom, test_filter_custom, custom_ymax = NULL, custom_replica_mode, days_order_param = days_order_param, analysis_method_param = analysis_method_param, signif_style_param = signif_style_param, custom_colors = plotly_group_colors())
    }

    output$dynamic_plot <- renderPlot({
      current_plot_params <- dynamic_plot_params()

      if(is.null(current_plot_params$groups) || length(current_plot_params$groups) == 0){
        plot.new()
        text(0.5, 0.5, "No Group Selected.", cex = 1.2)
        return()
      }

      p_drawn_dynamic <- generate_dynamic_plot(
        selected_pure_group_names_dyn = current_plot_params$groups,
        group_order_orig_names_dyn = current_plot_params$order,
        plot_title_dyn = input$plot_name,
        test_type_filter_dyn = current_plot_params$test,
        custom_replica_mode_dyn = current_plot_params$replica_mode,
        analysis_method_param_dyn = current_plot_params$analysis_method,
        signif_style_param_dyn = current_plot_params$signif_style
      )

      if (is.null(p_drawn_dynamic)) {
        plot.new()
        text(0.5, 0.5, "No Data to Plot.", cex = 1.2)
      } else {
        print(p_drawn_dynamic)
      }
    }, height=500)

    output$dynamic_plot_ui <- renderUI({
      plotly::plotlyOutput(ns("dynamic_plotly"), height = "500px")
    })

    output$dynamic_plotly <- plotly::renderPlotly({
      plot_color_token()  # Dependency on color token to trigger re-renders
      current_plot_params <- dynamic_plot_params()
      if(is.null(current_plot_params$groups) || length(current_plot_params$groups) == 0){
        return(NULL)
      }

      p_drawn_dynamic <- generate_dynamic_plot(
        selected_pure_group_names_dyn = current_plot_params$groups,
        group_order_orig_names_dyn = current_plot_params$order,
        plot_title_dyn = input$plot_name,
        test_type_filter_dyn = current_plot_params$test,
        custom_replica_mode_dyn = current_plot_params$replica_mode,
        analysis_method_param_dyn = current_plot_params$analysis_method,
        signif_style_param_dyn = current_plot_params$signif_style
      )

      if (is.null(p_drawn_dynamic)) return(NULL)
      
      # Convert ggplot to plotly with custom hover and remove download button
      py <- ggplotly(p_drawn_dynamic, tooltip = c("x", "y", "fill")) %>% 
        layout(legend = list(orientation = "v"), 
               hovermode = "closest",
               xaxis = list(title = "", zeroline = FALSE),
               yaxis = list(title = "Absorbance (492 nm)", zeroline = FALSE),
               dragmode = "zoom")  # Enable dragging for zoom; text annotations are natively draggable
      
      # Hide plotly's default camera and download buttons, add dragging mode button
      py %>% config(displayModeBar = TRUE, modeBarButtonsToRemove = list("pan2d", "lasso2d", "resetScale2d"),
                    toImageButtonOptions = list(format = "png", filename = "plot"))
    })

    # Capture current dynamic plot as a static ggplot (snapshot)
    observeEvent(input$capture_snapshot, {
      current_plot_params <- dynamic_plot_params()
      if(is.null(current_plot_params$groups) || length(current_plot_params$groups) == 0) {
        showNotification("No groups selected to capture.", type = "warning")
        return()
      }

      p_drawn_dynamic <- generate_dynamic_plot(
        selected_pure_group_names_dyn = current_plot_params$groups,
        group_order_orig_names_dyn = current_plot_params$order,
        plot_title_dyn = input$plot_name,
        test_type_filter_dyn = current_plot_params$test,
        custom_replica_mode_dyn = current_plot_params$replica_mode,
        analysis_method_param_dyn = current_plot_params$analysis_method,
        signif_style_param_dyn = current_plot_params$signif_style
      )

      if (is.null(p_drawn_dynamic)) {
        showNotification("Nothing to capture.", type = "warning")
        return()
      }

      # apply axis labels from user inputs (if provided)
      xlab_val <- if (!is.null(input$x_axis_label) && nzchar(input$x_axis_label)) input$x_axis_label else NULL
      ylab_val <- if (!is.null(input$y_axis_label) && nzchar(input$y_axis_label)) input$y_axis_label else NULL

      p_final_snapshot <- p_drawn_dynamic + labs(x = xlab_val, y = ylab_val)
      final_plot_gg(p_final_snapshot)
      showNotification("Snapshot captured as Final Graph.", type = "message")
    })

    output$final_static_plot_ui <- renderUI({
      if (is.null(final_plot_gg())) return(NULL)
      plotOutput(ns("final_static_plot"), height = "600px")
    })

    output$final_static_plot <- renderPlot({
      p <- final_plot_gg()
      req(p)
      print(p)
    }, height = 600)

    # Download handlers for the captured static plot
    output$download_final_png <- downloadHandler(
      filename = function() { paste0("FinalPlot_", Sys.Date(), ".png") },
      content = function(file) {
        p <- final_plot_gg()
        req(p)
        ggplot2::ggsave(filename = file, plot = p, device = "png", width = 10, height = 7, units = "in", dpi = 300)
      }
    )

    output$download_final_pdf <- downloadHandler(
      filename = function() { paste0("FinalPlot_", Sys.Date(), ".pdf") },
      content = function(file) {
        p <- final_plot_gg()
        req(p)
        ggplot2::ggsave(filename = file, plot = p, device = "pdf", width = 10, height = 7, units = "in")
      }
    )

    output$download_final_tiff <- downloadHandler(
      filename = function() { paste0("FinalPlot_", Sys.Date(), ".tiff") },
      content = function(file) {
        p <- final_plot_gg()
        req(p)
        ggplot2::ggsave(filename = file, plot = p, device = "tiff", width = 10, height = 7, units = "in", dpi = 300)
      }
    )

    observeEvent(input$selected_plot, {})

    # 1. Improved Color Picker UI (The Wheel)
    # 1. Fixed Color Picker UI (The Wheel)
    

    # Duplicate the color picker inline so it's visible next to the dynamic graph
    output$plotly_color_picker_inline <- renderUI({
      groups_available <- get_group_names()
      if (length(groups_available) == 0) return(NULL)
      pure_group_names <- remove_asterisks(remove_html(groups_available))
      tagList(
        h5("Individual Colors (Inline)"),
        selectInput(ns("selected_color_group"), "Select Column:", choices = setNames(pure_group_names, groups_available)),
        colourpicker::colourInput(ns("color_wheel"), "Pick Color:", value = "#1f77b4", showColour = "background", palette = "square", returnName = FALSE),
        actionButton(ns("apply_picker_color"), "Apply to Selected Column", class = "btn-primary", width = "100%"),
        hr(),
        h5("Palette Presets"),
        p(style = "font-size: 12px; color: #666;", "Apply a palette to all columns at once:"),
        div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 8px;",
            actionButton(ns("preset_viridis"), "Viridis", class = "btn-sm btn-info", width = "100%", title = "Perceptually uniform"),
            actionButton(ns("preset_cb"), "Colorblind", class = "btn-sm btn-info", width = "100%", title = "Blue-Orange friendly"),
            actionButton(ns("preset_tableau"), "Tableau", class = "btn-sm btn-info", width = "100%", title = "Professional"),
            actionButton(ns("preset_material"), "Material", class = "btn-sm btn-info", width = "100%", title = "Modern vibrant")
        ),
        br(),
        actionButton(ns("reset_colors"), "Reset All Colors", class = "btn-sm", width = "100%")
      )
    })
    
    # 2. Color Application Logic
    
    # Apply individual color to selected column
    observeEvent(input$apply_picker_color, {
      if (is.null(input$selected_color_group) || input$selected_color_group == "") {
        showNotification("Please select a column first", type = "warning")
        return()
      }
      if (is.null(input$color_wheel) || input$color_wheel == "") {
        showNotification("Please select a color first", type = "warning")
        return()
      }
      
      current_cols <- plotly_group_colors()
      current_cols[[input$selected_color_group]] <- input$color_wheel
      plotly_group_colors(current_cols)
      plot_color_token(plot_color_token() + 1)
      showNotification(paste("Color applied to", input$selected_color_group), type = "message", duration = 2)
    })
    
    # Preset colors - Viridis (apply to all)
    observeEvent(input$preset_viridis, {
      viridis_cols <- c("#440154", "#31688e", "#35b779", "#fde724")
      groups_available <- get_group_names()
      if (length(groups_available) == 0) return()
      pure_group_names <- remove_asterisks(remove_html(groups_available))
      
      current_cols <- plotly_group_colors()
      for (i in seq_along(pure_group_names)) {
        current_cols[[pure_group_names[i]]] <- viridis_cols[(i - 1) %% length(viridis_cols) + 1]
      }
      plotly_group_colors(current_cols)
      plot_color_token(plot_color_token() + 1)
      showNotification("Viridis palette applied to all columns", type = "message", duration = 2)
    })
    
    # Preset colors - Colorblind friendly (apply to all)
    observeEvent(input$preset_cb, {
      cb_cols <- c("#0173B2", "#029E73", "#CC78BC", "#DE8F05", "#CA9161", "#56B4E9")
      groups_available <- get_group_names()
      if (length(groups_available) == 0) return()
      pure_group_names <- remove_asterisks(remove_html(groups_available))
      
      current_cols <- plotly_group_colors()
      for (i in seq_along(pure_group_names)) {
        current_cols[[pure_group_names[i]]] <- cb_cols[(i - 1) %% length(cb_cols) + 1]
      }
      plotly_group_colors(current_cols)
      plot_color_token(plot_color_token() + 1)
      showNotification("Colorblind-friendly palette applied to all columns", type = "message", duration = 2)
    })
    
    # Preset colors - Tableau (apply to all)
    observeEvent(input$preset_tableau, {
      tableau_cols <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2")
      groups_available <- get_group_names()
      if (length(groups_available) == 0) return()
      pure_group_names <- remove_asterisks(remove_html(groups_available))
      
      current_cols <- plotly_group_colors()
      for (i in seq_along(pure_group_names)) {
        current_cols[[pure_group_names[i]]] <- tableau_cols[(i - 1) %% length(tableau_cols) + 1]
      }
      plotly_group_colors(current_cols)
      plot_color_token(plot_color_token() + 1)
      showNotification("Tableau palette applied to all columns", type = "message", duration = 2)
    })
    
    # Preset colors - Material Design (apply to all)
    observeEvent(input$preset_material, {
      material_cols <- c("#F44336", "#2196F3", "#4CAF50", "#FFC107", "#9C27B0", "#00BCD4", "#FF5722")
      groups_available <- get_group_names()
      if (length(groups_available) == 0) return()
      pure_group_names <- remove_asterisks(remove_html(groups_available))
      
      current_cols <- plotly_group_colors()
      for (i in seq_along(pure_group_names)) {
        current_cols[[pure_group_names[i]]] <- material_cols[(i - 1) %% length(material_cols) + 1]
      }
      plotly_group_colors(current_cols)
      plot_color_token(plot_color_token() + 1)
      showNotification("Material Design palette applied to all columns", type = "message", duration = 2)
    })
    
    observeEvent(input$reset_colors, {
      plotly_group_colors(list())
      plot_color_token(plot_color_token() + 1)
      showNotification("All colors reset to default", type = "message", duration = 2)
    })
    
    # Sync hex code input with color wheel
    observeEvent(input$color_wheel, {
      updateTextInput(session, "hex_code_input", value = input$color_wheel)
    })
    
    # Update color wheel when hex code is manually entered
    observeEvent(input$hex_code_input, {
      hex_val <- input$hex_code_input
      if (grepl("^#[0-9A-Fa-f]{6}$", hex_val)) {
        shinyjs::runjs(sprintf("$('#%s').colourpicker('setColor', '%s');", ns("color_wheel"), hex_val))
      }
    })
    
    # Eyedropper tool - use browser's native color picker
    observeEvent(input$eyedropper_tool, {
      shinyjs::runjs("
        const colorInput = document.createElement('input');
        colorInput.type = 'color';
        colorInput.addEventListener('change', function(e) {
          Shiny.setInputValue('" %+% ns("color_wheel") %+% "', e.target.value);
        });
        colorInput.click();
      ")
    })
    
    # 3. Ensure internal_plot_generation uses these colors correctly
    # (Inside your existing internal_plot_generation function, find the color logic:)
    # pgcols <- plotly_group_colors() 
    # ... and ensure it maps pgcols correctly to scale_fill_manual

    

    # original create_download_plot removed; final snapshot download handlers are used instead
  })
}
