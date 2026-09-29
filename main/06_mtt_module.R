library(shiny)
library(DT)
library(dplyr)

mtt_tabPanel <- function(id, name = "MTT Assay") {
  ns <- NS(id)

  tabPanel(name, class = "mtt-module-tab",
           sidebarLayout(
             sidebarPanel(
               fileInput(ns("files"), "Import Excel (MTT)", multiple = TRUE, accept = ".xlsx, .xls"),
               selectInput(ns("selected_plate"), "Select Plate", choices = list("No Plate Available" = "NA")),
               hr(),
               textInput(ns("control_wells"), "Control wells (comma-separated)", placeholder = "A1,A2"),
               textInput(ns("blank_wells"), "Blank wells (comma-separated)", placeholder = "H12"),
               actionButton(ns("run_mtt"), "Run MTT Analysis"),
               hr(),
               fileInput(ns("load_state"), "Load Saved MTT Project (.rds)", accept = ".rds"),
               downloadButton(ns("save_state"), "Save MTT State")
             ),
             mainPanel(
               h4("MTT Results"),
               DTOutput(ns("mtt_results_table")),
               plotOutput(ns("mtt_percent_plot"))
             )
           )
  )
}

mtt_server <- function(id, global_excel_format_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    all_data <- reactiveValues(files = list())

    current_file <- reactive({ input$selected_plate })

    observeEvent(input$files, {
      req(input$files)
      reader_function <- GLOBAL_EXCEL_READERS[[ global_excel_format_reactive() ]]
      if (is.null(reader_function)) { showNotification("No reader defined for selected format", type = "error"); return() }

      loaded <- list()
      for (f in seq_len(nrow(input$files))) {
        fp <- input$files$datapath[f]
        nm <- input$files$name[f]
        vals <- tryCatch(reader_function(fp), error = function(e) NULL)
        if (!is.null(vals)) {
          loaded[[nm]] <- list(vals = vals, wells = data.frame(Well = names(vals), Value = as.numeric(vals), stringsAsFactors = FALSE))
        }
      }
      if (length(loaded) > 0) {
        all_data$files <- modifyList(all_data$files, loaded)
        updateSelectInput(session, "selected_plate", choices = names(all_data$files), selected = names(all_data$files)[1])
      }
    })

    observeEvent(input$run_mtt, {
      req(current_file(), current_file() != "NA")
      fname <- current_file()
      plate <- all_data$files[[fname]]
      if (is.null(plate) || is.null(plate$wells)) { showNotification("No plate data loaded.", type = "error"); return() }

      wells_df <- plate$wells
      wells_df$Value <- as.numeric(wells_df$Value)

      control_list <- trimws(unlist(strsplit(input$control_wells, ",")))
      blank_list <- trimws(unlist(strsplit(input$blank_wells, ",")))

      control_vals <- wells_df %>% filter(Well %in% control_list) %>% pull(Value)
      blank_vals <- wells_df %>% filter(Well %in% blank_list) %>% pull(Value)

      if (length(control_vals) == 0) { showNotification("No control wells found.", type = "error"); return() }
      if (length(blank_vals) == 0) { showNotification("No blank wells found.", type = "error"); return() }

      mean_control <- mean(control_vals, na.rm = TRUE)
      mean_blank <- mean(blank_vals, na.rm = TRUE)

      wells_df <- wells_df %>% mutate(Percent_Viability = (Value - mean_blank) / (mean_control - mean_blank) * 100)

      output$mtt_results_table <- renderDT({ datatable(wells_df, options = list(pageLength = 10)) })

      output$mtt_percent_plot <- renderPlot({
        library(ggplot2)
        ggplot(wells_df, aes(x = Well, y = Percent_Viability)) +
          geom_col(fill = "steelblue") +
          coord_flip() +
          labs(y = "% Viability", x = "Well") +
          theme_minimal()
      })

    })

    output$save_state <- downloadHandler(
      filename = function() { paste0("MTT_Project_", Sys.Date(), ".rds") },
      content = function(file) { saveRDS(list(files = all_data$files), file) }
    )

    observeEvent(input$load_state, {
      req(input$load_state)
      st <- tryCatch(readRDS(input$load_state$datapath), error = function(e) NULL)
      if (!is.null(st) && is.list(st) && "files" %in% names(st)) {
        all_data$files <- st$files
        updateSelectInput(session, "selected_plate", choices = names(all_data$files), selected = names(all_data$files)[1])
        showNotification("MTT project loaded.", type = "message")
      } else { showNotification("Invalid MTT state file.", type = "error") }
    })

  })
}
