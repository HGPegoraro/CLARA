library(shiny)
library(shinyjs)

options(shiny.maxRequestSize = 30 * 1024^2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #title-container {
        display: flex;
        align-items: left;
      }

      #title-container img {
        margin-right: 20px;
      }

      #title-container .text-container {
        display: flex;
        flex-direction: column;
      }

      #title-container h1 {
        margin-bottom: 0;
      }
    "))
  ),
  br(),
  div(id = "title-container",
      img(src = "CLARA_LOGO_V2.png", height = "150px")
  ),
  hr(),
  fluidRow(
    column(6,
           selectInput("global_excel_format_selector", "Selecionar Formato do Arquivo Excel:",
                       choices = list(
                         "ELx800 (Lab. 3)" = "lab_3",
                         "Biochrom EZ Read 400 (Lab. 6)" = "lab_6",
                         "KASUAKI (Lab. 7)" = "lab_7"
                       ),
                       selected = "lab_3")
    )
  ),
  tabsetPanel(
    id = "main_tabs",
    elisa_tabPanel("elisa_module"),
    bca_tabPanel("bca_module")
  ),
  hr()
)

server <- function(input, output, session) {

  selected_global_excel_format <- reactive({ input$global_excel_format_selector })

  elisa_server("elisa_module", global_excel_format_reactive = selected_global_excel_format)
  bca_server("bca_module", global_excel_format_reactive = selected_global_excel_format)
}

shinyApp(ui = ui, server = server)
