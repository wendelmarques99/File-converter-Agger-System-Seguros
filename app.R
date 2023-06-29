library(shiny)
library(shinyjs)
library(shinyalert)
library(magrittr)


ler_html <- function(files){
 
  seguradora <- rvest::read_html(files) %>%  
    rvest::html_elements("h5") %>% 
    purrr::pluck(2) %>% 
    rvest::html_text() %>% 
    stringr::str_extract("(?<=SEGURADORA:)(.*)(?=USUÁRIO)") %>% 
    stringr::str_trim()
  
  
  data <- rvest::read_html(files) %>% 
    rvest::html_table() %>% 
    purrr::pluck(1) %>% 
    dplyr::pull(`DATA EXTRATO`) %>% 
    lubridate::dmy(.)
  
    rvest::read_html(files) %>% 
    rvest::html_table() %>% 
    purrr::pluck(2) %>% 
    dplyr::mutate(seguradora, data)
}

ui <- fluidPage(
  fluidRow(
    column(12,
           align = "center",
           
           fileInput("notas",
                     NULL,
                     label = "",
                     multiple = TRUE,
                     buttonLabel = "Arquivos Agger",
                     placeholder = "",
                     accept = ".pdf"),
           
           # Botao de dowload da nota de corretagem 
           downloadButton("download1", "Exportar em excel",
                          class = "butt", 
                          icon = icon("table")),
           tags$head(tags$style(".butt{background-color:#00bc8c;} .butt{color: white;} .butt{font-style: Courier New};}")), 
           useShinyalert(),
           tags$head(tags$style(".butt1{background-color:#00bc8c;} .butt1{color: white;} .butt1{font-style: Courier New};}"))
    )
  )
)

server <- function(input, output, session) {
  
  dados <- shiny::reactive({
    
    req(input$notas)
    
    BD_PDFS <- input$notas$datapath %>% 
      purrr::map_dfr(., ler_html) 
  })
  
  # Exportar notas em excel -------------------------------------------------
  output$download1 <- downloadHandler(
    filename <- function(){
      paste0("Agger-", Sys.Date(), ".csv")
    },
    content <- function(file) {
      shiny::withProgress(
        message = "Baixando base",
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(9/10)
          write.csv(dados(), file, row.names = FALSE)
        }
      )
    }
  )
  
}

shiny::shinyApp(ui, server)

