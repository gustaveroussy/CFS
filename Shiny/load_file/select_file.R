##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Shiny.
##----------------------------------------------------------------------------##

# input directory

output[["load_data_UI"]] <- renderUI({
  fluidPage( # Application title
    mainPanel(
      shinyDirButton("dir", "Input directory", "Upload"),
      verbatimTextOutput("dir", placeholder = TRUE)  
    ))
})

shinyDirChoose(
  input,
  'dir',
  roots = c(home = '/'),
  filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
)

global <- reactiveValues(datapath = getwd())

dir <- reactive(input$dir)

output$dir <- renderText({
  global$datapath
})

observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$dir
             },
             handlerExpr = {
               if (!"path" %in% names(dir())) return()
               home <- normalizePath("/")
               global$datapath <-
                 file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
               message(global$datapath)
             })

# output directory

output[["output_data_UI"]] <- renderUI({
  fluidPage( # Application title
    mainPanel(
      shinyDirButton("dir_out", "Output directory", "Upload"),
      verbatimTextOutput("dir_out", placeholder = TRUE)  
    ))
})

shinyDirChoose(
  input,
  'dir_out',
  roots = c(home_2 = '/'),
  filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
)

global_2 <- reactiveValues(output_datapath = getwd())

dir_out <- reactive(input$dir_out)

output$dir_out <- renderText({
  global_2$output_datapath
})

observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$dir_out
             },
             handlerExpr = {
               if (!"path" %in% names(dir_out())) return()
               home_2 <- normalizePath("/")
               global_2$output_datapath <-
                 file.path(home_2, paste(unlist(dir_out()$path[-1]), collapse = .Platform$file.sep))
               message(global_2$output_datapath)
             })

input_path <- reactive({
  path <- global$datapath
  return(path)
})

output_path <- reactive({
  path <- global_2$output_datapath
  return(path)
})