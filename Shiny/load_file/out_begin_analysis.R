##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Shiny.
##----------------------------------------------------------------------------##

Launch_analysis <- reactive({
  data <- readRDS(input$input_file$datapath)
  
  row_names = names(data@misc)[grep('IC_', names(data@misc))]
  data@misc$annotation = matrix(data = NA, nrow = length(row_names), ncol = 3)
  rownames(data@misc$annotation) = row_names
  colnames(data@misc$annotation) = c('Use','Type','Annotation')
  data@misc$annotation[,'Use'] = TRUE
  
  return(data)
})

values <- reactiveValues(data = NULL, IC_names = NULL, Stat = NULL, Annotation = NULL, UMAP = NULL)

observeEvent(input$input_file, {
  
  values$data = Launch_analysis()

  
  values$IC_names = rownames(values$data@misc$annotation)
  
  values$Stat = values$data@misc[["GeneAndStat"]]
  
  values$Annotation = values$data@misc$annotation
  
  updateSelectizeInput(session, "Ic_list", label = "list of IC",
                       choices = values$IC_names,
                       selected = NULL)
  
  if(!exists("values$data@misc$annotation")){
    values$data@misc$annotation = matrix(NA, nrow = length(values$IC_names), ncol = 3)
    rownames(values$data@misc$annotation) = values$IC_names
    colnames(values$data@misc$annotation) = c('Use','Type','Annotation')
  }
  
})