##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Shiny.
##----------------------------------------------------------------------------##

values <- reactiveValues(data = NULL, IC_names = NULL, Stat = NULL, Annotation = NULL, UMAP = NULL,
                         annotation_for_output = list(), low_image = NULL, HD_image = NULL)

Launch_analysis <- reactive({
  if (grep('.RDS',input$input_file$datapath) == 1) {
    
    data <- readRDS(input$input_file$datapath)
    
    if (is.null(data@misc$annotation)){
      row_names = names(data@misc)[grep('IC_', names(data@misc))]
      data@misc$annotation = matrix(data = "", nrow = length(row_names), ncol = 3)
      rownames(data@misc$annotation) = row_names
      colnames(data@misc$annotation) = c('Use','Type','Annotation')
      data@misc$annotation[,'Use'] = TRUE
    }
    
    return(data)
    
  } else {
    
    shinyalert("Wrong format", "requires .RDS.", type = "error")
    
    return(NULL)
  }
  
})

observeEvent(input$input_file, {
  
  values$annotation_for_output = list()
  values$data = NULL
  values$IC_names = NULL
  values$Stat = NULL
  values$Annotation = NULL
  values$UMAP = NULL
  values$low_image = NULL
  values$HD_image = NULL
  
  values$data = Launch_analysis()
  
  if (!is.null(values$data@reductions$umap)){
    values$UMAP = values$data
  }
  
  values$IC_names = rownames(values$data@misc$annotation)
  
  values$Stat = values$data@misc[["GeneAndStat"]]
  
  updateSelectizeInput(session, "Ic_list", label = "list of IC",
                       choices = values$IC_names,
                       selected = NULL)
  
  values$Annotation = values$data@misc$annotation
  
  # Get All annotations and their associated ICs
  list_names_IC = str_split(values$Annotation[,"Type"], pattern = ',', n = Inf, simplify = FALSE)
  
  for (i in 1:length(list_names_IC)) {
    list_annotation = list_names_IC[[i]]
    for (j in list_annotation) {
      if(is.null(values$annotation_for_output[[j]]) && j != ""){
        j <- gsub("\\+", "\\\\+", j)
        values$annotation_for_output[[j]] = na.omit(rownames(values$data@misc$annotation)[grep("TRUE", values$Annotation)[grep(j, values$Annotation)-length(values$Annotation[,'Use'])]])
      }
    }
  }
  
  values$low_image = raster2uri(raster::as.raster(values$data@images$slice1@image))
})

observeEvent(input$input_image, {
  values$HD_image <- raster2uri(raster::as.raster(readPNG(input$input_image$datapath)))
})