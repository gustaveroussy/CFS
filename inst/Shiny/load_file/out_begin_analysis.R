##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select file.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element to select data to load into Shiny.
##----------------------------------------------------------------------------##

values <- reactiveValues(data = NULL, IC_names = NULL, Stat = NULL, Annotation = NULL, UMAP = NULL,
                         annotation_for_output = list(), low_image = NULL, HD_image = NULL, HD_image_2 = NULL,
                         cropped_image = NULL, marker_gene = NULL)

Launch_analysis <- reactive({
  if (length(grep('.RDS',toupper(input$input_file$datapath))) != 0) {
    
    data <- readRDS(input$input_file$datapath)
    
    if(is.null(data@misc$GeneAndStat$kurtosis_value)){
      values$IC_names = names(data@misc$GeneAndStat$Kurtosis_ICs)[data@misc$GeneAndStat$Kurtosis_ICs > 3]
    } else {
      values$IC_names = names(data@misc$GeneAndStat$Kurtosis_ICs)[data@misc$GeneAndStat$Kurtosis_ICs > data@misc$GeneAndStat$kurtosis_value]
    }
    
    
    if (is.null(data@misc$annotation)){
      row_names = values$IC_names
      data@misc$annotation = matrix(data = "", nrow = length(row_names), ncol = 3)
      rownames(data@misc$annotation) = row_names
      colnames(data@misc$annotation) = c('Use','Type','Annotation')
      data@misc$annotation[,'Use'] = TRUE
    }
    
    data@misc$annotation = as.matrix(data@misc$annotation)
    
    return(data)
    
  } else {
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
  values$HD_image_2 = NULL
  
  values$data = Launch_analysis()
  
  if (!is.null(values$data)){
    if (!is.null(values$data@reductions$umap)){
      values$UMAP = values$data
    } else {
      values$UMAP = NULL
    }
    
    if (!is.null(values$data@misc$markers)){
      values$marker_gene = values$data@misc$markers
    } else {
      values$marker_gene = NULL
    }
    
    values$Stat = values$data@misc[["GeneAndStat"]]
    
    updateSelectizeInput(session, "Ic_list", label = "list of IC",
                         choices = values$IC_names,
                         selected = NULL)
    
    values$Annotation = values$data@misc$annotation
    
    # Get All annotations and their associated ICs
    list_names_IC = unique(unlist(str_split(values$Annotation[,"Type"], pattern = ',', n = Inf, simplify = FALSE)))
    
    for (list_annotation in list_names_IC) {
        if(list_annotation != ""){
          list_annotation <- gsub("\\+", "\\\\+", list_annotation)
          result = values$Annotation[,'Use'] == TRUE & values$Annotation[,'Type'] == list_annotation
          values$annotation_for_output[[list_annotation]] = names(result[result])
        }
    }
    
    
    if('image' %in% names(attributes(values$data@images[[1]]))){
      values$low_image = raster2uri(raster::as.raster(values$data@images$slice1@image))
    }
    
  } else {
    shinyalert("Wrong format", "requires .RDS.", type = "error")
  }
})

observeEvent(input$input_image, {
  values$HD_image = NULL
  if (length(grep('.JPG',toupper(input$input_image$datapath))) != 0) {
    values$HD_image <- raster2uri(raster::as.raster(readJPEG(input$input_image$datapath)))
  } else if (length(grep('.PNG',toupper(input$input_image$datapath))) != 0){
    values$HD_image <- raster2uri(raster::as.raster(readPNG(input$input_image$datapath)))
  } else {
    shinyalert("Oops!", "Wrong format (expecting .png or .jpg)", type = "error")
  }
})

observeEvent(input$input_image_2, {
  values$HD_image_2 = NULL
  if (length(grep('.JPG',toupper(input$input_image_2$datapath))) != 0) {
    values$HD_image_2 <- readJPEG(input$input_image_2$datapath)
  } else if (length(grep('.PNG',toupper(input$input_image_2$datapath))) != 0){
    values$HD_image_2 <- readPNG(input$input_image_2$datapath)
  } else {
    shinyalert("Oops!", "Wrong format (expecting .png or .jpg)", type = "error")
  }
})

images_names <- reactive({
  if(is.null(values$data)){
    return(NULL)
  }else{
    return(names(values$data@images))
  }
})

observe({
  updateSelectInput(
    session = getDefaultReactiveDomain(),
    "Plot_image_spatial",
    choices = images_names()
  )
})

observeEvent(input$Plot_image_spatial, {
  req(values$data)
  req(values$data@images[[input$Plot_image_spatial]])
  if("image" %in% slotNames(data@images[[input$Plot_image_spatial]])){
    values$low_image = raster2uri(raster::as.raster(values$data@images[[input$Plot_image_spatial]]@image))
  }
})
