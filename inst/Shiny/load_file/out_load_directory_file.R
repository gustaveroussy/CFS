##----------------------------------------------------------------------------##
## Tab: Load data
##
## Select local file
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## treat the input from local file
##----------------------------------------------------------------------------##

observeEvent(input$input_file_local, {
  volumes <- getVolumes()
  
  shinyFileChoose(input, 'input_file_local', roots=volumes(), session=session)
  if(!is.null(input$input_file_local)){
    path = parseFilePaths(getVolumes(), input$input_file_local)
    if (length(path$datapath) != 0){
      print(input$input_file_local)
      if (length(grep('.RDS',toupper(path[length(path$datapath)]))) != 0){
        
        values$annotation_for_output = list()
        values$data = NULL
        values$IC_names = NULL
        values$Stat = NULL
        values$Annotation = NULL
        values$low_image = NULL
        values$HD_image = NULL
        values$HD_image_2 = NULL
        
        values$data = readRDS(path$datapath)
        
        if(is.null(values$data@misc$GeneAndStat$kurtosis_value)){
          values$IC_names = names(values$data@misc$GeneAndStat$Kurtosis_ICs)[values$data@misc$GeneAndStat$Kurtosis_ICs > 3]
        } else {
          values$IC_names = names(values$data@misc$GeneAndStat$Kurtosis_ICs)[values$data@misc$GeneAndStat$Kurtosis_ICs > values$data@misc$GeneAndStat$kurtosis_value]
        }
        
        if (is.null(values$data@misc$annotation)){
          row_names = values$IC_names
          values$data@misc$annotation = matrix(data = "", nrow = length(row_names), ncol = 3)
          rownames(values$data@misc$annotation) = row_names
          colnames(values$data@misc$annotation) = c('Use','Type','Annotation')
          values$data@misc$annotation[,'Use'] = TRUE
        }
        
        values$data@misc$annotation = as.matrix(values$data@misc$annotation)
        
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
          values$low_image = c(raster2uri(raster::as.raster(values$data@images[[1]]@image)))
        }
        
      } else {
        shinyalert("Wrong format", "requires RDS", type = "error")
      }
    }
  }
})