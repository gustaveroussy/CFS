########################################
# reactive of the tissue coordinates to use
########################################

TissueCoordinates <- reactive({
  TC = list()
  
  for(image in names(values$data@images)){
    c = GetTissueCoordinates(values$data, image = image)
    names(c)[names(c) == "x"] <- "imagerow"
    names(c)[names(c) == "y"] <- "imagecol"
    
    if(is.null(values$HD_image)) {
      if(class(values$data@images[[image]])[1] == "VisiumV2"){
        c[,"imagerow"] <- c[,"imagerow"] * values$data@images[[image]]@scale.factors$lowres
        c[,"imagecol"] <- c[,"imagecol"] * values$data@images[[image]]@scale.factors$lowres
      }
    } else {
      if(class(values$data@images[[image]])[1] == "VisiumV2" | class(values$data@images[[image]])[1] == "VisiumV1"){
        c[,"imagerow"] <- c[,"imagerow"] * values$data@images[[image]]@scale.factors$hires
        c[,"imagecol"] <- c[,"imagecol"] * values$data@images[[image]]@scale.factors$hires
      }
    }
    
    if(input$spatial_mirror_X){
      c$imagecol = c$imagecol * (-1)
    }
    if(input$spatial_mirror_Y){
      c$imagerow = c$imagerow * (-1)
    }
    if(input$spatial_flip){
      imagerow = c$imagerow
      c$imagerow = c$imagecol
      c$imagecol = imagerow
    }
    
    TC = append(TC,list(c))
  }
  
  names(TC) = names(values$data@images)
  
  return(TC)
})

# TissueCoordinatesggplot <- reactive({
#   coordinates = list()
#   
# 
#   
#   if(length(input$gene_projection_gene_choice) > 1){
#     
#     for(image in names(values$data@images)){
#       
#     }
#     
#     coordinates = lapply(input$Plot_image_spatial, function(sample){coordinates[[sample]] = GetTissueCoordinates(values$data,sample);coordinates[[sample]] = cbind(coordinates[[sample]],t(values$data@assays$SCT@data[input$gene_projection_gene_choice,rownames(coordinates[[sample]])])); colnames(coordinates[[sample]])[1:2] = c("imagerow","imagecol");return(coordinates[[sample]])})
#     
#   } else {
#     
#     coordinates = lapply(input$Plot_image_spatial, function(sample){coordinates[[sample]] = GetTissueCoordinates(values$data,sample);coordinates[[sample]] = cbind(coordinates[[sample]],values$data@assays$SCT@data[input$gene_projection_gene_choice, rownames(coordinates[[sample]])]); colnames(coordinates[[sample]])[1:2] = c("imagerow","imagecol");colnames(coordinates[[sample]])[length(colnames(coordinates[[sample]]))] = c(input$gene_projection_gene_choice);return(coordinates[[sample]])})
#     
#   }
#   
#   names(coordinates) = input$Plot_image_spatial
#   
#   coordinates = coordinates[[input$Plot_image_spatial[1]]]
#   
#   return(coordinates)
# })