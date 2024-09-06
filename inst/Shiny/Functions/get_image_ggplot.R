########################################
# reactive of the tissue coordinates to use
########################################

img_ggplot <- reactive({
  img = lapply(values$data@images,function(n){if("image" %in% slotNames(n)){return(n@image)}})
  return(img)
})