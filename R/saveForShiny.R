######################################
# Classe of object saved
######################################

setClass("shiny_visium", slots=list(ica="list", images="list"))

############################
# Function to save for shiny
############################


saveForShiny <- function(object, path)
  {
  file <- new("shiny_visium",ica=object@misc, images=object@images)
  file@ica <- object@misc
  file@images <- object@images
  saveRDS(file, path)
}
