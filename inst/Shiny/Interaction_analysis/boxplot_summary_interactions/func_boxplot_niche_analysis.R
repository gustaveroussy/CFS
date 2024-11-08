##----------------------------------------------------------------------------##
## boxplot for each samples
##----------------------------------------------------------------------------##

fig_distance_boxplot_niche_analysis <- function(){
  
  req(values$distances)
  req(values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]])
  
  return(NULL)
}
