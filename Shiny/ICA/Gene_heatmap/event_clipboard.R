if (interactive()){
  observeEvent(input$clipa, clipr::write_clip(toString(names(GeneList_heatmap_IC()))))
}

if (interactive()){
  observeEvent(input$clipp, clipr::write_clip(toString(names(GeneList_heatmap_IC()[GeneList_heatmap_IC() > 0]))))
}