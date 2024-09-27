##----------------------------------------------------------------------------##
## reset palette to default
##----------------------------------------------------------------------------##

observeEvent(input[["type_of_palette_to_modify"]],{
  req(input[["type_of_palette_to_modify"]])
  
  if(input[["type_of_palette_to_modify"]] == "Scatter"){

    saveRDS(values$palette,paste0(Shiny.options[["shiny_root"]], "/../tmp_data/palette_interaction.RDS"))
    
    if(file.exists(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/palette.RDS"))){
      values$palette = readRDS(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/palette.RDS"))

    } else {
      values$palette = readRDS(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/def_palette.RDS"))
    }

    lapply(1:length(values$palette), function(i) {
      updateColourInput(session, inputId = paste0("palette_colors_",i), paste0("Color ",i), value = values$palette[i])

    })
    
    i = length(values$palette) + 1

    while(!is.null(input[[paste0("palette_colors_",i)]])){
      removeUI(selector = paste0("#palette_colors_div_",i))

      i = i + 1
    }
    
    update_palette()
    
  } else if(input[["type_of_palette_to_modify"]] == "Interaction analysis") {
    
    saveRDS(values$palette,paste0(Shiny.options[["shiny_root"]], "/../tmp_data/palette.RDS"))

    if(file.exists(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/palette_interaction.RDS"))){
      values$palette = readRDS(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/palette_interaction.RDS"))

    } else {
      values$palette = readRDS(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/def_palette.RDS"))
    }
    
    lapply(1:length(values$palette), function(i) {

      updateColourInput(session, inputId = paste0("palette_colors_",i), paste0("Color ",i), value = values$palette[i])
    })
    
    i = length(values$palette) + 1
    
    while(!is.null(input[[paste0("palette_colors_",i)]])){

      removeUI(selector = paste0("#palette_colors_div_",i))
      i = i + 1
    }
    
    update_palette()
    
  }
  
})
