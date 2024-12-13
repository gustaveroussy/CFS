##----------------------------------------------------------------------------##
## IC spatial
##----------------------------------------------------------------------------##

observeEvent(input$preprocessing_rename_sample_action_button,{
  req(values$data)
  
  sample = input$preprocessing_sample_to_rename
  rename = input$preprocessing_rename_text
  
  # rename assays
  for(i in names(values$data@assays)){
    values$data@assays[[i]] = RenameCells(values$data@assays[[i]], new.names = gsub(sample,rename,Cells(values$data)))
  }
  
  #rename metadata
  values$data@meta.data$sample[which(values$data@meta.data$sample == sample)] <- rename
  rownames(values$data@meta.data) = gsub(sample,rename,rownames(values$data@meta.data))
  
  #rename identities
  names(values$data@active.ident) = gsub(sample,rename,names(values$data@active.ident))
  
  #rename graph
  for(i in names(values$data@graphs)){
    for(k in 1:length(values$data@graphs[[i]]@Dimnames)){
      values$data@graphs[[i]]@Dimnames[[k]] = gsub(sample,rename,values$data@graphs[[i]]@Dimnames[[k]])
    }
  }
  
  # rename reduction
  for(i in names(values$data@reductions)){
    values$data@reductions[[i]] = RenameCells(values$data@reductions[[i]], new.names = gsub(sample,rename,Cells(values$data)))
  }
  
  # rename images
  names(values$data@images)[names(values$data@images) == sample] <- rename
  values$data@images[[rename]]@key = paste0(tolower(rename),"_")
  rownames(values$data@images[[rename]]@coordinates) = gsub(sample,rename,rownames(values$data@images[[rename]]@coordinates))
  
  # rename distances
  for(i in names(values$data@misc[["distances"]])){
    if(sample %in% names(values$data@misc[["distances"]][[i]])){
      names(values$data@misc[["distances"]][[i]])[names(values$data@misc[["distances"]][[i]]) == sample] = rename
    }
  }
  
})
