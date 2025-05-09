Calculate_distances <- function(method = NULL, sample = NULL) {
  
  withProgress(message = 'Calculating distances', value = 0, {
    
    if (input$choose_distances_to_determine == "Genes") {
      
      table_sample = raster::t(GetAssayData(values$data))
      
    } else if (input$choose_distances_to_determine != "Genes") {
      
      if (input$choose_distances_to_determine == "Metadata") {
        
        table_sample = values$data@meta.data[,sapply(values$data@meta.data, is.numeric)]
        table_sample = table_sample[,!sapply(colnames(table_sample), function(x){return(all(table_sample[,x] == 0))})]
        
      } else {
        table_sample = values$data@reductions[[input$choose_distances_to_determine]]@cell.embeddings
        
        if(input$choose_distances_to_determine != "ica"){
          colnames(table_sample) = values$data@misc$reduction_names[[input$choose_distances_to_determine]]
        }
        
      }
      
      if(input$use_positive_values_for_distances){
        table_sample[table_sample < 0] = 0
      }
      
    }
    
    if (input$choose_distances_to_determine_2 == "Genes") {
      
      table_sample_2 = raster::t(GetAssayData(values$data))
      
    } else if (input$choose_distances_to_determine_2 != "Genes") {
      if (input$choose_distances_to_determine_2 == "Metadata") {
        
        table_sample_2 = values$data@meta.data[,sapply(values$data@meta.data, is.numeric)]
        table_sample_2 = table_sample_2[,!sapply(colnames(table_sample_2), function(x){return(all(table_sample_2[,x] == 0))})]
        
        
      } else {
        
        table_sample_2 = values$data@reductions[[input$choose_distances_to_determine_2]]@cell.embeddings
        
        if(input$choose_distances_to_determine_2 != "ica"){
          colnames(table_sample_2) = values$data@misc$reduction_names[[input$choose_distances_to_determine_2]]
        }
      }
      
      if(input$use_positive_values_for_distances){
        table_sample_2[table_sample_2 < 0] = 0
      }
      
    }
    
    if(length(values$data@images) > 1){
      table_sample = table_sample[grepl(paste0(sample,"_[ACGT]+"), rownames(table_sample)),]
      table_sample_2 = table_sample_2[grepl(paste0(sample,"_[ACGT]+"), rownames(table_sample_2)),]
    }
    
    incProgress(0.1, detail = "Finding neighbors")
    
    if(method == "Lee"){
      
      knn = knearneigh(TissueCoordinates()[[sample]], k=6, longlat = NULL, use_kd_tree=TRUE)
      neighbours = knn2nb(knn, row.names = NULL, sym = FALSE)
      listw = nb2listw(neighbours, glist=NULL, style="W", zero.policy=NULL)
      
      incProgress(0.2, detail = "Calculating Lee")
      
      if (input$choose_distances_to_determine == "Genes" & input$choose_distances_to_determine_2 == "Genes"){
        if(is.null(input$import_genes_table_distances)){
          lr = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$lr_pair
          
          df <- data.frame(lr=lr)
          df <- df %>% separate(lr, into = c(input$choose_distances_to_determine, paste0(input$choose_distances_to_determine_2,"_2")), sep = "_")
        } else {
          df = read.csv(input$import_genes_table_distances$datapath)
          colnames(df) = c(input$choose_distances_to_determine, paste0(input$choose_distances_to_determine_2,"_2"))
        }
        
        table_sample = as.data.frame(table_sample[,colnames(table_sample) %in% unique(c(df[,1],df[,2]))])
        table_sample_2 = as.data.frame(table_sample_2[,colnames(table_sample_2) %in% unique(c(df[,1],df[,2]))])
        
        df = df[df[,1] %in% colnames(table_sample)[colSums(table_sample) > 0],]
        df = df[df[,2] %in% colnames(table_sample_2)[colSums(table_sample_2) > 0],]
        
      } else if (input$choose_distances_to_determine == "Genes") {
        if(is.null(input$import_genes_table_distances)){
          lr = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$lr_pair
          
          df <- data.frame(lr=lr)
          df <- df %>% separate(lr, into = c('l', 'r'), sep = "_")
        } else {
          df = read.csv(input$import_genes_table_distances$datapath)
          colnames(df) = c("l", "r")
        }
        
        table_sample = as.data.frame(table_sample[,colnames(table_sample) %in% unique(c(df$l,df$r))])
        
        df = df[df[,1] %in% colnames(table_sample)[colSums(table_sample) > 0],]
        df = df[df[,2] %in% colnames(table_sample)[colSums(table_sample) > 0],]
        
        df = expand_grid(unique(c(df$l,df$r)),unique(colnames(table_sample_2)))
        df = as.data.frame(df)
        colnames(df) = c(input$choose_distances_to_determine,input$choose_distances_to_determine_2)
        
      } else if(input$choose_distances_to_determine_2 == "Genes"){
        if(is.null(input$import_genes_table_distances)){
          lr = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$lr_pair
          
          df <- data.frame(lr=lr)
          df <- df %>% separate(lr, into = c('l', 'r'), sep = "_")
        } else {
          df = read.csv(input$import_genes_table_distances$datapath)
          colnames(df) = c("l", "r")
        }
        
        table_sample_2 = as.data.frame(table_sample_2[,colnames(table_sample_2) %in% unique(c(df$l,df$r))])
        
        df = df[df[,1] %in% colnames(table_sample_2)[colSums(table_sample_2) > 0],]
        df = df[df[,2] %in% colnames(table_sample_2)[colSums(table_sample_2) > 0],]
        
        df = expand_grid(unique(colnames(table_sample)),unique(c(df$l,df$r)))
        df = as.data.frame(df)
        colnames(df) = c(input$choose_distances_to_determine,input$choose_distances_to_determine_2)
        
      } else if(input$choose_distances_to_determine != "Genes" & input$choose_distances_to_determine_2 != "Genes") {
        if(input$choose_distances_to_determine == input$choose_distances_to_determine_2){
          df = t(combn(unique(colnames(table_sample)),2))
          df = as.data.frame(df)
          colnames(df) = c(input$choose_distances_to_determine,paste0(input$choose_distances_to_determine_2,"_2"))
          
        } else {
          df = expand_grid(unique(colnames(table_sample)),unique(colnames(table_sample_2)))
          df = as.data.frame(df)
          colnames(df) = c(input$choose_distances_to_determine,input$choose_distances_to_determine_2)
        }
      }
      
      if(nrow(df) > 0){
        
        x = apply(df,1,function(x){n = lee(table_sample[,x[1]], table_sample_2[,x[2]], listw, nrow(table_sample), zero.policy=attr(listw, "zero.policy"));return(n)})
        
        incProgress(0.7, detail = "Finished")

        df[,"weight"] = unlist(lapply(x,function(n){return(n$L)}))

        df = df[!is.na(df[,"weight"]),]
        
        values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[sample]][[method]] = df
      } else {
        shinyalert("Oops!", "No LR interaction found within selected sample", type = "error")
      }
      
    } else if (method == "Dotproduct"){
      
      if (input$choose_distances_to_determine == input$choose_distances_to_determine_2){
        
        if (input$choose_distances_to_determine == "Genes"){
          lr = read.delim(paste0(Shiny.options[["shiny_root"]], "/../tmp_data/human_lr_pair.csv"))$lr_pair
          
          df <- data.frame(lr=lr)
          df <- df %>% separate(lr, into = c(input$choose_distances_to_determine, paste0(input$choose_distances_to_determine_2,"_2")), sep = "_")
          
          table_sample = table_sample[,colnames(table_sample) %in% unique(c(df[,1],df[,2]))]
          table_sample_2 = table_sample_2[,colnames(table_sample_2) %in% unique(c(df[,1],df[,2]))]
          
        }
        
        dot_product = t(table_sample) %*% table_sample_2
        dot_product = as.data.frame(as.table(as.matrix(dot_product)))
        
        if (input$choose_distances_to_determine == "Genes"){
          df = merge(df, dot_product, by=c(1,2))
        } else {
          combinations = t(combn(colnames(table_sample), 2))
          df = merge(combinations, dot_product, by=c(1,2))
        }
        
        values$distances[[paste0(input$choose_distances_to_determine,"_",input$choose_distances_to_determine_2)]][[sample]][[method]] = df
        
      }
      
    } else if (method == "Dotproduct Genes"){
      
      dot_product = t(values$data@reductions$ica@feature.loadings) %*% values$data@reductions$ica@feature.loadings
      dot_product = as.data.frame(as.table(as.matrix(dot_product)))
      
      combinations = t(combn(colnames(values$data@reductions$ica@feature.loadings), 2))
      df = merge(combinations, dot_product, by=c(1,2))
      
      print(df)
      
      return(df)
      
    }
    
  })
  
  
  
}