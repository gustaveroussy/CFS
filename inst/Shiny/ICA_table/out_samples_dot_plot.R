##----------------------------------------------------------------------------##
## Sample based dotplot
##----------------------------------------------------------------------------##

output[["sample_based_dotplot"]] <- plotly::renderPlotly({
  return(sample_based_dotplot_react())
})

sample_based_dotplot_react <- reactive({
  req(values$data)
  if(length(values$data@images) >= 2){
    
    ICA = values$data@reductions$ica@cell.embeddings
    ICA[ICA < 0] = 0
    ICA_ratio <- sweep(ICA, 1, rowSums(ICA), "/") %>% as.data.frame()
    colnames(ICA_ratio) <- paste0(colnames(ICA_ratio), "_ratio")
    
    # Dotplot on normalized ICA
    ICA_ratio_long <- ICA_ratio %>% as.data.frame() %>%  rownames_to_column(var = "spot") %>% separate(spot, into = c("sample", "spot"), sep = "\\_(?!.*_)", remove = FALSE) %>% pivot_longer(cols = !c(spot | sample), names_to = "IC", values_to = "prop")
    
    # Average normalized value visualization
    ICA_ratio <- as.data.frame(ICA_ratio_long) %>% group_by(IC) %>% dplyr::filter(prop>quantile(prop,probs = input$percentil_dotplot)) %>% group_by(IC,sample) %>%  summarise(count=n())
    
    ICA_ratio[is.na(ICA_ratio)] <- 0
    
    ICA_ratio_avg <- as.data.frame(ICA_ratio_long) %>% group_by(IC,sample) %>% summarize( mean_ratio=quantile(prop, probs=input$percentil_dotplot)) %>% left_join(., ICA_ratio, by= c("IC", "sample"))
    ICA_ratio_avg_global <- as.data.frame(ICA_ratio_long) %>% group_by(IC) %>% summarize( mean_ratio=quantile(prop, probs=input$percentil_dotplot))
    
    spotCount <- ICA_ratio_long[ICA_ratio_long$IC == "IC_1_ratio",] %>% group_by(sample) %>% count()
    ICA_ratio_avg_test <- left_join(ICA_ratio_avg, spotCount, by="sample") %>% left_join(., ICA_ratio_avg_global, by="IC") %>% dplyr::mutate("above_avg"=count/n) %>% group_by(IC) %>% mutate(scaled_avg=scale(mean_ratio.x))
    
    ICA_ratio_avg_test = ICA_ratio_avg_test %>% dplyr::filter(mean_ratio.x>mean_ratio.y)
    
    Cells_above_average = ICA_ratio_avg_test$above_avg*100
    Proportion_above_the_percentil = ICA_ratio_avg_test$scaled_avg
    
    #Seurat-like Dotplot to identify signal distribution and intensity in each sample per IC
    fig = ICA_ratio_avg_test %>% 
      ggplot(aes(x=sample, y=IC))+
      geom_point(aes(size=Cells_above_average, fill=Proportion_above_the_percentil), color="black", shape=21)+
      scale_y_discrete(limits=rev(paste0(colnames(ICA),"_ratio")))+ # rownames(IC_anno)
      #scale_x_discrete(limits=unique(int_filt$sample))+
      scale_size_area(paste0("% of spots\n>=",input$percentil_dotplot * 100,"th Percentile"), max_size = 5, breaks=c(2.5,10,25), limits=c(0.5,25), oob=scales::squish) +
      scale_fill_gradientn(colours = viridisLite::viridis(100),
                           #limits=c(0.1,4.2), oob=scales::squish,
                           guide = guide_colorbar(ticks.colour = "black",
                                                  frame.colour = "black"),
                           name = paste0(input$percentil_dotplot * 100,"th Percentile\nprop")) + #"Scaled 95th\nPercentile prop")+
      ylab("IC") + xlab("sample") +
      theme_bw() +
      theme(axis.text.x = element_text(size=10, angle=45, hjust=1, color="black"),
            axis.text.y = element_text(size=12, color="black"),
            axis.title = element_text(size=14))
    
    fig = fig %>% ggplotly()
  
    return(fig)
  } else {
    return(NULL)
  }
})

##----------------------------------------------------------------------------##
## Create the colorscale for dotplot
##----------------------------------------------------------------------------##
colorscale_sample_based_dotplot <- reactive({
  if(input$select_color_sample_based_dotplot %in% c("Blues", "Reds","YlGnBu","YlOrRd")){
    return(input$select_color_sample_based_dotplot)
  } else {
    #prepare colorscales
    l = list()
    se = seq(0, 1, (1/(5000-1)))
    col = viridis_pal(option = input$select_color_sample_based_dotplot)(5000)
    for(i in 1:length(se)){
      l[[i]] = list(se[i],col[i])
    }
    return(l)
  }
})