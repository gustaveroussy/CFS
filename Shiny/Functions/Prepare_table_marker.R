Prepare_table_marker=function(table = NULL){
  table$p_val = formatC(table$p_val, format = "e", digits = 2)
  table$avg_log2FC = round(table$avg_log2FC,digit=3)
  table$p_val_adj = formatC(table$p_val_adj, format = "e", digits = 2)
  return(table)    
}
