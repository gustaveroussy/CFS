Prepare_table_marker=function(table = NULL, log = 0.6, pvalue = 0.05){
  table = table[which(table$p_val_adj < pvalue),]
  table = table[which(table$avg_log2FC > log |  table$avg_log2FC < (-log)),]
  table$p_val = formatC(table$p_val, format = "e", digits = 2)
  table$avg_log2FC = round(table$avg_log2FC,digit=3)
  table$p_val_adj = formatC(table$p_val_adj, format = "e", digits = 2)
  return(table)    
}
