CombineTables = function(table1_numeric, table2_percentages){
  cells = length(table1_numeric)
  table_combined = matrix(nrow = nrow(table1_numeric), ncol = ncol(table1_numeric))
  
  for(i in 1:cells){
    val = noquote(paste0(table1_numeric[i], " (", table2_percentages[i], ")"))
    table_combined[i] = val
  }
  
  table_combined = kable(noquote(table_combined))
  
  return(table_combined)
  
}