metrics = function(Predicted, Reference){
  confmatrix = confusionMatrix(Predicted, Reference)
  auc = auc(roc(as.numeric(Reference), as.numeric(Predicted)))
  tp = confmatrix$table[2,2]
  tn = confmatrix$table[1,1]
  fp = confmatrix$table[2,1]
  fn = confmatrix$table[1,2]
  
  recall = tp/(tp+fn)
  precision = tp/(tp+fp)
  specificity = tn/(tn+fp)
  ppv = tp/(tp+fp)
  npv = tn/(tn+fn)
  f1 = 2*(precision*recall)/(precision+recall)
  balancedaccuracy = (specificity+recall)/2
  
  
  return(c(confmatrix$overall['Accuracy'], 
           balancedaccuracy,
           recall,
           specificity,
           precision, 
           f1, 
           npv,
           auc
  ))
}