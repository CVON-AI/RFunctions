NetReclassificationIndex_MultipleCat = function(ID, TrueValue, Reference, New){
  df = data.frame(cbind(as.numeric(as.character(ID)), 
                        as.numeric(as.character(TrueValue)), 
                        as.numeric(as.character(Reference)),
                        as.numeric(as.character(New))))
  colnames(df) = c("ID", "TrueValue", "Reference","New")
  NRI_df = df %>%
    mutate(Up_Event = ifelse((TrueValue == 1 &
                                Reference < New ), 1, 0)) %>%
    mutate(Down_Event = ifelse((TrueValue == 1 & 
                                  Reference > New), 1, 0)) %>%
    mutate(Up_NonEvent = ifelse((TrueValue == 0 & 
                                   Reference < New), 1, 0)) %>%
    mutate(Down_NonEvent = ifelse((TrueValue == 0 & 
                                     Reference > New), 1, 0))
  
  NRI_result = data.frame(matrix(ncol = 3, nrow = 1))
  colnames(NRI_result) = c("Event_NRI", "NonEvent_NRI", "Overall_NRI")
  NRI_result$Event_NRI = (sum(NRI_df$Up_Event)-sum(NRI_df$Down_Event))/sum(NRI_df$TrueValue == 1)
  NRI_result$NonEvent_NRI = (sum(NRI_df$Down_NonEvent)-sum(NRI_df$Up_NonEvent))/sum(NRI_df$TrueValue == 0)
  NRI_result$Overall_NRI = NRI_result$Event_NRI + NRI_result$NonEvent_NRI
  
  
  
  return(NRI_result)
  
}