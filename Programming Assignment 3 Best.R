best<-function(state,outcome){
  data<-read.csv("outcome-of-care-measures.csv")
  states_data<-data[,7]
  outcome_data<-c("heart attack","heart failure","pneumonia")
  # stop the execution if state and outcome are not present in the data
  if(state %in% states_data==FALSE){
    stop(print("invalid state"))
  }
  else if(outcome %in% outcome_data==FALSE){
    stop(print("invalid outcome"))
  }
  # get the subset of the state in the parameter
  sub_data<-subset(data,State==state)
  # assign col number for the outcome
  if(outcome=="heart attack"){
    out_col<-11
  }
  else if(outcome=="heart failure"){
    out_col<-17
  }
  else{
    out_col<-23
  }
  # removing NA data
  col_req<-as.numeric(sub_data[,out_col])
  good<- !(is.na(col_req))
  new_data<-sub_data[good,]
  # find the hospital with min outcome value
  col_hospital<-as.numeric(new_data[,out_col])
  row_hospital<-which(col_hospital==min(col_hospital))
  hospital<-new_data[row_hospital,2]
  # to output the first occuring hospital alphabetically
  if (length(hospital) > 1) {
    hospitals_sorted <- sort(hospital)
    hospitals_sorted[1]
  }
  else {
    hospital
  }
}
