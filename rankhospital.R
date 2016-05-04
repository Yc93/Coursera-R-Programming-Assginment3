## This function tells users the  name of the hospital with the XX lowest 30- day death reate for specific outcome
rankhospital<- function(state,outcome, num = "best"){
  ## Read data
  data<- read.csv("outcome-of-care-measures.csv")
  ## Validation Test
  if(!state %in% data[,7]){
    stop(message("invalid state"))}
  if(!outcome %in% c("heart failure", "heart attack","pneumonia")){
    stop(message("invalid outcome"))}
  ##Prepare outcome for data read... Convert to column name
  if(outcome=="heart failure"){outcome2<-"Heart.Failure"}
  else if(outcome=="heart attack") {outcome2<-"Heart.Attack"}
  else{outcome2<- "Pneumonia"}
  Colname<-paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcome2,sep="")
  ## Subsetting data to state and disease
  statedata<-subset(data, State==state, select= c("Hospital.Name",Colname))
  ##Transform to Numeric In order to Rank and Order
  want.data<-as.numeric(as.character(statedata[[Colname]]))
  ## Rank differnet hospitals
  rank.statedata<- statedata[order(want.data, statedata$Hospital.Name, na.last= NA),]
  if (num=="best"){order<- 1}
  else if (num=="worst") {order<-nrow(rank.statedata)}
  else{order <-num}
  ## Pick the hospital
  if(order>nrow(rank.statedata)){
    print("NA")
  }
  else{
    Hpname<- rank.statedata$Hospital.Name
    print(Hpname[order])}
}


