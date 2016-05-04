## This function gives users the hospital in each state that has "num" ranking.
rankall<- function(outcome, num="best"){
  data<- read.csv("outcome-of-care-measures.csv")
  ##Test the valiadation of argument
  if (!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop("invlid outcome")}
  statelist<- as.character(sort(unique(data[,7])))
  ## Prepare the the column name for future column selection
  if(outcome== "heart attack"){outcome2<-"Heart.Attack"}
  else if(outcome== "heart failure"){outcome2<- "Heart.Failure"}
  else{outcome2<-"Pneumonia"}
  colname<- paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcome2,sep="")
  
  ##Subsetting function, Ranking Function, and Get Result  
  calculation<- function(x){
    x2<- list(x)
    eachstate<-subset(data, State==x2, select=c("Hospital.Name",colname))
    disorder.data<- as.numeric(as.character(eachstate[[colname]]))
    rankeddata<-eachstate[order(disorder.data, na.last=NA),]
    ## Pick the ranking
    if(num =="best"){rank<-1}
    else if(num =="worst"){rank<-nrow(rankeddata)}
    else{rank<-num}
    if(rank> nrow(rankeddata)){
      get<- NA}
    else{get<- rankeddata$Hospital.Name[rank]}
  }
  ##Create a function to Subset states
  rankhospital<-lapply(statelist, calculation)
  ##Convert the list to a vecotr in preparation for data.frame settings
  y<-as.character()
  i<- 1
  while (i <55){
    y[i]<- as.character(rankhospital[[i]])
    i= i+1
  }
  ##Make the result into a data frame
  result<- data.frame(hospital=y, state=statelist)
  print(result)
}