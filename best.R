## This function will give the name of the best hospital in solving specific deseases within states 
best<- function(state, outcome){
    data<- read.csv("outcome-of-care-measures.csv")
    ##check if input argument is valid
    judgestate<- any(state == data[,7])
    judgeoutcome<- any(outcome == c("heart attack", "heart failure", "pneumonia"))
    if(!judgestate & judgeoutcome){
        message("invalid state")
        stop()}
    else if(judgestate & !judgeoutcome){
        message("invalid outcome")
        stop()}
    else if(!judgestate & !judgeoutcome){
        message("invalid state and invalid outcome")
        stop()} 
    ##Sort by State
    else{data2<- split(data, data$State)}
    ## Selecta state
    rundata<- data2[[state]]
    
    if (outcome == "heart attack"){
        want<- min(as.numeric(as.character(rundata[,11])), na.rm=TRUE)
        numberofhospital<- which(as.numeric(as.character(rundata[,11]))==want)}
    else if (outcome =="heart failure"){
        want<- min(as.numeric(as.character(rundata[,17])),na.rm=TRUE)
        numberofhospital<- which(as.numeric(as.character(rundata[,17]))==want)}
    else {
        want<- min(as.numeric(as.character(rundata[,23])), na.rm=TRUE)
        numberofhospital<- which(as.numeric(as.character(rundata[,23]))==want)}
    namehospital<-rundata[numberofhospital,2]
    
    print(namehospital[1])
}
