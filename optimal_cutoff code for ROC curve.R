#Optimal cut off for ROC curves 
#constants
cost_fp=1
cost_fn=10

##Functions
test.error <- function(pred,test)
{  error.table<-table(pred, test)
er<-(error.table[1,2] + error.table[2,1])/sum(error.table)
}
fp<-function(pred,test){
  error.table<-table(pred, test)
  (error.table[2,1]/sum(error.table[,1]))
}
fn<-function(pred,test){
  error.table<-table(pred, test)
  (error.table[1,2]/sum(error.table[,2]))
}
#Compute optimal cut off by ROC curve, cost_fn can be changed
cut.off<-function(pred,test){
  pred1 <- prediction( pred,test )
  cost.perf = performance(pred1, "cost", cost.fp = cost_fp, cost.fn = cost_fn)
  cut.off<-pred1@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
  #  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  return(cut.off)
}
