##' Prediction with new data and return a saved forest with mean hazard function 
##' @export
rsfes.predict <-
function(rsfesfit,newdata,trlength=500){
    
    trees=rsfesfit$pectrees
    colindexes=rsfesfit$colindexes
    newindexes=rsfesfit$newindexes
    
    
    if (trlength>length(rsfesfit$pectrees))
        stop("Number of Trees for prediction should not be more than Number of Trees Fitted")
    
    # classify the test data
    testpre<-NULL
    for (i in 1:trlength) {
        #if (oobacc[i]<=avroobacc)
        {
            
            # preparing for testing
            if (ncol(newdata)<=100){
                testdata=extspace_testdat(newdata,newindexes[[i]])
                testdata=testdata[,colindexes[[i]]]
            }else{   
                testdata=newdata[,colindexes[[i]]]
                testdata=extspace_testdat(testdata,newindexes[[i]])
            }
            testdata=as.data.frame(testdata)
            
            predicts<-predict(trees[[i]]$rpart,testdata)
            
            testpre<-cbind(predicts,testpre)
        }
    }
    
    ensemble_predictions<-rowMeans(testpre)
    
    return(ensemble_predictions)
    
}
