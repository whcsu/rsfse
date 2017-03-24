##' Prediction with new data and return a saved forest with mean surv prob at each time points 
##' @export
rsfes.surv_predict <-
function(rsfesfit,newdata,uniquetimes,trlength=500){
    
    trees=rsfesfit$pectrees
    colindexes=rsfesfit$colindexes
    newindexes=rsfesfit$newindexes
   
    
    if (trlength>length(rsfesfit$pectrees))
        stop("Number of Trees for prediction should not be more than Number of Trees Fitted")
    
    # Preparing testpre dataframe 
    testpre <- matrix(0,nrow = dim(newdata)[1],  ncol= length(uniquetimes))
    colnames(testpre)=paste0(uniquetimes)
    
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
            predicts=predictSurvProb(trees[[i]],testdata,uniquetimes)
            #convert all NA into zero
            predicts[is.na(predicts)]=0
            colnames(predicts)=paste0(uniquetimes)
            #print((predicts[1,100]))
            testpre<-testpre+predicts
            #print(dim(testpre))
        }
    }
    
    
    return(testpre/trlength)
    
}
