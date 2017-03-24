##' Prediction with new data and return a saved forest brier score function 
##' This function has some problems
##' @export
rsfes.bs_predict <-
function(rsfesfit,testdat,rii,trlength=500){
    
    trees=rsfesfit$pectrees
    colindexes=rsfesfit$colindexes
    newindexes=rsfesfit$newindexes
    newdata=testdat[,-c(rii)]
    
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
             
            newtestdat=cbind.data.frame(testdat[,c(rii)],testdata)
            pecerror <- pec(list("rsfse"=trees[[i]]),formula=Surv(time,status)~., data=newtestdat,cens.model = "marginal",reference = FALSE)
            print((pecerror))
            pecerror$AppErr$rsfse[is.na(pecerror$AppErr$rsfse)]=0
            predicts=crps(pecerror)[1]
           
            print(crps(pecerror))
            testpre<-cbind(predicts,testpre)
        }
    }
    
    ensemble_predictions<-rowMeans(testpre)
    
    return(ensemble_predictions)
    
}
