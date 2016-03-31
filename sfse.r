
source("E:\\wh\\sees\\extendspace.R")

rsfes<-function (x, y, trlength=500,mtry=floor(sqrt(ncol(x))), control = control, na.action =  na.omit)
{
  Call <- match.call()
  
  
  if (!inherits(y, "Surv"))
    stop("Response must be a 'survival' object - use the 'Surv()' function")
  
  ny <- ncol(y)
  n <- nrow(y)
  
  status <- y[, ny]
  survtime=y[, 1L]
  
  
  
  if (any(survtime <= 0)) stop("Observation time must be > 0")
  
  if (all(status == 0)) stop("No deaths in training data set")
  
  
  if (!missing(control))
    controls[names(control)] <- control
  
  
  #names(data)=c("time","status",names(x))
  
    trees <- vector(mode = "list", length = trlength)
  
  colindexes <- vector(mode = "list", length = trlength)
  
  newindexes <- vector(mode = "list", length = trlength)
  
  varimp<-NULL
  
  # if (mtry<=2){
    # if (mtry<ncol(x)-1){ 
          # mtry=mtry+1 
          # }else
           # {
             # stop("Too few variables")
           # }
      # }
  
  for (i in 1:trlength)
  {
    #for small mtry use extension first otherwise use random subspace first
    
    if (ncol(x)<=100){
	newx=extspace_dat(x)
    newindexes[[i]]=newx$ncombsub	
   
	colindex=sample(ncol(newx$newxdata),size=mtry*2)
	colindexes[[i]]=colindex
	newxdata=newx$newxdata[,colindex]	
	}else{
	colindex=sample(ncol(x),size=mtry)
    colindexes[[i]]=colindex    
    newx=extspace_dat(x[,colindex])
	newindexes[[i]]=newx$ncombsub
	
	newxdata=newx$newxdata    
	}
  
    
    mf=data.frame(y,newxdata)
    p=dim(mf)[2]
    
    trainindex=sample(nrow(mf),replace=T)
    
    trset=mf[trainindex,]
    
    # oobset
    train_posp<-1:nrow(mf) %in% trainindex
    
    oobset=mf[!train_posp,]
    
    
    trees[[i]]=rpart(trset,control = control)
    
   
    
  }
  
  fit=trees

  class(fit) <- "rsfes"
 
    return(list(trees=trees,colindexes=colindexes,newindexes=newindexes))
  
}

rsfes.predict<-function(rsfesfit,newdata,trlength=500){
  
  trees=rsfesfit$trees
  colindexes=rsfesfit$colindexes
  newindexes=rsfesfit$newindexes
  
  
  if (trlength>length(rsfesfit$trees))
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
      
      predicts<-predict(trees[[i]],testdata)
      
      testpre<-cbind(predicts,testpre)
    }
  }
  
  ensemble_predictions<-rowMeans(testpre)
  
  return(ensemble_predictions)
  
}
