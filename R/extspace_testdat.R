##' space extension for test data
##' @export
extspace_testdat <-
function(testdata,ncombsub)
{
  #FIRST convert all catergorical values (factors) into binary values
  #0+ to del Intercept produced by model.matrix
  
   testdata=model.matrix(~0+.,as.data.frame(testdata))
  # candidate variable for genereation
  d=dim(ncombsub)[2]
  newx<- vector(mode = "list", length = d)
  newx<-NULL
  new_names <- paste("newvar", 1:d, sep="_")
  
  for (i in 1:d){
    newx=cbind(newx,testdata[,ncombsub[2,i]]-testdata[,ncombsub[1,i]])
  }
  colnames(newx)<-new_names
  #random choose d new features
  # choosing according to some log-rank standard
  
  newxdata=cbind(testdata,newx)
  return (newxdata)
}
