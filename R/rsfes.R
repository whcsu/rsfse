##' Grow a random survival forest with space extensions
##' @export
rsfes <-
function (x, y, trlength=500,mtry=floor(sqrt(ncol(x))), control = control, na.action =  na.omit)
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
    
    
    
    
  #  trees <- vector(mode = "list", length = trlength)
	pectrees <- vector(mode = "list", length = trlength)
    
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
        
        
        mf=data.frame(y[,1],y[,2],newxdata)
        colnames(mf)[c(1,2)]=c("time","status")
        
       
        trainindex=sample(nrow(mf),replace=T)
        
        trset=mf[trainindex,]
        
        # oobset
        #train_posp<-1:nrow(mf) %in% trainindex
        #oobset=mf[!train_posp,]
       
        
        
     #   trees[[i]]=rpart(Surv(time,status)~.,data=trset,control = control)
        pectrees[[i]]=pecRpart(Surv(time,status)~.,data=trset)
        
        
    }
    
    fit=pectrees
    
    class(fit) <- "rsfes"
    
    return(list(pectrees=pectrees,colindexes=colindexes,newindexes=newindexes))
    
}
