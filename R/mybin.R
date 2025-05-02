



#' Binomial Function
#'
#' @param iter number of iterations
#' @param n sample size
#' @param p probability
#'
#' @returns A barplot with extra colors and stuff
#' @export
#'
#' @importFrom graphics barplot
#' @importFrom grDevices rainbow
#'
#' @examples
#' mybin(iter=20, n=10, p=0.7)
mybin=function(iter=100,n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
mybin(iter=1000,n=18, p=0.3)

## Try a multinomial

mymult=function(iter=100,n=10, p=c(1,1,1,1)/4){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  #The number of categories is k
  k=length(p)
  # Make a matrix that will hold the frequencies in each sample
  tab.mat=matrix(NA,nrow=k,ncol=iter, byrow=TRUE)


  for(i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(1:k,n,replace=TRUE, prob=p)
    #Collect all the frequencies of each of the k values
    tab.mat[,i]=table(factor(sam.mat[,i],levels=1:k))
  }
  # sum the frequecies
  freq=apply(tab.mat,1,sum)
  # put names to them
  names(freq)=1:k
  #create a barplot of refative freq
  barplot(freq/(n*iter),col=rainbow(k) )
  tab.mat
}
