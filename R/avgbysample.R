#' Compute Avg. or Mean by Sample Size. Takes data column and samplesize as input and computes the avg or mean by samplesize as output dataframe.
#' @param data numeric
#' @param samplesize numeric
#' @return dataframe of avg or mean values by samplesize.
#' @examples
#' avgbysample(c(1,3,5,9),2)
#' avgbysample(c(1,3,5,9,10,15,19,18),4)
#' avgbysample(iris$Sepal.Length,5)
#' @export


# Call the avg_by function by passing the data column and sample value and convert
# output as data frame
# The following function computes an average of a data column by user specified samples i.e
# avg by 5 samples, avg by 10 samples etc.
avgbysample<-function (data,samplesize){
  d=c()
  avg=c()
  for(i in 1:length(data)){
    if(i%%samplesize==0){
      d=c(d,data[i])
      avg=c(avg,mean(d))
      d=c()
    }else{
      d=c(d,data[i])
    }
  }
  avg=as.data.frame(avg)
  colnames(avg)='Avg values'
  return (avg)
}



