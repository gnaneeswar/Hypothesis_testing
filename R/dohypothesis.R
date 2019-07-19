#' @title Hypothesis testing
#'
#' @description This package does all possible hypothesis test on a dataframe and outputs the results
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples stats(fram,"twotail",0.95)
#'
#' @export stats

stats_get<-function(df,test,alpha)

{
  library("dplyr")
  col<-names(df)

  nums<-c()
  labels<-c()
  binary<-c()

  nums<-names(select_if(df, is.numeric))
  labels<-names(select_if(df, is.factor))

  for (j in labels)

  {
    if(nrow(unique(df[j]))=="2")

    {append(binary,j)}
  }

  print(nums)
  print(labels)

  if((test=="twotail"))
  {

    for(i in nums)
    {
      for ( j in labels)

      {
        if(nrow(unique(df[j]))>2)

        {
          cat("\n")
          print(c(names(df[i]),"vs",names(df[j])))
          print(aov(unlist(df[i])~unlist(df[j]))) }

        else

        {
          cat("\n")
          print(c(names(df[i]),"vs",names(df[j])))
          print(t.test(unlist(df[i])~unlist(df[j]), mu=0,alt="two.sided",conf=alpha,var.eq=F,paired=F))
        }

      }}}

  if(test=="onetail")

  {

    for(i in nums)
    {
      for ( j in labels)

      {
        if(nrow(unique(df[j]))>2)

        { cat("\n")
          print(c(names(df[i]),"vs",names(df[j])))
          print(aov(unlist(df[i])~unlist(df[j]))) }

        else

        { cat("\n")
          print(c(names(df[i]),"vs",names(df[j])))
          print(t.test(unlist(df[i])~unlist(df[j]), mu=0,alt="less",conf=alpha,var.eq=F,paired=F))
          cat("\n")
          print(c(names(df[i]),"vs",names(df[j])))
          print(t.test(unlist(df[i])~unlist(df[j]), mu=0,alt="greater",conf=alpha,var.eq=F,paired=F)) }

      }}}


  if(test=="chisq")
  {
    print("entered")
    for(i in labels)
    { print("entered")
      for (j in labels)
      {
        if(i!=j)
        {
          cat("\n")
          print(c(names(df[i]),"vs",names(df[j])))
          #print(c(nrow(df[i]),"vs",nrow(df[j])))
          print(chisq.test(unlist(df[i]),unlist(df[j])))

        }}}}
}


