#'@title named.list
#'@description Creates a list object by specifying unquoted variable names in 
#'            the argument list. \\cr \\cr
#'@return named list object
#'@examples
#'x<-list(x = c(1,2,3), x2 = c(2,3,4))
#'y<-c("a","b","c")
#'named.list(x,y)

named.list <- function(...) { 
  l <- setNames( list(...) , as.character( match.call()[-1]) ) 
  l
}
