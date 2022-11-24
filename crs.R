library(ggplot2)
fib <- function(n, tab) {
  if (n<3) {
    ccc<- c(1, tab)
    tab <- c(1, tab)
    return(ccc)
  }
  fibvalueminus1 <- fib(n-1, tab)
 
  print(fibvalueminus1)
  
  fibvalueminus2 <- fib(n-2, tab)
  
  print(fibvalueminus2)
  
  fibvalue <- as.numeric(fibvalueminus1[1][[1]])  + as.numeric(fibvalueminus2[1][[1]])
  fibvalue
  ccc <- c(fibvalue, tab)
  tab <- rbind(tab, ccc)
  return(ccc)
}




tab <- data.frame('n', 'fibvalue')

tab <- fib(6, tab)



fib <- function(n, tab) {
  if (n<3) {
    tab <- rbind(tab, c(n, 1))
    ccc<- list(tab, 1)
    
    return(ccc)
  }
  fibvalueminus1 <- fib(n-1, tab)
  #print(as.numeric(fibvalueminus1[2][[1]]))
  tab <-fibvalueminus1[1][[1]]
  
  fibvalueminus2 <- fib(n-2, tab)
  #print(as.numeric(fibvalueminus2[2][[1]]))
  tab <- fibvalueminus2[1][[1]]
  
  fibvalue <- as.numeric(fibvalueminus1[2][[1]])   + as.numeric(fibvalueminus2[2][[1]]) 
  #print(fibvalue)
  
  tab <- rbind(tab, c(n, fibvalue))
  ccc <- list(tab, fibvalue)

  return(ccc)
}

tab <- data.frame("n" = 1, "fibvalue" = 1)

tab <- fib(10, tab)[1]
ggplot(abc, aes(x = a, y = b, size = c)) + geom_point()
a <- tab[[1]]$n
b <- tab[[1]]$fibvalue
c <-as.numeric(row.names(tab[[1]]))  
abc <- data.frame("iterationn" = c,  "n"= a, "fibvalue" = b)
