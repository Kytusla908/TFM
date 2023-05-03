# Normalize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


# Function to generate series from 1 to 0.1^x
sequence <- function(x){
  output <- c(1)
  for (i in 1:x){
    a <- 0.1^i
    output <- append(output, a)
  } 
  return(output)
}