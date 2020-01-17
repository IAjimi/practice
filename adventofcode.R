library(readr)

## Advent of Code 1 ######
adventofcode1 <- read_csv("C:/Users/ia767/Downloads/adventofcode1.txt", col_names = FALSE)
sum(floor(adventofcode1 / 3) - 2)

#divide by 3 and round down to get 4, then subtract 2 to get 2.

## Advent of Code 2 ##########

fuel_floor <- function(vec){
  temp <- floor(vec / 3) - 2
  temp[temp <= 0] <- 0
  return(temp)
}


for (i in c(1:100)){
  
  if (i == 1){
      temp <- fuel_floor(adventofcode1)
      cursum <- sum(temp)
  } else{
    temp <- fuel_floor(temp)
    cursum <- cursum + sum(temp)
  }
  
  if(length(temp[temp <= 0]) == length(temp)){
    return(cursum)
  }
  
}

### Advent of Code 3 #########
adventofcode2 <- read_csv("C:/Users/ia767/Downloads/adventofcode2.txt", col_names = FALSE)
adventofcode2 <- unname(t(adventofcode2[1, ])[, 1])
adventofcode2[2] <- 12
adventofcode2[3] <- 2


intcode <- function(vec){
  i <- 1
  
  while(i <= length(vec)){
    if(vec[i] == 1){
      vec[vec[i+3] + 1] <- vec[vec[i+1] + 1] + vec[vec[i+2] + 1] #need to add + 1 because first position in R is 1, not 0
      i <- i + 4
    }
    
    if(vec[i] == 2){
      vec[vec[i+3] + 1] <- vec[vec[i+1] + 1] * vec[vec[i+2] + 1]
      i <- i + 4
    }
    
    if(vec[i] == 99){
      return(vec)
    }
    
  }
  return(vec)
}


temp <- c(1,9,10,3,2,3,11,0,99,30,40,50)
intcode(temp)

intcode(adventofcode2)
