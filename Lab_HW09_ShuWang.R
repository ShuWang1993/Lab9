## Question 1:
odd_count = function(x) {
  odd_num = 0
  for (i in 1:length(x)) {
    if (x[i] %% 2 == 1) odd_num = odd_num + 1
  }
  return(odd_num)
}

odd_count_vec <- function(x){
  return(sum(x%%2 == 1))
}

library(microbenchmark)
x <- c(1:100)
microbenchmark(odd_count(x),odd_count_vec(x))
# It can be found that the running time of new vectorized form function is shorter than the original one.


## Question 2:
sort_vec <- function(x, ascending) {
  
  if (ascending == TRUE) {
  if (length(x) < 2) {
    return (x) 
  }
  
  for (last in length(x):2) { 
    for (first in 1:(last - 1)) {
      if (x[first] > x[first + 1]) { 
        temp <- x[first]
        x[first] <- x[first + 1] 
        x[first + 1] <- temp
      } 
    }
  }
  return(x) 
  } 
  
  if (ascending == FALSE) {
    if (length(x) < 2) {
      return (x) 
    }
    for (last in length(x):2) { 
      for (first in 1:(last - 1)) {
        if (x[first] < x[first + 1]) { 
          temp <- x[first]
          x[first] <- x[first + 1] 
          x[first + 1] <- temp
        } 
      }
    }
    return(x) 
  }
}

sort_vec(c(3, 1, 5, 2), ascending = TRUE)
sort_vec(c(3, 1, 5, 2), ascending = FALSE)


## Question 3:
N1 = 1000
data_series = 0 # original version
dy1 <- system.time({for (i in 2 : N1){
  data_series[i] = data_series[i-1] + sample(c(-1, 1), 1) }
})

N1 = 1000
data_series = c(NA) # preallocated version
length(data_series) <- N1
pre1 <- system.time({for (i in 2 : N1){
  data_series[i] = data_series[i-1] + sample(c(-1, 1), 1) }
})


N2 = 10000
data_series = 0 # original version
dy2 <- system.time({for (i in 2 : N2){
  data_series[i] = data_series[i-1] + sample(c(-1, 1), 1) }
})

N2 = 10000
data_series = c(NA) # preallocated version
length(data_series) <- N2
pre2 <- system.time({for (i in 2 : N2){
  data_series[i] = data_series[i-1] + sample(c(-1, 1), 1) }
})


N3 = 1000000
data_series = 0 # original version
dy3 <- system.time({for (i in 2 : N3){
  data_series[i] = data_series[i-1] + sample(c(-1, 1), 1) }
})

N3 = 1000000
data_series = c(NA) # preallocated version
length(data_series) <- N3
pre3 <- system.time({for (i in 2 : N3){
  data_series[i] = data_series[i-1] + sample(c(-1, 1), 1) }
})

output <- matrix(c(dy1[3], pre1[3], dy2[3], pre2[3], dy3[3], pre3[3]), byrow=T, nrow=3)
colnames(output) <- c("dynamically allocated memory", "preallocated memory")
rownames(output) <- c("N=1000", "N=10000", "N=1000000")
output

# It can be seen that the running time gets shorter with preallocated memory than that in dynamically allocated memory.