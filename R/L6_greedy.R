
greedy_knapsack <- function(x, W){
  
  value_per_weight <- x$v/x$w
  index <- order(value_per_weight, decreasing = TRUE)
  
  elements <- numeric(0)
  total_weight <- 0
  total_value <- 0
  
  for(i in index){
    
    if( (x$w[i] + total_weight) <= W){
      elements <- c(elements, i)
      total_weight <- x$w[i] + total_weight
      total_value <- x$v[i] + total_value 
    }      
  }
  
  return(list(w = total_weight, value = total_value, elements = elements))
}

# greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
# greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)






