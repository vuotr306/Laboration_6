set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )



brute_force_knapsack<-function(x,W){
  
  no_of_objects <- nrow(x)
  no_of_sets <- 2^nrow(x)
  weights <- rep(NA, no_of_sets)
  values <- rep(NA, no_of_sets)
  selected_objects <- as.list(numeric(no_of_sets))
  
  
  for(i in 1:no_of_sets){
    selected_objects[[i]] <- as.numeric(intToBits(i)[1:(no_of_objects)])
    sample <- x[selected_objects[[i]] == 1,]
    weights[i] <- sum(sample[,1])
    values[i] <- sum(sample[,2])    
  }
  index_OK <- which(weights<W)
  weightsOK <- weights[index_OK]
  valuesOK <- values[index_OK]
  index_best <- which.max(valuesOK)
  
  
  #   browser()
  best_elements <- (1:no_of_objects)[selected_objects[index_OK[index_best]][[1]]==1]
  return(list(value=values[index_OK[index_best]], elements=best_elements))
  
}

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)

system.time({
  brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000)
})











knapsack_dynamic<-function(x, W){
  
  n<-nrow(x)
  m<-matrix(nrow=n+1,ncol=W+1)
  for (i in 1:(W+1)){
    m[1,i]<-0
  }  
  
  for(i in 1:n){
    for(j in 1:(W+1)){
      if(x$w[i]<=j){
        m[i+1,j] <- max( m[i,j] , m[i,j-x$w[i] ]+x$v[i]) 
      }else{
        m[i+1,j] <- m[i,j]
      }
    }
  }
  
  return(list(value=m[nrow(m), ncol(m)],elements=NULL))
  
}



knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)



























knapsack_dynamic2<-function(x, W){

  n<-nrow(x)
  m<-matrix(nrow=n+1,ncol=W+1)
  element<-c()
  for (i in 1:(W+1)){
    m[1,i]<-0
  }  
  
  for(i in 2:(n+1)){
    for(j in 1:(W+1)){
      if(x$w[i-1]<=j){
        m[i,j] <- max( m[i-1,j] , m[i-1,j-x$w[i-1] ]+x$v[i-1]) 

        if(m[i-1,j]<m[i,j] & m[i,j-1]<m[i,j]){
          element[j]<-i-1


        }else{          
          element[j]=element[j]
        }
        
      }else{
        m[i,j] <- m[i-1,j]
      }
    }
  }
  
    
  return(list(value=m[nrow(m), ncol(m)],elements=element))
  
}



knapsack_dynamic2(x = knapsack_objects[1:8,], W = 3500)





















