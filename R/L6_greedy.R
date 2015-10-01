l <- lineprof(brute_force_knapsack(x = knapsack_objects[1:15,], W = 3500))
l
library(shiny)
shine(l)


