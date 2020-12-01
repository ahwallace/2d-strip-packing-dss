# Hybrid Genetic Algorithm
# WLLAID002
# 25/09/2020

# Function to sort items by desired criteria
sort_instance <- function(items, method = c("perimeter", "area", "width", "height"), decreasing = TRUE){
  if(decreasing){
    if(method == "perimeter" | method == 1){
      return(items[order(items$width + items$height, decreasing = TRUE), ])
    } else if(method == "area" | method == 2){
      return(items[order(items$width*items$height, decreasing = TRUE), ])
    } else if(method == "width" | method == 3){
      return(items[order(items$width, decreasing = TRUE), ])
    } else if(method == "height" | method == 4){
      return(items[order(items$height, decreasing = TRUE), ])
    }
  } else {
    if(method == "perimeter" | method == 1){
      return(items[order(items$width + items$height, decreasing = FALSE), ])
    } else if(method == "area" | method == 2){
      return(items[order(items$width*items$height, decreasing = FALSE), ])
    } else if(method == "width" | method == 3){
      return(items[order(items$width, decreasing = FALSE), ])
    } else if(method == "height" | method == 4){
      return(items[order(items$height, decreasing = FALSE), ])
    }
  }
}

get_fitness <- function(data, orders, FUN = NA, strip_width){
  # data = data.frame of problem instance; id, height, width
  # orders = matrix of population of permutations of item orders
  # FUN = fitness function (heuristic)
  # strip_width = strip width
  
  FUN <- match.fun(FUN)
  
  fitness <- c()
  for(i in 1:ncol(orders)){
    fitness[i] <- heuristic(data[orders[, i], ], strip_width, FUN)$height
  }
  
  return(fitness)
}

get_population <- function(data, population_size, seed = TRUE){
  # data = data.frame with columns id, height and width of instance items
  # population_size = number of solutions in each generation
  # seed = whether or not the initial population is seeded with ordered items
  
  population <- matrix(data$id, nrow(data), population_size)
  population <- apply(population, 2, sample)
  
  # Seed initial population with good quality solutions
  n <- ifelse(population_size < 8, population_size, 8)
  if(seed){
    inc_dec <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
    ord <- rep(c("perimeter", "area", "width", "height"), 2)
    for(i in 1:n){
      population[, i] <- sort_instance(data, ord[i], inc_dec[i])$id
    }
  }
  
  return(population)
}

roulette_selection <- function(fitness){
  # fitness = data.frame with item id and fitness
  
  # Size of population
  n <- length(fitness)
  
  # Vector of subintervals based on proportional fitness
  wheel <- cumsum(prop.table(1/fitness))
  
  # Index of parent 1 and 2
  index <- sample(1:n, size = 2, replace = FALSE, prob = wheel)
  
  return(index)
}

tournament_selection <- function(fitness, tournament_size){
  
  ind <- 1:length(fitness)
  
  parent1 <- 0
  parent2 <- 0
  while(parent1 == parent2){
    # create a random subsamples of individuals to compete
    candidatesParent1 <- sample(ind, tournament_size, replace = TRUE)
    candidatesParent2 <- sample(ind, tournament_size, replace = TRUE)
    parent1 <- candidatesParent1[which.min(fitness[candidatesParent1])]
    parent2 <- candidatesParent2[which.min(fitness[candidatesParent2])]
  }
  
  return(c(parent1, parent2))
}

pmx <- function(ch1, ch2){
  
  n <- length(ch1)
  k <- sample(1:n, size = 2)
  k <- seq(min(k), max(k))
  
  offspring1 <- ch1
  offspring2 <- ch2
  
  offspring1[k] <- ch2[k]
  offspring2[k] <- ch1[k]
  
  while(sum(duplicated(c(offspring1[-k], ch2[k]))) > 0){
    for(i in setdiff(1:n, k)){
      if(offspring1[i] %in% ch2[k]){
        ind <- which(ch2[k] == offspring1[i])
        offspring1[i] <- ch1[k][ind]
      }
    }
  }
  
  while(sum(duplicated(c(offspring2[-k], ch1[k]))) > 0){
    for(i in setdiff(1:n, k)){
      if(offspring2[i] %in% ch1[k]){
        ind <- which(ch1[k] == offspring2[i])
        offspring2[i] <- ch2[k][ind]
      }
    }
  }
  
  return(data.frame("ch1" = offspring1, "ch2" = offspring2))
}

ox <- function(ch1, ch2){
  
  n <- length(ch1)
  k <- sample(1:n, 2)
  k <- seq(min(k), max(k))
  
  if(min(k) == 1 & max(k) == n){
    return(data.frame("ch1" = ch2, "ch2" = ch1))
  } else if(min(k) == 1 & max(k) != n){
    ordering1 <- ch1
    ordering2 <- ch2
  } else if(max(k) == n & min(k) != 1){
    ordering1 <- c(ch1[k], ch1[-k])
    ordering2 <- c(ch2[k], ch2[-k])
  } else if(min(k) != 1 & max(k) != n){
    ordering1 <- c(ch1[k], ch1[(max(k)+1):n], ch1[1:(min(k)-1)])
    ordering2 <- c(ch2[k], ch2[(max(k)+1):n], ch2[1:(min(k)-1)])
  }
  
  offspring1 <- rep(NA, n)
  offspring2 <- rep(NA, n)
  
  offspring1[k] <- ch2[k]
  offspring2[k] <- ch1[k]
  
  offspring1[is.na(offspring1)] <- setdiff(ordering1, offspring1)
  offspring2[is.na(offspring2)] <- setdiff(ordering2, offspring2)
  
  return(data.frame("ch1" = offspring1, "ch2" = offspring2))
}

swap_mutation <- function(data, orders, fitness, FUN, strip_width){
  
  chromosome <- sample(1:ncol(orders), 1)
  swap_index <- sample(1:nrow(orders), 2, replace = F)
  
  FUN <- match.fun(FUN)
  
  # Swap index's of selected chromosome
  orders[swap_index, chromosome] <- orders[rev(swap_index), chromosome]
  
  # Calculate fitness of new chromosome
  fitness[chromosome] <- heuristic(data[orders[, chromosome], ], strip_width, FUN)$height
  
  return(list("orders" = orders, "fitness" = fitness))
}

# hybrid_ga <- function(data, strip_width, instance_optimal, FUN, population_size, n_generations, crossover_prob, mutation_prob, selection = c("roulette", "tournament"), crossover = c("pmx", "ox"), stop_time = NULL, tournament_size = NULL){
#   # data = data.frame if items id, height, width
#   # strip_width = strip width
#   # instance_optimal = the optimal height if known, otherwise -999
#   # FUN = heuristic function to calculate fitness
#   # population_size = size of population
#   # n_generations = number of generations
#   # crossover_prob = probability of crossover
#   # mutation_prob = probability of mutation
#   # elitism = keep best solution for subsequent generation or not
#   # stop_time = maximum run time of the genetic algorithm (vector with hours:minutes:seconds)
#   # tournament_size = the size of the tournament if tournament selection implemented
#   
#   # Set stopping time stop_time units from current 
#   if(sum(stop_time) != 0){stop_run <- Sys.time() + sum(stop_time*c(60^2, 60, 1))}
#   
#   orders <- get_population(data, population_size, TRUE)
#   
#   best_order <- matrix(NA, nrow(data), (n_generations+1))
#   best_fitness <- c()
#   avg_fitness <- c()
#   
#   fitness <- get_fitness(data, orders, FUN, strip_width)
#   
#   gen <- 1
#   while(gen <= n_generations){
#     print(paste("Generation: ", gen, sep = ""))
#     
#     for(i in 1:population_size){
#       
#       # Select parents and apply crossover
#       if(runif(1) <= crossover_prob){
#         
#         # Select 2 individuals for parents using roulette wheel selection
#         if(selection == "roulette"){
#           parentsIndex <- roulette_selection(fitness)
#         } else if(selection == "tournament"){
#           parentsIndex <- tournament_selection(fitness, tournament_size)
#         }
#         
#         # Parents
#         ch1 <- orders[, parentsIndex[1]]
#         ch2 <- orders[, parentsIndex[2]]
#         
#         if(crossover == "pmx"){
#           cx <- pmx(ch1, ch2)
#         } else if(crossover == "ox"){
#           cx <- ox(ch1, ch2)
#         }
#         
#         # Calculate fitness of offspring
#         offspring1Fitness <- heuristic(data[cx$ch1, ], strip_width, FUN)$height
#         offspring2Fitness <- heuristic(data[cx$ch2, ], strip_width, FUN)$height
#         
#         # Replace worst individuals in population with offspring if offspring is fitter
#         if(offspring1Fitness < max(fitness)){
#           ind <- which.max(fitness)
#           orders[, ind] <- cx$ch1
#           fitness[ind] <- offspring1Fitness
#         }
#         
#         if(offspring2Fitness < max(fitness)){
#           ind <- which.max(fitness)
#           orders[, ind] <- cx$ch2
#           fitness[ind] <- offspring2Fitness
#         }
#       }
#       
#       # Mutation
#       if(runif(1) <= mutation_prob){
#         mut <- swap_mutation(data, orders, fitness, FUN, strip_width)
#         orders <- mut$orders
#         fitness <- mut$fitness
#       }
#     }
#     
#     # Save best permutation and associated fitness value
#     best_order[, gen] <- orders[, which.min(fitness)]
#     best_fitness[gen] <- min(fitness)
#     avg_fitness[gen] <- mean(fitness)
#     
#     # Stopping conditions
#     # Time limit
#     if(sum(stop_time) != 0){
#       if(Sys.time() > stop_run){
#         stop_criteria <- "time"
#         break
#       }
#     }
#     
#     # Optimal solution found
#     if(best_fitness[gen] == instance_optimal){
#       stop_criteria <- "optimal"
#       break
#     }
#     
#     # if(gen > 2){
#     #   if(abs(avg_fitness[gen] - avg_fitness[gen-2]) < 0.0001){
#     #     stop_criteria <- "improvement"
#     #     break
#     #   }
#     # }
#     
#     # Max number of generations reached
#     if(gen == n_generations){
#       stop_criteria <- "generations"
#     }
#     
#     gen <- gen+1
#   }
#   
#   return(list(best_order = best_order[, 1:gen], best_fitness = best_fitness, mean_fitness = avg_fitness, stop_criteria = stop_criteria))
# }
