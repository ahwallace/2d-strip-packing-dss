# Hybrid Simulated Annealing
# WXXLES001
# 20/11/2020

# Returns packing height as decoded by the specified heuristic routine
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

temperature <- function(t0, current_temp, alpha, niter, schedule = c("geometric", "linear", "logarithmic", "lundymees")){
  # t0 = initial temperature of the optimization routine
  # current_temp = current temperature
  # alpha = cooling rate parameter
  # niter = iteration number of the temperature cycle
  # schedule = the function that governs the cooling behaviour
  if(schedule == "geometric" | schedule == 1){
    temp <- t0*alpha^niter
  } else if(schedule == "linear" | schedule == 2){
    temp <- t0 - alpha*niter
  } else if(schedule == "logarithmic" | schedule == 3){
    temp <- t0/(1+alpha*log(1+niter))
  } else if(schedule == "lundymees" | schedule == 4){
    temp <- current_temp/(1+alpha*current_temp)
  }
  return(temp)
}
# 
# hybrid_sa <- function(data, strip_width, instance_optimal, FUN, init_temp, alpha, schedule, epoch_management, stop_iter = NULL, stop_time = NULL, lb_temp = 0){
#   # data = data.frame of items id, height, width
#   # strip_width = strip width
#   # instance_optimal = the optimal height if known, otherwise -999
#   # FUN = heuristic function to calculate fitness
#   # init_temp = initial temperature
#   # alpha = cooling rate parameter
#   # schedule = procedure that governs the decrease of the temperature
#   # epoch_management = number of state transitions for each temperature
#   # stop_iter = maximum number of iterations of the algorithm
#   # stop_time = maximum run time of the genetic algorithm (vector with hours:minutes:seconds)
#   # lb_temp = lower bound on temperature
#   
#   # Set stopping time stop_time units from current
#   if(sum(stop_time) != 0){stop_run <- Sys.time() + sum(stop_time*c(60^2, 60, 1))}
#   
#   N <- nrow(data)
#   
#   # Initial packing order
#   best_packing <- packing_order <- init_packing <- apply(cbind(data$id), 2, sample)
#   best_fitness <- fitness <- get_fitness(data, init_packing, FUN, strip_width)
#   
#   temp <- init_temp
#   
#   # Algorithm progression
#   temps <- c()
#   probs <- c()
#   heights <- c()
#   
#   iter <- 1
#   while(iter <= stop_iter){
#     if(stop_iter <= 20)
#       print(paste("Iteration: ", iter, sep = ""))
#     else{
#       if(iter%%(stop_iter/10) == 0) print(paste("Iteration: ", iter, sep = ""))
#     }
#     
#     temps[iter] <- temp
#     
#     # Cycling through epoch_management transitions at temperature(iter)
#     for(i in 1:epoch_management){
#       
#       # Neighbourhood structure of permutations
#       ind1 <- sample(1:N, 1); ind2 <- sample((1:N)[-ind1], 1)
#       swap <- c(ind1,ind2)
#       nb_solution <- packing_order
#       nb_solution[swap,] <- nb_solution[rev(swap),]
#       
#       # Height of proposed neighbour solution
#       nb_fitness <- get_fitness(data, nb_solution, FUN, strip_width)
#       
#       # Is the neighbour an improvement?
#       if(nb_fitness <= best_fitness){
#         probs[(iter-1)*epoch_management+i] <- 1
#         packing_order <- nb_solution
#         fitness <- nb_fitness
#       }
#       else{
#         energy <- nb_fitness - fitness
#         probs[(iter-1)*epoch_management+i] <- exp(-energy/temp)
#         if(runif(1) <= probs[(iter-1)*epoch_management+i]){
#           packing_order <- nb_solution
#           fitness <- nb_fitness
#         }
#       }
#       
#       heights[(iter-1)*epoch_management+i] <- fitness
#       if(fitness <= best_fitness){
#        best_fitness <- fitness; best_packing <- packing_order 
#       }
#     }
#     
#     # Update temperature
#     temp <- temperature(init_temp, temp, alpha, iter, schedule)
#     
#     # Stopping conditions
#     if(sum(stop_time) != 0){
#       if(Sys.time() > stop_run){
#         stop_criteria <- "time"
#         break
#       }
#     }
#     
#     if(best_fitness == instance_optimal){
#       stop_criteria <- "optimal"
#       break
#     }
#     
#     if(temp <= lb_temp){
#       stop_criteria <- "temperature"
#       break
#     }
#     
#     if(iter == stop_iter){
#       stop_criteria <- "iterations"
#     }
#     
#     iter <- iter+1
#   }
#     
#   return(list(best_order = best_packing, best_height = best_fitness, stop_criteria = stop_criteria, 
#               temperature = temps, fitness_progress = heights, probabilities = probs))
# }
