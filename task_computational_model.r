# Task and Computational model


# Code Parameters

# lambda is the task generation rate
# p_o is the percentage of the tasks that will be offloaded to the edge server
# L is the task size in bits
# v is the number of CPU cycles required to process 1 bit data of task
# f_l is the CPU cycles per seconds of the IoT device
# f_e is the CPU cycles per seconds of the edge server
# mew_l is the task execution delay for local executation
# mew_e is the task executation delay on edge server
# Tw_l is the task expected waiting time for the IoT device
# Tw_e is the task expected waiting time for the edge server

# Simulation Parameters
lambda <- 0.1 # task generation rate
p_o <- 0.1 # percentage of the tasks that will be offloaded to the edge server
L <- 4 * 10 ^ 6 # task size in bits
v <- 500 # number of CPU cycles required to process 1 bit data of task
f_l <- 500 * 10 ^ 6 # CPU cycles per seconds of the IoT device
f_e <- 10 * 10 ^ 9 # CPU cycles per seconds of the edge server

# Function to calculate the task execution delay
task_execution_delay <- function(L, v, f) {
    result <- (L * v) / f
    return(result)
}

# Function to calculate the arrival rate of the tasks to be processed
# in the IoT device
task_arrival_rate_device <- function(p_o, lambda) {
    result <- (1 - p_o) * lambda
    return(result)
}

# Function to calculate the arrival rate of the tasks to be processed
# in the edge server
task_arrival_rate_edge <- function(p_o, lambda) {
    result <- p_o * lambda
    return(result)
}


# Function to calculate the task expected waiting time
task_expected_waiting_time <- function(lambda, mew) {
    row <- lambda / mew
    tmp <- 2 * mew * (1 - row)
    result <- row / tmp
    return(result)
}

# Function to calculate the task expected waiting time for the device
task_expected_waiting_time_device <- function(L, v, f_l, p_o, lambda) {
    mew_l <- task_execution_delay(L, v, f_l)
    lambda_l <- task_arrival_rate_device(p_o, lambda)
    result <- task_expected_waiting_time(lambda_l, mew_l)
    return(result)
}

# Function to calculate the task expected waiting time for the edge server
task_expected_waiting_time_edge <- function(L, v, f_e, p_o, lambda) {
    mew_e <- task_execution_delay(L, v, f_l)
    lambda_e <- task_arrival_rate_edge(p_o, lambda)
    result <- task_expected_waiting_time(lambda_e, mew_e)
    return(result)
}

# Computation

mew_l <- task_execution_delay(L, v, f_l)
mew_e <- task_execution_delay(L, v, f_e)

lambda_l <- task_arrival_rate_device(p_o, lambda)
lambda_e <- task_arrival_rate_edge(p_o, lambda)

Tw_l <- task_expected_waiting_time_device(L, v, f_l, p_o, lambda)
Tw_e <- task_expected_waiting_time_edge(L, v, f_e, p_o, lambda)

print("For the IoT Device")
print(paste("Task Executation Delay = ", mew_l))
print(paste("Task Arrival Rate = ", lambda_l))
print(paste("Task Expected Waiting Time = ", Tw_l))
cat("\n")
print("For the Edge Server")
print(paste("Task Executation Delay = ", mew_e))
print(paste("Task Arrival Rate = ", lambda_e))
print(paste("Task Expected Waiting Time = ", Tw_e))