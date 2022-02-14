source("parameters.r")
source("task_computational_model.r")
source("energy_consumption_model.r")
source("communication_model_tdma.r")

# Function to calculate the time to offload a task
time_to_offload_task <- function(D, R) {
    result <- D / R
    return(result)
}

# For Communication Model TDMA
R_tdma <- upload_data_rate_device_tdma(W, sigma_2, h, P_t)

t_o <- time_to_offload_task(D, R_tdma)

delta <- expected_delay(p_o, Tw_l, Tw_e, mew_l, mew_e, t_o)
E_l <- energy_cost_local(C_e, L, f_l, v, D, lambda, p_o)
E_o <- energy_cost_offload(p_i, t_o, lambda, p_o)

print(paste("Time to offload a task = ", t_o))
print(paste("Energy cost incurred when a task is processed locally = ", E_l))
print(paste("Energy cost incurred when offloading a task = ", E_o))