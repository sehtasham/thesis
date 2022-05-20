
upload_data_rate_device_node <- function(W, noise_power, h_i, P_i, theta_i) {
    tmp <- (P_i * h_i) / (theta_i * W * noise_power)
    result <- theta_i * W * log2(1 + tmp)
    return(result)
}