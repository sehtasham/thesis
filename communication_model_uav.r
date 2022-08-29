# Communication model

# The communication between a device i and a UAV-mounted base station


# Code Parameters

# Device denoted by i
# p_i is the device_power for device i
# W_i channel bandwidth between device i and the UAV
# di_UAV is the distance b/w device and UAV
# hi_UL is the uplink channel gain b/w device and UAV
# Ri_UAV is the Upload data rate between device and MBS
# alpha_0 is the received power at the reference distance of
# 1 m for a transmission power of 1 W
# L_UAV is the link for UAV
# L_i is the link for device i

# Function to calculate the uplink channel gain from a device i to the UAV
uplink_channl_gain_device_uav <- function(alpha_0, L_UAV, L_i) {
    di_UAV <- sqrt(sum((L_UAV - L_i) ^ 2))
    result <- alpha_0 / (di_UAV  ^ 2)
    return(result)
}

# Function to calculate Upload Data Rate
upload_data_rate_device_uav <- function(W_i, p_i, noise_power,
                                alpha_0, L_UAV, L_i) {
    hi_UL <- uplink_channl_gain_device_uav(alpha_0, L_UAV, L_i)
    tmp <- (hi_UL * p_i) / noise_power
    result <- W_i * log2(1 + tmp)
    return(result)
}