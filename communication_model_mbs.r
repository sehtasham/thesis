# Communication model

# A device will be associated to the Macro-cell base station (MBS) when
# it is not within the coverage area of any Small-cell base station (SBS)


# Code Parameters

# Device denoted by i
# Macro-cell base station (MBS)
# p_i is the device_power for device i
# W_m channel bandwidth between device i and the MBS
# r_i0 is the distance b/w device and MBS
# h_i0 is the channel gain b/w device and MBS
# alpha is the path loss exponent
# R_i0 is the Upload data rate between device and MBS


d_km <- seq(0.1, 10, by = 0.1) # Sequence of distance in Km
alpha <- 140 + 37.6 * log10(d_km)

# Function to calculate numerator in upload data rate equation for MBS
num_upload_data_rate_mbs <- function(p_i, h_i0, r_i0, alpha) {
  result <- p_i * h_i0 * (r_i0 ^ -alpha)
  return(result)
}

# Function to calculate Upload Data Rate
upload_data_rate_device_mbs() <- function(W_m, noise_power,
                                p_i, h_i0, r_i0, alpha) {
    tmp <- num_upload_data_rate_mbs(p_i, h_i0, r_i0, alpha) / noise_power
    result <- W_m * log2(1 + tmp)
    return(result)
}

R_i0 <- upload_data_rate_device_mbs(W_m, noise_power,
                            p_i, h_i0, r_i0, alpha)