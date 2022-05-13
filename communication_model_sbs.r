# Communication model

# Code Parameters

# Device denoted by i
# Small-cell base station (SBS) denoted by j
# p_i is the device_power for device i
# r_ij is the distance b/w device and SBS
# h_ij is the channel gain b/w device and SBS
# alpha is the path loss exponent
# R_ij is the Upload data rate between device and SBS
# U represents set of devices
# B represents set of base stations
# W represents bandwidth

# Assuming we have device IDs and base station IDs
# If we have data in the form of array,
# the IDs can also be in the form of order number of data from the list

B <- 10
U <- 100
noise_power <- 10 ^ -14
W <- 5 * 10 ^ 6
d_km <- seq(0.1, 10, by = 0.1) # Sequence of distance in Km
alpha <- 140 + 37.6 * log10(d_km)


# Function to calculate numerator in upload data rate equation
num_upload_data_rate_sbs <- function(p_i, h_ij, r_ij, alpha) {
  result <- p_i * h_ij * (r_ij ^ -alpha)
  return(result)
}

# Function to get power of device from the dataset
get_dev_power <- function(device) {
  #TODO depends on dataset
  return(result)
}

# Function to get channel gain between device and station from the dataset
get_channel_gain <- function(device, station) {
  #TODO depends on dataset
  return(result)
}

# Function to get distance between device and station from the dataset
get_device_station_distance_ <- function(device, station) {
  #TODO depends on dataset
  return(result)
}

# Function to calculate interference from other SBS
calc_interference <- function(i, j, U, B, alpha) {
  result <- 0
  for (device in U) {
    for (station in B) {
      if (device != i && station != j) {
        result <- result +
                num_upload_data_rate_sbs(get_dev_power(device),
                        get_channel_gain(device, station),
                                get_device_station_distance_(device, station),
                                alpha)
      }
  }
}
  return(result)
}

# Function to calculate Upload Data Rate between device and SBS
upload_data_rate_device_sbs <- function(W, noise_power,
                                p_i, h_ij, r_ij, i, j, U, B, alpha) {
    denom <- noise_power + calc_interference(i, j, U, B, alpha)
    tmp <- num_upload_data_rate_sbs(p_i, h_ij, r_ij, alpha) / denom
    result <- W * log2(1 + tmp)
    return(result)
}

R_ij <- upload_data_rate_device_sbs(W, noise_power,
                            p_i, h_ij, r_ij, i, j, U, B, alpha)
