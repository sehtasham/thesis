
# Function to covert dbm to watt
dbm_to_watt <- function(P_dbm) {
    result <- 1 * (10 ^ (P_dbm / 10)) / 1000
    return(result)
}

# Function to covert watt to dbm
watt_to_dbm <- function(P_watt) {
    result <- 10 * log10(P_watt) + 30
    return(result)
}
