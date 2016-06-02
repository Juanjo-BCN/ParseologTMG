getDF <- function(log = "./data/LOG_WEB.w3c") {
  temp <- read.csv(log, header = TRUE, sep = "\t")
  #  df <- datos_elegantes(temp)
  saveRDS(temp, file="./data/dataframelog.rds")
  return(temp)
}

ip_in_CIDR <- function(ip, cidr) {
  long.ip <- ip2long(ip)
  cidr.parts <- unlist(strsplit(cidr, "/"))
  cidr.range <- ip2long(cidr.parts[1])
  cidr.mask <- bitops::bitShiftL(bitops::bitFlip(0), (32 - as.integer(cidr.parts[2])))
  return(bitops::bitAnd(long.ip, cidr.mask) == bitops::bitAnd(cidr.range, cidr.mask))
}

ip2long <- function(ip) {
  # transforma a vector de characters
  ips <- unlist(strsplit(ip, '.', fixed = TRUE))
  # set up a function to bit-shift, then "OR" the octets
  octet <- function(x,y) bitops::bitOr(bitops::bitShiftL(x, 8), y)
  # Reduce applys a function cumulatively left to right
  return(Reduce(octet, as.integer(ips)))
}

url <- "http://geolite.maxmind.com/download/geoip/database/GeoLite2-Country-CSV.zip"
