# CUSTOM FUNCTION TO CONVERT COORDINATES FROM DEGREE DECIMAL MINUTES (DDM) TO
# DECIMAL DEGREE (DD) FORMAT. NOT PARTICULARLY GENERAL, WILL PROBABLY ONLY WORK
# WELL WITH PACIFIC NORTHWEST COORDINATES. 

conv_ddm_dd = function(lat, lon){
  
  # CONVERT LAT #
  
  # Split coordinate string into individual characters
  a = strsplit(lat,"")
  # Pull out the latitude degree (the first two characters of the string)
  b = sapply(a, function(x) x[1:2])
  # Convert the degree to numeric
  c = as.numeric(paste(b[1,], b[2,], sep=""))
  # Pull out the decimal minutes (last six characters of the string)
  e = sapply(a, function(x) x[4:9])
  # Convert the decimal minutes to numeric
  f = as.numeric(paste(e[1,], e[2,], e[3,], e[4,], e[5,], e[6,], sep=""))
  # Divide the decimal minutes by 60 to convert to decimal degrees
  f2 = f/60
  # Re-combine degrees and decimal for final output
  lat_dd = c+f2
  
  # CONVERT LONG #
  
  # Split coordinate string into individual characters
  a_long = strsplit(lon,"")
  # Pull out the longitude degree (the first three characters of the string)
  b_long = sapply(a_long, function(x) x[1:3])
  # Convert the degree to numeric
  c_long = as.numeric(paste(b_long[1,], b_long[2,], b_long[3,], sep=""))
  # Pull out the decimal minutes (last six characters of the string)
  e_long = sapply(a_long, function(x) x[5:10])
  # Convert the decimal minutes to numeric
  f_long = as.numeric(paste(e_long[1,], e_long[2,], e_long[3,], e_long[4,], e_long[5,], e_long[6,], sep=""))
  # Divide the decimal minutes by 60 to convert to decimal degrees
  f2_long = f_long/60
  # Re-combine degrees and decimal for final output
  long_dd = c_long+f2_long
  
  # Return output, adding a negative in front of the longitude to indicate Westerly
  output = data.frame(lat_dd=lat_dd, long_dd=-long_dd)
  
  return(output)
}
