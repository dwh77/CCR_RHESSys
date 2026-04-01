## Testing RHESSys Q conversions for mm to cfs based on tutorial data
#Data: https://github.com/ryanrbart/rhessys_training_2021/blob/main/data/streamflow/q_tule.csv
#A function: https://github.com/ryanrbart/EcoHydroConversions/blob/main/R/cfs_to_mm.R

## The code below was written before finding the linked function above
# q in cfs from first row is 2.8 cfs and 0.1315 mm
# The TULE station ID from USGS gives a basin area of 20.1 square miles

#so first lets get metric
#cfs to cms
q_cms <- 2.8 / 35.315

#cms to cubic meter per day (cmd)
q_cmd <- q_cms * 86400

#square miles to sqaure meters
SA_m2 <- 20.1 * 2.59e+6


#now from cmd to m/d
m_d <- q_cmd/SA_m2

#to mm_d
mm_d <- m_d * 1000
