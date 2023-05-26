library(tidyverse)
library(arrow)




df <- arrow::read_parquet(here::here("data", "tidy", "df.parquet"))


# Gipps Model Calculations for One Pair ------------------------------------------------
## Equation: https://umairdurrani.com/posts/2022-08-15-gipps-model-in-r-and-julia/gipps.html


df_47 <- df |>
  filter(vehicle_id == "47")


ggplot(df_47) +
  geom_line(aes(time, v_vel))



## Model parameters:
dfn1 <- df_47 # renaming dataset
resolution <- 0.1
an <- 2
Vn <- 22
tau <- 1.2
bn_const <- -1.5
bcap <- -2.0
ln1 <- 4.08


## Time:
### Last time of the simulation:
last_time <- (nrow(dfn1) - 1) * resolution # 1 is subtracted as we'll start at zero

### Time vector:
Time <- seq(from = 0, to = last_time, by = resolution)

### Length of the Time vector
time_length <- length(Time)





## Initialize empty vectors (they will later contain data):

# free-flow speed
vn_ff <- rep(NA_real_, time_length)

# car-following speed
vn_cf <- rep(NA_real_, time_length)

# speed
vn <- rep(NA_real_, time_length)

# position
xn <- rep(NA_real_, time_length)

# spacing
sn <- rep(NA_real_, time_length)

# speed difference
deltav <- rep(NA_real_, time_length)

# acceleration rate
bn <- rep(NA_real_, time_length)





## Initial values of Subject Vehicle (these are also required inputs):

# speed
vn_first <- dfn1$v_vel[1]

vn_ff[1] <- vn_first
vn_cf[1] <- vn_first
vn[1] <- vn_first

# position
xn_first <- dfn1$local_y[1]

xn[1] <- xn_first

# spacing
xn1 <- dfn1$preceding_local_y

sn[1] <- xn1[1] - xn_first

# speed difference
vn1 <- dfn1$preceding_vel

deltav[1] <- vn_first - vn1[1]




###### Gipps Calculations ############################


## free flow
vn_ff[2] <- vn[1] +
  (2.5 * an * tau * (1 - (vn[1]) / Vn)) * ((0.025 + (vn[1] / Vn))^(0.5))

## car following
bcap_part_cf <- (((vn1[1])^2) / bcap)

vn_cf[2] <- (bn_const * tau) +
  sqrt(
    ((bn_const^2) * (tau^2)) - (bn_const * (2 * (xn1[1] - ln1 - xn[1]) - (vn[1] * tau) - bcap_part_cf))
  )

## gipps speed
if (is.na(vn1[1])) {
  vn[2] <- vn_ff[2]
} else {
  vn[2] <- min(vn_ff[2], vn_cf[2])
}

### if the speed is negative, make it zero
vn[2] <- ifelse(vn[2] < 0, 0, vn[2])

## acceleration
bn[1] <- (vn[2] - vn[1]) / (resolution)

## position
xn[2] <- xn[1] + (vn[1] * resolution) + (0.5 * bn[1] * (resolution)^2)

# spacing
sn[2] <- xn1[2] - xn[2] - ln1

# speed difference
deltav[2] <- vn[2] - vn1[2]


















#######################################################################
## Let's do the calculations for all time frames using a for-loop:
#######################################################################

for (t in 2:(time_length)) {
  
  ## free flow
  vn_ff[t] <- vn[t - 1] +
    (2.5 * an * tau * (1 - (vn[t - 1]) / Vn)) * ((0.025 + (vn[t - 1] / Vn))^(0.5))
  
  ## car following
  bcap_part_cf <- (((vn1[t - 1])^2) / bcap)
  
  vn_cf[t] <- (bn_const * tau) +
    sqrt(
      ((bn_const^2) * (tau^2)) - (bn_const * (2 * (xn1[t - 1] - ln1 - xn[t - 1]) - (vn[t - 1] * tau) - bcap_part_cf))
    )
  
  ## gipps speed
  if (is.na(vn1[t - 1])) {
    vn[t] <- vn_ff[t]
  } else {
    vn[t] <- min(vn_ff[t], vn_cf[t])
  }
  
  ### if the speed is negative, make it zero
  vn[t] <- ifelse(vn[t] < 0, 0, vn[t])
  
  ## acceleration
  bn[t - 1] <- (vn[t] - vn[t - 1]) / (resolution)
  
  ## position
  xn[t] <- xn[t - 1] + (vn[t - 1] * resolution) + (0.5 * bn[t - 1] * (resolution)^2)
  
  # spacing
  sn[t] <- xn1[t] - xn[t] - ln1
  
  # speed difference
  deltav[t] <- vn[t] - vn1[t]
}













# ################## Result in a dataframe ###################################
result_dfn <- data.frame(Time, xn1, vn1, ln1, bcap, bn, xn, vn_ff, vn_cf, vn, sn, deltav)

plotly::ggplotly(
  
  ggplot() +
    geom_line(data = dfn1, aes(time, v_vel, color = "Subject Speed (Obs")) +
    geom_line(data = result_dfn, aes(Time, vn1, color = "Preceding Speed (Obs")) +
    geom_line(data = result_dfn, aes(Time, vn, color = "Subject Speed (Gipps"))
  
)





