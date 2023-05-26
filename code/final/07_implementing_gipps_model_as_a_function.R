library(tidyverse)
library(arrow)




df <- arrow::read_parquet(here::here("data", "tidy", "df.parquet"))


# Gipps Model Calculations for One Pair ------------------------------------------------
## Equation: https://umairdurrani.com/posts/2022-08-15-gipps-model-in-r-and-julia/gipps.html


df_47 <- df |>
  filter(vehicle_id == "47")



# Let's implement Gipps Model as a function -------------------------------

## This will allow us to provide model parameters and function inputs

## See the implementation in the functions folder
source(here::here("code", "final", "functions", "simulate_gipps.R"))

## Now, run it:
res <- simulate_gipps(
  resolution = 0.1, 
  dfn1 = df_47,
  xn1 = df_47$preceding_local_y, 
  vn1 = df_47$preceding_vel, 
  xn_first = df_47$local_y[1], 
  vn_first = df_47$v_vel[1], 
  ln = df_47$v_length[1], 
  an = 2,
  Vn = 22, 
  tau = 2, 
  bn_const = -1.5, 
  bcap = -2.0
)


ggplot() +
  geom_line(data = df_47, aes(time, v_vel, color = "Subject Speed (Obs")) +
  geom_line(data = res, aes(Time, vn1, color = "Preceding Speed (Obs")) +
  geom_line(data = res, aes(Time, vn, color = "Subject Speed (Gipps"))
