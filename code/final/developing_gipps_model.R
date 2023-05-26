library(tidyverse)
library(arrow)




df <- arrow::read_parquet(here::here("data", "tidy", "df.parquet"))



df_47 <- df |>
  filter(vehicle_id == "47")


ggplot(df_47) +
  geom_line(aes(time, v_vel))


names(df_47)


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
  tau = 1.2, 
  bn_const = -1.5, 
  bcap = -2.0
)


ggplot() +
  geom_line(data = df_47, aes(time, v_vel, color = "Subject Speed (Obs")) +
  geom_line(data = res, aes(Time, vn1, color = "Preceding Speed (Obs")) +
  geom_line(data = res, aes(Time, vn, color = "Subject Speed (Gipps"))
