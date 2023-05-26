library(tidyverse)
library(arrow)
library(patchwork)
library(trelliscopejs)
library(plotly)


df <- arrow::read_parquet(here::here("data", "tidy", "df.parquet"))


length(unique(df$pair_id)) # 142 pairs

View(df)



# Visualizing 1 pair ------------------------------------------------------

df_47 <- df |> 
  filter(vehicle_id == "47")

View(df_47)


ggplot() # Empty canvas

ggplot(data = df_47) # Empty canvas


ggplot(data = df_47, 
       mapping = aes(x = time, y = v_vel)) # aesthetic mappings


ggplot(data = df_47, 
       mapping = aes(x = time, y = v_vel)) +
  geom_point() # geometry to draw: points


ggplot(data = df_47, 
       mapping = aes(x = time, y = v_vel)) +
  geom_line() # geometry to draw: line


## Optionally provide the mappings to the geom_* function:

ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = v_vel)) # same result as above


## Change labels

ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = v_vel)) +
  labs(title = "Speed of vehicle 47 over time",
       x = "Time (s)",
       y = "Speed (m/s)")


## Change theme

ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = v_vel)) +
  labs(title = "Speed of vehicle 47 over time",
       x = "Time (s)",
       y = "Speed (m/s)") +
  theme_classic()


## Add preceding vehicle speed

ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = v_vel)) +
  geom_line(mapping = aes(x = time, y = preceding_vel)) +
  labs(title = "Vehicle speeds",
       x = "Time (s)",
       y = "Speed (m/s)") +
  theme_classic()


## Give different colors to different vehicles

ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = v_vel, color = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = preceding_vel, color = "Preceding Vehicle")) +
  labs(title = "Vehicle speeds",
       x = "Time (s)",
       y = "Speed (m/s)") +
  theme_classic()


## Or linetypes

ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = v_vel, linetype = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = preceding_vel, linetype = "Preceding Vehicle")) +
  labs(title = "Vehicle speeds",
       x = "Time (s)",
       y = "Speed (m/s)") +
  theme_classic()



## Want to use different colors? Add scale:

### default scales:

ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = v_vel, color = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = preceding_vel, color = "Preceding Vehicle")) +
  scale_color_brewer() +
  labs(title = "Vehicle speeds",
       x = "Time (s)",
       y = "Speed (m/s)") +
  theme_classic()


ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = v_vel, color = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = preceding_vel, color = "Preceding Vehicle")) +
  scale_color_viridis_d() +
  labs(title = "Vehicle speeds",
       x = "Time (s)",
       y = "Speed (m/s)") +
  theme_classic()


### Use your own colors:

ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = v_vel, color = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = preceding_vel, color = "Preceding Vehicle")) +
  scale_color_manual(values = c("grey50", "blue")) +
  labs(title = "Vehicle speeds",
       x = "Time (s)",
       y = "Speed (m/s)") +
  theme_classic()






## Other variables

names(df_47)

### position
ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = local_y)) +
  labs(x = "Time (s)",
       y = "Longitudinal position (m)") +
  theme_classic()


### space_headway
ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = space_headway)) +
  labs(x = "Time (s)",
       y = "Spacing (m)") +
  theme_classic()


### time_headway
ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = time_headway)) +
  labs(x = "Time (s)",
       y = "Time headway (s)") +
  theme_classic()


### acceleration
ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = v_acc, color = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = preceding_acc, color = "Preceding Vehicle")) +
  scale_color_manual(values = c("grey50", "blue")) +
  labs(x = "Time (s)",
       y = "Acceleration (m/s^2)") +
  theme_classic()




## make your plot interactive:
plotly::ggplotly(
  
  ggplot(data = df_47) +
    geom_line(mapping = aes(x = time, y = v_acc, color = "Subject Vehicle")) +
    geom_line(mapping = aes(x = time, y = preceding_acc, color = "Preceding Vehicle")) +
    scale_color_manual(values = c("grey50", "blue")) +
    labs(x = "Time (s)",
         y = "Acceleration (m/s^2)") +
    theme_classic()
)








# Combine multiple plots for 1 pair ---------------------------------------

speed <- ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = v_vel, color = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = preceding_vel, color = "Preceding Vehicle")) +
  scale_color_manual(values = c("grey50", "blue")) +
  labs(title = "Vehicle 47",
       x = "Time (s)",
       y = "Speed (m/s)") +
  theme_classic()



accel <- ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = v_acc, color = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = preceding_acc, color = "Preceding Vehicle")) +
  scale_color_manual(values = c("grey50", "blue")) +
  labs(x = "Time (s)",
       y = "Acceleration (m/s^2)") +
  theme_classic()


spacing <- ggplot(data = df_47) +
  geom_line(mapping = aes(x = time, y = space_headway)) +
  labs(x = "Time (s)",
       y = "Spacing (m)") +
  theme_classic()


## patchwork magic:

speed / accel / spacing


### Combine legends

(speed / accel + plot_layout(guides = "collect")) / spacing







# Visualizing all data ----------------------------------------------------

length(unique(df$pair_id))

## Speed

ggplot(data = df) + # <- changed data
  geom_line(mapping = aes(x = time, y = v_vel, color = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = preceding_vel, color = "Preceding Vehicle")) +
  scale_color_manual(values = c("grey50", "blue")) +
  facet_wrap(~ pair_id) + # <- added facets
  labs(x = "Time (s)",
       y = "Speed (m/s)") +
  theme_classic()


### Use trelliscope instead!

ggplot(data = df) + # <- changed data
  geom_line(mapping = aes(x = time, y = v_vel, color = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = preceding_vel, color = "Preceding Vehicle")) +
  scale_color_manual(values = c("grey50", "blue")) +
  facet_trelliscope(~ pair_id, self_contained = TRUE) + # <- trelliscope facets
  labs(x = "Time (s)",
       y = "Speed (m/s)") +
  theme_classic()


ggplot(data = df) + # <- changed data
  geom_line(mapping = aes(x = time, y = v_vel, color = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = preceding_vel, color = "Preceding Vehicle")) +
  scale_color_manual(values = c("grey50", "blue")) +
  facet_trelliscope(~ pair_id, self_contained = TRUE, as_plotly = TRUE) + # <- INTERACTIVITY!
  labs(x = "Time (s)",
       y = "Speed (m/s)") +
  theme_classic()


# What about multiple plots per pair but for all pairs?
