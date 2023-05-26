library(tidyverse)
library(arrow)
library(summarytools)


df <- arrow::read_parquet(here::here("data", "raw", "df_raw.parquet")) |> 
  janitor::clean_names()


View(df)





# Quick Exploration -------------------------------------------------------

## Option 1:
summary(df)

## Option 2:
st_options(use.x11 = FALSE)
view(summarytools::dfSummary(df))





# Data Transformation ----------------------------------------

## Creating preceding vehicle variables

df <- df |> 
  dplyr::group_by(frame_id) |>  # grouping by frame id
  dplyr::mutate(preceding_local_y = local_y[match(preceding, vehicle_id)],
                preceding_length = v_length[match(preceding, vehicle_id)],
                preceding_width = v_width[match(preceding, vehicle_id)],
                preceding_class= v_class[match(preceding, vehicle_id)],
                preceding_vel = v_vel[match(preceding, vehicle_id)],
                preceding_acc = v_acc[match(preceding, vehicle_id)]) |> #filter(vehicle_id %in% c(1543, 1555)) |> View()
  dplyr::ungroup()






## Removing some columns, converting units to metric, and changing the data type of some columns

### Option 1
df |> 
  dplyr::select(-c(total_frames, dplyr::starts_with("global"), following)) |>  # Keeping the desired columns only
  dplyr::mutate(v_length_m = round(v_length * 0.3048, 1)) # Convert to metric units

### Option 2
df <- df |>
  dplyr::select(-c(total_frames, dplyr::starts_with("global"), following)) |> # Keeping the desired columns only
  dplyr::mutate(across(.cols = c(starts_with("local"), starts_with("v_"), "space_headway", starts_with("preceding"), -preceding, -preceding_class, -v_class), 
                       .fns = ~ round(.x * .3048, 2))) |>  # Convert to metric units
  dplyr::mutate(across(.cols = c(vehicle_id, v_class, lane_id, preceding, preceding_class), 
                       .fns = ~ as.factor(.x))) # Convert a few columns to factor type





  






## Keeping data for lanes 1, 2 and 3 only and removing cases where there is no preceding vehicle

### How many unique vehicle ids?
length(unique(df$vehicle_id)) # 1725



unique(df$lane_id)
levels(df$preceding)



df <- df |> 
  dplyr::filter(lane_id %in% c("1", "2", "3"),
                preceding != "0")   
  

length(unique(df$vehicle_id)) # 903




## Create pair id, find car-following duration, filter out long time headway

df <- df |> 
  dplyr::group_by(vehicle_id) |> 
  mutate(num_of_lanes = length(unique(lane_id))) |> # find if a vehicle changed lane and how many times
  dplyr::ungroup() |> 
  filter(num_of_lanes == 1) |>  # remove any lane-changing vehicles
  select(-num_of_lanes) |> 
  mutate(pair_id = paste0(vehicle_id, "-", preceding)) |>  # create a pair_id column
  group_by(pair_id) |> 
  mutate(time = (0:(n()-1))/10, # elapsed time
         duration = tail(time, 1), # car-following pair was observed for this duration
         any_time_headway_gtoet_5 = any(time_headway >= 5)) |> # check if time headway was greater than or equal to 5 sec at any instant
  ungroup() |> 
  filter(duration >= 30,
         any_time_headway_gtoet_5 == FALSE) |> # remove any vehicle that had time headway greater than or equal to 5 sec at any instant
  select(-any_time_headway_gtoet_5) 


length(unique(df$pair_id)) # 142 pairs

View(df)



## Little housekeeping

df <- df |> 
  dplyr::relocate(pair_id, time, duration, everything())



# Quick Exploration (once again) -------------------------------------------------------
view(summarytools::dfSummary(df))





# Save --------------------------------------------------------------------

arrow::write_parquet(df, here::here("data", "tidy", "df.parquet"))
