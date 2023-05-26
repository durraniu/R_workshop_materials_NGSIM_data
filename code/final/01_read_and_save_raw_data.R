library(tidyverse)
library(arrow)

df_raw <- readr::read_csv("https://media.githubusercontent.com/media/Shuoxuan/NGSIM_Cleaned_Dataset/main/I-80/EMD_Reconstructed(i80%200400-0415).csv")

View(df_raw)

arrow::write_parquet(df_raw, here::here("data", "raw", "df_raw.parquet"))
