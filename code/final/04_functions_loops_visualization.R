library(tidyverse)
library(arrow)
library(patchwork)
library(trelliscopejs)
library(plotly)

df <- arrow::read_parquet(here::here("data", "tidy", "df.parquet"))

length(unique(df$pair_id)) # 142 pairs

View(df)





# Functions ---------------------------------------------------------------

## Creating a custom function (examples)

### 1 foot = 0.3048 meters. Say x is in ft and you want to convert it to m:
x <- 1:5
x
x_m <- x * 0.3048
x_m


### write a function to convert any vector containing values in ft to m:
#### define the function:
convert_ft_to_m <- function(input_vector_in_ft){
  
  output_vector_in_m <- input_vector_in_ft * 0.3048
  
  return(output_vector_in_m)
}

#### now use it:
convert_ft_to_m(1) # input is a vector with single value
convert_ft_to_m(x) # input is a vector with multiple values
x_m <- convert_ft_to_m(x) # make sure that you're storing the results in a new variable/object
x_m


#### It does not matter what names you use for INPUTS and OUTPUTS. 

output_vector_in_m # throws error because this NAME does not exist outside of the function


#### They are only available within the context of the function:

convert_ft_to_m <- function(x){
  
  x_m <- x * 0.3048
  
  return(x_m)
}

convert_ft_to_m(1) # input is a vector with single value
convert_ft_to_m(x) # input is a vector with multiple values
x_m <- convert_ft_to_m(x) # make sure that you're storing the results in a new variable/object
x_m











# Iteration (repeatedly using a function) ---------------------------------------------------------------

## Assume you have a list with multiple items:
my_list <- list(1, 2, 3)

my_list # full list
my_list[[1]] # first item of the list

## A list may contain any data type for any item:
another_list <- list(TRUE, c(1, 2, 3), c("four", "five", "six"))
another_list
another_list[[1]]
another_list[[2]]
another_list[[3]]

### You may want to check the data type of each item in the list:
class(another_list[[1]])
class(another_list[[2]])
class(another_list[[3]])

### You are repeating the function class() by re-typing it. There is another way:
purrr::map(.x = another_list, .f = class)


## It gets easier to understand if your list items have names:
another_list_named <- list(first_item = TRUE, second_item = c(1, 2, 3), third_item = c("four", "five", "six"))
another_list_named

purrr::map(.x = another_list_named, .f = class)

## May be you want to write your own function and apply that to each item in the list:
### custom function:
add_one <- function(x){
  
  if (class(x) == "numeric"){
    output <- x + 1
  } else {
    output <- x
  }
  
  output # return function is optional
}

#### test it:
add_one(1)
add_one(c(1, 2))

#### apply it for the intended purpose:
another_list_named
purrr::map(.x = another_list_named, .f = add_one)




## A dataframe contains vectors as individual columns, BUT it may also contain list columns:
mtcars |> tibble() # a built-in dataset in R

unique(mtcars$cyl)

mtcars |> 
  group_by(cyl)

mtcars |> 
  group_nest(cyl) # NOTE: data is a list column!


ggplot(mtcars) +
  geom_point(aes(x = wt, y = mpg))

plot_mpg_and_wt <- function(dataset){
  
  cyl <- unique(dataset$cyl)
  
  ggplot(dataset) +
    geom_point(aes(x = wt, y = mpg)) +
    labs(title = paste0("Cyl: ", cyl))
  
}

plot_mpg_and_wt(mtcars)

### Create a dataframe with plots
mtcars_nested <- mtcars |> 
  group_nest(cyl, keep = TRUE) |> 
  mutate(mpg_vs_wt_plot = purrr::map(.x = data, .f = plot_mpg_and_wt))

mtcars_nested$mpg_vs_wt_plot[[1]]
mtcars_nested$mpg_vs_wt_plot[[2]]
mtcars_nested$mpg_vs_wt_plot[[3]]



### Create a dataframe with plots and combine plots
mtcars_nested_tjs <- mtcars |> 
  group_nest(cyl, keep = TRUE) |> 
  mutate(mpg_vs_wt_plot = trelliscopejs::map_plot(.x = data, .f = plot_mpg_and_wt))


mtcars_nested_tjs %>%
  trelliscope(name = "Mileage against weight for different cylinders", nrow = 1, ncol = 3, self_contained = TRUE)






# Visualize all the trajectory data --------------------------------------------------

## Use a custom function to create multiple plots & combine them per pair and do this for all the pairs:

plot_multiple_vars_per_pair <- function(dataset){
  
  # Combine multiple plots for 1 pair 
  
  plot_speed <- ggplot(data = dataset) +
    geom_line(mapping = aes(x = time, y = v_vel, color = "Subject Vehicle")) +
    geom_line(mapping = aes(x = time, y = preceding_vel, color = "Preceding Vehicle")) +
    scale_color_manual(values = c("grey50", "blue")) +
    labs(x = "Time (s)",
         y = "Speed (m/s)") +
    theme_classic()
  
  
  
  plot_accel <- ggplot(data = dataset) +
    geom_line(mapping = aes(x = time, y = v_acc, color = "Subject Vehicle")) +
    geom_line(mapping = aes(x = time, y = preceding_acc, color = "Preceding Vehicle")) +
    scale_color_manual(values = c("grey50", "blue")) +
    labs(x = "Time (s)",
         y = "Acceleration (m/s^2)") +
    theme_classic()
  
  
  plot_spacing <- ggplot(data = dataset) +
    geom_line(mapping = aes(x = time, y = space_headway)) +
    labs(x = "Time (s)",
         y = "Spacing (m)") +
    theme_classic()
  
  
  
  ### Combine plots
  (plot_speed / plot_accel + plot_layout(guides = "collect")) / plot_spacing

}


## Apply the function:
df_nested_tjs <- df |> 
  group_nest(pair_id, keep = TRUE) |> 
  mutate(combined_plot = map_plot(.x = data, .f = plot_multiple_vars_per_pair))



df_nested_tjs$combined_plot[[1]]
df_nested_tjs$combined_plot[[142]]

df_nested_tjs  %>%
  trelliscope(name = "Trajectories", nrow = 1, ncol = 3, self_contained = TRUE)





