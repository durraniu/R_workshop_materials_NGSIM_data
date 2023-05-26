library(tidyverse)
library(arrow)
library(patchwork)
library(officer)
library(flextable)
library(rvg)



df <- arrow::read_parquet(here::here("data", "tidy", "df.parquet"))



# Save plots as images ----------------------------------------------------

## Let's use our previous function to create plots for a single pair and then store it on disk:
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


df_47 <- df |> filter(vehicle_id == "47")


plot_multiple_vars_per_pair(df_47)

final_plot <- plot_multiple_vars_per_pair(df_47)

## save:
ggplot2::ggsave(filename = here::here("results", "figures", 
                                      "final_trajectory_plot.png"), 
                plot = final_plot)

### change dimension size:
ggplot2::ggsave(filename = here::here("results", "figures", 
                                      "final_trajectory_plot_size_adjusted.png"), 
                plot = final_plot, width = 6, height = 6)

### increase resolution if needed:
ggplot2::ggsave(filename = here::here("results", "figures", 
                                      "final_trajectory_plot_size_res_adjusted.png"), 
                plot = final_plot, width = 6, height = 6, dpi = 600)







# Save table to a Word document -------------------------------------------

## Create a table of results:

results_table <- df |> 
  select(v_class, v_vel, v_acc, space_headway, preceding_vel, preceding_acc) |> 
  group_by(v_class) |> 
  summarise(across(.cols = where(is.numeric), .fns = median))



## Make it pretty:
flextable(results_table) 


my_ft <- flextable(results_table) |> 
  theme_vanilla()

my_ft


my_ft <- my_ft |> 
  set_header_labels(
    v_class = "Class",
    v_vel = "Speed (m/s)",
    v_acc = "Acceleration (m/s^2)",
    space_headway = "Spacing (m)",
    preceding_vel = "Speed (m/s)",
    preceding_acc = "Acceleration (m/s^2)"
  ) |> 
  italic(j = 1) |> 
  color(~ preceding_acc < 0.00, ~ preceding_acc, color = "red") |> 
  bold(~ preceding_acc < 0.00, ~ preceding_acc, bold = TRUE) |> 
  add_header_row(
    values = c("Subject Vehicle", "Preceding Vehicle"),
    colwidths = c(4, 2)
    ) |> 
  align(i = 1, part = "header", align = "center")


### save to a Word doc:
my_ft |>  
  save_as_docx(path = here::here("results", "mytable.docx"))






# Send table and plot to a PowerPoint presentation -------------------------------------------

final_plot

my_vec_graph <- rvg::dml(ggobj = final_plot)

doc <- officer::read_pptx()

doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")

doc <- ph_with(doc, my_vec_graph, location = ph_location_fullsize())

doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")

doc <- ph_with(doc, my_ft, location = ph_location_left())

print(doc, target = here::here("results", "my_ppt.pptx"))
