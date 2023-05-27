library(shiny)
library(plotly)
library(tidyverse)
library(arrow)
library(DT)


# Global ------------------------------------------------------------------
## Data
df <- arrow::read_parquet(here::here("data", "tidy", "df.parquet"))

## Function
source(here::here("code", "final", "functions", "simulate_gipps.R"))





# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      
      
      selectInput("driver", label = h4("Select Vehicle Pair"), 
                  choices = unique(df$pair_id), 
                  selected = "47-39"),
      
     sliderInput("Vn", "Desired Speed (V_n):", 
                 min = 10, max = 25, value = 20),
     sliderInput("an", "Max. Acceleration Rate (a_n):", 
                 min = 0.1, max = 3.5, value = 2.5),
     sliderInput("tau", "Reaction Time (tau):", 
                 min = 0.5, max = 2.0, value = 1.0),
     sliderInput("bn_const", "Max. Deceleration Rate (b_n):", 
                 max = -0.1, min = -3.5, value = -1.5),
     sliderInput("b_cap", "Perceived Deceleration Rate of Preceding Vehicle (b_cap):", 
                 max = -0.1, min = -3.5, value = -2.0)
        ),
      
    
    mainPanel(
     
      plotlyOutput("cplot"),
      DTOutput('data_table')
    )
)
)















# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  driver1 <- reactive({
    df %>% 
      filter(pair_id == input$driver)
  })
  
  res <- reactive({
    result <- simulate_gipps(
      resolution = 0.1, 
      dfn1 = driver1(),
      xn1 = driver1()$preceding_local_y, 
      vn1 = driver1()$preceding_vel, 
      xn_first = driver1()$local_y[1], 
      vn_first = driver1()$v_vel[1], 
      ln = driver1()$v_length[1], 
      an = input$an,
      Vn = input$Vn, 
      tau = input$tau, 
      bn_const = input$bn_const, 
      bcap = input$b_cap
    )
    
    result |> 
      mutate(across(.cols = bn:deltav, .fns = ~ round(.x, 2)))
  })
  
  output$cplot <- renderPlotly({
    
    plot_speed <- ggplot() +
      geom_line(data = driver1(), aes(time, v_vel, color = "Subject Speed (Obs)")) +
      geom_line(data = res(), aes(Time, vn1, color = "Preceding Speed (Obs)")) +
      geom_line(data = res(), aes(Time, vn, color = "Subject Speed (Gipps)")) +
      scale_color_manual(values = c("grey70", "darkblue", "skyblue")) +
      labs(title = "Observed and Predicted Speed Trajectories",
           x = "Time (s)",
           y = "Speed (m/s)",
           color = "") +
      theme_classic()
    
    ggplotly(plot_speed)
    
  })
  
  
  
  output$data_table <- renderDT(res())
  
  
}



# App ---------------------------------------------------------------------
shinyApp(ui, server)
