library(tidyverse)
library(stringr)
library(plotly)
# library(scales)

'%|%' <- function(a="",b="") paste0(a,b)
df_flash_prop_by_year <- read_rds("df_flash_prop_by_year.RDs")
rankings <- readRDS("ifsc_rankings_until_Hachioji.RDs")

tmp_df <- df_flash_prop_by_year %>%
  group_by(name) %>%
  summarise(n_comp = first(n_comp),
            sex = first(sex))

climber_names  <- tmp_df$name
climber_n_comp <- tmp_df$n_comp
climber_sex    <- tmp_df$sex

rm(tmp_df)

selected_climbers_default <-
  list("Akiyo Noguchi", 
       "Anna Stöhr", 
       "Shauna Coxsey", 
       "Miho Nonaka")

#Transformer object for reversed sqrt scale
# rev_sqrt_trans <- trans_new("rev_sqrt", 
                            # function(x) -sqrt(x), 
                            # function(x) (-x)^2, 
                            # function(x) bquote(.((-x)^2)))

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  # titlePanel("IFSC Statistics"),
  title = "IFSC Statistics",
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "climbers",
        label = "Climbers: ",
        choices = climber_names,
        selected = selected_climbers_default,
        multiple = TRUE),
      
      sliderInput(
        "n_comp_filter",
        "Filter by min number of comps",
        min = 0,
        max = max(climber_n_comp),
        value = 5,
        step = 1
      ),
      
      radioButtons(
        "sex_filter",
        "Show: ",
        choices = list("women", "men", "all"),
        selected = list("women"),
        inline = T
      ),
      
      width = 3
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotlyOutput("WR_plot"),
              plotlyOutput("flash_prop_by_year_plot")
              )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Filter the dataset 
  filtered_climber_names <- reactive({
    show_these <- climber_n_comp >= input$n_comp_filter
    
    show_these <- switch(input$sex_filter,
           all   = show_these,
           men   = show_these & climber_sex == "men",
           women = show_these & climber_sex == "women")
    
    climber_names[show_these]
  })
  
  observe({
    updateSelectizeInput(session, "climbers",
                         choices = filtered_climber_names(),
                         selected = input$climbers)
  })
  output$WR_plot <- renderPlotly({
    req(input$climbers)
    
    rankings %>%
      filter(name %in% input$climbers) %>%
      ggplot(aes(x = year, y = rank, color = name)) +
      geom_line() + geom_point() +
      scale_x_continuous(breaks = function(x) floor(x[1]):ceiling(x[2])) +
      scale_y_reverse(
        breaks = 1:100,
        labels = {
          zz <- 1:100
          zz[-c(1:4,6,8,seq(10,30,by = 5), seq(35,100,by=5))] <- ""
          zz
        }
        # trans = rev_sqrt_trans
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1
        )
      )
  })
  
  output$flash_prop_by_year_plot <- renderPlotly({
    req(input$climbers)
    
    df_flash_prop_by_year %>%
      filter(name %in% input$climbers) %>%
      ggplot(aes(x = year, y = flash_prob, color = name)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = 2007:2017) +
      ylab("Flash proportion") +
      theme_minimal() +
      theme(
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1
        )
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

