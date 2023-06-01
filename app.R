
#======================================
# Project 2
# Savings Rate Calculator 
# Noah Carlos
# 4 / 8 / 2022
#======================================



library(tidyverse)
library(shiny)
library(scales)
library(reshape2)
library(rsconnect)


# Define UI for application that draws a timeline
ui <- fluidPage(

    # Application title
    titlePanel("Savings Rate Calculator"),

    # Columns for widgets
    fluidRow(
      #Input for annual income
      column(2, 
             numericInput(inputId = "income",
                          label = "Enter Annual Income $",
                          min = 1,
                          value = 50000)
             
      ),
    
      #Input target amount
      column(2,
             numericInput(inputId = "target",
                          label = "Enter Savings Goal $",
                          min = 1,
                          value = 1000000)
             ),
      #Input current-age
      column(2,
             numericInput(inputId = "age",
                          label = "Enter Current-Age",
                          min = 1,
                          value = 25)
             ),
      #Select rate of return
      column(2,
             sliderInput(inputId = "rate",
                         label = "Choose Rate of Return %",
                         min = 0,
                         step = 0.5,
                         max = 100,
                         value = 5)
             )
      
    ),
    
    
    hr(),
    h4('Savings rate & the years to reach your goal'),
    plotOutput('plot1'),
    
    hr(),
    h4('Compare total contributions to total growth'),
    plotOutput('plot2'),
    
    hr(),
    h4('A summary of information for each possible savings rate'),
    DT::dataTableOutput('table')
)
    





# Define server logic required to draw a table
server <- function(input, output) {

    # Use reactive dataframe
    # Bar chart, years to reach target for various savings rates
  
    # dataframe for plot 1
    target_chart <- reactive({
      #Evaluate time variable to see how long it will take for each rate
      time_to_goal_var = log(((input$rate /100 * input$target)/(input$income * seq(from=5,to=105,by=5)/100))+1)/(log(1+(input$rate/100)))
       
      tbl1 = data.frame(
        savings_rate = seq(from=5,to=105,by=5),
        time_to_reach_goal = time_to_goal_var
      )
      return(tbl1)
    })
    
    
    # Code for plot 1
    output$plot1<- renderPlot({
      ggplot(data = target_chart(), aes(x = savings_rate, y = time_to_reach_goal)) +
        geom_col(fill = '#00b763', color = 'white') + 
        theme_classic() +
        labs(x = 'Savings Rates', y = 'Years to Reach Goal') + 
        scale_x_continuous(n.breaks = 25, limits=c(0,105), expand=c(0,0)) +
        scale_y_continuous(n.breaks = 20, expand = c(0,0))
        
    })
    
    

    
    
    # Dataframe for plot 2
    # Use reactive dataframe
    # "Total contributions" compared with "total growth"
    
    contributions_v_growth<- reactive({
      # Recycling our 'time_to_goal_var'
      time_to_goal_var = log(((input$rate /100 * input$target)/(input$income * seq(from=0,to=100,by=5)/100))+1)/(log(1+(input$rate/100)))
      
      # Total contributions vector
      total_c <- (input$income * (seq(from=0,to=100,by=5)/100))* time_to_goal_var
      
      #Total growth vector
      total_g <- rep(input$target, times=length(total_c))-total_c
      
      tbl2 = data.frame(
        savings_rate = seq(from=0,to=100,by=5),
        time_to_goal = time_to_goal_var,
        total_contributions = total_c,
        total_growth =  total_g,
        compare = c(total_c,total_g),
        labels = c('Total Growth','Total Contributions')

        
      )
      return(tbl2)
      
    })
    
    # Code for plot 2
    output$plot2<- renderPlot({
      ggplot(data = contributions_v_growth(),aes(x = savings_rate, color = labels)) +
        geom_point(stat = 'identity',aes(y = total_growth, colour = '#ff7802', size = I(3))) + 
        geom_point(stat = 'identity',aes(y = total_contributions, colour = '#00b763', size = I(3))) +
        scale_color_identity(name = "Legend",
                          labels = c('Total Contributions', 'Total Growth'),
                          guide = 'legend') +
        theme_classic() +
        labs(x = 'Savings Rates', y = 'Total Funds') + 
        scale_x_continuous(n.breaks = 25, limits=c(0,105), expand=c(0,0)) +
        scale_y_continuous(n.breaks = 10, expand = c(0,0), labels = comma) 

    })
    
    # Table for plot 3
    # Table of numeric values
    full_dat<-reactive({
    
    time_to_goal_var = log(((input$rate /100 * input$target)/(input$income * seq(from=5,to=100,by=5)/100))+1)/(log(1+(input$rate/100)))
    

    savings_r <- seq(from=5,to=100,by=5)
    annual_contribution <- as.numeric(input$income * (savings_r/100))
    total_c <- as.numeric(annual_contribution * time_to_goal_var)
    total_g <- as.numeric(rep(input$target, times=length(total_c))-total_c)
    percent_co <- as.numeric((total_c/input$target) * 100)
    percent_g <- as.numeric(100-percent_co)
    age_at_t <- as.integer(input$age + time_to_goal_var)
    
    tbl3<- data.frame(
      savings_rate <- savings_r,
      annual_contributions <- annual_contribution,
      total_contributions <- total_c,
      total_growth <- total_g,
      percent_contributed <-percent_co,
      percent_growth <- percent_g,
      number_of_years <- time_to_goal_var,
      age_at_time <- age_at_t

      
    )
    
    #Table Clean up
    colnames(tbl3) <- c('Savings Rate (%)', 'Annual Contributions ($)', 'Total Contributions ($)', 'Total Growth ($)', 'Percent Contribution (%)', 'Percent Growth (%)', 'Number of Years', 'Age at Target')
    tbl3[] <- lapply(tbl3, format, decimal.mark = ".", digits = 2)
    return(tbl3)
 
  })   
    # Code for plot 3
    output$table<- DT::renderDataTable({
      full_dat()
    })

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
