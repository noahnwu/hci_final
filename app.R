#test page here. 

# from Noah: will make simulated data this week. 

library(shiny)
library(tidyverse)
diabetes = read_csv("diabetes_dataset.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Provider Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          #only diabetes works right now
          selectInput("dz", 
                      "What disease process?", 
                      choices = list("Diabetes", 
                                     "Heart Failure")), 
          conditionalPanel(
            condition = "input.dz == 'Diabetes'",
            selectInput("diabetes_drop", 
                        "Which diabetes-related condition/lab?", 
                        choices = as.list(names(diabetes)[!str_detect(names(diabetes), "id$")])
            ), 
            conditionalPanel(
              condition = "input.dz == 'Diabetes'", 
              radioButtons('due_filters', 
                           "Label Plot by Vital Due Dates", 
                           as.list(names(diabetes)[str_detect(names(diabetes), "due")]))
              
              
            )
          )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("condition_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$condition_plot <- renderPlot({
      if(input$dz == "Diabetes") { 
        if(class(diabetes %>% pull(input$diabetes_drop)) == "character") { 
          #categorical variables
          temp = diabetes %>% 
            group_by(across(all_of(c(input$diabetes_drop, input$due_filters)))) %>%  
            count()
          
          
          ggplot(temp, aes_string(input$diabetes_drop, "n", 
                                  fill = input$due_filters))+
            geom_col(color = "black")+
            theme_bw()+
            labs(x = "Values", 
                 y = "N", 
                 title = input$diabetes_drop)
        } else{ 
          ggplot(diabetes, aes_string(input$diabetes_drop, fill = input$due_filters))+
            geom_histogram(color = "black")+
            theme_bw()+
            labs(title = paste0(input$diabetes_drop))
        }
        
        
      }
      
       
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
