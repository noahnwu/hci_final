#test page here. 

# from Noah: will make simulated data this week. 

library(shiny)
library(tidyverse)
library(lubridate)
library(shinythemes)
library(DT)
diabetes = read_csv("diabetes_dataset.csv")
diabetes_time = read_csv("a1c_time_series.csv") %>%  
  mutate(hba1c_normal = paste0("Last HbA1c ", hba1c_normal))
readmit = read_csv("readmission.csv") %>%  
  rename(number_of_readmissions = readmit_number)


readmit_variables = as.list(names(readmit)[!str_detect(names(readmit), "id")])
names(readmit_variables) = str_to_title(str_replace_all(readmit_variables, 
                                                        "_", 
                                                        " "))
diabetes_variables = as.list(
  c(names(diabetes)[!str_detect(names(diabetes), "id$")], 
    "a1c_over_time"))
#displays as capitalized and wihtout underscore
names(diabetes_variables) = str_to_title(str_replace_all(diabetes_variables, "_", " "))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"), 

    # Application title
    titlePanel("Report Visualization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          #only diabetes works right now
          selectInput("dz", 
                      "What disease process?", 
                      choices = list("Diabetes", 
                                     "Hospital Readmission")), 
          conditionalPanel(
            condition = "input.dz == 'Diabetes'",
            selectInput("diabetes_drop", 
                        "Which diabetes-related condition/lab?", 
                        choices = diabetes_variables
            ), 
            conditionalPanel(
              condition = "input.dz == 'Diabetes' && input.diabetes_drop != 'a1c_over_time'", 
              radioButtons('due_filters', 
                           "Label Plot by Vital Due Dates", 
                           choiceValues = as.list(names(diabetes)[str_detect(names(diabetes), "due")]), 
                           choiceNames = as.list(
                             str_to_title(str_replace_all(names(diabetes)[str_detect(names(diabetes), "due")], 
                                             "_", 
                                             " "))
                             )
                           )
            )
          ), 
          conditionalPanel(
            condition = "input.dz == 'Diabetes' && input.diabetes_drop == 'a1c_over_time'",
            radioButtons('time_filter', 
                         "Pick a Time Period to Filter By", 
                         choiceValues = list(as.character(min(diabetes_time$test_date)), 
                                        as.character(Sys.Date() - months(6)), 
                                        as.character(Sys.Date() - years(1))), 
                         choiceNames = list("All Available Data", 
                                            "Last 6 Months of Data", 
                                            "Last Year of Data")
              
            ), 
            radioButtons('patient_filter', 
                         "What Patient Population Do You Want to Look At?", 
                         choiceNames = list("All Patients", 
                                            "Last HbA1c in Top 25%", 
                                            "Last HbA1c in Top 50%"), 
                         choiceValues = list(1, 4, 2))
          ), 
          conditionalPanel(
            condition = "input.dz != 'Diabetes'",
            selectInput("readmit_drop", 
                        label = "Which readmit variable do you want to visualize?", 
                        choices = readmit_variables), 
            radioButtons('readmit_filter', 
                         "What time period?", 
                         choiceNames = list("All Available Data", 
                                            "Last 6 Months of Data", 
                                            "Last 3 Months of Data"), 
                         choiceValues = list(as.character(min(readmit$last_readmit)), 
                                             as.character(Sys.Date() - months(6)), 
                                             as.character(Sys.Date() - months(3)))
                         )
            
          )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("condition_plot"), 
           tags$br(), 
           tags$br(),
           dataTableOutput("condition_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$condition_plot <- renderPlot({
      if(input$dz == "Diabetes") { 
       
        if(input$diabetes_drop == "a1c_over_time") {
          recent_data <- diabetes %>%  
            mutate(quartile = ntile(hba1c_value, 4)) %>% 
            filter(quartile >= as.numeric(input$patient_filter))
          
          temp = diabetes_time %>%  
            filter(test_date >= ymd(input$time_filter)) %>%  
            filter(patient_id %in% recent_data$patient_id)
          
        ggplot(temp)+
            geom_smooth(aes(test_date, a1c_value, group = hba1c_normal), se = F, 
                        method = "loess")+
            geom_point(aes(test_date, a1c_value, color = hba1c_normal), alpha = 0.1)+
            facet_wrap(~hba1c_normal)+
            theme_bw()+
            theme(legend.position = "none")
        }
        
        else if(class(diabetes %>% pull(input$diabetes_drop)) == "character") { 
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
                 title = paste0(
                   "Last ", 
                   str_to_title(str_replace_all(input$diabetes_drop, "_", " "))
                 ), 
                 fill = str_to_title(str_replace_all(input$due_filters, "_", " ")))
        } else{ 
          ggplot(diabetes, aes_string(input$diabetes_drop, fill = input$due_filters))+
            geom_histogram(color = "black")+
            theme_bw()+
            labs(title = paste0(
              "Last ", 
              str_to_title(str_replace_all(input$diabetes_drop, "_", " "))
              ), 
              fill = str_to_title(str_replace_all(input$due_filters, "_", " "))
              
              )
        }
        
        
      } else{ 
        temp = readmit %>%  
          filter(last_readmit >= ymd(input$readmit_filter))
        
        ggplot(temp)+
          geom_histogram(aes_string(input$readmit_drop), 
                         fill = "light blue", 
                         color = "black")+
          theme_bw()+
          labs(x = str_to_title(str_replace_all(input$readmit_drop, "_", " ")), 
               y = "N", 
               title = paste0(
                 "Distribution of ", 
                 str_to_title(str_replace_all(input$readmit_drop, "_", " ")), 
                 " since ", 
                 input$readmit_filter
               )
               )
      }
      
    })
    
    output$condition_table = renderDataTable({
      
      if(input$dz == "Diabetes" & input$diabetes_drop != "a1c_over_time") { 
        diabetes %>% 
          arrange(patient_id) %>%  
          rename_with(.fn = ~str_to_title(str_replace_all(.x, "_", " ")))
      } else if(input$dz == "Diabetes" & input$diabetes_drop == "a1c_over_time") {
        recent_data <- diabetes %>%  
          mutate(quartile = ntile(hba1c_value, 4)) %>% 
          filter(quartile >= as.numeric(input$patient_filter))
        
        diabetes_time %>%  
          arrange(patient_id, test_date) %>% 
          filter(patient_id %in% recent_data$patient_id) %>% 
          filter(test_date >= input$time_filter)
        
        
        }
      
      
      
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
