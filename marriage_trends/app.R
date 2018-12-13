#Loading libraries

library(shiny)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(dplyr)
library(readr)

#Loading in data

all_data <- read_rds("all_data.rds")
all_data2 <- read_rds("all_data2.rds")
md <- read_rds("md.rds")

# Defining the UI Naming the navbarPage with a blank space because each page
# already has a different title, and I didn't want to be repetitive

ui <- navbarPage(
  " ",
  
  #Creating three separate tabPanels for three separate pages. There is probably
  #a better way to make tabs, perhaps with fluidPage, but this is the way that
  #made sense to me.
  
  tabPanel("Analysis", 
           fluidPage(
             
             # Application title
             titlePanel("Why is Marriage Declining and Divorce on the Rise?"),
             
             
             # Display plot, using splitLayout to plot two graphs side by side
             # for more effect
             
             mainPanel(
               splitLayout(cellWidths = c("50%", "50%"), plotOutput("marriagePlot"), plotOutput("divorcePlot")),
               h3("Summary of Findings"),
               h4("Marriage Trends:"),
               p("Less educated young people (aged 25-34) are less likely to be married. In 1960, 89% of those with a high school education or less reported having been married. In 2012, that same demographic reported 48%."),
               p("Regarding income among young people, those that are low income saw the sharpest decline in marriage since 1960 (when compared to their richer counterparts). In 1960, 86% of those that were low income had ever been married. In 2012, 35% of those people had ever been married. Middle income and high income people saw declines as well, although less drastically than low-income people."),
               p("When considering race (among young people), African-Americans see the greatest decline in marriage: 32% have been or were married in 2012 compared to 84% in 1960."),
               p("The age graph shows that young people are delaying marriage. Those aged 25-34 are much less likely to be married than those aged 35 or above (unsuprisingly). In 2012, only 51% of young people have ever been married, whereas in 1960, the number was at 88%."),
               p("The kids vs. no kids graphs, for both education and income, showed that those with no kids (as in the respondents have none of their own children living at home) are much less likely to be married than those who have kids (at least one own child living at home)."),
               h4("Divorce Trends:"),
               p("The education graphs show that those that are more educated are less likely to get divorced. For respondents in both the 35-44 and the 45-54 age categories, those with either some college education level or high school or less saw much higher increases in divorce rates than their college-educated counterparts."),
               p("The income graphs show that those of a low-income level are more likely to be divorced, no matter what age. 33% of low-income 25-34 year olds reported being divorced in 2012, versus 7% in 1960. This 26% increase is much greater than the 4% increase among high-income people: 6% in 2012 versus 2% in 1960. We see the same patterns among 35-44 year olds. 41% of low-income 35-44 year olds, reported being divorced in 2012, as opposed to 8% in 1960. Only 7% of high-income individuals reported being divorced in 2012, versus 2% in 1960.")
             )
           )
  ),

  tabPanel("Marriage Trends",
    
  fluidPage(
   
   # Application title
    
   titlePanel("United States Marriage Trends from 1960-2012"),
   
   # Sidebar
   
   sidebarLayout(
     
     sidebarPanel(
       
       #Creating drop-down menu so user can choose which variable to look at
       
       selectInput(inputId = "choice", 
                   label = "Select a variable:",
                   choices = c("Education", "Region", "Income", "Race", "Age", 
                               "Kids vs. No Kids: Education", "Kids vs. no Kids: Income"),
                   selected = "Education"),
      
       #Creating slider input so user can choose date range and look more
       #closely at trends
       
      sliderInput("slider1", label = "Date Range:", 
                   min = 1960, max = 2012, value = c(1960, 2012), sep = "")),
   
      # Displaying plot and notes under plot
     
      mainPanel(
         plotOutput("distPlot"),
         h3("Notes:"),
         p("The graph shows the relevant share of the population that has ever been married (that is, the respondent is either currently married or was married in the past)."),
         p("Regarding the income graph, 'Low Income' represents respondents with family income in the lowest 25%, 'Middle Income' represents respondents with family income in the middle 50%, and 'High Income' represents those with family income in the top 25%."),
         p("For the graphs that compare people with kids versus those with no kids, 'kids' means at least one own child living at home, whereas 'no kids' means no own children living at home.")
      )
   )
)
),

#Creating new tabPanel for second page

tabPanel("Divorce Trends", 
         
  fluidPage(
    
    # Application title
    
    titlePanel("United States Divorce Trends from 1960-2012"),
    
    # Sidebar
    
    sidebarLayout(
      
      sidebarPanel(
        
        #Creating drop-down menu so user can choose which variable to look at
        
        selectInput(inputId = "choice2", 
                    label = "Select a variable:",
                    choices = c("Education: Ages 35-44", "Education: Ages 45-54", 
                                "Income: Ages 35-44", "Income: Ages 45-54", "Age Comparison"),
                    selected = "Education: Ages 35-44"),
        sliderInput("slider2", label = "Date Range:", 
                    min = 1960, max = 2012, value = c(1960, 2012), sep = "")),
      
      # Display plot and notes under the plot
      
      mainPanel(
        plotOutput("distPlot2"),
        h3("Notes:"),
        p("Divorce means the respondent is currently divorced (conditional on having ever been married).")
      )
    )
  )
))

# Define server logic 
server <- function(input, output) {
  
  
  #Showing plot based off of what the user chooses, each choice from the
  #drop-down menu corresponds to a graph that has been manually made
  
   output$distPlot <- renderPlot({

     if (input$choice == "Education") {
       education_plot <- all_data %>%
         
         #Here, the filter command takes the user's input from the sliderInput
         #and then sorts the data accordingly - doing the same process for all
         #the other variables
         
       filter(year >= input$slider1[1] & year <= input$slider1[2]) %>%
         ggplot(aes(x = year, y = 100-highschool)) +          
         geom_line(size = 1.5, aes(color = "High School or Less")) +
         geom_line(aes(y = 100-some_college, color = "Some College"), size = 1.5) +   
         geom_line(aes(y = 100-college_grad, color = "College Graduate or More"), size = 1.5) +   
         ylab("Share of Population that Has Ever Been Married (%)") +
         xlab("Year") +
         scale_color_manual(values = c("High School or Less" = "#a167f7", 
                                       "Some College" = "#4741f4", 
                                       "College Graduate or More" = "#429ef4"),
                            breaks = c("High School or Less", "Some College", "College Graduate or More")) +
         theme_minimal() +
         labs(color = "Education",
              caption = "Data taken from FiveThirtyEight") +
         ggtitle("Sorted by: Education",
                 subtitle = "Ages 25-34") +
         theme(text=element_text(family="Times New Roman", size=16))
       print(education_plot)
     }
     
     else if (input$choice == "Region") {
       region_plot <- all_data %>%
         filter(year >= input$slider1[1] & year <= input$slider1[2]) %>%
       ggplot(aes(x = year, y = 100-newengland)) +          
         geom_line(size = 1.5, aes(color = "New England")) +
         geom_line(aes(y = 100-midwest, color = "Midwest"), size = 1.5) +   
         geom_line(aes(y = 100-south, color = "South"), size = 1.5) + 
         geom_line(aes(y = 100-pacific, color = "Pacific"), size = 1.5) + 
         ylab("Share of Population that Has Ever Been Married (%)") +
         xlab("Year") +
         scale_color_manual(values = c("New England" = "#411541", 
                                       "Midwest" = "#780775", 
                                       "South" = "#DE21D8",
                                       "Pacific" = "#E96DE5"),
                            breaks = c("New England", "Midwest", "South", "Pacific")) +
         theme_minimal() +
         labs(color = "Region",
              caption = "Data taken from FiveThirtyEight") +
         ggtitle("Sorted by: Region",
                 subtitle = "Ages 25-34") + 
         theme(text=element_text(family="Times New Roman", size=16))
       print(region_plot)
     }
     
     else if (input$choice == "Income") {
       income_plot <- all_data %>%
         filter(year >= input$slider1[1] & year <= input$slider1[2]) %>%
         ggplot(aes(x = year, y = 100-poor)) +
         geom_line(size = 1.5, aes(color = "Low Income")) +
         geom_line(aes(y = 100-middle, color = "Middle Income"), size = 1.5) +
         geom_line(aes(y = 100-rich, color = "High Income"), size = 1.5) +
         ylab("Share of Population that Has Ever Been Married (%)") +
         xlab("Year") +
         scale_color_manual(values = c("Low Income" = "#3D533C",
                                       "Middle Income" = "#57A054",
                                       "High Income" = "#96C195"),
                            breaks = c("Low Income", "Middle Income", "High Income")) +
         theme_minimal() +
         labs(color = "Income",
              caption = "Data taken from FiveThirtyEight") +
         ggtitle("Sorted by: Income",
                 subtitle = "Ages 25-34") + 
         theme(text=element_text(family="Times New Roman", size=16))
       print(income_plot)
     }
     
     else if (input$choice == "Race") {
       race_plot <- all_data %>%
         filter(year >= input$slider1[1] & year <= input$slider1[2]) %>%
         ggplot(aes(x = year, y = 100-white)) +          #Whites
         geom_line(size = 1.5, aes(color = "White")) +
         geom_line(aes(y = 100-black, color = "Black"), size = 1.5) +    #Blacks
         geom_line(aes(y = 100-hispanic, color = "Hispanic"), size = 1.5) +    #Hispanics
         ylab("Share of Population that Has Ever Been Married (%)") +
         xlab("Year") +
         scale_color_manual(values = c("White" = "#eeac99", 
                                       "Black" = "#c83349", 
                                       "Hispanic" = "#e06377"),
                            breaks = c("White", "Black", "Hispanic")) +
         theme_minimal() +
         labs(color = "Race",
              caption = "Data taken from FiveThirtyEight") +
         ggtitle("Sorted by: Race",
                 subtitle = "Ages 25-34") +
         theme(text=element_text(family="Times New Roman", size=16))
       print(race_plot)
     }
     
     else if (input$choice == "Age") {
       age_plot <- all_data %>%
         filter(year >= input$slider1[1] & year <= input$slider1[2]) %>%
         ggplot(aes(x = year, y = 100-young)) +
         geom_line(size = 1.5, aes(color = "Ages 25-34")) +
         geom_line(aes(y = 100-middle1, color = "Ages 35-44"), size = 1.5) +
         geom_line(aes(y = 100-old, color = "Ages 45-54"), size = 1.5) +
         ylab("Share of Population that Has Ever Been Married (%)") +
         xlab("Year") +
         scale_color_manual(values = c("Ages 25-34" = "#682736",
                                       "Ages 35-44" = "#DC2E56",
                                       "Ages 45-54" = "#DB9FAD"),
                            breaks = c("Ages 25-34", "Ages 35-44", "Ages 45-54")) +
         theme_minimal() +
         labs(color = "Age",
              caption = "Data taken from FiveThirtyEight") +
         ggtitle("Sorted by: Age") +
         theme(text=element_text(family="Times New Roman", size=16))
       print(age_plot)
     
     }
     
     else if (input$choice == "Kids vs. No Kids: Education") {
       kids_education <- all_data %>%
         filter(year >= input$slider1[1] & year <= input$slider1[2]) %>%
         ggplot(aes(x = year, y = 100-kids_hs)) + 
         geom_line(size = 1.5, aes(color = "Kids: High School or Less")) +
         geom_line(aes(y = 100-kids_college, color = "Kids: College Graduate or More"), size = 1.5) +
         geom_line(aes(y = 100-nokids_hs, color = "No Kids: High School or Less"), size = 1.5) +
         geom_line(aes(y = 100-nokids_college, color = "No Kids: College Graduate or More"), size = 1.5) +
         ylab("Share of Population that Has Ever Been Married (%)") +
         xlab("Year") +
         scale_color_manual(values = c("Kids: High School or Less" = "#08030C",
                                       "Kids: College Graduate or More" = "#532F6F",
                                       "No Kids: High School or Less" = "#762CAF",
                                       "No Kids: College Graduate or More" = "#C9A1E8"),
                            breaks = c("Kids: High School or Less", "Kids: College Graduate or More",
                                       "No Kids: High School or Less", "No Kids: College Graduate or More")) +
         theme_minimal() + 
         labs(color = "Kids & Level of Education",
              caption = "Data taken from FiveThirtyEight") +
         ggtitle("Sorted by: Kids & Level of Education",
                  subtitle = "Ages 25-34") +
         theme(text=element_text(family="Times New Roman", size=16))
       print(kids_education)
     }
     
     else if (input$choice == "Kids vs. no Kids: Income") {
       kids_income <- all_data %>%
         filter(year >= input$slider1[1] & year <= input$slider1[2]) %>%
         ggplot(aes(x = year, y = 100-kids_poor)) +
         geom_line(size = 1.5, aes(color = "Kids: Low Income")) +
         geom_line(aes(y = 100-kids_rich, color = "Kids: High Income"), size = 1.5) +
         geom_line(aes(y = 100-nokids_poor, color = "No Kids: Low Income"), size = 1.5) +
         geom_line(aes(y = 100-nokids_rich, color = "No Kids: High Income"), size = 1.5) +
         ylab("Share of Population that Has Ever Been Married (%)") +
         xlab("Year") +
         scale_color_manual(values = c("Kids: Low Income" = "#002352",
                                       "Kids: High Income" = "#24548F",
                                       "No Kids: Low Income" = "#4A8DDE",
                                       "No Kids: High Income" = "#C4D7ED"),
                            breaks = c("Kids: Low Income", "Kids: High Income", 
                                       "No Kids: Low Income", "No Kids: High Income")) +
         theme_minimal() +
         labs(color = "Kids & Income Level",
              caption = "Data taken from FiveThirtyEight") +
         ggtitle("Sorted by: Kids & Income Level",
                 subtitle = "Ages 25-34") +
         theme(text=element_text(family="Times New Roman", size=16))
       print(kids_income)
     }
   })
   
   output$distPlot2 <- renderPlot({
     
     if (input$choice2 == "Education: Ages 35-44") {
       education_divorce <- all_data2 %>%
         filter(year >= input$slider2[1] & year <= input$slider2[2]) %>%
         ggplot(aes(x = year, y = highschool)) +
         geom_line(size = 1.5, aes(color = "High School or Less")) +
         geom_line(aes(y = somecollege, color = "Some College"), size = 1.5) +
         geom_line(aes(y = collegegrad, color = "College Graduate or More"), size = 1.5) +
         ylab("Share of Population that Has Been Divorced (%)") +
         xlab("Year") +
         scale_color_manual(values = c("High School or Less" = "#a167f7", 
                                       "Some College" = "#4741f4", 
                                       "College Graduate or More" = "#429ef4"),
                            breaks = c("High School or Less", "Some College", "College Graduate or More")) +
         theme_minimal() +
         labs(color = "Education",
              caption = "Data taken from FiveThirtyEight") +
         ggtitle("Sorted by: Education (Ages 35-44)") +
         theme(text=element_text(family="Times New Roman", size=16))
      print(education_divorce)
     }
     
     else if (input$choice2 == "Education: Ages 45-54") {
       education_divorce2 <- all_data2 %>%
         filter(year >= input$slider2[1] & year <= input$slider2[2]) %>%
         ggplot(aes(x = year, y = highschool2)) +
         geom_line(size = 1.5, aes(color = "High School or Less")) +
         geom_line(aes(y = somecollege2, color = "Some College"), size = 1.5) +
         geom_line(aes(y = collegegrad2, color = "College Graduate or More"), size = 1.5) +
         ylab("Share of Population that Has Been Divorced (%)") +
         xlab("Year") +
         scale_color_manual(values = c("High School or Less" = "#411541", 
                                       "Some College" = "#780775", 
                                       "College Graduate or More" = "#DE21D8"),
                            breaks = c("High School or Less", "Some College", "College Graduate or More")) +
         theme_minimal() +
         labs(color = "Education",
              caption = "Data taken from FiveThirtyEight") +
         ggtitle("Sorted by: Education (Ages 45-54)") +
         theme(text=element_text(family="Times New Roman", size=16))
       print(education_divorce2)
     }
     
     else if (input$choice2 == "Income: Ages 35-44") {
       income_divorce <- all_data2 %>%
         filter(year >= input$slider2[1] & year <= input$slider2[2]) %>%
         ggplot(aes(x = year, y = poor)) +
         geom_line(size = 1.5, aes(color = "Low Income")) +
         geom_line(aes(y = middle, color = "Middle Income"), size = 1.5) +
         geom_line(aes(y = rich, color = "High Income"), size = 1.5) +
         ylab("Share of Population that Has Been Divorced (%)") +
         xlab("Year") +
         scale_color_manual(values = c("Low Income" = "#3D533C",
                                       "Middle Income" = "#57A054",
                                       "High Income" = "#96C195"),
                            breaks = c("Low Income", "Middle Income", "High Income")) +
         theme_minimal() +
         labs(color = "Income",
              caption = "Data taken from FiveThirtyEight") +
         ggtitle("Sorted by: Income (Ages 35-44)") +
         theme(text=element_text(family="Times New Roman", size=16))
       print(income_divorce)
     }
     
     else if (input$choice2 == "Income: Ages 45-54") {
       income_divorce2 <- all_data2 %>%
         filter(year >= input$slider2[1] & year <= input$slider2[2]) %>%
         ggplot(aes(x = year, y = poor2)) +
         geom_line(size = 1.5, aes(color = "Low Income")) +
         geom_line(aes(y = middle2, color = "Middle Income"), size = 1.5) +
         geom_line(aes(y = rich2, color = "High Income"), size = 1.5) +
         ylab("Share of Population that Has Been Divorced (%)") +
         xlab("Year") +
         scale_color_manual(values = c("Low Income" = "#C78500",
                                       "Middle Income" = "#F2BC50",
                                       "High Income" = "#FDDEA0"),
                            breaks = c("Low Income", "Middle Income", "High Income")) +
         theme_minimal() +
         labs(color = "Income",
              caption = "Data taken from FiveThirtyEight") +
         ggtitle("Sorted by: Income (Ages 45-54)") +
         theme(text=element_text(family="Times New Roman", size=16))
       print(income_divorce2)
     }
     
     else if (input$choice2 == "Age Comparison") {
       age_diff <- all_data2 %>%
         filter(year >= input$slider2[1] & year <= input$slider2[2]) %>%
         ggplot(aes(x = year, y = young)) +
         geom_line(size = 1.5, aes(color = "Ages 35-44")) +
         geom_line(aes(y = old, color = "Ages 45-54"), size = 1.5)  +
         ylab("Share of Population that Has Been Divorced (%)") +
         xlab("Year") +
         scale_color_manual(values = c("Ages 35-44" = "#E9496F",
                                       "Ages 45-54" = "#F7BFCC"),
                            breaks = c("Ages 35-44", "Ages 45-54")) +
         theme_minimal() +
         labs(color = "Age Range",
              caption = "Data taken from FiveThirtyEight") +
         ggtitle("Sorted by: Age (35-44 vs. 45-54)") +
         theme(text=element_text(family="Times New Roman", size=16))
       print(age_diff)
     }
 
     #Creating plots for marriage and divorce trends
     
     output$marriagePlot <- renderPlot({
      marriage <- md %>% 
       ggplot(aes(x = year, y = marriage)) +
         geom_line(aes(color = "Marriage"), size = 1.5) +
         xlab("Year") +
         ylab("Share of Relevant Population (%)") +
         scale_color_manual(values = c("Marriage" = "#A61152")) +
         theme_minimal() +
         theme(legend.position = "none") +
         labs(caption = "Data referring to those aged 35-44") +
         ggtitle("MARRIAGE") +
        theme(text=element_text(family="Times New Roman", size=16))
      print(marriage)
     })
     
     output$divorcePlot <- renderPlot({
       divorce <- md %>% 
         ggplot(aes(x = year, y = divorce)) +
         geom_line(aes(color = "Divorce"), size = 1.5) +
         xlab("Year") +
         ylab("Share of Relevant Population (%)") +
         scale_color_manual(values = c("Divorce" = "#F390BB")) +
         theme_minimal() +
         theme(legend.position = "none") +
         labs(caption = "Data referring to those aged 35-44") +
         ggtitle("DIVORCE") +
         theme(text=element_text(family="Times New Roman", size=16))
       print(divorce)
     })
     
})
}

# Run the application 

shinyApp(ui = ui, server = server) 

