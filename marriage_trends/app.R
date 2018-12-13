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
men_women <- read_rds("men_women.rds")

# Defining the UI. Naming the navbarPage with a blank space because each page
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
               h4("The Short Version:"),
               p("1. Less educated Americans are less likely to be married, and when they do get married, they're more likely to get divorced than their better-educated counterparts."),
               p("2. Working women are less likely to be married than their unemployed counterparts, while  the reverse is true for men."),
               p("3. Individuals with kids are nearly 4 times more likely to have been married than those without kids."),
               br(),
               h4("Marriage Trends:"),
               p("Less educated young people (aged 25-34) are less likely to be married. In 1960, 89% of those with a high school education or less reported having been married. In 2012, that same demographic reported 48%."),
               p("Regarding income among young people, those that are low income saw the sharpest decline in marriage since 1960 (when compared to their richer counterparts). In 1960, 86% of those that were low income had ever been married. In 2012, 35% of those people had ever been married. Middle income and high income people saw declines as well, although less drastically than low-income people."),
               p("When considering race (among young people), African-Americans see the greatest decline in marriage: 32% have been or were married in 2012 compared to 84% in 1960."),
               p("The age graph shows that young people are delaying marriage. Those aged 25-34 are much less likely to be married than those aged 35 or above (unsuprisingly). In 2012, only 51% of young people have ever been married, whereas in 1960, the number was at 88%."),
               p("The kids vs. no kids graphs, for both education and income, showed that those with no kids (as in the respondents have none of their own children living at home) are much less likely to be married than those who have kids (at least one own child living at home)."),
               h4("Divorce Trends:"),
               p("The education graphs show that those that are more educated are less likely to get divorced. For respondents in both the 35-44 and the 45-54 age categories, those with either some college education level or high school or less saw much higher increases in divorce rates than their college-educated counterparts."),
               p("The income graphs show that those of a low-income level are more likely to be divorced, no matter what age. 33% of low-income 25-34 year olds reported being divorced in 2012, versus 7% in 1960. This 26% increase is much greater than the 4% increase among high-income people: 6% in 2012 versus 2% in 1960. We see the same patterns among 35-44 year olds. 41% of low-income 35-44 year olds, reported being divorced in 2012, as opposed to 8% in 1960. Only 7% of high-income individuals reported being divorced in 2012, versus 2% in 1960."),
               h4("Men vs. Women:"),
               p("The comparisons among men vs. women show interesting trends. In terms of employment, unemployed women are more likely to be married than their employed counterpats, whereas with men, employed men are more likely to be married than unemployed men."),
               p("When considering kids, in 2012, men with kids are the most likely to be married. Men with kids are about six times more likely to be married than those without kids (89% vs. 11%, respectively). For women, the difference is not as large (77% for those with kids vs. 34% for those without kids) but still substantial."),
               p("When considering employment with race, white working women (58%)  are much more likely to be married than their Hispanic (45%) or black counterparts (33%), given the most recent data. Men see the same racial trend, although not as extreme. 54% of white men reported having ever been married, compared to 50% of Hispanic men and 39% of black men.")
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

#Creating new tabPanel for page

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
),
tabPanel("Men vs. Women",
         
         fluidPage(
           
           # Application title
           
           titlePanel("Comparing Marriage Among Men vs. Women from 1960-2012"),
           
           # Sidebar
           
           sidebarLayout(
             
             sidebarPanel(
               
               #Creating drop-down menu so user can choose which variable to look at
               
               selectInput(inputId = "choice3", 
                           label = "Select a variable:",
                           choices = c("Employed", "Unemployed: By Race", "Employed vs. Unemployed", "Kids vs. No Kids", 
                                       "Employed Women: By Race", "Employed Men: By Race"),
                           selected = "Employed"),
               
               #Creating slider input so user can choose date range and look more
               #closely at trends
               
               sliderInput("slider3", label = "Date Range:", 
                           min = 1960, max = 2012, value = c(1960, 2012), sep = "")),
             
             # Displaying plot and notes under plot
             
             mainPanel(
               plotOutput("distPlot4"),
               h3("Notes:"),
               p("All graphs show data from those aged 25-34."),
               p("The graph shows the relevant share of the population that has ever been married (that is, the respondent is either currently married or was married in the past)."),
               p("In terms of employment, 'employed' means the survey respondent was employed 50+ weeks in the prior year. 'Unemployed' means the survey respondent was not employed at least 50 weeks in the prior year."),
               p("For the graphs that compare people with kids versus those with no kids, 'kids' means at least one own child living at home, whereas 'no kids' means no own children living at home.")
             )
           )
         )
)
)

# Define server logic 
server <- function(input, output) {
  
  
  #Showing plot based off of what the user chooses, each choice from the
  #drop-down menu corresponds to a graph that has been manually made in my rmd
  #file
  
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
     
     output$distPlot4 <- renderPlot({
     
     if (input$choice3 == "Employed") {
       employed <- men_women %>%
         filter(year >= input$slider3[1] & year <= input$slider3[2]) %>%
         ggplot(aes(x = year, y = 100-work_hs_2534)) +
         geom_line(size = 1.5, aes(color = "Men: High School or Less")) +
         geom_line(aes(y = 100-work_sc_2534, color = "Men: Some College"), size = 1.5) +
         geom_line(aes(y = 100-work_b_ap_2534, color = "Men: College Graduate or More"), size = 1.5) +
         geom_line(aes(y = 100-work_hs_2534_women, color = "Women: High School or Less"), size = 1.5) +
         geom_line(aes(y = 100-work_sc_2534_women, color = "Women: Some College"), size = 1.5) +
         geom_line(aes(y = 100-work_b_ap_2534, color = "Women: College Graduate or More"), size = 1.5) +
         scale_color_manual(values = c("Men: High School or Less" = "#DEE9FC",
                                       "Men: Some College" = "#6395F2",
                                       "Men: College Graduate or More" = "#1258DC",
                                       "Women: High School or Less" = "#FDDEF3",
                                       "Women: Some College" = "#F033B4",
                                       "Women: College Graduate or More" = "#510639"),
                            breaks = c("Men: High School or Less", 
                                       "Men: Some College", 
                                       "Men: College Graduate or More",
                                       "Women: High School or Less",
                                       "Women: Some College",
                                       "Women: College Graduate or More")) +
         ylab("Share of Population that Has Ever Been Married (%)") +
         xlab("Year") +
         theme_minimal() +
         labs(color = "Education",
              caption = "Data taken from FiveThirtyEight") +
         ggtitle("Employed Men and Women (by Education)") +
         theme(text=element_text(family="Times New Roman", size=16))
       print(employed)
     }
       
       else if (input$choice3 == "Unemployed: By Race") {
         unemployed <- men_women %>%
           filter(year >= input$slider3[1] & year <= input$slider3[2]) %>%
           ggplot(aes(x = year, y = 100-nowork_white_2534)) +
           geom_line(size = 1.5, aes(color = "White Men")) +
           geom_line(aes(y = 100-nowork_black_2534, color = "Black Men"), size = 1.5) +
           geom_line(aes(y = 100-nowork_hisp_2534, color = "Hispanic Men"), size = 1.5) +
           geom_line(aes(y = 100-nowork_white_2534_women, color = "White Women"), size = 1.5) +
           geom_line(aes(y = 100-nowork_black_2534_women, color = "Black Women"), size = 1.5) +
           geom_line(aes(y = 100-nowork_hisp_2534_women, color = "Hispanic Women"), size = 1.5) +
           scale_color_manual(values = c("White Men" = "#DEE9FC",
                                         "Black Men" = "#6395F2",
                                         "Hispanic Men" = "#1258DC",
                                         "White Women" = "#FDDEF3",
                                         "Black Women" = "#F033B4",
                                         "Hispanic Women" = "#510639"),
                              breaks = c("White Men",
                                         "Black Men",
                                         "Hispanic Men",
                                         "White Women",
                                         "Black Women",
                                         "Hispanic Women"))+
           ylab("Share of Population that Has Ever Been Married (%)") +
           xlab("Year") +
           theme_minimal() +
           labs(color = "Race",
                caption = "Data taken from FiveThirtyEight") +
           ggtitle("Unemployed Men and Women (by Race)") +
           theme(text=element_text(family="Times New Roman", size=16))
         print(unemployed)
       }
       
       else if (input$choice3 == "Employed Women: By Race") {
         workingwomen <- men_women %>%
           filter(year >= input$slider3[1] & year <= input$slider3[2]) %>%
           ggplot(aes(x = year, y = 100-work_white_2534_women)) +
           geom_line(size = 1.5, aes(color = "White Women")) +
           geom_line(aes(y = 100-work_black_2534_women, color = "Black Women"), size = 1.5) +
           geom_line(aes(y = 100-work_hisp_2534_women, color = "Hispanic Women"), size = 1.5) +
           scale_color_manual(values = c("White Women" = "#F7B5D2",
                                         "Black Women" = "#D9176B",
                                         "Hispanic Women" = "#3A0A1F"),
                              breaks = c("White Women",
                                         "Black Women",
                                         "Hispanic Women")) +
           ylab("Share of Population that Has Ever Been Married (%)") +
           xlab("Year") +
           theme_minimal() +
           labs(color = "Race",
                caption = "Data taken from FiveThirtyEight") +
           ggtitle("Working Women by Race") +
           theme(text=element_text(family="Times New Roman", size=16))
         print(workingwomen)
       }
       
       else if (input$choice3 == "Employed Men: By Race") {
         workingmen <- men_women %>%
           filter(year >= input$slider3[1] & year <= input$slider3[2]) %>%
           ggplot(aes(x = year, y = 100-work_white_2534)) +
           geom_line(size = 1.5, aes(color = "White Men")) +
           geom_line(aes(y = 100-work_black_2534, color = "Black Men"), size = 1.5) +
           geom_line(aes(y = 100-work_hisp_2534, color = "Hispanic Men"), size = 1.5) +
           scale_color_manual(values = c("White Men" = "#A1CDA0",
                                         "Black Men" = "#417940",
                                         "Hispanic Men" = "#082807"),
                              breaks = c("White Men",
                                         "Black Men",
                                         "Hispanic Men")) +
                                ylab("Share of Population that Has Ever Been Married (%)") +
                                xlab("Year") +
                                theme_minimal() +
                                labs(color = "Race",
                                     caption = "Data taken from FiveThirtyEight") +
                                ggtitle("Working Men by Race") +
                                theme(text=element_text(family="Times New Roman", size=16))
          print(workingmen)
       }
       
       else if (input$choice3 == "Kids vs. No Kids") {
         women_kids <- men_women %>%
           filter(year >= input$slider3[1] & year <= input$slider3[2]) %>%
           ggplot(aes(x = year, y = 100-kids_women)) +
           geom_line(size = 1.5, aes(color = "Women with Kids")) +
           geom_line(aes(y = 100-nokids_women, color = "Women without Kids"), size = 1.5) +
           geom_line(aes(y = 100-kids, color = "Men with Kids"), size = 1.5) +
           geom_line(aes(y = 100-nokids, color = "Men without Kids"), size = 1.5) +
           scale_color_manual(values = c("Women with Kids" = "#FBBCE7",
                                         "Women without Kids" = "#AE0D7A",
                                         "Men with Kids" = "#0A2299",
                                         "Men without Kids" = "#A4B2F9"),
                              breaks = c("Women with Kids",
                                         "Women without Kids",
                                         "Men with Kids",
                                         "Men without Kids")) +
                                ylab("Share of Population that Has Ever Been Married (%)") +
                                xlab("Year") +
                                theme_minimal() +
                                labs(color = "Kids vs. no Kids",
                                     caption = "Data taken from FiveThirtyEight") +
                                ggtitle("Women: Kids vs. No Kids") +
                                theme(text=element_text(family="Times New Roman", size=16))
                              print(women_kids)
       }
       
       else if (input$choice3 == "Employed vs. Unemployed") {
         jobs <- men_women %>%
           filter(year >= input$slider3[1] & year <= input$slider3[2]) %>%
           ggplot(aes(x = year, y = 100-work_white_2534)) +
           geom_line(size = 1.5, aes(color = "Employed Men")) +
           geom_line(aes(y = 100-nowork_white_2534, color = "Unemployed Men"), size = 1.5) +
           geom_line(aes(y = 100-work_white_2534_women, color = "Employed Women"), size = 1.5) +
           geom_line(aes(y = 100-nowork_white_2534_women, color = "Unemployed Women"), size = 1.5) +
           scale_color_manual(values = c("Employed Women" = "#FBBCE7",
                                         "Unemployed Women" = "#AE0D7A",
                                         "Employed Men" = "#0A2299",
                                         "Unemployed Men" = "#A4B2F9"),
                              breaks = c("Employed Women",
                                         "Unemployed Women",
                                         "Employed Men",
                                         "Unemployed Men")) +
           ylab("Share of Population that Has Ever Been Married (%)") +
           xlab("Year") +
           theme_minimal() +
           labs(color = "Employed vs. Unemployed",
                caption = "Data taken from FiveThirtyEight") +
           ggtitle("Employed vs. Unemployed Among Whites") +
           theme(text=element_text(family="Times New Roman", size=16))
         print(jobs)
       }
       
})     
 
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

