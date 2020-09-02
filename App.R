require(shiny)
require(shinydashboard)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggthemes)
require(plotly)
require(stringr)
require(janitor)
theme_set(theme_minimal())
###############################################################################################################
df <- data %>% select(-3) %>% mutate(country = factor(country), year = factor(year), 
                                     bank_account = factor(bank_account), location_type = factor(location_type),
                                     cellphone_access = factor(cellphone_access), 
                                     gender_of_respondent = factor(gender_of_respondent), 
                                     relationship_with_head = factor(relationship_with_head), 
                                     marital_status = factor(marital_status), 
                                     education_level = factor(education_level), job_type = factor(job_type)) %>% 
  clean_names()
###################################################################################################################
header <- dashboardHeader(title = 'Financial Inclusion')
###
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = 'Home', tabName = 'home'),
    menuItem(text = 'Dashboard', tabName = 'dashboard'),
    menuItem(text = 'Extra', tabName = 'extra')
  ),
  sliderInput(inputId = 'ageid', label = 'Age', min = 15, max = 100, value = c(25,50)),
  radioButtons(inputId = 'locid', label = 'Location', choices = unique(location_type), selected = 'Urban'),
  selectInput(inputId = 'bankid', label = 'Bank Account', choices = unique(bank_account), selected = 'Yes', selectize = TRUE),
  radioButtons(inputId = 'genderid', label = 'Gender', choices = unique(gender_of_respondent), selected = 'Male'),
  selectInput(inputId = 'jobid', label = 'Job Type', choices = unique(job_type), selected = 'Self employed', selectize = TRUE)
)
###
body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'home',
            fluidRow(box(title = 'Bank Account', width = 4, plotlyOutput('bankaccplot'), height = '450px'),
                     box(title = 'Country', width = 4, plotlyOutput('countryplot'), height = '450px'),
                     box(title = 'Job Type & Year', width = 4, tableOutput('jobtypetable'), height = '430px')),
            fluidRow(box(title = 'Cell Phone Acces & Location', width = 4, tableOutput('cellacctable'), height = '200px'),
                     box(title = 'Education Level & Gender', width = 4, tableOutput('eductable'), height = '250px'),
                     box(title = 'Summary of Age', width = 4, tableOutput('agesumtable'), height = '200px'))
            ),
    tabItem(tabName = 'dashboard',
            fluidRow(column(width = 8,
                            box(title = 'Relationship with Head', width = NULL, plotOutput('rwhpie', height = 300)),
                            box(title = 'Relationship with Head', width = NULL, plotlyOutput('bar1', width = '100%', height = 300))),
                     column(width = 4, 
                            box(title = 'Country', width = NULL, plotlyOutput('bar3', height = 400)),
                            box(title = 'Year', width = NULL, plotlyOutput('bar2', height = 200))))
            ),
    tabItem(tabName = 'extra',
            fluidRow(column(width = 6,
                            box(title = 'Marita Status', width = NULL, plotOutput('mspie', height = 400)),
                            box(title = 'Point of Observation', width = NULL, textOutput('text'))),
                     column(width = 6,
                            box(title = 'Education Level', width = NULL, plotlyOutput('bar4', height = 400)))))
  )
)
###################################################################################################################
ui <- dashboardPage(
  header,
  sidebar,
  body
  )
##################################################################################################################
server <- function(input, output) {
  output$bankaccplot <- renderPlotly({
    ba <- df %>% ggplot(aes(bank_account, fill = year))+
      geom_bar(stat = 'count', position = 'dodge')+
      labs(x = 'Bank Account', y = 'Frequency', title = 'Bank Account Analysis per Year')+
      theme(axis.text.x = element_text(angle = 45, face = 'bold', size = 12), 
            axis.text.y = element_text(angle = 0, face = 'bold', size = 12),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank())
    ggplotly(ba)
  }) 
  
  output$countryplot <- renderPlotly({
    df %>% ggplot(aes(country, fill = bank_account))+
      geom_bar(stat = 'count', position = 'dodge')+
      labs(x = 'Country of Origin', y = 'Frequency', title = 'Bank Account Analysis per Country')+
      theme(axis.text.x = element_text(angle = 45, face = 'bold', size = 12), 
            axis.text.y = element_text(angle = 0, face = 'bold', size = 12),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank()) -> c
    ggplotly(c)
  })
  
  output$jobtypetable <- renderTable({
    df %>% select(2,12) %>% group_by(year, job_type) %>% summarise(Total = n()) %>% as.data.frame() %>% 
      spread(key = year, value = Total) %>% tibble() %>% replace_na(list(`2016`= 0, `2017` = 0))
  })
  
  output$cellacctable <- renderTable({
    df %>% select(4,5) %>% group_by(cellphone_access, location_type) %>% summarise(Total = n()) %>% 
      as.data.frame() %>% spread(key = cellphone_access, value = Total) %>% tibble()
  })
  
  output$eductable <- renderTable({
    df %>% select(8,11) %>% group_by(gender_of_respondent, education_level) %>% summarise(Total = n()) %>% 
      as.data.frame() %>% spread(key = gender_of_respondent, value = Total) %>% tibble()
  })
  
  output$agesumtable <- renderTable({
    df %>% select(1,2,7) %>%  group_by(country ,year) %>% 
      summarise(Avg = mean(age_of_respondent), 
                Min = min(age_of_respondent), 
                Max = max(age_of_respondent), 
                Median = median(age_of_respondent)) 
  })
  #############################################################################################################
  #############################################################################################################
  filtered <- reactive({
    df %>% filter(
      household_size == round(mean(household_size)),
      age_of_respondent >= input$ageid[1], age_of_respondent <= input$ageid[2],
      location_type == input$locid,
      bank_account == input$bankid,
      gender_of_respondent == input$genderid,
      job_type == input$jobid
    )
  })
  
  output$rwhpie <- renderPlot({
    df %>% select(9) %>% group_by(relationship_with_head) %>% summarise(Freq = n()) %>% 
      ggplot(aes(x = '',y = Freq, fill = relationship_with_head))+
      geom_bar(stat = 'identity', color = 'white')+
      coord_polar(theta = 'y', start = 0)+
      geom_text(aes(y = Freq, label = Freq), color = "white", position = position_stack(vjust = 0.5))+
      theme_void()+
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, color = "#666666"))
  })
  
  output$mspie <- renderPlot({
    df %>% select(10) %>% group_by(marital_status) %>% summarise(Freq = n()) %>% 
      ggplot(aes(x = '', y = Freq, fill = marital_status))+
      geom_bar(stat = 'identity', color = 'white')+
      coord_polar(theta = 'y', start = 0)+
      geom_text(aes(y = Freq, label = Freq), color = "white", position = position_stack(vjust = 0.5))+
      theme_void()+
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, color = "#666666"))
  })
  
  output$bar1 <- renderPlotly({
    filtered() %>% ggplot(aes(country))+
      geom_bar(stat = 'count', fill = 'steelblue')+
      theme(axis.text.x = element_text(angle = 0, face = 'bold', size = 12), 
            axis.text.y = element_text(angle = 0, face = 'bold', size = 12),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank()) -> b1
    ggplotly(b1)
  })
  
  output$bar2 <- renderPlotly({
    filtered() %>% ggplot(aes(year))+
      geom_bar(stat = 'count', fill = 'steelblue')+
      coord_flip()+
      theme(axis.text.x = element_text(angle = 0, face = 'bold', size = 12), 
            axis.text.y = element_text(angle = 0, face = 'bold', size = 12),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank()) -> b2
    ggplotly(b2)
  })
  
  output$bar3 <- renderPlotly({
    filtered() %>% ggplot(aes(relationship_with_head))+
      geom_bar(stat = 'count', fill = 'steelblue')+
      theme(axis.text.x = element_text(angle = 45, face = 'bold', size = 12), 
            axis.text.y = element_text(angle = 0, face = 'bold', size = 12),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank()) -> b3
    ggplotly(b3)
  })
  
  output$bar4 <- renderPlotly({
    filtered() %>% ggplot(aes(education_level))+
      geom_bar(stat = 'count', fill = 'steelblue')+
      theme(axis.text.x = element_text(angle = 45, face = 'bold', size = 12), 
            axis.text.y = element_text(angle = 0, face = 'bold', size = 12),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank()) -> b4
    ggplotly(b4)
  })
  
  output$text <- renderText({
    'N/B:
      Plots are taken with the average house hold size.'
  })
}
##############################################################################################################
shinyApp(ui, server)