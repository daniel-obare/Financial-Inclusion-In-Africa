require(shiny)
require(dplyr)
require(tidyr)
require(janitor)
require(stringr)
require(magrittr)
require(ggplot2)
require(ggthemes)
#
#
ui <- fluidPage(
  titlePanel(h1('Financial Inclusion in Africa.', align = 'center')),
  #############################################################################################################
  sidebarLayout(
    sidebarPanel('Input Panel',
      selectInput(inputId = 'catcol', label = 'category', choices = c("country", "year", "location_type",
                                                                      "cellphone_access", "gender_of_respondent",
                                                                      "relationship_with_head", "education_level",
                                                                      "job_type", "marital_status"), 
                  selected = 'country'),
      #
      sliderInput(inputId = 'ageid', label = 'Age of Respondent', min = 0,max = 100, value = c(26,49)),
      #
      selectInput(inputId = 'countryid', label = 'Country of Origin', choices = c( 'Kenya', 'Rwanda', 
                                                                                   'Tanzania', 'Uganda'),
                  selected = 'Kenya'),
      #
      radioButtons(inputId = 'genderid', label = 'Gender', selected = 'Male', choices = c('Female', 'Male')),
      selectInput(inputId = 'maritalstatusid', label = 'Marital Status', selected = 'Widowed', 
                  choices = c('Widowed', 'Divorced/Seperated', 'Dont know Married/Living together',
                              'Single/Never Married'))
      ),
    #############################################################################################################
    mainPanel(h2('Analysis of the Financial Inclusion In Africa'),
              plotOutput('catid'),
              plotOutput('histogram'),
              tableOutput('summarydt'))
  )
)
#
#
server <- function(input, output) {
  df <- data %>% select(-3) %>% mutate(country = factor(country), year = factor(year), 
                                       bank_account = factor(bank_account), location_type = factor(location_type),
                                       cellphone_access = factor(cellphone_access), 
                                       gender_of_respondent = factor(gender_of_respondent), 
                                       relationship_with_head = factor(relationship_with_head), 
                                       marital_status = factor(marital_status), 
                                       education_level = factor(education_level), job_type = factor(job_type)) %>% 
    clean_names()
  #############################################################################################################
  output$catid <- renderPlot({
    df %>% ggplot(aes(df[[input$catcol]], fill = bank_account))+
      geom_bar(position = "dodge")+
      xlab(input$catcol)+
      ggtitle(input$catcol)+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, face = 'bold', size = 12), 
            axis.text.y = element_text(angle = 0, face = 'bold', size = 12),
            axis.text = element_text(size=14),
            axis.title = element_text(size=16,face="bold"))+
      coord_flip()
  })
  filtered <- reactive({
    df %>% filter(age_of_respondent >= input$ageid[1],
                  age_of_respondent <= input$ageid[2],
                  country == input$countryid,
                  gender_of_respondent == input$genderid,
                  marital_status == input$maritalstatusid
    )
  })
  ##########################################################################################################
  output$histogram <- renderPlot({
    filtered() %>% ggplot(aes(household_size))+
      geom_histogram(fill = 'green', col = 'black', breaks = seq(1,21, 3))+
      ggtitle('Household Size for diverse Age Distribution of Respondents')+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, face = 'bold', size = 12), 
            axis.text.y = element_text(angle = 0, face = 'bold', size = 12),
            axis.text = element_text(size=14),
            axis.title = element_text(size=16,face="bold"))  
  })  
  #########################################################################################################
  output$summarydt <- renderTable({
    filtered() %>% select(1,3,7) %>% group_by(country, bank_account) %>% 
      summarise(Totals = sum(age_of_respondent),
                Average = mean(age_of_respondent),
                Minimum_Age = min(age_of_respondent),
                Maximum_Age = max(age_of_respondent),
                Median_Age = median(age_of_respondent)
                )
  })
}
#
#
shinyApp(ui = ui, server = server)