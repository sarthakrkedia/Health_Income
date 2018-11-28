library(shiny)
library(ggplot2)
library(shinythemes)

mortality_data <- read.csv("mortality.csv")
life_expectancy_data <- read.csv("life_expectancy.csv")
death_causes_data <- read.csv("causes_of_death.csv")

Region_list <- append(as.character(unique(mortality_data$Region)),"All Region",0)
Income_list <- append(as.character(unique(mortality_data$Income)),"All Income Groups",0)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$body(style="background-color:white"),
  theme = shinytheme("simplex"),
  # Sidebar
  navbarPage("Navigation Bar",
  tabPanel("Region",
  sidebarLayout(
    
    sidebarPanel(
      tags$style(".well{background-color:white;border-style:none;width:100%;}"),
      tags$style(".col-sm-4{width:20%;}"),
      radioButtons("Region","Select a region",Region_list),
      h6(strong("CIS"), "refers to CommonWealth of Independent States, which came into existence after Soviet Union,
        It includes all ten former Soviet Republics like Armenia, Kazakhstan, etc.",style="text-align:justify"),
      h6(strong("Malignant Neoplasams"), "refers to diseases which are cancerous in nature.",style="text-align:justify"),
      h6(strong("Neonatal Conditions"), "refers to Infant related diseases",style="text-align:justify"),
      h6(strong("Mortality Rate"), "or death rate, is a measure of the number of deaths (in general, or due to a specific cause) 
         in a particular population, scaled to the size of that population, per unit of time. 
         Mortality rate is typically expressed in units of deaths per 1,000 individuals per year; thus, 
         a mortality rate of 9.5 (out of 1,000) in a population of 1,000 would mean 9.5 deaths per year in that entire population",style="text-align:justify"),
      h6(strong("Life Expectancy"),  "is a statistical measure of the average time an organism is expected to live, based on the year of their birth, 
         their current age and other demographic factors", style="text-align:justify")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tags$style(".col-sm-8{width:80%;}"),
      fluidRow(
        column(12,plotOutput("plot_cau")),
        column(12, plotOutput("plot_mort")),
        column(12, plotOutput("plot_life"))
      ))
    )
  
    ), 
  tabPanel("Income",
           
           sidebarLayout(
             
             sidebarPanel(
               radioButtons("Income","Select a Income Groups",Income_list),
               h6(strong("CIS"), "refers to CommonWealth of Independent States, which came into existence after Soviet Union,
                It includes all ten former Soviet Republics like Armenia, Kazakhstan, etc.",style="text-align:justify"),
               h6(strong("High Income"),"economies have a gross national income per capita of greater than $12,475",style="text-align:justify"),
               h6(strong("Upper-Middle Income"),"economies have a gross national income per capita of greater than $4035",style="text-align:justify"),
               h6(strong("Lower-Middle Income"),"economies have a gross national income per capita of greater than $1025",style="text-align:justify"),
               h6(strong("Low Income"),"economies have a gross national income per capita of lesser than $1025",style="text-align:justify"),
               h6(strong("Malignant Neoplasams"), "refers to diseases which are cancerous in nature.",style="text-align:justify"),
               h6(strong("Neonatal Conditions"), "refers to Infant related diseases",style="text-align:justify"),
               h6(strong("Mortality Rate"), "or death rate, is a measure of the number of deaths (in general, or due to a specific cause) 
                  in a particular population, scaled to the size of that population, per unit of time. 
                  Mortality rate is typically expressed in units of deaths per 1,000 individuals per year; thus, 
                  a mortality rate of 9.5 (out of 1,000) in a population of 1,000 would mean 9.5 deaths per year in that entire population",style="text-align:justify"),
               h6(strong("Life Expectancy"),  "is a statistical measure of the average time an organism is expected to live, based on the year of their birth, 
                  their current age and other demographic factors", style="text-align:justify")
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               
               fluidRow(
                 column(12,plotOutput("plot_cau_inc")),
                 column(12, plotOutput("plot_mort_inc")),
                 column(12, plotOutput("plot_life_inc"))
               ))) 
  ),
  
  tabPanel("Country Comparision",
           
           sidebarLayout(
             
             sidebarPanel(
               selectInput("Country1","Select Country One",as.character(unique(mortality_data$Country)),selected = "Australia"),
               selectInput("Country2","Select Country Two",as.character(unique(mortality_data$Country)),selected = "Oman"),
               h6("This page lets you compare two countries for various statistics",style="text-align:justify"),
               h6(strong("CIS"), "refers to CommonWealth of Independent States, which came into existence after Soviet Union,
               It includes all ten former Soviet Republics like Armenia, Kazakhstan, etc.",style="text-align:justify",style="text-align:justify"),
               h6(strong("Malignant Neoplasams"), "refers to diseases which are cancerous in nature.",style="text-align:justify"),
               h6(strong("Neonatal Conditions"), "refers to Infant related diseases",style="text-align:justify"),
               h6(strong("Mortality Rate"), "or death rate, is a measure of the number of deaths (in general, or due to a specific cause) 
                  in a particular population, scaled to the size of that population, per unit of time. 
                  Mortality rate is typically expressed in units of deaths per 1,000 individuals per year; thus, 
                  a mortality rate of 9.5 (out of 1,000) in a population of 1,000 would mean 9.5 deaths per year in that entire population",style="text-align:justify"),
               h6(strong("Life Expectancy"),  "is a statistical measure of the average time an organism is expected to live, based on the year of their birth, 
                  their current age and other demographic factors", style="text-align:justify")
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               fluidRow(
                 column(5,plotOutput("plot_life_mor_cou1")),
                 column(7,plotOutput("plot_life_mor_cou2")),
                 column(6,plotOutput("plot_life_mor_cou3")),
                 column(6,plotOutput("plot_life_mor_cou4"))
               ))) 
           )
  )
  )
  )
