library(shiny)
library(plotly)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("2015 Connecticut Public High School Performance"),
  sidebarLayout(
    sidebarPanel(
      selectInput("outcome", label = h4("Class of 2015 Result"),
                  choices = list("Graduation Rate" = "GradRate",
                                 "College Entry Rate" = "CollegeRate"), selected = 1),
      selectInput("indepvar", label = h4("4-Year Average Data"),
                  choices = list("Absentee Rate" = "PctAbsent",
                                 "Spend Per Pupil ($)" = "TotalSpend",
                                 "School Enrollment" = "Enrolled"), selected = 1)
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Instructions", includeHTML("instructions.html")),
          
                  tabPanel("Scatterplot", br(), plotlyOutput("scatterplot")), # Plot
                  tabPanel("Distribution", br(), # Plots of distributions
                           fluidRow(
                             column(6, plotlyOutput("distribution1")),
                             column(6, plotlyOutput("distribution2")))
                  ),
                  tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                  tabPanel("Data", div(dataTableOutput('tbl'), style = "font-size:80%")) # Data as datatable

      )
    )
  ))
  )
