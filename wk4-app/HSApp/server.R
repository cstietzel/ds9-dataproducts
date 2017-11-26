library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(DT)

options(warn =-1)

## Get Absenteeism data
absent <- data_frame()
files <- list.files(path="../../data", pattern = "chronicAbsenteeism", full.names = TRUE)
for (f in files) {
  flen <- nchar(f)
  tempdf = read_csv(f, skip = 4, na = "*",
                    col_names  = c("DistrictName", "DistrictCode", "SchoolName", "SchoolCode", "PctAbsent"),
                    col_types = cols(
                      DistrictName = col_character(),
                      DistrictCode = col_integer(),
                      SchoolName = col_character(),
                      SchoolCode = col_integer(),
                      PctAbsent = col_double()
                    )) %>% 
    mutate(SchoolYear = substr(f, flen - 8,flen - 4)) %>%
    select(SchoolYear, SchoolName, everything())
  
  absent <-bind_rows(absent, tempdf) 
}

## Get Per Pupil Spending data
spending <- data_frame()
files <- list.files(path="../../data", pattern = "perPupilExpeditures", full.names = TRUE)
for (f in files) {
  flen <- nchar(f)
  tempdf <- read_csv(f, skip = 5, na = "N/A", 
                     col_names  = c("DistrictName", "TeachingStaff", "TeachingSupplies", "MediaServices", "StudentSupport", 
                                    "Administration", "Facilities", "Transportation", "Other", "TotalSpend")) %>%
    mutate(SchoolYear = substr(f, flen - 8,flen - 4)) %>%
    filter_all(all_vars(!is.na(.)))
  
  spending <-bind_rows(spending, tempdf) 
}

## Get School Enrollment Data
enrollment <- data_frame()
files <- list.files(path="../../data", pattern = "enrollmentSingleYear", full.names = TRUE)
for (f in files) {
  flen <- nchar(f)
  tempdf <- read_csv(f, skip = 6, na = "*",
                     col_names  = c("DistrictName", "DistrictCode", "SchoolName", "SchoolCode", "NativeAmerican", 
                                    "Asian", "Black", "Hispanic", "Islander", "Mixed", "White", "Total")) %>% 
    mutate(SchoolYear = substr(f, flen - 8,flen - 4)) %>%
    select(SchoolCode, SchoolYear, Enrolled = Total)
  
  
  enrollment <-bind_rows(enrollment, tempdf) 
}

## Combine datasets together and calculate 4-year averages
hsdata <- inner_join(absent, enrollment, by = c("SchoolCode", "SchoolYear")) %>%  
  inner_join(spending, by = c("DistrictName", "SchoolYear")) %>%
  group_by(SchoolName, SchoolCode, DistrictName, DistrictCode) %>%
  ## Use schools that have data for all four years
  filter(n() == 4) %>% select(-SchoolYear) %>%
  ## Calculate the 4-year mean values for all variables
  summarize_all(function (x) {signif(mean(x, na.rm=TRUE), 4)})

## Load graduation and college entry rates for class of 2015
results <- read_xlsx(path="../../data/nextgenresults15-16.xlsx", sheet = 3) %>%
  filter(grepl("School", SchoolOrgType) & !is.na(Ind8Rate) & 
           SchoolLowGrade == 9 & SchoolHighGrade == 12) %>%
  select(SchoolCode, SchoolOrgType, GradRate = Ind8Rate, CollegeRate = Ind10Rate) %>%
  mutate(SchoolCode = as.integer(SchoolCode), 
         GradRate = round(100*GradRate, 1), CollegeRate = round(100*CollegeRate, 1)) 

hsdatasum <- inner_join(results, hsdata, by = "SchoolCode") %>%
  select(SchoolName, DistrictName, GradRate, CollegeRate, Enrolled, PctAbsent, TotalSpend)

shinyServer(function(input, output) {
  
  fit <- reactive({
    Formula <- as.formula(paste(input$outcome, "~", input$indepvar))
    fit <- lm(data = hsdatasum, Formula)
    names(fit$coefficients) <- c("Intercept", input$indepvar)
    fit
  })
    
  
  # Regression output
  output$summary <- renderPrint({
    print(paste("Formula:", input$outcome, "~", input$indepvar))
    summary(fit())
  })
  
  # Data output
  output$tbl = renderDataTable({
    datatable(hsdatasum, options = list(lengthChange = FALSE), rownames = FALSE)
  })
  
  
  # Scatterplot output
  output$scatterplot1 <- renderPlotly({
   fmla.x <- as.formula(paste("~", input$indepvar))
   fmla.y <- as.formula(paste("~", input$outcome))
   loess1 <- loess.smooth(as.data.frame(hsdatasum[,input$indepvar]), 
                          as.data.frame(hsdatasum[,input$outcome]))
   
   plot_ly(hsdatasum, x = fmla.x, y = fmla.y, type = "scatter", mode = "markers",
           text = ~SchoolName, marker = list(size = 10, opacity = 0.5)) %>%
     layout(title = paste(input$indepvar, "vs", input$outcome),
            xaxis = list(title = input$indepvar),
            yaxis = list(title = input$outcome)) %>%
     add_trace(y = fitted(fit()), type = "scatter", mode = "line", name = "Smooth",
             line = list(width = 2))
  })
  
  output$scatterplot <- renderPlotly({
    p <- ggplot(hsdatasum, aes_string(x=input$indepvar, y=input$outcome)) + 
     geom_point(size = 2, alpha = 0.5) + geom_smooth(method="lm", col="red", size=0.5) +
    xlab(input$indepvar) + ylab(input$outcome)
   ggplotly(p)
    
    })
  
  
  # Histogram output var 1
  output$distribution1 <- renderPlotly({
    fmla.x <- as.formula(paste("~", input$indepvar))
    plot_ly(hsdatasum, x = fmla.x, type = "histogram") %>%
      layout(title = input$indepvar)
  })
  
  # Histogram output var 2
  output$distribution2 <- renderPlotly({
    fmla.y <- as.formula(paste("~", input$outcome))
    plot_ly(hsdatasum, x = fmla.y, type = "histogram") %>%
      layout(title = input$outcome)
  })
  
})
