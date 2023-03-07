library(shiny)
library(tidyverse)

df <- read_delim("df copy.csv") %>% 
  mutate(newRegion = factor(Region)) %>% 
  rename(Admission_Rate = "Admission Rate")
head(df)
region <- unique(df$Region)
states <- unique(df$State)


ui <- fluidPage(

# Application title
    titlePanel("The Admissions Rate of US Colleges and Universities in 2020-2021"),
    mainPanel(
      tabsetPanel(type = "tab",
        tabPanel("About",
                 h3("About SAT (Scholastic Aptitude Test) and ACT (American College Testing)", align = "center"),
                   p(strong("SAT and ACT scores are used in the process of
                            college admissions to allow students to narrow down
                            their choices and make an important decision on 
                            their acceptance. With the study of this data set,
                            we are able to compare the rate of acceptance of
                            multiple universities and colleges in the United
                            States to SAT and ACT scores."), align = "center"),
                     p("The data set ", 
                        em("US College & University Admissions 2020-2021"),
                        "examines all of the colleges and universities under
                        each state in the US and review the number of applicants
                        each university received, the number of students
                        admitted and enrolled, the average accemptance rate, and 
                        the average SAT/ACT score.", align = "center"),
                     hr("Within this data set, it contains 1386 institutions and
                        9 variables."),
                     br("Here, we have a random sample of the data: "),
                     tableOutput("sampleTable")),
        
        tabPanel("Plots",
                 h3("Rate of Students Admitted in Each University and College with Math SAT & ACT Scores"),
                 sidebarPanel(
                   sliderInput("SAT_Range",
                               "Range of SAT Scores: ",
                               min = 0,
                               max = 800,
                               value = c(0,800)
                    ),
                   checkboxGroupInput("Region",
                                      "Choose which region(s) to plot: ",
                                      choices = region,
                                      selected = region)
                 ),
                 mainPanel(
                   plotOutput("satplotmath")),
                 
                 sidebarPanel(
                   sliderInput("ACT_Range",
                               "Range of ACT Scores: ",
                               min = 0,
                               max = 36,
                               value = c(0,36)
                    ),
                   checkboxGroupInput("Region",
                                      "Choose which region(s) to plot: ",
                                      choiceNames = region,
                                      choiceValues = region)
                    ),
                 
                 mainPanel(
                   plotOutput("actplotmath"))),
        
        tabPanel("Table",
                 h3("Average SAT and ACT Math Scores in Each U.S. State"),
                 sidebarPanel(
                   selectInput("State", "Select State:",
                               choices = states
                   ),
                 ),
                 mainPanel(dataTableOutput("table"),
                           textOutput("sentence"))
        ),
        
        )))


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$sampleTable <- renderTable({
      df %>% 
        sample_n(5)
    })
    
    output$satplotmath <- renderPlot({
      df %>% 
        filter(newRegion %in% input$Region) %>% 
        filter(SATMT75 >= input$SAT_Range[1],
               SATMT75 <= input$SAT_Range[2]) %>% 
        ggplot(aes(SATVR75, Admission_Rate, col = newRegion)) +
        labs(title = "SAT Math Score and Admissions Rate",
             x = "75th Percentile SAT Math Score", y = "Admission Rate") +
        geom_point()
    })
    
    output$actplotmath <- renderPlot({
      df %>% 
        filter(newRegion %in% input$Region) %>% 
        filter(ACTMT75 >= input$ACT_Range[1],
               ACTMT75 <= input$ACT_Range[2]) %>% 
        ggplot(aes(ACTMT75, Admission_Rate, col = newRegion)) +
        labs(title = "ACT Math Score and Admissions Rate",
             x = "75th Percentile ACT Math Score", y = "Admission Rate") +
        geom_point()
    })
    
    output$avgdata <- renderTable({
      statefilter <- subset(df, df$State == input$inState)
    })
    
    output$table <- renderDataTable({
      df %>%
        group_by(input$State) %>%
        filter(!is.na(SATMT75), !is.na(ACTMT75), State == input$State) %>%
        summarize(avgSATMT = mean(SATMT75), avgACTMT = mean(ACTMT75)) 
    })
    
    output$sentence <- renderText({
      scoreavg <- df %>%
        group_by(input$State) %>%
        filter(State == input$State, !is.na(SATMT75), !is.na(ACTMT75)) %>%
        summarize(avgSATMT = mean(SATMT75), avgACTMT = mean(ACTMT75))
      paste("The average SAT and ACT math score for", scoreavg[1], "are", scoreavg[2],"and", scoreavg[3], "respectively." )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
