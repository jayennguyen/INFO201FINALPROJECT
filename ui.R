#load packages

library(dplyr)
library(ggplot2)
library(shiny)
library(maps)
library(mapdata)
library(ggmap)

source('test.R')
#states
states  <-
  c(
    "AL",
    "AK",
    "AS",
    "AZ",
    "AR",
    "CA",
    "CO",
    "CT",   
    "DE",
    "DC",
    "FL",
    "GA",
    "GU",
    "HI",
    "ID",
    "IL",
    "IN",
    "IA",
    "KS",
    "KY",
    "LA",
    "ME",
    "MD",
    "MH",
    "MA",
    "MI",
    "MN",
    "MS",
    "MO",
    "MT",
    "NE",
    "NV",
    "NH",
    "NJ",
    "NM",
    "NY",
    "NC",
    "ND",
    "OH",
    "OK",
    "OR",
    "PA",
    "PR",
    "RI",
    "SC",
    "SD",
    "TN",
    "TX",
    "UT",
    "VT",
    "VA",
    "WA",
    "WV",
    "WI",
    "WY"
  )


my.ui <- fluidPage(
  navbarPage("Interactive College Scorecard",
             
      tabPanel("Home",
        h2("This is the project introduction."),
        h3("if the page takes a long time to load, go and get a cup of coffee(my treat), and blame R for being so slow"),
        p("built by CaiZenan Tom Johnny"),
        h4("General Data Description:"),
        p("Our job is to help the parents and students and government to evaluate the several factors of relate to the university in each state, the tuition,
          the cost of living, the financial support, the earning after graduate. By going through this shinyapp, I am sure to some extent the client will have 
          the basic idea about university education in the quantitative perspective. Considering the data we use, we choose a data from gov open data "),
        h4("These two website(we source the dataframe we use, here)"),
        p("https://catalog.data.gov/dataset/college-scorecard/resource/2a7f670e-0799-436a-9394-df0a9b3ba7c5"),
        p(" https://collegescorecard.ed.gov/assets/CollegeScorecardDataDictionary.xlsx"),
        h4("EarningAnalysisPlot"),
        p("This tab is about the detailed analysis of the earning and earning tuition ratio within each states"),
        h4("MapAaboutEarning"),
        p("This tab is about earning condition throughout universities in US, the client is about to interpret the general condition about earning of students after graduate"),
        h4("Median Debt vs. Cost of Attendance"),
        p("This tab gives some information regarding the Median Debt for student of each college. The median debt is the calculated by taking the median of debt for each college
          student. The graph is then created by plotting the number of colleges which has this median debt. Debt is important to consider when evaluating which state you want to 
          go to school in. Also under the first graph is the cost of attendance. The cost of attendance is calculated for each college by totaling the cost of tuition, living expenses,
          fees, books, and more. This is also important to consider when deciding for a college as a higher cost of attendance means you need more money to go to a certain school.
          So by having both these histograms we can see patterns of high debt or high cost of attendance for each state. "),
        h4("Find College"),
        p("Let's you pick a college and it'll show up on a map!")
      ),
      #mario       
       tabPanel("Income After Grad Related",
                sidebarLayout(
                  ## there are two part ofthe control widge
                  ## one is for the map one is for the plot table
                  sidebarPanel(
                    conditionalPanel(
                      condition="input.tabselected==1",
                      selectInput("State", "Choose a state", state.name),
                      radioButtons("filterData", "Which type of earning you care the most", 
                                   c("The long term earning" = 1, "ShortTerm earning" = 2, "I do not care" = 3)),
                      radioButtons("mySummaryTable", "Do you want to see my conclusion(Of course yes)", 
                                   c("No" = 1, "Yes(after you click this you will be smart)" = 2))
                    ),
                    conditionalPanel(
                      condition="input.tabselected==2", 
                      radioButtons("mySummary", "Do you want to see my conclusion(Of course yes)", 
                                   c("No" = 1, "Yes(after you click this you will be smart)" = 2))
                    )
                  ),
                  
                  mainPanel(
                    ## there are two panel one is about the map and one is about plot
                    ## summary
                    tabsetPanel(type= "tabs", 
                                tabPanel("EarningAnalysisPlot", value = 1,
                                         textOutput("tableHead"),
                                         tags$head(tags$style("#tableHead{color:black;
                                                              font-size:25px;
                                                              font-weight:bold;
                                                              }")
                           ),
                           textOutput("tableTitle"),
                           textOutput("tableInstruction"),
                           plotOutput("cityInStateEarningRatio"),
                           textOutput("top10head"),
                           tags$head(tags$style("#top10head{
                                                color:black;
                                                font-size:25px;
                                                font-weight:bold;
                                                }")
                           ),
                           tableOutput("Top10EarningUniveristy"),
                           textOutput("conclusionAboutTableTitle"),
                           tags$head(tags$style("#conclusionAboutTableTitle{
                                                color:black;
                                                font-size:25px;
                                                font-weight:bold;
                                                }")
                           ),
                           textOutput("conclusionAboutTable"),
                           tags$head(tags$style("#conclusionAboutTable{
                                                color:black;
                                                font-size:15px;
                                                }")
                           )
                           ),
                           tabPanel("MapAboutEarning", value = 2,
                                    textOutput("maptitle"),
                                    tags$head(tags$style("#maptitle{color:black;
                                                         font-size:25px;
                                                         font-weight:bold;
                                                         }")
                           ),
                           textOutput("mapIntro"),
                           plotOutput("usmap", click = "plot_click"),
                           verbatimTextOutput("info"),
                           plotOutput("boxWdiagram"),
                           textOutput("explainboxandw"),
                           textOutput("myconclusiontitle"),
                           tags$head(tags$style("#myconclusiontitle{
                                                color:black;
                                                font-size:25px;
                                                font-weight:bold;
                                                }")
                           ),
                           textOutput("myconclusion"),
                           tags$head(tags$style("#myconclusion{
                                                color:black;
                                                font-size:15px;
                                                
                                                }")
                           ),
                           tableOutput("top10university")
                           
                           ),
                           id = "tabselected"
                           )
                           )
                  )      
      ),      
             
      #mario       
             

    tabPanel("Median Debt Histogram vs. Cost of Attendance Histogram)",
             sidebarLayout(
               sidebarPanel(
                 selectInput("state3", "Choose your State:", choices = states, selected = "WA")
               ),
               mainPanel(
                 h2("Introduction"),
                 p("The goal of this tab is to compare the Median Debt that each university has to the Cost of Attendance for each university. The graphs are created by taking the median student debt
                  of each college and plotting on the histogram. This way we can see which state has the high median debts or low median debts. By also adding the Cost of Attendance for the State as well, 
                  we can determine if going to school in that state is worth the Debt and the Cost.
                   "),
                 plotOutput("plot5"),
                 plotOutput("plot6")
               )
             )

    ),
    tabPanel("Find College",
      sidebarLayout(
        sidebarPanel(
          selectInput("state4", "Choose your State:", choices = states, selected = "WA"),
          conditionalPanel(condition = "input$state4 == input$state4", 
                           uiOutput("colleges"))
        ),
        mainPanel(
          plotOutput("plot4")
        )
        
      )
    ),
    tabPanel("Table",
              dataTableOutput("table")
    )
  )
)


