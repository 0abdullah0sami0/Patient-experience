dashboardPage(
  dashboardHeader(disable=T),
  dashboardSidebar(disable=T),
  
  dashboardBody(
    # set background to white
    setBackgroundColor(color = "#FFFFFF", shinydashboard = FALSE),
    setBackgroundColor(color = "#FFFFFF", shinydashboard = TRUE),
    tags$head(
      tags$link(rel = "shortcut icon", href = "Good-logo.ico")
    ),
    navbarPage(title=img(id="logo", src="Good logo.png", style="width:100%;height:150%"),
               theme = shinytheme("lumen"),
               windowTitle = "Employee report",
               tabPanel("Service description",
                        column(width = 12,align = "center",
                               h3("Employee report"),
                               p("This service is a gift from me for employees to help you track your progress in transactions processing in a simple and easy way using 7 indicators. To learn more about this tool, you can check the video below and download the sample data excel file to start using it yourself."),
                               br(),
                               fluidRow(downloadButton("id1","Download Excel file")
                               ),
                               br(),
                               fluidRow(
                                 column(width = 12,
                                        embed_url("https://www.youtube.com/watch?v=-IBxIXBaei8") %>%
                                          use_bs_responsive()
                                 )
                               )
                        )
               ),
               tabPanel("Dashboard",
                        column(width = 12,align = "center",
                               h3("Upload File"),
                               fluidRow(fileInput("id2", "Upload data (Max 20Mb)",accept = ".xlsm")
                               ),
                               br(),
                               sidebarLayout(
                                 sidebarPanel(
                                   dateRangeInput("dateinput","Action Date"),
                                   selectInput("division","Division",NA),
                                   pickerInput("type","Complaint type",NA,multiple = TRUE),
                                   pickerInput("concern_division","Concern Division",NA,multiple = TRUE),
                                   pickerInput("reason","Reason",NA,multiple = TRUE),
                                   actionButton("action1","Run"),
                                   downloadButton("action2","Download")
                                 ),
                                 mainPanel(column(width = 12,align = "left",
                                                  p("The objective of this application is to generate 7 indicators using the employee's transactions history database in the form of a dashboard. The indicators included in the dashboard are:"),
                                                  tags$ol(
                                                    tags$li("Total number of transactions"),
                                                    tags$li("Average time period to complete transactions"),
                                                    tags$li("Most finished transaction type"),
                                                    tags$li("Total count of transactions by status"),
                                                    tags$li("Total count of transaction type by status"),
                                                    tags$li("Average process time period by transaction"),
                                                    tags$li("Count of finished transactions by date")
                                                    
                                                  )
                                 ))
                               ),
                               br(),
                               fluidRow(
                                 column(width = 4, plotlyOutput("plot1")
                                 ),
                                 column(width = 4,valueBoxOutput("box1", 12),
                                        valueBoxOutput("box2", 12),
                                        valueBoxOutput("box3", 12),
                                        
                                 ),
                                 column(width = 4,valueBoxOutput("box4", 12),
                                        valueBoxOutput("box5", 12),
                                        valueBoxOutput("box6", 12),
                                        
                                 )
                               ),
                               fluidRow(
                                 column(width = 6,plotlyOutput("plot2")
                                 ),
                                 column(width = 6,plotlyOutput("plot3")
                                 )
                               ),
                               fluidRow(
                                 column(width = 12,dataTableOutput("table")))
                        )
               )
    )
  )
)
