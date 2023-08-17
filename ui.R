dashboardPage(
  dashboardHeader(disable=T),
  dashboardSidebar(disable=T),
  
  dashboardBody(
    # set background to white
    setBackgroundColor(color = "#FFFFFF", shinydashboard = FALSE),
    setBackgroundColor(color = "#FFFFFF", shinydashboard = TRUE),
    tags$head(
      tags$link(rel = "shortcut icon", href = "logo.ico")
    ),
    navbarPage(title=img(id="logo", src="logo.jpeg", style="width:100%;height:150%"),
               theme = shinytheme("lumen"),
               windowTitle = "إدارة تجربة المريض نطاق الخرج",
               tabPanel("Service description",
                        column(width = 12,align = "center",
                               h3("برنامج إدارة تجربة المريض"),
                               p("يساعد برنامج إدارة تجربة المريض الموظف على حفظ الملفات بشكل إلكتروني وآمن مع الملئ التلقائي لقاعدة البيانات وتقديم التقارير الاحترافية خلال ثواني"),
                               br(),
                               fluidRow(downloadButton("id1","تحميل البرنامج")
                               ),
                               br(),
                               fluidRow(
                                 column(width = 12,
                                        img(src="image.png", height = 600, width = 1200)
                                 )
                               )
                        )
               ),
               tabPanel("Dashboard",
                        column(width = 12,align = "center",
                               h3("رفع الملف"),
                               fluidRow(fileInput("id2", "القيمة القصوى للبيانات هو ٢٠ ميجا بايت (ملف اكسل)",accept = ".xlsm")
                               ),
                               br(),
                               sidebarLayout(
                                 sidebarPanel(
                                   dateRangeInput("dateinput","التاريخ"),
                                   selectInput("division","المركز",NA),
                                   pickerInput("type","نوع الشكوى",NA,multiple = TRUE),
                                   pickerInput("concern_division","يخص المركز",NA,multiple = TRUE),
                                   pickerInput("reason","سبب البلاغ",NA,multiple = TRUE),
                                   actionButton("action1","عرض"),
                                   downloadButton("action2","تحميل")
                                 ),
                                 mainPanel(column(width = 12,align = "right",
                                                  p("من خلال خمسة فلاتر يمكنك إنشاء تقرير احترافي عن إدارة تجربة المريض في المركز تتضمن العديد من المؤشرات"),
                                                  # tags$ol(
                                                  #   tags$li("Total number of transactions"),
                                                  #   tags$li("Average time period to complete transactions"),
                                                  #   tags$li("Most finished transaction type"),
                                                  #   tags$li("Total count of transactions by status"),
                                                  #   tags$li("Total count of transaction type by status"),
                                                  #   tags$li("Average process time period by transaction"),
                                                  #   tags$li("Count of finished transactions by date")
                                                  #   
                                                  # )
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
