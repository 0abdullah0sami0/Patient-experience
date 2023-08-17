shinyServer(function(input,output,session){
  
  df <- reactiveVal("AN")
  output$id1 <- downloadHandler(
    filename = function() {
      paste0("تجربة المريض نطاق الخرج ", Sys.Date(), ".zip")},
    content = function(My_data){
      file.copy("Patient experience.zip",My_data)}
  )
  observeEvent(input$id2,{
    data <- read_excel(input$id2$datapath, sheet = "بلاغات مركز الخالدية") 
    data <- data[,1:11]
    data2 <- read_excel(input$id2$datapath, sheet = "الإجراءات") 
    data3 <- read_excel(input$id2$datapath, sheet = "سجل الجلسات الاسترشادية") 
    data4 <- read_excel(input$id2$datapath, sheet = "سجل محاضر الاتصال") 
    df <- merge(data,data2, by = "TicketNumber", all.x = TRUE)
    df <- merge(df,data3, by = "TicketNumber", all.x = TRUE)
    df <- merge(df,data4, by = "TicketNumber", all.x = TRUE)
    
    print(df)
    
    df$ActionDate <- as.Date(df$ActionDate,format = "%yyyy-%m-%d")
    updateDateRangeInput(session, inputId = "dateinput","التاريخ", min = min(df$ActionDate,na.rm = TRUE),start = min(df$ActionDate,na.rm = TRUE),end = max(df$ActionDate,na.rm = TRUE), max = max(df$ActionDate,na.rm = TRUE))
    updateSelectInput(session, "division","المركز",df$Division[!is.na(df$Division)])
    updatePickerInput(session, "type","نوع الشكوى",choices = unique(df$`نوع الشكوى`[!is.na(df$`نوع الشكوى`)]),options = list(`actions-box` = T),selected = unique(df$`نوع الشكوى`[!is.na(df$`نوع الشكوى`)]))
    updatePickerInput(session, "concern_division","يخص المركز",choices = unique(df$`يخص المركز`[!is.na(df$`يخص المركز`)]),options = list(`actions-box` = T), selected = unique(df$`يخص المركز`[!is.na(df$`يخص المركز`)]))
    updatePickerInput(session, "reason","سبب البلاغ",choices = unique(df$`سبب البلاغ`[!is.na(df$`سبب البلاغ`)]),options = list(`actions-box` = T), selected = unique(df$`سبب البلاغ`[!is.na(df$`سبب البلاغ`)]))
    
    df(df)
  })
  
  observeEvent(input$action1,{
    
    print(input$dateinput)
    
    df <- df() %>%
      filter(ActionDate >= min(input$dateinput) & ActionDate <= max(input$dateinput) &
               Division == input$division &
               `نوع الشكوى` %in% input$type &
               `يخص المركز` %in% input$concern_division &
               `سبب البلاغ` %in% input$reason
               )

    output$box1 <- renderValueBox({
      boxA = df %>%
        summarise(count = n_distinct(TicketNumber)
        )
      valueBox(boxA$count,"عدد البلاغات الكلي")
    })
    
    output$box2 <- renderValueBox({
      boxA = df %>%
        filter(`نوع الشكوى` == "ذات أثر طبي") %>%
        summarise(count = n_distinct(TicketNumber)
        )
      valueBox(boxA$count,"البلاغات ذات أثر طبي")
    })
    
    output$box3 <- renderValueBox({
      boxA = df %>%
        filter(`نوع الشكوى` == "ذات أثر غير طبي") %>%
        summarise(count = n_distinct(TicketNumber)
        )
      valueBox(boxA$count,"البلاغات ذات أثر غير طبي")
    })
    
    output$box4 <- renderValueBox({
      boxA = df %>%
        summarise(count = n_distinct(`الاجراء`)
        )
      valueBox(boxA$count,"عدد الإجراءات")
    })
    
    output$box5 <- renderValueBox({
      boxA = df %>%
      filter(!is.na(`حالة الاتصال`)) %>%
        summarise(count = n_distinct(TicketNumber))
      valueBox(boxA$count,"عدد محاضر الاتصال")
    })
    
    output$box6 <- renderValueBox({
      boxA = df %>%
        filter(!is.na(`الاسم`)) %>%
        summarise(count = n_distinct(TicketNumber))
      valueBox(boxA$count,"عدد الجلسات الاسترشادية")
    })
    
    output$plot1 <- renderPlotly({
      
      stat_nbr = df %>%
        summarise(count = n_distinct(TicketNumber))
      call_nbr = df %>%
        filter(!is.na(`حالة الاتصال`)) %>%
        summarise(count = n_distinct(TicketNumber))
      orient_nbr = df %>%
        filter(!is.na(`الاسم`)) %>%
        summarise(count = n_distinct(TicketNumber))
      
      call_prct <- as.numeric(call_nbr)/as.numeric(stat_nbr) 
      orient_prct <- as.numeric(orient_nbr) /as.numeric(stat_nbr) 
      
      variable = c("الجلسات الاسترشادية","محاضر الاتصال")
      pct = c(orient_prct,call_prct)
      
      plot1data <- data.frame(variable,pct)
      
      print(plot1data)
      
      plot1 <- plot1data %>%
        ggplot(aes(x = variable, y = pct, fill = variable, text = str_glue("{round(pct*100)} %"))) +
        geom_bar(stat = "identity") +
        theme_classic() +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5)) +
        ggtitle("نسبة محاضر الاتصال والجلسات الاسترشادية من عدد البلاغات") +
        xlab("") +
        ylab("") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = c("#BFBFBF", "#417FA6"))
      # scale_fill_brewer(palette="Dark2")
      ggplotly(plot1,tooltip = c("text")
      )
    })
    
    output$plot2 <- renderPlotly({
        plot3 <- df %>%
          group_by(`سبب البلاغ`) %>%
          summarise(count = n()) %>%
          ggplot(aes(x = `سبب البلاغ`, y = count)) +
          geom_bar(stat = "identity", fill = "#417FA6") +
          ggtitle("عدد كل سبب من أسباب البلاغات") +
          xlab("") +
          ylab("") +
          theme_classic() +
          theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5))
        ggplotly(plot3,tooltip = c("y")
        )
    })
    
    output$plot3 <- renderPlotly({
      plot4 <- df %>%
        filter(!is.na(ActionDate)) %>%
        group_by(ActionDate) %>%
        summarise(count = n()) %>%
        ggplot(aes(x = ActionDate, y = count)) +
        geom_line(color = "#3F89A6") +
        geom_point() +
        ggtitle("عدد البلاغات اليومية") +
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5))
      ggplotly(plot4,tooltip = c("y","x")
      )
    })
    
    output$table <- renderDataTable({
      df %>%
        select(Title,`سبب البلاغ`,`الاجراء`,Description)
    })
  })
  
})
