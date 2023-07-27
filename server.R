shinyServer(function(input,output,session){
  
  df <- reactiveVal("AN")
  output$id1 <- downloadHandler(
    filename = function() {
      paste0("My data ", Sys.Date(), ".xlsm")},
    content = function(My_data){
      file.copy("database.xlsm",My_data)}
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
    updateDateRangeInput(session, inputId = "dateinput","Action Date", min = min(df$ActionDate,na.rm = TRUE), max = max(df$ActionDate,na.rm = TRUE))
    updateSelectInput(session, "division","Division",df$Division[!is.na(df$Division)])
    updatePickerInput(session, "type","Complaint type",choices = unique(df$`نوع الشكوى`[!is.na(df$`نوع الشكوى`)]),options = list(`actions-box` = T))
    updatePickerInput(session, "concern_division","Concerns Division",choices = unique(df$`يخص المركز`[!is.na(df$`يخص المركز`)]),options = list(`actions-box` = T))
    updatePickerInput(session, "reason","Reason",choices = unique(df$`سبب البلاغ`[!is.na(df$`سبب البلاغ`)]),options = list(`actions-box` = T))
    
    df(df)
    

    # output$plot2 <- renderPlotly({
    #   plot2 <- df() %>%
    #     filter(!is.na(situation)) %>%
    #     group_by(`type of process`,situation) %>%
    #     summarise(count = n()) %>%
    #     ggplot(aes(x = `type of process`, y = count, fill = situation)) +
    #     geom_bar(stat = "identity") +
    #     ggtitle("Count of each transaction type by status") +
    #     xlab("") +
    #     ylab("") +
    #     theme_classic() +
    #     theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5)) +
    #     scale_fill_manual(values = c("#BFBFBF", "#417FA6"))
    #   # scale_fill_brewer(palette="Dark2")
    #   ggplotly(plot2,tooltip = c("y","fill")
    #   )
    # })
    # output$plot3 <- renderPlotly({
    #   plot3 <- df() %>%
    #     filter(situation == "finished") %>%
    #     mutate(`last process` = as.Date(`last process`,format = "%y-%m-%d"),
    #            `first process` = as.Date(`first process`,format = "%y-%m-%d"),
    #            Average = `last process` - `first process`) %>%
    #     group_by(`type of process`) %>%
    #     summarise(Average = round(mean(Average))) %>%
    #     ggplot(aes(x = `type of process`, y = Average, fill = `type of process`)) +
    #     geom_bar(stat = "identity") +
    #     ggtitle("Average process time by transaction in days") +
    #     xlab("") +
    #     ylab("Days") +
    #     theme_classic() +
    #     theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5)) +
    #     scale_fill_manual(values = c("#A2A6F2","#BFBFBF", "#417FA6","#3F89A6","#0D0D0D"))
    #   ggplotly(plot3,tooltip = c("y")
    #   )
    # })
    # output$plot4 <- renderPlotly({
    #   plot4 <- df() %>%
    #     filter(situation == "finished") %>%
    #     mutate(`last process` = as.Date(`last process`,format = "%y-%m-%d")) %>%
    #     group_by(`last process`) %>%
    #     summarise(count = n()) %>%
    #     ggplot(aes(x = `last process`, y = count)) +
    #     geom_line(color = "#3F89A6") +
    #     geom_point() +
    #     ggtitle("Count of finished transactions by date") +
    #     xlab("End of transaction") +
    #     ylab("") +
    #     theme_classic() +
    #     theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5))
    #   ggplotly(plot4,tooltip = c("y")
    #   )
    # })
    # output$box1 <- renderValueBox({
    #   boxA = df() %>%
    #     filter(!is.na(`type of process`)) %>%
    #     summarise(count = n()
    #     )
    #   valueBox(boxA$count,"Number of transactions")
    # })
    # output$box2 <- renderValueBox({
    #   boxB = df() %>%
    #     filter(situation == "finished") %>%
    #     mutate(`last process` = as.Date(`last process`,format = "%y-%m-%d"),
    #            `first process` = as.Date(`first process`,format = "%y-%m-%d"),
    #            Average = `last process` - `first process`) %>%
    #     summarise(Average = round(mean(Average)))
    #   valueBox(boxB$Average,"Average process time in days")
    # })
    # output$box3 <- renderValueBox({
    #   boxC = df() %>%
    #     filter(situation == "finished") %>%
    #     group_by(`type of process`) %>%
    #     summarise(count = n())
    # 
    #   boxC <- boxC[boxC$count == max(boxC$count),"type of process"]
    # 
    #   valueBox(boxC$`type of process`,"Most finished transaction type")
    # })
    
    
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
      
      
    print(call_prct)
    print(orient_prct)
    
      variable = c("الجلسات الاسترشادية","محاضر الاتصال")
      pct = c(orient_prct,call_prct)
      
      plot1data <- data.frame(variable,pct)
      
      print(plot1data)
      
      plot1 <- plot1data %>%
        ggplot(aes(x = variable, y = pct, fill = variable, label = paste0(round(pct*100),"%"))) +
        geom_bar(stat = "identity") +
        theme_classic() +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5)) +
        ggtitle("") +
        xlab("") +
        ylab("") +
        scale_fill_manual(values = c("#BFBFBF", "#417FA6"))
      # scale_fill_brewer(palette="Dark2")
      ggplotly(plot1,tooltip = c("label")
      )
    })
    
  })
  
})
