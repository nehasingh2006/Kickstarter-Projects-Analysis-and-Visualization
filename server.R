server <- function(input, output, session) { 
  
  ################### Radio Button ###############################
  observeEvent(input$graph_id, {
    if (!is.null(input$graph_id)) {
      if(input$graph_id == "A") {
        output$value2 <- NULL
        #   output$mymap <- NULL
        output$obs1 <- renderPlotly({ TotaCountPlot })
      }
      else if(input$graph_id == "B") {
        output$value2 <- NULL
        # output$mymap <- NULL
        output$obs1 <- renderPlotly({ ggplotly(SuccessRate) })
      }
      else if(input$graph_id == "C") {
        output$value2 <- NULL
        #    output$mymap <- NULL
        output$obs1 <- renderPlotly({ ggplotly(backerplot2) })
      }
      else if(input$graph_id == "D") {
        output$value2 <- NULL
        #  output$mymap <- NULL
        output$obs1 <- renderPlotly({ ggplotly(ps) })
      } 
      else if(input$graph_id == "E") {
        output$value2 <- NULL
        # output$mymap <- NULL
        output$obs1 <- renderPlotly({ ggplotly(campaign_period_plot) })
      }
      else {NULL}
    }
  })
  
  ############################Tab - Analysis################################
  output$sn <- renderSankeyNetwork({ skplot })
  
  observeEvent(input$category, {
    observeEvent(input$Visual_types, {
      
      if (!is.null(input$Visual_types)) {
        if(input$Visual_types == "sub_categ") {
          #  output$aab <- renderPrint(print(input$Visual_types))
          # output$aaa <- renderPrint(print(input$category))
          output$Proj_title <- NULL
          output$obs2 <- renderPlotly({ CountPerCat(colN = input$category) })
        }
        if (input$Visual_types == "no_succ_sub_categ") {
          output$Proj_title <- NULL
          output$obs2 <- renderPlotly({ succsubpermain(colN = input$category) })
        }
        if (input$Visual_types == "succ_sub_categ_rate") {
          output$Proj_title <- NULL
          output$obs2 <- renderPlotly({ SuccRate(colN = input$category) })
        }
        if (input$Visual_types == "title") {
          output$obs2 <- NULL
          output$Proj_title <- renderPlot({
            withProgress(message = 'Plotting in progress',
                         detail = 'This may take a while...', value = 0, {
                           for (i in 1:240) {
                             incProgress(1/15)
                             Sys.sleep(0.25)
                           }
                         })
            wordcloudfun(input$category) })
        }
        if (input$Visual_types == "amount") {
          output$Proj_title <- NULL
          output$obs2 <- renderPlotly({ GoalVSPledge(colN = input$category) })
        }
        else { NULL}
      }
    }) 
  })
  
######################Tab - map and data ##################################
  output$mymap<-renderLeaflet({map})
  
  output$tableID <-DT::renderDataTable({
    DT::datatable(datatable)
  })
  
#############################################################################
  
#######################Tab - Wordcloud ######################################
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress(message = 'Plotting in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:240) {
                       incProgress(1/60)
                       Sys.sleep(0.25)
                     }
                   })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  observeEvent(input$category1, {  
    output$obs4 <- renderUI({
      selectInput(inputId = "subcategory",
                  label  = "Choose a Sub Category:",
                  choices = c(unique((KickStarter)%>%filter(main_category==input$category1)
                                     %>%select(category))))
    })
  })
  
  observeEvent(input$update, {  
    output$Proj_title <- renderPlot({ 
      word_cloud_data <- KickStarter %>% 
        filter(main_category == input$category1)  %>% 
        filter(category == input$subcategory) %>% filter(state == "successful") 
      
      text_only_word_cloud <- paste(word_cloud_data$name[0:400000], collapse = '')
      
      Rest_docs <- Corpus(VectorSource(text_only_word_cloud))
      
      # Convert the text to lower case
      Rest_docs <- tm_map(Rest_docs, content_transformer(tolower))
      # Remove numbers
      Rest_docs <- tm_map(Rest_docs, removeNumbers)
      # Remove english common stopwords
      Rest_docs <- tm_map(Rest_docs, removeWords, stopwords("english"))
      # Remove your own stop word
      # specify your stopwords as a character vector
      Rest_docs <- tm_map(Rest_docs, removeWords, c("blabla1", "blabla2")) 
      # Remove punctuations
      Rest_docs <- tm_map(Rest_docs, removePunctuation)
      # Eliminate extra white spaces
      Rest_docs <- tm_map(Rest_docs, stripWhitespace)
      
      ###############
      Rest_dtm <- TermDocumentMatrix(Rest_docs)
      Rest_m <- as.matrix(Rest_dtm)
      Rest_v <- sort(rowSums(Rest_m),decreasing=TRUE)
      Rest_d <- data.frame(word = names(Rest_v),freq=Rest_v)
      set.seed(1)
      
      wordcloud_rep(words = Rest_d$word, freq = Rest_d$freq, min.freq = 1,
                    max.words=input$max, random.order=FALSE, rot.per=0.35, 
                    colors=brewer.pal(8, "Dark2"))
    })
  })
##############################################################################

}  




