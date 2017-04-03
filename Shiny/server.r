library(ggplot2)

shinyServer(function(input, output) {
  output$dataPlot <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df = read.csv(inFile$datapath)
    
    df_subset <- df[c(input$xcol,input$ycol)]
    #print(df_subset)
 
    g <- ggplot(NULL)+
      geom_point(data=df_subset, aes(x=df_subset[1],y=df_subset[2], color="red", size=5))+
      xlab("Independent Variable")+
      ylab("Dependent Variable")
    #print(g)  
    
    if(is.null(input$checkGroup)){
      return(print(g))
    }
    else{
      
      if(input$checkGroup == 1 & length(input$checkGroup)==1) {
        g1 <- g + 
          geom_smooth(data=df_subset,aes(x=df_subset[1],y=df_subset[2]), method='lm')
        return(print(g1))
      }
      else if(input$checkGroup == 2 & length(input$checkGroup)==1) {
        g2 <- g + 
          geom_smooth(data=df_subset,aes(x=df_subset[1],y=df_subset[2]), method='lm',formula = y ~ x + I(x^2), color = "purple")
        return(print(g2))
      }  
      else {
        g3 <- g + 
          geom_smooth(data=df_subset,aes(x=df_subset[1],y=df_subset[2]), method='lm')+
          geom_smooth(data=df_subset,aes(x=df_subset[1],y=df_subset[2]), method='lm',formula = y ~ x + I(x^2), color = "purple")
        return(print(g3))  
      }
    }
  })
  

  
  output$Xcolumns = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(selectInput('xcol', 'X Variables', ' '))
    
    mydata = read.csv(inFile$datapath)
    selectInput('xcol', 'X Variables', names(mydata),
                selected=names(mydata)[[2]])
  })
  
  output$Ycolumns = renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(selectInput('ycol', 'Y Variables', ' '))
    
    mydata = read.csv(inFile$datapath)
    selectInput('ycol', 'Y Variables', names(mydata),
                selected=names(mydata)[[3]])
  })
})
