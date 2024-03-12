library(shiny)
library(shinydashboard)
library(DT)

# UI
ui <- dashboardPage(skin = "purple",
      dashboardHeader(title = "Dashboard Kelompok F",
                      titleWidth = 300),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Visualisasi", tabName = "visualisasi", icon = icon("chart-bar")),
      menuItem("Interpretasi", tabName = "interpretasi", icon = icon("comment"))
      )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content (Data)
      tabItem(tabName = "data", 
              h1("Input File Data"),
              fileInput('file1', h3("Choose CSV file"))
      ),
      # Second tab content (Visualisasi)
      tabItem(tabName = "visualisasi",
              h1("Visualisasi Data"),
              # dataset table
              h4("Dataset"),
              DTOutput("view_data"), 
              tags$hr(),
              
              # stat. deksriptif
              h4("Statistika Deskriptif"),
              verbatimTextOutput("summary"),
              verbatimTextOutput("summary2"),
              tags$hr(),
              
              # plot
              h4("Plot"),
              ## plot 1 
              h5("Histogram"),
              selectInput("var1", # select x variable
                          label = "Choose variable to display a histogram",
                          choices = c("math_score",                 
                                      "reading_score",               
                                      "writing_score",               
                                      "average_score"),
                          selected = "math_score"),
              sliderInput("bins1", # select bins
                          label = "Number of bins:",
                          min = 1,
                          max = 50,
                          value = 30),
              plotOutput("plot1"),
              tags$hr(),
              ## plot 2
              h5("Scatterplot"),
              selectInput("var2", # select x variable
                          label = "Choose x variable to display a scatterplot",
                          choices = c("math_score",                 
                                      "reading_score",               
                                      "writing_score",               
                                      "average_score"),
                          selected = "math_score"),
              selectInput("var3", # select y variable
                          label = "Choose y variable to display a scatterplot",
                          choices = c("math_score",                 
                                      "reading_score",               
                                      "writing_score",               
                                      "average_score"),
                          selected = "average_score"),
              plotOutput("plot2"),
      ),
      # Third tab content (Interpretasi)
      tabItem(tabName = "interpretasi",
              h1("Analisis & Interpretasi"),
              h3("Dataset"),
              h4("Dataset yang digunakan terdiri dari 9 variabel dengan jumlah data sebanyak 986 data."),
              h3("Analisis Deskriptif"),
              h4("Dari hasil analisis deskriptif, dapat diketahui bahwa :"),
              h4("- Untuk variabel math_score diperoleh score terendah yaitu 27.00 dan score tertinggi yaitu 100.00 dengan rata-rata score 66.69."),
              h4("- Untuk variabel reading_score diperoleh score terendah yaitu 31.00 dan tertinggi 100.00 dengan rata-rata reading_score yaitu 69.72."),
              h4("- Untuk variabel writing_score diperoleh score terendah yaitu 32.00 dan tertinggi 100.00 dengan rata-rata writing_score 68.65."),
              h4("- Untuk variabel average_score diperoleh nilai terendah yaitu 31.00 dan tertinggi 100.00 dengan rata-rata 68.36."),
              h4("- Nilai varians terkecil yang diperoleh yaitu pada variabel average_score sebesar 1.811470e+02 dan standar deviasi sebesar 1.345909e+01, sehingga dikatakan data pada variabel average_score tersebar dengan rentang nilai yang kecil
                 sedangkan nilai varians dan standar deviasi terbesar diperoleh pada variabel writing_score yang artinya semakin jauh penyebaran data variabel writing_score dari rata-ratanya dan data tersebar dengan rentang nilai yang besar."),
              h3("Plot"),
              h4("1. Berdasarkan plot histogram dapat diketahui bahwa :"),
              h4("- Dari visualisasi variabel math score terlihat rata-rata histogram berada di sekitar 70 dan terlihat frekuensi tertinggi pada nilai 70."),
              h4("- Dari visualisasi variabel reading score terlihat histogram membentuk bell-shaped dengan rata-rata berada di sekitar 70 dengan data yang terlihat berdistribusi normal."),
              h4("- Dari visualisasi variabel writing score terlihat histogram membentuk bell-shaped dengan rata-rata berada di antara 65-75 dengan data yang terlihat berdistribusi normal."),
              h4("- Dari visualisasi variabel average score terlihat histogram membentuk bell-shaped dengan rata-rata berada mendekati 70 dengan data yang terlihat berdistribusi normal."),
              h4("2. Berdasarkan plot scatter plot dapat diketahui bahwa :"),
              h4("- Hubungan Variabel math score dan reading score adalah naik ke kanan atas yang berarti memiliki hubungan positif kuat yang berarti kedua variabel saling mempengaruhi satu sama lain"),
              h4("- Hubungan Variabel math score dan writing score adalah naik ke kanan atas yang berarti memiliki hubungan positif kuat yang berarti kedua variabel saling mempengaruhi satu sama lain"),
              h4("- Hubungan Variabel math score dan average score adalah naik ke kanan atas yang berarti memiliki hubungan positif kuat yang berarti kedua variabel saling mempengaruhi satu sama lain"),
              h4("- Hubungan Variabel reading score dan writing score adalah naik ke kanan atas yang berarti memiliki hubungan positif kuat yang berarti kedua variabel saling mempengaruhi satu sama lain"),
              h4("- Hubungan Variabel reading score dan average score adalah naik ke kanan atas yang berarti memiliki hubungan positif kuat yang berarti kedua variabel saling mempengaruhi satu sama lain"),
              h4("- Hubungan Variabel writing score dan average score adalah naik ke kanan atas yang berarti memiliki hubungan positif kuat yang berarti kedua variabel saling mempengaruhi satu sama lain"),)
      )
    )
  )


# SERVER
server <- function(input, output) {
  observe({
    # read input data file
    file1 = input$file1
    if (is.null(file1)) {
      return(NULL)
    }
    data1 = read.csv(file1$datapath)
    
    # output dataset table
    output$view_data <- renderDT(
      {data1}, options=list(scrollX = TRUE)
    )
    
    # output stat. deskriptif
    ## summary 1
    output$summary <- renderPrint({
      summary(data1)
    })
    ## summary 2
    library(pastecs)
    nums <- unlist(lapply(data1, is.numeric))  
    output$summary2 <- renderPrint({
      stat.desc(data1[,nums])
    })
    
    # output plot
    ## plot 1
    output$plot1 <- renderPlot({
      #input variable
      if(input$var1=='math_score'){
        i<-6
      }
      if(input$var1=='reading_score'){
        i<-7
      }
      if(input$var1=='writing_score'){
        i<-8
      }
      if(input$var1=='average_score'){
        i <- 9 
      }
      x_plot1 <- data1[, i]
      #referring input bins 
      bins1 <- seq(min(x_plot1), max(x_plot1), length.out = input$bins1 + 1)
      #producing histogram as output
      hist(x_plot1, breaks = bins1, col = 'darkgray', main=paste("Histogram of", input$var1), border = 'white', xlab=input$var1)
      hist
    })
    
    ## plot 2
    output$plot2 <- renderPlot({
      #input variable x
      if(input$var2=='math_score'){
        j<-6
      }
      if(input$var2=='reading_score'){
        j<-7
      }
      if(input$var2=='writing_score'){
        j<-8
      }
      if(input$var2=='average_score'){
        j<-9
      }
      x_plot2 <- data1[, j]
      #input variable y
      if(input$var3=='math_score'){
        k<-6
      }
      if(input$var3=='reading_score'){
        k<-7
      }
      if(input$var3=='writing_score'){
        k<-8
      }
      if(input$var3=='average_score'){
        k<-9
      }
      y_plot2 <- data1[, k]
      # scatterplot as output
      plot(x_plot2, y_plot2, main = paste("Scatterplot of",input$var2,"and",input$var3),
           xlab = input$var2, ylab = input$var3,
           pch = 19, frame = FALSE)
    })
  })
}
  
shinyApp(ui=ui, server=server)