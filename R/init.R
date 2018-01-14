## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    
    includeCSS("www/textcompare.css"),
  
    sidebarLayout(
      sidebarPanel(
        h3("Shiny TextCompare"),
        p("Browse files or use drag and drop."),
        fileInput("file1", "File 1:"),
        fileInput("file2", "File 2:"),
        tags$hr(),
        p("The files will be compared as soon as the second file is selected."),
        actionButton("clear", "Clear input"),
        checkboxInput("header", "Header", TRUE)
      ),
      mainPanel(
        tableOutput("contents"),
        hr(),
        uiOutput("comparison")
      )
    )
  )
  
  server <- function(input, output) {
    
    output$contents <- renderTable({
      inFile1 <- input$file1
      inFile2 <- input$file2
      if (is.null(inFile1) | is.null(inFile2)) {
        return(NULL)
      } 
      if (!is.null(inFile1)) {
        df_contents <- data.frame(name = inFile1[1, 'name'],
                                  size = inFile1[1, 'size'],
                                  type = inFile1[1, 'type'])
        if (!is.null(inFile2)) {
          df_contents <- rbind(df_contents,
                               data.frame(name = inFile2[1, 'name'],
                                          size = inFile2[1, 'size'],
                                          type = inFile2[1, 'type']))
        }
      }
      df_contents[, 'size'] <- paste(round(df_contents[, 'size'] / 1024), 'kB')
      colnames(df_contents) <- c('File Name', 'File Size', 'File Type')
      df_contents
    })
    
    output$comparison <- renderText({
      File1 <- input$file1$datapath
      File2 <- input$file2$datapath
      if (is.null(File1) | is.null(File2)) {
        return(NULL)
      }
      
      A <- readLines(con <- file(File1, "r", encoding = "UTF-8"))
      close(con)
      B <- readLines(con <- file(File2, "r", encoding = "UTF-8"))
      close(con)
      
      out <- '<div>'
      # Run through all lines and compare them
      # If there's a difference:
      # - show file1 in red until the no more difference
      # - show file 2 in green until there is no more difference
      for (i in 1:length(A)) {
        if (A[i] == B[i]) {
          out <- paste0(out, '<p>', A[i], '</p>')
        } else {
          out <- paste0(out, '<p class="file1">', A[i], '</p>')
        }
      }
      out <- paste0(out, "</div>")
      out
    })
  }
  
  shinyApp(ui, server)
}
