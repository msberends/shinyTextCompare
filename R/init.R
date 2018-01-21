#' Compare text files line by line
#'
#' Compare text files line by line and see their differences immediately. You can use filepaths as input, or start the app and select them in the app (or use drag and drop).
#' @param file1,file2 Path of files. If not specified, an input box will be shown that supports drag and drop for files.
#' @param file1_colour,file2_colour HTML-values to display as difference between file1 and file2. Defaults tot GitHub colours.
#' @param title Title to display
#' @importFrom grDevices rgb
#' @import shiny
#' @export
textcompare <- function(file1 = NULL,
                        file2 = NULL,
                        file1_colour = rgb(253, 184, 192, maxColorValue = 255),
                        file2_colour = rgb(172, 242, 189, maxColorValue = 255),
                        title = "Shiny TextCompare") {

  # file 1
  if (!is.null(file1)) {
    if (!file.exists(file1)) {
      stop('file1 does not exist')
    }
    file_input1 <- data.frame(name = filename(file1),
                              size = file.size(file1),
                              type = mime::guess_type(file1),
                              datapath = file1)
    file_input1_form <- p('(File 1 already selected as parameter)', class = "file_already_selected")
  } else {
    file_input1 <- fileInput("file1", "File 1:")
    file_input1_form <- file_input1
  }

  # file 2
  if (!is.null(file2)) {
    if (!file.exists(file2)) {
      stop('file2 does not exist')
    }
    file_input2 <- data.frame(name = filename(file2),
                              size = file.size(file2),
                              type = mime::guess_type(file2),
                              datapath = file2)
    file_input2_form <- p('(File 2 already selected as parameter)', class = "file_already_selected")
  } else {
    file_input2 <- fileInput("file2", "File 2:")
    file_input2_form <- file_input2
  }

  if (!is.data.frame(file_input1) | !is.data.frame(file_input2)) {
    main_width <- 9
  } else {
    main_width <- 12
  }

  ui <- fluidPage(
    includeCSS("inst/textcompare.css"),
    tags$head(
      tags$style(HTML(
        paste(
          ".file1 { background-color:", file1_colour, ";}",
          ".file2 { background-color:", file2_colour, ";}")
      ))),
    sidebarLayout(
      if (!is.data.frame(file_input1) | !is.data.frame(file_input2)) {
        sidebarPanel(
          p("Browse files or use drag and drop."),
          file_input1_form,
          file_input2_form,
          tags$hr(),
          p("The files will be compared as soon as the second file is selected."),
          #actionButton("clear", "Clear input"),
          width = 3
        )
      },
      mainPanel(
        h3(title),
        #img(src = 'logo.png'),
        tableOutput("contents"),
        hr(),
        uiOutput("comparison"),
        width = main_width
      )
    )
  )

  server <- function(input, output) {

    output$contents <- renderTable({

      if (is.null(input$file1)) {
        inFile1 <- file_input1
      } else {
        inFile1 <- input$file1
      }
      if (is.null(input$file2)) {
        inFile2 <- file_input2
      } else {
        inFile2 <- input$file2
      }

      if (!is.data.frame(inFile1) | !is.data.frame(inFile2)) {
        return(NULL)
      }
      if (!is.null(inFile1)) {
        df_contents <- data.frame(name = inFile1[1, 'name'],
                                  size = inFile1[1, 'size'],
                                  type = inFile1[1, 'type'],
                                  modi = as.character(file.mtime(as.character(inFile1[1, 'datapath']))))
        if (!is.null(inFile2)) {
          df_contents <- rbind(df_contents,
                               data.frame(name = inFile2[1, 'name'],
                                          size = inFile2[1, 'size'],
                                          type = inFile2[1, 'type'],
                                          modi = as.character(file.mtime(as.character(inFile2[1, 'datapath'])))))
        }
      }
      df_contents[, 'size'] <- human_filesize(df_contents[, 'size'], 0)
      colnames(df_contents) <- c('File Name', 'File Size', 'File Type', 'Last modified')
      df_contents
    })

    output$comparison <- renderText({
      if (is.null(input$file1)) {
        file1 <- file_input1
      } else {
        file1 <- input$file1
      }
      if (is.null(input$file2)) {
        file2 <- file_input2
      } else {
        file2 <- input$file2
      }

      File1 <- as.character(file1$datapath)
      File2 <- as.character(file2$datapath)

      if (length(File1) == 0 | length(File2) == 0) {
        return(NULL)
      }

      A <- readLines(con <- file(File1, "r", encoding = "UTF-8"))
      close(con)
      rm(con)
      B <- readLines(con <- file(File2, "r", encoding = "UTF-8"))
      close(con)
      rm(con)

      max_nchar <- max(nchar(length(A)), nchar(length(B))) + 1

      out <- '<div>'
      # Run through all lines and compare them
      # If there's a difference:
      # - show file1 in red until there are no more differences
      # - show file 2 in green until there are no more differences
      offset_B <- 0
      last_isdiff <- FALSE
      for (i in 1:length(A)) {
        linenum_A <- i
        linenum_B <- i + offset_B
        if (A[i] == B[i + offset_B]) {
          # same, just print
          out <- paste0(out,
                        '<div class="line same linenumber">', i, '</div>',
                        '<div class="line same">', A[i], '</div>')
          last_isdiff <- FALSE
        } else {
          # differs, print A until a line in A matches a line in B
          for (j in (i + 1):length(A)) {
            print(j)
            # which next line exists in File 2?
            #print(A[j] == B)
            #print(B[A[j] == B])
            #print('----')
          }
          out <- paste0(out,
                        '<div class="line file1 linenumber">', i, '</div>',
                        '<div class="line file1">', A[i], '</div>')
          out <- paste0(out,
                        '<div class="line file2 linenumber">', i, '</div>',
                        '<div class="line file2">', B[i], '</div>')
          last_isdiff <- TRUE
          # out <- paste0(out, '<div class="line file1">', A[i], '</div>')
          #
          # out <- paste0(out, '<br>')
          # out <- paste0(out, '<div class="linenumber">', paste0(strrep(" ", max_nchar - nchar(i)), i, " "), '</div>')
          #
          # out <- paste0(out, '<div class="line file2">', B[i], '</div>')
          last_isdiff <- TRUE
        }
        # start new line
        out <- paste0(out, '<br>')
      }
      out <- paste0(out, "</div>")
      out
    })
  }

  shinyApp(ui, server)
}

human_filesize <- function(bytes, decimals = 2) {
  # translated from PHP to R from
  # http://jeffreysambells.com/2012/10/25/human-readable-filesize-php
  size <- c('B','kB','MB','GB','TB','PB','EB','ZB','YB')
  factor <- floor((nchar(bytes) - 1) / 3)
  paste(sprintf(paste0("%.", decimals, "f"), bytes / (1024 ^ factor)), size[factor + 1])
}

filename <- function(file) {
  rev(unlist(strsplit(gsub("/", "\\", file, fixed = TRUE), "\\", fixed = TRUE)))[1]
}
filelastedited <- function(file) {
  file.info(file)
}
