#' Compare text files line by line
#'
#' Compare text files line by line and see their differences immediately. You can use filepaths as input, or start the app and select them in the app (or use drag and drop). The function \code{textcompare_active} can be used in RStudio to use the active source document as input for \code{file1}.
#' @param file1,file2 Path of files. If not specified, an input box will be shown that supports drag and drop for files.
#' @param file1_backgroundcolor,file1_foregroundcolor,file2_backgroundcolor,file2_foregroundcolor HTML-values to display as difference between file1 and file2. Defaults tot GitHub colours. This can be used to theme the app yourself.
#' @param title Title to display. This can be used to theme the app yourself.
#'
#' @import shiny
#' @importFrom grDevices rgb
#' @importFrom tools md5sum
#' @importFrom htmltools htmlEscape
#'
#' @rdname textcompare
#' @export
textcompare <- function(file1 = NULL,
                        file2 = NULL,
                        file1_backgroundcolor = rgb(253, 184, 192, maxColorValue = 255),
                        file1_foregroundcolor = "black",
                        file2_backgroundcolor = rgb(172, 242, 189, maxColorValue = 255),
                        file2_foregroundcolor = "black",
                        title = "Shiny TextCompare") {

  # internal helper functions
  filename <- function(file) {
    rev(unlist(strsplit(gsub("/", "\\", file, fixed = TRUE), "\\", fixed = TRUE)))[1]
  }
  human_filesize <- function(bytes, decimals = 1) {
    # Adapted from:
    # http://jeffreysambells.com/2012/10/25/human-readable-filesize-php
    size <- c('B','kB','MB','GB','TB','PB','EB','ZB','YB')
    factor <- floor((nchar(bytes) - 1) / 3)
    # added slight improvement; no decimals for B and kB:
    decimals <- rep(decimals, length(bytes))
    decimals[size[factor + 1] %in% c('B', 'kB')] <- 0

    paste(sprintf(paste0("%.", decimals, "f"), bytes / (1024 ^ factor)), size[factor + 1])
  }
  escapeHTML <- function(text) {
    htmltools::htmlEscape(text = text, attribute = TRUE)
  }
  printline <- function(linenumA = "", linenumB = "", linetext = NA, printed_A, printed_B) {
    if (!is.na(linetext)
        & !(linenumA != "" & linenumA %in% printed_A)
        & !(linenumB != "" & linenumB %in% printed_B)) {
      if (linenumA != "" & linenumB != "") {
        lineclass <- "same"
      } else if (linenumA != "") {
        lineclass <- 'file1'
        linenumB <- "-"
      } else if (linenumB != "") {
        lineclass <- 'file2'
        linenumA <- "-"
      }
      paste0('<div class="line ', lineclass, ' linenumber">', linenumA, '</div>',
             '<div class="line ', lineclass, ' linenumber">', linenumB, '</div>',
             '<pre class="line ', lineclass, '">', linetext, '</pre>',
             '<br>')
    }
  }

  # process file 1
  if (!is.null(file1)) {
    if (!file.exists(file1)) {
      stop('file1 does not exist')
    }
    file_input1 <- data.frame(id = "X",
                              name = filename(file1),
                              size = file.size(file1),
                              type = character(1),
                              datapath = file1)
    file_input1_form <- p('(File 1 already selected)', class = "file_already_selected")
  } else {
    file_input1 <- fileInput("file1", "File 1:")
    file_input1_form <- file_input1
  }

  # process file 2
  if (!is.null(file2)) {
    if (!file.exists(file2)) {
      stop('file2 does not exist')
    }
    file_input2 <- data.frame(id = "X",
                              name = filename(file2),
                              size = file.size(file2),
                              type = character(1),
                              datapath = file2)
    file_input2_form <- p('(File 2 already selected)', class = "file_already_selected")
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
          ".file1 { color:", file1_foregroundcolor, "!important; background-color:", file1_backgroundcolor, "!important;}",
          "tbody > tr:first-child > td:first-child { color:", file1_foregroundcolor, "!important; background-color:", file1_backgroundcolor, "!important;}",
          ".file2 { color:", file2_foregroundcolor, "!important; background-color:", file2_backgroundcolor, "!important;}",
          "tbody > tr:last-child > td:first-child { color:", file2_foregroundcolor, "!important; background-color:", file2_backgroundcolor, "!important;}")
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
        tableOutput("contents"),
        uiOutput("contents2"),
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

      if (!is.data.frame(inFile1)) {
        return(NULL)
      }
      if (!is.null(inFile1)) {
        df_contents <- data.frame(id = "X",
                                  name = inFile1[1, 'name'],
                                  size = inFile1[1, 'size'],
                                  type = mime::guess_type(as.character(inFile1[1, 'datapath'])),
                                  modi = as.character(file.mtime(as.character(inFile1[1, 'datapath']))),
                                  hash = tools::md5sum(as.character(inFile1[1, 'datapath'])))
        if (!is.null(inFile2) & is.data.frame(inFile2)) {
          df_contents <- rbind(df_contents,
                               data.frame(id = "X",
                                          name = inFile2[1, 'name'],
                                          size = inFile2[1, 'size'],
                                          type = mime::guess_type(as.character(inFile2[1, 'datapath'])),
                                          modi = as.character(file.mtime(as.character(inFile2[1, 'datapath']))),
                                          hash = tools::md5sum(as.character(inFile2[1, 'datapath']))))
        }
      }
      df_contents[, 'size'] <- human_filesize(df_contents[, 'size'])
      colnames(df_contents) <- c(' ', 'File Name', 'File Size', 'File Type', 'Last modified', 'MD5 Hash')
      df_contents
    })

    output$contents2 <- renderUI({

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

      timedif <- difftime(file.mtime(File2), file.mtime(File1))
      div(HTML(paste0("Time difference of <b>",
          round(abs(timedif)),
          " ",
          attributes(timedif)$units,
          "</b>. Size difference of <b>",
          human_filesize(abs(file.size(File2) - file.size(File1))),
          "</b>."
        )))

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
      A <- escapeHTML(A)
      close(con)
      B <- readLines(con <- file(File2, "r", encoding = "UTF-8"))
      B <- escapeHTML(B)
      close(con)

      max_nchar <- max(nchar(length(A)), nchar(length(B))) + 1

      out <- '<div>'
      # Run through all lines and compare them
      # If there's a difference:
      # - show file1 in red until there are no more differences
      # - show file 2 in green until there are no more differences

      printed_A <- integer(0)
      printed_B <- integer(0)

      linenum_A <- 1
      linenum_B <- 1

      add <- 0
      del <- 0

      while (linenum_A <= length(A) | linenum_B <= length(B)) {

        # print(paste(linenum_A, '|', linenum_B))
        # print(paste('printing A:', A[linenum_A]))
        # print(paste('printing B:', B[linenum_B]))

        if (is.na(A[linenum_A])) {
          # we're thru with A, print rest of B and break
          for (j in linenum_B:length(B)) {
            out <- paste0(out, printline(linenumB = j, linetext = B[j],
                                         printed_A = printed_A, printed_B = printed_B))
            printed_B <- c(printed_B, linenum_B)
            add <- add + 1
            break
          }
        } else if (is.na(B[linenum_B])) {
          # we're thru with B, print rest of A and break
          for (j in linenum_A:length(A)) {
            out <- paste0(out, printline(linenumA = j, linetext = A[j],
                                         printed_A = printed_A, printed_B = printed_B))
            printed_A <- c(printed_A, linenum_A)
            del <- del + 1
            break
          }

        } else if (A[linenum_A] == B[linenum_B]) {
          # same, just print
          out <- paste0(out, printline(linenumA = linenum_A, linenumB = linenum_B, linetext = A[linenum_A],
                                       printed_A = printed_A, printed_B = printed_B))
          printed_A <- c(printed_A, linenum_A)
          printed_B <- c(printed_B, linenum_B)

        } else {
          # differs, print A until B has a same line
          counter <- 0
          counter2 <- 0
          for (j in linenum_A:length(A)) {
            counter <- counter + 1
            #print(paste('   DIFFERS, counter is now', counter, 'and A is now', linenum_A, 'and B is now', linenum_B))
            out <- paste0(out, printline(linenumA = j, linetext = A[j],
                                         printed_A = printed_A, printed_B = printed_B))
            printed_A <- c(printed_A, linenum_A)

            linenum_A <- linenum_A + 1
            del <- del + 1
            if (A[linenum_A + 1] == B[linenum_B + 1]) {
              offset_B <- 1
            } else {
              offset_B <- 0
            }
            if (A[linenum_A] %in% B[(linenum_B + offset_B):length(B)]) {
              # next line found in B, so set A to that point and print B until here
              for (k in linenum_B:linenum_A) {
                # print B until A has a same line
                counter2 <- counter2 + 1
                out <- paste0(out, printline(linenumB = k, linetext = B[k],
                                             printed_A = printed_A, printed_B = printed_B))
                printed_B <- c(printed_B, linenum_B)
                add <- add + 1
                if (B[k + counter2] %in% A[linenum_A:length(A)]) {
                  linenum_B <- linenum_B + counter2
                  # print(paste("counter2:", counter2))
                  break
                }
              }
              break
            }
          }
          linenum_A <- linenum_A - 1
          linenum_B <- linenum_B - 1
          # print(paste('   CHANGED, counter is now', counter, 'and A is now', linenum_A, 'and B is now', linenum_B))
        }

        #out <- paste0(out, '<br>')

        linenum_A <- linenum_A + 1
        linenum_B <- linenum_B + 1
      }

      out <- paste0(out, "</div>")

      # add header with additions and deletions
      out <- paste('<div class="summary"><b>',
                   del, '</b>deletions and<b>',
                   add, '</b>additions.',
                   '</div>',
                   out)
      out
    })
  }

  shinyApp(ui, server)
}

#' @rdname textcompare
#' @importFrom utils choose.files installed.packages
#' @export
textcompare_active <- function(file2 = NULL,
                               file1_backgroundcolor = rgb(253, 184, 192, maxColorValue = 255),
                               file1_foregroundcolor = "black",
                               file2_backgroundcolor = rgb(172, 242, 189, maxColorValue = 255),
                               file2_foregroundcolor = "black",
                               title = "Shiny TextCompare") {
  if (!"rstudioapi" %in% utils::installed.packages()) {
    stop("this function only works in RStudio")
  }

  filename <- function(file) {
    rev(unlist(strsplit(gsub("/", "\\", file, fixed = TRUE), "\\", fixed = TRUE)))[1]
  }

  file1 <- rstudioapi::getSourceEditorContext()$path

  file2 <- utils::choose.files(default = file1,
                               caption = paste("Compare", filename(file1), "with..."),
                               multi = FALSE)
  if (length(file2) == 0) {
    file2 <- NULL
  }

  textcompare(file1 = file1,
              file2 = file2,
              file1_backgroundcolor = file1_backgroundcolor,
              file1_foregroundcolor = file1_foregroundcolor,
              file2_backgroundcolor = file2_backgroundcolor,
              file2_foregroundcolor = file2_foregroundcolor,
              title = title)
}

