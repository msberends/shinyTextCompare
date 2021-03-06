% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/init.R
\name{textcompare}
\alias{textcompare}
\alias{textcompare_active}
\title{Compare text files line by line}
\usage{
textcompare(file1 = NULL, file2 = NULL, file1_backgroundcolor = rgb(253,
  184, 192, maxColorValue = 255), file1_foregroundcolor = "black",
  file2_backgroundcolor = rgb(172, 242, 189, maxColorValue = 255),
  file2_foregroundcolor = "black", title = "Shiny TextCompare")

textcompare_active(file2 = NULL, file1_backgroundcolor = rgb(253, 184, 192,
  maxColorValue = 255), file1_foregroundcolor = "black",
  file2_backgroundcolor = rgb(172, 242, 189, maxColorValue = 255),
  file2_foregroundcolor = "black", title = "Shiny TextCompare")
}
\arguments{
\item{file1, file2}{Path of files. If not specified, an input box will be shown that supports drag and drop for files.}

\item{file1_backgroundcolor, file1_foregroundcolor, file2_backgroundcolor, file2_foregroundcolor}{HTML-values to display as difference between file1 and file2. Defaults tot GitHub colours. This can be used to theme the app yourself.}

\item{title}{Title to display. This can be used to theme the app yourself.}
}
\description{
Compare text files line by line and see their differences immediately. You can use filepaths as input, or start the app and select them in the app (or use drag and drop). The function \code{textcompare_active} can be used in RStudio to use the active source document as input for \code{file1}.
}
