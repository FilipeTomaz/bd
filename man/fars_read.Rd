% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Title : Read a fars_read data file}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string giving the name of the file to read, the file may exist or not.}
}
\value{
A tibble containing the data from the CSV file. If the file does not exist an error message will be printed
}
\description{
Reads a CSV file containing fars data, a csv file and returns a tbl_df.
If the file does not exist, an error is thrown.
}
