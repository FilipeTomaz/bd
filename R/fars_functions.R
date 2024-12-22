#' Title : Read a fars_read data file
#'
#' Reads a CSV file containing fars data, a csv file and returns a tbl_df.
#' If the file does not exist, an error is thrown.
#'
#' @param filename A character string giving the name of the file to read, the file may exist or not.
#' @return A tibble containing the data from the CSV file. If the file does not exist an error message will be printed
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export


fars_read <- function(filename) {
  if (!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create a fars filename
#'
#' Generates a filename for a specific year in the FARS dataset. This function permits to create
#' accident data files by year
#' @param year An integer or a value coercible to an integer representing the year.
#' @return A character string representing the filename.
#' @export


make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read fars data for multiple years
#'
#' Reads FARS data files for multiple years and extracts the month and year columns.
#' If an invalid year is provided, a warning is issued, and the result for that year is `NULL`.
#'
#' @param years A vector of integers or values coercible to integers representing years.
#' @return A list of tibbles containing the `MONTH` and `year` columns for each valid year.
#' @importFrom dplyr mutate select
#' @export


fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize fars data by year and month
#'
#' Summarizes the number of accidents for each year and month.
#'
#' @param years A vector of integers or values coercible to integers representing years.
#' @return A tibble summarizing the number of accidents for each month and year.
#' Columns represent years, and rows represent months.
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot accidents on a state map
#'
#' Plots the locations of accidents in a specific state and year.
#' If there are no accidents or an invalid state number is provided, an appropriate message is displayed.
#'
#' @param state.num An integer representing the state number.
#' @param year An integer or a value coercible to an integer representing the year.
#' @return A plot of accidents for the specified state and year. Returns `NULL` if no data is available.
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @export



fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if (!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if (nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
