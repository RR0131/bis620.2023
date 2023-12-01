library(dplyr)
library(duckdb)
library(DBI)
library(DT)
library(ggplot2)
library(tidyr)
library(purrr)

# Connect to the database and get "studies", "sponsors","conditions" and "countries" tables.

con = dbConnect(
  duckdb(
    file.path("..","..","..","..", "0927", "ctgov.duckdb"),
    read_only = TRUE
  )
)

if (!exists("con")) {
  con = dbConnect(
    duckdb(
      file.path("..","..","..","..","0927", "ctgov.duckdb"),
      read_only = TRUE
    )
  )
  if (length(dbListTables(con)) != 50) {
    stop("Problem reading from connection.")
  }
}

  studies = tbl(con, "studies")
  sponsors = tbl(con, "sponsors")
  conditions = tbl(con, "conditions")
  countries = tbl(con, 'countries')
  dblist <- dbListTables(con)



#' @title Query keywords from a database table.
#' @description Description goes here.
#' @param d the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords
#' (intersection) or any of the keywords (union)? (default FALSE; union).
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query))
}

# Problem 1: Fix the phase histogram so that the x-axis values are uniform regardless of the query
# Create a histogram of the phases returned by a brief title keyword search
# @param d the database table.
# @param brief_title_kw the brief title keywords to look for. This is optional.
plot_phase_histogram = function(x) {
  x$phase[is.na(x$phase)] = "NA"

  phase_levels <- c('Early Phase 1', 'Phase 1', 'Phase 1/Phase 2', 'Phase 2', 'Phase 2/Phase 3', 'Phase 3', 'Phase 4', 'Not Applicable', 'NA')
  x$phase <- factor(x$phase, levels = phase_levels)

  x = x |>
    select(phase) |>
    group_by(phase) |>
    summarize(n = n())|>
    complete(phase, fill = list(n = 0))

  ggplot(x, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")
}

# Problem 2: Add a new tab that gives a histogram showing the conditions that trials in a query are examining.
## Create a histogram of the conditions returned by a brief title keyword search
## @param d the database table.
## @param brief_title_kw the brief title keywords to look for. This is optional.
 plot_condition_histogram = function(x, bins_by_input) {
  x$phase[is.na(x$phase)] = "NA"
  level = c('Early Phase 1','Phase 1', 'Phase 1/Phase 2', 'Phase 2', 'Phase 2/Phase 3', 'Phase 3', 'Phase 4','Not Applicable', "NA")
  x$phase = factor(x$phase,levels = level)
  trail_sel <-  x |> select(nct_id) |> distinct() |> pull(nct_id)
  x = conditions |>
    filter(nct_id %in% trail_sel) |>
   select(name) |>
   group_by(name) |>
    summarize(n = n()) |>
   arrange(desc(n)) |> collect()

  #Define the default bin number be 10
  if (is.na(as.integer(bins_by_input))) {
    bins_by_input <- 10
  } else {
    bins_by_input <- as.integer(bins_by_input)
  }

  final_length <- bins_by_input
  max_length <- nrow(x)
  if (final_length >= max_length) {
    final_length <- max_length
  }

  x <- head(x, final_length)

  ggplot(x, aes(x = reorder(name, -n), y = n)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    xlab("Condition") +
    ylab("Count")
}


#' Get the number of concurrent trials for each date in a set of studies
#' @param d the studies to get the number of concurrent trials for.
#' @return A tibble with a `date` column and a `count` of the number of
#' concurrent trials at that date.
get_concurrent_trials = function(d) {
  # Get all of the unique dates.
  num_dates = d |>
    pivot_longer(cols = everything()) |>
    select(-name) |>
    distinct() |>
    arrange(value) |>
    na.omit() |>
    rename(date = value)

  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }

  # capture the number of trials on each date
  num_dates$count =
    map_dbl(
      num_dates$date,
     ~ .x |>
        within_date(d$start_date, d$completion_date) |>
        sum(na.rm = TRUE)
    )

  return(num_dates)
}


plot_concurrent_studies = function(studies) {
  plot(mtcars$mpg, mtcars$cyl)
}
