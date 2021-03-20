#' Extract records from given page document
#' 
#' `query_records` will produce tidy data from WizNote database.
#' 
#' @param user_info An user information S3 object.
#' @param date Any input for `lubridate::ymd` function. One page of time ledge
#'  per day.
#' 
#' @return A tidy tibble that contains event time records of your each day.
#' 
#' @importFrom lubridate ymd ymd_hm
#' @importFrom magrittr %>%
#' @importFrom purrr map pmap reduce
#' @importFrom dplyr mutate bind_rows
#' 
#' @export
query_records <- function(user_info, start, end) {
  # Format inputs
  start_day <- ymd(start)
  end_day <- ymd(end)
  # Query meta information
  data_info_table <- query_page_metainfo(user_info, start_day, end_day)
  #TODO: handle empty data_info_table
  #TODO: Parse table from each document
  data_info_table %>%
    pmap(function(DOCUMENT_GUID, DOCUMENT_TITLE, DOCUMENT_LOCATION) {
      read_table_from_doc(user_info, DOCUMENT_GUID) %>%
        mutate(Date = ymd(DOCUMENT_TITLE), GUID = DOCUMENT_GUID)
    }) %>% map(tidy_records) %>% reduce(bind_rows)
}

retrieve_records <- function(obj_app, data_location, start, end, callback) {
  start_date <- lubridate::ymd(start)
  end_date <- lubridate::ymd(end)

  sql <- sprintf(
    "DOCUMENT_LOCATION like '%s%%'
      and DOCUMENT_TITLE like '____-__-__'
      and DOCUMENT_TITLE >= '%s' and DOCUMENT_TITLE <= '%s'",
    data_location, start_date, end_date)
  sql <- iconv(sql, to="utf-8")

  obj_db <- obj_app$Database
  obj_db$DocumentsFromSQLWhere(sql, function(doc_list) {
    fullfill <- logical(0)
    documents <- list()
    lapply(doc_list, function(doc) {
      obj_app$DatabaseManager$CheckDocumentData(doc, function(ret) {
        fullfill <<- c(fullfill, ret)
        if (length(fullfill) == length(doc_list)) {
          if (all(fullfill)) {
            data_info_table <- doc_list %>%
              purrr::map(function(doc) {
                doc$FileName %>%
                  read_table_from_doc_file() %>%
                  dplyr::mutate(Date = lubridate::ymd(doc$Title), GUID = doc$GUID)
              }) %>% purrr::map(tidy_records) %>% purrr::reduce(bind_rows)

              callback(data_info_table)
          } else {
            stop("Document download failed.")
          }
        }
      })
    })
  })
}

#' Find document meta information from WizNote database
#' 
#' `query_page_metainfo` will extract document meta information from WizNote
#' database with the given start to end date.
#' 
#' @param user_info An user information S3 object.
#' @param start,end Any input for `lubridate::ymd` function.
#' 
#' @return A tibble that contains meta informatin of WizNote document
#' 
#' @importFrom DBI dbConnect dbSendQuery dbFetch dbClearResult dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom lubridate ymd
#' @importFrom tidyr as_tibble
query_page_metainfo <- function(user_info, start, end) {
  start_date <- ymd(start)
  end_date <- ymd(end)
  
  # Connect to index.db
  con <- dbConnect(SQLite(), user_info$index_db)
  # Fetch document meta information
  res <- dbSendQuery(con, sprintf(
    "select DOCUMENT_GUID,DOCUMENT_TITLE,DOCUMENT_LOCATION from WIZ_DOCUMENT
      where DOCUMENT_LOCATION like '%s%%'
      and DOCUMENT_TITLE like '____-__-__'
      and DOCUMENT_TITLE >= '%s' and DOCUMENT_TITLE <= '%s'", 
    user_info$data_location, start_date, end_date))
  x <- dbFetch(res) %>% as_tibble()
  
  # Close connection
  dbClearResult(res)
  dbDisconnect(con)
  
  x
}

#' Get html text from WizNotePlus document zipfile
#' 
#' @param filename WizNote document zipfile name
#'
#' @return string. The html text of document zipfile.
#' 
#' @importFrom readr read_file
read_html_from_doc <- function(filename) {
  conn <- unz(filename, "index.html")
  read_file(conn)
}

#' Extract table from html text
#' 
#' @param user_info An user information S3 object.
#' @param doc_guid A string. The GUID of WizNote document.
#' 
#' @return A tibble that contains event time records.
#' 
#' @importFrom tidyr tibble as_tibble
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
read_table_from_doc <- function(user_info, doc_guid) {
  if (length(doc_guid) != 1 || doc_guid == "") {
    return(tibble())
  }
  html_text <- read_html_from_doc(
    file.path(user_info$notes_folder, sprintf("{%s}", doc_guid)))
  
  t <- xml2::read_html(html_text) %>% 
    html_node("div.wiz-table-container div.wiz-table-body table") %>%
    html_table_to_dataframe(header = T) %>% as_tibble()
  
  t
}

read_table_from_doc_file <- function(filename) {
  html_text <- read_html_from_doc(filename)

  t <- xml2::read_html(html_text) %>% 
    rvest::html_node("div.wiz-table-container div.wiz-table-body table") %>%
    html_table_to_dataframe(header = T) %>% tibble::as_tibble()
  
  t
}

#' Convert html table element to data.frame
#' 
#' @param x A node, node set or document.
#' @param header Use first row as header? If `NA`, will use first row
#'   if it consists of `<th>` tags.
#' @param trim Remove leading and trailing whitespace within each cell?
#' @param fill If `TRUE`, automatically fill rows with fewer than
#'   the maximum number of columns with `NA`s.
#' @param dec The character used as decimal mark.
#' 
#' @return A data.frame
#' 
#' @importFrom rvest html_name html_nodes html_node html_attr html_text
html_table_to_dataframe <- function(x, header = NA, trim = TRUE,
                                    fill = FALSE, dec = ".") {
  
  stopifnot(html_name(x) == "table")
  
  #browser()
  # Throw error if any rowspan/colspan present
  rows <- html_nodes(x, "tr")
  # Number of tr element or number of rows
  n <- length(rows)
  # A list in which each element contains cells of one row
  cells <- lapply(rows, "html_nodes", xpath = ".//td|.//th")
  
  # A list in which each element represent one row and each element is a numeric vector
  ncols <- lapply(cells, html_attr, "colspan", default = "1")
  ncols <- lapply(ncols, as.integer)
  # A list in which each element represent one row and each element is numeric vector
  nrows <- lapply(cells, html_attr, "rowspan", default = "1")
  nrows <- lapply(nrows, as.integer)
  
  # Calculate max number of cols in each row
  p <- unique(vapply(ncols, sum, integer(1)))
  maxp <- max(p)
  
  if (length(p) > 1 & maxp * n != sum(unlist(nrows)) &
      maxp * n != sum(unlist(ncols))) {
    # then malformed table is not parsable by smart filling solution
    if (!fill) { # fill must then be specified to allow filling with NAs
      stop("Table has inconsistent number of columns. ",
           "Do you want fill = TRUE?", call. = FALSE)
    }
  }
  
  values <- lapply(cells, html_text, trim = trim)
  # Construct table matrix
  out <- matrix(NA_character_, nrow = n, ncol = maxp)
  
  # fill colspans right with repetition
  for (i in seq_len(n)) {
    # a vector which contains values of one row
    row <- values[[i]]
    # a vector which contains colspan numbers of one row
    ncol <- ncols[[i]]
    # used to record current col location
    col <- 1
    # fill values into each cell of col
    for (j in seq_len(length(ncol))) {
      # `col:(col+ncol[j]-1)` will duplicate cells, and fill the same value
      out[i, col:(col+ncol[j]-1)] <- row[[j]]
      # Record current col location
      col <- col + ncol[j]
    }
  }
  
  # fill rowspans down with repetition
  # 变量i控制要span位于第几列的格子 
  for (i in seq_len(maxp)) {
    # loop all tr element
    # 变量j控制第几行的格子
    for (j in seq_len(n)) {
      # These value may be NA
      rowspan <- nrows[[j]][i]
      colspan <- ncols[[j]][i]
      if (!is.na(rowspan) & (rowspan > 1)) {
        # Duplicate cols
        if (!is.na(colspan) & (colspan > 1)) {
          # special case of colspan and rowspan in same cell
          nrows[[j]] <- c(
            utils::head(nrows[[j]], i),
            rep(rowspan, colspan-1),
            utils::tail(nrows[[j]], length(rowspan)-(i+1))
          )
          rowspan <- nrows[[j]][i]
        }
        # Duplicate rows
        # k 是要span的多少行数，j+k第几行要准备span
        for (k in seq_len(rowspan - 1)) {
          # 复制向量左边部分
          l <- utils::head(out[j+k, ], i-1)
          # 复制向量右边部分
          r <- utils::tail(out[j+k, ], maxp-i+1)
          # 将要复制的元素out[j,i]，出入到中间
          out[j + k, ] <- utils::head(c(l, out[j, i], r), maxp)
          # Apply path from https://github.com/tidyverse/rvest/pull/236
          nrows[[j+k]] <- c(1, nrows[[j+k]])
        }
      }
    }
  }
  
  if (is.na(header)) {
    header <- all(html_name(cells[[1]]) == "th")
  }
  if (header) {
    col_names <- out[1, , drop = FALSE]
    out <- out[-1, , drop = FALSE]
  } else {
    col_names <- paste0("X", seq_len(ncol(out)))
  }
  
  # Convert matrix to list to data frame
  df <- lapply(seq_len(maxp), function(i) {
    utils::type.convert(out[, i], as.is = TRUE, dec = dec)
  })
  names(df) <- col_names
  class(df) <- "data.frame"
  attr(df, "row.names") <- .set_row_names(length(df[[1]]))
  
  if (length(unique(col_names)) < length(col_names)) {
    warning('At least two columns have the same name')
  }
  
  df
}

#' Organize records table
#' 
#' `tidy_records` is very verbose.
#' 
#' @param records A tibble.
#' @param date Any input for `lubridate::ymd` function.
#' 
#' @return A tidy tibble.
#' 
#' @importFrom dplyr filter mutate
#' @importFrom tidyr separate_rows separate
#' @importFrom lubridate ymd ymd_hm
#' @importFrom purrr pwalk
tidy_records <- function(records) {
  # Remove empty records
  records <-  records %>% filter(is_valid(Record))
  # Seperate records to each rows
  records <- records %>% separate_rows(Record, sep = ";\\s*") %>%
    separate(Record, into = c("Start", "End"), sep = ",\\s*")
  # Notify user to check the tabble again
  records <- records %>%
    pwalk(check_record_startend) %>% filter(is_valid(Start) & is_valid(End))
  # Compute time
  records <- records %>% mutate(
    Start = ymd_hm(paste(Date, Start)),
    End = ymd_hm(paste(Date, End))) %>%
    mutate(Time = End - Start) %>%
    pwalk(check_record_startend) %>%
    filter(!is.na(Start) & !is.na(End))
  # Check time
  records %>% pwalk(check_record_time)
}

is_valid <- function(x) {
  !is.na(x) & !is.null(x) & x != ""
}

check_record_startend <- function(Class, Tag, Event, Start, End, Time, Progress, Date, GUID) {
  if (!is_valid(as.character(Start)) | !is_valid(as.character(End))) {
    cat(sprintf(
      "The record is not valid, and will be removed: \n\t%s\t%s\t%s\t%s\t%s\n",
      Date, Class, Event, Start, End))
  }
}

#' Check time field
#' 
#' @importFrom readr parse_number
check_record_time <- function(Class, Tag, Event, Start, End, Time, Progress, Date, GUID) {
  if (is_valid(as.character(Time)) & (parse_number(as.character(Time)) < 0) ) {
    cat(sprintf(
      "The time of record may be wrong: \n\t%s\t%s\t%s\t%s\t%s\t%s\n",
      Date, Class, Event, Start, End, Time))
  }
}