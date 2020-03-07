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