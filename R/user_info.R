#' Construct User information object.
#' 
#' `new_user_info` return an object that contains WizNote account
#'  information.
#' 
#' @param user_name A string, the email account of WizNote
#' @param data_location A string, where the documents of daily recods located.
#' @param wiznote_home The home directory of WizNote program.
#' 
#' @return An `user_info`` S3 object.
new_user_info <- function(user_name, data_location, wiznote_home) {
  user <- structure(list(
    user_name = user_name,
    wiznote_home = wiznote_home,
    data_location = data_location,
    index_db = file.path(wiznote_home, user_name, "data", "index.db"),
    notes_folder = file.path(wiznote_home, user_name, "data", "notes")
  ), class = "user_info")
  
  user
}

#' Get local user list
#' 
#' @param wiznote_home The home directory of WizNote program.
#' 
#' @return A list
#' 
#' @importFrom DBI dbConnect dbSendQuery dbFetch dbClearResult dbDisconnect
#' @importFrom RSQLite SQLite
get_local_users <- function(wiznote_home) {
  user_list <- list()
  sub_dirs <- list.dirs(wiznote_home, recursive = F)
  
  i = 1L
  for (subdir in  sub_dirs) {
    db_file = file.path(subdir, "data", "index.db")
    if (file.exists(db_file)) {
      con <- dbConnect(SQLite(), db_file)
      # Get user id and user guid
      res <- dbSendQuery(con,
                         "select META_KEY,META_VALUE from WIZ_META 
         where META_NAME == 'ACCOUNT' and 
         (META_KEY == 'GUID' or META_KEY == 'USERID')"
      )
      x <- dbFetch(res)
      
      user_list[[i]] <- list(
        USERID = x[x$META_KEY == "USERID", 2],
        GUID = x[x$META_KEY == "GUID", 2]
      )
      i = i + 1L
      
      dbClearResult(res)
      dbDisconnect(con)
    }
  }
  
  user_list
}

validate_user_info <- function(user) {
  
}

#' Helper to construct user info object.
#'
#' @param user_name A string, the email account of WizNote
#' @param data_location A string, where the documents of daily recods located.
#' 
#' @return An user information S3 object.
#' 
#' @importFrom ini read.ini
#' 
#' @export
user_info <- function(user_name = "", data_location = "/My Notes/") {
  if (.Platform$OS.type == "windows") {
    wiznote_home = file.path(dirname(path.expand("~")), "WizNote")
  } else {
    wiznote_home = file.path(path.expand("~"), ".wiznote")
  }

  # Select a default user
  if (user_name == "") {
    wiz_setting <- read.ini(file.path(wiznote_home, "wiznote.ini"))
    default_guid <- wiz_setting$Users$defaultuserguid
    user_list <- get_local_users(wiznote_home)
    
    if (!is.null(default_guid)) {
      for (user in user_list) {
        if (default_guid == user$GUID) {
          user_name = user$USERID
          cat("Use default account: ", user_name, "\n")
        }
      }
    }
  }
  
  if (user_name == "") {
    stop("Can not find a default user.")
  }
  
  user <- new_user_info(user_name, data_location, wiznote_home)
  user
}
