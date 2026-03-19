#' Dump a database (i.e. list of tables) into a named folder as .csv files
#'
#' @param db List of data.tables as created by one of the rattusPull functions
#' @param name Name for folder to write to. If NULL, system date will be used.
#' @return nothing - but writes folder to disk
#'
#' @export

rattusDump <- function(db, name = NULL) {

      # Set up folder if it doesn't already exist
      mainDir <- getwd()
      if(is.null(name)) {name = as.character(Sys.Date())}
      if (file.exists(name)){
            setwd(file.path(mainDir, name))
      } else {
            dir.create(file.path(mainDir, name))
            setwd(file.path(mainDir, name))
      }

      # Loop through db and save each item with it's own name
      tableNames <- names(db)
      for(i in 1:length(tableNames)) {
            write.table(db[[tableNames[i]]], file = paste0(tableNames[i], ".csv"), row.names = F )
      }

      # Reset working directory
      setwd('..')

}
