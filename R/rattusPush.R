#' Write the contents of a copy of the database out to an existing copy as defined by a MySQLConnection.
#' This will overwrite the existing contents to use extreme caution. Best used to write to localhost,
#' after which the results can be checked before restoring the SQL file to the UoY server version.
#'
#' @param db List of tables to write, in format produced by one of the rattusPull functions.
#' @param connection MySQLConnection defining the database connection to write to.
#' @param prompt Should function prompt user before overwriting data in an existing table?
#' @param report Should function provide a running report of tables written and numbers of rows dropped?
#' @param returnDroppedRows Should function return a list of tables showing dropped rows?
#' @return if returnDroppedRows is TRUE, a list of one data.table for each table in the databases, showing dropped rows. Otherwise none.
#'
#' @export

# Define function to push a database (i.e. list of tables) to a MySQL connection
rattusPush <- function(db, connection, prompt = T, report = T, returnDroppedRows = T) {

      # Load required packages
      library(data.table)
      library(RMariaDB)

      # Get list of tables already in database, to check against
      tablesSQL <- dbListTables(connection)
      tablesLocal <- names(db)

      # Function to write a table (first truncating it)
      overWrite <- function(tableName, exists = T) {

            # First remove the existing data (need to temporarily disable foreign key checks)
            if(exists == T) {
                  dbSendQuery(connection, "SET FOREIGN_KEY_CHECKS = 0;")
                  dbSendQuery(connection, paste0("TRUNCATE ", tableName ))
                  dbSendQuery(connection, "SET FOREIGN_KEY_CHECKS = 1;")
            }

            # Now write new data
            dbWriteTable(connection, tableName, db[[tableName]], append = exists, row.names = F)
      }

      # Create an empty list to store error tables
      droppedRows <- list()

      # Loop through tables
      for(i in 1:length(db)) {

            # Set a flag for whether or not to write the table, default being true
            # Also set the default message starting text
            writeFlag <- T
            writeText <- "Overwriting table "

            # Find number of rows in local version
            rowsInLocal <- nrow(db[[tablesLocal[i]]])

            # Check if table already exists and, if so, how many records it has
            if(tablesLocal[i] %in% tablesSQL) {
                  exists <- T
                  rowsInSQL <- dbGetQuery(connection, paste0("SELECT COUNT(*) FROM ", tablesLocal[i], ";"))[1,1]

                  # If there is date to be overwritten and prompt is true, check with user
                  if(rowsInSQL > 0) {

                        # If set to prompt, do so
                        if(prompt == T) {
                              response <- readline(prompt = paste0("Are you sure you want to overwrite existing table ",
                                                                   tablesLocal[i], " (", rowsInSQL, " rows) with new version (",
                                                                   rowsInLocal, " rows)? Type Y or N (or A to overwrite all tables without further prompts)."))

                              # If response is negative or unrecognised set flag to FALSE, otherwise keep it as TRUE
                              if(!response %in% c("Y", "A", "y", "a")) {
                                    writeFlag = F
                                    writeText <- "Skipping table "
                              }

                              # Set prompt to false if requested
                              if(response %in% c("A", "a")) {prompt <- F}
                        }

                        # If table is empty, keep flag true but change message text
                  } else {
                        writeText <- "Writing to empty table "
                  }

                  # If table doesn't exist yet, keep flag true but change message text
            } else {
                  exists <- F
                  writeText <- "Writing new table "
            }

            # Report progress
            if(writeFlag == F) {
                  print(paste0(writeText, tablesLocal[i]))
            } else {
                  print(paste0(writeText, tablesLocal[i], " with ", rowsInLocal, " rows."))
            }

            # Write table if flag is true
            if(writeFlag == T) {overWrite(tablesLocal[i], exists = exists)}

            # And check if all rows copied correctly
            temp <- dbReadTable(connection, tablesLocal[i])
            rowsLocal <- db[[tablesLocal[i]]][[1]]
            rowsPushed <- temp[[1]]
            missing <- rowsLocal[!rowsLocal %in% rowsPushed]

            # Report
            if(length(missing > 0) & report == T) {
                  print(paste0("Warning: ", length(missing) ," rows failed to copy"))
            }

            # Save dropped rows
            idCol <- colnames(db[[i]])[1]
            assign(tablesLocal[i], db[[tablesLocal[i]]][get(idCol) %in% missing])
            droppedRows[[i]] <- get(tablesLocal[i])
      }

      # Return list of dropped rows, if requested
      if(returnDroppedRows == T) {
            droppedRows
      }
}
