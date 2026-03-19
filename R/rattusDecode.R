#' Decode lookup columns in all tables in an R copy of the database. Optionally, also performs merges to pull
#' in commonly-used key fields from other tables to the PHASE, SPECIMEN, and/or ASSEMBLAGE tables. This option is
#' intended as a quick-and-dirty shortcut when performing ad hoc queries - it is not recommended in serious analysis
#' scripts, for which bespoke merges between tables would be mopre appropriate
#'
#' @param db List of tables to amend, in format produced by one of the rattusPull functions.
#' @param lookups Should true lookup fields be decoded? TRUE by default; set to FALSE where already decoded.
#' @param keyfields Character vector of tables (from PHASE, SPECIMEN, ASSEMBLAGE) into which key fields should be merged.
#' @return A list of tables matching the input, but with columns merged in the ways defined by lookups and keyfields
#'
#' @export

rattusDecode <- function(db, lookups = T, keyfields = NULL) {

      ## Define function to perform each merge
      decode <- function(table, lookup, tableIDcol, lookupIDcol, contentCols, contentColsNew = NULL) {

            # Record starting column order
            colorders <- colnames(db[[table]])

            # Merge selected columns from lookup in
            db[[table]] <<- merge(db[[table]], db[[lookup]][, c(lookupIDcol, contentCols), with = F],
                                  by.x = tableIDcol, by.y = lookupIDcol, all.x = T, all.y = F)

            # Update content column names if needed
            if(!is.null(contentColsNew)) {
                  setnames(db[[table]], old = contentCols, new = contentColsNew)
                  contentCols <- contentColsNew
            }

            # Reset column order
            posn <- match(tableIDcol, colorders)  # Find position of ID field in column list
            colorders <- c(colorders[1:posn], contentCols, colorders[(posn+1):length(colorders)])
            setcolorder(db[[table]], neworder = colorders)

      }

      ## Decode true lookup columns, by default
      # (can be disabled if already done - might add a flag that records whether a db list has been decoded already)
      if(lookups == T) {

            # Apply decode function to specimen table, after setting names of ID fields to distinguish them from content fields in lookup tables
            setnames(db$SPECIMEN, skip_absent = T,
                     old = c("ELEMENT", "SPECIES_SOURCE", "SPECIES_MORPH", "SPECIES_BEST", "SPECIES_BEST_AUTO"),
                     new = c("ELEMENT_ID", "SPECIES_SOURCE_ID", "SPECIES_MORPH_ID", "SPECIES_BEST_ID", "SPECIES_BEST_AUTO_ID")) # Re-name foreign key columns to avoid duplicates after merging

            decode(table = "SPECIMEN", lookup = "element",
                   tableIDcol = "ELEMENT_ID", lookupIDcol = "ELEMENT_ID",
                   contentCols = "ELEMENT")
            decode(table = "SPECIMEN", lookup = "taxon",
                   tableIDcol = "SPECIES_SOURCE_ID", lookupIDcol = "TAXON_ID",
                   contentCols = c("TAXON", "ENGLISH"), contentColsNew = c("SPECIES_SOURCE", "ENGLISH_SOURCE"))
            decode(table = "SPECIMEN", lookup = "taxon",
                   tableIDcol = "SPECIES_MORPH_ID", lookupIDcol = "TAXON_ID",
                   contentCols = c("TAXON", "ENGLISH"), contentColsNew = c("SPECIES_MORPH", "ENGLISH_MORPH"))
            decode(table = "SPECIMEN", lookup = "taxon",
                   tableIDcol = "SPECIES_BEST_ID", lookupIDcol = "TAXON_ID",
                   contentCols = c("TAXON", "ENGLISH"), contentColsNew = c("SPECIES_BEST", "ENGLISH_BEST"))
            if("SPECIES_BEST_AUTO_ID" %in% colnames(db$SPECIMEN)) {
                  decode(table = "SPECIMEN", lookup = "taxon",
                        tableIDcol = "SPECIES_BEST_AUTO_ID", lookupIDcol = "TAXON_ID",
                        contentCols = c("TAXON", "ENGLISH"), contentColsNew = c("SPECIES_BEST_AUTO", "ENGLISH_BEST_AUTO"))
            }

            # Apply it to site table
            setnames(db$SITE, skip_absent = T, old = "SITE_ENTRY_TYPE", new = "SITE_ENTRY_TYPE_ID")
            decode(table = "SITE", lookup = "site_entry_type",
                   tableIDcol = "SITE_ENTRY_TYPE_ID", lookupIDcol = "SITE_ENTRY_TYPE_ID",
                   contentCols = "SITE_ENTRY_TYPE")

            # Apply it to phase table
            setnames(db$PHASE, skip_absent = T, old = c("PHASE_ENTRY_TYPE", "SITE_CAT_1", "SITE_CAT_2"),
                     new = c("PHASE_ENTRY_TYPE_ID", "SITE_CAT_1_ID", "SITE_CAT_2_ID"))
            decode(table = "PHASE", lookup = "phase_entry_type",
                   tableIDcol = "PHASE_ENTRY_TYPE_ID", lookupIDcol = "PHASE_ENTRY_TYPE_ID",
                   contentCols = "PHASE_ENTRY_TYPE")
            decode(table = "PHASE", lookup = "site_cat_1",
                   tableIDcol = "SITE_CAT_1_ID", lookupIDcol = "SITE_CAT_1_ID",
                   contentCols = "SITE_CAT_1")
            decode(table = "PHASE", lookup = "site_cat_2",
                   tableIDcol = "SITE_CAT_2_ID", lookupIDcol = "SITE_CAT_2_ID",
                   contentCols = "SITE_CAT_2")

            # Apply it to assemblage table
            setnames(db$ASSEMBLAGE, skip_absent = T, old = "ASSEMBLAGE_ENTRY_TYPE", new = "ASSEMBLAGE_ENTRY_TYPE_ID")
            decode(table = "ASSEMBLAGE", lookup = "assemblage_entry_type",
                   tableIDcol = "ASSEMBLAGE_ENTRY_TYPE_ID", lookupIDcol = "ASSEMBLAGE_ENTRY_TYPE_ID",
                   contentCols = "ASSEMBLAGE_ENTRY_TYPE")

            # Apply it to individual table
            setnames(db$INDIVIDUAL, skip_absent = T, old = c("INDIVIDUAL_TYPE"), new = c("INDIVIDUAL_TYPE_ID"))
            decode(table = "INDIVIDUAL", lookup = "individual_type",
                   tableIDcol = "INDIVIDUAL_TYPE_ID", lookupIDcol = "INDIVIDUAL_TYPE_ID",
                   contentCols = "INDIVIDUAL_TYPE")

            # Apply it to ZooMS table
            setnames(db$ZOOMS, skip_absent = T, old = c("SPECIES_ZOOMS"), new = c("SPECIES_ZOOMS_ID"))
            decode(table = "ZOOMS", lookup = "taxon",
                   tableIDcol = "SPECIES_ZOOMS_ID", lookupIDcol = "TAXON_ID",
                   contentCols = c("TAXON", "ENGLISH"), contentColsNew = c("SPECIES_ZOOMS", "ENGLISH_ZOOMS"))

            # Apply it to DNA table
            setnames(db$DNA, skip_absent = T, old = c("SPECIES_DNA"), new = c("SPECIES_DNA_ID"))
            decode(table = "DNA", lookup = "taxon",
                   tableIDcol = "SPECIES_DNA_ID", lookupIDcol = "TAXON_ID",
                   contentCols = c("TAXON", "ENGLISH"), contentColsNew = c("SPECIES_DNA", "ENGLISH_DNA"))

            # Apply it to TAXA table
            setnames(db$TAXA, skip_absent = T, old = c("TAXON"), new = c("TAXON_ID"))
            decode(table = "TAXA", lookup = "taxon",
                   tableIDcol = "TAXON_ID", lookupIDcol = "TAXON_ID",
                   contentCols = c("TAXON", "ENGLISH"))
      }

      ## Short-cut merges of key info from main tables, if requested
      # Pull key information on site and phase into specimen table, if requested
      if("SPECIMEN" %in% keyfields) {
            decode(table = "SPECIMEN", lookup = "SITE",
                   tableIDcol = "SITE_ID", lookupIDcol = "SITE_ID",
                   contentCols = c("SITE_NAME", "SETTLEMENT", "LATITUDE", "LONGITUDE"))
            decode(table = "SPECIMEN", lookup = "PHASE",
                   tableIDcol = "PHASE_ID", lookupIDcol = "PHASE_ID",
                   contentCols = c("PHASE_AT_SITE", "DATING", "DATE_FROM", "DATE_TO"))
            decode(table = "SPECIMEN", lookup = "CONTEXT",
                   tableIDcol = "CONTEXT_ID", lookupIDcol = "CONTEXT_ID",
                   contentCols = "CONTEXT")
      }

      # Pull key information on site into phase table, if requested
      if("PHASE" %in% keyfields) {
            decode(table = "PHASE", lookup = "SITE",
                   tableIDcol = "SITE_ID", lookupIDcol = "SITE_ID",
                   contentCols = c("SITE_NAME", "SETTLEMENT", "LATITUDE", "LONGITUDE"))
      }

      # Pull key information on site and phase into assemblage table, if requested
      if("ASSEMBLAGE" %in% keyfields) {
            decode(table = "ASSEMBLAGE", lookup = "SITE",
                   tableIDcol = "SITE_ID", lookupIDcol = "SITE_ID",
                   contentCols = c("SITE_NAME", "SETTLEMENT", "LATITUDE", "LONGITUDE"))
            decode(table = "ASSEMBLAGE", lookup = "PHASE",
                   tableIDcol = "PHASE_ID", lookupIDcol = "PHASE_ID",
                   contentCols = c("PHASE_AT_SITE", "DATING", "DATE_FROM", "DATE_TO"))
      }

      # Return results
      db
}
