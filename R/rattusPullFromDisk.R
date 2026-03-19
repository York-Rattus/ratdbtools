#' Read a previously saved copy of the RATTUS database from disk.
#' NEED TO ADD IN NEW LOOK-UP TABLES!
#'
#' @param path Path to folder containing tables to read.
#' @return a list of (currently) 19 data.tables.
#'
#' @export

rattusPullFromDisk <- function(path) {

      tableNames <- c("taxon", "element", "measurements",
                      "site_cat_1", "site_cat_2", "tags",
                      "assemblage_entry_type", "individual_type",
                      "phase_entry_type", "site_entry_type",
                      "SITE", "PHASE", "CONTEXT", "INDIVIDUAL",
                      "SPECIMEN", "SAMPLE",
                      "METRICS", "MEDIA", "ISOTOPES",
                      "RADIOCARBON", "DNA", "ZOOMS",
                      "ASSEMBLAGE", "TAXA", "PHASE_TAGS")

      # Loop through tables and import each
      for(i in 1:length(tableNames)) {
            assign(tableNames[i], read.table(file = file.path(path, paste0(tableNames[i], ".csv")), header = T))
      }

      # Put all the tables into a list
      dataBase <- list(taxon = taxon, element = element, measurements = measurements,
                       site_cat_1 = site_cat_1, site_cat_2 = site_cat_2, tags = tags,
                       assemblage_entry_type = assemblage_entry_type, indivudal_type = individual_type,
                       phase_entry_type = phase_entry_type, site_entry_type = site_entry_type,
                       SITE = SITE, PHASE = PHASE, CONTEXT = CONTEXT, INDIVIDUAL = INDIVIDUAL,
                       SPECIMEN = SPECIMEN, SAMPLE = SAMPLE,
                       METRICS = METRICS, MEDIA = MEDIA, ISOTOPES = ISOTOPES,
                       RADIOCARBON = RADIOCARBON, DNA = DNA, ZOOMS = ZOOMS,
                       ASSEMBLAGE = ASSEMBLAGE, TAXA = TAXA, PHASE_TAGS = PHASE_TAGS)

      # Return it
      dataBase

}
