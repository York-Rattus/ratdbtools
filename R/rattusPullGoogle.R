#' Pull in data from the RATTUS Google Sheets-format database.
#'
#' @param litMod URI for the Google sheet containing the site and context tables.
#' @param specMod URI  for the Google sheet containing the specimen and sample tables.
#' @return a list of (currently) 23 data.tables (nb. doesn't include documentary source tables)
#'
#' @export

rattusPullGoogle <- function(litMod = "https://docs.google.com/spreadsheets/d/1JQIFVbUV3_rRwuOtGL0qbJpR-7EvkFwaBsThC7Zg9Po/edit#gid=0",
                       specMod = "https://docs.google.com/spreadsheets/d/1TtTrst6ps121h7BNZojJ7R1WzGEeF7jMhoeWzdrH7Mk/edit#gid=0") {

      # Load required packages
      library(data.table)
      library(googlesheets4)

      # Read in site & lit module tables (n=14)
      SITE <- data.table(read_sheet(litMod, "SITE", col_types = "iccc-ccnnccc"))
      PHASE <- data.table(read_sheet(litMod, "PHASE", col_types = "-ic--icccccccciiccclccc"))
      CONTEXT <- data.table(read_sheet(litMod, "CONTEXT", col_types = "i-ii--ccccccccccccclc"))
      INDIVIDUAL <- data.table(read_sheet(litMod, "INDIVIDUAL", col_types = "i-ii--ccccccccc"))
      ASSEMBLAGE <- data.table(read_sheet(litMod, "ASSEMBLAGE", col_types = "iiii--ccccclcc"))
      TAXA <- data.table(read_sheet(litMod, "TAXA", col_types = "ii-cliilccl-i---"))
      taxon <- data.table(read_sheet(litMod, "taxon", col_types = "iccccll"))
      site_cat_1 <- data.table(read_sheet(litMod, "site_cat_1"))
      site_cat_2 <- data.table(read_sheet(litMod, "site_cat_2"))
      site_entry_type <- data.table(read_sheet(litMod, "site_entry_type"))
      phase_entry_type <- data.table(read_sheet(litMod, "phase_entry_type"))
      individual_type <- data.table(read_sheet(litMod, "individual_type"))
      assemblage_entry_type <- data.table(read_sheet(litMod, "assemblage_entry_type"))
      tags <- data.table(read_sheet(litMod, "tags"))

      # Read in data from sample database (n=10)
      SPECIMEN <- data.table(read_sheet(specMod, range = "SPECIMEN!A2:BA",
                                        col_types = "iDcDDccccccccccc-i-i-i---i---cccccclc----ccicccccccnc"))
      SAMPLE <- data.table(read_sheet(specMod, "SAMPLE", col_types = "i-nDcnnccDDc"))
      METRICS <- data.table(read_sheet(specMod, "METRICS", col_types = "ii-cnccc"))
      RADIOCARBON <- data.table(read_sheet(specMod, "RADIOCARBON", col_types = "iin-------Dccnccnnnn--cccnnnnncl"))
      ISOTOPES <- data.table(read_sheet(specMod, "ISOTOPES", col_types = "iin------ccnnnnnnninnccc"))
      ZOOMS <- data.table(read_sheet(specMod, "ZOOMS",
                                     col_types = "ii-ncnnnnnnnnnnnnnncnDiccc"))
      DNA <- data.table(read_sheet(specMod, range = "DNA!A2:Z",
                                   col_types = "iin--cDnccccnnnllnnnnncncc"))
      MEDIA <- data.table(read_sheet(specMod, "MEDIA", col_types = "iinccccc"))
      measurements <- data.table(read_sheet(specMod, "measurements", col_types = "cccc"))
      element <- data.table(read_sheet(specMod, "element"))

      # Replace lookup values with indices
      # SPECIMEN TABLE
      specColNames <- copy(colnames(SPECIMEN))

      setnames(SPECIMEN, "SPECIES_SOURCE", "TAXON")
      SPECIMEN <- merge(SPECIMEN, taxon[, list(TAXON, TAXON_ID)], by = "TAXON", all.x = T, all.y = F)
      setnames(SPECIMEN, "TAXON_ID", "SPECIES_SOURCE")
      SPECIMEN[, TAXON := NULL]

      setnames(SPECIMEN, "SPECIES_MORPH", "TAXON")
      SPECIMEN <- merge(SPECIMEN, taxon[, list(TAXON, TAXON_ID)], by = "TAXON", all.x = T, all.y = F)
      setnames(SPECIMEN, "TAXON_ID", "SPECIES_MORPH")
      SPECIMEN[, TAXON := NULL]

      SPECIMEN <- merge(SPECIMEN, element[, list(ELEMENT, ELEMENT_ID)], by = "ELEMENT", all.x = T, all.y = F)
      SPECIMEN[, ELEMENT := NULL]
      setnames(SPECIMEN, "ELEMENT_ID", "ELEMENT")

      setcolorder(SPECIMEN, specColNames)

      # TAXA table
      taxaColNames <- copy(colnames(TAXA))
      TAXA <- merge(TAXA, taxon[, list(TAXON_ID, TAXON)], by = "TAXON", all.x = T, all.y = F)
      TAXA[, TAXON := NULL]
      setnames(TAXA, "TAXON_ID", "TAXON")
      setcolorder(TAXA, taxaColNames)

      # ASSEMBLAGE table
      assemColNames <- copy(colnames(ASSEMBLAGE))
      ASSEMBLAGE <- merge(ASSEMBLAGE, assemblage_entry_type, by = "ASSEMBLAGE_ENTRY_TYPE", all.x = T, all.y = F)
      ASSEMBLAGE[, ASSEMBLAGE_ENTRY_TYPE := NULL]
      setnames(ASSEMBLAGE, "ASSEMBLAGE_ENTRY_TYPE_ID", "ASSEMBLAGE_ENTRY_TYPE")
      setcolorder(ASSEMBLAGE, assemColNames)

      # PHASE table
      phaseColNames <- copy(colnames(PHASE))
      PHASE <- merge(PHASE, site_cat_1, by = "SITE_CAT_1", all.x = T, all.y = F)
      PHASE <- merge(PHASE, site_cat_2, by = "SITE_CAT_2", all.x = T, all.y = F)
      PHASE <- merge(PHASE, phase_entry_type, by = "PHASE_ENTRY_TYPE", all.x = T, all.y = F)
      PHASE[, c("SITE_CAT_1", "SITE_CAT_2", "PHASE_ENTRY_TYPE") := NULL]
      setnames(PHASE, c("SITE_CAT_1_ID", "SITE_CAT_2_ID", "PHASE_ENTRY_TYPE_ID"),
               c("SITE_CAT_1", "SITE_CAT_2", "PHASE_ENTRY_TYPE"))
      setcolorder(PHASE, phaseColNames)

      # SITE table
      siteColNames <- copy(colnames(SITE))
      SITE <- merge(SITE, site_entry_type, by = "SITE_ENTRY_TYPE", all.x = T, all.y = F)
      SITE[, "SITE_ENTRY_TYPE" := NULL]
      setnames(SITE, "SITE_ENTRY_TYPE_ID", "SITE_ENTRY_TYPE")
      setcolorder(SITE, siteColNames)

      # INDIVIDUAL table
      indivColNames <- copy(colnames(INDIVIDUAL))
      INDIVIDUAL <- merge(INDIVIDUAL, individual_type, by = "INDIVIDUAL_TYPE", all.x = T, all.y = F)
      INDIVIDUAL[, "INDIVIDUAL_TYPE" := NULL]
      setnames(INDIVIDUAL, "INDIVIDUAL_TYPE_ID", "INDIVIDUAL_TYPE")
      setcolorder(INDIVIDUAL, indivColNames)

      # DNA table
      dnaColNames <- copy(colnames(DNA))
      setnames(DNA, "SPECIES_DNA", "TAXON")
      DNA <- merge(DNA, taxon[, list(TAXON, TAXON_ID)], by = "TAXON", all.x = T, all.y = F)
      setnames(DNA, "TAXON_ID", "SPECIES_DNA")
      DNA[, TAXON := NULL]
      setcolorder(DNA, dnaColNames)

      # ZOOMS table
      zoomsColNames <- copy(colnames(ZOOMS))
      setnames(ZOOMS, "SPECIES_ZOOMS", "TAXON")
      ZOOMS <- merge(ZOOMS, taxon[, list(TAXON, TAXON_ID)], by = "TAXON", all.x = T, all.y = F)
      setnames(ZOOMS, "TAXON_ID", "SPECIES_ZOOMS")
      ZOOMS[, TAXON := NULL]
      setcolorder(ZOOMS, zoomsColNames)

      # Generate PHASE_TAGS table
      PHASE_TAGS <- data.table(PHASE_ID = PHASE$PHASE_ID, tag_master = PHASE$phase_tags)
      PHASE_TAGS <- PHASE_TAGS[!is.na(tag_master)]

      # find max number of tags (nb. no comma will show as 1 rather than 0 here)
      PHASE_TAGS[, nCommas := length(unlist(gregexpr(",", tag_master))), by = 1:nrow(PHASE_TAGS)]
      iterations <- max(PHASE_TAGS$nCommas)

      # Run through and separate tags into new columns
      for(i in 1:iterations) {
            # Find position of first comma
            PHASE_TAGS[, comma_posn := unlist(gregexpr(",", tag_master))[1], by = 1:nrow(PHASE_TAGS)]
            # Where there is a comma, move everything left of it to new column
            PHASE_TAGS[comma_posn > 0, (paste0("tag", i)) := substr(tag_master, 1, comma_posn - 1)]
            # Delete everything upto and including the comma
            PHASE_TAGS[comma_posn > 0, tag_master := substr(tag_master, comma_posn +1 , nchar(tag_master))]
      }

      # Melt to long format
      PHASE_TAGS[, c("nCommas", "comma_posn") := NULL]
      PHASE_TAGS <- melt(PHASE_TAGS, id.vars = "PHASE_ID", value.name = "TAG", na.rm = T)
      PHASE_TAGS[, variable := NULL]

      # Replace TAG with TAG_ID
      PHASE_TAGS[, TAG := trimws(TAG, "both")]  # strip whitespace
      PHASE_TAGS <- merge(PHASE_TAGS, tags[, list(TAG_ID, TAG)], by = "TAG", all.x = T, all.y = F)
      PHASE_TAGS[, "TAG" := NULL]
      PHASE_TAGS <- PHASE_TAGS[order(PHASE_ID)]

      # Create ID field for PHASE_TAGS
      PHASE_TAGS[, PHASE_TAGS_ID := 1:nrow(PHASE_TAGS)]
      setcolorder(PHASE_TAGS, c("PHASE_TAGS_ID", "PHASE_ID", "TAG_ID"))

      # Drop tags from PHASE table
      PHASE[, phase_tags := NULL]

      # Combine tables into list
      dataBase <- list(taxon = taxon, element = element, measurements = measurements,
                       site_cat_1 = site_cat_1, site_cat_2 = site_cat_2, tags = tags,
                       assemblage_entry_type = assemblage_entry_type, individual_type = individual_type,
                       phase_entry_type = phase_entry_type, site_entry_type = site_entry_type,
                       SITE = SITE, PHASE = PHASE, CONTEXT = CONTEXT, INDIVIDUAL = INDIVIDUAL,
                       SPECIMEN = SPECIMEN, SAMPLE = SAMPLE,
                       METRICS = METRICS, MEDIA = MEDIA, ISOTOPES = ISOTOPES,
                       RADIOCARBON = RADIOCARBON, DNA = DNA, ZOOMS = ZOOMS,
                       ASSEMBLAGE = ASSEMBLAGE, TAXA = TAXA, PHASE_TAGS = PHASE_TAGS)

      # Loop through list and remove blank rows from each table. Also sort them by ID field
      for(i in 1:length(dataBase)) {
            dataBase[[i]] <- dataBase[[i]][rowSums(is.na(dataBase[[i]])) != ncol(dataBase[[i]])]
            idCol <- colnames(dataBase[[i]])[1]
            dataBase[[i]] <- dataBase[[i]][order(get(idCol))]
            dataBase[[i]] <- dataBase[[i]][!is.na(get(idCol))]
      }

      # Return result
      dataBase
}
