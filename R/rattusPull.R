#' Pull in data from the RATTUS Google Sheets-format database.
#'
#' @param connection MySQLConnection defining the database connection, or use default for rattus_read connection.
#' @param bestDate Should a set of columns be added to SPECIMEN to show the best available dating info drawn from across all tables?
#' @param bestSpecies Should a set of columns be added to SPECIMEN to show the most confident species ID available?
#' @param prepMetrics Should the fusion status of the relevant end(s) for each metric value be worked out and appended to the METRICS table?
#' @return a list of (currently) 32 data.tables
#'
#' @export

rattusPull <- function(connection, bestDate = T, bestSpecies = T, prepMetrics = F) {

      # Load required packages
      library(data.table)
      library(googlesheets4)
      library(RMariaDB)

      # Get list of tables
      tablist <- dbListTables(connection)

      # Loop through list and read each table
      for(i in 1:length(tablist)) {
            assign(tablist[i], data.table(dbReadTable(connection, tablist[i])))
      }

      # Close connection
      dbDisconnect(connection)

      # Combine tables into list
      db <- list(taxon = taxon, element = element, measurements = measurements,
                       site_cat_1 = site_cat_1, site_cat_2 = site_cat_2, tags = tags,
                       assemblage_entry_type = assemblage_entry_type, individual_type = individual_type,
                       phase_entry_type = phase_entry_type, site_entry_type = site_entry_type,
                       SITE = SITE, PHASE = PHASE, CONTEXT = CONTEXT, INDIVIDUAL = INDIVIDUAL,
                       SPECIMEN = SPECIMEN, SAMPLE = SAMPLE,
                       METRICS = METRICS, MEDIA = MEDIA, ISOTOPES = ISOTOPES,
                       RADIOCARBON = RADIOCARBON, DNA = DNA, ZOOMS = ZOOMS,
                       ASSEMBLAGE = ASSEMBLAGE, TAXA = TAXA, PHASE_TAGS = PHASE_TAGS,
                       SOURCE = SOURCE, ENTRY = ENTRY, NAME_USE = NAME_USE, TROPE_MENTION = TROPE_MENTION,
                       PLACE_MENTION = PLACE_MENTION, TROPE = TROPE, PLACE = PLACE)

      # Loop through list sort tables by ID field
      for(i in 1:length(db)) {
            idCol <- colnames(db[[i]])[1]
            db[[i]] <- db[[i]][order(get(idCol))]
            db[[i]] <- db[[i]][!is.na(get(idCol))]
      }

      ### FIND BEST DATE INFO
      ## We have the following ranks, from best to worst
      # a) known historical capture date (from INDIVIDUAL)
      # b) direct radiocarbon date (from RADIOCARBON)
      # c) indirect radiocarbon date (from RADIOCARBON)
      # d) context date (from CONTEXT)
      # e) phase date (from PHASE)

      ## First merge in all the relevant columns
      if(bestDate == T) {
            dateInfo <- merge(db$SPECIMEN[, list(SPECIMEN_ID, PHASE_ID, CONTEXT_ID, INDIVIDUAL_ID, SPECIES_BEST)],
                        db$PHASE[, list(PHASE_ID, DATING, DATE_FROM, DATE_TO, DATE_NOTES)],
                        by = "PHASE_ID", all.x = T, all.y = F)
            dateInfo <- merge(dateInfo,
                        db$CONTEXT[, list(CONTEXT_ID, CONTEXT_DATING, CONTEXT_START, CONTEXT_END, CONTEXT_NOTES)],
                        by = "CONTEXT_ID", all.x = T, all.y = F)
            dateInfo <- merge(dateInfo,
                        db$RADIOCARBON[, list(RADIOCARBON_ID, SPECIMEN_ID, RC_DATE, RC_SD, CAL_95_EARLY, CAL_95_LATE,
                                              CAL_MEDIAN, RC_LAB_REF, RC_NOTES)],
                        by = "SPECIMEN_ID", all.x = T, all.y = F)
            dateInfo <- merge(dateInfo,
                        db$INDIVIDUAL[, list(INDIVIDUAL_ID, INDIVIDUAL_TYPE, INDIV_DATE, INDIV_DATE_TYPE, INDIVIDUAL_NOTES)],
                        by = "INDIVIDUAL_ID", all.x = T, all.y = F)

            ## Now start from the worst and work up the list
            # Type (e) - do this for everything; it'll get overwritten later in many cases
            dateInfo[, DATE_BEST := DATING]
            dateInfo[, DATE_FROM_BEST := DATE_FROM]
            dateInfo[, DATE_TO_BEST := DATE_TO]
            dateInfo[, DATE_BEST_NOTES := DATE_NOTES]
            dateInfo[, DATE_BEST_TYPE := "Phase"]

            # Type (d) - we'll mainly check for the actual values, but only overwrite the description if one is actually given
            dateInfo[!is.na(CONTEXT_DATING) & !is.na(CONTEXT_START) & !is.na(CONTEXT_END), DATE_BEST := CONTEXT_DATING]
            dateInfo[!is.na(CONTEXT_START) & !is.na(CONTEXT_END), DATE_FROM_BEST := CONTEXT_START]
            dateInfo[!is.na(CONTEXT_START) & !is.na(CONTEXT_END), DATE_TO_BEST := CONTEXT_END]
            dateInfo[!is.na(CONTEXT_START) & !is.na(CONTEXT_END), DATE_BEST_TYPE := "Context"]

            # Types (b) and (c) - we'll do these together
            # This is slow code, looping throught the table, but easier than attempting to vectorise properly
            for(i in 1:nrow(dateInfo)) {
                  # Find context-mates with dates
                  contextDates <- dateInfo[CONTEXT_ID == dateInfo[i, CONTEXT_ID]]
                  contextDates <- contextDates[!is.na(CAL_95_EARLY) & SPECIES_BEST %in% 1:3]

                  # If there is exactly one match, use that
                  if(nrow(contextDates) == 1) {
                        dateInfo[i, DATE_BEST := with(contextDates, paste0(RC_LAB_REF, ": ", RC_DATE, "±", RC_SD, " BP"))]
                        dateInfo[i, DATE_FROM_BEST := contextDates$CAL_95_EARLY]
                        dateInfo[i, DATE_TO_BEST := contextDates$CAL_95_LATE]
                        dateInfo[i, DATE_BEST_NOTES := contextDates$RC_NOTES]
                        dateInfo[i, MID := as.numeric(contextDates$CAL_MEDIAN)]
                        if(contextDates$SPECIMEN_ID == dateInfo[i, SPECIMEN_ID]) {
                              dateInfo[i, DATE_BEST_TYPE := "Direct radiocarbon"]
                        } else {
                              dateInfo[i, DATE_BEST_TYPE := "Indirect radiocarbon"]
                        }
                  }

                  # If multiple dates, check for a direct date and merge the others if there isn't one
                  if(nrow(contextDates) > 1) {
                        if(!is.na(dateInfo[i, CAL_95_EARLY])) {
                              dateInfo[i, DATE_BEST := paste0(RC_LAB_REF, ": ", RC_DATE, "±", RC_SD, " BP")]
                              dateInfo[i, DATE_FROM_BEST := CAL_95_EARLY]
                              dateInfo[i, DATE_TO_BEST := CAL_95_LATE]
                              dateInfo[i, DATE_BEST_NOTES := RC_NOTES]
                              dateInfo[i, MID := as.numeric(CAL_MEDIAN)]
                              dateInfo[i, DATE_BEST_TYPE := "Direct radiocarbon"]
                        } else {
                              contextDates[, DATE_BEST := paste0(RC_LAB_REF, ": ", RC_DATE, "±", RC_SD, " BP")]
                              dateInfo[i, DATE_BEST := paste(contextDates$DATE_BEST, collapse = ", ")]
                              dateInfo[i, DATE_FROM_BEST := min(contextDates$CAL_95_EARLY)]
                              dateInfo[i, DATE_TO_BEST := max(contextDates$CAL_95_LATE)]
                              dateInfo[i, DATE_BEST_NOTES := paste(contextDates$RC_NOTES, collapse = ", ")]
                              dateInfo[i, MID := mean(contextDates$CAL_MEDIAN)]  # Hacky but it'll do!
                              dateInfo[i, DATE_BEST_TYPE := "Indirect radiocarbon (multiple)"]
                        }
                  }
            }

            # Type (a) - individual date info
            # A date here always gives us out best TAQ
            dateInfo[!is.na(INDIV_DATE), DATE_BEST := INDIV_DATE]
            dateInfo[!is.na(INDIV_DATE),
                     DATE_TO_BEST := as.numeric(substr(INDIV_DATE, nchar(INDIV_DATE) - 3, nchar(INDIV_DATE)))]
            dateInfo[!is.na(INDIV_DATE), DATE_BEST_NOTES := INDIV_DATE_TYPE]
            dateInfo[!is.na(INDIV_DATE), DATE_BEST_TYPE := "Known date (TAQ)"]

            # Depending on the date type it might also give us our best TPQ (i.e. the same date)
            dateInfo[INDIV_DATE_TYPE %in% c("Death", "Capture"), DATE_FROM_BEST := DATE_TO_BEST]
            dateInfo[INDIV_DATE_TYPE %in% c("Death", "Capture"), DATE_BEST_TYPE := "Known date (exact)"]

            # For non-radiocarbon dates, replace the median with the mid-point
            dateInfo[!DATE_BEST_TYPE %in% c("Direct radiocarbon", "Indirect radiocarbon", "Indirect radiocarbon (multiple)"),
                     MID := (DATE_FROM_BEST + DATE_TO_BEST) / 2]

            ### Merge the result into the main SPECIMEN table
            db$SPECIMEN <- merge(db$SPECIMEN,
                                 dateInfo[, list(SPECIMEN_ID, DATE_BEST, DATE_FROM_BEST, DATE_TO_BEST, DATE_BEST_NOTES, DATE_BEST_TYPE, MID)],
                                 by = "SPECIMEN_ID", all.x = T, all.y = F)
      }

      ### FIND BEST SPECIES INFO TO BE INSERTED HERE
      ## We have the following ranks, from best to worst
      # (a) Molecular IDs (from DNA or ZOOMS)                                                         --> SCORE=4
      # (b) Confident ID on a museum/reference specimen (from INDIVIDUAL)                                       3
      # (c) Confident archaeological morph ID (from SPECIMEN; skull or mandible, not ticked as 'uncertain')     2
      # (4) Other morph ID                                                                                      1
      ## But the source of this info must also be ranked, again best to worst:
      # (i) Same specimen
      # (ii) Diff specimen, same individual
      # (iii) Diff specimen, same context

      ## Approach:
      # 1. Score specimens
      # 2. Score individuals, taking into account best IDs from component elements
      # 3. Score contexts, based on constituent specimens
      # 4. Merge all together

      # One big problem: non-rats

      ## First merge in all the relevant columns (dropping anything that's not listed as rat)
      if(bestSpecies == T) {

            ## Need to purge any duplicates from the ZooMS and DNA tables first
            # First reduce the respect tables' species columns to non-NA values that are unique for each SPECIMEN_ID
            validZooms <- unique(db$ZOOMS[!is.na(SPECIES_ZOOMS), list(SPECIMEN_ID, SPECIES_ZOOMS)])
            validDNA <- unique(db$DNA[!is.na(SPECIES_DNA), list(SPECIMEN_ID, SPECIES_DNA)])

            # Then check for remaining duplicate SPECIMEN_IDs, which must have contradictory species info
            dupZooms <- validZooms$SPECIMEN_ID[duplicated(validZooms$SPECIMEN_ID)]
            dupDNA <- validDNA$SPECIMEN_ID[duplicated(validDNA$SPECIMEN_ID)]

            # At time of writing there are none. If any do ever come up, our best bet is to purge them as unreliable
            validZooms <- validZooms[!SPECIMEN_ID %in% dupZooms]
            validDNA <- validDNA[!SPECIMEN_ID %in% dupDNA]

            # Now perform the merge, using our newly sanitised molecular ID tables
            specInfo <- merge(db$SPECIMEN[, list(SPECIMEN_ID, INDIVIDUAL_ID, CONTEXT_ID, ELEMENT, PORTIONS, SPECIES_SOURCE, SPECIES_MORPH, SPECIES_BEST, MORPH_BY, UNCERTAIN_SPEC)],
                              validDNA, by = "SPECIMEN_ID", all.x = T, all.y = F)
            specInfo <- merge(specInfo,
                              validZooms, by = "SPECIMEN_ID", all.x = T, all.y = F)

            ## Find best data from within SPECIMEN table
            # Start by setting the default to whatever the morph ID is (or SOURCE if morph not available), but not going to species level for rats
            specInfo[, SPECIES_BEST_AUTO := SPECIES_MORPH]
            specInfo[is.na(SPECIES_MORPH), SPECIES_BEST_AUTO := SPECIES_SOURCE]

            specInfo[SPECIES_MORPH %in% 1:2, SPECIES_BEST_AUTO := 3]  # For rats, force it to "Rattus sp." for now
            specInfo[, SPEC_BEST_TYPE := 1]

            # Confident morph (type d) : defined as being a skull or mandible, and IDed to species by DO or MF
            # and not flagged as uncertain
            specInfo[is.na(UNCERTAIN_SPEC), UNCERTAIN_SPEC := 0]
            specInfo[SPECIES_MORPH %in% 1:2 & ELEMENT %in% c(1,3) & MORPH_BY %in% c("DO", "MF") & UNCERTAIN_SPEC == 0, SPECIES_BEST_AUTO := SPECIES_MORPH]
            specInfo[SPECIES_MORPH %in% 1:2 & ELEMENT %in% c(1,3) & MORPH_BY %in% c("DO", "MF") & UNCERTAIN_SPEC == 0, SPEC_BEST_TYPE := 2]

            # Molecular (type a) - if not either 'inconclusive' or Rattus sp.
            specInfo[!is.na(SPECIES_ZOOMS) & !SPECIES_ZOOMS %in% c(3,5), SPECIES_BEST_AUTO:= SPECIES_ZOOMS]
            specInfo[!is.na(SPECIES_DNA) & !SPECIES_DNA %in% c(3,5), SPECIES_BEST_AUTO := SPECIES_DNA]
            specInfo[(!is.na(SPECIES_ZOOMS) & !SPECIES_ZOOMS %in% c(3,5)) | (!is.na(SPECIES_DNA) & !SPECIES_DNA %in% c(3,5)),
                     SPEC_BEST_TYPE := 4]

            ## Now move to INDIVIDUAL table
            # Find molecular IDs by individual
            molIDs <- unique(specInfo[!is.na(INDIVIDUAL_ID) & SPEC_BEST_TYPE == 4 & !SPECIES_BEST_AUTO == 3,
                                      list(INDIVIDUAL_ID, SPECIES_BEST_AUTO)])
            # Check for contradictory IDs and remove affected individuals
            dupes <- molIDs[duplicated(molIDs$INDIVIDUAL_ID), INDIVIDUAL_ID]
            molIDs <- molIDs[!INDIVIDUAL_ID %in% dupes]

            # Same for confident morph IDs
            confIDs <- unique(specInfo[!is.na(INDIVIDUAL_ID) & SPEC_BEST_TYPE == 2 & !SPECIES_BEST_AUTO == 3,
                                       list(INDIVIDUAL_ID, SPECIES_BEST_AUTO)])
            dupes <- confIDs[duplicated(confIDs$INDIVIDUAL_ID), INDIVIDUAL_ID]
            confIDs <- confIDs[!INDIVIDUAL_ID %in% dupes]

            # Reset column names and merge to INDIVIDUAL
            setnames(molIDs, old = "SPECIES_BEST_AUTO", new = "INDIVIDUAL_MOL")
            setnames(confIDs, old = "SPECIES_BEST_AUTO", new = "INDIVIDUAL_CONF")
            specIND <- merge(db$INDIVIDUAL[, list(INDIVIDUAL_ID, INDIVIDUAL_TYPE, SPECIES_SOURCE_INDIVIDUAL, SPECIES_BEST_INDIVIDUAL, SPECIES_UNCERTAIN_INDIV)],
                             molIDs, by = "INDIVIDUAL_ID", all.x = T, all.y = F)
            specIND <- merge(specIND, confIDs, by = "INDIVIDUAL_ID", all.x = T, all.y = F)

            # Set best ID for each individual
            # A molecular ID is best, even if it contradicts the 'official' species
            specIND[!is.na(INDIVIDUAL_MOL), INDIV_BEST := INDIVIDUAL_MOL]
            specIND[!is.na(INDIVIDUAL_MOL), INDIV_BEST_TYPE := 4]

            # Confident specimen-level IDs only outrank the 'official' species if
            # (a) it is archaeological or (b) it is flagged as uncertain at individual level
            specIND[is.na(INDIV_BEST_TYPE) & !is.na(INDIVIDUAL_CONF) &
                          (INDIVIDUAL_TYPE == 3 | SPECIES_UNCERTAIN_INDIV != 0), INDIV_BEST := INDIVIDUAL_CONF]
            specIND[is.na(INDIV_BEST_TYPE) & !is.na(INDIVIDUAL_CONF) &
                          (INDIVIDUAL_TYPE == 3 | SPECIES_UNCERTAIN_INDIV != 0), INDIV_BEST_TYPE := 2]

            # Otherwise, we stick with the original ID for that individual, but the score depends on the type of individual
            specIND[is.na(INDIV_BEST_TYPE), INDIV_BEST := SPECIES_BEST_INDIVIDUAL]
            specIND[is.na(INDIV_BEST_TYPE) & INDIVIDUAL_TYPE %in% 1:2, INDIV_BEST_TYPE := 3]  #not archaeological; score as reliable info
            specIND[is.na(INDIV_BEST_TYPE), INDIV_BEST_TYPE := 1]    # archaeological; score just as morph, since there were no confident IDs

            ## Now do the same thing for CONTEXT
            #### NEED TO AVOID CONTEXT IDS BASED ON NON-RATS!
            # Find molecular IDs by context
            molIDs <- unique(specInfo[!is.na(CONTEXT_ID) & SPEC_BEST_TYPE == 4 & SPECIES_BEST_AUTO %in% 1:2,
                                      list(CONTEXT_ID, SPECIES_BEST_AUTO)])
            # Check for contradictory IDs and remove affected individuals
            dupes <- molIDs[duplicated(molIDs$CONTEXT_ID), CONTEXT_ID]
            molIDs <- molIDs[!CONTEXT_ID %in% dupes]

            # Same for confident morph IDs
            confIDs <- unique(specInfo[!is.na(CONTEXT_ID) & SPEC_BEST_TYPE == 2 & SPECIES_BEST_AUTO %in% 1:2,
                                       list(CONTEXT_ID, SPECIES_BEST_AUTO)])
            dupes <- confIDs[duplicated(confIDs$CONTEXT_ID), CONTEXT_ID]
            confIDs <- confIDs[!CONTEXT_ID %in% dupes]

            # Reset column names and merge to INDIVIDUAL
            setnames(molIDs, old = "SPECIES_BEST_AUTO", new = "CONTEXT_MOL")
            setnames(confIDs, old = "SPECIES_BEST_AUTO", new = "CONTEXT_BEST")  # Use the 'conf' ID as default; we'll overwrite where there's molecular
            specCONT <- merge(molIDs, confIDs, by = "CONTEXT_ID", all = T)
            rm(molIDs, confIDs)

            # Set best ID for each context
            specCONT[, CONTEXT_BEST_TYPE := 2]
            specCONT[!is.na(CONTEXT_MOL), CONTEXT_BEST := CONTEXT_MOL]
            specCONT[!is.na(CONTEXT_MOL), CONTEXT_BEST_TYPE := 4]

            ## Merge INDIVIDAL and CONTEXT summaries back into specInfo
            specInfo <- merge(specInfo[, list(SPECIMEN_ID, INDIVIDUAL_ID, CONTEXT_ID, SPECIES_BEST, SPECIES_BEST_AUTO, SPEC_BEST_TYPE)],
                              specIND[!INDIV_BEST == 3, list(INDIVIDUAL_ID, INDIV_BEST, INDIV_BEST_TYPE)],
                              by = "INDIVIDUAL_ID", all = T)
            specInfo <- merge(specInfo, specCONT[, list(CONTEXT_ID, CONTEXT_BEST, CONTEXT_BEST_TYPE)],
                              by = "CONTEXT_ID", all = T)

            ## Now actually chose with source of info is most solid
            # By default, go with best from SPECIMEN
            specInfo[, SPECIES_BEST_SOURCE := ""]

            # If CONTEXT scores higher, take that - but flag it as such
            specInfo[CONTEXT_BEST_TYPE > SPEC_BEST_TYPE, SPECIES_BEST_AUTO := CONTEXT_BEST]
            specInfo[CONTEXT_BEST_TYPE > SPEC_BEST_TYPE, SPECIES_BEST_SOURCE := " (by association)"]
            specInfo[CONTEXT_BEST_TYPE > SPEC_BEST_TYPE, SPEC_BEST_TYPE := CONTEXT_BEST_TYPE]

            # likewise for INDIVIDUAL (here we'll also default to INDIVIDUAL where species hasn't been recorded for each specimen)
            specInfo[INDIV_BEST_TYPE > SPEC_BEST_TYPE | is.na(SPEC_BEST_TYPE), SPECIES_BEST_AUTO := INDIV_BEST]
            specInfo[INDIV_BEST_TYPE > SPEC_BEST_TYPE | (is.na(SPEC_BEST_TYPE) & !is.na(INDIV_BEST)), SPECIES_BEST_SOURCE := " (from individual)"]
            specInfo[INDIV_BEST_TYPE > SPEC_BEST_TYPE | is.na(SPEC_BEST_TYPE), SPEC_BEST_TYPE := INDIV_BEST_TYPE]

            # Tidy into a single TYPE column
            codes <- c("Morph", "Morph - confident", "Known species", "Molecular")
            specInfo[, SPECIES_BEST_TYPE := as.character(paste0(codes[SPEC_BEST_TYPE], SPECIES_BEST_SOURCE))]

            ### Merge the result into the main SPECIMEN table
            db$SPECIMEN <- merge(db$SPECIMEN,
                                 specInfo[, list(SPECIMEN_ID, SPECIES_BEST_AUTO, SPECIES_BEST_TYPE)],
                                 by = "SPECIMEN_ID", all.x = T, all.y = F)

      }

      if(prepMetrics == T) {
            # First merge the metrics lookup to the metric data, and then pull in fusion info from SPECIMEN
            db$METRICS <- merge(db$METRICS, db$measurements, by = "METRIC_CODE", all.x = T, all.y = F)
            db$METRICS <- merge(db$METRICS, db$SPECIMEN[, list(SPECIMEN_ID, PROX_FUSION, DIST_FUSION)],
                                by = "SPECIMEN_ID", all.x = T, all.y = F)

            # Now assign fusion status. Start by turning fusion columns into a factor so we can treat as ordinal
            db$METRICS[, PROX_FUSION := factor(PROX_FUSION, levels = c("U", "Fg", "F"))]
            db$METRICS[, DIST_FUSION := factor(DIST_FUSION, levels = c("U", "Fg", "F"))]

            # Assigning status of single ends is straightforward
            db$METRICS[METRIC_END == "Prox", FUSION := PROX_FUSION]
            db$METRICS[METRIC_END == "Dist", FUSION := DIST_FUSION]

            # Where both ends are implicated, we use the 'lower' of the two
            db$METRICS[METRIC_END == "Both" & as.numeric(PROX_FUSION) >= as.numeric(DIST_FUSION), FUSION := DIST_FUSION]
            db$METRICS[METRIC_END == "Both" & as.numeric(PROX_FUSION) < as.numeric(DIST_FUSION), FUSION := PROX_FUSION]

            # Where one is NA data must be missing and we can't be sure of the correct unless either end is marked asunfused
            db$METRICS[METRIC_END == "Both" & is.na(FUSION) & (PROX_FUSION == "U" | DIST_FUSION == "U"), FUSION := "U"]
      }

      # Return result
      db
}
