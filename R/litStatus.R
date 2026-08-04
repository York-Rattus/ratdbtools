#' Combine a db object with info from the live progress tracker to summarise the state of play in the literature database module
#'
#' @param db local copy of RATTUS database to work with
#' @param file URI to current file location
#' @param sheet Number of sheet/tab within file
#' @return a list of two data.tables, summarising progress firstly by country and secondly by site.
#'
#' @export

litStatus <- function(db, file = "https://docs.google.com/spreadsheets/d/1yegDMHO6l2DLcDJtaSfzhMeW8-6kW4kUM9417aLaYh4/edit?gid=613804957#gid=613804957",
                      sheet = 2) {

      # Load required packages
      library(data.table)
      library(googlesheets4)

      # Import country list
      countryList <- read_sheet(file, sheet = sheet)

      # For each country on the list, we want to report:
      # - number of sites in DB
      # - sites with with specimen
      # - sites with lit/both records
      # - number of phases in DB
      # - of which marked "complete" (or similar)
      # - of which have taxa
      # - of which has

      # Need to start from the bottom: scoring ASSEMBLAGE records
      # Make a copy to avoid messing with original, then drop assemblages marked for deletion
      db2 <- copy(db)
      db2$ASSEMBLAGE <- db2$ASSEMBLAGE[STATUS_ASSEMBLAGE != "DELETE" & !is.na(PHASE_ID)]

      # Assign status to assemblages based on entry type and the two status fields
      db2$ASSEMBLAGE[ASSEMBLAGE_ENTRY_TYPE == 2, assmStatus := "Specimens only"]
      db2$ASSEMBLAGE[ASSEMBLAGE_ENTRY_TYPE %in% c(1,3) & STATUS_ASSEMBLAGE == "Complete" & STATUS_TAXA %in% c("Full", "Full (presence)"),
                    assmStatus := "Complete"]
      db2$ASSEMBLAGE[ASSEMBLAGE_ENTRY_TYPE %in% c(1,3) & (STATUS_TAXA %in% c("Empty") | STATUS_ASSEMBLAGE == "Placeholder"),
                            assmStatus := "To do"]
      db2$ASSEMBLAGE[is.na(assmStatus), assmStatus := "In progress/check"]  # Catch-all for things not marked as complete or barely started
      db2$ASSEMBLAGE[, assmStatus := factor(assmStatus, levels = c("To do", "In progress/check", "Complete", "Specimens only"))]

      # Now want to tally these up by PHASE entry
      # We don't want to count 'specimens only' records here, but we do need to use them to show when a phase isn't exactly missing data
      # Since they are the highest level, they will only show up in the table generated below if there are no lit assemblages too
      phaseTable <- merge(db2$PHASE[, list(PHASE_ID, SITE_ID, PHASE_STATUS, PHASE_ENTRY_TYPE, ORIGINAL_CHECKED)],
                          db2$ASSEMBLAGE[, list(ASSEMBLAGE_ID, PHASE_ID, assmStatus)], by = "PHASE_ID", all = T)
      phaseTable <- phaseTable[PHASE_ENTRY_TYPE == 1, ]
      phase_scores <- phaseTable[, .(phase_status = min(as.numeric(assmStatus))), by = c("PHASE_ID", "SITE_ID")]
      phase_scores[is.na(phase_status), phase_status := 1]

      # So now we have a table of phases scored by their worst assemblage status
      # Let's add a field to flag all phases with specimens, just in case some are hidden by other assemblages with lit data
      phase_scores[, Specimens := FALSE]
      phase_scores[PHASE_ID %in% phaseTable[assmStatus == "Specimens only", PHASE_ID], Specimens := TRUE]

      # We want to summarise this by country, but we also want to attach it to SITE so we can score sites by completeness
      siteTable <- merge(db2$SITE[SITE_ENTRY_TYPE == 1, list(SITE_ID, COUNTRY, REGION, LATITUDE, LONGITUDE)],
                         phase_scores, by = "SITE_ID", all = T)
      siteTable[is.na(phase_status), phase_status := 1]
      site_scores <- siteTable[, .(site_status = min(phase_status)),
                               by = c("SITE_ID", "COUNTRY", "REGION", "LATITUDE", "LONGITUDE")]
      site_scores[, Specimens := FALSE]
      site_scores[SITE_ID %in% siteTable[phase_status == 4, SITE_ID], Specimens := TRUE]

      # Now we should be able to start making our summary table
      # Start with site level info
      sitesByCountry <- dcast(site_scores[COUNTRY %in% countryList$COUNTRY], COUNTRY ~ site_status, fun.aggregate = length, value.var = "SITE_ID")
      specByCountry <- dcast(site_scores[COUNTRY %in% countryList$COUNTRY], COUNTRY ~ ., fun.aggregate = sum, value.var = "Specimens")
      sitesByCountry <- merge(sitesByCountry, specByCountry, by = "COUNTRY")
      sitesByCountry[, LitSitesTotal := `1` + `2` + `3`]
      sitesByCountry[, LitSitesUnfinished := `1` + `2`]
      setnames(sitesByCountry, old = c("3", "."), new = c("LitSitesDone", "SitesWithSpecimens"))
      sitesByCountry[, c("1", "2", "4") := NULL]

      # Now phase level info
      phasesByCountry <- dcast(siteTable[COUNTRY %in% countryList$COUNTRY], COUNTRY ~ phase_status, fun.aggregate = length, value.var = "PHASE_ID")
      specPhasesByCountry <- dcast(siteTable[COUNTRY %in% countryList$COUNTRY], COUNTRY ~ ., fun.aggregate = sum, value.var = "Specimens")
      phasesByCountry <- merge(phasesByCountry, specPhasesByCountry, by = "COUNTRY")
      phasesByCountry[, LitPhasesTotal := `1` + `2` + `3`]
      setnames(phasesByCountry, old = c("1", "2", "3", "."), new = c("LitPhasesEmpty", "LitPhasesInProgress", "LitPhasesDone", "PhasesWithSpecimens"))
      phasesByCountry[, c("4") := NULL]

      # Merge the two together, re-ordering columns in the process
      statsByCountry <- merge(sitesByCountry[, list(COUNTRY, LitSitesTotal, LitSitesUnfinished, LitSitesDone, SitesWithSpecimens)],
                              phasesByCountry[, list(COUNTRY, LitPhasesTotal, LitPhasesEmpty, LitPhasesInProgress, LitPhasesDone, PhasesWithSpecimens)],
                              by = "COUNTRY")

      # And merge with master sheet
      countryList <- merge(countryList, statsByCountry, by = "COUNTRY", all.x = T, all.y = F)

      # Swap in some meaningful terms for phase status in siteTable
      siteTable <- siteTable[, phase_status := factor(phase_status, levels = 1:4,
                                                         labels = c("To do", "In progress/check", "Complete", "Specimens only"))]

      # Return results
      list(countryList, siteTable)

}
