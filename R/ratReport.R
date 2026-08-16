#' Produce a quick specimen report to send to collaborators.
#'
#' Replicate analyses of a specimen occupy successive rows rather than
#' multiplying out, so a specimen takes as many rows as its most-replicated
#' technique. Core fields are blanked below the first row of each specimen, so
#' that a filled cell always means a distinct result.
#'
#' Row inclusion and column selection are controlled separately. The integer
#' flags decide which results appear as rows; the `*Fields` arguments decide
#' which columns appear, and a field is drawn from whichever table holds it.
#' Naming D13C in `rcFields` with `isotopes = 0` therefore gives a table of
#' dates carrying the isotope values the dating lab reported alongside them,
#' and nothing else.
#'
#' Isotope results are attached to a date where the two record the same
#' laboratory. Since this is inferred rather than recorded, keep the two
#' laboratory fields in the output so that a reader can check it.
#'
#' Co-location of other techniques on a row carries no meaning: a ZooMS result
#' and a date on the same row were not necessarily run on the same material.
#'
#' @param db List of data.tables as created by one of the rattusPull functions
#' @param critField Character. What field should be used to select specimens? May name a column from any of SITE, PHASE, CONTEXT or SPECIMEN. Default "SITE_NAME"
#' @param critValues Vector of values of `critField` to include. NULL for all.
#' @param context Integer. Should context data be included? 0 for no, 1 for yes, 2 to only show specimens for which it is present. Default 1.
#' @param rc Integer. Should radiocarbon data be included? 0 for no, 1 for yes, 2 to only show specimens for which it is present. Default 1.
#' @param isotopes Integer. Should isotope data be included? 0 for no, 1 for yes, 2 to only show specimens for which it is present. Isotope results reported alongside a date are included whenever `rc` is set, whatever this is.
#' @param zooms Integer. Should detailed zooms data be included? 0 for no, 1 for yes, 2 to only show specimens for which it is present.
#' @param dna Integer. Should detailed DNA data be included? 0 for no, 1 for yes, 2 to only show specimens for which it is present.
#' @param metrics Integer. Should metric data be included? 0 for no, 1 for yes, 2 to only show specimens for which it is present. Default 1.
#' @param coreFields Character vector of core fields to include.
#' @param contextFields Character vector of context fields to include. These are spliced into the core fields rather than appended.
#' @param rcFields Character vector of radiocarbon fields to include. May name columns of any specialist table, not only RADIOCARBON. NULL for all radiocarbon fields.
#' @param isotopeFields Character vector of isotope fields to include. NULL for all.
#' @param zoomsFields Character vector of zooms fields to include. NULL for all.
#' @param dnaFields Character vector of DNA fields to include. NULL for all.
#' @param metricCode Character vector of metric codes to include. If not supplied, all codes will be included
#' @param inconclusive Character vector of values treated as non-answers when reconciling repeated identifications.
#' @param repeatFields Character vector of core fields repeated on every row of a specimen rather than blanked.
#' @param suppressEmpty Logical. Should entirely empty columns be removed? Default FALSE.
#' @return A data.table
#'
#' @import data.table
#' @export
ratReport <- function(db,
                      critField = "SITE_NAME",
                      critValues = NULL,
                      context = 0, rc = 1, isotopes = 1, zooms = 0, dna = 0, metrics = 0,
                      coreFields = c("SPECIMEN_ID", "SITE_NAME", "SETTLEMENT", "PHASE_AT_SITE", "DATING", "CONTEXT",
                                     "ID_SOURCE", "SPECIES_SOURCE", "SPECIES_MORPH",
                                     "SPECIES_ZOOMS", "SPECIES_DNA", "ELEMENT", "SIDE"),
                      contextFields = c("CONTEXT", "TRENCH_SECTOR", "FEATURE", "LAYER", "DEPTH", "ACCESSION_NO",
                                        "CONTEXT_TYPE", "DATE_EXCAVATED", "CONTEXT_DATING"),
                      rcFields = c("RC_LAB", "RC_LAB_REF", "RC_DATE", "RC_SD", "d13C_AMS", "d13C", "d15N", "C_N", "CAL_MEDIAN", "CAL_95_EARLY", "CAL_95_LATE", "RC_NOTES"),
                      isotopeFields = c("ISOTOPE_LAB", "ISOTOPE_LAB_CODE", "d13C", "d15N", "PC_YIELD", "PC_C", "PC_N", "C_N",
                                        "ANALYTICAL_REPLICATES_COUNT", "SD_d13C", "SD_d15N", "ISOTOPE_NOTES"),
                      zoomsFields = c("ZOOMS_MASS", "ZOOMS_NOTES", "a1_508", "a2_978", "a2_978_HYP", "a2_484", "a2_502", "a2_220", "a2_793", "a2_454", "a1_934", "a1_586",
                                      "a1_586_HYP", "a2_757", "a2_757_HYP", "a2_10"),
                      dnaFields = c("DNA_LAB", "S_NO_RAW_READS", "S_NO_UNIQUE_MAPPED_READS", "S_PC_ENDOGENOUS", "S_AVERAGE_MAPPED_LENGTH",  "PICKED_FOR_DEEP_SEQ",
                                    "NO_RAW_READS", "NO_UNIQUE_MAPPED_READS", "PC_ENDOGENOUS", "AVERAGE_MAPPED_LENGTH", "NUC_COVERAGE", "SEX_DNA", "MT_COVERAGE", "MT_HAPLOGROUP"),
                      metricCodes = NULL,
                      inconclusive = c("UNKNOWN", "Inconclusive"),
                      repeatFields = c("SPECIMEN_ID", "ID_SOURCE"),
                      suppressEmpty = FALSE) {

      # Return a length-one NA of the same type as x, so that blanking a carried
      # value doesn't coerce the column.
      naLike <- function(x) x[NA_integer_][1L]

      # Reduce several determinations for one specimen to a single value. Values
      # judged inconclusive are set aside; if two substantive values disagree,
      # both are returned so the conflict shows up in the report.
      resolveID <- function(x, inconclusive = character(0), sep = " | ") {
            x <- x[!is.na(x) & trimws(as.character(x)) != ""]
            if(!length(x)) return(NA_character_)
            good <- unique(x[!x %in% inconclusive])
            if(length(good) == 1L) return(good)
            if(length(good) == 0L) return(unique(x)[1L])
            paste(sort(good), collapse = sep)
      }

      # Tidy a laboratory name for matching.
      normLab <- function(x) {
            x <- toupper(trimws(as.character(x)))
            x[x == ""] <- NA_character_
            x
      }

      # Work out which report row each specialist result belongs on. Dates anchor
      # the rows; an isotope result joins a date where the two record the same
      # laboratory, and otherwise takes a row of its own, flagged .PAIRED = FALSE.
      # Other techniques have no basis for pairing and fill from the first row.
      assignReps <- function(tables, keep) {
            reps <- list()
            hasRC <- "RADIOCARBON" %in% tables
            hasISO <- "ISOTOPES" %in% tables
            if(hasRC) {
                  rcDat <- copy(data$RADIOCARBON)[SPECIMEN_ID %in% keep]
                  rcDat[, .ROW := seq_len(.N)]
                  rcDat[, .REP := seq_len(.N), by = SPECIMEN_ID]
                  reps$RADIOCARBON <- rcDat
            }
            if(hasISO) {
                  isoDat <- copy(data$ISOTOPES)[SPECIMEN_ID %in% keep]
                  isoDat[, .ROW := seq_len(.N)]
                  if(hasRC && nrow(isoDat) && nrow(reps$RADIOCARBON)) {
                        rcDat <- reps$RADIOCARBON
                        rcDat[, .LAB := normLab(RC_LAB)]
                        isoDat[, .LAB := normLab(ISOTOPE_LAB)]
                        # An unrecorded lab must never match another unrecorded lab
                        rcDat[is.na(.LAB), .LAB := paste0("<rc-unknown:", .ROW, ">")]
                        isoDat[is.na(.LAB), .LAB := paste0("<iso-unknown:", .ROW, ">")]
                        # Within a specimen and laboratory, pair by order
                        rcDat[, .K := seq_len(.N), by = .(SPECIMEN_ID, .LAB)]
                        isoDat[, .K := seq_len(.N), by = .(SPECIMEN_ID, .LAB)]
                        amb <- rcDat[, .N, by = .(SPECIMEN_ID, .LAB)][N > 1L]
                        if(nrow(amb)) {
                              warning("More than one date from the same laboratory for specimen(s): ",
                                      paste(unique(amb$SPECIMEN_ID), collapse = ", "),
                                      ". Isotope results were paired to dates by order, which may not ",
                                      "reflect which sample they were reported with.")
                        }
                        isoDat <- merge(isoDat, rcDat[, .(SPECIMEN_ID, .LAB, .K, .REP)],
                                        by = c("SPECIMEN_ID", ".LAB", ".K"), all.x = T, all.y = F)
                        isoDat[, .PAIRED := !is.na(.REP)]
                        # Unpaired runs take fresh rows below the dates
                        topRep <- rcDat[, .(.M = max(.REP)), by = SPECIMEN_ID]
                        isoDat <- merge(isoDat, topRep, by = "SPECIMEN_ID", all.x = T, all.y = F)
                        isoDat[is.na(.M), .M := 0L]
                        isoDat[is.na(.REP), .REP := .M + seq_len(.N), by = SPECIMEN_ID]
                        isoDat[, c(".LAB", ".K", ".M") := NULL]
                        rcDat[, c(".LAB", ".K") := NULL]
                        reps$RADIOCARBON <- rcDat
                  } else {
                        isoDat[, .REP := seq_len(.N), by = SPECIMEN_ID]
                        isoDat[, .PAIRED := FALSE]
                  }
                  reps$ISOTOPES <- isoDat
            }
            for(t in setdiff(tables, c("RADIOCARBON", "ISOTOPES"))) {
                  x <- copy(data[[t]])[SPECIMEN_ID %in% keep]
                  x[, .REP := seq_len(.N), by = SPECIMEN_ID]
                  reps[[t]] <- x
            }
            reps
      }

      # Columns that are never reported in their own right.
      keys <- c("SPECIMEN_ID", "SAMPLE_ID", ".ROW", ".REP", ".PAIRED")

      # Repeated determinations that should be reconciled rather than listed, and
      # the table each comes from.
      collapse <- list(SPECIES_ZOOMS = "ZOOMS", SPECIES_DNA = "DNA")

      # Check whether rattusDecode has been run, and run it if not.
      data <- copy(db)
      if(is.integer(data$SPECIMEN$SPECIES_MORPH)) {
            data <- rattusDecode(data)
      }

      # Merge SITE, PHASE, CONTEXT, SPECIMEN tables. These are all many-to-one
      # going up, so no specimen can be duplicated here.
      core <- with(data, merge(SPECIMEN, CONTEXT, by = c("CONTEXT_ID", "PHASE_ID", "SITE_ID"), all.x = T, all.y = F))
      core <- with(data, merge(core, PHASE, by = c("PHASE_ID", "SITE_ID"), all.x = T, all.y = F))
      core <- with(data, merge(core, SITE, by = "SITE_ID", all.x = T, all.y = F))

      # Select the specimens to report on.
      if(!is.null(critValues)) {
            if(!critField %in% names(core)) {
                  stop("critField '", critField, "' is not a column of the merged core tables.")
            }
            core <- core[get(critField) %in% critValues]
      }
      if(context == 2) core <- core[CONTEXT_ID %in% data$CONTEXT$CONTEXT_ID]

      # Decide which tables to process and which of their columns to show. A table
      # is processed if its own flag is set, or if another block has asked for one
      # of its columns - which is what lets a dates-only report carry the isotope
      # values the dating lab supplied.
      flags <- c(RADIOCARBON = rc, ISOTOPES = isotopes, ZOOMS = zooms, DNA = dna)
      wanted <- list(RADIOCARBON = rcFields, ISOTOPES = isotopeFields,
                     ZOOMS = zoomsFields, DNA = dnaFields)
      # Only blocks that are switched on may ask for columns, whether from their
      # own table or someone else's. A block with its own field list contributes
      # nothing beyond it; one left NULL contributes its whole table.
      allRequested <- unique(unlist(wanted[names(flags)[flags > 0]], use.names = F))
      show <- list()
      for(t in names(flags)) {
            if(is.null(data[[t]])) next
            cols <- setdiff(names(data[[t]]), keys)
            mine <- if(flags[[t]] > 0 && is.null(wanted[[t]])) cols else character(0)
            show[[t]] <- unique(c(mine, intersect(allRequested, cols)))
      }
      unknown <- setdiff(allRequested, unlist(lapply(names(flags), function(t) names(data[[t]]))))
      if(length(unknown)) {
            warning("Field(s) not found in any specialist table: ", paste(unknown, collapse = ", "))
      }
      process <- names(flags)[vapply(names(flags), function(t) {
            !is.null(data[[t]]) && (flags[[t]] > 0 || length(show[[t]]) > 0)
      }, logical(1))]
      # Isotope columns can only be placed against a date if dates are present
      if("ISOTOPES" %in% process && flags[["ISOTOPES"]] == 0 && rc == 0) {
            warning("Isotope columns were requested but neither isotope runs nor dates are ",
                    "included; they cannot be placed and will be dropped.")
            process <- setdiff(process, "ISOTOPES")
      }

      # Remove specimens without requested data (i.e. where any of the specialist
      # data arguments are set to 2). Where more than one is set, these combine as
      # OR: a specimen is kept if it has any of the requested data types.
      required <- names(flags)[flags == 2]
      if(metrics == 2) required <- c(required, "METRICS")
      if(length(required)) {
            hasData <- unique(unlist(lapply(required, function(t) {
                  x <- data[[t]]
                  if(is.null(x)) return(NULL)
                  if(t == "METRICS" && !is.null(metricCodes)) x <- x[MEASUREMENT_CODE %in% metricCodes]
                  x$SPECIMEN_ID
            })))
            core <- core[SPECIMEN_ID %in% hasData]
      }

      # Reconcile repeated identifications to one value per specimen and treat them
      # as core fields. Done whatever the parent flag says, since the resolved
      # species ID is wanted even when the full ZooMS or DNA block is not shown.
      collapsed <- character(0)
      for(fld in names(collapse)) {
            tbl <- collapse[[fld]]
            if(is.null(data[[tbl]]) || !fld %in% names(data[[tbl]])) next
            if(fld %in% names(core)) core[, (fld) := NULL]
            res <- data[[tbl]][SPECIMEN_ID %in% core$SPECIMEN_ID,
                               .(V1 = resolveID(get(fld), inconclusive)), by = SPECIMEN_ID]
            setnames(res, "V1", fld)
            conflicts <- res[grepl(" | ", get(fld), fixed = T), SPECIMEN_ID]
            if(length(conflicts)) {
                  warning("Conflicting ", fld, " for specimen(s): ",
                          paste(conflicts, collapse = ", "), ". Both values retained in the report.")
            }
            core <- merge(core, res, by = "SPECIMEN_ID", all.x = T, all.y = F)
            collapsed <- c(collapsed, fld)
      }
      for(t in names(show)) show[[t]] <- setdiff(show[[t]], collapsed)

      # Metrics are long-format, so cast to wide rather than treating them as
      # replicates. Repeated measurements of the same code are averaged.
      metricCols <- character(0)
      mWide <- NULL
      if(metrics > 0) {
            m <- data$METRICS[SPECIMEN_ID %in% core$SPECIMEN_ID]
            if(!is.null(metricCodes)) m <- m[METRIC_CODE %in% metricCodes]
            if(nrow(m)) {
                  mWide <- dcast(m, SPECIMEN_ID ~ METRIC_CODE, value.var = "METRIC_VALUE",
                                 fun.aggregate = function(v) mean(v, na.rm = T))
                  metricCols <- setdiff(names(mWide), "SPECIMEN_ID")
            }
      }

      # Assign report rows, then drop isotope runs that stand alone if only the
      # values supporting a date were asked for.
      reps <- assignReps(process, core$SPECIMEN_ID)
      if("ISOTOPES" %in% process && flags[["ISOTOPES"]] == 0) {
            reps$ISOTOPES <- reps$ISOTOPES[.PAIRED == T]
      }

      # Expand the core table to the number of rows each specimen needs. This is
      # the maximum across techniques, not the product.
      nrep <- rbindlist(lapply(reps, function(x) {
            if(!nrow(x)) return(NULL) else x[, .(N = max(.REP)), by = SPECIMEN_ID]
      }))
      if(nrow(nrep)) {
            nrep <- nrep[, .(N = max(N)), by = SPECIMEN_ID]
            core <- merge(core, nrep, by = "SPECIMEN_ID", all.x = T, all.y = F)
      } else {
            core[, N := NA_integer_]
      }
      core[is.na(N), N := 1L]
      out <- core[rep(seq_len(.N), N)]
      out[, .REP := seq_len(.N), by = SPECIMEN_ID]
      out[, N := NULL]

      # Merge the specialist tables in, on specimen and report row. Column names
      # are assumed unique across tables; a clash would otherwise be silently
      # mangled to .x/.y by merge and then dropped from the report.
      blockCols <- character(0)
      for(t in process) {
            vars <- show[[t]]
            if(!length(vars)) next
            clash <- intersect(vars, names(out))
            if(length(clash)) {
                  stop("Column name(s) from ", t, " already in the report: ",
                       paste(clash, collapse = ", "))
            }
            b <- reps[[t]][, c("SPECIMEN_ID", ".REP", vars), with = F]
            out <- merge(out, b, by = c("SPECIMEN_ID", ".REP"), all.x = T, all.y = F)
            blockCols <- c(blockCols, vars)
      }
      if(!is.null(mWide)) out <- merge(out, mWide, by = "SPECIMEN_ID", all.x = T, all.y = F)

      # Sort, then blank the values carried down onto a specimen's later rows. A
      # typed NA is used rather than "" so the columns keep their class, and reads
      # as an empty cell on writing out with na = "".
      sortCols <- intersect(c("SITE_NAME", "PHASE_AT_SITE", "CONTEXT", "SPECIMEN_ID"), names(out))
      setorderv(out, c(sortCols, ".REP"))
      carried <- setdiff(c(intersect(coreFields, names(out)), intersect(contextFields, names(out)),
                           metricCols), repeatFields)
      if(length(carried)) {
            out[.REP > 1L, (carried) := lapply(.SD, naLike), .SDcols = carried]
      }

      # Assemble the column list. Core fields keep the order given, with the extra
      # context columns spliced in after the last core field from the context level
      # or above, so they still land sensibly if CONTEXT itself was dropped.
      coreKeep <- intersect(coreFields, names(out))
      if(context > 0 && length(contextFields)) {
            extras <- intersect(setdiff(contextFields, coreKeep), names(out))
            lev <- c(SITE = 1L, PHASE = 2L, CONTEXT = 3L, SPECIMEN = 4L)
            fieldLev <- vapply(coreKeep, function(f) {
                  hits <- lev[vapply(names(lev), function(t) f %in% names(data[[t]]), logical(1))]
                  if(length(hits)) max(hits) else 4L
            }, integer(1))
            coreKeep <- append(coreKeep, extras, after = max(c(0L, which(fieldLev <= 3L))))
      }
      keep <- unique(c(coreKeep, intersect(blockCols, names(out)), metricCols))
      out <- out[, ..keep]

      if(suppressEmpty) {
            empty <- names(out)[vapply(out, function(x) all(is.na(x) | trimws(as.character(x)) == ""),
                                       logical(1))]
            if(length(empty)) out[, (empty) := NULL]
      }

      out[]
}
