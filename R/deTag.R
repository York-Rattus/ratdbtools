#' Turn phase tags into columns within PHASE table
#'
#' @param db local copy of RATTUS database to work with
#' @param textual Logical, default TRUE. Should text columns with concatenated strings of tags be created?
#' @param by.type Logical, default TRUE. Should tag strings be split by tag type?
#' @param tickbox Logical, default TRUE. Should a grid of logical columns be created to show which tags apply?
#' @param include.empty Logical, default FALSE. Should tags that never occur still be given columns?
#' @return A list of tables matching the input, but with new columns in PHASE.
#'
#' @export

deTag <- function(db, textual = T, by.type = T, tickbox = T, include.empty = F) {

      # Start by merging tag lookup onto table of tag occurrences
      allTags <- merge(db$PHASE_TAGS[!is.na(TAG_ID)], db$tags, by = "TAG_ID", all = include.empty)

      # Create textual column(s), if requested
      if(textual == T) {
            if(by.type == F) {
                  tagsText <- allTags[!is.na(PHASE_ID), .(TAGS = paste(TAG, collapse = ", ")), by = PHASE_ID]
            } else {
                  tagsText <- dcast(allTags[!is.na(PHASE_ID)], PHASE_ID ~ TAG_TYPE, value.var = "TAG",
                        fun.aggregate = function(x) {paste(x, collapse = ", ")})
            }
            db$PHASE <- merge(db$PHASE, tagsText, by = "PHASE_ID", all = T)
      }

      # Create logical columns, if requested
      if(tickbox == T) {
            tagsLogical <- dcast(allTags, PHASE_ID ~ TAG, value.var = "PHASE_TAGS_ID",
                                 fun.aggregate = length)
            tagsLogical <- tagsLogical[!is.na(PHASE_ID)]
            db$PHASE <- merge(db$PHASE, tagsLogical, by = "PHASE_ID", all = T)
      }

      # Return result
      db
}
