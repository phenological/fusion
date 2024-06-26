
#' function to left join data to acqus
#' @param A - first data.frame (acqus)
#' @param B - second data.frame (qc)
#' @param by - the name of the key column
#' @importFrom data.table setDT
#'
#' @return a data.frame with qc matched to acqus
#' @importFrom data.table setDT .SD setnames

# B <- data.frame(path = c(NA, NA, NA), value = c(10, 20, NA))
# qc <- data.frame(path = c(1, 2, 3), value = c(NA, NA, NA))
# A <- data.frame(path = c(1, 2, 3, 4), value = c(50, 3, 2, 2), test = c(34, 5, 22, 12))
# leftJoinWithAcqus(A, B, by = "path")

leftJoinWithAcqus <- function(A, B, by) {

  merged <- merge(A, B, by, all.x = TRUE, suffixes = c(".AAA",".BBB"))
  setDT(merged)

  matched <- merged[,.SD,.SDcols = c(names(merged)[!grepl("acqus", names(merged))])]

  setnames(matched, names(matched), gsub("\\.BBB$", "", names(matched)))

  return(as.data.frame(matched))
}



