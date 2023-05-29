#' Helper function to extract useful information
#'
#' @param dat Output from `load_dat`
#' @returns A list
#' \itemize{
#' \item{`flash_loc`: }{The starting point of each flash. }
#' \item{`flash_type`: }{Whether the flash is a target, same length as `flash_loc`.
#' Target as 1 and non-target as 0.}
#' \item{`code`: }{The flash code, same length as `flash_loc`,
#' 1-6 means rows and 7-12 means columns.}
#' \item{`nfpc`: }{Number of flash generated per each character. For example,
#' if one character has four sequences, then `nfpc = 48`, because
#' each sequence has 12 flashes}
#' }
#' @export

extract_info = function(dat) {
  states = dat$Data$RawData$states
  flash_loc = which(states$StimulusBegin == 1)
  flash_loc = flash_loc[seq(1, length(flash_loc), 8)]

  nc = nrow(dat[["Data"]][["DBIData"]][["DBI.EXP.Info"]][["Intended.Text"]])
  indi_last = which(diff(flash_loc) == 936)
  nfpc = indi_last[1]
  if (length(indi_last) == nc - 1) {
    stopifnot(length(flash_loc) - tail(indi_last, 1) == nfpc)
  } else if (length(indi_last) == nc) {
    flash_loc = flash_loc[1:tail(indi_last, 1)]
  } else {
    stop('Bad data!')
  }
  code = states$StimulusCode[flash_loc]
  flash_type = dat[["Data"]][["DBIData"]][["DBI.EXP.Info"]][["Stimulus.Type"]][flash_loc]

  return(list(
    flash_loc = flash_loc,
    flash_type = flash_type,
    code = code,
    nfpc = nfpc
  ))
}
