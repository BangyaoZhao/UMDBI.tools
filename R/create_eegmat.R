#' Helper function to convert the long EEG matrix into a tabular form
#'
#' @param signal The long EEG signal matrix of dim # of time points by # channels.
#' For example, `dat[["Data"]][["RawData"]][["signal"]]`.
#' @param flash_loc Locations of flashes corresponding to row indexes in `signal`.
#' @param nT How many time points to extract after each flash.
#' @returns A `length(flash_loc)` by `nT` EEG matrix.
#'
#' @export


create_eegmat = function(signal, flash_loc, nT = 205) {
  indi = as.vector(outer(0:(nT - 1), flash_loc, '+'))
  nflash = length(flash_loc)
  mat = sapply(flash_loc, function(i) {
    as.vector(signal[i:(i + nT - 1),])
  })
  mat = t(mat)
  return(mat)
}
