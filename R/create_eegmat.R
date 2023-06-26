#' Helper function to convert the long EEG matrix into a tabular form
#'
#' @param signal The long EEG signal matrix.
#' @param flash_loc Locations to extract EEG segments.
#' @param nT How many time points to extract.
#' @param downrate Downsampling rate.
#' @returns A `length(flash_loc)` by `nT * ncol(signal)` EEG matrix.
#'
#' @export

create_eegmat = function(signal, flash_loc, nT = 205, downrate = 1) {
  eeglst = list()
  indi = (0:(nT - 1))*downrate
  eeglst = lapply(flash_loc, function(i) {
    return(as.vector(signal[i+indi,]))
  })
  eegmat = do.call(rbind, eeglst)
  return(eegmat)
}
