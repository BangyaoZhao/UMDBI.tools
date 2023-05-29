#' @export

create_eegmat = function(signal, nT, flash_loc) {
  indi = as.vector(outer(0:(nT - 1), flash_loc, '+'))
  nflash = length(flash_loc)
  mat = sapply(flash_loc, function(i) {
    as.vector(signal[i:(i + nT - 1),])
  })
  mat = t(mat)
}
