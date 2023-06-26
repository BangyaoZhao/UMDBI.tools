#' Load .mat files to R
#'
#' @param file_path The file path of the .mat file
#' @returns A nested list that represents the .mat file
#'
#' @export
#' @importFrom R.matlab readMat

load_dat = function(file_path) {
  dat <- readMat(file_path)
  dat <- rename_fun(dat)
  # channel names may not be there
  cond1 = length(dat$Data$RawData$parameters$ChannelNames$Value) == 0
  cond2 = ncol(dat$Data$RawData$signal) == 16
  if (cond1 & cond2) {
    dat$Data$RawData$parameters$ChannelNames$Value = c(
      'F3',
      'Fz',
      'F4',
      'T7',
      'C3',
      'Cz',
      'C4',
      'T8',
      'CP3',
      'CP4',
      'P3',
      'Pz',
      'P4',
      'PO7',
      'PO8',
      'Oz'
    )
  }
  return(dat)
}

rename_fun = function(x) {
  if (is.list(x)) {
    field_names = attributes(x)$dimnames[[1]]
    if (is.null(field_names)) {
      field_names = names(x)
    }
    # conditions for not going further
    cond1 = is.null(field_names)
    cond2 = length(field_names) != length(x)
    if (cond1 | cond2) {
      return(x)
    }
    names(x) = field_names
    for (i in 1:length(x)) {
      x[[i]] = rename_fun(x[[i]])
    }
    attributes(x)$dimnames = NULL
    attributes(x)$dim = NULL
    return(x)
  } else {
    return(x)
  }
}
