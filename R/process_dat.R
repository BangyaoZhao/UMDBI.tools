#' Simplify the raw data to focus on less imformation
#'
#' @param dat The nested list of one run
#' @returns A simplier nested list that contains important information
#' \itemize{
#'   \item \code{signal}: The long EEG signal matrix.
#'   \item \code{states}: State variables.
#'   \item \code{additional_info}: Other useful variables such as
#'   sampling frequency and channel names.
#' }
#' @export

process_dat = function(dat) {
  newdat = list(
    signal = dat$Data$RawData$signal,
    states = lapply(dat$Data$RawData$states, as.vector),
    additional_info = list(
      channel_names = dat$Data$RawData$parameters$ChannelNames$Value,
      freq = dat[["Data"]][["RawData"]][["parameters"]][["SamplingRate"]][["NumericValue"]]
    )
  )
  # sometimes in K protocol StimulusType is stored else where
  if (all(newdat$states$StimulusType == 0)) {
    candidate1 = dat$Data$DBIData$DBI.EXP.Info$Stimulus.Type
    candidate2 = dat$Data$DBIData$DBI_EXP_Info$StimulusType
    if (!is.null(candidate1)) {
      newdat$states$StimulusType = candidate1
    } else if (!is.null(candidate2)) {
      newdat$states$StimulusType = candidate2
    } else {
      stop('Can not find StimulusType')
    }
  }
  # create TrialNR
  if (!'TrialNR' %in% names(newdat$states)) {
    TrialNR = numeric(nrow(newdat$signal))
    change_locs = which(diff(newdat$states$PhaseInSequence) <= -2)
    for (i in 1:length(change_locs)) {
      loc = change_locs[i]
      if (i == 1) {
        start = 1
      } else {
        start = change_locs[i - 1] + 1
      }
      TrialNR[start:loc] = i
    }
    TrialNR[newdat$states$PhaseInSequence == 0] = 0
    newdat$states$TrialNR = TrialNR
  }
  #
  x = newdat$states$StimulusBegin
  locs <- which(x[-1] == 1 & x[-length(x)] == 0) + 1
  locs <- locs[newdat$states$TrialNR[locs] != 0]
  newdat$additional_info$stimulus_locs = locs
  return(newdat)
}
