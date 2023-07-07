#' Apply bandpass filter to EEG signal
#'
#' @param signal The long EEG matrix
#' @param low_freq the lower cutoff frequency
#' @param high_freq the upper cutoff frequency
#' @param sampling_rate the sampling rate of the EEG data
#' @returns EEG data after applying the bandpass filter. It has the same dimension as `signal`.
#'
#' @export
#'
#' @importFrom signal butter filtfilt
apply_bandpass_filter <-
  function(signal,
           low_freq = 0.5,
           high_freq = 30,
           sampling_rate = 256) {
    # Normalize the frequencies
    low <- low_freq / sampling_rate
    high <- high_freq / sampling_rate

    # Design the bandpass filter
    b <- butter(4, c(low, high), type = "pass")

    # Apply the filter to the EEG data
    signal_bp <- apply(signal, 2, function(x)
      filtfilt(b, x))
    return(signal_bp)
  }
