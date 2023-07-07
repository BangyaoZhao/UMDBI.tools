test_that("K protocol", {
  n_test = 3
  folder = '../../../BDSI_material/Data/K_Protocol'
  all_files = list.files(folder)
  if (length(all_files) < n_test) {
    test_files = list()
  } else {
    test_files = sample(all_files, n_test)
  }
  for (file_name in test_files) {
    dat = load_dat(paste(folder, file_name, sep = '/'))
    datnew = process_dat(dat)
    expect_equal(ncol(datnew$signal), length(datnew$additional_info$channel_names))
    expect_false(all(datnew$states$StimulusType == 0))
    signal_bp = apply_bandpass_filter(datnew$signal)
    expect_equal(dim(signal_bp), dim(datnew$signal))
  }
})
