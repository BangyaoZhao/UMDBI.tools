test_that("multiplication works", {
  n_test = 3
  folder = '../../../K_Protocol'
  all_files = list.files(folder)
  if (length(all_files) < n_test) {
    test_files = list()
  } else {
    test_files = sample(all_files, n_test)
  }
  for (file_name in test_files) {
    dat = load_dat(paste(folder, file_name, sep = '/'))
    info = extract_info(dat)
    four_nums = c(
      nrow(info$eegmat),
      length(info$flash_type),
      length(info$code),
      length(info$flash_loc)
    )
    expect_equal(length(unique(four_nums)), 1)
  }
})
