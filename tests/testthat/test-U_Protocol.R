test_that("U Protocol", {
  folder = '../../../BDSI_material/Data/U_Protocol'
  all_files = list.files(folder, pattern = '\\.rds$')
  for (file_name in all_files) {
    dat = readRDS(paste(folder, file_name, sep = '/'))
    datnew = simplify(dat)
    expect_equal(ncol(datnew$signal), length(datnew$additional_info$channel_names))
    expect_false(all(datnew$states$StimulusType == 0))
  }
})
