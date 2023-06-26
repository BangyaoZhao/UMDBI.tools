test_that("V Protocol", {
  folder = '../../../BDSI_material/Data/V_Protocol'
  all_files = list.files(folder, pattern = '\\.rds$')
  for (file_name in all_files) {
    dat = readRDS(paste(folder, file_name, sep = '/'))
    datnew = process_dat(dat)
    expect_equal(ncol(datnew$signal), length(datnew$additional_info$channel_names))
    expect_false(all(datnew$states$StimulusType == 0))
  }
})
