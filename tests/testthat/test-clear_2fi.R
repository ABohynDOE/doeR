 test_that("clear2fi", {
   data("csw")
   random_row <- sample(1:dim(csw)[1], 1)
   nruns <- csw[random_row, ]$n.runs
   cols <- csw[random_row, ]$cols
   numcols <- sapply(strsplit(cols, ",")[[1]], as.numeric)
   tfi <- csw[random_row, ]$clear.2fi
   design <- custom_design(nruns, numcols)
   expect_equal(tfi, clear_2fi(design))
})
