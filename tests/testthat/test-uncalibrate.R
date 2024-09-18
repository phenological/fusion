test_that("testing SR shift", {
  specOpts = list(
                  fromTo = c(-0.1, 10),
                  length.out = 44079)
  file = "~/git/phenological/nmr-parser/inst/HB-COVID0001/10"
  x<-nmr.parser::readExperiment(expname = file)
  df<-x$spec$spec[[1]]$spec$y
  ppm<-as.numeric(x$spec$spec[[1]]$spec$x)
  info<-data.frame(t(x$spec$spec[[1]]$info))
  SR<-info$SR
  SF<-info$SF
  shift_idx<-round(c(SR/SF)/(ppm[2]-ppm[1]))
  expect_equal(as.character(is.na(shift_idx)), "FALSE")
})


