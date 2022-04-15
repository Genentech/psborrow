test_that("Ensure errors are correct", {

  dt1 <- tibble(
    HR = c(0.8, 0.8),
    driftHR = c(1,1),
    prior = c('full_ext','gamma'),
    pred = c('all','all'),
    reject = c(0.2, 0.3)
  )

  dt2 <- tibble(
    HR = c(0.8, 0.8, 1.0, 1.0),
    driftHR = c(1,1,1,1),
    prior = c('full_ext','gamma','full_ext','gamma'),
    pred = c('all','all','all','all'),
    reject = c(0.2, 0.3,0.4,0.5)
  )

  expect_error(plot_type1error(dt1, driftHR=1,pred='all'),
               "dt does not include HR = 1.0")
  expect_error(plot_type1error(dt2, driftHR=1.3,pred='all'),
               "dt does not include a driftHR of 1.3")
  expect_error(plot_type1error(dt2, driftHR=1,pred='cov1'),
               "dt does not include a pred of 'cov1'")
})

test_that("Ensure output is producing a correct ggplot2 object", {

  dt3 <- tibble(
    HR = c(rep(0.8,4), rep(1.0,4)),
    driftHR = rep(1.0, 8),
    prior = rep(c('no_ext','gamma','cauchy','full_ext'),2),
    pred = rep('all',8),
    reject = seq(0.1,0.3,length.out=8)
  )

  dt4 <- dt3[dt3$prior!='no_ext',]

  p1 <- plot_type1error(dt3, driftHR=1, pred='all')
  p2 <- plot_type1error(dt4, driftHR=1, pred='all')

  expect_equal(class(p1)[1],'gg')
  expect_equal(class(p2)[1],'gg')

  expect_equal(NROW(p1$layers), 3L)
  expect_equal(NROW(p2$layers), 2L)
  expect_equal(p1$labels$yintercept, 'ref')
  expect_equal(p1$labels$caption, 'Horizontal purple line refers to the type 1 error without any external arm (0.2143)')
  expect_equal(p2$labels$title, 'Summarizing posterior distributions: Type 1 Error')

})
