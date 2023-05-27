test_that("NLP with non-fully-factorial design", {
  data("nlp", package = "rsimsum")
  nlp.subset <- nlp %>%
    dplyr::filter(!(ss == 100 & esigma == 2))
  #
  s.nlp <- rsimsum::simsum(
    data = nlp,
    estvarname = "b",
    true = 0,
    se = "se",
    methodvar = "model",
    by = c("baseline", "ss", "esigma")
  )
  td.nlp <- tidy(summary(s.nlp))
  #
  s.nlp.subset <- rsimsum::simsum(
    data = nlp.subset,
    estvarname = "b",
    true = 0,
    se = "se",
    methodvar = "model",
    by = c("baseline", "ss", "esigma")
  )
  td.nlp.subset <- tidy(summary(s.nlp.subset))
  #
  expect_true(object = (nrow(td.nlp) > nrow(td.nlp.subset)))
  #
  expect_s3_class(object = autoplot(s.nlp, type = "nlp"), class = c("gg", "ggplot"))
  expect_s3_class(object = autoplot(s.nlp.subset, type = "nlp"), class = c("gg", "ggplot"))
})
