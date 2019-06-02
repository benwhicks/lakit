test_that("%!in% works", {
  expect_equal(2 %!in% c(1, 2), FALSE)
  expect_equal(2 %!in% c(1, 3), TRUE)
  expect_equal(2 %!in% NULL, TRUE)
})

test_that("numericable works", {
  expect_equal(is_numericable(1), TRUE)
  expect_equal(is_numericable(1.5), TRUE)
  expect_equal(is_numericable("1.5"), TRUE)
  expect_equal(is_numericable("1.5.5"), FALSE)
  expect_equal(is_numericable("A string"), FALSE)
  expect_equal(is_numericable(as.Date('2019-02-01')), TRUE)
  expect_equal(is_numericable(as.Date('2019-02-01') - as.Date('2019-01-01')), TRUE)
})

test_that("best bet works", {
  expect_equal(best_bet(list()), NA)
  expect_equal(best_bet(list(NA, NA)), NA)
  expect_equal(best_bet(list(NA, 2)), 2)
  expect_equal(best_bet(list(NA, 2, 4.6, "This long sentence")), "This long sentence")
  expect_equal(best_bet(list(5, 2)), 3.5)
})

test_that("switch name style works", {
  expect_equal(switch_name_style("Boris Johnson"), "Johnson, Boris")
  expect_equal(switch_name_style("Putin, Vladimir"), "Vladimir Putin")
  expect_equal(switch_name_style("Boris von Johnson"), "von Johnson, Boris")
  expect_equal(switch_name_style("van der Putin, Vlad"), "Vlad van der Putin")
})

test_that("switch name style works on vectors", {
  expect_equal(switch_name_style(
    c("Bob the Builder", "Wyatt Earp")),
    c("the Builder, Bob", "Earp, Wyatt"))
  expect_equal(switch_name_style(c("Potter, Harry", "von Doom, Victor")),
               c("Harry Potter", "Victor von Doom"))
})
