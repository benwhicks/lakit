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

test_that("str name to first last works", {
  expect_equal(str_name_to_first_last("Putin, Vladimir"), "Vladimir Putin")
  expect_equal(str_name_to_first_last("van der Putin, Vlad"), "Vlad van der Putin")
})

test_that("str name to last first works", {
  expect_equal(str_name_to_last_first("Boris Johnson"), "Johnson, Boris")
  expect_equal(str_name_to_last_first("Boris von Johnson"), "von Johnson, Boris")
})

test_that("str name to first last works on vectors", {
  expect_equal(
    str_name_to_first_last(c("Potter, Harry", "von Doom, Victor")),
    c("Harry Potter", "Victor von Doom"))
})

test_that("str name to last first works on vectors", {
  expect_equal(str_name_to_last_first(
    c("Bob the Builder", "Wyatt Earp")),
    c("the Builder, Bob", "Earp, Wyatt"))
})

# Could be improved to handle middle names as well.

test_that("str_to_name works", {
  expect_equal(str_to_name("connor mcGregoR"), "Connor McGregor")
  expect_equal(str_to_name("VICTOR von doom"), "Victor von Doom")
  expect_equal(str_to_name("Bob van der Wilde"), "Bob van der Wilde")
  expect_equal(str_to_name("liam o'connor"), "Liam O'Connor")
})
