# library(checkmate)
# library(testthat)
# library(bankr)

context("bankr")

test_that("implementation for Account works", {
  initial_balance <- 1000
  account <- Account$new(initial_balance)

  account$deposit(100)
  expect_equivalent(account$balance, 1100)


  account$withdraw(200)
  expect_equivalent(account$balance, 900)

  account$withdraw(1000)
  expect_equivalent(account$balance, -100)

  expect_error(account$withdraw(-10))
  expect_error(account$deposit(-10))
  expect_error(Account$new(-100))
})


test_that("implementation for GiroAccount works", {
  initial_balance <- 1000
  overdraft_limit <- 500
  overdraft_fee <- 5
  account <- GiroAccount$new(initial_balance, overdraft_limit, overdraft_fee)

  expect_warning(account$withdraw(1501), regexp = "Your current balance exceeds your overdraft limit.")
  expect_equivalent(account$balance, -506)
})


test_that("implementation for TransactionLog works", {
  initial_balance <- 1000
  account <- Account$new(initial_balance)

  account$withdraw(100)
  account$deposit(50)
  expect_equivalent(as.vector(account$transaction_log$history[, "amount"]), c(100, 50))
  expect_equivalent(as.vector(account$transaction_log$history[, "transaction"]), c("withdraw", "deposit"))

  cloned_account <- account$clone()
  cloned_account$withdraw(100)

  expect_equivalent(as.vector(account$transaction_log$history[, "amount"]), c(100, 50))
  expect_equivalent(as.vector(account$transaction_log$history[, "transaction"]), c("withdraw", "deposit"))

  expect_equivalent(as.vector(cloned_account$transaction_log$history[, "amount"]), c(100, 50, 100))
  expect_equivalent(as.vector(cloned_account$transaction_log$history[, "transaction"]), c("withdraw", "deposit", "withdraw"))

})
