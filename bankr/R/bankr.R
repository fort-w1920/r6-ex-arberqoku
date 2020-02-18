#' R6 Class representing a transaction log
#'
#' A transaction log stores a history of transactions for a bank account.
TransactionLog <- R6::R6Class("TransactionLog",
  public = list(
    #' @description
    #' Append a new transaction to current history.
    #' @param type One of c("withdraw", "deposit", "transfer")
    #' @param amount Amount of money processed by transaction
    #' @return self
    append_transaction = function(type, amount) {
      checkmate::assert_choice(type, choices = c("withdraw", "deposit", "transfer"))
      new_row <- data.frame(date(), type, amount, stringsAsFactors = FALSE)
      names(new_row) <- names(private$..history)
      private$..history <- rbind(private$..history, new_row)
      invisible(self)
    },
    #' @description
    #' Print transaction history
    #' @return NULL
    print = function() {
      cat("Start of transaction history\n")
      for (row_idx in seq_len(nrow(private$..history))) {
        cat(
          "Date:", private$..history[row_idx, "date"],
          "\tTransaction:", private$..history[row_idx, "transaction"],
          "\tAmount:", private$..history[row_idx, "amount"],
          "\n"
        )
      }
      cat("End of transaction history")
    }
  ),
  private = list(
    #' Dataframe with date, transaction and amount as colums
    ..history = data.frame(
      date = as.Date(character()),
      transaction = character(),
      amount = numeric(),
      stringsAsFactors = FALSE
    )
  ),
  active = list(
    #' @field history Expose private$..history
    history = function() {
      private$..history
    }
  )
)

#' R6 Class representing a general bank account
#'
#' An account has a balance and provides operations on this balance.
#' An account also has a history of transactions @seealso [TransactionLog].
Account <- R6::R6Class("Account",
  public = list(
    #' @description
    #' Create a new Account object.
    #' @param balance Initial account balance.
    #' @return A new `Account` object.
    initialize = function(balance = 0) {
      private$..balance <- balance
      private$..t_log <- TransactionLog$new()
      self$validate()
      self
    },
    #' @description
    #' Validate a freshly opened bank account.
    #' @return NULL
    validate = function() {
      checkmate::assert_number(private$..balance, lower = 0, finite = TRUE)
    },
    #' @description
    #' Deposit money into account.
    #' @param amount Amount of money
    #' @return self
    deposit = function(amount) {
      if (amount < 0) {
        stop("You cannot deposit a negative amount of money.")
      }
      private$..balance <- private$..balance + amount
      private$..t_log$append_transaction("deposit", amount)
      invisible(self)
    },
    #' @description
    #' Withdraw money from account.
    #' @param amount Amount of money
    #' @return self
    withdraw = function(amount) {
      if (amount < 0) {
        stop("You cannot withdraw a negative amount of money.")
      }
      private$..balance <- private$..balance - amount
      private$..t_log$append_transaction("withdraw", amount)
      invisible(self)
    }
  ),
  private = list(
    ..balance = 0,
    ..t_log = NULL
  ),
  active = list(
    #' @field balance Expose private$..balance
    balance = function() {
      if (is.na(private$..balance)) {
        return("Invalid balance")
      }
      private$..balance
    },
    #' @field transaction_log Expose methods to read and write private$..t_log
    transaction_log = function(value) {
      if (missing(value)) {
        return(private$..t_log)
      }
      checkmate::assert_r6(value, classes = c("TransactionLog"))
      private$..t_log <- value
    }
  ),
  cloneable = FALSE
)

#' Overwrite clone method of [Account]
#'
#' @return new Account
Account$set("public", "clone", function() {
  cloned_account <- Account$new(self$balance)
  cloned_account$transaction_log <- self$transaction_log$clone()
  cloned_account
})

#' R6 Class representing a giro bank account
#'
#' It has an overdraft limit which generates a small fee when exceeded.
#'
#' @seealso [Account]
GiroAccount <- R6::R6Class(
  "GiroAccount",
  inherit = Account,
  private = list(
    ..overdraft_limit = Inf,
    ..overdraft_fee = 0
  ),
  public = list(
    #' @description
    #' Create a new GiroAccount object.
    #' @param balance Initial account balance.
    #' @param overdraft_limit Nonchargeable limit
    #' @param overdraft_fee Chargable fee when `overdraft_limit` exceeded
    #' @return A new `GiroAccount` object.
    initialize = function(balance = 0, overdraft_limit = Inf, overdraft_fee = 0) {
      super$initialize(balance)
      private$..overdraft_limit <- overdraft_limit
      private$..overdraft_fee <- overdraft_fee
      self$validate()
      self
    },
    #' @description
    #' Validate a freshly opened bank account.
    #' @return NULL
    validate = function() {
      super$validate()
      checkmate::assert_number(private$..overdraft_limit, lower = 0)
      checkmate::assert_number(private$..overdraft_fee, lower = 0, finite = TRUE)
    },
    #' @description Draw additional fee when overdraft limit exceeded.
    #' @param amount Amount of money
    #'
    #' @return self
    withdraw = function(amount) {
      super$withdraw(amount)
      if (self$balance < -private$..overdraft_limit) {
        warning(
          "Your current balance exceeds your overdraft limit. ",
          "We will withdraw a fee of ",
          private$..overdraft_fee,
          " \u{20AC} from your balance."
        )
        super$withdraw(private$..overdraft_fee)
      }
      invisible(self)
    }
  )
)

# TODO: why active bindings for public methods???
