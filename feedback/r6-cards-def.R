#' R6 Class representing a card game
CardDeck <- R6::R6Class(
  "CardDeck",
  public = list(
    # a character vector holding the cards in the stack
    stack = character(),
    initialize = function(){
      self$refill()
    },
    # draw n cards from stack
    draw = function(n = 1) {
      checkmate::assert_integerish(n, lower = 0, upper = self$n_stack)
      which <- sample(seq_len(self$n_stack), n)
      drawn <- self$stack[which]
      self$stack <- self$stack[-which]
      drawn
    },
    # return stack to complete shuffled deck
    refill = function() {
      all_cards <- paste0(rep(c("G", "H", "E", "S"), each = 9),
                          rep(c(6:10, "U", "O", "K", "A"), times = 4))
      self$stack <- sample(all_cards)
      invisible(self)
    },
    # shuffle current stack
    shuffle = function() {
      self$stack <- sample(self$stack)
      invisible(self)
    },
    # cut stack after the "where"^th card
    cut = function(where) {
      if (self$n_stack == 0) {
        return(self)
      }
      if (missing(where)) {
        where <- sample(seq_len(self$n_stack), 1)
      }
      checkmate::assert_integerish(where, lower = 1, upper = self$n_stack)
      self$stack <- c(self$stack[seq_len(self$n_stack) > where],
                      self$stack[seq_len(self$n_stack) <= where])
      invisible(self)
    }
  ),
  active = list(
    # use a (read-only) active binding to query this changing parameter easily
    # in the code:
    n_stack = function() length(self$stack)
  )
)


