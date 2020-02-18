#' R6 Class representing a card game
CardGame <- R6::R6Class("CardGame",
  public = list(
    # public attributes
    deck = NULL,
    # public methods
    # object constructor
    initialize = function(){
      self$reset()$shuffle()
    },
    # draw n cards from the deck
    draw = function(n = 1) {
      if (n > length(self$deck)) {
        n <- length(self$deck)
      }
      if (n == 0) {
        stop("No more cards left, start a new game!")
      }
      drawn_cards_idx <- seq_len(n)
      drawn_cards <- self$deck[drawn_cards_idx]
      self$deck <- self$deck[-drawn_cards_idx]
      drawn_cards
    },
    # reset deck to ordered private$..cards
    reset = function() {
      self$deck <- private$..cards
      invisible(self)
      },
    # shuffle current deck
    shuffle = function() {
      self$deck <- sample(self$deck)
      invisible(self)
      },
    # cut deck somewhere in the middle
    cut = function() {
      cut_idx <- sample(seq_along(self$deck), size = 1)
      # TODO: could probably do better
      logical_mask <- logical(length(self$deck))
      logical_mask[seq_len(cut_idx)] <- TRUE
      self$deck <- c(self$deck[!logical_mask], self$deck[logical_mask])
      invisible(self)
    }
  ),
  private = list(
    # ..color = c("G", "H", "E", "S"),
    # ..value = c(6:10, "U", "O", "K", "A"),
    # TODO: how to access other attributes?
    ..cards = paste0(rep(c("G", "H", "E", "S"), each = 9), 
                     rep(c(6:10, "U", "O", "K", "A"), times = 4))
  )
)

n_players <- 4
n_cards_per_player <- 5
# init game
card_game <- CardGame$new()
init_card_count <- length(card_game$deck)

# cut deck
card_game$cut()

# deal 5 cards to each of 4 players
for (player in seq_len(n_players)) {
  card_game$draw(n_cards_per_player)
}

curr_card_count <- length(card_game$deck)
stopifnot(init_card_count - n_players * n_cards_per_player == curr_card_count)

# draw all cards as game continues
card_game$draw(100)

# draw one more from empty deck
card_game$draw()

# restart game
# TODO: is this ok? or should initialize never be called explicitely?
card_game$initialize()
