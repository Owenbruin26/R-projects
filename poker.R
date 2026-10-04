rm(list = ls())

setwd("~/econ 97")

numbas <- 2:14   # 11-14 = J, Q, K, A
suits <- c("H", "D", "C", "S")
deck  <- expand.grid(rank = ranks, suit = suits, stringsAsFactors = FALSE)

has_straight <- function(r) {
  r <- unique(r)
  if (14 %in% r) r <- c(r, 1)        
  any(sapply(r, function(s) all(s:(s + 4) %in% r)))
}

classify <- function(hand) {
  rc  <- table(hand$rank)
  sc  <- table(hand$suit)
  cnt <- sort(as.vector(rc), decreasing = TRUE)
  
  
  flush    <- any(sc >= 5)
  straight <- has_straight(hand$rank)
  str_flush <- flush &&
    has_straight(hand$rank[hand$suit == names(sc)[sc >= 5]])
  
  if (str_flush)                              "Straight flush"
  else if (cnt[1] == 4)                       "Four of a kind"
  else if (cnt[1] == 3 && cnt[2] >= 2)        "Full house"
  else if (flush)                             "Flush"
  else if (straight)                          "Straight"
  else if (cnt[1] == 3)                       "Three of a kind"
  else if (cnt[1] == 2 && cnt[2] == 2)        "Two pair"
  else if (cnt[1] == 2)                       "Pair"
  else                                        "High card"
}

n_sims <- 10000

hand_names <- c("High card", "Pair", "Two pair", "Three of a kind", "Straight",
                "Flush", "Full house", "Four of a kind", "Straight flush")


results <- character(n_sims)
for (i in 1:n_sims) {
  hand <- deck[sample(nrow(deck), 7), ]
  results[i] <- classify(hand)
}

results <- factor(results, levels = hand_names)  

counts <- table(results)
pct    <- round(100 * prop.table(counts), 2)

data.frame(hand = hand_names, n = as.vector(counts), pct = as.vector(pct))






