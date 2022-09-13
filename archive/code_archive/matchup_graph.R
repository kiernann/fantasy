gaa <- read.csv("https://raw.githubusercontent.com/k5cents/GAA/master/scores.csv")
teams <- c("coln", "bill")
cols <- c("#9C27B0", "#2196F3")
team1 <- as.numeric(unlist(subset(gaa[teams[1]], gaa$year == 3)))
team2 <- as.numeric(unlist(subset(gaa[teams[2]], gaa$year == 3)))
t1_odds <- pnorm(
  mean(team2),
  mean(team1),
  sd(team1),
  lower.tail = FALSE
  )
plot(
  density(team1),
  col = cols[1],
  lwd = 4,
  ylim = c(0, 0.05),
  xlim = c(50, 150))
lines(density(team2), col = cols[2], lwd = 4)
legend(
  "topleft",
  legend = teams,
  lwd = 5,
  col = cols)
text(140, 0.03, round(t1_odds, 4), cex = 1.5)
help(text)