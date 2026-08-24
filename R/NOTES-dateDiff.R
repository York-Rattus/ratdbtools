
library(rcarbon)
library(rethinking)
library(HDInterval)

dates <- calibrate(c(500, 550), c(15, 15), calCurves = "intcal20")
n  <- 1e4

#### Simple method with rcarbon's built-in sampleDates
dates <- calibrate(c(500, 550), c(15, 15), calCurves = "intcal20")
samples <- sampleDates(dates, nsim = 1e4)   
diff <- with(samples, sdates[, 1] - sdates[, 2])
quantile(diff, c(0.025, 0.5, 0.975))
with(samples, mean(sdates[, 1] > sdates[, 2]))
plot(density(diff))

#### To work out HPDI intervals, can use McElreath
rethinking::HPDI(diff, prob = 0.95)
#### Or if we need multiple windows (as we often will with RC dates)
HDInterval::hdi(density(diff), credMass = 0.95, allowSplit = TRUE)


# Method 2
draw <- function(g, n) {sample(g$calBP, n, replace = TRUE, prob = g$PrDens)}
set.seed(1)
s1 <- draw(dates$grids[[1]], n)
s2 <- draw(dates$grids[[2]], n)
quantile(s1 - s2, c(0.025, 0.5, 0.975))
mean(s1 > s2)
plot(density(s1-s2))

# Method 3
g1 <- dates$grids[[1]]; g2 <- dates$grids[[2]]
p1 <- g1$PrDens / sum(g1$PrDens)
p2 <- g2$PrDens / sum(g2$PrDens)

agg <- tapply(outer(p1, p2, "*"), outer(g1$calBP, g2$calBP, "-"), sum)
dg  <- data.frame(d = as.integer(names(agg)), p = as.numeric(agg))

hpd_grid <- function(d, p, prob = 0.95) {
  p <- p / sum(p)
  o <- order(p, decreasing = TRUE)
  keep <- sort(o[cumsum(p[o]) <= prob])
  grp  <- cumsum(c(1, diff(keep) != 1))
  do.call(rbind, lapply(split(keep, grp), function(i)
    data.frame(from = min(d[i]), to = max(d[i]), prob = sum(p[i]))))
}

hpd_grid(dg$d, dg$p)
sum(dg$p[dg$d > 0])   # P(date 1 older than date 2), exact


# Full exact grid pipeline
library(rcarbon)
dates <- calibrate(c(500, 550), c(15, 15), calCurves = "intcal20")

dif_grid <- function(g1, g2, tol = 1e-6) {
  g1 <- g1[g1$PrDens > tol, ]
  g2 <- g2[g2$PrDens > tol, ]
  p1 <- g1$PrDens / sum(g1$PrDens)
  p2 <- g2$PrDens / sum(g2$PrDens)
  
  agg <- rowsum(as.vector(outer(p1, p2, "*")),
                as.vector(outer(g1$calBP, g2$calBP, "-")))
  out <- data.frame(d = as.integer(rownames(agg)), p = as.numeric(agg))
  out <- out[order(out$d), ]
  
  full <- data.frame(d = seq(min(out$d), max(out$d)))
  out  <- merge(full, out, by = "d", all.x = TRUE)
  out$p[is.na(out$p)] <- 0
  out$p <- out$p / sum(out$p)
  out
}

dg <- dif_grid(dates$grids[[1]], dates$grids[[2]])

hpd_grid <- function(d, p, prob = 0.95) {
  p    <- p / sum(p)
  o    <- order(p, decreasing = TRUE)
  keep <- sort(o[cumsum(p[o]) <= prob])
  grp  <- cumsum(c(1, diff(keep) != 1))
  res  <- do.call(rbind, lapply(split(keep, grp), function(i)
    data.frame(from = min(d[i]), to = max(d[i]), prob = sum(p[i]))))
  res[order(-res$prob), ]
}

hpd <- hpd_grid(dg$d, dg$p, prob = 0.95)

p_older <- sum(dg$p[dg$d > 0])      # P(date1 older than date2)
p_same  <- sum(dg$p[dg$d == 0])
med     <- dg$d[which.max(cumsum(dg$p) >= 0.5)]
mode_d  <- dg$d[which.max(dg$p)]

print(hpd)
cat(sprintf("P(date1 older) = %.3f | median = %d | mode = %d\n",
            p_older, med, mode_d))

shade_hpd <- function(x, y, ints, col = adjustcolor("steelblue", 0.4)) {
  ints <- matrix(as.numeric(unlist(ints)), ncol = 2)
  for (i in seq_len(nrow(ints))) {
    sel <- x >= ints[i, 1] & x <= ints[i, 2]
    if (!any(sel)) next
    polygon(c(x[sel][1], x[sel], tail(x[sel], 1)),
            c(0, y[sel], 0), col = col, border = NA)
  }
}

plot(dg$d, dg$p, type = "n", xlab = "date1 - date2 (years)",
     ylab = "probability", main = "")
shade_hpd(dg$d, dg$p, hpd[, c("from", "to")])
lines(dg$d, dg$p)
abline(v = 0, lty = 2, col = "grey40")

text(rowMeans(hpd[, c("from", "to")]), max(dg$p) * 0.92,
     sprintf("%.1f%%", 100 * hpd$prob), cex = 0.75)
mtext(sprintf("P(date1 older) = %.2f", p_older), side = 3, adj = 1, cex = 0.8)

