
## probability mass function

f <- function(w, N) {
    R <- N - 2 * w
    M <- N / 2 - R
    S <- sapply(seq_len(N / 2), function(k) N - 2 * k + 1)
    U <- sapply(seq_len(M), function(k) N / 2 - k + 1)
    V <- sapply(seq_len(R / 2), function(k) R - 2 * k + 1)
    if (length(U) == 0)
        U <- 1
    if (length(V) == 0)
        V <- 1
    (prod(U) * choose(N / 2, M) * prod(V) ** 2) / prod(S)
}

sum(f(3, 12), f(4, 12), f(5, 12), f(6, 12))

## expected value

N <- 100
w <- 25:50
p <- sapply(w, f, N)

sum(p)
sum(w * p)

## probability distribution

plot(w, p, type = 'h', lty = 'dotted',
     ylab = 'Probability', xlab = 'W', frame.plot = FALSE)
points(w, p, pch = 20)

## empirical solution

sample.pairs <- function(cards) {
    out <- apply(matrix(sample(cards, length(cards)), nrow = 2), 2, sort)
    out[, order(out[1, ])]
}

N <- 100
reps <- 100000
s <- simplify2array(lapply(seq_len(reps), function(x) sample.pairs(seq_len(N))))

mean(apply(s[2, , , drop = FALSE] > N / 2, 3, sum))
