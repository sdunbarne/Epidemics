S0 <- 999
I0 <- 1
p  <- 0.2

T <- 30
S <- numeric(T + 1)
I <- numeric(T + 1)

S[1]  <- S0
I[1]  <- I0
for (t in 2:(T + 1)) {
    I[t]  <- rbinom(1, size=S[t-1], prob=p)
    S[t] <-  S[t - 1] - I[t]
}

plot(1:(T + 1), S, col="blue",
     xlim = c(0, (T + 1)), ylim=c(0, (S0 + I0)),
     main="Greenwood Epidemic Model",
     xlab="Time", ylab="S, I")
points(1:(T + 1), y=I, col="red")
