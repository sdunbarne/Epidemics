N <- 25
# N <- 3
p <- 0.6

popn <- matrix("S", 2*N+1, 2*N+1)
popn[N+1,N+1] <- "I"

dir <- list("N" = -1, "E" = 2*N+1, "S" = 1, "W" = -(2*N+1))

infect <- function(pos, d) {
       if (popn[pos + d] == "S") {
           if (runif(1) < p ){ popn[pos + d] <<- "I"}
       }
}

epidemic <- array("", dim=c(2*N+1, 2*N+1, N)) 

for (t in 1:N) { # RINGS or EPOCHS
    
    infs <- which(popn == "I")
    if (length(infs) == 0) {
        cat("Epidemic ended after", t-1, "steps.\n")
         break}
    else  { 
     for  (j in infs) {
          for (d in dir) {
              infect(j, d)
          }
     popn[j] <- "R"
     }
    epidemic[, , t] <- popn
   }
}   

# # Animation code is adapted from
# # https://ryouready.wordpress.com/2010/11/21/animate-gif-images-in-r-imagemagick/
# epidemicfactors <- array(as.numeric(factor(epidemic)), dim=c(2*N+1, 2*N+1, N))
# par(xaxt="n", yaxt="n")         #remove axis labels and ticks
# png(file="epidemicImage%02d.png")
# for (t in 1:N) {
#     image(epidemicfactors[ , , t], col=c( "red", "blue", "green" ))
# }
# dev.off()
# system("convert -delay 80 *.png epidemicAnimation.gif")

# file.remove(list.files(pattern=".png"))

