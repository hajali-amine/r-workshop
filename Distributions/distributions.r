save_pdf <- function(t, pdf_name) {
    path <- paste(
        "C:/Users/Asus TUF/Desktop/R/Distributions/pdfs/",
        pdf_name,
        ".pdf",
        sep = ""
    )
    pdf(file = path)
    barplot(t)
    dev.off()
}

X <- 0:20

# Binomial Distribution

# Task I & II
P <- dbinom(x = X, size = 20, prob = 0.1)
save_pdf(P, "dbinom") # plot of the Proba. Density Function
F <- pbinom(q = X, size = 20, prob = 0.1)
save_pdf(F, "pbinom") # plot of the Cumulative Distribution Function

# Task III
r <- rbinom(n = 1000, size = 20, prob = 0.1)

# First method:
# Count the number of occurences of each value and divide it by 1000
t <- integer(length = 21)
for (value in r) {
    t[value] <- t[value] + 1
}
t <- t / 1000
barplot(t)

# Second method:
# Use the table() function and divide the result by 1000
t <- table(r) / 1000
save_pdf(t, "rbinom")

# Poisson Distribution

# Task I & II
P <- dpois(x = X, lambda = 5)
save_pdf(P, "dpois") # plot of the Proba. Density Function
F <- ppois(q = X, lambda = 5)
save_pdf(F, "ppois") # plot of the Cumulative Distribution Function

# Task III
r <- rpois(n = 1000, lambda = 5)
t <- table(r) / 1000
save_pdf(t, "rpois")