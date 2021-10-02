path_without_filename <- "C:/Users/Asus TUF/Desktop/R/Distributions/pdfs/"

save_barplot_pdf <- function(t, pdf_name) {
    path <- paste(
        path_without_filename,
        pdf_name,
        ".pdf",
        sep = ""
    )
    pdf(file = path)
    barplot(t)
    dev.off()
}

save_value_table_pdf <- function(m, pdf_name) {
    values <- data.frame(m)
    pdf(
        paste(path_without_filename, pdf_name, "_table.pdf", sep = ""),
        height = 11,
        width = 8
    )
    gridExtra::grid.table(
        values,
        rows = seq(0, 3, by = .1),
        cols = seq(0, .09, by = .01)
    )
    dev.off()
}

# Binomial Distribution

# Task I & II
x <- 0:20
p <- dbinom(x = x, size = 20, prob = 0.1)
save_barplot_pdf(p, "dbinom") # plot of the Proba. Density Function
f <- pbinom(q = x, size = 20, prob = 0.1)
save_barplot_pdf(f, "pbinom") # plot of the Cumulative Distribution Function

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
save_barplot_pdf(t, "rbinom")

# Poisson Distribution

# Task I & II
x <- 0:20
p <- dpois(x = x, lambda = 5)
save_barplot_pdf(p, "dpois") # plot of the Proba. Density Function
f <- ppois(q = x, lambda = 5)
save_barplot_pdf(f, "ppois") # plot of the Cumulative Distribution Function

# Task III
r <- rpois(n = 1000, lambda = 5)
t <- table(r) / 1000
save_barplot_pdf(t, "rpois")

# Normal Distribution

# Task I
x <- c(-3, 0.1, 3)
d1 <- dnorm(x = x, mean = 0, sd = 1)
d05 <- dnorm(x = x, mean = 0, sd = 0.5)
d101 <- dnorm(x = x, mean = 0, sd = 0.1)

d1_curve <- function(x) {
    dnorm(x = x, mean = 0, sd = 1)
}
d05_curve <- function(x) {
    dnorm(x = x, mean = 0, sd = 0.5)
}
d01_curve <- function(x) {
    dnorm(x = x, mean = 0, sd = 0.1)
}

pdf(file = paste(path_without_filename, "dnorm.pdf", sep = ""))
curve(d01_curve, from = -2, to = 2, ylab = "N(x)", col = 2)
curve(d05_curve, from = -2, to = 2, col = 3, add = TRUE)
curve(d1_curve, from = -2, to = 2, col = 4, add = TRUE)
dev.off()

# Task II
x1 <- rnorm(x)
x05 <- rnorm(x, sd = 0.5)
x01 <- rnorm(x, sd = 0.1)

# Task III
m <- matrix(
    round(
        pnorm(seq(0, 3.09, by = 0.01)),
        digits = 4
    ),
    ncol = 10, nrow = 31, byrow = TRUE
)
save_value_table_pdf(m, "pnorm")

# Chi-square Distribution

# Task I
x <- c(-3, 0.1, 3)
d5 <- dchisq(x = x, df = 5)
d10 <- dchisq(x = x, df = 10)

d5_curve <- function(x) {
    dchisq(x = x, df = 5)
}
d10_curve <- function(x) {
    dchisq(x = x, df = 10)
}

pdf(paste(path_without_filename, "dchisq.pdf"))
curve(d5_curve, from = 0, to = 20, ylab = "Dchisq", col = 2)
curve(d10_curve, from = 0, to = 20, col = 3, add = TRUE)
dev.off()

# Task II
x5 <- rchisq(x, df = 5)
x10 <- rchisq(x, df = 10)

# Task III
m <- matrix(
    round(
        pchisq(seq(0, 3.09, by = 0.01), df = 5),
        digits = 4
    ),
    ncol = 10, nrow = 31, byrow = TRUE
)
save_value_table_pdf(m, "pchisq")