# reproducing figure 5.5
data <- read.csv('Desktop/phoneme.csv')
data <- data[,-c(1,ncol(data))]
data <- data[(data$g == 'aa') | (data$g == 'ao'), ]

set.seed(1)
frequencies <- 1:256
sample_data <- data[sample(1:nrow(data), 1000), ]  # taking a random sample
X <- sample_data[, -257]
y <- sample_data[, 257]
# first fit logistic regression to the raw data 
logistic_mod <- glm(g ~ 0 + . , data = sample_data, family = binomial())
plot(frequencies, logistic_mod$coefficients, type = 'n', ylim = c(-0.4,0.4),
     xlab = 'Frequency', ylab = 'Logistic Regression Coefficients',
     main = 'Phoneme Classification: Raw and Restricted Logistic Regression')
lines(frequencies, logistic_mod$coefficients, col='grey')

# setting the locations of the knots
num_knots <- 12
knots <- frequencies[seq(1, length(frequencies), (length(frequencies)/num_knots))]

# following procedure in 5.2.3
H <- ns(frequencies, knots = knots[2:(num_knots-1)], intercept = TRUE)
Xstar = as.matrix(X)%*%as.matrix(H)
smooth_mod <- glm.fit(x=Xstar, y=y, intercept = FALSE, family = binomial())

lines(frequencies, H%*%smooth_mod$coefficients, col='red', lwd=2)
abline(h=0)


