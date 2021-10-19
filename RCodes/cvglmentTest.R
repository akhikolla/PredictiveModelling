input.data <- data.table::fread("/Users/akhilachowdarykolla/Downloads/ReducedCoaching+CWISUpdated+NCES- binary.csv")
head(input.data)
colnames(input.data)[27] <- "ETLAverage"

trainind <- sort(sample(1:nrow(input.data), size=floor(nrow(input.data)*(2/3))))
testind <- setdiff(1:nrow(input.data), trainind)
datatrain <- input.data[trainind,]
datatest <- input.data[testind,]

x <- as.matrix(datatrain[,-27])
head(x)
y <- (datatrain[,27])
head(y)


x = matrix(rnorm(100 * 20), 100, 20)
y = rnorm(100)
cv.fit = cv.glmnet(x, y)
predict(cv.fit, newx = x[1:5, ])
coef(cv.fit)
coef(cv.fit, s = "lambda.min")
predict(cv.fit, newx = x[1:5, ], s = c(0.001, 0.002))
cv.fitr = cv.glmnet(x, y, relax = TRUE)
predict(cv.fit, newx = x[1:5, ])
coef(cv.fit)
coef(cv.fit, s = "lambda.min", gamma = "gamma.min")
predict(cv.fit, newx = x[1:5, ], s = c(0.001, 0.002), gamma = "gamma.min")
