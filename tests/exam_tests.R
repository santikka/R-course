## JYU Part 1

library(Rcourse)
valitse_kieli("english")
# Test without verification file
exam("11/11/1111", override = 1000, test_mode = TRUE, test_wd = FALSE, test_part = "1", test_inst = "1")
# Test with verification file
# exam("11/11/1111", override = 1000, test_mode = FALSE, test_wd = FALSE, test_part = "1", test_inst = "1")

# Q1
pisa <- read.table("./../data/PISA_value.txt", header = TRUE)
submit(pisa)
# Q2
pisa <- pisa[ind,]
submit(pisa)
# Q3
pisa <- pisa[,mut]
submit(pisa)
# Q4
pisa <- pisa[complete.cases(pisa),]
submit(pisa)
# Q5
submit("b")
# Q6
submit(pisa$mathscore[-ind2])
# Q7
submit(min(pisa$math))
# Q8
submit(mean(pisa$readscore))
# Q9
submit("city")
# Q10
submit(cor(pisa$readscore, pisa$sciescore))
# Q11
submit(TRUE)
# Q12
pisa$fgender <- factor(pisa$ST04Q01, labels = c("female", "male"))
pisa$furban <- factor(pisa$urban, labels = c("city", "countryside"))
fregion <- factor(pisa$region, levels = 1:5)
levels(fregion) <- c("Southern Finland", "Western Finland", "Eastern Finland", 
                     "Northern Finland", "\u00C5land")
pisa$fregion <- fregion
submit(pisa)
# Q13
submit(pisa[pisa$fgender == "male" & pisa$sciescore < 500,]$sciescore)
# Q14
submit(aggregate(readscore ~ fregion, data = pisa, FUN = sd)[,2])
# Q15
submit(sort(pisa$readscore, decreasing = TRUE))
# Q16
submit(cumprod(c(1, rep(2, 19))))
# Q17
submit(rbind(head(pisa, 10), tail(pisa, 10)))
# Q18
norm <- shapiro.test(pisa[pisa$fgender == "male", "mathscore"])
submit(norm$p.value)
# Q19
levene <- leveneTest(pisa$mathscore, pisa$fregion)
submit(levene$F[1])
# Q20
tab <- table(pisa[,"ST04Q01"], pisa[,"urban"])
submit(tab)
# Q21
submit(TRUE)
# Q22
chi <- chisq.test(tab, correct = FALSE)
submit(chi$statistic)
# Q23
tt <- t.test(sciescore ~ furban, data = pisa, var.equal = TRUE)
submit(tt$p.value)
# Q24
fit <- lm(readscore ~ sciescore, data = pisa)
submit(c(fit$coefficients[2], sqrt(vcov(fit)[2,2])))
exit()


## JYU Part 2

library(Rcourse)
valitse_kieli("english")
# Test without verification file
exam("11/11/1111", override = 1000, test_mode = TRUE, test_wd = FALSE, test_part = "2", test_inst = "1")
# Test with verification file
# exam("11/11/1111", override = 1000, test_mode = FALSE, test_wd = FALSE, test_part = "2", test_inst = "1")

# Q25
submit(pnorm(2, 3, sqrt(0.5), lower.tail = FALSE))
# Q26
submit(qt(0.2, 40))
# Q27
fct <- function(x) log(x^2 + 5) - x^2 + 1
root_1 <- uniroot(fct, lower = -50, upper = 0)$root
root_2 <- uniroot(fct, lower = 0, upper = 50)$root
submit(round(c(root_1, root_2), 2))
# Q28
fct <- function(x) sin(log(x))
opt <- optimize(fct, interval = c(30, 200))
submit(round(c(opt$minimum, opt$objective), 2))
# Q29
fct <- function(x) exp(x^4 - 3*x^2)
opt <- optimize(fct, interval = c(-1, 1), maximum = TRUE)
submit(round(c(opt$maximum, opt$objective), 2))
# Q30
fct <- function(x) 2*x[1]^2 - 6*x[1] + x[2]^2 + 3
opt <- optim(c(1.5, 0), fct, lower = c(1, -1), upper = c(2, 1), method = "L-BFGS-B")
submit(round(c(opt$par, opt$value), 2))
# Q31
fct <- function(x) x[1]^2*exp(-(x[1]^2 + x[2]^2))
opt <- optim(c(-1, 0), fct, lower = c(-2, -1), upper = c(0, 1), 
               method = "L-BFGS-B", control = list(fnscale = -1))
submit(round(c(opt$par, opt$value), 2))
# Q32
fct <- function(x) 1/(sin(x)^2)
submit(round(integrate(fct, lower = 1.5, upper = 3)$value, 2))
# Q33
ordercols <- function(x) {
    x[,order(names(x))]
}
submit(ordercols)
# Q34
fun <- function(x, y) 1 * (x^2 + y^2 <= 1)
submit(fun)
# Q35
likelihood <- function(x, mu, sigma) -length(x) / 2 * log(2 * pi * sigma^2) - 1 / (2 * sigma^2) * sum((x - mu)^2)
submit(likelihood)
# Q36
statistics <- function(x) list(x, summary(x))
submit(statistics)
exit()


## UEF

library(Rcourse)
valitse_kieli("english")
# Test without verification file
exam("11/11/1111", override = 1000, test_mode = TRUE, test_wd = FALSE, test_part = "1", test_inst = "2", test_id = "100000")
# Test with verification file
#exam("11/11/1111", override = 1000, test_mode = FALSE, test_wd = FALSE, test_part = "1", test_inst = "2", test_id = "100000")

# Q1
pisa <- read.table("./../data/PISA_value.txt", header = TRUE)
submit(pisa)
# Q2
pisa <- pisa[ind,]
submit(pisa)
# Q3
pisa <- pisa[,mut]
submit(pisa)
# Q4
pisa <- pisa[complete.cases(pisa),]
submit(pisa)
# Q5
submit("c")
# Q6
submit(dim(pisa))
# Q7
submit(var(pisa$sciescore))
# Q8
submit("city")
# Q9
submit(mean(pisa$readscore))
# Q10
submit(cor(pisa$mathscore, pisa$readscore))
# Q11
submit(TRUE)
# Q12
pisa$fgender <- factor(pisa$ST04Q01, labels = c("female", "male"))
pisa$furban <- factor(pisa$urban, labels = c("city", "countryside"))
fregion <- factor(pisa$region, levels = 1:5)
levels(fregion) <- c("Southern Finland", "Western Finland", "Eastern Finland", 
                     "Northern Finland", "\u00C5land")
pisa$fregion <- fregion
submit(pisa)
# Q13
submit(pisa[pisa$fgender == "female" & pisa$mathscore > 600,]$mathscore)
# Q14
submit(aggregate(sciescore ~ fregion, data = pisa, FUN = var)[,2])
# Q15
submit(sort(pisa$mathscore))
# Q16
submit(rep(c(0, 0, 1, 1), 250))
# Q17
submit(sort(names(pisa)))
# Q18
norm <- shapiro.test(pisa[pisa$fgender == "female", "mathscore"])
submit(norm$p.value)
# Q19
levene <- leveneTest(pisa$sciescore, pisa$furban)
submit(levene$F[1])
# Q20
tab <- table(pisa[,ST[1]], pisa[,ST[2]])
submit(tab)
# Q21
submit(TRUE)
# Q22
chi <- chisq.test(tab, correct = FALSE)
submit(chi$statistic)
# Q23
tt <- t.test(mathscore ~ fgender, data = pisa, var.equal = FALSE)
submit(tt$p.value)
# Q24
fit <- lm(mathscore ~ sciescore, data = pisa)
submit(c(fit$coefficients[2], sqrt(vcov(fit)[2,2])))
# Q25
submit(pnorm(0, -2, sqrt(3)))
# Q26
submit(qt(0.2, 40))
# Q27
removerows <- function(x, k) x[-seq(1,nrow(x),k),]
submit(removerows)
# Q28
sumsq <- function(x) sum(x^2)
submit(sumsq)
# Q29
skewness <- function(x) sqrt(length(x)) * sum((x - mean(x))^3) / (sum((x - mean(x))^2)^(3/2))
submit(skewness)
# Q30
likelihood <- function(x, mu, sigma) -length(x) / 2 * log(2 * pi * sigma^2) - 1 / (2 * sigma^2) * sum((x - mu)^2)
submit(likelihood)
exit()

