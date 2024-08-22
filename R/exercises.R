# Define the exercises
exercises_ <- vector(mode = "list", length = 11L)

exercises_[[1L]] <- function(e) {
  len <- sample(501:1000, 1)
  mind <- sample(1:20, 1)
  maxd <- sample(100:120, 1)
  d <- sample(mind:maxd, len, replace = TRUE)
  g <- sample(0:1, len, replace = TRUE)
  h <- sample(c(NA, 1), 100, replace = TRUE)
  solutions <- list(
    `1`  = 1,
    `2`  = c(1, 2, 3, 4),
    `3`  = 1:100,
    `4`  = "c",
    `5`  = seq(1, 20, by = 0.2),
    `6`  = min(d),
    `7`  = max(d),
    `8`  = length(d),
    `9`  = mean(d),
    `10` = median(d),
    `11` = sd(d),
    `12` = var(d),
    `13` = range(d),
    `14` = sort(d),
    `15` = d + 2,
    `16` = g - 2 * d,
    `17` = g * d,
    `18` = (1 / d)^2,
    `19` = d[500],
    `20` = d[1:50],
    `21` = d[d > 20],
    `22` = sum(is.na(h)),
    `23` = h[!is.na(h)]
  )
  code <- list(
    `1`  = "x <- 1\nsubmit(x)",
    `2`  = "x <- 1:4\nsubmit(x)",
    `3`  = "x <- 1:100\nsubmit(x)",
    `4`  = "x <- \"c\"\nsubmit(x)",
    `5`  = "x <- seq(1, 20, 0.2)\nsubmit(x)",
    `6`  = "x <- min(d)\nsubmit(x)",
    `7`  = "x <- max(d)\nsubmit(x)",
    `8`  = "x <- length(d)\nsubmit(x)",
    `9`  = "x <- mean(d)\nsubmit(x)",
    `10` = "x <- median(d)\nsubmit(x)",
    `11` = "x <- sd(d)\nsubmit(x)",
    `12` = "x <- var(d)\nsubmit(x)",
    `13` = "x <- range(d)\nsubmit(x)",
    `14` = "x <- sort(d)\nsubmit(x)",
    `15` = "x <- d + 2\nsubmit(x)",
    `16` = "x <- g - 2 * d\nsubmit(x)",
    `17` = "x <- g * d\nsubmit(x)",
    `18` = "x <- (1 / d)^2\nsubmit(x)",
    `19` = "x <- d[500]\nsubmit(x)",
    `20` = "x <- d[1:50]\nsubmit(x)",
    `21` = "x <- d[d > 20]\nsubmit(x)",
    `22` = "x <- sum(is.na(h))\nsubmit(x)",
    `23` = "x <- h[!is.na(h)]\nsubmit(x)"
  )
  e$data <- list(d = d, g = g, h = h)
  data <- vector(mode = "list", length = length(solutions))
  data[c(6:15, 18:21)] <- list(list(d = "d"))
  data[16:17] <- list(list(d = "d", g = "g"))
  data[23:24] <- list(list(h = "h"))
  e$ex <- compile(
    questions = section_questions[[1L]],
    solutions = solutions,
    data = data,
    code = code
  )
}

exercises_[[2L]] <- function(e) {
  d <- sample(-10:10, 60, replace = TRUE)
  dimE <- sample(5:10, 2)
  f <- sample(-10:10, dimE[1L] * dimE[2L], replace = TRUE)
  E <- array(f, dimE)
  g1 <- sample(-10:10, 3)
  g2 <- sample(-10:10, 3)
  g3 <- sample(-10:10, 3)
  H <- matrix(sample(-10:10, 9, replace = TRUE), 3, 3)
  dk <- data.frame(
    height = c(175, 180, 190, 174),
    weight = c(60, 72, 90, 95),
    category = c(2, 2, 1, 1)
  )
  solutions <- list(
    `1`  = array(d, c(6, 10)),
    `2`  = dim(E),
    `3`  = E[2, 3],
    `4`  = E[3, ],
    `5`  = cbind(g1, g2, g3),
    `6`  = rbind(g1, g2, g3),
    `7`  = cbind(g1, H, g2),
    `8`  = dk,
    `9`  = tapply(dk$weight, dk$category, mean),
    `10` = dk$height,
    `11` = dk[2, 2],
    `12` = dk[-2, ],
    `13` = dk[, -2],
    `14` = dk[, c("height", "weight")],
    `15` = head(dk, 2),
    `16` = tail(dk, 2),
    `17` = rbind(dk, c(178, 80, 2))
  )
  code <- list(
    `1`  = "x <- array(d, c(6, 10))\nsubmit(x)",
    `2`  = "x <- dim(E)\nsubmit(x)",
    `3`  = "x <- E[2, 3]\nsubmit(x)",
    `4`  = "x <- E[3, ]\nsubmit(x)",
    `5`  = "x <- cbind(g1, g2, g3)\nsubmit(x)",
    `6`  = "x <- rbind(g1, g2, g3)\nsubmit(x)",
    `7`  = "x <- cbind(g1, H, g2)\nsubmit(x)",
    `8`  = "dk <- data.frame(height = c(175, 180, 190, 174), weight = c(60, 72, 90, 95), category = c(2, 2, 1, 1))\nsubmit(dk)",
    `9`  = "x <- tapply(dk$weight, dk$category, mean)\nsubmit(x)",
    `10` = "x <- dk$height\nsubmit(x)",
    `11` = "x <- dk[2, 2]\nsubmit(x)",
    `12` = "x <- dk[-2, ]\nsubmit(x)",
    `13` = "x <- dk[, -2]\nsubmit(x)",
    `14` = "x <- dk[, c(\"height\", \"weight\")]\nsubmit(x)",
    `15` = "x <- head(dk, 2)\nsubmit(x)",
    `16` = "x <- tail(dk, 2)\nsubmit(x)",
    `17` = "x <- rbind(dk, c(178, 80, 2))\nsubmit(x)"
  )
  e$data <- list(d = d, E = E, g1 = g1, g2 = g2, g3 = g3, H = H, dk = dk)
  data <- vector(mode = "list", length = length(solutions))
  data[[1]] <- list(d = "d")
  data[2:4] <- list(list(E = "E"))
  data[5:6] <- list(list(g1 = "g1", g2 = "g2", g3 = "g3"))
  data[[7]] <- list(g1 = "g1", H = "H", g2 = "g2")
  data[9:17] <- list(list(dk = "dk"))
  e$ex <- compile(
    questions = section_questions[[2L]],
    solutions = solutions,
    data = data,
    code = code
  )
}

exercises_[[3L]] <- function(e, write = TRUE) {
  if (write) {
    write.table(carbon, file = "carbon.txt", quote = FALSE)
    write.table(carbon, file = "carbon2.txt", quote = FALSE, sep = ",")
    write.table(carbon, file = "carbon3.txt", quote = FALSE, row.names = FALSE)
    write.table(auto, file = "automobile.txt", quote = TRUE)
    write.table(heightweight, file = "heightweight.data", quote = FALSE)
  }
  heightweight2 <- heightweight
  heightweight2$BMI <- 703 * heightweight$weight / heightweight$height^2
  carbon_numeric_row_names <- carbon
  attr(carbon_numeric_row_names, "row.names") <- as.integer(rownames(carbon))
  solutions <- list(
    `1`  = carbon,
    `2`  = carbon,
    `3`  = carbon_numeric_row_names,
    `4`  = heightweight2,
    `5`  = mean(pigs$day6),
    `6`  = auto[order(auto$hp, auto$wt), ],
    `7`  = sum(auto$hp > 100),
    `8`  = rownames(auto[order(auto$wt, decreasing = TRUE), ])[1],
    `9`  = tapply(tomato$YIELD, tomato$STRAIN, mean),
    `10` = str(tomato),
    `11` = tomato[tomato$YIELD > 10 & tomato$YIELD < 15, ],
    `12` = tomato[tomato$STRAIN == 1 & tomato$DENSITY > 10, ],
    `13` = tomato[tomato$YIELD > 15 | tomato$DENSITY == 10, ]
  )
  code <- list(
    `1`  = "x <- read.table(\"carbon.txt\")\nsubmit(x)",
    `2`  = "x <- read.table(\"carbon2.txt\", sep = \",\")\nsubmit(x)",
    `3`  = "x <- read.table(\"carbon3.txt\", header = TRUE)\nsubmit(x)",
    `4`  = "hw <- read.table(\"heightweight.data\", header = TRUE)\nhw$BMI <- 703 * hw$weight/hw$height^2\nsubmit(hw)",
    `5`  = "pigs <- read.csv2(\"pigs.csv\", header = TRUE)\nx <- mean(pigs$day6)\nsubmit(x)",
    `6`  = "auto <- read.table(\"automobile.txt\", header = TRUE)\nauto <- auto[order(auto$hp, auto$wt),]\nsubmit(auto)",
    `7`  = "x <- sum(auto$hp > 100)\nsubmit(x)",
    `8`  = "x <- rownames(auto[order(auto$wt, decreasing = TRUE), ])[1]\nsubmit(x)",
    `9`  = "tomato <- read.table(\"tomato.dat\", header = TRUE)\nx <- tapply(tomato$YIELD, tomato$STRAIN, mean)\nsubmit(x)",
    `10` = "x <- str(tomato)\nsubmit(x)",
    `11` = "x <- tomato[tomato$YIELD > 10 & tomato$YIELD < 15, ]\nsubmit(x)",
    `12` = "x <- tomato[tomato$STRAIN == 1 & tomato$DENSITY > 10, ]\nsubmit(x)",
    `13` = "x <- tomato[tomato$YIELD > 15 | tomato$DENSITY == 10, ]\nsubmit(x)"
  )
  e$data <- list(tomato = tomato, auto = auto)
  data <- vector(mode = "list", length = length(code))
  data[7:8] <- list(list(auto = "auto"))
  data[10:13] <- list(list(tomato = "tomato"))
  e$ex <- compile(
    questions = section_questions[[3L]],
    solutions = solutions,
    data = data,
    code = code
  )
}

exercises_[[4L]] <- function(e) {
  x <- seq(-3, 3, by = 0.1)
  y <- x^2
  z <- x^3
  cars_mod <- lm(dist ~ speed, data = cars)
  a <- cars_mod$coef[1]
  b <- cars_mod$coef[2]
  f <- plogis
  solutions <- list(
    `1` = expression(plot(x, y, main = "Scatterplot")),
    `2` = expression(plot(x, y, las = 1)),
    `3` = expression(plot(x, y, col = 2, pch = 3)),
    `4` = expression(plot(cars, xlab = "Speed", ylab = "Stopping distance")),
    `5` = expression({
      plot(cars)
      abline(a = a, b = b)
    }),
    `6` = expression(hist(cars$dist)),
    `7` = expression(boxplot(cars$speed)),
    `8` = expression(ts.plot(Nile)),
    `9` = expression(plot(x, y, type = "l")),
    `10` = expression({
      plot(x, y, type = "l", ylim = c(-10, 10))
      points(x, z)
    }),
    `11` = expression({
      boxplot(hp ~ am, data = auto)
    }),
    `12` = expression({
      curve(f, from = -3, to = 3)
    })
  )
  code <- list(
    `1`  = "plot(x, y, main = \"Scatterplot\")",
    `2`  = "plot(x, y, las = 1)",
    `3`  = "plot(x, y, col = 2, pch = 3)",
    `4`  = "plot(cars, xlab = \"Speed\", ylab = \"Stopping distance\")",
    `5`  = "plot(cars)\nabline(a = a, b = b)",
    `6`  = "hist(cars$dist)",
    `7`  = "boxplot(cars$speed)",
    `8`  = "ts.plot(Nile)",
    `9`  = "plot(x, y, type = \"l\")",
    `10` = "plot(x, y, type = \"l\", ylim = c(-10, 10))\npoints(x, z)",
    `11` = "boxplot(hp ~ am, data = auto)",
    `12` = "curve(f, from = -3, to = 3)"
  )
  e$data <- list(a = a, b = b, x = x, y = y, z = z, auto = auto, f = f)
  data <- vector(mode = "list", length = length(code))
  data[c(1:3, 9)] <- list(list(x = "x", y = "y"))
  data[[5]] <- list(a = "a", b = "b")
  data[[10]] <- list(x = "x", y = "y", z = "z")
  data[[11]] <- list(auto = "auto")
  data[[12]] <- list(f = "f")
  e$ex <- compile(
    questions = section_questions[[4L]],
    solutions = solutions,
    data = data,
    code = code,
    params = list(graphical = TRUE)
  )
}

exercises_[[5L]] <- function(e) {
  t1 <- t.test(Height ~ Gender2, data = children)
  boys <- subset(children, children$Gender2 == "Boy")
  girls <- subset(children, children$Gender2 == "Girl")
  girls2 <- girls[-1, ]
  t2 <- t.test(Height ~ Gender2, data = children2)
  m <- lm(Weight ~ Height, data = children2)
  solutions <- list(
    `1`  = children,
    `2`  = "boys",
    `3`  = quantile(children$Weight, c(0.25, 0.75)),
    `4`  = c(t1$stat, t1$p.value),
    `5`  = c(var(boys$Height), var(girls$Height)),
    `6`  = children2,
    `7`  = head(children2, 10),
    `8`  = names(children2),
    `9`  = summary(children2),
    `10` = nrow(children2),
    `11` = c(t2$stat, t2$p.value),
    `12` = "pos",
    `13` = cor(children2$Height, children2$Weight),
    `14` = m,
    `15` = coef(m),
    `16` = sqrt(vcov(m)[2, 2]),
    `17` = confint(m)[2, ],
    `18` = summary(m)$adj.r.squared
  )
  code <- list(
    `1`  = "children <- read.table(\"Children2007.dat\", header = TRUE)\nchildren$Gender2 <- factor(children$Gender, labels = c(\"Boy\", \"Girl\"))\nsubmit(children)",
    `2`  = "boxplot(Height ~ Gender2, data = children)\nx <- \"boys\"\nsubmit(x)",
    `3`  = "x <- quantile(children$Weight, c(0.25, 0.75))\nsubmit(x)",
    `4`  = "tt <- t.test(Height ~ Gender2, data = children)\ntt\nx <- c(tt$statistic, tt$p.value)\nsubmit(x)",
    `5`  = "x <- tapply(children$Height, children$Gender2, var)\nsubmit(x)",
    `6`  = "children2 <- children[-1, ]\nsubmit(children2)",
    `7`  = "x <- head(children2, 10)\nsubmit(x)",
    `8`  = "x <- names(children2)\nsubmit(x)",
    `9`  = "x <- summary(children2)\nsubmit(x)",
    `10` = "x <- nrow(children2)\nsubmit(x)",
    `11` = "tt2 <- t.test(Height ~ Gender2, data = children2)\ntt2\nx <- c(tt2$statistic, tt2$p.value)\nsubmit(x)",
    `12` = "plot(Weight ~ Height, data = children2)\nx <- \"pos\"\nsubmit(x)",
    `13` = "x <- cor(children2$Height, children2$Weight)\nsubmit(x)",
    `14` = "m <- lm(Weight ~ Height, data = children2)\nsubmit(m)",
    `15` = "x <- coef(m)\nsubmit(x)",
    `16` = "x <- vcov(m)[2, 2]\nsubmit(x)",
    `17` = "x <- confint(m)[2, ]\nsubmit(x)",
    `18` = "x <- summary(m)$adj.r.squared\nsubmit(x)"
  )
  e$data <- list(children = children, children2 = children2, m = m)
  data <- vector(mode = "list", length = length(code))
  data[2:6] <- list(list(children = "children"))
  data[7:14] <- list(list(children2 = "children2"))
  data[15:18] <- list(list(children2 = "children2", m = "m"))
  e$ex <- compile(
    questions = section_questions[[5L]],
    solutions = solutions,
    data = data,
    code = code
  )
}

exercises_[[6L]] <- function(e) {
  lev <- leveneTest(YIELD ~ fSTRAIN, data = ftomato)
  fit1 <- lm(YIELD ~ fSTRAIN, data = ftomato)
  pred1 <- predict(fit1, newdata = data.frame(fSTRAIN = levels(ftomato$fSTRAIN)))
  fit2 <- lm(YIELD ~ fSTRAIN + DENSITY, data = ftomato)
  pred2 <- predict(fit2, newdata = data.frame(fSTRAIN = levels(ftomato$fSTRAIN), DENSITY = 20))
  mandatory <- c(rep(FALSE, 50), rep(TRUE, 62), rep(FALSE, 34), rep(TRUE, 64))
  firstyear <- c(rep(FALSE, 112), rep(TRUE, 210 - 112))
  fmandatory <- factor(mandatory, levels = c(TRUE, FALSE))
  ffirstyear <- factor(firstyear, levels = c(TRUE, FALSE))
  tab <- table(ffirstyear, fmandatory, dnn = c("firstyear", "mandatory"))
  tab_rel1 <- prop.table(tab, margin = 1L)
  tab_rel2 <- prop.table(tab, margin = 2L)
  chi <- chisq.test(tab)
  solutions <- list(
    `1`  = c(3, 2, 1),
    `2`  = ftomato,
    `3`  = fit1,
    `4`  = TRUE,
    `5`  = pred1,
    `6`  = fit2,
    `7`  = pred2,
    `8`  = tab,
    `9`  = prop.table(tab, margin = 1L),
    `10` = prop.table(tab, margin = 2L),
    `11` = TRUE,
    `12` = c(chi$statistic, chi$p.value)
  )
  code <- list(
    `1`  = "tomato <- read.table(\"tomato.dat\", header = TRUE)\nboxplot(YIELD ~ STRAIN, data = tomato)\nx <- c(3,2,1)\nsubmit(x)",
    `2`  = "tomato$fSTRAIN <- factor(tomato$STRAIN, labels = paste0(\"STRAIN\", 1:3))\nsubmit(tomato)",
    `3`  = "m <- lm(YIELD ~ fSTRAIN, data = tomato)\nsubmit(x)",
    `4`  = "qqnorm(m$residuals)\nqqline(m$residuals)\nx <- TRUE\nsubmit(x)",
    `5`  = "x <- predict(m, newdata = data.frame(fSTRAIN = levels(tomato$fSTRAIN)))\nsubmit(x)",
    `6`  = "m2 <- lm(YIELD ~ fSTRAIN + DENSITY, data = tomato)\nsubmit(x)",
    `7`  = "x <- predict(m2, newdata = data.frame(fSTRAIN = levels(tomato$fSTRAIN), DENSITY = 20))\nsubmit(x)",
    `8`  = "# We reverse the order of the values by converting both variables to factors and setting levels manually\nmandatory <- factor(mandatory, levels = c(TRUE, FALSE))\nfirstyear <- factor(firstyear, levels = c(TRUE, FALSE))\ntilp <- table(firstyear, mandatory)\nsubmit(tilp)",
    `9`  = "x <- prop.table(tab, margin = 1)\nsubmit(x)",
    `10` = "x <- prop.table(tab, margin = 2)\nsubmit(x)",
    `11` = "x <- rowSums(tilp) %*% t(colSums(tilp)) / sum(tilp)\nx\n# Expected frequencies satisfy the assumptions\nx <- TRUE\nsubmit(x)",
    `12` = "chi <- chisq.test(tilp)\nx <- c(chi$statistic, chi$p.value)\nsubmit(x)"
  )
  e$data <- list(
    mandatory = mandatory,
    firstyear = firstyear,
    tomato = tomato,
    ftomato = ftomato,
    tab = tab,
    fit1 = fit1,
    fit2 = fit2
  )
  data <- vector(mode = "list", length = length(code))
  data[[2]] <- list(tomato = "tomato")
  data[[3]] <- list(tomato = "ftomato")
  data[4:5] <- list(list(tomato = "ftomato", m = "fit1"))
  data[[6]] <- list(tomato = "ftomato")
  data[[7]] <- list(tomato = "ftomato", m2 = "fit2")
  data[[8]] <- list(mandatory = "mandatory", firstyear = "firstyear")
  data[9:12] <- list(list(tilp = "tab"))
  e$ex <- compile(
    questions = section_questions[[6L]],
    solutions = solutions,
    data = data,
    code = code
  )
}

exercises_[[7L]] <- function(e) {
  solutions <- list(
    `1` = dnorm(c(-1, 0, 2), 1, sqrt(2)),
    `2` = dt(c(-1, 0, 2), 15),
    `3` = pnorm(3, 3, 2),
    `4` = pnorm(2, 1, sqrt(5), lower.tail = FALSE),
    `5` = pnorm(2.5, 3, sqrt(1.5)) - pnorm(-1, 3, sqrt(1.5)),
    `6` = qt(0.3, 10),
    `7` = qt(0.6, 20, lower.tail = FALSE)
  )
  code <- list(
    `1` = "x <- dnorm(c(-1, 0, 2), 1, sqrt(2))\nsubmit(x)",
    `2` = "x <- dt(c(-1, 0, 2), 15)\nsubmit(x)",
    `3` = "x <- pnorm(3, 3, sqrt(4))\nsubmit(x)",
    `4` = "x <- pnorm(2, 1, sqrt(5), lower.tail = FALSE)\nsubmit(x)",
    `5` = "x <- pnorm(2.5, 3, sqrt(1.5)) - pnorm(-1, 3, sqrt(1.5))\nsubmit(x)",
    `6` = "x <- qt(0.3, 10)\nsubmit(x)",
    `7` = "x <- qt(0.6, 20, lower.tail = FALSE)\nsubmit(x)"
  )
  e$ex <- compile(
    questions = section_questions[[7L]],
    solutions = solutions,
    code = code
  )
}

