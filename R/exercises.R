# Define the exercises
exercises_ <- vector(mode = "list", length = 11L)

exercises_[[1]] <- function(e) {
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
        `22` = sum(is.na(h))
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
        `22` = "x <- sum(is.na(h)\nsubmit(x))"
    )
    e$data <- list(d = d, g = g, h = h)
    data <- vector(mode = "list", length = length(solutions))
    data[c(6:15,18:21)] <- list(list(d = "d"))
    data[16:17] <- list(list(d = "d", g = "g"))
    data[[22]] <- list(h = "h")
    e$ex <- compile(questions = section_questions[[1]],
                    solutions = solutions,
                    data = data,
                    code = code)
}

exercises_[[2]] <- function(e) {
    d <- sample(-10:10, 60, replace = TRUE)
    dimE <- sample(5:10, 2)
    f <- sample(-10:10, dimE[1] * dimE[2], replace = TRUE)
    E <- array(f, dimE)
    g1 <- sample(-10:10, 3)
    g2 <- sample(-10:10, 3)
    g3 <- sample(-10:10, 3)
    H <- matrix(sample(-10:10, 9, replace = TRUE), 3, 3)
    dk <- data.frame(height = c(175, 180, 190, 174), 
                     weight = c(60, 72, 90, 95), 
                     category = c(2, 2, 1, 1))
    solutions <- list(
        `1`  = array(d, c(6, 10)),
        `2`  = dim(E),
        `3`  = E[2,3],
        `4`  = E[3, ],
        `5`  = cbind(g1, g2, g3),
        `6`  = rbind(g1, g2, g3),
        `7`  = data.frame(g1, H, g2),
        `8`  = dk,
        `9`  = tapply(dk$weight, dk$category, mean),
        `10` = dk$height,
        `11` = dk[2,2],
        `12` = dk[-2, ],
        `13` = dk[ ,-2],
        `14` = dk[ ,c("height", "weight")],
        `15` = rbind(dk, c(178, 80, 2))
    )
    code <- list(
        `1`  = "x <- array(d, c(6, 10))\nsubmit(x)",
        `2`  = "x <- dim(E)\nsubmit(x)",
        `3`  = "x <- E[2,3]\nsubmit(x)",
        `4`  = "x <- E[3, ]\nsubmit(x)",
        `5`  = "x <- cbind(g1, g2, g3)\nsubmit(x)",
        `6`  = "x <- rbind(g1, g2, g3)\nsubmit(x)",
        `7`  = "x <- data.frame(g1, H, g2)\nsubmit(x)",
        `8`  = "dk <- data.frame(height = c(175, 180, 190, 174), weight = c(60, 72, 90, 95), category = c(2, 2, 1, 1))\nsubmit(dk)",
        `9`  = "x <- tapply(dk$weight, dk$category, mean)\nsubmit(x)",
        `10` = "x <- dk$height\nsubmit(x)",
        `11` = "x <- dk[2,2]\nsubmit(x)",
        `12` = "x <- dk[-2, ]\nsubmit(x)",
        `13` = "x <- dk[ ,-2]\nsubmit(x)",
        `14` = "x <- dk[ ,c(\"height\", \"weight\")]\nsubmit(x)",
        `15` = "x <- rbind(dk, c(178, 80, 2))\nsubmit(x)"
    )
    e$data <- list(d = d, E = E, g1 = g1, g2 = g2, g3 = g3, H = H, dk = dk)
    data <- vector(mode = "list", length = length(solutions))
    data[[1]] <- list(d = "d")
    data[2:4] <- list(list(E = "E"))
    data[5:6] <- list(list(g1 = "g1", g2 = "g2", g3 = "g3"))
    data[[7]] <- list(g1 = "g1", H = "H", g2 = "g2")
    data[9:15] <- list(list(dk = "dk"))
    e$ex <- compile(questions = section_questions[[2]],
                    solutions = solutions,
                    data = data,
                    code = code)
}

exercises_[[3]] <- function(e, write = TRUE) {
    if (write) {
        write.table(carbon, file = "carbon.txt", quote = FALSE)
        write.table(carbon, file = "carbon2.txt", quote = FALSE, sep = ",")
        write.table(carbon, file = "carbon3.txt", quote = FALSE, row.names = FALSE)
        write.table(auto, file = "automobile.txt", quote = TRUE)
        write.table(heightweight, file = "heightweight.data", quote = FALSE)
    }
    heightweight2 <- heightweight
    heightweight2$BMI <- 703 * heightweight$weight / heightweight$height^2
    solutions <- list(
        `1` = carbon,
        `2` = carbon,
        `3` = carbon,
        `4` = heightweight2,
        `5` = round(mean(pigs$day6), 2),
        `6` = auto[order(auto$hp, auto$wt),],
        `7` = list(tomato, mean(tomato$YIELD)),
        `8` = tomato[tomato$YIELD > 10 & tomato$YIELD < 15, ],
        `9` = tomato[tomato$STRAIN == 1 & tomato$DENSITY > 10, ],
        `10` = tomato[tomato$YIELD > 15 | tomato$DENSITY == 10, ]
    )
    code <- list(
        `1`  = "x <- read.table(\"carbon.txt\")\nsubmit(x)",
        `2`  = "x <- read.table(\"carbon2.txt\", sep = \",\")\nsubmit(x)",
        `3`  = "x <- read.table(\"carbon3.txt\", header = TRUE)\nsubmit(x)",
        `4`  = "hw <- read.table(\"heightweight.data\", header = TRUE)\nhw$BMI <- 703 * hw$weight/hw$height^2\nsubmit(hw)",
        `5`  = "pigs <- read.csv2(\"pigs.csv\", header = TRUE)\nx <- round(mean(pigs$day6), 2)\nsubmit(x)",
        `6`  = "auto <- read.table(\"automobile.txt\", header = TRUE)\nauto <- auto[order(auto$hp, auto$wt),]\nsubmit(auto)",
        `7`  = "tomato <- read.table(\"tomato.dat\", header = TRUE)\nx <- list(tomato, mean(tomato$YIELD))\nsubmit(x)",
        `8`  = "x <- tomato[tomato$YIELD > 10 & tomato$YIELD < 15, ]\nsubmit(x)",
        `9`  = "x <- tomato[tomato$STRAIN == 1 & tomato$DENSITY > 10, ]\nsubmit(x)",
        `10` = "x <- tomato[tomato$YIELD > 15 | tomato$DENSITY == 10, ]\nsubmit(x)"
    )
    e$data <- list(tomato = tomato)
    data <- vector(mode = "list", length = length(code))
    data[8:10] <- list(list(tomato = "tomato"))
    e$ex <- compile(questions = section_questions[[3]],
                    solutions = solutions,
                    data = data,
                    code = code)
}

exercises_[[4]] <- function(e) {
    x <- seq(-3, 3, by = 0.1)
    y <- x^2
    z <- x^3
    cars_mod <- lm(dist ~ speed, data = cars)
    a <- cars_mod$coef[1]
    b <- cars_mod$coef[2]
    xx <- seq(-2, 2, by = 0.1)
    yy <- seq(-2, 2, by = 0.1)
    zz <- exp(-outer(xx^2, yy^2, "+"))
    solutions <- list(
        `1`  = expression(plot(x, y, main = "Scatterplot")),
        `2`  = expression(plot(x, y, las = 1)),
        `3`  = expression(plot(x, y, col = 2, pch = 3)),
        `4`  = expression(plot(cars, xlab = "Speed", ylab = "Stopping distance")),
        `5`  = expression({
             plot(cars)
             abline(a = a, b = b)}),
        `6`  = expression(hist(cars$dist)),
        `7`  = expression(boxplot(cars$speed)),
        `8`  = expression(ts.plot(Nile)),
        `9`  = expression(plot(x, y, type = "l")),
        `10` = expression({
            plot(x, y, type = "l", ylim = c(-10, 10))
            points(x, z)}),
        `11` = expression(image(xx, yy, zz)),
        `12` = expression(contour(xx, yy, zz)),
        `13` = expression(persp(xx, yy, zz))
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
        `11` = "image(xx, yy, zz)",
        `12` = "contour(xx, yy, zz)",
        `13` = "persp(xx, yy, zz)"
    )
    e$data <- list(a = a, b = b, x = x, y = y, z = z, xx = xx, yy = yy, zz = zz)
    data <- vector(mode = "list", length = length(code))
    data[c(1:3, 9)] <- list(list(x = "x", y = "y"))
    data[[5]] <- list(a = "a", b = "b")
    data[[10]] <- list(x = "x", y = "y", z = "z")
    data[11:13] <- list(list(xx = "xx", yy = "yy", zz = "zz"))
    e$ex <- compile(questions = section_questions[[4]], 
                    solutions = solutions,
                    data = data,
                    code = code,
                    params = list(graphical = TRUE))
}

exercises_[[5]] <- function(e) {
    t1 <- t.test(Height ~ Gender2, data = children, var.equal = TRUE)
    t2 <- leveneTest(Height ~ Gender2, data = children)
    boys <- subset(children, children$Gender2 == "Boy")
    girls <- subset(children, children$Gender2 == "Girl")
    t3 <- shapiro.test(girls$Height)
    t4 <- shapiro.test(boys$Height)
    girls2 <- girls[-1,]
    t5 <- shapiro.test(girls2$Height)
    t6 <- t.test(Height ~ Gender2, data = children2, var.equal = TRUE)
    m <- lm(Weight ~ Height, data = children2)
    solutions <- list(
        `1`  = children,
        `2`  = "boys",
        `3`  = quantile(children$Weight, c(0.25, 0.75)),
        `4`  = t1$stat,
        `5`  = c(var(boys$Height), var(girls$Height)),
        `6`  = t2$F[1],
        `7`  = c(t4$p, t3$p),
        `8`  = t5$stat,
        `9`  = head(children2, 10),
        `10` = names(children2),
        `11` = summary(children2),
        `12` = t6$stat,
        `13` = "pos",
        `14` = cor(children2$Height, children2$Weight),
        `15` = TRUE,
        `16` = sqrt(vcov(m)[2,2]),
        `17` = TRUE
    )
    code <- list(
        `1`  = "children <- read.table(\"Children2007.dat\", header = TRUE)\nchildren$Gender2 <- factor(children$Gender, labels = c(\"Boy\", \"Girl\"))\nsubmit(children)",
        `2`  = "boxplot(Height ~ Gender2, data = children)\nx <- \"boys\"\nsubmit(x)",
        `3`  = "x <- quantile(children$Weight, c(0.25, 0.75))\nsubmit(x)",
        `4`  = "tt <- t.test(Height ~ Gender2, data = children, var.equal = TRUE)\ntt\nx <- tt$statistic\nsubmit(x)",
        `5`  = "x <- tapply(children$Height, children$Gender2, var)\nsubmit(x)",
        `6`  = "lev <- leveneTest(Height ~ Gender2, data = children)\nlev\nx <- lev$`F value`[1]\nsubmit(x)",
        `7`  = "shap <- tapply(children$Height, children$Gender2, shapiro.test)\nshap\nx <- c(shap[[1]]$p, shap[[2]]$p)\nsubmit(x)",
        `8`  = "children2 <- children[-1,]\nshap2 <- shapiro.test(children2$Height[children2$Gender2 == \"Girl\"])\nx <- shap2\nshap2$statistic\nsubmit(x)",
        `9`  = "x <- head(children2, 10)\nsubmit(x)",
        `10` = "x <- names(children2)\nsubmit(x)",
        `11` = "x <- summary(children2)\nsubmit(x)",
        `12` = "tt2 <- t.test(Height ~ Gender2, data = children2, var.equal = TRUE)\ntt2\nx <- tt2$stat\nsubmit(x)",
        `13` = "x <- \"pos\"\nsubmit(x)",
        `14` = "x <- cor(children2$Height, children2$Weight)\nsubmit(x)",
        `15` = "m <- lm(Weight ~ Height, data = children2)\nqqnorm(m$residuals)\nshapiro.test(m$residuals)\nx <- TRUE\nsubmit(x)",
        `16` = "# Read the value from column Std. Error and row Height.\nsummary_table <- summary(m)\nx <- summary_table$coefficients[\"Height\",\"Std. Error\"]\nsubmit(x)",
        `17` = "summary(m)\nx <- TRUE\nsubmit(x)"
    )
    e$data <- list(children = children, children2 = children2, m = m)
    data <- vector(mode = "list", length = length(code))
    data[2:8] <- list(list(children = "children"))
    data[9:15] <- list(list(children2 = "children2"))
    data[16:17] <- list(list(children2 = "children2", m = "m"))
    e$ex <- compile(questions = section_questions[[5]],
                    solutions = solutions,
                    data = data,
                    code = code)
}

exercises_[[6]] <- function(e) {
    lev <- leveneTest(YIELD ~ fSTRAIN, data = ftomato)
    fit <- anova(lm(YIELD ~ fSTRAIN, data = ftomato))
    mandatory <- c(rep(FALSE, 50), rep(TRUE, 62), rep(FALSE, 34), rep(TRUE, 64))
    firstyear <- c(rep(FALSE, 112), rep(TRUE, 210-112))
    tab <- table(1-firstyear, 1-mandatory)
    chi <- chisq.test(tab, correct = FALSE)
    solutions <- list(
        `1`  = c(3, 2, 1),
        `2`  = ftomato,
        `3`  = lev$F[1],
        `4`  = TRUE,
        `5`  = c(fit$Df[1], round(fit$F[1], 1)),
        `6`  = fit$Sum[1],
        `7`  = 2,
        `8`  = tab,
        `9`  = TRUE,
        `10` = chi$p.value
    )
    code <- list(
        `1`  = "tomato <- read.table(\"tomato.dat\", header = TRUE)\nboxplot(YIELD ~ STRAIN, data = tomato)\nx <- c(3,2,1)\nsubmit(x)",
        `2`  = "tomato$fSTRAIN <- factor(tomato$STRAIN, labels = paste0(\"STRAIN\", 1:3))\nsubmit(tomato)",
        `3`  = "lev <- leveneTest(YIELD ~ fSTRAIN, data = tomato)\nlev\nx <- lev$`F value`[1]\nsubmit(x)",
        `4`  = "an <- anova(lm(YIELD ~ fSTRAIN, data = tomato))\nan\nx <- TRUE\nsubmit(x)",
        `5`  = "x <- c(an$Df[1], round(an$`F value`[1], 1)\nsubmit(x)",
        `6`  = "x <- an$`Sum Sq`[1]\nsubmit(x)",
        `7`  = "TukeyHSD(aov(YIELD ~ fSTRAIN, data = tomato))\nx <- 2\nsubmit(x)",
        `8`  = "tilp <- table(1 - firstyear, 1 - mandatory)\nsubmit(tilp)",
        `9`  = "x <- rowSums(tilp) %*% t(colSums(tilp)) / sum(tilp)\nx <- TRUE\nsubmit(x)",
        `10` = "chi <- chisq.test(tilp, correct = FALSE)\nchi\nx <- chi$p.value\nsubmit(x)"
    )
    e$data <- list(mandatory = mandatory, 
                   firstyear = firstyear, 
                   tomato = tomato, 
                   ftomato = ftomato, 
                   tab = tab)
    data <- vector(mode = "list", length = length(code))
    data[[2]] <- list(tomato = "tomato")
    data[3:7] <- list(list(tomato = "ftomato"))
    data[[8]] <- list(mandatory = "mandatory", firstyear = "firstyear")
    data[9:10] <- list(list(tilp = "tab"))
    e$ex <- compile(questions = section_questions[[6]], 
                    solutions = solutions, 
                    data = data,
                    code = code)
}

exercises_[[7]] <- function(e) {
    x <- c(runif(100, -5, 5), 1, 1, -1, -1)
    y <- c(runif(100, -5, 5), 1, -1, 1, -1)
    a <- rbinom(100, 1, 0.5)
    b <- rbinom(100, 1, 0.5)
    points6 <- vector(mode = "list", length = 100)
    points7 <- vector(mode = "list", length = 100)
    points8 <- vector(mode = "list", length = 100)
    for (i in 1:100) {
        points6[[i]] <- list(x = x[i], y = y[i])
        points7[[i]] <- list(x = x[i])
        points8[[i]] <- list(a = a[i], b = b[i], x = x[i], y = y[i])
    }
    d.poist1 <- data.frame(c1 = runif(1000, -5, 5), c2 = runif(1000, 0, 2))
    d.poist2 <- data.frame(c1 = runif(1000, -5, 5), c2 = runif(1000, 0, 2))
    d1 <- data.frame(abc = 1, yt = 1, vac = 1)
    d2 <- data.frame(z = 1, g = 3, h = 1)
    d3 <- data.frame(c = 1, b = 1, d = 1)
    x2 <- rnorm(1000, 2, 0.5)
    x3 <- rt(1000, 20)
    test_input <- list(
        `1`  = list(p1 = list(celsius = -20), p2 = list(celsius = 0), p3 = list(celsius = 100)),
        `2`  = list(p1 = list(weight = 50, height = 1.60), p2 = list(weight = 80, height = 1.80)),
        `3`  = list(p1 = list(x = -3, a = 2, b = 1), p2 = list(x = 2, a = -2, b = 1), p3 = list(x = 0, a = 0, b = 5)),
        `4`  = list(p1 = list(celsius = -20), p2 = list(), p3 = list(celsius = 0), p4 = list(celsius = 100)),
        `5`  = list(p1 = list(x = 777), p2 = list(x = 0), p3 = list(x = -734)),
        `6`  = points6,
        `7`  = points7,
        `8`  = points8,
        `9`  = list(p1 = list(x = d.poist1, k = 3), p2 = list(x = d.poist2, k = 7)),
        `10` = list(p1 = list(x = d1), p2 = list(x = d2), p3 = list(x = d3)),
        `11` = list(p1 = list(x = x2), p2 = list(x = x3))
    )
    solutions <- list(
        `1` = function(celsius) { 
            return(celsius + 273.15) 
        },
        `2` = function(weight, height) {
            return(weight / height^2)
        },
        `3` = function(x, a, b) { 
            return(a * x^2 + b)
        },
        `4` = function(celsius = 20) { 
            return(celsius + 273.15)
        },
        `5` = function(x) { 
            if (x <= 0) {
                return(0)   
            } else {
                return(1)
            }
        },
        `6` = function(x, y) {
            if (x^2 + y^2 <= 1) {
                return(1)
            } else {
                return(0)
            }
        },
        `7` = function(x) {
            if (x <= -1) {
                return(0)
            } else if (-1 < x & x < 1) {
                return(x^2)
            } else {
                return(1)
            }
        },
        `8` = function(a, b, x, y) {
            if (a == 1 & b == 1) {
                return(x)
            } else if (a == 1 & b == 0) {
                return(y)
            } else {
                return(0)
            }
        },
        `9` = function(x, k) {
            return(x[-seq(1, nrow(x), by = k), ])
        },
        `10` = function(x) {
            return(x[,order(colnames(x))])
        },
        `11` = function(x) {
            return(sum(x^2))
        }
    )
    code <- list(
        `1`  = "kelvin <- function(celsius) {\n    return(celsius + 273.15)\n}\nsubmit(kelvin)",
        `2`  = "bmi <- function(weight, height) {\n    return(weight / height^2)\n}\nsubmit(bmi)",
        `3`  = "fun <- function(x, a, b) {\n    return(a * x^2 + b)\n}\nsubmit(fun)",
        `4`  = "fun <- function(celsius = 20) {\n    return(celsius + 273.15)\n}\nsubmit(fun)",
        `5`  = "fun <- function(x) {\n    if (x <= 0) {\n        return(0)\n    } else {\n        return(1)\n    }\n}\nsubmit(fun)",
        `6`  = "fun <- function(x, y) {\n    if (x^2 + y^2 <= 1) {\n        return(1)\n    } else {\n        return(0)\n    }\n}\nsubmit(fun)",
        `7`  = "fun <- function(x) {\n    if (x <= -1) {\n        return(0)\n    } else if (-1 < x & x < 1) {\n        return(x^2)\n    } else {\n        return(1)\n    }\n}\nsubmit(fun)",
        `8`  = "fun <- function(a, b, x, y) {\n    if (a == 1 & b == 1) {\n        return(x)\n    } else if (a == 1 & b == 0) {\n        return(y)\n    } else {\n        return(0)\n    }\n}\nsubmit(fun)",
        `9`  = "removerows <- function(x, k) {\n    return(x[-seq(1, nrow(x), by = k), ])\n}\nsubmit(removerows)",
        `10` = "ordercols <- function(x) {\n    return(x[,order(colnames(x))])\n}\nsubmit(ordercols)",
        `11` = "sumsq <- function(x) {\n    return(sum(x^2))\n}\nsubmit(sumsq)"
    )
    params <- lapply(1:length(solutions), function(i) {
        list(is_function = TRUE, test_input = test_input[[i]])
    })
    e$ex <- compile(questions = section_questions[[7]],
                    solutions = solutions,
                    code = code,
                    params = params)
}

exercises_[[8]] <- function(e) {
    fct1 <- function(x) exp(x + 4) + 3 * x
    fct2 <- function(x) x*log(x)
    fct3 <- function(x) -x[1]^2 * exp(-0.5 * (x[1]^2 + x[2]^2))
    fct4 <- function(x) 1 / sin(x)^3
    fct5 <- function(x) x^3 - 5 * x^2 + x + sqrt(2)
    opt1 <- optim(0.5, fct2, lower = c(0), upper = c(1), method = "L-BFGS-B")
    opt2 <- optim(c(0.5, 0), fct3, lower = c(0, -1), upper = c(2, 1), method = "L-BFGS-B")
    intgrl <- integrate(fct4, lower = 1, upper = 3)
    solutions <- list(
        `1` = round(pnorm(2.5, 3, sqrt(1.5)) - pnorm(-1, 3, sqrt(1.5)), 2),
        `2` = round(qt(0.6, 20, lower.tail = FALSE), 3),
        `3` = round(uniroot(fct1, lower = -3, upper = 2)$root, 2),
        `4` = c(round(opt1$par, 2), round(opt1$value, 2)),
        `5` = c(round(opt2$par, 2), -round(opt2$value, 2)),
        `6` = round(intgrl$value, 2),
        `7` = round(c(uniroot(fct5, c(-1, 0))$root, uniroot(fct5, c(0, 1))$root), 2)
    )
    code <- list(
        `1` = "x <- round(pnorm(2.5, 3, sqrt(1.5)) - pnorm(-1, 3, sqrt(1.5)), 2)\nsubmit(x)",
        `2` = "x <- round(qt(0.6, 20, lower.tail = FALSE), 3)\nsubmit(x)",
        `3` = "x <- round(uniroot(function(x) exp(x + 4) + 3 * x, lower = -3, upper = 2)$root, 2)\nsubmit(x)",
        `4` = "opt <- optim(0.5, function(x) x * log(x), lower = 0, upper = 1, method = \"L-BFGS-B\")\nx <- c(round(opt$par, 2), round(opt$value, 2))\nsubmit(x)",
        `5` = "opt <- optim(c(0.5, 0), function(x) -x[1]^2 * exp(-0.5 * (x[1]^2 + x[2]^2)), lower = c(0, -1), upper = c(2, 1), method = \"L-BFGS-B\")\nx <- c(round(opt$par, 2), -round(opt$value, 2))\nsubmit(x)",
        `6` = "int <- integrate(function(x) 1 / sin(x)^3, lower = 1, upper = 3)",
        `7` = "fun <- function(x) x^3 - 5*x^2 + x + sqrt(2)\n x <- round(c(uniroot(fun, c(-1, 0))$root, uniroot(fun, c(0, 1))$root), 2)\nsubmit(x)"
    )
    e$ex <- compile(questions = section_questions[[8]],
                    solutions = solutions,
                    code = code)
}

exercises_[[9]] <- function(e) {
    dimE <- sample(5:10, 2)
    ee <- sample(-10:10, dimE[1] * dimE[2], replace = TRUE)
    f <- sample(-10:10, dimE[1] * dimE[2], replace = TRUE)
    E <- array(ee, dimE)
    G <- array(f, dimE)
    K <- array(c(1, 1, 2, 1, 2, 1), c(3, 2))
    A <- matrix(ee, ncol = dimE[2], nrow = dimE[1])
    e1 <- E
    e1[K] <- 0
    H <- matrix(sample(-10:10, 9, replace = TRUE), 3, 3)
    d1 <- data.frame(pituus = c(175, 180, 190, 174), paino = c(60, 72, 90, 95), luokka = c(2, 2, 1, 1))
    rk1 <- "In order to complete the course, you will need to install R."
    rk2 <- "By removing all spaces, the text clearly becomes more readable!"
    create_string <- function(l = 12) {
        return(paste(sample(c(rep(0:9, each = 5), letters), l, replace = TRUE), collapse = ''))
    }
    len <- round(runif(300, 2, 10))
    stringlist <- character(300)
    for (i in 1:300) {
        stringlist[i] <- create_string(len[i])
    }
    words <- stringlist
    pattern_1 <- "^[4-8]{1}.{3,}$"
    pattern_2 <- "^.*(?=.*\\d)(?=.*[a-z]).*$"
    solutions <- list(
        `1` = array(0, c(3, 3, 3)),
        `2` = 2*E*G + G^2 + 1,
        `3` = det(E %*% t(G)),
        `4` = (det(H) != 0),
        `5` = E[K],
        `6` = e1,
        `7` = matrix(0, nrow = 14, ncol = 3),
        `8` = nrow(A),
        `9` = ncol(A),
        `10` = sort(grep(pattern_1, words, value = TRUE)),
        `11` = sort(grep(pattern_2, words, value = TRUE, perl = TRUE)),
        `12` = gsub("i", "e", rk1),
        `13` = gsub("\\s", "", rk2)
    )
    code <- list(
        `1`  = "x <- array(0, c(3, 3, 3))\nsubmit(x)",
        `2`  = "x <- 2 * E * G + G^2 + 1\nsubmit(x)",
        `3`  = "x <- det(E %*% t(G))\nsubmit(x)",
        `4`  = "x <- (det(H) != 0)\nsubmit(x)",
        `5`  = "x <- c(E[1,1], E[1,2], E[2,1])\nsubmit(x)",
        `6`  = "x <- x <- E\nx[K] <- 0\nsubmit(x)",
        `7`  = "x <- matrix(0, nrow = 14, ncol = 3)\nsubmit(x)",
        `8`  = "x <- nrow(A)\nsubmit(x)",
        `9`  = "x <- ncol(A)\nsubmit(x)",
        `10` = "x <- sort(grep(\"^[4-8]{1}.{3,}$\", words, value = TRUE))\nsubmit(x)",
        `11` = "x <- sort(grep(\"^.*(?=.*\\\\d)(?=.*[a-z]).*$\", words, value = TRUE, perl = TRUE))\nsubmit(x)",
        `12` = "x <- gsub(\"i\", \"e\", rk1)\nsubmit(x)",
        `13` = "x <- gsub(\"\\\\s\", \"\", rk2)\nsubmit(x)"
    )
    e$data <- list(E = E, G = G, H = H, A = A, K = K, words = words, rk1 = rk1, rk2 = rk2)
    data <- vector(mode = "list", length = length(code))
    data[2:3] <- list(list(E = "E", G = "G"))
    data[[4]] <- list(H = "H")
    data[[5]] <- list(E = "E")
    data[[6]] <- list(E = "E", K = "K")
    data[8:9] <- list(list(A = "A"))
    data[10:11] <- list(list(words = "words"))
    data[[12]] <- list(rk1 = "rk1")
    data[[13]] <- list(rk2 = "rk2")
    e$ex <- compile(questions = section_questions[[9]],
                    solutions = solutions,
                    data = data,
                    code = code)
}

exercises_[[10]] <- function(e, write = TRUE) {
    if (write) {
        write.table(auto, file = "automobile.txt", quote = TRUE)
    }
    m1 <- lm(children2$Weight ~ children2$Height + children2$Gender2)
    m2 <- lm(children2$Weight ~ children2$Height * children2$Gender2)
    logreg <- glm(am ~ wt, family = "binomial", data = auto)
    newdat <- data.frame(wt = 2.5)
    logreg2 <- glm(am ~ wt + hp, family = "binomial", data = auto)
    newdat2 <- data.frame(hp = 110, wt = 2.5)
    solutions <- list(
        `1` = DNase,
        `2` = summary(m1)$adj.r.s,
        `3` = FALSE,
        `4` = m2$coef[4],
        `5` = FALSE,
        `6` = predict(logreg, newdat, type = "response"),
        `7` = round(c(logreg$coef[2], sqrt(vcov(logreg)[2,2])), 2),
        `8` = predict(logreg2, newdat2, type = "response"),
        `9` = TRUE
    )
    code <- list(
        `1` = "x <- DNase\nsubmit(x)",
        `2` = "children <- read.table(\"Children2007edit.dat\", header = TRUE)\nm1 <- lm(Weight ~ Height + Gender, data = children)\nx <- summary(m1)$adj.r.s\nsubmit(x)",
        `3` = "summary(m1)\nx <- FALSE\nsubmit(x)",
        `4` = "m2 <- lm(Weight ~ Height * Gender, data = children)\nx <- m2$coef[4]\nsubmit(x)",
        `5` = "anova(m1, m2)\nx <- FALSE\nsubmit(x)",
        `6` = "auto <- read.table(\"automobile.txt\", header = TRUE)\nlogreg <- glm(am ~ wt, family = \"binomial\", data = auto)\nnewdat <- data.frame(wt = 2.5)\nx <- predict(logreg, newdat, type = \"response\")\nsubmit(x)",
        `7` = "x <- round(c(logreg$coef[2], sqrt(vcov(logreg)[2,2])), 2)\nsubmit(x)",
        `8` = "logreg2 <- glm(am ~ wt + hp, family = \"binomial\", data = auto)\nnewdat2 <- data.frame(hp = 110, wt = 2.5)\nx <- predict(logreg2, newdat2, type = \"response\")\nsubmit(x)",
        `9` = "summary(logreg2)\nx <- TRUE\nsubmit(x)"
    )
    e$data <- list(children = children2, auto = auto, m1 = m1, m2 = m2, logreg = logreg, logreg2 = logreg2)
    data <- vector(mode = "list", length = length(code))
    data[3:4] <- list(list(children = "children", m1 = "m1"))
    data[[5]] <- list(children = "children", m1 = "m1", m2 = "m2")
    data[[7]] <- list(auto = "auto", logreg = "logreg")
    data[[8]] <- list(auto = "auto")
    data[[9]] <- list(auto = "auto", logreg2 = "logreg2")
    e$ex <- compile(questions = section_questions[[10]],
                    solutions = solutions,
                    data = data,
                    code = code)
}

exercises_[[11]] <- function(e) {
    fun1 <- "fn1 <- function(x) { x + 3 "
    fun2 <- "fn2 <- function(x) { \n 2 x^2 + exp x \n}"
    fun3 <- "fn3 <- function(A, i) { \n ord <- order(A[ ,i]) \n A[ ,ord] \n}"
    fun4 <- "fn4 <- function(e) { \n s <- 1 \n while(e > 0) { \n   e - 1 \n   s <- s/2 \n  } \n s \n}"
    d <- 2
    B <- matrix(sample.int(24, 12), 4, 3)
    g <- 2
    i <- 3
    A1 <- matrix(sample.int(24, 12), 4, 3)
    A2 <- matrix(sample.int(30, 20), 10, 2)
    A3 <- matrix(sample.int(6, 4), 2, 2)
    i1 <- 3
    i2 <- 1
    i3 <- 2
    m <- sample(3:50, 1)
    points <- list(p1 = list(n = m), p2 = list(n = 1), p3 = list(n = 2), p4 = list(n = 3))
    ind <- matrix(c(sample(1:10, 6, replace = TRUE), sample(1:10, 6)), ncol = 2)
    ind <- ind[order(ind[ ,1], ind[ ,2]), ]
    A <- 0 * diag(10)
    A[ind] <- 1
    B <- 0 * diag(10)
    B[!ind] <- 1
    test_input <- list(
        `1` = list(p1 = list(x = -1), p2 = list(x = 0), p3 = list(x = 1)),
        `2` = list(p1 = list(x = -1), p2 = list(x = 0), p3 = list(x = 1)),
        `3` = list(p1 = list(A = A1, i = i1), p2 = list(A = A2, i = i2), p3 = list(A = A3, i = i3)),
        `4` = list(p1 = list(e = 1), p2 = list(e = 2), p3 = list(e = 3), p4 = list(e = 7)),
        `5` = points,
        `6` = list(p1 = list(A = A), p2 = list(A = B))
    )
    solutions <- list(
        `1` = function(x) x + 3,
        `2` = function(x) 2 * x^2 + exp(x),
        `3` = function(A, i) {
            ord <- order(A[,i])
            A[ord,]
        },
        `4` = function(e) {
            s <- 1
            while(e > 0) {
                e <- e - 1
                s <- s/2
            }
            s
        },
        `5` = function(n) {
            fib <- numeric(n + 2)
            fib[1] <- 1
            fib[2] <- 1
            if (n > 2) {
                for (i in 3:n) {
                    fib[i] <- fib[i-1] + fib[i-2]
                }
            }
            fib[1:n]
        },
        `6` = function(A) {
            B <- which(A == 1, arr.ind = TRUE)
            B[order(B[,1]),]
        }
    )
    code <- list(
        `1` = "fn1 <- function(x) x + 3\nsubmit(fn1)",
        `2` = "fn2 <- function(x) 2*x^2 + exp(x)\nsubmit(fn2)",
        `3` = "fn3 <- function(A, i) {\n    ord <- order(A[,i])\n    A[ord,]\n}\nsubmit(fn3)",
        `4` = "fn4 <- function(e) {\n    s <- 1\n    while(e > 0) {\n        e <- e - 1\n        s <- s/2\n    }\n    s\n}\nsubmit(fn4)",
        `5` = "fibonacci <- function(n) {\n    fib <- numeric(n + 2)\n    fib[1] <- 1\n    fib[2] <- 1\n    if (n > 2) {\n        for (i in 3:n) {\n            fib[i] <- fib[i-1] + fib[i-2]\n        }\n    }\n    fib[1:n]\n}\nsubmit(fibonacci)",
        `6` = "connections <- function(A) {\n    B <- which(A == 1, arr.ind = TRUE)\n    B[order(B[,1]),]\n}\nsubmit(connections)"
    )
    e$data <- list(fun1 = fun1, fun2 = fun2, fun3 = fun3, fun4 = fun4, d = d, B = B, i = i, g = g)
    data <- vector(mode = "list", length = length(code))
    data[[1]] <- list(fun1 = "fun1")
    data[[2]] <- list(fun2 = "fun2", d = "d")
    data[[3]] <- list(fun3 = "fun3", B = "B", i = "i")
    data[[4]] <- list(fun4 = "fun4", g = "g")
    params <- lapply(1:length(solutions), function(i) {
        list(is_function = TRUE,
             test_input = test_input[[i]])
    })
    e$ex <- compile(questions = section_questions[[11]], 
                    solutions = solutions, 
                    data = data,
                    code = code,
                    params = params)

}
