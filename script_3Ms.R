x3m$min <- x[last(x) == min(last(x)) , ]
x3m$med <- x[last(x) == med(last(x)) , ]
x3m$max <- x[last(x) == max(last(x)) , ]

data <- data.frame(stack(x[1:NCOL(x)]))
data$id <- as.character(rep(seq(1, NROW(x)), NCOL(x)))
colnames(data) <- c("Growth", "Simulation", "Day")