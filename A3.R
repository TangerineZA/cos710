library("gramEvol")

rule_def <- list(
    expr    = grule(op(expr, expr), var, n),
    op      = grule("+", "-", "*", "/"),
    var     = grule(Distance, Pickup_longitude,
     Pickup_latitude, Haversine, Pmonth, Pickup_day, Pickup_hour,
     Pickup_minute, Pickup_weekday, Dropoff_hour, Dropoff_minute,
     Temp, Precip, Wind, Humid, Solar, Snow, Dust),
    n = grule(1, 2, 3, 0.5)
)

print("Loading in dataset...")

training_dataset <- read.csv("training_cleaned.csv")

Duration <- c(training_dataset$Duration)
Distance <- c(training_dataset$Distance)
Pickup_longitude <- c(training_dataset$Pickup_longitude)
Pickup_latitude <- c(training_dataset$Pickup_latitude)
Haversine <- c(training_dataset$Haversine)
Pmonth <- c(training_dataset$PMonth)
Pickup_day <- c(training_dataset$Pickup_day)
Pickup_hour <- c(training_dataset$Pickup_hour)
Pickup_minute <- c(training_dataset$Pickup_minute)
Pickup_weekday <- c(training_dataset$Pickup_weekday)
Dropoff_hour <- c(training_dataset$Dropoff_hour)
Dropoff_minute <- c(training_dataset$Dropoff_minute)
Temp <- c(training_dataset$Temp)
Precip <- c(training_dataset$Precip)
Wind <- c(training_dataset$Wind)
Humid <- c(training_dataset$Humid)
Solar <- c(training_dataset$Solar)
Snow <- c(training_dataset$Snow)
Dust <- c(training_dataset$Dust)

print("Starting GE...")

grammar_def <- CreateGrammar(rule_def)
print(grammar_def)

sym_reg_fit_func <- function(expr) {
    result <- eval(expr)

    if (any(is.nan(result))) {
        return(Inf)
    }

    resultset <- (mean((abs((Duration - result)))))
    resultset[is.nan(resultset)] <- Inf
    resultset[is.na(resultset)] <- Inf
    return(resultset)
}

times <- list()
best_algs <- list()


for (x in 1:10) {
    ptm <- proc.time()
    ge <- GrammaticalEvolution(grammarDef = grammar_def,
                                evalFunc = sym_reg_fit_func,
                                terminationCost = NA,
                                monitorFunc = print,
                                iterations = 40,
                                popSize = 48,
                                numExpr = 1,
                                mutationChance = 0.65,
                                startSymb = GrammarStartSymbol(grammar_def),
                                wrappings = 3,
                                suggestions = NULL,
                                optimizer = c("ga"),
                                elitism = 1,
                                plapply = lapply,
                                )
    elapsed <- proc.time() - ptm

    append(times, elapsed)
    append(best_algs, ge)

    sink("out.txt", append = TRUE)
    print("GE:")
    print(ge)
    print("Time:")
    print(elapsed)
    sink()
}