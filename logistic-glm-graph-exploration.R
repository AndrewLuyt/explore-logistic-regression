# Explore 1-variable logistic regression by allowing a user to interactively
# choose slope and intercept coefficients for the linear component of a
# logistic model, using the manipulate library.
# Plots of the linear function and the associated logistic function are created,
# along with dotted plots for the detault coefficients of intercept=0
# and slope=1.
# A single point at x=1 is plotted on the logistic function to illustrate how
# changing coefficients defines different probability distributions at a given x.
# The plots are done using ggplot. Patchwork is used to arrange them side by side.

library(manipulate)
library(tidyverse)
library(patchwork)

logistic <- function(log.odds) {
    exp(log.odds) / (1 + exp(log.odds))
}

logit <- function(p) {
    log(p / (1-p))
}

# the linear component
f <- function(x, b0, b1) {
    b0 + b1*x
}

x <- seq(-10,10,0.05)  # data range

# This function is called every time the sliders from the manipulate library
# send an event for a changing coefficient.
# A default plot for slope 1, intercept 0, is plotted in grey for comparison.
myLogisticPlot <- function(b0, b1)  {
    df <- tibble(
        x=x,
        default.log.odds = f(x, 0, 1),
        default.p = logistic(default.log.odds),
        log.odds = f(x, b0, b1), # linear func predicts the log odds
        p = logistic(log.odds)
    )
    head(df)

    round.b0 <- round(b0, 1)
    round.b1 <- round(b1, 2)
    func.linear = paste0("log odds = ", round.b0, " + ", round.b1, "x")
    func.logistic = paste0("probability = logistic( ", round.b0, " + ", round.b1, "x )")

    linear.plot <- ggplot(df, aes(x, log.odds)) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
        geom_line(aes(x, default.log.odds), lty=2) +
        geom_line(col='blue') +
        labs(title = func.linear) +
        theme_bw()

    # annotate one point to show the probabilty at x=1
    ref.x = 1
    ref.y <- logistic(f(ref.x, b0, b1))
    logistic.plot <- ggplot(df, aes(x, p)) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
        geom_line(aes(x, default.p), lty=2) +
        geom_line(col='blue') +
        labs(title = func.logistic) +
        geom_point(data=tibble(x=ref.x, p = ref.y)) +
        annotate(geom = "text", x=ref.x+4.6, y=ref.y,
                 label=paste0("x = 1: Bernoulli(p = ", round(ref.y, 2), ")")) +
        theme_bw()

    linear.plot + logistic.plot
}

# The bug mentioned below seems to have been fixed?
# there was some sort of bug in manipulate that hid the gear icon. Running this
# manipulate call first makes the gear show up and only THEN does the
# gear icon show in the graph WE plotted.
# https://stackoverflow.com/questions/62982030/slider-doesnt-show-up-as-i-use-manipulate-function
# manipulate(plot(1:5, cex=size), size = slider(0.5,10,step=0.5))
manipulate(myLogisticPlot(b0 = intercept, b1 = slope),
           intercept=slider(-3, 3, step=.5, initial = 0),
           slope=slider(-2, 2, step=.05, initial = 1))
