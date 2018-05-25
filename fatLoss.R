Run <- function() {
  library(ggplot2)
  library(plotly)
  library(lubridate)
  library(gridExtra)
  
  targetFat <- 0.12
  bodyFat <- 0.174
  weights <- c(193.6, 193.2, 189, 192.4, 192.2, 189.8, 188, 187.8, 189, 189.4, 191.6, 190.8, 190, 188.6,
               191, 188.8, 187.6, 190.2, 188, 185.4, 184, 185, 187, 186.4, 187.4, 187.8, 188.2, 186.4, 185.6,
               186.4, 185, 185.6, 185, 184.4, 184.6, 185)
  dates <- seq(from=as.Date("2018-04-19"), by="1 day", length.out = length(weights))
  day <- lubridate::wday(dates)
  
  BodyFatPct <- function(wgt) {
    lastWgt <- tail(weights,1)
    fat <- lastWgt * bodyFat
    muscle <- lastWgt - fat
    pmax(0, (wgt-muscle)/wgt)
  }
  now <- tail(weights,1)
  possibleWgts <- sort(c(160:195, now))
  lab <- factor(ifelse(possibleWgts==now, "current", "path"))
  pcts <- BodyFatPct(possibleWgts)
  dpcts <- c(NA, diff(pcts)/diff(possibleWgts))
  plt.actfat <- ggplot(mapping=aes(x=dates, y=BodyFatPct(weights))) + 
    xlab("Date") + ylab("Body Fat %") +
    ggtitle("Fat Pct Over Time") + scale_y_continuous(labels=function(x) sprintf("%0.1f%%", 100*x)) + 
    geom_point() + geom_smooth(method="lm")
  plt.fat <- ggplot(mapping=aes(x=possibleWgts, y=pcts, color=lab)) + 
    xlab("Weight") + ylab("Body Fat %") +
    ggtitle("Fat Pct by Weight") + scale_y_continuous(labels=function(x) sprintf("%0.1f%%", 100*x)) + geom_point()
  plt.dfat <- ggplot(mapping=aes(x=possibleWgts, y=dpcts, color=lab)) + xlab("Weight") + ylab("Delta Body Fat %") +
    ggtitle("Delta Fat Pct by Weight") + scale_y_continuous(labels=function(x) sprintf("%0.2f%%", 100*x)) + geom_point()
  
  HowMuchToLose <- function(goalFatPct, startWgt, startFatPct) {
    startFat = startWgt * startFatPct
    nonFatStart = startWgt - startFat
    goalWeight = nonFatStart/(1-goalFatPct)
    max(0, startWeight - goalWeight)
  }
  
  FinishLine <- function(goalFat, startWgt, startFat, deltaPerWeek=-1.5) {
    howMuch = HowMuchToLose(goalFat, startWgt, startFat)
    Sys.Date() - 7/deltaPerWeek * howMuch
  }
  
  s <- summary(lm(weights ~ seq_along(weights)))
  #print(s)
  ratePerWeek <- coef(s)[2,"Estimate"] * 7
  
  howMuch <- HowMuchToLose(targetFat, tail(weights,1), bodyFat)
  doneDate <- FinishLine(targetFat, tail(weights,1), bodyFat, ratePerWeek)
  strPat <- paste(c("You are at %0.1flbs (%0.1f%% fat).  You have lost %0.1flbs.",
                    "Your target fat is %0.1f%%, which means an end weight of %0.1f.",
                    "You are losing %0.2f pounds per week.",
                    "You have %0.1flbs left to lose, with a target date of %s (%2f days).\n"), collapse="\n")
  cat(sprintf(strPat, now, bodyFat*100, head(weights,1)-now, 
              targetFat*100, now-howMuch, -ratePerWeek, howMuch, 
              as.character(doneDate), as.numeric(doneDate - Sys.Date())))
  
  plt.wgt <- ggplot(mapping=aes(x=dates, y=weights)) + geom_point() + expand_limits(y=c(180, 195)) + 
    ggtitle("Weight over Time") + stat_smooth(method="lm")
  
  grid.arrange(plt.actfat, plt.fat, plt.dfat, plt.wgt)
}

suppressWarnings(Run())
