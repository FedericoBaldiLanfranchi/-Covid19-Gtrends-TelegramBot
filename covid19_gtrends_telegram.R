library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)
library(gtrendsR)
library(RColorBrewer)
library(tseries)
library(stats)
library(forecast)
library(parallel)
library(telegram.bot)
library(gridExtra)

## Set working directory
wd <- dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(wd)

## Colors
colorMap    <- brewer.pal(8,"RdBu")

# ================================= Custom Functions =================================


## Downloades and preprocess Covid19 ts data from the CSSEGIS repository. Pick:
## series = c for Confirmed cases [Default]
## series = d for Death tolls
## series = r for Recovered patients
fbl_get_covid_data <- function(series = "c") {
  
  ## Base url of the raw CSSEGIS repository
  url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
  
  ## Determine file to download from CSSEGIS repository
  if     (series == "c")  subpath <- "time_series_covid19_confirmed_global.csv" 
  else if(series == "d")  subpath <- "time_series_covid19_deaths_global.csv" 
  else if(series == "r")  subpath <- "time_series_covid19_recovered_global.csv" 
  
  ## Throw error if unknown file requested
  else stop("Unknown series requested. Pick:
            -c for Confirmed cases
            -d for Death tolls
            -r for Recovered patients")
  
  
  ## Complete URL is:
  url <- paste0(url, subpath)
  
  ## Read raw time series from .csv to data.table
  ts <- fread(url)
  
  ## Discard information on Province and Geographic Coordinates
  ## (Too detailed for the scope of this analysis)
  ts <- ts[, -c("Province/State", "Lat", "Long")]
  
  ## For some Countries, regional information is provided next to the name
  ## in brackets. Remove this.
  ts[, `Country/Region` := str_replace(`Country/Region`, " \\(.*\\)", "")]
  
  ## Get totals by country and melt data into long format 
  ts <- ts %>% group_by(`Country/Region`) %>% summarise_all(sum)
  ts <- ts %>% melt(id.vars = "Country/Region",  variable.name = "Date", value.name = "Value")
  
  ## Parse date (strings) to standard format
  dates <- str_split(ts$Date, "\\/")
  dates <- sapply(dates, function(x) paste0(formatC(as.numeric(x), width = 2, flag = 0), collapse = "/"))
  
  ## Convert date strings to date format and tidy up column names
  setDT(ts)[, Date := as.Date(dates, format = "%m/%d/%y")]
  setnames(ts, "Country/Region", "Country")
  
  ## Fix some naming incontencies
  ts[Country == "US"     , Country := "United States"]
  ts[Country == "Taiwan*", Country := "Taiwan"]
  
  ## Add log transformed values
  ts[, logValue  := log(Value + 1)]
  
  ## Add Changes (on both scales)
  ts[, Change    := Value    - shift(Value)   , by = Country]
  ts[, logChange := logValue - shift(logValue), by = Country]

  ts <- na.omit(ts)
  
}


## Combine Google Country list with Countries from CSSEGIS repository
fbl_get_country_list <- function(covidData) {
  
  ## Retrieve Country list and relative codes from Google 
  data("countries")
  
  ## Uniform some Country Names to CSSEGIS repository
  setDT(countries)[name == "KOREA (NORTH)", name := "KOREA, NORTH"]
  countries[name == "KOREA (SOUTH)"       , name := "KOREA, SOUTH"]
  countries[name == "RUSSIAN FEDERATION"  , name := "RUSSIA"]
  
  ## Discard subregional codes
  countries <- countries[sub_code == "", .(country_code,name)]
  countries <- countries[!grep("&|=|,|\\(", name),]
  countries[, name := fbl_capitalize_all(name)]
  countries <- na.omit(countries)
  
  ## Restrict attention to Countries for which we have a match in Covid19 dataset
  countries <- countries[name %in% covidData$Country,]
  
  return(countries)
  
}


## Downloades and preprocess Google Trends search data. Pick
## source = o for online data [Default]
## source = l for local, predownloaded data
## ----
## overwrite = T to overwrite local data with download
## overwrite = F to not to overwrite local data [Default]
fbl_get_search_data <- function(file, source = "o", overwrite = F, date_vec = NULL, country_df = NULL) {
  
  
  ## Local data is loaded if requested
  if(source == "l") {
    
    load(paste0("Data/", file))
    return(gtsd)
    
  } else if(source == "o") {
    
    if(is.null(date_vec)) stop("date_vec not found.
                               Vector of dates required to gather data from Google Trends")
    
    ## Throw error if unknown file requested  
  } else stop("Unknown source requested. Pick:
            -o for online data
            -l for online data")
  
  ## Google Trends provides daily time series only for periods of 270 days or less
  end_date   <- min(fbl_yesterday(), max(date_vec))
  start_date <- max(min(date_vec)  , end_date - 270)
  
  ## Number of contries to download data for
  numcountries <- nrow(country_df)
  countrynames <- country_df$name
  countrycodes <- as.character(country_df$country_code)
  
  ## Declare empty list to store data
  gtsd <- vector("list")
  
  for(i in seq_along(countrycodes)) {
    
    name <- country_df$name[i]
    code <- countrycodes[i]
    
    ## Perform data call
    gtsd[[name]] <- gtrends(keyword           = URLdecode("%2Fm%2F01cpyy"),
                            time              = paste0(start_date, " ", end_date),
                            geo               = code,
                            low_search_volume = T, 
                            onlyInterest = T)$interest_over_time
    
    ## Print Progress:
    print(paste0("Search Data Downloaded for ", name, " (", i, " of ", numcountries, " complete)."))
    
    ## Sleep to space out requests so that Google does not block the query 
    Sys.sleep(10)
    
  }
  
  gtsd <- rbindlist(gtsd, idcol = "Country")
  
  ## Set <1 to 0
  gtsd[hits == "<1", hits := "0"]
  gtsd[, hits := as.numeric(hits)]
  
  ## Subset and tidy up
  gtsd[, Series := "hits"]
  gtsd[, date := as.Date(date)]
  gtsd <- gtsd[, .(Series, date, Country, hits)]
  colnames(gtsd) <- c("Series", "Date", "Country", "Value")
  
  ## Add log transformed values
  gtsd[, logValue  := log(Value + 1)]
  
  ## Add Changes (on both scales)
  gtsd[, Change    := Value    - shift(Value)   , by = Country]
  gtsd[, logChange := logValue - shift(logValue), by = Country]
  
  ## Drop NAs
  gtsd <- na.omit(gtsd)
  
  ## Overwrite file if requested
  if(overwrite == T) save(gtsd, file = paste0(wd, "/Data/", file))
  
  return(gtsd)
  
}


## Compute Current Cases [C - (D + R))] 
fbl_compute_current_cases <- function(covidData) {
  
  ## Current Cases
  current <- dcast(covidData[, .(Series, Country, Date, Value)], "Date + Country ~ Series", value.var = "Value")
  current[, Value := confirmed - deaths - recovered]
  current[, Series := "current"]
  current <- current[, .(Series, Country, Date, Value)]
  
  ## Add log transformed values
  current[, logValue  := log(Value + 1)]
  
  ## Add Changes (on both scales)
  current[, Change    := Value    - shift(Value)   , by = Country]
  current[, logChange := logValue - shift(logValue), by = Country]
  
}


## Combine Google and CSSEGIS data sources
fbl_combine_data <- function(covidData, googleData) {
  
  ## Subset relative changes from both tables
  covidData  <- covidData[,  .(Country, Date, logChange)]
  googleData <- googleData[, .(Country, Date, logChange)]
  setnames(googleData, "logChange", "logdHits")
  
  ## Merge and melt to long format
  combined <- merge(covidData, googleData) %>% 
     melt(id.vars  = c("Country", "Date"), 
     variable.name = c("Series"), 
     value.name    = c("logChange"))
  
  ## Sort
  combined <- combined[order(Country, Series, Date)]
  
  ## Drop NAs
  combined <- na.omit(combined)
  
  return(combined)
  
}


## Compute pairwise correlations between search data change and current case change
## for each country in the input data.frame
fbl_pairwise_correlations <- function(df, method = "p", lag = 0, sort = F) {
  
  ## Error handling
  if(!fbl_is.int(lag)) stop("'lag' should be a positive, integer parameter specifying the power of the lag operator")
  else if(lag < 0) stop("The forward operator is not supported. Please select a positive lag or 0 for no lag")
  
  
  country_names <- unique(df$Country)
  
  covid   <- df[Series == "logChange",]
  google  <- df[Series == "logdHits", ]
  
  
  ## Lag values if requested
  if (lag > 0) google[, logChange := shift(logChange, lag), by = Country]
 
  
  ## For each Country...
  corr <- lapply(country_names, function(x) {
    
    ## ... Compute and test correlation between logChanges in search hits and current cases
    test <- cor.test(google[Country == x]$logChange, 
                     covid[Country == x]$logChange, 
                     use = "pairwise.complete.obs", 
                     method = method)
    
    ## ... Round output to 3 digits
    round(with(test, data.frame(rho = estimate, t = statistic, p = p.value)), 3)
  
  })
  
  corr <- cbind(Country = factor(country_names, country_names), rbindlist(corr))
  
  if(sort == T) corr <- corr[order(-rho)]
  
  return(corr)
  
}


## Get Info Criteria of best ARIMA (baseline) and best augmented ARIMA (with gtrends data)
fbl_arima <- function(df, country_names) {
  
  ## Separate data sources
  covid   <- df[Series == "logChange",]
  google  <- df[Series == "logdHits", ]
  
  ## Save last gtrends observation for forecasting
  googleLast <- google[, last(logChange), by = Country]
    
  ## Lag gtrends predictors
  google[, logChange := shift(logChange, 1), by = Country]

  forecast_df <- lapply(country_names, function(x) {
    
    ## Subset country-relevant obs
    subcovid    <- covid[Country == x , logChange] 
    subgoogle   <- google[Country == x, logChange]
    gpredictor  <- googleLast[Country == x, V1]
    
    ## Pick best ARIMA models according to the AIC information criterion
    baseline <- auto.arima(subcovid, test = "p", ic = "aic")
    google   <- auto.arima(subcovid, test = "p", ic = "aic", xreg = subgoogle)

    ## Forecast 1 day forward
    bForecast <- as.data.frame(forecast(baseline, 1))[, c("Point Forecast", "Lo 95", "Hi 95")]
    gForecast   <- as.data.frame(forecast(google, h= 1, xreg = tail(subgoogle, 1)))  [, c("Point Forecast", "Lo 95", "Hi 95")]

    colnames(bForecast) <- c("bPoint", "bLo95", "bHi95")
    colnames(gForecast) <- c("gPoint", "gLo95", "gHi95")
    
    ##
    ic <- data.frame(Baseline  = AIC(baseline), 
                     Augmented = AIC(google))
    
    out <- cbind(ic, bForecast, gForecast)
    out <- round(out, 3)   
    out
  
  })
  
  ## Add country names and identify best model by country
  forecast_df <- cbind(Country = country_names, rbindlist(forecast_df))
  forecast_df[, Best := ifelse(Augmented <= Baseline, "Augmented", "Baseline")]
  
  
  ## Tidy up: Merge point estimates and relative confidence intervals in the same cell
  forecast_df[, `Augmented Forecast` := paste0(fbl_format_num(gPoint, T), " ", "(", fbl_format_num(gLo95, T), ", ", fbl_format_num(gHi95, T), ")")]
  forecast_df[, `Baseline Forecast`  := paste0(fbl_format_num(bPoint, T), " ", "(", fbl_format_num(bLo95, T), ", ", fbl_format_num(bHi95, T), ")")]
  forecast_df <- forecast_df[, .(Country, Baseline, Augmented, Best, `Baseline Forecast`, `Augmented Forecast`)]
  setnames(forecast_df, c("Baseline", "Augmented"), c("Baseline AIC", "Augmented AIC"))
  
  
  return(forecast_df)
  
}


## Allows number formatting with both thousands separator and prepended + sign
fbl_format_num <- function(num, sign = F) {
  
  ## Initialise vector of signs to prepend
  prep <- rep("", length(num))

  ## Replace empty slots with + signs where needed
  if(sign == T) prep[which(num > 0L)] <- "+" else prep <- ""
  
  ## 
  num <- paste0(prep, formatC(num, big.mark = "'", format = 'f', digits = ifelse(fbl_is.int(first(num)), 0, 3)))
  
  return(num)
  
}


## To title case from factor or uppercase
fbl_capitalize_all <- function(vec) {
  
  tools::toTitleCase(tolower(as.character(vec)))
  
}


## Get yesterday's date 
fbl_yesterday <- function() {
  
  Sys.Date() - 1
  
}


## R does not have a built in function to verify is an input number is int
## one possible solution is the following
fbl_is.int <- function(num) {
  
  return(all.equal(num, as.integer(num)) == T)
  
}



# ================================= Collect and Organise Data =================================


## Get list of Covid19 time series
data <- list(confirmed = fbl_get_covid_data(series = "c"),
             deaths    = fbl_get_covid_data(series = "d"),
             recovered = fbl_get_covid_data(series = "r"))
 
## Get panel to long
data <- rbindlist(data, idcol = "Series")

## Compute current cases by country
current <- fbl_compute_current_cases(data)

## Fetch Country list
countries <- fbl_get_country_list(data)


# ================================= Subset Analysis =================================


## Subset a Group of Countries severely impacted by the Virus and were Google is the go-to search engine
country_subs  <- c("Italy", "Brazil", "United States", "Spain", "France", "United Kingdom")

## Get Google Trends data for the subset
gtsd_subs <- fbl_get_search_data(file = "covid19_search_data_subs.rda",
                                 source = "o", 
                                 overwrite = T, 
                                 date_vec = data$Date, 
                                 country_df = countries[name %in% country_subs])

## Combine Data Sources
subs          <- fbl_combine_data(current[Country %in% country_subs], gtsd_subs)


## Compare Dynamics in Google Search data and Search Hits for Covid19 related topics
six_gt_plot <- ggplot(subs) + 
  geom_line(aes(x = Date, y = logChange, color = Series, linetype = Series), size = 0.35) + 
  facet_wrap(~ Country, scales = "free_y") +
  theme_bw() +
  scale_color_manual(values = colorMap[c(1, 8)], labels = c("Current Cases", "Search Hits")) +
  scale_linetype_manual(values = c(1, 6), labels = c("Current Cases", "Search Hits")) + 
  theme(legend.position = "bottom") + 
  labs(x = "Date [days]", y = "Log Change")

## Save and plot
ggsave("Img/six_gt_plot.png", six_gt_plot)
plot(six_gt_plot)

subset_cor_table <- fbl_pairwise_correlations(subs)
arima_subs       <- fbl_arima(subs, country_subs)


# ================================= Full Sample =================================


## Get Google Trends data [Local data source to save time]
## Decomment the next code line to update with online data[~30min ETA]
gtsd <- fbl_get_search_data(file = "covid19_search_data.rda", source = "l", date_vec = data$Date, country_df = countries)

## Decomment to update Google Trends data for all countries[~30min ETA]
## gtsd <- fbl_get_search_data(source = "o", overwrite = T, date_vec = data$Date, country_df = countries)


## Combine Data Sources
combined <- fbl_combine_data(current, gtsd)

## Compute Correlations and color code cases: 
## - Positive
## - Negative
## - Insignificant
cor_table <- fbl_pairwise_correlations(combined, lag = 0, sort = T)
cor_table[, Color := ifelse(p < 0.05, ifelse(rho >= 0, "Positive", "Negative"), "Insignificant")]


gtcorr_plot <- ggplot(cor_table, aes(x = factor(Country, Country), y = rho)) + 
  geom_bar(aes(fill = Color), stat = "identity", position = "dodge") + 
  theme_bw() +
  theme(axis.text.y = element_text(size  = 4),
        axis.title = element_text(size = 10),
        legend.position = c(0.99, 0.99),
        legend.justification = c(0.99, 0.99),
        legend.background = element_rect(linetype = "solid", color = "black", size = 0.5)) +
  coord_flip() +
  scale_fill_brewer() +
  labs(x = "Country", y = "Correlation Coefficient [Pearson]", fill = "Sign") 

## Save and plot
ggsave("Img/gtcorr_plot.png", gtcorr_plot, width = 5, height = 9)
plot(gtcorr_plot)

# ================================= Telegram Bot =================================

## Unique Token for this project's bot
token <- "1207107400:AAG_E7EXbVkAXMvwEvrZLMEZXhYiDRbFFxE"

## Init Telegram bot
bot <- Bot(token = token) 

## Greet new user on bot activation (\start)
start         <- function(bot, update) {
  
  
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = paste0("Hello ", update$message$from$first_name, 
                                 "!\nThis bot supports Covid19 statistics and analytics by country.\nIt also employs google trends search volume as a measure of attention payed by users to Covid19-related topics. Covid19 time series data the bot builds on is part of the John Hopkins University public repository.\n*Type* /help *for a list of supported commands.*"),
                 parse_mode = "Markdown")


}

## Inform user on available commands
help          <- function(bot, update) {
  
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = "This bot supports the following commands:
/start - Initialise and activate the bot
/countrylist - Output a list of supported countries (case sensitive), which can be passed as inline arguments to /stats and /tsplot
/stats - [i.e. /stats United States] Given an argument country, output statistics on current and total cases, deaths and recovered patients
/tsplot - [i.e. /tsplot United States] Given an argument country, plot total cases, deaths and recovered patients over time
/sixplot - Plot dynamics in current cases against dynamics in Google search volume (top 6 countries by deaths)
/sixgtcorr - Output correlation coefficients between log changes in current cases and lagged log changes in Google search volume (top 6 countries by deaths)
/gtcorrall -  Plot correlation coefficients between log changes in current cases and log changes in Google search volume (all countries)
/sixforecasts - Does Google Trends data improve forecasting accuracy wrt relative changes in current cases on the next day? Provide summary statistics and forecasts (top 6 countries by deaths)")
  
}

## Error handling when the bot receives an unknown command
unknown       <- function(bot, update){
  
  id      <- update$message$chat_id
  
  bot$sendMessage(chat_id = id,
                  text = "Sorry, that command is not supported. Please check /help for a list of available commands.")
}

## List of counties supported by /stats and /tsplot
countrylist   <- function(bot, update) {
  
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = paste0("*Supported Countries:*\n", paste0(countries$name, collapse = "\n")),
                  parse_mode = "Markdown")
  
}

## Covid statistics for input country
stats         <- function(bot, update, country) {
  
  id      <- update$message$chat_id
  country <- paste0(country, collapse = " ")
  
  if(country %in% data$Country == F) {
  
    bot$sendMessage(id, "Error: Input country not in Country list.\nType /countrylist for a list of supported countries.\nType /help to review supported commands.")
    return(NULL)
    
}
  
  last_date <- max(data$Date)
  countrydata <- data[Country == country & Date == last_date, .(Value, Change), by = Series]
  
  confirmed <- countrydata[Series == "confirmed", -"Series"]
  deaths    <- countrydata[Series == "deaths", -"Series"]
  recovered <- countrydata[Series == "recovered", -"Series"]
  current   <- confirmed - deaths - recovered
  
  
  msg <- paste0("Covid19 statistics in *", country, "* as of ", last_date, ":\n 
*Current Cases*: ", fbl_format_num(current$Value), " (", fbl_format_num(current$Change, T), ")
*Deaths*: ", fbl_format_num(deaths$Value)   , " (", fbl_format_num(deaths$Change, T)   , ")
*Recovered*: ", fbl_format_num(recovered$Value), " (", fbl_format_num(recovered$Change, T), ")\n
*Total Cases*: ", fbl_format_num(confirmed$Value), " (", fbl_format_num(confirmed$Change, T), ")")
  
  
  bot$sendMessage(chat_id = id,
                  text = msg,
                  parse_mode = "Markdown")
  
}

## Covid statistics plot for input country 
tsplot        <- function(bot, update, country) {
  
  id      <- update$message$chat_id
  country <- paste0(country, collapse = " ")
  
  if(country %in% data$Country == F) {
    
    bot$sendMessage(id, "Error: Input country not in Country list.\nType /countrylist for a list of supported countries.\nType /help to review supported commands.")
    return(NULL)
    
  }
  
  plot <- ggplot(data[Country == country], aes(x = Date, y = Value, color = Series)) +
    geom_line() +
    scale_color_manual(values = colorMap[c(1, 6, 8)]) +
    theme_bw() +
    labs(x = "Date [days]", y = "", title = paste0("Covid19 in ", country)) +
    theme(legend.position = "bottom")
    
  
  ggsave("Img/temp.png", plot)
  
  bot$sendPhoto(chat_id = id,
                photo = "Img/temp.png")
  
}

## Plot dynamics in current cases against dynamics in Google search volume (top 6 countries by deaths)
sixplot       <- function(bot, update) {
  
  id      <- update$message$chat_id
  
  
  bot$sendPhoto(chat_id = id,
                photo = "Img/six_gt_plot.png",
                caption = "Log Changes in Current Cases and Google Search Hits for Covid19 related topics (top 6 Countries by deaths as of 2020-05-22)")
  
}

## Output correlation coefficients between log changes in current cases and lagged log changes in Google search volume (top 6 countries by deaths)
sixgtcorr     <- function(bot, update) {
  
  id      <- update$message$chat_id
  
  out <- tableGrob(subset_cor_table, rows = NULL)
  
  ggsave("Img/temp.png", out, height = 3, width = 4)
         
  bot$sendPhoto(chat_id = id,
                photo   = "Img/temp.png",
                caption = "Pearson Correlation [rho], attached t-statistics [t], and p-values [p] between log changes in Current Cases and lagged log changes in Google Search Hits for Covid19 related topics (top 6 Countries by deaths as of 2020-05-22).")
                
}

## Plot correlation coefficients between log changes in current cases and log changes in Google search volume (all countries)
gtcorrall     <- function(bot, update) {
  
  id      <- update$message$chat_id
  
  bot$sendPhoto(chat_id = id,
                photo   = "Img/gtcorr_plot.png",
                caption = "Pearson Correlation between Log Changes in Current Cases and Google Search Hits")
  
}

## Does Google Trends data improve forecasting accuracy wrt relative changes in current cases on the next day? Provide summary statistics and forecasts (top 6 countries by deaths)
sixforecasts  <- function(bot, update) {
  
  id  <- update$message$chat_id
  
  ## Get image from table since Telegram offers limited support for plain tables
  out <- arima_subs
  out <- tableGrob(out, rows = NULL)
  
  ggsave("Img/temp.png", out, height = 3, width = 9)
  
  bot$sendPhoto(chat_id = id,
                photo = "Img/temp.png",
                caption = "Does Google Trends data help when forecasting log changes in current cases? Comparison between best ARMA model (AIC criterion): Baseline (plain ARMA) vs Augmented (includes lagged log change in Search Volume). 95% confidence intervals in brackets.")
  
  
}


## Build the updater and pass commands
updater <- Updater(token = token) +
  CommandHandler("start", start) +
  CommandHandler("countrylist", countrylist) +
  CommandHandler("stats", stats, pass_args = T) +
  CommandHandler("tsplot", tsplot, pass_args = T) +
  CommandHandler("sixplot", sixplot) +
  CommandHandler("sixgtcorr", sixgtcorr) +
  CommandHandler("gtcorrall", gtcorrall) +
  CommandHandler("sixforecasts", sixforecasts) +
  CommandHandler("help", help) +
  MessageHandler(unknown, MessageFilters$command)


## Start polling
updater$start_polling()

## Stop execution (within R or RStudio) and then Stop polling
updater$stop_polling()

