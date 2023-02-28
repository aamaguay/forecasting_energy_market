setwd("/home/user/Desktop/files_desktop/forecasting_energy_market")
source('other_test_exe/functions_d2c.R')
#region
# %%
pw <- "#q6a21I&OA5k"
library(conflicted)
library(tidyr)
library(RMariaDB)
library(dplyr)
library(dbx)
library(stringr)
library(lubridate)
library(readr)
library(shiny)
library(data.table)
library(zoo)
library(dbplyr)
library(data.table)
library(Matrix)
library(splines)

#####
library(EnvCpt)
library(prophet)
library(glmnet)
library(lightgbm)
library(astsa)
library(fastDummies)
library(RColorBrewer)
library(Hmisc) 
library(corrplot)
library(glmnet)
library(ggplot2) 
library(tidyverse)
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)
library(TSstudio)
library(timetk)
library(forecast)
library(xts)
library(mgcv)
library(purrr)
library(imputeTS)
library(forecast)
library(stats)
library(xgboost)
library(caret)
library(gbm)
library(lme4)
library(parallel)
library(sgd)

conflict_prefer("filter", "dplyr")

log_midpipe <- function(x, ...) {
  force(x)
  cat(..., "\n")
  return(x)
}
# %%
#endregion

#region
# %% Get DWD Data
# "BE", "NL", "LU"
country <- "LU"
ZONE <- c(country) 
i.z <- 1 # later in loop
zone <- ZONE[i.z]
CITY <- read_csv("new_data/data_test/worldcities.csv")
meteovar <- c("TTT", "Rad1h", "Neff", "RR1c", "FF", "FX1", 'R602','TN','TX','RRS1c')
inith <- 8 ## hour of day to use forecast, 8 means take data from 8am morning
MET <- list()
# write_csv(MET[['LU']], 'data_22dec_2022/MET_LU_additional.csv')

cat('prepare met dataset*******************************************************\n')
ds_met <- read.csv(str_replace("data_22dec_2022/MET_XX_additional.csv", "XX" , country))
ds_met$timetoforecast_utc <- as.POSIXct(ds_met$timetoforecast_utc, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
ds_met$forecast_origin <- as.POSIXct(ds_met$forecast_origin, format = "%Y-%m-%dT%H:%M:%S", tz="UTC")
MET[[country]] <- ds_met

cat('na in met dataset*******************************************************\n')
colSums(is.na(ds_met))

ds_met %>%
  arrange(desc(timetoforecast_utc)) %>% 
  dplyr::select(timetoforecast_utc) %>% 
  tail(1)

cat('prepare edat dataset*******************************************************\n')
EDAT<- list()
ds_edat <- read.csv(str_replace("new_data/EDAT_XX.csv", "XX" , country))
ds_edat$DateTime <- as.POSIXct(ds_edat$DateTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
yt_name <- paste(country, "_Load_Actual",sep = '')
cat('na in met dataset*******************************************************\n')
colSums(is.na(ds_edat))
cat('apply imputation of edat in met dataset*********************************************\n')
# I can use here 2 options:
# zoo::na.locf(ds_edat[,yt_name])
# na_interpolation(ds_edat[,yt_name], option='spline', method='natural')
ds_edat[,yt_name] <- na_interpolation(ds_edat[,yt_name], option='spline', method='natural')
EDAT[[country]] <- ds_edat

ds_edat %>% 
  arrange(desc(DateTime)) %>% 
  dplyr::select(DateTime) %>% 
  head(2)

# plot to show nan values of the last days
plot(tail(ds_edat$DateTime,100),
     tail(ds_edat[,yt_name],100),
     type = "l",
     col = 'red',
     xlab = "Year",
     ylab = "Values")

# **********************************************************************
# MAXIMUM AND MINIMUM DATES BY COUNTRY AND DATASET
# -------------------------- #
# BE -------------------------
# EDAT, max date: 2022-11-28 22:00:00, min date: 2015-01-01 00:00:00
# MET, max date: 2022-12-30 08:00:00, min date: 2021-01-11 09:00:00

# -------------------------- #
# LU -------------------------
# EDAT, max date: 2022-11-28 22:00:00, min date: 2015-01-01 00:00:00
# MET, max date: 2022-12-30 08:00:00, min date: 2021-01-11 09:00:00

# -------------------------- #
# NL -------------------------
# EDAT, max date: 2022-11-28 22:00:00, min date: 2015-01-01 00:00:00
# MET, max date: 2022-12-30 08:00:00, min date: 2021-01-11 09:00:00


#region select a zone and create main data frame
#from here we work only with the first country/zone:
# %% combine meteorologic and edat data
names(MET[[zone]])[1] <- "DateTime"
DATA <- merge(x=MET[[zone]], y=EDAT[[zone]], by="DateTime", all.x=TRUE)

#filter dataset only with dates with information for both datasets
# max: 2022-11-28 22:00:00, min: 2021-01-11 09:00:00 ("NL")
DATA <- DATA %>% 
  filter(DateTime <= as.POSIXct("2022-11-28 22:00:00", tz="UTC"),
         DateTime >= as.POSIXct("2021-01-12 08:00:00", tz="UTC")) %>% 
  arrange(DateTime, horizon) 
names(DATA) <- gsub(" ", "", names(DATA))
dnames <- names(DATA)

## DateTime is in UTC, so I need to convert to CET format
DATA <- DATA %>% 
  mutate( DateTimeCET = as.POSIXlt(DateTime, tz="CET") )
## replace zeros of target
DATA[(DATA[yt_name] == 0), yt_name] <- NA
DATA[yt_name] <- zoo::na.locf(DATA[,yt_name])

cat('agg additional features******************************************************\n')
SummerTime = lubridate::dst(DATA$DateTimeCET)
HoD = lubridate::hour(DATA$DateTime)
DoW = lubridate::wday(DATA$DateTime, week_start = 1)
DoY = lubridate::yday(DATA$DateTime)
MoY = lubridate::month(DATA$DateTime)
WoY = lubridate::week(DATA$DateTime)
DoM = lubridate::mday(DATA$DateTime)
QoY <- lubridate::quarter(DATA$DateTime)
HoDDST = lubridate::hour(DATA$DateTimeCET)
DoWDST = lubridate::wday(DATA$DateTimeCET, week_start = 1)
DoYDST = lubridate::yday(DATA$DateTimeCET)
MoYDST = lubridate::month(DATA$DateTimeCET)
WoYDST  = lubridate::week(DATA$DateTimeCET)
DoMDST = lubridate::mday(DATA$DateTimeCET)
QoYDST <- lubridate::quarter(DATA$DateTimeCET)
DET <- cbind(SummerTime, HoD, DoW, DoY, MoY, WoY, DoM, QoY, HoDDST, DoWDST, DoYDST, MoYDST, WoYDST, DoMDST, QoYDST)

cat('agg additional features and DATA ***************************************************\n')
DATA <- cbind(DATA, DET)

cat("add cyclic/seasonality features using cosine/sin and weekend**********************************************\n")
DATA$HoD_sin = sin((DATA$HoD*2*pi)/23)
DATA$HoD_cos = cos((DATA$HoD*2*pi)/23)
DATA$DoW_sin = sin((DATA$DoW*2*pi)/7)
DATA$DoW_cos = cos((DATA$DoW*2*pi)/7)
DATA$MoY_sin = sin((DATA$MoY*2*pi)/12)
DATA$MoY_cos = cos((DATA$MoY*2*pi)/12)
DATA$DoY_sin = sin((DATA$DoY*2*pi)/366)
DATA$DoY_cos = cos((DATA$DoY*2*pi)/366)
DATA$WoY_sin = sin((DATA$WoY*2*pi)/53)
DATA$WoY_cos = cos((DATA$WoY*2*pi)/53)
DATA$DoM_sin = sin((DATA$DoM*2*pi)/31)
DATA$DoM_cos = cos((DATA$DoM*2*pi)/31)
DATA$QoY_sin = sin((DATA$QoY*2*pi)/4)
DATA$QoY_cos = cos((DATA$QoY*2*pi)/4)
DATA$weekend =  ifelse((DATA$DoW %in% c(6,7)), 1, 0)
DATA$is_day_start <- ifelse((DATA$HoD %in% seq(1,6) ), 1, 0)
DATA$is_day_end <- ifelse((DATA$HoD%in% seq(18,23) ), 1, 0)

#plot of seasonal patterns
ds_unique <- DATA %>% distinct(DateTime, .keep_all= TRUE) %>% arrange(DateTime,horizon)
plot(head(ds_unique$DateTime,16+(24*1)),head(ds_unique[,yt_name],16+(24*1)),type='l')

cat('decomposition of target variable*****************************************\n')
min_date_vec <- c(lubridate::year(min(DATA$DateTime)),lubridate::month(min(DATA$DateTime)),
                  lubridate::day(min(DATA$DateTime)), lubridate::hour(min(DATA$DateTime)),
                  lubridate::minute(min(DATA$DateTime)), lubridate::second(min(DATA$DateTime))
                  )
yts <- ts(ds_unique[,yt_name][1:(24*30*12)], frequency = 24*7 ,start = 0)
plot( decompose(yts) )

par(mfrow = c(1,2))
acf(ds_unique[,yt_name], main='ACF of energy demand' )
pacf(ds_unique[,yt_name], main='PACF of energy demand')

par(mfrow=c(4,2), mar=c(4,5,3,1))
plot( decompose(yts))
plot(ts(DATA[,yt_name], frequency  = 24*7 ), 
     plot.type="single", col = c("blue"), type= "o" , lwd=.5,main="1", ylab="Miles",
     cex.lab=5, xlab = '', xaxt='n')
grid()

cat('control by changes on trends or means****************************************\n')
future <- DATA[,yt_name]
total_nrow <- length(DATA[,yt_name])
n_per_chunk <- (24*30)
n_chunk <- ceiling(total_nrow/n_per_chunk)
list_sequence <- list()
for (i in 0:(n_chunk-1)){
  lower_ind <- ((i*n_per_chunk)+1)
  upper_ind <- ((i+1)*n_per_chunk)
  values_future <- (future[c(lower_ind:upper_ind)])
  values_future <- values_future[!is.na(values_future)]
  list_sequence[[i+1]] <- values_future
}

est.trend.mean.changes <- function(i, list_sequence, vec_changes){
  var <- list_sequence[[i]]
  type_change <- vec_changes
  fit_envcpt = envcpt(var,models=type_change,minseglen=5)
  return(list(type_change = fit_envcpt))
}
vec_changes <- "trendcpt"
res <- mclapply(1:length(list_sequence), FUN = function(i) est.trend.mean.changes(i,list_sequence, vec_changes ))
for (i in 1:length(list_sequence)) res[[i]] <- unlist((res[[i]]$type_change$trendcpt@cpts)+(n_per_chunk*(i-1)) )
DATA$chgTrend <- ifelse(as.integer(rownames(DATA)) %in% unlist(res), 1, 0)


cat('create trend variable in FULL dataset*************************************\n')
ds_unique$Trend <- 1:nrow(ds_unique)
DATA <- merge(DATA, ds_unique %>% dplyr::select(DateTime,Trend), by = 'DateTime')
DATA$chgTrendByTrend <- DATA$chgTrend*DATA$Trend

cat('interaction between holidays and DoY***************************************\n')
#interaction day Of year by holidays
holidays <- read_csv("data_22dec_2022/holidays_2000_2030.csv")
holidays <- holidays %>% filter(CountryCode == zone) %>%
  filter(as.Date.character(Date)>= format(as.Date(min(DATA$DateTime),format="%Y-%m-%d"),"%Y-%m-%d") &
           as.Date.character(Date)<= format(as.Date(max(DATA$DateTime),format="%Y-%m-%d"),"%Y-%m-%d")) 

ts.plot(ds_unique %>% filter(
  format(as.Date((ds_unique$DateTime),format="%Y-%m-%d"),"%Y-%m-%d") >= as.Date.character("2022-04-26") &
    format(as.Date((ds_unique$DateTime),format="%Y-%m-%d"),"%Y-%m-%d") <= as.Date.character("2022-04-30")
  ) %>% dplyr::select(yt_name))


estimate.DHL.range <- function(DATA, holidays, S = 24, dayahead = 1.5, daybefore = 0.5){
  DHL_ <- array(0, dim=c(nrow(DATA), 1 ) )
  xtime <- DATA$DateTime
  holidays$Name <- paste(gsub(" ", "", holidays$Name), as.character(lubridate::year(holidays$Date)), sep ='_')
  holidays$Name <- as.factor(holidays$Name)
  holnames <- levels(holidays$Name)
  hldN <- length(holnames)
  cat(holnames,'..\n')
  S <- S
  dayahead <- dayahead
  daybefore <- daybefore
  for(i.hld in 1:hldN){
    main_date <- ymd_hms( paste(holidays$Date[ holidays$Name == holnames[i.hld] ], '00:00:00'), tz="UTC" ) 
    main_date <- as.POSIXct(main_date)
    lim_superior <- ymd_hms( (main_date) + (3600 * (S*dayahead)) , tz= 'UTC')
    lim_inferior <- ymd_hms( (main_date) - (3600 * (S*daybefore)) , tz= 'UTC')
    id_set <- which((as.Date.character(lim_inferior) <= xtime & as.Date.character(lim_superior) >= xtime))
    DHL_[id_set,] <- 1
  }
  return(DHL_)
}

DHL_ <- estimate.DHL.range(DATA, holidays, S = 24, dayahead = 1.5, daybefore = 0.5 )
DHL_ <- as.matrix(setNames(DHL_ %>% as.data.frame(), c('holidays_dummy')) )
DATA <- cbind(DATA,DHL_)

#dfDoY <- as.data.frame(base::as.factor(DATA$DoY))
#names(dfDoY) <- c("DoY")
#MDoY <- as.matrix(sparse.model.matrix(~.-1, 
#                                      data = setNames(as.data.frame(base::as.factor(DATA$DoY)),
#                                                      c('DoY'))   ))
# estimate interactions WoY and holidays
MWoY <- as.matrix(sparse.model.matrix(~.-1, 
                                      data = setNames(as.data.frame(base::as.factor(DATA$WoY)),
                                                      c('WoY'))   ))

estimate.vector.WoY.Holidays <- function(i, dummy_holidays, all_dummys){
  result <- (dummy_holidays[,1]*all_dummys[,colnames(all_dummys)[i]])
  return(result)
}

#estimate interactions -- est.result.holidays.DoY
mx.result.tibble.holidays.WoY <- lapply(1:length(colnames(MWoY)), FUN = function(i) estimate.vector.WoY.Holidays(i, DHL_, MWoY ) )
mx.result.tibble.holidays.WoY <- do.call(cbind, mx.result.tibble.holidays.WoY)
colnames(mx.result.tibble.holidays.WoY) <- paste(colnames(MWoY),'holidays', sep = '.')
mx.result.tibble.holidays.WoY <- as.tibble(mx.result.tibble.holidays.WoY[,colSums(mx.result.tibble.holidays.WoY)>0])
#dim(mx.result.tibble.holidays.WoY)
#colSums(mx.result.tibble.holidays.WoY)

cat('na in FULL dataset*******************************************************\n')
colSums(is.na(DATA))
# original selection: "TTT", "Rad1h", "Neff", "RR1c", "FF", "FX1"
# drop because they have a lot of missing values: R602 TN TX TX
# include: "TTT", "Rad1h", "Neff", RR1c", "FF", "FX1"
cat('correlation plot of DATA ********************************************************\n')

simple_corr_matrix_plot(DATA %>% 
                          select_if(is.numeric), 
                        0.8 ,0.9 , 'corr using spearman',
                        colnames(DATA %>% 
                                   select_if(is.numeric)),"spearman" )

#ds_unique <- DATA %>% distinct(DateTime, .keep_all= TRUE) %>% arrange(DateTime,horizon)
#simple_corr_matrix_plot(ds_unique %>% 
#  dplyr::select(yt_name,c('weekend', "SummerTime", "is_day_start", "is_day_end", "TTT", "FF",
#                          "Trend", "holidays_dummy"),c(paste_seasonality) ),0.8 ,0.9, 'corr using spearman',
#  c(yt_name,c('weekend', "SummerTime", "is_day_start", "is_day_end", "TTT", "FF",
#              "Trend", "holidays_dummy"),paste_seasonality)
#  ,"spearman" )

# Define train and test horizon
H <- 240
horizonfull <- 1:H
last_time <- min(DATA$DateTime) + (3600 * (18*24))
FSTUDYDAYS <- seq(from = last_time, 
                  to = max(DATA$DateTime) - (3600 * (H)),
                  by = 3600 * 24)
N <- length(FSTUDYDAYS)

IDTEST <- list()
for (i.hm in 1:N) {
  IDTEST[[i.hm]] <- which(DATA$DateTime >= FSTUDYDAYS[i.hm] + 1 * 3600 & DATA$DateTime <= FSTUDYDAYS[i.hm] + H * 3600 & FSTUDYDAYS[i.hm] == DATA$forecast_origin) # == FSTUDYDAYS[i.hm] == DATA$forecast_origin restricts to most recent known weather forecasts
  # View(DATA[which(DATA$DateTime >= FSTUDYDAYS[239+1] + 1 * 3600 & DATA$DateTime <= FSTUDYDAYS[239+1] + H * 3600),])
}


cat("holidays preparation********************************\n")
holidays <- read_csv("data_22dec_2022/holidays_2000_2030.csv")
holidays$Name[] <- gsub(" ", "", holidays$Name)
tmp <- get.HLD(DATA, holidays, zone=country)

# bind DATA with holidays
DATA <- cbind(DATA, tmp)

cat('impute met.features******************************************************\n')
#"TTT", 'FF', TTT doesn't have NA
DATA$FF <- na_interpolation(DATA$FF, option='spline')

cat("data preparation for gb, rf, sgd*****************************************\n")
S <- 24
ytarget <- yt_name
LAGS <- S * c(1:14, 21, 28)
vec <- as.integer(DATA$DateTime)
subs <- match(unique(vec), vec)
TMPDATA <- bind_cols(DateTime = DATA$DateTime[subs], 
                     shiftDST(DATA[subs,ytarget],
                              summer = DATA$SummerTime[subs], 
                              clag = LAGS) )

MHoD <- as.matrix(sparse.model.matrix(~.-1, 
                                      data = setNames(as.data.frame(base::as.factor(DATA$HoD)),
                                                      c('HoD')) ))

#dfDoW <- as.data.frame(base::as.factor(DATA$DoW))
#names(dfDoW) <- c("DoW")
MDoW <- as.matrix(sparse.model.matrix(~.-1,
                                      data = setNames(as.data.frame(base::as.factor(DATA$DoW)),
                                                      c('DoW')) ))

#dfMoY <- as.data.frame(base::as.factor(DATA$MoY))
#names(dfMoY) <- c("MoY")
MMoY <- as.matrix(sparse.model.matrix(~.-1,
                                      data = setNames(as.data.frame(base::as.factor(DATA$MoY )),
                                                      c('MoY'))  ))

#dfQoY <- as.data.frame(base::as.factor(DATA$QoY))
#names(dfQoY) <- c("QoY")
MQoY <- as.matrix(sparse.model.matrix(~.-1,
                                      data = setNames(as.data.frame(base::as.factor(DATA$MoY )),
                                                      c('QoY'))  ))

#weekend_mx <- matrix(DATA$weekend, nrow = nrow(DATA), ncol = 1 )
#colnames(weekend_mx) <- c("weekend")

all_dummys <- as_tibble(cbind(MHoD, MDoW, MMoY, MQoY, MWoY,
                              as.matrix(setNames(DATA %>% dplyr::select(weekend), c('weekend'))) ))

cat("interaction features****************************************************\n")
paste_hod <- paste("HoD",0:23, sep="")
paste_dow <- paste("DoW",1:7, sep="")
paste_moy <- paste("MoY",1:12, sep="")
paste_qoy <- paste("QoY",1:4, sep="")
paste_woy <- paste("WoY",1:53, sep="")
paste_interaction_holidays_WoY <- colnames(mx.result.tibble.holidays.WoY)
paste_seasonality <- as.vector( sapply(c("HoD","DoW", "MoY", "DoY", "WoY", "DoM", "QoY"), 
                                       function(x) c(paste(x,'_sin',sep = ''), paste(x,'_cos',sep = '')   )) )

col_comb_features <- c(paste_dow, paste_hod, paste_moy, paste_qoy, 'weekend' )
all_comb <- combn(col_comb_features, 2)
ls_ds_interaction <- list()
ls_ds_interaction_pt <- list()
for (ncomb in 1:ncol(all_comb)){
  pt_1 <- all_comb[1,ncomb]
  pt_2 <- all_comb[2,ncomb]
  if ( substr(pt_1, start=1, stop=3) != substr(pt_2, start=1, stop=3) ){
    ls_ds_interaction[[ncomb]] <- paste(pt_1,pt_2,sep = 'and')
    ls_ds_interaction_pt[[ncomb]] <- paste(pt_1,pt_2,sep = '.')
  }
}

ls_ds_interaction <- Filter(Negate(is.null), ls_ds_interaction)
ls_ds_interaction <- str_split(ls_ds_interaction,'and')

estimate.vector <- function(i, ls_ds_interaction, all_dummys){
  result <- (all_dummys[,ls_ds_interaction[[i]][1]]*all_dummys[,ls_ds_interaction[[i]][2]])
  return(result)
}
#est.result <- lapply(1:length(ls_ds_interaction), FUN = function(i) estimate.vector(i, ls_ds_interaction, all_dummys ) )
mx.result.tibble <- do.call(cbind, 
                            lapply(1:length(ls_ds_interaction),
                                   FUN = function(i) estimate.vector(i, ls_ds_interaction, all_dummys ) ) )
colnames(mx.result.tibble) <- unlist(ls_ds_interaction_pt)
mx.result.tibble <- as.tibble(mx.result.tibble)
mx.result.tibble <- mx.result.tibble[,(colSums(mx.result.tibble) >0)] #620----585, 759---710

cat("finish interaction features**************************************************\n")

#define features to use
features_holidays <- colnames(tmp)
features_interaction <- colnames(mx.result.tibble)
features_x <- c(paste_hod[-1], paste_dow[-1], paste_moy[-7],
                paste_qoy[-1], paste_woy[-40],
                paste("x_lag_",S * c(1:14, 21, 28), sep=""),
                paste_seasonality, features_interaction,
                paste_interaction_holidays_WoY, features_holidays,
                'weekend', "SummerTime", "is_day_start", "is_day_end", "TTT", "FF",
                "Trend", "holidays_dummy") #"chgTrendByTrend", "chgTrend"

TMPDATA <- cbind(TMPDATA, (all_dummys[subs, -unlist(list(ncol(all_dummys)))]), mx.result.tibble[subs,])
TMPDATA <- cbind(TMPDATA, mx.result.tibble.holidays.WoY[subs,])

# define models to estimate
# "true", "bench", "GAM", "AR", "hw", "elasticNet", 'sgdmodel', 'gb', 'rf', 'prophet'
cat('use only one algorith for each iteration in order to avoid ram problems....\n')
model.names <- c("true") #"rf", "gb",'sgdmodel', "AR","true", "bench")  ---> run jointly --> "AR","true", "bench"
M <- length(model.names)

# for (i.m in model.names)
FORECASTS <- array(NA, dim = c(N, H, M))
dimnames(FORECASTS) <- list(format(FSTUDYDAYS, "%Y-%m-%d"), paste("h_", 1:H, sep = ""), model.names)
cat('dimension of result matrix*******************************************************\n')
dim(FORECASTS)
#define ls of training time
ls_train_time <- list()

#define lists to save results and models
rf_results <- list()
gb_results <- list()
sgd_results <- list()
ar_results <- list()
length(features_x)
# run model estimation

for (i.m in seq_along(model.names)) {
  mname <- model.names[i.m]
  init_time <- Sys.time()
  cat('\\\\\\\\-- begin model', mname, '........i.m*******************************************//////////\n',sep = ' ')
  
  if (mname %in% c("rf","true", "bench", "GAM", "gb", "elasticNet", "sgdmodel")) {
    LAGS <- S * c(1:14, 21, 28) # S * c(1:14, 21, 28)
    horizonc <- unique(c(0, findInterval(LAGS, 1:H)))
  } else { # AR
    horizonc <- c(0, H)
  }
  
  # define hyperparamters once
  if (mname == 'gb'){
    # version 1
    #lgb.grid <- base::expand.grid(
    #  list(
    #    boosting= c("gbdt","dart"),
    #    obj = c("tweedie"),
    #    tweedie_variance_power = seq(1.1,1.9,0.1),
    #    metric = c('rmse'),
    #    num_leaves = seq(20, 27, 1),
    #    max_depth = seq(7,10,1),
    #    min_sum_hessian_in_leaf = c(6,7),
    #    learning_rate = seq(0.18, 0.3, 0.02),
    #    num_iterations = seq(40, 50, 3),
    #    lambda_l1 = seq(0.01, 0.1, 0.01),
    #    lambda_l2 = seq(0.01, 0.1, 0.01)
    #  )) #"mse"
    
    # original version --- good performance
    #lgb.grid <- base::expand.grid(
    #  list(
    #    boosting= c("gbdt","dart"),
    #    obj = c("regression"),
    #    metric = c('rmse'),
    #    num_leaves = seq(20, 27, 1),
    #    max_depth = seq(7,10,1),
    #    min_sum_hessian_in_leaf = c(6,7),
    #    learning_rate = seq(0.18, 0.3, 0.01),
    #    num_iterations = seq(40, 50, 3),
    #    lambda_l1 = seq(0.01, 0.1, 0.01),
    #    lambda_l2 = seq(0.01, 0.1, 0.01)
    #  ))
    
    #modified original version iter(i guess is 3)
    #lgb.grid <- base::expand.grid(
    #  list(
    #    boosting= c("gbdt","dart"),
    #    obj = c("regression"),
    #    metric = c('rmse'),
    #    num_leaves = seq(20, 25, 1),
    #    max_depth = seq(8,9,1),
    #    min_sum_hessian_in_leaf = seq(35,39,2),
    #    learning_rate = seq(0.1, 0.2, 0.03),
    #    num_iterations = seq(43, 48, 2),
    #    lambda_l1 = seq(0.01, 0.1, 0.02),
    #    lambda_l2 = seq(0.01, 0.1, 0.02)
    #  ))
    
    #modified  original version, iter4 (best performance)
    lgb.grid <- base::expand.grid(
      list(
        boosting= c("gbdt", "dart"),
        obj = c("regression"),
        metric = c('rmse'),
        num_leaves = seq(23, 24, 1), #seq(20, 25, 1)
        max_depth = seq(8,9,1),
        min_sum_hessian_in_leaf = seq(34,35,1), #seq(35,39,2)
        learning_rate = seq(0.38, 0.39, 0.01),#seq(0.1, 0.2, 0.03)
        num_iterations = seq(29, 31, 1), #seq(43, 48, 2)
        lambda_l1 = c(0.12), #seq(0.11, 0.12, 0.01), #seq(0.01, 0.1, 0.02)
        lambda_l2 = c(0.08) #seq(0.07, 0.08, 0.01)
      ))
    
    #saved as *_v2
    #lgb.grid <- base::expand.grid(
    #  list(
    #    boosting= c("gbdt","dart"),
    #    obj = c("regression"),
    #    metric = c('rmse'),
    #    num_leaves = seq(20, 35, 3), 
    #    max_depth = seq(6,11,2),
    #    min_sum_hessian_in_leaf = seq(5,50,5),
    #    learning_rate = seq(0.1, 0.25, 0.02),
    #    num_iterations = seq(40, 100, 5),
    #    lambda_l1 = seq(0.01, 0.1, 0.01),
    #    lambda_l2 = seq(0.01, 0.1, 0.01)
    #  ))
    
  }
  #dim(lgb.grid)#96768000
  if (mname == 'rf'){
    rf.grid <- base::expand.grid(
      list(
        boosting= c("rf"),
        obj = c("tweedie"),
        tweedie_variance_power = seq(1.1,1.9,0.1),
        metric = c('rmse'),
        max_depth = seq(5,12,1),
        num_leaves = seq(20, 27, 1),
        num_iterations = seq(40, 50, 3),
        lambda_l1 = seq(0.01, 0.1, 0.01),
        lambda_l2 = seq(0.01, 0.1, 0.01),
        learning_rate = seq(0.18, 0.3, 0.02)
      )) #"mse"
  }
  
  # define horizon separation
  reest <- 20 
  FSTUDYSEQid <- unique(c(seq(0, length(FSTUDYDAYS), by = reest), length(FSTUDYDAYS)))
  FSTUDYSEQ <- list()
  for (i.seq in 1:(length(FSTUDYSEQid) - 1)) FSTUDYSEQ[[i.seq]] <- FSTUDYDAYS[c((FSTUDYSEQid[i.seq] + 1), FSTUDYSEQid[i.seq + 1])]
  Nsplitlen <- length(FSTUDYSEQ) 
  cat('# of horizon separation *N*', Nsplitlen, 'of model', mname, '*******************************************\n',sep = ' ')
  
  # model specific data preparation for the forecasting study [model dependent]: 
  if(mname %in% c("GAM", "gb","rf", "elasticNet", "sgdmodel")){
    FDATA <- dplyr::full_join(DATA, TMPDATA, by = c("DateTime")) %>% arrange(DateTime, horizon)
    
    # define formula for gb and rf
    formula_str <- paste(
      paste( ytarget,' ~ ',sep = ''), 
      paste( paste_dow[-1], collapse=' + '),' + ',
      paste( paste_moy[-7], collapse=' + '),' + ',
      paste( paste_hod[-1], collapse=' + '),' + ',
      paste( paste_qoy[-1], collapse=' + '),' + ',
      paste( paste_woy[-40], collapse=' + '),' + ',
      paste( paste_interaction_holidays_WoY, collapse=' + '),' + ',
      paste( features_holidays, collapse=' + '),' + ',
      paste( paste_seasonality, collapse=' + '),' + ',
      paste( features_interaction, collapse=' + '),' + ',
      paste( paste("x_lag_",S * c(1:14, 21, 28), sep = '', collapse=' + '),
             '+ weekend + SummerTime + is_day_start + is_day_end + TTT + FF + Trend + holidays_dummy'),sep = '')
    
  } else {
    FDATA <- DATA 
  }
  DATATEST <- FDATA[unlist(IDTEST), ]
  
  # begin loop by rows 'N'
  #i.N <- 1
  cat('------||| begin iteration over each block of rows **N**, FSTUDYSEQ..rows..i.N........|||------\n')
  for (i.N in seq_along(FSTUDYSEQ)) {
    cat('**--part N#', i.N, 'of model', mname, '--**\n',sep = ' ')
    
    # define id of reestimation
    seqid <- ((FSTUDYSEQid[i.N] + 1):FSTUDYSEQid[i.N + 1])
    # id row....
    #  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
    HORIZON <- list()
    for (i.hl in seq_along(horizonc)[-1]) HORIZON[[i.hl - 1]] <- (horizonc[i.hl - 1] + 1):horizonc[i.hl]
    #HORIZON, BLOCK OF 24 DAYS, 10days
    #[[9]]
    #[1] 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216
    #[[10]]
    #[1] 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240
    
    # begin reestimation
    for (i.hl in seq_along(HORIZON)) {
      # cat('HORIZON........24,48....240..ahead.\n')
      #i.hl <- 1
      cat('+++++** reestimation #', i.hl, 'of model', mname, 'from the part #', i.N,'***+++\n',sep = ' ')
      
      # define max and min horizons
      hmin <- head(HORIZON[[i.hl]], 1)
      hmax <- tail(HORIZON[[i.hl]], 1)
      idt <- FDATA$DateTime <= FSTUDYSEQ[[i.N]][1] & FDATA$horizon >= hmin & FDATA$horizon <= hmax & !is.na(FDATA$horizon)
      
      DATAtrain <- FDATA[idt, ]
      
      idtestl <- list()
      for (i.hm in seqid) {
        idtestl[[i.hm]] <- which(DATATEST$DateTime >= FSTUDYDAYS[i.hm] + hmin * 3600 & DATATEST$DateTime <= FSTUDYDAYS[i.hm] + hmax * 3600 & DATATEST$horizon >= hmin & DATATEST$horizon <= hmax & FSTUDYDAYS[i.hm] == DATATEST$forecast_origin)
      }
      
      idtest <- unlist(idtestl)
      length(idtest)
      
      # form dataset and correct dataset
      ls_ds_each_row_date <- list()
      for (i.hm in seqid) {
        if (length(idtestl[[i.hm]])>0){
          ids_sample <- idtestl[[i.hm]]
          tmp_sample_ds <- DATATEST[ids_sample, ]
          ls_ds_each_row_date[[i.hm]] <- tmp_sample_ds
        } else {
          # create empty matrix/vector
          empty_ds <- as.data.frame(matrix(nrow = hmax-hmin+1 ,ncol = ncol(DATATEST) ) )
          colnames(empty_ds) <- colnames(DATATEST)
          empty_ds$DateTime <- seq(FSTUDYDAYS[i.hm] + hmin * 3600, FSTUDYDAYS[i.hm] + hmax * 3600, by = 3600)
          empty_ds$forecast_origin <- FSTUDYDAYS[i.hm]
          empty_ds$horizon <- seq(hmin, hmax, by = 1)
          ls_ds_each_row_date[[i.hm]] <- empty_ds
        }
      }
      
      # combine subsets
      DATAtest <- Reduce(function(x,y) bind_rows(x,y), ls_ds_each_row_date) %>% 
        arrange(DateTime, horizon)
      ytarget <- paste(zone, "_Load_Actual", sep = "")
      DATAtest <- DATAtest %>% 
        arrange(horizon, DateTime)
      
      
      if (mname == "GAM") {
        act_lags <- LAGS[LAGS >= hmax]
        formstr <- paste(ytarget, "~",paste(paste("LT_Load_Actual_lag_",act_lags, sep=""),collapse="+"),"+",paste(paste("HoD",0:23, sep=""),collapse="+"),sep = "") # + ti(TTT,k=6, bs='cs')
        form <- as.formula(formstr)
        mod <- lm(form, data = DATAtrain)#bam(form, data = DATAtrain, select = TRUE, gamma = log(dim(DATA)[1]) / 2, discrete = TRUE)
        print(summary(mod))
        pred <- t(matrix(predict(mod, newdata = DATAtest), nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
      } # GAM
      if (mname == "gb") {
        #View(DATAtrain)
      
        filter_train <- DATAtrain[, c(ytarget, features_x)] %>% 
          replace(is.na(.), 0)
        filter_test <- DATAtest[, c(features_x)] %>% 
          replace(is.na(.), 0)
        
        set.seed(11)
        samp <- sample(1:nrow(lgb.grid ), 5)
        lgb.gridFilter <- lgb.grid [samp,]
        # create datasets
        lgb.test <- lgb.Dataset(data = as.matrix(filter_test),
                                label = DATAtest[,ytarget] )
        lgb.train <- lgb.Dataset(data = as.matrix(filter_train %>% dplyr::select(-ytarget) %>% head(nrow(filter_train)-20) ),
                                 label = head(filter_train[,ytarget], nrow(filter_train)-20) )
        lgb.valid <- lgb.Dataset(data = as.matrix(filter_train %>% dplyr::select(-ytarget) %>% tail(20) ),
                                 label = head(filter_train[,ytarget], tail(20) ) )
        
        estimate.lgb <- function(hyper_combi, lgb.train, lgb.valid, lgb.gridFilter){
          ls_params <- as.list(data.frame(lgb.gridFilter[hyper_combi,]) )
          obj <- as.character(ls_params$obj)
          boosting <- as.character(ls_params$boosting)
          metric <- as.character(ls_params$metric)
          ls_params <- within(ls_params, rm(obj, boosting, metric))
          early_value <- 10
          if (boosting == 'dart'){
            early_value <- -1
          }
          watchlist <- list(validation = lgb.valid)
          lgb_alg <- lgb.train(params = ls_params,obj = obj, boosting = boosting, metric = metric,
                               data = lgb.train, valids = watchlist,
                               early_stopping_rounds = early_value, verbose = 1,
                               eval_freq = 10, force_col_wise=TRUE,
                               nthread = 6, bagging_fraction = 0.8, feature_fraction = 0.8)
          
          return( list(mod = lgb_alg, score = lgb_alg$best_score) )
        }
        
        #recent_time <- Sys.time()
        cat('train lgb.....\n')
        est.lgb <- lapply(1:nrow(lgb.gridFilter), FUN = function(hyper_combi) estimate.lgb(hyper_combi,
                                                                                             lgb.train,
                                                                                             lgb.valid,
                                                                                             lgb.gridFilter))
        cat('finish lgb.....\n')
        
        rmse_vec = c()
        for (i in 1:nrow(lgb.gridFilter)) rmse_vec[i] <- est.lgb[[i]]$score
        id_min_rmse <- which.min(rmse_vec)
        best_tune_model  <- est.lgb[[id_min_rmse]]$mod
        
        test_sparse  = Matrix(as.matrix(filter_test), sparse=TRUE)
        cat('test has: ',dim(filter_test),'\n')
        dim(test_sparse)
        cat(length(HORIZON[[i.hl]]),'----' ,length(seqid),'\n')
        pred <- t(matrix(predict(best_tune_model, data = test_sparse), 
                         nrow = length(HORIZON[[i.hl]]), ncol= length(seqid), byrow = TRUE))
        best_tune_model$params
        
      }
      if (mname == "rf") {
        
        filter_train <- DATAtrain[, c(ytarget, features_x)] %>% 
          replace(is.na(.), 0)
        filter_test <- DATAtest[, c(features_x)] %>% 
          replace(is.na(.), 0)
        
        set.seed(2)
        samp <- sample(1:nrow(rf.grid ), 5)
        rf.gridFilter <- rf.grid[samp,]
        # create datasets
        rf.test <- lgb.Dataset(data = as.matrix(filter_test),
                                label = DATAtest[,ytarget] )
        rf.train <- lgb.Dataset(data = as.matrix(filter_train %>% dplyr::select(-ytarget) %>% head(nrow(filter_train)-20) ),
                                 label = head(filter_train[,ytarget], nrow(filter_train)-20) )
        rf.valid <- lgb.Dataset(data = as.matrix(filter_train %>% dplyr::select(-ytarget) %>% tail(20) ),
                                 label = head(filter_train[,ytarget], tail(20) ) )
        
        estimate.rf <- function(hyper_combi, rf.train, rf.valid, rf.gridFilter){
          ls_params <- as.list(data.frame(rf.gridFilter[hyper_combi,]) )
          obj <- as.character(ls_params$obj)
          boosting <- as.character(ls_params$boosting)
          metric <- as.character(ls_params$metric)
          ls_params <- within(ls_params, rm(obj, boosting, metric))
          watchlist <- list(validation = rf.valid) #eval_freq = 5 bagging_freq = 5)
          rf_alg <- lgb.train(params = ls_params, obj = obj, metric = metric, boosting= boosting,
                               data = rf.train, valids = watchlist,
                              early_stopping_rounds = 70, verbose = 1,
                              force_col_wise=TRUE,
                              nthread = 6, bagging_fraction = 0.8, feature_fraction = 0.8,
                              bagging_freq = 5, eval_freq = 5)
          return( list(mod = rf_alg, score = rf_alg$best_score ) )
        }
        
        #recent_time <- Sys.time()
        cat('train rf.....\n')
        est.rf <- lapply(1:nrow(rf.gridFilter), FUN = function(hyper_combi) estimate.rf(hyper_combi,
                                                                                           rf.train,
                                                                                           rf.valid,
                                                                                           rf.gridFilter))
        cat('finish rf.....\n')
        
        rmse_vec = c()
        for (i in 1:nrow(rf.gridFilter)) rmse_vec[i] <- est.rf[[i]]$score
        id_min_rmse <- which.min(rmse_vec)
        best_tune_model <- est.rf[[id_min_rmse]]$mod
        
        test_sparse  = Matrix(as.matrix(filter_test), sparse=TRUE)
        cat('test has: ',dim(filter_test),'\n')
        cat(length(HORIZON[[i.hl]]),'----' ,length(seqid),'\n')
        pred <- t(matrix(predict(best_tune_model, data = test_sparse), 
                         nrow = length(HORIZON[[i.hl]]), ncol= length(seqid), byrow = TRUE))
      }
      if (mname == "elasticNet") {
        filter_train <- DATAtrain[, c(ytarget, features_x)] %>% 
          replace(is.na(.), 0)
        filter_test <- DATAtest[, c(ytarget, features_x)] %>% 
          replace(is.na(.), 0)
        
        lambdas <- c(seq(0.1, 0.91, 0.1), seq(1,45, 2) )
        alphas <- c(seq(0.8, 1, 0.1), 1)
        #length(lambdas)
        ts_trControl <- trainControl("timeslice", number= 2,
                                     initialWindow = nrow(filter_train)-20,
                                     skip = 0, fixedWindow = FALSE, horizon = 20, timingSamps = 1,
                                     search = "random")
        
        elastic.net.grid <- expand.grid(list(alpha = alphas, lambda = lambdas) )
        set.seed(2)
        samp <- sample(1:nrow(elastic.net.grid ), 20)
        elastic.net.gridFilter <- elastic.net.grid[samp,]
        
        #init_time1 <- Sys.time()
        elastic_net_reg <- train(
          x = as.matrix(filter_train %>% select(-ytarget) ),
          y = filter_train[,ytarget],
          method = "glmnet",
          trControl = ts_trControl,
          tuneGrid = elastic.net.gridFilter,
          metric = 'RMSE',
          maximize = FALSE   )
        
        pred <- t(matrix(predict(elastic_net_reg, as.matrix(filter_test %>% select(-ytarget))), 
                         nrow = length(HORIZON[[i.hl]]), ncol= length(seqid), byrow = TRUE))

      }
      if (mname == "sgdmodel"){
        filter_train <- DATAtrain[, c(ytarget, features_x)] %>% 
          replace(is.na(.), 0)
        filter_test <- DATAtest[, c(ytarget, features_x)] %>% 
          replace(is.na(.), 0)
        filter_valid <- filter_train %>% tail(20)
        filter_train <- filter_train %>% head(nrow(filter_train)-20)
        colSums(filter_test)
        
        set.seed(5)
        sgdmodel.grid <- base::expand.grid(
          list(
            lambdas = seq(0.98,1, 0.01),
            alphas = seq(20,23,1)
          ))
        samp <- sample(1:nrow(sgdmodel.grid), 2)
        sgdmodelFilter.grid <- sgdmodel.grid[samp,]
        
        estimate.sgd <- function(id_val, filter_train, filter_valid, sgdmodelFilter.grid, ytarget){
          lambda_val <- sgdmodelFilter.grid[id_val,'lambdas']
          alphas <- sgdmodelFilter.grid[id_val,'alphas']
          sgd_reg = sgd(x = as.matrix(filter_train %>% dplyr::select(-ytarget) ),
                        y = filter_train[,ytarget], model = "lm",
                        model.control = list(lambda1 = alphas, lambda2 = lambda_val),
                        sgd.control = list(method = 'ai-sgd'))
          sgd_pred_val <- predict(sgd_reg, newdata = as.matrix(filter_valid %>% dplyr::select(-ytarget) ) )
          rmse_val <- RMSE(as.matrix(filter_valid %>% dplyr::select(ytarget)), sgd_pred_val)
          #return(c(lambda_val, rmse_val))
          return(list(lambda = lambda_val, alphas = alphas, rmse = rmse_val, mod = sgd_reg ))
        }

        #init_time1 <- Sys.time()
        est.sgd <- mclapply(1:nrow(sgdmodelFilter.grid), FUN = function(i) estimate.sgd(i, filter_train, 
                                                                                filter_valid, sgdmodelFilter.grid,
                                                                                ytarget)) 
        rmse_vec = c()
        #min(rmse_vec)
        for (i in 1:nrow(sgdmodelFilter.grid)) rmse_vec[i] <- est.sgd[[i]]$rmse
        id_min_rmse <- which.min(rmse_vec)
        best_lambda <- est.sgd[[id_min_rmse]]$lambda
        best_alpha <- est.sgd[[id_min_rmse]]$alphas
        best_tune_model <- est.sgd[[id_min_rmse]]$mod
        
        test_sparse  = Matrix(as.matrix(filter_test %>% dplyr::select(-ytarget)), sparse=TRUE)
        pred <- t(matrix(predict(best_tune_model, newdata = test_sparse), 
                         nrow = length(HORIZON[[i.hl]]), ncol= length(seqid), byrow = TRUE))

      }
      if (mname == "prophet"){

        DATAtrainwow <- DATAtrain[DATAtrain$horizon <= S, ]
        DATAtestwow <- (DATAtest[DATAtest$horizon <= S, ] %>% arrange(DateTime))
        y <- unlist(DATAtrainwow[, ytarget])
        yn <- length(y)
        DATAwow <- c(y, unlist(DATAtestwow[, ytarget]))
        DATAwow <- zoo::na.locf(DATAwow)
        
        df <- data.frame(
          ds = DATAtrainwow$DateTime,
          y = y
        )
        m <- prophet(df, yearly.seasonality = TRUE, weekly.seasonality = TRUE,
                     daily.seasonality = TRUE, seasonality.mode = 'additive', fit = TRUE)
        prediction.n.ahead = H*length(seqid)
        
        future <- make_future_dataframe(m, periods = prediction.n.ahead )
        future <- tail(future, prediction.n.ahead)
        rownames(future) <- NULL
        total_nrow <- dim(future)[1]
        n_per_chunk <- 100
        n_chunk <- ceiling(total_nrow/n_per_chunk)
        list_sequence <- list()
        for (i in 0:(n_chunk-1)){
          #cat(i, '\n')
          lower_ind <- ((i*n_per_chunk)+1)
          upper_ind <- ((i+1)*n_per_chunk)
          values_future <- (future[c(lower_ind:upper_ind),])
          values_future <- values_future[!is.na(values_future)]
          list_sequence[[i+1]] <- values_future
        }
        
        estimate.prophet <- function(i, m, list_sequence){
          l <- list(ds = list_sequence[[i]])
          df <- as.data.frame(l)
          forecast_sample <- predict(m, df)
          return(list(ds_pred = forecast_sample) )
        }
        est.prophet <- mclapply(1:length(list_sequence), FUN = function(i) estimate.prophet(i, m, list_sequence))
        
        ls_predict_prophet <- list()
        for (i in 1:length(est.prophet)) ls_predict_prophet[[i]] <- est.prophet[[i]]$ds_pred$yhat
        pred <- matrix(unlist(ls_predict_prophet), nrow = length(seqid), ncol =  H)
        
      }
      if (mname == "AR") {
        DATAtrainwow <- DATAtrain[DATAtrain$horizon <= S, ]
        DATAtestwow <- (DATAtest[DATAtest$horizon <= S, ] %>% arrange(DateTime))
        y <- unlist(DATAtrainwow[, ytarget])
        yn <- length(y)
        DATAwow <- c(y, unlist(DATAtestwow[, ytarget]))
        DATAwow <- zoo::na.locf(DATAwow)
        om <- 4*24*7
        if (om > length(zoo::na.locf(y))){
          om <- (length(zoo::na.locf(y))-1)
        }
        mod <- ar(zoo::na.locf(y), order.max = om)
        pred <- matrix(NA, length(seqid), H)
        # save results
        id_save_result <- paste(mname, as.character(i.N), as.character(i.hl), sep = '_')
        ar_results[[id_save_result]] <- list(best_tune_model = mod)
        
        for (i.NN in 1:length(seqid)) pred[i.NN, ] <- predict(mod, newdata = DATAwow[yn + (-mod$order + 1):0 + (i.NN - 1) * S], n.ahead = H)$pred
      }
      if (mname == "ARMA") {
        DATAtrainwow <- DATAtrain[DATAtrain$horizon <= S, ]
        DATAtestwow <- (DATAtest[DATAtest$horizon <= S, ] %>% arrange(DateTime))
        y <- unlist(DATAtrainwow[, ytarget])
        yn <- length(y)
        DATAwow <- c(y, unlist(DATAtestwow[, ytarget]))
        DATAwow <- zoo::na.locf(DATAwow)
        om <- 2*24*7
        if (om > length(zoo::na.locf(y))){
          om <- (length(zoo::na.locf(y))-1)
        }
        tmp <- rbind(DATAtrainwow, DATAtestwow)
        rownames(tmp) <- NULL
        mod <- auto.arima(zoo::na.locf(y),
                          max.p = om, max.q = om, d = 0,
                          max.P = om, max.Q = om,
                          seasonal = TRUE, ic = "aic" )
        
        pred <- matrix(NA, length(seqid), H)
        for (i.NN in 1:length(seqid)) pred[i.NN, ] <- predict(mod, newdata = DATAwow[yn + (-mod$order + 1):0 + (i.NN - 1) * S], n.ahead = H)$pred
      }
      if (mname == "hw") {
        cat('estimating model. ........\n')
        DATAtrainwow <- DATAtrain[DATAtrain$horizon <= S, ] 
        DATAtestwow <- (DATAtest[DATAtest$horizon <= S, ] %>% arrange(DateTime))
        tmp <- rbind(DATAtrainwow, DATAtestwow)
        # View(tmp)
        y <- unlist(DATAtrainwow[, ytarget])
        yn <- length(y)
        DATAwow <- c(y, unlist(DATAtestwow[, ytarget])) 
        DATAwow <- zoo::na.locf(DATAwow)
        # correct outliers and zero values
        med_wo_outliers <- median(as.vector(y)[as.vector(y)!=0],na.rm = TRUE)
        y[as.vector(y)==0] <- med_wo_outliers
        ym <- ts(y,frequency = 24, 
                 start = c(lubridate::year(min(DATAtrainwow$DateTime)),
                           lubridate::month(min(DATAtrainwow$DateTime)),
                           as.integer(format(min(DATAtrainwow$DateTime), format = "%d")),
                           as.integer(format(min(DATAtrainwow$DateTime), format = "%H")),
                           as.integer(format(min(DATAtrainwow$DateTime), format = "%M")),
                           as.integer(format(min(DATAtrainwow$DateTime), format = "%S"))))
        ym <- ym %>% 
          replace_na(med_wo_outliers)
        mod <- hw(zoo::na.locf(ym), h = H*length(seqid), seasonal= "additive",
                  lambda= 'auto', damped= FALSE, initial = "optimal",
                  exponential= FALSE, alpha = NULL, beta= NULL, gamma = NULL)
        pred_values_all_days <- predict(mod, n.ahead = H*length(seqid))$mean
        pred <- matrix(pred_values_all_days, nrow = length(seqid), ncol =  H)
        
      }
      if (mname == "bench") {
        ybench <- paste(zone, "_Load_DayAhead", sep = "")
        pred <- t(matrix(unlist(DATAtest[, ybench]), nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
      } 
      if (mname == "true") {
        pred <- t(matrix(unlist(DATAtest[, ytarget]), nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
      }
      FORECASTS[seqid, HORIZON[[i.hl]], mname] <- pred
      
      
      cat(zone, "horizon:", hmin, "-", hmax, " done at split ", round(i.N / Nsplitlen * 100, 2), "% progress, mod:", mname, "\n")
    } # i.hl
    # cat('process for the horizon **N**finished*******************************************\n',sep = ' ')
  } # i.N
  end_time <- Sys.time()
  cat('....******* model training of', mname,',lasted:...\n',sep = ' ')
  print(difftime(init_time,end_time))
  ls_train_time[[mname]] <- (difftime(init_time,end_time))
  cat('....******* process for the model:', mname, 'finished****.......\n\n',sep = ' ')
} # i.m

cat("check training time..................................\n")
ls_train_time

#v2 using a modified version of the old hyper of gb
#_original version using 1sr hyper of gb
cat("save each the matrix result from each algorithm using the next line......\n")
##NL
#write.table(FORECASTS[,,'gb'], 'new_data/results_21feb_v02/gb_5comb_includNewFeatures_WoY_iter1.txt')
##BE
#write.table(FORECASTS[,,'sgdmodel'], 'new_data/BE_results/results_22feb_v02/sgdmodel_2comb_includNewFeatures_WoY_iter1.txt')
#write.table(FORECASTS[,,'bench'], 'new_data/BE_results/results_22feb_v02/bench_iter1.txt')
#dim(read.table('new_data/BE_results/results_22feb_v02/sgdmodel_2comb_includNewFeatures_WoY_iter1.txt'))
#ts.plot(FORECASTS[,,'rf'][200,])

##LU
#write.table(FORECASTS[,,'sgdmodel'], 'new_data/LU_results/results_22feb_v02/sgdmodel_2comb_includNewFeatures_WoY_iter1.txt')
#write.table(FORECASTS[,,'rf'], 'new_data/LU_results/results_22feb_v02/rf_5comb_includNewFeatures_WoY_iter2.txt')
#write.table(FORECASTS[,,'AR'], 'new_data/LU_results/results_22feb_v02/AR_iter1.txt')
#write.table(FORECASTS[,,'true'], 'new_data/LU_results/results_22feb_v02/true_iter1_v2fixed.txt')

# load the result tables
#BE
mx.sgdmodel <- read.table('new_data/BE_results/results_22feb_v02/sgdmodel_2comb_includNewFeatures_WoY_iter1.txt')
mx.gb <- read.table('new_data/BE_results/results_22feb_v02/gb_5comb_includNewFeatures_WoY_iter1.txt')
mx.rf <- read.table('new_data/BE_results/results_22feb_v02/rf_5comb_includNewFeatures_WoY_iter1.txt')
mx.AR <- read.table('new_data/BE_results/results_22feb_v02/AR_iter1.txt')
mx.true <- read.table('new_data/BE_results/results_22feb_v02/true_iter1.txt')
mx.bench <- read.table('new_data/BE_results/results_22feb_v02/bench_iter1.txt')

#LU
mx.sgdmodel <- read.table('new_data/LU_results/results_22feb_v02/sgdmodel_2comb_includNewFeatures_WoY_iter1.txt')
mx.gb <- read.table('new_data/LU_results/results_22feb_v02/gb_5comb_includNewFeatures_WoY_iter1.txt')
mx.rf <- read.table('new_data/LU_results/results_22feb_v02/rf_5comb_includNewFeatures_WoY_iter1.txt')
mx.AR <- read.table('new_data/LU_results/results_22feb_v02/AR_iter1.txt')
mx.true <- read.table('new_data/LU_results/results_22feb_v02/true_iter1_v2fixed.txt')
mx.bench <- read.table('new_data/LU_results/results_22feb_v02/bench_iter1.txt')

  

####NL
mx.sgdmodel <- read.table('new_data/results_21feb_v02/sgdmodel_2comb_includNewFeatures_WoY_iter1.txt')
#mx.sgdmodel <- read.table('new_data/results_21feb_v02/sgdmodel_5comb_includNewFeatures_iter1.txt')
#mx.sgdmodel <- read.table('new_data/results_18feb_v02/sgdmodel_2comb_changHyper.txt')
mx.gb <- read.table('new_data/results_21feb_v02/gb_5comb_includNewFeatures_WoY_iter1.txt')
#mx.gb <- read.table('new_data/results_21feb_v01/gb_5comb_modifiOriginalHyper_includNewFeatures_iter1.txt')
#mx.gb <- read.table('new_data/results_18feb_v02/gb_5comb_modifiOriginalHyper_iter4.txt')
#gb_5comb_originalHyper_iter2.txt (good performance this iter2)--
#gb_5comb_modifiOriginalHyper_iter3.txt (2nd option) -- 
# gb_5comb_modifiOriginalHyper_iter4(best performance)
mx.rf <- read.table('new_data/results_21feb_v02/rf_5comb_includNewFeatures_WoY_iter1.txt')
#mx.rf <- read.table('new_data/results_21feb_v01/rf_5comb_includNewFeatures_iter1.txt')
#mx.rf <- read.table('new_data/results_18feb_v02/rf_5comb.txt')
mx.AR <- read.table('new_data/results_18feb_v02/AR.txt')
mx.true <- read.table('new_data/results_18feb_v02/true.txt')
mx.bench <- read.table('new_data/results_18feb_v02/bench.txt')

#Update FORECASTS with ensembles
#'sgdmodel',"xgb","AR", "true", "bench"
model.names <- c('sgdmodel', "gb", "rf", "AR", "true", "bench",
                 'ensemble.sgd.gb', 'ensemble.sgd.rf', 'ensemble.sgd.AR',
                 'ensemble.rf.gb','ensemble.rf.AR',
                 'ensemble.gb.AR',
                 'ensemble.sgd.gb.AR', 'ensemble.sgd.rf.AR', 'ensemble.sgd.rf.gb.AR')
M <- length(model.names)
r_name <- rownames(mx.true)
c_name <- colnames(mx.true)
#nw.forecasts <- array(NA, dim = c(N, H, M))
nw.forecasts <- array(NA, dim = c(length(r_name), length(c_name), M))
dimnames(nw.forecasts) <- list(r_name, c_name, model.names)

nw.forecasts[,, model.names[1]] <- as.matrix(mx.sgdmodel)
nw.forecasts[,, model.names[2]] <- as.matrix(mx.gb)
nw.forecasts[,, model.names[3]] <- as.matrix(mx.rf)
nw.forecasts[,, model.names[4]] <- as.matrix(mx.AR)
nw.forecasts[,, model.names[5]] <- as.matrix(mx.true)
nw.forecasts[,, model.names[6]] <- as.matrix(mx.bench)
nw.forecasts[,, model.names[7]] <- ( as.matrix(mx.sgdmodel) + as.matrix(mx.gb) )/2
nw.forecasts[,, model.names[8]] <- ( as.matrix(mx.sgdmodel) + as.matrix(mx.rf) )/2
nw.forecasts[,, model.names[9]] <- ( as.matrix(mx.sgdmodel) + as.matrix(mx.AR) )/2
nw.forecasts[,, model.names[10]] <- ( as.matrix(mx.rf) + as.matrix(mx.gb) )/2
nw.forecasts[,, model.names[11]] <- ( as.matrix(mx.rf) + as.matrix(mx.AR) )/2
nw.forecasts[,, model.names[12]] <- ( as.matrix(mx.gb) + as.matrix(mx.AR) )/2
nw.forecasts[,, model.names[13]] <- ( as.matrix(mx.sgdmodel) + as.matrix(mx.gb) + as.matrix(mx.AR) )/3
nw.forecasts[,, model.names[14]] <- ( as.matrix(mx.sgdmodel) + as.matrix(mx.rf) + as.matrix(mx.AR) )/3
nw.forecasts[,, model.names[15]] <- ( as.matrix(mx.sgdmodel) + as.matrix(mx.rf)  + as.matrix(mx.gb) + as.matrix(mx.AR) )/4

FORECASTS <- nw.forecasts
FFT <- FORECASTS

dim(FORECASTS)

for(i.m in 1:M) FFT[,,i.m]<- FORECASTS[, , "true"]
RES <- FORECASTS - FFT

# performance metrics
RMSE <- sqrt(apply(abs(RES)^2, c(3), mean, na.rm = TRUE))
RMSE
as.data.frame(RMSE) %>% arrange(RMSE)

MAEh <- apply(abs(RES), c(2,3), mean, na.rm = TRUE) 
MAE <- apply(abs(RES), c(3), mean, na.rm = TRUE) 
MAE
as.data.frame(MAE) %>% arrange(MAE)

View(mx.bench)
View(max(RES[,,'bench']))
sort(c(RES[,,'bench'])[!is.na(c(RES[,,'bench']))], decreasing = TRUE)

cat("finish..........................................................\n")

ts.plot(MAEh[1:200, model.names ], col = 1:8, ylab = "MAE")
legend("topleft", model.names, col = 1:8, lwd = 1)
abline(v = 0:10 * S, col = "orange")
abline(v = 0:10 * S - 8, col = "steelblue")


# error verification post prediction
ts.plot(as.vector(FORECASTS[,,'rf'])[1:(24*12)],col='red')
lines(as.vector(FORECASTS[,,'true'])[1:(24*19)],col='black')
lines(as.vector(FORECASTS[,,'AR'])[1:(24*19)],col='green')
lines(as.vector(FORECASTS[,,'sgdmodel'])[1:(24*19)],col='brown')

ts.plot(as.vector(FORECASTS[,,'sgdmodel'])[1:(24*1000)],col='red')
#lines(as.vector(FORECASTS[,,'bench'])[1:(24*30)],col='pink')
lines(as.vector(FORECASTS[,,'true'])[1:(24*1000)],col='blue')
#lines(as.vector(FORECASTS[,,'AR'])[1:(24*10)],col='green')
lines(as.vector(FORECASTS[,,'gb'])[1:(24*1000)],col='yellow')
