setwd("/home/user/Desktop/files_desktop/forecasting_energy_market")
source('code/functions_d2c.R')
#region
# %%
pw<- "#q6a21I&OA5k"
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

#####
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
#####
#require(remotes)
#install_version("caret",version= "6.0.80")
#remove.packages("caret")  
#install.packages('caret')
#packageVersion("readr")
##

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
country <- "NL"
ZONE <- c(country) 
i.z <- 1 # later in loop
zone <- ZONE[i.z]
CITY <- read_csv("data_test/worldcities.csv")
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
  select(timetoforecast_utc) %>% 
  tail(1)


cat('prepare edat dataset*******************************************************\n')
EDAT<- list()
ds_edat <- read.csv(str_replace("new_data/EDAT_XX.csv", "XX" , country))
ds_edat$DateTime <- as.POSIXct(ds_edat$DateTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
yt_name <- paste(country, "_Load_Actual",sep = '')
cat('na in met dataset*******************************************************\n')
colSums(is.na(ds_edat))
cat('apply imputation of na in met dataset*********************************************\n')
# I can use here 2 options:
# zoo::na.locf(ds_edat[,yt_name])
# na_interpolation(ds_edat[,yt_name], option='spline', method='natural')
ds_edat[,yt_name] <- na_interpolation(ds_edat[,yt_name], option='spline', method='natural')
EDAT[[country]] <- ds_edat


ds_edat %>% 
  arrange(desc(DateTime)) %>% 
  select(DateTime) %>% 
  head(2)

# plot to show nan values of the last days
plot(tail(ds_edat$DateTime,100),
     tail(ds_edat$NL_Load_Actual,100),
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

cat('agg additional features******************************************************\n')
SummerTime = lubridate::dst(DATA$DateTimeCET)
HoD = lubridate::hour(DATA$DateTime)
DoW = lubridate::wday(DATA$DateTime, week_start = 1)
DoY = lubridate::yday(DATA$DateTime)
MoY = lubridate::month(DATA$DateTime)
HoDDST = lubridate::hour(DATA$DateTimeCET)
DoWDST = lubridate::wday(DATA$DateTimeCET, week_start = 1)
DoYDST = lubridate::yday(DATA$DateTimeCET)
MoYDST = lubridate::month(DATA$DateTimeCET)
DET <- cbind(SummerTime, HoD, DoW, DoY, MoY, HoDDST, DoWDST, DoYDST, MoYDST)

cat('agg additional features and DATA ***************************************************\n')
DATA <- cbind(DATA, DET)

cat("add additional seasonality using cosine**********************************************\n")
DATA$cos365 <- cos(2*pi*DATA$DoY / 365.25)

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

# verify length of each subset
mm <- list()
for (i.hm in 1:N) mm[[i.hm]] <- length(IDTEST[[i.hm]])
for (v in IDTEST){
  cat(length(v),'\n')
}


#x <- as.matrix(DATA[,c(8)])
#ts.plot(x)
#summer = SummerTime
#colnames(DATA)
#xouts <- shiftDST(x, clag=c(168), SummerTime)
#head(xouts)
#xout <- shiftDST(x, clag=c(168))
#ts.plot(xout)
#View(cbind(xouts,xout))



# define models to estimate
# "true", "bench", "GAM", "AR", "hw"
model.names <- c("xgb","true") 
M <- length(model.names)
ytarget <- yt_name
# for (i.m in model.names)
FORECASTS <- array(, dim = c(N, H, M))
dimnames(FORECASTS) <- list(format(FSTUDYDAYS, "%Y-%m-%d"), paste("h_", 1:H, sep = ""), model.names)
cat('dimension of result matrix*******************************************************\n')
dim(FORECASTS)

# run model estimation
S <- 24
for (i.m in seq_along(model.names)) {
    mname <- model.names[i.m]
    cat('\\\\\\\\-- begin model', mname, '........i.m*******************************************//////////\n',sep = ' ')
  
    if (mname %in% c("true", "bench", "GAM", "xgb")) {
        LAGS <- S * c(1:14, 21, 28)
        horizonc <- unique(c(0, findInterval(LAGS, 1:H)))
    } else { # AR
        horizonc <- c(0, H)
    }
    
    # define horizon separation
    reest <- 20 
    FSTUDYSEQid <- unique(c(seq(0, length(FSTUDYDAYS), by = reest), length(FSTUDYDAYS)))
    FSTUDYSEQ <- list()
    for (i.seq in 1:(length(FSTUDYSEQid) - 1)) FSTUDYSEQ[[i.seq]] <- FSTUDYDAYS[c((FSTUDYSEQid[i.seq] + 1), FSTUDYSEQid[i.seq + 1])]
    Nsplitlen <- length(FSTUDYSEQ) 
    cat('# of horizon separation *N*', Nsplitlen, 'of model', mname, '*******************************************\n',sep = ' ')
    
    # model specific data preparation for the forecasting study [model dependent]: 
    if(mname == "GAM" | mname == "xgb"){
        #DATA$DateTime
        vec <- as.integer(DATA$DateTime)
        subs <- match(unique(vec), vec)
        TMPDATA <- bind_cols(DateTime = DATA$DateTime[subs], 
                             shiftDST(DATA[subs,ytarget],
                                      summer = DATA$SummerTime[subs], 
                                      clag = LAGS) )
        
        #bind_cols(EDAT[[zone]][, "DateTime"], as.data.table(shift(as.data.frame(EDAT[[zone]][, 1:2 + 1]), LAGS, give.names = TRUE)))
        dff <- as.data.frame(base::as.factor(DATA$HoD))
        names(dff) <- "HoD"
        MHoW <- as.matrix(sparse.model.matrix(~.-1, data = dff))
        
        TMPDATA <- cbind(TMPDATA, MHoW[subs,])
        FDATA <- dplyr::full_join(DATA, TMPDATA, by = c("DateTime")) %>% arrange(DateTime, horizon) 
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
            #View(FDATA[FDATA$DateTime <= FSTUDYSEQ[[i.N]][1],])
            # View(FDATA[FDATA$DateTime <= FSTUDYSEQ[[i.N]][1] & FDATA$horizon >= hmin & FDATA$horizon <= hmax & !is.na(FDATA$horizon),])
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
            if (mname == "xgb") {
                # 5+'kk'
                act_lags <- LAGS[LAGS >= hmax]
                #colnames(DATAtrain)
                

                features_x <- c(paste("HoD",0:23, sep=""),
                                paste("x_lag_",S * c(1:14, 21, 28), sep=""),
                                'cos365')
                
                formula_str <- paste(
                  paste(ytarget,' ~ ',sep = ''),
                  paste( paste("HoD",0:23, sep=""), collapse=' + '),' + ',
                  paste( paste("x_lag_",S * c(1:14, 21, 28), sep = '', collapse=' + '), '+ cos365'), sep = '')
                
                filter_train <- DATAtrain[, c(ytarget,features_x)] %>% 
                                                                  replace(is.na(.), 0)
                filter_test <- DATAtest[, c(features_x)] %>% 
                                                            replace(is.na(.), 0)
                
                #colSums(is.na(filter_train))
                #class(filter_train)
                #5+"jol"
                tuneGrid = expand.grid(nrounds = c(5),
                                       eta = c(0.001),
                                       lambda = c(0.5),
                                       alpha = 0.01)
                #write.csv(filter_train, "/home/user/Desktop/files_desktop/forecasting_energy_market/mmz.csv")
                #class(filter_train)
                #filter_train <- read.csv("/home/user/Desktop/files_desktop/forecasting_energy_market/mmz.csv")
                cat('estimating xgb.....\n')
                gbm_op <- train(as.formula(formula_str),
                                data = filter_train,
                                method = "xgbLinear",
                                metric = "RMSE",
                                tuneGrid = tuneGrid,
                                verbose=FALSE)
                cat('finish xgb.....\n')
                #gbm_op$bestTune                                  
                #pred_y = predict(gbm_op, filter_test)
                #View(filter_train)
                cat('test has: ',dim(filter_test))
                cat(length(HORIZON[[i.hl]]),'----' ,length(seqid),'\n')
                pred <- t(matrix(predict(gbm_op, newdata = filter_test), nrow = length(HORIZON[[i.hl]]), ncol= length(seqid), byrow = TRUE))

               


                #pred_yt = predict(gbm_op, filter_train %>% select(-ytarget))
                #ts.plot(cbind(DATAtest[, c(ytarget)], pred_y), col= c('red','blue'))
                #ts.plot(cbind(DATAtrain[, c(ytarget)], pred_yt), col= c('red','blue'))
                #ts.plot(sin(2*pi*DATA$DoY / 365.25))
                #ts.plot(cos(2*pi*DATA$DoY / 365.25))
                #ts.plot(DATA[,ytarget])
                #ts.plot(head(DATAtest[,"cos365"],300))
                #as.matrix(sparse.model.matrix(~.-1, data = dff))
                #class(sparse.model.matrix(~.-1, data = dff))
                #class(as.matrix(DATAtrain[, features_x]))
                
                
            
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
                pred <- matrix(, length(seqid), H)
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
              #x_exog <- fastDummies::dummy_cols(tmp, 
              #                                  select_columns = c("SummerTime","DoW","DoY", "MoY"),
              #                                  remove_first_dummy = TRUE,
              #                                  remove_selected_columns =TRUE)
 
              #x_exog <- x_exog[, c(22:ncol(x_exog)) ]
              mod <- auto.arima(zoo::na.locf(y),
                                max.p = om, max.q = om, d = 0,
                                max.P = om, max.Q = om,
                                seasonal = TRUE, ic = "aic" )
              #xreg = as.matrix(x_exog[1:length(y), c(1:7,372:382)], 
              #                 nrow= length(y) , ncol= ncol(x_exog) ) 
              #sm <- sarima (y, 1,0,1, 2,1,2,24, no.constant=TRUE)
              #acf2(resid(sm$fit))
              #predict(sm, newdata = 0.5*(y[1:24]), n.ahead= 24)
              #forecast(sm, h= 24)
              
              pred <- matrix(, length(seqid), H)
              
              #for (i.NN in 1:length(seqid)){
              #  i.NN <- 2
              #  ym <- tmp[seq(1, (yn + (S * (i.NN-1)) )), ytarget ]
              #  exog_temp <- as.matrix(x_exog[ seq(1, (yn + (S * (i.NN-1)) )), c(1:7,372:382) ])
              #  dim(exog_temp)
              #  length(ym)
                #pred[i.NN, ] <- 
              #  zz <- forecast(mod,
              #          xreg = exog_temp,
              #          h = H)$mean
              #  ts.plot(zz)
              #  pp <- predict(mod, newdata = ym,
              #          n.ahead = 24, newxreg= exog_temp)$pred
              #  ts.plot(head(pp,48))
              #  ts.plot(head(y,72))
              #  ts.plot(zz)
              #  length(pp)
              #  length(ym)
                
              #  ts.plot(predict(mod, newdata = tmp[yn + (-0 + 1):(0 + (2 - 1) * S), c(ytarget)],
              #          newxreg = as.matrix(x_exog[ yn + (-0 + 1):(0 + (2 - 1) * S), c(1:7,372:382) ] ),
              #          n.ahead = H)$pred)
                
                
              #}
              
              
              #predict(mod, newdata = tmp[yn + (-mod$order + 1):0 + (2 - 1) * S, c(ytarget)],
             #        n.ahead = H, newxreg= as.matrix(x_exog[yn + (-mod$order + 1):0 + (2 - 1) * S, c("SummerTime","DoW")]) )$pred
              
              pred <- matrix(, length(seqid), H)
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
    cat('....******* process for the model:', mname, 'finished****.......\n\n',sep = ' ')
} # i.m

#xgb_true_pred <- FORECASTS

FFT <- FORECASTS

for(i.m in 1:M) FFT[,,i.m]<- FORECASTS[, , "true"]
RES <- FORECASTS - FFT
View(FORECASTS[, , "xgb"])
View(FORECASTS[, , "true"])
View(RES[, , "AR"])
ts.plot(FORECASTS[, , "xgb"][3,])

tail(head(DATATEST[DATATEST$DateTime >= ymd_hms('2022-07-28- 09:00:00'),] %>% 
       arrange(DateTime,horizon),10),10)

ts.plot(c(as.vector(FORECASTS[,,"hw"][17,]),
          as.vector(FORECASTS[,,"hw"][18,]) ) )

ts.plot(
  c(FORECASTS[,,"AR"][1:200,])
)

ts.plot(c(as.vector(FORECASTS[,,"hw"][17,]),
          as.vector(FORECASTS[,,"hw"][18,]),
          as.vector(FORECASTS[,,"hw"][19,]),
          as.vector(FORECASTS[,,"hw"][20,]),
          as.vector(FORECASTS[,,"hw"][21,]),
          as.vector(FORECASTS[,,"hw"][22,]),
          as.vector(FORECASTS[,,"hw"][23,]),
          as.vector(FORECASTS[,,"hw"][24,]), 
          as.vector(FORECASTS[,,"hw"][25,]),
          as.vector(FORECASTS[,,"hw"][26,]),
          as.vector(FORECASTS[,,"hw"][27,]),
          as.vector(FORECASTS[,,"hw"][28,]),
          as.vector(FORECASTS[,,"hw"][29,]) ))

#FFT<- FORECASTS
#ensemble_pred <- FORECASTS[,,'AR']+FORECASTS[,,'hw']/2
#new_forecast <- array( c( FORECASTS , ensemble_pred ) , dim = c( 147 , 240 , 5) )
#dimnames(new_forecast) <- list(format(FSTUDYDAYS, "%Y-%m-%d"), paste("h_", 1:H, sep = ""), c(model.names,'ensemble') )
#FORECASTS <- new_forecast
#FFT<- FORECASTS

#for(i.m in c(model.names,'ensemble') )FFT[,,i.m]<- FORECASTS[, , "true"]
#RES<- FORECASTS - FFT

RMSE <- sqrt(apply(abs(RES)^2, c(3), mean, na.rm = TRUE))
RMSE


MAEh <- apply(abs(RES), c(2,3), mean, na.rm = TRUE) 
MAE <- apply(abs(RES), c(3), mean, na.rm = TRUE) 
MAE

View(RES[,,'xgb'])

ts.plot(MAEh[1:20,c('true','hw','AR')], col = 1:8, ylab = "MAE")
legend("topleft", model.names, col = 1:8, lwd = 1)
abline(v = 0:10 * S, col = "orange")
abline(v = 0:10 * S - 8, col = "steelblue")
View(MAEh[,c('true','AR')])
# %%
#endregion

