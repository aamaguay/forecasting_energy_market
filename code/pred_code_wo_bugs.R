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
#ts.plot(DATA$weekend)
#plot of seasonal patterns
mmzz <- DATA %>% distinct(DateTime, .keep_all= TRUE) %>% arrange(DateTime,horizon)
plot(head(mmzz$DateTime,16+(24*1000)),head(mmzz$NL_Load_Actual,16+(24*1000)),type='l')

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

cat("holidays preparation********************************\n")
holidays <- read_csv("data_22dec_2022/holidays_2000_2030.csv")
holidays$Name[] <- gsub(" ", "", holidays$Name)
get.HLD<- function(xtime, zone="DE", S=24, deg=3, bridgep = 0.5, k=0.25){
  # zone only supports full countries at the moment 
  # deg is, cubic spline degree
  # fraction of bridging effects, if 0.5 then half a day before and after the holiday potential effects can occur, values between 0 and 2 are reasonable
  # k determines the number of grid points for the splines, if k=1/S then each data point we have a new basis (if in addition deg=1, these are indicators), the smaller k the more basis functions.
  yrange<- range(lubridate::year(xtime))+c(-1,1) # safety margins [allows for predictions up to 1 year ahead]
  holidays <- holidays %>% filter(CountryCode == zone) %>%
    filter(lubridate::year(Date)>=yrange[1] & lubridate::year(Date)<=yrange[2] ) 
  # remove holidays which were not lauched yet.
  holidays$LaunchYear[is.na(holidays$LaunchYear)]<- 0
  holidays %>% filter(lubridate::year(Date)>= LaunchYear ) 
  #holidays %>% select(Date, Name) %>% mutate(
  #            DoW = lubridate::wday(Date, week_start = 1)
  #            )## TODO think about more information es. Global and counties
  #mutate_at(vars(x, y), factor)
  holidays$Name <- as.factor(holidays$Name)
  holnames<- levels(holidays$Name)
  #holnames <- gsub(" ", "",holnames)
  hldN<- length(holnames)
  
  xbas<- -(S*bridgep):((1+bridgep)*S)
  xbask<- seq(min(xbas), max(xbas), length=k*(length(xbas)-1)+1)
  library(splines)
  hldbas<- splineDesign(xbask, xbas, outer.ok=TRUE, ord=deg+1)
  hldbasc<- t(apply(hldbas,1,cumsum))
  hldbasx<- hldbas#cbind(hldbas,hldbasc)
  K<- dim(hldbasx)[2]
  
  DHL<- array(0, dim=c(length(xtime),K*hldN) ) ## add cumulative
  i.hld<-1
  for(i.hld in 1:hldN){
    idhld <- which<-which(xtime %in% as.POSIXct( holidays$Date[ holidays$Name==holnames[i.hld]], tz="UTC"))
    # idhld<- which(as.numeric(format(dtime, "%m")) == DATEhld[[i.hld]][1]	& as.numeric(format(dtime, "%d")) == DATEhld[[i.hld]][2] & as.numeric(format(dtime, "%H")) == 0)
    for(i.b in seq_along(idhld)){ ## add basis segments
      idb<- (idhld[i.b]+min(xbas)):(idhld[i.b]+max(xbas)) ## TODO does not work properly if hld is first or last day...
      idbact<- which(idb>0 & idb<= dim(DHL)[1])
      DHL[idb[idbact],(1:K)+K*(i.hld-1)]<- hldbasx[idbact,]
    }
  }
  
  ## holidays prepared but not used.
  dimnames(DHL)<- list(NULL,paste0(rep(holnames, rep(K,length(holnames))),"_",1:K))
  DHL 
}

tmp <-get.HLD(DATA$DateTime, zone="NL")

length(unique(data.frame(holidays[holidays$Date>="2021-01-01",'Name'])$Name))
length((holidays[holidays$Date>="2021-01-01",'Name']))
View(tmp)
dim(tmp)
dim(DATA)

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
# "true", "bench", "GAM", "AR", "hw", "elasticNet", 'sgdmodel'
model.names <- c("xgb","AR", "true", "bench") 
M <- length(model.names)
ytarget <- yt_name
# for (i.m in model.names)
FORECASTS <- array(, dim = c(N, H, M))
dimnames(FORECASTS) <- list(format(FSTUDYDAYS, "%Y-%m-%d"), paste("h_", 1:H, sep = ""), model.names)
cat('dimension of result matrix*******************************************************\n')
dim(FORECASTS)
#define ls of training time
ls_train_time <- list()

# run model estimation
S <- 24
for (i.m in seq_along(model.names)) {
  mname <- model.names[i.m]
  init_time <- Sys.time()
  cat('\\\\\\\\-- begin model', mname, '........i.m*******************************************//////////\n',sep = ' ')
  
  if (mname %in% c("true", "bench", "GAM", "xgb", "elasticNet", "sgdmodel")) {
    LAGS <- S * c(1:14, 21, 28)
    horizonc <- unique(c(0, findInterval(LAGS, 1:H)))
  } else { # AR
    horizonc <- c(0, H)
  }
  
  # define hyperparamters once
  if (mname == 'xgb'){
    lgb.grid <- base::expand.grid(
      list(
        boosting= c("gbdt","dart"),
        obj = c("mse"),
        num_leaves = seq(20, 27, 1),
        max_depth = seq(7,10,1),
        min_sum_hessian_in_leaf = c(6,7),
        bagging_fraction = c(0.8),
        learning_rate = seq(0.18, 0.3, 0.01),
        num_iterations = seq(40, 50, 3),
        lambda_l1 = seq(0.01, 0.1, 0.01),
        lambda_l2 = seq(0.01, 0.1, 0.01)
      ))
  }
  #dim(lgb.grid)#96768000
  
  # define horizon separation
  reest <- 20 
  FSTUDYSEQid <- unique(c(seq(0, length(FSTUDYDAYS), by = reest), length(FSTUDYDAYS)))
  FSTUDYSEQ <- list()
  for (i.seq in 1:(length(FSTUDYSEQid) - 1)) FSTUDYSEQ[[i.seq]] <- FSTUDYDAYS[c((FSTUDYSEQid[i.seq] + 1), FSTUDYSEQid[i.seq + 1])]
  Nsplitlen <- length(FSTUDYSEQ) 
  cat('# of horizon separation *N*', Nsplitlen, 'of model', mname, '*******************************************\n',sep = ' ')
  
  # model specific data preparation for the forecasting study [model dependent]: 
  if(mname %in% c("GAM", "xgb", "elasticNet", "sgdmodel")){
    #DATA$DateTime
    vec <- as.integer(DATA$DateTime)
    subs <- match(unique(vec), vec)
    TMPDATA <- bind_cols(DateTime = DATA$DateTime[subs], 
                         shiftDST(DATA[subs,ytarget],
                                  summer = DATA$SummerTime[subs], 
                                  clag = LAGS) )
    
    #bind_cols(EDAT[[zone]][, "DateTime"], as.data.table(shift(as.data.frame(EDAT[[zone]][, 1:2 + 1]), LAGS, give.names = TRUE)))
    dff <- as.data.frame(base::as.factor(DATA$HoD))
    names(dff) <- c("HoD")
    MHoW <- as.matrix(sparse.model.matrix(~.-1, data = dff))
    
    dfDoW <- as.data.frame(base::as.factor(DATA$DoW))
    names(dfDoW) <- c("DoW")
    MDoW <- as.matrix(sparse.model.matrix(~.-1,data = dfDoW))
    
    dfMoY <- as.data.frame(base::as.factor(DATA$MoY))
    names(dfMoY) <- c("MoY")
    MMoY <- as.matrix(sparse.model.matrix(~.-1,data = dfMoY))
    
    dfQoY <- as.data.frame(base::as.factor(DATA$QoY))
    names(dfQoY) <- c("QoY")
    MQoY <- as.matrix(sparse.model.matrix(~.-1,data = dfQoY))
    
    weekend_mx <- matrix(DATA$weekend, nrow = nrow(DATA), ncol = 1 )
    colnames(weekend_mx) <- c("weekend")
    all_dummys <- cbind(MHoW, MDoW, MMoY,MQoY, weekend_mx )

    cat("interaction features****************************************************\n")
    paste_hod <- paste("HoD",0:22, sep="")
    paste_dow <- paste("DoW",1:6, sep="")
    paste_moy <- paste("MoY",1:11, sep="")
    paste_qoy <- paste("QoY",1:3, sep="")
    paste_seasonality <- as.vector( sapply(c("HoD","DoW", "MoY", "DoY", "WoY", "DoM", "QoY"), 
                                           function(x) c(paste(x,'_sin',sep = ''), paste(x,'_cos',sep = '')   )) )
    col_comb_features <- c(paste_dow, paste_hod, paste_moy, paste_qoy, 'weekend' )
    all_comb <- combn(col_comb_features, 2)
    ls_ds_interaction <- list()
    for (ncomb in 1:ncol(all_comb)){
      #View(all_comb)
      pt_1 <- all_comb[1,ncomb]
      pt_2 <- all_comb[2,ncomb]
      if ( substr(pt_1, start=1, stop=3) != substr(pt_2, start=1, stop=3) ){
        ls_ds_interaction[[ncomb]] <- paste(pt_1,pt_2,sep = 'and')
      }
    }
    ls_ds_interaction <- Filter(Negate(is.null), ls_ds_interaction)
    ls_ds_interaction <- str_split(ls_ds_interaction,'and')
    mx_interaction <- matrix(,ncol = 2, nrow = length(ls_ds_interaction) )
    for (i_comb in 1:length(ls_ds_interaction)) mx_interaction[i_comb,] <- c(ls_ds_interaction[[i_comb]][1],ls_ds_interaction[[i_comb]][2])
    ds_interaction <- setNames(data.frame(mx_interaction), c('x','y') )
    
    tmp_ds_transf <- list()

    tmp_ds_transf[[1]] <- all_dummys[,col_comb_features]
    ls_ds_transf <- list()
    for (id_ds in length(tmp_ds_transf)){
      ds_tmp_orig <- tmp_ds_transf[[id_ds]]
      comb_vector <- ds_interaction
      name_vector_comb <- sub("\\.var", ".", do.call(paste, c(comb_vector, sep=".")))
      formula_evaluate <- sprintf("transform(ds_tmp_orig, %s = %s*%s)", name_vector_comb, 
                                  comb_vector[,1], comb_vector[,2])
      colnames(ds_tmp_orig)
      for(i in seq_along(formula_evaluate)) ds_tmp_orig <- eval(parse(text = formula_evaluate[i]))
      final_transf_ds <- ds_tmp_orig[,c(name_vector_comb)]
      ls_ds_transf[[id_ds]] <- final_transf_ds
    }
    #define features to use
    features_interaction <- colnames(ls_ds_transf[[1]])
    features_x <- c(paste_hod, paste_dow, paste_moy,
                    paste_qoy, paste("x_lag_",S * c(1:14, 21, 28), sep=""),
                    paste_seasonality, features_interaction,
                    'weekend', "SummerTime")
    
    TMPDATA <- cbind(TMPDATA, all_dummys[subs, -unlist(list(ncol(all_dummys)))], ls_ds_transf[[1]][subs,])
    
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
        i.hm <- 222
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
        
        act_lags <- LAGS[LAGS >= hmax]

        formula_str <- paste(
          paste(ytarget,' ~ ',sep = ''), 
          paste( paste_dow, collapse=' + '),' + ',
          paste( paste_moy, collapse=' + '),' + ',
          paste( paste_hod, collapse=' + '),' + ',
          paste( paste_qoy, collapse=' + '),' + ',
          paste( paste_seasonality, collapse=' + '),' + ',
          paste( features_interaction, collapse=' + '),' + ',
          paste( paste("x_lag_",S * c(1:14, 21, 28), sep = '', collapse=' + '), '+ weekend+ SummerTime'), sep = '')
        
        #DATAtrain[, c(features_x[1:565])]
        #length(features_x[1:566])
        #colnames(DATAtrain)
        filter_train <- DATAtrain[, c(ytarget, features_x)] %>% 
          replace(is.na(.), 0)
        filter_test <- DATAtest[, c(features_x)] %>% 
          replace(is.na(.), 0)
        
        set.seed(1)
        samp <- sample(1:nrow(lgb.grid ), 12)
        lgb.gridFilter <- lgb.grid [samp,]
        # create datasets
        lgb.test <- lgb.Dataset(data = as.matrix(filter_test),
                                label = DATAtest[,ytarget] )
        lgb.train <- lgb.Dataset(data = as.matrix(filter_train %>% select(-ytarget) %>% head(nrow(filter_train)-20) ),
                                 label = head(filter_train[,ytarget], nrow(filter_train)-20) )
        lgb.valid <- lgb.Dataset(data = as.matrix(filter_train %>% select(-ytarget) %>% tail(20) ),
                                 label = head(filter_train[,ytarget], tail(20) ) )
        
        #6+"jjj"
        estimate.lgb <- function(hyper_combi, lgb.train, lgb.valid, lgb.gridFilter){
          ls_params <- as.list(data.frame(lgb.gridFilter[hyper_combi,]) )
          obj <- as.character(ls_params$obj)
          ls_params <- within(ls_params, rm(obj))
          watchlist <- list(validation = lgb.valid)
          lgb_alg <- lgb.train(params = ls_params,obj = obj,
                               data = lgb.train, valids = watchlist,
                               early_stopping_rounds = 70,verbose = 1,
                               eval_freq = 10, force_col_wise=TRUE,
                               nthread = 6)
          return( list(mod = lgb_alg, score = lgb_alg$best_score) )
        }
        
        #recent_time <- Sys.time()
        cat('train lgb.....\n')
        est.lgb <- lapply(1:nrow(lgb.gridFilter), FUN = function(hyper_combi) estimate.lgb(hyper_combi,
                                                                                             lgb.train,
                                                                                             lgb.valid,
                                                                                             lgb.gridFilter))
        cat('finish lgb.....\n')
        #last_time <- Sys.time()
        #diff_mode_iter <- difftime(recent_time,last_time)
        #View(cbind(lgb.gridFilter,rmse_vec))
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
      if (mname == "elasticNet") {
        filter_train <- DATAtrain[, c(ytarget, features_x)] %>% 
          replace(is.na(.), 0)
        filter_test <- DATAtest[, c(ytarget, features_x)] %>% 
          replace(is.na(.), 0)
        #filter_valid <- filter_train %>% tail(20)
        #filter_train <- filter_train %>% head(nrow(filter_train)-20)
        
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
        #View(elastic_net_reg$results)
        #end_time1 <- Sys.time()
        #print(difftime(init_time1,end_time1))
        #10*33*6/60
        
        pred <- t(matrix(predict(elastic_net_reg, as.matrix(filter_test %>% select(-ytarget))), 
                         nrow = length(HORIZON[[i.hl]]), ncol= length(seqid), byrow = TRUE))
        #ts.plot(pred)
        # since 18:00pm to 7:00am
        #plot(y = mm[, c(ytarget)][48+9:(24*5)],
        #     x =mm[, c("DateTime")][48+9:(24*5)], type = 'l')
        #View(mm)
      }
      if (mname == "sgdmodel"){
        filter_train <- DATAtrain[, c(ytarget, features_x)] %>% 
          replace(is.na(.), 0)
        filter_test <- DATAtest[, c(ytarget, features_x)] %>% 
          replace(is.na(.), 0)
        filter_valid <- filter_train %>% tail(20)
        filter_train <- filter_train %>% head(nrow(filter_train)-20)
        
        lambdas <- c(10,20,30,40 )
        alphas <- 1
        #5+"kk"
        estimate.sgd <- function(id_val, filter_train, filter_valid, alphas, lambdas, ytarget){
          lambda_val <- lambdas[id_val]
          sgd_reg = sgd(x = as.matrix(filter_train %>% select(-ytarget) ),
                        y = filter_train[,ytarget], model = "lm",
                        model.control = list(lambda1 = alphas, lambda2 = lambda_val),
                        sgd.control = list(method = 'ai-sgd'))
          sgd_pred_val <- predict(sgd_reg, newdata = as.matrix(filter_valid %>% select(-ytarget) ) )
          rmse_val <- RMSE(as.matrix(filter_valid %>% select(ytarget)), sgd_pred_val)
          #return(c(lambda_val, rmse_val))
          return(list(lambda = lambda_val,rmse = rmse_val, mod = sgd_reg ))
        }

        #init_time1 <- Sys.time()
        est.sgd <- mclapply(1:length(lambdas), FUN = function(i) estimate.sgd(i, filter_train, 
                                                                                filter_valid, alphas,
                                                                              lambdas, ytarget)) 
        #hist(est.sgd[[4]]$mod$coefficients)
        #end_time1 <- Sys.time()
        #print(difftime(init_time1,end_time1))

        rmse_vec = c()
        for (i in 1:length(lambdas)) rmse_vec[i] <- est.sgd[[i]]$rmse
        id_min_rmse <- which.min(rmse_vec)
        best_lambda <- lambdas[id_min_rmse]
        best_tune_model <- est.sgd[[id_min_rmse]]$mod
        test_sparse  = Matrix(as.matrix(filter_test %>% select(-ytarget)), sparse=TRUE)
        pred <- t(matrix(predict(best_tune_model, newdata = test_sparse), 
                         nrow = length(HORIZON[[i.hl]]), ncol= length(seqid), byrow = TRUE))
        #ts.plot(as.vector(t(pred)))

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
        mod <- auto.arima(zoo::na.locf(y),
                          max.p = om, max.q = om, d = 0,
                          max.P = om, max.Q = om,
                          seasonal = TRUE, ic = "aic" )
        
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
  end_time <- Sys.time()
  cat('....******* model training of', mname,',lasted:...\n',sep = ' ')
  print(difftime(init_time,end_time))
  ls_train_time[[mname]] <- (difftime(init_time,end_time))
  cat('....******* process for the model:', mname, 'finished****.......\n\n',sep = ' ')
} # i.m

#Update FORECASTS with ensembles
model.names <- c("xgb","AR", "true", "bench", 'sgdmodel',
                 'ensemble.sgd.xgb',
                 'ensemble.sgd.AR',
                 'ensemble.sgd.xgb.AR')
M <- length(model.names)
r_name <- rownames(FORECASTS[,,model.names[1]])
c_name <- colnames(FORECASTS[,,model.names[1]])
nw.forecasts <- array(NA, dim = c(N, H, M))
dimnames(nw.forecasts) <- list(r_name, paste("h_", 1:H, sep = ""), model.names)

nw.forecasts[,, model.names[1]] <- FORECASTS[,,model.names[1]]
nw.forecasts[,, model.names[2]] <- FORECASTS[,,model.names[2]]
nw.forecasts[,, model.names[3]] <- FORECASTS[,,model.names[3]]
nw.forecasts[,, model.names[4]] <- FORECASTS[,,model.names[4]]
nw.forecasts[,, model.names[5]] <- FORECASTS[,,model.names[5]]
nw.forecasts[,, model.names[6]] <- (FORECASTS[,,model.names[5]] + FORECASTS[,,model.names[1]])/2
nw.forecasts[,, model.names[7]] <- (FORECASTS[,,model.names[5]] + FORECASTS[,,model.names[2]])/2
nw.forecasts[,, model.names[8]] <- (FORECASTS[,,model.names[5]] + FORECASTS[,,model.names[2]]+
                                      FORECASTS[,,model.names[1]])/3

FORECASTS <- nw.forecasts
FFT <- FORECASTS

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

# plot de errors of MAE
ts.plot(MAEh[1:200,model.names], col = 1:8, ylab = "MAE")
legend("topleft", model.names, col = 1:8, lwd = 1)
abline(v = 0:10 * S, col = "orange")
abline(v = 0:10 * S - 8, col = "steelblue")

# error verification post prediction
ts.plot(as.vector(FORECASTS[,,'xgb'])[14400:(4800*9)],col='red')
lines(as.vector(FORECASTS[,,'true'])[14400:(4800*9)],col='blue')
lines(as.vector(FORECASTS[,,'AR'])[14400:(4800*9)],col='green')

ts.plot(as.vector(FORECASTS[,,'sgdmodel'])[1:(24*1000)],col='red')
#lines(as.vector(FORECASTS[,,'bench'])[1:(24*30)],col='pink')
lines(as.vector(FORECASTS[,,'true'])[1:(24*1000)],col='blue')
#lines(as.vector(FORECASTS[,,'AR'])[1:(24*10)],col='green')
lines(as.vector(FORECASTS[,,'xgb'])[1:(24*1000)],col='yellow')

ts.plot(as.vector(FORECASTS["2021-03-30",,'sgdmodel']),col='red')
ts.plot(as.vector(FORECASTS["2021-03-28",96:(72+48),'true']),col='blue')
lines(as.vector(FORECASTS["2021-03-30",,'xgb']),col='green')

View(FORECASTS[,,'sgdmodel'])
View(FORECASTS[1:(20*4),,'xgb'])
View(FORECASTS[1:(20*4),,'true'])
#2021-09-08

as.vector(FORECASTS[,,'sgdmodel'])[1:(24*10)]
