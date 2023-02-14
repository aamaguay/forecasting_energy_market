setwd("/home/user/Desktop/files_desktop/forecasting_energy_market")

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

#####
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
#####

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
db <- dbxConnect(
    adapter = RMariaDB::MariaDB(),
    host = "132.252.60.112",
    port = 3306,
    dbname = "DWD_MOSMIX",
    user = "student", # Read only user
    password = pw,
    timezone_out = "UTC"
)

## locations in 'wider' EUROPE in DWD_MOSMIX
LOCATIONS <- tbl(db, "locations") %>%
    filter(
        longitude >= -30,
        longitude <= 60,
        latitude >= 30,
        latitude <= 80
    ) %>%
    collect()

ZONE <- c("LU") # "BE", "NL", "LU"
i.z <- 1 # later in loop
zone <- ZONE[i.z]

CITY <- read_csv("data_test/worldcities.csv")

citythr <- 3 ## for simplification take citythr largest cities

cityidsub <- CITY %>%
    filter(iso2 %in% ZONE) %>%
    group_by(iso2) %>%
    slice_max(order_by = population, n = 3) %>%
    split(.$iso2) # Order may change compared to ZONE

STAT <- purrr::map(cityidsub, .f = function(x) {
    stationid <- numeric(nrow(x))
    for (i.x in 1:nrow(x)) { ## to loop across cityidsub
        L2toCity <- (LOCATIONS$longitude - x$lng[i.x])^2 +
            (LOCATIONS$latitude - x$lat[i.x])^2
        stationid[i.x] <- LOCATIONS$stationid[which.min(L2toCity)]
    }
    return(stationid)
})

meteovar <- c("TTT", "Rad1h", "Neff", "RR1c", "FF", "FX1", 'R602','TN','TX','RRS1c')
inith <- 8 ## hour of day to use forecast, 8 means take data from 8am morning

MET <- list()

for (zn in ZONE) {
    MET[[zn]] <- tbl(db, "mosmix_s_forecasts") %>%
        filter(
            stationid %in% !!STAT[[zn]]
        ) %>%
        select(
            stationid, horizon, timetoforecast_utc, all_of(meteovar)
        ) %>%
        log_midpipe("Start collecting") %>%
        collect() %>%
        log_midpipe("Collecting done") %>%
        mutate(forecast_origin = timetoforecast_utc - horizon * 3600) %>%
        filter(lubridate::hour(forecast_origin) == inith) %>%
        group_by(forecast_origin, horizon) %>%
        summarise_at(all_of(meteovar), mean, na.rm = TRUE) %>%
        arrange(forecast_origin, horizon) %>%
        mutate(
            timetoforecast_utc = forecast_origin + horizon * 3600, .before = 1
        ) #%>%
        # mutate(
        #     HoD = hour(timetoforecast_utc),
        #     DoW = wday(timetoforecast_utc, week_start = 1),
        #     DoY = yday(timetoforecast_utc),
        #     .after = 1
        # )
    cat(zn, "\n")
}

#write_csv(MET[['BE']], 'data_14feb_2023/MET_BE_additional.csv')
#write_csv(MET[['NL']], 'data_14feb_2023/MET_NL_additional.csv')
#write_csv(MET[['LU']], 'data_14feb_2023/MET_LU_additional.csv')


# write_csv(MET[['LU']], 'data_22dec_2022/MET_LU_additional.csv')
# write_csv(MET[['BE']], 'data_22dec_2022/MET_BE_additional.csv')
# write_csv(MET[['NL']], 'data_22dec_2022/MET_NL_additional.csv')

# write_csv(MET[['BE']], 'data_22dec_2022/MET_BE.csv')
# write_csv(MET[['NL']], 'data_22dec_2022/MET_NL.csv')
# write_csv(MET[['LU']], 'data_22dec_2022/MET_LU.csv')

# write_csv(MET[['DE']], 'MET_DE.csv')
# write_csv(MET[['BE']], 'MET_BE.csv')
# write_csv(MET[['LU']], 'MET_LU.csv')
# write_csv(MET[['NL']], 'MET_NL.csv')

dbxDisconnect(db)
lu <- read.csv('MET_LU.csv')
be <- read.csv('MET_BE.csv')
nl<- read.csv('MET_NL.csv')
MET <- list()
MET[['LU']] <- lu
MET[['BE']] <- be
MET[['NL']] <- nl
ZONE <- c('LU','BE','NL')
length(MET)
zn <- "NL"
MET[[zn]] <- read.csv("data_14feb_2023/EDAT_NL.csv")
EDAT[[zn]]$DateTime
max(as.POSIXct(EDAT[[zn]]$DateTime) )

# %%
#endregion
#as_datetime()
#region get entsoe data
# %% Get ENTSOE Data
edb <- dbxConnect(
    adapter = RMariaDB::MariaDB(),
    host = "132.252.60.112",
    port = 3306,
    dbname = "ENTSOE",
    user = "student", # Read only user
    password = pw,
    timezone_out = "UTC"
)

EDAT<- list()
ZONE <- c("LU") # "BE", "NL", "LU"
for (zn in ZONE) {

    # Say you want to get wind generation, actual and day-ahead for PT (portugal)
    # only where the year is 2022

    # Obtain specification table
    spec <- tbl(edb, "spec") %>% collect()

    # Get an overview
    glimpse(spec)

    unique(spec$Name) # We need "Generation" here...
    unique(spec$Type) # "DayAhead" and "Actual" ...
    unique(spec$ProductionType) # "Wind Onshore" and "Wind Offshore" ...
    unique(spec$MapCode) # We want "PT" here...
    unique(spec$MapTypeCode) # We take "BZN" here ...

    # Lets narrow down the spec table to get the targetTimeSeriesID's
    targets <- spec %>%
        filter(
            Name == "Load",
            Type %in% c("DayAhead", "Actual"),
            # ProductionType %in% c("Load"),
            MapCode == zn,
            MapTypeCode == "CTY"
        ) %>%
        # Remove empty columns
        select_if(function(x) !(all(is.na(x)) | all(x == "")))


    # Obtain (a connection to) the forecasts table
    values <- tbl(edb, "vals")

    glimpse(values)

    # Get the actual data
    EDAT[[zn]] <- values %>%
        filter(TimeSeriesID %in% !!targets$TimeSeriesID) %>%
        collect() %>%
        left_join(spec, by = "TimeSeriesID") %>%
        filter(
            lubridate::year(DateTime) >= 2015, # here keep only from 2021+
            lubridate::minute(DateTime) == 0
        )

# We may want to select and wrangle even further:
    EDAT[[zn]] <- EDAT[[zn]] %>%
        # Select the cols of interest
        select(DateTime, MapCode, Name, Type, Value) %>%
        arrange(DateTime) %>%
        pivot_wider(names_from = c(MapCode, Name, Type), values_from = Value)
names(EDAT[[zn]]) <- gsub(" ", "", names(EDAT[[zn]]))

}

dbxDisconnect(edb)
EDAT<- list()
zn <- 'NL'
EDAT[[zn]] <- read.csv("data_14feb_2023/EDAT_NL.csv")
EDAT[[zn]]$DateTime
max(as.POSIXct(EDAT[[zn]]$DateTime) )

#write_csv(EDAT[['NL']],'data_14feb_2023/EDAT_NL.csv')
#write_csv(EDAT[['BE']],'data_14feb_2023/EDAT_BE.csv')
#write_csv(EDAT[['LU']],'data_14feb_2023/EDAT_LU.csv')

# write_csv(EDAT[['NL']],'data_22dec_2022/EDAT_NL.csv')
# write_csv(EDAT[['BE']],'data_22dec_2022/EDAT_BE.csv')
# write_csv(EDAT[['LU']],'data_22dec_2022/EDAT_LU.csv')

# write_csv(EDAT[['DE']],'EDAT_DE.csv')
# write_csv(EDAT[['GE']],'EDAT_GE.csv')
# write_csv(EDAT[['LU']],'EDAT_LU.csv')
# write_csv(EDAT[['BE']],'EDAT_BE.csv')
# write_csv(EDAT[['NL']],'EDAT_NL.csv')

lu_met <- read.csv('EDAT_LU.csv')
be_met <- read.csv('EDAT_BE.csv')
nl_met <- read.csv('EDAT_NL.csv')
EDAT <- list()
EDAT[['LU']] <- lu_met
EDAT[['BE']] <- be_met
EDAT[['NL']] <- nl_met

# %%
#endregion

#region holidays
#%% holidays
holidays<-read_csv("holidays_2000_2030.csv")
holidays$Name[] <- gsub(" ", "", holidays$Name)
#shrink to relevant space
# hld <- holidays %>% filter(CountryCode == "DE") 

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

# tmp<-get.HLD(DATA$DateTime, zone="DE")

#View( MET[[zone]][unlist(which(MET[[zone]]$DateTime >= FSTUDYDAYS[124] + 1 * 3600 & MET[[zone]]$DateTime <= FSTUDYDAYS[124] + H * 3600 )),] %>% 
#        arrange(DateTime, horizon))

# & FSTUDYDAYS[i.hm] == DATA$forecast_origin
#%%
#endregion

#region select a zone and create main data frame
#from here we work only with the first country/zone:
# %% combine meteorologic and entsoe data
zone <- ZONE[[1]]

sum(is.na(EDAT[[zone]]$LU_Load_Actual))

names(MET[[zone]])[1] <- "DateTime"
DATA <- dplyr::full_join(MET[[zone]], EDAT[[zone]], by = c("DateTime")) %>% arrange(DateTime, horizon) 
names(DATA) <- gsub(" ", "", names(DATA))
dnames <- names(DATA)
DATA %>% arrange(DateTime, horizon)  ## this will be the input to all algorithms.
## DateTime is in UTC
DATA <- DATA %>% mutate( DateTimeCET = as.POSIXlt(DateTime, tz="CET") )
# duplicate row 
#DATA[c(25,26),]
#cat(length(EDAT[[zone]]$DateTime),'---' , length(unique(EDAT[[zone]]$DateTime)),'\n')
#cat(as.vector(EDAT[[zone]]$DateTime)[1],'---', tail(as.vector(EDAT[[zone]]$DateTime),1),'\n')
#cat(length(MET[[zone]]$DateTime), '--', length(unique(MET[[zone]]$DateTime)),'\n' )
#cat(as.vector(MET[[zone]]$DateTime)[1],'---', tail(as.vector(MET[[zone]]$DateTime),1),'\n')

#tail(sort(as.factor(DATA$DateTime)))
# as.POSIXlt(head(DATA$DateTime), tz="CET")
#DATA$DateTime <- ymd_hms(DATA$DateTime, tz='UTC')
#DATA$DateTimeCET <- as.factor(as.POSIXlt(DATA$DateTime, tz="CET"))
#DATA$DateTime <- as.factor(DATA$DateTime)

# as.factor(ymd_hms(DATA$DateTime, tz='UTC'))
# head(ymd_hms(DATA$DateTime, tz='CET'))
# head(as.factor(as.POSIXlt(DATA$DateTime, tz="CET")) )


SummerTime = lubridate::dst(DATA$DateTimeCET)
HoD = lubridate::hour(DATA$DateTime)
DoW = lubridate::wday(DATA$DateTime, week_start = 1)
DoY = lubridate::yday(DATA$DateTime)
HoDDST = lubridate::hour(DATA$DateTimeCET)
DoWDST = lubridate::wday(DATA$DateTimeCET, week_start = 1)
DoYDST = lubridate::yday(DATA$DateTimeCET)
DET <- cbind(SummerTime, HoD, DoW, DoY, HoDDST, DoWDST, DoYDST)

# DATA <- cbind(DATA[,seq(1,12)], DET)
DATA <- cbind(DATA, DET)
plot(head(DATA$DateTime,500),                              # Draw first time series
     head(DATA$LU_Load_Actual,500),
     type = "l",
     col = 'red',
     xlab = "Year",
     ylab = "Values")

# ts.plot(mm$LU_Load_Actual)
mm <- DATA %>% 
  select(DateTime,LU_Load_Actual) %>% 
  distinct(DateTime, LU_Load_Actual, .keep_all = TRUE) %>% 
  replace_na(list(LU_Load_Actual = 0) ) %>% 
  arrange(DateTime)

plot(ymd_hms(as.vector(mm$DateTime)),                              # Draw first time series
     mm$LU_Load_Actual,
     type = "l",
     col = 'red',
     xlab = "Year",
     ylab = "Values")
# head(as.vector(mm$DateTime),500)
dim(DATA[is.na(mm$LU_Load_Actual),])

# sum(is.na(DATA$LU_Load_Actual))
## illustration that HoDDST is better than HoD 
library(mgcv)
mod <- gam(LU_Load_Actual ~ ti(HoD, k=12), data=DATA)
summary(mod)
mod2<-gam(LU_Load_Actual ~ ti(HoDDST, k=12), data=DATA)
summary(mod2)

#DATA<- cbind(DATA, DateTimeDST)
#save.image("workspace.RData")

#load("workspace.RData")

# %%
#endregion

#region define shiftDST
# %%
# shift function that acts on local time if summer is provided otherwise simple shift
shiftDST<- function(x, clag=1, summer=NULL, hfreq=1){ 
    # clag == vector of lags
    # summer is index set of length/dim[1] of x indicating the summer time, e.g. comp by by "$isdst" argument of POSIXlt
    # hfreq=1 for hourly data, =4 for quarterly hourly etc. - indicated summer time shift, only relevant if summer is provided
    if(is.null(summer)){
        Xlst <- as.matrix(data.table(x)[, shift(.SD, clag, give.names=TRUE)])
    } else {
        SLAGS<- matrix(clag, ncol=length(clag),nrow=3, byrow=TRUE) + -1:1*hfreq
        Sxl <- as.matrix(data.table(x)[, data.table::shift(.SD, SLAGS, give.names=TRUE)])
        xd<- dim(Sxl)[2]/3
        Xlst <- as.matrix(Sxl[, 0:(xd-1)*3+2])
        dsummer<- c(0, diff(summer)) # TODO strictly minor error if first observation is at clock-change.
        marchid <- which(dsummer == 1) # 23 hours
        octid <- which( dsummer == -1)
        II<- diag(length(clag)) == 1
        for(i.l in seq_along(clag))for (i.r in seq_along(marchid)) {
            rid<- unique(pmax(pmin(marchid[i.r] + 0:(clag[i.l] - 1), dim(Xlst)[1]),1))
            Xlst[rid,i.l] <- Sxl[rid, (0:(xd-1)*3+1)[i.l]]
        }
        for(i.l in seq_along(clag))for (i.r in seq_along(octid)) {
            rid<- unique(pmax(pmin(octid[i.r] + 0:(clag[i.l] - 1), dim(Xlst)[1]),1))
            Xlst[rid,i.l] <- Sxl[rid, (0:(xd-1)*3+3)[i.l]]
        }
        }
    Xlst
}

clag<- c(24*1:4)
hfreq<- 1
x<- as.matrix(DATA[,c(11)])
summer = SummerTime

xouts<- shiftDST(x, clag=c(168), SummerTime)
head(xouts)
xout<- shiftDST(x, clag=c(168))

idx<- 1:20000
summary(mods<-lm(unlist(DATA[idx,c(11)]) ~ xouts[idx]))
summary(mod<-lm(unlist(DATA[idx,c(11)]) ~ xout[idx]))

# %%
#endregion

#region forecasting study part
# %%
library(data.table)
H <- 240
horizonfull <- 1:H

last_time <- ymd_hms("2022-07-27 08:00:00") ## last known time
FSTUDYDAYS <- seq(last_time, max(DATA$DateTime) - 3600 * (H), by = 3600 * 24)
N <- length(FSTUDYDAYS)

IDTEST <- list()
for (i.hm in 1:N) {
    IDTEST[[i.hm]] <- which(DATA$DateTime >= FSTUDYDAYS[i.hm] + 1 * 3600 & DATA$DateTime <= FSTUDYDAYS[i.hm] + H * 3600 & FSTUDYDAYS[i.hm] == DATA$forecast_origin) # == FSTUDYDAYS[i.hm] == DATA$forecast_origin restricts to most recent known weather forecasts
}

#for (v in IDTEST){
#  cat(v,length(v),'\n')
  #print(v,length(v))
#}
#length(unlist(IDTEST))
#dim(DATA)
model.names <- c("true","bench",'AR','hw') ## for every model specify horizons and model part (!) both "bench", "GAM", "AR"
M <- length(model.names)
# for (i.m in model.names)
FORECASTS <- array(, dim = c(N, H, M))
dimnames(FORECASTS) <- list(format(FSTUDYDAYS, "%Y-%m-%d"), paste("h_", 1:H, sep = ""), model.names)
dim(FORECASTS)
## reestimation ##


i.m <- 3
library(mgcv)
library(purrr)
library(imputeTS)
S <- 24
for (i.m in seq_along(model.names)) {
## forecasting horizon split [model dependent]
    print('begin model........i.m')
    mname <- model.names[i.m]
    print(mname)
    # "bench", 
    if (mname %in% c("true","GAM")) {
        LAGS <- S * c(1:14, 21, 28)
        horizonc <- unique(c(0, findInterval(LAGS, 1:H)))
    } else { # AR
        horizonc <- c(0, H)
    }
## REESTIMATION [model dependent]
    reest <- 20 # could be chosen depending on model (!), usually fast models can be reestimated regularly.
    FSTUDYSEQid <- unique(c(seq(0, length(FSTUDYDAYS), by = reest), length(FSTUDYDAYS)))
    FSTUDYSEQ <- list()
    for (i.seq in 1:(length(FSTUDYSEQid) - 1)) FSTUDYSEQ[[i.seq]] <- FSTUDYDAYS[c((FSTUDYSEQid[i.seq] + 1), FSTUDYSEQid[i.seq + 1])]
    Nsplitlen <- length(FSTUDYSEQ) ## 
    print(Nsplitlen)
#### model specific data preparation for the forecasting study [model dependent]: 
    if(mname == "GAM"){
        library(data.table)
        vec<- as.integer(DATA$DateTime)
        subs<- match(unique(vec), vec)
        TMPDATA <- bind_cols(DateTime=DATA$DateTime[subs], shiftDST(DATA[subs,ytarget], summer=DATA$SummerTime[subs], clag=LAGS) )#bind_cols(EDAT[[zone]][, "DateTime"], as.data.table(shift(as.data.frame(EDAT[[zone]][, 1:2 + 1]), LAGS, give.names = TRUE)))
        dff<- as.data.frame(base::as.factor(DATA$HoD))
        names(dff)<- "HoD"
        library(Matrix)
        MHoW<-as.matrix(sparse.model.matrix(~.-1,data=dff))

        TMPDATA<- cbind(TMPDATA, MHoW[subs,])
        FDATA <- dplyr::full_join(DATA, TMPDATA, by = c("DateTime")) %>% arrange(DateTime, horizon) 
    } else {
        FDATA<- DATA 
    }
    DATATEST <- FDATA[unlist(IDTEST), ] # maximum test data,

    i.N <- 2
    for (i.N in seq_along(FSTUDYSEQ)) {
        print('FSTUDYSEQ..rows..i.N........')
        print(i.N)
        seqid <- ((FSTUDYSEQid[i.N] + 1):FSTUDYSEQid[i.N + 1])
    ## model
    ### the horizonsplit is part of the model (!), esp. which lags etc. are used
        HORIZON <- list()
        for (i.hl in seq_along(horizonc)[-1]) HORIZON[[i.hl - 1]] <- (horizonc[i.hl - 1] + 1):horizonc[i.hl]

        i.hl <- 1
        for (i.hl in seq_along(HORIZON)) {
            print('HORIZON........24,48....140ahead.')
            print(i.hl)
            
            hmin <- head(HORIZON[[i.hl]], 1)
            hmax <- tail(HORIZON[[i.hl]], 1)
            ## define in-sample/train and out-of-sample/test [caution both depend on FSTUDYSEQ and(!) horizon (as availability of weather data and lags varies)]
            idt <- FDATA$DateTime <= FSTUDYSEQ[[i.N]][1] & FDATA$horizon >= hmin & FDATA$horizon <= hmax & !is.na(FDATA$horizon)
            DATAtrain <- FDATA[idt, ]
            #DATAtrain

            idtestl <- list()
            for (i.hm in seqid) {
                idtestl[[i.hm]] <- which(DATATEST$DateTime >= FSTUDYDAYS[i.hm] + hmin * 3600 & DATATEST$DateTime <= FSTUDYDAYS[i.hm] + hmax * 3600 & DATATEST$horizon >= hmin & DATATEST$horizon <= hmax & FSTUDYDAYS[i.hm] == DATATEST$forecast_origin)
            }
            
            #for (i.hm in seqid) {
              #idtestl[[i.hm]] <- 
            #  zz <- unlist(which(DATATEST$DateTime >= FSTUDYDAYS[i.hm] + hmin * 3600 & DATATEST$DateTime <= FSTUDYDAYS[i.hm] + hmax * 3600 & DATATEST$horizon >= hmin & DATATEST$horizon <= hmax & FSTUDYDAYS[i.hm] == DATATEST$forecast_origin))
            #  cat(i.hm,'---',length(zz),'\n')
            #}
            
            # DATATEST[unlist(which(DATATEST$DateTime >= FSTUDYDAYS[125] + 49 * 3600 & DATATEST$DateTime <= FSTUDYDAYS[125] + 72 * 3600 & DATATEST$horizon >= 49 & DATATEST$horizon <= 72 & FSTUDYDAYS[125] == DATATEST$forecast_origin)),]
            # DATATEST$horizon >= hmin & DATATEST$horizon <= hmax & FSTUDYDAYS[125] == DATATEST$forecast_origin)
            #125-128
            #View(DATATEST[which(DATATEST$DateTime >= FSTUDYDAYS[124] + hmin * 3600 & DATATEST$DateTime <= FSTUDYDAYS[124] + hmax * 3600),])
            #& DATATEST$horizon >= hmin & DATATEST$horizon <= hmax )
            #which(DATATEST$DateTime >= FSTUDYDAYS[125] + hmin * 3600 & DATATEST$DateTime <= FSTUDYDAYS[125] + hmax * 3600 & DATATEST$horizon >= hmin & DATATEST$horizon <= hmax & FSTUDYDAYS[125] == DATATEST$forecast_origin)
            idtest <- unlist(idtestl)
            #length(idtest)
            #24*20
            #View(as.data.frame(c(DATAtest[,ytarget],
            #                     na_ma(DATAtest[,ytarget], k = 12, weighting = "simple"),
            #                     na_interpolation(DATAtest[,ytarget], option='spline')) ) )
            #ts.plot(na_ma(DATAtest[,ytarget], k = 96, weighting = "simple"))
            #ts.plot( na_interpolation(DATAtest[,ytarget], option='spline', method='natural')  )
          
            
            #DATAtest <- DATATEST[idtest, ] %>% arrange(horizon, DateTime)
            # head(DATAtrain[,1:7])
            #tail(DATAtrain[, 1:7])
            #head(DATAtest[, 1:7])
            #tail(DATAtest[, 1:7])
            
            # form dataset
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
            if (mname == "AR") {
                DATAtrainwow <- DATAtrain[DATAtrain$horizon <= S, ] # without weather 'doubling'
                DATAtestwow <- (DATAtest[DATAtest$horizon <= S, ] %>% arrange(DateTime))
                y <- unlist(DATAtrainwow[, ytarget])
                yn <- length(y)
                DATAwow <- c(y, unlist(DATAtestwow[, ytarget]))
                DATAwow <- zoo::na.locf(DATAwow)
                om <- 4*24*7
                mod <- ar(zoo::na.locf(y), order.max = om)
                
                pred <- matrix(, length(seqid), H)
                for (i.NN in 1:length(seqid)) pred[i.NN, ] <- predict(mod, newdata = DATAwow[yn + (-mod$order + 1):0 + (i.NN - 1) * S], n.ahead = H)$pred
            }
            if (mname == "hw") {
              print('estimating model. ........')
              DATAtrainwow <- DATAtrain[DATAtrain$horizon <= S, ] # without weather 'doubling'
              DATAtestwow <- (DATAtest[DATAtest$horizon <= S, ] %>% arrange(DateTime))
              y <- unlist(DATAtrainwow[, ytarget])
              yn <- length(y)
              DATAwow <- c(y, unlist(DATAtestwow[, ytarget]))
              DATAwow <- zoo::na.locf(DATAwow)
              # om <- 4*24*7
              #mod <- ar(zoo::na.locf(y), order.max = om)
              med_wo_outliers <- median(as.vector(y)[as.vector(y)!=0],na.rm = TRUE)
              y[as.vector(y)==0] <- med_wo_outliers
              ym <- ts(y,frequency = 24, start = c(1985,1,1,0,0,0))
              ym <- ym %>% 
                replace_na(med_wo_outliers)
              #ts.plot(ym)
              mod <- hw(zoo::na.locf(ym), h = H, seasonal="multiplicative",
                 lambda=NULL, damped= FALSE, initial = "optimal")
              # mod <- stl(zoo::na.locf(ym), s.window = "periodic", t.window = 24, t.jump = 1)
              pred <- matrix(, length(seqid), H)
              for (i.NN in 1:length(seqid)) pred[i.NN, ] <- predict(mod, n.ahead= H)$mean
              
            }
            if (mname == "bench") {
                ybench <- paste(zone, "_Load_DayAhead", sep = "")
                pred <- t(matrix(unlist(DATAtest[, ybench]), nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
            } # entsoe day-ahead 'benchmark'
            if (mname == "true") {
                pred <- t(matrix(unlist(DATAtest[, ytarget]), nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
            }
            print('dimension of pred..........')
            print(dim(pred))
            FORECASTS[seqid, HORIZON[[i.hl]], mname] <- pred


            cat(zone, "horizon:", hmin, "-", hmax, " done at split ", round(i.N / Nsplitlen * 100, 2), "% progress, mod:", mname, "\n")
        } # i.hl
    } # i.N
} # i.m

dim(FORECASTS[,,"true"])
View(FORECASTS[,,"true"])
tail(head(DATATEST[DATATEST$DateTime >= ymd_hms('2022-07-28- 09:00:00'),] %>% 
       arrange(DateTime,horizon),10),10)

ts.plot(as.vector(FORECASTS[,,"stl"][2,]))
FFT<- FORECASTS
ensemble_pred <- FORECASTS[,,'AR']+FORECASTS[,,'hw']/2
new_forecast <- array( c( FORECASTS , ensemble_pred ) , dim = c( 147 , 240 , 5) )
dimnames(new_forecast) <- list(format(FSTUDYDAYS, "%Y-%m-%d"), paste("h_", 1:H, sep = ""), c(model.names,'ensemble') )
FORECASTS <- new_forecast
FFT<- FORECASTS

for(i.m in c(model.names,'ensemble') )FFT[,,i.m]<- FORECASTS[, , "true"]
RES<- FORECASTS - FFT

RMSE <- sqrt(apply(abs(RES)^2, c(3), mean, na.rm = TRUE))
RMSE

MAEh <- apply(abs(RES), c(2,3), mean, na.rm = TRUE) 
MAE <- apply(abs(RES), c(3), mean, na.rm = TRUE) 
MAE

ts.plot(MAEh, col = 1:8, ylab = "MAE")
legend("topleft", model.names, col = 1:8, lwd = 1)
abline(v = 0:10 * S, col = "orange")
abline(v = 0:10 * S - 8, col = "steelblue")

# %%
#endregion

