
correlationTable = function(graphs) {
  cross = matrix(nrow = length(graphs), ncol = length(graphs))
  for(graph1Id in 1:length(graphs)){
    graph1 = graphs[[graph1Id]]
    print(graph1Id)
    for(graph2Id in 1:length(graphs)) {
      graph2 = graphs[[graph2Id]]
      if(graph1Id == graph2Id){
        break;
      } else {
        correlation = ccf(graph1, graph2, lag.max = 0)
        cross[graph1Id, graph2Id] = correlation$acf[1]
      }
    }
  }
  return(cross)
}



findCorrelated = function(orig, highCorr){
  match = highCorr[highCorr[,1] == orig | highCorr[,2] == orig,]
  match = as.vector(match)
  match[match != orig]
}


bound = function(graphs, orign, match) {
  graphOrign = graphs[[orign]]
  graphMatch = graphs[match]
  allValue = c(graphOrign)
  for(m in graphMatch){
    allValue = c(allValue, m)
  }
  c(min(allValue), max(allValue))
}


# plotSimilar = function(graphs, orign, match){
#   lim = bound(graphs, orign, match)
# 
#   graphOrign = graphs[[orign]]
#   plot(ts(graphOrign), ylim=lim, xlim=c(1,length(graphOrign)+5), lwd=3)
#   title(paste("Similar to", orign, "(black bold)"))
# 
#   cols = c()
#   names = c()
#   for(i in 1:length(match)) {
#     m = match[[i]]
#     matchGraph = graphs[[m]]
#     lines(x = 1:length(matchGraph), y=matchGraph, col=i)
# 
#     cols = c(cols, i)
#     names = c(names, paste0(m))
#   }
#   legend("topright", names, col = cols, lty=c(1,1))
# }


plotSimilar = function(df, orign, match){
  lim = bound(df, orign, match)
  
  graphOrign = df[[orign]]
  plot(ts(graphOrign), ylim=lim, xlim=c(1,length(graphOrign)+2), lwd=3, ylab = "Evolution", xlab = '', xaxt='n')
  title(paste("Similar to", colnames(df[orign]), "(black bold)"))
  axis(side = 1, at = 1:length(graphOrign), 
       labels = format(seq(as.Date("2019-02-01"), length = length(graphOrign), by = "month"), "%b %y"), 
       las = 2) # put dates on plot
  
  cols = c()
  names = c()
  for(i in 1:length(match)) {
    m = match[[i]]
    matchGraph = df[[m]]
    lines(x = 1:length(matchGraph), y=matchGraph, col=i)
    
    cols = c(cols, i)
    names = c(names, colnames(df[m]))
  }
  legend("topright", names, col = cols, lty=c(1,1))
}

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}



plot_alerts = function(dataframe, win, vertical){
  
  # This function plots the alerts by each vertical.
  # dataframe: a dataframe with momentum_tweets_count and thresholds
  # win: window of rollmean
  # vertical: Topics in logs_topic_twitter table in postgreSQL
  
  if (nrow(dataframe) < win){
    cat('Error: Plot cannot compute because dimension of dataframe is less than window moving average\n')
  } else{
      position = data.frame(id= seq(1, nrow(dataframe)), 
                            value = dataframe$counts, 
                            thr = dataframe$threshold)
      anomalyH = position[position$value > position$thr, ]
      anomalyH = anomalyH[!is.na(anomalyH$value), ]
      
      anomaly = data.frame(id=anomalyH$id,
                           value=anomalyH$value)
      anomaly = anomaly[!is.na(anomaly$value), ]
      
      plot(as.ts(position$value), main = paste(win, 'lags Moving Average for:', vertical, sep = ' '), 
           xlab = 'Time',
           ylab = 'Logs Count')
      #lines(as.ts(position$thr), col = 'green')
      real = data.frame(id=seq(1, nrow(dataframe)), value=position$value)
      realAnomaly = real[anomaly$id, ]
      points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
    }
  }

search_na = function(list){
  for (i in 1:length(list)){
    if (list[i] == TRUE){
      return(TRUE)
    } else{
      return(FALSE)
    }
  }
}

get_rates <- function(x){
  growth_rate = round(((x - lag(x)) / lag(x)) * 100 , 2)
  growth_rate = growth_rate[!is.na(growth_rate)]
  return(growth_rate)
}

get_diff <- function(x){
  dif = round(x - lag(x), 6)*100
  dif = dif[!is.na(dif)]
  return(dif)
}

plot_df <- function(variable, start_date, name, ylim=c()) {
  # start_date = "YEAR-MONTH-DAY"
  new_df = data.frame(value = variable, date = seq(as.Date(start_date), length = length(variable), by = "month"))
  plot(xts(x= new_df$value, order.by = new_df$date), type="l", col="#2f002f", 
       ylim = c(1.5*min(variable), 1.5*max(variable)), 
       lwd=2, main = paste('Evolution of', name, sep = ' '), ylab = 'Growth Rate (%)', grid.col = NA)
}

get_anomalies <- function(variable, start_date, name){
  
  # Trend Analysis
  min = mean(variable, na.rm = TRUE) - 1.96*sd(variable, na.rm = TRUE)
  max = mean(variable, na.rm = TRUE) + 1.96*sd(variable, na.rm = TRUE)
  
  # plot circle 
  position = data.frame(id = seq(1, length(variable)), value = variable, 
                        date = seq(as.Date(start_date), length = length(variable), by = "month"))
  anomalyH = position[position$value > max, ]
  anomalyH = anomalyH[!is.na(anomalyH$value), ]
  anomalyL = position[position$value < min, ]
  anomalyL = anomalyL[!is.na(anomalyL$value), ]
  anomaly = data.frame(id = c(anomalyH$id, anomalyL$id), value = c(anomalyH$value, anomalyL$value), date = c(anomalyH$date, anomalyL$date))
  anomaly = anomaly[!is.na(anomaly$value), ]
  
  cat('-----------------------------------------\n')
  cat('Anomalies presented in ', name, ': \n')
  
  for (index_anomaly in 1:nrow(anomaly)){
    cat(paste(anomaly$date[index_anomaly]), '\n')
  }
  cat('-----------------------------------------\n')
  
  # plot metric and limits
  plot_df(variable, start_date, name)
  points(xts(x = anomaly$value, order.by = anomaly$date), col = '#ff3644', pch=19, cex=2)
  
}

corr_matrix_plot <- function(df ,label_size, size_coef, title_graph, colnames_df ) {
  #change colnames
  colnames(df)<- colnames_df
  #correlation matrix
  corr_matrix <-rcorr(as.matrix(df))
  # corr plot
  corrplot(corr_matrix$r , p.mat = corr_matrix$P, type="lower",
           # x,y label size
           tl.cex= label_size, 
           # sig_value
           sig.level = 0.1, insig = "blank",
           # distance between x,y label and x,y axe
           tl.offset = 0.5, tl.col = "black", method = "color",
           # palette color
           col = c('#bf9000ff','#f1c232ff','#ffe599ff','#fff2ccff','#c9daf8ff','#a4c2f4ff','#3c78d8ff','#21437aff'),diag = FALSE, 
           # change slant of the label
           tl.srt = 45, addCoef.col = "black",
           #change coefficeint size
           number.cex= size_coef,
           title = title_graph, outline=FALSE, cl.pos="b", number.digits=2,
           mar=c(0,0,1,0))
}

# simple correlation matrix plot 
simple_corr_matrix_plot <- function(df ,label_size, size_coef, title_graph, colnames_df, method ) {
  df <- df %>% 
    select_if(is.numeric)
  #change colnames
  colnames(df)<- colnames_df
  #correlation matrix
  corr_matrix <- cor(df, method = method)
  corrplot(corr_matrix , type="lower",
           tl.cex= label_size,
           tl.offset = 0.5, tl.col = "black", method = "color",
           number.cex= 0.8, tl.srt = 45,
           addCoef.col = "black",
           diag = FALSE,
           title = title_graph, outline=FALSE, cl.pos="b",
           number.digits=1, mar=c(0,0,0,0))
}

# plot with new colors
plotSimilarModif = function(df, orign, match){
  colors = c("blue","red","orange","darkgray","green", "cyan", "yellow1",
             "darkmagenta","darkgoldenrod4","deepskyblue4","black","violet",'chocolate1','darkred','deepskyblue3')
  lim = bound(df, orign, match)
  graphOrign = df[[orign]]
  plot(ts(graphOrign), ylim=lim, xlim=c(1,length(graphOrign)+2), lwd=4, ylab = "Evolution", xlab = '', xaxt='n')
  title(paste("Similar to\n", colnames(df[orign]), "(black bold)"))
  axis(side = 1, at = 1:length(graphOrign), 
       labels = format(seq(as.Date("2019-02-01"), length = length(graphOrign), by = "month"), "%b %y"), 
       las = 2) # put dates on plot
  cols = c()
  names = c()
  for(i in 1:length(match)) {
    m = match[[i]]
    matchGraph = df[[m]]
    lines(x = 1:length(matchGraph), y=matchGraph, col=colors[i],lwd=1.9)
    cols = c(cols, i)
    names = c(names, colnames(df[m]))
  }
  legend("topright", names, col = colors, lty=c(1,1), cex=.65 , lwd=2.5)
}


get_diff_modif <- function(x){
  dif = round(x - lag(x), 6)*100
  return(dif)
}


get_rates_modif <- function(x){
  growth_rate = round(((x - lag(x)) / lag(x)) * 100 , 2)
  return(growth_rate)}


#region define shiftDST
# %%
# shift function that acts on local time if summer is provided otherwise simple shift
get.HLD <- function(DATA, holidays, zone="DE", S=24, deg=3, bridgep = 0.5, k=0.25){
  xtime <- DATA$DateTime
  # zone only supports full countries at the moment 
  # deg is, cubic spline degree
  # fraction of bridging effects, if 0.5 then half a day before and after the holiday potential effects can occur, values between 0 and 2 are reasonable
  # k determines the number of grid points for the splines, if k=1/S then each data point we have a new basis (if in addition deg=1, these are indicators), the smaller k the more basis functions.
  yrange <- range(lubridate::year(xtime)) + c(-1,1) # safety margins [allows for predictions up to 1 year ahead]
  holidays <- holidays %>% filter(CountryCode == zone) %>%
    filter(lubridate::year(Date)>=yrange[1] & lubridate::year(Date)<=yrange[2] ) 
  # remove holidays which were not lauched yet.
  holidays$LaunchYear[is.na(holidays$LaunchYear)] <- 0
  holidays %>% filter(lubridate::year(Date)>= LaunchYear ) 
  #holidays %>% select(Date, Name) %>% mutate(
  #            DoW = lubridate::wday(Date, week_start = 1)
  #            )## TODO think about more information es. Global and counties
  #mutate_at(vars(x, y), factor)
  holidays$Name <- as.factor(holidays$Name)
  holnames <- levels(holidays$Name)
  #holnames <- gsub(" ", "",holnames)
  hldN <- length(holnames)
  
  #12----(-12)----24-----(+24+12)----36
  #-12 -11 -10  -9  -8  -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
  # 19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
  xbas <- -(S*bridgep):((1+bridgep)*S)
  xbask <- seq(min(xbas), max(xbas), length = k*(length(xbas)-1) +1)
  
  hldbas <- splineDesign(xbask, xbas, outer.ok=TRUE, ord=deg+1)
  hldbasc <- t(apply(hldbas,1,cumsum))
  hldbasx<- hldbas #cbind(hldbas,hldbasc)
  K <- dim(hldbasx)[2]
  
  DHL <- array(0, dim=c(length(xtime),K*hldN) ) ## add cumulative
  i.hld <-1
  for(i.hld in 1:hldN){
    idhld <- which<-which(xtime %in% as.POSIXct( holidays$Date[ holidays$Name == holnames[i.hld] ], tz="UTC") )
    # idhld<- which(as.numeric(format(dtime, "%m")) == DATEhld[[i.hld]][1]	& as.numeric(format(dtime, "%d")) == DATEhld[[i.hld]][2] & as.numeric(format(dtime, "%H")) == 0)
    for(i.b in seq_along(idhld)){ ## add basis segments
      idb <- (idhld[i.b]+min(xbas)):(idhld[i.b]+max(xbas)) ## TODO does not work properly if hld is first or last day...
      idbact <- which(idb > 0 & idb <= dim(DHL)[1])
      DHL[idb[idbact],(1:K)+K*(i.hld-1)] <- hldbasx[idbact,]
    }
  }
  
  ## holidays prepared but not used.
  dimnames(DHL)<- list(NULL,paste0(rep(holnames, rep(K,length(holnames))),"_",1:K))
  DHL 
}