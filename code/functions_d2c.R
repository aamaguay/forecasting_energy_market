
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
           number.digits=2, mar=c(0,0,1,0))
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
