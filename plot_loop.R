repeat {
  print(paste('start OD plot',Sys.time()))
#set working dir to location of dat files
# V. IMPORTANT. CHANGE THIS SO I DON'T OVERWRITE EXISTING FILES.
setwd("/Users/andrewmorgenthaler/Google Drive/MCDB/Copley/AMorgenthaler_lab_notebook/data_files/turbidostat/2017-03-27_Sphingobium_TNseq_1/")

#see what files are in there
list.files(getwd())

# import log.dat file into R and clean up
log <- read.table(file = "log.dat",sep = ",",col.names = c("timestamp","OD.1","OD.2","OD.3","OD.4","OD.5","OD.6","OD.7","OD.8","dil.1","dil.2","dil.3","dil.4","dil.5","dil.6","dil.7","dil.8","avgdil.1","avgdil.2","avgdil.3","avgdil.4","avgdil.5","avgdil.6","avgdil.7","avgdil.8"))
log$timestamp <- as.numeric(gsub(pattern = "{timestamp: ","",x = log$timestamp, fixed = TRUE))
log$OD.1 <- as.numeric(gsub(pattern = "ods: [","",x = log$OD.1, fixed = TRUE))
log$OD.8 <- as.numeric(gsub(pattern = "]","",x = log$OD.8, fixed = TRUE))
log$dil.1 <- as.integer(gsub(pattern = "u: [","",x = log$dil.1, fixed = TRUE))
log$dil.8 <- as.integer(gsub(pattern = "]","",x = log$dil.8, fixed = TRUE))
log$avgdil.1 <- as.numeric(gsub(pattern = "z: [","",x = log$avgdil.1, fixed = TRUE))
log$avgdil.8 <- as.numeric(gsub(pattern = "]}","",x = log$avgdil.8, fixed = TRUE))

#convert timestamp to days
log$day <- (log$timestamp - log[1,1])/86400

#stack columns
log.ODstacked <- data.frame(log[c(1,26)], stack(log[2:9]))
log.dilstacked <- stack(log[10:17])
log.avgdilstacked <- stack(log[18:25])
chamber <- rep(1:8, each=nrow(log))
log.stacked <- cbind(log.ODstacked, log.dilstacked, log.avgdilstacked, chamber)
log.stacked[,c(4,6,8)] <- NULL
colnames(log.stacked) <- c('timestamp','day','OD','dil','avg.dil','chamber')

#export data as a txt file. 
write.table(log,file = "log.wide.txt",sep = "\t",col.names = TRUE,row.names = FALSE, quote = FALSE)
write.table(log.stacked,file = "log.stacked.txt",sep = "\t",col.names = TRUE,row.names = FALSE, quote = FALSE)

#plot OD for each chamber and save as ODplot.png in working directory
png('ODplot.png', width = 1084, height = 805)
op <- par(mar = c(5.1,4.1,4.1,6.1), oma = c(1,1,1,1), xpd = TRUE, las = 1, cex.lab = 1.4, cex.axis = 1.2, cex.main = 4, lwd = 2)
palette(rainbow(8))
plot(x = log.stacked$day,
     y = log.stacked$OD,
     main = 'OD',
     xlab = 'day',
     ylab = 'OD',
     type = 'n',
     bty = 'n',
     axes = FALSE,
     xaxs = 'r',
     yaxs = 'r')
axis(1, pos = -0.03, 
     at = seq(from = 0, to = ceiling(max(log.stacked$day)), by = 0.5), 
     lwd = 2)
axis(2, pos = 0, 
     at = seq(from = round(min(log.stacked$OD), digits = 2), to = max(log.stacked$OD), by = 0.01), 
     lwd = 2)
for (i in 1:max(log.stacked$chamber)){
  points(x = log.stacked$day[log.stacked$chamber==i], 
        y = log.stacked$OD[log.stacked$chamber==i],
        col = i,
        pch = 16)
}
legend('topright',
       inset = c(-0.07,0),
       legend = unique(log.stacked$chamber),
       title = 'chamber',
       cex = 1.5,
       pt.cex = 1.5,
       pch = 19,
       y.intersp = 1,
       col = palette())
par(op)
dev.off()

# Push png files to git
system('git init')
system('git add ODplot.png')
system('git commit -m "OD plot with title!"')
system('git pull')
system('git push -u origin master')

# Wait time before generating new plots
Sys.sleep(time=60)
}
