
DD<-read.csv(file="stats2",head=TRUE)
DD$Time = DD$Time/1000
Data = DD
Data1 = DD[DD$Bits > 33,]
Data2 = DD[DD$Bits > 17 & DD$Bits <= 33, ]
Data3 = DD[DD$Bits <= 17,]

Duq = unique(Data$Bits)

postscript("rplot.eps")

par(mfrow=c(2,2))

RR = rainbow(n=length(unique(Data$Bits)))

Ccons = .03

plot(x=Data$Threads,y=Data$Time,cex=Ccons*Data$Bits,col=RR,xlab="Number of Threads",ylab="Time in Seconds",main="Time versus Number of Threads For All Encryptions")
             
Tmp = length(unique(Data1$Bits))-1

plot(x=Data1$Threads,y=Data1$Time,cex=Ccons*Data1$Bits
     ,col=RR[(length(RR)-Tmp):(length(RR))]
     ,xlab="Number of Threads",ylab="Time in Seconds",main="Time versus Number of Threads for N = [65,129,191]")

Tmp2 = length(unique(Data2$Bits))

plot(x=Data2$Threads,y=Data2$Time,cex=Ccons*Data2$Bits
     ,col=RR[(length(RR)-Tmp-Tmp2):(length(RR)-Tmp-1)]
     ,xlab="Number of Threads",ylab="Time in Seconds",main="Time versus Number of Threads for N = [25,33]")

plot(x=Data3$Threads,y=Data3$Time,cex=Ccons*Data3$Bits,col=RR[1:(length(RR)-Tmp-Tmp2-1)],xlab="Number of Threads",ylab="Time in Seconds",main="Time versus Number of Threads for N = [9,17]")

dev.off()
