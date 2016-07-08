## INTRO
###############################################################################
#setwd("~/Dropbox/XtraWork/Foresight")
setwd("C:/Users/sfos0247/Dropbox/XtraWork/Foresight")
require(dplyr)
.oldpar <- par(no.readonly = TRUE)

## Fig 1.1
###############################################################################
data.1.1 <- read.csv("fig13.csv")
par(mar=c(1.6, 3.1, 0.1,2.1))
# barplot(t(as.matrix(data.1.1[,2:4])),
#         names.arg = c(9,5,19,data.1.1[4:20,1]),
#         las=2,
#         beside=TRUE,
#         horiz=TRUE,
#         axes=FALSE,
#         legend = c("1925", "1950", "2015"), 
#         args.legend = list(x = 10,  cex=1,bty="n"),
#         xlim=c(0,10))
# axis(1)
# lines(c(0,0), c(0,95), lty=2, col="gray30")
# lines(c(2,2), c(0,95), lty=2, col="gray30")
# lines(c(4,4), c(0,95), lty=2, col="gray30")
# lines(c(6,6), c(0,95), lty=2, col="gray30")
# lines(c(8,8), c(0,95), lty=2, col="gray30")
# lines(c(10,10), c(0,95), lty=2, col="gray30")
# 
# 
# dev.copy2eps(file="fig01.eps", width=7, height=3.5)


## version 2
par(mar=c(1.6, 3.1, 0.1,0.1))
plot( c(0,data.1.1[,2],0), c(data.1.1[1:20,1],100, 100), type="s", ylim=c(0, 100), col="gray90",
      xaxt="n", lwd=2,
      axes=FALSE, xlim=c(0,10), xlab="", ylab="")
lines(c(0,0), c(-5,100), lty=2, col="gray80")
lines(c(2,2), c(-5,100), lty=2, col="gray80")
lines(c(4,4), c(-5,100), lty=2, col="gray80")
lines(c(6,6), c(-5,100), lty=2, col="gray80")
lines(c(8,8), c(-5,100), lty=2, col="gray80")
lines(c(10,10), c(-5,100), lty=2, col="gray80")
polygon(c(0,rep(data.1.1[,2], each=2),0), 
        c(rep(data.1.1[1:20,1],each=2), 100,100), col="gray90", border="gray90", lwd = 1)
lines(c(0,data.1.1[,3],0), c(data.1.1[1:20,1], 100, 100), type="s", col="gray50", lwd = 1)
polygon(c(0,rep(data.1.1[,3], each=2),0), 
        c(rep(data.1.1[1:20,1],each=2), 100,100), col="gray50", border="gray50", lwd = 1,angle=45, density=10)
lines(c(0,data.1.1[,4],0), c(data.1.1[1:20,1],100, 100), type="s", col="black", lwd = 1)
polygon(c(0,rep(data.1.1[,4], each=2),0), 
        c(rep(data.1.1[1:20,1],each=2), 100,100), col="black", border="black", angle=-45, density=10,lwd = 1)
#lines(c(0,data.1.1[,5],0), c(data.1.1[1:20,1], 100,100),type="s", col="red", lwd = 2)
axis(1)
axis(2, las=2, at= c(0,5,10,data.1.1[4:20,1])+2.5, labels= c(9,5,19,data.1.1[4:20,1]))
par("usr")
rect(7,70,11,100, col="white", border=NA)
legend(x=7, y=100, rev(c("A", "B", "C")), col=rev(c(NA, NA, NA)), 
       density=rev(c(10,10,NA)), angle=c(0,45,-45), fill=rev(c("black", "gray50", "gray90")),
       border=rev(c("black", "gray50", "gray90")),
       bty="n", lwd=c(1,1,1), cex=1.5, lty = c(NA, NA, NA),
       x.intersp=rep(-1,3))
dev.copy2eps(file="fig01.2.eps", width=7, height=3.5)
par(.oldpar)

## Fig 1.2
###############################################################################
data.1.2 <- read.csv("fig14.csv")
# par(mar=c(1.6, 3.1, 0.1,2.1))
# barplot(t(as.matrix(data.1.2[,2:4])),
#         names.arg = c(9,5,19,data.1.2[4:20,1]),
#         las=2,
#         beside=TRUE,
#         horiz=TRUE,
#         axes=FALSE,
#         legend = c("1925", "1950", "2015"), 
#         args.legend = list(x = 10,  cex=1,bty="n"),
#         xlim=c(0,10))
# axis(1)
# lines(c(0,0), c(0,95), lty=2, col="gray30")
# lines(c(2,2), c(0,95), lty=2, col="gray30")
# lines(c(4,4), c(0,95), lty=2, col="gray30")
# lines(c(6,6), c(0,95), lty=2, col="gray30")
# lines(c(8,8), c(0,95), lty=2, col="gray30")
# lines(c(10,10), c(0,95), lty=2, col="gray30")
# dev.copy2eps(file="fig02.eps", width=7, height=3.5)

## version 2

par(mar=c(1.6, 3.1, 0.1,0.1))
plot( c(0,data.1.2[,2],0), c(data.1.2[1:20,1],100, 100), type="s", ylim=c(0, 100), col="gray90",
      xaxt="n", lwd=2,
      axes=FALSE, xlim=c(0,10), xlab="", ylab="")
lines(c(0,0), c(-5,100), lty=2, col="gray80")
lines(c(2,2), c(-5,100), lty=2, col="gray80")
lines(c(4,4), c(-5,100), lty=2, col="gray80")
lines(c(6,6), c(-5,100), lty=2, col="gray80")
lines(c(8,8), c(-5,100), lty=2, col="gray80")
lines(c(10,10), c(-5,100), lty=2, col="gray80")
polygon(c(0,rep(data.1.2[,2], each=2),0), 
        c(rep(data.1.2[1:20,1],each=2), 100,100), col="gray90", border="gray90", lwd = 1)
lines(c(0,data.1.2[,3],0), c(data.1.2[1:20,1], 100, 100), type="s", col="gray50", lwd = 1)
polygon(c(0,rep(data.1.2[,3], each=2),0), 
        c(rep(data.1.2[1:20,1],each=2), 100,100), col="gray50", border="gray50", lwd = 1,angle=45, density=10)
lines(c(0,data.1.2[,4],0), c(data.1.2[1:20,1],100, 100), type="s", col="black", lwd = 1)
polygon(c(0,rep(data.1.2[,4], each=2),0), 
        c(rep(data.1.2[1:20,1],each=2), 100,100), col="black", border="black", angle=-45, density=10,lwd = 1)
#lines(c(0,data.1.2[,5],0), c(data.1.2[1:20,1], 100,100),type="s", col="red", lwd = 2)
axis(1)
axis(2, las=2, at= c(0,5,10,data.1.2[4:20,1])+2.5, labels= c(9,5,19,data.1.2[4:20,1]))
par("usr")
rect(7,70,11,100, col="white", border=NA)
legend(x=7, y=100, rev(c("A", "B", "C")), col=rev(c(NA, NA, NA)), 
       density=rev(c(10,10,NA)), angle=c(0,45,-45), fill=rev(c("black", "gray50", "gray90")),
       border=rev(c("black", "gray50", "gray90")),
       bty="n", lwd=c(1,1,1), cex=1.5, lty = c(NA, NA, NA),
       x.intersp=rep(-1,3))
dev.copy2eps(file="fig02.2.eps", width=7, height=3.5)
par(.oldpar)


## Fig 1 and 2 together
###############################################################################

par(mfrow=c(1,2))
par(mar=c(2.6, 1.1, 1.1,2.1))
plot( c(0,-data.1.1[,2],0), c(data.1.1[1:20,1],100, 100), type="s", ylim=c(0, 100), col="gray90",
      xaxt="n", lwd=2,
      axes=FALSE, xlim=c(-10,0), xlab="", ylab="")
lines(c(0,0), c(-5,100), lty=2, col="gray80")
lines(c(-2,-2), c(-5,100), lty=2, col="gray80")
lines(c(-4,-4), c(-5,100), lty=2, col="gray80")
lines(c(-6,-6), c(-5,100), lty=2, col="gray80")
lines(c(-8,-8), c(-5,100), lty=2, col="gray80")
lines(c(-10,-10), c(-5,100), lty=2, col="gray80")
polygon(c(0,-rep(data.1.1[,2], each=2),0), 
        c(rep(data.1.1[1:20,1],each=2), 100,100), col="gray90", border="gray90", lwd = 1)
lines(c(0,-data.1.1[,3],0), c(data.1.1[1:20,1], 100, 100), type="s", col="gray50", lwd = 1)
polygon(c(0,-rep(data.1.1[,3], each=2),0), 
        c(rep(data.1.1[1:20,1],each=2), 100,100), col="gray50", border="gray50", lwd = 1,angle=45, density=10)
lines(c(0,-data.1.1[,4],0), c(data.1.1[1:20,1],100, 100), type="s", col="black", lwd = 1)
polygon(c(0,-rep(data.1.1[,4], each=2),0), 
        c(rep(data.1.1[1:20,1],each=2), 100,100), col="black", border="black", angle=-45, density=10,lwd = 1)
#lines(c(0,data.1.1[,5],0), c(data.1.1[1:20,1], 100,100),type="s", col="red", lwd = 2)
axis(1,at=c(0,-2,-4,-6,-8,-10), labels=c(0,2,4,6,8,10))
axis(4, las=2, at= c(0,5,10,data.1.1[4:20,1])+2.5, labels=FALSE)
par("usr")
rect(-7,70,-11,100, col="white", border=NA)
legend(x=-10, y=105, rev(c("A", "B", "C")), col=rev(c(NA, NA, NA)), 
       density=rev(c(10,10,NA)), angle=c(0,45,-45), fill=rev(c("black", "gray50", "gray90")),
       border=rev(c("black", "gray50", "gray90")),
       bty="n", lwd=c(1,1,1), cex=1.5, lty = c(NA, NA, NA),
       x.intersp=rep(-1,3))
mtext("x", 3)
par(mar=c(2.6, 2.1, 1.1,1.1))

plot( c(0,data.1.2[,2],0), c(data.1.2[1:20,1],100, 100), type="s", ylim=c(0, 100), col="gray90",
      xaxt="n", lwd=2,
      axes=FALSE, xlim=c(0,10), xlab="", ylab="")
lines(c(0,0), c(-5,100), lty=2, col="gray80")
lines(c(2,2), c(-5,100), lty=2, col="gray80")
lines(c(4,4), c(-5,100), lty=2, col="gray80")
lines(c(6,6), c(-5,100), lty=2, col="gray80")
lines(c(8,8), c(-5,100), lty=2, col="gray80")
lines(c(10,10), c(-5,100), lty=2, col="gray80")
polygon(c(0,rep(data.1.2[,2], each=2),0), 
        c(rep(data.1.2[1:20,1],each=2), 100,100), col="gray90", border="gray90", lwd = 1)
lines(c(0,data.1.2[,3],0), c(data.1.2[1:20,1], 100, 100), type="s", col="gray50", lwd = 1)
polygon(c(0,rep(data.1.2[,3], each=2),0), 
        c(rep(data.1.2[1:20,1],each=2), 100,100), col="gray50", border="gray50", lwd = 1,angle=45, density=10)
lines(c(0,data.1.2[,4],0), c(data.1.2[1:20,1],100, 100), type="s", col="black", lwd = 1)
polygon(c(0,rep(data.1.2[,4], each=2),0), 
        c(rep(data.1.2[1:20,1],each=2), 100,100), col="black", border="black", angle=-45, density=10,lwd = 1)
#lines(c(0,data.1.2[,5],0), c(data.1.2[1:20,1], 100,100),type="s", col="red", lwd = 2)
axis(1)
axis(2, las=2, at= c(0,5,10,data.1.2[4:20,1])+2.5, labels= c(91,51,19,data.1.2[4:20,1]),hadj=1.5)
par("usr")
rect(7,70,11,100, col="white", border=NA)
legend(x=7, y=105, rev(c("A", "B", "C")), col=rev(c(NA, NA, NA)), 
       density=rev(c(10,10,NA)), angle=c(0,45,-45), fill=rev(c("black", "gray50", "gray90")),
       border=rev(c("black", "gray50", "gray90")),
       bty="n", lwd=c(1,1,1), cex=1.5, lty = c(NA, NA, NA),
       x.intersp=rep(-1,3))
mtext("y", 3)
dev.copy2eps(file="fig01.3.eps", width=7, height=4)
par(mfrow=c(1,1))

## Tab 1.1
###############################################################################

require(xtable)
table.1.1 <- cbind(data.1.1[1:4], data.1.2[2:4])
print(xtable(table.1.1),include.rownames=FALSE )


## Fig 1.3
###############################################################################
data.1.3 <- read.csv("fig15.csv")

par(mar=c(4.1, 4.1, 0.1,0.1))

plot(data.1.3[,1], data.1.3[,2], typ="l",
     ylim=c(0,3.5), axes= FALSE,
     xlab="", ylab="tfr", lwd=2)
axis(1, at=c(seq(1960, 2010, 5), 2012), labels = FALSE)
axis(2, las=2)
axis(1, at=tlab, labels=FALSE)
text(x=c(seq(1960, 2010, 5), 2012), y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=c(seq(1960, 2010, 5), 2012), srt=45, adj=1, xpd=TRUE)

lines(c(1960, 2012), c(0,0), lty=2, col="gray80")
lines(c(1960, 2012), c(1,1), lty=2, col="gray80")
lines(c(1960, 2012), c(2,2), lty=2, col="gray80")
lines(c(1960, 2012), c(3,3), lty=2, col="gray80")
dev.copy2eps(file="fig03.eps", width=7, height=3.5)
par(.oldpar)


require(xtable)
table.1.2 <- t(data.1.3[c(1,6,11,16,21,26,31,36,41,46, 51, 53),])
print(xtable(table.1.2),include.rownames=FALSE )


## Fig 1.4
###############################################################################
data.1.4 <- read.csv("fig16.csv")
par(mar=c(4.1, 3.1, 0.1,0.1))
plot(data.1.4[,1], data.1.4[,2], typ="l",
     axes= FALSE,
     xlab="", ylab="tfr", lwd=2,
     ylim=c(0,30))
lines(data.1.4[,1], data.1.4[,3], lwd=2, col= "red")
axis(1, at=c(1953,seq(1955, 2010, 5), 2012), labels = FALSE)
axis(2, las=2)
axis(1, at=tlab, labels=FALSE)
text(x=c(1953,seq(1955, 2010, 5), 2012), y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=c(1953,seq(1955, 2010, 5), 2012), srt=45, adj=1, xpd=TRUE)
legend(1990, 26, legend=c("CDR", "IMR"), col = c("black", "red"), lwd=2, 
       bty="n", y.intersp = 1.4)
lines(c(1953, 2012), c(5,5), lty=2, col="gray80")
lines(c(1953, 2012), c(10,10), lty=2, col="gray80")
lines(c(1953, 2012), c(20,20), lty=2, col="gray80")
lines(c(1953, 2012), c(30,30), lty=2, col="gray80")
lines(c(1953, 2012), c(15,15), lty=2, col="gray80")
lines(c(1953, 2012), c(25,25), lty=2, col="gray80")
dev.copy2eps(file="fig04.eps", width=7, height=3.5)
par(.oldpar)

## table for 1.4
###############################################################################
table1.4 <- data.1.4[c(1,seq(3,60,5), 60),]
require(xtable)
print(xtable(t(table1.4)), include.rownames = FALSE)


## Fig 1.5
###############################################################################
data.1.1 <- read.csv("fig13.csv")
# par(mar=c(1.6, 3.1, 0.1,2.1))
# barplot(t(as.matrix(data.1.1[,2:5])),
#         names.arg = c(9,5,19,data.1.1[4:20,1]),
#         las=2,
#         beside=TRUE,
#         horiz=TRUE,
#         axes=FALSE,
#         legend = c("1925", "1950", "2015", "2050"), 
#         args.legend = list(x = 10,  cex=1,bty="n"),
#         xlim=c(0,10),
#         col=c("gray20", "gray50", "gray80", "red"))
# axis(1)
# lines(c(0,0), c(0,95), lty=2, col="gray30")
# lines(c(2,2), c(0,95), lty=2, col="gray30")
# lines(c(4,4), c(0,95), lty=2, col="gray30")
# lines(c(6,6), c(0,95), lty=2, col="gray30")
# lines(c(8,8), c(0,95), lty=2, col="gray30")
# lines(c(10,10), c(0,95), lty=2, col="gray30")
# 
# 
# dev.copy2eps(file="fig05.eps", width=7, height=3.5)


## Fig 1.5 version 2
###############################################################################
# par(mar=c(1.6, 3.1, 0.1,0.1))
# plot( c(0,data.1.1[,2],0), c(data.1.1[1:20,1],100, 100), type="s", ylim=c(0, 100), col="black",
#       xaxt="n", lwd=2,
#       axes=FALSE, xlim=c(0,10))
# lines(c(0,data.1.1[,3],0), c(data.1.1[1:20,1], 100, 100), type="s", col="gray70", lwd = 2)
# lines(c(0,data.1.1[,4],0), c(data.1.1[1:20,1],100, 100), type="s", col="gray80", lwd = 2)
# polygon(c(0,rep(data.1.1[,4], each=2),0), 
#         c(rep(data.1.1[1:20,1],each=2), 100,100), col="gray80", border="gray80", angle=45, density=10,lwd = 2)
# lines(c(0,data.1.1[,5],0), c(data.1.1[1:20,1], 100,100),type="s", col="red", lwd = 2)
# axis(1)
# axis(2, las=2, at= c(0,5,10,data.1.1[4:20,1])+2.5, labels= c(9,5,19,data.1.1[4:20,1]))
# par("usr")
# legend(x=7, y=100, rev(c("A", "B", "C", "D")), col=rev(c(NA, NA, NA,NA)), 
#        density=rev(c(0,0,10,0)), fill=rev(c("black", "gray70", "gray80", "red")),
#        border=rev(c("black", "gray70", "gray80", "red")),
#        bty="n", lwd=c(2,2,2,2), cex=1.5, lty = c(NA, NA, NA,NA),
#        x.intersp=rep(-1,4))
# 
# dev.copy2eps(file="fig05.eps", width=7, height=3.5)

# par(mar=c(1.6, 3.1, 0.1,0.1))
# plot( c(0,data.1.1[,2],0), c(data.1.1[1:20,1],100, 100), type="s", ylim=c(0, 100), col="gray90",
#       xaxt="n", lwd=2,
#       axes=FALSE, xlim=c(0,10))
# polygon(c(0,rep(data.1.1[,2], each=2),0), 
#         c(rep(data.1.1[1:20,1],each=2), 100,100), col="gray90", border="gray90", lwd = 1)
# lines(c(0,data.1.1[,3],0), c(data.1.1[1:20,1], 100, 100), type="s", col="gray50", lwd = 1)
# polygon(c(0,rep(data.1.1[,3], each=2),0), 
#         c(rep(data.1.1[1:20,1],each=2), 100,100), col="gray50", border="gray50", lwd = 1,angle=45, density=10)
# lines(c(0,data.1.1[,4],0), c(data.1.1[1:20,1],100, 100), type="s", col="black", lwd = 1)
# polygon(c(0,rep(data.1.1[,4], each=2),0), 
#         c(rep(data.1.1[1:20,1],each=2), 100,100), col="black", border="black", angle=-45, density=10,lwd = 1)
# polygon(c(0,rep(data.1.1[,5], each=2),0), 
#         c(rep(data.1.1[1:20,1],each=2), 100,100),border="red", lwd = 2)
# axis(1)
# axis(2, las=2, at= c(0,5,10,data.1.1[4:20,1])+2.5, labels= c(9,5,19,data.1.1[4:20,1]))
# 
# legend(x=7, y=100, rev(c("D", "A", "B", "C")), col=rev(c(NA, NA, NA,NA)), 
#        density=rev(c(0,10,10,NA)), fill=rev(c( "red","black", "gray70", "gray80")),
#        border=rev(c("red","black", "gray70", "gray80")),angle=c(0,45,-45,0),
#        bty="n", lwd=c(2,2,2,2), cex=1.5, lty = c(NA, NA, NA,NA),
#        x.intersp=rep(-1,4))
# dev.copy2eps(file="fig05.2.eps", width=7, height=3.5)

## Fig 1.5 version 3
# ###############################################################################
# data.1.1 <- read.csv("fig13.csv")
# data.1.1.2 <- read.csv("fig15.2.csv")
# require(dplyr)
# data.1.m <- data.frame(age.group=c(rep(1:6, each=3),6,6),M2015=data.1.1[,4])
# data.1.m <- data.1.m %>%
#   group_by(age.group) %>%
#   summarise(M2015=sum(M2015)) %>%
#   mutate(M2040 = data.1.1.2[,2])
# 

#dev.copy2eps(file="fig05.3.eps", width=7, height=3.5)

## Fig 1.6
###############################################################################

par(mar=c(1.6, 3.1, 0.1,0.1))
plot( c(0,data.1.2[,2],0), c(data.1.2[1:20,1],100, 100), type="s", ylim=c(0, 100), col="black",
      xaxt="n", lwd=2,
      axes=FALSE, xlim=c(0,10))
lines(c(0,data.1.2[,3],0), c(data.1.2[1:20,1], 100, 100), type="s", col="gray70", lwd = 2)
lines(c(0,data.1.2[,4],0), c(data.1.2[1:20,1],100, 100), type="s", col="gray80", lwd = 2)
polygon(c(0,rep(data.1.2[,4], each=2),0), 
        c(rep(data.1.2[1:20,1],each=2), 100,100), col="gray80", border="gray80", angle=45, density=10,lwd = 2)
lines(c(0,data.1.2[,5],0), c(data.1.2[1:20,1], 100,100),type="s", col="red", lwd = 2)
axis(1)
axis(2, las=2, at= c(0,5,10,data.1.2[4:20,1])+2.5, labels= c(9,5,19,data.1.2[4:20,1]))
par("usr")
legend(x=7, y=100, rev(c("A", "B", "C", "D")), col=rev(c(NA, NA, NA,NA)), 
       density=rev(c(0,0,10,0)), fill=rev(c("black", "gray70", "gray80", "red")),
       border=rev(c("black", "gray70", "gray80", "red")),
       bty="n", lwd=c(2,2,2,2), cex=1.5, lty = c(NA, NA, NA,NA),
       x.intersp=rep(-1,4))

dev.copy2eps(file="fig06.eps", width=7, height=3.5)
par(.oldpar)

par(mar=c(1.6, 3.1, 0.1,0.1))
plot( c(0,data.1.2[,2],0), c(data.1.2[1:20,1],100, 100), type="s", ylim=c(0, 100), col="gray90",
      xaxt="n", lwd=2,
      axes=FALSE, xlim=c(0,10))
polygon(c(0,rep(data.1.2[,2], each=2),0), 
        c(rep(data.1.2[1:20,1],each=2), 100,100), col="gray90", border="gray90", lwd = 1)
lines(c(0,data.1.2[,3],0), c(data.1.2[1:20,1], 100, 100), type="s", col="gray50", lwd = 1)
polygon(c(0,rep(data.1.2[,3], each=2),0), 
        c(rep(data.1.2[1:20,1],each=2), 100,100), col="gray50", border="gray50", lwd = 1,angle=45, density=10)
lines(c(0,data.1.2[,4],0), c(data.1.2[1:20,1],100, 100), type="s", col="black", lwd = 1)
polygon(c(0,rep(data.1.2[,4], each=2),0), 
        c(rep(data.1.2[1:20,1],each=2), 100,100), col="black", border="black", angle=-45, density=10,lwd = 1)
lines(c(0,data.1.2[,5],0), c(data.1.2[1:20,1], 100,100),type="s", col="red", lwd = 2)
axis(1)
axis(2, las=2, at= c(0,5,10,data.1.2[4:20,1])+2.5, labels= c(9,5,19,data.1.2[4:20,1]))
legend(x=7, y=100, rev(c("D", "A", "B", "C")), col=rev(c(NA, NA, NA,NA)), 
       density=rev(c(0,10,10,NA)), fill=rev(c( "red","black", "gray70", "gray80")),
       border=rev(c("red","black", "gray70", "gray80")),angle=c(0,45,-45,0),
       bty="n", lwd=c(2,2,2,2), cex=1.5, lty = c(NA, NA, NA,NA),
       x.intersp=rep(-1,4))
dev.copy2eps(file="fig06.2.eps", width=7, height=3.5)
par(.oldpar)


## Fig 1.6 version 3
###############################################################################
###############################################################################
data.1.1 <- read.csv("fig13.csv")
data.1.1.2 <- read.csv("fig15.2.csv")

data.1.2 <- read.csv("fig14.csv")
data.1.1.2 <- read.csv("fig15.2.csv")
require(dplyr)
data.1.f <- data.frame(age.group=c(rep(1:6, each=3),6,6),F2015=data.1.2[,4])
data.1.f <- data.1.f %>%
  group_by(age.group) %>%
  summarise(F2015=sum(F2015)) %>%
  mutate(F2040 = data.1.1.2[,3])

data.1.m <- data.frame(age.group=c(rep(1:6, each=3),6,6),M2015=data.1.1[,4])
data.1.m <- data.1.m %>%
  group_by(age.group) %>%
  summarise(M2015=sum(M2015)) %>%
  mutate(M2040 = data.1.1.2[,2])

data.1.5to6 <- cbind(data.1.f,data.1.m[,2:3])

par(mfrow=c(1,2))
par(mar=c(3.1, 0.6, 1.1,2.6))

plot(c(0,-data.1.5to6[,4],0), c(0,data.1.5to6[1:6,1], 6), type="s", ylim=c(0, 6), col="gray50",
     lwd=2,
     xlim=c(-25,0), axes=FALSE, xlab="", ylab="", main="XX")
axis(1, at=seq(0,-25,-5), labels=seq(0,25,5))
axis(4, las=2, at=seq(0.5, 5.5,1), labels=FALSE)
lines(c(0,0), c(0,6.2), lty=2, col="gray80")
lines(c(-5,-5), c(0,6.2), lty=2, col="gray80")
lines(c(-10,-10), c(0,6.2), lty=2, col="gray80")
lines(c(-15,-15), c(0,6.2), lty=2, col="gray80")
lines(c(-20,-20), c(0,6.2), lty=2, col="gray80")
lines(c(-25,-25), c(0,6.2), lty=2, col="gray80")
polygon(c(0,rep(-data.1.5to6[,4],each=2),0),
        c(0,0,rep(data.1.5to6[1:6,1], each=2)), col="gray50",
        lwd=1, density=10, angle=-45)
polygon(c(0,rep(-data.1.5to6[,5],each=2),0),
        c(0,0,rep(data.1.5to6[1:6,1], each=2)), col="red",
        lwd=1, density=10, angle=45)
par(mar=c(3.1, 2.6, 1.1,0.6))
plot(c(0,data.1.5to6[,2],0), c(0,data.1.5to6[1:6,1], 6), type="s", ylim=c(0, 6), col="gray50",
     lwd=2,
     xlim=c(0,25), axes=FALSE, xlab="", ylab="", main="YY")
lines(c(0,0), c(0,6.2), lty=2, col="gray80")
lines(c(5,5), c(0,6.2), lty=2, col="gray80")
lines(c(10,10), c(0,6.2), lty=2, col="gray80")
lines(c(15,15), c(0,6.2), lty=2, col="gray80")
lines(c(20,20), c(0,6.2), lty=2, col="gray80")
lines(c(25,25), c(0,6.2), lty=2, col="gray80")
polygon(c(0,rep(data.1.5to6[,2],each=2),0),
        c(0,0,rep(data.1.5to6[1:6,1], each=2)), col="gray50",
        lwd=1, density=10, angle=-45)
polygon(c(0,rep(data.1.5to6[,3],each=2),0),
        c(0,0,rep(data.1.5to6[1:6,1], each=2)), col="red",
        lwd=1, density=10, angle=45)

axis(1)
axis(2,at=seq(0.5, 5.5,1), labels=LETTERS[3:8], hadj=3, las=2)
legend(x=17, y=6.4, c("A", "B"), col=c(NA, NA),
       density=c(10,10), fill=c( "red","gray50"),
       border=c( "red","gray50"),angle=c(45,-45),
       bty="n", lwd=c(2,1), cex=1.5, lty = c(NA,NA),
       x.intersp = c(-1,-1),
       y.intersp = 2)

dev.copy2eps(file="fig15to6.eps", width=7, height=3.5)
par(.oldpar)

## Tab 1.3
###############################################################################
require(xtable)
table.1.5to6 <- data.1.5to6[,c(1,4,5,2,3)]
print(xtable(table.1.5to6),include.rownames=FALSE )



## Fig 2.1
###############################################################################
data.2.1 <- read.csv("fig21.csv")

par(mar=c(3.1, 3.1, 1.1,0.1))
plot(data.2.1[,1], data.2.1[,2], typ="l",
     axes= FALSE,
     xlab="", ylab="tfr", lwd=2,
     ylim=c(0,100))
lines(data.2.1[,1], data.2.1[,3], lwd=2, col= "red")
polygon(c(1950,data.2.1[,1], 2010), c(0,data.2.1[,4],0), border="gray", col= "gray")

axis(1, at=c(seq(1950, 2010, 5)), labels = FALSE)
axis(2, at=seq(0,100,10),las=2)
text(x=c(seq(1950, 2010, 5)), y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=c(seq(1950, 2010, 5)), srt=45, adj=1, xpd=TRUE)


lines(c(1950, 2010), c(50,50), lty=2, col="gray80")
lines(c(1950, 2010), c(10,10), lty=2, col="gray80")
lines(c(1950, 2010), c(20,20), lty=2, col="gray80")
lines(c(1950, 2010), c(30,30), lty=2, col="gray80")
lines(c(1950, 2010), c(40,40), lty=2, col="gray80")
lines(c(1950, 2010), c(60,60), lty=2, col="gray80")
lines(c(1950, 2010), c(70,70), lty=2, col="gray80")
lines(c(1950, 2010), c(80,80), lty=2, col="gray80")
lines(c(1950, 2010), c(90,90), lty=2, col="gray80")
lines(c(1950, 2010), c(100,100), lty=2, col="gray80")
lines(c(1950, 2010), c(0,0), lty=2, col="gray80")
rect(1990, 25,2008, 55, col="white", border="white")
legend(1990, 60, legend=rev(c("d", "m", "f")), col = rev(c("gray", "black", "red")), lwd=c( 2,2, NA), 
       box.col="white", lty = c(1,1, NA), fill=c(NA, NA, "gray"), border=c(NA, NA, "gray"),x.intersp=c(1,1,1),
       bg = "white", merge = TRUE, cex=1.5, y.intersp = 1.4)

dev.copy2eps(file="fig07.eps", width=7, height=3.5)
par(.oldpar)

(data.2.1[13,2]-data.2.1[1,2])*12/60
(data.2.1[13,3]-data.2.1[1,3])*12/60

## Fig 2.2
###############################################################################
data.2.2 <- read.csv("fig22.csv")

par(mar=c(3.1, 3.1, 1.1,4.1), xpd=TRUE)
plot(data.2.2[,1], data.2.2[,2], typ="l",
     axes= FALSE,
     xlab="", ylab="tfr", lwd=2,
     ylim=c(0,25))
lines(data.2.2[,1], data.2.2[,3], lwd=2, col= "red")
lines(data.2.2[,1], data.2.2[,4], lwd=2, col= "black", lty=2)
lines(data.2.2[,1], data.2.2[,5], lwd=2, col= "red", lty=2)

axis(1, at=c(seq(1950, 2010, 5)), labels = FALSE)
axis(2, at=seq(0,25,5),las=2)
text(x=c(seq(1950, 2010, 5)), y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=c(seq(1950, 2010, 5)), srt=45, adj=1, xpd=TRUE)


lines(c(1950, 2010), c(10,10), lty=2, col="gray80")
lines(c(1950, 2010), c(20,20), lty=2, col="gray80")
lines(c(1950, 2010), c(0,0), lty=2, col="gray80")
lines(c(1950, 2010), c(5,5), lty=2, col="gray80")
lines(c(1950, 2010), c(15,15), lty=2, col="gray80")
lines(c(1950, 2010), c(25,25), lty=2, col="gray80")

text(2012, data.2.2[13,2], "a")
text(2012, data.2.2[13,3], "b")
text(2012, data.2.2[13,4]-.5, "c")
text(2012, data.2.2[13,5]+.5, "d")

dev.copy2eps(file="fig08.eps", width=7, height=3.5)
par(.oldpar)

data.2.2[13,2]-data.2.2[1,2]
data.2.2[13,3]-data.2.2[1,3]
data.2.2[13,4]-data.2.2[1,4]
data.2.2[13,5]-data.2.2[1,5]

data.2.2[1,3]-data.2.2[1,2]
data.2.2[13,3]-data.2.2[13,2]
data.2.2[1,5]-data.2.2[1,4]
data.2.2[13,5]-data.2.2[13,4]

data.2.2[,3]-data.2.2[,2]
data.2.2[,5]-data.2.2[,4]

## Tab 2.1
###############################################################################
data.2.1 <- read.csv("fig21.csv")

require(xtable)
table.2.1 <- data.2.1
print(xtable(t(table.2.1),include.rownames=FALSE ))

## Tab 2.2
###############################################################################
data.2.2 <- read.csv("fig22.csv")

require(xtable)
table.2.2 <- data.2.2[,c(1,2,4,3,5)]
print(xtable(t(table.2.2)),include.rownames=FALSE )

## Fig 2.3 MISING
###############################################################################
data.2.3 <- read.csv("fig23.csv")



## Fig 2.4 
###############################################################################
data.2.4 <- read.csv("fig24.csv")

par(mar=c(3.1, 3.1, 1.1,0.1))
plot(data.2.4[,1], data.2.4[,2], typ="l",
     axes= FALSE,
     xlab="", ylab="", lwd=2,
     ylim=c(0,100))
lines(data.2.4[,1], data.2.4[,3], lwd=2, col= "red")
polygon(c(2012,data.2.4[,1], 2050), c(0,data.2.4[,4],0), border="gray", col= "gray")

axis(1, at=c(2012,seq(2015, 2050, 5)), labels = FALSE)
axis(2, at=seq(0,100,10),las=2)
text(x=c(2012,seq(2015, 2050, 5)), y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=c(2012,seq(2015, 2050, 5)), srt=45, adj=1, xpd=TRUE)


lines(c(2012, 2050), c(50,50), lty=2, col="gray80")
lines(c(2012, 2050), c(10,10), lty=2, col="gray80")
lines(c(2012, 2050), c(20,20), lty=2, col="gray80")
lines(c(2012, 2050), c(30,30), lty=2, col="gray80")
lines(c(2012, 2050), c(40,40), lty=2, col="gray80")
lines(c(2012, 2050), c(60,60), lty=2, col="gray80")
lines(c(2012, 2050), c(70,70), lty=2, col="gray80")
lines(c(2012, 2050), c(80,80), lty=2, col="gray80")
lines(c(2012, 2050), c(90,90), lty=2, col="gray80")
lines(c(2012, 2050), c(100,100), lty=2, col="gray80")
lines(c(2012, 2050), c(0,0), lty=2, col="gray80")
rect(2040, 20,2050, 65, col="white", border="white")
legend(2040, 60, legend=rev(c("d", "m", "f")), col = rev(c("gray", "black", "red")), lwd=c( 2,2, NA), 
       box.col="white", lty = c(1,1, NA), fill=c(NA, NA, "gray"), border=c(NA, NA, "gray"),x.intersp=c(1,1,1),
       bg = "white", merge = TRUE, cex=1.5, y.intersp = 1.4)

dev.copy2eps(file="fig10.eps", width=7, height=3.5)
par(.oldpar)


## Fig 2.5
###############################################################################
data.2.5 <- read.csv("fig25.csv")

par(mar=c(3.1, 3.1, 1.1,4.1), xpd=TRUE)
plot(data.2.5[,1], data.2.5[,2], typ="l",
     axes= FALSE,
     xlab="", ylab="", lwd=2,
     ylim=c(0,30))
lines(data.2.5[,1], data.2.5[,3], lwd=2, col= "red")
lines(data.2.5[,1], data.2.5[,4], lwd=2, col= "black", lty=2)
lines(data.2.5[,1], data.2.5[,5], lwd=2, col= "red", lty=2)

axis(1, at=c(2012,seq(2015, 2050, 5)), labels = FALSE)
axis(2, at=seq(0,30,5),las=2)
text(x=c(2012,seq(2015, 2050, 5)), y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=c(2012,seq(2015, 2050, 5)), srt=45, adj=1, xpd=TRUE)

lines(c(2012, 2050), c(10,10), lty=2, col="gray80")
lines(c(2012, 2050), c(20,20), lty=2, col="gray80")
lines(c(2012, 2050), c(30,30), lty=2, col="gray80")
lines(c(2012, 2050), c(15,15), lty=2, col="gray80")
lines(c(2012, 2050), c(25,25), lty=2, col="gray80")
lines(c(2012, 2050), c(5,5), lty=2, col="gray80")

text(2051, data.2.5[39,2], "a")
text(2051, data.2.5[39,3], "b")
text(2051, data.2.5[39,4]-.5, "c")
text(2051, data.2.5[39,5]+.5, "d")

dev.copy2eps(file="fig11.eps", width=7, height=3.5)
par(.oldpar)


## Fig 2.6
###############################################################################
data.2.6 <- read.csv("fig26.csv", colClasses = c("factor", rep("numeric", 9)) )
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

data.2.6[,11] <- data.2.6[,4]+20
data.2.6[,12] <- data.2.6[,5]+40
data.2.6[,13] <- data.2.6[,6]+60
data.2.6[,14] <- data.2.6[,7]+65
data.2.6[,15] <- data.2.6[,8]+75
data.2.6[,16] <- data.2.6[,9]+80
data.2.6[,17] <- data.2.6[,10]+85

par(mar=c(3.6, 4.1, 2.1,1.1), xpd=TRUE)

barplot(as.matrix(data.2.6[c(1,8),c(3,11:17)]), beside = TRUE, 
        angle=45, density=15, 
        col = c("black", redgray[1]), horiz=TRUE,
        axes=FALSE, las=2, ylab="aa", xlim=c(0,100),names.arg = LETTERS[1:8])
mtext("LE", side=1, line=2.5)

lines(c(0, 0), c(0,25), lty=2, col="gray80")
lines(c(20, 20), c(0,25), lty=2, col="gray80")
lines(c(40, 40), c(0,25), lty=2, col="gray80")
lines(c(60, 60), c(0,25), lty=2, col="gray80")
lines(c(80, 80), c(0,25), lty=2, col="gray80")
lines(c(85, 85), c(0,25), lty=2, col="gray80")
lines(c(90, 90), c(0,25), lty=2, col="gray80")
lines(c(95, 95), c(0,25), lty=2, col="gray80")
lines(c(100, 100), c(0,25), lty=2, col="gray80")
barplot(as.matrix(data.2.6[c(1,8),c(3,11:17)]), beside = TRUE, 
        angle=45, density=15, 
        col = c("black", redgray[1]), horiz=TRUE,
        axes=FALSE, las=2, ylab="aa", xlab="", add=TRUE, names.arg = NA)
barplot(matrix(rep(c(0,20,40,60,65,75,80,85), each=2), nrow=2), 
        beside = TRUE, add=TRUE,angle=-45, density=15,
        col=c( "black", redgray[1]), horiz=TRUE,names.arg = NA)
axis(1, at=seq(0,100,20))
legend(x=20,y=27.5, letters[2],col =c( "black"), fill =  c("black"),
       density=15, bty="n", cex=1.3, x.intersp = 1.2, horiz = TRUE)

legend(x=40,y=27.5, letters[4],col =c(  redgray[1]), fill =  c( redgray[1]),
       border= redgray[1],
       density=15, bty="n", cex=1.3, x.intersp = 1.2, horiz = TRUE)

dev.copy2eps(file="fig26.eps", width=7, height=3.5)
par(.oldpar)

## Fig 2.7
###############################################################################
data.2.6 <- read.csv("fig26.csv", colClasses = c("factor", rep("numeric", 9)) )
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

data.2.6[,11] <- data.2.6[,4]+20
data.2.6[,12] <- data.2.6[,5]+40
data.2.6[,13] <- data.2.6[,6]+60
data.2.6[,14] <- data.2.6[,7]+65
data.2.6[,15] <- data.2.6[,8]+75
data.2.6[,16] <- data.2.6[,9]+80
data.2.6[,17] <- data.2.6[,10]+85

par(mar=c(3.6, 4.1, 2.1,1.1), xpd=TRUE)

barplot(as.matrix(data.2.6[c(9,16),c(3,11:17)]), beside = TRUE, 
        angle=45, density=15, 
        col = c("black", redgray[1]), horiz=TRUE,
        axes=FALSE, las=2, ylab="aa",  xlim=c(0,100),names.arg = LETTERS[1:8])
mtext("LE", side=1, line=2.5)
lines(c(0, 0), c(0,25), lty=2, col="gray80")
lines(c(20, 20), c(0,25), lty=2, col="gray80")
lines(c(40, 40), c(0,25), lty=2, col="gray80")
lines(c(60, 60), c(0,25), lty=2, col="gray80")
lines(c(80, 80), c(0,25), lty=2, col="gray80")
lines(c(85, 85), c(0,25), lty=2, col="gray80")
lines(c(90, 90), c(0,25), lty=2, col="gray80")
lines(c(95, 95), c(0,25), lty=2, col="gray80")
lines(c(100, 100), c(0,25), lty=2, col="gray80")
barplot(as.matrix(data.2.6[c(9,16),c(3,11:17)]), beside = TRUE, 
        angle=45, density=15, 
        col = c("black", redgray[1]), horiz=TRUE,
        axes=FALSE, las=2, ylab="aa", xlab="", add=TRUE, names.arg = NA)
barplot(matrix(rep(c(0,20,40,60,65,75,80,85), each=2), nrow=2), 
        beside = TRUE, add=TRUE,angle=-45, density=15,
        col=c( "black", redgray[1]), horiz=TRUE,names.arg = NA)
axis(1, at=seq(0,100,20))
legend(x=20,y=27.5, letters[2],col =c( "black"), fill =  c("black"),
       density=15, bty="n", cex=1.3, x.intersp = 1.2, horiz = TRUE)

legend(x=40,y=27.5, letters[4],col =c(  redgray[1]), fill =  c( redgray[1]),
       border= redgray[1],
       density=15, bty="n", cex=1.3, x.intersp = 1.2, horiz = TRUE)

dev.copy2eps(file="fig27.eps", width=7, height=3.5)
par(.oldpar)



## Joined 2.6 and 2.7
###############################################################################

data.2.6 <- read.csv("fig26.csv", colClasses = c("factor", rep("numeric", 9)) )
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

data.2.6[,11] <- data.2.6[,4]+20
data.2.6[,12] <- data.2.6[,5]+40
data.2.6[,13] <- data.2.6[,6]+60
data.2.6[,14] <- data.2.6[,7]+65
data.2.6[,15] <- data.2.6[,8]+75
data.2.6[,16] <- data.2.6[,9]+80
data.2.6[,17] <- data.2.6[,10]+85

par(mfrow=c(1,2))
par(mar=c(3.6, 1.1, 2.1,2.1), xpd=TRUE)

barplot(-as.matrix(data.2.6[c(1,8),c(3,11:17)]), beside = TRUE, 
        angle=45, density=15, 
        col = c("black", redgray[1]), horiz=TRUE,
        axes=FALSE, las=2, ylab=NA, xlim=c(-100,0), names.arg = NA)
mtext("LE", side=1, line=2.5)
mtext("xx", side=3, line=0.5)

lines(c(0, 0), c(0,25), lty=2, col="gray80")
lines(c(-20, -20), c(0,25), lty=2, col="gray80")
lines(c(-40, -40), c(0,25), lty=2, col="gray80")
lines(c(-60, -60), c(0,25), lty=2, col="gray80")
lines(c(-80, -80), c(0,25), lty=2, col="gray80")
lines(c(-85, -85), c(0,25), lty=2, col="gray80")
lines(c(-90, -90), c(0,25), lty=2, col="gray80")
lines(c(-95, -95), c(0,25), lty=2, col="gray80")
lines(c(-100, -100), c(0,25), lty=2, col="gray80")
barplot(-as.matrix(data.2.6[c(1,8),c(3,11:17)]), beside = TRUE, 
        angle=45, density=15, 
        col = c("black", redgray[1]), horiz=TRUE,
        axes=FALSE, las=2, ylab="", xlab="", add=TRUE, names.arg = NA)
barplot(-matrix(rep(c(0,20,40,60,65,75,80,85), each=2), nrow=2), 
        beside = TRUE, add=TRUE,angle=-45, density=15,
        col=c( "black", redgray[1]), horiz=TRUE,names.arg = NA)
axis(1, at=seq(0,-100,-20), labels=seq(0,100,20) )
legend(x=-10,y=29.5, letters[2],col =c( "black"), fill =  c("black"),
       density=15, bty="n", cex=1.3, x.intersp = 1.2, horiz = TRUE)

par(mar=c(3.6, 2.1, 2.1,1.1), xpd=TRUE)
barplot(as.matrix(data.2.6[c(9,16),c(3,11:17)]), beside = TRUE, 
        angle=45, density=15, 
        col = c("black", redgray[1]), horiz=TRUE,
        axes=FALSE, las=2, ylab="aa",  hadj=2,xlim=c(0,100),names.arg = LETTERS[1:8])
mtext("LE", side=1, line=2.5)
mtext("yy", side=3, line=0.5)

lines(c(0, 0), c(0,25), lty=2, col="gray80")
lines(c(20, 20), c(0,25), lty=2, col="gray80")
lines(c(40, 40), c(0,25), lty=2, col="gray80")
lines(c(60, 60), c(0,25), lty=2, col="gray80")
lines(c(80, 80), c(0,25), lty=2, col="gray80")
lines(c(85, 85), c(0,25), lty=2, col="gray80")
lines(c(90, 90), c(0,25), lty=2, col="gray80")
lines(c(95, 95), c(0,25), lty=2, col="gray80")
lines(c(100, 100), c(0,25), lty=2, col="gray80")
barplot(as.matrix(data.2.6[c(9,16),c(3,11:17)]), beside = TRUE, 
        angle=45, density=15, 
        col = c("black", redgray[1]), horiz=TRUE,
        axes=FALSE, las=2, ylab="", xlab="", add=TRUE, names.arg = NA)
barplot(matrix(rep(c(0,20,40,60,65,75,80,85), each=2), nrow=2), 
        beside = TRUE, add=TRUE,angle=-45, density=15,
        col=c( "black", redgray[1]), horiz=TRUE,names.arg = NA)
axis(1, at=seq(0,100,20))
legend(x=-10,y=29.5, letters[4],col =c(  redgray[1]), fill =  c( redgray[1]),
       border= redgray[1],
       density=15, bty="n", cex=1.3, x.intersp = 1.2, horiz = TRUE)

dev.copy2eps(file="fig267.eps", width=7, height=3.5)
par(.oldpar)



## Tab 2.4
###############################################################################
data.2.4 <- read.csv("fig24.csv")
table.2.4 <- data.2.4[c(1,seq(4,39,5 )),]
require(xtable)

print(xtable(t(table.2.4)),include.rownames=FALSE )

## Tab 2.5
###############################################################################
data.2.5 <- read.csv("fig25.csv")
table.2.5 <- data.2.5[c(1,seq(4,39,5 )),]
require(xtable)
print(xtable(t(table.2.5)),include.rownames=FALSE )


## Tab 2.6
###############################################################################

require(xtable)
data.2.6 <- read.csv("fig24.csv")
table.2.6 <- cbind(data.2.6[c(1,8,9,16), 1:10])

print(xtable(table.2.6),include.rownames=FALSE )


## Fig 3.1
###############################################################################
data.3.1 <- read.csv("fig31.csv")
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)
require(plotrix)

par(mar=c(3.1, 3.1, 1.1,4.1), xpd=TRUE)
plot(2000:2009, data.3.1[,2], typ="l",
     axes= FALSE,
     xlab="", ylab="", lwd=2,    ylim=c(0,85))
lines(2000:2009, data.3.1[,4], lwd=2, col= "gray50")
lines(2000:2009, data.3.1[,3], lwd=2, col= "black", lty=2)
lines(2000:2009, data.3.1[,5], lwd=2, col= "red", lty=1)
lines(2000:2009, data.3.1[,7], lwd=2, col= redgray[20])
lines(2000:2009, data.3.1[,6], lwd=2, col= "red", lty=2)

axis(1, at=2000:2009, labels = FALSE)
axis(2, las=2)
#axis(2, las=2, at=c(45, 50, 60, 70, 80), labels=c(0, 50, 60, 70, 80) )

#axis.break(2,47,style="slash") 

text(x=2000:2009, y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=2000:2009, srt=45, adj=1, xpd=TRUE)

lines(c(2000, 2009), c(0,0), lty=2, col="gray80")
lines(c(2000, 2009), c(20,20), lty=2, col="gray80")
lines(c(2000, 2009), c(40,40), lty=2, col="gray80")
lines(c(2000, 2009), c(60,60), lty=2, col="gray80")
lines(c(2000, 2009), c(80,80), lty=2, col="gray80")

rect(2003,15,2008.5, 55, col="white", border="white")

legend(2003, 30, legend=letters[1:3], col =c("black", "gray50", "black"), lwd=c( 2,2, 2), 
       box.col="white", lty = c(1,1, 2), fill=c(NA, NA, NA), border=c(NA, NA, NA),x.intersp=c(1,1,1),
       bg = "white", merge = TRUE, cex=1, y.intersp = 1.4)

legend(2003, 55, legend=letters[4:6], col =c("red", redgray[20], "red"), lwd=c( 2,2, 2), 
       box.col="white", lty = c(1,1, 2), fill=c(NA, NA, NA), border=c(NA, NA, NA),x.intersp=c(1,1,1),
       bg = "white", merge = TRUE, cex=1, y.intersp = 1.4)

dev.copy2eps(file="fig31.eps", width=7, height=3.5)

par(.oldpar)


## Fig 4.1
###############################################################################
data.4.1 <- read.csv("fig41.csv")

par(mar=c(4.1, 4.1, 0.1,0.1))

plot(data.4.1[,1], data.4.1[,2], typ="l",
     ylim=c(30,42), axes= FALSE,
     xlab="", ylab="ma", lwd=2)
axis(1, at=c(seq(1974, 2014, 4)), labels = FALSE)
axis(2, las=2)
text(x=c(seq(1974, 2014, 4)), y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=c(seq(1974, 2014, 4)), srt=45, adj=1, xpd=TRUE)

lines(c(1974, 2014), c(32,32), lty=2, col="gray80")
lines(c(1974, 2014), c(34,34), lty=2, col="gray80")
lines(c(1974, 2014), c(36,36), lty=2, col="gray80")
lines(c(1974, 2014), c(38,38), lty=2, col="gray80")
lines(c(1974, 2014), c(40,40), lty=2, col="gray80")
lines(c(1974, 2014), c(42,42), lty=2, col="gray80")

dev.copy2eps(file="fig12.eps", width=7, height=3.5)
par(.oldpar)
    
require(xtable)
table.4.1 <-  t(data.4.1[seq(1, 41, 4),])
print(xtable(table.4.1), include.rownames=FALSE)

## Fig 4.2
###############################################################################
data.4.2 <- read.csv("fig42.csv")

par(mar=c(3.1, 3.1, 1.1,5.1))
plot(data.4.2[,1], data.4.2[,2], typ="n",
     axes= FALSE,
     xlab="", ylab="", lwd=2,
     ylim=c(0,12))
polygon(c(1974,data.4.2[,1], 2014), c(0,data.4.2[,4],0), border="black", col= "black", density=20)
polygon(c(data.4.2[,1], rev(data.4.2[,1])), c(data.4.2[,4],rev(data.4.2[,3])), border="black", col= "black", density=10)
polygon(c(data.4.2[,1], rev(data.4.2[,1])), c(data.4.2[,3],rev(data.4.2[,2])), border="black", col= "black", density=5)

axis(1, at=c(seq(1974, 2014, 4)), labels = FALSE)
axis(2, las=2)
text(x=c(seq(1974, 2014, 4)), y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=c(seq(1974, 2014, 4)), srt=45, adj=1, xpd=TRUE)

lines(c(1974, 2014), c(0,0), lty=2, col="gray80")
lines(c(1974, 2014), c(2,2), lty=2, col="gray80")
lines(c(1974, 2014), c(4,4), lty=2, col="gray80")
lines(c(1974, 2014), c(6,6), lty=2, col="gray80")
lines(c(1974, 2014), c(8,8), lty=2, col="gray80")
lines(c(1974, 2014), c(10,10), lty=2, col="gray80")
lines(c(1974, 2014), c(12,12), lty=2, col="gray80")

text(2015, mean(c(data.4.2[41,2], data.4.2[41,3])), "a")
text(2015, mean(c(data.4.2[41,3], data.4.2[41,4])), "b")
text(2015, mean(c(data.4.2[41,4], 0)), "c")

dev.copy2eps(file="fig13.eps", width=7, height=3.5)
par(.oldpar)


## Tab 4.1
###############################################################################
require(xtable)
table.4.1 <- cbind(data.4.2, data.4.1[,2])
print(xtable(table.4.1), include.rownames=FALSE)

## Fig 4.3
###############################################################################
data.4.3 <- read.csv("fig43.csv")

par(mar=c(3.1, 4.1, 1.1,0.1))
plot(data.4.3[,1], data.4.3[,2], typ="l",
     axes= FALSE,
     xlab="", ylab="xx", lwd=2,
     ylim=c(0,50000))
lines(data.4.3[,1], data.4.3[,3], lwd=2, col= "red")

axis(1, at=c(2012,seq(2015, 2040, 5), 2041), labels = FALSE)
axis(2, at=seq(0,50000,10000),labels=seq(0,50,10),las=2)
text(x=c(2012,seq(2015, 2040, 5), 2041), y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=c(2012,seq(2015, 2040, 5), 2041), srt=45, adj=1, xpd=TRUE)


lines(c(2012, 2041), c(0,0), lty=2, col="gray80")
lines(c(2012, 2041), c(10000,10000), lty=2, col="gray80")
lines(c(2012, 2041), c(20000,20000), lty=2, col="gray80")
lines(c(2012, 2041), c(30000,30000), lty=2, col="gray80")
lines(c(2012, 2041), c(40000,40000), lty=2, col="gray80")
lines(c(2012, 2041), c(50000,50000), lty=2, col="gray80")

rect(2030, 25000,2050, 35000, col="white", border="white")
legend(2030, 38000, legend=c( "m", "f"), col = c("black", "red"), lwd=c( 2,2), 
       box.col="white", lty = c(1,1), fill=c(NA, NA), border=c(NA, NA),
       bg = "white", merge = TRUE, cex=1.5, y.intersp = 1.4)

dev.copy2eps(file="fig14.eps", width=7, height=3.5)

par(.oldpar)

## Fig 4.4
###############################################################################
data.4.4 <- read.csv("fig44.csv")

par(mar=c(3.1, 5.6, 1.1,3.1), xpd=TRUE)
plot(data.4.4[,1], data.4.4[,2], typ="l",
     axes= FALSE,
     xlab="", ylab="", lwd=2,
     ylim=c(2.2,4), col="red")
lines(data.4.4[,1], data.4.4[,3], lwd=2, col= "black", lty=2)
lines(data.4.4[,1], data.4.4[,4], lwd=2, col= "gray60", lty=2)
lines(data.4.4[,1], data.4.4[,5], lwd=2, col= "gray40", lty=2)
lines(data.4.4[,1], data.4.4[,6], lwd=2, col= "gray70", lty=2)

axis(1, at=c(2012,seq(2015, 2035, 5), 2037), labels = FALSE)
axis(4, at=seq(2.2,4,0.2),las=2)
text(x=c(2012,seq(2015, 2035, 5), 2037), y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=c(2012,seq(2015, 2035, 5), 2037), srt=45, adj=1, xpd=TRUE)

lines(c(2012, 2037), c(2.2,2.2), lty=2, col="gray80")
lines(c(2012, 2037), c(2.4,2.4), lty=2, col="gray80")
lines(c(2012, 2037), c(2.6,2.6), lty=2, col="gray80")
lines(c(2012, 2037), c(2.8,2.8), lty=2, col="gray80")
lines(c(2012, 2037), c(3.2,3.2), lty=2, col="gray80")
lines(c(2012, 2037), c(3.4,3.4), lty=2, col="gray80")
lines(c(2012, 2037), c(3.6,3.6), lty=2, col="gray80")
lines(c(2012, 2037), c(3.8,3.8), lty=2, col="gray80")
lines(c(2012, 2037), c(3.0,3.0), lty=2, col="gray80")
lines(c(2012, 2037), c(4.0,4.0), lty=2, col="gray80")

text(2011, data.4.4[1,2], "a")
text(2011, data.4.4[1,3]+0.1, "b")
text(2011, data.4.4[1,4], "c")
text(2011, data.4.4[1,5]-0.1, "d")
text(2011, data.4.4[1,6], "e")

dev.copy2eps(file="fig15.eps", width=7, height=3.5)

par(.oldpar)


## Tab 4.2
###############################################################################
require(xtable)
table.4.2 <- cbind(round(data.4.3,2), round(data.4.3[,2]/data.4.3[,3],2))
print(xtable(table.4.2,digits=c(0,0,0,0,2)), include.rownames=FALSE )



## Tab 4.3
###############################################################################
require(xtable)
table.4.4 <-data.4.4[c(1,seq(4,26,5), 26),]
print(xtable(table.4.4,digits=c(0,0,2,2,2,2,2)), include.rownames=FALSE )


## Fig 5.1
###############################################################################

data.5.1.1 <- read.csv("fig51.1.csv")
data.5.1.1p <- prop.table(as.matrix(data.5.1.1[10:1,2:5]), 1)


par(mar=c(3.1, 8.5, 1.6,0.1), xpd=TRUE)

barplot(t(as.matrix(data.5.1.1p)), width=data.5.1.1[10:1,6], horiz = TRUE,
        axes=FALSE, las=2, angle=45, density = c(20,15,10,5),
        col=c("black", "gray30", "gray50", "gray80"))
axis(1)
legend(x=0, y=39000, letters[1:4], horiz = TRUE, bty="n", x.intersp = 4,
       col=c("black", "gray30", "gray50", "gray80"), angle=45, density = c(20,15,10,5))
lines(c(0.0,0.0), c(0, 35500), lty=2, col="gray80")
lines(c(0.2,0.2), c(0, 35500), lty=2, col="gray80")
lines(c(0.4,0.4), c(0, 35500), lty=2, col="gray80")
lines(c(0.6,0.6), c(0, 35500), lty=2, col="gray80")
lines(c(0.8,0.8), c(0, 35500), lty=2, col="gray80")
lines(c(1.0,1.0), c(0, 35500), lty=2, col="gray80")


barplot(t(as.matrix(data.5.1.1p)), width=data.5.1.1[10:1,6], horiz = TRUE,
        axes=FALSE, las=2, angle=45, density = c(20,15,10,5),
        col=c("black", "gray30", "gray50", "gray80"), add=TRUE)
dev.copy2eps(file="fig51.eps", width=7, height=5.5)
par(.oldpar)


## Fig 5.2
###############################################################################

data.5.1.1 <- read.csv("fig51.1.csv")
data.5.1.2p <- prop.table(as.matrix(data.5.1.1[,2:5]), 2)
# 
# barplot(t(as.matrix(data.5.1.2p)), beside=TRUE)
#         col=c("black", "gray30", "gray50", "gray80"))
# axis(1)
# legend(x=0.1, y=38000, letters[1:4], horiz = TRUE, bty="n", x.intersp = 5,
#        col=c("black", "gray30", "gray50", "gray80"), angle=45, density = c(30,20,10,5))
# 

## Tab 5.1
###############################################################################
require(xtable)

data.5.1.1 <- read.csv("fig51.1.csv")
table.5.1.1 <- cbind(data.5.1.1[,1], as.data.frame(round(100*prop.table(as.matrix(data.5.1.1[,2:5]), 1), 2)), rep(100,10))
print(xtable(table.5.1.1), include.rownames=FALSE )


## Tab 5.2
###############################################################################
require(xtable)

data.5.1.1 <- read.csv("fig51.1.csv")
table.5.1.2 <- cbind(data.5.1.1[,1], as.data.frame(round(100*prop.table(as.matrix(data.5.1.1[,2:6]), 2), 2)))
table.5.1.2 <- rbind(table.5.1.2, rep(100,5))
print(xtable(table.5.1.2), include.rownames=FALSE )



## Fig 5.3
###############################################################################

data.5.3 <- read.csv("fig53.csv")
data.5.3$m1 <- data.5.3[,2]/(data.5.3[,2]+data.5.3[,3])
data.5.3$m2 <- data.5.3[,3]/(data.5.3[,2]+data.5.3[,3])

# par(mfrow=c(1,2))
# par(mar=c(3.1, 1.6, 1.1,1.6))
# mp <- barplot(-data.5.3$m1, horiz = TRUE, xlim=c(-1,0), width=(data.5.3[,2]+data.5.3[,3]),
#               axes=FALSE, angle=45, density=20, col = "black")
# axis(4, at=mp[seq(1,31,5)], labels=seq(40,70,5), las=2)
# axis(1, at=seq(0,-1,-0.2))
# barplot(data.5.3$m2, horiz = TRUE, xlim=c(0,1), width=(data.5.3[,2]+data.5.3[,3]),
#         axes=FALSE, angle=45, density=20, col = "black")
# axis(2, at=mp[seq(1,31,5)], labels=FALSE)
# axis(1, at=seq(0,1,0.2))

par(mfrow=c(2,1))
par(mar=c(1.3, .6, 1.3,2.6))
mp <- barplot(data.5.3$m1,  ylim=c(0,1), width=(data.5.3[,2]+data.5.3[,3]),
              axes=FALSE, angle=45, density=20, col = "black")
axis(1, at=mp[seq(1,31,5)], labels=seq(40,70,5))
axis(4, at=seq(0,1,0.2), las=2)
mtext("xx", side=2)
lines(c(0, 7750), c(0,0), lty=2, col="gray80")
lines(c(0, 7750), c(0.2,0.2), lty=2, col="gray80")
lines(c(0, 7750), c(0.4,0.4), lty=2, col="gray80")
lines(c(0, 7750), c(0.6,0.6), lty=2, col="gray80")
lines(c(0, 7750), c(0.8,0.8), lty=2, col="gray80")
lines(c(0, 7750), c(1,1), lty=2, col="gray80")
barplot(data.5.3$m1,  ylim=c(0,1), width=(data.5.3[,2]+data.5.3[,3]),
        axes=FALSE, angle=45, density=20, col = "black", add=TRUE)
barplot(-data.5.3$m2, ylim=c(-1,0), width=(data.5.3[,2]+data.5.3[,3]),
        axes=FALSE, angle=45, density=20, col = "black")
axis(3, at=mp[seq(1,31,5)], labels=FALSE)
axis(4, at=seq(-1,0,0.2), las=2)
mtext("yy", side=2)
lines(c(0, 7750), c(-0,-0), lty=2, col="gray80")
lines(c(0, 7750), c(-0.2,-0.2), lty=2, col="gray80")
lines(c(0, 7750), c(-0.4,-0.4), lty=2, col="gray80")
lines(c(0, 7750), c(-0.6,-0.6), lty=2, col="gray80")
lines(c(0, 7750), c(-0.8,-0.8), lty=2, col="gray80")
lines(c(0, 7750), c(-1,-1), lty=2, col="gray80")
barplot(-data.5.3$m2, ylim=c(-1,0), width=(data.5.3[,2]+data.5.3[,3]),
        axes=FALSE, angle=45, density=20, col = "black", add=TRUE)
dev.copy2eps(file="fig53.eps", width=14, height=7)
par(.oldpar)

## Fig 5.4
###############################################################################

data.5.3 <- read.csv("fig53.csv")
data.5.3$f1 <- data.5.3[,4]/(data.5.3[,4]+data.5.3[,5])
data.5.3$f2 <- data.5.3[,5]/(data.5.3[,4]+data.5.3[,5])
# 
# par(mfrow=c(1,2))
# par(mar=c(3.1, 1.6, 1.1,1.6))
# mp <- barplot(-data.5.3$f1, horiz = TRUE, xlim=c(-1,0), width=(data.5.3[,4]+data.5.3[,5]),
#               axes=FALSE, angle=45, density=20, col = "black")
# axis(4, at=mp[seq(1,31,5)], labels=seq(40,70,5), las=2)
# axis(1, at=seq(0,-1,-0.2))
# barplot(data.5.3$f2, horiz = TRUE, xlim=c(0,1), width=(data.5.3[,4]+data.5.3[,5]),
#         axes=FALSE, angle=45, density=20, col = "black")
# axis(2, at=mp[seq(1,31,5)], labels=FALSE)
# axis(1, at=seq(0,1,0.2))
# dev.copy2eps(file="fig54.eps", width=14, height=8)


par(mfrow=c(2,1))
par(mar=c(1.3, .6, 1.3,2.6))
mp <- barplot(data.5.3$f1,  ylim=c(0,1), width=(data.5.3[,4]+data.5.3[,5]),
              axes=FALSE, angle=45, density=20, col = "black")
axis(1, at=mp[seq(1,31,5)], labels=seq(40,70,5))
axis(4, at=seq(0,1,0.2), las=2)
mtext("xx", side=2)
lines(c(0, 7750), c(0,0), lty=2, col="gray80")
lines(c(0, 7750), c(0.2,0.2), lty=2, col="gray80")
lines(c(0, 7750), c(0.4,0.4), lty=2, col="gray80")
lines(c(0, 7750), c(0.6,0.6), lty=2, col="gray80")
lines(c(0, 7750), c(0.8,0.8), lty=2, col="gray80")
lines(c(0, 7750), c(1,1), lty=2, col="gray80")
barplot(data.5.3$f1,  ylim=c(0,1), width=(data.5.3[,4]+data.5.3[,5]),
        axes=FALSE, angle=45, density=20, col = "black", add=TRUE)
barplot(-data.5.3$f2, ylim=c(-1,0), width=(data.5.3[,4]+data.5.3[,5]),
        axes=FALSE, angle=45, density=20, col = "black")
axis(3, at=mp[seq(1,31,5)], labels=FALSE)
axis(4, at=seq(-1,0,0.2), las=2)
mtext("yy", side=2)
lines(c(0, 7750), c(-0,-0), lty=2, col="gray80")
lines(c(0, 7750), c(-0.2,-0.2), lty=2, col="gray80")
lines(c(0, 7750), c(-0.4,-0.4), lty=2, col="gray80")
lines(c(0, 7750), c(-0.6,-0.6), lty=2, col="gray80")
lines(c(0, 7750), c(-0.8,-0.8), lty=2, col="gray80")
lines(c(0, 7750), c(-1,-1), lty=2, col="gray80")
barplot(-data.5.3$f2, ylim=c(-1,0), width=(data.5.3[,4]+data.5.3[,5]),
        axes=FALSE, angle=45, density=20, col = "black", add=TRUE)
dev.copy2eps(file="fig54.eps", width=14, height=7)
par(.oldpar)


## Tab 5.3
###############################################################################
require(xtable)
data.5.3 <- read.csv("fig53.csv")
data.5.3$m1 <- data.5.3[,2]/(data.5.3[,2]+data.5.3[,3])
data.5.3$m2 <- data.5.3[,3]/(data.5.3[,2]+data.5.3[,3])
data.5.3$f1 <- data.5.3[,4]/(data.5.3[,4]+data.5.3[,5])
data.5.3$f2 <- data.5.3[,5]/(data.5.3[,4]+data.5.3[,5])

table.5.3 <- data.5.3[,c(1,2,3,7,4,5,9)]
table.5.3[,4] <- table.5.3[,4]*100
table.5.3[,7] <- table.5.3[,7]*100
print(xtable(table.5.3), digits = c(0,0,0,2,0,0,2), include.rownames=FALSE )




## Fig 5.7
###############################################################################
require(RColorBrewer)
require(plotrix)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)
data.5.7 <- read.csv("fig57.csv")

data.5.7$m.over65 <- data.5.7[,5]-data.5.7[,6]
data.5.7$am.over65 <- data.5.7[,13]-data.5.7[,14]
data.5.7$P.am.over65 <- data.5.7$m.over65/data.5.7$am.over65

data.5.7$w.over60 <- data.5.7[,7]-data.5.7[,8]
data.5.7$aw.over60 <- data.5.7[,15]-data.5.7[,16]
data.5.7$P.aw.over60 <- data.5.7$w.over60/data.5.7$aw.over60

par(mar=c(3.1, 3.6, 1.1,1.1))
plot(data.5.7[1:4,1],data.5.7$P.am.over65[1:4], type="l", ylim=c(0.05,0.15), 
     xlim=c(1990, 2020), lwd=2, col="black", bty="n", axes=FALSE, xlab="", ylab="")
lines(data.5.7[1:4,1],data.5.7$P.aw.over60[1:4], lwd=2, col="red")
lines(data.5.7[4:8,1],data.5.7$P.aw.over60[4:8], lwd=2, col="red", lty=2)
lines(data.5.7[4:8,1],data.5.7$P.am.over65[4:8], lwd=2, col="black", lty=2)

axis(2, las=2, at=c(5,6,8,10,12,14)/100, labels=c(0,6,8,10,12,14)/100)
axis.break(2,5.5/100,style="slash") 
axis(1)

lines(c(1990, 2020), c(0.06,0.06), lty=2, col="gray80")
lines(c(1990, 2020), c(0.08,0.08), lty=2, col="gray80")
lines(c(1990, 2020), c(0.10,0.10), lty=2, col="gray80")
lines(c(1990, 2020), c(0.12,0.12), lty=2, col="gray80")
lines(c(1990, 2020), c(0.14,0.14), lty=2, col="gray80")
lines(c(1990, 2020), c(0.14,0.14), lty=2, col="gray80")
lines(c(2005, 2005), c(0.05,0.145), lty=2, col="gray80")

rect(1990, 0.11, 2005, 0.15, col="white", border=NA)
legend(1990, 0.14, legend=c( "m", "f"), col = c("black", "red"), lwd=c( 2,2), 
       box.col="white", lty = c(1,1), fill=c(NA, NA), border=c(NA, NA),
       bg = "white", merge = TRUE, cex=1.5, y.intersp = 1.4)

dev.copy2eps(file="fig57.eps", width=7, height=3.5)

par(.oldpar)

## Tab 5.3
###############################################################################
require(xtable)
data.5.7 <- read.csv("fig57.csv")

data.5.7$m.over65 <- data.5.7[,5]-data.5.7[,6]
data.5.7$am.over65 <- data.5.7[,13]-data.5.7[,14]
data.5.7$P.am.over65 <- data.5.7$m.over65/data.5.7$am.over65

data.5.7$w.over60 <- data.5.7[,7]-data.5.7[,8]
data.5.7$aw.over60 <- data.5.7[,15]-data.5.7[,16]
data.5.7$P.aw.over60 <- data.5.7$w.over60/data.5.7$aw.over60

table.5.7 <- cbind(data.5.7[,1],100*data.5.7[,6]/data.5.7[,14], 100*data.5.7[20],
                   100*data.5.7[,8]/data.5.7[,16], 100*data.5.7[23])

print(xtable(table.5.7), digits = c(0,2,2,2,2), include.rownames=FALSE )



## Fig 5.8
###############################################################################
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

data.5.8 <- read.csv("fig58.csv")
data.5.8$date <- rep(seq(1994,2014), each=4)+ rep(c(0, .25, .5, .75),21)


par(mar=c(3.1, 3.6, 3.1,1.1), xpd=TRUE)
plot(data.5.8[,10],data.5.8[,2], type="l",
     lwd=2, col="black", bty="n", axes=FALSE, xlab="", ylab="", ylim=c(0,100))
lines(data.5.8[,10],data.5.8[,3], col="gray40", lwd=2)
lines(data.5.8[,10],data.5.8[,4], col="gray60", lwd=2)
lines(data.5.8[,10],data.5.8[,5], col=redgray[1], lwd=2)

lines(data.5.8[,10],data.5.8[,6], col="black",lwd=2, lty=2)
lines(data.5.8[,10],data.5.8[,7], col="gray40", lwd=2, lty=2)
lines(data.5.8[,10],data.5.8[,8], col="gray60", lwd=2, lty=2)
lines(data.5.8[,10],data.5.8[,9],col=redgray[1], lwd=2, lty=2)
axis(1, at=seq(1994, 2015), labels=FALSE)
axis(2, las=2)
text(x=seq(1994, 2015), y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=seq(1994, 2015), srt=45, adj=1, xpd=TRUE)

legend(x=1995, y=115, c("a", "c", "e", "o"), horiz = TRUE, bty="n", x.intersp = 4,
       col=c("black", "gray40", "gray60", redgray[1]), lwd=2)


dev.copy2eps(file="fig58.eps", width=7, height=3.5)
par(.oldpar)

## Tab 5.8
###############################################################################
require(xtable)
data.5.8 <- read.csv("fig58.csv")
table.5.8 <- data.5.8[  seq(1,81,4),c(10,2:9)]

print(xtable(table.5.8,digits = c(0,0,1,1,1,1,1,1,1,1)) , include.rownames=FALSE )



## Fig 5.9 - working futures projections sectors
############################################################################### 
data.5.9 <- read.csv("labourproj.csv")
require(tidyr)
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

data.5.9 %>%
  filter(variable == "type", sector != "All industries") %>%
  dplyr::select(sector, variable, categroy, X1992, X2002, X2012, X2022) %>%
  mutate(X1992 = X1992/sum(X1992),
         X2002 = X2002/sum(X2002),
         X2012 = X2012/sum(X2012),
         X2022 = X2022/sum(X2022)) ->
  emp.typ

emp.typ %>%
  gather( year,prop, X1992:X2022,-variable) %>%
  dplyr::select(-variable) %>%
  arrange(sector) %>%
  filter(year !="X1992") %>%
  spread(categroy, prop) ->
  emp.typ

emp.typ.proj <- emp.typ %>%
  mutate(FT = ifelse(year %in% c("X2002", "X2012"), 0, FT ),
         PT = ifelse(year %in% c("X2002", "X2012"), 0, PT ),
         SE = ifelse(year %in% c("X2002", "X2012"), 0, SE ))


par(mar=c(4.6, 3, 1.1,3), xpd=TRUE)
  
mp <- barplot(t(as.matrix(emp.typ[3:5])),
        space=c(0.1,rep(c(0.1,0.1,0.9), 5), 0.1, 0.1),
        col= c(c("gray30", "gray50", "gray80"),  c(redgray[20], redgray[10],redgray[1])),
        density=10,
        names.arg = rep("", 18), axes=FALSE)
lines(mp[c(1,18)]+ c(-1, 1), c(0, 0), col="gray80", lty=2)
lines(mp[c(1,18)]+ c(-1, 1), c(0.1, 0.1), col="gray80", lty=2)
lines(mp[c(1,18)]+ c(-1, 1), c(0.2, 0.2), col="gray80", lty=2)
lines(mp[c(1,18)]+ c(-1, 1), c(0.3, 0.3), col="gray80", lty=2)
lines(mp[c(1,18)]+ c(-1, 1), c(0.05, 0.05), col="gray80", lty=2)
lines(mp[c(1,18)]+ c(-1, 1), c(0.15, 0.15), col="gray80", lty=2)
lines(mp[c(1,18)]+ c(-1, 1), c(0.25, 0.25), col="gray80", lty=2)
barplot(t(as.matrix(emp.typ[3:5])),
        space=c(0.1,rep(c(0.1,0.1,0.9), 5), 0.1, 0.1),
        col= c(c("gray30", "gray50", "gray80"),  c(redgray[20], redgray[10],redgray[1])),
        density=10,
        names.arg = rep("", 18), axes=FALSE, add=TRUE)

barplot(t(as.matrix(emp.typ.proj[3:5])),
        space=c(0.1,rep(c(0.1,0.1,0.9), 5), 0.1, 0.1),
        col= c(redgray[1], redgray[20],redgray[30]),
        density=10, add=TRUE,
        names.arg = rep("", 18), axes=FALSE)
text(mp, -0.02, rep(c(1,2,3),3))
text(rowMeans(matrix(mp, byrow=TRUE,c(6,3))), -0.06, LETTERS[1:6])
axis(2, las=2)
yz<- zoo::rollapply(c(0, cumsum(t(as.matrix(emp.typ[3:5]))[,18])), 2, mean)
text(mp[18]+1, yz, letters[1:3])

dev.copy2eps(file="fig59.eps", width=7, height=3.8)
par(.oldpar)


## Fig 5.10 - working futures projections sectors gender
############################################################################### 
data.5.9 %>%
  filter(variable == "gender", sector != "All industries") %>%
  select(sector, variable, categroy, X1992, X2002, X2012, X2022) %>%
  mutate(X1992 = X1992/sum(X1992),
         X2002 = X2002/sum(X2002),
         X2012 = X2012/sum(X2012),
         X2022 = X2022/sum(X2022)) ->
  emp.sex

emp.sex %>%
  gather( year,prop, X1992:X2022,-variable) %>%
  select(-variable) %>%
  arrange(sector) %>%
  filter(year !="X1992") %>%
  spread(categroy, prop) ->
  emp.sex

emp.sex.proj <- emp.sex %>%
  mutate(Men = ifelse(year %in% c("X2002", "X2012"), 0, Men ),
         Women = ifelse(year %in% c("X2002", "X2012"), 0, Women ))
         


par(mar=c(4.6, 3, 1.1,3), xpd=TRUE)

mp <- barplot(t(as.matrix(emp.sex[3:4])),
              space=c(0.1,rep(c(0.1,0.1,0.9), 5), 0.1, 0.1),
              col= c(c("gray30", "gray50", "gray80"),  c(redgray[20], redgray[10],redgray[1])),
              density=10,
              names.arg = rep("", 18), axes=FALSE)
lines(mp[c(1,18)]+ c(-1, 1), c(0, 0), col="gray80", lty=2)
lines(mp[c(1,18)]+ c(-1, 1), c(0.1, 0.1), col="gray80", lty=2)
lines(mp[c(1,18)]+ c(-1, 1), c(0.2, 0.2), col="gray80", lty=2)
lines(mp[c(1,18)]+ c(-1, 1), c(0.3, 0.3), col="gray80", lty=2)
lines(mp[c(1,18)]+ c(-1, 1), c(0.05, 0.05), col="gray80", lty=2)
lines(mp[c(1,18)]+ c(-1, 1), c(0.15, 0.15), col="gray80", lty=2)
lines(mp[c(1,18)]+ c(-1, 1), c(0.25, 0.25), col="gray80", lty=2)
barplot(t(as.matrix(emp.sex[3:4])),
        space=c(0.1,rep(c(0.1,0.1,0.9), 5), 0.1, 0.1),
        col= c(c("gray30", "gray50", "gray80"),  c(redgray[20], redgray[10],redgray[1])),
        density=10,
        names.arg = rep("", 18), axes=FALSE, add=TRUE)

barplot(t(as.matrix(emp.sex.proj[3:4])),
        space=c(0.1,rep(c(0.1,0.1,0.9), 5), 0.1, 0.1),
        col= c(redgray[1], redgray[20],redgray[30]),
        density=10, add=TRUE,
        names.arg = rep("", 18), axes=FALSE)
text(mp, -0.02, rep(c(1,2,3),3))
text(rowMeans(matrix(mp, byrow=TRUE,c(6,3))), -0.06, LETTERS[1:6])
axis(2, las=2)
yz<- zoo::rollapply(c(0, cumsum(t(as.matrix(emp.sex[3:4]))[,18])), 2, mean)
text(mp[18]+1, yz, letters[1:2])

dev.copy2eps(file="fig59.2.eps", width=7, height=3.8)


## Tab 5.10 - working futures tables
###############################################################################

require(xtable)
table.5.10<-cbind(emp.typ, emp.sex[,3:4])
table.5.10[,3:7] <- table.5.10[,3:7]*100

print(xtable(table.5.10,digits = c(0,0,0,2,2,2,2,2)), include.rownames=FALSE)


## Fig 6.1
###############################################################################
#2013-14_Section_1_Households_tables_and_figures_FINAL (1)
data.6.1 <- read.csv("fig61-2.csv")
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

par(mar=c(2.6, 2.5, 2.1,8.1), xpd=TRUE)

mp <- barplot(t(as.matrix(data.6.1[,2:5])), beside=TRUE, 
        col = c(redgray[2], redgray[2], "black", "black"),
        border = c(redgray[2],redgray[2],  "black", "black"),
        angle = c(45, 45, 45, 45),
        density = c(15, 5, 15, 5),
        axes=FALSE)
axis(2, las=2)

text(x=colMeans(mp), y=-3,
     labels=LETTERS[1:11], srt=45, adj=1, xpd=TRUE)

legend(x=53, y=50, c("a", "b", "c", "d"), 
       col = c(redgray[2],redgray[2], "black", "black"),
       border = c(redgray[2],redgray[2], "black", "black"),
       fill = c(redgray[2],redgray[2], "black", "black"),
       angle = c(45, 45, 45, 45),
       density = c(15, 5, 15, 5),
       bty="n", lwd=c(1,1,1,1), cex=1.2, lty = c(NA, NA, NA,NA),
       x.intersp=rep(-1.5,4))

lines(c(0,55), c(10,10), lty=2, col="gray80")
lines(c(0,55), c(20,20), lty=2, col="gray80")
lines(c(0,55), c(30,30), lty=2, col="gray80")
lines(c(0,55), c(40,40), lty=2, col="gray80")
lines(c(0,55), c(50,50), lty=2, col="gray80")
lines(c(0,55), c(60,60), lty=2, col="gray80")
lines(c(0,55), c(70,70), lty=2, col="gray80")
barplot(t(as.matrix(data.6.1[,2:5])), beside=TRUE, 
        col = c(redgray[2],redgray[2], "black", "black"),
        border = c(redgray[2],redgray[2], "black", "black"),
        angle = c(45, 45, 45, 45),
        density = c(15,5,15,5),
        axes=FALSE, add=TRUE)
dev.copy2eps(file="fig16.eps", width=7, height=3)
par(.oldpar)

# 
# par(mar=c(3.1, 3.1, 1.1,5.1))
# plot(c(2003:2013), data.6.1[,2], typ="n",
#      
#      xlab="", ylab="", lwd=2,
#      ylim=c(0,100))
# polygon(c(2003,c(2003:2013), 2013), c(0,data.6.1[,2],0), border="black", col= "black", density=30)
# polygon(c(c(2003:2013),c(2013:2003)), c((data.6.1[,2]), rev(data.6.1[,2]+data.6.1[,3])),
#         border="black", col= "black", density=20)
# polygon(c(data.6.1[,1], rev(data.6.1[,1])), c(data.6.1[,3],rev(data.6.1[,2])), border="black", col= "black", density=10)
# 

## Fig 6.2
###############################################################################
#2013-14_Section_1_Households_tables_and_figures_FINAL (1)
data.6.1 <- read.csv("fig61-2.csv")
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

par(mar=c(3.1, 2.5, 1.1,8.1), xpd=TRUE)
plot(2003:2013, data.6.1[,4], typ="l",
     axes= FALSE,
     col= "gray50",
     xlab="", ylab="", lwd=2,
     ylim=c(0,16))
lines(2003:2013, data.6.1[,6], lwd=2, col= redgray[1])
lines(2003:2013, data.6.1[,7], lwd=2, col= "red")

lines(c(2003, 2013), c(10,10), lty=2, col="gray80")
lines(c(2003, 2013), c(0,0), lty=2, col="gray80")
lines(c(2003, 2013), c(15,15), lty=2, col="gray80")
lines(c(2003, 2013), c(5,5), lty=2, col="gray80")
axis(2, las=2)
axis(1, at=2003:2013, labels=FALSE)
text(x=2003:2013, y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=c("2003-04","2004-05",
              "2005-06","2006-07",
              "2007-08","2008-09",
              "2009-10","2010-11",
              "2011-12","2012-13", "2013-14"), srt=45, adj=1, xpd=TRUE)
text(2013.2, data.6.1[11,4], "a")
text(2013.2, data.6.1[11,6], "b")
text(2013.2, data.6.1[11,7], "c")


dev.copy2eps(file="fig17.eps", width=7, height=3.5)
par(.oldpar)

# 

## Fig 6.3
###############################################################################
data.6.3 <- read.csv("fig63.csv")
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)


par(mar=c(2.1, 2.5, 1.1,8.1), xpd=TRUE)
plot(data.6.3[,1], data.6.3[,2], typ="l",
     axes= FALSE,
     col= redgray[1],
     xlab="", ylab="", lwd=2,
     ylim=c(0,35))
lines(data.6.3[,1], data.6.3[,3], lwd=2, col= "red")
lines(data.6.3[,1], data.6.3[,4], lwd=2, col= redgray[80])
lines(data.6.3[,1], data.6.3[,5], lwd=2, col= redgray[90])
lines(data.6.3[,1], data.6.3[,6], lwd=2, col= "black")

lines(c(2008, 2013), c(10,10), lty=2, col="gray80")
lines(c(2008, 2013), c(0,0), lty=2, col="gray80")
lines(c(2008, 2013), c(15,15), lty=2, col="gray80")
lines(c(2008, 2013), c(5,5), lty=2, col="gray80")
lines(c(2008, 2013), c(20,20), lty=2, col="gray80")
lines(c(2008, 2013), c(30,30), lty=2, col="gray80")
lines(c(2008, 2013), c(25,25), lty=2, col="gray80")
lines(c(2008, 2013), c(35,35), lty=2, col="gray80")

axis(2, las=2)
axis(1, at=2008:2013)

text(2013.2, data.6.3[6,2], "a")
text(2013.2, data.6.3[6,3], "b")
text(2013.2, data.6.3[6,4], "c")
text(2013.2, data.6.3[6,5], "d")
text(2013.2, data.6.3[6,6]-1, "e")
text(2013.2, data.6.3[6,6]-2.8, "f")

dev.copy2eps(file="fig18.eps", width=7, height=3.5)

par(.oldpar)

## Fig 6.6
###############################################################################
data.6.6 <- read.csv("failing.csv")

par(mar = c(3.6, 3.6, 0.6, 0.1), xpd=TRUE)
mp <- barplot(as.matrix(data.6.6[2:6]),
              space=c(0.1,rep(c(0.1,0.1,0.9), 4), 0.1, 0.1),
              col= c("gray30", "gray50", "gray80"),
              density=10, beside=TRUE,
              names.arg = rep("", 15), axes=FALSE)
lines(c(0, 20), c(5,5), lty=2, col="gray80")
lines(c(0, 20), c(15,15), lty=2, col="gray80")
lines(c(0, 20), c(0,0), lty=2, col="gray80")
lines(c(0, 20), c(10,10), lty=2, col="gray80")
lines(c(0, 20), c(20,20), lty=2, col="gray80")
barplot(as.matrix(data.6.6[2:6]),
        space=c(0.1,rep(c(0.1,0.1,0.9), 4), 0.1, 0.1),
        col= c("gray30", "gray50", "gray80"),
        density=10, beside=TRUE,
        names.arg = rep("", 15), axes=FALSE, add=TRUE)
axis(2, las=2)
legend(15,20, c("a", "b", "c"), 
       col = c("gray30", "gray50", "gray80"),
       border = c("gray30", "gray50", "gray80"),
       fill = c("gray30", "gray50", "gray80"),
       angle = c(45, 45, 45, 45),
       density = 10,
       bty="n", lwd=c(1,1,1), cex=1.2, lty = c(NA,  NA,NA),
       y.intersp=1.4)

text(colMeans(mp), -1, LETTERS[1:5])
dev.copy2eps(file="fig66.eps", width=7, height=3.5)

## Fig 6.7
###############################################################################
data.6.7 <- read.csv("fig67.csv")
par(mar=c(2.1,2.1, 2.1,1), xpd=TRUE)
x <- barplot(data.6.7[,2], axes=FALSE,
        density=15, space=1)
axis(2, las=2, at=c(0,10,20,30,40))
text(x, -2, letters[1:4])
for(i in c(0,10,20,30,40)) {
  lines(c(0.75,max(x)+.75), c(i,i), lty=2, col="gray80")
}
barplot(data.6.7[,2], axes=FALSE,
        density=15, add=TRUE, space=1)

dev.copy2eps(file="fig67.eps", width=7, height=3.5)

## Tab 6.1
###############################################################################
require(xtable)
table.6.1 <-data.6.1
print(xtable(table.6.1), include.rownames=FALSE )

## Tab 6.2
###############################################################################
require(xtable)
table.6.2 <-t(data.6.3)
print(xtable(table.6.2) )

## Tab 6.3
###############################################################################
require(xtable)
table.6.2 <-t(data.6.3)
print(xtable(table.6.2) )

## Tab 6.6
###############################################################################
require(xtable)
table.6.6 <-t(data.6.6)
print(xtable(table.6.6) )


## Tab 6.7
###############################################################################
require(xtable)
table.6.7 <-t(data.6.7)
print(xtable(table.6.7) )


## Fig 7.1
###############################################################################
data.7.1 <- read.csv("fig71.csv")
par(mar=c(3.1, 2.5, 1.1,7.1), xpd=TRUE)

barplot(t(as.matrix(data.7.1[,2:3])), beside=TRUE, col =c("black","gray50"), 
        density=c(15, 10), names.arg = 1:6,
        axes=FALSE)
axis(2, las=2)
legend(19,50, letters[1:2],col =c( "gray50", "black"), fill =  c("black","gray50"),
       density=c(15,10), bty="n", cex=2, y.intersp = 2)
lines(c(0, 19), c(20,20), lty=2, col="gray80")
lines(c(0, 19), c(40,40), lty=2, col="gray80")
lines(c(0, 19), c(60,60), lty=2, col="gray80")
lines(c(0, 19), c(80,80), lty=2, col="gray80")
barplot(t(as.matrix(data.7.1[,2:3])), beside=TRUE, col =c("black","gray50"), 
        density=c(15, 10), names.arg = 1:6,
        axes=FALSE, add=TRUE)
# par(mar=c(3.6, 7.1, 2.1,3.6))
# plot( c(1,2,3,4,5,6),c(data.7.1[,2]), type="l", ylim=c(0, 100), col=redgray[1],
#       xaxt="n", lwd=2,xlim=c(1,6.2),
#      axes=FALSE, xlab="", ylab="")
# lines(c(1,2,3,4,5,6),data.7.1[,3], type="l", col="black", lwd=2)
# axis(4, las=2)
# mtext("y", side=4, line=3)
# axis(1, at=seq(1,6, 1), labels=letters[1:6])

dev.copy2eps(file="fig19.eps", width=7, height=2.5)
par(.oldpar)

## Fig 7.2
###############################################################################
data.7.2 <- read.csv("fig72.csv")

par(mar=c(1.1, 11.1, 0,0.1), xpd=TRUE)
par(lwd = 1)
mp <- barplot(as.matrix(data.7.2[,2:7]), beside=TRUE, horiz=TRUE, names.arg = rep(NA,6),
              col =gray.colors(6), angle=45, lwd=2, density=rev(c(1,5,10,15,20,30)))
axis(1)
text(rep(-11,6), colMeans(mp), LETTERS[1:6])
lines(c(20,20), c(0,43), lty=2, col="gray80")
lines(c(40,40), c(0,43), lty=2, col="gray80")
lines(c(60,60), c(0,43), lty=2, col="gray80")
lines(c(80,80), c(0,43), lty=2, col="gray80")
barplot(as.matrix(data.7.2[,2:7]), beside=TRUE, horiz=TRUE, names.arg = rep(NA,6),
        col =gray.colors(6), angle=45, lwd=2, density=rev(c(1,5,10,15,20,30)), add=TRUE)
text(rep(-2,6), mp[,1], 1:6)
text(rep(-2,6), mp[,2], 1:6)
text(rep(-2,6), mp[,3], 1:6)
text(rep(-2,6), mp[,4], 1:6)
text(rep(-2,6), mp[,5], 1:6)
text(rep(-2,6), mp[,6],1:6)

dev.copy2eps(file="fig20.eps", width=7, height=5)

par(.oldpar)
# 
# par(mar=c(3.6, 7.1, 2.1,3.6))
# plot( c(1,2,3,4,5,6),c(data.7.2[,2]), type="l", ylim=c(0, 100), col=redgray[1],
#       xaxt="n", lwd=2,xlim=c(1,6.2),
#       axes=FALSE, xlab="", ylab="")
# lines(c(1,2,3,4,5,6),data.7.2[,3], type="l", col="black", lwd=2)
# lines(c(1,2,3,4,5,6),data.7.2[,5], type="l", col="gray30", lwd=2)
# lines(c(1,2,3,4,5,6),data.7.2[,6], type="l", col="gray70", lwd=2)
# lines(c(1,2,3,4,5,6),data.7.2[,7], type="l", col=redgray[20], lwd=2)
# lines(c(1,2,3,4,5,6),data.7.2[,4], type="l", col="red", lwd=2)
# 
# axis(4, las=2)
# mtext("y", side=4, line=3)
# axis(1, at=seq(1,6, 1), labels=letters[1:6])
# 
# text(0.8, data.7.2[1,2], "a")
# text(0.8, data.7.2[1,3], "b")
# text(0.8, data.7.2[1,4], "c")
# text(0.8, data.7.2[1,5], "d")
# text(0.8, data.7.2[1,6], "e")
# text(0.8, data.7.2[1,7], "f")

## Fig 7.3
###############################################################################
data.7.3 <- read.csv("fig73.csv")

par(mar=c(3.1, 2.5, 1.1,7.1), xpd=TRUE)
mp <- barplot(t(as.matrix(data.7.3[,2:9])), beside=TRUE, names.arg = 1:6,
              col =gray.colors(8), lwd=2, density=rev(c(1,5,10,15,20,25,30,35)), angle=45,
              axes=FALSE)
axis(2, las=2)
lines(c(0, 55), c(20,20), lty=2, col="gray80")
lines(c(0, 55), c(40,40), lty=2, col="gray80")
lines(c(0, 55), c(60,60), lty=2, col="gray80")
lines(c(0, 55), c(80,80), lty=2, col="gray80")
barplot(t(as.matrix(data.7.3[,2:9])), beside=TRUE, names.arg = 1:6,
        col =gray.colors(8), lwd=2, density=rev(c(1,5,10,15,20,25,30,35)), angle=45,
        axes=FALSE, add=TRUE)

legend(55,75, LETTERS[1:8], fill =  rev(gray.colors(8)),
       bty="n", cex=1.5, y.intersp = 1,density=c(1,5,10,15,20,25,30,35), angle=45)

dev.copy2eps(file="fig21.eps", width=7, height=3.5)

par(.oldpar)


## Fig 7.4
###############################################################################
data.7.4 <- read.csv("fig74.csv")

par(mar=c(3.1, 2.5, 1.1,7.1), xpd=TRUE)
mp <- barplot(t(as.matrix(data.7.4[,2:6])), beside=TRUE, names.arg = 1:6,
              col =gray.colors(5), lwd=2,density=rev(c(5,10,15,20,25)), angle=45,
              axes=FALSE)
axis(2, las=2)

lines(c(0, 36), c(20,20), lty=2, col="gray80")
lines(c(0, 36), c(40,40), lty=2, col="gray80")
lines(c(0, 36), c(60,60), lty=2, col="gray80")
lines(c(0, 36), c(80,80), lty=2, col="gray80")
legend(37,75, LETTERS[1:5], fill =  rev(gray.colors(5)),density=c(5,10,15,20,25), angle=45,
       bty="n", cex=1.5, y.intersp = 1)
barplot(t(as.matrix(data.7.4[,2:6])), beside=TRUE, names.arg = 1:6,
        col =gray.colors(5), lwd=2, density=rev(c(5,10,15,20,25)), angle=45,
        axes=FALSE, add=TRUE)
dev.copy2eps(file="fig22.eps", width=7, height=3.5)


par(.oldpar)

## Fig 7.5
###############################################################################
data.7.5 <- read.csv("fig75.csv")

par(mar=c(3.1, 2.5, 1.1,7.1), xpd=TRUE)
mp <- barplot(t(as.matrix(data.7.5[,2:6])), beside=TRUE, names.arg = 1:6,
              col =gray.colors(5), lwd=2,density=rev(c(5,10,15,20,25)), angle=45,
              axes=FALSE)
axis(2, las=2)

lines(c(0, 36), c(10,10), lty=2, col="gray80")
lines(c(0, 36), c(20,20), lty=2, col="gray80")
lines(c(0, 36), c(30,30), lty=2, col="gray80")
lines(c(0, 36), c(40,40), lty=2, col="gray80")
legend(37,30, LETTERS[1:5], fill =  rev(gray.colors(5)),
       bty="n", cex=1.5, y.intersp = 1, density=c(5,10,15,20,25), angle=45)

barplot(t(as.matrix(data.7.5[,2:6])), beside=TRUE, names.arg = 1:6,
        col =gray.colors(5), lwd=2, density=rev(c(5,10,15,20,25)), angle=45,
        axes=FALSE,add=TRUE)
dev.copy2eps(file="fig23.eps", width=7, height=3.5)
par(.oldpar)
## Fig 7.6
###############################################################################
data.7.6 <- read.csv("fig76.csv")

par(mar=c(3.1, 2.5, 1.1,7.1), xpd=TRUE, lwd=1)
mp <- barplot(as.matrix(data.7.6[,2:5]), beside=TRUE, names.arg = 1:4,
              col =gray.colors(3), border=c("black", "black",  "red"), lwd=2,
              density=rev(c(5,15,25)),angle=45,
              axes=FALSE)
axis(2, las=2)

lines(c(0, 17), c(60,60), lty=2, col="gray80")
lines(c(0, 17), c(20,20), lty=2, col="gray80")
lines(c(0, 17), c(80,80), lty=2, col="gray80")
lines(c(0, 17), c(40,40), lty=2, col="gray80")
legend(17,50, LETTERS[1:3], fill =  rev(gray.colors(3)),border=c(  "red","black", "black"),
       bty="n", cex=1.5, y.intersp = 1,density=c(5,15,25),angle=45)
barplot(as.matrix(data.7.6[,2:5]), beside=TRUE, names.arg = 1:4,
        col =gray.colors(3), border=c("black", "black",  "red"), lwd=2,
        density=rev(c(5,15,25)),angle=45,
        axes=FALSE, add=TRUE)

dev.copy2eps(file="fig24.eps", width=7, height=3.5)
par(.oldpar)

## Fig 7.7
###############################################################################
data.7.7 <- read.csv("fig77.csv")

par(mar=c(3.1, 2.5, 1.1,4.1), xpd=TRUE, lwd=1)
mp <- barplot(as.matrix(data.7.7[,2:11]), beside=TRUE, names.arg = rep("",10),
              col =gray.colors(4), lwd=2,
              density=rev(c(5,10,15,20)),angle=45,
              axes=FALSE, space = c(1, rep(0,3),3,rep(0,3),1,rep(0,3),1,rep(0,3),1,rep(0,3),1,
                                    rep(0,3),1,rep(0,3),3,rep(0,3),1,rep(0,3),1,rep(0,3)))
axis(2, las=2)
lines(c(0, 55), c(0,0), lty=2, col="gray80")
lines(c(0, 55), c(60,60), lty=2, col="gray80")
lines(c(0, 55), c(20,20), lty=2, col="gray80")
lines(c(0, 55), c(80,80), lty=2, col="gray80")
lines(c(0, 55), c(40,40), lty=2, col="gray80")

barplot(as.matrix(data.7.7[,2:11]), beside=TRUE, names.arg = rep("",10),
        col =gray.colors(4), lwd=2,
        density=rev(c(5,10,15,20)),angle=45,
        axes=FALSE, space = c(1, rep(0,3),3,rep(0,3),1,rep(0,3),1,rep(0,3),1,rep(0,3),1,
                              rep(0,3),1,rep(0,3),3,rep(0,3),1,rep(0,3),1,rep(0,3)), add=TRUE)
legend(54,70, LETTERS[1:4], fill =  rev(gray.colors(4)),
       bty="n", cex=1.5, y.intersp = 1,density=c(5,10,15,20),angle=45)
text(colMeans(mp), -7, 1:10)

text(mean(mp[,4:5]), -15, "X")
text(mean(mp[,9]), -15, "Y")

dev.copy2eps(file="fig77.eps", width=7, height=3.5)
par(.oldpar)


## Tab 7.1
###############################################################################
require(xtable)
table.7.1 <- t(data.7.1)
print(xtable(table.7.1) )



## Tab 7.2
###############################################################################
require(xtable)
table.7.2 <- data.7.2
print(xtable(table.7.2), include.rownames=FALSE )



## Tab 7.3
###############################################################################
require(xtable)
table.7.3 <- data.7.3
print(xtable(table.7.3), include.rownames=FALSE )


## Tab 7.4
###############################################################################
require(xtable)
table.7.4 <- data.7.4
print(xtable(table.7.4), include.rownames=FALSE )

## Tab 7.5
###############################################################################
require(xtable)
table.7.5 <- data.7.5
print(xtable(table.7.5), include.rownames=FALSE )

## Tab 7.6
###############################################################################
require(xtable)
table.7.6 <- data.7.6
print(xtable(table.7.6), include.rownames=FALSE )

## Tab 7.7
###############################################################################
require(xtable)
table.7.7 <- t(data.7.7)
print(xtable(table.7.7) )


## Fig 8.1
###############################################################################
data.8.1 <- read.csv("fig81.csv")
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

par(mar=c(3.1, 2.5, 1.1,2.1), xpd=TRUE)
plot(1998:2013, data.8.1[,2], typ="l",
     axes= FALSE,
     col= "red",
     xlab="", ylab="", lwd=2,
     ylim=c(0,45))

lines(c(1998, 2013), c(10,10), lty=2, col="gray80")
lines(c(1998, 2013), c(0,0), lty=2, col="gray80")
lines(c(1998, 2013), c(20,20), lty=2, col="gray80")
lines(c(1998, 2013), c(30,30), lty=2, col="gray80")
lines(c(1998, 2013), c(40,40), lty=2, col="gray80")
lines(c(2010, 2010), c(0,40), lty=2, col="gray80")
points(1998:2013, data.8.1[,2], pch=17, col="red")

points(1998:2013, data.8.1[,3], pch=15, col= redgray[1])

lines(1998:2013, data.8.1[,3], lwd=2, col= redgray[1])
lines(1998:2013, data.8.1[,4], lwd=2, col= "black")
points(1998:2013, data.8.1[,4], pch=16, col= "black")

axis(2, las=2)
axis(1, at=1998:2013, labels=FALSE)
text(x=1998:2013, y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=c("1998-99",
       "1999-00","2000-01",
       "2001-02","2002-03",
       "2003-04","2004-05",
              "2005-06","2006-07",
              "2007-08","2008-09",
              "2009-10","2010-11",
              "2011-12","2012-13", "2013-14"), srt=45, adj=1, xpd=TRUE)

legend(2004,50, LETTERS[1:3], col =  c(redgray[1], "red", "black"),
       bty="n", cex=1, y.intersp = 3.5, lwd=2, pch=c(15, 17, 16))


dev.copy2eps(file="fig25.eps", width=7, height=3.5)
par(.oldpar)

## Fig 8.2
###############################################################################
data.8.2 <- read.csv("fig82.csv")

# manually add totals
totals <- c(13,	24,	24,	21,	18)

par(mar=c(4.1, 2.5, 2.1,7.1), xpd=TRUE)
mp <- barplot(as.matrix(data.8.2[,2:6]), beside=TRUE, names.arg = letters[1:5],
              col =gray.colors(6), lwd=2,
              axes=FALSE, xlab = "ic", ylim=c(0,30),
              density=rev(c(1,5,10,15,20,25)),angle=45)
axis(2, las=2)

lines(c(0, 36), c(30,30), lty=2, col="gray80")
lines(c(0, 36), c(10,10), lty=2, col="gray80")
lines(c(0, 36), c(20,20), lty=2, col="gray80")
lines(c(0, 36), c(0,0), lty=2, col="gray80")
lines(c(0, 36), c(15,15), lty=2, col="gray80")
lines(c(0, 36), c(5,5), lty=2, col="gray80")
lines(c(0, 36), c(25,25), lty=2, col="gray80")
legend(37,20, LETTERS[1:6], fill =  rev(gray.colors(6)),
       bty="n", cex=1.5, y.intersp = 1,density=c(1,5,10,15,20,25),angle=45)
legend(36,23, LETTERS[7], bty="n", cex=1.5,
        x.intersp = 0.2, lwd=2, lty=3, col="red")

for (i in 1:5){
lines(c((i-1)*7+1, (i-1)*7+7), c(totals[i], totals[i]), lty=3, lwd=2, col="red")}


barplot(as.matrix(data.8.2[,2:6]), beside=TRUE, names.arg = letters[1:5],
        col =gray.colors(6), lwd=2, ylim=c(0,30),
        density=rev(c(1,5,10,15,20,25)),angle=45,
        axes=FALSE, add=TRUE)

dev.copy2eps(file="fig82.eps", width=7, height=3.8)
par(.oldpar)

## Fig 8.3
###############################################################################
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)
data.8.3 <- read.csv("fig83.csv")
par(mar=c(4.1, 2.5, 1.1,7.1), xpd=TRUE)

mp <- barplot(as.matrix(data.8.3[c(1,3,2,4),3:7]), beside=TRUE, 
              lwd=2,ylim=c(0,25),
              space = c(0,rep(c(0,0.3,0,1),4), 0,0.3,0),
              axes=FALSE,
              col=c(redgray[1], redgray[1], "gray50", "gray50"),
              angle=45, density = c(20,10))

axis(2, las=2, at = seq(0,20,5))

lines(c(0, 26), c(10,10), lty=2, col="gray80")
lines(c(0, 26), c(20,20), lty=2, col="gray80")
lines(c(0, 26), c(0,0), lty=2, col="gray80")
lines(c(0, 26), c(15,15), lty=2, col="gray80")
lines(c(0, 26), c(5,5), lty=2, col="gray80")
barplot(as.matrix(data.8.3[c(1,3,2,4),3:7]), beside=TRUE, 
        lwd=2,ylim=c(0,25),
        space = c(0,rep(c(0,0.3,0,1),4), 0,0.3,0),
        axes=FALSE,
        col=c(redgray[1], redgray[1], "gray50", "gray50"),
        angle=45, density = c(20,10), add=TRUE)
legend(25,15, LETTERS[1:2], fill =  c(redgray[1], redgray[1] ),angle=45, density = c(20,10),
       bty="n", cex=1.5, y.intersp = 1)

legend(25,10, LETTERS[3:4], fill =  c("gray50", "gray50"),angle=45, density = c(20,10),
       bty="n", cex=1.5, y.intersp = 1)

dev.copy2eps(file="fig83.eps", width=7, height=3.5)

par(.oldpar)

data.8.4[,3:7] - data.8.3[,3:7]
data.8.3[,3] - data.8.3[,7]
data.8.4[,3] - data.8.4[,7]
## Fig 8.4
###############################################################################
data.8.4 <- read.csv("fig84.csv")
par(mar=c(4.1, 2.5, 1.1,7.1), xpd=TRUE)
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

mp <- barplot(as.matrix(data.8.4[c(1,3,2,4),3:7]), beside=TRUE, 
              lwd=2,
              space = c(0,rep(c(0,0.3,0,1),4), 0,0.3,0),
              axes=FALSE, ylim=c(0,25),
              col=c(redgray[1], redgray[1], "gray50", "gray50"),
              angle=45, density = c(20,10))

axis(2, las=2, at = seq(0,20,5))

lines(c(0, 26), c(10,10), lty=2, col="gray80")
lines(c(0, 26), c(20,20), lty=2, col="gray80")
lines(c(0, 26), c(0,0), lty=2, col="gray80")
lines(c(0, 26), c(15,15), lty=2, col="gray80")
lines(c(0, 26), c(5,5), lty=2, col="gray80")
barplot(as.matrix(data.8.4[c(1,3,2,4),3:7]), beside=TRUE, 
        lwd=2,ylim=c(0,25),
        space = c(0,rep(c(0,0.3,0,1),4), 0,0.3,0),
        axes=FALSE,
        col=c(redgray[1], redgray[1], "gray50", "gray50"),
        angle=45, density = c(20,10), add=TRUE)
legend(25,15, LETTERS[1:2], fill =  c(redgray[1], redgray[1] ),angle=45, density = c(20,10),
       bty="n", cex=1.5, y.intersp = 1)

legend(25,10, LETTERS[3:4], fill =  c("gray50", "gray50"),angle=45, density = c(20,10),
       bty="n", cex=1.5, y.intersp = 1)

dev.copy2eps(file="fig28.eps", width=7, height=3.5)

par(.oldpar)

## Fig 8.6
###############################################################################
data.8.6 <- read.csv("fig86.csv")
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

par(mar=c(3.1, 4.5, 3.1,1.1), xpd=TRUE)

mp <- barplot(t(as.matrix(data.8.6[,2:3])), beside=TRUE, 
              lwd=2,
              axes=FALSE,
              col=c(redgray[1],  "gray50"),
              angle=45, density = c(20,10), ylab="x")

for (i in seq(0,180,30)){
  lines(c(0, 18.5), c(i,i), lty=2, col="gray80")
}
axis(2, las=2, at = seq(0,180,30))
barplot(t(as.matrix(data.8.6[,2:3])), beside=TRUE, 
        lwd=2,
        axes=FALSE,
        col=c(redgray[1],  "gray50"),
        angle=45, density = c(20,10), add=TRUE)
text(colMeans(mp), -10, letters[1:6])
legend(5,205, LETTERS[1:2], fill =  c(redgray[1], "gray50" ),angle=45, density = c(20,10),
       bty="n", cex=1.5, y.intersp = 1)

dev.copy2eps(file="fig86.eps", width=7, height=3.5)
par(.oldpar)

## Fig 8.7
###############################################################################
data.8.7 <- read.csv("fig87.csv")
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

par(mar=c(3.1, 4.5, 2.1,0.1), xpd=TRUE)

mp <- barplot(t(as.matrix(data.8.7[,c(2,3,10)])), beside=TRUE, 
              lwd=2,
              axes=FALSE,
              col=c( "gray50", "red", redgray[1]),
              angle=45, density = c(10,15,15), ylab="x")
for (i in seq(0,700,100)){
  lines(c(0, 28.5), c(i,i), lty=2, col="gray80")
}
axis(2, las=2, at = seq(0,700,100))
barplot(t(as.matrix(data.8.7[,c(2,3,10)])), beside=TRUE, 
        lwd=2,
        axes=FALSE,
        col=c( "gray50", "red", redgray[1]),
        angle=45, density = c(10,15,15), ylab="x", add=TRUE)
text(colMeans(mp), -30, letters[1:7])
legend(3,700, LETTERS[1:3], fill =  c( "gray50", "red", redgray[1]),angle=45, density = c(10,20,20),
       bty="n", cex=1.5, y.intersp = 1)

dev.copy2eps(file="fig87.eps", width=7, height=3.5)

par(.oldpar)

data.8.7

## Fig 8.8
###############################################################################
data.8.8 <- read.csv("fig88.csv")
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

cbind(data.8.8, data.8.8[,9]-data.8.8[,4])

par(mar=c(3.1, 2.5, 1.1,2.1), xpd=TRUE)
plot(seq(1984, 2009, 5), data.8.8[2,4:9], typ="l",
     axes= FALSE,
     col= "red", ylim=c(70,90),
     xlab="", ylab="", lwd=2)

for (i in seq(70,90,5)){
  lines(c(1983.5, 2009.5), c(i,i), lty=2, col="gray80")
}

polygon(c(seq(1984, 2009, 5), rev(seq(1984, 2009, 5))),
        c(data.8.8[1,4:9], rev(data.8.8[3,4:9])), col="black", density=20, lwd=0.2, border=NA)
lines(seq(1984, 2009, 5), data.8.8[2,4:9],lwd=2)
polygon(c(seq(1984, 2009, 5), rev(seq(1984, 2009, 5))),
        c(data.8.8[4,4:9], rev(data.8.8[6,4:9])), col="gray50", density=20, lwd=0.2,border=NA)
lines(seq(1984, 2009, 5), data.8.8[5,4:9],lwd=2, col="gray50")
polygon(c(seq(1984, 2009, 5), rev(seq(1984, 2009, 5))),
        c(data.8.8[7,4:9], rev(data.8.8[9,4:9])), col="gray60", density=20, lwd=0.2,border=NA)
lines(seq(1984, 2009, 5), data.8.8[8,4:9], lwd=2, col="gray60")
polygon(c(seq(1984, 2009, 5), rev(seq(1984, 2009, 5))),
        c(data.8.8[10,4:9], rev(data.8.8[12,4:9])), col=redgray[1], density=20, lwd=0.2,angle=-45,border=NA)
lines(seq(1984, 2009, 5), data.8.8[11,4:9],lwd=2, col=redgray[1])
polygon(c(seq(1984, 2009, 5), rev(seq(1984, 2009, 5))),
        c(data.8.8[13,4:9], rev(data.8.8[15,4:9])), col=redgray[10], density=20, lwd=0.2,angle=-45,border=NA)
lines(seq(1984, 2009, 5), data.8.8[14,4:9],lwd=2, col=redgray[10])
polygon(c(seq(1984, 2009, 5), rev(seq(1984, 2009, 5))),
        c(data.8.8[16,4:9], rev(data.8.8[18,4:9])), col=redgray[20], density=20, lwd=0.2,angle=-45, border=NA)
lines(seq(1984, 2009, 5), data.8.8[17,4:9],lwd=2, col=redgray[20])

points(seq(1984, 2009, 5), data.8.8[2,4:9],lwd=2, col="black")
points(seq(1984, 2009, 5), data.8.8[5,4:9],lwd=2, col="gray50")
points(seq(1984, 2009, 5), data.8.8[8,4:9], lwd=2, col="gray60")
points(seq(1984, 2009, 5), data.8.8[11,4:9],lwd=2, col=redgray[1])
points(seq(1984, 2009, 5), data.8.8[14,4:9],lwd=2, col=redgray[10])
points(seq(1984, 2009, 5), data.8.8[17,4:9],lwd=2,col=redgray[20])

axis(2, las=2)
axis(1, at=seq(1984, 2009, 5), labels=letters[1:6])

legend(1984,91, legend=c("", "", ""), col =  c("black", "gray50", "gray60"),
       bty="n", cex=1, y.intersp = 1.5, lwd=2, pch=c(1,1,1))
legend(1985,91, legend=c("X", "Y", "Z"), col =  c(redgray[1], redgray[10], redgray[20]),
       bty="n", cex=1, y.intersp = 1.5, lwd=2, pch=c(1,1,1))

text( 2009.5,mean(data.8.8[1:9,9]), "g")
text( 2009.5,mean(data.8.8[10:18,9]), "h")
dev.copy2eps(file="fig88.eps", width=7, height=3.5)
par(.oldpar)

## Fig 8.9
###############################################################################
data.8.9 <- read.csv("fig89.csv")
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

layout(matrix(c(1,2,3),1), widths=c(7,1,7), heights=c(1,1,1))
par(mar=c(3.6, 0.3, 1.1,3), xpd=TRUE)
x <- barplot(as.matrix(-data.8.9[1:4,3:6]), beside=TRUE,
             axes=FALSE, 
             col=gray.colors(4),
             density=15, horiz = TRUE,
             names.arg = rep("", 4),  xlim=c(-60,0),space = c(0,2),
             main="mm")

text(-data.8.9[4,4]-1, 11.5, "X")
text(-data.8.9[4,5]-1, 17.5, "X")
text(-data.8.9[4,6]-1, 23.5, "X")
for (i in seq(0,-60,-10)){
  lines( c(i,i),c(1, max(x)+1), lty=2, col="gray80")
}
barplot(as.matrix(-data.8.9[1:4,3:6]), beside=TRUE,
        axes=FALSE, 
        col=gray.colors(4),
        density=15, horiz = TRUE,
        names.arg = rep("", 4),  xlim=c(-60,0),space = c(0,2),
        main="mm", add=TRUE)
axis(1, at=seq(0,-60,-10), labels=seq(0,60,10))

par("usr")

par(mar=c(3.6, 0, 1.1,0), xpd=TRUE)
plot.new()
par("usr")
par(mar=c(3.6, 0, 0.1,0), xpd=TRUE)
  t.pos <- x/(24.88-1.12)*1.08-0.09

text( x=.5,y=t.pos, LETTERS[1:16])
text( x=.5, y=mean(c(t.pos[4,1], t.pos[1,2])), "a")
text( x=.5, y=mean(c(t.pos[4,2], t.pos[1,3])), "b")
text( x=.5, y=mean(c(t.pos[4,3], t.pos[1,4])), "c")
text( x=.5, y=1.05, "d")

par(mar=c(3.6, 3, 1.1,0.3), xpd=TRUE)
x <-barplot(as.matrix(data.8.9[5:8,3:6]), beside=TRUE,
            axes=FALSE, 
            col=gray.colors(4),
            density=15, horiz = TRUE,
            names.arg = rep("", 4),  xlim=c(0,60),space = c(0,2),
            main="ff")

for (i in seq(0,60,10)){
  lines( c(i,i),c(1, max(x)+1), lty=2, col="gray80")
}

axis(1, at=seq(0,60,10))
barplot(as.matrix(data.8.9[5:8,3:6]), beside=TRUE,
        axes=FALSE, 
        col=gray.colors(4),
        density=15, horiz = TRUE,
        names.arg = rep("", 4),  xlim=c(0,60),space = c(0,2),
        main="ff", add=TRUE)

text(data.8.9[8,5]+1, 17.5, "X")
text(data.8.9[8,6]+1, 23.5, "X")
text(60, -1.5, "XX")

dev.copy2eps(file="fig89.eps", width=7, height=4.5)
par(.oldpar)
layout(1)


## Fig 8.10
###############################################################################
data.8.10 <- read.csv("fig810.csv")
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)
data.8.10[10,2:6] <- apply(data.8.10[6:8,2:6], 2, sum)


par(mar=c(3.6, 3.1, 1.1,7), xpd=TRUE)
x <- barplot(as.matrix(data.8.10[c(1:5,10),2:6]),
        axes=FALSE,names.arg = rep("", 5),
        ylim=c(0,10000), density=c(25,20,15,10,5,0))
axis(2, las=2)
text( x,-500, letters[1:5])
for (i in seq(0,10000,2000)){
  lines( c(0, max(x)+0.7), c(i,i), lty=2, col="gray80")
}
barplot(as.matrix(data.8.10[c(1:5,10),2:6]),
        axes=FALSE,names.arg = rep("", 5),
        ylim=c(0,10000), density=c(25,20,15,10,5,0), add=TRUE)

legend(max(x)+0.7,8000, rev(LETTERS[1:6]), fill =  rev(gray.colors(6)),angle=45, 
       density = rev(c(25,20,15,10,5,0)),bty="n", cex=1.5, y.intersp = 1)

dev.copy2eps(file="fig810.eps", width=7, height=3.5)


## Tab 8.1
###############################################################################
require(xtable)
table.8.1 <- data.8.1
print(xtable(table.8.1),include.rownames=FALSE )

## Tab 8.2
###############################################################################
require(xtable)
table.8.2 <- data.8.2
print(xtable(table.8.2),include.rownames=FALSE )


## Tab 8.3
###############################################################################
require(xtable)
table.8.3 <- t(data.8.3)
print(xtable(table.8.3),include.rownames=FALSE )

## Tab 8.4
###############################################################################
require(xtable)
table.8.4 <- t(data.8.4)
print(xtable(table.8.4),include.rownames=FALSE )


## Tab 8.6
###############################################################################
require(xtable)
table.8.6 <- t(data.8.6[,2:3])
print(xtable(table.8.6),include.rownames=FALSE )


## Tab 8.7
###############################################################################
require(xtable)
table.8.7 <- data.8.7[,c(3,4,2)]
print(xtable(table.8.7),include.rownames=FALSE )


## Tab 8.8
###############################################################################
require(xtable)


data.8.8 %>%
  filter(type=="Life Ex1") %>%
  dplyr::select(-type) ->
  table.8.8

print(xtable(table.8.8),include.rownames=FALSE )

## Tab 8.9
###############################################################################
require(xtable)
table.8.9 <- data.8.9[,c(2,1,3,4,5,6)]
print(xtable(table.8.9),include.rownames=FALSE )


## Tab 8.10
###############################################################################
require(xtable)
table.8.10 <- data.8.10
print(xtable(table.8.10, digits=c(0,0,0,0,0,0,0)),include.rownames=FALSE)


## Fig 9.1
###############################################################################

data.9.1 <- read.csv("fig91.csv", colClasses = c("numeric","factor",rep("numeric",9)))
table.9.1 <- cbind(x2002.60 = data.9.1[c(1,2,5,7:12),10], 
                   x2013.60 = data.9.1[c(133,134,137,139:144),10],
                   x2002.70 = data.9.1[c(1,2,5,7:12),11],
                   x2013.70 = data.9.1[c(133,134,137,139:144),11])

par(mar=c(1.6, 9.1, 0.1,1), xpd=TRUE)

mp <- barplot(t(as.matrix(table.9.1)),
        space = c(0,rep(c(0,0.3,0,.7),8), 0,0.3,0),
        beside=TRUE, horiz=TRUE,  xlim=c(0,350),
        col=c(redgray[1], redgray[1], "gray50", "gray50"),
        angle=45, density = c(20,10))

lines(c(50, 50), c(0,50), lty=2, col="gray80")
lines(c(100, 100), c(0,50), lty=2, col="gray80")
lines(c(150, 150), c(0,50), lty=2, col="gray80")
lines(c(200, 200), c(0,50), lty=2, col="gray80")
lines(c(250, 250), c(0,50), lty=2, col="gray80")
lines(c(300, 300), c(0,50), lty=2, col="gray80")
lines(c(350, 350), c(0,50), lty=2, col="gray80")
barplot(t(as.matrix(table.9.1)),
        space = c(0,rep(c(0,0.3,0,.7),8), 0,0.3,0),
        beside=TRUE, horiz=TRUE,  xlim=c(0,350),
        col=c(redgray[1], redgray[1], "gray50", "gray50"),
        angle=45, density = c(20,10), add=TRUE, axes=FALSE)
axis(2, las=2, at=colMeans(mp), labels=letters[1:9])

legend(180,37, LETTERS[1:2], fill =  c(redgray[1], redgray[1] ),angle=45, density = c(10,20),
       bty="n", cex=1.2, y.intersp = 1)

legend(180,45, LETTERS[3:4], fill =  c("gray50", "gray50"),angle=45, density = c(10,20),
       bty="n", cex=1.2, y.intersp = 1)

dev.copy2eps(file="fig91.eps", width=7, height=4.2)
par(.oldpar)


## Fig 9.2
###############################################################################

data.9.2 <- read.csv("fig92.csv")

par(mar=c(3.1, 4.1, 0.6,1), xpd=TRUE)
plot(data.9.2[,1], data.9.2[,2], type="l",
     ylim=c(0,80), bty="n", axes=FALSE, xlab="", ylab="")
axis(2, las=2, at=seq(0,80,10))
axis(1)
lines(c(2008, 2015), c(0,0), lty=2, col="gray80")
lines(c(2008, 2015), c(10,10), lty=2, col="gray80")
lines(c(2008, 2015), c(20,20), lty=2, col="gray80")
lines(c(2008, 2015), c(30,30), lty=2, col="gray80")
lines(c(2008, 2015), c(40,40), lty=2, col="gray80")
lines(c(2008, 2015), c(50,50), lty=2, col="gray80")
lines(c(2008, 2015), c(60,60), lty=2, col="gray80")
lines(c(2008, 2015), c(70,70), lty=2, col="gray80")
lines(c(2008, 2015), c(80,80), lty=2, col="gray80")
lines(data.9.2[,1], data.9.2[,2], type="l",
     col="black", lwd=2)

lines(data.9.2[,1], data.9.2[,3], type="l",
      col=redgray[1], lwd=2)


legend(2012,20, LETTERS[1:2], col =  c( "black",redgray[1]),lwd=2,
       bty="n", cex=1.2, y.intersp = 1.5, x.intersp = 0.6)

dev.copy2eps(file="fig92.eps", width=7, height=2.5)
par(.oldpar)

## Fig 9.3
###############################################################################

data.9.3 <- read.csv("fig93.csv")

par(mar=c(3.1, 2.5, 3.1,2.1), xpd=TRUE)

barplot(t(as.matrix(data.9.3[,2:3])), beside=TRUE, col =c("black","gray50"), 
        density=c(15, 10), names.arg = LETTERS[1:7],
        axes=FALSE)
axis(2, las=2)
legend(6,29, letters[1],col =c( "gray50"), fill =  c("gray50"),
       density=c(15,10), bty="n", cex=1.3, x.intersp = 1.2, horiz = TRUE)
legend(2,29, letters[2],col =c("black"), fill =  c("black"),
       density=c(15,10), bty="n", cex=1.3, x.intersp = 1.2, horiz = TRUE)

lines(c(0, 22), c(20,20), lty=2, col="gray80")
lines(c(0, 22), c(10,10), lty=2, col="gray80")
lines(c(0, 22), c(0,0), lty=2, col="gray80")
lines(c(0, 22), c(25,25), lty=2, col="gray80")
lines(c(0, 22), c(15,15), lty=2, col="gray80")
lines(c(0, 22), c(5,5), lty=2, col="gray80")

barplot(t(as.matrix(data.9.3[,2:3])), beside=TRUE, col =c("black","gray50"), 
        density=c(15, 10), names.arg = LETTERS[1:7],
        axes=FALSE, add=TRUE)

# par(mar=c(3.6, 7.1, 2.1,3.6))
# plot( c(1,2,3,4,5,6),c(data.7.1[,2]), type="l", ylim=c(0, 100), col=redgray[1],
#       xaxt="n", lwd=2,xlim=c(1,6.2),
#      axes=FALSE, xlab="", ylab="")
# lines(c(1,2,3,4,5,6),data.7.1[,3], type="l", col="black", lwd=2)
# axis(4, las=2)
# mtext("y", side=4, line=3)
# axis(1, at=seq(1,6, 1), labels=letters[1:6])

dev.copy2eps(file="fig93.eps", width=7, height=3.5)
par(.oldpar)



## Fig 9.4
###############################################################################

data.9.4 <- read.csv("fig94.csv")

par(mar=c(3.1, 2.5, 3.1,2.1), xpd=TRUE)

barplot(t(as.matrix(data.9.4[,2:3])), beside=TRUE, col =c(redgray[1],"black"), 
        density=c(20,20), names.arg = LETTERS[1:7],
        axes=FALSE)
axis(2, las=2)
legend(3.4,90, letters[1],col =c(redgray[1]), fill =  c(redgray[1]),
       density=20, bty="n", cex=1.3, x.intersp = 0.2, horiz = TRUE)
legend(11,90, letters[2],col =c("black"), fill =  c("black"),
       density=20, bty="n", cex=1.3, x.intersp = 0.2, horiz = TRUE)
lines(c(0, 22), c(20,20), lty=2, col="gray80")
lines(c(0, 22), c(40,40), lty=2, col="gray80")
lines(c(0, 22), c(60,60), lty=2, col="gray80")
lines(c(0, 22), c(80,80), lty=2, col="gray80")

barplot(t(as.matrix(data.9.4[,2:3])), beside=TRUE, col =c(redgray[1],"black"), 
        density=c(20,20), names.arg = LETTERS[1:7],
        axes=FALSE, add=TRUE)

dev.copy2eps(file="fig94.eps", width=7, height=3.5)
par(.oldpar)



## Tab 9.1
###############################################################################
require(xtable)
data.9.1 <- read.csv("fig91.csv", colClasses = c("numeric","factor",rep("numeric",9)))
rownames(table.9.1) <- data.9.1[c(1,2,5,7:12),2]
print(xtable(table.9.1, digits=c(0,0,0,0,0)) )



## Tab 9.2
###############################################################################
require(xtable)
data.9.2 <- read.csv("fig92.csv")
print(xtable(t(data.9.2) ))


## Tab 9.3
###############################################################################
require(xtable)
data.9.3 <- read.csv("fig93.csv")
t(as.matrix(data.9.3))
print(xtable(t(data.9.3) ))

## Tab 9.4
###############################################################################
require(xtable)
data.9.4 <- read.csv("fig94.csv")
t(as.matrix(data.9.4))
print(xtable(data.9.4), include.rownames=FALSE )


## Fig extra george
###############################################################################
data.G01<- read.csv("figG01.csv")

require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

par(mar=c(2.7, 2.5, 0.1,4.1), xpd=TRUE)
plot(c(1922, seq(1925,2010,5)), data.G01[,2], typ="l",
     axes= FALSE,
     col= "gray50",
     xlab="", ylab="", lwd=2,
     ylim=c(0,1.2))
lines(c(1922, seq(1925,2010,5)), data.G01[,3], lwd=2, col= redgray[1])

lines(c(1922, 2010), c(0,0), lty=2, col="gray80")
lines(c(1922, 2010), c(0.2,0.2), lty=2, col="gray80")
lines(c(1922, 2010), c(0.4,0.4), lty=2, col="gray80")
lines(c(1922, 2010), c(0.6,0.6), lty=2, col="gray80")
lines(c(1922, 2010), c(0.8,0.8), lty=2, col="gray80")
lines(c(1922, 2010), c(1.0,1.0), lty=2, col="gray80")
lines(c(1922, 2010), c(1.2,1.2), lty=2, col="gray80")

axis(2, las=2)
axis(1, c(1922, seq(1925,2010,5)), labels=FALSE)
text(x=c(1922, seq(1925,2010,5)), y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=LETTERS[1:19], srt=45, adj=1, xpd=TRUE)
text(2013, data.G01[19,2], "a")
text(2013, data.G01[19,3], "b")

dev.copy2eps(file="figG01.eps", width=7, height=3.5)
par(.oldpar)

## Tab extra george
###############################################################################
require(xtable)
xx <- cbind(data.G01[1:10,], rbind(data.G01[11:19,], c(NA, NA)))
print(xtable(xx), include.rownames=FALSE )


## Fig extra ethn
###############################################################################
data.E01<- read.csv("figE01.csv")

require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)

par(mar=c(2.9, 2.9, 0.1,0.5), xpd=FALSE)

plot(data.E01[,3], data.E01[,2],
     ylim=c(5,35),
     xlim=c(7,60),
     axes=FALSE, pch=21, bg=redgray[1],
     xlab="",
     ylab="")
mtext("a", side=1, line=2)
mtext("b", side=2, line=2)
lines(c(10, 10), c(5,35), lty=2, col="gray80")
lines(c(20, 20), c(5,35), lty=2, col="gray80")
lines(c(30, 30), c(5,35), lty=2, col="gray80")
lines(c(40, 40), c(5,35), lty=2, col="gray80")
lines(c(50, 50), c(5,35), lty=2, col="gray80")
lines(c(60, 60), c(5,35), lty=2, col="gray80")
lines(c(5,62), c(10,10), lty=2, col="gray80")
lines(c(5,62), c(20,20), lty=2, col="gray80")
lines(c(5,62), c(30,30), lty=2, col="gray80")

axis(1)
axis(2, las=2)
abline(lm(data.E01[,2] ~ data.E01[,3]))

text(data.E01[,3], data.E01[,2], labels=LETTERS[1:18], cex= 0.7, pos=c(3,4,3,4,3,4,4,2,4,4,3,3,2,2,2,2,1,2))

dev.copy2eps(file="figE01.eps", width=7, height=3.5)
par(.oldpar)

require(xtable)
print(xtable(data.E01), include.rownames=FALSE )


## Fig extra ethn2
###############################################################################
data.E02<- read.csv("figE02.csv")

layout(1)
require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)



par(mar=c(2.1, 11, 2.1,0.5), xpd=TRUE)

barplot(t(as.matrix(data.E02[,2:4])), horiz = TRUE,
        axes=FALSE, las=2, angle=45, density = c(10,20,20),
        col=c("black",  "gray50", redgray[1]),
        xlim=c(0,12), names.arg = LETTERS[1:18])
axis(1)
lines(c(0.0,0.0), c(0, 22), lty=2, col="gray80")
lines(c(2,2), c(0, 22), lty=2, col="gray80")
lines(c(4,4), c(0, 22), lty=2, col="gray80")
lines(c(6,6), c(0, 22), lty=2, col="gray80")
lines(c(8,8), c(0, 22), lty=2, col="gray80")
lines(c(10,10), c(0, 22), lty=2, col="gray80")
lines(c(12,12), c(0, 22), lty=2, col="gray80")
barplot(t(as.matrix(data.E02[,2:4])), horiz = TRUE,
        axes=FALSE, las=2, angle=45, density = c(10,20,20),
        col=c("black",  "gray50", redgray[1]),
        xlim=c(0,12), names.arg = LETTERS[1:18], add=TRUE)

legend(x=0,y=25, letters[1],col =c( "black"), fill =  c("black"),
       density=15, bty="n", cex=1.3, x.intersp = 1.2, horiz = TRUE)

legend(x=4,y=25, letters[3],col =c(  "gray50"), fill =  c( "gray50"),
       density=15, bty="n", cex=1.3, x.intersp = 1.2, horiz = TRUE)

legend(x=8,y=25, letters[5],col =c(  redgray[1]), fill =  c( redgray[1]),
       density=15, bty="n", cex=1.3, x.intersp = 1.2, horiz = TRUE)


dev.copy2eps(file="figE02.eps", width=7, height=5)
par(.oldpar)


data.E02 <- data.E02[order(data.E02[,1]),]
data.E01 <- data.E01[order(data.E01[,1]),]
data.E1n2 <- left_join(data.E01, data.E02, by=c("Ethnic.group"="ethnicity"))
require(xtable)
print(xtable(data.E1n2), include.rownames=FALSE )

###############################################################################
## Fig US01 - understanding society
###############################################################################
data.US01<- read.csv("understanding.soc.csv")

require(dplyr)
require(tidyr)

# remove missing values
# recode new variables
data.US01 <- data.US01 %>%
  filter(a_racel > 0) %>%
  filter(a_sf2a > 0) %>%
  filter(a_racel > 0) %>%
  mutate(agegroup = ifelse(a_age_cr >=75, 4, 
                           ifelse(a_age_cr>=60, 3,
                                  ifelse(a_age_cr >= 40,2,1)))) %>%
  mutate(race_group = ifelse(a_racel ==3 | a_racel ==4, 3,
                             ifelse(a_racel >=5 & a_racel <=8, 4,
                                    ifelse(a_racel ==12 | a_racel ==13, 12,
                                           ifelse(a_racel ==16 | a_racel ==17 | a_racel ==97, 16,
                                                  a_racel))))) %>%
  mutate(limit = ifelse(a_sf2a == 3, 0, 1))
  
## men only, weighted
data.US01.men <-data.US01 %>%
  group_by(agegroup, race_group, a_sex_cr) %>%
  filter(a_sex_cr == 1) %>%
  summarize(prop = sum(limit*a_indpxus_xw)/sum(a_indpxus_xw)) %>%
  spread(agegroup, prop)

## women only, weighted
data.US01.women <-data.US01 %>%
  group_by(agegroup, race_group, a_sex_cr) %>%
  filter(a_sex_cr == 2) %>%
  summarize(prop = sum(limit*a_indpxus_xw)/sum(a_indpxus_xw)) %>%
  spread(agegroup, prop)
# data.US01.women[7,6] <- 0
# 

##plot
###############################################################################

layout(matrix(c(1,2,3),1), widths=c(9,1,9), heights=c(1,1,1))

par(mar=c(3.6, 0.3, 0.1,3), xpd=TRUE)

x <- barplot(as.matrix(-data.US01.men[,3:6]), beside=TRUE,
             axes=FALSE, 
             col=gray.colors(11),
             density=c(rep(seq(25,5,-5),each=2),0), horiz = TRUE,
             names.arg = rep("", 4), space = c(0,3), xlim=c(-1,0),
             main="mm")
lines(c(0,0), c(0,57.1), col="gray80", lty=2)
lines(c(-0.2,-0.2), c(0,57.1), col="gray80", lty=2)
lines(c(-0.4,-0.4), c(0,57.1), col="gray80", lty=2)
lines(c(-0.6,-0.6), c(0,57.1), col="gray80", lty=2)
lines(c(-0.8,-0.8), c(0,57.1), col="gray80", lty=2)
lines(c(-1,-1), c(0,57.1), col="gray80", lty=2)
barplot(as.matrix(-data.US01.men[,3:6]), beside=TRUE,
        axes=FALSE, 
        col=gray.colors(11),
        density=c(rep(seq(25,5,-5),each=2),0), horiz = TRUE,
        names.arg = rep("", 4), space = c(0,3), xlim=c(-1,0),
        add=TRUE)
axis(1, at=seq(0,-1,-0.2), labels=seq(0,1,0.2))
par("usr")

par(mar=c(3.6, 0, 0.1,0), xpd=TRUE)
plot.new()
par("usr")
par(mar=c(3.6, 0, 0.1,0), xpd=TRUE)
t.pos <- x/(58.12-0.88)*1.08-0.052

text( x=.5,y=t.pos, LETTERS[1:11])
text( x=.5, y=mean(c(t.pos[11,1], t.pos[1,2])), "a")

text( x=.5, y=mean(c(t.pos[11,2], t.pos[1,3])), "b")
text( x=.5, y=mean(c(t.pos[11,3], t.pos[1,4])), "c")
text( x=.5, y=1.031, "d")


par(mar=c(3.6, 3, 0.1,0.3), xpd=TRUE)
x <- barplot(as.matrix(data.US01.women[,3:6]), beside=TRUE,
             axes=FALSE, 
             col=gray.colors(11),
             density=c(rep(seq(25,5,-5),each=2),0), horiz = TRUE,
             names.arg = rep("", 4),space = c(0,3), xlim=c(0,1),
             main="ff")
axis(1, at=seq(0,1,0.2))
lines(c(0,0), c(0,57.1), col="gray80", lty=2)
lines(c(0.2,0.2), c(0,57.1), col="gray80", lty=2)
lines(c(0.4,0.4), c(0,57.1), col="gray80", lty=2)
lines(c(0.6,0.6), c(0,57.1), col="gray80", lty=2)
lines(c(0.8,0.8), c(0,57.1), col="gray80", lty=2)
lines(c(1,1), c(0,57.1), col="gray80", lty=2)
barplot(as.matrix(data.US01.women[,3:6]), beside=TRUE,
        axes=FALSE, 
        col=gray.colors(11),
        density=c(rep(seq(25,5,-5),each=2),0), horiz = TRUE,
        names.arg = rep("", 4),space = c(0,3), xlim=c(0,1),
        add=TRUE)

dev.copy2eps(file="figUS01.eps", width=7, height=9.5)
par(.oldpar)

##############################################################################
### TABLE US1 - limited daily
###############################################################################

require(xtable)
print(xtable(cbind(data.US01.men[c(1,3:6)]*100, data.US01.women[3:6]*100)),
      include.rownames = FALSE, digits=2)

##############################################################################
### figure 2 - self rated health poor
###############################################################################
require(tidyr)
data.US02<- read.csv("understanding.soc.csv")
data.US02 <- data.US02 %>%
  filter(a_racel > 0) %>%
  filter(a_sf1 > 0) %>%
  mutate(agegroup = ifelse(a_age_cr >=75, 4, 
                           ifelse(a_age_cr>=60, 3,
                                  ifelse(a_age_cr >= 40,2,1)))) %>%
  mutate(race_group = ifelse(a_racel ==3 | a_racel ==4, 3,
                             ifelse(a_racel >=5 & a_racel <=8, 4,
                                    ifelse(a_racel ==12 | a_racel ==13, 12,
                                           ifelse(a_racel ==16 | a_racel ==17 | a_racel ==97, 16,
                                                  a_racel))))) %>%
  mutate(health = ifelse(a_sf1 == 5, 1, 0))

data.US02 %>%
  filter(a_racel==11,
         a_age_cr>=75)
## men only, weighted
data.US02.men <-data.US02 %>%
  group_by(agegroup, race_group, a_sex_cr) %>%
  filter(a_sex_cr == 1) %>%
  summarize(prop = sum(health*a_indpxus_xw)/sum(a_indpxus_xw)) %>%
  spread(agegroup, prop)

## women only, weighted
data.US02.women <-data.US02 %>%
  group_by(agegroup, race_group, a_sex_cr) %>%
  filter(a_sex_cr == 2) %>%
  summarize(prop = sum(health*a_indpxus_xw)/sum(a_indpxus_xw)) %>%
  spread(agegroup, prop)
# data.US01.women[7,6] <- 0


##plot
###############################################################################

layout(matrix(c(1,2,3),1), widths=c(9,1,9), heights=c(1,1,1))

par(mar=c(3.6, 0.3, 0.1,3), xpd=TRUE)

x <- barplot(as.matrix(-data.US02.men[,3:6]), beside=TRUE,
             axes=FALSE, 
             col=gray.colors(11),
             density=c(rep(seq(25,5,-5),each=2),0), horiz = TRUE,
             names.arg = rep("", 4), space = c(0,3), xlim=c(-1,0),
             main="mm")
lines(c(0,0), c(0,57.1), col="gray80", lty=2)
lines(c(-0.2,-0.2), c(0,57.1), col="gray80", lty=2)
lines(c(-0.4,-0.4), c(0,57.1), col="gray80", lty=2)
lines(c(-0.6,-0.6), c(0,57.1), col="gray80", lty=2)
lines(c(-0.8,-0.8), c(0,57.1), col="gray80", lty=2)
lines(c(-1,-1), c(0,57.1), col="gray80", lty=2)
barplot(as.matrix(-data.US02.men[,3:6]), beside=TRUE,
        axes=FALSE, 
        col=gray.colors(11),
        density=c(rep(seq(25,5,-5),each=2),0), horiz = TRUE,
        names.arg = rep("", 4), space = c(0,3), xlim=c(-1,0),
        add=TRUE)
axis(1, at=seq(0,-1,-0.2), labels=seq(0,1,0.2))
par("usr")

par(mar=c(3.6, 0, 0.1,0), xpd=TRUE)
plot.new()
par("usr")
par(mar=c(3.6, 0, 0.1,0), xpd=TRUE)
t.pos <- x/(58.12-0.88)*1.08-0.052

text( x=.5,y=t.pos, LETTERS[1:11])
text( x=.5, y=mean(c(t.pos[11,1], t.pos[1,2])), "a")

text( x=.5, y=mean(c(t.pos[11,2], t.pos[1,3])), "b")
text( x=.5, y=mean(c(t.pos[11,3], t.pos[1,4])), "c")
text( x=.5, y=1.031, "d")


par(mar=c(3.6, 3, 0.1,0.3), xpd=TRUE)
x <- barplot(as.matrix(data.US02.women[,3:6]), beside=TRUE,
             axes=FALSE, 
             col=gray.colors(11),
             density=c(rep(seq(25,5,-5),each=2),0), horiz = TRUE,
             names.arg = rep("", 4),space = c(0,3), xlim=c(0,1),
             main="ff")
axis(1, at=seq(0,1,0.2))
lines(c(0,0), c(0,57.1), col="gray80", lty=2)
lines(c(0.2,0.2), c(0,57.1), col="gray80", lty=2)
lines(c(0.4,0.4), c(0,57.1), col="gray80", lty=2)
lines(c(0.6,0.6), c(0,57.1), col="gray80", lty=2)
lines(c(0.8,0.8), c(0,57.1), col="gray80", lty=2)
lines(c(1,1), c(0,57.1), col="gray80", lty=2)
barplot(as.matrix(data.US02.women[,3:6]), beside=TRUE,
        axes=FALSE, 
        col=gray.colors(11),
        density=c(rep(seq(25,5,-5),each=2),0), horiz = TRUE,
        names.arg = rep("", 4),space = c(0,3), xlim=c(0,1),
        add=TRUE)

dev.copy2eps(file="figUS02.eps", width=7, height=10)
par(.oldpar)

##############################################################################
### TABLE US2 - self rated health poor
###############################################################################

require(xtable)
print(xtable(cbind(data.US02.men[c(1,3:6)]*100, data.US02.women[3:6]*100)),
      include.rownames = FALSE, digits=2)


##############################################################################
### figure 3 - self rated health income quintile
###############################################################################
layout(1)
data.US03orig<- read.csv("understanding.soc.csv")
require(laeken)

data.US032 <- data.US03orig %>%
  filter(a_racel > 0) %>%
  filter(a_netinc1 >= 0) %>%
  filter(a_age_cr >=60) %>%
  mutate(income.q = ntile(a_netinc1*a_indpxus_xw, 5),
         income.q2=cut(a_netinc1,
                       c(-1, incQuintile(a_netinc1, a_indpxus_xw, k=0:5)[2:6]),
                       labels = FALSE, inlcude.lowest=TRUE)) %>%
  mutate(race_group = ifelse(a_racel ==3 | a_racel ==4, 3,
                             ifelse(a_racel >=5 & a_racel <=8, 4,
                                    ifelse(a_racel ==12 | a_racel ==13, 12,
                                           ifelse(a_racel ==16 | a_racel ==17 | a_racel ==97, 16,
                                                  a_racel))))) %>%
  group_by(race_group) %>%
  mutate(countT = sum(a_indpxus_xw) ) %>%
  group_by(income.q2, add=TRUE) %>%
  mutate(countG = sum(a_indpxus_xw)) %>%
  mutate(per=countG/countT) %>%
  #summarize(G = unique(countG), T=unique(countT), prop = unique(per))%>%
  summarize( per=unique(per)) %>%
  #summarize( total=unique(countT)) %>%
  spread(race_group, per )

##plot
###############################################################################

par(mar=c(2.1, 7, 1.1,0.3), xpd=TRUE)
barplot(as.matrix(data.US032[2:12]), horiz = TRUE,
        col=gray.colors(5), density=c(25,25,15,15,5),
        names.arg=LETTERS[1:11], las=2, axes=FALSE)
lines(c(0,0), c(0,13.5), col="gray80", lty=2)
lines(c(0.2,0.2), c(0,13.5), col="gray80", lty=2)
lines(c(0.4,0.4), c(0,13.51), col="gray80", lty=2)
lines(c(0.6,0.6), c(0,13.5), col="gray80", lty=2)
lines(c(0.8,0.8), c(0,13.5), col="gray80", lty=2)
lines(c(1,1), c(0,13.5), col="gray80", lty=2)
barplot(as.matrix(data.US032[2:12]), horiz = TRUE,
        col=gray.colors(5), density=c(25,25,15,15,5),
        names.arg=LETTERS[1:11], las=2, axes=FALSE, add=TRUE)
text( 0.1, 14,"a")
text( 0.3, 14,"b")
text( 0.5, 14,"c")
text( 0.7, 14,"d")
text( 0.9, 14,"e")
axis(1)
dev.copy2eps(file="figUS03.eps", width=7, height=5)
par(.oldpar)

##############################################################################
### TABLE US2 - self rated health poor
###############################################################################

require(xtable)
print(xtable(t(data.US032[2:12]*100)),
      include.rownames = FALSE, digits=2)



##############################################################################
### figure 4 - home ownership value
###############################################################################
require(Hmisc)
require(dplyr)
require(tidyr)
require(laeken)

# manual csv from e_hhresp.tab including:

data.USW5hh.orig<- read.csv("UsocW5ownership.hh.csv")
## which member(s)are owners
data.USW5hh<- data.USW5hh.orig %>%
  mutate(e_hsowr10 = ifelse(e_hsowr10 ==1, 0, -9), 
         e_hsowr11 = ifelse(e_hsowr11 ==1, 1, -9),
         e_hsowr12 = ifelse(e_hsowr12 ==1, 2, -9),
         e_hsowr13 = ifelse(e_hsowr13 ==1, 3, -9),
         e_hsowr14 = ifelse(e_hsowr14 ==1, 4, -9),
         e_hsowr15 = ifelse(e_hsowr15 ==1, 5, -9),
         e_hsowr16 = ifelse(e_hsowr16 ==1, 6, -9),
         e_hsowr17 = ifelse(e_hsowr17 ==1, 7, -9),
         e_hsowr18 = ifelse(e_hsowr18 ==1, 8, -9),
         e_hsowr19 = ifelse(e_hsowr19 ==1, 9, -9),
         e_hsowr110 = ifelse(e_hsowr110 ==1, 10, -9),
         e_hsowr111 = ifelse(e_hsowr111 ==1, 11, -9),
         e_hsowr112 = ifelse(e_hsowr112 ==1, 12, -9),
         e_hsowr113 = ifelse(e_hsowr113 ==1, 13, -9),
         e_hsowr114 = ifelse(e_hsowr114 ==1, 14, -9),
         e_hsowr115 = ifelse(e_hsowr115 ==1, 15, -9),
         e_hsowr116 = ifelse(e_hsowr116 ==1, 16, -9))

# manual csv from 
data.USW5ind.orig<- read.csv("UsocW5ages.ind.csv")


## take all owned households where at least one member is over 65
## within each region divide into 4 quantiles
## unweighted
data.USW5ind  <- data.USW5ind.orig %>%
  filter(e_age_cr >=65) %>%
  distinct(e_hidp) %>%
  left_join(data.USW5hh) %>%
  filter(e_hsownd == 1 | e_hsownd == 2) %>%
  filter(e_hsval >= 0) %>%
  filter(e_gor_dv >0) %>%
  group_by(e_gor_dv) %>%
  mutate(valueq=cut(e_hsval,
                    c(-1, quantile(e_hsval)[2:6]),
                #c(-1, incQuintile(e_hsval, e_hhdenub_xw, k=0:5)[2:6]),
                labels = FALSE, inlcude.lowest=TRUE)) %>%
  group_by(valueq, add = TRUE) %>%
  dplyr::summarize(meanhv = mean(e_hsval)) %>%
  spread(valueq, meanhv)

par(mar=c(2,4,1,1))
barplot(t(as.matrix(data.USW5ind[,2:5])),
        beside=TRUE, las=2)

## take all owned households where at least one member is over 65
## within each region divide into 4 quantiles
## weighted
data.USW5ind  <- data.USW5ind.orig %>%
  filter(e_age_cr >=65) %>%
  distinct(e_hidp) %>%
  left_join(data.USW5hh.orig) %>%
  filter(e_hsownd == 1 | e_hsownd == 2) %>%
  filter(e_hsval >= 0) %>%
  filter(e_gor_dv >0) %>%
  group_by(e_gor_dv) %>%
  mutate(valueq = cut(e_hsval,
                    wtd.quantile(e_hsval, weights=e_hhdenub_xw,
                                 probs=c(0, .25, .5, .75, 1),
                                 type="i/n"),
                    include.lowest=TRUE,labels = FALSE)) %>%
  group_by(valueq, add = TRUE) %>%
  dplyr::summarize(meanhv = mean(e_hsval)) %>%
  spread(valueq, meanhv)

par(mar=c(4,4,1,1))
barplot(t(as.matrix(data.USW5ind[,2:5])),
        beside=TRUE, las=2, names.arg = groz)
groz <- c("North East",
"North West",
"Yorkshire and the Humber	",
"East Midlands",	
"West Midlands",	
"East of England",	
"London",
"South East",
"South West",
"Wales",
"Scotland",
"Northern Ireland")


## take all over 65, who are (co) owners of the house
## get the 4 quantiles, 25, 40, 75, 95
## weighted
data.USW5ind  <- data.USW5ind.orig %>%
  filter(e_age_cr >=65) %>%
  left_join(data.USW5hh) %>%
  filter(e_hsownd == 1 | e_hsownd == 2) %>%
  filter(e_hsval >= 0) %>%
  filter(e_gor_dv >0) %>%
 mutate(owner=ifelse(e_pno %in% c(e_hsowr11, e_hsowr12,e_hsowr13, e_hsowr14,
                                  e_hsowr15, e_hsowr16, e_hsowr17, e_hsowr18,
                                  e_hsowr19, e_hsowr110, e_hsowr111, e_hsowr112,
                                  e_hsowr113, e_hsowr114, e_hsowr115, e_hsowr116), 1, 0)) %>%
  filter(owner==1) %>%
  group_by(e_gor_dv) %>%
  dplyr::summarize(q25 =  wtd.quantile(e_hsval, probs=0.25),
                   q50 =  wtd.quantile(e_hsval, probs=0.50),
                   q75 =  wtd.quantile(e_hsval, probs=0.75),
                   q95 =  wtd.quantile(e_hsval, probs=0.95))
  




groz <- c("North East",
          "North West",
          "Yorkshire and the Humber	",
          "East Midlands",	
          "West Midlands",	
          "East of England",	
          "London",
          "South East",
          "South West",
          "Wales",
          "Scotland",
          "Northern Ireland")


par(mar=c(4,4,1,1))
barplot(t(as.matrix(data.USW5ind[,2:5])),
        beside=TRUE, las=2, names.arg = groz)



##############################################################################
### EUROSTAT fertility EU chart
###############################################################################

data.eu.fert <- read.csv("eurostatfertilityEU/demo_find_1_Data.csv",
                         colClasses = c("factor","factor", "factor", "numeric", "factor"))
data.ined.fert <- read.csv("eurostatfertilityEU/INEDfertility.csv")
require(tidyr)
require(dplyr)

require(RColorBrewer)
redgray <- colorRampPalette(brewer.pal(9,"RdGy"))(100)


data.eu.fert %>%
  filter(INDIC_DE == "Total fertility rate") %>%
  spread(TIME, Value) %>%
  filter(GEO %in% c("Denmark", "Finland", "Iceland", "Norway",
                    "Sweden", "United Kingdom", "Ireland", "Switzerland",
                    "Austria", "Belgium", "France", "Germany (until 1990 former territory of the FRG)", "Luxembourg",
                    "Netherlands", "Greece", "Italy", "Portugal", "Spain", 
                    "Bulgaria", "Czech Republic", "Estonia",
                    "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia")) ->
  data.eu.fert

# replace missing values with broken/provisional data
data.eu.fert[2,55] <- data.eu.fert[3,55]
data.eu.fert[12,56] <- data.eu.fert[13,56]
data.eu.fert[15,57] <- data.eu.fert[16,57]
data.eu.fert[20,56] <- data.eu.fert[21,56]
data.eu.fert[24,c(44,53)]<- data.eu.fert[25,c(44,53)]
data.eu.fert[32,55] <- data.eu.fert[33,55]

data.eu.fert %>%
  filter (Flag.and.Footnotes == "") %>%
  dplyr::select(-Flag.and.Footnotes, -INDIC_DE) %>%
  dplyr::select(GEO, `2010`,`2011`, `2012`, `2013`) ->
  data.eu.fert

data.ined.fert %>%
  dplyr::select(country,X1960:X2013) %>%
  filter(country %in% c("Denmark", "Finland", "Iceland", "Norway",
                    "Sweden", "United Kingdom", "Ireland", "Switzerland",
                    "Austria", "Belgium", "France", "Germany", "Luxembourg",
                    "Netherlands", "Greece", "Italy", "Portugal", "Spain", 
                    "Bulgaria", "Czech Republic", "Estonia", "Germany  Former Democratic Republic",
                    "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia"))->
  data.ined.fert
data.ined.fert[,1] <- as.character(factor(data.ined.fert[,1]))
data.eu.fert[,1] <- as.character(factor(data.eu.fert[,1]))

data.ined.fert[9,1] <- data.eu.fert[9,1]
# remove DDR after 1990
data.ined.fert[10,32:55] <- NA
fert <- left_join(data.ined.fert, data.eu.fert, by=c("country"= "GEO")) %>%
  mutate(X2010 = ifelse(is.na(X2010), `2010`, X2010),
         X2011 = ifelse(is.na(X2011), `2011`, X2011),
         X2012 = ifelse(is.na(X2012), `2012`, X2012),
         X2013 = ifelse(is.na(X2013), `2013`, X2013)) %>%
  mutate(group = c("gfs", "gfs", "tps", "tps", "de", "tps", 
                   "de", "gfs", "gfs", "tps", "fam", "tps",
                   "de", "lib", "fam", "tps", "tps", "gfs",
                   "gfs", "de", "tps", "fam", "tps", "tps",
                   "tps", "fam", "de", "lib", "lib")) %>%
  group_by(group) %>%
  summarize_each(funs(mean(., na.rm=TRUE)))

par(mar=c(4.1, 4.1, 0.1,0.1), xpd=TRUE)
plot(1960:2013,fert[1,3:56], type="l", ylim=c(1,3.5), lwd=2, col="black",
     axes=FALSE, xlab="", ylab="y")
lines(c(1960,2013), c(1,1), col="gray80", lty=2)
lines(c(1960,2013), c(1.5,1.5), col="gray80", lty=2)
lines(c(1960,2013), c(2,2), col="gray80", lty=2)
lines(c(1960,2013), c(2.5,2.5), col="gray80", lty=2)
lines(c(1960,2013), c(3,3), col="gray80", lty=2)
lines(c(1960,2013), c(3.5,3.5), col="gray80", lty=2)
lines(1960:2013,fert[1,3:56], type="l", lwd=3,col="black")
lines(1960:2013,fert[2,3:56], type="l", lwd=3,col=redgray[1])
lines(1960:2013,fert[3,3:56], type="l", lwd=3,col=redgray[15])
lines(1960:2013,fert[4,3:56], type="l", lwd=3,col=redgray[30])
lines(1960:2013,fert[5,3:56], type="l", lwd=3,col=redgray[80])
axis(1, at=c(seq(1960, 2010, 5), 2013), labels=letters[1:12])
axis(2, las=2)

legend(1980, 3.6, LETTERS[1:5], 
       col=c("black", redgray[30], redgray[15], redgray[80], redgray[1]),
       lwd=3, y.intersp = 1.5, bty="n")

dev.copy2eps(file="figEUfert.eps", width=7.5, height=4.5)
par(.oldpar)
### EUROSTAT fertility EU table
###############################################################################

fert.table <- t(fert[,3:56])[c(seq(1,51,5), 54),]
require(xtable)


print(xtable(fert.table,digits=c(2,2,2,2,2,2)) )


uk.fert.table <- data.1.3[c(seq(1,51,5), 53),2]

## version with UK data as well. 
print(xtable(cbind(uk.fert.table, fert.table),digits=c(2,2,2,2,2,2,2)) )




##############################################################################
### OECD employment rate charts
###############################################################################
require(dplyr)
data.oecd1 <- read.csv("oecdemploy.csv", as.is =c(2:8))
                       str(data.oecd1)

data.oecd1 %>%
  arrange(desc(Year), desc(X60.64)) %>%
  filter(country != "EU21") %>%
  mutate(lab =  c(letters, LETTERS[1:16])) ->
  data.oecd1 


myseq <- c(1:34, 37:44)
par(mar=c(5.6, 3.6, 3.6, 0.1), xpd=TRUE)

plot(myseq,data.oecd1[,2], ylim=c(0,100),
     axes=FALSE, pch=16, col="black", cex=2,
     xlab="", ylab="")
axis(1, at=myseq, labels = c(letters, LETTERS[1:16]))
axis(2, las=2)

#lines(c(14.5, 14.5), c(par("usr")[3], 100), col="gray80", lty=2)
#lines(c(15.5, 15.5), c(par("usr")[3], 100), col="gray80", lty=2)

lines(c(1,44), rep(data.oecd1[1,5],2), col="gray20", lty=2, lwd=2)
lines(c(1,44), rep(data.oecd1[1,6],2), col="pink", lty=2, lwd=2)
lines(c(1,44), rep(data.oecd1[1,7],2), col="gray80", lty=2, lwd=2)

mywidth <- 0.23
for (i in myseq) {
if(i <=35)      {
  rect(i-mywidth, data.oecd1[i,4], i+mywidth, data.oecd1[i,3],
       density=15, col="gray80")
  lines(c(i,i), c(par("usr")[3],data.oecd1[i,4]), col="gray80", lty=2, lwd=1)
}
if(i > 35)       {
  rect(i-mywidth, data.oecd1[i-2,4], i+mywidth, data.oecd1[i-2,3],
       density=15, col="gray80", bg="white")
  lines(c(i,i), c(par("usr")[3], data.oecd1[i-2,4]), col="gray80", lty=2, lwd=1)
}
}
for (i in myseq) {
  if(i <=35)      {
    rect(i-mywidth, data.oecd1[i,3], i+mywidth, data.oecd1[i,2],
         density=15, col="pink")
  }
  if(i > 35)       {
    rect(i-mywidth, data.oecd1[i-2,3], i+mywidth, data.oecd1[i-2,2],
         density=15, col="pink")
  }
}
lines(c(15,15), c(par("usr")[3], data.oecd1[15,4]), col="red", lty=2, lwd=1)

points(myseq,data.oecd1[,2], pch=16, col="black",cex=3)
points(myseq,data.oecd1[,3], pch=16, col="red",cex=3)
points(myseq,data.oecd1[,4], pch=16, col="gray50", cex=3)

legend(20, 125, legend = c("Q", "R", "S"), pch=16, col=c("black", "red", "gray50"), 
       pt.cex=3, bty="n", y.intersp = 1.2, title="V")

legend(30, 115, legend = c("T", "U"),  fill=c("pink", "gray80"),
       density = 15, angle=45, border=c("pink", "gray80"),
       bty="n", y.intersp = 1.2, title = "W")

dev.copy2eps(file="figoecd1.eps", width=20, height=14)


cbind(as.character(data.oecd1$country),data.oecd1[,2]-data.oecd1[,4])
### OECD empl 1 table
###############################################################################

oecd.table <- data.oecd1[,1:4]
require(xtable)

print(xtable(oecd.table,digits=c(0,0,1,1,1)), include.rownames = FALSE)


##############################################################################
### OECD employment rate chart 2
###############################################################################
require(dplyr)
data.oecd2 <- read.csv("oecdemploy2.csv", as.is =c(1:5))
str(data.oecd2)

data.oecd2 %>%
  arrange(group, Difference)

par(mar=c(5.8, 3.6, 1.3,.6))
mp <- barplot(as.matrix(data.oecd2[5]), beside = TRUE, ylim=c(-5,25),
              axes=FALSE, angle=45, density=c(rep(20,18), 0, rep(20,24)), 
              col = c(rep("black", 10), "red", rep("black", 31)), 
              border = c(rep("black", 10), "red", rep("black", 31)),
              space=c(rep(1, 35), 3, rep(1,7)), names.arg="")
axis(2, las=2)
text( y= c(1,1,rep(-1, 41)), x=mp, labels=c(letters, LETTERS[1:17]))

lines(c(0, 89), c(0,0), lty=2, col="gray80")
lines(c(0, 89), c(5,5), lty=2, col="gray80")
lines(c(0, 89), c(10,10), lty=2, col="gray80")
lines(c(0, 89), c(15,15), lty=2, col="gray80")
lines(c(0, 89), c(20,20), lty=2, col="gray80")
lines(c(0, 89), c(25,25), lty=2, col="gray80")
lines(c(0, 89), c(-5,-5), lty=2, col="gray80")

barplot(as.matrix(data.oecd2[5]), beside = TRUE, 
        axes=FALSE, angle=45, density=c(rep(20,18), 0, rep(20,24)), 
        col = c(rep("black", 10), "red", rep("black", 31)), 
        border = c(rep("black", 10), "red", rep("black", 31)),
        space=c(rep(1, 35), 3, rep(1,7)), add=TRUE, names.arg="")

dev.copy2eps(file="figoecd2.eps", width=20, height=10)

### OECD empl 2 table
###############################################################################

oecd.table2 <- data.oecd2[,2:5]
require(xtable)

print(xtable(oecd.table2,digits=c(0,0,2,2,2)), include.rownames = FALSE)

mean(oecd.table2[1:36,3])
### ELSA
###############################################################################
data.elsa <- read.csv("elsa2.csv")
require(dplyr)

data.elsa %>%
  filter(indager>=50, hormvjr >=0) -> over50
colSums(over50)/nrow(over50)  

data.elsa %>%
  filter(indager>=65, hormvjr >=0) -> over65
colSums(over65)/nrow(over65)  

data.elsa %>%
  filter(indager>=75, hormvjr >=0) -> over75
colSums(over75)/nrow(over75)  

data.elsa %>%
  filter(indager>=85, hormvjr >=0) -> over85
colSums(over85)/nrow(over85)  


moving <- rbind(colSums(over50)/nrow(over50),
      colSums(over65)/nrow(over65),
      colSums(over75)/nrow(over75),
      colSums(over85)/nrow(over85)  
   )

moving <- moving[,c(4, 11, 7, 3, 12)]

barplot(moving, beside=TRUE)
