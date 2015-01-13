# example1-rdd.R
# Bastiaan Quast
# bquast@gmail.com

require("foreign") #For Stata data
require("dummies")

setwd("C:/Users/drewdim/Dropbox/rd/paper/dalbodalbo")
d<-read.dta("PoliticalDynastiesData.dta")

#Set up data as per ``Political Dynasties''
reg <- data.frame(dummy(d$region))
dec <- data.frame(dummy(d$decade))
reg<-reg[,-1]
dec<-dec[,-c(1,10,20,21,22)]
nreg<-names(reg)
ndec<-names(dec)
d<-cbind(d,reg)
d<-cbind(d,dec)
rm(list=c("dec","reg"))

covs<-c(nreg,ndec,"democrat","republican","female","collegeatt",
        "outsider","preanyoffice","age","military","farmer",
        "lawyer","business")
sub<-d$year==d$yearenter & d$realbirthyear<=1910 & 
  d$diedinoffice==0 & d$prerelative==0

# Simple RD estimate
rd<-RDestimate(postrelative~marginvote+longterm,data=d)

# Full RD estimate
rd<-RDestimate(
  paste("postrelative~marginvote+longterm|",paste(covs,collapse="+"),sep=""),
  data=d,
  subset=sub)

summary(rd)
plot(rd)

# With different bandwidths
rd25 <- RDestimate(
  paste("postrelative~marginvote+longterm|",paste(covs,collapse="+"),sep=""),
  data=d,
  subset=sub,
  bw=2.5)

rd5 <- RDestimate(
  paste("postrelative~marginvote+longterm|",paste(covs,collapse="+"),sep=""),
  data=d,
  subset=sub,
  bw=5)

rd10 <- RDestimate(
  paste("postrelative~marginvote+longterm|",paste(covs,collapse="+"),sep=""),
  data=d,
  subset=sub,
  bw=10)

# Bind the output into a table
rbind(rd.default=rd[c language="("est","se","z","p")"][/c],
      rd2.5=rd25[c language="("est","se","z","p")"][/c],
      rd5=rd5[c language="("est","se","z","p")"][/c], 
      rd10=rd10[c language="("est","se","z","p")"][/c])

# Plot the discontinuity
par(pty="m")
pdf("PD_example_1.pdf",width=7.5,height=5)
plot(rd,which=1)
title(xlab="Vote Margin (%)",ylab="Proportion with Post-relative")
dev.off()

pdf("PD_example_2.pdf",width=7.5,height=5)
plot(rd,which=2)
title(xlab="Vote Margin (%)",ylab="Proportion Long term")
dev.off()

pdf("PD_example_3.pdf",width=7.5,height=5)
DCdensity(d$marginvote[sub])
title(xlab="Vote Margin (%)",ylab=''Density'')
dev.off()