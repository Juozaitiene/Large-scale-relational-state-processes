#--------------------------------functions for covariates---------------------------------

#sender out-degree
s.1_cal <- function(x, df) {
  t <- which(df$sender == as.numeric(x["sender"]) & df$receiver == as.numeric(x["receiver"]))
  k <- which(df$sender == as.numeric(x["sender"]) & df[,4] == 1)
  k <- k[k!=t]
  return(1)
}

#receiver in-degree/popularity
#calculate how many incoming links in state 1 has receiver
r.1_cal <- function(x, df) {
  t <- which(df$sender == as.numeric(x["sender"]) & df$receiver == as.numeric(x["receiver"]))
  k <- which(df$receiver == as.numeric(x["receiver"]) & df[,4] == 1)
  k <- k[k!=t]
  return(1)
}

#row id of the opposite link
find_opposite <- function(x, df) {
  which(df$sender == x["receiver"] & df$receiver == x["sender"])
}

#transitivity in state 0
x.00_cal <- function(x, df) {
  k <- df$sender[which(df$receiver == as.numeric(x["receiver"]) & df[,4] == 0)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$sender == as.numeric(x['sender']) & df$receiver %in% k & df[,4] == 0)
  return(length(temp))
}

#transitivity in state 1
x.11_cal <- function(x, df) {
  k <- df$sender[which(df$receiver == as.numeric(x["receiver"]) & df[,4] == 1)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$sender == as.numeric(x['sender']) & df$receiver %in% k & df[,4] == 1)
  return(length(temp))
}

#cyclic closure in state 0
c.00_cal <- function(x, df) {
  k <- df$sender[which(df$receiver == as.numeric(x["sender"]) & df[,4] == 0)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$sender == as.numeric(x['receiver']) & df$receiver %in% k & df[,4] == 0)
  return(length(temp))
}

#cyclic closure in state 1
c.11_cal <- function(x, df) {
  k <- df$sender[which(df$receiver == as.numeric(x["sender"]) & df[,4] == 1)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$sender == as.numeric(x['receiver']) & df$receiver %in% k & df[,4] == 1)
  return(length(temp))
}

#sending balance in state 0
sb.00_cal <- function(x, df) {
  k <- df$receiver[which(df$sender == as.numeric(x["sender"]) & df[,4] == 0)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$sender == as.numeric(x['receiver']) & df$receiver %in% k & df[,4] == 0)
  return(length(temp))
}

#sending balance in state 1
sb.11_cal <- function(x, df) {
  k <- df$receiver[which(df$sender == as.numeric(x["sender"]) & df[,4] == 1)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$sender == as.numeric(x['receiver']) & df$receiver %in% k & df[,4] == 1)
  return(length(temp))
}

#receiving balance in state 0
rb.00_cal <- function(x, df) {
  k <- df$sender[which(df$receiver == as.numeric(x["sender"]) & df[,4] == 0)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$receiver == as.numeric(x['receiver']) & df$sender %in% k & df[,4] == 0)
  return(length(temp))
}

#receiving balance in state 1
rb.11_cal <- function(x, df) {
  k <- df$sender[which(df$receiver == as.numeric(x["sender"]) & df[,4] == 1)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$receiver == as.numeric(x['receiver']) & df$sender %in% k & df[,4] == 1)
  return(length(temp))
}

#sender in-degree popularity
#calculate how many incoming links in state 1 has sender
inPop.1_cal <- function(x, df) {
  t <- which(df$sender == as.numeric(x["sender"]) & df$receiver == as.numeric(x["receiver"]))
  k <- which(df$receiver == as.numeric(x["sender"]) & df[,4] == 1)
  k <- k[k!=t]
  return(length(k))
}


#out-degree popularity
#calculate how many outgoing links in state 1 has receiver
outPop.1_cal <- function(x, df) {
  k <- which(df$sender == as.numeric(x["receiver"]) & df[,4] == 1)
  return(length(k))
}

#isolate effect state 0
isolate.0_cal <- function(x, df) {
  k <- which(df$sender == as.numeric(x["sender"]) & df[,4] == 1)
  t <- which(df$receiver == as.numeric(x["sender"]) & df[,4] == 1)
  k <- c(t,k)
  return(ifelse(length(k)==0,1,0))
}

#isolate effect state 1
isolate.1_cal <- function(x, df) {
  k <- which(df$sender == as.numeric(x["sender"]) & df[,4] == 0)
  t <- which(df$receiver == as.numeric(x["sender"]) & df[,4] == 0)
  k <- c(t,k)
  return(ifelse(length(k)==0,1,0))
}

#covariate-ego (egoX)
egoX.1_cal <- function(x, df,cov) {
  t <- which(df$sender == as.numeric(x["sender"]) & df$receiver == as.numeric(x["receiver"]))
  k <- which(df$sender == as.numeric(x["sender"]) & df[,4] == 1)
  k <- k[k!=t]
  return(length(k)*df[[cov]][t])
}

#find number of nodes s can reach in two-steps
dist2_cal <- function(x, df)
{
  t <- df$receiver[which(df$sender == as.numeric(x["sender"]) & df[,4] == 1)]
  k <- df$receiver[which(df$sender %in% t & df[,4] == 1)]
  k <- unique(k)
  k <- setdiff(k,t)
  k <- setdiff(k,x["sender"])
  k <- length(k)

  temp <- which(df$sender == as.numeric(x["sender"]) & df$receiver == as.numeric(x["receiver"]))
  df[temp,4] <- ifelse(df[temp,4] == 0, 1, 0)
  t <- df$receiver[which(df$sender == as.numeric(x["sender"]) & df[,4] == 1)]
  k2 <- df$receiver[which(df$sender %in% t & df[,4] == 1)]
  k2 <- unique(k2)
  k2 <- setdiff(k2,t)
  k2 <- setdiff(k2,x["sender"])
  k2 <- length(k2)
  return(k2-k)
}
#--------------------------------data preparation---------------------------------
#change state 2 to 1 and 10 to 0
#1 - best friend
#2 - just a friend
#10 - at least one of the students was not part of the school cohort
friendship.1[friendship.1 == 2] <- 1
friendship.1[friendship.1 == 10] <- 0
friendship.2[friendship.2 == 2] <- 1
friendship.2[friendship.2 == 10] <- 0
friendship.3[friendship.3 == 2] <- 1
friendship.3[friendship.3 == 10] <- 0

#create a data set of list of pairs
#number of individuals
p <- nrow(friendship.1)
data <- cbind(rep(1:p,each=p),rep(1:p,p))
t <- which(data[,1] == data[,2])
data <- data[-t,]
data <- as.data.frame(data)
colnames(data) <- c("sender","receiver")

data$oppo_row <- apply(data, 1, find_opposite, df = data)
data$t1 <- as.vector(friendship.1)[row(friendship.1) != col(friendship.1)]
data$t2 <- as.vector(friendship.2)[row(friendship.2) != col(friendship.2)]
data$t3 <- as.vector(friendship.3)[row(friendship.3) != col(friendship.3)]

#--------------------------------calculate covariates---------------------------------
#number of intervals 
m <- 2
#The interval between consecutive observations was one year.
d_t <- 1

x0 <- cbind(data[,1:3])
x0 <- cbind(x0,1 - data[data$oppo_row,4:(4+m)])
colnames(x0) <- colnames(data)

x.00 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))
x.11 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))

dist2 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))

r.1 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))
s.1 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))


c.00 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))
c.11 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))

sb.00 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))
sb.11 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))

rb.00 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))
rb.11 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))

inPop.1 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))
outPop.1 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))

isolate.0 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))
isolate.1 <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))

for(i in 4:ncol(data))
{
  s.1[,i] <- apply(data, 1, s.1_cal, df = as.data.frame(data[,c(1:3,i)]))
  r.1[,i] <- apply(data, 1, r.1_cal, df = as.data.frame(data[,c(1:3,i)]))
  x.00[,i] <- apply(data, 1, x.00_cal, df = as.data.frame(data[,c(1:3,i)]))
  x.11[,i] <- apply(data, 1, x.11_cal, df = as.data.frame(data[,c(1:3,i)]))
  
  dist2[,i] <- apply(data, 1, dist2_cal, df = as.data.frame(data[,c(1:3,i)]))

    
  c.00[,i] <- apply(data, 1, c.00_cal, df = as.data.frame(data[,c(1:3,i)]))
  c.11[,i] <- apply(data, 1, c.11_cal, df = as.data.frame(data[,c(1:3,i)]))
  
  sb.00[,i] <- apply(data, 1, sb.00_cal, df = as.data.frame(data[,c(1:3,i)]))
  sb.11[,i] <- apply(data, 1, sb.11_cal, df = as.data.frame(data[,c(1:3,i)]))
  
  rb.00[,i] <- apply(data, 1, rb.00_cal, df = as.data.frame(data[,c(1:3,i)]))
  rb.11[,i] <- apply(data, 1, rb.11_cal, df = as.data.frame(data[,c(1:3,i)]))
  
  inPop.1[,i] <- apply(data, 1, inPop.1_cal, df = as.data.frame(data[,c(1:3,i)]))
  outPop.1[,i] <- apply(data, 1, outPop.1_cal, df = as.data.frame(data[,c(1:3,i)]))
  
  isolate.0[,i] <- apply(data, 1, isolate.0_cal, df = as.data.frame(data[,c(1:3,i)]))
  isolate.1[,i] <- apply(data, 1, isolate.1_cal, df = as.data.frame(data[,c(1:3,i)]))
}


dat_Lc1 <- NULL

#find all rows where 0->0
dat_Ln00 <- NULL
#find all rows where 1->1
dat_Ln11 <- NULL
state_dist <- NA

#Tobacco use has the scores 1 (non), 2 (occasional) and 3 (regular, i.e. more than once per week).
#Cannabis use is coded 1 (non), 2 (tried once), 3 (occasional) and 4 (regular).
#Alcohol consumption is coded as follows: 1 (non), 2 (once or twice a year), 3 (once a month), 4 (once a week) and 5 (more than once a week);

tobacco <- as.data.frame(apply(tobacco,2, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))))
cannabis <- as.data.frame(apply(cannabis,2, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))))
alcohol <- as.data.frame(apply(alcohol,2, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))))

#create data.frame for exo covariates
tobacco_homo <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))
sender_tobacco <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))
receiver_tobacco <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))

cannabis_homo <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))
sender_cannabis <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))
receiver_cannabis <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))

alcohol_homo <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))
sender_alcohol <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))
receiver_alcohol <- cbind(data[,1:3],matrix(0,nrow(data),ncol(data)-3))
  
for(i in 1:3)
{
  tobacco_homo[,i+3] <- abs(as.numeric(tobacco[tobacco_homo$sender,i] -  tobacco[tobacco_homo$receiver,i]))
  sender_tobacco[,i+3] <- as.numeric(tobacco[sender_tobacco$sender,i]==1)
  receiver_tobacco[,i+3] <- as.numeric(tobacco[receiver_tobacco$receiver,i]==1)
  
  cannabis_homo[,i+3] <- abs(as.numeric(cannabis[cannabis_homo$sender,i] -  cannabis[cannabis_homo$receiver,i]))
  sender_cannabis[,i+3] <- as.numeric(cannabis[sender_cannabis$sender,i]==1)
  receiver_cannabis[,i+3] <- as.numeric(cannabis[receiver_cannabis$receiver,i]==1)
  
  alcohol_homo[,i+3] <- abs(as.numeric(alcohol[alcohol_homo$sender,i] ==  alcohol[alcohol_homo$receiver,i]))
  sender_alcohol[,i+3] <- as.numeric(alcohol[sender_alcohol$sender,i]==1)
  receiver_alcohol[,i+3] <- as.numeric(alcohol[receiver_alcohol$receiver,i]==1)
}


for(i in 5:ncol(data))
{
  t1 <- which((data[,i] - data[,(i-1)])==1)
  same_gender <- 2*as.numeric(sex.F[data$sender[t1]] ==  sex.F[data$receiver[t1]])
  sender_F <- 2*as.numeric(sex.F[data$sender[t1]]==2)
  receiver_F <- 2*as.numeric(sex.F[data$receiver[t1]]==2)

  #0 -> 1
  age_diff <- abs(age[data$sender[t1]] - age[data$receiver[t1]])
  dat_Lc1 <- rbind(dat_Lc1,cbind(rep(1,length(t1)),
                                 rep(0,length(t1)), #x00
                                 apply(dist2[t1,(i-1):i],1,mean),
                                 rep(2,length(t1)), #s1
                                 rep(2,length(t1)), #r1
                                 2*apply(1-x0[t1,(i-1):i],1,mean), #reciprocity, if the opposite link is in state 1
                                 rep(0,length(t1)), #c00
                                 2*apply(c.11[t1,(i-1):i],1,mean),
                                 rep(0,length(t1)), #sb00
                                 2*apply(sb.11[t1,(i-1):i],1,mean),
                                 rep(0,length(t1)),#rb00
                                 2*apply(rb.11[t1,(i-1):i],1,mean),
                                 2*apply(inPop.1[t1,(i-1):i],1,mean),
                                 2*apply(outPop.1[t1,(i-1):i],1,mean),
                                 2*apply(isolate.1[t1,(i-1):i],1,mean),
                                 2*apply(isolate.0[t1,(i-1):i],1,mean),
                                 rep(1,length(t1)),
                                 rep(1,length(t1)),same_gender, sender_F,receiver_F,age_diff,
                                 2*apply(tobacco_homo[t1,(i-1):i],1,mean),
                                 2*apply(sender_tobacco[t1,(i-1):i],1,mean),
                                 2*apply(receiver_tobacco[t1,(i-1):i],1,mean),
                                 2*apply(cannabis_homo[t1,(i-1):i],1,mean),
                                 2*apply(sender_cannabis[t1,(i-1):i],1,mean),
                                 2*apply(receiver_cannabis[t1,(i-1):i],1,mean),
                                 2*apply(alcohol_homo[t1,(i-1):i],1,mean),
                                 2*apply(sender_alcohol[t1,(i-1):i],1,mean),
                                 2*apply(receiver_alcohol[t1,(i-1):i],1,mean),
                                 rep(i-4,length(t1))))
  
  #1 -> 0
  t0 <- which((data[,i] - data[,(i-1)])==-1)
  same_gender2 <- 2*as.numeric(sex.F[data$sender[t0]] ==  sex.F[data$receiver[t0]])
  sender_F2 <-2*as.numeric(sex.F[data$sender[t0]]==2)
  receiver_F2 <- 2*as.numeric(sex.F[data$receiver[t0]]==2)
  age_diff2 <- abs(age[data$sender[t0]] - age[data$receiver[t0]])
  
  
  dat_Lc1 <- rbind(dat_Lc1,cbind(rep(0,length(t0)),
                                 rep(0,length(t0)), #x00
                                 apply(dist2[t0,(i-1):i],1,mean),
                                 rep(2,length(t0)), #s1
                                 rep(2,length(t0)), #r1
                                 2*apply(1-x0[t0,(i-1):i],1,mean), #reciprocity, if the opposite link is in state 1
                                 rep(0,length(t0)),#c00
                                 2*apply(c.11[t0,(i-1):i],1,mean),
                                 rep(0,length(t0)),#sb00
                                 2*apply(sb.11[t0,(i-1):i],1,mean),
                                 rep(0,length(t0)),#rb00
                                 2*apply(rb.11[t0,(i-1):i],1,mean),
                                 2*apply(inPop.1[t0,(i-1):i],1,mean),
                                 2*apply(outPop.1[t0,(i-1):i],1,mean),
                                 2*apply(isolate.1[t0,(i-1):i],1,mean),
                                 2*apply(isolate.0[t0,(i-1):i],1,mean),
                                 rep(-1,length(t0)),
                                 rep(1,length(t0)),same_gender2,sender_F2,receiver_F2,age_diff2,
                                 2*apply(tobacco_homo[t0,(i-1):i],1,mean),
                                 2*apply(sender_tobacco[t0,(i-1):i],1,mean),
                                 2*apply(receiver_tobacco[t0,(i-1):i],1,mean),
                                 2*apply(cannabis_homo[t0,(i-1):i],1,mean),
                                 2*apply(sender_cannabis[t0,(i-1):i],1,mean),
                                 2*apply(receiver_cannabis[t0,(i-1):i],1,mean),
                                 2*apply(alcohol_homo[t0,(i-1):i],1,mean),
                                 2*apply(sender_alcohol[t0,(i-1):i],1,mean),
                                 2*apply(receiver_alcohol[t0,(i-1):i],1,mean),
                                 rep(i-4,length(t0))))
  
  #0 -> 0
  t00 <- which(data[,(i-1)]==0 & data[,i]==0)
  same_gender3 <- 2*as.numeric(sex.F[data$sender[t00]] ==  sex.F[data$receiver[t00]])
  sender_F3 <- 2*as.numeric(sex.F[data$sender[t00]]==2)
  receiver_F3 <- 2*as.numeric(sex.F[data$receiver[t00]]==2)
  
  age_diff3 <- abs(age[data$sender[t00]] - age[data$receiver[t00]])
  
  dat_Ln00 <- rbind(dat_Ln00,cbind(rep(0,length(t00)),
                                   rep(0,length(t00)),#x00
                                   apply(dist2[t00,(i-1):i],1,mean),
                                   rep(2,length(t00)), #s1
                                   rep(2,length(t00)), #r1
                                   2*apply(1-x0[t00,(i-1):i],1,mean), #reciprocity, if the opposite link is in state 1
                                   rep(0,length(t00)),#c00
                                   2*apply(c.11[t00,(i-1):i],1,mean),
                                   rep(0,length(t00)),#sb00
                                   2*apply(sb.11[t00,(i-1):i],1,mean),
                                   rep(0,length(t00)),#rb00
                                   2*apply(rb.11[t00,(i-1):i],1,mean),
                                   2*apply(inPop.1[t00,(i-1):i],1,mean),
                                   2*apply(outPop.1[t00,(i-1):i],1,mean),
                                   2*apply(isolate.1[t00,(i-1):i],1,mean),
                                   2*apply(isolate.0[t00,(i-1):i],1,mean),
                                   rep(1,length(t00)),
                                   rep(-1,length(t00)),same_gender3,sender_F3,receiver_F3,age_diff3,
                                   2*apply(tobacco_homo[t00,(i-1):i],1,mean),
                                   2*apply(sender_tobacco[t00,(i-1):i],1,mean),
                                   2*apply(receiver_tobacco[t00,(i-1):i],1,mean),
                                   2*apply(cannabis_homo[t00,(i-1):i],1,mean),
                                   2*apply(sender_cannabis[t00,(i-1):i],1,mean),
                                   2*apply(receiver_cannabis[t00,(i-1):i],1,mean),
                                   2*apply(alcohol_homo[t00,(i-1):i],1,mean),
                                   2*apply(sender_alcohol[t00,(i-1):i],1,mean),
                                   2*apply(receiver_alcohol[t00,(i-1):i],1,mean),
                                   rep(i-4,length(t00))))
  
  #1 -> 1
  t11 <- which(data[,(i-1)]==1 & data[,i]==1)
  same_gender4 <- 2*as.numeric(sex.F[data$sender[t11]] ==  sex.F[data$receiver[t11]])
  sender_F4 <- 2*as.numeric(sex.F[data$sender[t11]]==2)
  receiver_F4 <- 2*as.numeric(sex.F[data$receiver[t11]]==2)
  age_diff4 <- abs(age[data$sender[t11]] - age[data$receiver[t11]])
  
  
  dat_Ln11 <- rbind(dat_Ln11,cbind(rep(1,length(t11)),
                                   rep(0,length(t11)),#x00
                                   apply(dist2[t11,(i-1):i],1,mean),
                                   rep(2,length(t11)), #s1
                                   rep(2,length(t11)), #r1
                                   2*apply(1-x0[t11,(i-1):i],1,mean), #reciprocity, if the opposite link is in state 1
                                   rep(0,length(t11)), #c00
                                   2*apply(c.11[t11,(i-1):i],1,mean),
                                   rep(0,length(t11)), #sb00
                                   2*apply(sb.11[t11,(i-1):i],1,mean),
                                   rep(0,length(t11)),#rb00
                                   2*apply(rb.11[t11,(i-1):i],1,mean),
                                   2*apply(inPop.1[t11,(i-1):i],1,mean),
                                   2*apply(outPop.1[t11,(i-1):i],1,mean),
                                   2*apply(isolate.1[t11,(i-1):i],1,mean),
                                   2*apply(isolate.0[t11,(i-1):i],1,mean),
                                   rep(-1,length(t11)),
                                   rep(-1,length(t11)),same_gender4,sender_F4,receiver_F4,age_diff4,
                                   2*apply(tobacco_homo[t11,(i-1):i],1,mean),
                                   2*apply(sender_tobacco[t11,(i-1):i],1,mean),
                                   2*apply(receiver_tobacco[t11,(i-1):i],1,mean),
                                   2*apply(cannabis_homo[t11,(i-1):i],1,mean),
                                   2*apply(sender_cannabis[t11,(i-1):i],1,mean),
                                   2*apply(receiver_cannabis[t11,(i-1):i],1,mean),
                                   2*apply(alcohol_homo[t11,(i-1):i],1,mean),
                                   2*apply(sender_alcohol[t11,(i-1):i],1,mean),
                                   2*apply(receiver_alcohol[t11,(i-1):i],1,mean),
                                   rep(i-4,length(t11))))
                                  
  state_dist <- rbind(state_dist, c(length(t00),length(t11),length(t0),length(t1)))
  
}
state_dist <-  state_dist[-1,]
colnames(state_dist) <- c("00","11","10","01")
colnames(dat_Lc1) <- c("state","x.00","dist2","s.1","r.1","x0","c.00","c.11","sb.00",
                       "sb.11","rb.00","rb.11","inPop.1","outPop.1","isolate.1","isolate.0",
                       "inertia_new","inertia_old","same_gender", "sender_F", "receiver_F", "age_diff",
                       "tobacco_homo","sender_tobacco","receiver_tobacco","cannabis_homo",
                       "sender_cannabis", "receiver_cannabis","alcohol_homo",
                        "sender_alcohol","receiver_alcohol","wave")
                       
dat_Lc1 <- as.data.frame(dat_Lc1)


colnames(dat_Ln11) <- c("state","x.00","dist2","s.1","r.1","x0","c.00","c.11","sb.00",
                        "sb.11","rb.00","rb.11","inPop.1","outPop.1","isolate.1","isolate.0",
                        "inertia_new","inertia_old","same_gender", "sender_F", "receiver_F", "age_diff",
                        "tobacco_homo","sender_tobacco","receiver_tobacco","cannabis_homo",
                        "sender_cannabis", "receiver_cannabis","alcohol_homo",
                        "sender_alcohol","receiver_alcohol","wave")

dat_Ln11 <- as.data.frame(dat_Ln11)

colnames(dat_Ln00) <- c("state","x.00","dist2","s.1","r.1","x0","c.00","c.11","sb.00",
                        "sb.11","rb.00","rb.11","inPop.1","outPop.1","isolate.1","isolate.0",
                        "inertia_new","inertia_old","same_gender", "sender_F", "receiver_F", "age_diff",
                        "tobacco_homo","sender_tobacco","receiver_tobacco","cannabis_homo",
                        "sender_cannabis", "receiver_cannabis","alcohol_homo",
                        "sender_alcohol","receiver_alcohol","wave")
dat_Ln00 <- as.data.frame(dat_Ln00)

full <- rbind(dat_Lc1,dat_Ln00, dat_Ln11)

#--------------------------------model fitting---------------------------------
library(mgcv)
fit <- gam(state~ -1 + dist2 + s.1 + x0  +
 inertia_new:as.factor(wave) + same_gender+sender_F+receiver_F+
 tobacco_homo+sender_tobacco+receiver_tobacco+cannabis_homo+sender_cannabis+receiver_cannabis+
alcohol_homo + sender_alcohol + receiver_alcohol, family=binomial, data = full)
summary(fit)
