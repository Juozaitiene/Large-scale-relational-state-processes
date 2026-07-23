#--------------------------------functions for covariates---------------------------------
#sender out-degree in state 0
s.0_cal <- function(x, df) {
  t <- which(df$sender == as.numeric(x["sender"]) & df$receiver == as.numeric(x["receiver"]))
  k <- which(df$sender == as.numeric(x["sender"]) & df[,5] == 0)
  k <- k[k!=t]
  return(length(k))
}
#sender out-degree in state 1
s.1_cal <- function(x, df) {
  t <- which(df$sender == as.numeric(x["sender"]) & df$receiver == as.numeric(x["receiver"]))
  k <- which(df$sender == as.numeric(x["sender"]) & df[,5] == 1)
  k <- k[k!=t]
  return(length(k))
}

#row id of the opposite link
find_opposite <- function(x, df) {
  which(df$sender == x["receiver"] & df$receiver == x["sender"])
}

#transitivity in state 0
x.00_cal <- function(x, df) {
  k <- df$sender[which(df$receiver == as.numeric(x["receiver"]) & df[,5] == 0)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$sender == as.numeric(x['sender']) & df$receiver %in% k & df[,5] == 0)
  return(length(temp))
}

#transitivity in state 1
x.11_cal <- function(x, df) {
  k <- df$sender[which(df$receiver == as.numeric(x["receiver"]) & df[,5] == 1)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$sender == as.numeric(x['sender']) & df$receiver %in% k & df[,5] == 1)
  return(length(temp))
}

#receiving balance in state 0
rb.00_cal <- function(x, df) {
  k <- df$sender[which(df$receiver == as.numeric(x["sender"]) & df[,5] == 0)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$receiver == as.numeric(x['receiver']) & df$sender %in% k & df[,5] == 0)
  return(length(temp))
}

#receiving balance in state 1
rb.11_cal <- function(x, df) {
  k <- df$sender[which(df$receiver == as.numeric(x["sender"]) & df[,5] == 1)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$receiver == as.numeric(x['receiver']) & df$sender %in% k & df[,5] == 1)
  return(length(temp))
}


coef <- matrix(0,20,4)
#number of replications
for(z in 1:20)
{

#--------------------------------data simulation---------------------------------
  
#number of nodes
p <- 20

#PARAMETERS:
beta_p_n <- 2
beta_n_n <- 1

beta_p_rec <- 1
beta_n_rec <- 2

beta_p_tc <- -3
beta_n_tc <- -2

info <- cbind(rep(1:p,each=p),rep(1:p,p),0,0,0,0,0,0,0,0,0,0,0)
t <- which(info[,1] == info[,2])
info <- info[-t,]
info <- as.data.frame(info)
colnames(info) <- c("sender","receiver","id","oppo.id","state","s.0","s.1","x.0","x.1","x.00","x.11","rb.00","rb.11")
info$id <- 1:nrow(info)
info$state <- sample(0:1, nrow(info), replace = TRUE)
initial <- info$state
info$s.0 <- apply(info,1,s.0_cal, df = info)
info$s.1 <- apply(info,1,s.1_cal, df = info)
info$x.0 <- ifelse(info$state[apply(info, 1, find_opposite, df = info)] == 0,1,0)
info$x.1 <- ifelse(info$state[apply(info, 1, find_opposite, df = info)] == 1,1,0)
info$x.00 <- apply(info, 1, x.00_cal, df = as.data.frame(info))
info$x.11 <- apply(info, 1, x.11_cal, df = as.data.frame(info))
info$rb.00 <- apply(info, 1, rb.00_cal, df = as.data.frame(info))
info$rb.11 <- apply(info, 1, rb.11_cal, df = as.data.frame(info))

#number of events to simulate
n <- 10000

#event simulation
#starting time
st <- 0
simdat <- as.data.frame(matrix(0,nrow = n, ncol =5))
colnames(simdat) <- c("sender","receiver","time","row_id",'state')

for(i in 1:n)
{
  parameters <- ifelse(info$state==1,
          1*exp(beta_p_n*log(info$s.0+1) + beta_p_rec*info$x.0 + beta_p_tc*log(info$rb.00+1)),
          1*exp(beta_n_n*log(info$s.0+1) + beta_n_rec*info$x.0+ beta_n_tc*log(info$rb.00+1)))
  
  tm<-rexp(1,sum(parameters))
  
  #which link
  link <- rmultinom(1,1, prob = c(parameters)/sum(parameters))
  id <- which(link==1)
  
  st<-st+tm
  #change the state
  info$state[id] <- ifelse(info$state[id]==0,1,0)
  
  #update covariates
  s <- info$sender[id]
  r <- info$receiver[id]
  
  if(info$state[id] == 1)
  {
    #transitivity covariates
    k <- info$sender[which(info$receiver == s & info$state == 1)] 
    k <- k[k != r]
    up_id <- which(info$sender %in% k & info$receiver == r)
    info$x.11[up_id] <- info$x.11[up_id]+1
    
    k <- info$receiver[which(info$sender == r & info$state == 1)] 
    k <- k[k != s]
    up_id <- which(info$sender == s & info$receiver %in% k)
    info$x.11[up_id] <- info$x.11[up_id] + 1
    
    #also check which x00 it affects
    s2 <- info$sender[which(info$receiver == s & info$state == 0)] 
    s2 <- s2[s2 != r]
    up_id2 <- which(info$sender %in% s2 & info$receiver == r)
    info$x.00[up_id2] <- ifelse(info$x.00[up_id2] == 0, 0,info$x.00[up_id2] - 1)
    
    s2 <- info$receiver[which(info$sender == r & info$state == 0)] 
    s2 <- s2[s2 != s]
    up_id2 <- which(info$sender == s  & info$receiver %in% s2)
    info$x.00[up_id2] <- ifelse(info$x.00[up_id2] == 0, 0,info$x.00[up_id2] - 1)
    
    #receiving balance
    k <- info$receiver[which(info$sender == s & info$state == 0)] 
    k <- k[k != r]
    up_id <- which(info$sender %in% k & info$receiver == r)
    info$rb.00[up_id] <- ifelse(info$rb.00[up_id] == 0, 0,info$rb.00[up_id] - 1)
    up_id2 <- which(info$sender == r  & info$receiver %in% k)
    info$rb.00[up_id2] <- ifelse(info$rb.00[up_id2] == 0, 0,info$rb.00[up_id2] - 1)
    
    
    #recip covariates
    k <- which(info$sender == r & info$receiver == s)
    info$x.0[k] <- 0
    
    #sender-receiver covariates
    k <- which(info$sender == s)
    k <- k[k!=id]
    info$s.0[k] <- info$s.0[k] -1
    
  }
  
  if(info$state[id] == 0)
  {
    #transitivity covariates
    #s->k->r we observed a link k -> r
    k <- info$sender[which(info$receiver == s & info$state == 0)] 
    k <- k[k != r]
    up_id <- which(info$sender %in% k & info$receiver == r)
    info$x.00[up_id] <- info$x.00[up_id] + 1
    
    #s->k->r we observed a link s -> k
    k <- info$receiver[which(info$sender == r & info$state == 0)] 
    k <- k[k != s]
    up_id <- which(info$sender == s & info$receiver %in% k)
    info$x.00[up_id] <- info$x.00[up_id] + 1
    
    #also check which x11 it affects
    s2 <- info$sender[which(info$receiver == s & info$state == 1)] 
    s2 <- s2[s2 != r]
    up_id2 <- which(info$sender %in% s2 & info$receiver == r)
    info$x.11[up_id2] <- ifelse(info$x.11[up_id2] == 0, 0,info$x.11[up_id2] - 1)
    
    s2 <- info$receiver[which(info$sender == r & info$state == 1)] 
    s2 <- s2[s2 != s]
    up_id2 <- which(info$sender == s  & info$receiver %in% s2)
    info$x.11[up_id2] <- ifelse(info$x.11[up_id2] == 0, 0,info$x.11[up_id2] - 1)
    
    
    #receiving balance
    k <- info$receiver[which(info$sender == s & info$state == 0)] 
    k <- k[k != r]
    up_id <- which(info$sender %in% k & info$receiver == r)
    info$rb.00[up_id] <- info$rb.00[up_id]+1
    up_id2 <- which(info$sender == r  & info$receiver %in% k)
    info$rb.00[up_id2] <- info$rb.00[up_id2]+1
    
    
    #recip covariate
    k <- which(info$sender == r & info$receiver == s)
    info$x.0[k] <- 1
    
    #sender covariate
    k <- which(info$sender == s)
    k <- k[k!=id]
    info$s.0[k] <- info$s.0[k] + 1
    
  }
  
  simdat[i,] <- c(info[id,1:2],st,info$id[id], info$state[id])
  
}
#--------------------------------transform data to panel data---------------------------------
#number of intervals
m <- 5
d_t <- max(simdat$time)/m
data <- cbind(info$id,0, info$sender, info$receiver,matrix(0,nrow(info),m+1))
colnames(data) <- c('id','oppo_row','sender','receiver',paste0("t", 0:m))
data <- as.data.frame(data)
data[,5:ncol(data)] <- initial

for(i in 1:m)
{
  t <-  which(simdat$time <= d_t*i & simdat$time > d_t*(i-1))
  t_id <- simdat$row_id[t]
  for(j in t_id)
  {
    temp <- subset(simdat[t,], row_id == j, c(time,state))
    data[j,(i+5):(m+5)] <- temp[which.max(temp$time),2]
  }
}

#--------------------------------covariates for panel data---------------------------------
data$oppo_row <- apply(data, 1, find_opposite, df = data)
x0 <- cbind(data[,1:4])
x0 <- cbind(x0,1 - data[data$oppo_row,5:(5+m)])
colnames(x0) <- colnames(data)


x.00 <- cbind(data[,1:4],matrix(0,nrow(data),ncol(data)-4))
x.11 <- cbind(data[,1:4],matrix(0,nrow(data),ncol(data)-4))
rb.00 <- cbind(data[,1:4],matrix(0,nrow(data),ncol(data)-4))
rb.11 <- cbind(data[,1:4],matrix(0,nrow(data),ncol(data)-4))

s.0 <- cbind(data[,1:4],matrix(0,nrow(data),ncol(data)-4))


for(i in 5:ncol(data))
{
  s.0[,i] <- apply(data, 1, s.0_cal, df = as.data.frame(data[,c(1:4,i)]))
  x.00[,i] <- apply(data, 1, x.00_cal, df = as.data.frame(data[,c(1:4,i)]))
  x.11[,i] <- apply(data, 1, x.11_cal, df = as.data.frame(data[,c(1:4,i)]))
  rb.00[,i] <- apply(data, 1, rb.00_cal, df = as.data.frame(data[,c(1:4,i)]))
  rb.11[,i] <- apply(data, 1, rb.11_cal, df = as.data.frame(data[,c(1:4,i)]))
}


dat_Lc1 <- NULL

#find all rows where 0->0
dat_Ln00 <- NULL
#find all rows where 1->1
dat_Ln11 <- NULL
for(i in 6:ncol(data))
{
  t1 <- which((data[,i] - data[,(i-1)])==1)
  dat_Lc1 <- rbind(dat_Lc1,cbind(rep(1,length(t1)),
                                 apply(x.00[t1,(i-1):i],1,mean),
                                 apply(x.11[t1,(i-1):i],1,mean),
                                 apply(rb.00[t1,(i-1):i],1,mean),
                                 apply(rb.11[t1,(i-1):i],1,mean),
                                 apply(s.0[t1,(i-1):i],1,mean),
                                 apply(x0[t1,(i-1):i],1,mean),rep(1,length(t1))))
  
  
  t0 <- which((data[,i] - data[,(i-1)])==-1)
  dat_Lc1 <- rbind(dat_Lc1,cbind(rep(0,length(t0)),
                                 apply(x.00[t0,(i-1):i],1,mean),
                                 apply(x.11[t0,(i-1):i],1,mean),
                                 apply(rb.00[t0,(i-1):i],1,mean),
                                 apply(rb.11[t0,(i-1):i],1,mean),
                                 apply(s.0[t0,(i-1):i],1,mean),
                                 apply(x0[t0,(i-1):i],1,mean),rep(-1,length(t0))))
  
  
  t00 <- which(data[,(i-1)]==0 & data[,i]==0)
  dat_Ln00 <- rbind(dat_Ln00,cbind(rep(0,length(t00)),
                                   apply(x.00[t00,(i-1):i],1,mean),
                                   apply(x.11[t00,(i-1):i],1,mean),
                                   apply(rb.00[t00,(i-1):i],1,mean),
                                   apply(rb.11[t00,(i-1):i],1,mean),
                                   apply(s.0[t00,(i-1):i],1,mean),
                                   apply(x0[t00,(i-1):i],1,mean),rep(1,length(t00))))
  
  
  t11 <- which(data[,(i-1)]==1 & data[,i]==1)
  dat_Ln11 <- rbind(dat_Ln11,cbind(rep(1,length(t11)),
                                   apply(x.00[t11,(i-1):i],1,mean),
                                   apply(x.11[t11,(i-1):i],1,mean),
                                   apply(rb.00[t11,(i-1):i],1,mean),
                                   apply(rb.11[t11,(i-1):i],1,mean),
                                   apply(s.0[t11,(i-1):i],1,mean),
                                   apply(x0[t11,(i-1):i],1,mean),rep(-1,length(t11))))
  
  
}
colnames(dat_Lc1) <- c("state","x.00","x.11","rb.00","rb.11","s.0","x0","ini")

dat_Lc1 <- as.data.frame(dat_Lc1)
colnames(dat_Ln11) <- c("state","x.00","x.11","rb.00","rb.11","s.0","x0","ini")
dat_Ln11 <- as.data.frame(dat_Ln11)
colnames(dat_Ln00) <- c("state","x.00","x.11","rb.00","rb.11","s.0","x0","ini")
dat_Ln00 <- as.data.frame(dat_Ln00)

full <- rbind(dat_Lc1,dat_Ln00, dat_Ln11)

#--------------------------------model fitting---------------------------------
library(mgcv)

fit2 <- glm(state ~ -1+
              log(s.0+1) +
              + x0 + 
              log(rb.00+1)  
            +ini
            ,family=binomial, data = full)
summary(fit2)

coef[z,] <- fit2$coefficients

}
#--------------------------------plotting results---------------------------------
pdf("panel_res2.pdf", height = 8, width = 12)
label=c(expression(Delta*beta[out]), expression(Delta*beta[rec]),
        expression(Delta*beta[rb]),"inertia")


estimates <- c(beta_n_n-beta_p_n,beta_n_rec-beta_p_rec,beta_n_tc-beta_p_tc,0)
boxplot(coef,frame=FALSE, cex.axis = 2, xaxt="n")
axis(1, at = 1:4, labels = label, cex.axis = 2,mgp = c(3, 2, 0))
for (i in seq_along(estimates)) {
  abline(
    h   = estimates[i],
    col = "grey60",
    lwd = 1.5,
    lty = 2   # dashed
  )
}


dev.off()

