#--------------------------------functions for covariates---------------------------------

#sender out-degree in state 0
s.0_cal <- function(x, df) {
  t <- which(df$sender == as.numeric(x["sender"]) & df$receiver == as.numeric(x["receiver"]))
  k <- which(df$sender == as.numeric(x["sender"]) & df[,3] == 0)
  k <- k[k!=t]
  return(length(k))
}

#sender out-degree in state 1
s.1_cal <- function(x, df) {
  t <- which(df$sender == as.numeric(x["sender"]) & df$receiver == as.numeric(x["receiver"]))
  k <- which(df$sender == as.numeric(x["sender"]) & df[,3] == 1)
  k <- k[k!=t]
  return(length(k))
}
#sender out-degree in state 2
s.2_cal <- function(x, df) {
  t <- which(df$sender == as.numeric(x["sender"]) & df$receiver == as.numeric(x["receiver"]))
  k <- which(df$sender == as.numeric(x["sender"]) & df[,3] == 2)
  #k <- k[k!=t]
  return(length(k))
}

#row id of the opposite link
find_opposite <- function(x, df) {
  which(df$sender == x["receiver"] & df$receiver == x["sender"])
}

#transitivity in state 0
x.00_cal <- function(x, df) {
  k <- df$sender[which(df$receiver == as.numeric(x["receiver"]) & df[,3] == 0)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$sender == as.numeric(x['sender']) & df$receiver %in% k & df[,3] == 0)
  return(length(temp))
}

#transitivity in state 1
x.11_cal <- function(x, df) {
  k <- df$sender[which(df$receiver == as.numeric(x["receiver"]) & df[,3] == 1)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$sender == as.numeric(x['sender']) & df$receiver %in% k & df[,3] == 1)
  return(length(temp))
}

#transitivity in state 2
x.22_cal <- function(x, df) {
  k <- df$sender[which(df$receiver == as.numeric(x["receiver"]) & df[,3] == 2)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$sender == as.numeric(x['sender']) & df$receiver %in% k & df[,3] == 2)
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
#receiving balance in state 2
rb.22_cal <- function(x, df) {
  k <- df$sender[which(df$receiver == as.numeric(x["sender"]) & df[,5] == 2)]
  k <- k[k != as.numeric(x["receiver"])]
  k <- k[k != as.numeric(x["sender"])]
  temp <- which(df$receiver == as.numeric(x['receiver']) & df$sender %in% k & df[,5] == 1)
  return(length(temp))
}




coef <- matrix(0,20,4)
tran_i <- list()
tran_j <- list()
x_i <- list()
x_j <- list()


#number of replications
for(z in 1:20){
  
#--------------------------------data simulation---------------------------------
  
#number of nodes
p <- 20

#PARAMETERS:
beta_p_n <- 2
beta_n_n <- -2

beta_p_rec <- 1
beta_n_rec <- -1

beta_p_tc <- -1
beta_n_tc <- -0.5


info <- cbind(rep(1:p,each=p),rep(1:p,p),0,0,0,0,0,0,0,0,0,0)
t <- which(info[,1] == info[,2])
info <- info[-t,]
info <- as.data.frame(info)
colnames(info) <- c("sender","receiver","state","s.0","s.1","s.2","x.0","x.1","x.2","x.00","x.11","x.22")
info$state <- sample(0:2, nrow(info), replace = TRUE)
initial <- info$state
info$s.0 <- apply(info,1,s.0_cal, df = info)
info$s.1 <- apply(info,1,s.1_cal, df = info)
info$s.2 <- apply(info,1,s.2_cal, df = info)
info$x.0 <- ifelse(info$state[apply(info, 1, find_opposite, df = info)] == 0,1,0)
info$x.1 <- ifelse(info$state[apply(info, 1, find_opposite, df = info)] == 1,1,0)
info$x.2 <- ifelse(info$state[apply(info, 1, find_opposite, df = info)] == 2,1,0)
info$x.00 <- apply(info, 1, x.00_cal, df = as.data.frame(info))
info$x.11 <- apply(info, 1, x.11_cal, df = as.data.frame(info))
info$x.22 <- apply(info, 1, x.22_cal, df = as.data.frame(info))
info$rb.00 <- apply(info, 1, x.00_cal, df = as.data.frame(info))
info$rb.11 <- apply(info, 1, x.11_cal, df = as.data.frame(info))
info$rb.22 <- apply(info, 1, x.22_cal, df = as.data.frame(info))
info$id <- 1:nrow(info)

#number of events to simulate
n <- 20000

#event simulation
#starting time
st <- 0
simdat <- as.data.frame(matrix(0,nrow = n, ncol =18))
colnames(simdat) <- c("sender","receiver","time","row_id",'prev_state','state',"s.0","s.1","s.2","x.0","x.1","x.2","x.00","x.11","x.22","rb.00","rb.11","rb.22")
non_events <- as.data.frame(matrix(0,nrow = n, ncol =17))
colnames(non_events) <- c("sender","receiver","time","row_id",'state',"s.0","s.1","s.2","x.0","x.1","x.2","x.00","x.11","x.22","rb.00","rb.11","rb.22")

for(i in 1:n)
{
  #rates:
  rates <- matrix(0,nrow(info),3)
  #0->1
  rates[info$state == 0,2] <-  1*exp(beta_p_n*log(info$s.1[info$state == 0]+1) 
                                     + beta_n_n*log(info$s.0[info$state == 0]+1)  
                                     + beta_p_rec*info$x.1[info$state == 0] 
                                     + beta_n_rec*info$x.0[info$state == 0] 
                                     + beta_p_tc*log(info$rb.11[info$state == 0]+1)
                                     + beta_n_tc*log(info$rb.00[info$state == 0]+1))
  #0->2
  rates[info$state == 0,3] <-  1*exp(beta_p_n*log(info$s.2[info$state == 0]+1) 
                                      + beta_n_n*log(info$s.0[info$state == 0]+1)  
                                      + beta_p_rec*info$x.2[info$state == 0] 
                                      + beta_n_rec*info$x.0[info$state == 0] 
                                      + beta_p_tc*log(info$rb.22[info$state == 0]+1)
                                      + beta_n_tc*log(info$rb.00[info$state == 0]+1))
  
  #1->0
  rates[info$state == 1,1] <-  1*exp(beta_p_n*log(info$s.0[info$state == 1]+1) 
                                     + beta_n_n*log(info$s.1[info$state == 1]+1)  
                                     + beta_p_rec*info$x.0[info$state == 1] 
                                     + beta_n_rec*info$x.1[info$state == 1] 
                                     + beta_p_tc*log(info$rb.00[info$state == 1]+1)
                                     + beta_n_tc*log(info$rb.11[info$state == 1]+1))
  #1->2
  rates[info$state == 1,3] <-  1*exp(beta_p_n*log(info$s.2[info$state == 1]+1) 
                                     + beta_n_n*log(info$s.1[info$state == 1]+1)  
                                     + beta_p_rec*info$x.2[info$state == 1] 
                                     + beta_n_rec*info$x.1[info$state == 1] 
                                     + beta_p_tc*log(info$rb.22[info$state == 1]+1)
                                     + beta_n_tc*log(info$rb.11[info$state == 1]+1))
  
  #2->0
  rates[info$state == 2,1] <-  1*exp(beta_p_n*log(info$s.0[info$state == 2]+1) 
                                     + beta_n_n*log(info$s.2[info$state == 2]+1)  
                                     + beta_p_rec*info$x.0[info$state == 2] 
                                     + beta_n_rec*info$x.2[info$state == 2] 
                                     + beta_p_tc*log(info$rb.00[info$state == 2]+1)
                                     + beta_n_tc*log(info$rb.22[info$state == 2]+1))
  #2->1
  rates[info$state == 2,2] <-  1*exp(beta_p_n*log(info$s.1[info$state == 2]+1) 
                                     + beta_n_n*log(info$s.2[info$state == 2]+1)  
                                     + beta_p_rec*info$x.1[info$state == 2] 
                                     + beta_n_rec*info$x.2[info$state == 2] 
                                     + beta_p_tc*log(info$rb.11[info$state == 2]+1)
                                     + beta_n_tc*log(info$rb.22[info$state == 2]+1))
  
  #Sum over all possible transitions per edge
  parameters <- rowSums(rates) 
  tm<-rexp(1,sum(parameters))
  
  #which link happened
  link <- rmultinom(1,1, prob = c(parameters)/sum(parameters))
  id <- which(link==1)
  
  #which state occured
  state_sim <- rmultinom(1,1, prob = rates[id,]/parameters[id])
  state <- which(state_sim == 1)-1
  
  st<-st+tm
  #change the state
  prev_state <-info$state[id]
  info$state[id] <- state
  
  #sample non-event:
  non_id <- sample(info$id[-id],1)
  non_events[i,] <- c(info[non_id,1:2],st,info$id[non_id], info$state[non_id],
                      info[non_id,4:15])
  
  simdat[i,] <- c(info[id,1:2],st,info$id[id],prev_state, info$state[id],info[id,4:15])
  
  #update covariates
  s <- info$sender[id]
  r <- info$receiver[id]
  
  if(info$state[id] == 1)
  {
    if(prev_state == 2){
    #also check which x22 it affects
    s2 <- info$sender[which(info$receiver == s & info$state == 2)] 
    s2 <- s2[s2 != r]
    up_id2 <- which(info$sender %in% s2 & info$receiver == r)
    info$x.22[up_id2] <- ifelse(info$x.22[up_id2] == 0, 0,info$x.22[up_id2] - 1)
    
    s2 <- info$receiver[which(info$sender == r & info$state == 2)] 
    s2 <- s2[s2 != s]
    up_id2 <- which(info$sender == s  & info$receiver %in% s2)
    info$x.22[up_id2] <- ifelse(info$x.22[up_id2] == 0, 0,info$x.22[up_id2] - 1)
    
    k <- which(info$sender == s)
    k <- k[k!=id]
    info$s.2[k] <- info$s.2[k] -1
    
    #receiving balance
    k <- info$receiver[which(info$sender == s & info$state == 2)] 
    k <- k[k != r]
    up_id <- which(info$sender %in% k & info$receiver == r)
    info$rb.22[up_id] <- ifelse(info$rb.22[up_id] == 0, 0,info$rb.22[up_id] - 1)
    up_id2 <- which(info$sender == r  & info$receiver %in% k)
    info$rb.22[up_id2] <- ifelse(info$rb.22[up_id2] == 0, 0,info$rb.22[up_id2] - 1)
    
    
    }
    
    if(prev_state == 0){
      #also check which x00 it affects
      s2 <- info$sender[which(info$receiver == s & info$state == 0)] 
      s2 <- s2[s2 != r]
      up_id2 <- which(info$sender %in% s2 & info$receiver == r)
      info$x.00[up_id2] <- ifelse(info$x.00[up_id2] == 0, 0,info$x.00[up_id2] - 1)
      
      s2 <- info$receiver[which(info$sender == r & info$state == 0)] 
      s2 <- s2[s2 != s]
      up_id2 <- which(info$sender == s  & info$receiver %in% s2)
      info$x.00[up_id2] <- ifelse(info$x.00[up_id2] == 0, 0,info$x.00[up_id2] - 1)
      
      k <- which(info$sender == s)
      k <- k[k!=id]
      info$s.0[k] <- info$s.0[k] -1
      
      #receiving balance
      k <- info$receiver[which(info$sender == s & info$state == 0)] 
      k <- k[k != r]
      up_id <- which(info$sender %in% k & info$receiver == r)
      info$rb.00[up_id] <- ifelse(info$rb.00[up_id] == 0, 0,info$rb.00[up_id] - 1)
      up_id2 <- which(info$sender == r  & info$receiver %in% k)
      info$rb.00[up_id2] <- ifelse(info$rb.00[up_id2] == 0, 0,info$rb.00[up_id2] - 1)
      
      
    }
    #recip covariates
    k <- which(info$sender == r & info$receiver == s)
    info$x.0[k] <- 0
    info$x.1[k] <- 1
    info$x.2[k] <- 0
    
    #sender-receiver covariates
    k <- which(info$sender == s)
    k <- k[k!=id]
    info$s.1[k] <- info$s.1[k] +1
    
    #transitivity covariates
    #s->k->r we observed a link k -> r
    k <- info$sender[which(info$receiver == s & info$state == 1)] 
    k <- k[k != r]
    up_id <- which(info$sender %in% k & info$receiver == r)
    info$x.11[up_id] <- info$x.11[up_id] + 1
    
    #s->k->r we observed a link s -> k
    k <- info$receiver[which(info$sender == r & info$state == 1)] 
    k <- k[k != s]
    up_id <- which(info$sender == s & info$receiver %in% k)
    info$x.11[up_id] <- info$x.11[up_id] + 1
    
    #receiving balance
    k <- info$receiver[which(info$sender == s & info$state == 1)] 
    k <- k[k != r]
    up_id <- which(info$sender %in% k & info$receiver == r)
    info$rb.11[up_id] <- info$rb.11[up_id]+1
    up_id2 <- which(info$sender == r  & info$receiver %in% k)
    info$rb.11[up_id2] <- info$rb.11[up_id2]+1
    
    
    
  }
  
  if(info$state[id] == 0)
  {
    #transitivity covariates
    
    if(prev_state == 2){
    #also check which x22 it affects
    s2 <- info$sender[which(info$receiver == s & info$state == 2)] 
    s2 <- s2[s2 != r]
    up_id2 <- which(info$sender %in% s2 & info$receiver == r)
    info$x.22[up_id2] <- ifelse(info$x.22[up_id2] == 0, 0,info$x.22[up_id2] - 1)
    
    s2 <- info$receiver[which(info$sender == r & info$state == 2)] 
    s2 <- s2[s2 != s]
    up_id2 <- which(info$sender == s  & info$receiver %in% s2)
    info$x.22[up_id2] <- ifelse(info$x.22[up_id2] == 0, 0,info$x.22[up_id2] - 1)
    
    k <- which(info$sender == s)
    k <- k[k!=id]
    info$s.2[k] <- info$s.2[k] - 1
    
    #receiving balance
    k <- info$receiver[which(info$sender == s & info$state == 2)] 
    k <- k[k != r]
    up_id <- which(info$sender %in% k & info$receiver == r)
    info$rb.22[up_id] <- ifelse(info$rb.22[up_id] == 0, 0,info$rb.22[up_id] - 1)
    up_id2 <- which(info$sender == r  & info$receiver %in% k)
    info$rb.22[up_id2] <- ifelse(info$rb.22[up_id2] == 0, 0,info$rb.22[up_id2] - 1)
    
    
    }
    
    if(prev_state == 1){
      
      #also check which x11 it affects
      s2 <- info$sender[which(info$receiver == s & info$state == 1)] 
      s2 <- s2[s2 != r]
      up_id2 <- which(info$sender %in% s2 & info$receiver == r)
      info$x.11[up_id2] <- ifelse(info$x.11[up_id2] == 0, 0,info$x.11[up_id2] - 1)
      
      s2 <- info$receiver[which(info$sender == r & info$state == 1)] 
      s2 <- s2[s2 != s]
      up_id2 <- which(info$sender == s  & info$receiver %in% s2)
      info$x.11[up_id2] <- ifelse(info$x.11[up_id2] == 0, 0,info$x.11[up_id2] - 1)
      
    #sender-receiver covariates
    k <- which(info$sender == s)
    k <- k[k!=id]
    info$s.1[k] <- info$s.1[k] - 1
    
    #receiving balance
    k <- info$receiver[which(info$sender == s & info$state == 1)] 
    k <- k[k != r]
    up_id <- which(info$sender %in% k & info$receiver == r)
    info$rb.11[up_id] <- ifelse(info$rb.11[up_id] == 0, 0,info$rb.11[up_id] - 1)
    up_id2 <- which(info$sender == r  & info$receiver %in% k)
    info$rb.11[up_id2] <- ifelse(info$rb.11[up_id2] == 0, 0,info$rb.11[up_id2] - 1)
    
    
    }
    
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
    
    
    #recip covariates
    k <- which(info$sender == r & info$receiver == s)
    info$x.0[k] <- 1
    info$x.1[k] <- 0
    info$x.2[k] <- 0
    
    #sender-receiver covariates
    k <- which(info$sender == s)
    k <- k[k!=id]
    info$s.0[k] <- info$s.0[k] +1
    
    #receiving balance
    k <- info$receiver[which(info$sender == s & info$state == 0)] 
    k <- k[k != r]
    up_id <- which(info$sender %in% k & info$receiver == r)
    info$rb.00[up_id] <- info$rb.00[up_id]+1
    up_id2 <- which(info$sender == r  & info$receiver %in% k)
    info$rb.00[up_id2] <- info$rb.00[up_id2]+1
    
  }
  
  if(info$state[id] == 2)
  {
    #transitivity covariates
    k <- info$sender[which(info$receiver == s & info$state == 2)] 
    k <- k[k != r]
    up_id <- which(info$sender %in% k & info$receiver == r)
    info$x.22[up_id] <- info$x.22[up_id]+1
  
    k <- info$receiver[which(info$sender == r & info$state == 2)] 
    k <- k[k != s]
    up_id <- which(info$sender == s & info$receiver %in% k)
    info$x.22[up_id] <- info$x.22[up_id] + 1
    
    #recip covariates
    k <- which(info$sender == r & info$receiver == s)
    info$x.2[k] <- 1
    info$x.1[k] <- 0
    info$x.0[k] <- 0
    
    #sender-receiver covariates
    k <- which(info$sender == s)
    k <- k[k!=id]
    info$s.2[k] <- info$s.2[k] +1
    
    #receiving balance
    k <- info$receiver[which(info$sender == s & info$state == 2)] 
    k <- k[k != r]
    up_id <- which(info$sender %in% k & info$receiver == r)
    info$rb.22[up_id] <- info$rb.22[up_id]+1
    up_id2 <- which(info$sender == r  & info$receiver %in% k)
    info$rb.22[up_id2] <- info$rb.22[up_id2]+1
    
    
    
    if(prev_state == 1){
    #sender-receiver covariates
    k <- which(info$sender == s)
    k <- k[k!=id]
    info$s.1[k] <- info$s.1[k] - 1
    
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
    k <- info$receiver[which(info$sender == s & info$state == 1)] 
    k <- k[k != r]
    up_id <- which(info$sender %in% k & info$receiver == r)
    info$rb.11[up_id] <- ifelse(info$rb.11[up_id] == 0, 0,info$rb.11[up_id] - 1)
    up_id2 <- which(info$sender == r  & info$receiver %in% k)
    info$rb.11[up_id2] <- ifelse(info$rb.11[up_id2] == 0, 0,info$rb.11[up_id2] - 1)
    
    }
    
    if(prev_state == 0){
      #sender-receiver covariates
      k <- which(info$sender == s)
      k <- k[k!=id]
      info$s.0[k] <- info$s.0[k] - 1
      
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
      
    }
  }
  
}

#--------------------------------sampling of non-transition---------------------------------
non_events$state_up <- sapply(non_events$state, function(x) {
  sample(setdiff(0:2, x), 1)})

data <- as.data.frame(matrix(0,nrow(simdat),7))
colnames(data) <- c("y","node_n","node_p","recip_n","recip_p","triad_n",
                    "triad_p")
data$y <- 1
data$node_n <- log(simdat[cbind(seq_len(nrow(simdat)), match(paste0("s.", simdat$prev_state), names(simdat)))]+1) - log(non_events[cbind(seq_len(nrow(simdat)), match(paste0("s.", non_events$state), names(non_events)))]+1)
data$node_p <- log(simdat[cbind(seq_len(nrow(simdat)), match(paste0("s.", simdat$state), names(simdat)))]+1) - log(non_events[cbind(seq_len(nrow(simdat)), match(paste0("s.", non_events$state_up), names(non_events)))]+1)

data$recip_n <- simdat[cbind(seq_len(nrow(simdat)), match(paste0("x.", simdat$prev_state), names(simdat)))] - non_events[cbind(seq_len(nrow(simdat)), match(paste0("x.", non_events$state), names(non_events)))]
data$recip_p <- simdat[cbind(seq_len(nrow(simdat)), match(paste0("x.", simdat$state), names(simdat)))] - non_events[cbind(seq_len(nrow(simdat)), match(paste0("x.", non_events$state_up), names(non_events)))]

data$triad_n <- log(simdat[cbind(seq_len(nrow(simdat)), match(paste0("x.", sprintf("%02d", simdat$prev_state * 11)), names(simdat)))]+1) - log(non_events[cbind(seq_len(nrow(simdat)), match(paste0("x.", sprintf("%02d", non_events$state * 11)), names(non_events)))]+1)
data$triad_p <- log(simdat[cbind(seq_len(nrow(simdat)), match(paste0("x.", sprintf("%02d", simdat$state * 11)), names(simdat)))]+1) - log(non_events[cbind(seq_len(nrow(simdat)), match(paste0("x.", sprintf("%02d", non_events$state_up* 11)), names(non_events)))]+1)

#previous state
triad_i<-cbind(simdat[cbind(seq_len(nrow(simdat)), match(paste0("rb.", sprintf("%02d", simdat$prev_state * 11)), names(simdat)))],
              non_events[cbind(seq_len(nrow(simdat)), match(paste0("rb.", sprintf("%02d", non_events$state * 11)), names(non_events)))])
#next state
triad_j<-cbind(simdat[cbind(seq_len(nrow(simdat)), match(paste0("rb.", sprintf("%02d", simdat$state * 11)), names(simdat)))],
               non_events[cbind(seq_len(nrow(simdat)), match(paste0("rb.", sprintf("%02d", non_events$state_up* 11)), names(non_events)))])

id.mat<-cbind(rep(1, nrow(simdat)),rep(-1, nrow(simdat)))


#--------------------------------model fitting---------------------------------
library(mgcv)
fit2 <- gam(y ~ -1 + node_n+ node_p + recip_n + recip_p
            + s(triad_i, by=id.mat, k= 5)+
              s(triad_j, by=id.mat, k= 5),family=binomial, data = data)
summary(fit2)
#--------------------------------estimates---------------------------------

nt<-200
x <-seq(min(c(triad_i,triad_j)),max(c(triad_i,triad_j)),length=nt)
ndat<-data.frame(triad_i=x,triad_j=x,id.mat=rep(1,nt), node_n=rep(0,nt),node_p=rep(0,nt),
                 recip_n=rep(0,nt), recip_p=rep(0,nt))
pred<-predict(fit2,newdata = ndat,type="terms",se.fit = T)
plot(x, pred[[1]][,5],type="l", ylim = c(-10,10))
lines(x, beta_n_tc*log(x+1)-mean(beta_n_tc*log(x+1)),  col="red")
plot(x, pred[[1]][,6],type="l", ylim = c(-10,10))
lines(x, beta_p_tc*log(x+1)-mean(beta_p_tc*log(x+1)),  col="red")

t1 <- which(x > max(c(triad_i)))
t2 <- which(x > max(c(triad_j)))

tran_i[[z]] <- if(length(t1)==0) pred[[1]][,5] else pred[[1]][-t1,5]
tran_j[[z]] <- if(length(t2)==0) pred[[1]][,6] else pred[[1]][-t2,6]
x_i[[z]] <- if(length(t1)==0) x else x[-t1]
x_j[[z]] <- if(length(t2)==0) x else x[-t2]
coef[z,] <- fit2$coefficients[1:4]
}

estimates <- c(beta_n_n,beta_p_n,beta_n_rec,beta_p_rec)  
boxplot(coef)
for (i in seq_along(estimates)) {
  segments(
    x0 = i - 0.3, x1 = i + 0.3,
    y0 = estimates[i], y1 = estimates[i],
    col = "red", lwd = 2
  )
}

#--------------------------------plotting results---------------------------------
pdf("full_res.pdf", height = 8, width = 12)
label=c(expression(beta[out]^{"-"}), expression(beta[out]^{"+"}),
        expression(beta[rec]^{"-"}), expression(beta[rec]^{"+"})
)
estimates <- c(beta_n_n,beta_p_n,beta_n_rec,beta_p_rec)  
 boxplot(coef,frame=FALSE, cex.axis = 2, xaxt="n")
 axis(1, at = 1:length(estimates), labels = label, cex.axis = 2,mgp = c(3, 2, 0))
 for (i in seq_along(estimates)) {
   abline(
     h   = estimates[i],
     col = "grey60",
     lwd = 1.5,
     lty = 2   # dashed
   )
 }
 
 
dev.off()

pdf("full_rb.pdf", height = 8, width = 12)
col_grey <- adjustcolor("grey20", alpha.f = 0.35)
col_true <- "red3"

# --- Helper function for a single panel ---
plot_rb_panel <- function(x_list, y_list, beta_tc,
                          xlim = c(0, 12), ylim = c(-1, 2),
                          main = "", xlab = "", ylab = "",
                          cex_axis = 1.5, cex_lab = 1.8, cex_main = 1.5,
                          legend_cex = 1.3) {
  
  # Initial plot (draw the first curve to establish the axis limits)
  plot(x_list[[1]], y_list[[1]],
       type = "l", col = col_grey, lwd = 1,
       xlim = xlim, ylim = ylim,
       xaxs = "i", yaxs = "i",
       xlab = xlab, ylab = ylab, main = main,
       cex.lab = cex_lab, cex.main = cex_main,
       frame.plot = FALSE, axes = FALSE)
  
  # axes and frame
  axis(1, cex.axis = cex_axis)
  axis(2, las = 1, cex.axis = cex_axis)
  box(bty = "l")
  
  # add other curves
  if (length(x_list) > 1) {
    for (i in 2:length(x_list)) {
      lines(x_list[[i]], y_list[[i]], type = "l", col = col_grey, lwd = 1)
    }
  }
  
  # actual curve
  x_ref <- x_list[[1]]
  true_curve <- beta_tc * log(x_ref + 1)
  true_curve <- true_curve - mean(true_curve, na.rm = TRUE)
  lines(x_ref, true_curve, col = col_true, lwd = 2.5)
  
  
  legend("topleft",
         legend = c("Estimated", "True"),
         lwd = c(2.5, 2.5), col = c(col_grey, col_true),
         bty = "n", cex = legend_cex, inset = 0.02)
}

# --- plotmath labels ---
xlab_i <- expression(Rb[sr]^(i))
xlab_j <- expression(Rb[sr]^(j))
ylab_i <- expression( paste("f"^"-", "(Rb"[sr]^(i),")")) 
ylab_j <- expression( paste("f"^"+", "(Rb"[sr]^(j),")")) 
#ylab_j <- expression( f^+(Rb[sr]^(j)) )

# --- Layout and margins for larger labels ---
op <- par(mfrow = c(1, 2), mar = c(5, 5, 2, 1), mgp = c(2.6, 0.9, 0))
on.exit(par(op), add = TRUE)



# left panel: current state i
plot_rb_panel(
  x_list = x_i, y_list = tran_i, beta_tc = beta_n_tc,
  main = "Receiving balance (current state i)",
  xlab = xlab_i, ylab = ylab_i,
  cex_axis = 1.5, cex_lab = 1.8, cex_main = 1.5
)

# right panel: target state j
plot_rb_panel(
  x_list = x_j, y_list = tran_j, beta_tc = beta_p_tc,
  main = "Receiving balance (target state j)",
  xlab = xlab_j, ylab = ylab_j,
  cex_axis = 1.5, cex_lab = 1.8, cex_main = 1.5
)
dev.off()
 
