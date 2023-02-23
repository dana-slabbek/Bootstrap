#bootstrap resampling for stomach cancer data
s <- c(25,42,45,46,51,103,124,146,340,396,412,876,1112)
n <- length(s)
lmb_hat_obs <- mean(s)
s_star <- sample(s,size=n,replace=T)
T_star <- replicate(1000,{s_star <- sample(s,size=n,replace=T); (mean(s_star)-lmb_hat_obs)/(sd(s_star)/sqrt(n))}) 
tq <- quantile(T_star, probs=c(0.025,0.975))
mean(s)-tq[2]*sd(s)/sqrt(n) #gives lower bound of CI
mean(s)-tq[1]*sd(s)/sqrt(n) #gives upper bound of CI

#bootstrap resampling for breast cancer data
b <- c(24,40,719,727,791,1166,1235,1581,1804,3460,3808)
n <- length(b)
lmb_hat_obs <- mean(b)
b_star <- sample(b,size=n,replace=T)
T_star <- replicate(1000,{b_star <- sample(b,size=n,replace=T); (mean(b_star)-lmb_hat_obs)/(sd(b_star)/sqrt(n))}) 
tq <- quantile(T_star, probs=c(0.025,0.975))
mean(b)-tq[2]*sd(b)/sqrt(n)  #gives lower bound of CI
mean(b)-tq[1]*sd(b)/sqrt(n)  #gives upper bound of CI
