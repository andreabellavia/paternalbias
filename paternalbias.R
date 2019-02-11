
set.seed(123)
library(ggplot2)

# Setting simulations

correct<-c(rep(0),100)
estimated<-c(rep(0),100)
for (i in 1:100) {
  n<-10000
  x_p<-rbinom(n,1,.4)
  x_m<-rnorm(n, 27+5*x_p, 4)
  y <-rnorm(n, 3000+5*x_m+5*x_p,5)
  reg <- glm(y ~ x_m+x_p, family = "gaussian")
  reg_unad <- glm(y ~ x_m, family = "gaussian")
  correct[i]<- summary(reg)$coefficients[2]
  estimated[i]<- summary(reg_unad)$coefficients[2]}
mean(correct)
mean(estimated)

# No interactions
a<-seq(5, 50, by = 5)
correct_res<-c(0)
estimated_res<-c(0)
for (j in a){
  correct<-c(rep(0),100)
  estimated<-c(rep(0),100)
  for (i in 1:100) {
    n<-10000
    x_p<-rbinom(n,1,.4)
    x_m<-rnorm(n, 27+5*x_p, 4)
    y <-rnorm(n, 3000+5*x_m+j*x_p,5)
    reg <- glm(y ~ x_m+x_p, family = "gaussian")
    reg_unad <- glm(y ~ x_m, family = "gaussian")
    correct<- summary(reg)$coefficients[2]
    estimated<- summary(reg_unad)$coefficients[2]}
  correct_res[which(a==j)]<-mean(correct)*5
  estimated_res[which(a==j)]<-mean(estimated)*5
}
correct_res
estimated_res

#plot
df <- data.frame(a,correct_res,estimated_res)

ggplot(df, aes(a)) +                 
  geom_line(aes(y=correct_res), colour="red") +
  geom_line(aes(y=estimated_res), colour="green")

# Interactions
a<-seq(5, 50, by = 5)
correct_res<-c(0)
estimated_res<-c(0)
for (j in a){
  correct<-c(rep(0),100)
  estimated<-c(rep(0),100)
  for (i in 1:100) {
    n<-10000
    x_p<-rbinom(n,1,.4)
    x_m<-rnorm(n, 27+5*x_p, 4)
    y <-rnorm(n, 3000+5*x_m+j*x_p+5+x_m*x_p,5)
    reg <- glm(y ~ x_m+x_p, family = "gaussian")
    reg_unad <- glm(y ~ x_m, family = "gaussian")
    correct<- summary(reg)$coefficients[2]
    estimated<- summary(reg_unad)$coefficients[2]}
  correct_res[which(a==j)]<-mean(correct)*5
  estimated_res[which(a==j)]<-mean(estimated)*5
}
correct_res
estimated_res

