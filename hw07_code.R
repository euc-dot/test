rep_ex = read.csv("growth.csv",header=TRUE)


library(ggplot2)

#restructure data from wide to long
rep_ex_long=reshape(data=rep_ex,
                    varying=c('age8','age10','age12','age14'),
                    times=c(8,10,12,14),
                    v.names='growth_measure',
                    direction='long')
rep_ex_long

crep_ex=rep_ex[1:11, ]
crep_ex

Sc=cov(crep_ex[, 1:4])
Sc
Xbarc=cbind((colMeans(crep_ex[, 1:4])))

trep_ex=rep_ex[12:17, ]
trep_ex

St=cov(trep_ex[, 1:4])
St
Xbart=cbind((colMeans(trep_ex[, 1:4])))

Sp=(1/(17-2))*((11-1)*Sc + (6-1)*St)
W=(17-2)*Sp


SpI=solve(Sp)

B1=c(1,8,64,512)
B2=c(1,10,100,1000)
B3=c(1,12,144,1728)
B4=c(1,14,196,2744)

B=rbind(B1,B2,B3,B4)
product=(t(B) %*% SpI %*% B)
productI=solve(product)
productI

(Beta.hatc=productI %*% t(B) %*% SpI %*% Xbarc )
(Beta.hatt=productI %*% t(B) %*% SpI %*% Xbart )


k=((17-2)*(17-2-1))/((17-2-4+2)*(17-2-4+2+1))
((k/11)*productI)
((k/6)*productI)

x=as.matrix(crep_ex[,1:4])
Wc=cbind(c(0,0,0,0),c(0,0,0,0),c(0,0,0,0),c(0,0,0,0))
Wc
for (i in 1:11) {
  Wc= Wc +  (cbind(x[i,]) - B %*% Beta.hatc) %*% 
    t(cbind(x[i,]) - B %*% Beta.hatc)   
}
Wc

x=as.matrix(trep_ex[,1:4])
Wt=cbind(c(0,0,0,0),c(0,0,0,0),c(0,0,0,0),c(0,0,0,0))
Wt
for (i in 1:6) {
  Wt= Wt + (cbind(x[i,]) - B %*% Beta.hatt) %*% 
    t(cbind(x[i,]) - B %*% Beta.hatt)   
}
Wt
(W2=Wc+Wt)

W=(17-2)*Sp

(LAMBDA=det(W)/det(W2))

(-17+((1/2)*(4-2+2)))*log(LAMBDA)

qchisq(0.95, 2, ncp = 0, lower.tail = TRUE, log.p = FALSE)
qchisq(0.99, 2, ncp = 0, lower.tail = TRUE, log.p = FALSE)



plot_obs <- ggplot(data=rep_ex_long,                           #data set
                   aes(x = time, y = growth_measure, group = id)) +    #calling variables
  geom_line() +                               #adding lines to plot
  theme_bw() +                                     #changing style/background
  scale_x_continuous(breaks = 8:14, name = "Age (yr)") +    #creating breaks in the x-axis and labeling the x-axis
  scale_y_continuous(name = "growth measure")               #creating breaks in the y-axis and labeling the y-axis

crep_ex_long=reshape(data=crep_ex,
                    varying=c('age8','age10','age12','age14'),
                    times=c(8,10,12,14),
                    v.names='growth_measure',
                    direction='long')


plot_obsc <- ggplot(data=crep_ex_long,                           #data set
                   aes(x = time, y = growth_measure, group = id)) +    #calling variables
  geom_line() +                               #adding lines to plot
  theme_bw() +                                     #changing style/background
  scale_x_continuous(breaks = 8:14, name = "Age (yr)") +    #creating breaks in the x-axis and labeling the x-axis
  scale_y_continuous(name = "growth measure") 

trep_ex_long=reshape(data=trep_ex,
                    varying=c('age8','age10','age12','age14'),
                    times=c(8,10,12,14),
                    v.names='growth_measure',
                    direction='long')


plot_obst <- ggplot(data=trep_ex_long,                           #data set
                   aes(x = time, y = growth_measure, group = id)) +    #calling variables
  geom_line() +                               #adding lines to plot
  theme_bw() +                                     #changing style/background
  scale_x_continuous(breaks = 8:14, name = "Age (yr)") +    #creating breaks in the x-axis and labeling the x-axis
  scale_y_continuous(name = "growth measure") 

Xbarc
Xbart 

pdf(file="HW07_out.pdf")
#print the object (plot)
print(plot_obs)
print(plot_obsc)
print(plot_obst)

##-----------------------------------------------##
dev.off()
