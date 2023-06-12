
fit2a<-glm(p.total ~ species, family="poisson", data=FishData)
fit2.simple<-glm(p.total ~ 1, family="poisson", data=FishData)
summary(fit2a) #Yes, effect of species IS significant.

# LRT test for significance:
test.stat <- -2*logLik(fit2.simple) - (-2*logLik(fit2a))
p.val <- pchisq(test.stat, df=1, lower.tail=F)

# Avg number of sea lice
b2a<-summary(fit2a)$coefficients
est2a1<-c(b2a[1,1], b2a[1,1]+b2a[2,1])
varc<-b2a[1,2]^2
varp<-b2a[2,2]^2
covcp<-vcov(fit2a)[1,2]
SE.pink<-sqrt(varp+varc+2*covcp)
SE<-c(b2a[1,2], SE.pink)

est2aL<-est2a1-1.96*SE
est2aU<-est2a1+1.96*SE
cbind(est2a1,est2aL,est2aU)

modavgPred(list(fit2a),newdata=data.frame(species=c("chum","pink")))
exp(cbind(est2a1,est2aL,est2aU))


x<-c(1:120)

b2a<-summary(model_treat1)$coef
spec_se<-function(x){
  return(b2a[1,2]^2+(x^2)*b2a[2,2]^2+2*x*vcov(model_treat1)[1,2])
}

spec_se(x)
pred_0<-predict(model_treat1, re.form=NA,newdata =data.frame(treatment=x))
plot(x,back_trans(pred_0),type = "l")
lines(x,back_trans(pred_0+spec_se(x)),col="red")
lines(x,back_trans(pred_0-spec_se(x)),col="blue")
par(mfrow=c(1,1))

plot(x,pred_0,type = "l")
lines(x,(pred_0+spec_se(x)),col="red")
lines(x,back_trans(pred_0-spec_se(x)),col="blue")


spec_se<-function(x){
  return(sqrt(b2a[1,2]^2+(x^2)*b2a[2,2]^2+2*x*vcov(model_treat1)[1,2]))
}

predltemp<-predict(model_tt1,data.frame(treatment=c(1:120),temp=(rep(seq(-2,2),24))), interval='none',re.form=NA)


plot(back_trans(pred0))




pred0<-predict(model_tt1,data.frame(treatment=c(1:120),temp=(rep(0,120))),re.form=NA)
b2a<-summary(model_tt1)$coefficients
spec_se<-function(newdata){
  return(sqrt(b2a[1,2]^2+(newdata[,1]^2)*b2a[2,2]^2+(newdata[,2]^2)*b2a[3,2]^2+2*vcov(model_tt1)[1,2]+2*vcov(model_tt1)[1,3]+2*vcov(model_tt1)[2,3]))
}
SE<-spec_se(data.frame(treatment=c(1:120),temp=(rep(0,120))))

plot(pred0,type="l")
lines(SE+pred0, col="red")
lines(pred0-SE, col="blue")

plot(back_trans(pred0),type="l")
lines(back_trans(SE+pred0), col="red")
lines(back_trans(pred0-SE), col="blue")


spec_se<-function(newdata){
  x1<-newdata[,1]
  x2<-newdata[,2]
  return(sqrt(b2a[1,2]^2+(newdata[,1]^2)*b2a[2,2]^2+(newdata[,2]^2)*b2a[3,2]^2+x1*2*vcov(model_tt1)[1,2]+x2*2*vcov(model_tt1)[1,3]+x1*x2*2*vcov(model_tt1)[2,3]))
}

SE<-spec_se(data.frame(treatment=c(1:120),temp=(rep(0,120))))

plot(pred0,type="l")
lines(1.96*SE+pred0, col="red")
lines(-1.96*SE+pred0, col="blue")

plot(back_trans(pred0),type="l")
lines(back_trans(1.96*SE+pred0), col="red")
lines(back_trans(-1.96*SE+pred0), col="blue")

CI<-modavgPred(list(model_tt1), newdata=data.frame(temp=rep(mu_temp,120),treatment=c(1:120))) 
CI1<-modavgPred(list(model_tt1), newdata=data.frame(temp=rep(max_temp,120),treatment=c(1:120))) 
CI2<-modavgPred(list(model_tt1), newdata=data.frame(temp=rep(min_temp,120),treatment=c(1:120))) 

plot(CI$mod.avg.pred,type="l")
lines(CI$upper.CL, col="red")
lines(CI$lower.CL, col="blue")

plot(CI1$mod.avg.pred,type="l")
lines(CI1$upper.CL, col="red")
lines(CI1$lower.CL, col="blue")

plot(CI2$mod.avg.pred,type="l")
lines(CI2$upper.CL, col="red")
lines(CI2$lower.CL, col="blue")






CI<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(mu_temp,120),treatment=c(1:120))) 
CI1<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(max_temp,120),treatment=c(1:120))) 
CI2<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(min_temp,120),treatment=c(1:120))) 

CI3<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(mu_temp,120),treatment=c(1:120)), type="link") 
CI4<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(max_temp,120),treatment=c(1:120)),type="link") 
CI5<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(min_temp,120),treatment=c(1:120)),type="link") 



library(ggplot2)
colors<-c("Min temp"="blue", 'Mean temp'="black", "Max Temp"="red")
plot_df1<-data.frame(x=1:120,y=seq(-1,3,120/4),min=CI5$mod.avg.pred,mid=CI3$mod.avg.pred,max=CI4$mod.avg.pred)
plot_df<-data.frame(x=1:120,y=seq(0,1,(1/119)),min=CI2$mod.avg.pred,mid=CI$mod.avg.pred,max=CI1$mod.avg.pred)
#plot1<-ggplot(plot_df1, aes(x))+xlim(c(0,115))+ylim(c(-2,3))+geom_line(aes(y=min, ), size = 1)+geom_ribbon(aes(ymin=CI5$lower.CL, ymax=CI5$upper.CL), alpha=0.1, fill = "blue",  color = "black", linetype = "dotted")+geom_line(aes(x, mid),color = "black", size = 1) +geom_line(aes(x, max),color = "red", size = 1) +geom_ribbon(aes(ymin=CI4$lower.CL, ymax=CI4$upper.CL), alpha=0.1, fill = "red",  color = "black", linetype = "dotted")+geom_ribbon(aes(ymin=CI3$lower.CL, ymax=CI3$upper.CL), alpha=0.1, fill = "black",  color = "black", linetype = "dotted")+labs(x="Treatment",y="Logit Ps")
plot2<-ggplot(plot_df, aes(x))+xlim(c(0,115))+ylim(c(0,1))+geom_line(aes(y=min, color="Min temp"), size = 1)+geom_ribbon(aes(ymin=CI2$lower.CL, ymax=CI2$upper.CL), alpha=0.1, fill = "blue",  color = "black", linetype = "dotted")+geom_line(aes(y=mid, color="Mean temp"), size = 1) +geom_line(aes(y=max, color="Max temp"), size = 1) +geom_ribbon(aes(ymin=CI1$lower.CL, ymax=CI1$upper.CL), alpha=0.1, fill = "red",  color = "black", linetype = "dotted")+geom_ribbon(aes(ymin=CI$lower.CL, ymax=CI$upper.CL), alpha=0.1, fill = "green",  color = "black", linetype = "dotted")+labs(x="Treatment",y="Ps", color="Legend")

plot1<-ggplot(plot_df1, aes(x))+xlim(c(0,115))+ylim(c(-5,3))+geom_line(aes(y=min, color="Min temp"), size = 1)+geom_ribbon(aes(ymin=CI5$lower.CL, ymax=CI5$upper.CL), alpha=0.1, fill = "blue",  color = "black", linetype = "dotted")+geom_line(aes(y=mid, colour="Mean temp"), size = 1) +geom_line(aes(y=max, colour="Max temp"), size = 1) +geom_ribbon(aes(ymin=CI4$lower.CL, ymax=CI4$upper.CL), alpha=0.1, fill = "red",  color = "black", linetype = "dotted")+geom_ribbon(aes(ymin=CI3$lower.CL, ymax=CI3$upper.CL), alpha=0.1, fill = "green",  color = "black", linetype = "dotted")+labs(x="Treatment",y="Logit Ps", color="Legend")

setwd(here("figures"))
png(paste(Sys.Date(), "tti_survival_curve.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(2,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot2
dev.off()

setwd(here("figures"))
png(paste(Sys.Date(), "tti_survival_lines.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(2,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot1
dev.off()

library(boot)
b_par<-bootMer(x=model_tti1,FUN=fixef,nsim=200)
boot.ci(b_par,type="perc",index=1)
boot(model_tti1, R=5000)

par(mfrow = c(2,2))
plot(model_tti1)
ggplot(data.frame(eta=predict(model_tti1,type="link"),pearson=residuals(model_tti1,type="pearson")),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()

ggplot(data.frame(x1=model_df_2$treatment,pearson=residuals(model_tti1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()

ggplot(data.frame(x2=model_df_2$temp,pearson=residuals(model_tti1,type="pearson")),
       aes(x=x2,y=pearson)) +
  geom_point() +
  theme_bw()

means <- aggregate(model_df_2[,c("treatment","temp")],by=list(model_df_2$trial_trap),FUN=mean)
lmcoefs <- summary(lm(alive ~ treatment + temp + trial_trap, data=model_df_2))$coefficients[,"Estimate"]
means$effects <- c(0,0,lmcoefs[substr(names(lmcoefs),1,3) == "tri"])
means$effects <- means$effects - mean(means$effects)

cor(means[,c("treatment","temp","effects")])

ggplot(means, aes(x=treatment,y=effects)) +
  geom_point() +
  theme_bw()

ggplot(means, aes(x=temp,y=effects)) +
  geom_point() +
  theme_bw()

means$treatment
qqnorm(residuals(model_length2))


nrow(newdata1)
newdata1<-expand.grid(temp=c(min_temp,mean(model_df_2$temp),max_temp),treatment=c(0,15,30,45,60,75,90,105,120), trial_trap=levels(model_df_2$trial_trap))
newdata1$logit_ps <- predict(model_tti1, newdata = newdata1)
newdata1$ps<-back_trans(newdata1$logit_ps)
newdata1[order(newdata1$treatment,newdata1$temp),]
plot(newdata1$treatment,newdata1$ps)

length(levels(newdata1$trial_trap))
graphics.off()
setwd(here("figures"))
png(paste(Sys.Date(), "min_temp_re.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
ggplot(newdata1[which(newdata1$temp==min_temp),],aes(x=treatment, y=ps, col=trial_trap))+geom_line()#+theme(legend.position = "none") 
dev.off()

setwd(here("figures"))
png(paste(Sys.Date(), "mean_temp_re.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(2,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
ggplot(newdata1[which(newdata1$temp==mean(model_df_2$temp)),],aes(x=treatment, y=ps, col=trial_trap))+geom_line()+theme(legend.position = "none") 
dev.off()

setwd(here("figures"))
png(paste(Sys.Date(), "max_temp_re.png", sep="_"), width=480, height=480, units = "px", pointsize=12)
par(mfrow=c(2,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
ggplot(newdata1[which(newdata1$temp==max_temp),],aes(x=treatment, y=ps, col=trial_trap))+geom_line()+theme(legend.position = "none") 
dev.off()

plot(model_tti1,residuals(.) ~log(fitted(.)))
ggplot(fortify(model_tt1),
       aes(x=.fitted,y=sqrt(abs(.scresid))))+geom_point()+
  geom_smooth(colour="red",alpha=0.3)

dd <- lattice::dotplot(ranef(model_tti1,condVar=TRUE))

do.call(gridExtra::grid.arrange,c(dd,list(nrow=1)))
par(mfrow=c(1,1))
ci_best<-confint(profile(model_tti2))
plot(model_tti1,ylim=c(-3,3),type=c("p","smooth"))
install.packages('report')
report::report(model_tti2)
lme4::qqmath(ranef(model_tti1,condVar=TRUE))



