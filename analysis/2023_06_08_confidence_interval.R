library(ggplot2)
library(AICcmodavg)
library(here)

setwd(here("data-clean"))

survival<-read.csv("2023-05-09_prawn_combined_survival_data.csv")
trial<-read.csv("2023-05-09_prawn_combined_trial_data.csv")

trial<-trial[order(trial$trial_number),]
temp<-trial$exp_set_temp_air

model_df<-survival[(is.na(survival$treatment)==FALSE),]
model_df$treatment<-as.integer(model_df$treatment)
model_df<-model_df[order(model_df$trial_number,model_df$prawn_id),]
model_df$trial_trap<-paste(model_df$trial_number,"-",model_df$trap_number)

model_df_1<-model_df[c(-564,-1255),]
model_df_1$temp<-rep(0, nrow(model_df_1))
n_trials<-nrow(trial)

for(i in 1:n_trials){
  if(i<11){
    model_df_1[which(model_df_1$trial_number==i),]$temp<-rep(temp[i],length(which(model_df_1$trial_number==i)))
  }
  if(i>=11){
    model_df_1[which(model_df_1$trial_number==i+2),]$temp<-rep(temp[i],length(which(model_df_1$trial_number==i+2)))
  }
}


model_df_1$trial_trap<-as.factor(model_df_1$trial_trap)
model_df_1$length<-round(model_df_1$length/0.5)*0.5

model_df_1
model_df_2<-model_df_1[is.na(model_df_1$length)==FALSE,]

q25<-quantile(model_df_2$temp,0.25)
q75<-quantile(model_df_2$temp,0.75)

model_df_cold<-subset(model_df_2, model_df_2$temp<=q25)
model_df_mean_1<-subset(model_df_2, q25<model_df_2$temp)
model_df_mean<-subset(model_df_mean_1, q75>model_df_mean_1$temp)
model_df_hot<- subset(model_df_2, model_df_2$temp>=q75)

min_temp<-min(trial$exp_set_temp_air)
mu_temp<-mean(trial$exp_set_temp_air)
max_temp<-max(trial$exp_set_temp_air)


CI<-AICcmodavg::modavgPred(list(model_tti1), newdata=data.frame(temp=rep(mu_temp,120),treatment=c(1:120))) 
CI1<-AICcmodavg::modavgPred(list(model_tti1), newdata=data.frame(temp=rep(max_temp,120),treatment=c(1:120))) 
CI2<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(min_temp,120),treatment=c(1:120))) 

CI3<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(mu_temp,120),treatment=c(1:120)), type="link") 
CI4<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(max_temp,120),treatment=c(1:120)),type="link") 
CI5<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(min_temp,120),treatment=c(1:120)),type="link") 

colors<-c("Min temp"="black", 'Mean temp'="purple", "Max temp"="hotpink")
plot_df1<-data.frame(x=1:120,min=CI5$mod.avg.pred,mid=CI3$mod.avg.pred,max=CI4$mod.avg.pred)
plot_df<-data.frame(x=1:120,min=CI2$mod.avg.pred,mid=CI$mod.avg.pred,max=CI1$mod.avg.pred)

plot1<-ggplot(plot_df1, aes(x=x))+xlim(c(5,120))+ylim(c(-7,3))+geom_line(aes(y=min, color="Min temp"), size = 1)+
  geom_ribbon(aes(ymin=CI5$lower.CL, ymax=CI5$upper.CL), alpha=0.3, fill = "black",  color = "black", linetype = "dotted")+
  geom_line(aes(y=mid, colour="Mean temp"), size = 1) +geom_line(aes(y=max, colour="Max temp"), size = 1) +
  geom_ribbon(aes(ymin=CI4$lower.CL, ymax=CI4$upper.CL), alpha=0.3, fill = "hotpink",  color = "black", linetype = "dotted")+
  geom_ribbon(aes(ymin=CI3$lower.CL, ymax=CI3$upper.CL), alpha=0.3, fill = "purple",  color = "black", linetype = "dotted")+
  labs(x="Treatment",y="Logit Ps", color="Legend")+scale_color_manual(values = colors)+
  theme(panel.grid.major.y = element_line(color="grey"), panel.border=element_rect(fill=NA),panel.background = element_rect(fill = "white", colour = "grey50"))+
  geom_jitter(data=model_df_2,aes(x=treatment,y=trans(alive)), size=0.2, height = 0.03,width=3)

plot2<-ggplot(plot_df, aes(x=x))+geom_line(aes(y=min, color="Min temp"), size = 1)+
  geom_ribbon(aes(ymin=CI2$lower.CL, ymax=CI2$upper.CL), alpha=0.5, fill = "black",  color = "black", linetype = "dotted")+
  geom_line(aes(y=mid, color="Mean temp"), size = 1) +geom_line(aes(y=max, color="Max temp"), size = 1) +
  geom_ribbon(aes(ymin=CI1$lower.CL, ymax=CI1$upper.CL), alpha=0.5, fill = "hotpink",  color = "hotpink", linetype = "dotted")+
  geom_ribbon(aes(ymin=CI$lower.CL, ymax=CI$upper.CL), alpha=0.5, fill = "purple",  color = "purple", linetype = "dotted")+
  labs(x="Treatment",y="Ps", color="Legend")+scale_color_manual(values = colors)+
  theme(panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", colour = "grey50"))+
  geom_jitter(data=model_df_cold,aes(x=treatment,y=alive), size=0.2, height = 0.03,width=5,color='black', alpha=0.3)+
  geom_jitter(data=model_df_mean,aes(x=treatment,y=alive), size=0.2, height = 0.03,width=5,color='purple', alpha=0.2)+
  geom_jitter(data=model_df_hot,aes(x=treatment,y=alive), size=0.2, height = 0.03,width=5,color='hotpink', alpha=0.2)



setwd(here("figures"))
png(paste(Sys.Date(), "tti_survival_curve_jitter.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot2
dev.off()



x0<-vector(length=21)
x30<-vector(length=21)
x60<-vector(length=21)
x90<-vector(length=21)
x120<-vector(length=21)

point_df<-trial
point_df$quart<-rep(0,21)

point_df[which(point_df$exp_set_temp_air<=q25),]$quart<-"black"
point_df[which(point_df$exp_set_temp_air>q25),]$quart<-"purple"
point_df[which(point_df$exp_set_temp_air>=q75),]$quart<-"hotpink"

for (i in c(1:10)){
trial_df<-subset(model_df_2,model_df_2$trial_number==i)
x0[i]<-sum(trial_df[which(trial_df$treatment==0),]$alive)/nrow(trial_df[which(trial_df$treatment==0),])
x30[i]<-sum(trial_df[which(trial_df$treatment==30),]$alive)/nrow(trial_df[which(trial_df$treatment==30),])
x60[i]<-sum(trial_df[which(trial_df$treatment==60),]$alive)/nrow(trial_df[which(trial_df$treatment==60),])
x90[i]<-sum(trial_df[which(trial_df$treatment==90),]$alive)/nrow(trial_df[which(trial_df$treatment==90),])
x120[i]<-sum(trial_df[which(trial_df$treatment==120),]$alive)/nrow(trial_df[which(trial_df$treatment==120),])
}
for (i in c(13:23)){
  trial_df<-subset(model_df_2,model_df_2$trial_number==i)
  x0[i-2]<-sum(trial_df[which(trial_df$treatment==0),]$alive)/nrow(trial_df[which(trial_df$treatment==0),])
  x30[i-2]<-sum(trial_df[which(trial_df$treatment==30),]$alive)/nrow(trial_df[which(trial_df$treatment==30),])
  x60[i-2]<-sum(trial_df[which(trial_df$treatment==60),]$alive)/nrow(trial_df[which(trial_df$treatment==60),])
  x90[i-2]<-sum(trial_df[which(trial_df$treatment==90),]$alive)/nrow(trial_df[which(trial_df$treatment==90),])
  x120[i-2]<-sum(trial_df[which(trial_df$treatment==120),]$alive)/nrow(trial_df[which(trial_df$treatment==120),])
}
point_df$x0<-x0
point_df$x30<-x30
point_df$x60<-x60
point_df$x90<-x90
point_df$x120<-x120

model_df_cold<-subset(model_df_2, model_df_2$temp<=q25)
model_df_mean_1<-subset(model_df_2, q25<model_df_2$temp)
model_df_mean<-subset(model_df_mean_1, q75>model_df_mean_1$temp)
model_df_hot<- subset(model_df_2, model_df_2$temp>=q75)

ci_df_cold<-data.frame(treatment=c(0,30,60,90,120),proportion=c(sum(model_df_cold[which(model_df_cold$treatment==0),]$alive)/length(model_df_cold[which(model_df_cold$treatment==0),]$alive),sum(model_df_cold[which(model_df_cold$treatment==30),]$alive)/length(model_df_cold[which(model_df_cold$treatment==30),]$alive),sum(model_df_cold[which(model_df_cold$treatment==60),]$alive)/length(model_df_cold[which(model_df_cold$treatment==60),]$alive),sum(model_df_cold[which(model_df_cold$treatment==90),]$alive)/length(model_df_cold[which(model_df_cold$treatment==90),]$alive),sum(model_df_cold[which(model_df_cold$treatment==120),]$alive)/length(model_df_cold[which(model_df_cold$treatment==120),]$alive)))
ci_df_mean<-data.frame(treatment=c(0,30,60,90,100,120),proportion=c(sum(model_df_mean[which(model_df_mean$treatment==0),]$alive)/length(model_df_mean[which(model_df_mean$treatment==0),]$alive),sum(model_df_mean[which(model_df_mean$treatment==30),]$alive)/length(model_df_mean[which(model_df_mean$treatment==30),]$alive),sum(model_df_mean[which(model_df_mean$treatment==60),]$alive)/length(model_df_mean[which(model_df_mean$treatment==60),]$alive),sum(model_df_mean[which(model_df_mean$treatment==90),]$alive)/length(model_df_mean[which(model_df_mean$treatment==90),]$alive),sum(model_df_mean[which(model_df_mean$treatment==100),]$alive)/length(model_df_mean[which(model_df_mean$treatment==100),]$alive),sum(model_df_mean[which(model_df_mean$treatment==120),]$alive)/length(model_df_mean[which(model_df_mean$treatment==120),]$alive)))
ci_df_hot<-data.frame(treatment=c(0,30,60,90,120),proportion=c(sum(model_df_hot[which(model_df_hot$treatment==0),]$alive)/length(model_df_hot[which(model_df_hot$treatment==0),]$alive),sum(model_df_hot[which(model_df_hot$treatment==30),]$alive)/length(model_df_hot[which(model_df_hot$treatment==30),]$alive),sum(model_df_hot[which(model_df_hot$treatment==60),]$alive)/length(model_df_hot[which(model_df_hot$treatment==60),]$alive),sum(model_df_hot[which(model_df_hot$treatment==90),]$alive)/length(model_df_hot[which(model_df_hot$treatment==90),]$alive),sum(model_df_hot[which(model_df_hot$treatment==120),]$alive)/length(model_df_hot[which(model_df_hot$treatment==120),]$alive)))

back_trans<-function(x){
  return(exp(x)/(1+exp(x)))
}
trans<-function(x){
  return(log(x/(1-x)))
}

plot3<-ggplot(plot_df, aes(x=x))+geom_line(aes(y=min, color="Min temp"), size = 1)+
  geom_ribbon(aes(ymin=CI2$lower.CL, ymax=CI2$upper.CL), alpha=0.5, fill = "black",  color = "black", linetype = "dotted")+
  geom_line(aes(y=mid, color="Mean temp"), size = 1) +geom_line(aes(y=max, color="Max temp"), size = 1) +
  geom_ribbon(aes(ymin=CI1$lower.CL, ymax=CI1$upper.CL), alpha=0.5, fill = "hotpink",  color = "hotpink", linetype = "dotted")+
  geom_ribbon(aes(ymin=CI$lower.CL, ymax=CI$upper.CL), alpha=0.5, fill = "purple",  color = "purple", linetype = "dotted")+
  labs(x="Treatment",y="Ps", color="Legend")+scale_color_manual(values = colors)+
  theme(panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", colour = "grey50"))+
  geom_point(data=ci_df_cold, aes(x=treatment,y=proportion),color="black")+geom_point(data=ci_df_mean, aes(x=treatment,y=proportion),color="purple")+
  geom_point(data=ci_df_hot, aes(x=treatment,y=proportion),color="hotpink")

setwd(here("figures"))
png(paste(Sys.Date(), "tti_survival_curves_average.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot3
dev.off()

CI10<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(10,120),treatment=c(1:120))) 
CI14<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(14,120),treatment=c(1:120))) 
CI18<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(18,120),treatment=c(1:120))) 
CI22<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(22,120),treatment=c(1:120))) 
CI26<-modavgPred(list(model_tti1), newdata=data.frame(temp=rep(26,120),treatment=c(1:120))) 

colors<-c("10\u00B0C"="black","14\u00B0C"="blue" ,'18\u00B0C'="purple", "22\u00B0C"="hotpink","26\u00B0C"="red")
plot_df<-data.frame(x=1:120,min=CI10$mod.avg.pred,mid1=CI14$mod.avg.pred,mid2=CI18$mod.avg.pred,max3=CI22$mod.avg.pred,max4=CI26$mod.avg.pred)

plot5<-ggplot(plot_df, aes(x=x))+geom_line(aes(y=min, color="10\u00B0C"), size = 1)+  geom_line(aes(y=mid1, color="14\u00B0C"), size = 1) +
  geom_line(aes(y=mid2, color="18\u00B0C"), size = 1) +geom_line(aes(y=max3, color="22\u00B0C"), size = 1) +geom_line(aes(y=max4, color="26\u00B0C"), size = 1)+
  geom_ribbon(aes(ymin=CI10$lower.CL, ymax=CI10$upper.CL), alpha=0.5, fill = "black",  color = "black", linetype = "dotted")+
  geom_ribbon(aes(ymin=CI14$lower.CL, ymax=CI14$upper.CL), alpha=0.5, fill = "blue",  color = "blue", linetype = "dotted")+
  geom_ribbon(aes(ymin=CI18$lower.CL, ymax=CI18$upper.CL), alpha=0.5, fill = "purple",  color = "purple", linetype = "dotted")+
  geom_ribbon(aes(ymin=CI22$lower.CL, ymax=CI22$upper.CL), alpha=0.5, fill = "hotpink",  color = "hotpink", linetype = "dotted")+
  geom_ribbon(aes(ymin=CI26$lower.CL, ymax=CI26$upper.CL), alpha=0.5, fill = "red",  color = "red", linetype = "dotted")+
  labs(x="Treatment",y="Ps", color="Temperature")+scale_color_manual(values = colors)+
  theme(panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", colour = "grey50")) 

setwd(here("figures"))
png(paste(Sys.Date(), "tti_survival_curves.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot5
dev.off()



ggplot(point_df, aes(x=0:120, y=x0))+geom_point(point_df,aes(x=rep(0,21),colour = cut(x0, c(-Inf, q25, q75, Inf))),
                                                size = 5) +scale_color_manual(name = "x0", values = c("(-Inf,12.8]" = "black",
                                                                                                      "(12.8,17.8]" = "purple",
                                                                                                      "(17.8, Inf]" = "hotpink"),
            
                                                                                                                                                labels = c("<= 12.8", "12.8 < qsec <= 17.8", "> 17.8"))





newdata1<-expand.grid(temp=c(min_temp,mean(model_df_2$temp),max_temp),treatment=c(0,15,30,45,60,75,90,105,120), trial_trap=levels(model_df_2$trial_trap))
newdata1$logit_ps <- predict(model_tti1, newdata = newdata1)
newdata1$ps<-back_trans(newdata1$logit_ps)
newdata1[order(newdata1$treatment,newdata1$temp),]
plot(newdata1$treatment,newdata1$ps)

graphics.off()
setwd(here("figures"))
png(paste(Sys.Date(), "min_temp_re.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
ggplot(newdata1[which(newdata1$temp==min_temp),],aes(x=treatment, y=ps, col=trial_trap))+geom_line()+theme(legend.position = "none",panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", colour = "grey50"))
dev.off()

setwd(here("figures"))
png(paste(Sys.Date(), "mean_temp_re.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(2,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
ggplot(newdata1[which(newdata1$temp==mean(model_df_2$temp)),],aes(x=treatment, y=ps, col=trial_trap))+geom_line()+theme(legend.position = "none", panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", colour = "grey50"))
dev.off()

setwd(here("figures"))
png(paste(Sys.Date(), "max_temp_re.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(2,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
ggplot(newdata1[which(newdata1$temp==max_temp),],aes(x=treatment, y=ps, col=trial_trap))+geom_line()+theme(legend.position = "none",panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", colour = "grey50"))
dev.off()


#------------SUMMARY PLOTS--------------------------#

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


CI4
plot4<-ggplot(plot_df, aes(x=x))+geom_line(aes(y=min, color="Min temp"), size = 1)+
  geom_ribbon(aes(ymin=CI2$lower.CL, ymax=CI2$upper.CL), alpha=0.5, fill = "black",  color = "black", linetype = "dotted")+
  geom_line(aes(y=mid, color="Mean temp"), size = 1) +geom_line(aes(y=max, color="Max temp"), size = 1) +
  geom_ribbon(aes(ymin=CI1$lower.CL, ymax=CI1$upper.CL), alpha=0.5, fill = "hotpink",  color = "hotpink", linetype = "dotted")+
  geom_ribbon(aes(ymin=CI$lower.CL, ymax=CI$upper.CL), alpha=0.5, fill = "purple",  color = "purple", linetype = "dotted")+
  labs(x="Treatment",y="Ps", color="Legend")+scale_color_manual(values = colors)+
  theme(panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", colour = "grey50")) 


setwd(here("figures"))
png(paste(Sys.Date(), "residuals.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot(model_tti1,type=c("p","smooth"))
dev.off()


ggplot(point_df, aes(x=0:120, y=x0))+geom_point(point_df,aes(x=rep(0,21),colour = cut(x0, c(-Inf, q25, q75, Inf))),
                                                size = 5) +scale_color_manual(name = "x0", values = c("(-Inf,12.8]" = "black",
                                                                                                      "(12.8,17.8]" = "purple",
                                                                                                      "(17.8, Inf]" = "hotpink"),
                                                                              
                                                                              labels = c("<= 12.8", "12.8 < qsec <= 17.8", "> 17.8"))



quantile(model_df_2$temp,na.rm=TRUE)

CInew1<-modavgPred(list(model_6.1_1), newdata=data.frame(length=rep(18,120),temp=rep(14.1,120),treatment=c(1:120))) 
CInew2<-modavgPred(list(model_6.1_1), newdata=data.frame(length=rep(31.5,120),temp=rep(14.1,120),treatment=c(1:120))) 
CInew3<-modavgPred(list(model_6.1_1), newdata=data.frame(length=rep(34.25,120),temp=rep(14.1,120),treatment=c(1:120))) 
CInew4<-modavgPred(list(model_6.1_1), newdata=data.frame(length=rep(39,120),temp=rep(14.1,120),treatment=c(1:120))) 
CInew5<-modavgPred(list(model_6.1_1), newdata=data.frame(length=rep(52.5,120),temp=rep(14.1,120),treatment=c(1:120))) 



colors<-c("18"="black","31.5"="blue" ,'34.25'="purple", "39"="hotpink","52.5"="red")
plot_df<-data.frame(x=1:120,min=CInew1$mod.avg.pred,mid1=CInew2$mod.avg.pred,mid2=CInew3$mod.avg.pred,max3=CInew4$mod.avg.pred,max4=CInew5$mod.avg.pred)

plot6<-ggplot(plot_df, aes(x=x))+geom_line(aes(y=min, color="18"), size = 1)+  geom_line(aes(y=mid1, color="31.5"), size = 1) +
  geom_line(aes(y=mid2, color="34.25"), size = 1) +geom_line(aes(y=max3, color="39"), size = 1) +geom_line(aes(y=max4, color="52.5"), size = 1)+
  geom_ribbon(aes(ymin=CInew1$lower.CL, ymax=CInew1$upper.CL), alpha=0.5, fill = "black",  color = "black", linetype = "dotted")+
  geom_ribbon(aes(ymin=CInew2$lower.CL, ymax=CInew2$upper.CL), alpha=0.5, fill = "blue",  color = "blue", linetype = "dotted")+
  geom_ribbon(aes(ymin=CInew3$lower.CL, ymax=CInew3$upper.CL), alpha=0.5, fill = "purple",  color = "purple", linetype = "dotted")+
  geom_ribbon(aes(ymin=CInew4$lower.CL, ymax=CInew4$upper.CL), alpha=0.5, fill = "hotpink",  color = "hotpink", linetype = "dotted")+
  geom_ribbon(aes(ymin=CInew5$lower.CL, ymax=CInew5$upper.CL), alpha=0.5, fill = "red",  color = "red", linetype = "dotted")+
  labs(x="Treatment",y="Ps", color="Length (cm)")+scale_color_manual(values = colors)+
  theme(panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", colour = "grey50")) 

colors<-c("18"="black","31.5"="blue" , "39"="hotpink","52.5"="red")
plot_df<-data.frame(x=1:120,min=CInew1$mod.avg.pred,mid1=CInew2$mod.avg.pred,max3=CInew4$mod.avg.pred,max4=CInew5$mod.avg.pred)


plot7<-ggplot(plot_df, aes(x=x))+geom_line(aes(y=min, color="18"), size = 1)+  geom_line(aes(y=mid1, color="31.5"), size = 1) +
  geom_line(aes(y=max3, color="39"), size = 1) +geom_line(aes(y=max4, color="52.5"), size = 1)+
  geom_ribbon(aes(ymin=CInew1$lower.CL, ymax=CInew1$upper.CL), alpha=0.4, fill = "black",  color = "black", linetype = "dotted")+
  geom_ribbon(aes(ymin=CInew2$lower.CL, ymax=CInew2$upper.CL), alpha=0.4, fill = "blue",  color = "blue", linetype = "dotted")+
  geom_ribbon(aes(ymin=CInew4$lower.CL, ymax=CInew4$upper.CL), alpha=0.4, fill = "hotpink",  color = "hotpink", linetype = "dotted")+
  geom_ribbon(aes(ymin=CInew5$lower.CL, ymax=CInew5$upper.CL), alpha=0.4, fill = "red",  color = "red", linetype = "dotted")+
  labs(x="Treatment",y="Ps", color="Length (cm)", title= "Prawn Survival at 14.1C")+scale_color_manual(values = colors)+
  theme(plot.title = element_text(hjust = 0.5),panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", colour = "grey50")) 

setwd(here("figures"))
png(paste(Sys.Date(), "survival_by_temp.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot7
dev.off()

CI10<-modavgPred(list(model_6.1_1), newdata=data.frame(length=rep(35,120),temp=rep(10,120),treatment=c(1:120))) 
CI14<-modavgPred(list(model_6.1_1), newdata=data.frame(length=rep(35,120),temp=rep(14,120),treatment=c(1:120))) 
CI18<-modavgPred(list(model_6.1_1), newdata=data.frame(length=rep(35,120),temp=rep(18,120),treatment=c(1:120))) 
CI22<-modavgPred(list(model_6.1_1), newdata=data.frame(length=rep(35,120),temp=rep(22,120),treatment=c(1:120))) 
CI26<-modavgPred(list(model_6.1_1), newdata=data.frame(length=rep(35,120),temp=rep(26,120),treatment=c(1:120))) 


colors<-c("10\u00B0C"="black","14\u00B0C"="blue" ,'18\u00B0C'="purple", "22\u00B0C"="hotpink","26\u00B0C"="red")
plot_df<-data.frame(x=1:120,min=CI10$mod.avg.pred,mid1=CI14$mod.avg.pred,mid2=CI18$mod.avg.pred,max3=CI22$mod.avg.pred,max4=CI26$mod.avg.pred)

plot8<-ggplot(plot_df, aes(x=x))+geom_line(aes(y=min, color="10\u00B0C"), size = 1)+  geom_line(aes(y=mid1, color="14\u00B0C"), size = 1) +
  geom_line(aes(y=mid2, color="18\u00B0C"), size = 1) +geom_line(aes(y=max3, color="22\u00B0C"), size = 1) +geom_line(aes(y=max4, color="26\u00B0C"), size = 1)+
  geom_ribbon(aes(ymin=CI10$lower.CL, ymax=CI10$upper.CL), alpha=0.5, fill = "black",  color = "black", linetype = "dotted")+
  geom_ribbon(aes(ymin=CI14$lower.CL, ymax=CI14$upper.CL), alpha=0.5, fill = "blue",  color = "blue", linetype = "dotted")+
  geom_ribbon(aes(ymin=CI18$lower.CL, ymax=CI18$upper.CL), alpha=0.5, fill = "purple",  color = "purple", linetype = "dotted")+
  geom_ribbon(aes(ymin=CI22$lower.CL, ymax=CI22$upper.CL), alpha=0.5, fill = "hotpink",  color = "hotpink", linetype = "dotted")+
  geom_ribbon(aes(ymin=CI26$lower.CL, ymax=CI26$upper.CL), alpha=0.5, fill = "red",  color = "red", linetype = "dotted")+
  labs(title = "Survival of a 35cm prawn",x="Treatment",y="Ps", color="Temperature")+scale_color_manual(values = colors)+
  theme(plot.title = element_text(hjust = 0.5),panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", colour = "grey50")) 

setwd(here("figures"))
png(paste(Sys.Date(), "survival_by_length.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot8
dev.off()


temp_quant<-quantile(model_df_2$temp)
length_quant<-quantile(model_df_2$length)

newdata2<-expand.grid(length=c(length_quant[2],length_quant[4]),temp=c(temp_quant[2],temp_quant[4]),treatment=c(0,15,30,45,60,75,90,105,120), trial_trap=levels(model_df_2$trial_trap))
newdata2$logit_ps <- predict(model_6.1_1, newdata = newdata2)
newdata2$ps<-back_trans(newdata2$logit_ps)
newdata2[order(newdata2$treatment,newdata2$temp),]

low_temp<-newdata2[which(newdata2$temp==temp_quant[2]),]
high_temp<-newdata2[which(newdata2$temp==temp_quant[4]),]
graphics.off()
setwd(here("figures"))
png(paste(Sys.Date(), "low_temp_short_re.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(2,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
ggplot(low_temp[which(low_temp$length==length_quant[2]),],aes(x=treatment, y=ps, col=trial_trap))+geom_line()+labs(title="1st quartile length 1st quartile temp")+theme(legend.position = "none",panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", colour = "grey50"))
dev.off()

setwd(here("figures"))
png(paste(Sys.Date(), "low_temp_long_re.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(2,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
ggplot(low_temp[which(low_temp$length==length_quant[4]),],aes(x=treatment, y=ps, col=trial_trap))+geom_line()+labs(title="3rd quartile length 1st quartile temp")+theme(legend.position = "none",panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", colour = "grey50"))
dev.off()

setwd(here("figures"))
png(paste(Sys.Date(), "high_temp_short_re.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(2,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
ggplot(high_temp[which(high_temp$length==length_quant[2]),],aes(x=treatment, y=ps, col=trial_trap))+geom_line()+labs(title="1st quartile length 3rd quartile temp")+theme(legend.position = "none",panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", colour = "grey50"))
dev.off()

setwd(here("figures"))
png(paste(Sys.Date(), "high_temp_long_re.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(2,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
ggplot(high_temp[which(high_temp$length==length_quant[4]),],aes(x=treatment, y=ps, col=trial_trap))+geom_line()+labs(title="3rd quartile length 3rd quartile temp")+theme(legend.position = "none",panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", colour = "grey50"))
dev.off()



##ATTEMPT 2----
NEWDATA<-expand.grid(length=seq(18,52,length.out=20),temp=seq(10.7,25.4,length.out=20),treatment=c(0,15,30,45,60,75,90,105,120))
NEWDATA<-data.frame(length=rep(35,30),temp=rep(17,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
library(MuMIn)
ms1 <- dredge(model_7_2, rank=BIC)
confset.95p <- get.models(ms1, subset = weight >0)
avgm<-model.avg(confset.95p, rank = BIC)

BIC <- function(x) AIC(x, k = log(length(residuals(x))))


NEWDATA
NEWDATAA<-NEWDATA3
NEWDATAB<-NEWDATA6

survival_predictor<-function(NEWDATA, NEWDATAA,NEWDATAB){

NEWDATA$trial_trap<-rep(NA, nrow(NEWDATA))
NEWDATA5<-NEWDATA
NEWDATA5$trial_trap<-rep(0,nrow(NEWDATA))
#
NEWDATAA$trial_trap<-rep(NA, nrow(NEWDATAA))

NEWDATAA5<-NEWDATAA
NEWDATAA5$trial_trap<-rep(0,nrow(NEWDATAA))
#
NEWDATAB$trial_trap<-rep(NA, nrow(NEWDATAB))

NEWDATAB5<-NEWDATAB
NEWDATAB5$trial_trap<-rep(0,nrow(NEWDATAB))





# Predictions from each of the models in a set, and with averaged coefficients
pred.se <- data.frame(average=predict(avgm, NEWDATA, se.fit = TRUE, type = "response"),
                      mod1=predict(model_6.1_1, NEWDATA5,re.form=NA, type = "response"),
                      mod2=predict(model_3_1, NEWDATA5,re.form=NA, type = "response"),
                      
                     average2=predict(avgm, NEWDATAA, se.fit = TRUE, type = "response"),
                     mod3=predict(model_6.1_1, NEWDATAA5,re.form=NA, type = "response"),
                     mod4=predict(model_3_1, NEWDATAA5,re.form=NA, type = "response"),
                      
                     average3=predict(avgm, NEWDATAB, se.fit = TRUE, type = "response"),
                     mod5=predict(model_6.1_1, NEWDATAB5,re.form=NA, type = "response"),
                      mod6=predict(model_3_1, NEWDATAA5,re.form=NA, type = "response"))
                      
                     #,
                     # mod2=predict(model_5.1_1, NEWDATA5, re.form=NA, type = "response"),
                     # mod3=predict(model_4.1_1, NEWDATA5, re.form=NA, type = "response"),
                     # mod4=predict(model_7_1, NEWDATA5, re.form=NA, type = "response"),
                     # mod5=predict(model_6.2_1, NEWDATA5, re.form=NA, type = "response"))

plot(NEWDATA$treatment,pred.se$average.fit, type="l", ylim=c(0,1),
        lty = 1, col = 1, lwd = 1, ylab="Probability of survival", xlab="Treatment time (min)",
        main=paste(unique(NEWDATA$length),"mm prawn survival"))
lines(NEWDATA5$treatment,pred.se$mod1,lty=2)
lines(NEWDATA5$treatment,pred.se$mod2,lty=3)


lines(NEWDATAA5$treatment,pred.se$average2.fit,col=2,lty=1)
lines(NEWDATAA5$treatment,pred.se$mod3,col=2,lty=2)
lines(NEWDATAA5$treatment,pred.se$mod4,col=2,lty=3)

lines(NEWDATAB5$treatment,pred.se$average3.fit,col=3, lty=1)
lines(NEWDATAB5$treatment,pred.se$mod5,col=3,lty=2)
lines(NEWDATAB5$treatment,pred.se$mod6,col=3,lty=3)

#lines(NEWDATA5$treatment,pred.se$mod2,col=2)
#lines(NEWDATA5$treatment,pred.se$mod3,col=3)
#lines(NEWDATA5$treatment,pred.se$mod4,col=4)
#lines(NEWDATA5$treatment,pred.se$mod5,col=5)
#legend("topright",
       #legend=c("temp x treatment + length x temp",
                #"temp x treatment + length",
                #"temp x treatment",
                #"length x temp + length x treatment + temp x treatment",
                #"treatment x length + treatment x temp ",
                #"averaged predictions"),
      # lty = c(1,2),  cex = 1, col=c(1,6))
}



NEWDATA<-data.frame(length=rep(23,30),temp=rep(11,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
NEWDATA3<-data.frame(length=rep(23,30),temp=rep(16,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
NEWDATA6<-data.frame(length=rep(23,30),temp=rep(22,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))


NEWDATA1<-data.frame(length=rep(35,30),temp=rep(11,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
NEWDATA4<-data.frame(length=rep(35,30),temp=rep(16,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
NEWDATA7<-data.frame(length=rep(35,30),temp=rep(22,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))



NEWDATA2<-data.frame(length=rep(49,30),temp=rep(11,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
NEWDATA5<-data.frame(length=rep(49,30),temp=rep(16,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
NEWDATA8<-data.frame(length=rep(49,30),temp=rep(22,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))


datums<-list(NEWDATA,NEWDATA1,NEWDATA2,NEWDATA3,NEWDATA4,NEWDATA5)
leg<-c(1,0,0,0,0,0)

plot.new()
legend("left",
       legend=c(lapply(confset.95p, formula),formula(model_6.2_1),
                "averaged predictions + CI"),
       lty = 1,  cex = 1, col=1:6)

setwd(here("figures"))
pdf(paste(Sys.Date(), "avg_model_comparison.pdf", sep="_"), width=7, height=11)
par(mfrow=c(3,2),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot.new()
legend("topright",
       legend=c("11 degrees","16 degrees", "22 degrees"),
         #"temp x treatment + length x temp",
                #"temp x treatment + length",
                #"temp x treatment",
                #"length x temp + length x treatment + temp x treatment",
                #"treatment x length + treatment x temp ",
                #"averaged predictions"),
       lty = c(1,1,1),  cex = 1, col=c(1,2,3))
survival_predictor(NEWDATA2,NEWDATA5,NEWDATA8)
#survival_predictor(NEWDATA1)
survival_predictor(NEWDATA2)
plot.new()
#survival_predictor(NEWDATA3)
#survival_predictor(NEWDATA4)
#survival_predictor(NEWDATA5)
survival_predictor(NEWDATA6)
#survival_predictor(NEWDATA7)
survival_predictor(NEWDATA8)
dev.off()



#Compare coefficients
top_five<-c(model_6.1_1,model_5.1_1,model_4.1_1,model_7_1,model_6.2_1)
for (i in 1:5){
print(summary(top_five[[i]])$coef[,1])
  
}

##TEMPxlength 9 curves 


NEWDATA1<-data.frame(length=rep(24,30),temp=rep(11,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
NEWDATA2<-data.frame(length=rep(32,30),temp=rep(11,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
NEWDATA3<-data.frame(length=rep(40,30),temp=rep(11,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
NEWDATA4<-data.frame(length=rep(24,30),temp=rep(16,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
NEWDATA5<-data.frame(length=rep(32,30),temp=rep(16,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
NEWDATA6<-data.frame(length=rep(40,30),temp=rep(16,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
NEWDATA7<-data.frame(length=rep(24,30),temp=rep(21,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
NEWDATA8<-data.frame(length=rep(32,30),temp=rep(21,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))
NEWDATA9<-data.frame(length=rep(40,30),temp=rep(21,30),treatment=seq(0,120,length.out=30), trial_trap=rep(NA, 30))

x<-seq(0,120,length.out=30)
pred1<-predict(model_6.1_1, NEWDATA1, re.form=NA, type = "response")
pred2<-predict(model_6.1_1, NEWDATA2, re.form=NA,type = "response")
pred3<-predict(model_6.1_1, NEWDATA3, re.form=NA, type = "response")
pred4<-predict(model_6.1_1, NEWDATA4, re.form=NA, type = "response")
pred5<-predict(model_6.1_1, NEWDATA5, re.form=NA, type = "response")
pred6<-predict(model_6.1_1, NEWDATA6,re.form=NA, type = "response")
pred7<-predict(model_6.1_1, NEWDATA7, re.form=NA, type = "response")
pred8<-predict(model_6.1_1, NEWDATA8, re.form=NA, type = "response")
pred9<-predict(model_6.1_1, NEWDATA9,re.form=NA, type = "response")

cols=viridis(3)
library(viridis)
setwd(here("New-figures"))
pdf(paste(Sys.Date(), "9_survival_curves.pdf", sep="_"), width=11, height=7)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot(x,pred1, type="l",lwd=2, ylim=c(0,1), ylab="Probability of Survvial",xlab="Time out of water", col=cols[3])
lines(x,pred2,lwd=2, lty=2,col=cols[3])
lines(x,pred3,lwd=2, lty=3,col=cols[3])
lines(x,pred4,lwd=2,col=cols[2])
lines(x,pred5,lwd=2, lty=2,col=cols[2])
lines(x,pred6,lwd=2, lty=3,col=cols[2])
lines(x,pred7,lwd=2,col=cols[1])
lines(x,pred8,lwd=2, lty=2,col=cols[1])
lines(x,pred9,lwd=2, lty=3,col=cols[1])
legend(90,1.03,title="Temperature",
       legend=c("11 degrees","16 degrees", "22 degrees"),
       lty = c(1,1,1),  cex = 0.8, col=cols[3:1])
legend(90,0.85,title="Carapace Length",
       legend=c("24 mm","32 mm", "40 mm"),
       lty = c(1,2,3),  cex = 0.8)

dev.off()


#Survival curves with temp x treat averages
library(AICcmodavg)
mean_length<-mean(model_df_2$length)
hist_temps<-hist(trial$exp_set_temp_air,plot=FALSE, breaks= c(9,13,17,21,26))
hist_temps$counts


cols<-viridis(4)
cols1<-viridis(4,alpha=0.5)
CInew1<-modavgPred(list(model_6.1_1), newdata=data.frame(temp=rep(hist_temps$mids[1],120),length=rep(mean_length,120),treatment=c(1:120))) 
CInew2<-modavgPred(list(model_6.1_1), newdata=data.frame(temp=rep(hist_temps$mids[2],120),length=rep(mean_length,120),treatment=c(1:120))) 
CInew3<-modavgPred(list(model_6.1_1), newdata=data.frame(temp=rep(hist_temps$mids[3],120),length=rep(mean_length,120),treatment=c(1:120))) 
CInew4<-modavgPred(list(model_6.1_1), newdata=data.frame(temp=rep(hist_temps$mids[4],120),length=rep(mean_length,120),treatment=c(1:120))) 

x<-c(1:120)

hist_temps$breaks

colors<-c("11\u00B0C"= cols[1],"15\u00B0C"=cols[2] ,"19\u00B0C"=cols[3], "23.5\u00B0C"= cols[4])

temp_12<-model_df_2[which(model_df_2$temp<=hist_temps$breaks[2]),][,c("treatment","alive")]
temp_16<-model_df_2[which(model_df_2$temp>hist_temps$breaks[2] & 
                               model_df_2$temp<=hist_temps$breaks[3]),][,c("treatment","alive")]
temp_20<-model_df_2[which(model_df_2$temp>hist_temps$breaks[3] & 
                                      model_df_2$temp<=hist_temps$breaks[4]),][,c("treatment","alive")]
temp_24<-model_df_2[which(model_df_2$temp>hist_temps$breaks[4]),][,c("treatment","alive")]

length(c(which(model_df_2$temp<=hist_temps$breaks[2]),which(model_df_2$temp>hist_temps$breaks[2] & model_df_2$temp<=hist_temps$breaks[3]),
  which(model_df_2$temp>hist_temps$breaks[3] & model_df_2$temp<=hist_temps$breaks[4]),which(model_df_2$temp>hist_temps$breaks[4])))
hist_temps$counts
treatments<-sort(unique(temp_12$treatment))


confint<-function(df, t=c(0,30,60,90,120)){
  
  N<-vector(length=length(t))
  vec_mean<-vector(length=length(t))
  vec_upper<-vector(length=length(t))
  vec_lower<-vector(length=length(t))
  
  for (i in 1:length(t)){
    
  N[i]=nrow(df[df$treatment==t[i],])
  vec_mean[i]<-mean(df[which(df$treatment==t[i]),]$alive)
  vec_upper[i]<-prop.test(x= sum(df[which(df$treatment==t[i]),]$alive),n=nrow(df[df$treatment==t[i],]))$conf.int[2]
  vec_lower[i]<-prop.test(x= sum(df[which(df$treatment==t[i]),]$alive),n=nrow(df[df$treatment==t[i],]))$conf.int[1]
  
  }
  
  return(data.frame(treat=t,N=N,mean=vec_mean,lower=vec_lower,upper=vec_upper))
  
}

av<-rbind(confint(temp_12), confint(temp_16,t=c(0,30,60,90,100,120)),confint(temp_20),confint(temp_24))

#DELETE
for (i in 1:nrow(av)){
  
ifelse(av$mean[i]+av$sd[i]>1,av$ymax[i]<-1,av$ymax[i]<-av$mean[i]+av$sd[i])
ifelse(av$mean[i]-av$sd[i]<0,av$ymin[i]<-0,av$ymin[i]<-av$mean[i]-av$sd[i])
}

hist_temps$mids
colors<-c("11\u00B0C"= cols[1],"15\u00B0C"=cols[2] ,"19\u00B0C"=cols[3], "23.5\u00B0C"= cols[4])

library(ggplot2)
plot7<-ggplot(data = NULL, aes(x=x))+geom_line(aes(y=CInew1$mod.avg.pred, color="11\u00B0C"), lwd = 1)+  geom_line(aes(y=CInew2$mod.avg.pred, color="15\u00B0C"), lwd = 1) +
  geom_line(aes(y=CInew3$mod.avg.pred, color="19\u00B0C"), lwd = 1) +geom_line(aes(y=CInew4$mod.avg.pred, color="23.5\u00B0C"), lwd = 1)+
  geom_ribbon(aes(ymin=CInew1$lower.CL, ymax=CInew1$upper.CL), alpha=0.3, fill = cols[1],  color = cols[1], linetype = "dotted")+
  geom_ribbon(aes(ymin=CInew2$lower.CL, ymax=CInew2$upper.CL), alpha=0.3, fill = cols[2],  color = cols[2], linetype = "dotted")+
  geom_ribbon(aes(ymin=CInew3$lower.CL, ymax=CInew3$upper.CL), alpha=0.3, fill = cols[3],  color = cols[3], linetype = "dotted")+
  geom_ribbon(aes(ymin=CInew4$lower.CL, ymax=CInew4$upper.CL), alpha=0.3, fill = cols[4],  color = cols[4], linetype = "dotted")+
  geom_point(data=av[1:5,], aes(x=treat,y=mean),color=cols1[1])+geom_point(data=av[6:11,], aes(x=treat,y=mean),color=cols1[2])+
  geom_point(data=av[12:16,], aes(x=treat,y=mean),color=cols1[3])+geom_point(data=av[17:21,], aes(x=treat,y=mean),color=cols1[4])+
  geom_errorbar(data = av,aes(x=treat,ymin=lower, ymax=upper), width=.2, alpha=0.4)+
  labs(x="Time out of water",y="Probability of Survival", color="Air Temperature", title= "35mm Prawn Survival")+scale_color_manual(values = cols1)+
  theme(    legend.position = c(.95, .95),
            legend.justification = c("right", "top"),
            legend.box.just = "right",
            legend.margin = margin(6, 6, 6, 6),plot.title = element_text(hjust = 0.5),panel.grid.major.y = element_line(color="grey"),panel.background = element_rect(fill = "white", color = "grey50"))


setwd(here("figures"))
png(paste(Sys.Date(), "survival_by_temp_with_treat_av.png", sep="_"), width=3000, height=2000, units = "px", pointsize=1, res=300)
par(mfrow=c(1,1),mar=c(4,4,1,2), oma=c(0,0,4,0))
plot7
dev.off()


##Model Table----
library(here)
library(lme4)
library(MuMIn)
library(glmmTMB)
setwd(here('Data-clean'))
model_df<-read.csv("2023_08_10_model_dataframe")

model_df$temp<-scale(model_df$temp)
model_df$treatment<-scale(model_df$treatment)


model_df_nosmall<-model_df[which(model_df$length>28.5),]
model_df$length<-scale(model_df$length)
model_df_nosmall$length<-scale(model_df_nosmall$length)

BIC <- function(x) AIC(x, k = log(length(residuals(x))))


model_main<-glmer(alive~length+temp+treatment+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_best<-glmer(alive~length*temp+temp*treatment+(1|trial_trap),data=model_df,family=binomial,nAGQ=10)
model_all<-glmmTMB(alive~temp*length+treatment*length+temp*treatment+(1|trial_trap),data=model_df,family=binomial,na.action="na.fail")

model_best_nolength<-glmer(alive~length*temp+temp*treatment+(1|trial_trap),data=model_df_nosmall,family=binomial,nAGQ=10)

model_all_nosmall<-glmmTMB(alive~length*temp+treatment*length+temp*treatment+(1|trial_trap),data=model_df_nosmall,family=binomial,na.action="na.fail")

ms1 <- dredge(model_all, rank=BIC)
confset.95p <- get.models(ms1, subset = weight >0)
avgm<-model.avg(confset.95p, rank = BIC)

avgm$coefArray
ms1_nosmall <- dredge(model_all_nosmall, rank=BIC)
confset.95p <- get.models(ms1_nosmall, subset = weight >0)
avgm_nosmall<-model.avg(confset.95p, rank = BIC)
avgm

coefficients(avgm)
coefficients(avgm_nosmall)[c(1,5,2,3,7,4,6)]
fixef(model_best)
fixef(model_main)
fixef(model_best_nolength)

round(2.1345,3)
model_table<-data.frame(cbind(round(c(fixef(model_best),0),3),round(c(fixef(model_best_nolength),0),3),round(coefficients(avgm),3),
                              round(coefficients(avgm_nosmall)[c(1,5,2,3,7,4,6)],3),round(c(fixef(model_main),0,0,0),3)))
#model_table<-model_table[,2:4]
rownames(model_table)[7]<-"length:treatment"
colnames(model_table)<-c("Best model","Best model(No small)","Average model","Average model (No small)","Main Effects")



#Manually get values for the random effect hyperparameter

summary(model_best)
summary(model_best_nolength)

summary(avgm)
summary(avgm_nosmall)

summary(model_main)

hyperparameter<-c(0.8791,0.9342,NA,NA,0.8697)

model_table["RE SD",]<-hyperparameter


#Accuracy
model_df_pred<-data.frame(alive=model_df$alive)

avgm_pred<-predict(avgm,model_df, type = "response")
best_pred<-predict(model_best,model_df, type = "response")
main_pred<-predict(model_all,model_df, type = "response")

model_df_pred$predict_avg <- ifelse(avgm_pred>= .5, 1,0)
model_df_pred$predict_best <- ifelse(best_pred>= .5, 1,0)
model_df_pred$predict_main <- ifelse(main_pred>= .5, 1,0)

accuracy_avg<-mean(model_df_pred$alive==model_df_pred$predict_avg)
accuracy_best<-mean(model_df_pred$alive==model_df_pred$predict_best)
accuracy_main<-mean(model_df_pred$alive==model_df_pred$predict_main)

accuracy=c(accuracy_avg,accuracy_best,accuracy_main)

##Correlation Factor

expit<-function(x){
  return(exp(x)/(1+exp(x)))
}

##Correlation with data
correlation<-c(cor(expit(avgm_pred),expit(model_df$alive)),
cor(expit(best_pred),expit(model_df$alive)),
cor(expit(main_pred),expit(model_df$alive)))

#MAXIMUM DEVIANCE

##Make Fake data
minlength<-min(model_df$length)
maxlength<-max(model_df$length)
mintemp<-min(model_df$temp)
maxtemp<-max(model_df$temp)
mintreat<-min(model_df$treatment)
maxtreat<-max(model_df$treatment)


length=seq(minlength,maxlength,length.out=20)
temp=seq(mintemp,maxtemp,length.out=20)
treatment=seq(mintemp,maxtemp,length.out=20)
trial_trap=c(NA)


dev_data<-expand.grid(length=length,temp=temp,treatment=treatment,trial_trap=trial_trap)
head(dev_data)

avgm_pred_new<-predict(avgm,dev_data,re.form=NA, type = "response")
best_pred_new<-predict(model_best,dev_data,re.form=NA, type = "response")
main_pred_new<-predict(model_all,dev_data,re.form=NA, type = "response")

max_dev_average<-0
max_dev_best<-max(c(abs(max(avgm_pred-best_pred)),abs(min(avgm_pred-best_pred))))
max_dev_main<-max(c(abs(max(avgm_pred-main_pred)),abs(min(avgm_pred-main_pred))))

deviance=c(max_dev_average,max_dev_best,max_dev_main)

model_table["Correlation",]<-c(correlation[2],NA,correlation[1],NA,correlation[3])

model_table["Accuracy",]<-c(accuracy[2],NA,accuracy[1],NA,accuracy[3])
model_table["Max Deviance",]<-c(deviance[2],NA,deviance[1],NA,deviance[3])

model_table

setwd(here("figures"))
write.csv(model_table, paste(Sys.Date(),"model_comparison.csv",sep ="_"))
