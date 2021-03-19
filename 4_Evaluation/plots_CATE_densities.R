#Plots for thesis
#Set working directory
setwd("C:/Users/pimva/Documents/Studie/Thesis/Programming")


library(ggplot2)
library(dplyr)
library(bayestestR)
library(miceadds)

#BMI PLOT CROSS SECTIONAL by race unweighted 

CATE_white <-
  rowMeans(bcf_test$`posterior_results BMI`$tau[, which(bcf_test$effect_moderators[, "race.1.white.caucasian"] == 1)])
CATE_white_CI <-
  ci(CATE_white, method = "HDI")

CATE_black <-
  rowMeans(bcf_test$`posterior_results BMI`$tau[, which(bcf_test$effect_moderators[, "race.2.black.african.american"] == 1)])
CATE_black_CI <-
  ci(CATE_black, method = "HDI")

CATE_other <-
  rowMeans(bcf_test$`posterior_results BMI`$tau[, which(bcf_test$effect_moderators[, "race.3.other"] == 1)])
CATE_other_CI <-
  ci(CATE_other, method = "HDI")

CATES_BMI <- as.data.frame(cbind(c(CATE_white, CATE_black, CATE_other),
                                 c(rep("White or Caucasian", length(CATE_white)), rep("Black or African American", length(CATE_black)),
                                   rep("Other", length(CATE_other)))))
names(CATES_BMI) <- c("CATES", "Race")
CATES_BMI$CATES <- as.numeric(CATES_BMI$CATES)

a <- ggplot(CATES_BMI, aes(x = CATES))
a + geom_density()
a + geom_density(aes(fill = Race), alpha = 0.4)
mu <- CATES_BMI %>% group_by(Race) %>% summarize(grp.mean = mean(CATES))
plot_cross_sectional <- a + geom_density(aes(fill = Race), alpha = 0.4) + geom_vline(data = mu, aes(xintercept = grp.mean, color = Race), linetype = "dashed") + ylab("Posterior Probability Density") + xlab("CATE perceived racial discrimination on BMI")

plot_cross_sectional <- plot_cross_sectional + theme(legend.title = element_text(size = 20),
                                       legend.text = element_text(size = 20), axis.text=element_text(size=14), axis.title=element_text(size=20))
ggsave(filename = "CATES_unw.eps",
       plot = print(plot_cross_sectional),
       device = cairo_ps)

#BMI PLOT CROSS SECTIONAL by race weighted

#Load in data form weighted reuslts
namebasis <- "C_Results_Weighted_Analysis/CATE cross"
PRS_iterations <- 10
low_index <- 1
high_index <- 100000
CATES_BMI_w <- as.data.frame(cbind(c(rep(NA,1000000), rep(NA, 1000000), rep(NA, 1000000)),
                                 c(rep("White or Caucasian", 1000000), rep("Black or African American", 1000000),
                                   rep("Other", 1000000))))
                                 
names(CATES_BMI_w) <- c("CATES", "Race")

for(i in 1:PRS_iterations){
  loadname <- paste(namebasis, i,".RData" , sep = "")
  load.Rdata(loadname, "iteration_posteriordraws") 
  
  low_white <- low_index
  high_white <- high_index
  low_black <- low_index + 1000000
  high_black <- high_index + 1000000
  low_other <- low_index + 2000000
  high_other <- high_index + 2000000
  
  CATES_BMI_w$CATES[low_white:high_white] <- iteration_posteriordraws$CATE_race$white.caucasian[,2]
  CATES_BMI_w$CATES[low_black:high_black] <- iteration_posteriordraws$CATE_race$black.african.american[,2]
  CATES_BMI_w$CATES[low_other:high_other] <- iteration_posteriordraws$CATE_race$other[,2]
  
  
  low_index <- low_index + 100000
  high_index <- high_index + 100000
}

CATES_BMI_w$CATES <- as.numeric(CATES_BMI_w$CATES)
a <- ggplot(CATES_BMI_w, aes(x = CATES))
a + geom_density()
a + geom_density(aes(fill = Race), alpha = 0.3)
mu <- CATES_BMI_w %>% group_by(Race) %>% summarize(grp.mean = mean(CATES))
plot_weighted <- a + geom_density(aes(fill = Race), alpha = 0.3) + geom_vline(data = mu, aes(xintercept = grp.mean, color = Race), linetype = "dashed") + ylab("Posterior Probability Density") + xlab("CATE perceived racial discrimination on BMI")
#mu$Race <- as.factor(mu$Race)
#plot_weighted <- plot_weighted + scale_fill_discrete(breaks = rev(c(levels(mu$Race)[3], levels(mu$Race)[1], levels(mu$Race[2]))))
plot_weighted <- plot_weighted + theme(legend.title = element_text(size = 20),
legend.text = element_text(size = 20), axis.text=element_text(size=14), axis.title=element_text(size=20))

ggsave(filename = "CATES_weighted.eps",
       plot = print(plot_weighted),
       device = cairo_ps)

#Code below also highlights the credibility intervals for the distributions

#library(data.table)
#dt <- data.table(CATES_BMI_w)
#gg <- dt[,list(x=density(CATES)$x, y=density(CATES)$y),by="Race"]

#plot_weighted + geom_ribbon(data=subset(gg,Race=="White or Caucasian" & x>quantile(CATES_BMI_w[which(CATES_BMI_w$Race == "White or Caucasian"), ]$CATES, 0.89)),
#                              aes(x=x,ymax=y),ymin=0,fill="gray20", alpha=0.2)+
#  geom_ribbon(data=subset(gg,Race=="Black or African American" & x>quantile(CATES_BMI_w[which(CATES_BMI_w$Race == "Black or African American"), ]$CATES, 0.89)),
#              aes(x=x,ymax=y),ymin=0,fill="gray40", alpha=0.2)+
#  geom_ribbon(data=subset(gg,Race=="Other" & x> quantile(CATES_BMI_w[which(CATES_BMI_w$Race == "Other"), ]$CATES, 0.89)),
#              aes(x=x,ymax=y),ymin=0,fill="gray30", alpha=0.2) +
#  geom_ribbon(data=subset(gg,Race=="White or Caucasian" & x<quantile(CATES_BMI_w[which(CATES_BMI_w$Race == "White or Caucasian"), ]$CATES, 0.11)),
#              aes(x=x,ymax=y),ymin=0,fill="gray20", alpha=0.2)+
#  geom_ribbon(data=subset(gg,Race=="Black or African American" & x<quantile(CATES_BMI_w[which(CATES_BMI_w$Race == "Black or African American"), ]$CATES, 0.11)),
#              aes(x=x,ymax=y),ymin=0,fill="gray40", alpha=0.2)+
#  geom_ribbon(data=subset(gg,Race=="Other" & x< quantile(CATES_BMI_w[which(CATES_BMI_w$Race == "Other"), ]$CATES, 0.11)),
#              aes(x=x,ymax=y),ymin=0,fill="gray30", alpha=0.2) 



