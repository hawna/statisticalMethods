##One-way ANOVAs

library(car)

#one-way ANOVA All Staff, Total actions compared across types of Staff
resAllStaffAct.aov <-aov(actTotal ~ staffType, data = AllStaffMetrics)
summary(resAllStaffAct.aov)
TukeyHSD(resAllStaffAct.aov)
pairwise.t.test(AllStaffMetrics$actTotal, AllStaffMetrics$staffType,
                p.adjust.method = "BH" )
#pairwise T-test with no assumption of equal variance
#pairwise.t.test(my_data$weight, my_data$group,
#                 p.adjust.method = "BH", pool.sd = FALSE)

#Checking Assumptions

##Assumption 1: Homogeneity of Variances
plot(resAllStaffAct.aov)
leveneTest(actTotal ~ staffType, data = AllStaffMetrics)
###significance here means that variance across groups is stat sig

##Assumption 2: normality of residuals
plot(resAllStaffAct.aov, 2)
###extract residuals
resAllStaffActaov_residuals <- residuals(object = resAllStaffAct.aov)
###run shapiro-wilk test
shapiro.test(x = resAllStaffActaov_residuals)

##Non-Parametric Tests
#kruskal-Wallis rank sum test - non-Parametric
kruskal.test(actTotal ~ LCAType, data = lcaAllStaffMetrics)
pairwise.wilcox.test(lcaAllStaffMetrics$actTotal, lcaAllStaffMetrics$LCAType,
                     p.adjust.method = "BH"
)

##Other adjustment methods
# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")

##Links/Sources
#https://data.library.virginia.edu/the-wilcoxon-rank-sum-test/
