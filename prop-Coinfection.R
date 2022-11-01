# Please use English in R files for convenience of analyses----
install.packages('metafor')
require(devtools)
install_version("XLConnect", version = "1.0.2", repos = "https://mirrors.sjtug.sjtu.edu.cn/cran/")
install.packages('XLConnect')
install.packages("readxl")
install.packages("addploy")
library(XLConnect)
library(xlsx)
vignette("XLConnect")#查看包的使用说明
library(meta)
library(dplyr)

# install and upload packages----
{
library(metafor)
library(meta)
library(forestplot)
library(ggplot2)
library(tidyverse)
library(readxl)
  }
# main codes----
# estimate proportion# mixed-effects logistic regression model
rma.glmm(measure = "PLO", xi = , ni = , data = , 
         add =0.005, to="only0", method = "ML")
model="CM.EL"
#to= only0 means only the result is '0' needed to be correct.
# xi: column name of flu co-infection group 
# ni: column name of flu infection (single + co infection)
# ci: column name of flu single-infection & have the outcome
# di: column name of flu single-infection & NOT have the outcome
# data: name of your dataset


# compare severity: odds ratio
rma.glmm(measure = "OR", ai = , bi = , ci = , di = ,
         data = , add=0.005, to="only0",
         model = "UM.RS", method = "ML")
# ai: column name of flu co-infection group & have the outcome 
# bi: column name of flu co-infection group & NOT have the outcome
# ci: column name of flu single-infection & have the outcome
# di: column name of flu single-infection & NOT have the outcome
# data: name of your dataset

# 数据导入及处理 -----------------------------------------------------------------
{
Study <- read_xlsx("ExtractForm.xlsx", sheet = "Study")#导入Excel表单
Study <- arrange(Study,Study$First_author)
Study <- filter(Study,Study$Whether_included == "y"|Study$Whether_included=="y2")
proportion <- read_xlsx("ExtractForm.xlsx", sheet = "proportion")
proportion <- arrange(proportion,proportion$FirstAuthor)
proportion2 <- filter(proportion, proportion$Overall_age == "1")
Severity <- read_xlsx("ExtractForm.xlsx", sheet = "Severity")
Severity <- arrange(Severity,Severity$'First author')
sev_comorbidities<-read_xlsx("ExtractForm.xlsx", sheet = "sev_comorbidities")
sev_comorbidities<-arrange(sev_comorbidities, sev_comorbidities$First.author)
proportion$NumFlu <- as.numeric(proportion$NumFlu)
proportion$NumCo <- as.numeric(proportion$NumCo)
  
}

Study<-filter(Study, Study$Whether_included == "y") #筛选
Study<-Study[ ,c("StudyID","First_author" ,"Publish_year" ,"Title" , "Study_period" ,"before",                                                            
                 "during" ,"after" ,"Study_design"  )]##仅留下指定列
Study_setting<-Study[ , c("StudyID","Setting" )]
proportion_setting<-filter(proportion, proportion$Overall_age == "1")[ , c("StudyID","setting" )]
View(cbind(proportion_setting,Study_setting))


##纳入所有研究的prop的meta分析-by 'random model via generalized mixed linear'----------
{
proportion$NumFlu <- as.numeric(proportion$NumFlu)
proportion$NumCo <- as.numeric(proportion$NumCo)
  
prop_rma1<-rma.glmm(measure = "PLO", #logit transformed proportion
               xi = NumCo, ni = NumFlu, 
               data = filter(proportion, Overall_age == "1"), 
               add = 0.005, 
               #non-negative number to specify the amount to add to zero cells, counts, or frequencies when calculating the observed effect sizes or outcomes of  individual studies.
               to = "only0", method = "ML",model="CM.EL",
               slab = paste(FirstAuthor, PublishYear))
#print forest plot
prop_forest<-forest(prop_rma1, header = T,
                    xlim=c(-5,5),alim=c(0,2),#change the position of middle line
                    cex=0.5, cex.lab = 1.0,cex.axis = 1,#character size
                                             #search ?transf for more information
                    refline = 0.2,#set middle line
                    transf=transf.ilogit.int,#inverse of logit transformation
                    at=(c(-1,0,0.002,0.5,1,500)),digits = 3,order = "obs"
                    )#set the degree scale of lower line behind 'RE model'
                                #get the 'RE model' synthesis result=0.20
                              ##how to show weight?
}ylim = c(0,79)


#funnel plot漏斗图-----------------
funnel(prop_rma1)
ranktest(prop_rma1)

# escalc函数来计算库克距离cook's distance
prop <- proportion2[,c("StudyID","NumFlu","NumCo")]
prop$NumFlu <- as.numeric(prop$NumFlu)
prop$NumCo <- as.numeric(prop$NumCo)
dat <- prop
dat <- escalc(measure = "PR",xi = NumCo,ni = NumFlu,data = dat,add = 0)
res <- rma(yi,vi,data = dat)
inf <- influence(res)
plot(inf)#paper185为强影响

predict(res)#????
dis <- cooks.distance(res)
summary(dis)
dis <- as.data.frame.numeric(dis)
dis[which(dis$dis>0.01),]
dfbetas(res)#????
hatvalues(res)#????
#sensitivity analysis敏感性分析----
#去除强影响paper185
{
prop_rma1<-rma.glmm(measure = "PLO", #logit transformed proportion
                     xi = NumCo, ni = NumFlu, 
                     data = prop[which(prop$StudyID !="paper185"),], 
                     add = 0.005, 
                     to = "only0", method = "ML",model="CM.EL",
                     slab = paste(StudyID))
prop_forest<-forest(prop_rma1, header = T,
                    xlim=c(-5,5),alim=c(0,2),#change the position of middle line
                    cex=0.5, cex.lab = 1.0,cex.axis = 1,#character size
                    refline = 0.2,#set middle line
                    transf=transf.ilogit.int,#inverse of logit transformation
                    at=(c(-1,0,0.002,0.5,1,500)),digits = 3,order = "obs")
}
# 按照年龄分亚组subgorup analysis：<18y, >=18y,未分组的 -----
# 18岁以下
{
propUnder18_rma<-rma.glmm(measure = "PLO", 
                          xi = NumCo, ni = NumFlu, 
                          data = filter(proportion, age_num == "1"),add = 0.005,
                          to = "only0",method = "ML",
                          slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
propUnder18_forest<-forest(propUnder18_rma, header = T,digits = 3,
                           alim=c(0,2),#change the position of middle line
                           cex=1, cex.lab = 1.0,cex.axis = 1,refline = 0.15,
                           transf=transf.ilogit.int,at=(c(0,1)),xlab = "prop under 18")
}
# 18岁以上
{
propabove18_rma<-rma.glmm(measure = "PLO", 
                          xi = NumCo, ni = NumFlu, 
                          data = filter(proportion, age_num == "2"),add = 0.005,
                          to = "only0",method = "ML",
                          slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
propabove18_forest<-forest(propabove18_rma, header = T,digits = 3,
                           alim=c(0,2),#change the position of middle line
                           cex=1, cex.lab = 1.0,cex.axis = 1,refline = 0.15,
                           transf=transf.ilogit.int,at=(c(0,1)),xlab = "prop over 18")
}
# unstratified年龄组
{
  propabove18_rma<-rma.glmm(measure = "PLO", 
                            xi = NumCo, ni = NumFlu, 
                            data = filter(proportion, age_num == "3"),add = 0.005,
                            to = "only0",method = "ML",
                            slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  propabove18_forest<-forest(propabove18_rma, header = T,digits = 3,
                             alim=c(0,2),#change the position of middle line
                             cex=1, cex.lab = 1.0,cex.axis = 1,refline = 0.15,
                             transf=transf.ilogit.int,at=(c(0,1)),xlab = "unstratified age group")
}
# 组间统计学差异分析
all_age <- proportion[,c("age_num","NumFlu","NumCo")]#研究内细分组
all_age$age_num <- gsub("NA",3,all_age$age_num)
all_age$age_num <- factor(all_age$age_num)
prop_rma1<-rma.glmm(measure = "PLO", xi = NumCo, ni = NumFlu,data = all_age, 
                    add = 0.5,to = "only0", method = "ML",model="CM.EL",mods = ~relevel(age_num,ref = 2))
prop_rma1

all_age <- proportion[which(proportion$Overall_age == 1),]#以64篇研究本体
all_age <- all_age[,c("age_num","NumFlu","NumCo")]
all_age$age_num <- gsub("NA",3,all_age$age_num)
all_age$age_num <- factor(all_age$age_num)
prop_rma1<-rma.glmm(measure = "PLO", xi = NumCo, ni = NumFlu,data = all_age, 
                    add = 0.5,to = "only0", method = "ML",model="CM.EL",mods = ~relevel(age_num,ref = 2))
prop_rma1

# 按照阶段：大流行前、期间、之后、未分组的 ---------------------------------------------------------
{
  a <- Study[,c("before","during","after")]
prop3 <- cbind(proportion2,a)
prop3$NumFlu <- as.numeric(prop3$NumFlu)
prop3$NumCo <- as.numeric(prop3$NumCo)
pan <- prop3[,c("StudyID","NumFlu","NumCo","before","during","after")]
pan1 <- pan[,c(-5,-6)]
pan1 <- filter(pan1, before == 1)
pan1 <- rename(pan1, c("period" = "before"))
pan2 <- pan[,c(-4,-6)]
pan2 <- filter(pan2, during == 1)
pan2$during <- gsub(1,2,pan2$during)
pan2 <- rename(pan2, c("period" = "during"))
pan3 <- pan[,c(-4,-5)]
pan3 <- filter(pan3, after == 1)
pan3$after <- gsub(1,3,pan3$after)
pan3 <- rename(pan3, c("period" = "after"))
p <- rbind(pan1,pan2,pan3)
p$period <- as.numeric(p$period)

a <- Study[,c("period")]
prop3 <- cbind(proportion2,a)
prop3$NumFlu <- as.numeric(prop3$NumFlu)
prop3$NumCo <- as.numeric(prop3$NumCo)
p <- prop3[,c("StudyID","NumFlu","NumCo","period")]
p$period <- as.numeric(p$period)
p$period <- factor(p$period)
}
# 亚组统计学意义分析
prop_rma1<-rma.glmm(measure = "PLO", xi = NumCo, ni = NumFlu,data = p, 
                    add = 0.005,to = "only0", method = "ML",model="CM.EL",mods = ~relevel(period,ref = 3))
prop_rma1
prop_rma1<-rma.glmm(measure = "PLO", xi = NumCo, ni = NumFlu,data = p, 
                    add = 0.005,to = "only0", method = "ML",model="CM.EL",mods = period)
prop_rma1
# 大流行前
{
  prop3pro<-rma.glmm(measure = "PLO",
                     xi = NumCo, ni = NumFlu,
                     data = filter(prop3, period == 1),add = 0.005, 
                     to = "only0",method = "ML",
                     slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  forest(prop3pro, header = T,
         cex=0.8, cex.lab = 1.0,cex.axis = 1,refline = 0.15,digits = 3,
         transf=transf.ilogit.int,at=(c(0,1)),xlab = "大流行前")
}
# 大流行期间
{
  prop3pro<-rma.glmm(measure = "PLO",
                     xi = NumCo, ni = NumFlu,
                     data = filter(prop3, period == 2),add = 0.005, 
                     to = "only0",method = "ML",
                     slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  forest(prop3pro, header = T,
         cex=0.8, cex.lab = 1.0,cex.axis = 1,refline = 0.15,digits = 3,
         transf=transf.ilogit.int,at=(c(0,1)),xlab = "大流行期间")
}
# 大流行之后
{
  prop3pro<-rma.glmm(measure = "PLO",
                     xi = NumCo, ni = NumFlu,
                     data = filter(prop3, period == 3),add = 0.005, 
                     to = "only0",method = "ML",
                     slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  forest(prop3pro, header = T,
         cex=0.8, cex.lab = 1.0,cex.axis = 1,refline = 0.15,digits = 3,
         transf=transf.ilogit.int,at=(c(0,1)),xlab = "大流行之后")
}
# unstratified大流行
{
  prop3pro<-rma.glmm(measure = "PLO",
                     xi = NumCo, ni = NumFlu,
                     data = filter(prop3, period == 4),add = 0.005, 
                     to = "only0",method = "ML",
                     slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  forest(prop3pro, header = T,
         cex=0.8, cex.lab = 1.0,cex.axis = 1,refline = 0.15,digits = 3,
         transf=transf.ilogit.int,at=(c(0,1)),xlab = "unstratified大流行")
}

# 按照研究类型分亚组: 前瞻，回顾，前瞻和回顾 -----------------------------------------------------------------
prop3 <- cbind(proportion2,Study$Study_design,Study$Setting,Study$`Influenza test method`, Study$`Bacterial confirmation`)
prop3 <- rename(prop3, c("Study_design" = "Study$Study_design","Setting" = "Study$Setting",
                         "flu_test"="Study$`Influenza test method`","bac_confirm"="Study$`Bacterial confirmation`"))
prop3$NumFlu <- as.numeric(prop3$NumFlu)
prop3$NumCo <- as.numeric(prop3$NumCo)
pan <- prop3[,c("StudyID","NumFlu","NumCo","Study_design")]
pan1 <- pan
pan1$Study_design <- gsub("prospective and retrospective",3,pan1$Study_design)
pan1$Study_design <- gsub("prospective",1,pan1$Study_design)
pan1$Study_design <- gsub("retrospective",2,pan1$Study_design)
p <- pan1
p$Study_design <- factor(p$Study_design)
# 亚组统计学意义分析
prop_rma1<-rma.glmm(measure = "PLO", xi = NumCo, ni = NumFlu,data = p %>% filter(Study_design != 3), 
                    add = 0.5,to = "only0", method = "ML",model="CM.EL",mods = ~relevel(Study_design,ref = 1))
prop_rma1
# 前瞻
{
prop3pro<-rma.glmm(measure = "PLO",
                   xi = NumCo, ni = NumFlu,
                   data = filter(prop3, Study_design == "prospective"),add = 0.005, 
                   to = "only0",method = "ML",
                   slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
forest(prop3pro, header = T,
                 cex=0.8, cex.lab = 1.0,cex.axis = 1,refline = 0.15,digits = 3,
                 transf=transf.ilogit.int,at=(c(0,1)),xlab = "前瞻性研究proportion")
}
# 回顾
{
prop3re<-rma.glmm(measure = "PLO",
                   xi = NumCo, ni = NumFlu,
                   data = filter(prop3,Study_design == "retrospective"),add = 0.005, 
                   to = "only0",method = "ML",
                   slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
forest(prop3re, header = T,
                 cex=0.8, cex.lab = 1.0,cex.axis = 1,refline = 0.15,digits = 3,
                 transf=transf.ilogit.int,at=(c(0,1)),xlab="回顾性研究proportion")
}
# 回顾和前瞻混合的研究
{
prop3pro.re<-rma.glmm(measure = "PLO",
                   xi = NumCo, ni = NumFlu,
                   data = filter(prop3, Study_design == "prospective and retrospective"),add = 0.005, 
                   to = "only0",method = "ML",
                   slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
forest(prop3pro.re, header = T,digits = 3,
       transf=transf.ilogit.int,at=(c(0,1)),xlab = "同时包含前瞻和回顾proportion")
}

#亚组分析
meta_prop <- metaprop(n=prop3$NumFlu,event =prop3$NumCo,data=prop3,
                      studlab = paste(StudyID, PublishYear,sep = ","),
                      sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, subgroup = prop3$Study_design)
forest(meta_prop)

# 按照setting：ED, IP, ICU ---------------------------------------------------------------
{
prop3 <- cbind(proportion2,Study$Study_design,Study$Setting,Study$`Influenza test method`, Study$`Bacterial confirmation`)
prop3 <- rename(prop3, c("Study_design" = "Study$Study_design","Setting" = "Study$Setting",
                         "flu_test"="Study$`Influenza test method`","bac_confirm"="Study$`Bacterial confirmation`"))
prop3$NumFlu <- as.numeric(prop3$NumFlu)
prop3$NumCo <- as.numeric(prop3$NumCo)
}
# 亚组分析统计学差异分析
{
pan <- prop3[,c("StudyID","NumFlu","NumCo","Setting")]
pan1 <- pan
pan1$Setting <- gsub("ED",1,pan1$Setting)
pan1$Setting <- gsub("IP",2,pan1$Setting)
pan1$Setting <- gsub("ICU",3,pan1$Setting)
p <- pan1
p
p$Setting <- as.numeric(p$Setting)
p$Setting <- factor(p$Setting)
}
# 亚组统计学意义分析
p$Setting <- factor(p$Setting)
prop_rma1<-rma.glmm(measure = "PLO", xi = NumCo, ni = NumFlu,data = p,
                    add = 0.5,to = "only0", method = "ML",model="CM.EL",
                    mods = ~relevel(Setting, ref = 2))
prop_rma1
prop_rma1<-rma.glmm(measure = "PLO", xi = NumCo, ni = NumFlu,data = p,
                    add = 0.5,to = "only0", method = "ML",model="CM.EL",
                    mods = Setting)
prop_rma1
# ED
{
  prop3pro.re<-rma.glmm(measure = "PLO",
                        xi = NumCo, ni = NumFlu,
                        data = filter(prop3, Setting == "ED"),add = 0.005, 
                        to = "only0",method = "ML",
                        slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  forest(prop3pro.re, header = T,digits = 3,
         transf=transf.ilogit.int,at=(c(0,1)),xlab = "ED proportion")
}
# IP
{
  prop3pro.re<-rma.glmm(measure = "PLO",
                        xi = NumCo, ni = NumFlu,
                        data = filter(prop3, Setting == "IP"),add = 0.005, 
                        to = "only0",method = "ML",
                        slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  forest(prop3pro.re, header = T,cex = 0.6,cex.lab = 1,digits = 3,
         transf=transf.ilogit.int,at=(c(0,1)),xlab = "IP proportion")
}
# ICU
{
  prop3pro.re<-rma.glmm(measure = "PLO",
                        xi = NumCo, ni = NumFlu,
                        data = filter(prop3, Setting == "ICU"),add = 0.005, 
                        to = "only0",method = "ML",
                        slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  forest(prop3pro.re, header = T,digits = 3,
         transf=transf.ilogit.int,at=(c(0,1)),xlab = "ICU proportion")
}
# 86 98 95
#亚组分析
meta_prop <- metaprop(n=prop3$NumFlu,event =prop3$NumCo,data=prop3,
                      studlab = paste(StudyID, PublishYear,sep = ","),
                      sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, subgroup = prop3$Setting)
forest(meta_prop)


# 按照influenza test methods: l, h ----------------------------------------------------------
# low bias
{
  prop3pro.re<-rma.glmm(measure = "PLO",
                        xi = NumCo, ni = NumFlu,
                        data = filter(prop3, flu_test == "l"),add = 0.005, 
                        to = "only0",method = "ML",
                        slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  forest(prop3pro.re, header = T,cex = 0.5,digits = 3,
         transf=transf.ilogit.int,at=(c(0,1)),xlab = "flu_test low bias proportion")
}
# hign bias
{
  prop3pro.re<-rma.glmm(measure = "PLO",
                        xi = NumCo, ni = NumFlu,
                        data = filter(prop3, flu_test == "h"),add = 0.005, 
                        to = "only0",method = "ML",
                        slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  forest(prop3pro.re, header = T,digits = 3,
         transf=transf.ilogit.int,at=(c(0,1)),xlab = "flu_test low bias proportion")
}

#亚组分析
meta_prop <- metaprop(n=prop3$NumFlu,event =prop3$NumCo,data=prop3,
                      studlab = paste(StudyID, PublishYear,sep = ","),
                      sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, subgroup = prop3$flu_test)
forest(meta_prop)

# 按照specimen for bacterial confirmation: l, h  --------------------------------------
# low bias
{
  prop3pro.re<-rma.glmm(measure = "PLO",
                        xi = NumCo, ni = NumFlu,
                        data = filter(prop3, bac_confirm == "l"),add = 0.005, 
                        to = "only0",method = "ML",
                        slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  forest(prop3pro.re, header = T,cex = 0.5,digits = 3,
         transf=transf.ilogit.int,at=(c(0,1)),xlab = "bac_confirm low bias proportion")
}
# hign bias
{
  prop3pro.re<-rma.glmm(measure = "PLO",
                        xi = NumCo, ni = NumFlu,
                        data = filter(prop3, bac_confirm == "h"),add = 0.005, 
                        to = "only0",method = "ML",
                        slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  forest(prop3pro.re, header = T,digits = 3,
         transf=transf.ilogit.int,at=(c(0,1)),xlab = "bac_confirm high bias proportion")
}

#亚组分析
meta_prop <- metaprop(n=prop3$NumFlu,event =prop3$NumCo,data=prop3,
                      studlab = paste(StudyID, PublishYear,sep = ","),
                      sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, subgroup = prop3$bac_confirm)
forest(meta_prop)

# 剔除强影响点 ------------------------------------------------------------------
attach(proportion)
proportion$NumFlu <- as.numeric(proportion$NumFlu)
proportion$NumCo <- as.numeric(proportion$NumCo)

prop_rma1<-rma.glmm(measure = "PLO",xi = NumCo, ni = NumFlu, 
                    data = filter(proportion, Overall_age == "1",!StudyID == "paper185"), 
                    add = 0.005, to = "only0", method = "ML",model="CM.EL",
                    slab = paste(StudyID, FirstAuthor, PublishYear))
prop_forest<-forest(prop_rma1, header = T,xlim=c(-5,5),alim=c(0,2),
                    cex=0.5, cex.lab = 1.0,cex.axis = 1,
                    refline = 0.2,transf=transf.ilogit.int,
                    at=(c(-1,0,0.002,0.5,1,500)),digits = 3,)


# 剔除小样本研究 -----------------------------------------------------------------
proportion$NumFlu <- as.numeric(proportion$NumFlu)
proportion$NumCo <- as.numeric(proportion$NumCo)
prop_rma1<-rma.glmm(measure = "PLO",xi = NumCo, ni = NumFlu, 
                    data = filter(proportion, Overall_age == "1",NumFlu > 50), 
                    add = 0.005, to = "only0", method = "ML",model="CM.EL",
                    slab = paste(StudyID, FirstAuthor, PublishYear))
prop_forest<-forest(prop_rma1, header = T,xlim=c(-5,5),alim=c(0,2),
                    cex=0.5, cex.lab = 1.0,cex.axis = 1,
                    refline = 0.2,transf=transf.ilogit.int,
                    at=(c(-1,0,0.002,0.5,1,500)),digits = 3,)

