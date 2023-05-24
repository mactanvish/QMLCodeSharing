# 第一次分析请下好所需package----------
# 在版本R version 4.2.0下进行分析
install.packages('metafor')
install.packages("readxl")
{
library(metafor)
library(forestplot)
library(ggplot2)
library(tidyverse)
library(readxl)
}

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
sev_comorbidities<-arrange(sev_comorbidities, sev_comorbidities$'First author')
proportion$NumFlu <- as.numeric(proportion$NumFlu)
proportion$NumCo <- as.numeric(proportion$NumCo)
}

# Study<-filter(Study, Study$Whether_included == "y") #筛选
# Study<-Study[ ,c("StudyID","First_author" ,"Publish_year" ,"Title" , "Study_period" ,"before",                                                            
#                  "during" ,"after" ,"Study_design"  )]##仅留下指定列
# Study_setting<-Study[ , c("StudyID","Setting" )]
# proportion_setting<-filter(proportion, proportion$Overall_age == "1")[ , c("StudyID","setting" )]
# View(cbind(proportion_setting,Study_setting))


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
}


#funnel plot漏斗图-----------------
# 保存漏斗图
png(filename = "fp1.png",width = 1500,height = 1500,units = "px",res = 250)
fp1 <- funnel(prop_rma1,xlab = "Prevalence",atransf=transf.ilogit.int)
dev.off()
ranktest(prop_rma1) # 定量漏斗图对称性检验，如果tau较高说明不对称，存在发表偏倚

# escalc函数来计算库克距离cook's distance判断强影响点
prop <- proportion2[,c("StudyID","NumFlu","NumCo")]
prop$NumFlu <- as.numeric(prop$NumFlu)
prop$NumCo <- as.numeric(prop$NumCo)
dat <- prop
dat <- escalc(measure = "PR",xi = NumCo,ni = NumFlu,data = dat,add = 0)
res <- rma(yi,vi,data = dat)
inf <- influence(res)
plot(inf) # 绘图查看异常点
prop[42,]# paper131为强影响
prop[56,]# paper185为强影响且总病例数少于50

#sensitivity analysis敏感性分析--------
#去除强影响paper185绘制森林图查看meta analysis后的合并结果是否发生明显改变----------
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
# 按照年龄分亚组subgorup analysis：<18y, >=18y,未分组的 -------
age_group <- function(age_id) {
  ai <- age_id
  if (ai == "1") {xxlab <- "prop under 18"}
  if (ai == "2") {xxlab <- "prop over 18"}
  if (ai == "3") {xxlab <- "unstratified age group"}
  propUnder18_rma<-rma.glmm(measure = "PLO", 
                            xi = NumCo, ni = NumFlu, 
                            data = filter(proportion, age_num == age_id),add = 0.005,
                            to = "only0",method = "ML",
                            slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  propUnder18_forest<-forest(propUnder18_rma, header = T,digits = 3,
                             alim=c(0,2),#change the position of middle line
                             cex=1, cex.lab = 1.0,cex.axis = 1,refline = 0.15,
                             transf=transf.ilogit.int,at=(c(0,1)),xlab = xxlab)
}
age_group(age_id = "1")# 18岁以下
age_group(age_id = "2")# 18岁以上
age_group(age_id = "3")# unstratified年龄组
# 组间统计学差异分析
all_age <- proportion[which(proportion$Overall_age == 1),]#以63篇研究本体
all_age <- all_age[,c("age_num","NumFlu","NumCo")]
all_age$age_num[is.na(all_age$age_num)==TRUE] <- 3
all_age$age_num <- factor(all_age$age_num)
prop_rma1<-rma.glmm(measure = "PLO", xi = NumCo, ni = NumFlu,data = all_age, 
                    add = 0.5,to = "only0", method = "ML",model="CM.EL",mods = ~relevel(age_num,ref = 2))
prop_rma1 # 组间无差异

# 按照阶段：大流行前、期间、之后、未分组的 ---------------------------------------------------------
# 数据处理
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
# 各流行阶段meta结果和森林图
fun_period <- function(period_id) {
  pp <- period_id
  if (pp == 1) {xxlab <- "大流行前"}
  if (pp == 2) {xxlab <- "大流行期间"}
  if (pp == 3) {xxlab <- "大流行之后"}
  if (pp == 4) {xxlab <- "unstratified大流行"}
  prop3pro<-rma.glmm(measure = "PLO",
                     xi = NumCo, ni = NumFlu,
                     data = filter(prop3, period == period_id),add = 0.005, 
                     to = "only0",method = "ML",
                     slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  forest(prop3pro, header = T,
         cex=0.8, cex.lab = 1.0,cex.axis = 1,refline = 0.15,digits = 3,
         transf=transf.ilogit.int,at=(c(0,1)),xlab = xxlab)
}
fun_period(period_id = 1) # 大流行前
fun_period(period_id = 2)# 大流行期间
fun_period(period_id = 3)# 大流行之后
fun_period(period_id = 4)# unstratified大流行

# 按照研究类型分亚组: 前瞻，回顾，前瞻和回顾 -----------------------------------------------------------------
# 数据处理
{
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
}
# 亚组统计学意义分析
prop_rma1<-rma.glmm(measure = "PLO", xi = NumCo, ni = NumFlu,data = p , 
                    add = 0.5,to = "only0", method = "ML",model="CM.EL",mods = ~relevel(Study_design,ref = 1))
prop_rma1
# 各研究类型meta结果
fun_study_type <- function(type) {
  t <- type
  if (t == "prospective") {xxlab <- "前瞻性研究proportion"}
  if (t == "retrospective") {xxlab <- "回顾性研究proportion"}
  if (t == "prospective and retrospective") {xxlab <- "同时包含前瞻和回顾proportion"}
  prop3pro<-rma.glmm(measure = "PLO",
                     xi = NumCo, ni = NumFlu,
                     data = filter(prop3, Study_design == type),add = 0.005, 
                     to = "only0",method = "ML",
                     slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  forest(prop3pro, header = T,
         cex=0.8, cex.lab = 1.0,cex.axis = 1,refline = 0.15,digits = 3,
         transf=transf.ilogit.int,at=(c(0,1)),xlab = xxlab)
}
fun_study_type(type = "prospective")# 前瞻
fun_study_type(type = "retrospective")# 回顾
fun_study_type(type = "prospective and retrospective")# 回顾和前瞻混合的研究


# 按照setting：ED, IP, ICU ---------------------------------------------------------------
# 数据处理
{
prop3 <- cbind(proportion2,Study$Study_design,Study$Setting,Study$`Influenza test method`, Study$`Bacterial confirmation`)
prop3 <- rename(prop3, c("Study_design" = "Study$Study_design","Setting" = "Study$Setting",
                         "flu_test"="Study$`Influenza test method`","bac_confirm"="Study$`Bacterial confirmation`"))
prop3$NumFlu <- as.numeric(prop3$NumFlu)
prop3$NumCo <- as.numeric(prop3$NumCo)

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
# 亚组差异分析
p$Setting <- factor(p$Setting)
prop_rma1<-rma.glmm(measure = "PLO", xi = NumCo, ni = NumFlu,data = p,
                    add = 0.5,to = "only0", method = "ML",model="CM.EL",
                    mods = ~relevel(Setting, ref = 2))
prop_rma1
prop_rma1<-rma.glmm(measure = "PLO", xi = NumCo, ni = NumFlu,data = p,
                    add = 0.5,to = "only0", method = "ML",model="CM.EL",
                    mods = Setting)
prop_rma1
# 各亚组meta结果分析
fun_setting <- function(st) {
  s <- st
  if (s == "ED") {xxlab <- "ED proportion"}
  if (s == "IP") {xxlab <- "IP proportion"}
  if (s == "ICU") {xxlab <- "ICU proportion"}
  prop3pro.re<-rma.glmm(measure = "PLO",
                        xi = NumCo, ni = NumFlu,
                        data = filter(prop3, Setting == st),add = 0.005, 
                        to = "only0",method = "ML",
                        slab = paste(StudyID, FirstAuthor, PublishYear,sep = ","))
  forest(prop3pro.re, header = T,digits = 3,
         transf=transf.ilogit.int,at=(c(0,1)),xlab = xxlab)
}
fun_setting(st = "ED")# ED
fun_setting(st = "IP")# IP
fun_setting(st = "ICU")# ICU
# 86 98 95

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


# 按照国家收入 ------------------------------------------------------------------

load("income_level.Rdata")
prop3 <- proportion2 %>% select(StudyID,NumFlu,NumCo)
prop3 <- data.frame(prop3)
prop3$NumFlu <- as.numeric(prop3$NumFlu)
prop3$NumCo <- as.numeric(prop3$NumCo)
prop3 <- prop3 %>% left_join(income_level)
# 高收入
prop_rma1<-rma.glmm(measure = "PLO",xi = NumCo, ni = NumFlu, 
                    data = filter(prop3, income_level == "h"), 
                    add = 0.005, to = "only0", method = "ML",model="CM.EL")
forest(prop_rma1, header = T,xlim=c(-5,5),alim=c(0,2),
       cex=0.5, cex.lab = 1.0,cex.axis = 1,
       refline = 0.2,transf=transf.ilogit.int,
       at=(c(-1,0,0.002,0.5,1,500)),digits = 3,)
# 低收入
prop_rma1<-rma.glmm(measure = "PLO",xi = NumCo, ni = NumFlu, 
                    data = filter(prop3, income_level != "h"), 
                    add = 0.005, to = "only0", method = "ML",model="CM.EL")
forest(prop_rma1, header = T,xlim=c(-5,5),alim=c(0,2),
       cex=0.5, cex.lab = 1.0,cex.axis = 1,
       refline = 0.2,transf=transf.ilogit.int,
       at=(c(-1,0,0.002,0.5,1,500)),digits = 3,)

# 剔除强影响点 ------------------------------------------------------------------
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

