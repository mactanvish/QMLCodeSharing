#####main analysis of severity
{
Severity<-read_xlsx("ExtractForm.xlsx", sheet = "Severity")
Severity <- arrange(Severity,Severity$'First author')
Severity$CNumOutcome <- as.numeric(Severity$CNumOutcome)
Severity$CNoOutcome <- as.numeric(Severity$CNoOutcome)
Severity$SNumOutcome <- as.numeric(Severity$SNumOutcome)
Severity$SNoOutcome <- as.numeric(Severity$SNoOutcome)
Severity <- rename(Severity,"First_author"="First author","Publish_year"="Publish year")
a<-read_xlsx("severity.xlsx", sheet = "death")#导入Excel表单
a$CNumOutcome <- as.numeric(a$CNumOutcome)
a$CNoOutcome <- as.numeric(a$CNoOutcome)
a$SNumOutcome <- as.numeric(a$SNumOutcome)
a$SNoOutcome <- as.numeric(a$SNoOutcome)
}
# death--------------------------------
SeDeath_rma<-rma.glmm(measure = "OR", 
                      ai = CNumOutcome,bi = CNoOutcome, 
                      ci = SNumOutcome,di = SNoOutcome,
                      data = a[which(a$setting == 1),],#[which(a$setting != 1),]
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int,xlab = "odds ratio",
                       refline = 1)#order = "obs",
# 主分析
a$period <- factor(a$period)
SeDeath_rma<-rma.glmm(measure = "OR", 
                      ai = CNumOutcome,bi = CNoOutcome, 
                      ci = SNumOutcome,di = SNoOutcome,
                      data = a,#[which(a$setting != 1),]
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int,xlab = "death OR",#
                       refline = 0)#绘制森林图并按照观察结果顺序排序order = "obs",
# ,refline = 1,xlim=c(0,10), alim=c(-40,20),at=c(0,1,3,7,100)
# 漏斗图，发表偏倚分析
funnel(SeDeath_rma)
ranktest(SeDeath_rma)

# 亚组分析,及分组因素是否有统计学差异
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, age == 1), #<18
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)#,transf=transf.exp.int
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, age == 2), #>18
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)#,transf=transf.exp.int
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, age == 3), #mixed age
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# 组间统计学差异分析
a$age <- factor(a$age)
prop_rma1<-rma.glmm(measure = "OR", ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                    data = a[which(a$setting != 1),], #a[which(a$setting != 1),] 排除ED
                    add = 0.5,to = "only0", method = "ML",model="UM.RS",mods = ~relevel(age,ref = 2))
prop_rma1

a <- a[which(a$setting != 1),]#排除ED的研究
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, period == 1), #before pandemic出不了结果
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, period == 2), #during pandemic
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, period == 3), #after pandemic
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, period == 4), #mixed preiods
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# 组间统计学差异分析
a<-read_xlsx("severity.xlsx", sheet = "death")#导入Excel表单
a <- a[which(a$setting != 1),]#排除ED的研究
a$period <- factor(a$period)
a$CNumOutcome <- as.numeric(a$CNumOutcome)
a$CNoOutcome <- as.numeric(a$CNoOutcome)
a$SNumOutcome <- as.numeric(a$SNumOutcome)
a$SNoOutcome <- as.numeric(a$SNoOutcome)
prop_rma1<-rma.glmm(measure = "OR", ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                    data = a, #[which(a$setting != 1),]
                    add = 0.005,to = "only0", method = "ML",model="UM.RS",mods = ~relevel(period,ref = 3))
prop_rma1

SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, design == 1), #prospective
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, design == 2), #retrospective
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, design == 3), #pro&retro
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# 组间统计学差异分析
a$design <- factor(a$design)
prop_rma1<-rma.glmm(measure = "OR", ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                    data = a, #a[which(a$setting != 1),] 排除ED
                    add = 0.5,to = "only0", method = "ML",model="UM.RS",mods = ~relevel(design,ref = 2))
prop_rma1

SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, setting == 1), #ED
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, setting == 2), #IP
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, setting == 3), #ICU
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = a[which(a$setting != 1),], #exclude ED
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# 组间统计学差异分析
a$setting <- factor(a$setting)
prop_rma1<-rma.glmm(measure = "OR", ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                    data = a, 
                    add = 0.5,to = "only0", method = "ML",model="UM.RS",mods = ~relevel(setting,ref = 2))
prop_rma1
a2 <- a[,c("FirstAuthor","SNumTotal" ,"SNumOutcome" ,"SNoOutcome" , "CNumTotal" ,"CNumOutcome",                                                            
            "CNoOutcome" ,"setting")]
# 敏感性分析
readClipboard()
a <- read.table("clipboard",sep = "\t",header = T)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, fluTest == "l"),# flu test
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeath_rma
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, bacConfir == "l"),# bacterial confirmation
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeath_rma
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, confoundingAdj == "l"),# confounding
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeath_rma
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# outlier
dat <- a
dat <- escalc(measure = "OR",data = dat,add = 0.5,to = "only0",
              ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome)
res <- rma(yi,vi,data = dat)
inf <- influence(res)
plot(inf)#paper98为强影响
SeDeath_rma<-rma.glmm(measure = "OR", 
                      ai = CNumOutcome,bi = CNoOutcome, 
                      ci = SNumOutcome,di = SNoOutcome,
                      data = filter(a, !StudyID == "paper98"),
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeath_rma
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)


# ICU admission--------------------------------
a <- filter(Severity, typeoutcome == "ICU",!SNumTotal == 25)
SeICU_rma<-rma.glmm(measure = "OR", 
                    ai = CNumOutcome,bi = CNoOutcome, 
                    ci = SNumOutcome,di = SNoOutcome,
                    data = a, #
                    add=0.5, to="only0",model = "UM.RS", method = "ML",
                    slab = paste(First_author,Publish_year,sep = ","))
SeICUForest<-forest(SeICU_rma, header = T,
                    xlim=c(-10,10), alim=c(-40,40),cex=1,transf=transf.exp.int,refline = 1,
                    at=c(0,1,5,10,15,20),
                    xlab = "Forest plot of the meta-estimate of the OR of ICU admission",
                    annotate = T)#图900*520
c1 <- a$CNumOutcome
text(24,7.6,c1,cex = 1.2)
# 漏斗图，发表偏倚分析
funnel(SeICU_rma)
ranktest(SeICU_rma)
# 敏感性分析,强影响点判断
a <- Severity[,c("StudyID","CNumOutcome","CNoOutcome","SNumOutcome","SNoOutcome")]
dat <- filter(a, typeoutcome == "ICU",!SNumTotal == 25)
dat <- escalc(measure = "OR",data = dat,add = 0.5,to = "only0",
              ai = CNumOutcome,bi = CNoOutcome, 
              ci = SNumOutcome,di = SNoOutcome)
res <- rma(yi,vi,data = dat)
inf <- influence(res)
plot(inf)
# 亚组分析,及分组因素是否有统计学差异
a<-read_xlsx("severity.xlsx", sheet = "ICU")#导入Excel表单

SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, age == 1), #<18
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, age == 2), #>18
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, age == 0), #mixed age
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# 组间统计学差异分析
prop_rma1<-rma.glmm(measure = "OR", ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,data = a, 
                    add = 0.5,to = "only0", method = "ML",model="UM.RS",mods = ~age)
prop_rma1

SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, period == 1), #before pandemic
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, period == 2), #during pandemic
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, period == 3), #after pandemic
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, period == 0), #mixed preiods
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.05, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# 组间统计学差异分析
prop_rma1<-rma.glmm(measure = "OR", ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,data = a, 
                    add = 0.5,to = "only0", method = "ML",model="UM.RS",mods = ~period)
prop_rma1

SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, design == 1), #prospective
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, design == 2), #retrospective
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, design == 3), #pro&retro
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# 组间统计学差异分析
prop_rma1<-rma.glmm(measure = "OR", ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,data = a, 
                    add = 0.5,to = "only0", method = "ML",model="UM.RS",mods = ~design)
prop_rma1

SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, setting == 1), #ED
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, setting == 2), #IP
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, setting == 3), #ICU
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# 组间统计学差异分析
prop_rma1<-rma.glmm(measure = "OR", ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,data = a, 
                    add = 0.5,to = "only0", method = "ML",model="UM.RS",mods = ~setting)
prop_rma1

# 敏感性分析
readClipboard()
a <- read.table("clipboard",sep = "\t",header = T)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, fluTest == "l"),# flu test
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeath_rma
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, bacConfir == "l"),# bacterial confirmation
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeath_rma
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, confoundingAdj == "l"),# confounding
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeath_rma
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# outlier
dat <- a
dat <- escalc(measure = "OR",data = dat,add = 0.5,to = "only0",
              ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome)
res <- rma(yi,vi,data = dat)
inf <- influence(res)
plot(inf)#无outlier
SeDeath_rma<-rma.glmm(measure = "OR", 
                      ai = CNumOutcome,bi = CNoOutcome, 
                      ci = SNumOutcome,di = SNoOutcome,
                      data = a,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeath_rma
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)

# MV--------------------------------
SeICU_rma<-rma.glmm(measure = "OR", 
                    ai = CNumOutcome,bi = CNoOutcome, 
                    ci = SNumOutcome,di = SNoOutcome,
                    data = filter(Severity, typeoutcome == "MV"), 
                    add=0.5, to="only0",model = "UM.RS", method = "ML",
                    slab = paste(First_author,Publish_year,sep = ","))
SeICUForest<-forest(SeICU_rma, header = T,
                    xlim=c(-10,10), alim=c(-40,40),cex=1.5,transf=transf.exp.int,refline = 1,
                    at=c(0,1,5,10,20),
                    xlab = "Forest plot of the meta-estimate of the OR of requirement of MV",
                    )#,at=c(0,1,3,7)
# 漏斗图，发表偏倚分析
funnel(SeICU_rma)
ranktest(SeICU_rma)
# 敏感性分析
a <- Severity[,c("StudyID","CNumOutcome","CNoOutcome","SNumOutcome","SNoOutcome")]
dat <- filter(a, typeoutcome == "MV")
dat <- escalc(measure = "OR",data = dat,add = 0.5,to = "only0",
              ai = CNumOutcome,bi = CNoOutcome, 
              ci = SNumOutcome,di = SNoOutcome)
res <- rma(yi,vi,data = dat)
inf <- influence(res)
plot(inf)#paper179
SeICU_rma<-rma.glmm(measure = "OR", 
                    ai = CNumOutcome,bi = CNoOutcome, 
                    ci = SNumOutcome,di = SNoOutcome,
                    data = filter(Severity, typeoutcome == "MV",!StudyID == "paper179"), 
                    add=0.5, to="only0",model = "UM.RS", method = "ML",
                    slab = paste(StudyID,First_author,sep = ","))
SeICUForest<-forest(SeICU_rma, header = T,
                    xlim=c(-10,10), alim=c(-5,10),cex=.8,transf=transf.exp.int,refline = 1,
                    at=c(0,1,3,7))#,at=c(0,1,3,7)
# 亚组分析,及分组因素是否有统计学差异
a<-read_xlsx("severity.xlsx", sheet = "MV")#导入Excel表单

SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, age == 1), #<18
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, age == 2), #>18
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, age == 0), #mixed age
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# 组间统计学差异分析
prop_rma1<-rma.glmm(measure = "OR", ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,data = a, 
                    add = 0.5,to = "only0", method = "ML",model="UM.RS",mods = ~age)
prop_rma1

SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, period == 1), #before pandemic
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, period == 2), #during pandemic
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, period == 3), #after pandemic
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, period == 0), #mixed preiods
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# 组间统计学差异分析
prop_rma1<-rma.glmm(measure = "OR", ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,data = a, 
                    add = 0.5,to = "only0", method = "ML",model="UM.RS",mods = ~period)
prop_rma1

SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, design == 1), #prospective
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, design == 2), #retrospective
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, design == 3), #pro&retro
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# 组间统计学差异分析
prop_rma1<-rma.glmm(measure = "OR", ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,data = a, 
                    add = 0.5,to = "only0", method = "ML",model="UM.RS",mods = ~design)
prop_rma1

SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, setting == 1), #ED
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, setting == 2), #IP
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, setting == 3), #ICU
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# 组间统计学差异分析
prop_rma1<-rma.glmm(measure = "OR", ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,data = a, 
                    add = 0.5,to = "only0", method = "ML",model="UM.RS",mods = ~setting)
prop_rma1

# 敏感性分析
readClipboard()
a <- read.table("clipboard",sep = "\t",header = T)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, fluTest == "l"),# flu test
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeath_rma
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, bacConfir == "l"),# bacterial confirmation
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeath_rma
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
SeDeath_rma<-rma.glmm(measure = "OR",data = filter(a, confoundingAdj == "l"),# confounding
                      ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome,
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeath_rma
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)
# outlier
dat <- a
dat <- escalc(measure = "OR",data = dat,add = 0.5,to = "only0",
              ai = CNumOutcome,bi = CNoOutcome,ci = SNumOutcome,di = SNoOutcome)
res <- rma(yi,vi,data = dat)
inf <- influence(res)
plot(inf)#paper179
SeDeath_rma<-rma.glmm(measure = "OR", 
                      ai = CNumOutcome,bi = CNoOutcome, 
                      ci = SNumOutcome,di = SNoOutcome,
                      data = filter(a,!StudyID == "paper179"),
                      add=0.5, to="only0",model = "UM.RS", method = "ML",
                      slab = paste(FirstAuthor,PublishYear,sep = ","))
SeDeath_rma
SeDeathForest1<-forest(SeDeath_rma, header = T,cex=.8,transf=transf.exp.int)

# LOS--------------------------------
a<-read_xlsx("severity.xlsx", sheet = "LOS")#导入Excel表单
a <- a[c(1:6),]
a <- as.data.frame(a)
for (i in 21:26) {               #2.迭代器
  a[,i] <- as.numeric(a[,i])         #3.循环体
}
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, 
                  sd1i = c_SD, sd2i = s_SD, data = a, measure = "MD", append = TRUE,)
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,slab = paste(a$FirstAuthor,a$PublishYear, sep = ", "),xlab = "LOS",digits = 1,
             alim = c(-50,40),at = c(-5,0 ,5,10,15,20),cex = 1.2)
text(-43,7.6,"Study",cex = 1.2)
text(24,7.6,"Mean Difference",cex = 1.2)

ma_model_1$pval
summary(ma_model_1)

# 漏斗图
funnel(ma_model_1)
ranktest(ma_model_1)
# 强影响点
inf <- influence(ma_model_1)
plot(inf)
# 亚组分析,及分组因素是否有统计学差异
readClipboard()
a <- read.table("clipboard",sep = "\t",header = T)
view(a)
table(a$age)
table(a$period)
table(a$design)
table(a$setting)
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #<18y
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,age == 1), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #>18y
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,age == 2), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #mixed
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,age == 0), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
# 组间统计学差异分析
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, 
                  sd1i = c_SD, sd2i = s_SD, data = a, measure = "MD", append = TRUE)
ma_model_1 <- rma(yi, vi, data = my_data,mods= ~age)
ma_model_1

my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #before
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,period == 1), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #during
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,period == 2), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #after
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,period == 3), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #mixed
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,period == 0), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
# 组间统计学差异分析
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, 
                  sd1i = c_SD, sd2i = s_SD, data = a, measure = "MD", append = TRUE)
ma_model_1 <- rma(yi, vi, data = my_data,mods= ~period)
ma_model_1

my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #pro
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,design == 1), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #re
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,design == 2), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #pro and re
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,design == 3), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
# 组间统计学差异分析
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, 
                  sd1i = c_SD, sd2i = s_SD, data = a, measure = "MD", append = TRUE)
ma_model_1 <- rma(yi, vi, data = my_data,mods= ~design)
ma_model_1

my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #ED
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,setting == 1), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #IP
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,setting == 2), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #ICU
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,setting == 3), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
# 组间统计学差异分析
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, 
                  sd1i = c_SD, sd2i = s_SD, data = a, measure = "MD", append = TRUE)
ma_model_1 <- rma(yi, vi, data = my_data,mods= ~setting)
ma_model_1
# 敏感性分析
readClipboard()
a <- read.table("clipboard",sep = "\t",header = T)
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #flu test
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,fluTest == "l"), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #bacterial confirmation
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,bacConfir  == "l"), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, #confounding
                  sd1i = c_SD, sd2i = s_SD, data = filter(a,confoundingAdj == "l"), measure = "MD", append = TRUE,
                  slab = paste(FirstAuthor,PublishYear, sep = ", "))
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,header = T)
# outlier
my_data <- escalc(n1i = CNumTotal, n2i = SNumTotal, m1i = c_mean, m2i = s_mean, 
                  sd1i = c_SD, sd2i = s_SD, data = a, measure = "MD", append = TRUE)
ma_model_1 <- rma(yi, vi, data = my_data)
p1 <- forest(ma_model_1,slab = paste(a$FirstAuthor,a$PublishYear, sep = ", "))
ma_model_1$pval
summary(ma_model_1)
inf <- influence(ma_model_1)
plot(inf)#无强影响


# 森林图优化 -------------------------------------------------------------------
{
readClipboard()
a <- read.table("clipboard",sep = "\t",header = T)
data <- a
}
# 添加角标
data$y <- c(1:20,NA)
data$slab.ref <- mapply(function(x,y) as.expression(bquote(.(x)^.(y))), data$Study, data$y)
## The rest of the columns in the table.
#death主图和亚组图
tabletext <- cbind(c(NA,"Study",data$Study),# Study 为顶端label，这样行数+1
                   c("No. of deaths for","influenza single-infection (%)",data$a),
                   c("No. of deaths for","bacterial co-infection (%)",data$b),
                   c(NA,"OR (95%CI)",data$OR..95.CI.))

## 定义亚组，方便后面线条区分
#subgps <- c(4,5,8,9,10,24)
#data$Study[subgps] <- paste("c4",data$Study[subgps])

f <- forestplot(labeltext=tabletext, graph.pos=4,
         mean=c(NA,NA,data$mean),#设置tabletext中额外添加的一个label值为NA
         lower=c(NA,NA,data$lower), upper=c(NA,NA,data$upper),
         #data[, c("mean", "lower", "upper")],
         #is.summary = c(TRUE, FALSE, FALSE),
          ##根据亚组的位置，设置线型，宽度造成“区块感”
          # hrzl_lines=list("3" = gpar(lwd=1, col="black")#上线
          #                 ,"23" = gpar(lwd=1, col="black")#death下线
          # #                 #"7" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),#增加区块阴影
          # #                 #"15" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
          # #                 #"26" = gpar(lwd=55, lineend="butt", columns=c(1:6), col="#99999922"),
          #                 ),
          #fpTxtGp函数中的cex参数设置各个组件的大小
          txt_gp=fpTxtGp(label=gpar(cex=0.7),#整体字号
                         ticks=gpar(cex=0.8),#标度尺字号
                         xlab=gpar(cex = 0.8),#横轴标签字号
                         title=gpar(cex = 1)),#标题字号
          ##fpColors函数设置颜色
         col=fpColors(box="black", lines="black", zero = "gray50"),
          #col=fpColors(box="#1c61b6", lines="#1c61b6", zero = "gray50"),#设置可信区间线条和箱子为特定颜色（此处为蓝色）
          #箱线图中基准线的位置
          zero=1,lineheight = "auto",#无效线位置和高度
         cex=0.5, 
          xlab = "OR (Log scale)",
         slab= "wsssssssw",
          #scale_x_continuous(trans='log10'),
          colgap=unit(6,"mm"),#各列之间的宽度
          #箱子大小，线的厚度
          lwd.ci=1, boxsize=0.3,
          #箱线图两端添加小竖线，高度
          ci.vertices=TRUE, ci.vertices.height = 0.2,
          lwd.xaxis=1,#坐标轴线的厚度
          #xticks=c(0.12,1,8,64),#death坐标轴取值
         xticks=c(0.1,1,10,100),#death time period坐标轴取值
         #xticks = my_xticks,
          graphwidth = unit(0.25,"npc"),#调整刻度的宽度
          xlog = TRUE,# 设置对数坐标轴
         clip = c(0.1,100)#限制显示的坐标轴，可信区间超出部分用箭头表示
          ,is.summary = c(T,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F)#death
         # ,is.summary = c(T,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F)#对相应行进行加粗
         #,is.summary = c(T,T,T,T,F,F,F,F,F,F,T,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F)#death time period
         #,is.summary = c(T,T,T,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,T,F,F)#death study design
         #,is.summary = c(T,T,T,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F)#death setting
)
png(filename = "death.png",width = 2500,height = 1200,units = "px",res = 250)
f
dev.off()

#ICU主图
{
readClipboard()
a <- read.table("clipboard",sep = "\t",header = T)
data <- a
}
## The rest of the columns in the table.
tabletext <- cbind(c(NA,"Study",data$Study),#Study为顶端label，这样行数+1
                   c("No. of single influenza","infection admitted to ICU (%)",data$a),
                   c("No. of influenza-bacterial","co-infection admitted to ICU (%)",data$b),
                   c(NA,"OR (95%CI)",data$OR..95.CI.))

f <- forestplot(labeltext=tabletext, graph.pos=4,
           mean=c(NA,NA,data$mean),#设置tabletext中额外添加的一个label值为NA
           lower=c(NA,NA,data$lower), upper=c(NA,NA,data$upper),
           ##根据亚组的位置，设置线型，宽度造成“区块感”
           # hrzl_lines=list("3" = gpar(lwd=1, col="black")#上线
           #                 ,"11" = gpar(lwd=1, col="black")#下线
           #                 #"7" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),#增加区块阴影
           #                 #"15" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
           #                 #"26" = gpar(lwd=55, lineend="butt", columns=c(1:6), col="#99999922"),
           # ),
           #fpTxtGp函数中的cex参数设置各个组件的大小
           txt_gp=fpTxtGp(label=gpar(cex=0.7),#整体字号
                          ticks=gpar(cex=0.7),#标度尺字号
                          xlab=gpar(cex = 0.8),#横轴标签字号
                          title=gpar(cex = 1)),#标题字号
           col=fpColors(box="black", lines="black", zero = "gray50"),##fpColors函数设置颜色
           #col=fpColors(box="#1c61b6", lines="#1c61b6", zero = "gray50"),#设置可信区间线条和箱子为特定颜色（此处为蓝色）         
           zero=1, cex=0.5, lineheight = "auto",#箱线图中无效线的位置
           xlab = "OR (Log scale)",
           colgap=unit(6,"mm"),#各列之间的宽度
           lwd.ci=1, boxsize=0.2,#箱子大小，线的厚度
           ci.vertices=TRUE, ci.vertices.height = 0.1,#箱线图两端添加小竖线，高度
           lwd.xaxis=1,#坐标轴线的厚度
           xticks=c(0.1,1,10,100),#death time period坐标轴取值
           graphwidth = unit(0.25,"npc"),#调整刻度的宽度
           xlog = TRUE# 设置对数坐标轴
           ,clip = c(0.12,32)#限制显示的坐标轴，可信区间超出部分用箭头表示
           ,is.summary = c(T,T,F,F,F,F,F,F,F,F,T)
)
png(filename = "ICU2.png",width = 3500,height = 1000,units = "px",res = 250)
f
dev.off()

#MV主图
{
readClipboard()
a <- read.table("clipboard",sep = "\t",header = T)
data <- a
}
## The rest of the columns in the table.
tabletext <- cbind(c(NA,"Study",data$Study),#Study为顶端label，这样行数+1
                   c("No. of single influenza","infection requiring MV (%)",data$a),
                   c("No. of influenza-bacterial","co-infection requiring MV (%)",data$b),
                   c(NA,"OR (95%CI)",data$OR..95.CI.))

f <- forestplot(labeltext=tabletext, graph.pos=4,
           mean=c(NA,NA,data$mean),#设置tabletext中额外添加的一个label值为NA
           lower=c(NA,NA,data$lower), upper=c(NA,NA,data$upper),
           ##根据亚组的位置，设置线型，宽度造成“区块感”
           # hrzl_lines=list("3" = gpar(lwd=1, col="black")#上线
           #                 ,"11" = gpar(lwd=1, col="black")#下线
           #                 #"7" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),#增加区块阴影
           #                 #"15" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
           #                 #"26" = gpar(lwd=55, lineend="butt", columns=c(1:6), col="#99999922"),
           # ),
           #fpTxtGp函数中的cex参数设置各个组件的大小
           txt_gp=fpTxtGp(label=gpar(cex=0.7),#整体字号
                          ticks=gpar(cex=0.7),#标度尺字号
                          xlab=gpar(cex = 0.8),#横轴标签字号
                          title=gpar(cex = 1)),#标题字号
           col=fpColors(box="black", lines="black", zero = "gray50"),##fpColors函数设置颜色
           #col=fpColors(box="#1c61b6", lines="#1c61b6", zero = "gray50"),#设置可信区间线条和箱子为特定颜色（此处为蓝色）         
           zero=1, cex=0.5, lineheight = "auto",#箱线图中无效线的位置
           xlab = "OR (Log scale)",
           colgap=unit(6,"mm"),#各列之间的宽度
           lwd.ci=1, boxsize=0.2,#箱子大小，线的厚度
           ci.vertices=TRUE, ci.vertices.height = 0.1,#箱线图两端添加小竖线，高度
           lwd.xaxis=1,#坐标轴线的厚度
           xticks=c(0.1,1,10,100),#death time period坐标轴取值
           graphwidth = unit(0.25,"npc"),#调整刻度的宽度
           xlog = TRUE# 设置对数坐标轴
           ,clip = c(0.12,32)#限制显示的坐标轴，可信区间超出部分用箭头表示
           ,is.summary = c(T,T,F,F,F,F,F,F,F,F,T)
)
png(filename = "MV2.png",width = 3500,height = 1000,units = "px",res = 250)
f
dev.off()

#LOS主图
{
readClipboard()
a <- read.table("clipboard",sep = "\t",header = T)
data <- a
}
## The rest of the columns in the table.
tabletext <- cbind(c(NA,"Study",data$Study),#Study为顶端label，这样行数+1
                   c(NA,"Mean",data$Mean1),
                   c("Single infection        ","SD",data$SD1),
                   #c(NA,"Total",data$Total1),
                   c(NA,"Mean",data$Mean2),
                   c("Co-infection        ","SD",data$SD2),
                   #c(NA,"Total",data$Total2),
                   c(NA,"Mean difference (95%CI)",data$MD))

forestplot(labeltext=tabletext, graph.pos=6,
           mean=c(NA,NA,data$mean),#设置tabletext中额外添加的一个label值为NA
           lower=c(NA,NA,data$lower), upper=c(NA,NA,data$upper),
           ##根据亚组的位置，设置线型，宽度造成“区块感”
           hrzl_lines=list("3" = gpar(lwd=1, col="black")#上线
                           ,"8" = gpar(lwd=1, col="black")#下线
                           ),
           #fpTxtGp函数中的cex参数设置各个组件的大小
           txt_gp=fpTxtGp(label=gpar(cex=0.7),#整体字号
                          ticks=gpar(cex=0.6),#标度尺字号
                          xlab=gpar(cex = 0.8),#横轴标签字号
                          title=gpar(cex = 1)),#标题字号
           col=fpColors(box="black", lines="black", zero = "gray50"),##fpColors函数设置颜色
           zero=0, cex=0.5, lineheight = "auto",#箱线图中无效线的位置
           xlab = "Mean difference",
           colgap=unit(1,"mm"),#各列之间的宽度
           lwd.ci=1, boxsize=0.2,#箱子大小，线的厚度
           ci.vertices=TRUE, ci.vertices.height = 0.1,#箱线图两端添加小竖线，高度
           lwd.xaxis=1,#坐标轴线的厚度
           xticks=seq(from=-5,to=10,by=2.5),
           #xticks=c(-5,-2.5,0,2.5,5,7.5,10),#death time period坐标轴取值
           graphwidth = unit(0.2,"npc"),#调整刻度的宽度
           #xlog = TRUE# 设置对数坐标轴
           clip = c(-5,10),#限制显示的坐标轴，可信区间超出部分用箭头表示
           is.summary = c(T,T,F,F,F,F,F,T)
)
text(x = 3, y = 3, labels = "text")
plot(1:5, 1:5, xlim = c(0,6), ylim = c (0,6), type = "n")
text(x = c(3, 3), y = c(3, 5), labels = c("text", "text"))
