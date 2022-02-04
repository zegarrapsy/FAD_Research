#--------------------------------------------/ Demographic associated factors of depression in 
#--------------------------------------------/ Peru during the COVID-19 pandemic: An exploratory analysis
#--------------------------------------------/ Data selection procedures

#--------------------------------------------/ Settings
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
options(scipen = 999)

#--------------------------------------------/ Packages
library(tidyverse)
library(psych)
library(TAM)
library(haven)
library(openxlsx)
library(lavaan)
library(effectsize)
library(lme4)

#--------------------------------------------/ Health Questionnaire data
data <- read_sav("Data/depression_data.sav") 

#--------------------------------------------/ Stage 1: Symptom frequencies
#------------------/ Estimating frequencies
items <- data %>% select(starts_with("phq"))
items <- items[,order(colnames(items))]
items[items>0]<-1
items <- bind_cols(lapply(items,function(x) prop.table(table(x)))) %>% mutate(cat = c(0, 1))
items <- pivot_longer(items,cols=starts_with("phq"))
items <- items %>% mutate(value = as.numeric(value)) %>% 
                   mutate(p = ifelse(cat==0,value*-1*100,value*100))

#------------------/ Item labels
items_lab <- c("Thoughts that you would be better off dead or of hurting
                yourself in some way",
               "Moving or speaking so slowly that other people could have
                noticed? Or the opposite — being so fidgety or restless
                that you have been moving around a lot more than usual",
               "Trouble concentrating on things, such as reading the
                newspaper or watching television",
               "Feeling bad about yourself — or that you are a failure or
                have let yourself or your family down",
               "Poor appetite or overeating",
               "Feeling tired or having little energy",
               "Trouble falling or staying asleep, or sleeping too much",
               "Feeling down, depressed, or hopeless",
               "Little interest or pleasure in doing things")

#------------------/ Divergent stacked bar plot
graph <-
  ggplot(items, aes(fill=factor(cat,levels = c(0,1)),label = paste0(sprintf("%.1f",round(value*100,1)),"%"), y=p, x=reorder(name, desc(name)))) +
  geom_hline(yintercept = seq(-100,100,by=25), color = "#E5E5E5")+
  geom_bar(position="stack", stat="identity")+
  coord_flip()+theme_bw()+
  scale_y_continuous(limits=c(-100,100),breaks = seq(-100,100,by=25),labels = paste0(abs(seq(-100,100,by=25)),"%"))+
  scale_x_discrete(labels = items_lab)+
  xlab("")+ylab("")+
  scale_fill_manual(values=c("#C5D3DB","#59A7D4"),
                    breaks=c(0,1),
                    labels = c("Not at all",
                               "Several days to nearly every day"))+
  geom_text(size=2.5,position = position_stack(vjust = .5))+
  theme(legend.position="top",
        legend.title = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,-10,0),
        legend.justification="right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1,1,1,-30))+
  guides(fill = guide_legend(override.aes = list(size = 5)))

#------------------/ Export graph
ggsave("Results/stage1_item_frequencies.png",graph,width = 12.5,height = 5.5,dpi=300)

#--------------------------------------------/ Stage 2: Depression severity
#--------------------/ Variables and levels 
covariates <- c("sexo","neduca_cat","casado","iri_q","area","n_per_cat","edad_cat","region2_cat","lengua_cat","seguro")
labs <- list()
labs[[1]] <- c("Men", "Women")
labs[[2]] <- c("Primary school","Secondary School","Higher education (Non-university)","Higher education (University)")
labs[[3]] <- c("Married/Live-in", "Single/Divorced/Widowed")
labs[[4]] <- c("Low", "Lower-middle","Middle","Upper-middle","High")
labs[[5]] <- c("Urban","Rural")
labs[[6]] <- c("Living alone", "Between 2 and 4","More than 4")
labs[[7]] <- c("15 - 24", "25 - 39","40 - 59","\u2265 60")
labs[[8]] <- c("Coast","Highlands","Jungle")
labs[[9]] <- c("Spanish", "Native language")
labs[[10]] <- c("Has health insurance", "Does not have health insurance")

#--------------------/ Frequencies
covf <- list()
covf[[1]] <- paste0(table(data$category)," (",sprintf("%.1f",round(prop.table(table(data$category))*100,1)),"%)")
covf[[1]] <- data.frame(t(covf[[1]])) %>% rename(None = 1, Mild = 2, Moderate = 3, Moderately_severe = 4, Severe = 5) %>% 
             mutate(np = paste0(nrow(data)," (",sprintf("%.1f",round(nrow(data)/nrow(data)*100,1)),"%)"), strata = "National", level = "-") 

for (k in 1:length(covariates)) {
datx <- split(data,data[,covariates[k]])
freq <- list()
  for (i in 1:length(datx)) {
    freq[[i]] <- paste0(table(datx[[i]]$category)," (",sprintf("%.1f",round(prop.table(table(datx[[i]]$category))*100,1)),"%)")
    freq[[i]] <- data.frame(t(freq[[i]])) %>% rename(None = 1, Mild = 2, Moderate = 3, Moderately_severe = 4, Severe = 5) %>% 
                 mutate(np = paste0(nrow(datx[[i]])," (",sprintf("%.1f",round(nrow(datx[[i]])/nrow(data)*100,1)),"%)"))
  }
covf[[k+1]] <- bind_rows(freq) %>% mutate(strata = covariates[k], level = labs[[k]])
}

covf <- bind_rows(covf) %>% select(strata,level,np,everything())
write.xlsx(covf,"Results/stage2_depression_severity.xlsx",overwrite = T)

#--------------------------------------------/ Stage 3: Mean differences
#--------------------/ Rasch scaling
items <- data %>% select(starts_with("phq9"))
items <- items[,order(colnames(items))]

model <- tam.mml(as.matrix(items), verbose = FALSE, irtmodel="RSM")
data$measure_depre <- as.numeric(scale(model$person$EAP))

#--------------------/ Unidimensionality, reliability
mod <- "depre =~ phq9_01 + phq9_02 + phq9_03 + phq9_04 + phq9_05 + phq9_06 +
                 phq9_07 + phq9_08 + phq9_09"
mod <- cfa(mod,data=items,ordered = names(items),estimator="WLSMV")
summary(mod,fit.measures=TRUE)
round(model$EAP.rel, 3)

#--------------------/ Thresholds and item fit
tam.threshold(model, prob.lvl=0.5)
fit <- tam.fit(model)$itemfit
range(fit$Infit[1:9])
range(fit$Outfit[1:9])

#-------------------/ Mean differences
results <- list()
for (i in 1:length(covariates)) {
  results[[i]] <- bind_rows(unclass(describeBy(data$measure_depre,data[,covariates[i]])))
  results[[i]] <- data.frame(results[[i]]) %>% mutate(grupo = paste0(covariates[i],1:nrow(results[[i]])), nivel = labs[[i]])
}

final <- bind_rows(results) %>% mutate(ic_low = mean-se*qt(0.975,df=n-1)) %>% 
         mutate(ic_top = mean+se*qt(0.975,df=n-1))
#$write.xlsx(final,"stage_3descriptivos_rasch.xlsx",overwrite = T)
final$nivel <- factor(final$nivel,levels=unlist(labs))
graph<-
  final %>% 
  ggplot(aes(x=factor(nivel),y=mean)) + 
  geom_hline(yintercept = 0,linetype="dashed",color="#86CDED")+
  geom_point(color="#009FE6")+
  scale_y_continuous(limits=c(-0.6,0.7), breaks=unique(c(seq(-0.6,0,by=.1),seq(0,0.6,by=.1))),expand = c(0.02, 0.02))  + theme_bw()+
  geom_errorbar(aes(ymin=ic_low, ymax=ic_top), width=.5,color="#009FE6")+#+facet_wrap(~Indicador)+
  theme(legend.position = "bottom")+
  #theme(strip.background =element_rect(fill="#00559D"))+
  #theme(strip.text = element_text(colour = 'white'))+
  #theme(axis.ticks.x = element_blank())+
  #theme(axis.text.x = element_blank())+
  ylab("Depression measure")+xlab("")+
  geom_vline(xintercept = c(2.5,6.5,9.5,14.5,16.5,19.5,21.5,25.5,27.5),color="#333333")+
  #  guides(color=FALSE)+
  theme(panel.grid = element_blank())+
  geom_hline(yintercept = .65)+
  scale_x_discrete(expand = c(0.025, 0.025))+
  #  geom_text(aes(label=nivel),hjust=-0, vjust=0,size=2)+
    annotate("text", x =1, y = final$ic_top[1]+.02, label = "Men",size=2)+
    annotate("text", x =2, y = final$ic_top[2]+.02, label = "Women",size=2)+
    annotate("text", x =3, y = final$ic_top[3]+.028, label = "Primary\nschool",size=2,lineheight=.8)+
    annotate("text", x =4, y = final$ic_top[4]+.028, label = "Secondary\nschool",size=2,lineheight=.8)+
    annotate("text", x =5, y = final$ic_top[5]+.028, label = "Higher\n(n-u)",size=2,lineheight=.8)+
    annotate("text", x =6, y = final$ic_top[6]+.028, label = "Higher\n(u)",size=2,lineheight=.8)+
    annotate("text", x =7, y = final$ic_top[7]+.02, label = "Coast",size=2,lineheight=.8)+
    annotate("text", x =8, y = final$ic_top[8]+.02, label = "Highlands",size=2,lineheight=.8)+
    annotate("text", x =9, y = final$ic_top[9]+.02, label = "Jungle",size=2,lineheight=.8)+
    annotate("text", x =10, y = final$ic_top[10]+.02, label = "Low",size=2,lineheight=.8)+
    annotate("text", x =11, y = final$ic_top[11]+.028, label = "Lower-\nmiddle",size=2,lineheight=.8)+
    annotate("text", x =12, y = final$ic_top[12]+.02, label = "Middle",size=2,lineheight=.8)+
    annotate("text", x =13, y = final$ic_top[13]+.028, label = "Higher-\nmiddle",size=2,lineheight=.8)+
    annotate("text", x =14, y = final$ic_top[14]+.02, label = "High",size=2,lineheight=.8)+
    annotate("text", x =15, y = final$ic_top[15]+.02, label = "Urban",size=2,lineheight=.8)+
    annotate("text", x =16, y = final$ic_top[16]+.02, label = "Rural",size=2,lineheight=.8)+
    annotate("text", x =17, y = final$ic_top[17]+.028, label = "Living\nalone",size=2,lineheight=.8)+
    annotate("text", x =18, y = final$ic_top[18]+.028, label = "Between\n2 and 4",size=2,lineheight=.8)+
    annotate("text", x =19, y = final$ic_top[19]+.028, label = "More\nthan 4",size=2,lineheight=.8)+
    annotate("text", x =20, y = final$ic_top[20]+.028, label = "Married\nLive-in",size=2,lineheight=.8)+
    annotate("text", x =21, y = final$ic_top[21]+.038, label = "Single\nDivorced\nWidowed",size=2,lineheight=.8)+
    annotate("text", x =22, y = final$ic_top[22]+.02, label = "15-24",size=2,lineheight=.8)+
    annotate("text", x =23, y = final$ic_top[23]+.02, label = "25-39",size=2,lineheight=.8)+
    annotate("text", x =24, y = final$ic_top[24]+.02, label = "40-59",size=2,lineheight=.8)+
    annotate("text", x =25, y = final$ic_top[25]+.02, label = "\u2265 60",size=2,lineheight=.8)+
    annotate("text", x =26, y = final$ic_top[26]+.02, label = "Spanish",size=2,lineheight=.8)+
    annotate("text", x =27, y = final$ic_top[27]+.028, label = "Native\nlanguage",size=2,lineheight=.8)+
    annotate("text", x =28, y = final$ic_top[28]+.02, label = "Insured",size=2,lineheight=.8)+
    annotate("text", x =29, y = final$ic_top[29]+.028, label = "Non-\ninsured",size=2,lineheight=.8)+
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  annotate("text", x =1.5, y = 0.7, label = "Gender",size=3,lineheight=.8,vjust=.5)+
  annotate("text", x =4.5, y = 0.7, label = "Education level",size=3,lineheight=.8,vjust=.5)+
  annotate("text", x =8, y = 0.7, label = "Region",size=3,lineheight=.8,vjust=.5)+
  annotate("text", x =12, y = 0.7, label = "Wealth index",size=3,lineheight=.8,vjust=.5)+
  annotate("text", x =15.5, y = 0.7, label = "Area",size=3,lineheight=.8,vjust=.5)+
  annotate("text", x =18, y = 0.7, label = "Coresidents",size=3,lineheight=.8,vjust=.5)+
  annotate("text", x =20.5, y = 0.7, label = "Marital\nstatus",size=3,lineheight=.8,vjust=.5)+
  annotate("text", x =23.5, y = 0.7, label = "Age",size=3,lineheight=.8,vjust=.5)+
  annotate("text", x =26.5, y = 0.7, label = "First\nlanguage",size=3,lineheight=.8,vjust=.5)+
  annotate("text", x =28.5, y = 0.7, label = "Health\ninsurance",size=3,lineheight=.8,vjust=.5)

ggsave("Results/stage3_mean_comparisson.png",graph,height = 10,width = 14,dpi=300)

#-------------------/ inferential tests and effect sizes
#sexo
t.test(measure_depre ~ factor(sexo),data=data)
unlist(cohens_d(measure_depre ~ sexo,data=data))

#neduca
educa <- aov(measure_depre ~ factor(neduca_cat), data = data)
summary(educa)
unlist(omega_squared(educa,ci=0.95))
TukeyHSD(educa)
unlist(cohen.d(measure_depre ~ neduca_cat,data=data[data$neduca_cat==1|data$neduca_cat==2,]))[2]
unlist(cohen.d(measure_depre ~ neduca_cat,data=data[data$neduca_cat==1|data$neduca_cat==3,]))[2]
unlist(cohen.d(measure_depre ~ neduca_cat,data=data[data$neduca_cat==1|data$neduca_cat==4,]))[2]

unlist(cohen.d(measure_depre ~ neduca_cat,data=data[data$neduca_cat==2|data$neduca_cat==3,]))[2]
unlist(cohen.d(measure_depre ~ neduca_cat,data=data[data$neduca_cat==2|data$neduca_cat==4,]))[2]

unlist(cohen.d(measure_depre ~ neduca_cat,data=data[data$neduca_cat==3|data$neduca_cat==4,]))[2]


#region
reg <- aov(measure_depre ~ factor(region2_cat), data = data)
summary(reg)
unlist(omega_squared(reg,ci=0.95))
TukeyHSD(reg)
unlist(cohen.d(measure_depre ~ region2_cat,data=data[data$region2_cat==2|data$region2_cat==1,]))[2]
unlist(cohen.d(measure_depre ~ region2_cat,data=data[data$region2_cat==3|data$region2_cat==2,]))[2]
unlist(cohen.d(measure_depre ~ region2_cat,data=data[data$region2_cat==1|data$region2_cat==3,]))[2]

##wealth
welath <- aov(measure_depre ~ factor(iri_q), data = data)
summary(welath)
unlist(omega_squared(welath,ci=0.95))
TukeyHSD(welath)
unlist(cohen.d(measure_depre ~ iri_q,data=data[data$iri_q==5|data$iri_q==1,]))[2]
unlist(cohen.d(measure_depre ~ iri_q,data=data[data$iri_q==5|data$iri_q==2,]))[2]
unlist(cohen.d(measure_depre ~ iri_q,data=data[data$iri_q==5|data$iri_q==3,]))[2]
unlist(cohen.d(measure_depre ~ iri_q,data=data[data$iri_q==5|data$iri_q==4,]))[2]


#area
t.test(measure_depre ~ factor(area),data=data)
unlist(cohens_d(measure_depre ~ area,data=data))

##nperr
nper <- aov(measure_depre ~ factor(n_per_cat), data = data)
summary(nper)
unlist(omega_squared(nper,ci=0.95))
TukeyHSD(nper)
unlist(cohen.d(measure_depre ~ n_per_cat,data=data[data$n_per_cat==1|data$n_per_cat==2,]))[c(2)]
unlist(cohen.d(measure_depre ~ n_per_cat,data=data[data$n_per_cat==1|data$n_per_cat==3,]))[c(2)]
unlist(cohen.d(measure_depre ~ n_per_cat,data=data[data$n_per_cat==2|data$n_per_cat==3,]))[c(2)]

#Marital
t.test(measure_depre ~ factor(casado),data=data)
unlist(cohens_d(measure_depre ~ factor(casado),data=data))

##age
age <- aov(measure_depre ~ factor(edad_cat), data = data)
summary(age)
unlist(omega_squared(age,ci=0.95))
TukeyHSD(age)

unlist(cohen.d(measure_depre ~ edad_cat,data=data[data$edad_cat==1|data$edad_cat==4,]))[c(2)]
unlist(cohen.d(measure_depre ~ edad_cat,data=data[data$edad_cat==2|data$edad_cat==4,]))[c(2)]
unlist(cohen.d(measure_depre ~ edad_cat,data=data[data$edad_cat==3|data$edad_cat==4,]))[c(2)]


unlist(cohen.d(measure_depre ~ edad_cat,data=data[data$edad_cat==1|data$edad_cat==3,]))[c(2)]
unlist(cohen.d(measure_depre ~ edad_cat,data=data[data$edad_cat==2|data$edad_cat==3,]))[c(2)]

unlist(cohen.d(measure_depre ~ edad_cat,data=data[data$edad_cat==1|data$edad_cat==2,]))[c(2)]


#language
t.test(measure_depre ~ factor(lengua_cat),data=data)
unlist(cohens_d(measure_depre ~ factor(lengua_cat),data=data))

#insurance
t.test(measure_depre ~ factor(seguro),data=data)
unlist(cohens_d(measure_depre ~ factor(seguro),data=data))

#--------------------------------------------/ Stage 4: Model
#--------------------/ model
library(lmerTest)

data$sexo <- factor(data$sexo)
data$neduca_cat <- factor(data$neduca_cat)
data$region2_cat <- factor(data$region2_cat)
data$casado <- factor(data$casado)
data$lengua_cat <- factor(data$lengua_cat)
data$seguro <- factor(data$seguro)
data$region <- factor(data$region)
mod <- 
lmer( measure_depre~(1|region) + sexo + neduca_cat+ region2_cat + iri_s + n_per + casado + edad + lengua_cat + seguro, data=data)
res <- summary(mod)
library(MuMIn)
r.squaredGLMM(mod)

write.csv(res$coefficients,"Results/stage4_coefficients.csv")

