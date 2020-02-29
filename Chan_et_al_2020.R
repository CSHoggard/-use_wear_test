### TITLE: TOWARDS AN UNDERSTANDING OF RETOUCH FLAKES: A USE-WEAR BLIND TEST ON KNAPPED STONE MICRODEBITAGE
### LEAD AUTHOR: BEN CHAN (B.CHAN@SOTON.AC.UK)

### ABSTRACT ###
### THE ANALYSIS OF LITHIC MICRODEBITAGE FORMS PART OF A BROADER FIELD OF MICROARTEFACT 
### STUDIES WHICH IS PRIMARILY DIRECTED TOWARDS THE IDENTIFICATION OF ACTIVITY AREAS 
### WITHIN ARCHAEOLOGICAL SITES. ANALYTICAL APPROACHES TO MICRODEBITAGE, INCLUDING RETOUCH 
### FLAKES FROM THE PRODUCTION AND MAINTENANCE OF KNAPPED STONE TOOLS, INVOLVE THE MAPPING 
### OF MICROARTEFACT DENSITIES, AND THE CREATION OF TECHNO-TYPOLOGIES TO CHARACTERISE THE 
### FORM OF RETOUCH FLAKES FROM DIFFERENT TYPES OF TOOLS. WHILST USE-WEAR ANALYSIS IS A 
### COMMON APPROACH TO COMPLETE TOOLS, IT HAS BEEN APPLIED MUCH LESS COMMONLY TO THE 
### RETOUCH, OR RESHARPENING, FLAKES PRODUCED DURING THEIR MAINTENANCE. THIS PAPER 
### CONTENDS THAT THIS TYPE OF ANALYSIS HOLDS GREAT POTENTIAL IN IDENTIFYING ACTIVITY 
### AREAS ON ARCHAEOLOGICAL SITE, REPRESENTING A RELATIVELY UNEXPLORED ANALYTICAL RESOURCES 
### WITHIN MICROARTEFACT ASSEMBLAGES. IN ORDER TO TEST THE RANGE OF FACTORS THAT AFFECT 
### THE IDENTIFICATION OF USE-WEAR TRACES ON SMALL RETOUCH FLAKES, A BLIND TEST CONSISTING 
### OF 40 RETOUCH FLAKES WAS CONDUCTED. THE RESULTS SHOW THAT WEAR TRACES CAN BE IDENTIFIED 
### WITH COMPARABLE LEVELS ACCURACY TO THOSE REPORTED FOR HISTORIC BLIND TESTS OF STANDARD 
### LITHIC TOOLS SUGGESTING THAT THE USE-WEAR ANALYSIS OF RETOUCH FLAKES CAN BE A USEFUL 
### ANALYTICAL TOOL IN UNDERSTANDING SITE FUNCTION. 

### SCRIPT AUTHOR: CHRISTIAN STEVEN HOGGARD ###
### SCRIPT CONTACT: C.S.HOGGARD@SOTON.AC.UK / CHRISTIANHOGGARD@GMAIL.COM ###
### LAST EDITED: 21/10/2019 ###

### SYSTEM INFORMATION ###
### R version 3.6.1 (2019-07-05) ###
### Platform: x86_64-w64-mingw32/x64 (64-bit) ###
### Running under: Windows 10 x64 (build 18362) ###

if(!require("tidyverse")) install.packages('tidyverse', repos='http://cran.us.r-project.org') ### tidyverse 1.2.1
if(!require("rms")) install.packages('rms', repos='http://cran.us.r-project.org') ### rms 5.1-3.1
if(!require("ggpubr")) install.packages('ggpubr', repos='http://cran.us.r-project.org') ### ggpubr 0.2.3

dataset <- as.data.frame(readxl::read_excel("Chan_et_al_2020_1.xlsx", col_names = TRUE)) ### load the data frame
compdata1 <- read.csv("Chan_et_al_2020_2.csv", header = T) ### load the comparative dataset
compdata2 <- read.csv("Chan_et_al_2020_3.csv", header = T) ### load the comparative dataset (transformed)

df1 <- select(dataset, "classification_id1" = UCT1, W)
df2 <- select(dataset, "classification_id2" = UCT2, W)
df3 <- select(dataset, "classification_id3" = UCT3, W)

df1$classification_id1 <- as.factor(df1$classification_id1)
df2$classification_id2 <- as.factor(df2$classification_id2)
df3$classification_id3 <- as.factor(df3$classification_id3)

fig1a <- ggplot(df1, aes(classification_id1, W)) + geom_jitter(width = 0.15, size = 2, aes(colour = classification_id1)) + ylim(0,1) + labs(x = "Classification", y = "Weight (g)") + scale_colour_manual(values=c("#56B4E9", "#E69F00")) + theme_minimal() + theme(legend.position = "none")
fig1b <- ggplot(df2, aes(classification_id2, W)) + geom_jitter(width = 0.15, size = 2, aes(colour = classification_id2)) + ylim(0,1) + labs(x = "Classification", y = "Weight (g)") + scale_colour_manual(values=c("#56B4E9", "#E69F00")) + theme_minimal() + theme(legend.position = "none")
fig1c <- ggplot(df3, aes(classification_id3, W)) + geom_jitter(width = 0.15, size = 2, aes(colour = classification_id3)) + ylim(0,1) + labs(x = "Classification", y = "Weight (g)") + scale_colour_manual(values=c("#56B4E9", "#E69F00")) + theme_minimal() + theme(legend.position = "none")
ggarrange(fig1a, fig1b, fig1c, labels = c("B1", "B2", "B3"), ncol = 1, font.label = list(size = 7))
ggsave("Figure_1.tiff", plot = last_plot(), width = 120, height = 160, units = "mm")

glm1 <- glm(classification_id1 ~ W, df1, family = binomial) ### generalised linear model (binomial)
glm2 <- glm(classification_id2 ~ W, df2, family = binomial) ### generalised linear model (binomial)
glm3 <- glm(classification_id3 ~ W, df3, family = binomial) ### generalised linear model (binomial)

summary(glm1) ### summary (inc. parameter signifiance)
summary(glm2) ### summary (inc. parameter signifiance)
summary(glm3) ### summary (inc. parameter signifiance)

fig2a <- ggplot(df1, aes(x=W, y=as.numeric(df1$classification_id1) - 1)) + geom_point(size = 2, aes(colour = classification_id1)) + stat_smooth(method="glm", se=TRUE, colour = "dark grey", method.args = list(family=binomial)) +  labs(x = "Weight (g)", y = "Classification") + scale_x_continuous(breaks=c(0,0.25,0.5,0.75)) + scale_y_continuous(breaks=c(0,1)) + theme_minimal() + theme(legend.position = "none") + scale_colour_manual(values=c("#56B4E9", "#E69F00"))
fig2b <- ggplot(df2, aes(x=W, y=as.numeric(df2$classification_id2) - 1)) + geom_point(size = 2, aes(colour = classification_id2)) + stat_smooth(method="glm", se=TRUE, colour = "dark grey", method.args = list(family=binomial)) +  labs(x = "Weight (g)", y = "Classification") + scale_x_continuous(breaks=c(0,0.25,0.5,0.75)) + scale_y_continuous(breaks=c(0,1)) + theme_minimal() + theme(legend.position = "none") + scale_colour_manual(values=c("#56B4E9", "#E69F00"))
fig2c <- ggplot(df3, aes(x=W, y=as.numeric(df3$classification_id3) - 1)) + geom_point(size = 2, aes(colour = classification_id3)) + stat_smooth(method="glm", se=TRUE, colour = "dark grey", method.args = list(family=binomial)) +  labs(x = "Weight (g)", y = "Classification") + scale_x_continuous(breaks=c(0,0.25,0.5,0.75)) + scale_y_continuous(breaks=c(0,1)) + theme_minimal() + theme(legend.position = "none") + scale_colour_manual(values=c("#56B4E9", "#E69F00"))
figure_2 <-ggarrange(fig2a, fig2b, fig2c, labels = c("B1", "B2", "B3"), nrow = 2, ncol = 2, font.label = list(size = 8))
annotate_figure(figure_2, bottom = text_grob("0: Correct  1: Incorrect", color = "black", face = "bold", size = 10))
ggsave("Figure_2.tiff", width = 180, height = 180, units = "mm")

glm1$null.deviance ### null deviance value (log-likelihood values)
glm1$deviance ### deviance value (log-likelihood values)
glm2$null.deviance ### null deviance value (log-likelihood values)
glm2$deviance ### deviance value (log-likelihood values)
glm3$null.deviance ### null deviance value (log-likelihood values)
glm3$deviance ### deviance value (log-likelihood values)

modelchi1 <- glm1$null.deviance - glm1$deviance ### model chi square value (csv)
modelchi2 <- glm2$null.deviance - glm2$deviance ### model chi square value (csv)
modelchi3 <- glm3$null.deviance - glm3$deviance ### model chi square value (csv)
modelchi1 ### call csv
modelchi2 ### call csv
modelchi3 ### call csv

pseudo.r2.gml1 <- modelchi1 / glm1$null.deviance ### pseudo r-square (Hosmer and Lemeshow r-square)
pseudo.r2.gml2 <- modelchi2 / glm2$null.deviance ### pseudo r-square (Hosmer and Lemeshow r-square)
pseudo.r2.gml3 <- modelchi3 / glm3$null.deviance ### pseudo r-square (Hosmer and Lemeshow r-square)

chisq.prob.1 <- 1 - pchisq(modelchi1, (glm1$df.null - glm1$df.residual)) ### pseudo p value (likelihood ratio p-value: model significance)
chisq.prob.2 <- 1 - pchisq(modelchi2, (glm2$df.null - glm2$df.residual)) ### pseudo p value (likelihood ratio p-value: model significance)
chisq.prob.3 <- 1 - pchisq(modelchi3, (glm3$df.null - glm3$df.residual)) ### pseudo p value (likelihood ratio p-value: model significance)

chisq.test(compdata1$TS, compdata1$APS)

figure_3 <- ggplot(compdata2, aes(Contact.Material, Percentage, fill = Study)) + geom_bar(stat="identity", color="black", position=position_dodge()) + theme_minimal() + ylim(0, 60) + scale_fill_brewer(palette="Paired") + labs(y = "Percent of correct responses (%)") + geom_text(aes(label=Percentage), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)
figure_3
ggsave("Figure_3.tiff", width = 120, height = 120, units = "mm")
