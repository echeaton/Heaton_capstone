---
title: "IBS 538 2020 Capstone"
author: "Liz Heaton"
date: "4/28/2020"
output: html_document
---
### Background 

**Background and Significance.** Many neuropsychiatric diseases, including substance use disorder, are characterized, in part, by a failure to flexibly adapt to an ever-changing environment. The dorsomedial striatum (DMS) is essential for maintaining goal-directed behavior – it is active during new task learning, and it is re-engaged when habits are “broken”. However, the neuronal factors underlying an organism’s ability to flexibly toggle between goal-directed and habit-based behavior are not completely understood. Recent experiments revealed that lower protein levels of the melanocortin-4 receptor (MC4R) in the DMS are associated with behavioral flexibility (breaking habits) during an operant conditioning task. The key outcome of this task is that animals with lower response rates are considered more "goal directed", while animals with higher response rates are considered more "habitual".


**It is unknown whether selectively reducing Mc4r expression in the DMS will be sufficient to faciliate goal directed behavior (decrease response rates during an operant conditioning task).**

#

**Prediction.** If we selectively reduce Mc4r in the DMS, then animals' responses during an operant conditioning task will differ compared to control animals.

#

**Variables.** The dependent variable will be response rate (nose pokes per min) during an operant conditioning task. Response rate is a continuous measure. Lower response rates are indicative of more "goal directed" behavior, while higher response rates are indicative of more "habitual" behavior. Each subject is the experimental unit.

The independent variable is surgery treatment into the DMS. Surgery treatment is a discrete variable with three levels: control without surgery, surgery with vehicle infusion, and surgery with Mc4r knockdown infusion.

#

**Hypothesis.** Where $\mu$ represents the response rate of the populations corresponding to the samples groups, the null and alternate hypotheses are $H_0:\mu_{knockdown}=\mu_{control}$ and $H_1: \mu_{knockdown}\ne\mu_{control}$.

#

**Statistical Test.** The statistical test I would use to test my hypothesis is one-way completely randomized ANOVA. This dataset has one predictor variable: surgery condition. This variable is completely randomized: animals are randomly assigned to a surgical condition. The one-way completely randomized ANOVA will successfully meet the goal of determining if response rate differes between our three surgical conditions during an operant conditioning task.

#

**Procedure.** Equal numbers of male and female mice will be randomly assigned to one of three surgical conditions. Mice that are slated for surgery will undergo surgery at or around postnatal day 56. Behavioral testing in a operant conditioning task will occur at or around postnatal day 77. The primary output of this task is a "nose poke", where the animal literally sticks its nose into a recess in a box to receive a food reward. Responses are collected across a 25 minute period. We interpret animals with increased rates of nose poke responses as being more "habitual", while animals with decreased response rates are considered more "goal directed." 

**Decision Rules.** The sample size will be based on a power of 90%, which means that the tolerance level for type2 error will be 10%. The decision threshold for type1 error will be 5%. Thus, the null hypothesis will be rejected at a p-value of less than 0.05.

**Independent Replicate.** Each mouse in a group will be an independent replicate. I will take care to use an outbred mouse strain so that my mice are not genetically identical.


### Graph and dataMaker 

```{r message=FALSE, warning=FALSE}
#libraries
library(ez)
library(tidyverse)

#set values
a = 100 #expected basal response rate
b = 1.1 #expected fold-to-basal effect of surgery alone
c = 0.5 #minimal scientifically relevant fold-to-basal effect of Mc4r knockdown

sd = 25 #expected standard deviation of Outcome variable
n = 5 # number of independent replicates per group
sims = 100 #number of Monte Carlo simulations to run. 

#datamaker
dataMaker <- function(n, control, vehicle, knockdown, sd) { 
  
  
  control <- rnorm(n, a, sd) #basal effect
  vehicle <- rnorm(n, (a*b), sd) #effect of surgery without knockdown
  knockdown <- rnorm(n, (a*c), sd) #knockdown effect
    
    Outcome <- c(control, vehicle, knockdown)
    Predictor <- c(rep(c("control", "vehicle", "knockdown"), each = n))
    ID <- as.factor(c(1:length(Predictor)))
    df <-data.frame(ID, Predictor, Outcome)
    }

dat <- dataMaker(n,a, b, c,sd)

#plot 
ggplot(dat, aes(Predictor, Outcome)) +
  geom_jitter(width=0.15, size=4) +
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult=1),
               geom="crossbar",
               width=0.2,
               color="red"
               )+
  labs(x="surgical condition", y = "response rate (nose pokes / min)")



```


### Monte Carlo

```{r message=FALSE}
#monte carlo one-way ANOVA
pval <- replicate(
  sims, {
 
    sample.df <- dataMaker(n, a,b,c, sd)
    
    sim.ezaov <- ezANOVA(
            data = sample.df, 
            wid = ID,
            dv = Outcome,
            between = Predictor,
            type = 2
            )
  
  pval <- sim.ezaov$ANOVA[1,5]
    
    }
  )

pwr.pct <- sum(pval<0.05)/sims*100
paste(pwr.pct, sep="", "% power. Change 'n' in your initializer for higher or lower power.")

```


