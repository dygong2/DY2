---
title: 'Exploration 2: Engaging with Alternative Explanations with By Matched Stratification'
author: "Jake Bowers"
date: '`r format(Sys.Date(), "%B %d, %Y")`'
output:
  pdf_document:
    number_sections: true
    fig_caption: yes
    fig_height: 8
    fig_width: 8
    latex_engine: xelatex
    citation_package: biblatex
    keep_tex: true
geometry: "left=1.25in,right=1.25in,top=1in,bottom=1in"
graphics: yes
mainfont: "Helvetica"
fontsize: 11pt
bibliography: classbib.bib
biblio-style: "authoryear-comp,natbib"
---

<!-- Make this document using library(rmarkdown); render("exploration1.Rmd") -->
\input{mytexsymbols}


```{r setup, echo=FALSE, results=FALSE, include=FALSE, cache=FALSE}
library(here)
source(here("rmd_setup.R"))
```

```{r loadlibs, echo=FALSE, include=FALSE, results=FALSE}
library(tidyverse)
library(coin)
library(DeclareDesign)
```

"Hey data scientist!" The voice on the phone is chipper. "I am involved in a
~~hearts and minds~~ anti-crime campaign for the peaceful and helpful United
Nations now. I ran across this dataset and thought that it might teach me
about whether I should fund public transportation to be ~~re~~built in order
to decrease violence. I'm sending you the description and the code. I just
can't get the code to work at all. Also, even if I could get it to work, I
wouldn't know how to interpret any of it. Can you please help? Does
infrastructure investment like this seem to decrease violence? Or produce
other social goods? Here is what I found out."

> In 2004 the municipality of Medell\'{i}n, Columbia built built the first line
 of the Metrocable --- a set of cable cars that connected poor neighborhoods
 on the edges of the city to the center of the city \autocite{cerda2012reducing}.
 Professor Magdalena Cerda and her collaborators asked whether this kind
 of integration could improve life in these poor (and heretofore violent)
 neighborhoods. We ~~extracted~~ were given some of the data from this project to use
 here.\footnote{The articles can be both found in this web directory
 \url{http://jakebowers.org/Matching/}.}

```{r}
library(MASS)
library(RItools)
library(optmatch)
## install.packages("remotes")
remotes::install_github("lmiratrix/blkvar")
library(blkvar)
library(arm)
load(url("http://jakebowers.org/Data/meddat.rda"))
```


> The data Cerd\'{a} collected tell us about the roughly `r nrow(meddat)`
neighborhoods in the study, `r signif(sum(meddat$nhTrt),2)` of which had
access to the Metrocable line and `r signif(sum(1-meddat$nhTrt),2)` did not.
> We don't have a formal codebook. Here are some guesses about the meanings of
some of the variables. There are more variables in the data file than those
listed here.
```
## The Intervention
nhTrt        Intervention neighborhood (0=no Metrocable station, 1=Metrocable station)
## Some Covariates (there are others, see the paper itself)
nh03         Neighborhood id
nhGroup      Treatment (T) or Control (C)
nhTrt        Treatment (1) or Control (0)
nhHom        Mean homicide rate per 100,000 population in 2003
nhDistCenter Distance to city center (km)
nhLogHom     Log Homicide (i.e. log(nhHom))
## Outcomes (BE03,CE03,PV03,QP03,TP03 are baseline versions)
BE      Neighborhood amenities Score 2008
CE      Collective Efficacy Score 2008
PV      Perceived Violence Score 2008
QP      Trust in local agencies Score 2008
TP      Reliance on police Score 2008
hom     Homicide rate per 100,000 population Score 2008-2003 (in log odds)
HomCount2003 Number of homicides in 2003
Pop2003      Population in 2003
HomCount2008 Number of homicides in 2008
Pop2008      Population in 2008
```


```{r}
## These next are equivalent ways to get rates per 1000 from counts
## meddat$HomRate03<-with(meddat, (HomCount2003/Pop2003)*1000)
## meddat$HomRate08<-with(meddat, (HomCount2008/Pop2008)*1000)
meddat <- transform(meddat, HomRate03 = (HomCount2003 / Pop2003) * 1000)
meddat <- transform(meddat, HomRate08 = (HomCount2008 / Pop2008) * 1000)
```

> First we did this:
```{r}
covadjfmla <- reformulate(c("nhTrt", names(meddat)[c(5:7, 9:24)], "HomRate03"), response = "HomRate08")
lm1 <- lm(covadjfmla, data = meddat)
```

> But then people yelled at us! They all yelled a different reason why this was the wrong approach. Can you tell me at least two or three different problems with adjusting for covariates in this way? Also, can you interpret the focal coefficient here (the one on `nhTrt`) in substantive terms?
> So, to avoid more yelling, we decided to actually "hold constant". We tried to make a matched design to counter alternative explanations for the intervention-versus-non-intervention comparison. We have `nhTrt` as our intervention or "treatment variable" (even though this is certainly not an experiment) and things measured in 2008 as outcomes with things measured in 2003 as occuring before the Metrocable was built. The other variables, measured before the treatment are plausibly covariates.
> To make things simple at first we decided to control for only one variable --- baseline homicides --- and we had two choices that we show below. What are the trade-offs between these two? Can you interpret how they each "control" for baseline homicides in different ways? Which should we use and why? After doing the reading (below) I was worried about interpolation, extrapolation, the curse of dimensionality, influential points and correct functional form (but not worried any kind of collinearity or overfitting since we are only trying to remove the effects of a single variable). I was also very confused about what `xBalance` was doing. Can you explain? And I didn't know how to understand the fact that `fullmatch` was producing sets with a varying number of controls and even treated observations --- of course I was only skimming the reading. Maybe you can help me?
```{r}
lm2 <- lm(HomRate08 ~ nhTrt + HomRate03, data = meddat)
coef(lm2)
## Residuals plots?
## Influential points like Cook's Distance?
## Other functions of HomRate03? Should we use gam(...,family=gaussian)? Or just ns() for splines?
## Scalar distance on baseline outcome
tmp <- meddat$HomRate03
names(tmp) <- rownames(meddat)
absdist <- match_on(tmp, z = meddat$nhTrt)
fm0 <- fullmatch(absdist, data = meddat, min.controls = .5)
summary(fm0)
meddat$fm0 <- fm0
## Did we reduce differences between neighrborhoods, on average, in baseline homicide rates?
xb0a <- xBalance(nhTrt ~ HomRate03,
  strata = list(raw = NULL, fm0 = ~fm0),
  data = meddat,
  report = c(
    "std.diffs", "z.scores", "adj.means",
    "adj.mean.diffs", "chisquare.test", "p.values"
  )
)
## Here we just look at each matched set directly
xb0a
meddat %>%
  group_by(fm0) %>%
  summarize(diff = mean(HomRate03[nhTrt == 1]) - mean(HomRate03[nhTrt == 0]), n = n())
```

> So that created the sets. This next are different ways to calculate the average difference in homicides between neighborhoods after removing the effects of the matched strata or set. The Rosenbaum reading does something different which we could also try here.
```{r}
## Here are the differences in outcome within each set. We need to combine them somehow, with some weighting depending on the set size, at least.
calc_summary_stats(Yobs=meddat$HomRate08,Z=meddat$nhTrt,B=fm0,data=meddat)
## This uses a precision weighting --- set size plus how lopsided is the treated-to-control ratio within each set
res_fm0_lm_HC2 <- lm_robust(HomRate08 ~ nhTrt, fixed_effects = ~fm0, data = meddat) ##HC2 SE
coef(res_fm0_lm_HC2)
## This does the same thing but different approach to p-values
res_fm0_xb <- xBalance(nhTrt~HomRate08,strata=list(fm0=~fm0),data=meddat,report="all")
res_fm0_xb$results[1,1:3,]
## This uses an approach to weighting that is new and cool --- see the Pashley piece
res_fm0_bv <- block_estimator(Yobs=meddat$HomRate08,Z=meddat$nhTrt,B=fm0,data=meddat,
    method="hybrid_p")
res_fm0_bv$ATE_hat
## You can look at other variance estimators
##compare_methods(Yobs=meddat$HomRate08,Z=meddat$nhTrt,B=fm0,data=meddat,include_MLM=FALSE,include_DB=FALSE,include_method_characteristics=TRUE)
## calc_summary_stats(Yobs=meddat$HomRate08,Z=meddat$nhTrt,B=fm0,data=meddat)
```

> Next, people yelled a bit about how I was only controlling for one thing. So, I tried to use the matching approach to the `lm1` model above but collapsing down all of the variables into a couple of scores --- the propensity score and a Mahalanobis distance score.
```{r find_design}
## Some commands like a formula object:
balfmla <- reformulate(c(names(meddat)[c(5:7, 9:24)], "HomRate03"), response = "nhTrt")
xb0 <- xBalance(balfmla,
  strata = list(raw = NULL),
  data = meddat,
  report = c(
    "std.diffs", "z.scores", "adj.means",
    "adj.mean.diffs", "chisquare.test", "p.values"
  )
)
## Ordinary Propensity score
### Why do these two glm models differ? Why use two of them? What is going on with glm1 here? What is separation in logistic regression models? Why should we care?
glm1 <- glm(balfmla, data = meddat, family = binomial)
glm2 <- bayesglm(balfmla, data = meddat, family = binomial)
## Add scores back to data
meddat$pscore <- predict(glm2,type="link") ## linear.predictors not probs
## Make distance matrices
psdist <- match_on(nhTrt ~ pscore, data = meddat)
## Look at a bit of this distance matrix
as.matrix(psdist)[1:5, 1:5]
## Rank-Based Mahalanobis distance (Rosenbaum, Chap 8)
mhdist <- match_on(balfmla, data = meddat, method = "rank_mahalanobis")
## Do it
fm1 <- fullmatch(mhdist, data = meddat) ## , min.controls=1) # min.controls=.5
summary(fm1, data = meddat, min.controls = 0, max.controls = Inf)
## Add matched set indicators back to data
meddat$fm1 <- NULL
meddat[names(fm1), "fm1"] <- fm1
## We have to show that we have adjusted enough. Did we adjust enough?
xb1 <- xBalance(balfmla,
  strata = list(raw = NULL, fm1 = ~fm1),
  data = meddat,
  report = c(
    "std.diffs", "z.scores", "adj.means",
    "adj.mean.diffs", "chisquare.test", "p.values"
  )
)
xb1$overall
## What is the biggest difference within set.
diffswithinsets <- meddat %>%
  group_by(fm1) %>%
  summarize(meandiff = mean(HomRate03[nhTrt == 1]) - mean(HomRate03[nhTrt == 0]),
            nb=n())
summary(diffswithinsets$meandiff)
## Which set is the biggest diff? Which neighborhoods are these?
bigdiff <- diffswithinsets[which.max(diffswithinsets$meandiff), ]
meddat[meddat$fm1 == bigdiff$fm1, ]
## An actual randomized experiment
meddat$fakez <- sample(meddat$nhTrt)
xbFun <- xBalance(update(balfmla,fakez~.),
  strata = list(raw = NULL),
  data = meddat,
  report = "all")
## Diff pre-matching
with(meddat, mean(HomRate03[nhTrt == 1]) - mean(HomRate03[nhTrt == 0]))
## What are the distances like?
quantile(as.vector(absdist), seq(0, 1, .1))
## CALIPERS! (What is going on here?)
caldist <- mhdist + caliper(absdist, 1) ## + caliper(blah,10)
as.matrix(absdist)[1:5, 1:5]
as.matrix(mhdist)[1:5, 1:5]
as.matrix(caldist)[1:5, 1:5]
quantile(as.vector(mhdist), seq(0, 1, .1))
fm2 <- fullmatch(psdist + caliper(absdist, 2) + caliper(mhdist, 50), data = meddat, tol = .00001, min.controls = 1)
summary(fm2)
meddat$fm2 <- NULL
meddat[names(fm2), "fm2"] <- fm2
xb2 <- xBalance(balfmla,
  strata = list(raw = NULL, fm1 = ~fm1, fm2 = ~fm2),
  data = meddat,
  report = c(
    "std.diffs", "z.scores", "adj.means",
    "adj.mean.diffs", "chisquare.test", "p.values"
  )
)
xb2$overall
xb2$results["HomRate03", , ]
set.seed(12345)
meddat$fakeZ <- sample(meddat$nhTrt)
xbFake <- xBalance(update(balfmla, fakeZ ~ .),
  strata = list(raw = NULL),
  data = meddat,
  report = c(
    "std.diffs", "z.scores", "adj.means",
    "adj.mean.diffs", "chisquare.test", "p.values"
  )
)
xbFake$results
xbFake$overall
```

> And this is what the outcome analysis looked like
```{r outcome_analysis}
outcome1 <- xBalance(nhTrt ~ HomRate08,
  strata = list(raw = NULL, fm1 = ~fm1, fm2 = ~fm2),
  data = meddat,
  report = c(
    "std.diffs", "z.scores", "adj.means",
    "adj.mean.diffs", "chisquare.test", "p.values"
  )
)
outcome1$results
lm3b <- lm_robust(HomRate08 ~ nhTrt, data = meddat)
lm3a <- lm_robust(HomRate08 ~ nhTrt, fixed_effects = ~fm1, data = meddat, subset = !is.na(meddat$fm1))
lm3 <- lm_robust(HomRate08 ~ nhTrt, fixed_effects = ~fm2, data = meddat, subset = !is.na(meddat$fm2))
coef(lm3)["nhTrt"]
## What is the biggest difference within set.
outcomeres <- meddat %>%
  filter(!is.na(fm2)) %>%
  group_by(fm2) %>%
  summarize(meandiff = mean(HomRate08[nhTrt == 1]) - mean(HomRate08[nhTrt == 0]),
            nb=n(),
            probt=mean(nhTrt))
outcomeres$bsizewt <- outcomeres$nb/sum(outcomeres$nb)
outcomeres$pwt <- with(outcomeres, probt*(1-probt)*bsizewt)
# Set nt   nc  blocksize_wt1  precision_wt2
# A   100 100   .5             .5+
# B    1   99   .5             .5-
## Trying it by hand
with(outcomeres,sum(meandiff*pwt/sum(pwt)))
## Compare to "fixed effects"
lm3
with(outcomeres,sum(meandiff*bsizewt))
res_fm1_xb <- xBalance(nhTrt~HomRate08,strata=list(fm0=~fm2),data=meddat,report="all")
res_fm1_xb$results[1,1:3,]
res_fm1_bv <- block_estimator(Yobs=meddat$HomRate08,Z=meddat$nhTrt,B=fm2,data=meddat,
    method="hybrid_p")
res_fm1_bv$ATE_hat
compare_methods(Yobs=HomRate08,Z=nhTrt,B=fm2,data=meddat[!is.na(meddat$fm2),])
```

Useful reading:
 - *\citealp[Chap 21.4]{fox2008applied} explains about bootstrap hypothesis tests (i.e. sampling model justified hypothesis tests).

 - \citealp[Chap 1,3,7,8,9,13]{rosenbaum2010design}  (\url{http://www.springerlink.com/content/978-1-4419-1212-1/contents/})

 - \citealp[Chap 9 and 10]{gelman2007dau} (on causal inference and the problems of interpolation and extrapolation)

 - \citealp{hans:04} on full matching for adjustment

 - \citealp{hansen2008cbs} on assessing balance.

 - \citealp{pashley2020blocked} for a discussion of different weighting approaches when you have strata of different sizes.

## Specification Curves

```{r speccurves}
library(specr)
library(MASS)
results <- run_specs(df = meddat, 
                     y = c("HomRate08"), 
                     x = c("nhTrt"), 
                     model = c("lm","rlm"), 
                     controls = all.vars(balfmla)[-1])
```

# References
