library(dplyr)
library(reshape2)
library(stringr)
library(quanteda)
library(lawstat)
library(emmeans)
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# load sentiment disctionary
load('../res/sentiWords-db.RDS')
# custom plot function
abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")

df <- read.csv('../input/Preregistered+Social+Rules_September+24,+2020_10.16.csv', stringsAsFactors = F)
df <- as_tibble(df)
df <- df[-c(1:2),]
df <- as_tibble(df)
df <- mutate(df, id = 1:nrow(df))
df <- rename(df, CompProb = Q161)
names(df)
#df[, grepl('DO', names(df))]

# extracting item order
dfx <- df[,grepl('^FL_4(7|8)', names(df), perl = T)]
dfx_grid <- df[,grepl('^id$|^FL_403', names(df))]
dfx_grid <- melt(dfx_grid, id.vars = 'id', variable.name = 'item', value.name = 'item_name')
dfx_grid <- mutate(dfx_grid, item = gsub('\\_DO', '', item))
dfx <- mutate(dfx, id = 1:nrow(dfx))
dfx <- melt(dfx, id.vars = 'id', value.name = 'item_order')
dfx <- filter(dfx, !item_order=='')
dfx <- select(dfx, -variable)
dfx <- mutate(dfx, item_order = tolower(item_order))

# collapsing data to long format
df <- df[,c(20:43, length(df))]
df <- melt(df, id.vars = 'id', variable.name = 'item')
df <- mutate(df, value = as.numeric(value))
#df <- mutate(df, item = gsub('Eval', '', item))
df <- filter(df, !is.na(value))
table(table(df$id)==4)
df <- mutate(df, group = gsub('prob', '', tolower(item)),
             group = case_when(
               group == 'manip' ~ 'manipulative',
               group == 'comp' ~ 'compassionate',
               TRUE ~ group
             ))
df <- mutate(df, prob = as.numeric(grepl('Prob', item)))

# combine data
df <- left_join(df, dfx)

# apply item order
df <- str_split(df$item_order, '\\|') %>% 
  do.call(rbind, .) %>%
  as_tibble %>% 
  mutate(id = df$id) %>% 
  filter(!duplicated(id)) %>% 
  melt(., id.vars = 'id', value.name = 'group', variable.name = 'item_step') %>% 
  as_tibble %>% 
  left_join(df, .)
table(table(df$id)==4)

# annotate corpus
toks <- unique(df$group)
toks <- tokens(toks)
annot <- tokens_lookup(toks, dictionary = sentiWords$dichot)
toks <- tibble(group = unlist(toks), polarity = unlist(annot)) %>% 
  mutate(polarity = factor(polarity, levels = c('positive', 'negative')))
table(toks$polarity)==6
df <- left_join(df, toks)
df <- mutate(df, item_step = factor(item_step, levels = c('V1', 'V2')))
df <- mutate(df, domain = case_when(
  group %in% c('virtuous', 'vicious') ~ 'virtous/vicious',
  group %in% c('generous', 'selfish') ~ 'generous/selfish',
  group %in% c('compassionate', 'cruel') ~ 'compassionate/cruel',
  group %in% c('courageous', 'reckless') ~ 'courageous/reckless',
  group %in% c('honest', 'manipulative') ~ 'honest/manipulative',
  group %in% c('friendly', 'rude') ~ 'friendly/rude'
))
table(df$domain)

save(df, file = '../output/data/4-social-norms.RDS', compress = 'gzip')

### H1
# It is more acceptable to use positive thick terms without intending to communicate the evaluation THAN
#  it is to use negative thick terms without intending to communicate the evaluation

h1 <- df %>% filter(prob == 0)
h1_V1 <- h1 %>% filter(item_step == 'V1')
#h1_V2 <- h1 %>% filter(item_step == 'V2')
nrow(h1_V1)==nrow(df)/4

## are there outliers?
outliers <- boxplot(h1_V1$value, plot=FALSE)$out
outliers
# no outliers
## test ANOVA assumptions
# homogeneity of variances
lawstat::levene.test(h1$value, h1$polarity, location = 'mean', trim.alpha=0.25, correction.method = 'correction.factor')
lawstat::levene.test(h1_V1$value, h1_V1$polarity, location = 'mean', trim.alpha=0.25, correction.method = 'correction.factor')
# PERFECT!
# normality
# global
shapiro.test(h1$value)
shapiro.test(h1_V1$value)
# conclusion: non-parametric tests are not needed

# testing
m1 <- aov(value ~ polarity, data = h1_V1)
summary(m1)

t.test(h1_V1$value[h1_V1$polarity == 'positive'], h1_V1$value[h1_V1$polarity == 'negative'], alternative = 'less')

# are there order effects
m2 <- aov(value ~ polarity*item_step, data = h1)
summary(m2)
# order effects are not significant on 0.05-level

t.test(h1$value[h1$polarity == 'positive'], h1$value[h1$polarity == 'negative'], alternative = 'less')

### H1 cannot be rejected!!!

### H2
# It is more problematic to get a wrong impression of a person if the impression is wrongly negative, compared to when it is wrongly positive

h2 <- df %>% filter(prob == 1)
h2_V1 <- h2 %>% filter(item_step == 'V1')
#h1_V2 <- h1 %>% filter(item_step == 'V2')
nrow(h2_V1)==nrow(df)/4

## are there outliers?
outliers <- boxplot(h2_V1$value, plot=FALSE)$out
outliers
# yes there are outliers
## test ANOVA assumptions
# homogeneity of variances
lawstat::levene.test(h2$value, h2$polarity, location = 'mean', trim.alpha=0.25, correction.method = 'correction.factor')
lawstat::levene.test(h2_V1$value, h2_V1$polarity, location = 'mean', trim.alpha=0.25, correction.method = 'correction.factor')
# PERFECT!
# normality
# global
shapiro.test(h2$value)
shapiro.test(h2_V1$value)
# conclusion: non-parametric tests are not needed

# testing
m1 <- aov(value ~ polarity, data = h2_V1)
summary(m1)

t.test(h1_V1$value[h1_V1$polarity == 'positive'], h1_V1$value[h1_V1$polarity == 'negative'], alternative = 'less')

# are there order effects
m2 <- aov(value ~ polarity*item_step, data = h1)
summary(m2)
# order effects are not significant on 0.05-level

t.test(h1$value[h1$polarity == 'positive'], h1$value[h1$polarity == 'negative'], alternative = 'less')

### H2 cannot be rejected!!!

##### Differences between the domains
m3 <- aov(value ~ polarity*domain + item_step, data = h1)
summary(m3)
TukeyHSD(m3, which = "domain")
TukeyHSD(m3, which = "polarity")
TukeyHSD(m3)
fittedMeans <- emmeans(m3, ~ polarity|domain)
pairs(fittedMeans)
contrast(fittedMeans)
# the estimated means fit the TukeyHSD results


