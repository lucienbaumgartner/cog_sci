library(dplyr)
library(reshape2)
library(stringr)
library(quanteda)
library(lme4)
library(emmeans)
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# load sentiment disctionary
load('../res/sentiWords-db.RDS')
# custom plot function
abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")

df <- read.csv('../input/Moral+Intensity+Ratings_2+Scales_September+24,+2020_10.15.csv', stringsAsFactors = F)
df <- as_tibble(df)
df <- df[-c(1:2),]
df <- mutate(df, id = 1:nrow(df))

df[, grepl('DO', names(df))]

dfx <- df[,grepl('^FL_49', names(df), perl = T)]
dfx_grid <- df[,grepl('^id$|^FL_404', names(df))]
dfx_grid <- melt(dfx_grid, id.vars = 'id', variable.name = 'item', value.name = 'item_name')
dfx_grid <- mutate(dfx_grid, item = gsub('\\_DO', '', item))
dfx <- mutate(dfx, id = 1:nrow(dfx))
dfx <- melt(dfx, id.vars = 'id', value.name = 'item_order')
dfx <- filter(dfx, !item_order=='')
dfx <- select(dfx, -variable)
dfx <- mutate(dfx, item_order = tolower(item_order))

# collapsing data to long format
names(df)
df <- df[,c(19:42, length(df))]
df <- melt(df, id.vars = 'id', variable.name = 'item')
df <- mutate(df, value = as.numeric(value))
#df <- mutate(df, item = gsub('Eval', '', item))
df <- filter(df, !is.na(value))
df <- filter(df, id %in% names(table(df$id)[table(df$id)==12]))
unique(df$item)
table(table(df$id)==12)
df <- mutate(df, group = gsub('prob', '', tolower(item)),
             group = case_when(
               group == 'vicious2' ~ 'vicious',
               group == 'reckl2' ~ 'reckless',
               group == 'gen2' ~ 'generous',
               group == 'cour.2' ~ 'courageous',
               group == 'cruel2' ~ 'cruel',
               group == 'friend2' ~ 'friendly',
               group == 'rude2' ~ 'rude',
               group == 'self2' ~ 'selfish',
               group == 'virt2' ~ 'virtuous',
               group == 'hon2' ~ 'honest',
               group == 'comp2' ~ 'compassionate',
               group == 'manip2' ~ 'manipulative',
               TRUE ~ group
             ))
unique(df$group)
df <- mutate(df, question = ifelse(grepl('2', item), 'sentence', 'behaviour'),
             question = factor(question, levels = c('sentence', 'behaviour')))
df <- mutate(df, item_bind = ifelse(grepl('2', item), paste0(group, 2), group))

# combine data
df <- left_join(df, dfx)

# apply item order
df <- str_split(df$item_order, '\\|') %>% 
  do.call(rbind, .) %>%
  as_tibble %>% 
  mutate(id = df$id) %>% 
  filter(!duplicated(id)) %>% 
  melt(., id.vars = 'id', value.name = 'item_bind', variable.name = 'item_step') %>% 
  as_tibble %>% 
  left_join(df, .)
table(table(df$id)==12)
table(table(df$item_step)==103)
df <- mutate(df, item_step = factor(item_step))

# annotate corpus
toks <- unique(df$group)
toks <- tokens(toks)
annot <- tokens_lookup(toks, dictionary = sentiWords$dichot)
toks <- tibble(group = unlist(toks), polarity = unlist(annot)) %>% 
  mutate(polarity = factor(polarity, levels = c('positive', 'negative')))
table(toks$polarity)==6
df <- left_join(df, toks)
df <- as_tibble(df)
df <- mutate(df, questionPol = interaction(question, polarity))

save(df, file = '../output/data/2-intensity-ratings.RDS', compress = 'gzip')

### We hypothesize that there is no dierence between thick positive and thick negative thick terms for our two dependent variables.
summary(aov(value ~ question*polarity + Error(id/polarity), data=df))
anova(nlme::lme(value ~ question*polarity, random=~1 | id, method="ML", data=df))
anova(nlme::lme(value ~ question*polarity, random=~1 | id, correlation=nlme::corCompSymm(form=~1|id), method="ML", data=df))
anova(nlme::lme(value ~ question*polarity, random=list(id=nlme::pdCompSymm(~polarity-1)), method="REML", data=df))
anova(lmer(value ~ question*polarity + (1|id), data=df))

m2 <- aov(value ~ polarity + Error(id/polarity), data=df[df$question == 'behaviour',])
TukeyHSD(m2)
m3 <- aov(value ~ polarity + Error(id/polarity), data=df[df$question == 'sentence',])
emmeans(m2, specs = pairwise ~ polarity)
emmeans(m3, specs = pairwise ~ polarity)

df <- mutate(df, questionPol = interaction(question, polarity))
m1 <- nlme::lme(value ~ questionPol, random=~1 | id, method="ML", data=df)
m2 <- nlme::lme(value ~ polarity, random=~1 | id, method="ML", data=df[df$question == 'behaviour',])
m3 <- nlme::lme(value ~ polarity, random=~1 | id, method="ML", data=df[df$question == 'sentence',])
require(multcomp)
summary(glht(m1, linfct=mcp(questionPol = "Tukey")), test = adjusted(type = "bonferroni"))
summary(glht(m2, linfct=mcp(polarity = "Tukey")), test = adjusted(type = "bonferroni"))
summary(glht(m3, linfct=mcp(polarity = "Tukey")), test = adjusted(type = "bonferroni"))
