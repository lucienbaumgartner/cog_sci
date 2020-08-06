library(dplyr)
library(reshape2)
library(stringr)
library(quanteda)
library(ggplot2)
library(lawstat)
library(stargazer)
library(permuco)
#library(tidyr)
#library(emmeans)
library(lmerTest)
library(easystats)
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

load('../res/sentiWords-db.RDS')
abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")

## import data
df <- read.csv('../input/TC+Cognition+Exp+1a_Re-Run+Within_August+6,+2020_10.09.csv', stringsAsFactors = F)
df <- df[-c(1:2),]
df <- as_tibble(df)
names(df)

## clean and format data
df <- mutate(df, id = 1:nrow(df))
dfx <- df[,grepl('^FL_4', names(df), perl = T)]
dfx_grid <- df[,grepl('^id$|^FL_1', names(df))]
dfx_grid <- melt(dfx_grid, id.vars = 'id', variable.name = 'item', value.name = 'item_name')
dfx_grid <- mutate(dfx_grid, item = gsub('\\_DO', '', item))
df <- df[,grepl('Eval|^id$', names(df), perl = T)]
dfx <- mutate(dfx, id = 1:nrow(dfx))
dfx <- melt(dfx, id.vars = 'id', value.name = 'item_order')
df <- melt(df, id.vars = 'id', variable.name = 'item')
df <- mutate(df, value = as.numeric(value))
df <- mutate(df, item = gsub('Eval', '', item))
df <- filter(df, !is.na(value))
df <- mutate(df, group = ifelse(grepl('^SE|^CI|^BEH|^ATT', item), str_extract(item, '^SE|^CI|^BEH|^ATT'), NA))
df <- mutate(df, item = gsub('^BEH|^ATT', '', item))
table(df$group)
toks <- unique(df$item)
toks <- toks[!grepl('^SE|^CI', toks)]
toks <- tolower(toks)
toks <- tokens(toks)
annot <- tokens_lookup(toks, dictionary = sentiWords$dichot)
toks <- tibble(item = unlist(toks), polarity = unlist(annot))
df <- mutate(df, item = tolower(gsub('^SE(\\.)?|^CI', '', item, perl = T)))
table(df$item)
df <- left_join(df, toks)
dfx <- filter(dfx, !item_order=='')
dfx <- select(dfx, -variable)
df <- left_join(df, dfx)
#df <- select(df, - annot, - id)
dfx <- str_split(df$item_order, '\\|') %>% 
  do.call(rbind, .) %>%
  as_tibble %>% 
  mutate(id = df$id) %>% 
  filter(!duplicated(id)) %>% 
  melt(., id.vars = 'id', value.name = 'item', variable.name = 'item_step') %>% 
  as_tibble %>% 
  left_join(., dfx_grid) %>% 
  arrange(id) %>% 
  select(-item) %>% 
  rename(item = item_name) %>% 
  mutate(group = str_extract(item, '^CI|^SE|^ATT|^BEH'),
         item = tolower(gsub('^CI|^SE|^ATT|^BEH', '', item, perl =T)))
df <- left_join(df, dfx) %>% arrange(id)
table(table(df$id)==6)
df <- mutate(df, item_step = as.numeric(gsub('V', '', item_step)))
#df <- mutate(df, group = ifelse(is.na(group), annot, group))
#df <- select(df, - annot, - id)
check <- df %>% group_by(item, group) %>% summarise(n=n()) %>% arrange(group, item)
check
sum(check$n) == nrow(df)
rm(list = c('check', 'annot', 'sentiWords', 'toks'))
df <- ungroup(df)
df <- as_tibble(df)

### analysis
## distribution plot
df <- mutate(df, group = factor(group, levels = c('ATT', 'BEH', 'CI', 'SE')))
df <- mutate(df, polarity = factor(polarity, levels = c('negative', 'positive')))
means <- df %>% group_by(item, group, polarity) %>% summarise(avg = mean(value, na.rm = T))
global_means <- df %>% group_by(group, polarity) %>% summarise(avg = mean(value, na.rm = T))
p <- ggplot(df, aes(x = item, y = value, fill = polarity)) +
  geom_boxplot() +
  geom_point(data = means, aes(y = avg, colour = polarity)) +
  geom_point(data = means, aes(y = avg), shape = 1) +
  geom_hline(data = global_means, aes(yintercept = avg)) +
  facet_grid(group ~ polarity, scales = 'free_x') +
  labs(
    y = 'Contradiction Rating',
    x = 'Item',
    fill = 'Polarity',
    colour = 'Polarity',
    title = '1.a) Distribution of Contradiction Ratings per Item',
    subtitle = abbrv('The distributions are futher divided by sentiment polarity of the TC (<NA> for CI and SE) and the item group (ATT, BEH, CI, SE).')
  ) +
  theme(
    plot.title = element_text(face = 'bold'),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
p
ggsave(p, filename = '../output/plots/1a_RERUN_boxplot_item.png', width = 11, height = 6)

df_aggr <- df %>% mutate(group = ifelse(group %in% c('ATT', 'BEH'), 'TC', as.character(group)))
means <- df_aggr %>% mutate(group = ifelse(group %in% c('ATT', 'BEH'), 'TC', as.character(group))) %>% group_by(group, polarity) %>% summarise(avg = mean(value, na.rm = T))
p <- ggplot(df_aggr, aes(x = group, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(data = means, aes(y = avg)) +
  geom_point(data = means, aes(y = avg), shape = 1) +
  facet_wrap(~ polarity, scales = 'free_x') +
  labs(
    y = 'Contradiction Rating',
    x = 'Item',
    fill = 'Group',
    colour = 'Group',
    title = abbrv('1.a) Distribution of Contradiction Ratings per Group and Polarity')
  ) +
  theme(
    plot.title = element_text(face = 'bold'),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
p
ggsave(p, filename = '../output/plots/1a_RERUN_boxplot_group.png', width = 11, height = 6)


## create alternative sample with removed outliers
outliers <- boxplot(df$value, plot=FALSE)$out
outliers
# no outliers
## test ANOVA assumptions
# homogeneity of variances
levene.test(df$value, df$group, location = 'mean', trim.alpha=0.25, correction.method = 'correction.factor')
# normality
# per groupp
lapply(unique(df$group), function(x){shapiro.test(df$value[df$group==x])}) %>% setNames(., unique(df$group))
# global
shapiro.test(df$value)
# conclusion: non-parametric tests advised

### H1_ATT & H1_BEH
## Planned Contrasts using Pairwise Wilcoxon Rank Sum Tests
# global version
# create subset only containing BEH and ATT data
dfsub <- filter(df, group %in% c('BEH', 'ATT')) %>% mutate(id = factor(id), item_step = factor(item_step))
fit_att <- lmer(value ~ group*polarity + (1|id) + (1|item_step), data=dfsub)
summary(fit_att)
anova(fit_att)
emmeans(fit_att, specs = pairwise ~ polarity, at = list(.group = c("positive", "negative")), by = 'group')
# pooled data
fit_att <- lmer(value ~ polarity + (1|id) + (1|item_step), data=dfsub)
anova(fit_att)
emmeans(fit_att, specs = pairwise ~ polarity, at = list(.group = c("positive", "negative")))


### H_aux1 & H_aux2
# alternative: mean difference is less than zero (contradiction rating for CI < SE)
df_factor <- df %>% mutate(id = factor(id), item_step = factor(item_step))
fit <- lmer(value ~ group + (1|id) + (1|item_step), data=df_factor %>% filter(group %in% c('CI', 'SE')))
anova(fit)
emmeans(fit, specs = ~ group, at = list(group = c("CI", "SE")))











wilcox.test(df$value[df$group == 'CI'], df$value[df$group == 'SE'], alternative = 'less', paired = T, p.adjust.method = "BH")
# semantic entailment, for which average cancellability ratings should be significantly above the midpoint
t.test(df$value[df$group == 'SE'], alternative = "greater", mu = median(1:9), paired = TRUE)
# conversational implicatures, for which average cancellability ratings should be significantly below the midpoint
t.test(df$value[df$group == 'CI'], alternative = "less", mu = median(1:9), paired = TRUE)





att <- filter(df, !group == 'BEH') %>%  
  mutate(.group = ifelse(group == 'ATT', as.character(polarity), as.character(group)),
         id = factor(id))
beh <- filter(df, !group == 'ATT') %>%  
  mutate(.group = ifelse(group == 'BEH', as.character(polarity), as.character(group)),
         id = factor(id))
# ATT
pairwise.wilcox.test(att$value, att$.group, paired = T, p.adjust.method = "BH")
# BEH
pairwise.wilcox.test(beh$value, beh$.group, paired = T, p.adjust.method = "BH")

# between positive and negative alone
# alternative: mean difference is greater than zero (contradiction rating for positive TC > negative TC)
# ATT
wilcox.test(att$value[att$.group == 'positive'], att$value[att$.group == 'negative'], alternative = 'less', paired = T, p.adjust.method = "BH")
# BEH
wilcox.test(beh$value[beh$.group == 'positive'], beh$value[beh$.group == 'negative'], alternative = 'less', paired = T, p.adjust.method = "BH")

## Planned Contrasts based on ANOVA of a linear mixed mode
# ATT
fit_att <- lmer(value ~ .group + (1|id), data=att)
anova(fit_att)
emmeans(fit_att, specs = pairwise ~ .group, at = list(.group = c("positive", "negative")))
# BEH
fit_beh <- lmer(value ~ .group + (1|id), data=beh)
anova(fit_beh)
emmeans(fit_beh, specs = pairwise ~ .group, at = list(.group = c("positive", "negative")))
