library(dplyr)
library(reshape2)
library(stringr)
library(quanteda)
library(ggplot2)
library(lawstat)
library(stargazer)
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

load('../res/sentiWords-db.RDS')

## import data
df <- read.csv('../input/TC+Cognition+Exp+1a_Re-Run+Within_August+6,+2020_10.09.csv', stringsAsFactors = F)
df <- df[-c(1:2),]
df <- as_tibble(df)
names(df)

## clean and format data
df <- df[,grepl('Eval', names(df), perl = T)]
df <- mutate(df, id = 1:nrow(df))
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
    fill = 'Group',
    colour = 'Group',
    title = '1.a) Distribution of Contradiction Ratings per Item'
  ) +
  theme(
    plot.title = element_text(face = 'bold'),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
p
ggsave(p, filename = '../output/plots/1a_RERUN_boxplot_item.png', width = 11, height = 6)


means <- df %>% group_by(group) %>% summarise(avg = mean(value, na.rm = T))
p <- ggplot(df, aes(x = group, y = value, fill = group)) +
  geom_boxplot() +
  geom_point(data = means, aes(y = avg, colour = group)) +
  geom_point(data = means, aes(y = avg), shape = 1) +
  labs(
    y = 'Contradiction Rating',
    x = 'Item',
    fill = 'Group',
    colour = 'Group',
    title = '1.a) Distribution of Contradiction Ratings per Group'
  ) +
  theme(
    plot.title = element_text(face = 'bold'),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
p
ggsave(p, filename = '../output/plots/1a_boxplot_group.png', width = 6, height = 6)


## create alternative sample with removed outliers
outliers <- boxplot(df$value, plot=FALSE)$out
# no outliers
## test ANOVA assumptions
# homogeneity of variances
levene.test(df$value, df$group, location = 'mean', trim.alpha=0.25, correction.method = 'correction.factor')
# normality
# per group
lapply(unique(df$group), function(x){shapiro.test(df$value[df$group==x])}) %>% setNames(., unique(df$group))
# global
shapiro.test(df$value)
# conclusion: non-parametric tests advised

## Kruskall-Wallis Test (global ANOVA pendant)
kruskal.test(value ~ group, data = df)
kruskal.test(value ~ group, data = df_ALT)
# there are significant group differences, globally

## Planned Contrasts using Pairwise Wilcoxon Rank Sum Tests
# global version
att <- filter(df, !group == 'BEH') %>% mutate(.group = ifelse(group == 'ATT', as.character(polarity), as.character(group)))
att <- arrange(att, id, .group) %>% mutate(id = ordered(id))
beh <- filter(df, !group == 'ATT') %>% mutate(.group = ifelse(group == 'BEH', as.character(polarity), as.character(group)))
beh <- arrange(beh, id, .group) %>% mutate(id = ordered(id))
# ATT
pairwise.wilcox.test(att$value, att$.group, p.adjust.method = "BH", paired = T)
# BEH
pairwise.wilcox.test(beh$value, beh$.group, p.adjust.method = "BH", paired = T)

# between positive and negative alone
# alternative: mean difference is greater than zero (contradiction rating for positive TC > negative TC)
wilcox.test(df$value[df$group == 'positive'], df$value[df$group == 'negative'], alternative = 'greater')
# between CI and SE alone
# alternative: mean difference is less than zero (contradiction rating for CI < SE)
wilcox.test(df$value[df$group == 'CI'], df$value[df$group == 'SE'], alternative = 'less')
# semantic entailment, for which average cancellability ratings should be significantly above the midpoint
t.test(df$value[df$group == 'SE'], alternative = "greater", mu = median(1:9))
# conversational implicatures, for which average cancellability ratings should be significantly below the midpoint
t.test(df$value[df$group == 'CI'], alternative = "less", mu = median(1:9))
