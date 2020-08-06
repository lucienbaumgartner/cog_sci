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
df <- read.csv('../input/TC+Cognition+Exp+1a_Preregistered_July+22,+2020_09.55.csv', stringsAsFactors = F)
df <- df[-c(1:2),]
df <- as_tibble(df)



## clean and format data
dfx <- df[,grepl('^FL_55', names(df), perl = T)]
df <- df[,grepl('Eval|^CI|^SE(?!=A)', names(df), perl = T)]
dfx <- mutate(dfx, id = 1:nrow(dfx))
df <- mutate(df, id = 1:nrow(df))
dfx <- melt(dfx, id.vars = 'id', value.name = 'item_order')
df <- melt(df, id.vars = 'id', variable.name = 'item')
df <- mutate(df, value = as.numeric(value))
df <- mutate(df, item = gsub('SEA|Eval', '', item))
df <- filter(df, !is.na(value))
df <- mutate(df, group = ifelse(grepl('^SE|^CI', item), str_extract(item, '^SE|^CI'), NA))
table(df$group)
toks <- unique(df$item)
toks <- toks[!grepl('^SE|^CI', toks)]
toks <- tolower(toks)
toks <- tokens(toks)
annot <- tokens_lookup(toks, dictionary = sentiWords$dichot)
toks <- tibble(item = unlist(toks), annot = unlist(annot))
df <- mutate(df, item = tolower(gsub('^SE(\\.)?|^CI', '', item, perl = T)))
table(df$item)
df <- left_join(df, toks)
df <- mutate(df, group = ifelse(is.na(group), annot, group))
dfx <- filter(dfx, !item_order=='')
dfx <- select(dfx, -variable)
df <- left_join(df, dfx)
#df <- select(df, - annot, - id)
dfx <- str_split(df$item_order, '\\|') %>% 
  do.call(rbind, .) %>%
  as_tibble %>% 
  mutate(id = df$id) %>% 
  melt(., id.vars = 'id', value.name = 'item', variable.name = 'item_step') %>% 
  as_tibble %>% 
  mutate(item = tolower(gsub('^CI|^SE(A|\\.)?', '', item, perl =T)))
df <- mutate(df, 
       item_order = sapply(str_split(item_order, '\\|'), function(x) return(x[1])),
       item_order = tolower(gsub('^CI|^SE(A|\\.)?', '', item_order, perl =T)))
df <- left_join(df, dfx) %>% filter(!(duplicated(id)&duplicated(item)))
df <- mutate(df, item_step = as.numeric(gsub('V', '', item_step)))

ggplot(df, aes(x=as.factor(item_step), y=value, fill=group)) +
  #geom_point() +
  #geom_smooth()
  geom_boxplot() +
  facet_wrap(~group) +
  labs(
    title = 'How the participants answered from one item to another within their block',
    x='Item Step'
  )
#df <- filter(df, item==item_order)
check <- df %>% group_by(item, group) %>% summarise(n=n()) %>% arrange(group, item)
check
sum(check$n) == nrow(df)
rm(list = c('check', 'annot', 'sentiWords', 'toks'))
df <- ungroup(df)
df <- as_tibble(df)

### analysis
## distribution plot
df <- mutate(df, group = factor(group, levels = c('positive', 'negative', 'CI', 'SE')))
means <- df %>% group_by(item, group) %>% summarise(avg = mean(value, na.rm = T))
p <- ggplot(df, aes(x = item, y = value, fill = group)) +
  geom_boxplot() +
  geom_point(data = means, aes(y = avg, colour = group)) +
  geom_point(data = means, aes(y = avg), shape = 1) +
  facet_grid(~ group, scales = 'free_x') +
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
ggsave(p, filename = '../output/plots/1a_boxplot_item_firstonly.png', width = 11, height = 6)


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
ggsave(p, filename = '../output/plots/1a_boxplot_group_firstonly.png', width = 6, height = 6)


## create alternative sample with removed outliers
outliers <- boxplot(df$value, plot=FALSE)$out
df_ALT <- df[-which(df$value %in% outliers),]
## test ANOVA assumptions
# homogeneity of variances
levene.test(df$value, df$group, location = 'mean', trim.alpha=0.25, correction.method = 'correction.factor')
levene.test(df_ALT$value, df_ALT$group, location = 'mean', trim.alpha=0.25, correction.method = 'correction.factor')
# normality
# per group
lapply(unique(df$group), function(x){shapiro.test(df$value[df$group==x])}) %>% setNames(., unique(df$group))
lapply(unique(df_ALT$group), function(x){shapiro.test(df_ALT$value[df_ALT$group==x])}) %>% setNames(., unique(df_ALT$group))
# global
shapiro.test(df$value)
shapiro.test(df_ALT$value)
# conclusion: non-parametric tests advised

## Kruskall-Wallis Test (global ANOVA pendant)
kruskal.test(value ~ group, data = df)
kruskal.test(value ~ group, data = df_ALT)
# there are significant group differences, globally

## Planned Contrasts using Pairwise Wilcoxon Rank Sum Tests
# global version
pairwise.wilcox.test(df$value, df$group, p.adjust.method = "BH")
pairwise.wilcox.test(df_ALT$value, df_ALT$group, p.adjust.method = "BH")
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
