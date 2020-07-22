# Analysis 1a

## Hypotheses
- H1: There is a significant difference between TNC (thick negative concepts) and TPC (thick positive concepts), such that Contradiction ratings are higher for TNC than for TPC.
- H_aux1: Semantic entailment, for which average cancellability ratings should be significantly above the midpoint.
- H_aux2: Conversational implicatures, for which average cancellability ratings should be significantly below the midpoint

**H1 has to be rejected. H_aux1 cannot be rejected. H_aux2 has to be rejected. However, CI and SE have significantly different avg cancellability ratings.**

## Data Distribution
These are the distributions of cancellability ratings per item:

![per item](/output/plots/1a_boxplot_item.png)

And aggregated per group:
![per item](/output/plots/1a_boxplot_group.png)

We already see that the group means between TPC and TNC are most probably not significant. Also, TNC seem to have marginally higher cancellability ratings on average.

## Testing for ANOVA assumptions
### Homogeneity of variance
We performed a classical Levene's test based on the absolute deviations from the mean with correction factor and an alpha trimming of .25:

`Test Statistic = 67.637, p-value < 2.2e-16`

The H0 has to be rejected and we can assume significant differences between the group variances in the population. This ANOVA assumption is violated.

### Normality
To test whether the cancellability ratings are normally distributed, we performed a Shapiro-Wilk normality test, both globally and per group. For the global distribution we get a test statistic of

`W = 0.83878, p-value < 2.2e-16`

This means that the cancellability ratings are not normally distributed. The same holds true on group level (results omitted for brevity, see l99-100 in `src/1a_analysis.R`). This ANOVA assumption is also violated.

**Since both ANOVA assumptions are violated, we will use non-parametric alternatives.**

## Hypothesis testing
As an alternative to our 1x4 global ANOVA, we use a Kruskal-Wallis rank sum test.

`Kruskal-Wallis chi-squared = 112.69, df = 3, p-value < 2.2e-16`

There are indeed significant differences in the group means. In order to test our H1, we will first apply pairwise comparisons for the whole 1x4 design using Wilcoxon rank sum test with continuity correction. In a second step, we will use a directed version of the same test. For H_aux1 and H_aux2 we will use directed One Sample t-tests.

### H1
The global pairwise 1x4 Wilcoxon rank sum test with continuity correction produces the following test statistic:

```
         positive negative CI    
negative 0.44     -        -     
CI       <2e-16   <2e-16   -     
SE       0.40     0.77     <2e-16

P value adjustment method: BH
```

As we can see, the difference in group means between TNC and TPC are not significant. The only significant differences are `TNC~CI`, `TPC~CI` and `CI~SE`. The H0s in those pairwise comparisons are that the difference in group means is equal to 0.

In the directed version of this test we specify the alternative to H0. According to our hypothesis, we expect the cancellability ratings for TPC to be significantly higher than the once for TNC, on average and ceteris paribus. Thus, we can specify the alternative to H0 such that we expect that the true location shift is greater than 0 (TPC > TNC).

```
data:  df$value[df$group == "positive"] and df$value[df$group == "negative"]
W = 131922, p-value = 0.8177
alternative hypothesis: true location shift is greater than 0
```

The results show that H0 cannot be rejected and TPC > TNC is not supported. We thus reject H1.

### H_aux1
In the one sample t-test for H_aux1 we specify the alternative hypothesis such that we expect the true mean of the cancellability ratings of the SE group to be above the midpoint (>5). H0 is always that the difference in the group mean and the midpoint is euqual to 0.

```
data:  df$value[df$group == "SE"]
t = 21.689, df = 411, p-value < 2.2e-16
alternative hypothesis: true mean is greater than 5
95 percent confidence interval:
 7.040855      Inf
sample estimates:
mean of x
 7.208738
```

As we can see, H0 has to be rejected, so that the alternative hypothesis is supported. This means that H_aux1 cannot be rejected.

### H_aux2
For H_aux2 we specify the alternative hypothesis such that we expect the true mean of the cancellability ratings of the CI group to be below the midpoint (<5).

```
data:  df$value[df$group == "CI"]
t = 4.4774, df = 399, p-value = 1
alternative hypothesis: true mean is less than 5
95 percent confidence interval:
     -Inf 5.848296
sample estimates:
mean of x
     5.62
```

We see that we cannot reject H0, i.e. that the true mean is significantly different from them midpoint. Consequently, the alternative hypothesis has to be rejected.
