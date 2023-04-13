# ABAP Statistical Tools

## Basic Features & Elementary Statistics
Let's compute some statistics on `SBOOK` table
```abap
SELECT * FROM sbook INTO TABLE @DATA(T_SBOOK).

DATA(stats) = NEW ztbox_cl_stats( t_sbook ).
```

Use `->col( )` method to select a column on which make calculations

```abap
DATA(prices) = stats->col( `LOCCURAM` ).
```

Each statistic has its own method

```abap
* The smallest value
DATA(min)        = prices->min( ).                       " [148.00]

* The largest value
DATA(max)        = prices->max( ).                       " [6960.12]

* The range, i.e. the difference between largest and smallest values
DATA(range)      = prices->range( ).                     " [6812.12]

* The sum of the values
DATA(tot)        = prices->sum( ).                       " [25055655.41]

* The sample mean of the values
DATA(mean)       = prices->mean( ).                      " [922.96]

* The mean absolute deviation (MAD) from the mean
DATA(mad_mean)   = prices->mad_mean( ).                  " [480.41]

* The sample median of the values
DATA(median)     = prices->median( ).                    " [670.34]

* The mean absolute deviation (MAD) from the median
DATA(mad_median) = prices->mad_median( ).                " [436.36]

* The sample variance of the values
DATA(variance)   = prices->variance( ).                  " [572404.48]

* The sample standard deviation of the values
DATA(std_dev)    = prices->standard_deviation( ).        " [756.57]

* The coefficent of variation, ratio of the standard deviation to the mean
DATA(coeff_var)  = prices->coefficient_variation( ).     " [0.819]

* The dispersion index, ratio of the variance to the mean
DATA(disp_index) = prices->dispersion_index( ).          " [620.18]

* The number of distinct values
DATA(dist_val)   = prices->count_distinct( ).            " [324]

* The number of not initial values
DATA(not_init)   = prices->count_not_initial( ).         " [27147]
```

Alternatively, you can use the main instance, which represents the entire table, passing the name of the relevant column:

```abap
DATA(min_price)       = stats->min( `LOCCURAM` ).
```

## More specific descriptive statistics

### Quartiles
25% of the data is below the *first quartile* $Q1$

```abap
DATA(first_quartile) = prices->first_quartile( ). " [566.10]
```

50% of the data is below the *second quartile* or *median* $Q2$

```abap
DATA(second_quartile) = prices->second_quartile( ). " [670.34]
DATA(median)          = prices->median( ). " It's just a synonym for second_quartile( )
```

75% of the data is below the *third quartile* $Q3$

```abap
DATA(third_quartile) = prices->third_quartile( ). " [978.50]
```

The difference between third and first quartile is called *interquartile range* $\mathrm{IQR} = Q3 - Q1$, and it is a measure of spread of the data

```abap
DATA(iqr) = prices->interquartile_range( ). " [412.40]
```

A value outside the range $\left[Q1 - 1.5\mathrm{IQR},\ Q3 + 1.5\mathrm{IQR}\right]$ can be considered an *outlier*
```abap
DATA(outliers) = prices->outliers( ). " Found 94 outliers, from 1638.36 to 6960.12
```

### Means

Harmonic Mean is $\frac{n}{\frac{1}{x_1}\+\ \ldots\ +\ \frac{1}{x_n}}$, used often in averaging rates

```abap
DATA(hmean) = prices->harmonic_mean( ). " [586.17]
```

Geometric Mean is $\sqrt[n]{x_1\cdot \ldots \cdot x_n}$, used for population growth or interest rates

```abap
DATA(gmean) = prices->geometric_mean( ). " [731.17]
```

Quadratic Mean is $\sqrt{\frac{x_1^2\ +\ \ldots\ +\ x_n^2}{n}}$, used, among other things, to measure the fit of an estimator to a data set

```abap
DATA(qmean) = prices->quadratic_mean( ). " [1193.42]

* The values calculated so far confirm the HM-GM-AM-QM inequalities
* harmonic mean <= geometric mean <= arithmetic mean <= quadratic mean
```
 
### Moments

*Skewness* is a measure of the asymmetry of the distribution of a real random value about its mean. We estimate it with a sample skewness computed with the adjusted Fisher-Pearson standardized moment coefficient (the same used by Excel).

$$\mathrm{skewness} = \frac{n}{(n-1)(n-2)}\frac{\sum\limits_{i=1}^n {(x_i - \bar{x})}^3}{\left[\frac{1}{n-1}\sum\limits_{i=1}^{n} (x_i - \bar{x})^2 \right]^{3/2}}$$

```abap
DATA(skewness) = prices->skenewss( ). " [3.19] 
* positive skewness: right tail is longer, the mass of the distribution is concentrated on the left
```

*Kurtosis* is a measure of the tailedness of the distribution of a real random value: higher kurtosis corresponds to greater extremity of outliers

$$\mathrm{kurtosis} = \frac{1}{(n-1)}\frac{\sum\limits_{i=1}^n {(x_i - \bar{x})}^4}{\left[\frac{1}{n-1}\sum\limits_{i=1}^{n} (x_i - \bar{x})^2 \right]^2}$$

```abap
DATA(kurtosis) = prices->kurtosis( ). " [19.18]
* positive excess kurtosis (kurtosis minus 3): leptokurtic distribution with fatter tails
```

## Empirical Inference

The *histogram* is a table of couples $(\mathrm{bin}_i, \mathrm{f}_i)$ where $\mathrm{bin}_i$ is the first endpoint of the $i$-th *bin*, i.e. the interval with which the values were partitioned, and $\mathrm{f}_i$ is the $i$-th frequency, i.e. the number of values inside the $i$-th bin.

```abap
DATA(histogram) = prices->histogram( ).
```

The bins are created using *Freedman-Diaconis rule*: the bins width is $\frac{2\mathrm{iqr}}{\sqrt[3]{n}}$ where $\mathrm{iqr}$ is the interquartile range, and the total number of bins is $\mathrm{floor}\left(\frac{\mathrm{max} - \mathrm{min}}{\mathrm{bin\ width}}\right)$

Dividing each frequency by the total we get an estimate of the probability to draw a value in the corresponding bin, this is the *empirical probability*

```abap
DATA(empirical_prob) = prices->empiricial_pdf( ).
```

Similarly, for each distinct value $x$, we can compute the number $\frac{\mathrm{number\ of\ elements}\ \le x}{n}$, this is the *empirical distribution function*

```abap
DATA(empirical_dist) = prices->empirical_cdf( ).
```

In order to answer the question "are the values normally distributed" you can use method `->are_normal( )`

```abap
DATA(normality_test) = prices->are_normal( ) " [abap_false].
```

This method implements the [Jarque-Bera normality test](https://en.wikipedia.org/wiki/Jarque%E2%80%93Bera_test). The $p$-value is an exported parameter and the test is considered passed if $p\mathrm{-value} \le 0.5$.

## Distributions

The following are static methods to generate sample from various distributions

```abap
DATA(uniform_values) = ztbox_cl_stats=>uniform( low = 1 high = 50 size = 10000 ).
" Generate a sample of 10000 values from a uniform distribution in the interval [1, 50]
" default is =>uniform( low = 0 high = 1 size = 1 )
```
