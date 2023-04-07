# ABAP Statistical Tools

## Basic Features
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
DATA(min_price)       = prices->min( ).                 " --> Returns the smallest value [148.00]
DATA(max_price)       = prices->max( ).                 " --> Returns the largest value [6960.12]
DATA(tot_price)       = prices->sum( ).                 " --> Returns the sum of the values [25055655.41]
DATA(mean_price)      = prices->mean( ).                " --> Returns the sample mean of the values [922.96]
DATA(variance_price)  = prices->variance( ).            " --> Returns the sample variance of the values [572404.48]
DATA(std_dev_price)   = prices->standard_deviation( ).  " --> Returns the sample standard deviation of the values [756.57]
DATA(dist_prices)     = prices->count_distinct( ).      " --> Returns the number of distinct values [324]
DATA(not_ini_prices)  = prices->count_not_initial( ).   " --> Returns the number of not initial values [27147]
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
DATA(median)          = prices->median( ). " Method median( ) is just another way to call second_quartile( )
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
