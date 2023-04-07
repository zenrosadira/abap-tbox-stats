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
The smallest value
DATA(min_price)       = prices->min( ).                 " [148.00]

The largest value
DATA(max_price)       = prices->max( ).                 " [6960.12]

The sum of the values
DATA(tot_price)       = prices->sum( ).                 " [25055655.41]

The sample mean of the values
DATA(mean_price)      = prices->mean( ).                " [922.96]

The sample variance of the values
DATA(variance_price)  = prices->variance( ).            " [572404.48]

The sample standard deviation of the values
DATA(std_dev_price)   = prices->standard_deviation( ).  " [756.57]

The number of distinct values
DATA(dist_prices)     = prices->count_distinct( ).      " [324]

The number of not initial values
DATA(not_ini_prices)  = prices->count_not_initial( ).   " [27147]
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
