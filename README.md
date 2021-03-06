
### The Operating Cycle World-Wide

This repository uses Compustat Global data from WRDS to provide some
insights into the development of the operating cycle of publicly listed
firms. The operating cycle is normally presented in days and reflects
how long cash is needed to finance the operating activity of a given
firm. It consists of three components:

-   Days Payable Outstanding (DPO) = 365 times average Accounts Payable
    divided by Costs of Goods Sold. How long does it take a firm to pay
    its trade credit liabilities?
-   Days Inventory Held (DIH) = 365 times average Inventory divided by
    Costs of Goods Sold. How long is money needed to finance the
    inventory of a firm?
-   Days Sales Outstanding (DSO) = 365 times average Receivables divided
    by Sales. How long does it take the customers of a firm to pay their
    invoices?

Taken together these three measures describe the Operating Cycle and
yield a measure that is also called Days Working Capital
*D**W**C* = *D**I**H* + *D**S**O* − *D**P**O*. Longer operating cycles
generally indicate larger financing needs.

### Operating Cycle Across Firms

``` r
library(tidyverse)
library(ExPanDaR)
source("code/R/theme_trr.R")

smp <- readRDS("data/generated/opcycle_sample.rds")
smp_win <- treat_outliers(smp)
```

Based on a sample of 189,664 firm-year observations from 26 countries
covering the period 2000 to 2020, we can get an idea how the operating
cycle is distributed across firms.

``` r
ggplot(smp_win, aes(x = dwc)) + 
  geom_histogram(fill = col_trr266_nightblue) +
  labs(x = "Days Working Capital [days]", y = "Number of firm-years") +
  theme_minimal()
```

<img src="README_files/figure-gfm/Hist-1.png" style="display: block; margin: auto;" />

Even though the data is winsorized to the top and bottom percentile, we
see quite a range. A sizable fraction of firms have even negative
operating cycles meaning that they finance their working capital via
trade credit.

How does the operating cycle compare across industries?

``` r
ggplot(smp_win, aes(x = ff12_ind, group = ff12_ind, y = dwc)) + 
  geom_boxplot(outlier.shape = NA) +
  ylim(-200, 500) + 
  labs(x = "", y = "Days Working Capital [days]") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

<img src="README_files/figure-gfm/Industries-1.png" style="display: block; margin: auto;" />

You see that the Telecom industry has particular short operating cycles,
while Manufacting and Health Sectors have longer cycles.

### Operating Cycle by Country

How does the operating cycle vary across countries?

``` r
df <- smp_win %>%
  group_by(loc) %>%
  mutate(loc = sprintf("%s (N = %s)", loc, format(n(), big.mark = ",")))

ggplot(df, aes(x = loc, y = dwc)) + 
  geom_boxplot(outlier.shape = NA) +
  ylim(-250, 650) + 
  labs(x = "", y = "Days Working Capital [days]") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

<img src="README_files/figure-gfm/Countries-1.png" style="display: block; margin: auto;" />

Jupp. There is some variation but this might also be caused by varying
sample sizes.

### Operating Cycle over Time

How does the operating cycle vary over the years?

``` r
df <- smp_win %>%
  group_by(fyear) %>%
  summarise(
    mn = mean(dwc),
    ub = mn + 1.96*sd(dwc)/sqrt(n()),
    lb = mn - 1.96*sd(dwc)/sqrt(n())
  )
ggplot(df, aes(x = fyear, y = mn)) + 
  geom_point(color = col_trr266_nightblue) + 
  geom_linerange(aes(ymin = lb, ymax = ub)) +
  labs(x = "", y = "Days Working Capital [days]") + 
  theme_minimal() 
```

<img src="README_files/figure-gfm/OCYears-1.png" style="display: block; margin: auto;" />

You can see a decline during the 2000s, a spike during the financial
crisis and then an increasing trend in the 2010s. Interesting. How does
this compare across the components of the operating cycle?

``` r
df <- smp_win %>%
  select(fyear, DPO = dpo, DIH = dih, DSO = dso) %>%
  group_by(fyear) %>%
  pivot_longer(cols = -fyear, names_to = "stat", values_to = "vals") %>%
  group_by(fyear, stat) %>%
  summarise(
    mn = mean(vals),
    ub = mn + 1.96*sd(vals)/sqrt(n()),
    lb = mn - 1.96*sd(vals)/sqrt(n())
  )

ggplot(df, aes(x = fyear, y = mn, group = stat, color = stat)) + 
  geom_point() + 
  geom_line() + 
  geom_linerange(aes(ymin = lb, ymax = ub)) +
  labs(x = "", y = "Operating Cycle Component [days]", color = "") + 
  theme_minimal() +
  theme(legend.position = "bottom")
```

<img src="README_files/figure-gfm/ComponentYears-1.png" style="display: block; margin: auto;" />
All components are upwards trending, consistent with decreasing cost of
capital making companies more willing to invest in working capital.

### See for yourself

If you want to explore the data yourself, consider running the below:

``` r
conf <- readRDS("data/external/expand_config.rds")

# This might take a while to start up because of the large sample
ExPanD(
  smp, cs_id = c("gvkey", "conm"), ts_id = "fyear",
  config_list = conf,
  title = "Operating Cycle of Publicly-Listed Firms"
)
```

For this to work, you need to fork/download the Github repo, have WRDS
access, configure your logon credentials in `config.csv` and source the
`pull_wrds.r`and `prepare_data.R`to generate the sample. See [this
repo](https://github.com/trr266/treat) for more details.

Alternatively, Humboldt students of the Controlling class can simply use
the data file provided via Moodle and store it in `data/generated` after
forking/downloading this repo from Github. The code to generate this
README is in `README.Rmd`. Enjoy!
