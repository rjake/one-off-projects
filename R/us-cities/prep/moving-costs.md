# Cost of moving:
* If driving round-trip (to destination and returning at pick-up location )
  * cheap per-day cost
  * charged extra per mile
    
* If driving one-way
  * expensive cost for rental, includes 4 days
  * no extra per mile
    
* Other factors
  * mpg of vehicle x cost of gas

---

```r
library(tidyverse)
library(data.table)
library(gt)

data.table::fread("
  company trip      mpg  miles  base_cost  days_needed  per_mile
  uhaul    round     10   800    20         3            2.0
  uhaul    one-way   10   400    1060       1            0
  budget   round     8    800    30         3            1.0
  budget   one-way   8    400    960        1            0
  penskee  round     12   800    30         3            1.3
  penskee  one-way   12   400    1343       1            0
") |> 
  as_tibble() |> 
  transmute(
    est_cost = 
      moving_cost(
        distance = miles, 
        mpg = mpg, 
        base_cost = base_cost, 
        days_needed = days_needed, 
        per_mile = per_mile
      ),
    company, 
    trip,
    rental = base_cost * days_needed,
    miles,
    mpg, 
    gas = miles / mpg * 3,
    per_mile,
    milage_addon = miles * per_mile
  ) |> 
  mutate(
    across(where(is.numeric), ~ifelse(.x == 0, NA, .x))
  ) |> 
  arrange(est_cost) |> 
  gt() |> 
  opt_horizontal_padding(scale = 3) |> 
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = "-"
  )
```

| est_cost|company |trip    | rental| miles| mpg| gas| per_mile| milage_addon|
|--------:|:-------|:-------|------:|-----:|---:|---:|--------:|------------:|
|     1110|budget  |one-way |    960|   400|   8| 150|        -|            -|
|     1180|uhaul   |one-way |   1060|   400|  10| 120|        -|            -|
|     1190|budget  |round   |     90|   800|   8| 300|      1.0|          800|
|     1330|penskee |round   |     90|   800|  12| 200|      1.3|         1040|
|     1443|penskee |one-way |   1343|   400|  12| 100|        -|            -|
|     1900|uhaul   |round   |     60|   800|  10| 240|      2.0|         1600|

