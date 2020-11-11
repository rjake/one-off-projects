# Dorling Cartogram (packed circles)

Inspired by [this post](https://twitter.com/i/status/1324333128375279616):

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://twitter.com/i/status/1324333128375279616"><img src="img/election_pop.png" height="200"/></a>

I was able to use the `cartogram` package to create what is called a "dorling cartogram"

Here is: 

* the **[code](dorling_cartogram.R)** used to generate the bubbles

* **[an Rds file](map_dorling_cartogram.Rds)** with the circle radius proportional to the population (2010 census) Note: they are `sf` polygons
  
* What it looks like with `ggplot2`

    &nbsp;&nbsp;&nbsp;&nbsp;<img src="img/ggplot-cartogram.png" height="300"/>
    
* And with `tmap`

    &nbsp;&nbsp;&nbsp;&nbsp;<img src="img/tmap-cartogram.png" height="300"/>

