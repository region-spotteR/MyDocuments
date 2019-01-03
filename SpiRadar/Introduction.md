SpiRadar: Intro
================
August 19, 2018

### Some common sense

For a radar plot there are four components to be calculated

-   the grid lines
-   the 'axis' paths/lines
-   the positions of the ('axis') labels
-   positions of the datapoints to compare

Since in my documentation I constantly use the terms 'grid lines'(blue),'axis lines' (orange) and 'axis labels' (green), here is quick representation what I mean by that:
<p align="center">
<img src="https://raw.githubusercontent.com/region-spotteR/MyDocuments/master/screenshots/terms_spiradar1_datapoints.png" width="800"/>
</p>
Note that for reference in grey I also added the datapoints.

Implementations
---------------

For `plotly` and `ggplot2` (currently the main plotting packages for R) there are two ways of implementing this either using *polar coordinates* or using a *cartesian coordinate system*.

Using polar coordinates yields quick results and is at for first tryout plots faster. However customization with polar coordinates is usually tricky. Adjusting the ('axis') label positions or the text angle, changing the way the grid lines look, filling the grid lines with colors - all these operations are finicky for normal users. The documentation for functions which create polar coordinates, e.g., `coord_polar` is minimal, but the biggest issue is that polar coordinates make it difficult to customize your plot step by step, e.g., the `gg+`-smth-way.

Using the cartesian coordinate system, the input data has to be transformed using sinus- and cosinus-functions. The resulting x and y values are then used for plotting (cf. the `mat_circle` function). In addition it is possible to return the input data and specify some opacity values (cf. the `prep_plot` function).

Therefore this package uses the cartesian coordinate system. To increase flexibility there is one function to prepare your data for radar plots (`prep_plot`) and one function to plot your data (`SpiRadar`).

Further documentation
---------------------

-   The function documentation of `prep_plot`,`SpiRadar` and `mat_circle`
-   I also describe how I benchmarked my way from `ggradar` to this package [here](https://github.com/region-spotteR/MyDocuments/blob/master/SpiRadar/benchmarks.md), however this ressource is meant for advanced users
