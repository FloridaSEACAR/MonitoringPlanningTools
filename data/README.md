This application is for selecting oyster reef sample sites. Sites are chosen using the Generalized Random Tessellation Stratified (GRTS) algorithm, which is a de facto standard used by the EPA. Site selection is done randomly to avoid statistical bias and to reflect the spatial arrangement of available sites. Because we have data at some sites, we can use a model to predict parameters at unsampled sites. The GRTS algorithm uses uncertainties in the prediction model to guide site selection – those sites with greater prediction uncertainty are more likely to appear in the selected sites. Selections use prediction uncertainties for three parameters: shell height, percent live and density.

There are map layers for bathymetry (depth in meters), existing oyster reefs from Fish and Wildlife Conservation (FWC) data, prior sample sites, and prediction uncertainties for shell height, percent live and density. The larger the disks, the greater the uncertainty and the more likely tey will appear in the GRTS generated sample sites. GRTS sample sites can be displayed after they have been generated.

How to use this application. Inspect the graphs in the center panels. The one on the left shows improvement as a function of sample size within a reef. The one of the right guides the number of additional sites to select. Based on your desired standard deviation, follow the smoothed curves to see show many additional sites are need to achieve your desired error rate. Please note that the number of sites to select is based on simulations and is only approximate. Generally, after the curves flatten, there is little estimated improvement in the error. It is recommended that one choose the maximum from the three curves. If the number of prior sites is small or there are few unsampled oyster reefs in the managed area, owing to statistical estimation issues, the curves may not decrease for one or more of the parameters. In this case, it is recommended to use the other curves for guidance.

1.  If there are sites that should be removed from consideration, enter those in a .csv file with OBJECTID, lon and lat as headers. You can open up a dialog box to input that file.

2.  You can also select ranges of depths or reef areas to include. Beware that too stringent selection criteria could drastically reduce or even eliminate remaining sites.

3.  Click on the number of GRTS selection and enter the number of sites. Note that this will be in addition to the prior sites. Closing the dialog will compute the sites. Choose more that you will need because some sites may not be available.

4.  Click on the GRTS sample sites layer selection to view your sites on the map.

5.  Use the panel at the bottom left to export the GRTS sample sites to one of three formats: csv, kml or gpx. The file has OBJECTID, lon and lat. The order of the reefs is randomized so if you need to eliminate reefs you can remove them from the top or bottom of the list.
