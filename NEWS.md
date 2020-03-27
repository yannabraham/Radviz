# 2019-12-18

Radviz now uses ggplot2 for plotting, enabling richer options for labeling and statistics.

# 2020-01-10

After some testing the following enhancements have been made to the code:

 - created a `theme_radviz` function, mostly to enable changing the overall font size
 - label.size and label.color options have been reinstated, enabling each plot function
     to change the spring aspects
 - added a `springs` function to access the channels used for projection
 - fixed a bug where `do.radviz` would fail on a `tbl_df` or `tibble` object when some
     points could not be projected

# 2020-02-03

 - Fixed a bug related to arguments in smoothRadviz
 - Fixed a bug in do.L where NA is returned if the column contains a single value after transformation
 - Fixed a bug in do.radviz where creating Radviz objects from tbl_df could throw an error

# 2020-03-17

 - subset now can be used directly on column names from the Radviz object
 - optimization of anchor position using the Freeviz method from [Demsar **et al**](http://dx.doi.org/10.1016/j.jbi.2007.03.010) has been implemented by Nicolas Sauwen
 - extended the optimization of anchor position from classes (Freeviz) to graphs (Graphviz)

# 2020-03-27

 - fixed hexbin import for hexplot
 - rescalePlot will now issue a warning when some anchors cannot be interpreted after rescaling
 - added a filtering option to plot.radviz to remove short anchors in Freeviz and Graphviz plots
 - fixed some memory issues related to Freeviz & Graphviz C++ code
