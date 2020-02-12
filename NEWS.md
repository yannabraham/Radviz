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
