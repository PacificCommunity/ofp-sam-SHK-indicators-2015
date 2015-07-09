# define information by figure types for record
# figref: string template to use in report under \label
# caption: caption to use for figure, if needed
# landscape: whether the figure should be drawn in landscape mode

fig.index <- list()
fig.index$"Template" <- list(figref="",    # character, template for label to refer to with \ref
                               pos=NA,       # where should the figure be plotted?
                               caption="", # # character vector
                             landscape=NA) # TRUE or FALSE
fig.index$"OAF_compr-SPC-vs-OD" <- list(figref="hookcovrg",
                            pos=1,
                            caption="Temporal coverage in effort for the operational dataset compared to the SPC aggregated holdings.",
                            landscape=TRUE)
fig.index$"General" <- list(figref="dataavail",
                            pos=1,
                            caption="The number of sets, effort (hooks) and catch-in-numbers of BET by region and flag available before filtering.",
                            landscape=TRUE)
fig.index$"Avail" <- list(figref="vesavail",
                          pos=2,
                          caption="The number of sets for which the individual vessel undertaking the fishing could or could not be indentified, by region.",
                          landscape=TRUE)
fig.index$"Panel-Map-Dec-tot-catch" <- list(figref="mapcatch",
                                            pos=3,
                                            caption="Bigeye catch (numbers of fish) by decade from the operational data set.",
                                            landscape=TRUE)
#dmm <- sapply(c(main.flags,"OTH"), function(x) panel.map.smr.dec(flag=x, year=1960:2014, var="bet_n", smr.type = "tot-catch", save=TRUE, nice.map=TRUE))
fig.index$"Panel-Map-Dec-tot-effort" <- list(figref="mapeffort",
                                            pos=3,
                                            caption="Longline effort (in hundred hooks) by decade from the operational data set.",
                                            landscape=TRUE)
#dmm <- sapply(c(main.flags,"OTH"), function(x) panel.map.smr.dec(flag=x, year=1960:2014, var="hhook", smr.type = "tot-effort", save=TRUE, nice.map=TRUE))
fig.index$"Panel-Map-Dec-ag-cpue" <- list(figref="mapcpue",
                                            pos=3,
                                            caption="Aggregated bigeye tuna CPUE (in individuals per hundred hooks) by decade from the operational data set.",
                                            landscape=TRUE)
#dmm <- sapply(c(main.flags,"OTH"), function(x) panel.map.smr.dec(flag=x, year=1960:2014, var="bet_n", smr.type = "ag-cpue", save=TRUE, nice.map=TRUE))

fig.index$"Cluster-All" <- list(figref="clustBP",
                                pos=6,
                                caption="Mean proportion of species in the catch for each cluster - for 2, 3, and 4 cluster models. The width of the bar is proportional to the number of records in the cluster.",
                                landscape=FALSE)
fig.index$"Cluster-comp-time-series" <- list(figref="clustTS",
                                             pos=7,
                                             caption="Time series of cluster membership for the 2, 3, and 4 cluster models, with the colour matching the dominant species in the cluster and the top panel indicating the number of records over time.",
                                             landscape=FALSE)
fig.index$"Vessel-filtering" <- list(figref="mapyqlim",
                                             pos=4,
                                             caption="Effect of thresholds of minimal year-quarter presence for core fleet membership on the proportion of vessels remaining by fleet, and the spatial distribution of retained effort and catch.",
                                             landscape=TRUE)
fig.index$"Vessel-filtering-ts-panel" <- list(figref="tsyqlim",
                                             pos=5,
                                             caption="Effect of thresholds of minimal year-quarter presence for core fleet membership on the temporal trends in catch and effort retained for the CPUE analysis.",
                                              landscape=FALSE)
fig.index$"Cluster-map-all-regs" <- list(figref="clmap",
                                         pos=8,
                                         caption="Map of cluster composition at the 5 degree scale, by decade, with region-specific cluster numbers as described in Table \\ref{modeltbl1}. ALB, BET, SWO and YFT are given by green, red, blue and yellow, respective",
                                         landscape=TRUE)
fig.index$"DLNvsNBvsDG-byYRQTR" <- list(figref="ind-distrib",
                                        pos=9,
                                        caption="Comparison of standardised indices for the three different models (NB, DG and DLN) for the full short dataset (no cluster data removed) at the year-quarter scale.",
                                        landscape=TRUE)
fig.index$"DLN-LongVshort-byYR" <- list(figref="indlongshort",
                                        pos=10,
                                        caption="Comparison of standardised DLN indices for the short and long datasets (no cluster data removed) at the year scale, normalized to have a mean of 1 over the time period of the shorter indices.",
                                        landscape=TRUE)
fig.index$"DLN-long-datrm-clstVDLN-short-datrm-clst-byYR" <- list(figref="indlongvshortdatrm",
                                                                  pos=11,
                                                                  caption="Comparison of standardised DLN indices for the short and long datasets with cluster data removed at the year scale, normalized to have a mean of 1 over the time period of the shorter indices. Both models do not have the \\textsf{cluster} variable included",
                                                                  landscape=TRUE)
fig.index$"DLN-datrm-FullvNoVesvNom-byYR" <- list(figref="indbyfactrm",
                                                  pos=12,
                                                  caption="Comparison of standardised DLN indices with and without the \\textsf{vesselid} variable included, and the nominal indices, for the short dataset with cluster data removed, at the year scale. Note that R11 does not have a `full' model as only one cluster remains in the dataset.",
                                                  landscape=TRUE)
fig.index$"DLN-ClstRmvsClstRm-clst-Nom-byYR" <- list(figref="indshortclustrm",
                                                     pos=13,
                                                     caption="Comparison of standardised DLN indices with and without the \\textsf{cluster} variable included, and the nominal indices, for the short dataset with cluster data removed, at the year scale. Note that R11 does not have a `full' model as only one cluster remains in the dataset.y.",
                                                     landscape=TRUE)
fig.index$"InfCoefPlots" <- list(figref="infplots",
                             pos=15,
                             caption=quote(sprintf("Summary plots of fitted GLM models for region %s, step %s; standardised versus nominal indices (top left), and influence plots (left column) and model coefficients for each explanatory variable (right column)", rnum, stepval)), # # character vector
                             landscape=TRUE)
fig.index$"Residuals-panel" <- list(figref="resid",
                                    pos=15,
                                    caption=quote(sprintf("Diagnostic plots of fitted GLM models for region %s, step %s, showing characteristics of the model residuals and comparisons between observed and simulated data",rnum,stepval)), # # character vector
                                    landscape=FALSE)


diagno.legend <- "\n\\clearpage\n\\subsection*{Diagnostic plots}\nThe plots presented below display a range of diagnostics that describe the fit of the GLM models and the features of the model that
affect the standardisation of the raw CPUE data. There are two figures for each model, a figure displaying the indices and their determinants (summary plots),
and a figure showing residuals and simulated data (diagnostic plots). The indices figure is the same for all GLM families and displays the nominal
and standardised indices in the top left panel (note that for binomial, normal and gamma the standardised index is the DLN, DLN and DG, respectively),
the rest of the plots in the left column are influence plots for the models categorical variables, and the right column are plots of coefficient values
for these same variables. Note that the vessel coefficients are positioned on the x axis by the first year-quarter that they appeared in the
dataset for that region. The residual figures are the same for the log normal, negative binomial and gamma models, but different for the binomial models.
For the former, the top left figure plots the observed distribution (blue bars) of BET counts or cpue (on the log-scale for log normal) and the distribution
of data simulated from the fitted model (red lines, one for each simulated dataset). The top right plot is a normal quantile-quantile of the observed quantile (binomial, negative binomial, gamma)
or deviance (log normal) residuals. The middle left plot shows the same residuals against the models fitted values. The remaining plots show
box and whiskers of the residuals for the categorical variables in the model. The binomial figure differs in two ways; the observed proportions
of positive counts of BET (blue bars) are compared to proportions calculated from data simulated from the fitted model (red points, one for each simulated
dataset, jittered), and in the top left plot the observed and fitted proportions of positive counts are compared after aggregating data at the
vessel-year-quarter-cell scale.\\label{diagnofigs}\n\n"
