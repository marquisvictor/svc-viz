import numpy as np
import matplotlib.pyplot as plt 
from mpl_toolkits.axes_grid1 import make_axes_locatable
import pandas as pd
import geopandas as gpd
from .utils import merge_index
from matplotlib.patches import Patch  
import geopandas as gp # type: ignore
from shapely import wkt # type: ignore

from .utils import *

def _compare_surfaces_grid(data, vars, use_tvalues=True, savefig=None, cbar_label=None, cmap=plt.cm.RdBu_r):
    """
    Internal function to plot coefficient surfaces in a grid layout, optionally overlaying non-significant areas
    using t-values and displaying a colorbar.

    Parameters
    ----------
    data : GeoDataFrame
        GeoDataFrame with coefficients and (optionally) t-values.
    vars : list of str
        List of column names to visualize as coefficient surfaces.
    use_tvalues : bool, optional
        Whether to gray out non-significant regions using t-values. Default is True.
    savefig : str, optional
        Path to save the figure. If None, the figure is not saved.
    cbar_label : str, optional
        Label for the colorbar. Default is None.
    cmap : matplotlib colormap, optional
        Colormap to use for visualizing coefficients. Default is plt.cm.RdBu_r.
    """
    n_vars = len(vars)
    tvalues = ['t_' + var for var in vars]

    grid_dim = int(np.ceil(np.sqrt(n_vars)))

    if n_vars in [1, 2]:
        figsize = (11, 9 * n_vars)
        fig, axes = plt.subplots(nrows=n_vars, ncols=1, figsize=figsize)
    else:
        figsize = (13, 11)
        fig, axes = plt.subplots(nrows=grid_dim, ncols=grid_dim, figsize=figsize)

    if n_vars == 1:
        axes = [axes]
    else:
        axes = axes.ravel()

    vmin = min(data[var].min() for var in vars)
    vmax = max(data[var].max() for var in vars)

    if (vmin < 0) & (vmax < 0):
        cmap = truncate_colormap(cmap, 0.0, 0.5)
    elif (vmin > 0) & (vmax > 0):
        cmap = truncate_colormap(cmap, 0.5, 1.0)
    else:
        cmap = shift_colormap(cmap, start=0.0,
                              midpoint=1 - vmax / (vmax + abs(vmin)),
                              stop=1.0)

    sm = plt.cm.ScalarMappable(cmap=cmap, norm=plt.Normalize(vmin=vmin, vmax=vmax))

    for i, var in enumerate(vars):
        ax = axes[i]
        ax.set_title(var, fontsize=15)
        data.plot(var, cmap=sm.cmap, ax=ax, vmin=vmin, vmax=vmax, edgecolor='grey', linewidth=0.2)

        if use_tvalues:
            tvalue_col = tvalues[i]
            if data[data[tvalue_col] == 0].empty:
                print(f"No significant values for {tvalue_col}, skipping mask.")
            else:
                data[data[tvalue_col] == 0].plot(color='lightgrey', edgecolor='black', ax=ax, linewidth=0.005)

        ax.get_xaxis().set_visible(False)
        ax.get_yaxis().set_visible(False)

    if n_vars > 2:
        for j in range(i + 1, grid_dim * grid_dim):
            axes[j].axis('off')

    fig.subplots_adjust(left=0.05, right=0.70, bottom=0.05, top=0.70, wspace=0.04, hspace=-0.35)

    cax = fig.add_axes([0.75, 0.17, 0.03, 0.42])
    sm._A = []
    cbar = fig.colorbar(sm, cax=cax)
    if cbar_label is not None:
        cbar.set_label(cbar_label, fontsize=15, labelpad=10)
    cbar.ax.tick_params(labelsize=15)

    if savefig is not None:
        plt.savefig(savefig)
    plt.show()


       
def viz_gwr(col_names, df_geo, gwr_object, use_tvalues=True, alpha=0.05,
            coef_surfaces=None, cbar_label=None, cmap=plt.cm.RdBu_r):
    """
    Visualize GWR results by plotting coefficient surfaces with optional t-value overlay.

    Parameters
    ----------
    col_names : list of str
        Names of the covariates used in the model.
    df_geo : GeoSeries
        Geometry used for mapping.
    gwr_object : GWRResults object
        Output from a fitted GWR model.
    use_tvalues : bool, optional
        Whether to mask non-significant t-values. Default is True.
    alpha : float, optional
        Significance threshold. Default is 0.05.
    coef_surfaces : list of str, optional
        Specific variables to plot. If None, all coefficients are plotted.
    cbar_label : str, optional
        Colorbar label. Default is None.
    cmap : matplotlib colormap, optional
        Colormap for coefficients. Default is plt.cm.RdBu_r.
    """
    data = gpd.GeoDataFrame(gwr_object.params, geometry=df_geo)
    col_names = ['intercept'] + col_names + ['geometry']
    data.columns = col_names

    tvl = pd.DataFrame(gwr_object.filter_tvals(alpha=alpha))
    tvl.columns = ['t_' + col for col in col_names if col != 'geometry']
    merged = data.merge(tvl, left_index=True, right_index=True)

    col_names.pop()  # remove 'geometry'

    if coef_surfaces is not None:
        _compare_surfaces_grid(merged, coef_surfaces, use_tvalues=use_tvalues, cbar_label=cbar_label, cmap=cmap)
    else:
        _compare_surfaces_grid(merged, col_names, use_tvalues=use_tvalues, cbar_label=cbar_label, cmap=cmap)


        
def viz_gw(df_geo, betas, std_errs, use_tvalues=True,
           coef_surfaces=None, alpha=0.05, cbar_label=None, cmap=plt.cm.RdBu_r):
    """
    Visualize GW results using beta and standard error inputs, with optional t-value masking.

    Parameters
    ----------
    df_geo : GeoSeries
        Geometry of the study area.
    betas : DataFrame
        Beta coefficient estimates.
    std_errs : DataFrame
        Corresponding standard errors.
    use_tvalues : bool, optional
        Whether to overlay t-value significance. Default is True.
    coef_surfaces : list of str, optional
        Variables to visualize. If None, visualize all.
    alpha : float, optional
        Significance level. Default is 0.05.
    cbar_label : str, optional
        Label for the colorbar. Default is None.
    cmap : matplotlib colormap, optional
        Colormap for coefficient surfaces. Default is plt.cm.RdBu_r.
    """
    betas.columns = ['beta_' + col for col in betas.columns]
    std_errs.columns = ['std_' + std for std in std_errs.columns]

    data = merge_index(betas, std_errs)
    mask = mask_insignificant_t_values(data.copy(), alpha=alpha)
    tvals = mask[[col for col in mask.columns if col.startswith('t')]]
    data_df = gpd.GeoDataFrame(merge_index(data, tvals), geometry=df_geo)

    betas_list = betas.columns

    if coef_surfaces is not None:
        _compare_surfaces_grid(data_df, coef_surfaces, use_tvalues=use_tvalues, cbar_label=cbar_label, cmap=cmap)
    else:
        _compare_surfaces_grid(data_df, betas_list, use_tvalues=use_tvalues, cbar_label=cbar_label, cmap=cmap)

        
        
        
    
def add_scalebar(ax, length=10, location=(0.1, 0.05), linewidth=3, units='m'):
    x0, x1 = ax.get_xlim()
    y0, y1 = ax.get_ylim()
    sb_x = x0 + (x1 - x0) * location[0]
    sb_y = y0 + (y1 - y0) * location[1]
    ax.plot([sb_x, sb_x + length], [sb_y, sb_y], color='black', linewidth=linewidth)
    ax.text(sb_x + length / 2, sb_y - (y1 - y0) * 0.01, f'{length} {units}',
            ha='center', va='top', fontsize=10)
    
def compare_conf(df_geo, est1, stderr1, est2, stderr2, var1,
                     var2, z_value=1.96, savefig=None):

    
    """
    Compare confidence intervals between two models and visualize regions of overlap.

    This function computes confidence intervals for a shared variable estimated in two models,
    and identifies where the intervals overlap or diverge. It produces a map showing regions
    of agreement (overlapping CIs) and disagreement (non-overlapping CIs) between the models.

    Parameters
    ----------
    df_geo      : GeoSeries
                    Geometry column for spatial plotting.
    est1        : DataFrame
                    Beta coefficients from the first model. Columns must be named by covariate.
    stderr1     : DataFrame
                    Standard errors corresponding to `est1`.
    est2        : DataFrame
                    Beta coefficients from the second model.
    stderr2     : DataFrame
                    Standard errors corresponding to `est2`.
    var1        : str
                    Covariate name in the first model to compare.
    var2        : str
                    Covariate name in the second model to compare.
    z_value     : float, optional
                    Z-score for constructing confidence intervals (default is 1.96 for 95% CI).
    savefig     : str, optional
                    If provided, saves the figure to the specified path. File format is inferred 
                    from the extension (e.g., 'plot.png', 'plot.pdf').

    Returns
    -------
    None
        Displays a map with two categories:
        - Overlapping confidence intervals (light gray)
        - Non-overlapping confidence intervals (yellow)

        Notes
    -----
    Overlapping confidence intervals do not imply statistical equivalence —
    further hypothesis testing would be required to formally assess equivalence. However,
    non-overlapping intervals provide strong evidence of differences, indicating spatial
    regions where relationships do not replicate between the two models.

    This function is particularly useful for spatially varying coefficient (SVC) models,
    where reproducibility and replicability are often assessed by comparing local estimates
    across model specifications, methods, or datasets.
    
    Examples
    --------
    >>> compare_conf(df.geometry, model1_betas, model1_ses, model2_betas, model2_ses, 'income', 'income')
    >>> compare_conf(df.geometry, m1, s1, m2, s2, 'pop_density', 'pop_density', z_value=1.645, savefig='ci_overlap.pdf')
    """

    est1.columns = ['beta_'+col if not col.startswith('beta_') else col for col in est1.columns]
    stderr1.columns = ['std_'+col if not col.startswith('std') else col for col in stderr1.columns]
    model_1 = merge_index(est1, stderr1)
    
    est2.columns = ['beta_'+col for col in est2.columns]
    stderr2.columns = ['std_'+col for col in est2.columns]
    data = merge_index(est2, stderr2)  
    
    data_df = gpd.GeoDataFrame(merge_index(model_1, data), geometry=df_geo)
        
    model_1['lower_'+var1] = model_1['beta_'+var1] - z_value * model_1['std_'+var1]
    model_1['upper_'+var1]  = model_1['beta_'+var1] + z_value * model_1['std_'+var1]
    
    data['lower_'+var2] = data['beta_'+var2] - z_value * data['std_beta_'+var2]
    data['upper_'+var2] = data['beta_'+var2] + z_value * data['std_beta_'+var2]

    data_df[var1] = ((model_1['lower_'+var1] <= data['upper_'+var2]) &
                     (model_1['upper_'+var1] >= data['lower_'+var2]))
    
    fig, ax = plt.subplots(figsize=(14, 12))
    # data_df[~data_df[var1]].plot(ax=ax, color='yellow', edgecolor='grey', linewidth=.6, label='Non-overlapping CI')
    # data_df[data_df[var1]].plot(ax=ax, color='whitesmoke', edgecolor='black', linewidth=0.040, label='Overlapping CI')
    
    data_df[~data_df[var1]].plot(ax=ax, color='yellow', edgecolor='grey', linewidth=.6)
    data_df[data_df[var1]].plot(ax=ax, color='whitesmoke', edgecolor='black', linewidth=0.040)

    # Add custom legend
    legend_elements = [
        Patch(facecolor='whitesmoke', edgecolor='black', label='Overlapping CI'),
        Patch(facecolor='yellow', edgecolor='grey', label='Non-overlapping CI')
    ]
    ax.legend(handles=legend_elements, loc='lower left', fontsize=10, frameon=True)

    plt.xticks([])
    plt.yticks([])
    
    ax.set_title(f' Model 1 vs Model 2 Confidence Interval Agreement \n {round(100-(data_df[var1].sum()/len(data_df)*100), 2)}% of the confidence intervals do not overlap, while {round(data_df[var1].sum()/len(data_df)*100, 2)}% do.', fontsize=12);
    

    # North arrow in bottom right
    ax.annotate('N',
                xy=(0.96, 0.08), xytext=(0.96, 0.093),
                arrowprops=dict(facecolor='black', width=2, headwidth=18),
                ha='center', va='center', fontsize=12,
                xycoords=ax.transAxes)

    # Optional scale bar (adjust length for your CRS)
    # add_scalebar(ax, length=10, units='m')
    
    
     # Optional: Save figure if filename is provided
    if savefig is not None:
        ext = savefig.split('.')[-1].lower()
        if ext in ['svg', 'pdf', 'eps']:
            plt.savefig(savefig, format=ext, bbox_inches='tight')
        else:
            plt.savefig(savefig, dpi=300)  # for png/jpg

    plt.show()

def _compare_surfaces(data, var1, var2, var1_t, var2_t,
                      use_tvalues=False, savefig=None,
                      cbar_label=None, cmap=plt.cm.RdBu_r):
    """
    Internal function to create a comparative visualization of two parameter surfaces.

    Parameters
    ----------
    data : GeoDataFrame
        GeoDataFrame containing parameter estimates and t-values.
    var1 : str
        Column name for the first parameter surface.
    var2 : str
        Column name for the second parameter surface.
    var1_t : str
        Column name for the t-values associated with var1.
    var2_t : str
        Column name for the t-values associated with var2.
    use_tvalues : bool, optional
        Whether to mask non-significant areas using t-values. Default is False.
    savefig : str, optional
        File path to save the figure. Default is None.
    cbar_label : str, optional
        Label for the colorbar. Default is None.
    cmap : matplotlib colormap, optional
        Colormap for surface visualization. Default is plt.cm.RdBu_r 
        (red = positive, blue = negative).
    """
    fig, axes = plt.subplots(nrows=2, ncols=1, figsize=(9, 9))
    ax0 = axes[0]
    ax0.set_title(f'Model 1 local parameter estimates for {var1}', fontsize=14)

    ax1 = axes[1]
    ax1.set_title(f'Model 2 local parameter estimates for {var2}', fontsize=14)

    vmin = np.min([data[var1].min(), data[var2].min()])
    vmax = np.max([data[var1].max(), data[var2].max()])

    if (vmin < 0) & (vmax < 0):
        cmap = truncate_colormap(cmap, 0.0, 0.5)
    elif (vmin > 0) & (vmax > 0):
        cmap = truncate_colormap(cmap, 0.5, 1.0)
    else:
        cmap = shift_colormap(cmap, start=0.0,
                              midpoint=1 - vmax / (vmax + abs(vmin)),
                              stop=1.0)

    sm = plt.cm.ScalarMappable(cmap=cmap, norm=plt.Normalize(vmin=vmin, vmax=vmax))

    data.plot(var1, cmap=sm.cmap, ax=ax0, vmin=vmin, vmax=vmax, edgecolor='k', linewidth=.1)
    if use_tvalues and (data[var1_t] == 0).any():
        data[data[var1_t] == 0].plot(color='lightgrey', edgecolor='grey', ax=ax0, linewidth=.05)

    data.plot(var2, cmap=sm.cmap, ax=ax1, vmin=vmin, vmax=vmax, edgecolor='k', linewidth=.1)
    if use_tvalues and (data[var2_t] == 0).any():
        data[data[var2_t] == 0].plot(color='lightgrey', edgecolor='grey', ax=ax1, linewidth=.05)

    fig.subplots_adjust(right=0.9)
    cax = fig.add_axes([0.92, 0.14, 0.03, 0.675])
    sm._A = []
    cbar = fig.colorbar(sm, cax=cax)
    if cbar_label is not None:
        cbar.set_label(cbar_label, fontsize=14, labelpad=10)
    cbar.ax.tick_params(labelsize=14)

    for ax in [ax0, ax1]:
        ax.get_xaxis().set_visible(False)
        ax.get_yaxis().set_visible(False)

    if savefig is not None:
        plt.savefig(savefig)
    plt.show()


def compare_two_surf(df_geo, est1, stderr1, est2, stderr2, var1,
                     var2, use_tvalues=False, alpha=0.05,
                     cbar_label=None, cmap=plt.cm.RdBu_r):
    """
    Compare the surfaces of two estimated models for specific variables,
    with an option to overlay t-values to highlight significant regions.

    Parameters
    ----------
    df_geo : GeoSeries
        Geometry column of the dataset.
    est1 : DataFrame
        DataFrame containing beta coefficients of the first model.
    stderr1 : DataFrame
        DataFrame containing standard errors of the first model.
    est2 : DataFrame
        DataFrame containing beta coefficients of the second model.
    stderr2 : DataFrame
        DataFrame containing standard errors of the second model.
    var1 : str
        Variable name of interest in the first model (e.g., 'beta_income').
    var2 : str
        Variable name of interest in the second model (e.g., 'beta_income').
    use_tvalues : bool, optional
        Whether to overlay non-significant areas based on t-values. Default is False.
    alpha : float, optional
        Significance level for masking non-significant t-values. Default is 0.05.
    cbar_label : str, optional
        Label for the shared colorbar. Default is None.
    cmap : matplotlib colormap, optional
        Colormap for surface visualization. Default is plt.cm.RdBu_r 
        (red = positive, blue = negative).
    """
    est1.columns = ['beta_' + col if not col.startswith('beta_') else col for col in est1.columns]
    stderr1.columns = ['std_' + col if not col.startswith('std') else col for col in stderr1.columns]
    model_1 = merge_index(est1, stderr1)
    model_1mask = mask_insignificant_t_values(model_1.copy(), alpha=alpha)
    tvals = model_1mask[[col for col in model_1mask.columns if col.startswith('t')]]
    model_1df = merge_index(model_1, tvals)

    est2.columns = ['beta_' + col for col in est2.columns]
    stderr2.columns = ['std_' + col for col in stderr2.columns]
    data = merge_index(est2, stderr2)
    model2_mask = mask_insignificant_t_values(data.copy(), alpha=alpha)
    model2_tvals = model2_mask[[col for col in model2_mask.columns if col.startswith('t')]]
    model_2 = merge_index(data, model2_tvals)

    data_df = gpd.GeoDataFrame(merge_index(model_1df, model_2), geometry=df_geo)

    t_var1 = 't_' + var1
    t_var2 = 't_' + var2

    _compare_surfaces(data_df, var1, var2, t_var1, t_var2,
                      use_tvalues=use_tvalues, cbar_label=cbar_label, cmap=cmap)

    
    
def three_panel(df, col_names, gwr_object, coef_surfaces=None, gwr_selector=None, aicc=None, cmap=plt.cm.RdBu_r):
    """
    Entry function for creating a three-panel visualization for a single covariate.
    
    Parameters
    ----------
    df : GeoDataFrame
        The original dataframe with geometry.
    col_names : list of str
        List of covariate names used in the GWR model.
    gwr_object : GWRResults object
        Fitted GWR model.
    coef_surfaces : list of str
        List containing a single covariate name for visualization.
    gwr_selector : GWRSelector object
        Bandwidth selector object used in GWR.
    aicc : list or array
        AICc values for bandwidth tuning.
    cmap : matplotlib colormap, optional
        Colormap used for plotting. Default is plt.cm.RdBu_r (red=positive, blue=negative).
    """
    if coef_surfaces is None or len(coef_surfaces) != 1:
        raise ValueError("You must have only one surface for the 3 panel visualization.")
    
    params = gpd.GeoDataFrame(gwr_object.params, columns=['intercept'] + 
                              col_names, geometry=df['geometry'])     
    df['intercept'] = params['intercept']
    
    tvl = pd.DataFrame(gwr_object.filter_tvals(), 
                       columns=['t_intercept'] + ['t_' + col for col in col_names])
    
    bse = gpd.GeoDataFrame(gwr_object.bse, columns=['se_intercept'] + 
                           ['se_' + col for col in col_names], geometry=df['geometry'])

    t_coefname = 't_' + coef_surfaces[0]
    se_coefname = 'se_' + coef_surfaces[0]
    
    _threePanel(tvl[t_coefname], bse[se_coefname], params, 
                coef_surfaces, gwr_object, df, gwr_selector, 
                fits=aicc, cmap=cmap)


def _threePanel(var_t, var_se, params, coef_surfaces, gwr_object, df, gwr_selector, fits, cmap=plt.cm.RdBu_r):
    """
    Internal function to build the three-panel figure.

    Parameters
    ----------
    var_t : Series
        t-values for the covariate of interest.
    var_se : Series
        Standard errors for the covariate of interest.
    params : GeoDataFrame
        GWR parameter estimates.
    coef_surfaces : list of str
        List containing a single covariate name.
    gwr_object : GWRResults object
        Fitted GWR model.
    df : GeoDataFrame
        Original data with geometry.
    gwr_selector : GWRSelector
        Bandwidth selector object.
    fits : array
        AICc or other fit criterion across bandwidths.
    cmap : matplotlib colormap, optional
        Colormap to use. Default is plt.cm.RdBu_r.
    """

    fig, ax = plt.subplots(3, 1, figsize=(8, 8), gridspec_kw={'height_ratios': [1.4, 12, 1.8]})
    bw = gwr_selector.search()
    fig.subplots_adjust(hspace=-0.53)

    if isinstance(bw, list):
        mgwr_bw = gwr_selector.search()
        ax[0].plot(range(24, len(var_t)), fits, c='k')
        ax[0].axvline(mgwr_bw[0], c='g')
        ax[0].axvline(mgwr_bw[0]-200, c='orange', linestyle='--')
        ax[0].axvline(mgwr_bw[0]+100, c='orange', linestyle='--')
    else:
        gwr_bw = gwr_selector.search()
        ax[0].plot(range(100, len(var_t), 100), fits, c='k')
        ax[0].axvline(220, c='g')
        ax[0].axvline(220-100, c='orange', linestyle='--')
        ax[0].axvline(220+100, c='orange', linestyle='--')
        ax[0].tick_params(axis='both', labelsize=10)

    # Compute value range
    gwr_min = params[coef_surfaces].min()
    gwr_max = params[coef_surfaces].max()
    vmin = np.min([gwr_min])
    vmax = np.max([gwr_max])

    # Adjust colormap based on value signs
    if (vmin < 0) & (vmax < 0):
        cmap = truncate_colormap(cmap, 0.0, 0.5)
    elif (vmin > 0) & (vmax > 0):
        cmap = truncate_colormap(cmap, 0.5, 1.0)
    else:
        cmap = shift_colormap(cmap, start=0.0,
                              midpoint=1 - vmax / (vmax + abs(vmin)), stop=1.)

    sm = plt.cm.ScalarMappable(cmap=cmap, norm=plt.Normalize(vmin=vmin, vmax=vmax))

    # Middle map
    kwargs1 = {'edgecolor': 'white', 'alpha': .65, 'linewidth': 0.2}
    params['geometry'] = params.buffer(0)
    gpd.GeoSeries(params.unary_union.boundary).plot(ax=ax[1], color='black', linewidth=0.5)
    params.plot(coef_surfaces[0], cmap=sm.cmap, ax=ax[1], vmin=vmin, vmax=vmax, **kwargs1)

    divider = make_axes_locatable(ax[1])
    cax = divider.append_axes("bottom", size="5%", pad=0.1)
    cbar = plt.colorbar(sm, cax=cax, orientation='horizontal')
    cbar.ax.tick_params(labelsize=11)

    ax[1].tick_params(axis='both', which='both', bottom=False, top=False,
                      left=False, right=False, labelleft=False, labelbottom=False)

    crit = gwr_object.critical_tval()
    df['var_t'] = var_t
    df['var_se'] = var_se
    df = df.sort_values(coef_surfaces).reset_index().drop('index', axis=1)

    clust1 = df[df['var_t'] > crit]
    if not clust1.empty:
        gpd.GeoSeries(clust1.unary_union.boundary).plot(ax=ax[1], color='black', linewidth=0.8)

    clust2 = df[df['var_t'] < -1.*crit]
    if not clust2.empty:
        gpd.GeoSeries(clust2.unary_union.boundary).plot(ax=ax[1], color='black', linewidth=0.8)

    # Bottom plot
    ax[2].errorbar(range(len(df)), 
                   params[coef_surfaces].values.flatten(), 
                   yerr=crit * var_se.values,
                   ecolor='grey', capsize=1, c='grey', alpha=.65, lw=.75)

    color1 = np.array([(sm.to_rgba(v)) for v in clust1[coef_surfaces[0]].values.flatten()])
    for x, y, e, c in zip(clust1.index, 
                          clust1[coef_surfaces[0]].values.flatten(), 
                          crit * clust1['var_se'], 
                          color1):
        ax[2].errorbar(x, y, e, lw=2.25, capsize=5, c=c)

    color2 = np.array([(sm.to_rgba(v)) for v in clust2[coef_surfaces[0]].values.flatten()])
    for x, y, e, c in zip(clust2.index, 
                          clust2[coef_surfaces[0]].values.flatten(), 
                          crit * clust2['var_se'], 
                          color2):
        ax[2].errorbar(x, y, e, lw=2.25, capsize=5, color=c)

    ax[2].axhline(0, c='black', linestyle='--')
    ax[2].tick_params(axis='both', labelsize=11)
    
    # Titles for each subplot
    ax[0].set_title("Bandwidth Selection (AICc)", fontsize=13, pad=10)
    ax[1].set_title("Spatial Surface of Local Intercept", fontsize=13, pad=10)
    ax[2].set_title("Coefficient Uncertainty (Caterpillar Plot)", fontsize=13, pad=10)
    
    # Axis labels
    ax[0].set_ylabel("AICc", fontsize=11)

    ax[2].set_ylabel("Coefficient Estimate", fontsize=11)
    ax[2].set_xlabel("Spatial Unit Index", fontsize=11)

    fig.tight_layout()
    fig.suptitle(f'Three Panel Visualization for the Local {coef_surfaces[0]}', fontsize=15, va='baseline', y=1)
    plt.savefig('3panel2.png')
    plt.show()
