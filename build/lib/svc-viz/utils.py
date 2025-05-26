import numpy as np
import matplotlib as mpl
import scipy.stats as stats
import seaborn as sns
import scipy.stats as stats

"""
Author Information:
- Name: Victor Irekponor, Taylor Oshan
- Email: vireks@umd.edu
- Date Created: 2023-10-12
"""

sns.set_theme(style='white', context='talk', rc={'font.family':'serif', 'font.serif':['Georgia'], 'font.size':13})
sns.set_palette('bright')


def shift_colormap(cmap, start=0, midpoint=0.5, stop=1.0, name='shiftedcmap'):
    """
    Shift the colormap to make visualization easier and more intuitive.
    
    Parameters:
    - cmap: A matplotlib colormap instance or name.
    - start (float): Start point of the new colormap segment. Defaults to 0.0.
    - midpoint (float): Midpoint (anchor point) for the shifted colormap. Defaults to 0.5.
    - stop (float): End point for the new colormap segment. Defaults to 1.0.
    - name (str): The name of the new colormap. Defaults to 'shiftedcmap'.
    
    Returns:
    - new_cmap: A new colormap that is shifted as per the given parameters.
    """

    cdict = {'red': [], 'green': [], 'blue': [], 'alpha': []}
    reg_index = np.linspace(start, stop, 257)
    shift_index = np.hstack([
        np.linspace(0.0, midpoint, 128, endpoint=False),
        np.linspace(midpoint, 1.0, 129, endpoint=True)
    ])
    for ri, si in zip(reg_index, shift_index):
        r, g, b, a = cmap(ri)
        cdict['red'].append((si, r, r))
        cdict['green'].append((si, g, g))
        cdict['blue'].append((si, b, b))
        cdict['alpha'].append((si, a, a))
    new_cmap = mpl.colors.LinearSegmentedColormap(name, cdict)
    return new_cmap


def truncate_colormap(cmap, minval=0.0, maxval=1.0, n=100):
    """
    Truncate a colormap by creating a new colormap from a subsection of an existing one.
    
    Parameters:
    - cmap: A matplotlib colormap instance.
    - minval (float): The minimum value for the new truncated colormap. Defaults to 0.0.
    - maxval (float): The maximum value for the new truncated colormap. Defaults to 1.0.
    - n (int): The number of divisions for the linspace function that generates new colors. Defaults to 100.
    
    Returns:
    - new_cmap: A new colormap that is truncated as per the given parameters.
    """
    new_cmap = mpl.colors.LinearSegmentedColormap.from_list(
        'trunc({n},{a:.2f},{b:.2f})'.format(n=cmap.name, a=minval, b=maxval),
        cmap(np.linspace(minval, maxval, n)))
    return new_cmap


def merge_index(left, right):

    """
    Merge two pandas DataFrames based on their indices.
    
    Parameters:
    - left: The left pandas DataFrame.
    - right: The right pandas DataFrame.
    
    Returns:
    - A new pandas DataFrame resulting from merging the left and right DataFrames based on their indices.
    """
    return left.merge(right, left_index=True, right_index=True)


def mask_insignificant_t_values(df, alpha=0.05):

    """
    Calculate t-values from a dataframe containing coefficients and their standard errors.
    Mask insignificant t-values with zero.
    
    Args:
    - df (pd.DataFrame): Dataframe containing columns with coefficients (prefixed with "b") and standard errors (prefixed with "se").
    - alpha (float): Significance level. Default is 0.05 (95% confidence).
    
    Returns:
    - pd.DataFrame: A new dataframe with coefficients and t-values, where insignificant t-values are set to zero.
    """
    
    coefficient_cols = [col for col in df.columns if col.startswith("beta")]
    se_cols = [col for col in df.columns if col.startswith("std")]
    
    # Check if the number of coefficient columns matches the number of standard error columns
    if len(coefficient_cols) != len(se_cols):
        raise ValueError("Mismatch in number of coefficient and standard error columns.")
    
    # Calculate t-values
    for coeff_col, se_col in zip(coefficient_cols, se_cols):
        t_value_col = f"t_{coeff_col}"
        df[t_value_col] = df[coeff_col] / df[se_col]
    
    # Calculate degrees of freedom (assuming we're in the context of linear regression)
    # df - number of predictors - 1
    degrees_of_freedom = len(df) - len(coefficient_cols) - 1
    
    # Calculate critical t-value from the t-distribution
    critical_t_value = stats.t.ppf(1 - alpha/2, degrees_of_freedom)
    
    # Mask where t-values are insignificant
    insignificant_mask = df.filter(like="t_").abs().le(critical_t_value)
    # Set t-values to zero where they are insignificant
    for t_value_col in df.filter(like="t_").columns:
        df.loc[insignificant_mask[t_value_col], t_value_col] = 0
    
    return df