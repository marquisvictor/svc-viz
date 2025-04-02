### SVCVIZ
![Conda (channel only)](https://img.shields.io/conda/vn/conda-forge/geosnap)
![GitHub commits since latest release (branch)](https://img.shields.io/github/commits-since/oturns/geosnap/latest)

<div align="center"><img src="notebooks/data/svcvizlogo.png" width="600px" /></div>

<br /><br />
The spatially varying coefficient visualization (svc-viz) tool is an open-source Python software package designed to faciilitate the visualization of SVC model results with minimal effort. Svc-viz provides a codified interface for interpreting the results of local regression models based on the existing best practices for visualizing these models, requiring only a minimal amount of coding.

<div align="center"><img src="notebooks/data/svcviz-diagram.png" width="600px" /></div> 
<br /><br />

### Features
- Enables visualization of coefficient estimates from SVC models, adhering to visualization best practices as outlined in Irekponor and Oshan (in preparation).
- Facilitates exploration of replicability by comparing coefficient surfaces across two different SVC models.
- Provides a user-friendly 3-panel visualization template for systematic and consistent analysis.
- Offers simplicity and compatibility with any SVC model, ensuring broad applicability with minimal effort.

### Installation:

svc-viz can be installed from PyPI:

```bash
$ pip install svc-viz
```

To install the latest version from Github:

```bash
$ pip install git+https://github.com/marquisvictor/svc-viz.git
```
### Usage:
After installing svc-viz, it can be imported into a python environment with the following command: 
```bash
> from svc-viz.viz_gwr import compare_two_surf, viz_gwr, compare_conf, three_panel
```
