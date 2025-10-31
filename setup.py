from setuptools import setup, find_packages

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setup(
    name='svc_viz',
    version='0.1.5',
    author='Victor Irekponor, Taylor Oshan',
    author_email='vireks@umd.edu',
    description='A python software package for visualizing the results of spatially varying coefficient (SVC) models, enhancing reproducibility and replicability',
    url='https://github.com/marquisvictor/svc-viz',
    packages=find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License v2 (GPLv2)",
        "Operating System :: OS Independent",
    ],
)