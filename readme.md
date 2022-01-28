**Last update**: 28 January 2022 by Thomas


This folder contains the scripts and documentation related to the publication:

- Feliciani, T., Morreau, M., Luo, J., Lucas, P., & Shankar, K. (2022). Designing grant-review panels for better funding decisions: Lessons from an empirically calibrated simulation model. *Research Policy*, 51(4), 104467. [https://doi.org/10.1016/j.respol.2021.104467](https://doi.org/10.1016/j.respol.2021.104467)

Follows a description of the contents:

# Documentation: "ODD protocol.docx"
This file contains the *ODD protocol* (Overview, Design concepts, Details) for the simulation model.

# Scripts
These scripts run the ABM and plot the results shown in the paper. The code runs in R 4.1.0. Some of the scripts rely on a few external libraries. These are listed at the top of each script. Here follows a summary description of the scripts:

## util.r
Here we defined the fundamental functions necessary to run the ABM.
This script is automatically executed by "simulation.r".
Runs in base R.

## simulation.r
Defines the main function to run the ABM of peer review. At the end of the script we provide an example call to the function to show how to run the ABM.
Runs in base R.
Relies on the extra R libraries *irr* and *AUC*.

## batteries.r
This script sets up an automatic swipe of the specified parameter space. It loads all required resources (i.e. "simulation.r" and "util.r"), runs the requested simulations, and saves the output to the folder "./output/".
Relies on the extra R library *compiler*.

## results.r
This scripts loads the results files generated by "batteries.r", draws all figures for the accompanying paper, and saves them to the folder "./outputGraphics/".
Relies on the extra R libraries *ggplot2*, *reshape2*, *viridis* and *ggpubr*.






