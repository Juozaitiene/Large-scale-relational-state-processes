# Relational-State Network Models

Code and materials for reproducing simulation studies and an empirical
application of continuous-time relational-state network models under
full-event-history and panel-data observation schemes.

## Overview

The accompanying study introduces a framework in which network ties evolve
between two or more relational states. This repository contains:

- a full-event-history simulation for a three-state network;
- a panel-data simulation for a binary network;
- an empirical application to adolescent friendship data;

### Scripts

- `R/01_full_history_simulation.R` reproduces the full-history simulation and
  the results associated with Figure 3.
- `R/02_panel_simulation.R` reproduces the panel-data simulation and the
  results associated with Figure 4.
- `R/03_empirical_application.R` runs the empirical analysis associated with
  Section 5 and Table 1.
  
## Data

`data/friendship_inputs.RData` contains the input objects required for the
empirical application:
- three friendship-network matrices;
- sex and age variables;
- tobacco, cannabis, and alcohol-use variables.
Participants are represented by numeric labels. 
