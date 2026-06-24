---
author: Cristian Iranzo
---
<!-- badges: start -->
[![R-CMD-check](https://github.com/CristianICS/lidaynight/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CristianICS/lidaynight/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# `lidaynight`: Day–night UAV LiDAR comparison tools

This R package provides functions for comparing day and night LiDAR data collected using uncrewed aerial vehicles (UAVs).

The package includes tools to:

1. Retile original LiDAR flight lines into a regular grid.
2. Classify ground points using the Progressive TIN Densification (PTD) algorithm.[^1]
3. Compute point-cloud statistics for areas with high overlap.
4. Assess point-cloud accuracy using ground reference points.


[^1]: Progressive TIN Densification (PTD). [Axelsson (2000)](https://www.isprs.org/proceedings/xxxiii/congress/part4/111_xxxiii-part4.pdf)
