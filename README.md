# EGU Short course 

## Communicating data quality through open reproducible research

This repository contains the materials for the [short course](https://meetingorganizer.copernicus.org/EGU23/session/46566) given at [EGU 2023](https://www.egu23.eu/).
The idea is to demonstrate the use of reproducibility tools (R Markdown, GitHub, Binder, The Whole Tale) in the context of the [MINKE](https://minke.eu/) research project.

### Branches

- __main__ contains a computational workflow to generate a map showing an interpolation and quality parameters to filter the underlying dataset. The structure of the repository is based on this [binder template](https://github.com/MarkusKonk/binder_template_rmarkdown).
- __map1__ and __map2__ contain a short workflow to show the importance of the versions of the used library (The Turing Way Community 2019). 
- __calc__ contains the same code as __map1/2__ but with a different Python version. It is used to show that the result of a simple calculation __1/5__ is different in Python 2 and Python 3. 

### Links

- Binder: https://mybinder.org/
- The Whole Tale: https://wholetale.org/  

------------------------------------------

- Binder for __map1__: https://mybinder.org/v2/gh/MarkusKonk/egu_shortcourse.git/map1
- Binder for __map2__: https://mybinder.org/v2/gh/MarkusKonk/egu_shortcourse.git/map2
- Binder for __calc__: https://mybinder.org/v2/gh/MarkusKonk/egu_shortcourse.git/calc
- Whole Tale for __map1__: tba
- Whole Tale for __map2__: tba
- Whole Tale for __calc__: tba
- Whole Tale for __main__: tba

### References

The Turing Way Community, Becky Arnold, Louise Bowler, Sarah Gibson, Patricia Herterich, Rosie Higman, … Kirstie Whitaker. (2019, March 25). The Turing Way: A Handbook for Reproducible Data Science (Version v0.0.4). Zenodo. http://doi.org/10.5281/zenodo.3233986
