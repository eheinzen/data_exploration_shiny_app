News and Updates for this Shiny App:

# v2.0.0.9000, 09/12/2019

- Updated code based on `dq` updates.

- Added notes about compute time.

- Removed support for `.rds` files.

- Made data import a whole tab, with options for column types.

# v2.0.0, 08/20/2019

- Used `dq` package.

# v1.6.0, 06/06/2019

- Updated documentation.

- Made plots interactive. (#21)

- Allowed for tableby download. (#1)

- Allowed for strata term in tableby output. 

- Added link to rtools.mayo.edu.

# v1.5.1, 06/06/2019

- Updated documentation.

# v1.5.0, 05/28/2019

- Added documentation. (#20)

# v1.4.1, 05/24/2019

- Added the app version to the "Documentation" tab.

# v1.4.0, 03/07/2019

- Made plots skinnier and labels bolder.

- Added a warning when no dataset is selected.

- Tweaked number of digits shown for the trend test.

- Changed trend test permutations to 100 instead of 10.

- Changed trend test to ignore the first 5 percent or 5 observations, whichever is bigger.

# v1.3.0, 02/14/2019

- Added outlier cutoff to univariate tab. (#23)

- Added outlier cutoff to by-observation tab to color significant points. (#25)

- Sped up multivariate outlier detection and trend test. (#17)

- Added title to multivariate outlier plot. (#24)

- Added data structure output. (#19)

- Fixed some bugs with trend test. (#22)

# v1.2.0, 12/20/2018

- Added Q-Q plot.

- Updated the labels on the data quality tables.

- Added some validation to inputs.

- Changed the theme to use Mayo colors.

# v1.1.0, 12/19/2018

- Added a data quality tab.

# v1.0.3, 06/21/2018

- Updated the error handling to use `shiny::validate()`. (#14)

- Made `tableby()` output prettier.

# v1.0.2, 10/16/2017

- Added "Use sample dataset" button. (#3)

- Increased the maximum file size to 10 MB. (#4)

- Added data table tab. (#2)

- Added options to transform x- and y-scales. (#6)

- Added code to error out more graciously. (#7)

- Added link to NEWS file. (#10)

# v1.0.1, 10/12/2017

- Fixed reading in of `.tsv` and `.txt` files.

# v1.0.0, 10/12/2017

- Deployed the basic app, with three main features:

  - Make `tableby` results (Summary Statistics)
  
  - Make ggplots (Plot)
  
  - Make KM-plots (Survival Analysis)
