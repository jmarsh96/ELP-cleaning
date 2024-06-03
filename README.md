# ELP-cleaning
Data cleaning for the English Lexicon Project [1]. Here the raw LDT files are processed into the following outputs:

* ELP_individual_level - Contains the raw LDT files appropriately combined
* ELP_single_trial - Contains the individual level data that has been cleaned (see below)
* ELP - Contains the mean response time data for each word

where each of the files above are available in .csv or .rds formats.

The precise data cleaning steps applied are as follows. Firstly, we pull together the LDT files appropriately and remove any erroneous data sources, for example, entries with incorrect data or files recommended to skip (see https://osf.io/n63s2/wiki/home/ for more details). This forms the "ELP_individual_level" data set, which contains all raw information including inaccurate observations (accuracy = 0) and non-words (type = 0).

Next, we proceed to clean the data using the following steps. We set an accuracy threshold of $\alpha = 0.6$ whereby we are prepared to remove any participant who achieves an accuracy score lower than the threshold. In addition, we define the minimum and maximum RT thresholds as $RT_{\min} = 150$ and $RT_{\max} = 2000$ respectively and proceed to calculate the proportion of responses that are out of this range for each participant. Participants that have this proportion greater than $\beta = 0.2$ are also removed from the analysis. Once we filter the data set and remove all participants identified above, we then remove any observation outside of the acceptable range.


[1] Balota, D. A., Yap, M. J., Cortese, M. J., Hutchison, K. A., Kessler, B., Loftis, B., Neely, J. H., Nelson, D. L., Simpson, G. B., & Treiman, R. (2007). The English Lexicon Project. Behavior Research Methods, 39, 445-459.




