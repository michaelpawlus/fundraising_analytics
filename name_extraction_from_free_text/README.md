# Fork This Script Friday (#1)

## Name Extraction from Free Text

This script is the first of hopefully many "Fork This Script Friday" submissions.

Currently, this R Script will use regular expressions to extract two words that both have capital letters from free text (as is the case with names).

This has practical applications if, for example, you have survey data that contains the names of faculty, staff, doctors or other key individuals within your organization and you want to check the frequency with which these individuals are mentioned.

In my case, we collect GradFest surveys when our students graduated and one question asks which professors have had the most impact on your time here.  Knowing which professors are most frequently referenced can help us from a strategy perspective think about who we should partner with.

The sample data set included is a subset of this survey data with all professor names changed to musicians from the 1960s.  The sample data only contains two columns: a numerical id column and a column of free text strings containing the response to the question: "which faculty/staff member has had the biggest impact on your time at university?"

In its current state, the script provided does require some manual clean-up.  In my real data set, I was also getting the name of my university and a few other non-name values returned. 

In the sample data you will see that "John Lennon" and "Professor Lennon" are counted separately when they should be counted as a single entity.

A few suggestions for improving on this script:

1. Find a way to combine all like individuals even when they are referred to by different names:
  * For example: John Smith, Professor Smith, Dr. Smith, Prof, Smith, etc.
  * Also, fuzzy searching methods to combine values such as  John Smith, Johnny Smith, Jonathan Smith, Jon Smith, Jon W. Smith, John Smyth
2. Find a way to exclude all non-name values (perhaps referencing a taxonomy of names)
3. Extracting all rows that contain a particular name
4. What other data points should be extracted from survey data? (may not be present in this sample data set) 

What other ideas do you have for working with this type of data set or improving on this script?