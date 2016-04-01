# Fork This Script Friday (#5)

## Feature Selection

I recently saw a post about beginner analytics for annual giving and there were some really good suggestions on where to start.

I though as part of a multi-part series we could all take a real example as see what we can do with it.

Here is the start of a very simple model.

As stated on the DMM.  It is good to start with a specific question.  Here I pulled all donors from two years ago and I am trying to predict who then gave again last year.  Since those fiscal years are complete we can use them to build a model and then apply it to those that gave last year and predict who will give this year.

I did a little pre-processing and clean up in SQL while I was pulling the data.  A data dictionary is provided.

I used a lot of the ideas shared on the DMM for feature selection.

For me, I think one of the best places to start is to visualize the data and look for patterns and then do some additional clean-up.

After that, it is easy to build a simple model.  Check which features had the most influence and the accuracy.

From there, you can improve performance by selecting new features, removing some additional features, feature engineering and tuning the parameters of the model.  As well as running multiple models and ensembling them.

Here, I do two things:

Exploratory data plots:

(I will move this to am RMarkdown doc but from here I can't publish to RPubs for some reason so maybe over the weekend.)

I like to go feature by feature and see where there is a difference in the pattern between the two target groups.  In this case, those that gave again and those that did not.

I also include a correlation plot

Caret offers some more helpful functions for this step but that package is not loading for me at the moment.

After this, I build a simple model and check the feature importance and accuracy to establish a baseline.

If you are willing to work with this semi-messy script:

1. What types of plots are you using?
2. What tactics do you use in the exploratory stage?
3. What do use to check which features to keep and what do you use to select features to remove?


Any other ideas? Steal this script or maybe just share what you are doing with feature selection on the DMM