# subs-predict

# Purpose

The purpose of this whole activity is to improve our ability to

1. Make detailed and accurate budgets based on a simulation of future subscriptions based on 
   current subscribers and their subscriptions and a budget for new subscribers coming in 
   in the future from marketing activities.
2. Make detailed and accurate lifetime predictions (and therefore CLTV predictions) for new users 
   so that we can calculate the proper CAC that we are willing to pay for these users, depending on 
   market, possibly device and other controllable variables.

# Goal

The goal of the project is

1. To come up with a good probabilistic prediction model for how actual subscriptions in given markets, with given ages,
   with given periodlengths, (and possibly other explanatory variables) that expire will continue. Either they churn 
   with some probability or they continue as a new subscription of a certain periodlength with some probability.
   * This model can be used in simulating actual budgets by taking the actual subscribers and propagating them forward
     in time in a very detailed manner.
2. To be able to determine good lifetime estimates for a new fresh user entering today a given market.
3. To find out how to aggregate and work with explanatory variables in 1. and 2. (maybe in different ways) 
   so that we get the best possible
   predictions - that is probably a balance between using important information like market and periodlength, 
   but not subdividing too much and just create noise.
4. To investigate which explanatory variables are important	for making good estimates in 1. and 2.
5. To get a good grasp of how to use "recent" observations of churn and continuation to make the best
   possible "current" guess at future behavior. Here, we probably strike a balance between using only very recent 
   expiry behavior and having enough data for robust predictions.
6. To look at time evolution of lifetime estimates, by moving the "observation window" back in time and 
   seeing how we would have estimated lifetime at previous times, using the same models.
	 
# Main complication

We know that churn and continuation depends heavily on

1. The market that the user is in (we are present in 200 markets...)
2. The age of the user whose subscription expires
3. The periodlength of an expiring subscription
4. The business model "structure" that the user is in
5. Probably a few other variables, like device...

Naively doing "churn frequencies" on all combinations of the above will lead to having virtually
no data on all but a few prominent combinations. So the challenge is to come up with something that
is detailed and accurate, using details of 1. to 5., but "sharing information" across the whole material.

# Main insight

It seems that a we get a lot of good results from a generalized linear model that 

1. uses a proper aggregation of "structure" (like age, periodlength, etc.) as one variable
2. uses a proper aggregation of market into big, important ones, and smaller "lumped together" markets
   as another variable
3. assumes the "log of the monthly churn-rate" is well represented as linear model in 1. and 2. A bit like
   a "proportional hazard model", where each market provides a multiplier on the same set of churn-rates as
   viewed from the structure dimension

We want to investigate how good a model we can create exploring this idea.

Important note: For doing the model, I am using:

```R
new_model=glm(logmonthchurn~marketcat+churncatnomarket, data=churntable, weights = churntable$weight)
```

Very little insight has gone into whether this is actually a good choice for this problem. But 
good results seem to come out.

# Idea of how to approach the project

1. Find a model that predicts churn reasonably well as a function of market, age, periodlength, device, etc.
   * Understand the actual structure of subscriptions
   * Plot some representative churn rates on various aggregations to get a feel
   * Get a a basic model for predicting churn running and show some plots of quality
2. Find a model that predicts the periodlength (months) of subses that continue (don't churn)
   as either 1, 3, 12 or 14
   * Before modeling, understand qualitatively the structure of what typically continues as what
     * Dependencies such as siteverkey, business model, market?, age, others?
	 * Document it through some plots or tables
	 * Document in writing typical cases of subscription histories
   * Create a model that predicts reasonably well how a non-expiring subs continues 
     * Find some reasonable aggregation model
3. Create a lifetime model
   * Using 1. and 2., one can start a user in, e.g. the US. That user starts with a
     1-month subscription. It then forks out. The user can do one of the following
	   * Churn and get no more subscriptions (with some percentage chance). Then this branch is finished.
	   * Continue with one of several kinds of subscription periodlengths (1,3,12,24) with certain percentages
	     * And each of these can branch on to either churn or several types of periodlengths
		 * There is a recursion here which can be terminated when the total probability of being on a particular sub-branch
		   drops below some pre-decided level.
	* Doing this recursively on the branching tree of possibilities will then lead to a lot of lifetime "chunks" with 
	  given probabilities.
	* Summing periodlength * probability for all subes in the tree will give a total estimate of life-time.
4. Investigate a lot and get best model
   * Now we have a basic reasonable model running. Then we can try to improve it and investigate it's 
     properties, it's stability and so on.
   * Play with different aggregation models
   * Play with different statistical models
   * Find ways of assessing the quality of the different models, both 
     in terms of making good precictions and in terms of sensitivity to using only sample data.
   * Find good ways of visualizing the performance of different models (e.g. by looking at how we do on 
     important representative sub-groups of customers and their histories).
   * Note: Probably, in the investigative phase, it will be beneficial to create some code structure, where different 
     prediction models and different aggregations can programmatically be "plugged in" to a general engine that performs
	 all the analysis on a given model.
   
# twodim-logmonthlychurn.Rmd

This is a *very messy* throw together of bits of code from various different projects. It only serves to document
what I am thinking and do some initial proof-of-concept that the approach outlined above can provide good estimates
of churn probabilities across structure and market. The main point is that there is a working approach 
in the "proportional hazard" idea plus a linear model for the log of churn using "structure * market".

Probably, initial.Rmd should be thrown away entirely and *not* form the basis for this project, but only serve 
as an explanation of the idea, expressed in code.

The code that generates aggregation on "structure" is adapted from my current budget-model. 
I know from using it over time, that this aggregation is reasonably good and captures important real aspects of 
subscription behavior. But that is not to say that it should not be heavily challenged. But it can serve as an informed
starting point.

Other parts of the code is copy-paste from other projects on Upwork. And still other parts is glue that I wrote yesterday.
The goal was not pretty, well-structured or maintanable code, but rather to provide you with information *fast*.

# Stuff to look into   
  * Understand "kink" in churn curves around 5 months of age.