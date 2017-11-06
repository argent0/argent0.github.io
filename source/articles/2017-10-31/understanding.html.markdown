---
title: Tree analysis for Medical No Show data
abstract: A tree model used to discover patterns in the medical No Show data.
---

# Tree Analysis

This article deals with further analysing the data explored on a previous
[article](/articles/2017-10-04/exploration.html).
Briefly, the data describes patient's appointments to the doctor. Particularly
it says whether the patient showed or not. Using the original data nomenclature:

* `No.show = Yes` means the patient missed the appointment.
* `No.show = No` means the patient showed up to the appointment.

The appointment's characteristics relevant to this analysis are:

* `Wait`: The number of days the patients had to wait for their appointment.
* `Age`: patient's age
* `Gender`: self explanatory
* `SMS_received`: whether the patient recived an remainder by SMS
* `Scholarship` : whether the patitent receives economic aid.

The data contains more characteristics about the appointmets, but the variables
above where the ones that resulted more relevant in this analysis.

The objective is to gain knowledge of what groups of patients are more prone to
miss their appointments. This knowledge can, in turn, be used to sustain future
policies or evaluate the effectiveness of current ones.

One way to achieve the objective is to
train a [decision tree](https://en.wikipedia.org/wiki/Decision_tree_learning).
This
is a machine learning technique that, in this case, will attempt divide the
appointments using a sequence of conditions into those that realize and those
that don't.  Although it may not be as accurate as other approaches,
decisions trees stands out for producing easy to interpret
and understand relations among the provided data and the target variable. These
produced relations will be useful tools for sustaining future decisions.

In this particular case: the **features** are data about the appointment and the patients;
while the **target variable** is whether the appointment realizes or not.

## The Resulting model

Before entering into technical details, here is the resulting tree model

**WARNING!**:

* Use <kbd>Ctrl</kbd>&nbsp;<kbd>+</kbd> to enlarge the image below.
* Use <kbd>Ctrl</kbd>&nbsp;<kbd>-</kbd> to downscale.
* Use <kbd>Ctrl</kbd>&nbsp;<kbd>0</kbd> to return to the default size.

![plot of chunk resulting_model_show](figures/understanding.md/resulting_model_show -1.svg)

The ellipses represent the nodes. Above each node there is a number inside a
square that I'll use to refer the nodes here. Below the variable name inside
the node is the estimated probability that the partition criteria is an accident
originating from random fluctuations.

For example, node 1 partitions the appointments into those that occur in the
same day (`Wait <= 0`) and those that don't (`Wait > 0`). Comparing the leaves
(Terminal nodes) to the left and to the right of node 1, it is evident that
same-day appointments have higher probability of realizing (<i>i.e.</i> to
become real).

Those patients may already be at the hospital when scheduling the appointment.
The source of the data doesn't mention whether this is the case.

Notably, among the nodes to the left of node 1 is node 6 (a leaf). Node 6 represents
same-day appointments (node 1), of patients less than 21 years old (node 2) but
older than 6 years old (node 3), who are male (node 5). The model shows that
this is the population that misses most of the same day appointments.

To right of node 1, nodes 20 and 30 show that `SMS_received > 0` increases the
chances that the patients show to their appointments. Contrary to what the
previous, more superficial, analysis suggested; sending SMSs is working for the
populations in nodes 24 and 32.

Finally, in the scope of this analysis, patients with `Scholarship` in nodes 19
and 33 are more prone to miss the appointment.

## Analysing the results.

The tree model has provided interesting insights on the structure of the data
with respect to the `No.show` variable. Yet there is further work to be done in
order to ensure that we are contemplating something more than correlation.

For example, the model shows that patients with `Scholarship > 0` are more prone
to miss the appointment. But it is also plausible that those patients are the
ones more likely to live in poverty, and thus less able to miss a workday.

Something similar could be said about `SMS_received > 0`.

Controlled experiments and statistical hypothesis testing, where applicable,
would shed light on the causality relations. The result of the tree model has
been to guide and suggest which are worthy venues for such experimentation.
Concretely:

* **The less the wait the better**: So policies that reduce the wait should be
  favoured (assuming the hospital's objective is to serve the maximum number of 
  patients).

* **SMSs seems to be working**: It still may not be the case, but experiments
  could be designed to corroborate this hypothesis. Or implement other ways of
  reminding (e.g. e-mail).

# Technical details of the construction of the tree.

The final result of the analysis is the tree model. What follow are the
technical details of its construction.

##  The Decision Tree

The tree model used in this analysing is called *conditional inference tree*.
The used implementation comes from R's
[party](https://cran.r-project.org/web/packages/party/vignettes/party.pdf)
package.

**Constraining the model to prevent overfit**

This particular analysis constrains the tree model in two ways:

1. Each decision in the tree will have an statistical significance, of at least,
   95%. In other terms, the chance of fitting accidental features on the data is less
   than 5%.
2. The *bucket size*, the number of cases at the ends of the tree, should be
   greater than 500.

Both these constrains will improve the model's performance on unseen data, *e.g.*
future data.


## Splitting the data into Train, Validation & Test

In order to construct the model, the data is split in three sets 
using by the feature `AppointmentDay`, which indicated the day that the
appointment takes place, in the following way:

* the first 14 days of data go for training
* the next 7 days for validation
* the final 6 days for testing

This criteria is sustained in the fact that number of planned appointments per
day is reasonably uniform (save for one particular date).

![plot of chunk appointments_per_day](figures/understanding.md/appointments_per_day-1.svg)

The following graph shows the resulting partition sizes:

![plot of chunk sample_split_plot](figures/understanding.md/sample_split_plot-1.svg)

Unfortunately, partitioning this way doesn't distribute the maximum `Wait`
equally among the sets.
Nevertheless, this method is preferred because,
having the sets one following the other in time,
is a better portrayal of the conditions in which the model would be deployed.

![plot of chunk max_wait](figures/understanding.md/max_wait-1.svg)

## Evaluation Criteria

Finding the final model involves training models with different complexities and
picking the one that best generalizes to unseen data.

The [cross entropy](https://en.wikipedia.org/wiki/Cross_entropy), *a.k.a* log loss,
is the measure used to pick the wining model. This error measure favors models
that produce accurate probabilities.

## Learning Curve


**Convergence of the algorithm**

An important question is *whether there is enough data to train a model*.
Complex models being, generally more data demanding than simpler ones. The
common procedure to answer this question is using a *learning curve*.

In a learning curve, an increasing amount of data is used to train the model while
observing its performance on a validation set. This is a diminishing returns
scenario where doubling the amount of data won't generally double the model's
performance. Models reach a, practically, maximum performance after which
using more data doesn't improve performance. Simpler models reach that limit
with less data than more complex ones.

This is the learning curve for this analysis:


![plot of chunk learning_curve_plot](figures/understanding.md/learning_curve_plot-1.svg)

The performance in the validation set shows the diminishing returns of using more
data, but it seems the limit has not been reached yet after spending
all the available data. It also shows, as it is expected, that for small number of samples, the
training and validation errors differ substantially.

One peculiarity in this analysis is that the training
error is greater than the validation error for bigger numbers of samples. As
mentioned before, this might be due to the validation set having "easier" cases
than the training set.

## Validation curve.

**Finding the ideal tree depth**

Another aspect of a tree model is its depth, the maximum number of operations
performed on the sample before getting an answer from the model.

Lower depths mean simpler to interpret models, so they are preferred. This
analysis trains trees of up to a depth of 8. The validation set is used,
afterwards, to pick the simpler one with the least error.

![plot of chunk validation_curve](figures/understanding.md/validation_curve-1.svg)

This curve suggests that a tree of depth 5 is enough for the available amount of
data.

## Evaluating the resulting model

The final model, a tree with depth 5, is trained in both validation and training
set. 

Its performance is evaluated using a test set. In this case, the model has
*log loss* of `0.4267598` in the test set.

**Comparison with the baseline performance**

The baseline model, in this case, is one that assigns to all patients a
the same probability $$p$$ of missing the appointment. This $$p$$ is the ratio between
those who didn't showed up and the total amount of patients.

Such a basic model has a log loss of `0.4798831` in the test set, which is
greater than that of the tree model. This means that the tree model has
succeed in finding populations with markedly different probabilities of showing up.

### Calibration in the test set

Another important aspect of evaluating a model's performance, for the case of
classification, is to check whether the probabilities predicted by the model are
in tune(or calibrated) with those of the population it represents.

This is evaluated using a calibration curve, where the probabilities predicted
by the model are compared against the actual ratios seen in the test set. The
ideal `Test ratio = estimated probability` line is included for reference.

![plot of chunk calibration_curve_test](figures/understanding.md/calibration_curve_test-1.svg)

The plot shows that the model generally tends to slightly overestimate the
probability of patients missing their appointments. However, this is not enough
to prevent the extraction of valuable insights.

## Appendix

The deviation of the ratio in the test set is calculated as the posterior
variance of beta Bernoulli distribution in the limit where the amount of data
overwhelms the prior.

The magnitudes are in the order of $$10^{-4}$$ and are thus omitted in the calibration
curve.

<script type="text/javascript" async src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-MML-AM_CHTML"> </script>
