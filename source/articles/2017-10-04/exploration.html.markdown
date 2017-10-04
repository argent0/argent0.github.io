---
title: Medical Appointments No-show
categories: "exploratory analyis"
order: 1
abstract: "A patient makes an appointment with a doctor but then fails to show up. A visual exploration."
---

##Context

From the source:

> A person makes a doctor appointment, receives all the instructions and
> no-show. Who to blame? 

The objectives of this exploration are:
First, to visually present the composition of the sample data;
And second, to show how the appointments' features relate to whether the
patients will show up nor not.






## Available Features.

The structure of the data is:


~~~
## 'data.frame':	110527 obs. of  14 variables:
##  $ PatientId     : num  2.99e+13 5.59e+14 4.26e+12 8.68e+11 8.84e+12 ...
##  $ AppointmentID : int  5642903 5642503 5642549 5642828 5642494 5626772 5630279 5630575 5638447 5629123 ...
##  $ Gender        : Factor w/ 2 levels "F","M": 1 2 1 1 1 1 1 1 1 1 ...
##  $ ScheduledDay  : Factor w/ 103549 levels "2015-11-10T07:13:56Z",..: 27742 27504 27539 27709 27498 20074 21386 21496 24945 20895 ...
##  $ AppointmentDay: Factor w/ 27 levels "2016-04-29T00:00:00Z",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ Age           : int  62 56 62 8 56 76 23 39 21 19 ...
##  $ Neighbourhood : Factor w/ 81 levels "AEROPORTO","ANDORINHAS",..: 40 40 47 55 40 59 26 26 2 13 ...
##  $ Scholarship   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ Hipertension  : int  1 0 0 0 1 1 0 0 0 0 ...
##  $ Diabetes      : int  0 0 0 0 1 0 0 0 0 0 ...
##  $ Alcoholism    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ Handcap       : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ SMS_received  : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ No.show       : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 2 2 1 1 ...
~~~

Next are some variables that might require a short explanation:

* **ScheduledDay**: The day the appointment was scheduled.
* **AppointmentDay**: The day of the appointment.
* **Neighbourhood**: The Neighbourhood where the clinic is located.
* **Scholarship**: 1 if the person receives
  [aid from the government](https://en.wikipedia.org/wiki/Bolsa_Fam%C3%ADlia).
* **Handcap**: According to the source, A number that represents the person's
  disabilities.
* **SMS_received**: Whether the people received an SMS reminding them of the
  appointment. 
* **No.show**: The value "Yes", means that the person missed the appointment.
  The value "No", means that the person didn't miss the appointment.

## Computing Derived Features.

One of the factors that might be relevant to the turn up ratio is the
length of time from the day the appointment was scheduled to the appointment
itself. Let us, thus, define:

**Wait** as the number of days from the scheduling date to the appointment date.



## Cleaning The Data

Some rows with negative `Wait` or `Age` where ignored when performing the
analysis.



## Data composition

First a visualization of the sample's composition. Starting with the proportion
of missed appointments, which is the target of the study.

![plot of chunk no_show_distribution](figure/no_show_distribution-1.svg)

Now, a visualization of the features and how they relate to the target.

**Gender**

![plot of chunk gender_distribution](figure/gender_distribution-1.svg)

**Age**

After the age of 90 years there are only a few samples, so the proportions aren't
significative.


![plot of chunk hist_ages_2](figure/hist_ages_2-1.svg)

**Waiting Time**

Most appointments are same-day or for the next day. Those
appointments also have better turn up. *Warning*, the histograms below have
non-uniform bin sizes.



![plot of chunk hist_wait_2](figure/hist_wait_2-1.svg)

**Distribution of Neighbourhoods**

![plot of chunk hist_neighbourhood](figure/hist_neighbourhood-1.svg)

**Distribution of other categorical variables**

![plot of chunk hists_categorical_variables](figure/hists_categorical_variables-1.svg)


The last row of plots suggests that when `SMS_received == 0` the patient is more
likely to show up. **Thus the `0` might mean that SMS was received**.

## Some Characteristics

**Are there patients with more than one appointment on the same day?**

Yes, but most have only one.


~~~
## Appointments per day
##     1     2     3     4     5     6     8    10 
## 94323  6531   739   157    33    18     1     1
~~~

**How does showing up for the prevoius appointment predicts showing up for the
next?**

Patients that showed up for their previous appointment are more
likely to show up again.

![plot of chunk previous_appointment](figure/previous_appointment-1.svg)

## [EXTRA]Gender Composition By Age

**Why is the proportion of women so large?**

![plot of chunk age_by_gender](figure/age_by_gender-1.svg)

There is a sharp drop in the proportion of men after the age of 16.

**License**

Released under [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/).
Attributed to Kaggle's user [JoniHoppen](https://www.kaggle.com/joniarroba)
([Twitter](https://twitter.com/jonihoppen),[LinkedIn](https://www.linkedin.com/in/joniarroba/)).
Changes where made to the data as shown in the code.

Source code available on
[Github](https://github.com/argent0/medical-appointment-no-show).
