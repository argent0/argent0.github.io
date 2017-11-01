---
title: Medical Appointments No-Show
abstract: A patient makes an appointment with a doctor but then fails to show up. Exploratory data analysis for medial appointments data.
---
# Exploring No Show data

This is an exploratory data analysis (EDA), of data describing medical
appointments.

The objectives are: first, to visually present the structure of the
sample data; and second, to show how the appointments' features relate
to whether the patients will show up nor not.





## Medical Appointment Data.

The appointments' characteristics present in the data are:


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

Some of them might require a short explanation:

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

### Computing Derived Features.

One of the factors that might be relevant to the show up ratio is the
length of time from the day the appointment was scheduled to the appointment
itself: 

**Wait** the number of days from the scheduling date to the appointment date.



### Removing malformed records.

Some rows with negative `Wait` or `Age` where ignored when performing the
analysis.



## Exploration

This section contains the actual exploration of the data after having
established what features are present and removed the malformed records.

### Some numbers

The first set of questions are about quantities. 



* How many appointments? 110 521 appointments.
* How many patients? 62 298 patients.
* Where do the appointments take place? In 81 different neighbourhoods.
* When were the appointments scheduled? From 2015-11-10 to 2016-06-08.
* When was the appointment? From 2016-04-29 to 2016-06-08.
* How many days did the patients had to wait? From 0 to 179 days.

### The target composition.

Now a visualization of the sample's composition. Starting with the proportion
of missed appointments, which is the target variable of the study. The data
shows that most patients don't miss their appointments.

![plot of chunk no_show_distribution](figure/no_show_distribution-1.svg)

### Features composition.

It is also interesting to visualize the relation between the features and the
target.

Two questions about the features are explored: what is the feature's
distribution in the data?; and, what is it relation with the target?

**Gender**

There are almost twice as many women than men.
Both genders are equally likely to miss their appointments.

![plot of chunk gender_distribution](figure/gender_distribution-1.svg)

**Patient's Age**

The patients' age distribution. The relation with whether they show up or don't
indicates that patients around the 15-20 age range are slightly more likely to
miss the appointment.

As expected, there are few samples for very old ages.


![plot of chunk hist_ages_2](figure/hist_ages_2-1.svg)

**Waiting Time**

Most appointments are same-day or for the next day. Those
appointments also have better turn up. Lower waiting times appear to improve the
show up ratio.  

*Warning*, the histograms below have non-uniform bin sizes.



![plot of chunk hist_wait_2](figure/hist_wait_2-1.svg)

**Distribution of Neighbourhoods**

There is considerable variance in the quantities of patients by neighbourhood,
but not so in the show up ratio.

![plot of chunk hist_neighbourhood](figure/hist_neighbourhood-1.svg)

**Distribution of other categorical variables**

Most notably, it appears that patients that received an SMS (`SMS_received == 1`)
where also less likely to show up.

This is due the fact that patients with same-day appointments don't receive a
SMS and are, simultaneously the more likely to show up (maybe they already at
the hospital).

![plot of chunk hists_categorical_variables](figure/hists_categorical_variables-1.svg)



Actually, if we only consider appointments that don't occur the same day,
patients who received a SMS a bit more likely to show up.

![plot of chunk not_same_day_sms](figure/not_same_day_sms-1.svg)

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
likely to show up again. This is considering appointments on different days.

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
