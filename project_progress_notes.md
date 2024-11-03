# power_outage_national_cvd_hosp progress notes 


November 2nd, 2024:

We know what we have to do, and there's a lot to do! 

We did:
- we fixed the error in the power outage processing pipeline that we found
- updated and documented everything so Vivian can review
- we looked at the relationship between precipitation and hospitalizations, and
it is strange. I think modelling it linearly might be ok. Maybe the relationship 
is explained by geography/spatial correlations, but need to ask marianthi the best way to model/if this is the correct conclusion.
- we looked at the relationship between wind speed and outage and hospitalizations, and I think creating a four category variable makes the most sense
- again need to check w marianthi
- or maybe just modelling wind speed with a natural spline?
- idk i think both should be natural spline with df = 3, doing this for now.

Need to do:
- calculate coverage (DONE) 
- missingness interpretation using simulation paper results (DONE)
- look at autocorrelations in order to decide if we need dlnm or unconstrained 
lags (DONE - autocorr is really small, no need for DLNM)
- analyses for CVD and respiratory separately (ongoing)
- fix hypertension hosp codes (DONE)
- check dispersion

Additional analyses:
- sex
- age

Additional datasets:
- DME use
- poverty
- hot and cold days 



Update october 31st:
- updated power outage pipeline, but haven't checked to see if it's correct
- suspicious that each county has same dimension, but I guess most chunks might
be the same (edited to add: this is right)

Had meeting yesterday about POUS.
Next steps in project:
- check autocorrelation of lags for 4 and 12 hour outages as well
- for next analyses, want to look for effect mod by DME use, poverty, age (75+ 
vs <75), and hot and cold days, sex
- also need to run separate analyses by cardio visits and respiratory, and 
maybe not exclude hypertension

Notes about changing b02_expand_outages:
- needed to put in locf changes discovered while working on simulation 
- need to check that this works with the rest of the pipeline and then
rerun everything.
- will take 2 hrs to rerun

Notes from meeting with marianthi about additional analyses and controlling 
for confounding:

- cvd and respiratory visits have different lag patterns after heat
- this might explain some of the lag issues we've been seeing in the one plot
that i made
- need to run analyses where they are separate, to see if the effects are 
different
- if they are, it might not make sense to run the rest of the analyses with them 
together

Before running any more models, want to also decide on exactly how to control 
for confounding
- temp: non-linear, that's fine, ns df = 3
- for wind, when we are controlling for wind, we wanted to capture the 
wind-related weather events that have an impact on hospitalizations, including hurricanes, 
tropical cyclones, tornadoes, blizzards, gales, and other snowstorms 
- we made a 5-category variable for wind based on the beaufort scale bc we 
thought that reflected
how different events would affect both power outage and hospitalization rates 
- need to create that variable and make sure we have support in each of the five
categories to create estimates. 

- for precipitation, check that it includes snow (edited to add: IT DOES)
- would be associated w hospitalizations
- don't know how related to hosp but we do know it's related to PO
- make model of CVD and respiratory outcomes separately and see what the 
association is between them and precip after controlling for temperature 
- need to include a penalized spline in a gam to check this 
- if it's linear and we do non-linear we risk overadjustment 

- not sure i really understand about the wind speed 

- additional analyses:
- cvd + resp separately
- 75 + compared to <75, that's the median age
- effect modification by county-level DME
- sex-stratified analyses
- urgent + emergency, and all
- poverty 


This is the document where we write what we did, transcribe meeting notes, and 
write next steps. 

Marianthi says we should match on every two months. don't need to go more than 
that 

Oct 11th:
Notes on what is the primary analysis vs. sensitivity analyses.
- primary analysis is binary power outage exposure, Y/N, on all hospitalizations
and emergency hospitalizations, 8 hrs, cut point 3%
- sensitivity are all other cut points and durations 
- also do continuous hrs out analysis, where we model exposure non-linearly 
hopefully 
- not sure about percentile-based measurements at this point - right now all are
zero in the analytic dataset so need to fix that 
- going to try strata that are 4 months long to start 
- also going to add lags for 4 days after exposure 

Oct 10th: 
- meeting scheduled. 
- going to include hawaii and alaska after code review bc I don't want to run 
everything again - looks like we're missing AK for weather vars as well
- going to do percentile based estimates today.

- is there any way that we can use the emergency code to distinguish between
visits for hypertension that are emergencies vs everyone has hypertension?

- i think we can run a test analysis for some exposure, though maybe we want to 
look at outage prevalence first? analytic data is there

Oct 9th evening:
I finished doing missingness by hour and plotted power outage frequency in a 
bunch of different ways. 
Still need to:
- include hawaii and alaska
- estimate what proportion of customers we're actually covering with downscaled
county estimates and EIA data (done: answer: around 70%, 67.6% of EIA customers.)
- identify outages based on percentile
- bring things into FASSE 

Okay so for the missingness but interpolating over the hour, we'll need to 
change the order in which we summarize the county data. We'll ignore the 
missingness estimates that we've made so far. 

Oct 9th, 2024

Met with Joan regarding the data cleaning. Vivian will do code review and make 
sure that it's correct. Can ask Robbie to give critical feedback on power outage 
code, and maybe attend some team meetings?

Joan suggests doing missingness checks at the hourly level rather than at the 
10-minute level, and interpolating anything that is different between those two 
timescales. That could help us have less missing data. 

Also need to edit code to include HI and AK. Also maybe worth defining outages 
that are 99.9th percentile of customers out or higher. 
Probably want 99th percentile within county.

Could help with the issue of the denominator being blewn. 

I sent a when2meet for a team meeting. 

Still to do:
- modify code to be missingness at the hourly level
- plot customers out values that we have 
- bring data into FASSE and make analytic dataset. 
- identify outages by percentile rather than cut point



Oct 1st, 2024

Need to clean up and add power outage data processing code to this repository, 
and then continue to work on making an analytic dataset. 

Question for Joan: are there any papers looking at rates of Medicare insurance by county among those who are eligible? Thinking about who our denominator should be, and if it should be people over 65 or the benes files already on FASSE. 

Another question: is there a way to distinguish between emergency and non-emergency admissions? Robbie says there is and that it's key. ok turns out you can. we should definitely do this!! 
https://resdac.org/cms-data/variables/admission-type-code

some more documentation resources:
https://resdac.org/cms-data/variables/source-admission-inpatient-facility-newborn-admit-type-delivery-code
https://resdac.org/cms-data/variables/inpatient-admission-type-code

TODOS:
- also need to maybe delete some unnecessary files from FASSE, like the git.ignore.txt file 

Questions for Robbie:
- should I still be loading packages at the start of each script, or should I load them all somewhere else? what's the point of pacman? 
- how do i handle cleaning the environment while running the pipeline, since I'll have a lot of memory usage and I need to run gc() or something? 

Oct 4th: where we are now:

- it looks like the downscaled EIA customer estimates might be wrong
- finally got a working verson of b02_expand_to_hourly working, so that's good
- no idea if it's correct though 

- need to fix the EIA estimates and GET CODE REVIEW. 

