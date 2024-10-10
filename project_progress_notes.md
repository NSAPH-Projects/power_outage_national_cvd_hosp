# power_outage_national_cvd_hosp progress notes 

This is the document where we write what we did, transcribe meeting notes, and 
write next steps. 

Oct 10th: 
- meeting scheduled. 
- going to include hawaii and alaska after code review bc I don't want to run 
everything again 
- going to do percentile based estimates today.

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

