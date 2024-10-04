# power_outage_national_cvd_hosp progress notes 

This is the document where we write what we did, transcibe meeting notes, and 
write next steps. 

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