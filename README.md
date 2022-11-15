# Beahavioral-LD_task-analysis

This project presents analysis for behavioral data of Lexical Decision (LD) task in Hebrew.
In the LD task participants are exposed to a visual string of letters, one string at a time, appearing on a computer screen.
Participants are instructed to press one button if the letter string represents an existing word, and another if it does not.
Participant's Reaction times (RT's) are measured.

The project aim to test wheather reaction time to words differs from reaction time to nonwords. It is done using permutation and Bayesian hierarchical model 

LD_data-
  -Subject-the subject's number
  -Word- the Hebrew word or nonword
  -RT- Reaction time of the subject to that word/nonword
  -Word_type- 0 for word, 1 for nonword
There are 35 subjects, each one has 1000 datapoints.

Lexical_Decision_analysis- 
  sections:
    -Visualizations of raw data- has 2 visualizations demonstrating the difference of RT's between words and nonwords
    -Statistical analysis- permutation and Bayesian hierarchical model using JAGS. Diagnistics of the model are also presented.
    -Results visualization- visualizations of the permutation anf the Bayesian hierarchical model results.
    
