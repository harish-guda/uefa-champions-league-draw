# Simulating UEFA Champions League

Here's a simple code snippet that I use to simulate the UEFA Champions League's Round of 16. 

The round of 16 is a special round due to a number of constraints on the possible matches:
1. No two teams from the same country can be matched. 
2. No two teams from the same group (from round of 32) can be matched. 

This results in a number of infeasible matches. The following code generates 100,000 possible outcomes[^1]. This can be used to predict which outcomes are more likely. 

[^1]: An outcome refers to an outcome of the assignment process, i.e., a feasible set of matches for all teams. 
