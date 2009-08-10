August 7, 2009 (Draft 1)
August 9, 2009 (Draft 2)

This branch of PBSresilate contains experimental code by Sanae Rosen 
and me to investigate Newtonian gravity. Our code may eventually 
become part of the package, because a system of particles moving under 
the influence gravity seems to illustrate "lumping" behaviour in a 
complex system. The universe has stars, planetary sytems, star 
clusters, galaxies, and other hierarchies of matter.

Buzz Holling considers "lumping" a generic feature of complex systems. 
Is this an inevitable conseqence of Newton's laws? 

We're using preliminary code for the "two-body" problem to explore the 
feasibility of more complex code that would govern the movements of n 
particles. Ultimately, we want to look for the emergence of "lumping" 
among groups of particles that start with random distributions of 
positions and velocities.

Some technical issues have emerged so far.

1. We need to allow flexible (relative) scales for mass, distance, and 
time, as well as Newton's gravitational constant G.

2. We need to "fix" gravity so that forces don't become infinite as 
objects approach the same point in space. Our algorithms for solving 
differential equations can't handle infinite values.

3. Ideally, we should normalize intial values so the system has net 
momentum zero and a center of mass at the origin. This can be done 
while preserving relative positions and velocities in an arbitrary 
initial configuration.

Sanae's GUI related to PBSoptions could prove useful in Linux 
installations.

Jon Schnute
