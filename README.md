# Grade
|Marking Criteria|Weight|Mark out of 10| Mark (%)|
|----------------|------|--------------|---------|
|FORTRAN         |  40% |       9      |    36   |
|Control & Ease of Use|10%|     8      |     8   |
|Results         |  20% |      10      |    20   |
|Report          |  30% |       8      |    24   |
|----------------|------|--------------|---------|
|Total           |      |              |    88   |

# Comments from Marker
(David Forest, University of Birmingham Physics and Astronomy)

Compiled without errors, only a couple of warnings about unused variables, ‘InputCorrect’and ‘len’.

The FORTRAN itself is very good, with good modularity. Several of the SUBROUTINES could actually be FUNCTIONS, as there is only one output argument, but this is minor.

The code is well commented, with good error trapping.

It would be nice if the user were asked if they wanted to use the default output filename, or specify their own. Also, some mechanism allowing the various parameters such as fuel dimensions, mass flow rates etc. to be changed without recompiling – maybe via parameter files or user input – would have been a useful extension. Nevertheless, a very good code that does the job nicely.

The results are excellent, and agree with mine to a good level of precision. The report gives very good information on the structure and function of your code. The methodology is explained clearly, and the details of potential error sources described.

However, it would have been useful to present the results. Graphs showing the temperature distributions would have shown them well, while the actual values could have been given in a table in an appendix.

# Analytics
|		   |lines of code	|Comment	|total	|
|------------------|------------|-------|-------|
|T_profile.f	   |244		|48	|321	|
|FortranReport.tex |275		|12	|336	|

|		   |words	|char |
|------------------|------------|-----|
|FortranReport.tex |2985,	|18196|
|FortranReport.pdf |2283,	|13243|
