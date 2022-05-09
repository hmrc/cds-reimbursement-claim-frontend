# Technical Debt 

Add any tech debt you spot to the list below. Before eliminating the tech debt, make sure a JIRA ticket is created so 
that the work done can be prioritised and tracked.


| Issue # | Refactoring Description                                                                                                                                             | JIRA Ticket # |
|---------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------|
| 1       | Remove conf/resources/countries.txt                                                                                                                                 |               |
| 2       | Move views/schedule templates and views/supportevidence  templates into a new views/fileupload package to make  it consistent with the controller package structure |               |
| 3       | Check if views/claimant_details_timed_out.scala.html  can be removed                                                                                                |               |
| 4       | Remove FeatureService                                                                                                                                               |               |
| 5       | Rename claims package to reflect true nature of code  under that package                                                                                            |               |
| 6       | Need a mechanism which allows easy updating for the  enumerations which work forms which have list based  components e.g.checkboxes, radio buttons                  |               |
| 7       | Remove redundant message keys                                                                                                                                       |               |
| 8       | Remove redundant test data generators                                                                                                                               |               |
| 9       | Refactor redundant signed in user details model to eliminate unused data                                                                                            |               |
| 10      | Remove redundant user type model                                                                                                                                    |               |
| 11      | Remove unused imports and params from twirl files to reduce compiler warnings                                                                                       |               |