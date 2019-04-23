# DecisionSupportTool
Atlantic Large Whale Take Reduction Team Decision Support Tool

## What is this tool used for?
The tool takes the following management measures and model how it would change risk to whales:

1.	Seasonal Closures, either with gear removed from the water or allowed to redistribute.
2.	Trap Reductions
3.	Regulations in Trawl Length
4.	Regulations in vertical line characteristics
5.	Implementation of ropeless or timed release technology.

## How to use the tool:
For users with [R](https://cran.r-project.org/) and the necessary packages installed on their computers, run the following code to initiate an [R shiny](https://shiny.rstudio.com/) interactive web application.

```
shiny::runGitHub("DecisionSupportTool", username = "NOAA-EDAB", ref = "master")

```

### Create scenarios and run the model:
Once you have the app up and running, scenarios can be created by 1) adding a unique and descriptive scenario name, 2) defining actions (see "Defining Scenarios" section below), and 3) running the model. ![Figure 1. Steps to run a new scenario.](demo_figures/tool_demo-newscenario.png)

You can also click the drop-down to select a scenario that has already been analyzed. If you just want to modify an existing scenario, select the scenario you want to update, give it a new name, and click the "run model" button. If you aren't certain of the spatial extent of the different areas (e.g., LMA, StatArea, State waters, etc), please click the "Visualize Areas" tab to explore the different spatial domains used in the model.
![Figure 2. Spatial extent viewing tool showing the labeled "StatArea" available to the model.](demo_figures/tool_demo-shpviewer.PNG)


### Evaluate model output:
Once the model finishes running, there are two ways to evaluate the output: spatial heatmaps and risk tables.

![Figure 3. Risk tables for a scenario.](demo_figures/tool_demo-risk.PNG)

![Figure 4. Heatmap of risk for a scenario. The images can be magnified by right-click dragging a polygon of interest followed by a right double-click. To zoom back out just right double-click again.](demo_figures/tool_demo-riskmap.PNG)




# Defining Scenarios								
Each "action" (or row in the spreadsheet" is an independent record that is interpreted by the model function. *Note: actions are added to the model sequentially (e.g., row 1 is followed by row 2, etc) and the order of the rows may influence the outcome of the model.*

## Actions

In the Action column, select an option from the drop-down list								
Currently implemented: 
*	Closure
*	Constraint_Fishery
*	Constraint_Spatial
*	TrapReduction
							
## Action input types

Fill out the rest of the record with the desired inputs. Note that not all columns are applicable all actions.

*	LMA	- Constrain action to Lobster Management Area						
*	State	- Constrain action to waters off given state						
*	StatArea - Constrain action to statistical area						
*	Fishery - Constrain model run to given fleet						
*	Shapefile - Constrain action to specified input shapefile. Should match name of shapefile in /TempShapefiles	*	Months - Text string of months to apply the action over.						
*	Percentage - Input percentages for action (i.e. trap reductions, etc.)						

## Example inputs and interpretation.

### Spatial constraints.

This specifies the spatial domain of the model run. Think of specifications within a line as "ands" and subsequent lines as "ors" so the following would be interpreted as "Constrain the model to (LMA1 and Maine and StatAreas 511 and 512 and the bounds of the 'Example_511_512_Border' shapefile) or (LMA3)". You can have as many records for spatial constraints as desired. The model will use the union of constraints as the domain for the model run.

| Action             | LMA | State | StatArea | Fishery | Shapefile              |
|--------------------|-----|-------|----------|---------|------------------------|
| Constraint_Spatial | A1  | ME    | 511, 512 |         | Example_511_512_Border |
| Constraint_Spatial | A3  |       |          |         |                        |

### Fishery constraints. 

This specifies the spatial fleet of the model run. The following interpreted as "constrain the model run to include the NonExempt fishery". You can only specify one fishery constraint							
								
| Action             | Fishery   |
|--------------------|-----------|
| Constraint_Fishery | NonExempt |

### Closures. 

This removes all fishing from a region, either seasonally or for the entire year. The following would be interpreted as close the area within 'TinyWedge_LMA1' for May - August. Like spatial constraints, the extent of closures can use the LMA, State, and StatArea columns as well with each field interpreted as an "and"

| Action  | LMA | State | StatArea | Shapefile      | Months  |
|---------|-----|-------|----------|----------------|---------|
| Closure |     |       |          | TinyWedge_LMA1 | 5,6,7,8 |
								
### Trap Reductions.

This removes a percentage of traps from a spatial domain. *This cannot be specified seasonally.* The following would implement a 60% trap reduction in LMA3 and StatArea 515.
								
| Action        | LMA | State | StatArea | Shapefile | Percentage |
|---------------|-----|-------|----------|-----------|------------|
| TrapReduction | A3  |       | 515      |           | 0.6        |


### Overview of all options.

All settings that are currently available can be seen below.

| Action             | LMAs        | States | Fishery          | StatArea | TrawlRegulation | MaxRopeDia | BuoylineDevice     | RopelessDevice  |
|--------------------|-------------|--------|------------------|----------|-----------------|------------|--------------------|-----------------|
| Closure            | All         | All    | All              | 464      | Min             | 1,700      | 1,700@100m         | TimedRelease    |
| Constraint_Fishery | A1          | ME     | NonExempt        | 465      | Max             | 3/8in      | SSS_Regular        | AcousticRelease |
| Constraint_Spatial | A2          | NH     | Exempt           | 511      | Exactly         | 7/16in     | SSS_To500m         |                 |
| MaxRopeDia         | A2_3overlap | MA     | Midshelf_Lobster | 512      |                 | 1/2in      | TimedTensionCutter |                 |
| TrawlLength        | A3          |        | Midshelf_Crab    | 513      |                 | 9/16in     |                    |                 |
| TrapCap            |             |        | Offshore_Lobster | 514      |                 | 5/8in      |                    |                 |
| TrapReduction      |             |        | Offshore_Crab    | 515      |                 |            |                    |                 |
| BuoylineDevice     |             |        |                  | 521      |                 |            |                    |                 |
| RopelessDevice     |             |        |                  | 522      |                 |            |                    |                 |
|                    |             |        |                  | 561      |                 |            |                    |                 |
|                    |             |        |                  | 562      |                 |            |                    |                 |
|                    |             |        |                  | 537      |                 |            |                    |                 |
|                    |             |        |                  | 538      |                 |            |                    |                 |
|                    |             |        |                  | 539      |                 |            |                    |                 |
Additional management actions coming soon!


## More background information on the tool:

First, because both trap reductions and how traps are aggregated into trawls, the model needs to start at the currency of traps, then sequentially transition to trawls, vertical lines, line diameters, lethality, and then risk based on whale co-occurrence.

Second, it seems appropriate to model these processes at a scale of a 1Nm grid, as this is the finest scale that the IEc model employs, allows splitting stat areas along LMA lines, and allows some added precision to modeling the boundaries of closed areas. Much of the other model inputs do not have this resolution so we’ll have to accept simple down-scaling of these processes.

Tool layout:
1.	Start with traps per grid by month. This is calculated from the Area 3 Vertical Line model as a function of the number of vertical lines and trawl lengths. For Area 3, this need to be modeled separately for the crab and lobster fishery with two different classes of lobster vessels.
2.	Traps are removed or redistributed based on the locations and timing of seasonal closures (management option #1 above). We would need to consider a rough approach to how to model redistributing traps. We can start with a basic set of rules for now.
3.	Traps are further removed due to trap reductions (management option #2). Easiest assumption is that traps will be removed proportionally over the entire management area.
4.	Traps are converted to distribution of trawls based on existing empirical data or models of traps / trawl at the scale of stat areas and proposed management (#3 above).
5.	Trawls are converted to vertical lines based on trawl length. Expected to be two vertical lines per trawl for all offshore areas but may be different for inshore areas with shorter trawls.
6.	Number of vertical lines are further adjusted for any regulations on ropeless or timed-release fishing (#5 above). Given the technology available to the lobster fishery and other fisheries, it is improbable that these measures can be implemented immediately but it is appropriate to include this now in the model framework.
7.	A distribution of line diameters for vertical lines is characterized based on observed relationships with trawl length and further modified based on management options (#4 above).
8.	Line diameters are converted to lethality based on a model to be developed, possibly by polling the TRT.
9.	Lethality for the grid cell is calculated as the product of line lethality and number of vertical lines.
10.	Risk is calculated as the product of lethality and whale presence.
11.	Grids are then aggregated to appropriate scales to get risk scores by 10min cells, stat areas, LMA’s, etc.
12.	Aggregate risk scores are then be compared to a baseline risk value to determine risk reduction.


*This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.*