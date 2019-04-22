# DecisionSupportTool
ALW TRT Decision Support Tool


Fill out the ScenarioTemplate tab with the criteria for a model run and save it as a csv file in the InputSpreadsheets directory								
								
Each line in the sheet is an independent record, interpreted by the model function								
This file gets read and interpreted in the function at section 0.2								
								
## 1. Actions

In the Action column, select an option from the drop-down list								
Currently implemented: 
*	Closure
*	Constraint_Fishery
*	Constraint_Spatial
*	TrapReduction
							
## 2. Action input types

Fill out the rest of the record with the desired inputs. Note that not all columns are applicable all actions.

*	LMA	- Constrain action to Lobster Management Area						
*	State	- Constrain action to waters off given state						
*	StatArea - Constrain action to statistical area						
*	Fishery - Constrain model run to given fleet						
*	Shapefile - Constrain action to specified input shapefile. Should match name of shapefile in /TempShapefiles	*	Months - Text string of months to apply the action over.						
*	Percentage - Input percentages for action (i.e. trap reductions, etc.)						

## 3. Example inputs and interpretation.

### 3.1 Spatial constraints.

This specifies the spatial domain of the model run. Think of specifications within a line as "ands" and subsequent lines as "ors" so the following would be interpreted as "Constrain the model to (LMA1 and Maine and StatAreas 511 and 512 and the bounds of the 'Example_511_512_Border' shapefile) or (LMA3)". You can have as many records for spatial constraints as desired. The model will use the union of constraints as the domain for the model run.

| Action             | LMA | State | StatArea | Fishery | Shapefile              |
|--------------------|-----|-------|----------|---------|------------------------|
| Constraint_Spatial | A1  | ME    | 511, 512 |         | Example_511_512_Border |
| Constraint_Spatial | A3  |       |          |         |                        |

### 3.2 Fishery constraints. 

This specifies the spatial fleet of the model run. The following interpreted as "constrain the model run to include the NonExempt fishery". You can only specify one fishery constraint							
								
| Action             | Fishery   |
|--------------------|-----------|
| Constraint_Fishery | NonExempt |

### 3.3 Closures. 

This removes all fishing from a region, either seasonally or for the entire year. The following would be interpreted as close the area within 'TinyWedge_LMA1' for May - August. Like spatial constraints, the extent of closures can use the LMA, State, and StatArea columns as well with each field interpreted as an "and"

| Action  | LMA | State | StatArea | Shapefile      | Months  |
|---------|-----|-------|----------|----------------|---------|
| Closure |     |       |          | TinyWedge_LMA1 | 5,6,7,8 |
								
### 3.4 Trap Reductions.

This removes a percentage of traps from a spatial domain. *This cannot be specified seasonally.* The following would implement a 60% trap reduction in LMA3 and StatArea 515.
								
| Action        | LMA | State | StatArea | Shapefile | Percentage |
|---------------|-----|-------|----------|-----------|------------|
| TrapReduction | A3  |       | 515      |           | 0.6        |

Additional management actions coming soon!