DecisionTool=function(
  HomeDir,
  InputSpreadsheetName,
  HighResolution=FALSE, ## run moel at 10Nm (TRUE) or 1Nm (FALSE); lower resolution runs faster
  PrintTables=TRUE,
  PrintDefaultMaps=TRUE,
  PrintScenarioMaps=TRUE,
  PrintRedistributionMaps=TRUE, ## maps of traps that were moved or removed as a result of a closure
  WriteOutputCsv=TRUE,
  WriteMapSources=FALSE
) {
  
  # HD="/net/work4/LobsterGroup/Management/RightWhales/DecisionSupportTool"
  # InputSpreadsheetName="GearScenarioTests.csv"
  # InputSpreadsheetName="NEAq_TrapReduction50_WeakRopeEverywhere_TagOffshore.csv"
  
  print(paste("Running ", InputSpreadsheetName))
  
  # HomeDir=HD; HighResolution=FALSE; PrintTables=TRUE; PrintDefaultMaps=TRUE
  # PrintScenarioMaps=TRUE; PrintRedistributionMaps=TRUE; 
  # WriteMapSources=TRUE;   WriteOutputCsv=TRUE
  
  ## V1.2 added option to run in high-resolution (1Nm) rather than low-resolution (10Nm)
  ## This is only useful if one wants to apply management decisions at finer scales
  ## or expects that there is fine-scale co-occurrence patterns between whales and gear
  ## otherwise, this significantly increases model run time.
  
  ## V1.3 added redistribution of traps around closures. Should have further revision to allow
  ## simultaneous assessment of multiple closures by building the Unmoved / Unaffected traps monthly
  
  ## V1.4 Implementation of min / max trawl lengths
  ## SC_MaxRopeDia
  ## SC_TrawlLength
  ## SC_BuoylineDevice
  ## SC_RopelessDevice
  ## Updated to handle High Resolution
  ## Writing produced maps to .Rdata
  
  
  library(grid)
  library(gtable)
  library(gridExtra)
  library(maptools)
  library(rgdal)
  
  # HD=("/net/work4/LobsterGroup/Management/RightWhales/DecisionSupportTool");
  # HD=HomeDir; setwd(HD)
  OutputDir=gsub(".csv", "", InputSpreadsheetName); OutputDir
  
  spRef_UTM_19="+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  spRef_DD="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
  
  ####################### Misc Functions ############################--
  source(paste(HD, "/FunctionsEtc/Long2Wide.R", sep=""))
  source(paste(HD, "/FunctionsEtc/Wide2Long.R", sep=""))
  source(paste(HD, "/FunctionsEtc/functionPrintTable.R", sep=""))
  
  
  Fold=TRUE ## dummy variable to allow text folding
  
  ##############################################################--
  ## 0.0 Scenario Inputs
  if(Fold) {
    ## read in input spreadsheet
    ScenarioInputs=read.csv(
      paste("InputSpreadsheets", ## subdirectory
            InputSpreadsheetName, ## file name
            sep="/"), stringsAsFactors=FALSE, na.strings=""); 
    ScenarioInputs=ScenarioInputs[!is.na(ScenarioInputs$Action), ]
    ScenarioInputs$StatArea=as.character(ScenarioInputs$StatArea)
    ScenarioInputs$Months=as.character(ScenarioInputs$Months)
    
    ScenarioInputs
    
    # ScenarioInputs$LMA[ScenarioInputs$LMA==""]=NA
    # ScenarioInputs$State[ScenarioInputs$State==""]=NA
    # ScenarioInputs$StatArea[ScenarioInputs$StatArea==""]=NA
    # ScenarioInputs$Fishery[ScenarioInputs$Fishery==""]=NA
    # ScenarioInputs$Shapefile[ScenarioInputs$Shapefile==""]=NA
    # ScenarioInputs$Months[ScenarioInputs$Months==""]=NA
    # 
    Constraints_Spatial=ScenarioInputs[ScenarioInputs$Action=="Constraint_Spatial", #&
                                       # !is.na(ScenarioInputs$Action),
                                       c("Action", "LMA", "State", "StatArea", "Shapefile")]; Constraints_Spatial
    
    Constraints_Fishery=ScenarioInputs[ScenarioInputs$Action=="Constraint_Fishery", #&
                                       # !is.na(ScenarioInputs$Action),
                                       c("Action", "Fishery")]; Constraints_Fishery
    Closures=ScenarioInputs[
      ScenarioInputs$Action=="Closure", #&
      # !is.na(ScenarioInputs$Action), 
      c("Action", "Shapefile", "Months", "Percentage")]; Closures
    
    TrapReductions=ScenarioInputs[
      ScenarioInputs$Action=="TrapReduction", #&
      # !is.na(ScenarioInputs$Action),
      c("Action", "LMA", "State", "StatArea", "Shapefile", "Months","Percentage")]; TrapReductions
    
    SC_MaxRopeDia=ScenarioInputs[
      ScenarioInputs$Action=="MaxRopeDia",
      c("Action", "LMA", "State", "StatArea", "Shapefile", "Months", "MaxRopeDia")]; SC_MaxRopeDia
    
    SC_TrawlLength=ScenarioInputs[
      ScenarioInputs$Action=="TrawlLength",
      c("Action", "LMA", "State", "StatArea", "Shapefile", "Months", "TrawlRegulation", "TrawlLen")]; SC_TrawlLength
    
    SC_BuoylineDevice=ScenarioInputs[
      ScenarioInputs$Action=="BuoylineDevice",
      c("Action", "LMA", "State", "StatArea", "Shapefile", "Months", "BuoylineDevice")]; SC_BuoylineDevice
    
    SC_RopelessDevice=ScenarioInputs[
      ScenarioInputs$Action=="RopelessDevice",
      c("Action", "LMA", "State", "StatArea", "Shapefile", "Months", "RopelessDevice")]; SC_RopelessDevice
    
  }  ## Scenario.csv
  
  if(Fold) { ## load GIS layers and bathymetry
    ShapefileDir=paste(HD, "/InputShapefiles", sep="")
    print("Loading Shapefiles")
    spStatAreas=readOGR(dsn=ShapefileDir, 
                        layer="StatAreas_DecisionTool",
                        verbose=FALSE)
    
    # plot(spStatAreas)
    # text(
    #   getSpPPolygonsLabptSlots(spStatAreas),
    #   labels=as.character(spStatAreas$Id),
    #   cex=0.4)
    
    spLMAs=readOGR(dsn=ShapefileDir, 
                   layer="LMA_IEC_Union",
                   verbose=FALSE)
    summary(spLMAs)
    spLMAs=spLMAs[spLMAs$Name %in% c("A1", "A2", "OCC", "A2_3overlap", "A3"), ]
    proj4string(spLMAs)=CRS(spRef_DD)
    # plot(spLMAs)
    
    # par(mar=c(1,1,1,1))
    # plot(spStatAreas)
    # text(
    #   getSpPPolygonsLabptSlots(spStatAreas),
    #   labels=as.character(spStatAreas$Id),
    #   cex=1)
    # plot(spLMAs, border="blue", add=TRUE, lwd=3)
    
    ############### Closure in effect [April - June]
    # GSCRA=readOGR(dsn="//net/work4/LobsterGroup/Shapefiles", layer="GreatSouthChannelRestrictedArea"); plot(GSCRA); summary(GSCRA)
    # plot(GSCRA, add=T)
    
    Coast=readOGR(dsn=ShapefileDir, layer="EastCoastLines",
                  verbose=FALSE); #plot(Coast); 
    # Iso100=readOGR(dsn=ShapefileDir, layer="100f_Isobath",
    #                verbose=FALSE); #plot(Iso100); summary(Iso100)
    
    ## objects for spatial plotting
    spCoast_layout1=list("sp.lines", as(Coast, "SpatialLinesDataFrame"), lwd=3, col="white")
    spCoast_layout2=list("sp.lines", as(Coast, "SpatialLinesDataFrame"), lwd=1, col="black")
    
    spLMA_layout1=list("sp.lines", as(spLMAs, "SpatialLinesDataFrame"), lwd=3, col="white")
    spLMA_layout2=list("sp.lines", as(spLMAs, "SpatialLinesDataFrame"), lwd=1, col="black")
    
    spStatAreas_layout1=list("sp.lines", as(spStatAreas, "SpatialLinesDataFrame"), lwd=3, col="white")
    spStatAreas_layout2=list("sp.lines", as(spStatAreas, "SpatialLinesDataFrame"), lwd=1, col="black")
  } ## load GIS layers and bathymetry 
  
  ## 0.1 Load data #############################################--
  if(Fold) { 
    print("Loading Data")
    ## 0.1 Start with traps per grid by month. 
    ## This is calculated from the Area 3 Vertical Line model as a function of the number of vertical lines and trawl lengths. 
    ## For Area 3, this need to be modeled separately for the crab and lobster fishery with two different classes of lobster vessels.
    
    ## StandarPx for aggregate mapping
    # #### build grid and constrain to domain of trap map
    # Grid=merge(data.frame(Longitude=seq(from=-75.75, to=-64.75, by=1/6)),
    #            data.frame(Latitude=seq(from=34.75, to=45.25, by=1/6)))
    # Grid$GridID=1:nrow(Grid);
    # ## convert to spatial Pixels data frame
    # coordinates(Grid)=c("Longitude", "Latitude"); proj4string(Grid)=proj4string(spLMAs)
    # plot(Grid)
    # GridPx=as(Grid, "SpatialPixelsDataFrame")
    # Pts=unique(TrapMap[ ,c("IecIndex_1", "x", "y")]); ## get trap map coordinates to define domain
    # coordinates(Pts)=c("x", "y"); proj4string(Pts)=CRS(spRef_DD)
    # Pts$GridID=over(Pts, GridPx)$GridID; summary(Pts)
    # StandardPx=GridPx[GridPx$GridID %in% Pts$GridID, c("GridID")]
    # summary(StandardPx);
    # plot(StandardPx)
    # save(StandardPx, file="StandardPx.Rdata")
    load(paste(HD, "/Inputs/StandardPx.Rdata", sep=""))
    ### summary(StandardPx)
    
    load(paste(HD, "/Inputs/MapRef.Rdata", sep=""))
    ###summary(MapRef)
    
    ## Trap Map ##################################################--
    load(paste(HD, "/Inputs/TrapMap_V0.1.Rdata", sep="")); 
    ### summary(TrapMap)
    ## head(TrapMap)
    DistanceDF=unique(TrapMap[ ,c("IecIndex_1", "Distance")]); dim(DistanceDF)
    Tmp=data.frame(Distance=unique(DistanceDF$Distance)); Tmp
    Tmp$Val=1:nrow(Tmp)
    DistanceDF=merge(DistanceDF, Tmp); dim(DistanceDF)
    DistanceDF=aggregate(Val~IecIndex_1, DistanceDF, min); dim(DistanceDF)
    MapRef=merge(MapRef, DistanceDF, all.x=TRUE); summary(MapRef)
    MapRef=merge(MapRef, Tmp); summary(MapRef)
    # plot(MapRef)
    # points(MapRef[is.na(MapRef$Distance), ], col="green")
    
    
    ## Zone Adjacency for spatial redistribution of traps ################--
    ZoneAdjacency=read.csv(paste(HD, "/Inputs/ZoneAdjacency_2.csv", sep=""), stringsAsFactors = FALSE); 
    
    BuoylineThreat=read.csv(paste(HD, "/Inputs/BuoylineRisk.csv", sep=""), stringsAsFactors = FALSE); 
    names(BuoylineThreat)=c("Rope", "RopeDiam", "TrawlLen", "BuoylineDevice", "Threat");
    # summary(BuoylineThreat) 
    LineConversion=unique(BuoylineThreat[ ,c("Rope", "RopeDiam")]); LineConversion
    
    RopelessRisk=read.csv(paste(HD, "/Inputs/RopelessRisk.csv", sep=""), stringsAsFactors = FALSE); 
    # RopelessRisk=RopelessRisk[ ,c("RopelessDevice", "Distance", "LineMultiplier")]
    
    TrapConversion=read.csv(paste(HD, "/Inputs/TrapConversion.csv", sep=""), stringsAsFactors=FALSE);
    # summary(TrapConversion)
    
    
    
    ## TrawlLengthModel #########################################--
    load(paste(HD, "/Inputs/TrawlLengthModel.Rdata", sep="")); 
    # names(TrawlLengthModel)=c("Region", "VesselClass", "TrapsPerTrawl", "TrawlProportion", "Month")
    # ### summary(TrawlLengthModel)
    # TrawlLengthModel$IndProd=with(TrawlLengthModel, TrapsPerTrawl * TrawlProportion); summary(TrawlLengthModel)
    # Tmp=aggregate(IndProd~Region+VesselClass+Month, TrawlLengthModel, sum); 
    # names(Tmp)=c("Region", "VesselClass", "Month", "TrawlUnitCost"); summary(Tmp)
    # TrawlLengthModel=merge(TrawlLengthModel, Tmp); summary(TrawlLengthModel)
    # TrawlLengthModel=TrawlLengthModel[ ,c("Region", "VesselClass", "Month", "TrapsPerTrawl", "TrawlProportion", "TrawlUnitCost")]
    ### summary(TrawlLengthModel)
    
    ## RopeDiameterModel
    load(paste(HD, "/Inputs/LineDiameterModel.Rdata", sep="")); 
    ### summary(LineMod); 
    names(LineMod)=c("TrapsPerTrawlInt", "RopeDiam", "Prop_Line")
    
    ## Endlines per trawl model
    load(paste(HD, "/Inputs/EndlinesPerTrawlModel.Rdata", sep=""))
    # summary(EndlinesPerTrawl)
    
    ## Threat Model
    # load(paste(HD, "/Inputs/GearThreatModel.Rdata", sep="")); 
    ### summary(ThreatMod)
    
    ## Whale Habitat Model
    load(paste(HD, "/Inputs/DukeWhaleModel_v8.Rdata", sep=""));
    # load(paste(HD, "/Inputs/DukeWhaleModel_v9.Rdata", sep="")); 
    ### summary(WhalesAt1Nm)
    
    # sub=WhalesAt1Nm[WhalesAt1Nm$Month==4, ]
    # coordinates(sub)=c("x", "y");
    # spplot(sub["Density10Nm"])
    
    MonthString=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    MonthOrder=c("Sep", "Oct", "Nov", "Dec",
                 "May", "Jun", "Jul", "Aug", 
                 "Jan", "Feb", "Mar", "Apr")
    
    MonthDF=data.frame(
      Month=1:12,
      MonthName=MonthString
    )
  } ## Load model inputs and submodels
  
  OutputData=data.frame(
    Variable=character(0),
    Scenario=character(0),
    Month=character(0),
    Value=numeric(0)
  ); OutputData
  
  #############################################################--
  ## 0.2 Constrain spatial extent based on user inputs
  if(Fold) { ## fold spatial constraint 
    if(nrow(Constraints_Spatial)>0){
      
      for(i in 1:nrow(Constraints_Spatial)){
        ## constrain spatially
        MapRef_CrI=MapRef;
        if(!is.na(Constraints_Spatial$LMA[i])) {
          MapRef_CrI=MapRef_CrI[MapRef_CrI$LMA==Constraints_Spatial$LMA[i], ]
        } 
        if(!is.na(Constraints_Spatial$State[i])) {
          MapRef_CrI=MapRef_CrI[MapRef_CrI$State==Constraints_Spatial$State[i], ]
        } 
        if(!is.na(Constraints_Spatial$StatArea[i])) {
          StatAreasI=as.numeric(strsplit(Constraints_Spatial$StatArea[i], ",")[[1]])
          MapRef_CrI=MapRef_CrI[MapRef_CrI$StatArea %in% StatAreasI, ]
        }
        if(!is.na(Constraints_Spatial$Shapefile[i])) {
          Constraints_SpatialShape=Constraints_Spatial$Shapefile[i]; Constraints_SpatialShape ## name of shapefile
          ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=Constraints_SpatialShape, verbose=FALSE) ## load shapefile
          ShapeI$ID=1 ## create a known field 
          ShapeI=spTransform(ShapeI, proj4string(MapRef_CrI))
          MapRef_CrI = MapRef_CrI[!is.na(over(MapRef_CrI, ShapeI)$ID), ] ## get spatial overlap of shapefile from overlay
        }
        
        if(exists("MapRef_Cr")){
          MapRef_Cr=rbind(MapRef_Cr, MapRef_CrI)
        } else {     MapRef_Cr=MapRef_CrI }
      }
      # plot(MapRef_Cr)
      TrapMap=TrapMap[TrapMap$IecIndex_1 %in% unique(MapRef_Cr$IecIndex_1), ]
      WhalesAt1Nm=WhalesAt1Nm[WhalesAt1Nm$IecIndex_1 %in% unique(MapRef_Cr$IecIndex_1), ]
      # summary(WhalesAt1Nm)
      if(nrow(TrapMap)==0){ print("Error: Spatial Constraints removed all data"); break()}
    } ##
  } ## Spatial Constraints
  
  if(Fold) {
    if(nrow(Constraints_Fishery)>0) {
      Constraints_Fishery
      TrapMap=TrapMap[TrapMap$Fishery %in% Constraints_Fishery$Fishery, ]; dim(TrapMap) ## drop all data outside constraints
      WhalesAt1Nm=WhalesAt1Nm[WhalesAt1Nm$IecIndex_1 %in% TrapMap$IecIndex_1, ] ## filter whale data
    }
  } ## Fishery constraints
  
  ##########################################################--
  ## 1.0 Closures
  if(Fold) {
    print("1. Applying any closures");
    ## Traps are removed or redistributed based on the locations and timing of seasonal closures 
    ## (management option #1 above). 
    ## We would need to consider a rough approach to how to model redistributing traps. 
    ## We can start with a basic set of rules for now.
    Stage1d=TrapMap
    Stage1s=TrapMap
    
    if(nrow(Closures)>0){
      
      RemovedTraps=Stage1s[0, ];
      RedistributedTraps=RemovedTraps;
      # UnmovedTraps=Stage1s
      
      ## Define set of traps that will NOT be moved #######################################################--
      for(i in 1:nrow(Closures)){
        print(paste("Overlaying Closure", i))
        
        ## define the area affected by the closure
        ClosureShape=Closures$Shapefile[i]; ClosureShape ## name of shapefile
        print(ClosureShape)
        ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=ClosureShape, verbose=FALSE) ## load shapefile
        ShapeI$ID=1 ## create a known field 
        ShapeI=spTransform(ShapeI, proj4string(MapRef))
        ClosedPx=MapRef$IecIndex_1[!is.na(over(MapRef, ShapeI)$ID)] ## get spatial overlap of shapefile from overlay
        
        ## define the months applicable to the closure
        if(!is.na(Closures$Months[i])){
          ClosureMonthsI=as.numeric(strsplit(Closures$Months[i], ",")[[1]]); ClosureMonthsI ## get months
        } else {ClosureMonthsI=1:12}; ## or use all months
        
        # if(is.na(Closures$Percentage[i])) { Multiplier=0} else { Multiplier=Closures$Percentage}; Multiplier
        
        ## set of traps outside the closure but from the same months
        UnmovedTraps=Stage1s[
          !Stage1s$IecIndex_1 %in% ClosedPx &
            Stage1s$Month %in% ClosureMonthsI, ]; ## data frame of remaining traps adjacent to closure
        names(UnmovedTraps)[names(UnmovedTraps)=="SourceZone"]="SinkZone"
        summary(UnmovedTraps)
        
        UnaffectedTraps=Stage1s[!Stage1s$Month %in% ClosureMonthsI, ]; dim(UnaffectedTraps)
        
        
        # sub=unique(UnmovedTraps[ ,c("x", "y", "StatArea")]); coordinates(sub)=c("x", "y")
        # points(sub, pch=2)
        
        # ## remove any specified traps
        # Stage1s$TrapsFished[Stage1s$IecIndex_1 %in% ClosedPx &
        #                       Stage1s$Month %in% ClosureMonthsI]=
        #   Stage1s$TrapsFished[Stage1s$IecIndex_1 %in% ClosedPx &
        #                         Stage1s$Month %in% ClosureMonthsI] * Multiplier
      }
      
      ## For each Fishery / Distance / SourceZone, redistribute traps #######################################--
      for(i in 1:nrow(Closures)){
        
        ## define the area affected by the closure
        ClosureShape=Closures$Shapefile[i]; ClosureShape ## name of shapefile
        ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=ClosureShape, verbose=FALSE) ## load shapefile
        ShapeI$ID=1 ## create a known field 
        ShapeI=spTransform(ShapeI, proj4string(MapRef))
        ClosedPx=MapRef[!is.na(over(MapRef, ShapeI)$ID), ] ## get spatial overlap of shapefile from overlay
        summary(ClosedPx)
        
        ## get months that closure is active
        if(!is.na(Closures$Months[i])){
          ClosureMonthsI=as.numeric(strsplit(Closures$Months[i], ",")[[1]]); ClosureMonthsI ## get months
        } else {ClosureMonthsI=1:12}; ## or use all months
        
        
        AffectedPx=Stage1s[Stage1s$IecIndex_1 %in% ClosedPx$IecIndex_1 &
                             Stage1s$Month %in% ClosureMonthsI, ]; summary(AffectedPx)
        TrapMonthSetsIndices = unique(AffectedPx[ ,c("Fishery", "Distance", "SourceZone", "Month")])
        TrapSetsIndices = unique(TrapMonthSetsIndices[ ,c("Fishery", "Distance", "SourceZone")])
        
        ## for each combination of fishery, distance, and zone
        for(j in 1:nrow(TrapSetsIndices)){
          print(paste("Relocating traps for Closure", ClosureShape, "Area", j, "of", nrow(TrapSetsIndices), sep=" "))
          
          ## relevant traps to move
          TrapSetsIndexJ=TrapSetsIndices[j, ] ## start with fishery, distance, zone
          TrapMonthSetsIndicesJ=merge(TrapMonthSetsIndices, TrapSetsIndexJ); ## add in applicable months
          
          TrapSetJ=merge(AffectedPx, TrapSetsIndexJ); summary(TrapSetJ)
          
          ## check if source destination is consistent
          if(
            length(unique(TrapSetJ$Month))>1 & 
                           sd(table(unique(TrapSetJ[ ,c("Month", "IecIndex_1")])$Month))>0){
            print("Error; Trap Redistribution Source Domain Inconsistent. Terminating Scenario Run"); break()
          }
          
          sp=unique(TrapSetJ[ ,c("x", "y", "IecIndex_1")]); coordinates(sp)=c("x", "y");
          # plot(ShapeI)
          # points(sp)
          
          ## Adjacent areas where traps can be moved to
          AdjacentSetIndexJ=merge(TrapMonthSetsIndicesJ, ZoneAdjacency); AdjacentSetIndexJ
          AdjacentSetIndexJ=AdjacentSetIndexJ[ ,c("SinkZone", "Fishery", "Distance", "Month")]
          AdjacentSetJ=merge(UnmovedTraps, AdjacentSetIndexJ); summary(AdjacentSetJ)
          head(AdjacentSetJ)
          
          ## check if sink domain is consistent
          if(
            length(unique(AdjacentSetJ$Month))>1 & 
            sd(table(unique(AdjacentSetJ[ ,c("Month", "IecIndex_1")])$Month))>0){
            print("Error; Trap Redistribution Sink Domain Inconsistent. Terminating Scenario Run"); break()
          }
          
          ############# Redistribute traps to adjacent areas
          if(nrow(AdjacentSetJ)==0){
            RemovedTraps=rbind(RemovedTraps, TrapSetJ) ## if not adjacent area, remove traps
          } else {
            
            ## for interior distance (traps inside polygon to boundary) use a subset to speed up calculations
            TrapsToMove=aggregate(TrapsFished~Month, TrapSetJ, sum); 
            names(TrapsToMove)=c("Month", "TrapsToMove"); TrapsToMove
            
            ## condensing to unique locations removed redundancy from multiple model vessels
            spTrapSetJ=unique(TrapSetJ[ ,c("x", "y","IecIndex_1")]); coordinates(spTrapSetJ)=c("x", "y"); proj4string(spTrapSetJ)=proj4string(ShapeI)
            NumPts=min(1000, nrow(spTrapSetJ)); NumPts;
            subspTrapSetJ=spTrapSetJ[sample(1:nrow(spTrapSetJ), NumPts), ];
            InteriorDist=mean(data.frame(dist2Line(subspTrapSetJ, ShapeI))$distance)/1000; InteriorDist
            
            spAdjacentSetJ=unique(AdjacentSetJ[ c("x", "y", "IecIndex_1")]); coordinates(spAdjacentSetJ)=c("x", "y"); 
            proj4string(spAdjacentSetJ)=proj4string(ShapeI)
            
            ## calculate distances from adjacent cell to closure boundary for each location
            ExteriorDist=data.frame(dist2Line(spAdjacentSetJ, ShapeI))
            
            ## combine exterior and inter distances
            spAdjacentSetJ$DistanceToMove=ExteriorDist$distance/1000+InteriorDist ## approximate distance to move traps
            summary(spAdjacentSetJ)
            
            ## merge back into broader adjacent data set
            AdjacentSetJ_2=merge(AdjacentSetJ, spAdjacentSetJ); summary(AdjacentSetJ_2)
            AdjacentSetJ_2$MoveCost=1/AdjacentSetJ_2$DistanceToMove ## distance-based cost of moving traps
            
            # plot(MoveCost~DistanceToMove, AdjacentSetJ_2)
            
            AdjacentSetJ_2$TrapWt=AdjacentSetJ_2$MoveCost * AdjacentSetJ_2$TrapsFished ## weighting based on cost and trap density 
            
            ## get monthly weights for standardizing
            TrapWtByMonth=aggregate(TrapWt~Month, AdjacentSetJ_2, sum); names(TrapWtByMonth)=c("Month", "TotalTrapWt");
            AdjacentSetJ_2=merge(AdjacentSetJ_2, TrapWtByMonth); summary(AdjacentSetJ_2)
            ## get proportion of moved traps that should be applied to a given location / model vessel
            AdjacentSetJ_2$TrapProp=AdjacentSetJ_2$TrapWt / AdjacentSetJ_2$TotalTrapWt 
            
            ## bring in number of traps to move
            AdjacentSetJ_2=merge(AdjacentSetJ_2, TrapsToMove); summary(AdjacentSetJ_2);
            AdjacentSetJ_2$NewTraps=AdjacentSetJ_2$TrapProp * AdjacentSetJ_2$TrapsToMove; ## Traps to move times adjusted weights
            summary(AdjacentSetJ_2)
            
            aggregate(NewTraps~Month, AdjacentSetJ_2, sum)
            
            
            Tmp=AdjacentSetJ_2[ ,c("IecIndex_1","Region","Month","x","y","LMA","StatArea","Open","State","Fishery","Distance",
                                   "VesselClass","SinkZone","NewTraps","GridID")]
            names(Tmp)=names(RedistributedTraps); head(Tmp)
            
            RedistributedTraps=rbind(RedistributedTraps, Tmp); summary(RedistributedTraps)
            
            
          } ## end if there are adjacent fishing habitats
          
          # plot(ShapeI)
          # 
          # sub=unique(AdjacentSetJ[ ,c("x", "y", "StatArea")]); coordinates(sub)=c("x", "y")
          # points(sub, pch=2)
          # plot(ShapeI, add=TRUE, border="green")
        } ## end TrapSet loop
        
        
        # ## remove any specified traps
        # Stage1s$TrapsFished[Stage1s$IecIndex_1 %in% ClosedPx &
        #                       Stage1s$Month %in% ClosureMonthsI]=
        #   Stage1s$TrapsFished[Stage1s$IecIndex_1 %in% ClosedPx &
        #                         Stage1s$Month %in% ClosureMonthsI] * Multiplier
      } ## end loop across closures
      
      StartTraps=sum(Stage1s$TrapsFished); StartTraps
      EndTraps=    sum(UnaffectedTraps$TrapsFished) +
        sum(UnmovedTraps$TrapsFished) +
        sum(RedistributedTraps$TrapsFished) + 
        sum(RemovedTraps$TrapsFished); EndTraps
      
      if(abs(StartTraps-EndTraps)/StartTraps>0.05) {
        print("Error; Greater than 5% of traps unaccounted for in redistribution around closures; Ending Scenario Run")
        break()
      }
      
      names(UnmovedTraps)=names(RedistributedTraps)
      Stage1s=rbind(UnaffectedTraps, UnmovedTraps, RedistributedTraps)
      
      if(PrintRedistributionMaps){
        ###################### Stage1 Scenario ########################################################--
        Redist_Agg=aggregate(TrapsFished~GridID+Month, RedistributedTraps, sum); ###summary(Redist_Agg)
        Redist_Agg$TrapsFished=Redist_Agg$TrapsFished+.01
        # Redist_Agg=merge(Redist_Agg, MonthDF);
        Redist_Agg$LogTraps=log10(Redist_Agg$TrapsFished);
        ### summary(Redist_Agg)
        
        ############--
        RedistMonths=sort(unique(Redist_Agg$Month))
        Redist_Wide=Long2Wide(Redist_Agg[ ,c("GridID", "Month", "TrapsFished")],
                              fName="Month", vName="TrapsFished", Prefix="m_"); ###summary(Redist_Wide)
        names(Redist_Wide)=c("GridID", MonthString[RedistMonths]); ###summary(Redist_Wide)
        Redist_TrapDensity_Px=merge(StandardPx, Redist_Wide); ###summary(Redist_Px)
        
        ColumnsToPrint=2:ncol(Redist_TrapDensity_Px)
        
        map1sRedistributedTrapDensity=
          spplot(Redist_TrapDensity_Px[names(Redist_TrapDensity_Px)[ColumnsToPrint]], #[MonthOrder],
                 Cuts=100, 
                 sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
                 main="Redistributed Trap Density from Closures"
                 # layout=c(4,3)
          ); ### map1sRedistributedTrapDensity
        
        ###########--
        Ymax=floor(max(Redist_Agg$LogTraps)); Ymax; 
        Yseq=0:Ymax; Yval=10^Yseq;
        
        RedistLog_Wide=Long2Wide(Redist_Agg[ ,c("GridID", "Month", "LogTraps")],
                                 fName="Month", vName="LogTraps", Prefix="m_"); ###summary(RedistLog_Wide)
        names(RedistLog_Wide)=c("GridID", MonthString[RedistMonths]); ###summary(RedistLog_Wide)
        Redist_TrapDensityLog_Px=merge(StandardPx, RedistLog_Wide); ###summary(RedistLog_Px)
        
        map1sRedistributedTrapDensityLog=
          spplot(Redist_TrapDensityLog_Px[names(Redist_TrapDensity_Px)[ColumnsToPrint]],
                 Cuts=100, 
                 sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
                 colorkey=list(labels=list(labels=Yval, at=Yseq)),
                 main="Redistributed Trap Density from Closures, Log-Scaled"
                 # layout=c(4,3)
          ); ### map1sRedistributedTrapDensityLog
        
        ##############################################################################--
        if(nrow(RemovedTraps)==0) {
          print("No trap removals associated with closures; No trap removal maps produced")
        } else {
          TrapRemov_Agg=aggregate(TrapsFished~GridID+Month, RemovedTraps, sum); ###summary(TrapRemov_Agg)
          TrapRemov_Agg$TrapsFished=TrapRemov_Agg$TrapsFished+.01
          # TrapRemov_Agg=merge(TrapRemov_Agg, MonthDF);
          TrapRemov_Agg$LogTraps=log10(TrapRemov_Agg$TrapsFished);
          ### summary(TrapRemov_Agg)
          
          ############--
          ColumnsToPrint=2:ncol(TrapRemov_Agg)
          
          TrapRemovMonths=sort(unique(TrapRemov_Agg$Month))
          TrapRemov_Wide=Long2Wide(TrapRemov_Agg[ ,c("GridID", "Month", "TrapsFished")],
                                   fName="Month", vName="TrapsFished", Prefix="m_"); ###summary(TrapRemov_Wide)
          names(TrapRemov_Wide)=c("GridID", MonthString[TrapRemovMonths]); ###summary(TrapRemov_Wide)
          TrapRemov_TrapDensity_Px=merge(StandardPx, TrapRemov_Wide); ###summary(TrapRemov_Px)
          
          map1sTrapRemovributedTrapDensity=
            spplot(TrapRemov_TrapDensity_Px[names(TrapRemov_TrapDensity_Px)[ColumnsToPrint]],
                   Cuts=100, 
                   sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
                   main="Trap Removals  associated with Closures",
                   layout=c(4,3)
            ); ### map1sTrapRemovributedTrapDensity
          
          ###########--
          Ymax=floor(max(TrapRemov_Agg$LogTraps)); Ymax; 
          Yseq=0:Ymax; Yval=10^Yseq;
          
          TrapRemovLog_Wide=Long2Wide(TrapRemov_Agg[ ,c("GridID", "Month", "LogTraps")],
                                      fName="Month", vName="LogTraps", Prefix="m_"); ###summary(TrapRemovLog_Wide)
          names(TrapRemovLog_Wide)=c("GridID", MonthString[TrapRemovMonths]); ###summary(TrapRemovLog_Wide)
          TrapRemov_TrapDensityLog_Px=merge(StandardPx, TrapRemovLog_Wide); ###summary(TrapRemovLog_Px)
          
          map1sTrapRemovributedTrapDensityLog=
            spplot(TrapRemov_TrapDensity_Px[names(TrapRemov_TrapDensity_Px)[ColumnsToPrint]],
                   Cuts=100, 
                   sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
                   colorkey=list(labels=list(labels=Yval, at=Yseq)),
                   main="Trap Removals  associated with Closures, Log-Scaled",
                   layout=c(4,3)
            ); ### map1sRedistributedTrapDensityLog
        }
        
      } ## Trap redistribution maps
      
    } ## end loop accounting for closures
    
    if(PrintDefaultMaps){     
      ###################### Stage1 Default ########################################################--
      Stage1_Agg=aggregate(TrapsFished~GridID+Month, Stage1d, sum); ###summary(Stage1_Agg)
      Stage1_Agg$TrapsFished=Stage1_Agg$TrapsFished+1
      # Stage1_Agg=merge(Stage1_Agg, MonthDF);
      Stage1_Agg$LogTraps=log10(Stage1_Agg$TrapsFished);
      ### summary(Stage1_Agg)
      
      ############--
      Stage1_Wide=Long2Wide(Stage1_Agg[ ,c("GridID", "Month", "TrapsFished")],
                            fName="Month", vName="TrapsFished", Prefix="m_"); ###summary(Stage1_Wide)
      names(Stage1_Wide)=c("GridID", MonthString); ###summary(Stage1_Wide)
      Stage1d_TrapDensity_Px=merge(StandardPx, Stage1_Wide); ###summary(Stage1_Px)
      
      map1dTrapDensity=
        spplot(Stage1d_TrapDensity_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
               main="Aggregate Trap Density - Default",
               layout=c(4,3)
        ); ###map1dTrapDensity
      
      ###########--
      Ymax=floor(max(Stage1_Agg$LogTraps)); Ymax; 
      Yseq=0:Ymax; Yval=10^Yseq;
      
      Stage1Log_Wide=Long2Wide(Stage1_Agg[ ,c("GridID", "Month", "LogTraps")],
                               fName="Month", vName="LogTraps", Prefix="m_"); ###summary(Stage1Log_Wide)
      names(Stage1Log_Wide)=c("GridID", MonthString); ###summary(Stage1Log_Wide)
      Stage1d_TrapDensityLog_Px=merge(StandardPx, Stage1Log_Wide); ###summary(Stage1Log_Px)
      
      map1dTrapDensityLog=
        spplot(Stage1d_TrapDensityLog_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
               colorkey=list(labels=list(labels=Yval, at=Yseq)),
               main="Aggregate Trap Density, Log-Scaled - Default",
               layout=c(4,3)
        ); ###map1dTrapDensityLog
    } ## Stage 1 default plots
    
    if(PrintScenarioMaps){
      ###################### Stage1 Scenario ########################################################--
      Stage1_Agg=aggregate(TrapsFished~GridID+Month, Stage1s, sum); ###summary(Stage1_Agg)
      Stage1_Agg$TrapsFished=Stage1_Agg$TrapsFished+1
      # Stage1_Agg=merge(Stage1_Agg, MonthDF);
      Stage1_Agg$LogTraps=log10(Stage1_Agg$TrapsFished);
      ### summary(Stage1_Agg)
      
      ############--
      Stage1_Wide=Long2Wide(Stage1_Agg[ ,c("GridID", "Month", "TrapsFished")],
                            fName="Month", vName="TrapsFished", Prefix="m_"); ###summary(Stage1_Wide)
      names(Stage1_Wide)=c("GridID", MonthString); ###summary(Stage1_Wide)
      Stage1s_TrapDensity_Px=merge(StandardPx, Stage1_Wide); ###summary(Stage1_Px)
      
      map1sTrapDensity=
        spplot(Stage1s_TrapDensity_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
               main="Aggregate Trap Density - Scenario",
               layout=c(4,3)
        ); ###map1sTrapDensity
      
      ###########--
      Ymax=floor(max(Stage1_Agg$LogTraps)); Ymax; 
      Yseq=0:Ymax; Yval=10^Yseq;
      
      Stage1Log_Wide=Long2Wide(Stage1_Agg[ ,c("GridID", "Month", "LogTraps")],
                               fName="Month", vName="LogTraps", Prefix="m_"); ###summary(Stage1Log_Wide)
      names(Stage1Log_Wide)=c("GridID", MonthString); ###summary(Stage1Log_Wide)
      Stage1s_TrapDensityLog_Px=merge(StandardPx, Stage1Log_Wide); ###summary(Stage1Log_Px)
      
      map1sTrapDensityLog=
        spplot(Stage1s_TrapDensityLog_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
               colorkey=list(labels=list(labels=Yval, at=Yseq)),
               main="Aggregate Trap Density, Log-Scaled - Scenario",
               layout=c(4,3)
        ); ###map1sTrapDensityLog
    } ## Stage 1 scenario maps 
    
    
  } ## fold closures
  
  ##########################################################--
  ## 2.0 Trap Reductions
  if(Fold) {
    print("2. Applying any trap reductions")
    ## Traps are further removed due to trap reductions (management option #2). 
    ## Easiest assumption is that traps will be removed proportionally over the entire management area.
    Stage2d=Stage1d
    Stage2s=Stage1s
    
    if(nrow(TrapReductions)>0){
      for(i in 1:nrow(TrapReductions)){
        ## 
        MapRef_I=MapRef;
        if(!is.na(TrapReductions$LMA[i])) {
          MapRef_I=MapRef_I[MapRef_I$LMA==TrapReductions$LMA[i], ]
        } 
        if(!is.na(TrapReductions$State[i])) {
          MapRef_I=MapRef_I[MapRef_I$State==TrapReductions$State[i], ]
        } 
        if(!is.na(TrapReductions$StatArea[i])) {
          StatAreasI=as.numeric(strsplit(TrapReductions$StatArea[i], ",")[[1]])
          MapRef_I=MapRef_I[MapRef_I$StatArea %in% StatAreasI, ]
        }
        if(!is.na(TrapReductions$Shapefile[i])) {
          TrapReductionsShape=TrapReductions$Shapefile[i]; TrapReductionsShape ## name of shapefile
          ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=TrapReductionsShape, verbose=FALSE) ## load shapefile
          ShapeI$ID=1 ## create a known field 
          ShapeI=spTransform(ShapeI, proj4string(MapRef))
          ReductionPx=MapRef$IecIndex_1[!is.na(over(MapRef, ShapeI)$ID)] ## get spatial overlap of shapefile from overlay
          MapRef_I=MapRef_I[MapRef_I$IecIndex_1 %in% ReductionPx, ]
        }
        
        summary(MapRef_I)
        ## applicable months
        if(!is.na(TrapReductions$Months)){
          print("Really, you're performing a seasonal trap reduction?")
          Months=as.numeric(strsplit(TrapReductions$Months[i], ",")[[1]])
        } else {Months=1:12}
        
        Stage2s$TrapsFished[Stage2s$IecIndex_1 %in% MapRef_I$IecIndex_1 &
                              Stage2s$Month %in% Months]=
          Stage2s$TrapsFished[Stage2s$IecIndex_1 %in% MapRef_I$IecIndex_1 &
                                Stage2s$Month %in% Months] * (1-(TrapReductions$Percentage[i])) ## apply reduction
      }
      # aggregate(TrapsFished~StatArea, TrapMap, sum)
      
    } ## perform trap reductions
    
    if(PrintDefaultMaps){ ## section currently unnecessary as duplicate output of stage 1
      #   ############### Stage 2 Default ######################################################--
      # Stage2_Agg=aggregate(TrapsFished~GridID+Month, Stage2d, sum); ###summary(Stage2_Agg)
      # Stage2_Agg$TrapsFished=Stage2_Agg$TrapsFished+1
      # # Stage2_Agg=merge(Stage2_Agg, MonthDF);
      # Stage2_Agg$LogTraps=log10(Stage2_Agg$TrapsFished);
      # ### summary(Stage2_Agg)
      # 
      # ############--
      # Stage2_Wide=Long2Wide(Stage2_Agg[ ,c("GridID", "Month", "TrapsFished")],
      #                       fName="Month", vName="TrapsFished", Prefix="m_"); ###summary(Stage2_Wide)
      # names(Stage2_Wide)=c("GridID", MonthString); ###summary(Stage2_Wide)
      # Stage2d_TrapDensity_Px=merge(StandardPx, Stage2_Wide); ###summary(Stage2_Px)
      # 
      # map2dTrapDensity=
      #   spplot(Stage2d_TrapDensity_Px[MonthOrder],
      #          Cuts=100, 
      #          sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
      #          main="Aggregate Trap Density - Post Reduction - Default",
      #          layout=c(4,3)
      #   ); ###map2dTrapDensity
      # 
      # ###########--
      # Ymax=floor(max(Stage2_Agg$LogTraps)); Ymax; 
      # Yseq=0:Ymax; Yval=10^Yseq;
      # 
      # Stage2Log_Wide=Long2Wide(Stage2_Agg[ ,c("GridID", "Month", "LogTraps")],
      #                          fName="Month", vName="LogTraps", Prefix="m_"); ###summary(Stage2Log_Wide)
      # names(Stage2Log_Wide)=c("GridID", MonthString); ###summary(Stage2Log_Wide)
      # Stage2d_TrapDensityLog_Px=merge(StandardPx, Stage2Log_Wide); ###summary(Stage2Log_Px)
      # 
      # map2dTrapDensityLog=
      #   spplot(Stage2d_TrapDensityLog_Px[MonthOrder],
      #          Cuts=100, 
      #          sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
      #          colorkey=list(labels=list(labels=Yval, at=Yseq)),
      #          main="Aggregate Trap Density, log-scaled, Post Reduction - Default",
      #          layout=c(4,3)
      #   ); ###map2dTrapDensityLog
    } ## stage 2 default maps
    
    if(PrintScenarioMaps){
      ############### Stage 2 Scenario ######################################################--
      Stage2_Agg=aggregate(TrapsFished~GridID+Month, Stage2s, sum); ###summary(Stage2_Agg)
      Stage2_Agg$TrapsFished=Stage2_Agg$TrapsFished+1
      # Stage2_Agg=merge(Stage2_Agg, MonthDF);
      Stage2_Agg$LogTraps=log10(Stage2_Agg$TrapsFished);
      ### summary(Stage2_Agg)
      
      ############--
      Stage2_Wide=Long2Wide(Stage2_Agg[ ,c("GridID", "Month", "TrapsFished")],
                            fName="Month", vName="TrapsFished", Prefix="m_"); ###summary(Stage2_Wide)
      names(Stage2_Wide)=c("GridID", MonthString); ###summary(Stage2_Wide)
      Stage2s_TrapDensity_Px=merge(StandardPx, Stage2_Wide); ###summary(Stage2_Px)
      
      map2sTrapDensity=
        spplot(Stage2s_TrapDensity_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
               main="Aggregate Trap Density - Post Reduction - Scenario",
               layout=c(4,3)
        ); ###map2sTrapDensity
      
      ###########--
      Ymax=floor(max(Stage2_Agg$LogTraps)); Ymax; 
      Yseq=0:Ymax; Yval=10^Yseq;
      
      Stage2Log_Wide=Long2Wide(Stage2_Agg[ ,c("GridID", "Month", "LogTraps")],
                               fName="Month", vName="LogTraps", Prefix="m_"); ###summary(Stage2Log_Wide)
      names(Stage2Log_Wide)=c("GridID", MonthString); ###summary(Stage2Log_Wide)
      Stage2s_TrapDensityLog_Px=merge(StandardPx, Stage2Log_Wide); ###summary(Stage2Log_Px)
      
      map2sTrapDensityLog=
        spplot(Stage2s_TrapDensityLog_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
               colorkey=list(labels=list(labels=Yval, at=Yseq)),
               main="Aggregate Trap Density, log-scaled, Post Reduction - Scenario",
               layout=c(4,3)
        ); ###map2sTrapDensityLog
      
    } ## Stage2 Scenario Maps
    
    #########################--
    Stage2dOutput=aggregate(TrapsFished~Month, Stage2d, sum); Stage2dOutput
    Stage2dTotal=aggregate(TrapsFished~1, Stage2dOutput, sum); Stage2dTotal
    Stage2dTotal$Month="Total"
    Stage2dOutput=rbind(Stage2dOutput, Stage2dTotal); Stage2dOutput
    Stage2dOutput$Scenario="Default"
    
    Stage2sOutput=aggregate(TrapsFished~Month, Stage2s, sum); Stage2sOutput
    Stage2sTotal=aggregate(TrapsFished~1, Stage2sOutput, sum); Stage2sTotal
    Stage2sTotal$Month="Total"
    Stage2sOutput=rbind(Stage2sOutput, Stage2sTotal); Stage2sOutput
    Stage2sOutput$Scenario="Scenario"
    
    Stage2Output=rbind(Stage2dOutput, Stage2sOutput); Stage2Output
    Stage2Output$Variable="TrapsFished"
    Stage2Output=Stage2Output[ ,c("Variable", "Scenario", "Month", "TrapsFished")]; #Stage2Output
    names(Stage2Output)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage2Output); #OutputData
    
  } ## fold trap reductions
  
  ###################################################################--
  ## 3.0 Convert traps to Trawls
  if(Fold) { 
    print("3 Converting Traps to Trawls")
    
    if(HighResolution){
      Stage3d=aggregate(TrapsFished~Region+VesselClass+Month+IecIndex_1+GridID, Stage2d, sum); 
      Stage3s=aggregate(TrapsFished~Region+VesselClass+Month+IecIndex_1+GridID, Stage2s, sum); 
    } else {
      Stage3d=aggregate(TrapsFished~Region+VesselClass+Month+GridID, Stage2d, sum); 
      Stage3s=aggregate(TrapsFished~Region+VesselClass+Month+GridID, Stage2s, sum); 
    }
    
    if(Fold) { ## Default data 
      ## Traps are converted to distribution of trawls based on existing empirical data or models of traps / trawl 
      ## at the scale of stat areas and proposed management (#3 above).
      ## Note: Trawl Length Model may need to be modified before merging due to new regulations on min / max trawl lengths
      # with(Stage3, tapply(X=TrapsFished, INDEX=list(Month, Region, VesselClass), FUN=sum))
      
      dim(Stage3d)
      Stage3d=merge(Stage3d, 
                    TrawlLengthModel, 
                    all.x=TRUE); #summary(Stage3d)
      ## Stage3d[is.na(Stage3d$TrawlProportion), ]
      Stage3d$TrawlProportion[is.na(Stage3d$TrawlProportion) &
                                Stage3d$TrapsFished==0]=0
      Stage3d$TrapsPerTrawl[is.na(Stage3d$TrapsPerTrawl) &
                              Stage3d$TrapsFished==0]=1
      Stage3d$TrawlUnitCost[is.na(Stage3d$TrawlUnitCost) &
                              Stage3d$TrapsFished==0]=1
      if(length(which(is.na(Stage3d$TrawlProportion)))>0 | length(which(is.na(Stage3d$TrawlUnitCost)))>1){
        print("Error: Some traps not matched to trawl configurations. Error in Stage3d");
        break()
      }
      
      Stage3d$TrawlUnits=with(Stage3d, TrapsFished / TrawlUnitCost); #summary(Stage3d) ## get the trawls multiplier
      Stage3d$TrawlsAtLength=with(Stage3d, TrawlUnits * TrawlProportion); ## get number of trawls at trawl length for grid cell
      ### summary(Stage3d)
      sum(Stage3d$TrawlsAtLength)
      
      if(PrintDefaultMaps){
        ############--
        Tmp=Stage3d[ ,c("VesselClass", "Month", "GridID", "TrapsPerTrawl", "TrawlsAtLength")];
        Tmp2=aggregate(TrawlsAtLength~Month+GridID, Tmp, sum); ###summary(Tmp2)
        names(Tmp2)=c("Month", "GridID", "Totals")
        Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
        Tmp$Totals[Tmp$Totals==0]=NA
        Tmp$Trawls=with(Tmp, TrapsPerTrawl * TrawlsAtLength/Totals);
        # Tmp$Trawls[is.na(Tmp$Trawls)]=0
        ### summary(Tmp)
        ############### Plotting ####################--
        Stage3_Agg=aggregate(Trawls~GridID+Month, Tmp, sum); ###summary(Stage3_Agg)
        
        Stage3_Wide=Long2Wide(Stage3_Agg[ ,c("GridID", "Month", "Trawls")],
                              fName="Month", vName="Trawls", Prefix="m_"); ###summary(Stage3_Wide)
        names(Stage3_Wide)=c("GridID", MonthString); ###summary(Stage3_Wide)
        Stage3d_TrawlLength_Px=merge(StandardPx, Stage3_Wide); ###summary(Stage3_Px)
        
        map3dTrawlLength=
          spplot(Stage3d_TrawlLength_Px[MonthOrder],
                 Cuts=100, 
                 sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
                 main="Mean Trawl Length (# Pots) - Default",
                 layout=c(4,3)
          ); ###map3dTrawlLength
      } ## print Stage 3 default map
    } ## end default data
    
    ################## Stage 3 Scenario ############################################################--
    if(Fold) { ## fold Scenario data 

      # with(Stage3, tapply(X=TrapsFished, INDEX=list(Month, Region, VesselClass), FUN=sum))
      dim(Stage3s)
      
      Stage3s=merge(Stage3s, 
                    TrawlLengthModel, 
                    all.x=TRUE); #summary(Stage3s)
      
      ## implement trawl length regulations ###############################################################--
      if(nrow(SC_TrawlLength)>0){
        for(i in 1:nrow(SC_TrawlLength)){
          print(paste("Reallocating trawl lengths for ", i, " of ", nrow(SC_TrawlLength), " scenarios", sep=""))
          
          ## constrain spatially
          MapRef_CrI=MapRef;
          if(!is.na(SC_TrawlLength$LMA[i])) {
            MapRef_CrI=MapRef_CrI[MapRef_CrI$LMA==SC_TrawlLength$LMA[i], ]
          } 
          if(!is.na(SC_TrawlLength$State[i])) {
            MapRef_CrI=MapRef_CrI[MapRef_CrI$State==SC_TrawlLength$State[i], ]
          } 
          if(!is.na(SC_TrawlLength$StatArea[i])) {
            StatAreasI=as.numeric(strsplit(SC_TrawlLength$StatArea[i], ",")[[1]])
            MapRef_CrI=MapRef_CrI[MapRef_CrI$StatArea %in% StatAreasI, ]
          }
          if(!is.na(SC_TrawlLength$Shapefile[i])) {
            SC_TrawlLengthShape=SC_TrawlLength$Shapefile[i]; SC_TrawlLengthShape ## name of shapefile
            ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=SC_TrawlLengthShape, verbose=FALSE) ## load shapefile
            ShapeI$ID=1 ## create a known field 
            ShapeI=spTransform(ShapeI, proj4string(MapRef_CrI))
            MapRef_CrI = MapRef_CrI[!is.na(over(MapRef_CrI, ShapeI)$ID), ] ## get spatial overlap of shapefile from overlay
          }
          
          if(HighResolution){
            AffectedPx=MapRef_CrI$IecIndex_1
            
            ## split data into affected, unaffected; modify affected; recombine
            UnAffectedTraps=Stage3s[!Stage3s$IecIndex_1 %in% AffectedPx, ]; dim(UnAffectedTraps)
            AffectedTraps=Stage3s[Stage3s$IecIndex_1 %in% AffectedPx, ]; dim(AffectedTraps)
          } else {
            AffectedPx=unique(MapRef_CrI$GridID)
            ## split data into affected, unaffected; modify affected; recombine
            UnAffectedTraps=Stage3s[!Stage3s$GridID %in% AffectedPx, ]; dim(UnAffectedTraps)
            AffectedTraps=Stage3s[Stage3s$GridID %in% AffectedPx, ]; dim(AffectedTraps)
          }

          AffectedTraps$TrapsPerTrawl=AffectedTraps$TrapsPerTrawlInt ## create copy of TrapsPerTrawl
          if(SC_TrawlLength$TrawlRegulation[i]=="Max"){
            AffectedTraps$TrapsPerTrawl[AffectedTraps$TrapsPerTrawl>SC_TrawlLength$TrawlLen[i]]=SC_TrawlLength$TrawlLen[i] #1 Adjust trawl lengths
          } 
          if(SC_TrawlLength$TrawlRegulation[i]=="Min"){
            AffectedTraps$TrapsPerTrawl[AffectedTraps$TrapsPerTrawl<SC_TrawlLength$TrawlLen[i]]=SC_TrawlLength$TrawlLen[i] #1 Adjust trawl lengths
          } 
          if(SC_TrawlLength$TrawlRegulation[i]=="Exactly"){
            AffectedTraps$TrapsPerTrawl=SC_TrawlLength$TrawlLen[i] #1 Adjust trawl lengths
          } 
          
          
          if(HighResolution){
            # hist(AffectedTraps$TrapsPerTrawl)
          AffectedTraps$Prop2=with(AffectedTraps, TrapsPerTrawlInt/TrapsPerTrawl * TrawlProportion ) #2 Apply multiplier to proportion
          AffectedTraps=aggregate(Prop2~Region+VesselClass+Month+IecIndex_1+GridID+TrapsFished+TrapsPerTrawl, AffectedTraps, sum) #3 recalculate proportion
          
          
          Totals=aggregate(Prop2~VesselClass + IecIndex_1 + Month, AffectedTraps, sum); # 4 sum new proportions
          names(Totals)=c("VesselClass", "IecIndex_1", "Month", "Prop2Total"); ##summary(Totals)
          AffectedTraps=merge(AffectedTraps, Totals); ## append 
          
          AffectedTraps$TrawlProportion=with(AffectedTraps, Prop2 / Prop2Total) # 5 new proportions
          AffectedTraps$Product=with(AffectedTraps, TrapsPerTrawl * TrawlProportion) # 6 TrawlLength weight
          
          ##summary(AffectedTraps)
          
          ProductTotals=aggregate(Product~VesselClass+Month+IecIndex_1, AffectedTraps, sum) # sum across trawl weights
          names(ProductTotals)=c("VesselClass", "Month", "IecIndex_1", "TrawlUnitCost")
          
          AffectedTraps=merge(AffectedTraps, ProductTotals); ##summary(AffectedTraps)
          
          names(AffectedTraps)[which(names(AffectedTraps)=="TrapsPerTrawl")]="TrapsPerTrawlInt" ## update names
          
          AffectedTraps=AffectedTraps[ ,names(UnAffectedTraps)]; summary(AffectedTraps) ## discard unwanted columns
          
          Stage3s=rbind(UnAffectedTraps, AffectedTraps)
          } else {
            # hist(AffectedTraps$TrapsPerTrawl)
            AffectedTraps$Prop2=with(AffectedTraps, TrapsPerTrawlInt/TrapsPerTrawl * TrawlProportion ) #2 Apply multiplier to proportion
            AffectedTraps=aggregate(Prop2~Region+VesselClass+Month+GridID+TrapsFished+TrapsPerTrawl, AffectedTraps, sum) #3 recalculate proportion
            
            
            Totals=aggregate(Prop2~VesselClass + GridID + Month, AffectedTraps, sum); # 4 sum new proportions
            names(Totals)=c("VesselClass", "GridID", "Month", "Prop2Total"); ##summary(Totals)
            AffectedTraps=merge(AffectedTraps, Totals); ## append 
            
            AffectedTraps$TrawlProportion=with(AffectedTraps, Prop2 / Prop2Total) # 5 new proportions
            AffectedTraps$Product=with(AffectedTraps, TrapsPerTrawl * TrawlProportion) # 6 TrawlLength weight
            
            ##summary(AffectedTraps)
            
            ProductTotals=aggregate(Product~VesselClass+Month+GridID, AffectedTraps, sum) # sum across trawl weights
            names(ProductTotals)=c("VesselClass", "Month", "GridID", "TrawlUnitCost")
            
            AffectedTraps=merge(AffectedTraps, ProductTotals); ##summary(AffectedTraps)
            
            names(AffectedTraps)[which(names(AffectedTraps)=="TrapsPerTrawl")]="TrapsPerTrawlInt" ## update names
            
            AffectedTraps=AffectedTraps[ ,names(UnAffectedTraps)]; summary(AffectedTraps) ## discard unwanted columns
            
            Stage3s=rbind(UnAffectedTraps, AffectedTraps)
            
          }
          
        } ## end SC_TrawlLength loop
        
      } ##
      
      ## Stage3s[is.na(Stage3s$TrawlProportion), ]
      Stage3s$TrawlProportion[is.na(Stage3s$TrawlProportion) &
                                Stage3s$TrapsFished==0]=0
      Stage3s$TrapsPerTrawl[is.na(Stage3s$TrapsPerTrawl) &
                              Stage3s$TrapsFished==0]=1
      Stage3s$TrawlUnitCost[is.na(Stage3s$TrawlUnitCost) &
                              Stage3s$TrapsFished==0]=1
      if(length(which(is.na(Stage3s$TrawlProportion)))>0 | length(which(is.na(Stage3s$TrawlUnitCost)))>1){
        print("Error: Some traps not matched to trawl configurations. Error in Stage3s");
        break()
      }
      
      Stage3s$TrawlUnits=with(Stage3s, TrapsFished / TrawlUnitCost); #summary(Stage3s) ## get the trawls multiplier
      Stage3s$TrawlsAtLength=with(Stage3s, TrawlUnits * TrawlProportion); ## get number of trawls at trawl length for grid cell
      ### summary(Stage3s)
      sum(Stage3s$TrawlsAtLength)
      
      if(PrintScenarioMaps){
        
        ############--
        Tmp=Stage3s[ ,c("VesselClass", "Month", "GridID", "TrapsPerTrawl", "TrawlsAtLength")];
        Tmp2=aggregate(TrawlsAtLength~Month+GridID, Tmp, sum); ###summary(Tmp2)
        names(Tmp2)=c("Month", "GridID", "Totals")
        Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
        Tmp$Totals[Tmp$Totals==0]=NA
        Tmp$Trawls=with(Tmp, TrapsPerTrawl * TrawlsAtLength/Totals);
        # Tmp$Trawls[is.na(Tmp$Trawls)]=0
        ### summary(Tmp)
        ############### Plotting ####################--
        Stage3_Agg=aggregate(Trawls~GridID+Month, Tmp, sum); ###summary(Stage3_Agg)
        
        Stage3_Wide=Long2Wide(Stage3_Agg[ ,c("GridID", "Month", "Trawls")],
                              fName="Month", vName="Trawls", Prefix="m_"); ###summary(Stage3_Wide)
        names(Stage3_Wide)=c("GridID", MonthString); ###summary(Stage3_Wide)
        Stage3s_TrawlLength_Px=merge(StandardPx, Stage3_Wide); ###summary(Stage3_Px)
        
        map3sTrawlLength=
          spplot(Stage3s_TrawlLength_Px[MonthOrder],
                 Cuts=100, 
                 sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
                 main="Mean Trawl Length (# Pots) - Scenario",
                 layout=c(4,3)
          ); ###map3sTrawlLength
      } ## Stage 3 scenario maps
      
    } ## fold Scenario data 
    ##################################################--
    Stage3dOutput=aggregate(TrawlsAtLength~Month, Stage3d, sum); #Stage3dOutput
    Stage3dTotal=aggregate(TrawlsAtLength~1, Stage3dOutput, sum); #Stage3dTotal
    Stage3dTotal$Month="Total"
    Stage3dOutput=rbind(Stage3dOutput, Stage3dTotal); #Stage3dOutput
    Stage3dOutput$Scenario="Default"
    
    Stage3sOutput=aggregate(TrawlsAtLength~Month, Stage3s, sum); #Stage3sOutput
    Stage3sTotal=aggregate(TrawlsAtLength~1, Stage3sOutput, sum); #Stage3sTotal
    Stage3sTotal$Month="Total"
    Stage3sOutput=rbind(Stage3sOutput, Stage3sTotal); #Stage3sOutput
    Stage3sOutput$Scenario="Scenario"
    
    Stage3Output=rbind(Stage3dOutput, Stage3sOutput); #Stage3Output
    Stage3Output$Variable="Trawls"
    Stage3Output=Stage3Output[ ,c("Variable", "Scenario", "Month", "TrawlsAtLength")]; #Stage3Output
    names(Stage3Output)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage3Output); #OutputData
    
  } ## convert traps to trawls; management action
  
  ###################################################################--
  ## 4.0 Convert Trawls to vertical Lines
  if(Fold) { 
    print("4 Calculating Vertical Lines from Trawls")
    ## Trawls are converted to vertical lines based on trawl length. 
    ## Expected to be two vertical lines per trawl for all offshore areas 
    ## but may be different for inshore areas with shorter trawls.
    Stage4d=aggregate(TrawlsAtLength~+GridID+Month+TrapsPerTrawl, Stage3d, sum)
    if(HighResolution){
      Stage4s=aggregate(TrawlsAtLength~IecIndex_1+GridID+Month+TrapsPerTrawl, Stage3s, sum)
      Stage4d=aggregate(TrawlsAtLength~IecIndex_1+GridID+Month+TrapsPerTrawl, Stage3d, sum)
    } else {
        Stage4d=aggregate(TrawlsAtLength~+GridID+Month+TrapsPerTrawl, Stage3d, sum)
        Stage4s=aggregate(TrawlsAtLength~+GridID+Month+TrapsPerTrawl, Stage3s, sum); #summary(Stage4s);
      } 
    dim(Stage4s)
    
    Stage4d$TrapsPerTrawlInt=round(Stage4d$TrapsPerTrawl);
    Stage4d=merge(Stage4d, EndlinesPerTrawl, all.x=TRUE); ###summary(Stage4d) 
    
    if(length(which(is.na(Stage4d$EndlinesPerTrawl)))>0){ ## if there are any trawl lengths that don't match an endline
      print("Error: Some trawls lengths not matched to an endline.  Error in Stage4d");
      break()
    }
    
    Stage4d$NumVerticalLines=Stage4d$TrawlsAtLength * Stage4d$EndlinesPerTrawl
    ### summary(Stage4d)
    # aggregate(NumVerticalLines~Month, Stage4d, sum)
    
    ######################## Stage 4 Scenario #####################################################################--
    Stage4s$TrapsPerTrawlInt=round(Stage4s$TrapsPerTrawl);
    Stage4s=merge(Stage4s, EndlinesPerTrawl, all.x=TRUE); ###summary(Stage4s) 
    
    if(length(which(is.na(Stage4s$EndlinesPerTrawl)))>0){ ## if there are any trawl lengths that don't match an endline
      print("Error: Some trawls lengths not matched to an endline.  Error in Stage4s");
      break()
    }
    
    Stage4s$NumVerticalLines=Stage4s$TrawlsAtLength * Stage4s$EndlinesPerTrawl
    
    ############################### Ropeless Scenarios################################################################--
    if(nrow(SC_RopelessDevice)>0){
      for(i in 1:nrow(SC_RopelessDevice)){
        print(paste("Applying ropeless devices for ", i, " of ", nrow(SC_RopelessDevice), " scenarios", sep=""))
        
        ## constrain spatially
        MapRef_CrI=MapRef;
        if(!is.na(SC_RopelessDevice$LMA[i])) {
          MapRef_CrI=MapRef_CrI[MapRef_CrI$LMA==SC_RopelessDevice$LMA[i], ]
        } 
        if(!is.na(SC_RopelessDevice$State[i])) {
          MapRef_CrI=MapRef_CrI[MapRef_CrI$State==SC_RopelessDevice$State[i], ]
        } 
        if(!is.na(SC_RopelessDevice$StatArea[i])) {
          StatAreasI=as.numeric(strsplit(SC_RopelessDevice$StatArea[i], ",")[[1]])
          MapRef_CrI=MapRef_CrI[MapRef_CrI$StatArea %in% StatAreasI, ]
        }
        if(!is.na(SC_RopelessDevice$Shapefile[i])) {
          SC_RopelessDeviceShape=SC_RopelessDevice$Shapefile[i]; SC_RopelessDeviceShape ## name of shapefile
          ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=SC_RopelessDeviceShape, verbose=FALSE) ## load shapefile
          ShapeI$ID=1 ## create a known field 
          ShapeI=spTransform(ShapeI, proj4string(MapRef_CrI))
          MapRef_CrI = MapRef_CrI[!is.na(over(MapRef_CrI, ShapeI)$ID), ] ## get spatial overlap of shapefile from overlay
        }
        # plot(MapRef_CrI)
        
        if(HighResolution){
          AffectedPx=MapRef_CrI$IecIndex_1
          
          ## split data into affected, unaffected; modify affected; recombine
          UnAffectedTraps=Stage4s[!Stage4s$IecIndex_1 %in% AffectedPx, ]; dim(UnAffectedTraps)
          AffectedTraps=merge(Stage4s, MapRef_CrI[ ,c("IecIndex_1", "Distance")]); dim(AffectedTraps)
          unique(AffectedTraps$Distance)
          RopelessRisk
          
          AffectedTraps$RopelessDevice=SC_RopelessDevice$RopelessDevice[i]
          AffectedTraps=merge(AffectedTraps, RopelessRisk); dim(AffectedTraps)
          
          AffectedTraps$NumVerticalLines_Adj=with(AffectedTraps, NumVerticalLines * LineMultiplier);
          summary(AffectedTraps)
          
          AffectedTraps=AffectedTraps[ ,c("TrapsPerTrawlInt","IecIndex_1","GridID","Month","TrapsPerTrawl","TrawlsAtLength",
                                          "EndlinesPerTrawl","NumVerticalLines_Adj")]
          names(AffectedTraps)=c("TrapsPerTrawlInt","IecIndex_1","GridID","Month","TrapsPerTrawl","TrawlsAtLength",
                                 "EndlinesPerTrawl","NumVerticalLines")
          
          Stage4s=rbind(UnAffectedTraps, AffectedTraps); dim(Stage4s)
        } else {
          AffectedPx=unique(MapRef_CrI$GridID)
          
          ## split data into affected, unaffected; modify affected; recombine
          UnAffectedTraps=Stage4s[!Stage4s$GridID %in% AffectedPx, ]; dim(UnAffectedTraps)
          AffectedTraps=merge(Stage4s, MapRef_CrI[ ,c("GridID", "Distance")]); dim(AffectedTraps)
          unique(AffectedTraps$Distance)
          RopelessRisk
          
          AffectedTraps$RopelessDevice=SC_RopelessDevice$RopelessDevice[i]
          AffectedTraps=merge(AffectedTraps, RopelessRisk); dim(AffectedTraps)
          
          AffectedTraps$NumVerticalLines_Adj=with(AffectedTraps, NumVerticalLines * LineMultiplier);
          summary(AffectedTraps)
          
          AffectedTraps=AffectedTraps[ ,c("TrapsPerTrawlInt","GridID","Month","TrapsPerTrawl","TrawlsAtLength",
                                          "EndlinesPerTrawl","NumVerticalLines_Adj")]
          names(AffectedTraps)=c("TrapsPerTrawlInt","GridID","Month","TrapsPerTrawl","TrawlsAtLength",
                                 "EndlinesPerTrawl","NumVerticalLines")
          
          Stage4s=rbind(UnAffectedTraps, AffectedTraps); dim(Stage4s)
          
        }
      } ## end loop across scenarios
    } ## end application of ropeless
    
    ### summary(Stage4s)
    # aggregate(NumVerticalLines~Month, Stage4s, sum)
    
    if(PrintDefaultMaps){    
      ############ Plotting ##############################################################--
      Stage4_Agg=aggregate(NumVerticalLines~GridID+Month, Stage4d, sum); ###summary(Stage4_Agg)
      Stage4_Agg$NumVerticalLines=Stage4_Agg$NumVerticalLines+0.01
      Stage4_Agg$LogLines=log10(Stage4_Agg$NumVerticalLines);
      ### summary(Stage4_Agg)
      
      Stage4_Wide=Long2Wide(Stage4_Agg[ ,c("GridID", "Month", "NumVerticalLines")],
                            fName="Month", vName="NumVerticalLines", Prefix="m_"); ###summary(Stage4_Wide)
      names(Stage4_Wide)=c("GridID", MonthString); ###summary(Stage4_Wide)
      Stage4d_LineDensity_Px=merge(StandardPx, Stage4_Wide); ###summary(Stage4_Px)
      
      map4dLineDensity=
        spplot(Stage4d_LineDensity_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               main="Aggregate Vertical Line Density - Default",
               layout=c(4,3)
        ); ###map4dLineDensity
      
      ###########--
      Ymax=floor(max(Stage4_Agg$LogLines)); Ymax; 
      Yseq=-2:Ymax; Yval=10^Yseq; Yval
      
      Stage4Log_Wide=Long2Wide(Stage4_Agg[ ,c("GridID", "Month", "LogLines")],
                               fName="Month", vName="LogLines", Prefix="m_"); ###summary(Stage4Log_Wide)
      names(Stage4Log_Wide)=c("GridID", MonthString); ###summary(Stage4Log_Wide)
      Stage4d_LineDensityLog_Px=merge(StandardPx, Stage4Log_Wide); 
      
      map4dLineDensityLog=
        spplot(Stage4d_LineDensityLog_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               colorkey=list(labels=list(labels=Yval, at=Yseq)),
               main="Aggregate Line Density, Log-Scaled - Default",
               layout=c(4,3)
        ); ###map4dLineDensityLog
    } ## Stage 4 default maps
    
    if(PrintScenarioMaps){
      ############ Plotting ##############################################################--
      Stage4_Agg=aggregate(NumVerticalLines~GridID+Month, Stage4s, sum); ###summary(Stage4_Agg)
      Stage4_Agg$NumVerticalLines=Stage4_Agg$NumVerticalLines+0.01
      Stage4_Agg$LogLines=log10(Stage4_Agg$NumVerticalLines);
      ### summary(Stage4_Agg)
      
      Stage4_Wide=Long2Wide(Stage4_Agg[ ,c("GridID", "Month", "NumVerticalLines")],
                            fName="Month", vName="NumVerticalLines", Prefix="m_"); ###summary(Stage4_Wide)
      names(Stage4_Wide)=c("GridID", MonthString); ###summary(Stage4_Wide)
      Stage4s_LineDensity_Px=merge(StandardPx, Stage4_Wide); ###summary(Stage4_Px)
      
      map4sLineDensity=
        spplot(Stage4s_LineDensity_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               main="Aggregate Vertical Line Density - Scenario",
               layout=c(4,3)
        ); ###map4sLineDensity
      
      ###########--
      Ymax=floor(max(Stage4_Agg$LogLines)); Ymax; 
      Yseq=-2:Ymax; Yval=10^Yseq; Yval
      
      Stage4Log_Wide=Long2Wide(Stage4_Agg[ ,c("GridID", "Month", "LogLines")],
                               fName="Month", vName="LogLines", Prefix="m_"); ###summary(Stage4Log_Wide)
      names(Stage4Log_Wide)=c("GridID", MonthString); ###summary(Stage4Log_Wide)
      Stage4s_LineDensityLog_Px=merge(StandardPx, Stage4Log_Wide); 
      
      map4sLineDensityLog=
        spplot(Stage4s_LineDensityLog_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               colorkey=list(labels=list(labels=Yval, at=Yseq)),
               main="Aggregate Line Density, Log-Scaled - Scenario",
               layout=c(4,3)
        ); ###map4sLineDensityLog
    } ## Stage 4 scenario maps
    
    ######################################################################--
    Stage4dOutput=aggregate(NumVerticalLines~Month, Stage4d, sum); #Stage4dOutput
    Stage4dTotal=aggregate(NumVerticalLines~1, Stage4dOutput, sum); #Stage4dTotal
    Stage4dTotal$Month="Total"
    Stage4dOutput=rbind(Stage4dOutput, Stage4dTotal); #Stage4dOutput
    Stage4dOutput$Scenario="Default"
    
    Stage4sOutput=aggregate(NumVerticalLines~Month, Stage4s, sum); #Stage4sOutput
    Stage4sTotal=aggregate(NumVerticalLines~1, Stage4sOutput, sum); #Stage4sTotal
    Stage4sTotal$Month="Total"
    Stage4sOutput=rbind(Stage4sOutput, Stage4sTotal); #Stage4sOutput
    Stage4sOutput$Scenario="Scenario"
    
    Stage4Output=rbind(Stage4dOutput, Stage4sOutput); #Stage4Output
    Stage4Output$Variable="VerticalLines"
    Stage4Output=Stage4Output[ ,c("Variable", "Scenario", "Month", "NumVerticalLines")]; #Stage4Output
    names(Stage4Output)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage4Output); #OutputData
    
  } ## convert trawls to vertical lines
  
  ###################################################################--
  ## 5.0 Adjust Vertical Line Numbers through management actions
  ## Number of vertical lines are further adjusted for any regulations on ropeless or timed-release fishing (#5 above). 
  ## Given the technology available to the lobster fishery and other fisheries, 
  ## it is improbable that these measures can be implemented immediately 
  ## but it is appropriate to include this now in the model framework.
  Stage5d=Stage4d;
  Stage5s=Stage4s;
  
  ####################################################################--
  ## 6.0 Characterize Vertical Line Diameters
  if(Fold) {
    print("6 Characterizing Vertical Line Diameters")
    ## A distribution of line diameters for vertical lines is characterized 
    ## based on observed relationships with trawl length and further modified based on management options (#4 above).
    Stage6d=Stage5d; dim(Stage6d)
    Stage6d=merge(Stage6d, LineMod, all.x=TRUE); #summary(Stage6d)
    
    if(length(which(is.na(Stage6d$RopeDiam)))>0){ ## if there are any trawl lengths that don't match an endline
      print("Error: Some trawls lengths not matched to line diameters.  Error in Stage6d");
      break()
    }
    
    ## apportion lines across sizes
    Stage6d$NumVerticalLinesAtSize=with(Stage6d, NumVerticalLines * Prop_Line)
    #summary(Stage6d)
    
    if(PrintDefaultMaps){
      ############--
      Tmp=Stage6d[ ,c("Month", "GridID", "RopeDiam", "NumVerticalLinesAtSize")];
      Tmp2=aggregate(NumVerticalLinesAtSize~Month+GridID, Tmp, sum); ###summary(Tmp2)
      names(Tmp2)=c("Month", "GridID", "Totals")
      # plot(quantile(Tmp2$Totals, (0:100)/100), ylim=c(0,1));
      Tmp2$Totals[Tmp2$Totals==0]=0.01
      Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
      Tmp$LineDia=with(Tmp, RopeDiam * NumVerticalLinesAtSize/Totals);
      
      ### summary(Tmp)
      ###############--
      Stage6_Agg=aggregate(LineDia~GridID+Month, Tmp, sum); ###summary(Stage6_Agg)
      
      Stage6_Wide=Long2Wide(Stage6_Agg[ ,c("GridID", "Month", "LineDia")],
                            fName="Month", vName="LineDia", Prefix="m_"); ###summary(Stage6_Wide)
      names(Stage6_Wide)=c("GridID", MonthString); ###summary(Stage6_Wide)
      Stage6d_LineDia_Px=merge(StandardPx, Stage6_Wide); ###summary(Stage6_Px)
      
      map6dLineDia=
        spplot(Stage6d_LineDia_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               main="Mean Line Diameter (inches) - Default",
               layout=c(4,3)
        ); ###map6dLineDia
    } ## Stage 6 default maps
    
    
    ############### Stage 6 Scenario #####################################--
    Stage6s=Stage5s; dim(Stage6s)
    Stage6s=merge(Stage6s, LineMod, all.x=TRUE); #summary(Stage6s)
    
    if(length(which(is.na(Stage6s$RopeDiam)))>0){ ## if there are any trawl lengths that don't match an endline
      print("Error: Some trawls lengths not matched to line diameters.  Error in Stage6s");
      break()
    }
    
    ## apportion lines across sizes
    Stage6s$NumVerticalLinesAtSize=with(Stage6s, NumVerticalLines * Prop_Line)
    #summary(Stage6s)
    table(Stage6s$Rope)
    
    if(PrintScenarioMaps){
      ############--
      Tmp=Stage6s[ ,c("Month", "GridID", "RopeDiam", "NumVerticalLinesAtSize")];
      Tmp2=aggregate(NumVerticalLinesAtSize~Month+GridID, Tmp, sum); ###summary(Tmp2)
      names(Tmp2)=c("Month", "GridID", "Totals")
      ###plot(quantile(Tmp2$Totals, (0:100)/100), ylim=c(0,1));
      Tmp2$Totals[Tmp2$Totals==0]=0.01
      Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
      Tmp$LineDia=with(Tmp, RopeDiam * NumVerticalLinesAtSize/Totals);
      
      ### summary(Tmp)
      ###############--
      Stage6_Agg=aggregate(LineDia~GridID+Month, Tmp, sum); ###summary(Stage6_Agg)
      
      Stage6_Wide=Long2Wide(Stage6_Agg[ ,c("GridID", "Month", "LineDia")],
                            fName="Month", vName="LineDia", Prefix="m_"); ###summary(Stage6_Wide)
      names(Stage6_Wide)=c("GridID", MonthString); ###summary(Stage6_Wide)
      Stage6s_LineDia_Px=merge(StandardPx, Stage6_Wide); ###summary(Stage6_Px)
      
      map6sLineDia=
        spplot(Stage6s_LineDia_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               main="Mean Line Diameter (inches) - Scenario",
               layout=c(4,3)
        ); ###map6sLineDia
    } ## Stage 6 Scenario Maps
    
  } ## fold line diameters
  
  ##################################################################--
  ## 7.0 Modify line diameters according to management action
  ## this includes 
  if(Fold) { ## fold line diameter management 
    print("7 Applying any management measures to line diameters")
    
    if(HighResolution){
      Stage7d=aggregate(NumVerticalLinesAtSize~IecIndex_1+GridID+Month+RopeDiam+TrapsPerTrawl, Stage6d, sum); 
      Stage7s=aggregate(NumVerticalLinesAtSize~IecIndex_1+GridID+Month+RopeDiam+TrapsPerTrawl, Stage6s, sum); 
    } else {
      Stage7d=aggregate(NumVerticalLinesAtSize~GridID+Month+RopeDiam+TrapsPerTrawl, Stage6d, sum); 
      Stage7s=aggregate(NumVerticalLinesAtSize~GridID+Month+RopeDiam+TrapsPerTrawl, Stage6s, sum); 
    } ## 
    
    Stage7d=merge(Stage7d, LineConversion)
    dim(Stage7d)
    Stage7d$BuoylineDevice=NA
    
    
    ## Manipulate line diameters for management ######################################################--
    
    if(nrow(SC_MaxRopeDia)>0){
      names(SC_MaxRopeDia)=c("Action", "LMA", "State", "StatArea", "Shapefile", "Months", "Rope");
      SC_MaxRopeDia=merge(SC_MaxRopeDia, LineConversion)
      
      for(i in 1:nrow(SC_MaxRopeDia)){
        print(paste("Applying changes to vertical lines for ", i, " of ", nrow(SC_MaxRopeDia), " scenarios", sep=""))
        
        ## constrain spatially
        MapRef_CrI=MapRef;
        if(!is.na(SC_MaxRopeDia$LMA[i])) {
          MapRef_CrI=MapRef_CrI[MapRef_CrI$LMA==SC_MaxRopeDia$LMA[i], ]
        } 
        if(!is.na(SC_MaxRopeDia$State[i])) {
          MapRef_CrI=MapRef_CrI[MapRef_CrI$State==SC_MaxRopeDia$State[i], ]
        } 
        if(!is.na(SC_MaxRopeDia$StatArea[i])) {
          StatAreasI=as.numeric(strsplit(SC_MaxRopeDia$StatArea[i], ",")[[1]])
          MapRef_CrI=MapRef_CrI[MapRef_CrI$StatArea %in% StatAreasI, ]
        }
        if(!is.na(SC_MaxRopeDia$Shapefile[i])) {
          SC_MaxRopeDiaShape=SC_MaxRopeDia$Shapefile[i]; SC_MaxRopeDiaShape ## name of shapefile
          ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=SC_MaxRopeDiaShape, verbose=FALSE) ## load shapefile
          ShapeI$ID=1 ## create a known field 
          ShapeI=spTransform(ShapeI, proj4string(MapRef_CrI))
          MapRef_CrI = MapRef_CrI[!is.na(over(MapRef_CrI, ShapeI)$ID), ] ## get spatial overlap of shapefile from overlay
        }
        # plot(MapRef_CrI)
        
        
        if(!is.na(SC_MaxRopeDia$Months[i])){
          SC_MaxRopeDiaMonthsI=as.numeric(strsplit(SC_MaxRopeDia$Months[i], ",")[[1]]); SC_MaxRopeDiaMonthsI ## get months
        } else {SC_MaxRopeDiaMonthsI=1:12}; ## or use all months
        
        ## split data into affected, unaffected; modify affected; recombine
        if(HighResolution){
          AffectedPx=MapRef_CrI$IecIndex_1
          UnAffectedTraps=Stage7s[!Stage7s$IecIndex_1 %in% AffectedPx |
                                    (Stage7s$IecIndex_1 %in% AffectedPx &
                                       !Stage7s$Month %in% SC_MaxRopeDiaMonthsI), ]; dim(UnAffectedTraps)
          AffectedTraps=Stage7s[Stage7s$IecIndex_1 %in% AffectedPx & 
                                  Stage7s$Month %in% SC_MaxRopeDiaMonthsI, ]; dim(AffectedTraps)
        } else {
          AffectedPx=unique(MapRef_CrI$GridID)
          UnAffectedTraps=Stage7s[!Stage7s$GridID %in% AffectedPx |
                                    (Stage7s$GridID %in% AffectedPx &
                                       !Stage7s$Month %in% SC_MaxRopeDiaMonthsI), ]; dim(UnAffectedTraps)
          AffectedTraps=Stage7s[Stage7s$GridID %in% AffectedPx & 
                                  Stage7s$Month %in% SC_MaxRopeDiaMonthsI, ]; dim(AffectedTraps)
        }
        if(nrow(Stage7s)!= (nrow(UnAffectedTraps) + nrow(AffectedTraps)) ) {
          print("Error: Records lost in Stage7 splitting Unaffected and Affected")
          break()
        }
        
        AffectedTraps$RopeDiam[AffectedTraps$RopeDiam>SC_MaxRopeDia$RopeDiam[i]]=SC_MaxRopeDia$RopeDiam[i]
        
        # table(AffectedTraps$RopeDiam)
        
        Stage7s=rbind(UnAffectedTraps, AffectedTraps);
        
      } ## end scenario loop
    } ## end management actions
    
    Stage7s=merge(Stage7s, LineConversion);
    # summary(Stage7s)
    # table(Stage7s$Rope)
    Stage7s$BuoylineDevice=NA
    
    
    ## Add Buoyline Devices ##########################################################################--
    
    ##  SC_BuoylineDevice
    # unique(BuoylineThreat$BuoylineDevice)
    
    if(nrow(SC_BuoylineDevice)>0){
      
      for(i in 1:nrow(SC_BuoylineDevice)){
        print(paste("Applying changes to vertical lines devices for ", i, " of ", nrow(SC_BuoylineDevice), " scenarios", sep=""))
        
        ## constrain spatially
        MapRef_CrI=MapRef;
        if(!is.na(SC_BuoylineDevice$LMA[i])) {
          MapRef_CrI=MapRef_CrI[MapRef_CrI$LMA==SC_BuoylineDevice$LMA[i], ]
        } 
        if(!is.na(SC_BuoylineDevice$State[i])) {
          MapRef_CrI=MapRef_CrI[MapRef_CrI$State==SC_BuoylineDevice$State[i], ]
        } 
        if(!is.na(SC_BuoylineDevice$StatArea[i])) {
          StatAreasI=as.numeric(strsplit(SC_BuoylineDevice$StatArea[i], ",")[[1]])
          MapRef_CrI=MapRef_CrI[MapRef_CrI$StatArea %in% StatAreasI, ]
        }
        if(!is.na(SC_BuoylineDevice$Shapefile[i])) {
          SC_BuoylineDeviceShape=SC_BuoylineDevice$Shapefile[i]; SC_BuoylineDeviceShape ## name of shapefile
          ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=SC_BuoylineDeviceShape, verbose=FALSE) ## load shapefile
          ShapeI$ID=1 ## create a known field 
          ShapeI=spTransform(ShapeI, proj4string(MapRef_CrI))
          MapRef_CrI = MapRef_CrI[!is.na(over(MapRef_CrI, ShapeI)$ID), ] ## get spatial overlap of shapefile from overlay
        }
        # plot(MapRef_CrI)
        
        
        if(!is.na(SC_BuoylineDevice$Months[i])){
          SC_BuoylineDeviceMonthsI=as.numeric(strsplit(SC_BuoylineDevice$Months[i], ",")[[1]]); SC_BuoylineDeviceMonthsI ## get months
        } else {SC_BuoylineDeviceMonthsI=1:12}; ## or use all months
        
        ## split data into affected, unaffected; modify affected; recombine
        if(HighResolution){
          AffectedPx=MapRef_CrI$IecIndex_1
          UnAffectedTraps=Stage7s[!Stage7s$IecIndex_1 %in% AffectedPx |
                                    (Stage7s$IecIndex_1 %in% AffectedPx &
                                       !Stage7s$Month %in% SC_BuoylineDeviceMonthsI), ]; dim(UnAffectedTraps)
          AffectedTraps=Stage7s[Stage7s$IecIndex_1 %in% AffectedPx & 
                                  Stage7s$Month %in% SC_BuoylineDeviceMonthsI, ]; dim(AffectedTraps)
        } else {
          AffectedPx=unique(MapRef_CrI$GridID)
          UnAffectedTraps=Stage7s[!Stage7s$GridID %in% AffectedPx |
                                    (Stage7s$GridID %in% AffectedPx &
                                       !Stage7s$Month %in% SC_BuoylineDeviceMonthsI), ]; dim(UnAffectedTraps)
          AffectedTraps=Stage7s[Stage7s$GridID %in% AffectedPx & 
                                  Stage7s$Month %in% SC_BuoylineDeviceMonthsI, ]; dim(AffectedTraps)
        }
        
        if(nrow(Stage7s)!= (nrow(UnAffectedTraps) + nrow(AffectedTraps)) ) {
          print("Error: Records lost in Stage7 splitting Unaffected and Affected")
          break()
        }
        
        AffectedTraps$BuoylineDevice = SC_BuoylineDevice$BuoylineDevice[i]
        
        # table(AffectedTraps$RopeDiam)
        
        Stage7s=rbind(UnAffectedTraps, AffectedTraps);
        
      } ## end scenario loop
    } ## end management actions
    
  }  ## fold line diameter management   
  #################################################################--
  ## 8.0 Merge line diameters with Threat
  if(Fold) {
    print("8 Calculating gear configuration threat")
    ## Line diameters are converted to Threat based on a model to be developed, possibly by polling the TRT.
    Stage8d=Stage7d;
    Stage8d$TrapsPerTrawl=round(Stage8d$TrapsPerTrawl)
    Stage8d$TrapsPerTrawl[Stage8d$TrapsPerTrawl==0]=1
    head(Stage8d)
    dim(Stage8d)
    Stage8d=merge(Stage8d, TrapConversion, all.x=TRUE); 
    Stage8d=merge(Stage8d, BuoylineThreat, all.x=TRUE); summary(Stage8d)
    
    #summary(BuoylineRisk) SC_BuoylineDevice
    
    # Stage8d=merge(Stage8d, ThreatMod, all.x=TRUE)
    ### summary(Stage8d)
    
    if(length(which(is.na(Stage8d$Threat)))>0){ ## if there are any trawl lengths that don't match an endline
      print("Error: Some gear configurations not matched to a threat score.  Error in Stage8d");
      break()
    }
    
    ##move down to 9
    Stage8d$ThreatScore=with(Stage8d, NumVerticalLinesAtSize * Threat); 
    ### summary(Stage8d)
    
    ########################## Stage 8 Scenario ##################################--
    Stage8s=Stage7s;
    Stage8s$TrapsPerTrawl=round(Stage8s$TrapsPerTrawl)
    Stage8s$TrapsPerTrawl[Stage8s$TrapsPerTrawl==0]=1
    head(Stage8s)
    dim(Stage8s)
    Stage8s=merge(Stage8s, TrapConversion, all.x=TRUE); 
    Stage8s=merge(Stage8s, BuoylineThreat, all.x=TRUE); summary(Stage8s)
    
    #  NAs=Stage8s[is.na(Stage8s$Threat), ]; summary(NAs)
    #  table(NAs$BuoylineDevice)
    #  table(NAs$Rope)
    #  
    #  NAsDevice=unique(NAs$BuoylineDevice); NAsDevice
    # BLDevice=unique(BuoylineThreat$BuoylineDevice); BLDevice
    # match(NAsDevice, BLDevice)
    # table(BuoylineThreat$Rope)
    # merge(NAs, BuoylineThreat)
    # match(unique(NAs$Rope), unique(BuoylineThreat$Rope))
    # match(unique(NAs$TrawlLen), unique(BuoylineThreat$TrawlLen))
    # NAs1=NAs[1, ]; NAs1
    # head(BuoylineThreat)
    # WR=BuoylineThreat[BuoylineThreat$BuoylineDevice=="1,700@100m", ]; WR
    
     ### summary(Stage8s)
    
    if(length(which(is.na(Stage8s$Threat)))>0){ ## if there are any trawl lengths that don't match an endline
      print("Error: Some gear configurations not matched to a threat score.  Error in Stage8s");
      break()
    }
    
    Stage8s$ThreatScore=with(Stage8s, NumVerticalLinesAtSize * Threat); 
    ### summary(Stage8s)
    
    if(PrintDefaultMaps){
      ############# mean Threat #############################--
      Tmp=Stage8d[ ,c("Month", "GridID", "NumVerticalLinesAtSize", "Threat" )];
      Tmp2=aggregate(NumVerticalLinesAtSize~Month+GridID, Tmp, sum); ###summary(Tmp2)
      names(Tmp2)=c("Month", "GridID", "Totals")
      Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
      Tmp$MeanThreat=with(Tmp, Threat * NumVerticalLinesAtSize /Totals);
      
      ##################--
      Stage8d_Agg=aggregate(MeanThreat~GridID+Month, Tmp, sum); ###summary(Stage8d_Agg)
      
      Stage8d_Wide=Long2Wide(Stage8d_Agg[ ,c("GridID", "Month", "MeanThreat")],
                             fName="Month", vName="MeanThreat", Prefix="m_"); ###summary(Stage8d_Wide)
      names(Stage8d_Wide)=c("GridID", MonthString); ###summary(Stage8d_Wide)
      Stage8d_MeanThreat_Px=merge(StandardPx, Stage8d_Wide); ###summary(Stage8d_Px)
      
      map8dMeanThreat=
        spplot(Stage8d_MeanThreat_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               main="Mean Individual Vertical Line Threat - Default",
               layout=c(4,3)
        ); ###map8dMeanThreat
      
      ############ total Threat #########################--
      Tmp=Stage8d[ ,c("Month", "GridID", "NumVerticalLinesAtSize", "Threat" )];
      Tmp2=aggregate(Threat~Month+GridID, Tmp, sum); ###summary(Tmp2)
      names(Tmp2)=c("Month", "GridID", "Totals")
      Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
      Tmp$TotalThreat=with(Tmp, NumVerticalLinesAtSize * Threat/Totals);
      
      ### summary(Tmp)
      ###############--
      Stage8_Agg=aggregate(TotalThreat~GridID+Month, Tmp, sum); ###summary(Stage8_Agg)
      Stage8_Agg$TotalThreat=Stage8_Agg$TotalThreat+0.01;
      Stage8_Agg$LogThreat=log10(Stage8_Agg$TotalThreat);
      ### summary(Stage8_Agg)
      
      Stage8_Wide=Long2Wide(Stage8_Agg[ ,c("GridID", "Month", "TotalThreat")],
                            fName="Month", vName="TotalThreat", Prefix="m_"); ###summary(Stage8_Wide)
      names(Stage8_Wide)=c("GridID", MonthString); ###summary(Stage8_Wide)
      Stage8d_TotalThreat_Px=merge(StandardPx, Stage8_Wide); ###summary(Stage8_Px)
      
      map8dTotalThreat=
        spplot(Stage8d_TotalThreat_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               main="Aggregate Line Threat Score (Line Threat X Line Density) - Default",
               layout=c(4,3)
        ); ###map8dTotalThreat
      
      ###########--
      Ymax=floor(max(Stage8_Agg$LogThreat)); Ymax; 
      Yseq=-2:Ymax; Yval=10^Yseq; Yval
      
      Stage8Log_Wide=Long2Wide(Stage8_Agg[ ,c("GridID", "Month", "LogThreat")],
                               fName="Month", vName="LogThreat", Prefix="m_"); ###summary(Stage8Log_Wide)
      names(Stage8Log_Wide)=c("GridID", MonthString); ###summary(Stage8Log_Wide)
      Stage8d_TotalThreatLog_Px=merge(StandardPx, Stage8Log_Wide); ###summary(Stage8Log_Px)
      
      map8dTotalThreatLog=
        spplot(Stage8d_TotalThreatLog_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               colorkey=list(labels=list(labels=Yval, at=Yseq)),
               main="Aggregate Line Threat Score (Line Threat X Line Density), Log-Scaled - Default",
               layout=c(4,3)
        ); ###map8dTotalThreatLog
    } ## stage 8 default maps
    
    if(PrintScenarioMaps){
      ############# mean Threat #############################--
      Tmp=Stage8s[ ,c("Month", "GridID", "NumVerticalLinesAtSize", "Threat" )];
      Tmp2=aggregate(NumVerticalLinesAtSize~Month+GridID, Tmp, sum); ###summary(Tmp2)
      names(Tmp2)=c("Month", "GridID", "Totals")
      Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
      Tmp$MeanThreat=with(Tmp, Threat * NumVerticalLinesAtSize /Totals);
      
      ##################--
      Stage8_Agg=aggregate(MeanThreat~GridID+Month, Tmp, sum); ###summary(Stage8_Agg)
      
      Stage8_Wide=Long2Wide(Stage8_Agg[ ,c("GridID", "Month", "MeanThreat")],
                            fName="Month", vName="MeanThreat", Prefix="m_"); ###summary(Stage8_Wide)
      names(Stage8_Wide)=c("GridID", MonthString); ###summary(Stage8_Wide)
      Stage8s_MeanThreat_Px=merge(StandardPx, Stage8_Wide); ###summary(Stage8_Px)
      
      map8sMeanThreat=
        spplot(Stage8s_MeanThreat_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               main="Mean Individual Vertical Line Threat - Scenario",
               layout=c(4,3)
        ); ###map8sMeanThreat
      
      ############ total Threat #########################
      Tmp=Stage8s[ ,c("Month", "GridID", "NumVerticalLinesAtSize", "Threat" )];
      Tmp2=aggregate(Threat~Month+GridID, Tmp, sum); ###summary(Tmp2)
      names(Tmp2)=c("Month", "GridID", "Totals")
      Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
      Tmp$TotalThreat=with(Tmp, NumVerticalLinesAtSize * Threat/Totals);
      
      ### summary(Tmp)
      ###############
      Stage8_Agg=aggregate(TotalThreat~GridID+Month, Tmp, sum); ###summary(Stage8_Agg)
      Stage8_Agg$TotalThreat=Stage8_Agg$TotalThreat+0.01;
      Stage8_Agg$LogThreat=log10(Stage8_Agg$TotalThreat);
      ### summary(Stage8_Agg)
      
      Stage8_Wide=Long2Wide(Stage8_Agg[ ,c("GridID", "Month", "TotalThreat")],
                            fName="Month", vName="TotalThreat", Prefix="m_"); ###summary(Stage8_Wide)
      names(Stage8_Wide)=c("GridID", MonthString); ###summary(Stage8_Wide)
      Stage8s_TotalThreat_Px=merge(StandardPx, Stage8_Wide); ###summary(Stage8_Px)
      
      map8sTotalThreat=
        spplot(Stage8s_TotalThreat_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               main="Aggregate Line Threat Score (Line Threat X Line Density) - Scenario",
               layout=c(4,3)
        ); ###map8sTotalThreat
      
      ###########--
      Ymax=floor(max(Stage8_Agg$LogThreat)); Ymax; 
      Yseq=-2:Ymax; Yval=10^Yseq; Yval
      
      Stage8Log_Wide=Long2Wide(Stage8_Agg[ ,c("GridID", "Month", "LogThreat")],
                               fName="Month", vName="LogThreat", Prefix="m_"); ###summary(Stage8Log_Wide)
      names(Stage8Log_Wide)=c("GridID", MonthString); ###summary(Stage8Log_Wide)
      Stage8s_TotalThreatLog_Px=merge(StandardPx, Stage8Log_Wide); ###summary(Stage8Log_Px)
      
      map8sTotalThreatLog=
        spplot(Stage8s_TotalThreatLog_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               colorkey=list(labels=list(labels=Yval, at=Yseq)),
               main="Aggregate Line Threat Score (Line Threat X Line Density), Log-Scaled - Scenario",
               layout=c(4,3)
        ); ###map8sTotalThreatLog
    } ## Stage 8 scenario maps
    
  } ## Threat
  
  #############################################################--
  ## 9. Calculate Risk
  if(Fold) {
    print("9 Calculating composite risk values")
    ## Risk is calculated as the product of Threat and whale presence.
    if(HighResolution){
      WhaleModel=WhalesAt1Nm[ ,c("IecIndex_1", "GridID", "Month", "Density10Nm")] ## keep  model at 1Nm resolution
      names(WhaleModel)=c("IecIndex_1", "GridID", "Month", "WhaleDensity")
      WhaleModel_Map=aggregate(WhaleDensity~GridID+Month, WhaleModel, mean) ## aggregate to 10Nm for plotting
    } else {
      WhaleModel=aggregate(Density10Nm~Month+GridID, WhalesAt1Nm, mean) ## aggregate to 10Nm
      names(WhaleModel)=c("Month", "GridID", "WhaleDensity")
      WhaleModel_Map=WhaleModel ## create copy for mapping     
    }
    
    if(Fold){ ## fold whale habitat mapping 
      WhaleModel_Map$WhaleDensity=WhaleModel_Map$WhaleDensity+10^(-10) ## add small amount to avoid zero densities for mapping
      WhaleModel_Map$WhaleDensityLog=log10(WhaleModel_Map$WhaleDensity)
      
      ############### Whale habitat density ############################--
      Stage9_Wide=Long2Wide(WhaleModel_Map[ ,c("GridID", "Month", "WhaleDensity")],
                            fName="Month", vName="WhaleDensity", Prefix="m_"); ###summary(Stage9_Wide)
      names(Stage9_Wide)=c("GridID", MonthString); ###summary(Stage9_Wide)
      Stage9_WhaleHabitat_Px=merge(StandardPx, Stage9_Wide); ###summary(Stage9_Px)
      
      map9WhaleHabitat=
        spplot(Stage9_WhaleHabitat_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               main="Whale Habitat Score",
               layout=c(4,3)
        ); ###map9WhaleHabitat
      
      ###########--
      Ymax=floor(max(WhaleModel_Map$WhaleDensityLog)); Ymax; 
      Yseq=c(-10:Ymax, max(WhaleModel_Map$WhaleDensityLog)); Yval=10^Yseq; Yval
      Yval[length(Yval)]=round(Yval[length(Yval)],1); Yval
      
      Stage9Log_Wide=Long2Wide(WhaleModel_Map[ ,c("GridID", "Month", "WhaleDensityLog")],
                               fName="Month", vName="WhaleDensityLog", Prefix="m_"); ###summary(Stage9Log_Wide)
      names(Stage9Log_Wide)=c("GridID", MonthString); ###summary(Stage9Log_Wide)
      Stage9_WhaleHabitatLog_Px=merge(StandardPx, Stage9Log_Wide); ###summary(Stage9Log_Px)
      
      map9WhaleHabitatLog=
        spplot(Stage9_WhaleHabitatLog_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               colorkey=list(labels=list(labels=Yval, at=Yseq)),
               main="Whale Habitat Score, Log-Scaled",
               layout=c(4,3)
        ); ###map9WhaleHabitatLog
    } ## fold whale habitat mapping 
    
    ############# merge gear threats with whale habitat model ###########################    --
    if(HighResolution){
      Stage9d=aggregate(ThreatScore~IecIndex_1+GridID+Month, Stage8d, sum); ###summary(Stage9d)
      Stage9s=aggregate(ThreatScore~IecIndex_1+GridID+Month, Stage8s, sum); ###summary(Stage9s)
    } else {
      Stage9d=aggregate(ThreatScore~GridID+Month, Stage8d, sum); ###summary(Stage9d)
      Stage9s=aggregate(ThreatScore~GridID+Month, Stage8s, sum); ###summary(Stage9s)
    }
    Stage9d=merge(Stage9d, WhaleModel, all.y=TRUE); 
    ### summary(Stage9d)
    Stage9d$ThreatScore[is.na(Stage9d$ThreatScore)]=0
    # Stage9d$WhaleDensity[is.na(Stage9d$WhaleDensity)]=0
    # plot(quantile(Stage9d$WhaleDensity, (0:1000)/1000), ylim=c(0,1))
    # plot(quantile(Stage9d$WhaleDensityLog, (0:1000)/1000))
    
    Stage9d$Risk=with(Stage9d, WhaleDensity * ThreatScore); ###summary(Stage9d)
    Stage9d$Risk=Stage9d$Risk+10^(-10)
    Stage9d$RiskLog=log10(Stage9d$Risk)
    
    # plot(sort(log10(Stage9d$Risk)))
    ### summary(Stage9d)
    
    if(PrintDefaultMaps){
      ############### Risk Values ####################--
      Stage9_Wide=Long2Wide(Stage9d[ ,c("GridID", "Month", "Risk")],
                            fName="Month", vName="Risk", Prefix="m_"); ###summary(Stage9_Wide)
      names(Stage9_Wide)=c("GridID", MonthString); ###summary(Stage9_Wide)
      Stage9d_RiskScore_Px=merge(StandardPx, Stage9_Wide); ###summary(Stage9_Px)
      
      map9dRiskScore=
        spplot(Stage9d_RiskScore_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               main="Total Risk Score - Default",
               layout=c(4,3)
        ); ###map9dRiskScore
      
      ###########--
      Ymax=floor(max(Stage9d$RiskLog)); Ymax;
      Ymin=ceiling(min(na.omit(Stage9d$RiskLog))); Ymin;
      # Ymin=max(Ymin, 1); Stage9d$RiskLog[Stage9d$RiskLog<=Ymin]=NA
      Yseq=Ymin:Ymax; Yval=10^Yseq; Yval
      
      Stage9Log_Wide=Long2Wide(Stage9d[ ,c("GridID", "Month", "RiskLog")],
                               fName="Month", vName="RiskLog", Prefix="m_"); ###summary(Stage9Log_Wide)
      names(Stage9Log_Wide)=c("GridID", MonthString); ###summary(Stage9Log_Wide)
      Stage9d_RiskScoreLog_Px=merge(StandardPx, Stage9Log_Wide); ###summary(Stage9Log_Px)
      
      map9dRiskScoreLog=
        spplot(Stage9d_RiskScoreLog_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               colorkey=list(labels=list(labels=Yval, at=Yseq)),
               main="Total Risk Score, Log-Scaled - Default",
               layout=c(4,3)
        ); ###map9dRiskScoreLog
      
      ###################################################################--
      Ymin=max(Ymin, 1); Stage9d$RiskLog[Stage9d$RiskLog<=Ymin]=NA
      Yseq=Ymin:Ymax; Yval=10^Yseq; Yval
      
      Stage9Log_Wide=Long2Wide(Stage9d[ ,c("GridID", "Month", "RiskLog")],
                               fName="Month", vName="RiskLog", Prefix="m_"); ###summary(Stage9Log_Wide)
      names(Stage9Log_Wide)=c("GridID", MonthString); ###summary(Stage9Log_Wide)
      Stage9d_RiskScoreLog_Abridged_Px=merge(StandardPx, Stage9Log_Wide); ###summary(Stage9Log_Px)
      
      map9dRiskScoreLog_Abridged=
        spplot(Stage9d_RiskScoreLog_Abridged_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               colorkey=list(labels=list(labels=Yval, at=Yseq)),
               main="Total Risk Score, Log-Scaled - Default - Abridged",
               layout=c(4,3)
        ); ###map9dRiskScoreLog_Abridged
      
    } ## stage 9 default maps
    
    ################## Stage 9 Scenario ############################################--
    ## option would be to use all=TRUE and pick up pixels missing in both sets and assume=0
    Stage9s=aggregate(ThreatScore~GridID+Month, Stage8s, sum); ###summary(Stage9s)
    Stage9s=merge(Stage9s, WhaleModel, all.y=TRUE); 
    ### summary(Stage9s)
    Stage9s$ThreatScore[is.na(Stage9s$ThreatScore)]=0
    # Stage9s$WhaleDensity[is.na(Stage9s$WhaleDensity)]=0
    # plot(quantile(Stage9s$WhaleDensity, (0:1000)/1000), ylim=c(0,1))
    Stage9s$WhaleDensity=Stage9s$WhaleDensity+10^(-10)
    Stage9s$WhaleDensityLog=log10(Stage9s$WhaleDensity)
    # plot(quantile(Stage9s$WhaleDensityLog, (0:1000)/1000))
    
    Stage9s$Risk=with(Stage9s, WhaleDensity * ThreatScore); ###summary(Stage9s)
    Stage9s$Risk=Stage9s$Risk+10^(-10)
    Stage9s$RiskLog=log10(Stage9s$Risk)
    
    # plot(sort(log10(Stage9s$Risk)))
    ### summary(Stage9s)
    
    if(PrintScenarioMaps){
      
      ############### Risk Values ####################--
      Stage9_Wide=Long2Wide(Stage9s[ ,c("GridID", "Month", "Risk")],
                            fName="Month", vName="Risk", Prefix="m_"); ###summary(Stage9_Wide)
      names(Stage9_Wide)=c("GridID", MonthString); ###summary(Stage9_Wide)
      Stage9s_RiskScore_Px=merge(StandardPx, Stage9_Wide); ###summary(Stage9_Px)
      
      map9sRiskScore=
        spplot(Stage9s_RiskScore_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               main="Total Risk Score - Scenario",
               layout=c(4,3)
        ); ###map9sRiskScore
      
      ####################--
      Ymax=floor(max(na.omit(Stage9s$RiskLog))); Ymax;
      Ymin=ceiling(min(na.omit(Stage9s$RiskLog))); Ymin;
      # Ymin=max(Ymin, 1); Stage9s$RiskLog[Stage9s$RiskLog<=Ymin]=NA
      Yseq=Ymin:Ymax; Yval=10^Yseq; Yval
      
      Stage9Log_Wide=Long2Wide(Stage9s[ ,c("GridID", "Month", "RiskLog")],
                               fName="Month", vName="RiskLog", Prefix="m_"); ###summary(Stage9Log_Wide)
      names(Stage9Log_Wide)=c("GridID", MonthString); ###summary(Stage9Log_Wide)
      Stage9s_RiskScoreLog_Px=merge(StandardPx, Stage9Log_Wide); ###summary(Stage9Log_Px)
      
      map9sRiskScoreLog=
        spplot(Stage9s_RiskScoreLog_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               colorkey=list(labels=list(labels=Yval, at=Yseq)),
               main="Total Risk Score, Log-Scaled - Default",
               layout=c(4,3)
        ); ###map9sRiskScoreLog
      
      ###################################################################--
      Ymin=max(Ymin, 1); Stage9s$RiskLog[Stage9s$RiskLog<=Ymin]=NA
      Yseq=Ymin:Ymax; Yval=10^Yseq; Yval
      
      Stage9Log_Wide=Long2Wide(Stage9s[ ,c("GridID", "Month", "RiskLog")],
                               fName="Month", vName="RiskLog", Prefix="m_"); ###summary(Stage9Log_Wide)
      names(Stage9Log_Wide)=c("GridID", MonthString); ###summary(Stage9Log_Wide)
      Stage9s_RiskScoreLog_Abridged_Px=merge(StandardPx, Stage9Log_Wide); ###summary(Stage9Log_Px)
      
      map9sRiskScoreLog_Abridged=
        spplot(Stage9s_RiskScoreLog_Abridged_Px[MonthOrder],
               Cuts=100, 
               sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
               colorkey=list(labels=list(labels=Yval, at=Yseq)),
               main="Total Risk Score, Log-Scaled - Abridged",
               layout=c(4,3)
        ); ###map9sRiskScoreLog_Abridged
      
    } ## stage 9 scenario maps
    
    ###############################################################################--
    Stage9dOutput=aggregate(Risk~Month, Stage9d, sum); #Stage9dOutput
    Stage9dTotal=aggregate(Risk~1, Stage9dOutput, sum); #Stage9dTotal
    Stage9dTotal$Month="Total"
    Stage9dOutput=rbind(Stage9dOutput, Stage9dTotal); #Stage9dOutput
    Stage9dOutput$Scenario="Default"
    
    Stage9sOutput=aggregate(Risk~Month, Stage9s, sum); #Stage9sOutput
    Stage9sTotal=aggregate(Risk~1, Stage9sOutput, sum); #Stage9sTotal
    Stage9sTotal$Month="Total"
    Stage9sOutput=rbind(Stage9sOutput, Stage9sTotal); #Stage9sOutput
    Stage9sOutput$Scenario="Scenario"
    
    Stage9Output=rbind(Stage9dOutput, Stage9sOutput); #Stage9Output
    Stage9Output$Variable="RelativeRisk"
    Stage9Output=Stage9Output[ ,c("Variable", "Scenario", "Month", "Risk")]; #Stage9Output
    names(Stage9Output)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage9Output); #OutputData
    
  } ## Whale habitat and risk maps
  
  #####################################################################--
  ## 10. Create output directory and write maps and files
  
  ScenariosDir=(paste(HD, "Scenarios", sep="/"));
  setwd(ScenariosDir)
  if(!dir.exists(OutputDir)) {dir.create(OutputDir)}
  setwd(OutputDir)
  
  #######################################################################################################--
  
  OutputData_Wide=Long2Wide(OutputData, fName="Scenario", vName="Value"); ## OutputData_Wide
  OutputData_Wide$Reduction=with(OutputData_Wide, 1-Scenario/Default); ## OutputData_Wide
  
  #######################################################################################################--
  
  print("Writing output")
  
  if(PrintDefaultMaps){
    print("Writing Default Maps")
    
    pdf(file=paste(OutputDir, "_DefaultFigures.pdf", sep=""),
        width=11,height=9.5,paper="special")
    plot(map1dTrapDensity);
    plot(map1dTrapDensityLog);
    
    # plot(map2dTrapDensity); ## post reduction
    # plot(map2dTrapDensityLog); ## post reduction
    
    plot(map3dTrawlLength);
    
    plot(map4dLineDensity);
    plot(map4dLineDensityLog);
    
    plot(map6dLineDia);
    
    plot(map8dMeanThreat);
    plot(map8dTotalThreat);
    plot(map8dTotalThreatLog);
    
    plot(map9WhaleHabitat);
    plot(map9WhaleHabitatLog);
    
    plot(map9dRiskScore);
    plot(map9dRiskScoreLog);
    
    dev.off()
    
    ## save maps
    save(
      map1dTrapDensity,
      map1dTrapDensityLog,
      map3dTrawlLength,
      
      map4dLineDensity,
      map4dLineDensityLog,
      
      map6dLineDia,
      
      map8dMeanThreat,
      map8dTotalThreat,
      map8dTotalThreatLog,
      
      map9WhaleHabitat,
      map9WhaleHabitatLog,
      
      map9dRiskScore,
      map9dRiskScoreLog,
      file=paste(OutputDir, "_DefaultMaps.Rdata", sep="")
    )
    
  } ## print default maps
  
  if(PrintScenarioMaps){
    print("Writing Scenario Maps")
    
    pdf(file=paste(OutputDir, "_ScenarioFigures.pdf", sep=""),
        width=11,height=9.5,paper="special")
    
    plot(map1sTrapDensity);
    plot(map1sTrapDensityLog);
    
    
    plot(map2sTrapDensity); ## post reduction
    plot(map2sTrapDensityLog); ## post reduction
    
    plot(map3sTrawlLength);
    
    plot(map4sLineDensity);
    plot(map4sLineDensityLog);
    
    plot(map6sLineDia);
    
    plot(map8sMeanThreat);
    plot(map8sTotalThreat);
    plot(map8sTotalThreatLog);
    plot(map9WhaleHabitat);
    plot(map9WhaleHabitatLog);
    plot(map9sRiskScore);
    plot(map9sRiskScoreLog);
    
    dev.off()
    
    ## save maps
    save(
      map1sTrapDensity,
      map1sTrapDensityLog,
      
      map2sTrapDensity, 
      map2sTrapDensityLog, 
      
      map3sTrawlLength,
      
      map4sLineDensity,
      map4sLineDensityLog,
      
      map6sLineDia,
      
      map8sMeanThreat,
      map8sTotalThreat,
      map8sTotalThreatLog,
      map9WhaleHabitat,
      map9WhaleHabitatLog,
      map9sRiskScore,
      map9sRiskScoreLog,
      
      file=paste(OutputDir, "_ScenarioMaps.Rdata", sep="")
    )
    
  } ## print scenario maps
  
  if(exists("map1sRedistributedTrapDensity")) {
    print("Writing Scenario Maps")
    
    pdf(file=paste(OutputDir, "_TrapRedistributionFigures.pdf", sep=""),
        width=11,height=9.5,paper="special")
    
    plot(map1sRedistributedTrapDensity);
    plot(map1sRedistributedTrapDensityLog);
    
    if(exists("map1sTrapRemovributedTrapDensity")) {
      plot(map1sTrapRemovributedTrapDensity);
      plot(map1sTrapRemovributedTrapDensityLog);
    }
    dev.off()
  }
  
  if(PrintTables){
    print("Writing Tables")
    
    pdf(file=paste(OutputDir, "_Tables.pdf", sep=""),
        width=11, height=8.5, paper="special")
    
    # grid.draw(PrintTable(Tbl=ScenarioInputs,
    #                      Title="Trap Numbers",
    #                      TitleFont=14))
    # grid.newpage()
    grid.draw(PrintTable(Tbl=OutputData_Wide[OutputData_Wide$Variable=="TrapsFished", ],
                         Title="Trap Numbers",
                         TitleFont=14))
    grid.newpage()
    grid.draw(PrintTable(Tbl=OutputData_Wide[OutputData_Wide$Variable=="Trawls", ],
                         Title="Total Trawls",
                         TitleFont=14))
    grid.newpage()
    grid.draw(PrintTable(Tbl=OutputData_Wide[OutputData_Wide$Variable=="VerticalLines", ],
                         Title="Total Vertical Line",
                         TitleFont=14))
    grid.newpage()
    grid.draw(PrintTable(Tbl=OutputData_Wide[OutputData_Wide$Variable=="RelativeRisk", ],
                         Title="Relative Risk",
                         TitleFont=14))
    dev.off()
    
  }  ## print tables
  
  ### Add list of maps
  
  if(WriteMapSources){
    print("Writing Map Sources")
    if(PrintDefaultMaps){
      save(
        Stage1d_TrapDensity_Px,
        Stage1d_TrapDensityLog_Px,
        
        Stage3d_TrawlLength_Px,
        Stage3s_TrawlLength_Px,
        
        Stage4d_LineDensity_Px,
        Stage4d_LineDensityLog_Px,
        
        ## no output from Stage5 yet
        
        Stage6d_LineDia_Px,
        Stage6s_LineDia_Px,
        
        ## no output from Stage7 yet
        
        Stage8d_MeanThreat_Px,
        Stage8d_TotalThreat_Px,
        Stage8d_TotalThreatLog_Px,
        
        Stage9_WhaleHabitat_Px,
        Stage9_WhaleHabitatLog_Px,
        Stage9d_RiskScore_Px,
        Stage9d_RiskScoreLog_Px,
        Stage9d_RiskScoreLog_Abridged_Px,
        
        file=paste(OutputDir, "_MapSources_Default.Rdata", sep="")
      )
    } 
    
    if(PrintScenarioMaps){
      save(
        Stage1s_TrapDensity_Px,
        Stage1s_TrapDensityLog_Px,
        
        Stage2s_TrapDensity_Px,
        Stage2s_TrapDensityLog_Px,
        
        Stage3s_TrawlLength_Px,
        
        Stage4s_LineDensity_Px,
        Stage4s_LineDensityLog_Px,
        
        ## no output from Stage5 yet
        
        Stage6d_LineDia_Px,
        Stage6s_LineDia_Px,
        
        ## no output from Stage7 yet
        
        Stage8s_MeanThreat_Px,
        Stage8s_TotalThreat_Px,
        Stage8s_TotalThreatLog_Px,
        
        Stage9_WhaleHabitat_Px,
        Stage9_WhaleHabitatLog_Px,
        Stage9s_RiskScore_Px,
        Stage9s_RiskScoreLog_Px,
        Stage9s_RiskScoreLog_Abridged_Px,
        
        file=paste(OutputDir, "_MapSources_Scenario.Rdata", sep="")
      )
    } 
    
  }
  
  if(WriteOutputCsv) {
    print("Writing Output to .csv")
    
    write.csv(x=OutputData_Wide,
              file=paste(OutputDir, "_OutputData.csv", sep=""),
              row.names = FALSE)
  }  
  
  message("Writing rmd file")  
  OutputPathDir <- here::here("Scenarios",OutputDir)
  rmarkdown::render(input = here::here("template.Rmd"), output_format="html_document",output_file=paste0(OutputDir,".html"),output_dir=OutputPathDir,params = list(set_title = InputSpreadsheetName,
                                                                                                                                                                   set_path = OutputDir))
  
  message("Model run completed")  
  
} ## end function
