DecisionTool=function(
  HomeDir,
  InputSpreadsheetName,
  HighResolution=FALSE, ## run moel at 10Nm (TRUE) or 1Nm (FALSE); lower resolution runs faster
  PrintTables=TRUE,
  PrintDefaultMaps=TRUE,
  PrintScenarioMaps=TRUE,
  WriteOutputCsv=TRUE,
  WriteMapSources=FALSE
) {
  HD <- HomeDir
  defaultMapNames <- c("map1dTrapDensity","map1dTrapDensityLog","map3dTrawlLength","map4dLineDensity",
                       "map4dLineDensityLog","map6dLineDia","map8dMeanThreat", "map8dTotalThreat",
                       "map8dTotalThreatLog","map9WhaleHabitat", "map9WhaleHabitatLog",
                       "map9dRiskScore", "map9dRiskScoreLog"  )
  scenarioMapNames <- c(    "map1sTrapDensity", "map1sTrapDensityLog","map2sTrapDensity", 
                            "map2sTrapDensityLog","map3sTrawlLength",
                            "map4sLineDensity", "map4sLineDensityLog","map6sLineDia",
                            "map8sMeanThreat","map8sTotalThreat", "map8sTotalThreatLog",
                            "map9WhaleHabitat", "map9WhaleHabitatLog","map9sRiskScore",
                            "map9sRiskScoreLog")
  
  # HomeDir="/net/work4/LobsterGroup/Management/RightWhales/DecisionSupportTool"
  # InputSpreadsheetName="CashesClosure.csv"
  # InputSpreadsheetName="ScenarioTemplate.csv"
  
  ## V1.2 added option to run in high-resolution (1Nm) rather than low-resolution (10Nm)
  ## This is only useful if one wants to apply management decisions at finer scales
  ## or expects that there is fine-scale co-occurrence patterns between whales and gear
  ## otherwise, this significantly increases model run time.

  
  # HD=("/net/work4/LobsterGroup/Management/RightWhales/DecisionSupportTool");
  # HD=here::here("")
  
  spRef_UTM_19="+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  spRef_DD="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
  
  OutputDir=gsub(".csv", "", InputSpreadsheetName); OutputDir
  
  ####################### Misc Functions ############################--
  source(paste(HD, "/FunctionsEtc/Long2Wide.R", sep=""))
  source(paste(HD, "/FunctionsEtc/Wide2Long.R", sep=""))
  source(paste(HD, "/FunctionsEtc/functionPrintTable.R", sep=""))
  
  
  Fold=TRUE ## dummy variable to allow text folding
  
  ##
  if(Fold) { ## load GIS layers and bathymetry
    ShapefileDir=paste(HD, "/InputShapefiles", sep="")
    message("Loading Shapefiles")
    spStatAreas=readOGR(dsn=ShapefileDir, 
                        layer="StatAreas_DecisionTool",
                        verbose=FALSE)
    
    # plot(spStatAreas)
    # text(
    #   getSpPPolygonsLabptSlots(spStatAreas),
    #   labels=as.character(spStatAreas$Id),
    #   cex=0.4)
    
    spLMAs=readOGR(dsn=ShapefileDir, 
                   layer="LCMAs",
                   verbose=FALSE)
    summary(spLMAs)
    # plot(spLMAs)
    proj4string(spLMAs)=CRS(spRef_DD)
    
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
    Iso100=readOGR(dsn=ShapefileDir, layer="100f_Isobath",
                   verbose=FALSE); #plot(Iso100); summary(Iso100)
    
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
    message("Loading Data")
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
    # Pts=unique(TrapMap_factor[ ,c("IecIndex_1", "x", "y")]); ## get trap map coordinates to define domain
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
    load(paste(HD, "/Inputs/TrapMapFactor_V0.1.Rdata", sep="")); 
    ### summary(TrapMap_factor)
    ## head(TrapMap_factor)
    
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
    
    ## Endlines per trawl model
    load(paste(HD, "/Inputs/EndlinesPerTrawlModel.Rdata", sep=""))
    # summary(EndlinesPerTrawl)
    
    ## Threat Model
    load(paste(HD, "/Inputs/GearThreatModel.Rdata", sep="")); 
    ### summary(ThreatMod)
    
    ## Whale Habitat Model
    load(paste(HD, "/Inputs/DukeWhaleModel_v8.Rdata", sep="")); 
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
  
  ##############################################################--
  ## 0.2 Model configurations
  
  ## read in input spreadsheet
  ScenarioInputs=read.csv(
    paste(HomeDir, 
          "InputSpreadsheets", ## subdirectory
          InputSpreadsheetName, ## file name
          sep="/"), stringsAsFactors=FALSE, na.strings=""); ScenarioInputs
  

  # ScenarioInputs$LMA[ScenarioInputs$LMA==""]=NA
  # ScenarioInputs$State[ScenarioInputs$State==""]=NA
  # ScenarioInputs$StatArea[ScenarioInputs$StatArea==""]=NA
  # ScenarioInputs$Fishery[ScenarioInputs$Fishery==""]=NA
  # ScenarioInputs$Shapefile[ScenarioInputs$Shapefile==""]=NA
  # ScenarioInputs$Months[ScenarioInputs$Months==""]=NA
  # 
  Constraints_Spatial=ScenarioInputs[ScenarioInputs$Action=="Constraint_Spatial",
                                     c("Action", "LMA", "State", "StatArea")]; Constraints_Spatial
  
  #### Note implement shapefile spatial constraint ###############--
  
  Constraints_Fishery=ScenarioInputs[ScenarioInputs$Action=="Constraint_Fishery",
                                     c("Action", "Fishery")]; Constraints_Fishery
  Closures=ScenarioInputs[
    ScenarioInputs$Action=="Closure", c("Action", "Shapefile", "Months",
                                        "Percentage", "TrapRedistributionArea", 
                                        "TrapRedistributionMethod")]; Closures
  TrapReductions=ScenarioInputs[
    ScenarioInputs$Action=="TrapReduction", 
    c("Action", "LMA", "State", "StatArea", "Shapefile", "Months","Percentage")]; TrapReductions
  
  #############################################################--
  ## 0.3 Constrain spatial extent based on user inputs
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
        if(exists("MapRef_Cr")){
          MapRef_Cr=rbind(MapRef_Cr, MapRef_CrI)
        } else {     MapRef_Cr=MapRef_CrI }
      }
      # plot(MapRef_Cr)
      TrapMap_factor=TrapMap_factor[TrapMap_factor$IecIndex_1 %in% unique(MapRef_Cr$IecIndex_1), ]
      WhalesAt1Nm=WhalesAt1Nm[WhalesAt1Nm$IecIndex_1 %in% unique(MapRef_Cr$IecIndex_1), ]
      # summary(WhalesAt1Nm)
      if(nrow(TrapMap_factor)==0){ message("Error: Spatial Constraints removed all data"); break()}
    } ##
  } ## Spatial Constraints
  
  if(Fold) {
    if(nrow(Constraints_Fishery)>0) {
      Constraints_Fishery
      TrapMap_factor=TrapMap_factor[TrapMap_factor$Fishery %in% Constraints_Fishery$Fishery, ]; dim(TrapMap_factor) ## drop all data outside constraints
      WhalesAt1Nm=WhalesAt1Nm[WhalesAt1Nm$IecIndex_1 %in% TrapMap_factor$IecIndex_1, ] ## filter whale data
    }
  } ## Fishery constraints
  
  ##########################################################--
  ## 1.0 Closures
  if(Fold) {
    message("1. Applying any closures");
    ## Traps are removed or redistributed based on the locations and timing of seasonal closures 
    ## (management option #1 above). 
    ## We would need to consider a rough approach to how to model redistributing traps. 
    ## We can start with a basic set of rules for now.
    Stage1d=TrapMap_factor
    Stage1s=TrapMap_factor
    
    if(nrow(Closures)>0){
      for(i in 1:nrow(Closures)){
        message(paste("Overlaying Closure", i))
        ClosureShape=Closures$Shapefile[i]; ClosureShape ## name of shapefile
        ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=ClosureShape, verbose=FALSE) ## load shapefile
        ShapeI$ID=1 ## create a known field 
        ShapeI=spTransform(ShapeI, proj4string(MapRef))
        ClosedPx=MapRef$IecIndex_1[!is.na(over(MapRef, ShapeI)$ID)] ## get spatial overlap of shapefile from overlay
        
        if(!is.na(Closures$Months[i])){
          ClosureMonthsI=as.numeric(strsplit(Closures$Months[i], ",")[[1]]); ClosureMonthsI ## get months
        } else {ClosureMonthsI=1:12}; ## or use all months
        if(is.na(Closures$Percentage[i])) { Multiplier=0} else { Multiplier=Closures$Percentage}; Multiplier
        Stage1s$TrapsFished[Stage1s$IecIndex_1 %in% ClosedPx &
                              Stage1s$Month %in% ClosureMonthsI]=
          Stage1s$TrapsFished[Stage1s$IecIndex_1 %in% ClosedPx &
                                Stage1s$Month %in% ClosureMonthsI] * Multiplier
      }
    }
    
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
    message("2. Applying any trap reductions")
    ## Traps are further removed due to trap reductions (management option #2). 
    ## Easiest assumption is that traps will be removed proportionally over the entire management area.
    Stage2d=Stage1d
    Stage2s=Stage1s
    
    if(nrow(TrapReductions>0)){
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
          message("Really, you're performing a seasonal trap reduction?")
          Months=as.numeric(strsplit(TrapReductions$Months[i], ",")[[1]])
        } else {Months=1:12}
        
        Stage2s$TrapsFished[Stage2s$IecIndex_1 %in% MapRef_I$IecIndex_1 &
                              Stage2s$Month %in% Months]=
          Stage2s$TrapsFished[Stage2s$IecIndex_1 %in% MapRef_I$IecIndex_1 &
                                Stage2s$Month %in% Months] * (1-(TrapReductions$Percentage[i])) ## apply reduction
      }
      aggregate(TrapsFished~StatArea, TrapMap_factor, sum)
      
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
    Stage2Output=Stage2Output[ ,c("Variable", "Scenario", "Month", "TrapsFished")]; Stage2Output
    names(Stage2Output)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage2Output); OutputData
    
  } ## fold trap reductions
  
  ###################################################################--
  ## 3.0 Convert traps to Trawls
  if(Fold) { 
    message("3 Converting Traps to Trawls")
    ## Traps are converted to distribution of trawls based on existing empirical data or models of traps / trawl 
    ## at the scale of stat areas and proposed management (#3 above).
    ## Note: Trawl Length Model may need to be modified before merging due to new regulations on min / max trawl lengths
    Stage3d=aggregate(TrapsFished~Region+VesselClass+Month+GridID, Stage2d, sum); 
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
      message("Error: Some traps not matched to trawl configurations. Error in Stage3d");
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
    
    ################## Stage 3 Scenario ############################################################--
    Stage3s=aggregate(TrapsFished~Region+VesselClass+Month+GridID, Stage2s, sum); 
    # with(Stage3, tapply(X=TrapsFished, INDEX=list(Month, Region, VesselClass), FUN=sum))
    dim(Stage3s)
    Stage3s=merge(Stage3s, 
                  TrawlLengthModel, 
                  all.x=TRUE); #summary(Stage3s)
    ## Stage3s[is.na(Stage3s$TrawlProportion), ]
    Stage3s$TrawlProportion[is.na(Stage3s$TrawlProportion) &
                              Stage3s$TrapsFished==0]=0
    Stage3s$TrapsPerTrawl[is.na(Stage3s$TrapsPerTrawl) &
                            Stage3s$TrapsFished==0]=1
    Stage3s$TrawlUnitCost[is.na(Stage3s$TrawlUnitCost) &
                            Stage3s$TrapsFished==0]=1
    if(length(which(is.na(Stage3s$TrawlProportion)))>0 | length(which(is.na(Stage3s$TrawlUnitCost)))>1){
      message("Error: Some traps not matched to trawl configurations. Error in Stage3s");
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
    message("4 Calculating Vertical Lines from Trawls")
    ## Trawls are converted to vertical lines based on trawl length. 
    ## Expected to be two vertical lines per trawl for all offshore areas 
    ## but may be different for inshore areas with shorter trawls.
    if(HighResolution){
      Stage4d=aggregate(TrawlsAtLength~IecIndex_1+GridID+Month+TrapsPerTrawl, Stage3d, sum) 
      Stage4s=aggregate(TrawlsAtLength~IecIndex_1+GridID+Month+TrapsPerTrawl, Stage3s, sum)} else {
        Stage4d=aggregate(TrawlsAtLength~GridID+Month+TrapsPerTrawl, Stage3d, sum)
        Stage4s=aggregate(TrawlsAtLength~GridID+Month+TrapsPerTrawl, Stage3s, sum); #summary(Stage4s);
      }
    
    Stage4d$TrapsPerTrawlInt=round(Stage4d$TrapsPerTrawl);
    Stage4d=merge(Stage4d, EndlinesPerTrawl, all.x=TRUE); ###summary(Stage4d) 
    
    if(length(which(is.na(Stage4d$EndlinesPerTrawl)))>0){ ## if there are any trawl lengths that don't match an endline
      message("Error: Some trawls lengths not matched to an endline.  Error in Stage4d");
      break()
    }
    
    Stage4d$NumVerticalLines=Stage4d$TrawlsAtLength * Stage4d$EndlinesPerTrawl
    ### summary(Stage4d)
    # aggregate(NumVerticalLines~Month, Stage4d, sum)

    ######################## Stage 4 Scenario #####################################################################--
    Stage4s$TrapsPerTrawlInt=round(Stage4s$TrapsPerTrawl);
    Stage4s=merge(Stage4s, EndlinesPerTrawl, all.x=TRUE); ###summary(Stage4s) 
    
    if(length(which(is.na(Stage4s$EndlinesPerTrawl)))>0){ ## if there are any trawl lengths that don't match an endline
      message("Error: Some trawls lengths not matched to an endline.  Error in Stage4s");
      break()
    }
    
    Stage4s$NumVerticalLines=Stage4s$TrawlsAtLength * Stage4s$EndlinesPerTrawl
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
    message("6 Characterizing Vertical Line Diameters")
    ## A distribution of line diameters for vertical lines is characterized 
    ## based on observed relationships with trawl length and further modified based on management options (#4 above).
    Stage6d=Stage5d; dim(Stage6d)
    Stage6d=merge(Stage6d, LineMod, all.x=TRUE); #summary(Stage6d)
    
    if(length(which(is.na(Stage6d$LineSize)))>0){ ## if there are any trawl lengths that don't match an endline
      message("Error: Some trawls lengths not matched to line diameters.  Error in Stage6d");
      break()
    }
    
    ## apportion lines across sizes
    Stage6d$NumVerticalLinesAtSize=with(Stage6d, NumVerticalLines * Prop_Line)
    #summary(Stage6d)

    ############### Stage 6 Scenario #####################################--
    Stage6s=Stage5s; dim(Stage6s)
    Stage6s=merge(Stage6s, LineMod, all.x=TRUE); #summary(Stage6s)
    
    if(length(which(is.na(Stage6s$LineSize)))>0){ ## if there are any trawl lengths that don't match an endline
      message("Error: Some trawls lengths not matched to line diameters.  Error in Stage6s");
      break()
    }
    
    ## apportion lines across sizes
    Stage6s$NumVerticalLinesAtSize=with(Stage6s, NumVerticalLines * Prop_Line)
    #summary(Stage6s)
    
    if(PrintDefaultMaps){
      ############--
      Tmp=Stage6d[ ,c("Month", "GridID", "LineSize", "NumVerticalLinesAtSize")];
      Tmp2=aggregate(NumVerticalLinesAtSize~Month+GridID, Tmp, sum); ###summary(Tmp2)
      names(Tmp2)=c("Month", "GridID", "Totals")
      # plot(quantile(Tmp2$Totals, (0:100)/100), ylim=c(0,1));
      Tmp2$Totals[Tmp2$Totals==0]=0.01
      Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
      Tmp$LineDia=with(Tmp, LineSize * NumVerticalLinesAtSize/Totals);
      
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
    
    if(PrintScenarioMaps){
      ############--
      Tmp=Stage6s[ ,c("Month", "GridID", "LineSize", "NumVerticalLinesAtSize")];
      Tmp2=aggregate(NumVerticalLinesAtSize~Month+GridID, Tmp, sum); ###summary(Tmp2)
      names(Tmp2)=c("Month", "GridID", "Totals")
      ###plot(quantile(Tmp2$Totals, (0:100)/100), ylim=c(0,1));
      Tmp2$Totals[Tmp2$Totals==0]=0.01
      Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
      Tmp$LineDia=with(Tmp, LineSize * NumVerticalLinesAtSize/Totals);
      
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
  message("7 Applying any management measures to line diameters")
  if(HighResolution){
    Stage7d=aggregate(NumVerticalLinesAtSize~IecIndex_1+GridID+Month+LineSize, Stage6d, sum); 
    Stage7s=aggregate(NumVerticalLinesAtSize~IecIndex_1+GridID+Month+LineSize, Stage6s, sum); 
  } else {
    Stage7d=aggregate(NumVerticalLinesAtSize~GridID+Month+LineSize, Stage6d, sum); 
    Stage7s=aggregate(NumVerticalLinesAtSize~GridID+Month+LineSize, Stage6s, sum); 
  } ## 
  
  #################################################################--
  ## 8.0 Merge line diameters with Threat
  if(Fold) {
    message("8 Calculating gear configuration threat")
    ## Line diameters are converted to Threat based on a model to be developed, possibly by polling the TRT.
    Stage8d=Stage7d;
    Stage8d=merge(Stage8d, ThreatMod, all.x=TRUE)
    ### summary(Stage8d)
    
    if(length(which(is.na(Stage8d$Threat)))>0){ ## if there are any trawl lengths that don't match an endline
      message("Error: Some gear configurations not matched to a threat score.  Error in Stage8d");
      break()
    }
    
    ##move down to 9
    Stage8d$ThreatScore=with(Stage8d, NumVerticalLinesAtSize * Threat); 
    ### summary(Stage8d)
    
    ########################## Stage 8 Scenario ##################################--
    ## Line diameters are converted to Threat based on a model to be developed, possibly by polling the TRT.
    Stage8s=Stage7s;
    Stage8s=merge(Stage8s, ThreatMod, all.x=TRUE)
    ### summary(Stage8s)
    
    if(length(which(is.na(Stage8s$Threat)))>0){ ## if there are any trawl lengths that don't match an endline
      message("Error: Some gear configurations not matched to a threat score.  Error in Stage8s");
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
    
    #################################################################--
    # aggregate(ThreatScore~GridID+Month, Stage8d, length)
    # 
    # Stage8dOutput=aggregate(ThreatScore~Month, Stage8d, length); Stage8dOutput
    # Stage8dTotal=aggregate(NumVerticalLines~1, Stage8dOutput, sum); Stage8dTotal
    # Stage8dTotal$Month="Total"
    # Stage8dOutput=rbind(Stage8dOutput, Stage8dTotal); Stage8dOutput
    # Stage8dOutput$Scenario="Default"
    # 
    # Stage8sOutput=aggregate(NumVerticalLines~Month, Stage8s, sum); Stage8sOutput
    # Stage8sTotal=aggregate(NumVerticalLines~1, Stage8sOutput, sum); Stage8sTotal
    # Stage8sTotal$Month="Total"
    # Stage8sOutput=rbind(Stage8sOutput, Stage8sTotal); Stage8sOutput
    # Stage8sOutput$Scenario="Scenario"
    # 
    # Stage8Output=rbind(Stage8dOutput, Stage8sOutput); Stage8Output
    # Stage8Output$Variable="VerticalLines"
    # Stage8Output=Stage8Output[ ,c("Variable", "Scenario", "Month", "NumVerticalLines")]; Stage8Output
    # names(Stage8Output)=c("Variable", "Scenario", "Month", "Value")
    # 
    # OutputData=rbind(OutputData, Stage8Output); OutputData
    
  } ## Threat
  
  #############################################################--
  ## 9. Calculate Risk
  if(Fold) {
    message("9 Calculating composite risk values")
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
  
  message("Writing output")
  
  if(PrintDefaultMaps){
    message("Writing Default Maps")
    
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
    for (iplot in 1:length(defaultMapNames)){
      png(here::here(paste0("Scenarios/",OutputDir,"/",defaultMapNames[iplot],".png")),width=1000,height=1000,units="px")
      eval(parse(text=paste0("plot(",defaultMapNames[iplot],")")))
      dev.off()
    }
  } ## print default maps
  
  if(PrintScenarioMaps){
    message("Writing Scenario Maps")
    
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
    for (iplot in 1:length(scenarioMapNames)){
      png(here::here(paste0("Scenarios/",OutputDir,"/",scenarioMapNames[iplot],".png")),width=1000,height=1000,units="px")
      eval(parse(text=paste0("plot(",scenarioMapNames[iplot],")")))
      dev.off()
    }
    
  } ## print scenario maps
  
  if(PrintTables){
    message("Writing Tables")
    
    pdf(file=paste(OutputDir, "_Tables.pdf", sep=""),
        width=11, height=8.5, paper="special")
    
    grid.draw(PrintTable(Tbl=ScenarioInputs,
                         Title="Trap Numbers",
                         TitleFont=14))
    grid.newpage()
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
  
  if(WriteMapSources){
    message("Writing Map Sources")
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
    message("Writing Output to .csv")
    
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