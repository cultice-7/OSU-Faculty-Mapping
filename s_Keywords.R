################################################################################
### Keywords
################################################################################

CRA.v <- c("Transportation",
           "Buildings",
           "Electricity Distribution",
           "Industrial/Manufacturing",
           "Solar",
           "Wind",
           "Geothermal",
           "Hydrogen",
           "Advanced Nuclear",
           "Bioenergy",
           "Coupled Carbon Capture",
           "Direct Air Capture",
           "Geoengineering",
           "Land-Based Sequestration")
CRA.v2 <- c(rep("Efficiency, Electrification, and Sector Decarbonization",
                times = 4),
            rep("Clean Energy Sources",
                times = 6),
            rep("Direct CO2 Management",
                times = 4)
            )
CRA.v3 <- c(rep("Mitigation",
                times = 14))

CRA.dt <- data.table("ClimateResearchAreas" = CRA.v,
                     "Category"             = CRA.v2,
                     "Strategy"             = CRA.v3)

keywords1 <- c("electric vehicle", "transit", "cars ", " bus ", " train", " rail")
keywords4 <- c("solar", "photovoltaic", " pv ", "silicon cell",
               "thinfilm cell")
keywords14 <- c("co2 reduction", "")
