# Load packages
using Omniscape, Rasters, Plots

# Loop for each species
for sdm in ["SHMA_RF_nil1400_1ha","SHAL_RF_pa1400_1ha","MOCA_RF_nil1400_1ha","MOFA_RF_pa1400_1ha","ANNI_RF_nilpa1400_25ha","EUAL_RF_nilpa1400_1ha","FINI_RF_nilpa1400_1ha"]
     
    # Loop for each resistance layer
    for constant in ["c0.25","c2", "c8"]

        # Loop for range of species-specific dispersal distance values
        if sdm == "ANNI_RF_nilpa1400_25ha" 
            dist = ["1km","1.5km","3km"]
        elseif sdm == "EUAL_RF_nilpa1400_1ha"
            dist = ["0.5km","1km","1.5km"]
        else
            dist = ["0.2km","0.5km","1km"]
        end
        
        for radius in dist

            # Loop for each scenario: past and present
            for year in ["1995","2017"]

                # Convert dispersal distance to pixel values
                if sdm == "ANNI_RF_nilpa1400_25ha"
                    if radius == "1km"
                        radius_pixels = string(2)
                    elseif radius == "1.5km"
                        radius_pixels = string(3)
                    else radius == "3km"
                        radius_pixels = string(6)
                    end
                else
                    if radius == "0.2km"
                        radius_pixels = string(2)
                    elseif radius == "0.5km"
                        radius_pixels = string(5)
                    elseif radius == "1km"
                        radius_pixels = string(10)
                    else radius == "1.5km"
                        radius_pixels = string(15)
                    end
                end                    

                # Read resistance layer
                res, wkt, transform = Omniscape.read_raster("Input/" * year * "/Resistance layers/" * sdm * "_" * constant * ".tif", Float64)

                # Set model parameters
                config = Dict{String, String} (
                    "radius" => radius_pixels,
                    "project_name" => "Output/" * year * "/" * sdm * "_" * constant * "/" * radius,
                    "source_from_resistance" => "false",
                    "source_file" => "Input/" * year * "/Source layers/" * sdm * ".tif", 
                    "calc_normalized_current" => "true",
                    "calc_flow_potential" => "true",
                    "write_raw_currmap" => "true",
                    "mask_nodata" => "true",
                    "write_as_tif" => "true")

                # Run model and write output maps to TIF files
                currmap, flow_pot, norm_current = run_omniscape(config,
                                                                res,
                                                                wkt = wkt,
                                                                geotransform = transform,
                                                                write_outputs = true)
            end
        end
    end
end