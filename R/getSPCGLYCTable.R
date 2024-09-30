
getSPCGLYCTable <- function() {

  data.frame(Name = c('SPC_All','SPC3','SPC2','SPC1','Glyc_All',
                      'GlycA','GlycB','SPC3_2','SPC_Glyc'),
             Unit = "a.u",
             `Max Value (ref.)` = NA,
             `Min Value (ref.)` = NA,
             `Reference Unit` = "a.u",
             `Reference Range [Unit]` = NA,
             Description = c("Integral from 3.18 to 3.32 ppm",
                             "Integral from 3.262 to 3.3 ppm",
                             "Integral from 3.236 to 3.262 ppm",
                             "Integral from 3.2 to 3.236 ppm",
                             "Integral from 2.050 to 2.118 ppm",
                             "Integral from 2.050 to 2.089 ppm",
                             "Integral from 2.089 to 2.118 ppm",
                             "Ratio of SPC3 / SPC2",
                             "Ratio of SPC_All / Glyc_All"),
             check.names = FALSE)
}


