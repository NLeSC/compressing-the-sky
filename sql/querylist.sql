/**
 * This a list of the most important queries that are generated by django and banana
 */

-- Q1: select light-curve datapoints
SELECT "assocxtrsource"."id"
      ,"assocxtrsource"."runcat"
      ,"assocxtrsource"."xtrsrc"
      ,"assocxtrsource"."type"
      ,"assocxtrsource"."distance_arcsec"
      ,"assocxtrsource"."r"
      ,"assocxtrsource"."loglr"
      ,"assocxtrsource"."v_int"
      ,"assocxtrsource"."eta_int"
      ,"assocxtrsource"."f_datapoints"
      ,"extractedsource"."id"
      ,"extractedsource"."image"
      ,"extractedsource"."zone"
      ,"extractedsource"."ra"
      ,"extractedsource"."decl"
      ,"extractedsource"."uncertainty_ew"
      ,"extractedsource"."uncertainty_ns"
      ,"extractedsource"."ra_err"
      ,"extractedsource"."decl_err"
      ,"extractedsource"."ra_fit_err"
      ,"extractedsource"."decl_fit_err"
      ,"extractedsource"."ew_sys_err"
      ,"extractedsource"."ns_sys_err"
      ,"extractedsource"."error_radius"
      ,"extractedsource"."x"
      ,"extractedsource"."y"
      ,"extractedsource"."z"
      ,"extractedsource"."racosdecl"
      ,"extractedsource"."margin"
      ,"extractedsource"."det_sigma"
      ,"extractedsource"."semimajor"
      ,"extractedsource"."semiminor"
      ,"extractedsource"."pa"
      ,"extractedsource"."f_peak"
      ,"extractedsource"."f_peak_err"
      ,"extractedsource"."f_int"
      ,"extractedsource"."f_int_err"
      ,"extractedsource"."chisq"
      ,"extractedsource"."reduced_chisq"
      ,"extractedsource"."extract_type"
      ,"extractedsource"."fit_type"
      ,"extractedsource"."ff_runcat"
      ,"extractedsource"."node"
      ,"extractedsource"."nodes"
      ,"image"."id"
      ,"image"."dataset"
      ,"image"."tau"
      ,"image"."band"
      ,"image"."stokes"
      ,"image"."tau_time"
      ,"image"."freq_eff"
      ,"image"."freq_bw"
      ,"image"."taustart_ts"
      ,"image"."skyrgn"
      ,"image"."rb_smaj"
      ,"image"."rb_smin"
      ,"image"."rb_pa"
      ,"image"."deltax"
      ,"image"."deltay"
      ,"image"."fwhm_arcsec"
      ,"image"."fov_degrees"
      ,"image"."rms_qc"
      ,"image"."rms_min"
      ,"image"."rms_max"
      ,"image"."detection_thresh"
      ,"image"."analysis_thresh"
      ,"image"."url"
      ,"image"."node"
      ,"image"."nodes"
      ,"frequencyband"."id"
      ,"frequencyband"."freq_central"
      ,"frequencyband"."freq_low"
      ,"frequencyband"."freq_high" 
  FROM "assocxtrsource" 
       LEFT OUTER JOIN "extractedsource" ON ( "assocxtrsource"."xtrsrc" = "extractedsource"."id" ) 
       LEFT OUTER JOIN "image" ON ( "extractedsource"."image" = "image"."id" ) 
       LEFT OUTER JOIN "frequencyband" ON ( "image"."band" = "frequencyband"."id" ) 
 WHERE "assocxtrsource"."runcat" = 7 
ORDER BY "image"."taustart_ts" ASC
;


--Q2: Get the properties of a single datapoint
SELECT "extractedsource"."id"
      ,"extractedsource"."image"
      ,"extractedsource"."zone"
      ,"extractedsource"."ra"
      ,"extractedsource"."decl"
      ,"extractedsource"."uncertainty_ew"
      ,"extractedsource"."uncertainty_ns"
      ,"extractedsource"."ra_err"
      ,"extractedsource"."decl_err"
      ,"extractedsource"."ra_fit_err"
      ,"extractedsource"."decl_fit_err"
      ,"extractedsource"."ew_sys_err"
      ,"extractedsource"."ns_sys_err"
      ,"extractedsource"."error_radius"
      ,"extractedsource"."x"
      ,"extractedsource"."y"
      ,"extractedsource"."z"
      ,"extractedsource"."racosdecl"
      ,"extractedsource"."margin"
      ,"extractedsource"."det_sigma"
      ,"extractedsource"."semimajor"
      ,"extractedsource"."semiminor"
      ,"extractedsource"."pa"
      ,"extractedsource"."f_peak"
      ,"extractedsource"."f_peak_err"
      ,"extractedsource"."f_int"
      ,"extractedsource"."f_int_err"
      ,"extractedsource"."chisq"
      ,"extractedsource"."reduced_chisq"
      ,"extractedsource"."extract_type"
      ,"extractedsource"."fit_type"
      ,"extractedsource"."ff_runcat"
      ,"extractedsource"."node"
      ,"extractedsource"."nodes" 
  FROM "extractedsource" 
 WHERE "extractedsource"."id" = 76
;
