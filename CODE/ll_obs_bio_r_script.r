

# this query was initially C:/Data/SHK_SQL_SVR/ll_obs_bio.sql
#/*
# * Execute with:
# * sqlcmd -S nousql02 -d TUBS_MASTER_ENTRY -E -i ll_obs_bio.sql -oc:\temp\ll_obs_bio.csv -s"," -W
# */
# 
#;


require(RODBC)
require(R4MFCL)
 
channel_obs <- odbcDriverConnect("driver={SQL Server};Server=nousql02;Database=tubs_master_entry;Trusted_Connection=yes;")
# sqlTables(channel_obs,schema = "obsv")


ll_bio <- sqlQuery(channel_obs,"
SELECT
     T.obstrip_id
    ,SHL.l_setlog_id AS l_shaul_id
    ,SC.l_setcatch_id AS l_cmon_id
    ,T.obsprg_code AS obs_prg_id
    ,V.vessel_name AS vesselname
    ,T.gear_code AS gr_id
    ,V.reg_country_code AS flag_id
    ,T.staff_code AS obsv_id
    ,T.tripno
    ,convert(varchar(10),T.dep_date,120) as o_dep_date
    ,convert(varchar(10),T.ret_date,120) as o_ret_date
    ,convert(varchar(10),LS.set_date,120) as setdate
    ,LS.hk_bt_flt
    ,LS.hook_observed as hook
    ,LS.hook_set
    ,LS.hook_est
    ,LS.hook_calc
    ,LS.bask_set as bask
	,CASE
        WHEN LS.target_skh_yn = 1 AND LS.target_tun_yn = 1 THEN 4
        WHEN LS.target_tun_yn = 1 THEN 1
        WHEN LS.target_swo_yn = 1 THEN 2
        WHEN LS.target_skh_yn = 1 THEN 3
        ELSE NULL
     END as target_sp_id
	,LS.tar_sp_code as tar_sp_id
    ,T.sharktarget
    ,SHL.lon AS lon_long
    ,SHL.lat AS lat_long
    ,SHL.eez_code AS ez_id
    ,[obsv].[Lond1](SHL.lond) as lon1
    ,[obsv].[Latd1](SHL.latd) as lat1
    ,[obsv].[LonD5](SHL.lond) as lon5
    ,[obsv].[Latd5](SHL.latd) as lat5
    ,DATEPART(YEAR,LS.set_date) as yy
    ,DATEPART(MONTH,LS.set_date) as mm
    ,DATEPART(DAY,LS.set_date) as [day]
    ,DATEPART(QUARTER,LS.set_date) as [quarter]
    ,SC.sp_code AS sp_id
    ,SC.hook_no
    ,SC.catch_time AS ctime
    ,SC.[len] AS [len]
    ,SC.len_code AS len_id
    ,SC.fate_code AS fate_id
    ,SC.cond_code AS cond_id
    ,SC.cond_code2 AS cond_2_id
    ,SC.wt_est
    ,SC.sex_code AS sex_id
    ,SP.sp_name
    ,SP.sp_cat_code AS sp_cat_id
    ,SP.sp_sci_name
    ,SP.sci_category
FROM
    obsv.trip T INNER JOIN obsv.l_set LS ON T.obstrip_id = LS.obstrip_id
                INNER JOIN ref.vessels V ON T.vessel_id = V.vessel_id
                INNER JOIN obsv.l_sethaullog SHL ON LS.l_set_id = SHL.l_set_id
                INNER JOIN obsv.l_setcatch SC ON SC.l_set_id = LS.l_set_id
                INNER JOIN ref.species SP ON SP.sp_code = SC.sp_code
WHERE
    SHL.stend_id = 83
    AND SP.sp_cat_code = 'SHK'
    order by T.obstrip_id
    ,SHL.l_setlog_id 
    ,SC.l_setcatch_id  ")
    
    
    
    summary(ll_bio)
      str(ll_bio)
    ll_bio[508900:508910,]
    
       length(ll_bio$l_cmon_id)
       length( unique(ll_bio$l_cmon_id))
    