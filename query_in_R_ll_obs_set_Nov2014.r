
# this query was initially C:/Data/SHK_SQL_SVR/ll_obs_set.sql
#/*
# * Execute with:
# * sqlcmd -S nousql02 -d TUBS_MASTER_ENTRY -E -i ll_obs_set.sql -oc:\temp\ll_obs_set.csv -s"," -W
# */
# 
#;


require(RODBC)
require(R4MFCL)
 
channel_obs <- odbcDriverConnect("driver={SQL Server};Server=nousql02;Database=tubs_master;Trusted_Connection=yes;")
# sqlTables(channel_obs,schema = "obsv")


ll_set <- sqlQuery(channel_obs,"WITH Species_Count_CTE (
	[l_set_id], 
	[DGX],
	[DUS],
	[WSH],
	[CCP],
	[BRO],                               
	[PTH],
	[ALV],
	[AML],
	[CCE],
	[TIG],
	[LMA],
	[STT],
	[BTH],
	[SPN],
	[BSH],
	[THR],
	[CCL],
	[SMA],
	[RMB],
	[MAK],
	[PLS],
	[OCS],
	[MAN],
	[FAL],
	[EAG],
	[GAG],
	[GUQ],
	[HXT],
	[ISB],
	[NTC],
	[POR],
	[RAJ],
	[SCK],
	[SHF],
	[SHK],
	[SHL],
	[PSK],
	[CYW],
	[STI],
	[ALS],
	[CZI],
	[SSQ],
	[CCG],
	[SPL],
	[BLR],
	[BAI],
	[SPZ],
	[SPK],
	[WST],
	[SRX],
	[CVX],
	[DGS],
	[LMD],
	[TRB],
	[RMJ],
	[BSK],
	[RMT],
	[CYU],
	[CYO],
	[SKH],
	[CCA],
	[HDQ],
	[TOE],
	[RMV],
	[YSM],
	[CCU],
	[GTF],
	[CYP],
	[OSF],
	[RDR],
	[RHN],
	[CNX],
	[YSA],
	[RSK],
	[ODH],
	[EUB],
	[CPS],
	[SYR],
	[SKX],
	[RUZ],
	[DOP],
	[DCA],
	[CCB])
AS
(
	SELECT 
		l_set_id 
		,COALESCE([DGX],0) AS [DGX]
		,COALESCE([DUS],0) AS [DUS]
		,COALESCE([WSH],0) AS [WSH]
		,COALESCE([CCP],0) AS [CCP]
		,COALESCE([BRO],0) AS [BRO]
		,COALESCE([PTH],0) AS [PTH]
		,COALESCE([ALV],0) AS [ALV]
		,COALESCE([AML],0) AS [AML]
		,COALESCE([CCE],0) AS [CCE]
		,COALESCE([TIG],0) AS [TIG]
		,COALESCE([LMA],0) AS [LMA]
		,COALESCE([STT],0) AS [STT]
		,COALESCE([BTH],0) AS [BTH]
		,COALESCE([SPN],0) AS [SPN]
		,COALESCE([BSH],0) AS [BSH]
		,COALESCE([THR],0) AS [THR]
		,COALESCE([CCL],0) AS [CCL]
		,COALESCE([SMA],0) AS [SMA]
		,COALESCE([RMB],0) AS [RMB]
		,COALESCE([MAK],0) AS [MAK]
		,COALESCE([PLS],0) AS [PLS]
		,COALESCE([OCS],0) AS [OCS]
		,COALESCE([MAN],0) AS [MAN]
		,COALESCE([FAL],0) AS [FAL]
		,COALESCE([EAG],0) AS [EAG]
		,COALESCE([GAG],0) AS [GAG]
		,COALESCE([GUQ],0) AS [GUQ]
		,COALESCE([HXT],0) AS [HXT]
		,COALESCE([ISB],0) AS [ISB]
		,COALESCE([NTC],0) AS [NTC]
		,COALESCE([POR],0) AS [POR]
		,COALESCE([RAJ],0) AS [RAJ]
		,COALESCE([SCK],0) AS [SCK]
		,COALESCE([SHF],0) AS [SHF]
		,COALESCE([SHK],0) AS [SHK]
		,COALESCE([SHL],0) AS [SHL]
		,COALESCE([PSK],0) AS [PSK]
		,COALESCE([CYW],0) AS [CYW]
		,COALESCE([STI],0) AS [STI]
		,COALESCE([ALS],0) AS [ALS]
		,COALESCE([CZI],0) AS [CZI]
		,COALESCE([SSQ],0) AS [SSQ]
		,COALESCE([CCG],0) AS [CCG]
		,COALESCE([SPL],0) AS [SPL]
		,COALESCE([BLR],0) AS [BLR]
		,COALESCE([BAI],0) AS [BAI]
		,COALESCE([SPZ],0) AS [SPZ]
		,COALESCE([SPK],0) AS [SPK]
		,COALESCE([WST],0) AS [WST]
		,COALESCE([SRX],0) AS [SRX]
		,COALESCE([CVX],0) AS [CVX]
		,COALESCE([DGS],0) AS [DGS]
		,COALESCE([LMD],0) AS [LMD]
		,COALESCE([TRB],0) AS [TRB]
		,COALESCE([RMJ],0) AS [RMJ]
		,COALESCE([BSK],0) AS [BSK]
		,COALESCE([RMT],0) AS [RMT]
		,COALESCE([CYU],0) AS [CYU]
		,COALESCE([CYO],0) AS [CYO]
		,COALESCE([SKH],0) AS [SKH]
		,COALESCE([CCA],0) AS [CCA]
		,COALESCE([HDQ],0) AS [HDQ]
		,COALESCE([TOE],0) AS [TOE]
		,COALESCE([RMV],0) AS [RMV]
		,COALESCE([YSM],0) AS [YSM]
		,COALESCE([CCU],0) AS [CCU]
		,COALESCE([GTF],0) AS [GTF]
		,COALESCE([CYP],0) AS [CYP]
		,COALESCE([OSF],0) AS [OSF]
		,COALESCE([RDR],0) AS [RDR]
		,COALESCE([RHN],0) AS [RHN]
		,COALESCE([CNX],0) AS [CNX]
		,COALESCE([YSA],0) AS [YSA]
		,COALESCE([RSK],0) AS [RSK]
		,COALESCE([ODH],0) AS [ODH]
		,COALESCE([EUB],0) AS [EUB]
		,COALESCE([CPS],0) AS [CPS]
		,COALESCE([SYR],0) AS [SYR]
		,COALESCE([SKX],0) AS [SKX]
		,COALESCE([RUZ],0) AS [RUZ]
		,COALESCE([DOP],0) AS [DOP]
		,COALESCE([DCA],0) AS [DCA]
		,COALESCE([CCB],0) AS [CCB]
	FROM
	(
		SELECT l_set_id, 1 as fish_count, sp_code FROM  [obsv].[l_setcatch]
	) SC
	PIVOT (
	    /* Use PIVOT to convert rows to columns */
		SUM(fish_count) FOR sp_code in (
		DGX,
		DUS,
		WSH,
		CCP,
		BRO,
		PTH,
		ALV,
		AML,
		CCE,
		TIG,
		LMA,
		STT,
		BTH,
		SPN,
		BSH,
		THR,
		CCL,
		SMA,
		RMB,
		MAK,
		PLS,
		OCS,
		MAN,
		FAL,
		EAG,
		GAG,
		GUQ,
		HXT,
		ISB,
		NTC,
		POR,
		RAJ,
		SCK,
		SHF,
		SHK,
		SHL,
		PSK,
		CYW,
		STI,
		ALS,
		CZI,
		SSQ,
		CCG,
		SPL,
		BLR,
		BAI,
		SPZ,
		SPK,
		WST,
		SRX,
		CVX,
		DGS,
		LMD,
		TRB,
		RMJ,
		BSK,
		RMT,
		CYU,
		CYO,
		SKH,
		CCA,
		HDQ,
		TOE,
		RMV,
		YSM,
		CCU,
		GTF,
		CYP,
		OSF,
		RDR,
		RHN,
		CNX,
		YSA,
		RSK,
		ODH,
		EUB,
		CPS,
		SYR,
		SKX,
		RUZ,
		DOP,
		DCA,
		CCB)
	) as pvt
)
SELECT
     T.obstrip_id
    ,SHL.l_set_id as l_shaul_id
    ,convert(varchar(10),LS.set_date,120) as setdate
    ,LS.set_time as settime
    ,LS.l_set_id as set_id
    ,LS.tar_sp_code as tar_sp_id
    ,LS.hk_bt_flt
    ,LS.no_obsv_yn as no_obsv
    ,LS.hook_observed as hook
    ,LS.hook_set
    ,LS.hook_est
    ,LS.hook_calc
    ,LS.bask_set as bask
    ,LS.bask_observed as baskobs
    ,LS.branch_length as branch
    ,LS.float_length as float
    ,LS.bait1_sp_code as bait1_sp_id
    ,LS.bait2_sp_code as bait2_sp_id
    ,LS.bait3_sp_code as bait3_sp_id
    ,LS.bait4_sp_code as bait4_sp_id
    ,LS.bait5_sp_code as bait5_sp_id
    ,LS.lightsticks
    ,LS.hook99_n as nbshark_lines
    ,CASE
        WHEN LS.target_skh_yn = 1 AND LS.target_tun_yn = 1 THEN 4
        WHEN LS.target_tun_yn = 1 THEN 1
        WHEN LS.target_swo_yn = 1 THEN 2
        WHEN LS.target_skh_yn = 1 THEN 3        
        ELSE NULL
     END as target_sp_id
    ,LS.tdr_len
    ,V.vessel_name as vesselname
    ,T.gear_code as gr_id
    ,v.reg_country_code as flag_id
    ,T.natfleet_id as fleet_id
    ,T.staff_code as obsv_id
    ,T.tripno
	,T.obsprg_code as program_code
    ,convert(varchar(10),T.dep_date,120) as o_dep_date
    ,convert(varchar(10),T.ret_date,120) as o_ret_date
    ,NULL as shark
    ,T.sharktarget
    ,SHL.lond
    ,SHL.latd
    ,SHL.eez_code as ez_id
    ,[obsv].[Lond1](SHL.lond) as lon1
    ,[obsv].[Latd1](SHL.latd) as lat1
    ,[obsv].[LonD5](SHL.lond) as lon5
    ,[obsv].[Latd5](SHL.latd) as lat5
    ,DATEPART(YEAR,LS.set_date) as yy
    ,DATEPART(MONTH,LS.set_date) as mm
    ,DATEPART(DAY,LS.set_date) as [day]
    ,DATEPART(QUARTER,LS.set_date) as [quarter]
    ,SCC.DGX
	,SCC.DUS
	,SCC.WSH
	,SCC.CCP
	,SCC.BRO
	,SCC.PTH
	,SCC.ALV
	,SCC.AML
	,SCC.CCE
	,SCC.TIG
	,SCC.LMA
	,SCC.STT
	,SCC.BTH
	,SCC.SPN
	,SCC.BSH
	,SCC.THR
	,SCC.CCL
	,SCC.SMA
	,SCC.RMB
	,SCC.MAK
	,SCC.PLS
	,SCC.OCS
	,SCC.MAN
	,SCC.FAL
	,SCC.EAG
	,SCC.GAG
	,SCC.GUQ
	,SCC.HXT
	,SCC.ISB
	,SCC.NTC
	,SCC.POR
	,SCC.RAJ
	,SCC.SCK
	,SCC.SHF
	,SCC.SHK
	,SCC.SHL
	,SCC.PSK
	,SCC.CYW
	,SCC.STI
	,SCC.ALS
	,SCC.CZI
	,SCC.SSQ
	,SCC.CCG
	,SCC.SPL
	,SCC.BLR
	,SCC.BAI
	,SCC.SPZ
	,SCC.SPK
	,SCC.WST
	,SCC.SRX
	,SCC.CVX
	,SCC.DGS
	,SCC.LMD
	,SCC.TRB
	,SCC.RMJ
	,SCC.BSK
	,SCC.RMT
	,SCC.CYU
	,SCC.CYO
	,SCC.SKH
	,SCC.CCA
	,SCC.HDQ
	,SCC.TOE
	,SCC.RMV
	,SCC.YSM
	,SCC.CCU
	,SCC.GTF
	,SCC.CYP
	,SCC.OSF
	,SCC.RDR
	,SCC.RHN
	,SCC.CNX
	,SCC.YSA
	,SCC.RSK
	,SCC.ODH
	,SCC.EUB
	,SCC.CPS
	,SCC.SYR
	,SCC.SKX
	,SCC.RUZ
	,SCC.DOP
	,SCC.DCA
	,SCC.CCB
FROM
    obsv.trip T INNER JOIN obsv.l_set LS ON T.obstrip_id = LS.obstrip_id
                INNER JOIN ref.vessels V ON T.vessel_id = V.vessel_id
                INNER JOIN obsv.l_sethaullog SHL ON LS.l_set_id = SHL.l_set_id
                INNER JOIN Species_Count_CTE SCC ON SCC.l_set_id = LS.l_set_id
WHERE
    SHL.stend_id = 83
	AND LS.set_date > '1989-12-31'
	")
	
	  head(ll_set)
	
 save(ll_set, file=paste("ll_obs_set",format(Sys.time(), "%d%b%Y" ), ".RData", sep='') )

# so I couldn't get these to work, so no send of set..	
#--    ,max(case SHL.stend_id when 83 then SHL.log_dtime else 0 end) as start_set 
#--    ,max(case SHL.stend_id when 84 then SHL.log_dtime else 0 end) as end_set 
#--		,max(case SHL.stend_id when 85 then SHL.log_dtime else 0 end) as start_haul 