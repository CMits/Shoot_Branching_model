# defining node equations
nextStep <- function(dat, gen, delay = NA) {
	dat[nrow(dat) + 1, ] = NA
	t = nrow(dat)
	delay = nrow(dat) - 1

	dat$SL[t] = ((2/(1 + dat$CXE[t-1])) * (gen$SL_n)) * (dat$LBO[t-1])
	dat$BRC1_2[t] = (2 * ((dat$PIFs[t-1] + dat$SPL9_15[t-1])/2)/(1 + (dat$CK[t-1] + dat$SUC[t-1] + dat$PhyB[t-1])/3)) * (gen$BRC1_2_n)
	dat$CK[t] = (2 * (dat$SUC[t-1])/(1 + (dat$Aux_Shoot[t-1] + dat$Perception_SL[t-1])/2)) * (gen$CK_n)
	dat$Aux_Shoot[t] = (2/(1 + dat$Decap_signal[t-1])) * (gen$Aux_n)
	dat$ABA[t] = (dat$NCED3[t-1]) * (gen$ABA_n)
	dat$Bud_release[t] = 2 * ((dat$SUC[t-1] + dat$CK[t-1])/2)/(1 + (dat$ABA[t-1] + dat$BRC1_2[t-1])/2)
	dat$PAT_Bud[t] = (2 * ((dat$Aux_Bud[t-1] + dat$CK[t-1] + dat$Decap_signal[t-1])/3)/(1 + dat$Aux_Shoot[t-1])) * (gen$PIN_1_3_4_7_n)
	dat$Aux_Bud[t] = (1) * (gen$Aux_Bud_n)
	dat$SUC[t] = (dat$Decap_signal[t-1]) * (gen$SUC_n)
	dat$Sustained_growth[t] = (2 * (min(dat$GA[t-1], dat$PAT_Bud[t-1]))/(1 + dat$SUC[t-1])) * (dat$Bud_release[t-1])
	dat$GA[t] = (dat$Aux_Bud[t-1]) * (gen$GA_n)
	dat$Decap_signal[t] = (1) * (gen$Decap_signal_n)
	dat$CXE[t] = (1) * (gen$CXE_n)
	dat$SMXL6_7_8[t] = (2 * ((dat$SMXL6_7_8_Transcription[t-1] + dat$FHY3_FAR1[t-1])/2)/(1 + dat$Perception_SL[t-1])) * (gen$SMXL6_7_8_n)
	dat$FHY3_FAR1[t] = (2 * (dat$High_R_FR[t-1])/(1 + dat$Low_R_FR[t-1])) * (gen$FHY3_FAR1_n)
	dat$SPL9_15[t] = (2/(1 + (dat$FHY3_FAR1[t-1] + dat$SMXL6_7_8[t-1])/2)) * (gen$SPL9_15_n)
	dat$SMXL6_7_8_Transcription[t] = 2/(1 + dat$SMXL6_7_8[t-1])
	dat$Low_R_FR[t] = 1
	dat$MAX2[t] = (2/(1 + (dat$SUC[t-1] + dat$PhyB[t-1])/2)) * (gen$MAX2_n)
	dat$D14[t] = (1) * (gen$D14_n)
	dat$Perception_SL[t] = (1) * ( min(dat$D14[t-1], dat$MAX2[t-1], dat$SL[t-1]))
	dat$D27[t] = (1) * (gen$D27_n)
	dat$MAX3[t] = ((2 * (dat$Aux_Shoot[t-1])/(1 + dat$Perception_SL[t-1])) * (gen$MAX3_n)) * (dat$D27[t-1])
	dat$MAX4[t] = ((2 * (dat$Aux_Shoot[t-1])/(1 + (dat$Perception_SL[t-1] + dat$PhyB[t-1])/2)) * (gen$MAX4_n)) * (dat$MAX3[t-1])
	dat$MAX1[t] = ((2/(1 + dat$Perception_SL[t-1])) * (gen$MAX1_n)) * (dat$MAX4[t-1])
	dat$LBO[t] = ((1) * (gen$LBO_n)) * (dat$CLAMT[t-1])
	dat$CLAMT[t] = ((1) * (gen$CLAMT_n)) * (dat$MAX1[t-1])
	dat$PhyB[t] = (2 * (dat$High_R_FR[t-1])/(1 + dat$Low_R_FR[t-1])) * (gen$PhyB_n)
	dat$High_R_FR[t] = 1
	dat$PIFs[t] = (2/(1 + dat$High_R_FR[t-1])) * (gen$PIFs_n)
	dat$HB21_40_53[t] = (min(dat$BRC1_2[t-1], dat$Low_R_FR[t-1])) * (gen$HB21_40_53_n)
	dat$NCED3[t] = (dat$HB21_40_53[t-1]) * (gen$NCED3_n)
	dat[t, ]
}