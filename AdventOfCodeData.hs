module AdventOfCodeData where

data_day01a :: [Int]
data_day01a=[104042,112116,57758,139018,105580,148312,139340,62939,50925,63881,138725,54735,54957,103075,55457,98808,87206,58868,120829,124551,63631,103358,149501,147414,59731,88284,59034,116206,52299,119619,63648,85456,110391,90254,99360,59529,82628,82693,64331,123779,123064,93600,104226,74068,74354,149707,51503,130433,80778,72279,148782,113454,138409,148891,79257,126927,141696,107136,66200,120929,149350,76952,134002,62354,144559,125186,85169,61662,90252,147774,101960,55254,96885,88249,133866,121809,103675,94407,59078,81498,82547,132599,81181,141685,73476,107700,133314,77982,149270,119176,148255,81023,143938,54348,121790,126521,101123,139921,51152,97943]

data_day02a_raw :: [Int]
data_day02a_raw=[1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,10,23,27,2,27,13,31,1,10,31,35,1,35,9,39,2,39,13,43,1,43,5,47,1,47,6,51,2,6,51,55,1,5,55,59,2,9,59,63,2,6,63,67,1,13,67,71,1,9,71,75,2,13,75,79,1,79,10,83,2,83,9,87,1,5,87,91,2,91,6,95,2,13,95,99,1,99,5,103,1,103,2,107,1,107,10,0,99,2,0,14,0]

data_day03a_raw="R1009,D117,L888,D799,L611,U766,L832,U859,L892,D79,R645,U191,L681,D787,R447,D429,L988,U536,L486,D832,R221,D619,R268,D545,L706,U234,L528,D453,R493,D24,L688,U658,L74,D281,R910,D849,L5,U16,R935,D399,L417,U609,R22,D782,L432,D83,L357,D982,L902,U294,L338,U102,R342,D621,R106,U979,L238,U158,R930,D948,L700,D808,R445,U897,R980,U227,L466,D416,R244,U396,R576,U157,R548,U795,R709,U550,R137,U212,L977,U786,L423,D792,R391,D974,R390,U771,R270,D409,L917,D9,R412,D699,L170,D276,L912,U710,R814,U656,R4,D800,R596,U970,L194,U315,L845,D490,L303,U514,L675,D737,L880,D86,L253,D525,R861,D5,R424,D113,L764,D900,R485,D421,R125,U684,R53,U96,L871,U260,R456,U378,L448,D450,L903,D482,R750,U961,R264,D501,R605,D367,R550,U642,R228,U164,L343,U868,R595,D318,R452,U845,L571,D281,R49,D889,L481,U963,R182,U358,R454,U267,L790,D252,R455,D188,L73,U256,L835,D816,R503,U895,L259,U418,R642,U818,L187,U355,R772,U466,R21,U91,R707,D349,L200,U305,R931,D982,L334,D416,L247,D935,L326,U449,L398,D914,R602,U10,R762,D944,L639,D141,L457,U579,L198,U527,R750,U167,R816,D753,R850,D281,L712,D583,L172,D254,L544,D456,R966,U839,R673,D479,R730,D912,R992,D969,R766,U205,R477,D719,R172,D735,R998,D687,R698,D407,R172,U945,R199,U348,L256,D876,R580,U770,L483,D437,R353,D214,R619,U541,R234,D962,R842,U639,R520,D354,L279,D15,R42,U138,L321,D376,L628,D893,L670,D574,L339,U298,L321,D120,L370,U408,L333,D353,L263,D79,R535,D487,R113,D638,R623,D59,L508,D866,R315,U166,L534,U927,L401,D626,L19,D994,L778,D317,L936,U207,L768,U948,R452,U165,R864,D283,L874;L995,D93,L293,U447,L793,D605,R497,D155,L542,D570,R113,D779,L510,U367,L71,D980,R237,U290,L983,U49,R745,U182,L922,D174,L189,D629,R315,D203,R533,U72,L981,D848,L616,U654,R445,D864,R526,D668,L678,U378,L740,D840,L202,D429,R136,D998,L116,D554,L893,U759,R617,U942,R999,U582,L220,U447,R895,D13,R217,U743,L865,U950,R91,D381,R662,D518,L798,D637,L213,D93,L231,D185,R704,U581,L268,U773,R405,U862,R796,U73,L891,U553,L952,U450,R778,D868,R329,D669,L182,U378,L933,D83,R574,U807,R785,D278,R139,D362,R8,U546,R651,U241,L462,D309,L261,D307,L85,U701,L913,U271,R814,U723,L777,D256,R417,U814,L461,U652,R198,D747,R914,U520,R806,U956,L771,D229,R984,U685,R663,D812,R650,U214,R839,U574,L10,U66,R644,D371,L917,D819,L73,D236,R277,U611,R390,U723,L129,D496,L552,D451,R584,U105,L805,U165,R179,D372,L405,D702,R14,U332,L893,D419,R342,D146,R907,D672,L316,U257,L903,U919,L942,U771,R879,U624,L280,U150,L320,U220,R590,D242,R744,U291,R562,U418,L898,U66,L564,U495,R837,D555,L739,D780,R409,D122,L426,D857,R937,D600,R428,D592,R727,U917,R256,D680,L422,U630,L14,U240,R617,D664,L961,D554,L302,U925,L376,D187,L700,D31,L762,U397,L554,D217,R679,D683,R680,D572,R54,D164,L940,D523,R140,U52,L506,D638,R331,D415,R389,D884,R410,D62,R691,U665,R889,U864,L663,D690,R487,U811,L190,U780,L758,U267,R155,D344,L133,D137,R93,D229,L729,U878,L889,D603,R288,U890,R251,U531,L249,D995,R863,D257,R655,D311,R874,U356,L833,U151,L741,U246,R694,D899,L48,U915,L900,U757,L861,U402,R971,U537,R460,D844,R54,U956,L151,U74,R892,U248,R677,D881,R99,D931,R427"

data_day04a_raw=[123257..647015]

data_day05a :: [Int]
data_day05a=[3,225,1,225,6,6,1100,1,238,225,104,0,1101,78,5,225,1,166,139,224,101,-74,224,224,4,224,1002,223,8,223,1001,224,6,224,1,223,224,223,1002,136,18,224,101,-918,224,224,4,224,1002,223,8,223,101,2,224,224,1,224,223,223,1001,83,84,224,1001,224,-139,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1102,55,20,225,1101,53,94,225,2,217,87,224,1001,224,-2120,224,4,224,1002,223,8,223,1001,224,1,224,1,224,223,223,102,37,14,224,101,-185,224,224,4,224,1002,223,8,223,1001,224,1,224,1,224,223,223,1101,8,51,225,1102,46,15,225,1102,88,87,224,1001,224,-7656,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,1101,29,28,225,1101,58,43,224,1001,224,-101,224,4,224,1002,223,8,223,1001,224,6,224,1,224,223,223,1101,93,54,225,101,40,191,224,1001,224,-133,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,1101,40,79,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1008,226,677,224,1002,223,2,223,1005,224,329,1001,223,1,223,1107,226,677,224,1002,223,2,223,1005,224,344,1001,223,1,223,8,677,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,1108,226,677,224,1002,223,2,223,1006,224,374,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,389,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,404,101,1,223,223,1007,226,226,224,1002,223,2,223,1006,224,419,101,1,223,223,107,677,226,224,1002,223,2,223,1006,224,434,1001,223,1,223,1007,226,677,224,102,2,223,223,1005,224,449,101,1,223,223,1107,226,226,224,1002,223,2,223,1005,224,464,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,479,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,494,101,1,223,223,107,677,677,224,102,2,223,223,1005,224,509,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,524,101,1,223,223,1107,677,226,224,102,2,223,223,1006,224,539,1001,223,1,223,108,677,226,224,102,2,223,223,1006,224,554,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,569,1001,223,1,223,8,677,677,224,1002,223,2,223,1005,224,584,1001,223,1,223,7,677,677,224,1002,223,2,223,1005,224,599,101,1,223,223,1108,226,226,224,102,2,223,223,1006,224,614,101,1,223,223,1008,226,226,224,1002,223,2,223,1005,224,629,101,1,223,223,7,677,226,224,102,2,223,223,1006,224,644,1001,223,1,223,7,226,677,224,102,2,223,223,1005,224,659,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,674,101,1,223,223,4,223,99,226]

data_day06a_raw="2TJ)S3Z;MJD)YP8;9K5)7Q4;HJK)ZKW;6D4)P2Y;VPM)CB4;6FM)8V9;QC8)KDV;6JH)S5H;SLF)MVN;DM8)Y3H;LKP)RVY;HRJ)LKP;MVZ)YJP;PF6)C8M;3B8)32Q;NRH)6T1;GQG)6QR;5VD)T72;MV6)57B;7TR)SQG;LT5)KBZ;XSZ)VZQ;78F)DMV;CH4)45L;9WL)K75;X9C)JGS;BK5)P8B;97C)CX1;X3N)RGF;TQB)49B;T45)8QL;FCP)S6B;P7L)HHQ;BLN)SYN;4W4)HF1;JJJ)LMG;MTZ)46N;5YX)MTZ;XJL)PBY;F2T)FSF;C8J)G5D;624)L12;Y9R)9TV;RF1)XTP;5XL)SYY;FHG)B49;1D7)Y6V;9XV)W4X;V3N)WW1;3MX)XLW;W15)D6W;RM4)S86;CQ8)Q22;YP5)T17;BLF)GDT;5HW)TBZ;ZN7)F1S;WRV)HPP;T44)2LL;QCV)BSZ;56Y)RF1;SNB)42B;X3G)TY2;BD5)VCP;66X)LVL;SGB)DLR;HQB)HXW;XQ1)W99;L4F)FN5;2LB)63H;C35)1LX;64V)N8K;C2D)S53;COM)46J;F1S)HRB;B8G)BJ1;Y37)Q7M;57B)3TQ;9S3)YWK;W36)Z6M;Q21)Z73;LPM)23Y;CNF)QPF;ZPJ)DQS;BWC)MB5;ZPJ)GZ3;GFQ)HL6;29L)WLY;BXT)XLR;4JX)KWZ;PBR)2LB;PYX)Q2M;CB4)HZM;BH1)6BL;YH3)65L;PFS)KRK;NSM)281;YL1)ZGS;LZF)9LM;H85)4KV;H9X)ZLF;YN8)MYQ;T7G)5CD;DQS)5QM;X8W)B1R;N12)573;5JY)DV9;8Y5)CMH;T66)H85;31W)W15;148)PVK;WDL)KCF;WBK)11M;32H)M3C;59N)H4G;L5V)V9Q;7K1)9JK;YZR)N19;9Z2)H2J;XTW)WH1;K7V)PWD;1Q6)CSZ;HVS)H5G;J9Q)PBP;FBQ)83F;XCM)XQV;643)MYL;C87)JST;YN5)H5W;2GZ)J7Z;R6C)T3N;RB9)BZJ;HKV)6TJ;1W2)RVR;TVT)B1S;YQM)4RF;W6K)7LX;YXG)3P7;MY1)LPD;DYK)PNZ;SJ4)XGL;6MT)28C;5X7)BYJ;X58)SP9;KRK)YN8;5YK)YQV;GMX)JK3;CGJ)H84;213)NMJ;5SV)2TJ;4CJ)79Z;Y6W)RMM;F5Z)X3S;9RM)D61;V9Q)CW3;HSQ)5D1;L31)H1T;2BM)WVX;N8K)NBV;ZHD)QR4;JGS)7VY;R9R)N4B;111)9XV;1J3)KZH;XLR)XRB;PNZ)122;FSL)6MT;J81)JPW;B19)R38;KDM)HJK;XNM)544;135)49V;G77)RS5;PWS)YHG;Q7M)C2B;RKR)33K;MPF)4LH;CM8)W58;L1J)M6J;5HB)J5S;DM7)1XT;WBC)F2R;MP6)5SV;RVM)MVR;TVR)QGT;15D)6Y4;YLG)6HP;CW3)8VG;DJ8)QH6;C3F)448;Z8F)LHC;RXG)1JH;G2M)JHY;YD8)GBR;WDG)ZFM;6LY)SHH;VM4)YDS;5YP)HNR;2S2)ZN9;SPB)NY7;H84)LLC;ZQ5)T61;SPB)3CS;1PM)HZQ;F67)CY9;4KC)PK3;2CM)XJ5;W2L)NZC;3W5)NG5;VWC)DWF;6BM)J34;JX3)1YP;V2D)Y7P;9DJ)KD6;S5L)2CM;MZT)MQ1;PLC)ZQ5;TX2)JG6;1LX)JTT;MYB)F6Z;96X)5N4;BWD)9VX;44Q)JJN;MFP)1XG;PLN)XXS;F6Z)VVV;N4V)5NH;QZL)TMQ;9D5)89D;XW2)FQK;1WN)QP5;LK1)TMR;L6X)5WK;1DT)C8G;8B9)NTV;PD3)BXT;T46)LP9;PFX)38D;DPN)FFB;SFC)GBF;5CN)L3S;YJ5)3X7;PN6)M8Z;GKS)FMG;R74)H23;2T5)9YZ;GZS)PS6;PLZ)HJP;W87)NPX;SBC)31L;R6K)TJV;LGD)VJB;187)LYV;SW8)J1N;KCN)2P8;5FH)TBY;SJ9)7SL;B2R)6NM;MVS)GXS;XTP)HBF;5S8)7RS;FRD)7JK;C5F)SL5;VVP)2XG;RX1)1GD;F1X)M4V;7ZL)4T5;KQX)XBQ;96L)4CJ;BQ6)86H;ZND)W36;NW3)8KT;N62)WZD;7X1)YPB;C38)RPF;WQQ)7X1;38D)C3F;2XG)H2M;SF4)W2L;L12)BH1;MBL)9XB;GPC)WKG;BQL)T7G;3X2)ZYY;4RF)B1M;2PH)G5P;HNR)R5T;5XC)98T;MXX)DJ8;GK9)CPN;TMR)4VH;946)CT8;M2D)946;BGZ)BXL;J51)136;QVH)YDB;RM6)H8X;H8X)PVY;G6R)PSC;YP5)XN4;6YL)TPM;NQT)76L;SZN)B94;N63)M2D;H4Y)H9R;KS3)ZYM;F2H)S96;NVB)V1P;6HS)XXM;8Z1)KYS;VSY)JBS;X49)TD7;3WB)8MD;GY2)G25;PT6)JV2;Z73)FKG;P9H)YXR;VHL)Y15;9N7)PLZ;JWB)9YC;J5G)VWJ;6XD)HRJ;TS4)CQ9;J5S)NJT;XZH)D2Y;12X)RW8;79N)K1Y;CPN)6L2;JLF)7GC;RKF)WKF;SRZ)M17;135)FTN;K75)9XZ;F4S)BDT;152)CXZ;3PY)QD9;8T1)B3L;SDD)CSP;J9W)829;BHG)FFK;2MZ)F5Z;KD6)BWC;SV5)QM7;MVZ)T1M;6PW)6FP;9Z9)1Q6;R7G)GX9;338)WMT;LMW)HNG;R1P)KNM;FGB)RX5;CPN)YNB;973)LCR;NFN)5SJ;Z9Z)X3N;Q1H)XZH;H25)621;3CS)338;VV2)C87;RWM)SNB;H4G)SGB;DGS)3KF;M3C)PWJ;528)P4M;QQP)H9Q;NFR)NPR;TPM)ND9;D7D)BV3;QR4)YJX;RMM)HV9;RLJ)LMJ;RHH)PVR;6LV)5HW;3X7)63M;57G)9RM;K3C)7XS;P4D)KDP;TMR)KD4;F92)RXG;SJY)3DK;XRB)1GT;1TM)MJD;T8X)CDZ;HV9)P4Z;DGM)H8V;VTN)59N;PMZ)MD1;38N)2W4;HQM)T8X;WDD)C4L;KNK)MS7;9J7)CGQ;LYG)823;FVP)SJY;6S5)N4W;V7D)M7V;KXN)FM1;CMH)4R2;MZK)VMR;8XR)QWC;CJ9)TX4;MT6)TZL;6XV)7J9;RPX)QF4;KPZ)TGP;488)V7D;GX9)M61;ZPV)K5H;6YL)R3R;RG2)FD8;C89)L21;PHM)5J3;JKH)NSL;GYG)2C7;T26)SLZ;WDZ)QW7;1CJ)VP8;JGS)9K3;23D)KGS;BVN)L75;4CH)NL1;JHY)P4D;NJT)LWJ;2Z2)NWH;1VS)912;RZY)1D7;PCW)9JW;H16)YP5;R6T)HJ1;ZPN)5S8;WB9)YLL;R3R)B9T;89D)L86;VK5)Y38;K9M)8HM;HQM)RKZ;Q7Q)NW1;B8S)H7G;H5W)HXJ;QVP)GKQ;M4Y)PNQ;35C)C5C;1KH)BWH;L8Q)B8V;4FL)BSK;JM9)7B7;FJS)C9F;HNG)RZY;K4T)K29;LZY)7C6;BK5)YZR;FPS)SW8;BYJ)SZP;TGG)KPV;NNH)LNC;DTK)HG3;GNC)M2R;9WQ)BGZ;TX4)YZG;HWQ)B8B;281)59Q;XXZ)TV3;97X)3VN;PVK)RXY;SW8)7NJ;3VN)SLS;6HP)1LV;T3R)YWX;KNW)68M;1C1)R45;K8Z)7VZ;XFW)GSF;PWP)LXK;85M)TWM;F56)X49;545)3ZK;P2R)545;F7Y)DQ9;L75)P98;5P9)FJS;VCP)NBS;K9M)KSZ;QF4)D82;FY6)G5V;8ZV)2H8;JLX)L82;H1L)9RJ;Z9Z)F2H;8T5)11V;SRM)1W2;34F)J2G;1T7)XW2;3RD)PQQ;WB5)D49;H9Q)1PV;DSR)Z3B;TQW)FNP;77L)CD1;TZ5)KF7;D9K)MY1;5WL)HQB;CP3)B8M;4CL)187;211)S6N;8M9)513;82H)643;H23)LZY;F8G)PG5;6FF)R7G;WJ7)FYV;QS9)2P4;VR5)RJF;81H)23X;ZCJ)M4Y;J7K)MYJ;MGW)XBD;MYQ)779;6C6)3M4;KDP)3M8;KYS)K9Z;HG3)YJ4;4YD)97X;451)HPV;TY2)9GG;GWF)91M;7J9)1ZT;LHQ)38Q;FH7)QLZ;PLW)KY7;7FN)2FV;3MY)GZF;ZWR)CD5;TZ2)FGB;B8V)H25;QS8)HRG;JST)VHM;TNS)VTS;KHG)PJY;Z94)5DQ;DGJ)XPY;VZQ)H88;RH4)3GM;3CC)533;ZH7)TZZ;7ZX)GBP;ST9)BTM;9VP)VNH;MHN)D2C;4TM)V1T;QK8)Y4Y;RKX)VGD;84M)MCZ;Q37)5QP;J2Y)451;Q7F)3K2;1BJ)KQZ;CP9)VRR;R8Q)ST9;NW3)MZT;ZW2)Q7Q;3DK)ZCJ;4MC)L16;N6J)LFV;YJP)8LR;7B9)2HD;1JH)ZWR;H1T)4B5;4N2)944;23D)N5C;R34)JLX;T5J)BLX;RXY)VCV;MN2)TQB;82J)FD5;DTS)84M;M8Z)Y37;S6G)DBG;6ZT)1YT;JSL)RPX;GSF)LPM;MB5)WLC;11M)6CK;RGY)B9D;BNK)VV2;NL1)V5D;Z8T)QGF;R4T)VH6;N4W)X8H;1NL)DKP;ZJ6)RT5;6PX)Z89;1GD)WT5;LBF)PHM;GH8)SBC;JV2)1JR;R2B)2J6;S5H)P3V;FFH)C9P;BMY)X8W;CS3)JD4;ZNZ)4CL;YNB)YVG;VJV)XXZ;MFS)3JC;M6J)S9D;7RX)S8H;VM2)92K;ZZW)T26;R86)K5B;Q9H)ZBK;P1W)J64;57H)NW3;85V)1MJ;91M)K8Z;XBQ)GYC;DSM)ZPV;KM8)L8Q;M85)148;QRC)JSS;LGZ)BTB;ZTR)SZN;C2B)J7K;CVV)3YV;WJ7)PSX;3W5)KWJ;F8H)3DF;L49)213;S53)3ML;GJN)ZLQ;GR9)1D4;TDY)74Y;KSZ)2WV;6Y4)342;B5V)WPG;6B5)Y6T;J1N)BNN;LYG)82H;DVP)KL5;9LM)NKL;12X)1BN;YNK)GN8;PWL)N2P;BDT)XHV;KLS)3NQ;RVR)CYJ;CQ9)T46;ZPM)84L;12N)XD1;RB2)4W4;Z9C)8XQ;ZMY)RCT;44H)JVD;SW2)8ZP;DM5)LGD;P7R)9X5;V1P)BZ1;4F5)C81;2J9)R9N;7CH)WQ5;FXC)Y3V;WKF)WDD;HF1)F6J;KNK)7MK;BRZ)96X;V1T)7T3;Y37)ZZ7;ZYP)56Y;MSG)ZK6;LWM)JPR;Z71)FBZ;KDN)FQ2;P2Y)M85;NZC)PFS;NW1)H8S;CVX)SKD;NY7)9CY;FQX)XJL;TWM)SQK;ZXC)12N;3S8)M2B;KHN)X8V;ZKW)4BY;S6B)BTJ;J41)6S3;5V7)PWP;WLY)NL7;DV9)6FV;TH2)19Z;JTT)LSV;Z6D)JJQ;BTX)7XB;MNC)X5N;LVG)2FM;BQZ)SJ4;YVG)17F;GDM)Z9W;QBW)1WN;XZC)943;7RS)RG2;WRQ)3D3;D2Y)NVK;QW7)Y5H;FK7)GH8;85Y)9DM;9DQ)XVN;YJX)L2P;XPB)47W;SP7)4GH;76L)ZTL;RX1)W6Y;1XG)BQ1;HYB)BF1;3S6)48Q;GBW)F92;17F)7RX;YXR)GM3;P9H)8BJ;VVH)MPF;5VT)ZJX;NFH)GGQ;VNH)64V;XYX)39Z;T45)WBC;N98)X9C;2W4)QQP;136)LBF;GW8)66L;FM1)9K5;5Z1)TXH;ZZ7)L8V;CPS)WJ7;Z8D)BQ6;PSC)8JR;4YN)HVB;7LX)KXC;SP9)K1W;148)Y94;D2C)98P;YRT)JY3;HGJ)786;RGN)MSL;W4W)VZH;DF4)F8H;TZY)JW4;MVN)FC9;3K2)B3J;KYR)GGN;ZCJ)J9Q;P98)44H;S96)GBM;DJV)KP2;YF8)VMV;LFT)8ZV;R39)1CS;4T5)937;GN8)ZX1;Y6T)WRQ;XXM)JS9;L95)1J3;7TC)L5V;GYC)S5L;51N)M6C;722)GK9;8BJ)8MJ;8PM)PTN;7B7)2ZL;LG6)YD8;BB2)H1L;7S9)TNS;Y3H)C89;J3J)LG8;2SN)4HS;DKP)DVT;FYM)MVF;L14)4N2;FGX)HLM;GPC)M7J;2WV)528;D32)DBD;Q44)WC9;CBL)NTN;8QW)BHR;KJD)1D1;NSL)FNK;LV8)BYN;JJ8)Z8K;G6B)NRH;MCW)WK4;JB1)1T7;C9F)RW6;3MX)WYX;YLL)QYX;S31)DVP;Z9W)RJY;QF4)FGC;MV3)9RG;7Q1)ZM8;R7D)46M;H1Z)ZXC;ND9)YXG;JPR)51Y;LLJ)MP6;MD1)XTW;FSJ)S9S;VZR)WCQ;KD4)BLF;G5V)9Y6;DSJ)ZN7;ZK6)5FL;11K)8D4;7XS)ZGV;GXS)619;FKG)KS3;DRY)R6G;7YS)N7X;W59)DLP;2QY)H7V;8D4)418;91M)77L;B94)LX8;8XP)LVG;63M)MVM;J2G)CZC;YC1)SH2;ZC1)YF8;MQ4)51N;49K)TFY;S8H)RKD;FBZ)CH4;1PV)RFF;448)1FP;6K3)CVV;TZV)CKB;LVL)BGN;MVR)3VJ;8NR)X58;SBN)GPV;SRC)H8H;KP2)1VD;187)Y1J;LVP)P7P;LP9)CMN;ZRN)FQX;N8S)HCR;YP8)HWQ;D5W)MLZ;JQK)4KQ;QSX)TWL;1L9)Q65;SS2)1HG;SKD)Z6D;2P4)24K;G2M)KFD;46N)1CT;FQ2)8MM;9GP)FZH;GPV)DTK;JH4)ZMY;TGT)HXR;LH1)7MY;2FZ)JW7;3KF)9DJ;BXM)4SS;BCT)DWD;YQC)CBL;SJ5)JQ1;GHH)NM1;4FX)52F;F34)DKG;L8V)TCG;5SJ)VFT;PNT)7F5;BSK)L85;B8B)RB2;6L2)KHN;XD1)CNF;Y5Y)4TM;R43)95Z;TH2)3N1;K29)4RX;C81)RJV;JWX)MDL;YWX)2GZ;2S2)4TF;YPB)VZR;K2G)4FB;WC9)RQD;7MK)1W4;HCR)JH4;PMK)S5T;FJS)5K4;RDR)7B9;XBX)FK7;VVX)DNJ;76H)ZJC;911)GLZ;1DY)97C;7MF)XCX;4RN)G1Q;L6C)VWR;PK3)JNY;Y15)PRZ;W36)MQ3;QC1)7CZ;SVY)BKR;B9D)X61;H8M)VZ7;SQK)XZS;P1W)G2M;1MJ)LWV;BQ1)GW6;G8P)CLK;1N9)VJ4;C8M)YPG;H2M)BQZ;49B)6VT;S8Z)KTK;1HG)MSG;9HT)JQK;R31)973;3BL)F8N;LQJ)ZNZ;8DR)M9W;T21)L28;LDJ)1TM;9DQ)F8G;2B7)CN5;113)X9T;L8W)JZ6;MSL)Y9R;HMV)D5W;XDH)YJ5;TZV)JF3;RW6)2QY;F2T)K4T;WVN)T1W;D61)SRZ;ZBK)XJD;QDG)ZKG;9DM)WJV;SVR)Z71;PQQ)RKX;DV2)YJ6;6FS)6C6;5DQ)5FH;4QH)XBX;8D4)LZ1;XNJ)4J3;P77)R4T;YCG)GQF;KPV)XCK;6TJ)PWS;45L)TYN;BLX)BPT;3DF)9TM;XZS)7ZX;5FR)YL1;Q22)MBL;H9N)JSK;47W)4PJ;JSV)488;3Q7)7FM;4T9)KLS;XGR)4CH;HCR)R34;YZG)YJC;8GV)R4Q;YNN)8G5;DND)YRT;H9X)X74;Z7H)Y5Y;6SD)N5P;ZZ7)6FF;JB1)MVZ;VHM)JYP;73N)DB7;TMQ)29L;XZH)FYM;JRH)BLD;R5Z)VVP;JN9)1ST;T79)HYH;PVR)5YX;8YC)5RD;4Z2)W5B;MQ3)5WL;CYY)5YK;XT1)R86;QX9)BMT;48Q)Q1V;B2X)8QW;VZH)WZG;BZD)H33;BLD)WBK;92K)85V;8RX)KBN;BH1)N4D;MM5)PZL;FBZ)D2K;T72)TQW;RS5)X1K;RKD)MRQ;TJN)R43;7NJ)YGL;RTM)FGX;TV3)YSH;1Q6)6XD;4GH)LLJ;KBZ)LQJ;MYL)DV2;9SC)FQ1;KQZ)4TS;WRN)QVP;T1M)ZCH;6FN)4YN;BZ1)QZL;8VG)P8W;WKG)5VT;SHS)CGD;DGX)JLB;LZZ)S31;XVN)PD7;JNZ)W87;NDB)MCW;RCT)MYB;GZ3)DRY;X3M)RX1;6T1)HFT;CD1)VM4;2PL)73N;5NH)FVP;QH6)XT7;FFB)KZG;HXR)HMV;CGK)PBR;B14)7ZL;SRC)ZC1;38S)5VD;YCT)7WB;TCN)L6C;24K)S4V;G5V)LJF;WJV)JSV;27F)LZZ;JSK)5HD;2H8)TJL;TFT)GW8;RQJ)BXM;XJ5)9GP;GNM)2T5;KFW)13J;VS2)Y6S;5WL)BNK;754)MBG;XXJ)ZYP;L86)ZS8;JRF)6Q1;3ML)KXN;WDW)RM6;CX1)MNC;17Y)JX7;2ZM)8XR;RG3)FYC;B6V)81N;3M8)C48;HJ1)N2K;WWR)1CL;7V9)C27;8KT)JYT;SGC)3B8;L82)N62;G8H)GMX;G1Q)L1Q;C5F)5MS;ZCH)TZ2;KVF)KDM;2HD)N5H;QD6)YQC;J32)9ZB;RKZ)VHL;8WX)H16;MVN)L6X;YG4)XLL;5MF)FYZ;FHS)RB9;FZH)J32;LNC)3BL;7GC)46B;JDM)FH7;DDK)TPR;FQ1)H2S;J7L)FFH;4B5)RDR;81C)FPS;KWJ)1FB;CS3)P92;MLZ)1L9;SJD)YXC;YL3)4FS;WYX)ZPM;L16)GYP;B5Q)XNM;XJD)G36;7Q4)XKS;H7V)3S6;BTJ)CQT;C38)BKK;W4X)C35;79N)H68;RQD)6ZK;B3L)DS5;3LT)N2F;GGN)L49;7SF)W6V;H3B)K4Y;J74)HCV;PJY)5B6;84L)KRF;RV2)C2D;42B)6SD;1YP)PMK;S8G)19G;5KZ)DJN;Q6P)6W7;KNM)MJX;LKP)CP9;JQ7)XCM;9T8)VS7;6S3)BVN;M17)R6K;6ZT)JZZ;M61)H4Y;KRN)5HB;D6W)8XH;9J7)8B9;ZGV)CGJ;B8M)Q6P;F5P)W7Z;HRG)D7D;YY5)RLJ;GZL)XFW;4PJ)CGK;R8W)R8Q;R4Z)K5R;LMJ)FSJ;R38)KCN;L2P)GZL;K9T)B14;6VT)R39;BWH)7G5;W58)GQT;MVM)58N;WW1)SBN;FD8)76H;6BL)PLC;PZL)D5D;F3Y)Z7H;SCY)89P;KNG)QLR;HJP)GGC;X74)G77;3N1)23D;GKQ)6FS;69Y)9WQ;P8W)F34;X61)4RN;TWL)G4R;XSZ)HYB;KMF)QNY;BVD)MV3;J5M)57G;QPF)8M9;P7L)771;2FM)P2R;YXQ)Z94;G3R)NM2;QGT)N8S;RW3)XZ3;WPG)M4M;VQC)WPP;C5C)DGJ;W6Y)RW3;ZZ1)R9R;533)SV5;BV6)TZ5;SLK)7V9;ZLQ)M4T;SR2)BV6;G1K)4JX;B1R)G8H;9M1)ZH7;3D7)SVR;W7Z)VVX;ZFM)MH3;QD3)DTS;8MJ)KRN;QK8)4FL;NJD)1KH;QD9)QMN;5QP)69Y;5X4)9XX;8MR)V5S;ZM8)JL3;D32)L31;FD5)GBW;6CK)QVW;TSC)P9H;8QL)SW2;65N)85Y;PN5)18W;TSS)NJD;23X)T66;V9Y)R6T;R51)B59;KXC)26M;8ZP)T3R;58N)J7L;CXB)9WL;G36)2S2;P4M)BWD;D2K)BCT;JQ1)2PH;4VC)624;HFT)NSM;SSM)BQL;9RJ)111;8G5)DHN;3F2)CNM;B59)BRZ;48W)LZ4;5WK)9SC;9XX)4FX;3VJ)QD6;MT1)9J7;46M)ZPN;2HQ)WDZ;XKS)MQJ;VJB)HGY;R4D)H8D;B1Y)GZC;3TH)3RD;3JB)274;2LL)Q27;8MD)1VS;QNY)Z8F;B9T)YDZ;PD7)Z63;K4Y)82K;P3V)C1W;1PY)JK6;9GG)3S8;4KL)3LR;89P)8Z6;5RD)81H;GBF)7TC;18W)MFP;RW8)DG4;81N)H9X;9VX)4F5;DLP)YWZ;KNG)6BM;68G)M59;H8H)YNN;9TV)R74;6ZK)Q21;NR8)6D4;5SV)KPZ;C1W)B9R;WC9)XR2;5FL)YQM;TJV)1BJ;PRP)JXF;BKR)QQ1;Z2L)5P9;SYY)6W4;MKD)44Q;3QH)7S9;C13)Z1H;V3Q)BVD;W5P)5CN;KGS)HKV;T3N)8RR;6B3)KVP;PQ9)SSM;R6G)WZW;33K)MXX;GGQ)DND;R1P)SS2;H4K)49K;7X6)SRC;J64)CQ8;N5C)GHH;PRZ)PN5;WPG)VQC;274)GR9;HT4)5VJ;786)Z9P;779)P3Q;VQH)PQ9;RHH)KPJ;N98)P1W;YVM)4KC;7FM)NYR;ZJC)DGR;YPG)LF7;L28)YVK;HDT)GYT;DKG)G2P;T61)H71;7MY)2B7;9JW)QC1;JY3)SLF;9CY)6MC;MF4)MN1;342)J74;MYZ)85M;CNM)98N;NPX)LMW;KR3)BZD;JMC)WZM;Q37)NFR;VS7)4PX;7C6)SF4;1GT)MFS;CSZ)61M;9ZB)8T5;Z1J)8XP;HN6)FN8;L1Q)3VX;5RS)XPT;ZCZ)MVS;Y3H)YXQ;CDZ)KJB;GGL)5JX;Q2M)WVN;JZG)QBW;51Y)DGS;N4W)9T8;JL3)1DT;LPD)YF4;DWD)YH3;17Y)BYC;44L)DCS;CD5)QSX;HXW)Q9S;R4D)R92;HZM)PCV;Q1Y)81C;VD6)3CC;S6N)BK5;86H)XPB;XPB)JNZ;1LQ)HSQ;9LL)B8S;QWH)D9K;XT7)3X2;RJF)34F;XRQ)KX3;M6C)8T1;Z8T)FSL;W15)5P1;FF9)KDN;V9Q)R1P;8LR)CH8;B1S)SJD;FCY)RM4;GZF)FX8;5VJ)6B3;VVV)C21;DX4)X3M;FX8)3F2;LZ4)N17;QH6)9D5;DBV)RTM;YWZ)82J;N4B)PMW;8D7)BB2;P92)SJ5;ZS8)6FN;PDS)L14;K5Q)9N7;KCF)754;F2R)QVN;QP5)FS4;CSP)S8Z;HGH)NFN;H7G)NVB;R45)8D7;Z1T)J41;GBM)2SN;HJD)DM7;8XH)HC9;YFZ)R5Z;XGL)DYK;VWR)GJN;GYT)Q68;JY3)R27;BWD)PQ6;FG5)LV8;3TQ)DLH;YNB)4QH;D49)9D8;M59)C13;CJP)SHS;PTB)KR3;DCY)13T;TYJ)B5V;JXF)8WX;HZQ)DX4;HRJ)SRM;JLB)YLG;Y15)CS3;6MC)C9S;HC9)5V7;TPR)TSN;8HM)B19;FK1)RGN;BPT)RWM;8YP)JJ8;46B)XGS;RTM)K3C;G5P)7X6;H71)Q37;VZF)12X;NTN)JX3;1VD)B2X;N5H)PNT;1TM)6HS;Q1P)F56;HCV)4QR;LYV)6JH;VMR)WDG;111)CQ3;W94)VYM;RVY)8LB;H1M)37L;4XB)ZCF;XGS)MQL;QGF)4KL;DG4)31W;MN1)QJK;WJV)CSG;DGJ)LH1;TD7)WDW;LJF)WRV;1ZT)68G;TTX)JK4;XLW)MM5;6W7)YNK;KL5)3Q7;4LH)TTH;QJK)BQD;GS1)11K;H8D)HND;1D1)7G9;WXT)W9N;YJ4)QD3;VFT)3MY;YJ6)QVH;QMN)JRF;WK4)BHG;5VD)9RC;VRR)NR8;D82)GDG;ZKD)TGG;W5B)FJC;P7P)W5P;68M)VTN;VP8)D32;SH2)XRQ;Z6M)F79;QR9)RQJ;TXH)PWL;96M)D28;FVW)MV6;WLC)QCV;BCY)W3X;XBD)4VC;9JK)X3G;1D1)CJP;Z9P)JKH;JLP)KFN;LZ1)CM8;819)V3N;6MC)XZ7;1TH)N13;JSS)5RH;Z8K)TCN;PVY)38N;JZ6)RKF;W6V)SM7;7JK)SVY;3D3)B2R;HBF)35C;Z1H)QX9;PNT)25H;JRK)2T1;JZZ)HB4;829)ZL8;PMW)PLW;QMS)7T1;DBD)ZGR;4BY)FHG;38Q)L97;Z5K)RGY;2T1)ZRZ;4PX)G3R;7FH)91N;NSL)MGW;J34)J5M;XH7)6B5;3GM)VFB;DLH)C38;CGJ)F1Z;4Z7)QMS;NTV)MHN;L21)66X;R5T)RWS;N19)KWW;4SF)9DQ;271)VWC;RJY)JDD;9RG)CZS;FZH)GDM;GBR)CJ9;8Z6)F7Y;4WF)4TP;9K3)LVD;5SG)TFT;RGR)6PX;DYX)BD3;CFZ)5GW;56Y)DDK;HHQ)4MC;H68)T5J;WCQ)TP8;HPV)BTX;7G5)3JB;RX5)MZK;4PJ)Z5K;M7J)TZV;5P1)K9T;CZC)DSM;136)R31;H2V)ZRN;VWJ)DPN;Z1H)S3M;MQ1)JLF;SZP)9LL;YPB)LG6;B9R)RR3;3TQ)4JW;JSD)BK3;JL3)XDH;FFH)CLL;G2P)2J9;DNJ)R4D;CKB)CGB;KTK)JN9;VRM)SR2;2DT)6J9;PDK)211;HXJ)CYY;BNN)BSC;KF7)MYZ;573)XY6;XN4)7SF;95Z)LHT;GDG)VJV;H33)G29;28R)6LY;PWJ)3LT;4VH)38S;H4F)CFZ;QYX)XT1;122)G41;XQV)985;3S8)NBN;1R3)KM8;1FB)PND;FN8)L8W;L3S)TJN;4T9)FCY;CGQ)6PW;1BN)5Z1;XXM)819;98N)1CJ;88T)7R9;H2S)BTF;JK4)9Z2;M9W)PF2;C4L)PFR;LLC)QRC;6J9)FBQ;TGP)5XC;BJ1)WRN;3KH)LZF;LSV)M5P;4BB)JB1;37L)H9N;J7Z)KNK;PQX)3R5;YBT)SLK;Y38)PDK;R3Q)H1Z;ZCZ)5X4;TFY)YL3;S31)W6K;MB1)XH7;4QR)FPT;Q9S)VZF;CGD)WHC;5SK)PRP;T4C)QC8;BXL)Z1T;7WB)GY2;2YT)GFQ;V52)FVW;HPP)TYJ;Y94)WXT;89T)2YT;WVX)JRH;NBN)48W;GM3)GQB;8JR)5TG;VFB)ZZ1;544)J5G;ZKG)Q1Y;5KB)K5Q;JK6)4T9;N2K)1NL;KBN)9M1;TZ2)L6H;BV3)VVH;D2K)1TT;LFG)17Y;XZ3)Q9H;S4V)F1X;82K)Z75;83F)3W5;XCK)RVM;RT5)P7R;FXH)TDY;YD3)3WB;FPT)RKR;FNK)PN6;NFH)JZG;HVB)T45;LWJ)5RS;FYZ)5X7;HRB)FCP;CQ8)1LH;FS4)DM8;SYN)65N;1ZG)YYB;WHC)WX6;S3Z)DF4;JYT)YFZ;WX6)RG3;YJC)Z1J;CN5)R6C;RJ5)7S3;DQ9)J3J;X9C)LYG;ZL8)CFN;7T3)PCW;G25)B53;MDL)GKS;923)FF9;LKL)ZHD;F6J)H21;CQ9)PFX;V1K)F4S;BYC)4J7;JXF)3HJ;M4T)113;H8V)N6J;9RC)DJS;3P7)VS2;NKL)Z8T;7JR)J5T;YGL)2DT;Y1J)QS8;CLK)VPM;X9T)MKD;MVF)QPS;W5T)YOU;M5P)N4V;H2J)H1M;S5T)WQQ;L85)722;XXS)Z8D;VFJ)GS1;23Y)3TH;NL7)HGJ;MS7)QRB;FQK)T79;KRF)5FR;TZZ)B6V;4FL)LWB;QPF)LKL;GBP)FHS;C9F)KFW;ZDR)YN5;PND)JLP;MJR)Z9C;RR3)TSS;ZYY)9HT;T1W)LVP;KZG)W1L;WZW)BH5;QRB)GQG;DCS)CP3;CQT)H4F;HND)FDP;H5X)MF4;DJS)BD5;HPP)YQP;771)JQ7;TBY)PMZ;RPX)28R;X1K)B6F;1CS)JJJ;JZ7)4Z2;BKK)XYX;PF2)LT5;J53)Z9Z;7G9)89T;6NM)9Z9;WBK)B5Q;KFD)GWF;VHM)2FZ;JS9)T21;B49)JWX;W3X)JMC;BTB)LWM;3NQ)VK5;Q1Y)K4G;2PT)1PM;2DY)8Y5;GZL)1C1;7S3)T1S;9BV)R4L;K1W)Q9Z;PSX)DGX;T17)VM2;PNQ)3D7;2SV)M18;C9S)GNC;XCM)WB9;BD3)ZPJ;BZJ)RV2;YWK)47J;VJB)5SK;WZD)ZCZ;JF3)G9W;6D4)TZY;HXJ)XSZ;ZD4)HVS;VP8)DM5;MYJ)7Q1;DHN)FK1;LCR)4WF;HGY)RJ5;JLB)RGR;YDZ)K2G;513)LDJ;DM8)5SG;VJ4)6XV;XZD)SP7;TTH)J53;9G6)Z2L;PFL)P8P;J1M)H8M;JX7)J51;65L)S8G;JW4)SPB;NMJ)15D;1ZT)Q1H;1D4)KMF;WMT)FY9;HB4)9BV;9WJ)JSL;1S3)S6G;C27)S99;VYM)JZ7;QPS)88T;2JL)R51;TCG)LK1;8MJ)KQX;T3N)KNW;NC9)J72;G9W)CYG;3VX)FXH;FDP)YVM;1ST)57H;JX7)KJD;JJQ)ZDR;5CD)5MF;JYP)CWC;LHT)3VD;9Y6)VFJ;943)JRK;BZJ)4XB;Q2M)2BM;KVP)GNM;T1W)7CH;QJK)V52;LWB)7K1;CZC)2PT;CYG)4YD;9XB)PF6;4J7)MDR;SL5)1XV;6FF)ZD4;63H)RHH;4TP)F2T;CGQ)TTX;1XV)KHG;K29)V9Y;CH8)8NR;5RS)2LZ;39Z)96M;SHH)13D;8XQ)KB5;N2P)TP1;1XT)L4F;BTF)Q7F;WH1)78F;N7X)JSD;985)1S8;YVK)WXC;NWH)T44;BKR)MB1;TP8)6YL;WZM)8MR;K1Y)DBV;C7Z)4QV;7SL)8Z1;7VZ)2PL;CWC)4SF;FYV)J1M;FK7)TH2;C48)DYX;J5T)5KB;L4F)MT1;SKD)GGL;31V)LHQ;7VY)DGM;F8N)R4Z;ZTL)W94;LNC)2WJ;B19)2HQ;1LV)W59;937)1TH;8MM)7WN;YDS)JDM;SQT)Q1X;KY7)6LV;C35)MQ4;6QR)R3Q;BYN)QDG;CYJ)B1Y;52F)VQH;CXZ)S6J;823)PD3;2J6)M49;H16)VSY;13D)TVT;DS5)ZJ6;BF1)DSJ;32Q)GPC;N4D)PTB;FM1)YCG;ZLF)8YP;NBV)FG5;CFN)BCY;7F5)BLN;96L)DJV;LHC)8YC;KCF)PZ2;T1S)J81;8KT)1ZG;TZL)1K5;RFF)R7D;H5W)GZS;D28)8PM;XR2)XZD;ZGR)Q1P;2C7)QWH;BKC)X6K;X61)P7L;NCK)ZTR;B49)C5F;K5B)YC1;DQ9)ZZW;VMV)L95;PCV)RH4;Y4Y)KVF;VFJ)NNH;1M9)VD6;MQL)QMW;M49)V3Q;28C)2DY;YYB)9WJ;JDD)7TR;BXM)MM6;1LH)32H;MQJ)135;1FP)XZL;M4M)XZC;57Z)YBT;YQP)N98;9DM)8GV;XHV)FXC;QQ1)PT6;LG8)4BB;P8P)G6R;SP7)N12;5JX)F5P;SF5)ZND;GDT)S5R;S6J)DSR;N57)6K3;KWW)7FH;CMN)2SV;HLM)TM1;RVR)FY6;YQV)V2D;NBS)P77;M2B)L1J;PQ6)NDB;C21)FLL;MM6)9G6;3KH)ZKD;FN5)C8J;LF7)911;B5Q)B8G;CZS)KYR;S8G)M41;Z3B)6FM;5MS)HDT;FSF)SGC;X8H)71S;PZ2)BKC;944)3KH;YD8)152;Z75)TYV;G41)YCT;B1M)2ZM;619)PDS;CSG)271;D6W)SCY;6FP)F13;9XZ)KPF;8D7)7JR;XCX)27F;BMT)PYX;JG6)9S3;FC9)CXB;M2R)PQX;GQF)HN6;X5N)9VP;PBP)JM9;31L)QS9;KX3)HT4;1BJ)SFC;F6F)TGT;VCV)C7Z;M7V)4Z7;BSC)7PT;4ZY)YTH;8V9)FS1;R9N)TSC;KZH)7MF;CP9)44L;CQ3)GYG;H88)WB5;K5R)YY5;ZN9)LFT;L6H)J9W;MDR)7FN;TRS)96L;ZKD)VR5;R34)1N9;7R9)5YP;46J)NC9;BGN)FBN;F5Z)NQT;DWF)1DY;V5D)G1K;MJX)TX2;61M)79N;PNZ)XPS;R4L)MT6;66L)WWR;ZLQ)R2B;QR9)T4C;MRQ)2MZ;J72)PLN;G8H)SDD;4FB)N57;H8S)1XQ;JZ6)QK8;HXW)K7V;QWC)923;Y6S)FRD;X6K)3MX;49V)WDL;KYR)ZW2;PTN)6ZT;CLL)31V;KTK)MJR;98T)4CZ;JVD)1M9;4KV)W5T;WXC)2B2;KFD)CPS;79Z)R8W;59Q)F6F;8PF)LT9;BK3)BMY;1JR)5JY;QX9)YD3;9X5)PFL;LVD)H4K;3YV)NCK;W9N)1S3;DMV)HGH;QMW)TVR;TBZ)LGZ;13J)N63;QV5)2Z2;M8Z)1PY;BSZ)JWB;HJK)BPH;418)J2Y;1W4)3CD;4SS)HJD;1W2)DP7;MH3)LJ6;Y5H)LBC;G29)8PF;JBS)H5X;W1L)Y8D;R4L)CVX;2LZ)H2V;BHR)7YS;7K1)G8P;ZCF)V1K;B6F)MN2;VZ7)MZB;RPF)6S5;QLR)F3Y;R6T)5XL;CGB)4ZY;RWS)XQ1;JK3)XGR;MB5)SAN;RGF)57Z;XZ7)KNG;M41)LFG;DM5)G6B;NVK)3PY;3CD)H3B;GZC)F67;SM7)SQT;KDP)YG4;KB5)5KZ;GYP)TRS;8LB)W4W;X3G)HQM;N17)PHG;TJL)1LQ;QM7)SJ9;4B5)2JL;Z63)3QH;TYN)K9M;BH5)XNJ;QVN)1R3;NPR)QV5;DVT)DCY;DGR)VRM;TGG)SF5;1N9)Q44;MF4)XXJ;DP7)NFH;J41)8RX;TQB)QR9;19Z)TS4;5TG)8DR;KDV)Y6W"

data_day07a :: [Int]
data_day07a=[3,8,1001,8,10,8,105,1,0,0,21,38,59,84,93,110,191,272,353,434,99999,3,9,101,5,9,9,1002,9,5,9,101,5,9,9,4,9,99,3,9,1001,9,3,9,1002,9,2,9,101,4,9,9,1002,9,4,9,4,9,99,3,9,102,5,9,9,1001,9,4,9,1002,9,2,9,1001,9,5,9,102,4,9,9,4,9,99,3,9,1002,9,2,9,4,9,99,3,9,1002,9,5,9,101,4,9,9,102,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,99]

data_day08a="222201222021222122221212222222202122222022102220222202222120222222120222222222022222022222122221222222220222222222222202202222022222222221222222222222222200222120222222121202222222222022222122112220222212222022222222221222222222122222122222222222222222222222222222222222212222022222222221222222222222222220222120222222221202222222212222222222002221222212222020222222022222222222122222022222222221222222220222221222222222202222022222220222222222222222222200222020222222121202222222202022222222002221222222222222222222121222222222022222122222222222222222220222221222222202212222022222220222222222222222222221222020222122120202022222202022222222222221222202222020222022020222222222222222022222022221222222221222220222222202202222122222222222222222222222222222222022222022120212222222212122222122112220222222222121222122121222222222122222222222022221222222221222222222222212202222122222221221222222022222222220222021222222122222122222222022222022002220222202222021222122221222222222022222122222222221222222220222222022222212222222022222222222222222122222222220222221222222020212122222212122222022112222222212222221222122220222222222222222222222121222222222222222220022222222212222122222221220222222022222222201222221222122220212222222202122222022122222222212222222222222120222222222022222122222122220222222220222220002222222212222122222221221222222022222122202222122222022122212122222212222222022002220222202222122222122121222222222022222022222222220222220221222202212222212222222222222220221222222122222222220222120222122222222022222212022222122112221222212222021222122220222222222022222022222220221222221221222200222222212202202022222220222222222022222122212222220222222221212022222212122222222022221222222202221222022021222222222022222122222222221222221221222222122222202212212122222221221222222122222122221222221222022120222222222202122222222212221222202202022222022022222222222122222222222122221222222222222202222222202202202222222221221222222122222022211222221222022122222122222212022222222020222222222222022222122122222222222022222122222122220222220220202222022222202222222022222221220221222122222122202222222222022122212222222212122222222202220222222202022222122221222222122022222022222221201222221220212221002222212202202122222221220222222222222222220222122222202220202022222212122222122010220222212212220222122022222222022122222022222222212222221222202220202222202202202222222222220221222022222222210222221222122120212222222222222222022121220222222202220222122022222222022222222222222221200222220222222221222222202222212222222220221221222222222122210222120222022222202222222212122222222202220222212222221222222221222222022122222022222020200222222221202211022222202222222222222220221221222022222122201222121222012220212222222202222222222020220222212212121222021022222222122222222222222022210222221221202201212222212222212122222221222220222222222022201222020222112120202122222212222220022212220222222202221222121122222222222022222122222220202222221222212212002222212212222222222222221220222222222122212222020222102222222022222202122222022122221202222212222222222121222222122022222222222121211222220222222200212222222202222122222220220220222122222022201222122222022222212122222212222222122110221202212212022222220220222220022222222022222222202222221222212212002222202222222122222222221221222222222122202222122222022222202222222212122220222102222212212212220222220121222221122222222122222020200222220221222222012222222212202222222222220222222222222022221222122222002022212222222222122222122101221212202202120222020220222221022022222222222221210222221222212212222222212212202122222220220222222022222022212222022222112220222122222222122222022200220222212202021222221220222221022022122222222221212222220220202211112222202202212122222221222221222222222122212222020222022020202122222220122220022010220202212202122222222221222221122122222222222222202222221222222222122222202212202122222220220222222222222222212222022222102222222022222212022220222022222202222202021222021122222221122122022122222121202222222222212200112222202222222122222221220220222122222222210222121222212221222022222012222221222111220212202222021222122220222221122022022222222021210222221220202221012222202222212022222221220220222022222122212222120222122021212222222121022222222220222202222202022222122021222220022122222022222222211222222221202210112222222222212222222222221222222022222022202222222222202221202122222122022221222201221202212212120222020222222221122122022222222022220222221220202212122222212222202222222220222222222222222122200222121222002020202022222120122221022012221222202202220222121120222220222222022122222220222222221221222211222222222222212222222222220222222022222022202222122222200021222222222200222222122010220202222222221222121121222201122022022222222021211222222220202222212222222222202222222222120222222222222022210222121212110022202122222111222220022111220212212222220222122120222222122122122222222121201202221220222211212222202202222122222222121220222022222221221222221202111120212122222002222221022200221212202222122222022120212220022122122220222121201202220220202221102222202202222122222222220220222122222222200222022222212021222022222012122220222202222212202202022222221020222210221122022122222020212222221222212202122222212222222122222222122222222022222121200222121202102020202122222202122221122212221202222202022222120021212212220200022021222222212202221222212220202222212212212222222222222222222122222120010222221222111221222222222002122222122222220222212212120222221222202211122002022122222222211212222220222211022222202222202122222220220221222222222122201222122202120021202022222210222220022222221202212212020222021222222212221011120120222022200212220221212200102222222202202122222222220220222122222120002222221222021122222022222020122220222020221222212212121222221121222200221002122120222121222202221221212211122222212222222022222221022222222022222210020222122222011222202122222101122220022122221202202202121122022122202200122012222220222220220222220221202210012222202202222122221221021220222022222012100222122202202221202122222102222222022121200222212212122222222020202212222212021022222122211212220222212202202222212222202122222220021220222022222020202222121212100120202122222001222220022002212202222212222022120020222210022202221121222020201211221221202200202222202222222222221220120201222122222121122222220212111120202122022022122222022012221212212202222222222221202220221220022122222120222221220221202201202222222122212222220220121202222122222111120222020222120020212022022021222221022111201212222212122022121221202222121212021021222221201211221220222200022222212202222022220212121222222222222012220222121222122221222022022021222221122202222212202212022022022221212210020202222120222221210200222220212210222222222222222122222221022212222022222010012222120202120020212022122120222222022220212202212202222022120122212211221222020222222020200202221221202201202222212022212022220221122201222222222100202222020202020222202022222110222221222210200212222202122222220220222201211212121122222022202221022222222201122222202012212022222212120222222122222001102222020222002121212222022120122222122011102102212212121222120022202202211020220021222021221211022220202202102222212122202122222220120201222022222022220222122202120022212222112212022220222121121012222222120022121020212210012020222222222020210201121221212202022222222002212022221201221221222122222220000222121222211021212222122001022221222010012122212222220222120021221221021221002020222020201201222222202222112222222022212122220221222202222022222222111222120222202022202222222102222220222122112112202222221022020122200210011120002020222220212221120222222220022222222022212122222201120201222122222222221222220212012022222222202121222221122100102222212222120122022220200220021002220122222120211202010220212220102222212112212222221221221221222022222212011222020222012120202022222120022221122102001122222212222022022020221200222022112221222022222200200220202212022222212012212222222201121220222122222220021222121202112101202022102210022220222001222022202202021122022120200222211220101221222120200221011222222220202222212112202122221220221212222022222220210222222222221001202222022000122220222100212112222222120222022121212210100222111021222220200200010222212212222222202022212022220200220221222222222220120222222222001111202022022220222220222221000022212202220022222222212221111011202120222120221221010220222200022222212202212022221200022200022222222001201222222202201002202122012111022222122210210202202212120222020220221221122220021021222222222212220222212202112222212212212122220221220221222022222021200222122202100011212122202002022221022121021012222222121022222020211210210012100122222120222201021221222200002222202202222222220202021200222222222212010222221222121200222122022112222221022100011202212202120122220121220221112001101220222121211220212222202221012212212202222022221202121221022122222011120222020222110010212222122102222222122011211002202202222022020221220210100001120222222121221210020220202220112222102202212022220201220200022222222210000222021222002110212022012012122222022112010012222202022122122221221210100110021120222021201200222220212210002222212022212122220210220200022122222011001222120212121112212122122002122220122022100102212222020022221022210221112022212222222122210212022220212200122222002222202022222200221222022222222110221222122222220101222022012221222220022220110212202202222222020220212222111012100220222221212212011220212222212202102012222222222212221200122222222101221222020202210220202122112001122221022020200222202202220022022122201220002210100021222121220201221221212222002222222022212022221201122222222222222122012222220212212001222022012111122220122011022122212212222222120221200201001002201020222221220222201222212202112222212012202222220201120200222122222110120222121212002212202122012111022222222122012102222222222022220222200211020001201122222221222201002202202201202202112122212120222201021210222122222001222222120202101122222212212200022222022012001012202222022122020222201212221110112022222020212201120202222202202212012122212020221220122221122222222202201222122202001221212202202010122220122112111102212212122122122222210201222112212122222220212210022220202222002222212122222221222212221212122122222222022222022212212002212202202212122221022110122101222202102022120221200220021222011022222122210212010210212222112222222002212222221212222200222022222000012222222102021211212022122201122222222211122001202212122122021120202202002120212022222221200200102222212211012202202002212021221210020202222122222100111222022002002222202222202000222221222000210112222212122222121221212201001211112221022122202210222220212201122222212002212022220200022221022222222222202222022002202121222002012112221222020120101111202222112122022222221222221110200021022120222221000210212200112212102112222120220210020222021222222212012222222112120200212012222011220220020221101202202212211222021021212202010112221222122022202220202222202211022222022222222020221211221221022122222222221222121012002112202012112111120222220012000022212220011022220222220212010212202120222021201222012202222202122202002002222021221220122210120122222112001222122022220111212102022112220220221110021012202212100022020221212212201000220222222222211222200201222200112222102022202122221202021210222122222202110222022122201112212222002102122222020202210121202211202022120220220220221211110221022121200201202021222201202222002022222022222212221210120222222011221222021222122102222012012010021220122102201001212221222022220121200222111120110221022120200211200101212212212222012102212120221220120222221022222022021222222222210110202202112201022221021200110012212200112022020220201201211001111122222220201200220102222200202212222022212220222221120220122022222111202222222212002011222222012202021222120220201120022202110112221122222212010022002020122120222220112222222202222202112122222222221212021222121222222110110222122220102102202222102010120221122001001211002212122222222121212221221120020222222020211210121111202210212212102202212121220210120210221222222221112222222012100222202022112122121222022022202202122202111012102221212212201002012121022020011210202202202211022212002222202121221200122200221022222121112222221122220122212122202211220220220121100102102202120022112121210201121011110222202101012201112201212212022212222012222220221202222222022222222202200222221211011020222102102121122201221002212201022221202022120020211200010220200220202212201222120122202021212222012102222120220212222201122122222120201222220102002000112122012122022200020200012200222202221202110221211212122010000220102221010221110112202122022202102012222221222221120212121022222110202222222020121202022222022021222201121210202110012220120022200222202202200020201222122002210210101200202010022212102112222100220202022220222022222111022222121120201000012012012222021212120112002211022220001222001021222220000220220020202012220201220010202021112212002012201112220200220211220222222010000222021020100121102102002010022201220102201201212222001122110121212200202020022122122100201211221111212210112212012002201010222210121210021222222221220222220001020200012022102221122220220121200221022220222112120022201212122101100021202102121220101222212221222222202012211211220211222221122122222000002222121001020221122202202111021210121112210022022200001022101122212220002100012020002211111210022002222011220222002112210202221211121212120222220022110222020202021220022002022021022212222202111001002221000002221020212212111021012221202120121222222120212202212202102222220101221201222201120222202011020222121000222112112102002010222220221112202112102221222112111120200201112202102122122102200210020122212121011202002222222100221200120200022222222211122222222012210000022102202002020221122021022002222201220122222121221202212210111221102000212222121212222102001212002202201110220220221221020122211111201222022101221122222102102220021201120222112002112221211122201120211212212022100020202202110220010210202120001222012002200111222222121212022122200200000222121102222000012102202111221212221020120002212202120002110120221221212001121021022202000212112121202100110202022202211020222202120220220022202020221222220111221201002202112212122221122202021021212211201212100021001200011021102220222101012201212102202201220202222212200200220200021220100222220020110222120010020202222112202100021202221200001002102200110202002122122220002011102122202110001221201100222212002202022212222212221200221202121122102101010122222012002220212012212112001201222202002111222211101002022221001222121211121221102102002012221110212221112222012212202120221122121200110022201212112222120002212120022212102102012221121100210121122210100002122220101221202021201022122222020211112210202101000202212012222210220212122212000022001112201222220100111021202202002020012211020210021211002010101212212121202222000220122122112212220212012022212022112202102222211021022102020202101222010011010111010100112021110100020021011012010111101100211111101011120201222100212111220211000210101010110201100011121010110100121001211112212120012211201"