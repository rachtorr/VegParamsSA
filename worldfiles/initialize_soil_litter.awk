{a = 0;}
($2=="veg_parm_ID") && ($1==11) {h=1;}
($2 == "litter_cs.litr1c") {printf("%f %s\n",$1=0.01750144,$2); a=1;}
($2 == "litter_ns.litr1n") {printf("%f %s\n",$1=0.00065488,$2); a=1;}
($2 == "litter_cs.litr2c") {printf("%f %s\n",$1=0.09678136,$2); a=1;}
($2 == "litter_cs.litr3c") {printf("%f %s\n",$1=0.12390227,$2); a=1;}
($2 == "litter_cs.litr4c") {printf("%f %s\n",$1=0.20446007,$2); a=1;}
($2 == "soil_cs.soil1c") {printf("%f %s\n",$1=0.03211751,$2); a=1;}
($2 == "soil_ns.sminn") {printf("%f %s\n",$1=0.00000000,$2); a=1;}
($2 == "soil_ns.nitrate") {printf("%f %s\n",$1=0.00000000,$2); a=1;}
($2 == "soil_cs.soil2c") {printf("%f %s\n",$1=0.13780940,$2); a=1;}
($2 == "soil_cs.soil3c") {printf("%f %s\n",$1=1.76464549,$2); a=1;}
($2 == "soil_cs.soil4c") {printf("%f %s\n",$1=10.69857115,$2); a=1;}

(a == 0) {printf("%s	%s\n",$1,$2);}
