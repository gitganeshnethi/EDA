
conc = read.csv("concrete_mod.csv")
conc
conc$conc_cement_mod = (conc$cement-mean(conc$cement))/sd(conc$cement)
conc$conc_slag_mod = (conc$slag-mean(conc$slag))/sd(conc$slag)
conc$conc_water_mod = (conc$water-mean(conc$water))/sd(conc$water)
conc$conc_coarseagg_mod = (conc$coarseagg-mean(conc$coarseagg))/sd(conc$coarseagg)
conc$conc_fineagg_mod = (conc$fineagg-mean(conc$fineagg))/sd(conc$fineagg)
conc$conc_age_mod = (conc$age-mean(conc$age))/sd(conc$age)
conc$conc_strength_mod = (conc$strength-mean(conc$strength))/sd(conc$strength)
#View(conc)
conc1 = conc[8:14]
e = eigen(var(conc1))   
e
conc1$pc1 = e$vectors[1,1]*conc1$conc_cement_mod+e$vectors[1,2]*conc1$conc_slag_mod+e$vectors[1,3]*conc1$conc_water_mod+e$vectors[1,4]*conc1$conc_coarseagg_mod+e$vectors[1,5]*conc1$conc_fineagg_mod+e$vectors[1,6]*conc1$conc_age_mod+e$vectors[1,7]*conc1$conc_strength_mod
View(conc1$pc1)
conc1$pc2 = e$vectors[2,1]*conc1$conc_cement_mod+e$vectors[2,2]*conc1$conc_slag_mod+e$vectors[2,3]*conc1$conc_water_mod+e$vectors[2,4]*conc1$conc_coarseagg_mod+e$vectors[2,5]*conc1$conc_fineagg_mod+e$vectors[2,6]*conc1$conc_age_mod+e$vectors[2,7]*conc1$conc_strength_mod
conc1$pc3 = e$vectors[3,1]*conc1$conc_cement_mod+e$vectors[3,2]*conc1$conc_slag_mod+e$vectors[3,3]*conc1$conc_water_mod+e$vectors[3,4]*conc1$conc_coarseagg_mod+e$vectors[3,5]*conc1$conc_fineagg_mod+e$vectors[3,6]*conc1$conc_age_mod+e$vectors[3,7]*conc1$conc_strength_mod
conc1$pc4 = e$vectors[4,1]*conc1$conc_cement_mod+e$vectors[4,2]*conc1$conc_slag_mod+e$vectors[4,3]*conc1$conc_water_mod+e$vectors[4,4]*conc1$conc_coarseagg_mod+e$vectors[4,5]*conc1$conc_fineagg_mod+e$vectors[4,6]*conc1$conc_age_mod+e$vectors[4,7]*conc1$conc_strength_mod
conc1$pc5 = e$vectors[5,1]*conc1$conc_cement_mod+e$vectors[5,2]*conc1$conc_slag_mod+e$vectors[5,3]*conc1$conc_water_mod+e$vectors[5,4]*conc1$conc_coarseagg_mod+e$vectors[5,5]*conc1$conc_fineagg_mod+e$vectors[5,6]*conc1$conc_age_mod+e$vectors[5,7]*conc1$conc_strength_mod
conc1$pc6 = e$vectors[6,1]*conc1$conc_cement_mod+e$vectors[6,2]*conc1$conc_slag_mod+e$vectors[6,3]*conc1$conc_water_mod+e$vectors[6,4]*conc1$conc_coarseagg_mod+e$vectors[6,5]*conc1$conc_fineagg_mod+e$vectors[6,6]*conc1$conc_age_mod+e$vectors[6,7]*conc1$conc_strength_mod
conc1$pc7 = e$vectors[7,1]*conc1$conc_cement_mod+e$vectors[7,2]*conc1$conc_slag_mod+e$vectors[7,3]*conc1$conc_water_mod+e$vectors[7,4]*conc1$conc_coarseagg_mod+e$vectors[7,5]*conc1$conc_fineagg_mod+e$vectors[7,6]*conc1$conc_age_mod+e$vectors[7,7]*conc1$conc_strength_mod
#View(conc1)
p1=var(conc1$pc1)
p2=var(conc1$pc2)
p3=var(conc1$pc3)
p4=var(conc1$pc4)
p5=var(conc1$pc5)
p6=var(conc1$pc6)
p7=var(conc1$pc7)
totalvar = p1+p2+p3+p4+p5+p6+p7
totalvar
pp1 = (p1/totalvar)*100
pp1
pp2 = (p2/totalvar)*100
pp2
pp3 = (p3/totalvar)*100
pp3
pp4 = (p4/totalvar)*100
pp4
pp5 = (p5/totalvar)*100
pp5
pp6 = (p6/totalvar)*100
pp6
pp7 = (p7/totalvar)*100
pp7

pp1+pp2+pp3+pp4+pp6+pp7

