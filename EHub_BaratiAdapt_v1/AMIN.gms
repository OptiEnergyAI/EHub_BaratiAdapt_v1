Sets

i Ernergy Hubs just one in this scenario /EH1/

t Hours in a day /1*24/;

*Now Parametrs, only the thing that are time or EH dependant*

Parameters

Lambda_gas(t) Gas price should be constat check later 

Pel(i,t) Electrical Load profile 24hrs

Phl(i,t) Thermal Load profile 24hrs

Ppv(i,t) PV loads 24hrs

Pwt(i,t) WT loads 24hrs

CL(i,t) Cooling load

Lt(t) Electricity Price 24hrs

Ees_min(i) Minimum capcity of ES

Ees_max(i)  Maximum capacity of ES

PHac_max(i) Maximum input heat of AC

Pec_max(i) Maximum electricity input of EC

Pice_max(i) Maximum electricity input of CS

Ehs_min(i) Minimum capacity of HS

Ehs_max(i) Maximum capacity of HS

Ecs_min(i) Minimum capacity of CS

Ecs_max(i) Maximum capacity of CS

Ph_chp_max

Ph_b_max

Ph_max

Pg_max(i) Maximum grid exchangew capacity

;

Ees_max(i)  = 150;
Ehs_max(i)  = 300;
Ecs_max(i)  = 200;

Pice_max(i) = 80;
PHac_max(i) = 100;
Pec_max(i)  = 120;
Ees_min(i)  = 0;
Ehs_min(i)  = 0;
Ecs_min(i)  = 0;

Pg_max(i) = 500;

*Pel(i,t) Import .xlsx data string*

$call gdxxrw.exe loads.xlsx output=loads_el.gdx par=Pel rng=el!A2:C25 rdim=2 cdim=0

$gdxin loads_el.gdx
$load Pel
$gdxin

$call gdxxrw.exe loads.xlsx output=loads_th.gdx par=Phl rng=th!A2:C25 rdim=2 cdim=0

$gdxin loads_th.gdx
$load Phl
$gdxin

$call gdxxrw.exe loads.xlsx output=loads_cs.gdx par=CL rng=cs!A2:C25 rdim=2 cdim=0

$gdxin loads_cs.gdx
$load CL
$gdxin

$call gdxxrw.exe loads.xlsx output=loads_pv.gdx par=Ppv rng=pv!A2:C25 rdim=2 cdim=0

$gdxin loads_pv.gdx
$load Ppv
$gdxin

$call gdxxrw.exe loads.xlsx output=loads_wt.gdx par=Pwt rng=wt!A2:C25 rdim=2 cdim=0

$gdxin loads_wt.gdx
$load Pwt
$gdxin

$call gdxxrw.exe loads.xlsx output=loads_Lt.gdx par=Lt rng=price_el!A2:B25 rdim=1 cdim=0

$gdxin loads_Lt.gdx
$load Lt
$gdxin

$call gdxxrw.exe loads.xlsx output=loads_gas.gdx par=Lambda_gas rng=gas22!A2:B25 rdim=1 cdim=0

$gdxin loads_gas.gdx
$load Lambda_gas
$gdxin




Scalars

OM_chp Maintenance cost coefficient CHP /2/

OM_b Maintenance cost Boiler /2.7/

PEN_curt Penalty of load curtailment /20/

LHV Low calorific value of natural gas /9.7/

ETA_b Boiler gas to coefficient /0.8/

ETAes_c Charging efficency of ES /0.9/

ETAes_d Discharging efficency of ES /0.9/

COPec performace coefficient EC /4/

COPice performance coefficiewnt CS  /3.5/

COPac performance coefficient AC probably single effect try double effect /1.2/

COcurt coefficient of maximum curtailed load /0.25/

MRh_up Maximum coefficient for up heating load /0.5/

MRh_down Maximum coefficient for down heating load /0.2/

MRe_up maximum coefficient DRP upper limit /0.5/

MRe_down Maximum coeeficient DROP lower limit /0.2/

ETAcs_c Chargin efficency CS /0.97/

ETAcs_d Discharging efficency CS /0.95/

Pes_cmax Maximum charge power ES /15/

Pes_dmax Maximum discharge power ES /15/

Phs_cmax Maximum charge power HS /40/

Phs_dmax Maximum discharg power HS /40/

Pcs_cmax Maximum charge power CS /120/

Pcs_dmax Maximum discharge power CS /140/

ETAhs_c Charging efficency HS /0.9/

ETAhs_d Discharging efficency HS /0.9/

ETA_chp_h Thermal efficency CHP /0.45/

ETA_chp_e El efficency CHP /0.35/

Gchp_max /60/

Gb_max /30/

;

Ph_chp_max = Gchp_max * LHV * ETA_chp_h;   
Ph_b_max   = Gb_max   * LHV * eta_b;       
Ph_max     = Ph_chp_max + Ph_b_max;        

Display Ph_chp_max, Ph_b_max, Ph_max;
                                                        
Variables

Pg(i,t) Imported Power from the grid

Z1

;

Positive Variables

Pes_c(i,t) Charging power ES

Pes_d(i,t) Discharging power ES

Pe_up(i,t) shift up of electrical load by DRP

Pe_down(i,t) shift down of electrical load by DRP

Ph_up(i,t) shift up of thermal load by DRP

Ph_down(i,t) shift dwon of thermal load by drp

Phs_c(i,t) Charging heat of HS

Phs_d(i,t) Discharging heat of HS

Pcs_c(i,t)

Pcs_d(i,t)

Pe_chp(i,t) output electrical power from CHP

Ph_chp(i,t) output heating power from CHP

Ph_b(i,t) output heating power from boiler units

Ees(i,t) Saved energy in ES

Ehs(i,t) Saved energy in AZ

Ecs(i,t) Saved energy in CS

Cec(i,t) cooling power EC

Cac(i,t) cooling power AC

Pcsd(i,t) cooling power CS

Gchp(i,t) Imported gas from gas network by CHP

Gb(i,t) Imported gas from gas network by biler units

Pice(i,t) Imported power to CS

Pec(i,t) Imported power to EC

Pcurt(i,t) Curtailed load

PHac(i,t) Imported heat to AC

Binary Variables

Ih_down(i,t) binary variable of down heating DR

Ih_up(i,t) binary variable of up heating DR

Kes_c(i,t) binary variable of ES charging
 
Kes_d(i,t)   binary variable of ES discharging

Khs_c(i,t) binary variable of HS charging
 
Khs_d(i,t)  binary variable of HS discharging

Kcs_c(i,t)

Kcs_d(i,t)

Ie_up(i,t)  binary variable of up of electrical DR
 
Ie_down(i,t)  binary variable of down of electrical DR;

Equations  
    Obj,
    Const1,Const2,Const3,Const4,Const5,Const6,Const7,Const8,Const9,
    Const10,Const11,Const12,Const13,Const14,Const15,Const16,
    Const17,Const18,Const19,Const20,Const21,Const22,Const23,
    Const24,Const25,Const26,Const27,Const28,Const29,Const30,
    Const31,Const32,Const33,Const34,Const35,Const36,Const37,
    Const38,Const40,Const41,Const42,Const43,Const44,Const39,
    Const45,Const46,Const47,Const48,Const49,Const50,Const51,
    Const52,Const53,Const54,Const55,Const56,Const57;

Obj.. Z1 =e= sum((i,t),Pg(i,t)*Lt(t) + Gchp(i,t)*Lambda_gas(t) + (Pe_CHP(i,t) + Ph_CHP(i,t))*OM_chp + Gb(i,t)*Lambda_gas(t) + Ph_b(i,t)*OM_b + Pcurt(i,t)*PEN_curt);


*Electric DRP*

Const1(i).. sum(t,Pe_up(i,t)) =e= sum(t,Pe_down(i,t));
*Grants that total energy doesn't change, peak hour equal to off peak, eq.4*

Const2(i,t).. Pe_up(i,t) =g= 0;
*LOWER limitation on how much can increase electrical load eq.5*

Const3(i,t).. Pe_up(i,t) =l= MRe_up*Pel(i,t)*Ie_up(i,t);
*UPPER limitation on how much can increase electrical load eq.5*

Const4(i,t).. Pe_down(i,t) =g= 0;
*LOWER limitation on how much can DEcrease electrical load eq.6*

Const5(i,t).. Pe_down(i,t) =l= MRe_down*Pel(i,t)*Ie_down(i,t);
*UPPER limitation on how much can DEcrease electrocal load eq.6*

Const6(i,t).. Ie_up(i,t)+Ie_down(i,t) =g= 0;

Const7(i,t).. Ie_up(i,t)+Ie_down(i,t) =l= 1;

Const8(i,t).. Pcurt(i,t) =g= 0;

Const9(i,t).. Pcurt(i,t) =l= Pel(i,t)*COcurt;

*Thermal DRP*

Const10(i).. sum(t,Ph_up(i,t)) =e= sum(t,Ph_down(i,t));
*Grants that total thermal energy doesn't change, peak hour equal to off peak, eq.4*

Const11(i,t).. Ph_up(i,t) =g= 0;
*LOWER limitation on how much can increase thermal load*

Const12(i,t).. Ph_up(i,t) =l= MRh_up*Phl(i,t)*Ih_up(i,t);
*UPPER limitation on how much can increase thermal load eq.5*

Const13(i,t).. Pe_down(i,t) =g= 0;
*LOWER limitation on how much can DEcrease thermal load eq.6*

Const14(i,t).. Ph_down(i,t) =l= MRh_down*Phl(i,t)*Ih_down(i,t);
*UPPER limitation on how much can DEcrease thermal load eq.6*

Const15(i,t).. Ih_up(i,t)+Ih_down(i,t) =g= 0;

Const16(i,t).. Ih_up(i,t)+Ih_down(i,t) =l= 1;

*----------------------ELECTRICAL ENERGY STORAGE-----------------------*

Alias (t,tp);

*With this i'm making a copy of the t set so that i can use it to define the next t*
*t and tp represents 24hrs but now i can use it together, basically i'm creating a bidemensional set and need to filter it*
*The filter is needed so that i can choose only the t+1 couples for example (1,2) and not (1,5)*

Set tnext(t,tp);

*with this every row of t will have the next t=tp*

tnext(t,tp) = yes$(
    ord(tp) = ord(t) + 1
  or (ord(t) = card(t) and ord(tp) = 1)
);


*this is the line that allow me to assign to every t the t+1*
*basically with the previous lines i created a kind of a matrix and now i'm declaring that for every t, tp needs to be t+1*
*so it's YES for every consecutive couples so that GAMS doesn't choose couples like (1,5)*

*Now I can write the equation needed, filtering it only for these couples and that is...*

Const23(i,t,tp)$tnext(t,tp)..
    Ees(i,tp) =e= Ees(i,t)
                 + Pes_c(i,t)*ETAes_c
                 - Pes_d(i,t)/ETAes_d;

*keep this in mind, everytime you need to have a t+1, you need to write all this above*

Const17(i,t).. Ees(i,t) =g= Ees_min(i);
*Lower limitation on stored energy level eq.14*

Const18(i,t).. Ees(i,t) =l= Ees_max(i);
*Upper limitation on storage enrgy level*
*note to self: it's not about power it's about the quantity of energy that the ES can have, these are boundaries, basically declaring the storage capacity i guess eq.14*

Const19(i,t).. Pes_c(i,t) =l= Pes_cmax*Kes_c(i,t);

Const20(i,t).. Pes_d(i,t) =l= Pes_dmax*Kes_d(i,t);

Const21(i,t).. Kes_c(i,t)+Kes_d(i,t) =l= 1;

Const22(i,t).. Kes_c(i,t)+Kes_c(i,t) =g= 0;

*----------------------THERMAL ENERGY STORAGE-----------------------*

Const24(i,t,tp)$tnext(t,tp)..
    Ehs(i,tp) =e= Ehs(i,t)
                 + Phs_c(i,t)*ETAhs_c
                 - Phs_d(i,t)/ETAhs_d;

Const25(i,t).. Ehs(i,t) =g= Ehs_min(i);
*Lower limitation on stored energy level eq.14*

Const26(i,t).. Ehs(i,t) =l= Ehs_max(i);
*Upper limitation on storage enrgy level*
*note to self: it's not about power it's about the quantity of energy that the ES can have, these are boundaries, basically declaring the storage capacity i guess eq.14*

Const27(i,t).. Phs_c(i,t) =l= Phs_cmax*Khs_c(i,t);

Const28(i,t).. Phs_d(i,t) =l= Phs_dmax*Khs_d(i,t);

Const29(i,t).. Khs_c(i,t)+Khs_d(i,t) =l= 1;

Const30(i,t).. Khs_c(i,t)+Khs_d(i,t) =g= 0;

*----------------------COOLING STORAGE-----------------------*

Const31(i,t,tp)$tnext(t,tp)..
    Ecs(i,tp) =e= Ecs(i,t)
                 + Pcs_c(i,t)*ETAcs_c
                 - Pcs_d(i,t)/ETAcs_d;

Const32(i,t).. Ecs(i,t) =g= Ecs_min(i);

*Lower limitation on stored energy level eq.14*

Const33(i,t).. Ecs(i,t) =l= Ecs_max(i);
*Upper limitation on storage enrgy level*
*note to self: it's not about power it's about the quantity of energy that the ES can have, these are boundaries, basically declaring the storage capacity i guess eq.14*

Const34(i,t).. Pcs_c(i,t) =l= Pcs_cmax*Kcs_c(i,t);

Const35(i,t).. Pcs_d(i,t) =l= Pcs_dmax*Kcs_d(i,t);

Const36(i,t).. Kcs_c(i,t)+Kcs_d(i,t) =l= 1;

Const37(i,t).. Kcs_c(i,t)+Kcs_d(i,t) =g= 0;

*----------------------ENERGY BALANCE--------------------*

Const38(i,t).. Pg(i,t)+Ppv(i,t)+Pes_d(i,t)+Pe_chp(i,t)+Pe_down(i,t)+Pcurt(i,t) =e= Pel(i,t)+Pes_c(i,t)+Pe_up(i,t)+Pice(i,t)+Pec(i,t);

Const39(i,t).. Pg(i,t) =g= -Pg_max(i);

Const40(i,t).. Pg(i,t) =l= Pg_max(i);

Const41(i,t)..Ph_chp(i,t)+Ph_b(i,t)+Phs_d(i,t)+Ph_down(i,t) =e= Phl(i,t)+Phs_c(i,t)+Ph_up(i,t)+PHac(i,t);

Const42(i,t).. Ph_chp(i,t)+Ph_b(i,t)+Phs_d(i,t)-Phs_c(i,t) =l= Ph_max;

Const43(i,t).. Ph_chp(i,t)+Ph_b(i,t)+Phs_d(i,t)-Phs_c(i,t) =g= 0;

Const44(i,t).. Ph_b(i,t) =e= Gb(i,t) * LHV * ETA_b;

Const45(i,t).. Gb(i,t) =g= 0;

Const46(i,t).. Gb(i,t) =l= Gb_max;

Const47(i,t).. Pe_chp(i,t) =e= Gchp(i,t) * LHV * ETA_chp_e;

Const48(i,t).. Ph_chp(i,t) =e= Gchp(i,t) * LHV * ETA_chp_h;

Const49(i,t).. Gchp(i,t) =l= Gchp_max;

Const50(i,t)..Gchp(i,t) =g= 0;

Const51(i,t)..  
    Cec(i,t) + Cac(i,t) + Pcs_d(i,t) =e= CL(i,t);

Const52(i,t).. 
    Pcs_c(i,t) =e= Pice(i,t) * COPice; 

Const53(i,t).. 
    Pice(i,t) =l= Pice_max(i);

Const54(i,t).. 
    Cac(i,t) =e= PHac(i,t) * COPac;

Const55(i,t).. 
    PHac(i,t) =l= PHac_max(i);

Const56(i,t).. 
    Cec(i,t) =e= Pec(i,t) * COPec;

Const57(i,t).. 
    Pec(i,t) =l= Pec_max(i);

Model AMIN /all/;

Option MIP = Cplex;
Option limrow=10, limcol=10, solprint=on, sysout=on;

Solve AMIN using MIP minimizing Z1;

*----------------------------------temproary check*-------------
Display Pes_c.l, Pice.l, Pec.l, Pcs_c.l;


* Piccolo check a video
Display Z1.l, Pg.l, Pe_chp.l, Ph_chp.l, Ph_b.l, Ppv, Pwt, Pcurt.l;


*===========================
* POST-PROCESSING RESULTS
*===========================

* ---------- COST ANALYSIS ----------
Set c / grid, gas_chp, om_chp, gas_boiler, om_boiler, curtail /;

Parameter
    CostComp(i,t,c)   "costo per componente, hub, ora"
    CostComp_tot(c)   "costo totale per componente"
    Cost_totale       "costo totale complessivo"
;

CostComp(i,t,'grid')       = Pg.l(i,t)*Lt(t);
CostComp(i,t,'gas_chp')    = Gchp.l(i,t)*Lambda_gas(t);
CostComp(i,t,'om_chp')     = (Pe_chp.l(i,t)+Ph_chp.l(i,t))*OM_chp;
CostComp(i,t,'gas_boiler') = Gb.l(i,t)*Lambda_gas(t);
CostComp(i,t,'om_boiler')  = Ph_b.l(i,t)*OM_b;
CostComp(i,t,'curtail')    = Pcurt.l(i,t)*PEN_curt;

CostComp_tot(c) = sum((i,t), CostComp(i,t,c));
Cost_totale     = sum(c, CostComp_tot(c));

Display CostComp_tot, Cost_totale;


* ---------- ENERGY BALANCE (check residui) ----------
Set side /supply, demand/;

Parameter
    ElBal(i,t,side), ElResidual(i,t)
    HtBal(i,t,side), HtResidual(i,t)
    ClBal(i,t,side), ClResidual(i,t)
;

* Elettrico
ElBal(i,t,'supply') =
      Pg.l(i,t) + Ppv(i,t) + Pes_d.l(i,t) + Pe_chp.l(i,t)
    + Pe_down.l(i,t) + Pcurt.l(i,t);
ElBal(i,t,'demand') =
      Pel(i,t) + Pes_c.l(i,t) + Pe_up.l(i,t)
    + Pice.l(i,t) + Pec.l(i,t);
ElResidual(i,t) = ElBal(i,t,'supply') - ElBal(i,t,'demand');

* Termico
HtBal(i,t,'supply') =
      Ph_chp.l(i,t) + Ph_b.l(i,t) + Phs_d.l(i,t) + Ph_down.l(i,t);
HtBal(i,t,'demand') =
      Phl(i,t) + Phs_c.l(i,t) + Ph_up.l(i,t) + PHac.l(i,t);
HtResidual(i,t) = HtBal(i,t,'supply') - HtBal(i,t,'demand');

* Cooling
ClBal(i,t,'supply') = Cec.l(i,t) + Cac.l(i,t) + Pcs_d.l(i,t);
ClBal(i,t,'demand') = CL(i,t);
ClResidual(i,t) = ClBal(i,t,'supply') - ClBal(i,t,'demand');

Display ElResidual, HtResidual, ClResidual;


* ---------- COMPONENTI PER I GRAFICI STILE PAPER ----------

Set
    eSup "offerta elettrica"
/ grid, pv, wt, chp, es_dis, dr_down, curt /
    eDem "domanda elettrica"
/ load, es_ch, dr_up, ice_ch, ec /

    hSup "offerta termica"
/ chp, boiler, hs_dis, dr_down_h /
    hDem "domanda termica"
/ load, hs_ch, dr_up_h, ac /

    cSup "offerta di freddo"
/ ec, ac, cs_dis /
    cDem "domanda di freddo"
/ load_c /
;

Parameter
    PelSup(i,t,eSup)  "offerta elettrica per componente"
    PelDem(i,t,eDem)  "domanda elettrica per componente"
    HtSup(i,t,hSup)   "offerta termica per componente"
    HtDem(i,t,hDem)   "domanda termica per componente"
    ClSup(i,t,cSup)   "offerta cooling per componente"
    ClDem(i,t,cDem)   "domanda cooling per componente"
;

* ---- ELETTRICO: OFFERTA ----
PelSup(i,t,'grid')   = Pg.l(i,t);
PelSup(i,t,'pv')     = Ppv(i,t);
PelSup(i,t,'wt')     = Pwt(i,t);
PelSup(i,t,'chp')    = Pe_chp.l(i,t);
PelSup(i,t,'es_dis') = Pes_d.l(i,t);
PelSup(i,t,'dr_down')= Pe_down.l(i,t);
PelSup(i,t,'curt')   = Pcurt.l(i,t);

* ---- ELETTRICO: DOMANDA ----
PelDem(i,t,'load')   = Pel(i,t);
PelDem(i,t,'es_ch')  = Pes_c.l(i,t);
PelDem(i,t,'dr_up')  = Pe_up.l(i,t);
PelDem(i,t,'ice_ch') = Pice.l(i,t);
PelDem(i,t,'ec')     = Pec.l(i,t);

* ---- TERMICO: OFFERTA ----
HtSup(i,t,'chp')       = Ph_chp.l(i,t);
HtSup(i,t,'boiler')    = Ph_b.l(i,t);
HtSup(i,t,'hs_dis')    = Phs_d.l(i,t);
HtSup(i,t,'dr_down_h') = Ph_down.l(i,t);

* ---- TERMICO: DOMANDA ----
HtDem(i,t,'load')    = Phl(i,t);
HtDem(i,t,'hs_ch')   = Phs_c.l(i,t);
HtDem(i,t,'dr_up_h') = Ph_up.l(i,t);
HtDem(i,t,'ac') = PHac.l(i,t);

* ---- COOLING: OFFERTA ----
ClSup(i,t,'ec')      = Cec.l(i,t);
ClSup(i,t,'ac')      = Cac.l(i,t);
ClSup(i,t,'cs_dis')  = Pcs_d.l(i,t);

* ---- COOLING: DOMANDA ----
ClDem(i,t,'load_c')  = CL(i,t);


*===========================
* EXPORT SU GDX + EXCEL
*===========================

Parameter Z1_opt "Optimal operation cost";
Z1_opt = Z1.l;

execute_unload "results.gdx",
    Z1_opt,
    CostComp, CostComp_tot, Cost_totale,
    ElBal, ElResidual,
    HtBal, HtResidual,
    ClBal, ClResidual,
    PelSup, PelDem,
    HtSup,  HtDem,
    ClSup,  ClDem;

* Unico file Excel con tutti i fogli
$call gdxxrw.exe results.gdx output=results.xlsx par=Z1_opt        rng=Summary!A1      rdim=0 cdim=0
$call gdxxrw.exe results.gdx output=results.xlsx par=CostComp      rng=Costi!A1        rdim=3 cdim=0
$call gdxxrw.exe results.gdx output=results.xlsx par=CostComp_tot  rng=Costi_tot!A1    rdim=1 cdim=0
$call gdxxrw.exe results.gdx output=results.xlsx par=ElBal         rng=El_Bal!A1       rdim=3 cdim=0
$call gdxxrw.exe results.gdx output=results.xlsx par=HtBal         rng=Ht_Bal!A1       rdim=3 cdim=0
$call gdxxrw.exe results.gdx output=results.xlsx par=ClBal         rng=Cl_Bal!A1       rdim=3 cdim=0
$call gdxxrw.exe results.gdx output=results.xlsx par=PelSup        rng=El_Sup!A1       rdim=3 cdim=0
$call gdxxrw.exe results.gdx output=results.xlsx par=PelDem        rng=El_Dem!A1       rdim=3 cdim=0
$call gdxxrw.exe results.gdx output=results.xlsx par=HtSup         rng=Ht_Sup!A1       rdim=3 cdim=0
$call gdxxrw.exe results.gdx output=results.xlsx par=HtDem         rng=Ht_Dem!A1       rdim=3 cdim=0
$call gdxxrw.exe results.gdx output=results.xlsx par=ClSup         rng=Cl_Sup!A1       rdim=3 cdim=0
$call gdxxrw.exe results.gdx output=results.xlsx par=ClDem         rng=Cl_Dem!A1       rdim=3 cdim=0
























