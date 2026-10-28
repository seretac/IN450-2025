(* ::Package:: *)

(* ::Input:: *)
(*ClearAll["Global`*"];*)


(* ::Input:: *)
(*n=16;*)
(*alpha=7;*)
(*beta=2;*)
(*rotL[x_,s_]:=BitAnd[BitOr[BitShiftLeft[x,s],BitShiftRight[x,n-s]],2^n-1];*)
(*rotR[x_,s_]:=rotL[x,n-s];*)
(*(* funzioni round di SPECK *)*)
(*round[{x_,y_},k_]:=Module[{nuovoX=BitXor[Mod[rotR[x,alpha]+y,2^n],k]},*)
(*{nuovoX,BitXor[rotL[y,beta],nuovoX]}];*)
(*roundInverso[{x_,y_},k_]:=Module[{vecchioY=rotR[BitXor[x,y],beta]},*)
(*{rotL[Mod[BitXor[x,k]-vecchioY,2^n],alpha],vecchioY}];*)
(**)
(*(*key della forma {l_2,l_1,l_0,k_0}, r # round *)*)
(*RoundKeys[key_List,r_]:=Module[{l,k,KeySchedule},*)
(*k={key[[-1]]};*)
(*l=Reverse[key[[1;;3]]];*)
(*KeySchedule[{ll_List,kk_List},i_]:=MapThread[Append,{{ll,kk},round[{ll[[i+1]],kk[[i+1]]},i]}];*)
(*Fold[KeySchedule,{l,k},Range[0,r-2]][[2]]];*)
(**)
(*(* Il cifrario E viene scomposto in tre sotto-cifrari E = E_0 E_m E_1. Siano p e q le probabilit\[AGrave] dei differenziali per E_0 ed E_1,e sia pm la probabilit\[AGrave] dello switch centrale E_m. La probabilit\[AGrave] totale del distinguisher boomerang \[EGrave] P = p^2*pm* q^2 e viene calcolata nella funzione ProbTotale*)*)
(**)
(*(*test differenziale con r round di SPECK, mi restituisce la probabilit\[AGrave] "empirica" ossia il numero di successi sulle nProve*)
(*din e dout sono le differenze in ingresso e in uscita*)*)
(*TestDiff[din_,dout_,r_Integer,nProve_Integer]:=Module[{ks,a,b,s},s=Sum[ks=RoundKeys[RandomInteger[2^n-1,4],r];*)
(*a=RandomInteger[2^n-1,2];*)
(*b=BitXor[a,din];*)
(*a=Fold[round,a,ks];*)
(*b=Fold[round,b,ks];*)
(*Boole[BitXor[a,b]===dout],{nProve}];*)
(*(*Restituisce:{probabilit\[AGrave], -Log2(prob)}*)*)
(*{N[s/nProve],If[s>0,N[-Log2[s/nProve]],Indeterminate]}];*)


(* ::Input:: *)
(**)
(*(* test dello switch a 2 round di SPECK (analogo a TestDiff) *)*)
(*SwitchTest[din_,nout_,nProve_Integer]:=Module[{k,x1,x2,z1,z2,y1,y2,z3,z4,x3,x4,s},s=Sum[*)
(*(* Generazione chiavi e plaintext *)*)
(*k=RandomInteger[2^n-1,2];*)
(*x1=RandomInteger[2^n-1,2];*)
(*x2=BitXor[x1,din];*)
(*(* Cifratura in avanti per 2 round *)*)
(*z1=round[x1,k[[1]]];*)
(*z2=round[x2,k[[1]]];*)
(*y1=round[z1,k[[2]]];*)
(*y2=round[z2,k[[2]]];*)
(*(* Aggiunta della differenza in uscita e decifratura all'indietro*)*)
(*z3=roundInverso[BitXor[y1,nout],k[[2]]];*)
(*z4=roundInverso[BitXor[y2,nout],k[[2]]];*)
(*x3=roundInverso[z3,k[[1]]];*)
(*x4=roundInverso[z4,k[[1]]];*)
(*(* Verifica del quartetto boomerang, s tiene il conto dei successi *)*)
(*Boole[BitXor[x3,x4]===din],{nProve}];*)
(**)
(*{N[s/nProve],If[s>0,N[-Log2[s/nProve]],Indeterminate]}];*)
(**)


(* ::Input:: *)
(*(* Differenze di input ed output prese dalla tabella 6*)*)


(* ::Input:: *)
(*p=TestDiff[{16^^2800,16^^0010},{16^^8000,16^^840A},4,10^5] (* \[TildeTilde]2^-5*)*)


(* ::Input:: *)
(*q=N[TestDiff[{16^^0A04,16^^0804},{16^^8102,16^^8108},4,10^5]]  (* \[TildeTilde]2^-6*)*)


(* ::Input:: *)
(*pm=SwitchTest[{16^^8000,16^^840A},{16^^0A04,16^^0804},10^5] (* \[TildeTilde]2^-10*)*)
(**)


(* ::Input:: *)
(*(* La probabilit\[AGrave] finale \[EGrave] scritta nella forma -Log2[P]*)*)


(* ::Input:: *)
(*ProbTotale[p_,q_,pm_]:=N[2p+pm+2q];*)
(*ProbTotale[p[[2]],q[[2]],pm[[2]]]  (* \[TildeTilde]2^-32*)*)


(* ::Input:: *)
(*(* altra prova cambiando nabla in e nabla out*)*)
(*ProbTotale[TestDiff[{16^^2800,16^^0010},{16^^8000,16^^840A},4,10^4][[2]],TestDiff[{16^^0a60,16^^4205},{16^^8000,16^^8000},4,10^4][[2]],SwitchTest[{16^^8000,16^^840A},{16^^0a60,16^^4205},10^4][[2]]]*)


(* ::Section:: *)
(*Confronto dei risultati con quelli del paper*)


(* ::Text:: *)
(*Eseguendo ProbTotale sulle differenze della Tabella 6,  ottengo una probabilit\[AGrave] di 2^-32. Nella tabella 4 invece si ha probabilit\[AGrave] stimata di 2^-29.15 e sperimentale di 2^-27.34.*)
(*Questa discrepanza \[EGrave] normale perch\[EGrave] nella Tabella 4 sono riportate solo le differenze esterne (delta in e nabla out), e quindi la probabilit\[AGrave] di tale distinguisher boomerang \[EGrave] la somma delle probabilit\[AGrave] di tutti i possibili percorsi intermedi compatibili con gli estremi. Nel mio test invece, visto che tale conto risultava infattibile, ho calcolato la probabilit\[AGrave] di un singolo percorso, avendo fissato le differenze intermedie (delta out e nabla in). Perci\[OGrave] 2^-32 rappresenta un singolo addendo della sommatoria che nel paper d\[AGrave] come risultato 2^-27.34 ed \[EGrave] coerente che la probabilit\[AGrave] del singolo trail sia inferiore rispetto a quella complessiva.*)
(**)
(*Per quanto riguarda la probabilit\[AGrave] "stimata", ossia quella teorica, l'ho calcolata solo per lo switch a 2 round (Em), non avendo gli strumenti per calcolarla per i round differenziali (E0 ed E1).*)
(*Ad ogni modo,  il calcolo per lo switch a 2 round di ProbSwitch, fissate le differenze intermedie sempre della tabella 6, restituisce una probabilit\[AGrave] di circa 2^-10 coerente con la probabilit\[AGrave] empirica misurata con SwitchTest (nel notebook "UBCT, LBCT e Probabilit\[AGrave]  switch a 2 round").*)
(**)
