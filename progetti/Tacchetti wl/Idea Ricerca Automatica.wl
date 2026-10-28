(* ::Package:: *)

(* ::Input:: *)
(*ClearAll["Global`*"];*)


(* ::Subsection:: *)
(*Framework Ricerca Automatica di Boomerang Distinguisher e Ricerca delle differenze intermedie per lo switch (Em)*)


(* ::Text:: *)
(*Secondo il framework della Sez .4.3 del paper, per la ricerca automatica di caratteristiche boomerang si studiano insieme i comportamenti delle differenze in E0, Em ed E1 e si scelgono le migliori in base al vincolo klb (Sez. 4.2) sul valore della BCT e vedendo quali massimizzino il prodotto delle probabilit\[AGrave] di E0 ed E1. *)
(**)
(*Si divide in 3 step:*)
(*- Passo 1: Si imposta klb = 0 e si massimizza il prodotto delle probabilit\[AGrave] di E0 ed E1 (funzione obiettivo) per trovare il massimo pmax.*)
(*- Passo 2: Il solver SAT enumera tutte le soluzioni che rispettano le condizioni Klb >= k e funz obiettivo >= pmax - t. e le conta in una tabella indicizzata da Din e Nout. *)
(*- Passo 3: Si fissano Din e Nout con il valore pi\[UGrave] alto nella hash table (ossia con il maggior numero di caratteristiche boomerang che rispettano i vincoli al passo 2). Si enumerano di nuovo tutte le soluzioni Dout e Nin con Klb=0 o minore di prima e tali che il prodotto delle probabilit\[AGrave] di E0 ed E1 sia in [pmax \[Minus] tp,pmax]. La somma delle probabilit\[AGrave] \[EGrave] la probabilit\[AGrave] del "cluster", ossia di tutti i possibili percorsi con  Din e Nout agli estremi.*)
(**)
(*Non avendo gli strumenti per la ricerca delle caratteristiche differenziali di E0 ed E1 e per calcolarne le probabilit\[AGrave] teoriche, di seguito mi limiter\[OGrave] a cercare delle buone differenze  Dout e Nin  per il solo switch centraele Em, grazie all'uso del filtro Klb. Saltiamo quindi Passo 1 e 2 e facciamo una versione semplificata del Passo 3.*)
(**)
(* Le possibili quaterne (dL, dR, nL, nR) sono 2^4n, cio\[EGrave] 2^64 per n = 16, allora campiono a caso e mantengo le quaterne tali che klb >= k e BCT > 0, fermandomi dopo max risultati o maxProve. Ovviamente essendo questa "ricerca" svincolata da E0 ed E1 le differenze trovate, seppur ottimizzino localmente lo switch, potrebbero facilmente non essere raggiungibili da buone caratteristiche differenziali.*)


(* ::Input:: *)
(*n=16;*)
(*sBCTPrime=Map[#==1&,IntegerDigits[FromDigits[#,2],2,4]&/@{"0000","0011","0101","0110"},{2}];*)
(*bool[x_]:=Map[#==1&,IntegerDigits[x,2,n]];*)
(*carry[a_,b_,c_]:=Xor[a&&b,a&&c,b&&c];*)
(**)
(*g[x_,y_,u_,v_,dl_,dr_,nl_,nr_]:=Module[{eq1,eq2,eq3,eq4},eq1=Equivalent[v[[1]],carry[x,y,u[[1]]]];*)
(*eq2=Equivalent[v[[2]],carry[Xor[x,y,u[[1]],nl],Not[Xor[y,nr]],u[[2]]]];*)
(*eq3=Equivalent[v[[3]],carry[Xor[x,dl],Xor[y,dr],u[[3]]]];*)
(*eq4=Equivalent[v[[4]],carry[Xor[x,dl,y,dr,u[[3]],nl],Not[Xor[y,dr,nr]],u[[4]]]];*)
(*eq1&&eq2&&eq3&&eq4];*)
(**)
(*TPrimeSAT[uPrime_,u_,dl_,dr_,nl_,nr_]:=Module[{x,y},SatisfiabilityCount[g[x,y,uPrime,u,dl,dr,nl,nr],{x,y}]+SatisfiabilityCount[g[x,y,Map[Not,uPrime],u,dl,dr,nl,nr],{x,y}]];*)
(**)
(*Klb[dLb_,dRb_,nLb_,nRb_]:=Module[{v={0,0,1,0},s=1,klb=0,i=n,new},While[i>=2&&s>0,new=Table[Sum[TPrime[sBCTPrime[[k]],sBCTPrime[[j]],dLb[[i]],dRb[[i]],nLb[[i]],nRb[[i]]]*v[[k]],{k,4}],{j,4}];*)
(*If[Total[new]==4 s,klb++];*)
(*v=new;s=Total[new];i--];*)
(*{klb,4 s}];*)
(*(* klb conta quante volte s_i=4*s_(i-1). L'idea \[EGrave] mantenere le differenze tali che klb >= k (per n=16, k=8) e BCT>0 e mi fermo dopo max risultati o un numero max di prove *)*)
(**)
(*BuoneDifferenze[k_,max_,maxProve_,seme_]:=Block[{TPrime},TPrime[a_,b_,dl_,dr_,nl_,nr_]:=TPrime[a,b,dl,dr,nl,nr]=TPrimeSAT[a,b,dl,dr,nl,nr];(*memoization*)*)
(*SeedRandom[seme];*)
(*Module[{found={},prove=0,dL,dR,nL,nR,r},While[Length[found]<max&&prove<maxProve,prove++;*)
(*{dL,dR,nL,nR}=RandomInteger[{1,2^n-1},4];*)
(*r=Klb[bool[dL],bool[dR],bool[nL],bool[nR]];*)
(*If[r[[2]]>0&&r[[1]]>=k,AppendTo[found,{{dL,dR},{nL,nR},r[[1]],r[[2]]}]]];*)
(*found]];*)


(* ::Input:: *)
(*BuoneDifferenze[8,6,10^4,1]*)


(* ::Input:: *)
(*BuoneDifferenze[10,6,10^4,1]*)
