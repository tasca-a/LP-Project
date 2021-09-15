%%%% Polinomi multivariati


%% rappresentazione monomio m(Coefficient, TotalDegree, VarsPowers)
% Coefficient -> coefficiente del monomio
% TotalDegree -> è il grado totale del monomio
% VarsPowers -> è una lista di varpowers
% varpower -> è la rappetensatzione di una variabile elevata ad un esponente E
% v(VarSymbol, Power)


%%ESEMPIO MONOMIO
% m(Coefficient, TotalDegree, VarsPowers).
% m(1, 7, [v(3, s), v(3, t), v(1, y)])

%%is_monomial/1
%ritorna true se l'input è la rappresentazione di un monomio
is_monomial(m(C, TD, VPs)) :-
  integer(C),
  is_var_power_list(VPs),
  get_monomial_degree(VPs, CalculatedTD),
  TD = CalculatedTD.

%%is_var_power/1
% ritorna true se l'input è la rappresentazione di una variabile elevata ad
% una potenza
is_var_power(v(VarSymbol, Power)) :-
  atom(VarSymbol),
  integer(Power),
  Power >= 0.

%%is_var_power_list/1
is_var_power_list(VPs) :-
  is_list(VPs),
  forall(member(VP, VPs), is_var_power(VP)).

%%get_pwr_from_vp/1
% prende in input un VarPower e ne restituisce l'esponente
get_pwr_from_vp(Vp, Result) :-
 Vp =.. TempList,
 last(TempList, Result).

%%get_td/2
%prende come primo parametro la lista di VarPowers e mette il grado totale
%nel secondo parametro
get_monomial_degree([], 0).
get_monomial_degree([Vp|Vps], TotalSum) :-
  get_monomial_degree(Vps, PartialSum),
  get_pwr_from_vp(Vp, Pow),
  TotalSum is Pow + PartialSum.

%%is_polinomial/1
is_polinomial(poly(Monomials)) :-
  is_list(Monomials),
  forall(member(M, Monomials), is_monomial(M)).

%%is_zero/1
% ritorna true per tutte le rappresenzazioni definite dello "0"
is_zero(X) :- X == 0, !.
is_zero(X) :- X == m(0, 0, []) , !.
is_zero(X) :- X == poly([]), !.
