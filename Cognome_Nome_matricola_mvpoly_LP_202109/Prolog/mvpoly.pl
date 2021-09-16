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
%prende in input un VarPower e ne restituisce l'esponente
get_pwr_from_vp(Vp, Result) :-
 Vp =.. TempList,
 last(TempList, Result).

%%get_monomial_degree/2
%prende come primo parametro la lista di VarPowers e mette il grado totale
%nel secondo parametro
get_monomial_degree([], 0).
get_monomial_degree([Vp|Vps], TotalSum) :-
  get_monomial_degree(Vps, PartialSum),
  get_pwr_from_vp(Vp, Pow),
  TotalSum is Pow + PartialSum.

%%get_monomial_coef/2
%prende come primo parametro un Monomial e mette il  suo coefficiente nel
%nel secondo parametro
get_monomial_coef(m(C, _, _), C) :- !.

%%is_polinomial/1
is_polinomial(poly(Monomials)) :-
  is_list(Monomials),
  forall(member(M, Monomials), is_monomial(M)).

%%is_zero/1
%ritorna true per tutte le rappresenzazioni definite dello "0"
is_zero(X) :- X == 0, !.
is_zero(X) :-
  is_monomial(X),
  get_monomial_coef(X, C),
  C == 0, !.
is_zero(X) :- X == poly([]), !.
is_zero(poly([X])) :-
  is_zero(X), !.

%%get_poli_coeffs/2
%usato da coefficients/2
get_poli_coeffs(poly([]), []) :- !.
get_poli_coeffs(poly(Monomials), CoeffList) :-
  get_poli_coeffsCall(poly(Monomials), CoeffList).

%%get_poli_coeffsCall/2
%usato da get_poli_coeffs/2
get_poli_coeffsCall(poly([]), []) :- !.
get_poli_coeffsCall(poly([M|Ms]), [C|Cs]) :-
  get_monomial_coef(M, C),
  get_poli_coeffsCall(poly(Ms), Cs).

%%coefficients/2
%ritorna true quando vero quando Coefficients è una lista dei
%coefficienti di Poly
coefficients([], []).
coefficients(poly(Monomials), CoefList) :-
  get_poli_coeffs(poly(Monomials), CoefList).
