%%%% Polinomi multivariati


%% rappresentazione monomio m(Coefficient, TotalDegree, VarsPowers)
% Coefficient -> coefficiente del monomio
% TotalDegree -> è il grado totale del monomio
% VarsPowers -> è una lista di varpowers
% varpower -> è la rappetensatzione di una variabile elevata ad un esponente E
% v(VarSymbol, Power)


%%is_monomial/1
%ritorna true se l'input è la rappresentazione di un monomio
is_monomial(m(C, TD, VPs)) :-
  integer(C),
  integer(TD),
  TD >= 0,
  is_var_power_list(VPs).


%%is_var_power/1
% ritorna true se l'input è la rappresentazione di una variabile elevata ad
% una potenza
is_var_power(v(VarSymbol, Power)) :-
  integer(Power),
  Power >= 0,
  atom(VarSymbol).

%%is_var_power_list/1
is_var_power_list(VPs) :-
  is_list(VPs),
  forall(member(VP, VPs), is_varpower(VP)).

%%is_polinomial/1
is_polinomial(poly(Monomials)) :-
  is_list(Monomials),
  forall(member(M, Monomials), is_monomial(M)).

%% is_zero/1
% ritorna true per tutte le rappresenzazioni definite dello "0"
is_zero(X) :-
  X == 0, !;
  X == m(0, 0, []), !;
  X == poly([]).
