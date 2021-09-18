%%Polinomi multivariati


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
is_var_power(v(Power, VarSymbol)) :-
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
get_monomial_degree([Vp | Vps], TotalSum) :-
  get_monomial_degree(Vps, PartialSum),
  get_pwr_from_vp(Vp, Pow),
  TotalSum is Pow + PartialSum.

%%get_monomial_coef/2
%prende come primo parametro un Monomial e mette il  suo coefficiente nel
%nel secondo parametro
get_monomial_coef(m(C, _, _), C) :- !.

%%is_polynomial/1
is_polynomial(poly(Monomials)) :-
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

%%as_monomial/2
%ritorna true se Monomial unifica con il Monomial creato attraverso
%il parsing di Expression
as_monomial(Expression, Monomial) :-
  as_monomialCall(Expression, UnreducedMono),
  reduce_monomial_same_var(UnreducedMono, Monomial).


%%as_monomialCall/2
%ritorna true se il secondo argomento è un Monomial parsato ed ordinato
%a parire da Expression (primo argomento)
as_monomialCall(Expression, m(C, TD, VPs)) :-
  as_unsorted_monomial(Expression, m(C, TD, VPs2)),
  sort(2, @=<, VPs2, VPs).


%%as_unsorted_monomial/2
%predicato che parsa il primo argomenzto senza ordinare il Monomio risultante
as_unsorted_monomial(0, m(0, 0, [])) :- !.
as_unsorted_monomial(_ + _, _) :- false.
as_unsorted_monomial(-Mono, m(NegativeC, TD, VPs)) :-
  !,
  as_unsorted_monomial(Mono, m(C, TD, VPs)), !,
  NegativeC is -C.
as_unsorted_monomial(VarSymbol,
                     m(1, 1, [v(1, VarSymbol)])) :-
atom(VarSymbol), !.

as_unsorted_monomial(VarSymbol ^ Power,
                     m(1, 0, [])) :-
  atom(VarSymbol), !,
  integer(Power).
as_unsorted_monomial(VarSymbol ^ Power,
                     m(1, Power, [v(Power, VarSymbol)])) :-
  Power \= 0,
  atom(VarSymbol), !,
  integer(Power), !.

as_unsorted_monomial(Head * Tail,
                     m(C, TD, [v(1, Tail) | VPs])) :-
  atom(Tail),	!,
  as_unsorted_monomial(Head, m(C, TD1, VPs)),
  TD is TD1 + 1.
as_unsorted_monomial(Head * A ^ 0,
                     m(C, TD, VPs)) :-
  atom(A), !,
  as_unsorted_monomial(Head, m(C, TD1, VPs)),
  TD is TD1.
as_unsorted_monomial(Head * A ^ B,
                     m(C, TD, [v(B, A) | VPs])) :-
  number(B), !,
  atom(A), !,
  as_unsorted_monomial(Head, m(C, TD1, VPs)),
  TD is TD1 + B.
as_unsorted_monomial(Coef, m(P, 0, [])) :-
  Coef \= 0, !,
  arithmetic_expression_value(Coef, P).


%%reduce_monomial_same_var/2
%ritorna true se ReducedMono è Mono con tutte le variabili moltiplicate
%ESEMPIO: a^2 * a -> a^3
reduce_monomial_same_var(Mono, ReducedMono) :-
  !,
  reduce_monomial_same_var_call(Mono, ReducedMono).

reduce_monomial_same_var_call(m(0, _, _), m(0, 0, [])) :- !.
reduce_monomial_same_var_call(m(C, 0, []), m(C, 0, [])) :- !.
reduce_monomial_same_var_call(m(C, TD, [v(Exp, Var)]),
                                   m(C, TD, [v(Exp, Var)])) :- !.
reduce_monomial_same_var_call(
    m(C, TD, [v(D1, Var), v(D2, Var) | VPs]),
    m(C, TD, VPsReduced)) :-
  !,
  Z is D1 + D2, !,
  reduce_monomial_same_var(m(C, TD, [v(Z, Var) | VPs]), m(C, TD, VPsReduced)).
reduce_monomial_same_var_call(m(C, TD, [v(D1, Var), v(D2, DiffVar) | VPs]),
		                 m(C, TD, [v(D1, Var) | VPsReduced])) :-
  !,
  reduce_monomial_same_var(m(C, TD, [v(D2, DiffVar) | VPs]),
  m(C, TD, VPsReduced)).

%%get_poly_coeffs/2
%usato da coefficients/2
get_poly_coeffs(poly([]), []) :- !.
get_poly_coeffs(poly(Monomials), CoeffList) :-
  get_poly_coeffsCall(poly(Monomials), CoeffList).

%%get_poly_coeffsCall/2
%usato da get_poly_coeffs/2
get_poly_coeffsCall(poly([]), []) :- !.
get_poly_coeffsCall(poly([M|Ms]), [C|Cs]) :-
  get_monomial_coef(M, C),
  get_poly_coeffsCall(poly(Ms), Cs).

%%coefficients/2
%ritorna true quando vero quando Coefficients è una lista dei
%coefficienti di Poly
coefficients(poly([]), []).
coefficients(Poly, CoefList) :-
  to_polynomial(Poly, PolyParsed), !,
  get_poly_coeffs(PolyParsed, CoefList).


%%as_polynomial/2
%ritorna true se Poly unifica con Expression parsata e senza i monomi con
%coeff pari a zero
as_polynomial(Expression, Poly) :-
  as_polynomialCall(Expression, DirtyPoly),
  remove_mono_coeff_zero(DirtyPoly, Poly).


%%as_polynomialCall/2
%parsa e ordina un poly per grado e poi lo semplifica
as_polynomialCall(m(0, _, _), poly([])) :- !.
as_polynomialCall(m(C, TD, VPs2), poly([m(C, TD, VPs)])) :-
  is_monomial(m(C, TD, VPs2)),
  !,
  sort(2, @=<, VPs2, VPs).
as_polynomialCall(Expression, poly(Monomials)) :-
  as_polynomial_unordered(Expression, poly(Monomials2)),
  sort_monomials_in_polynomial(poly(Monomials2), poly(SortedMonomials)),
  sum_monomials_same_variable(poly(SortedMonomials), poly(Monomials)).
as_polynomial_unordered(MonoHead + MonoTail, poly(Parsed)) :-
  as_monomial(MonoTail, ParsedTail),
  as_polynomial_unordered(MonoHead, poly(ParsedHead)), !,
  append([ParsedTail], ParsedHead, Parsed).
as_polynomial_unordered(MonoHead - MonoTail, poly(Parsed)) :-
  as_monomial(-MonoTail, ParsedTail),
  as_polynomial_unordered(MonoHead, poly(ParsedHead)), !,
  append([ParsedTail], ParsedHead, Parsed).
as_polynomial_unordered(Mono, poly([ParsedMono])) :-
  !, as_monomial(Mono, ParsedMono).


%%to_polynomial/2
%ritorna true se PolyParsed è la versione parsata, ordinata per grado,
%semplificata ed in ordine lessicografico di Poly
to_polynomial(Mono, PolyParsed) :-
    is_monomial(Mono),
    !,
    as_polynomial(Mono, PolyParsed).
to_polynomial(Poly, PolyParsed) :-
    is_polynomial(Poly), !,
    sort_monomials_in_polynomial(Poly, Sorted),
    sum_monomials_same_variable(Sorted, Sum),
    remove_mono_coeff_zero(Sum, PolyParsed).
to_polynomial(Poly, PolyParsed) :-
    as_polynomial(Poly, PolyParsed).


%%sort_monomials_in_polynomial/2
%ritorna true se il secondo argomento è un poly ordinato per grado e
%in ordine lessicografico
sort_monomials_in_polynomial(poly(Monomials), poly(SortedMonomials)) :-
  remove_mono_coeff_zero(poly(Monomials), poly(MonoWithout0s)),
  predsort(compare_monomials, MonoWithout0s, SortedMonomials).


%%compare_monomials/3
compare_monomials(<, m(_C1, TD1, _VPs1), m(_C2, TD2, _VPs2)) :-
  TD1 < TD2, !.
compare_monomials(<, m(_C1, TD, VPs1), m(_C2, TD, VPs2)) :-
  compare_variables(<, VPs1, VPs2), !.
compare_monomials(>, m(_C1, TD1, _VPs1), m(_C2, TD2, _VPs2)) :-
  TD1 > TD2, !.
compare_monomials(>, m(_C1, TD, VPs1), m(_C2, TD, VPs2)) :-
  compare_variables(>, VPs1, VPs2), !.


%%compare_variables/3
%usato da compare_monomials/3 e sort_monomials_in_polynomial/2
compare_variables(<, _, []) :- !.
compare_variables(>, [], _) :- !.
compare_variables(<, v(_, Var1), v(_, Var2)) :-
  Var1 @< Var2, !.
compare_variables(>, v(_, Var1), v(_, Var2)) :-
  Var1 @> Var2, !.
compare_variables(<, v(Exp1, Var), v(Exp2, Var)) :-
  Exp1 < Exp2, !.
compare_variables(>, v(Exp1, Var), v(Exp2, Var)) :-
  Exp1 > Exp2, !.
compare_variables(<, [v(Exp, Var) | Vs1] , [v(Exp, Var) | Vs2]) :-
  !, compare_variables(<, Vs1, Vs2).
compare_variables(<, [v(Exp1, Var) | _] , [v(Exp2, Var) | _]) :-
  Exp1 < Exp2, !.
compare_variables(>, [v(Exp, Var) | Vs1] , [v(Exp, Var) | Vs2]) :-
  !, compare_variables(>, Vs1, Vs2).
compare_variables(>, [v(Exp1, Var) | _] , [v(Exp2, Var) | _]) :-
  Exp1 > Exp2, !.
compare_variables(<, [v(_, Var1) | _Vs1] , [v(_, Var2) | _Vs2]) :-
  Var1 @< Var2, !.
compare_variables(>, [v(_, Var1) | _Vs1] , [v(_, Var2) | _Vs2]) :-
  Var1 @> Var2, !.


%%remove_mono_coeff_zero/2
%serve per rimuovere tutti i Mono con coefficiente uguale a zero da un poly
remove_mono_coeff_zero(poly([]), poly([])) :- !.
remove_mono_coeff_zero(poly([m(0, _, _) | T]), poly(T2)) :-
  !, remove_mono_coeff_zero(poly(T), poly(T2)).
remove_mono_coeff_zero(poly([m(C, TD, VPs) | T]),
                  poly([m(C, TD, VPs) | T2])) :-
  !, remove_mono_coeff_zero(poly(T), poly(T2)).


%%sum_monomials_same_variable/2
%ritorna true se il secondo argomento è un poly con tutte le variabili
%identiche sommate
%ESEMPIO: 2a + a -> 3a
sum_monomials_same_variable(poly([]), poly([])) :- !.
sum_monomials_same_variable(poly([X]), poly([X])) :- !.
sum_monomials_same_variable(poly([m(C1, TD, VPs), m(C2, TD, VPs) | T1]),
                            poly(T2)) :-
  !,
  Z is C1 + C2, !,
  sum_monomials_same_variable(poly([m(Z, TD, VPs) | T1]), poly(T2)).
sum_monomials_same_variable(poly([A, B | T1]), poly([A | T2])) :-
  !, sum_monomials_same_variable(poly([B | T1]), poly(T2)).
