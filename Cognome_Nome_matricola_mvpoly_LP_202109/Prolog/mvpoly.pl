%%%% -*- Mode: Prolog -*-

%%%% 845150 Tasca Alessandro
%%%% 847020 Suteu Antonio

%%% Polinomi multivariati


%%is_monomial/1
%ritorna true se l'input è la rappresentazione di un monomio
is_monomial(m(C, TD, VPs)) :-
  integer(C),
  is_var_power_list(VPs),
  get_monomial_degree(VPs, CalculatedTD),
  TD = CalculatedTD.

%%is_var_power/1
%ritorna true se l'input è la rappresentazione di una variabile elevata ad
%una potenza
is_var_power(v(Power, VarSymbol)) :-
  atom(VarSymbol),
  integer(Power),
  Power >= 0.

%%is_var_power_list/1
is_var_power_list(VPs) :-
  is_list(VPs),
  forall(member(VP, VPs), is_var_power(VP)).

%%get_vps_from_mono/2
%prende in input un Monomio e ne restituisce la lista sua lista di VarPowers
get_vps_from_mono(m(_, _, Vps), Vps) :- !.

%%get_pwr_from_vp/2
%prende in input un VarPower e ne restituisce l'esponente
get_pwr_from_vp(v(Pwr, _), Pwr) :- !.

%%get_varsymbols_from_vps/2
%prende in input un VarPower e ne restituisce la variabile
get_varsymbols_from_vps([], []) :- !.
get_varsymbols_from_vps([v(_, VarSymbol) | Vps],
                        [VarSymbol | VarList]) :-
  !, get_varsymbols_from_vps(Vps, VarList).

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
get_monomial_coef(m(Coeff, _, _), Coeff) :- !.

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

%%reduce_all_monomials_in_poly/2
%ritorna true se il secondo argomento è il primo argomento (un Poly) dove tutti
%i suoi monomi sono ridotti
reduce_all_monomials_in_poly(poly([]), poly([])) :- !.
reduce_all_monomials_in_poly(poly([H | T]), poly([HRedMono | TRedMonos])) :-
  reduce_monomial_same_var(H, HRedMono),
reduce_all_monomials_in_poly(poly(T), poly(TRedMonos)).


%%reduce_monomial_same_var/2
%ritorna true se ReducedMono è Mono con tutte le variabili moltiplicate
%usato da reduce_all_monomials_in_poly/2
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


%%coefficients/2
%ritorna true quando vero quando Coefficients è una lista dei
%coefficienti di Poly
coefficients(poly([]), []).
coefficients(Poly, CoefList) :-
  to_polynomial(Poly, PolyParsed), !,
  get_poly_coeffs(PolyParsed, CoefList).


%%get_poly_coeffs/2
%usato da coefficients/2
get_poly_coeffs(poly([]), []) :- !.
get_poly_coeffs(poly(Monomials), CoeffList) :-
  get_poly_coeffsCall(poly(Monomials), CoeffList).


%%get_poly_coeffsCall/2
%usato da get_poly_coeffs/2
get_poly_coeffsCall(poly([]), []) :- !.
get_poly_coeffsCall(poly([M | Ms]), [C | Cs]) :-
  get_monomial_coef(M, C),
  get_poly_coeffsCall(poly(Ms), Cs).


%%variables/2
%ritorna true se il secondo argomento è la lista di tutte le varaibili di
%un Poly passato come primo argomento
variables(poly([]), []) :- !.
variables(Poly, Variables) :-
  to_polynomial(Poly, PolyParsed), !,
  get_vars_from_poly(PolyParsed, VarsList),
  get_varsymbols_from_vps(VarsList, UnsortedVarList), !,
  sort(UnsortedVarList, Variables).

%%get_vars_from_poly/2
%ritorna true se il secondo argomento unifica con la lista delle variabili dei
%monomi che formano il Poly passato come primo argomento
get_vars_from_poly(poly([]), []) :- !.
get_vars_from_poly(poly(Monos), VarList) :-
  get_vars_from_polyCall(poly(Monos), VarList2),
  append(VarList2, VarList3),
  sort(2, @=<, VarList3, VarList).

%%get_vars_from_polyCall/2
%usato da get_vars_from_poly/2
get_vars_from_polyCall(poly([]), []) :- !.
get_vars_from_polyCall(poly([FirstMono | RestMono]),
                       [R | ListaVar]) :-
  get_vps_from_mono(FirstMono, R),
  get_vars_from_polyCall(poly(RestMono), ListaVar).

%%monomials/2
%ritorna true quando MonoList è la lista ordinata di tutti i monomi di Poli
monomials(Poly, MonoList) :-
  to_polynomial(Poly, poly(MonoList)).


%%max_degree/2
%ritorna true quando Degree è il grado massimo di tutti i Monomi
%che compaiono in Poly
max_degree(Poly, Degree) :-
  to_polynomial(Poly, poly(OrderedMonoList)),
  last(OrderedMonoList, m(_, Degree, _)).

%%head/2 restituisce il primo elemento di una lista
%usato da min_degree/2
head([H | _], H) :- !.

%%min_degree/2
%ritorna true quando Degree è il grado minimo di tutti i Monomi
%che compaiono in Poly
min_degree(Poly, Degree) :-
  to_polynomial(Poly, poly(OrderedMonoList)),
  head(OrderedMonoList, m(_, Degree, _)).



%%poly_plus/3
%ritrona true se Result è la somma fra P1 e P2 (due polinomi)
poly_plus(P1, P2, Result):-
  to_polynomial(P1, P1Parsed),
  to_polynomial(P2, P2Parsed),
  poly_plusCall(P1Parsed, P2Parsed, Result).


%%poly_plusCall/3
%usato da poly_plus/3 e poly_minus/3
poly_plusCall(poly([]), poly([]), poly([])) :- !.
poly_plusCall(poly(MonoList), poly([]), poly(ResultMonoList)) :-
  sort_monomials_in_polynomial(poly(MonoList), poly(Temp)),
  reduce_all_monomials_in_poly(poly(Temp), poly(ResultMonoList)), !.
poly_plusCall(poly([]), poly(MonoList), poly(ResultMonoList)) :-
  sort_monomials_in_polynomial(poly(MonoList), poly(Temp)),
  reduce_all_monomials_in_poly(poly(Temp), poly(ResultMonoList)), !.
poly_plusCall(poly(ML1), poly(ML2), poly(ResultMonos)) :-
  append(ML1, ML2, TempML1),
  sort_monomials_in_polynomial(poly(TempML1), poly(TempML2)),
  reduce_all_monomials_in_poly(poly(TempML2), poly(TempML3)),
  sum_monomials_same_variable(poly(TempML3), poly(TempML4)),
  remove_mono_coeff_zero(poly(TempML4), poly(ResultMonos)).



%%opposite_polynomial/2
%moltiplica per -1 il coefficiente di ogni monomio che compare in Poly
%(primo argomento), il risultato viene messo nel secondo argomento
negated_polynomial(poly([]), poly([])) :- !.
negated_polynomial(poly([m(Coeff, TD, VPs) | Monos]),
		                poly([m(NegativeCoeff, TD, VPs) | NegatedMonos])) :-
  NegativeCoeff is -Coeff, !,
  negated_polynomial(poly(Monos), poly(NegatedMonos)).


%%poly_minus/2
%ritrona true se Result è la differenza fra P1 e P2 (due polinomi)
%(fa la somma fra il primo poly ed il secondo poly negato)
poly_minus(P1, P2, Result):-
  to_polynomial(P1, P1Parsed),
  to_polynomial(P2, P2Parsed),
  negated_polynomial(P2Parsed, P2Negated),
  poly_plusCall(P1Parsed, P2Negated, Result).



%%poly_times/3
%ritorna true se result è il prodotto fra P1 e P2 (due Polinomi)
poly_times(P1, P2, Result) :-
  to_polynomial(P1, P1Parsed), !,
  to_polynomial(P2, P2Parsed), !,
  poly_timesCall(P1Parsed, P2Parsed, Temp1),
  sort_monomials_in_polynomial(Temp1, Temp2),
  reduce_all_monomials_in_poly(Temp2, Temp3),
  sum_monomials_same_variable(Temp3, Temp4),
  remove_mono_coeff_zero(Temp4, Result).



%%poly_timesCall/3
%usato da poly_times/3
poly_timesCall(poly([]), poly([]), poly([])) :- !.
poly_timesCall(poly([]), poly(_), poly([])) :- !.
poly_timesCall(poly(_), poly([]), poly([])) :- !.
poly_timesCall(poly([H1 | T1]),
               poly([H2 | T2]),
	             poly([HR | T3])) :-
  mono_times(H1, H2, HR),
  poly_timesCall(poly([H1]), poly(T2), poly(T4)),
  poly_timesCall(poly(T1), poly([H2 | T2]), poly(T5)),
  append(T4, T5, T3).



%%mono_times/3
%moltiplica i monomi che si trovato come primo e secondo argomento e mette
%il risultato nel terzo argomento
%usato da poly_timesCall/3
mono_times([], M, M) :- !.
mono_times(M, [], M) :- !.
mono_times(m(_, _, _), m(0, _, _), m(0, 0, [])) :- !.
mono_times(m(0, _, _), m(_, _, _), m(0, 0, [])) :- !.
mono_times(m(Coeff1, TD1, VPs1),
           m(Coeff2, TD2, VPs2),
           m(Coeff, TD, VPs)) :-
  TD is TD1 + TD2, !,
  Coeff is Coeff1 * Coeff2, !,
  sum_same_variables_in_monos(VPs1, VPs2, VPs).


%%sum_same_variables_in_monos/3
%i primi due argomenti sono due liste di varpower e il terzo argomento è la
%lista risultante dalla somma delle variabili simili contenute nei primi 2
%argomenti
%usato da mono_times/3
sum_same_variables_in_monos([], V, V) :- !.
sum_same_variables_in_monos(V, [], V) :- !.
sum_same_variables_in_monos([], [], []) :- !.

sum_same_variables_in_monos([v(Pow1, VarSymbol1) | Vs1],
                   [v(Pow2, VarSymbol2) | Vs2],
		               [v(Pow1, VarSymbol1) | Vs]) :-
  VarSymbol2 @>= VarSymbol1,
  !,
  sum_same_variables_in_monos(Vs1, [v(Pow2, VarSymbol2) | Vs2], Vs).

sum_same_variables_in_monos([v(Pow1, VarSymbol1) | Vs1],
                   [v(Pow2, VarSymbol2) | Vs2],
		               [v(Pow2, VarSymbol2) | Vs]) :-
  VarSymbol1 @>= VarSymbol2,
  !,
  sum_same_variables_in_monos([v(Pow1, VarSymbol1) | Vs1], Vs2, Vs).

sum_same_variables_in_monos([v(Pow1, VarSymbol) | Vs1],
                   [v(Pow2, VarSymbol) | Vs2],
		               [v(Pow, VarSymbol) | Vs]) :-
  Pow is Pow1 + Pow2,
  !,
  sum_same_variables_in_monos(Vs1, Vs2, Vs).


%%poly_val/3
%valuta un polinomio in un punto P
%ritorna true se Value è il valore del Poly valutato nel punto P
%(rappresentato da VariableValues)
%ESEMPIO: Poly = 5a + b^2, VariableValues = [5, 2], Value = 5*5 + 2^2 = 29
poly_val(Poly, VariableValues, Value) :-
  poly_valCall(Poly, VariableValues, Value), nl.


%%poly_valCall/3
%usato da poly_val/3
poly_valCall(Poly, VariableValues, Value) :-
  to_polynomial(Poly, poly(Monos)), !,
  is_list(VariableValues), !,
  sort_monomials_in_polynomial(poly(Monos), poly(SortedMonos)),
  variables(poly(SortedMonos), VarSymbols),
  length(VarSymbols, L1),
  length(VariableValues, L2),
  L1 == L2,
  link_vs_to_vars(VarSymbols, VariableValues, LinkedVarSymbols),
  eval_poly(poly(SortedMonos), LinkedVarSymbols, Value).


%%link_vs_to_vars/3
%ritorna true quando il terzo argomaneto è una lista composto dall'unione
%della prima e seconda lista (primo e secondo argomento rispettivamente)
%la terza lista viene riempita prendendo in modo alternato elementi
%dalla prima e seconda lista
%usato da poly_valCall/3
link_vs_to_vars([], [], []) :- !.
link_vs_to_vars([], T, T) :- !.
link_vs_to_vars([X | Xs], [Y | Ys], [X, Y | Zs]) :-
  link_vs_to_vars(Xs, Ys, Zs).


%%eval_poly/3
%valuta un Poly applicando la sostituzione delle variabili per ogni suo Mono
eval_poly(poly([]), _, 0) :- !.
eval_poly(poly([m(C, TD, VPs) | RestMonos]),
              LinkedVSs,
              Result) :-
  eval_mono(m(C, TD, VPs), LinkedVSs, ValueMono),
  eval_poly(poly(RestMonos), LinkedVSs, ValueRestPoly),
  Result is ValueMono + ValueRestPoly.


%%eval_mono/3
%valuta un Mono in un determinato punto P
eval_mono(m(C, TD, VPs), LinkedVSs, Result) :-
    sub_mono_vars(m(C, TD, VPs), LinkedVSs, m(C, TD, VPSub)),
    eval_vars(VPSub, VarsValue),
    Result is C * VarsValue.


%%sub_mono_vars/3
%ritora true dopo aver sostituito le variabili del monomio con dei valori
%presi da LinkedVSs
sub_mono_vars(m(C, TD, VPs), LinkedVSs, m(C, TD, ListOfVarsOk)) :-
  get_vps_from_mono(m(c, TD, VPs), ListOfVars),
  substitute_vars(ListOfVars, LinkedVSs, ListOfVarsOk).


%%substitute_vars/3
%usato da sub_mono_vars/3
substitute_vars([], _, []) :- !.
substitute_vars([v(Pow, Var)], [Var, NewValue], [v(Pow, NewValue)]) :- !.

substitute_vars([v(Pow, SameVar) | Vs],
                [SameVar, NewValue | Rest],
	              [v(Pow, NewValue) | Others]) :-
  !,
  substitute_vars(Vs, Rest, Others).

substitute_vars([v(Pow, Var)],
                [DiffVar, _NewValue | Rest],
                Others) :-
  Var \= DiffVar, !,
  substitute_vars([v(Pow, Var)], Rest, Others).

substitute_vars([v(Pow, Var) | Vs],
                [Var2, _NewValue | Rest],
                Others) :-
  Var \= Var2, !,
  substitute_vars([v(Pow, Var) | Vs], Rest, Others).


%%eval_vars/2
%usato da eval_mono/3
eval_vars([], 1) :- !.
eval_vars([v(Pow, Base) | RestVs], VarsValue) :-
  Value is Base ** Pow,
  eval_vars(RestVs, ValueRest),
  VarsValue is Value * ValueRest.



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
%usato da sort_monomials_in_polynomial/2
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



%%pprint_polynomial/1
%ritorna true dopo aver stampato sullo "standard output" una rappresentazione
%tradizionale del termine polinomio associato a Polynomial.
%Il simbolo di moltiplicazione può essere omesso.
pprint_polynomial(Poly) :-
  to_polynomial(Poly, PolyParsed), !,
  pprint_polynomialCall(PolyParsed), nl , !.

%%pprint_polynomialCall/1
%usato da pprint_polynomial/1
pprint_polynomialCall(Poly) :-
  is_zero(Poly),
  write('Polynomial is empty.') , !.
pprint_polynomialCall(poly([m(C, 0, [])])) :-
  write(C), !.
pprint_polynomialCall(poly([m(1, _TD, VPs)])) :-
  pprint_varsymbols(VPs).
pprint_polynomialCall(poly([m(C, _TD, VPs)])) :-
  write(C), !,
  pprint_varsymbols(VPs).
pprint_polynomialCall(poly([HeadMono | TailMono])) :-
  pprint_polynomialCall(poly([HeadMono])),
  write(' + '), !,
  pprint_polynomialCall(poly(TailMono)).

%%pprint_varsymbols/1
%stampa sullo 'standard output' tutti glil elementi di VarSymbols
%usato da pprint_polynomialCall/1
pprint_varsymbols([]) :- !.
pprint_varsymbols([v(1, Var)]) :- !, write(Var).
pprint_varsymbols([v(Exp, Var)]) :- Exp = 0, atom(Var), !.
pprint_varsymbols([v(Exp, Var)]) :-
  Exp \= 1, !,
  write(Var),
  write(^),
  write(Exp).
pprint_varsymbols([v(Exp, Var) | VPs]) :-
  pprint_varsymbols([v(Exp, Var)]),
  pprint_varsymbols(VPs).
