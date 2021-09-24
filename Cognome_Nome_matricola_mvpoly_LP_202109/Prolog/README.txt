%%Polinomi multivariati


%% rappresentazione monomio m(Coefficient, TotalDegree, VarsPowers)
% Coefficient -> coefficiente del monomio
% TotalDegree -> è il grado totale del monomio
% VarsPowers -> è una lista di varpowers
% varpower -> è la rappetensatzione di una variabile elevata ad un esponente E
% v(Power, VarSymbol)

%% rappresentazione polinomio poly([lista_di_monomi])


%%ESEMPIO MONOMIO
%m(Coefficient, TotalDegree, VarsPowers).
%m(1, 7, [v(3, s), v(3, t), v(1, y)])

%%is_monomial/1
%ritorna true se l'input è la rappresentazione di un monomio
is_monomial(m(C, TD, VPs))
ESEMPIO: ?- is_monomial(m(2, 2, [v(1,x),v(1,y)])).
true.

%%is_var_power/1
%ritorna true se l'input è la rappresentazione di una variabile elevata ad
%una potenza
is_var_power(v(Power, VarSymbol))

%%is_var_power_list/1
%ritorna true se l'input è un alista di var powers
is_var_power_list(VPs)

%%get_vps_from_mono/2
%prende in input un Monomio e restituisce la sua lista di VarPowers
get_vps_from_mono(m(_, _, Vps), Vps) :- !.

%%get_pwr_from_vp/2
%prende in input un VarPower e ne restituisce l'esponente
get_pwr_from_vp(v(Pwr, _), Pwr) :- !.

%%get_varsymbols_from_vps/2
%prende in input una lista di VP e restituisce una lista di tutte 
%le varaibili 
get_varsymbols_from_vps(VarPowers, Vars)

%%get_monomial_degree/2
%prende come primo parametro la lista di VarPowers e mette il grado totale
%nel secondo parametro
get_monomial_degree(VarPowers, MonoDegree)


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
is_zero(X)

%%as_monomial/2
%ritorna true se Monomial unifica con il Monomial creato attraverso
%il parsing di Expression
as_monomial(Expression, Monomial)


%%as_monomialCall/2
%ritorna true se il secondo argomento è un Monomial parsato ed ordinato
%a parire da Expression (primo argomento)
as_monomialCall(Expression, m(C, TD, VPs))


%%as_unsorted_monomial/2
%predicato che parsa il primo argomenzto senza ordinare il Monomio risultante
as_unsorted_monomial(Expression, Mono) :- !.

%%reduce_all_monomials_in_poly/2
%ritorna true se il secondo argomento è il primo argomento (un Poly) dove tutti
%i suoi monomi sono ridotti
reduce_all_monomials_in_poly(Poly, ReducedPoly)


%%reduce_monomial_same_var/2 e reduce_monomial_same_var_call/2
%ritorna true se ReducedMono è Mono con tutte le variabili moltiplicate
%usato da reduce_all_monomials_in_poly/2
%ESEMPIO: a^2 * a -> a^3
reduce_monomial_same_var(Mono, ReducedMono)
reduce_monomial_same_var_call(Mono, ReducedMono).


%%coefficients/2
%ritorna true quando vero quando Coefficients è una lista dei
%coefficienti di Poly
coefficients(Poly, CoefList)

%%get_poly_coeffs/2
%usato da coefficients/2
get_poly_coeffs(poly(Monomials), CoeffList)

%%get_poly_coeffsCall/2
%usato da get_poly_coeffs/2
get_poly_coeffsCall(poly([Mono|Ms]), [Coeff|Cs]) :-


%%variables/2
%ritorna true se il secondo argomento è la lista di tutte le varaibili ordinate di
%un Poly passato come primo argomento
variables(Poly, Variables) :-

%%get_vars_from_poly/2 e get_vars_from_polyCall/2
%ritorna true se il secondo argomento unifica con la lista delle variabili dei
%monomi che formano il Poly passato come primo argomento
get_vars_from_poly(poly(Monos), VarList) 
get_vars_from_polyCall((poly[Monos]), VarList)


%%monomials/2
%ritorna true quando MonoList è la lista ordinata di tutti i monomi di Poli
monomials(Poly, MonoList)


%%max_degree/2
%ritorna true quando Degree è il grado massimo di tutti i Monomi
%che compaiono in Poly
max_degree(Poly, Degree)

%%head/2 restituisce il primo elemento di una lista
%usato da min_degree/2
head([H | _], H)

%%min_degree/2
%ritorna true quando Degree è il grado minimo di tutti i Monomi
%che compaiono in Poly
min_degree(Poly, Degree)


%%poly_plus/3
%ritrona true se Result è la somma fra P1 e P2 (due polinomi)
poly_plus(P1, P2, Result)


%% poly_plusCall/3
%usato da poly_plus/3 e poly_minus/3
poly_plusCall(poly([]), poly([]), poly([]))



%%opposite_polynomial/2
%moltiplica per -1 il coefficiente di ogni monomio che compare in Poly
%(primo argomento), il risultato viene messo nel secondo argomento
negated_polynomial(Poly, NPoly)

%%poly_minus/2
%ritrona true se Result è la differenza fra P1 e P2 (due polinomi)
%(fa la somma fra il primo poly ed il secondo poly negato)
poly_minus(P1, P2, Result)



%%poly_times/3
%ritorna true se result è il prodotto fra P1 e P2 (due Polinomi)
poly_times(P1, P2, Result)


%%poly_timesCall/3
%usato da poly_times/3
poly_timesCall(poly([]), poly([]), poly([])) :- !.



%%mono_times/3
%moltiplica i monomi che si trovato come primo e secondo argomento e mette
%il risultato nel terzo argomento
%usato da poly_timesCall/3
mono_times(M1, M2, Result)


%%sum_same_variables_in_monos/3
%i primi due argomenti sono due liste di varpower e il terzo argomento è la
%lista risultante dalla somma delle variabili simili contenute nei primi 2
%argomenti
%usato da mono_times/3
sum_same_variables_in_monos(VPs1, VPs2, VPResult) :- !.

%%poly_val/3
%valuta un polinomio in un punto P
%ritorna true se Value è il valore del Poly valutato nel punto P
%(rappresentato da VariableValues)
%ESEMPIO: Poly = 5a + b^2, VariableValues = [5, 2], Value = 5*5 + 2^2 = 29
poly_val(Poly, VariableValues, Value)

%%poly_valCall/3
%usato da poly_val/3
poly_valCall(Poly, VariableValues, Value)


%%link_vs_to_vars/3
%ritorna true quando il terzo argomaneto è una lista composto dall'unione
%della prima e seconda lista (primo e secondo argomento rispettivamente)
%la terza lista viene riempita prendendo in modo alternato elementi
%dalla prima e seconda lista
%usato da poly_valCall/3
link_vs_to_vars([], [], []) 

%%eval_poly/3
%valuta un Poly applicando la sostituzione delle variabili per ogni suo Mono
eval_poly(Poly, LinkedVSs, Result)


%%eval_mono/3
%valuta un Mono in un determinato punto P
eval_mono(Mono, LinkedVSs, Result) :-

%%sub_mono_vars/3
%ritora true dopo aver sostituito le variabili del monomio con dei valori
%presi da LinkedVSs
sub_mono_vars(Mono, LinkedVSs, ResultMono)


%%% substitute_vars/3
%usato da sub_mono_vars/3
substitute_vars([v(Pow, Var)], [Var, NewValue], [v(Pow, NewValue)])


%%eval_vars/2
%suato da eval_mono/3
eval_vars([v(Pow, Base) | RestVs], VarsValue)



%%as_polynomial/2
%ritorna true se Poly unifica con Expression parsata e senza i monomi con
%coeff pari a zero
as_polynomial(Expression, Poly)


%%as_polynomialCall/2
%parsa e ordina un poly per grado e poi lo semplifica
as_polynomialCall(Expression, ParsedPoly)

%%to_polynomial/2
%ritorna true se PolyParsed è la versione parsata, ordinata per grado,
%semplificata ed in ordine lessicografico di Poly
to_polynomial(Expression, PolyParsed)


%%sort_monomials_in_polynomial/2
%ritorna true se il secondo argomento è un poly ordinato per grado e
%in ordine lessicografico
sort_monomials_in_polynomial(poly(Monomials), poly(SortedMonomials))


%%compare_monomials/3
%serve per comparare due monomi dati in input
%usato da sort_monomials_in_polynomial/2
compare_monomials(<, m(_C1, TD1, _VPs1), m(_C2, TD2, _VPs2))


%%compare_variables/3
%usato da compare_monomials/3 e sort_monomials_in_polynomial/2
%serve per comparare due variabili
compare_variables(<, _, []) :- !.



%%remove_mono_coeff_zero/2
%serve per rimuovere tutti i Mono con coefficiente uguale a zero da un poly
remove_mono_coeff_zero(Poly, PolyCleaned)


%%sum_monomials_same_variable/2
%ritorna true se il secondo argomento è un poly con tutte le variabili
%identiche sommate
%ESEMPIO: 2a + a -> 3a
sum_monomials_same_variable(Poly, PolyResult)


%%pprint_polynomial/1
%ritorna true dopo aver stampato sullo "standard output" una rappresentazione
%tradizionale del termine polinomio associato a Polynomial.
%Il simbolo di moltiplicazione può essere omesso.
pprint_polynomial(Poly)


%%pprint_polynomialCall/1
%usato da pprint_polynomial/1
pprint_polynomialCall(Poly)

%%pprint_varsymbols/1
%stampa sullo 'standard output' tutti glil elementi di VarSymbols
%usato da pprint_polynomialCall/1
pprint_varsymbols(VPs)
