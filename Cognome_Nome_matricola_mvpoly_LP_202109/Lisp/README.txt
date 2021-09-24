%%% -*- Mode: Text -*-

In questo file README verranno illustrate tutte le funzioni
relative al progetto di Linguaggi di Programmazione dell'
appello di settembre 2021.

Questo README è organizzato nel seguente modo:
	- Funzioni richieste dal documento di consegna
	- Funzioni ausiliarie create a supporto delle
	funzioni principali.
	
Da notare che nel file "mvpoly.LISP" le funzioni ausiliarie
sono state commentate anteponendo il termine "TOOL" per
distinguerle meglio.

;; FUNZIONI RICHIESTE

IS-MONOMIAL/1:
	
	is-monomial (m) -> Boolean
	
	Ritorna TRUE quando "m" è un monomio, ovvero una lista
	formattata nel seguente modo:
	'(M Coefficiente Grado-totale Vars-n-powers)
	
IS-VARPOWER/1:
	
	is-varpower (vp) -> Boolean
	
	Ritorna TRUE quando "vp" è una varpower, ovvero una lista
	formattata nel seguente modo:
	'(V Potenza Simbolo-di-variabile)

IS-POLYNOMIAL/1:
	
	is-polynomial (p) -> Boolean
	
	Ritorna TRUE quando "p" è un polinomio, ovvero una lista
	formattata nel seguente modo:
	'(POLY Monomials)
	
IS_ZERO/1:

	is_zero (p) -> Boolean
	
	Ritorna TRUE quando "p" è una rappresentazione dello 0,
	incluso il caso in cui sia proprio 0.
	
VAR-POWERS/1
	
	var-powers (m) -> VP-list
	
	Ritorna la lista di varpowers contenuta nel monomio "m".
	
VARS-OF/1
	
	vars-of (m) -> Variables
	
	Ritorna la lista della variabili contenute nel monomio "m".
	
MONOMIAL-DEGREE/1
	
	monomial-degree (m) -> TotalDegree
	
	Ritorna il grado totale del monomio "m".
	
MONOMIAL-COEFFICIENTS/1
	
	monomial-coefficient (m) -> Coefficient
	
	Ritorna il coefficiente del monomio "m".
	
COEFFICIENTS/1
	
	coefficients (p) -> Coefficients
	
	Ritorna la lista di tutti i coefficienti del polinomio "p".
	
VARIABLES/1
	
	variables (p) -> Variables
	
	Ritorna una lista di tutti i simboli di variabile contenuti
	nel polinomio "p".
	
MONOMIALS/1
	
	monomials (p) -> Monomials
	
	Ritorna una lista ordinata di tutti i monomi che appaiono
	nel polinomio "p".
	
MAX-DEGREE/1
	
	max-degree (p) -> Degree
	
	Ritorna il massimo grado dei monomi che appaiono nel
	polinomio "p".
	
MIN-DEGREE/1

	min-degree (p) -> Degree
	
	Ritorna il grado minimo dei monomi che appaiono nel
	polinomio "p".
	
POLY-PLUS/2
	
	poly-plus (p1 p2) -> Result
	
	Ritorna il polinomio risultante dalla somma tra il
	polinomio "p1" e il polinomio "p2".
	
POLY-MINUS/2
	
	poly-minus (p1 p2) -> Result
	
	Ritorna il polinomio risultante dalla differenza tra il
	polinomio "p1" e il polinomio "p2".

POLY-TIMES/2

	poly-times (p1 p2) -> Result
	
	Ritorna il polinomio risultante dalla moltiplicazione tra
	il polinomio "p1" e il polinomio "p2".
	
AS-MONOMIAL/1

	as-monomial (in) -> Monomial
	
	Ritorna un monomio costruito eseguendo il parse dell'input 
	"in" riducendolo alla forma base e ordinandolo.
	
AS-POLYNOMIAL/1
	
	as-polynomial (in) -> Polynomial
	
	Ritorna un polinomio costruito eseguendo il parse dell'input
	"in" ordinandolo e riducendolo alla forma minima.
	
POLY-VAL/2
	
	poly-val (p v) -> Value
	
	Ritorna il valore del polinomio "p" valutato nel punto
	n-dimensionale "v".
	"v" e' una lista contenente un valore per ogni variabile
	del polinomio.
	
PPRINT-POLYNOMIAL/1
	
	pprint-polynomial (p) -> NIL
	
	Ritorna NIL dopo aver eseguito una stampa del polinomio "p"
	in forma tradizionale sullo standard output.
	
;; FUNZIONI AUSILIARIE

REPLACE-VARS/2
	
	replace-vars (ms alt) -> Monomial
	
	Ritorna un monomio costruito sostituendo alle variabili della 
	lista di monomi "ms" il loro valore corrispondente contenuto
	nella lista "alt" passata come parametro
	
REPLACE-VPS/2
	
	replace-vps (vps alt) -> Varpowers
	
	Ritorna una lista di varpowers costruita sostituendo alla lista 
	di varpowers passata in input ("vps") i corrispettivi valori 
	contenuti nella lista "alt"
	
REPLACE-VP/2

	replace-vp (vp alt) -> Varpower

	Ritorna una varpower costruita sostituendo alla variabile della 
	varpower passata in input ("vp") il valore contenuto nella
	lista "alt".
	
EVAL-M/1
	
	eval-m (ms) -> Value
	
	Ritorna il valore della lista di monomi passati in input ("ms") in 
	un determinato punto.
	
EVAL-VPS/1

	eval-vps (vps) -> Value
	
	Ritorna il valore della valutazione della lista di var-powers ("vps")
	in un punto.
	
LIST-ALTERNATOR/2

	list-alternator (l1 l2) -> Alternate-list
	
	Ritorna una lista contenente tutti gli elementi di "l1" alternati con
	tutti quelli di "l2"
	
P-MULTIPLIER/2

	p-multiplier (ms1 ms2) -> Monomials
	
	Ritorna una lista di monomi risultante dalla moltiplicazione tra le due
	liste di monomi "ms1" e "ms2", senza nessun ordinamento.
	
M-MULTIPLIER/2

	m-multiplier (m1 m2) -> Monomial
	
	Ritorna il monomio risultante dalla moltiplicazione tra i due monomi
	"m1" e "m2".
	
V-MULTIPLIER/2

	v-multiplier (vps1 vps2) -> Varpower
	
	Ritorna una var-power risultante dalla moltiplicazione tra le due varpower
	"vps1" e "vps2" già ordinate.
	
SIGN-INVERTER/1

	sign-inverter (ms) -> Monomials
	
	Ritorna la lista di monomi "ms" con tutti i suoi coefficienti invertiti.
	
POLY-PRINT/1

	poly-print (ms) -> NIL
	
	Stampa sullo standard output la lista di monomi "ms" in formato 
	tradizionale.
	
MONO-C-AND-V/1

	mono-c-and-v (m) -> NIL
	
	Stampa sullo standard output i coefficienti e le variabli del monomio "m".
	
MONO-VAR/1
	
	mono-var (vps) -> NIL
	
	Stampa i simboli di variabile presenti in una lista di varpowers sullo 
	standard output.
	
POLY-PARSE/1
	
	poly-parse (in) -> Polynomial
	
	Ritorna il polinomio risultante dal parsing dell'input "in", trasformandolo
	nella forma canonica dei polinomi senza pero' alcun tipo di ordinamento.
	
M-IN-P-SUM/1
	
	m-in-p-sum (ms) -> Monomials
	
	Somma tra di loro i monomi con gli stessi simboli di variabile facenti parte
	della lista di monomi "ms". Ritorna la lista di monomi risultante.
	
ZERO-C-REMOVE/1

	zero-c-remove (ms) -> Monomials
	
	Ritorna una lista di monomi costruita a partire dalla lista di monomi in
	input "ms" escludendone pero' tutti quelli con coefficiente 0.
	
TO-POLY/1

	to-poly (in) -> Polynomial
	
	Se l'input "in" e' gia' un polinomio lo ritorna inalterato, se invece 
	l'input "in" è un monomio allora ritornera' il polinomio corrispondente.
	
POLY-SORT/1
	
	poly-sort (ms) -> Monomials
	
	Restituisce la lista di monomi "ms" riordinata lessicograficamente.
	
COMP-D/2

	comp-d (m r) -> Boolean
	
	Ritorna TRUE se "m" ha grado totale minore di "r".
	
MONO-REDUCE/1

	mono-reduce (m) -> Monomial
	
	Ritorna il monomio "m" ridotto alla forma base.
	"m" dev'essere gia' ordinato.
	
VARPOW-REDUCE/1

	varpow-reduce (varpows) -> Varpowers
	
	Ritorna la lista di varpowers "varpows" ridotta ai minimi termini.
	"varpows" dev'essere già ordinata.
	
MONO-SORT/1
	
	mono-sort (m) -> Monomial
	
	Ritorna il monomio "m" con le variabili riordinate in modo lessicografico.
	
BASIC-MONOMIAL-CHECKS/1

	basic-monomial-checks (m) -> Boolean
	
	Ritorna TRUE quando "m" risulta essere nella forma corretta per un monomio.
	
VARPOWER-POWER/1

	varpower-power (vp) -> Integer
	
	Ritorna l'esponente della varpower "vp".
	
VARPOWER-SYMBOL/1

	varpower-symbol (vp) -> Character
	
	Ritorna il simbolo di variabile associato alla varpower "vp".
	
MONO-PARSE/1 

	mono-parse (in) -> Monomial
	
	Esegue il parse dell'input "in" e lo trasforma in un monomio nella forma
	canonica, ma senza nessun tipo di ordinamento.
	
PARSE-PV/1

	parse-pv (pv) -> Varpower
	
	Esegue il parse di un input in forma (expt VAR ESP) trasformandolo in
	(v ESP VAR), quindi in una varpower corretta.
	
BUILD-VP/2

	build-vp (m tot-d) -> (Total-Degree Varpowers)
	
	Costruisce tutte le potenze di un monomio nella forma adeguata.
	
BUILD-C/1

	build-c (m) -> Integer
	
	Ritorna un intero che rappresenta la valutazione del coefficiente di un 
	monomio.
	
EVAL-AS-NUMBER/1

	eval-as-number (in) -> Integer
	
	Ritorna l'input "in" se la sua valutazione risulta essere un numero,
	altrimenti NIL.
	
PARSE-NEGATIVE-POW/1

	parse-negative-pow (pw) -> Monomial
	
	Data in input una varpower "pw", ritorna un monomio con coefficiente -1
	se tale varpower risulta avere coefficiente negativo.
	
IS-POW-NOT-PARSED/1

	is-pow-not-parsed (pw) -> Boolean
	
	Ritorna TRUE quando il parametro passatogli e' una potenza sulla quale
	ancora non e' stato eseguito il parse.
	
IS-OPERATOR/1
	
	is-operator (c) -> Boolean
	
	Ritorna TRUE quando il parametro passatogli e' un'operatore.
	
COMPARE-VARPOWERS/2

	compare-varpowers (vps1 vps2) -> Boolean
	
	Ritorna TRUE quando tutte le variabili di "vps1" ordinate alfabeticamente
	sono precedenti rispetto alle variabili di "vps2".