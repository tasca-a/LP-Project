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
	
