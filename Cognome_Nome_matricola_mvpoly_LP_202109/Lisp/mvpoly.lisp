;;;; -*- Mode: Lisp -*-
;;;; 845150 Tasca Alessandro
;;;; 847020 Suteu Antonio


;; is-monomial/1
; Ritorna TRUE quando m ? un monomio.
; Controlla che il primo elemento della lista m sia "m",
; che il grado sia >= 0 e che vps sia una lista di varpower.
(defun is-monomial (m)
  (and (listp m)
       (eq 'm (first m))
       (let ((mtd (monomial-degree m))
             (vps (var-powers m)))
         (and (integerp  mtd)
              (>= mtd 0)
              (listp vps)
              (every #'is-varpower vps)))))

;; is-varpower/1
; Ritorna TRUE quando vp ? una lista di varpower.
(defun is-varpower (vp)
  (and (listp vp)
       (eq 'v (first vp))
       (let ((p (varpower-power vp))
             (v (varpower-symbol vp)))
         (and (integerp p)
              (>= p 0)
              (symbolp v)))))

;; is-polynomial/1
; Ritorna TRUE quando p ? un polinomio.
(defun is-polynomial (p)
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (monomials p)))
         (and (listp ms)
              (every #'is-monomial ms)))))

;; is_zero/1
; Ritorna TRUE quando il parametro passato come input ? una
; rappresentazione dello zero.
(defun is_zero (p)
  (if (equal '(poly nil) (as-polynomial p)) t
    (if (equal (second (first (second (as-polynomial p)))) 0) t nil)))

;; var-powers/1
; Ritorna una lista contenente le varpowers di un monomio.
(defun var-powers (m)
  (cond ((basic-monomial-checks m)
         (fourth m))))

;; vars-of/1
; Ritorna la lista delle variabili contenuti in un monomio
(defun vars-of (m)
  (if (basic-monomial-checks m)
      (mapcar (lambda (x) (third x)) (var-powers (mono-sort m)))
    (error "Il parametro passato non e' un monomio")))

;; monomial-degree/1
; Ritorna il grado totale di un monomio.
; Per ora diamo per scontato che m sia un monomio corretto. 
(defun monomial-degree (m)
  (if (basic-monomial-checks m)
      (let ((gt (third m)))
        (if (>= (third m) 0) gt (error "Grado totale del monomio < 0")))
    (error "Il parametro passato non ? un monomio")))

;; monomial-coefficient/1
; Ritorna il coefficiente di un monomio.
(defun monomial-coefficient (m)
  (cond ((is-monomial m) (second m))))

;; coefficients/1
; Ritorna una lista di tutti i coefficienti di un polinomio
(defun coefficients (p)
  (let* ((ms (monomials (to-poly p))))
    (if (not (null ms))
        (mapcar 'monomial-coefficient ms)
      '(0))))

;; variables/1
; Ritorna una lista di tutte le variabili di un polinomio
(defun variables (p)
  (let ((p-parsed (to-poly p)))
    (remove-duplicates 
     (mapcar #'varpower-symbol
             (apply #'append
                    (mapcar #'var-powers (monomials p-parsed)))))))

;; monomials/1
; Ritorna la lista ordinata di tutti i monomi che appaiono nel polinomio
(defun monomials (p)
  (if (not (equal (first p) 'poly))
      (monomials (to-poly p))
    (poly-sort (first (rest p)))))

;; max-degree/1
; Ritorna il grado massimo dei monomi di un polinomio
(defun max-degree (p)
  (monomial-degree (first (last (monomials (to-poly p))))))

;; min-degree/1
; Ritorna il grado minimo dei monomi di un polinomio
(defun min-degree (p)
  (monomial-degree (first (monomials (to-poly p)))))

;; poly-plus/2
; Ritorna il polinomio risultante dalla somma dei due polinomi passati in input
(defun poly-plus (p1 p2)
  (append (list 'poly)
          (list 
           (zero-c-remove 
            (poly-sort 
             (m-in-p-sum 
              (poly-sort
               (append (monomials (to-poly p1))
                       (monomials (to-poly p2))))))))))

;; poly-minus/2
; Ritorna il polinomio risultante dalla differenda dei due 
; polinomi passati in input
(defun poly-minus (p1 p2)
  (append (list 'poly)
          (list
           (zero-c-remove
            (poly-sort
             (m-in-p-sum
              (poly-sort
               (append (monomials (to-poly p1))
                       (sign-inverter (monomials (to-poly p2)))))))))))

;; poly-times/2
; Ritorna il polinomio risultante dalla moltiplicazione dei due
;  polinomi passati in input
(defun poly-times (p1 p2)
  (append (list 'poly)
          (list
           (zero-c-remove
            (poly-sort
             (m-in-p-sum
              (p-multiplier
               (monomials (to-poly p1))
               (monomials (to-poly p2)))))))))

;; as-monomial/1
; Esegue il parse di un monomio riducendolo alla forma base e ordinandolo.
; Dato un input "in", esegue questi passaggi:
; 1- costruisce un monomio non ordianto nella rappresentazione scelta
; 2- ordina il monomio appena generato
; 3- riduce monomio sommando tra loro gli esponenti delle variabili simili
(defun as-monomial (in)
  (mono-reduce
   (mono-sort 
    (mono-parse in)))) ; ex "as-monomial-unordered"

;; as-polynomial/1
; Esegue il parse di un polinomio riducendolo alla forma base e ordinandolo.
(defun as-polynomial (in)
  (if (is-monomial in)
      (to-poly in)
    (append (list 'poly) 
            (list (zero-c-remove (m-in-p-sum (poly-sort (poly-parse in)))))))) 

;; poly-val/2
; Restituisce il valore del polinomio passato in input nel punto n-dimensionale
; passato come secondo parametro
(defun poly-val (p v)
  (if (equal 'poly (first p))
      (if (listp v)
          (let* ((p-parsed (to-poly p))
                 (vs (variables p-parsed))
                 (alt (list-alternator vs v)))
            (if (not (null vs))
                (eval-m (replace-vars (monomials p-parsed) alt))
              (monomial-coefficient (second p))))
        (error "I valori non sono in una lista."))
    (poly-val (to-poly p) v)))

;; pprint-polynomial/1
; Stampa a video un polinomio in una rappresentazione "tradizionale"
(defun pprint-polynomial (p)
  (format t "~a" (format nil "~a" (poly-print (second (to-poly p))))))


;; TOOL
;; replace-vars/2
; Ritorna un monomio costruito sostituendo alle variabili il loro valore
; corrispondente contenuto nella seconda lista passata come parametro
(defun replace-vars (ms alt)
  (if (null (rest ms))
      (list (list 'm 
                  (second (first ms)) 
                  (third (first ms)) 
                  (replace-vps (var-powers (first ms)) alt)))
    (append (list (list 'm
                        (second (first ms))
                        (third (first ms))
                        (replace-vps (var-powers (first ms)) alt)))
            (replace-vars (rest ms) alt))))

;; TOOL
;; replace-vps/2
; Ritorna una lista di varpowers costruita sostituendo alla lista di varpowers
; passata in input i corrispettivi valori contenuti nella seconda lista passata
; come parametro
(defun replace-vps (vps alt)
  (if (null (rest vps))
      (list (replace-vp (first vps) alt))
    (append (list (replace-vp (first vps) alt))
            (replace-vps (rest vps) alt))))

;; TOOL
;; replace-vp/2
; Ritorna una varpower costruita sostituendo alla variabile della varpower
; passata in input il valore contenuto nella seconda lista passata come
; parametro
(defun replace-vp (vp alt)
  (when (not (null alt))
    (if (and (null (third vp))
             (null (second vp)))
        (list 'v 0 0)
      (if (eq (third vp) (first (first alt)))
          (list 'v (second vp) (second (first alt)))
        (replace-vp vp (rest alt))))))

;; TOOL
;; eval-m/1
; Ritorna il valore dei monomi passati in input in un determinato punto
(defun eval-m (ms)
  (if (not (null (rest ms)))
      (+ (* (second (first ms)) (eval-vps (var-powers (first ms))))
         (eval-m (rest ms)))
    (* (second (first ms)) (eval-vps (var-powers (first ms))))))

;; TOOL
;; eval-vps/1
; Ritorna il valore della valutazione delle var-powers in un punto
(defun eval-vps (vps)
  (if (null (rest vps))
      (expt (third (first vps)) (second (first vps)))
    (* (expt (third (first vps)) (second (first vps)))
       (eval-vps (rest vps)))))

;; TOOL
;; list-alternator/2
; Ritorna una lista contenente tutti gli elementi delle due liste passate in
; input messi in maniera alternata
(defun list-alternator (l1 l2)
  (when (not (null l1))
    (if (not (null l2))
        (append (list (list (first l1) (first l2)))
                (list-alternator (rest l1) (rest l2)))
      (error "Non ci sono abbastanza valori da alternare!"))))

;; TOOL
;; p-multiplier/2
; Ritorna una lista di monomi risultante dalla moltiplicazione tra
; le due liste di monomi passate in input, senza nessun ordinamento.
(defun p-multiplier (ms1 ms2)
  (when (and (not (null ms1)) (not (null ms2)))
    (append
     (list (m-multiplier (first ms1) (first ms2)))
     (p-multiplier (list (first ms1)) (rest ms2))
     (p-multiplier (rest ms1) ms2))))

;; TOOL
;; m-multiplier/2
; Ritorna il monomio risultante dalla moltiplicazione tra i due
; monomi passati in input.
(defun m-multiplier (m1 m2)
  (cond
   ((null m1) m2)
   ((null m2) m1)
   (t
    (if (or (= (monomial-coefficient m1) 0)
            (= (monomial-coefficient m2) 0))
        (list 'm 0 0 nil)
      (append (list 'm (* (monomial-coefficient m1) (monomial-coefficient m2))
                    (+ (monomial-degree m1) (monomial-degree m2))
                    (v-multiplier (var-powers m1) (var-powers m2))))))))

;; TOOL
;; v-multiplier/2
; Ritorna una var-power risultante dalla moltiplicazione tra le due
; var-powers ordinate passate in input.
(defun v-multiplier (vps1 vps2)
  (cond
   ((null vps1) vps2)
   ((null vps2) vps1)
   (t
    (let* ((vp1 (first vps1))
           (vp2 (first vps2)))
      (if (equal (varpower-symbol vp1) (varpower-symbol vp2))
          (append (list (list 'v 
                              (+ (varpower-power vp1) (varpower-power vp2)) 
                              (varpower-symbol vp1)))
                  (v-multiplier (rest vps1) (rest vps2)))
        (if (string>= (varpower-symbol vp1) (varpower-symbol vp2))
            (append (list (list 'v (varpower-power vp2) (varpower-symbol vp2)))
                    (v-multiplier vps1 (rest vps2)))
          (append (list (list 'v (varpower-power vp1) (varpower-symbol vp1)))
                  (v-multiplier (rest vps1) vps2))))))))

;; TOOL
;; sign-inverter/1
; Data una lista di monomi in input, inverte il segno di 
; tutti i suoi coefficienti
(defun sign-inverter (ms)
  (when (not (null ms))
    (let* ((m (first ms))
           (c (second m))
           (d (third m))
           (vps (fourth m)))
      (append
       (list (list 'm (- c) d vps))
       (sign-inverter (rest ms))))))

;; TOOL
;; poly-print/1
; Data una lista di monomi che compongono un polinomio, la stampa
; sullo standard output.
(defun poly-print (ms)
  (let ((m (first ms))
        (coeff (second (second ms))))
    (if (null coeff)
        (append (mono-c-and-v m))
      (if (> coeff 0)
          (append (mono-c-and-v m)
                  (list '+)
                  (poly-print (rest ms)))
        (append (mono-c-and-v m) (poly-print (rest ms)))))))

;; TOOL
;; mono-c-and-v/1
; Stampa sullo standard output i coefficienti e le variabili del
; monomio passato in input
(defun mono-c-and-v (m)
  (let ((coeff (second m))
        (var-n-p (fourth m)))
    (if (null var-n-p)
        (append (list coeff))
      (append (list coeff) (list '*) (mono-var var-n-p)))))

;; TOOL
;; mono-var/1
; Stampa le variabili presenti in una struttura di var-powers
(defun mono-var (vps)
  (when (not (null vps))
    (let ((esp (second (first vps)))
          (var (third (first vps))))
      (if (null (rest vps))
          (if (= esp 1)
              (append (list var))
            (append (list var '^ esp)))
        (if (= esp 1)
            (append (list var '*) (mono-var (rest vps)))
          (append (list var '^ esp '*) (mono-var (rest vps))))))))

;; TOOL
;; poly-parse/1
; Esegue il parse dell'input "in" e lo trasforma in un polinomio  nella
; forma canonica, ma senza nessun tipo di ordinazione.
(defun poly-parse (in)
  (if (null in) nil
    (if (atom in)
        (list (as-monomial in))
      (if (is-operator (first in))
          (if (equal (first in) '+)
              (poly-parse (rest in))
            (list (as-monomial in)))
        (if (and (not (null (rest in))) (listp in))
            (append (list (as-monomial (first in)))
                    (poly-parse (rest in)))
          (list (as-monomial (first in))))))))

;; TOOL
;; m-in-p-sum/1
; Somma tra loro i monomi con lo stesso simbolo di variabile in
; una lista di monomi passata in input
(defun m-in-p-sum (ms)
  (cond
   ((null ms) nil)
   ((null (second ms)) ms)
   (t (let* ((m1 (first  ms))
             (m2 (second ms))
             (c1 (monomial-coefficient m1))
             (c2 (monomial-coefficient m2))
             (var-ps1 (var-powers m1))
             (var-ps2 (var-powers m2))
             (d (monomial-degree m1)))
        (if (equal var-ps1 var-ps2)
            (m-in-p-sum (append (list (list 'm (+ c1 c2) d var-ps1))
                                (rest (rest ms))))
          (append (list m1) (m-in-p-sum (rest ms))))))))

;; TOOL
;; zero-c-remove/1
; Ritorna la lista di monomi passata in input senza quelli con
; coefficiente 0
(defun zero-c-remove (ms)
  (when (not (null ms))
    (if (= (monomial-coefficient (first ms)) 0)
        (zero-c-remove (rest ms))
      (append (list (first ms)) (zero-c-remove (rest ms))))))

;; TOOL
;; to-poly/1
; Controlla se "in" ? un polinomio.
; Se "in" non ? un polinomio, esegue il parse.
(defun to-poly (in)
  (cond
   ((is-monomial in) (append (list 'poly) (list (list in))))
   ((is-polynomial in) (append (list 'poly) (poly-sort (list (monomials in)))))
   ((if (or (equal '* (first in)) (atom in)) (to-poly (as-monomial in))
      (as-polynomial in)))))
   ;(t (error "Input non valido!")

;; TOOL
;; poly-sort/1
(defun poly-sort (ms)
  (stable-sort (copy-list ms) #'comp-d))

;; TOOL
;; comp-d/2
; Ritorna TRUE se "m" ha grado totale minore di "r"
(defun comp-d (m r)
  (if (null m)
      nil
    (let ((ds (list (monomial-degree m) (monomial-degree r))))
      (cond 
       ((null m) (not (null r)))
       ((null r) nil)
       ((= (first ds) (second ds)) (compare-varpowers
                                    (var-powers m) (var-powers r)))
       (t (< (first ds) (second ds)))))))


;; TOOL
;; mono-reduce/1
; Somma tra loro gli esponenti simili in un monomio, per
; ridurlo alla forma base.
(defun mono-reduce (m)
  (if (null (var-powers m)) m
    (append (list 'm (monomial-coefficient m) (monomial-degree m))
            (list (varpow-reduce (var-powers m))))))

;; TOOL
;; varpow-reduce/1
; Somma tra loro gli esponenti simili in una struttura di
; var-powers.
(defun varpow-reduce (varpows)
  (when (not (null varpows))
    (if (null (second varpows))
        varpows
      (let* ((varp1 (first varpows))
            (varp2 (second varpows))
            (esp1 (varpower-power varp1))
            (esp2 (varpower-power varp2))
            (v1 (varpower-symbol varp1))
            (v2 (varpower-symbol varp2))
            (coda (rest (rest varpows))))
        (if (null coda)
            (if (equal v1 v2)
                (list (list 'v (+ (eval esp1) (eval esp2)) v1))
              (append (list (list 'v esp1 v1))
                      (list (list 'v esp2 v2))))
          (if (not (null varp2))
              (if (equal v1 v2)
                  (varpow-reduce 
                   (append
                    (list (list 'v (+ (eval esp1) (eval esp2)) v1))
                    coda))
                (append (list (list 'v esp1 v1))
                        (varpow-reduce (rest varpows))))))))))

;; TOOL
;; mono-sort/1
; Ordina le variabili di un monomio in ordine lessicografico
(defun mono-sort (m)
  (let ((nvp (copy-list (var-powers m))))
    (append (list (first m) (second m) (third m))
            (list (sort nvp 'string< :key 'third)))))

;; TOOL
;; basic-monomial-checks/1
; Controlla che il parametro passato sia una lista con 4
; elementi, il primo del quale deve essere "m" e il secondo
; (cio? il grado) un numero maggiore o uguale a 0.
(defun basic-monomial-checks (m)
  (if (and (= (length m) 4) (eq 'm (first m)))
      t
    (error "Il parametro passato non ? un monomio.")))

;; TOOL
;; varpower-power/1
; Ritorna l'esponente di una variabile
; (v Exp Var)
(defun varpower-power (vp)
  (let ((exp (second vp)))
    (if (numberp exp) exp (error "L'esponente non ? un numero."))))

;; TOOL
;; varpower-symbol/1
; Ritorna il simbolo di variabile
; (v Exp Var)
(defun varpower-symbol (vp)
  (let ((sym (third vp)))
    (if (and (atom sym) (not (numberp sym)))
        sym
      (error "Il simbolo di variabile non e' un carattere"))))

;; TOOL
;; mono-parse/1
; Esegue il parse dell'input "in" e lo trasforma in un monomio nella
; forma canonica, ma senza nessun tipo di ordinamento.
(defun mono-parse (in)
  (cond
   ((eval-as-number in) (list 'm (eval in) 0 nil))
   ((atom in) (list 'm 1 1 (list (list 'v 1 in))))
   (t (let ((testa (first in)) (coda (rest in)))
        (if (is-operator testa)
            (cond
             ((equal testa '-) (if (listp (second in))
                                   (parse-negative-pow (second in))
                                 (list 'm -1 1 (list 'v 1 (second in)))))
             ((equal testa '*) (if (eql (build-c coda) 0)
                                   (list 'm 0 0 nil)
                                 (let ((vps (build-vp coda 0)))
                                   (append (list 'm)
                                           (list (build-c coda))
                                           (list (first vps))
                                           (list (rest vps))))))
             ((equal testa '+) (error "Monomio non scritto correttamente.")))
          (if (is-pow-not-parsed testa) (parse-pv testa)
            (list 'm 1 1 (list (list 'v 1 testa)))))))))

;; TOOL
;; parse-pv/1
; Esegue il parse di un input in forma (expt VAR ESP) trasformandolo
; nella forma (v ESP VAR).
(defun parse-pv (pv)
  (when (is-pow-not-parsed pv)
    (if (not (eq (third pv) 0))
        (list 'm 1 (third pv) (list 'v (third vp) (second vp)))
      (list 'm 1 '0 nil))))

;; TOOL
;; build-vp/2
; Costruisce tutte le potenze di un monomio nella forma adeguata
(defun build-vp (m tot-d)
  (let ((testa (first m)) (coda (rest m)))
    (cond
     ((and (listp testa)
           (not (null testa))
           (not (eq (third testa) 0))
           (equal (first testa) 'expt))
      (append (build-vp coda (+ (eval tot-d) (eval (third testa))))
              (list (list 'v (third testa) (second testa)))))
     ((and (listp testa)
           (not (null testa))
           (eq (third testa) 0)
           (equal (first testa) 'expt))
      (append (build-vp coda (+ (eval tot-d) (eval (third testa)))) nil))
     ((and (symbolp testa) (not (null testa)))
      (append (build-vp coda (+ 1 (eval tot-d)))
              (list (list 'v 1 testa))))
     ((numberp (eval testa)) (build-vp coda tot-d))
     ((null testa) (list tot-d)))))

;; TOOL
;; build-c/1
; Esegue la valutazione del coefficiente di un monomio
(defun build-c (m)
  (if (null m) 1
    (if (eval-as-number (first m))
        (* 1 (eval (first m)) (build-c (rest m)))
      (* 1 (build-c (rest m))))))

;; TOOL
;; eval-as-number/1
; Ritorna l'input se la sua valutazione risulta essere un numero,
; altrimenti nil.
(defun eval-as-number (in)
  (let ((result (handler-case (eval in) (error () nil) (warning () nil))))
    (when (numberp result) result)))

;; TOOL
;; parse-negative-pow/1
; Esegue il parse di un input del tipo (expt VAR ESP) 
; trasformandolo nella forma
; (v ESP VAR) e gestisce il coefficiente negativo.
(defun parse-negative-pow (pw)
  (when (is-pow-not-parsed pw)
    (list 'm -1 (third pw) (list 'v (third pw) (second pw)))))

;; TOOL
;; is-pow-parsed/1
; Ritorna TRUE quando il parametro passatogli e' una potenza ancora
; da parsare
(defun is-pow-not-parsed (pw)
  (if (listp pw)
      (if (and (eql (first pw) 'expt)
               (symbolp (second pw))
               (numberp (third pw)))
          T
        nil)
    nil))

;; TOOL
;; is-operator/1
; Ritorna TRUE quando il parametro passatogli e' un operatore
(defun is-operator (c)
  (if (or (eql c '+) (eql c '-) (eql c '*) (eql c '/)) t nil))

;; TOOL
;; compare-varpowers/2
; Dati:
;   - v1 = (first vps1) -> es. (v 3 a)
;   - v2 = (first vps2) -> es. (v 2 b)
; Ritorna TRUE quando:
;   - v1 ? alfabeticamente prima di v2
;   - v1 ha lo stesso simbolo di v2 ma (exp v1) < (exp v2)
(defun compare-varpowers (vps1 vps2)
  (cond
   ((null vps1) (not (null vps2)))
   ((null vps2) nil)
   (t (let ((v1 (first vps1))
            (v2 (first vps2)))
        (cond
         ((string< (third v1) (third v2)) t)
         ((string> (third v1) (third v2)) nil)
         ((and (equal (third v1) (third v2)) (= (second v1) (second v2)))
          (compare-varpowers (rest vps1) (rest vps2)))
         (t (< (second v1) (second v2))))))))