;;;; -*- Mode: Lisp -*-
;;;; 845150 Tasca Alessandro


;; is-monomial/1
; Ritorna TRUE quando m è un monomio.
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
; Ritorna TRUE quando vp è una lista di varpower.
(defun is-varpower (vp)
  (and (listp vp)
       (eq 'v (first vp))
       (let ((p (varpower-power vp))
             (v (varpower-symbol vp)))
         (and (integerp p)
              (>= p 0)
              (symbolp v)))))

;; is-polynomial/1
; Ritorna TRUE quando p è un polinomio.
(defun is-polynomial (p)
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (monomials p)))
         (and (listp ms)
              (every #'is-monomial ms)))))

;; var-powers/1
; Ritorna una lista contenente le varpowers di un monomio.
(defun var-powers (m)
  (cond ((basic-monomial-checks m)
         (fourth m))))

;; vars-of/1
; Ritorna la lista delle variabili contenuti in un monomio
(defun vars-of (m)
  (if (basic-monomial-checks m)
      (mapcar (lambda (x) (third x)) (var-powers m))
    (error "Il parametro passato non e' un monomio"))) ; TODO qua ci vanno altri controlli!

;; monomial-degree/1
; Ritorna il grado totale di un monomio.
; Per ora diamo per scontato che m sia un monomio corretto. 
(defun monomial-degree (m)
  (if (basic-monomial-checks m)
      (let ((gt (third m)))
        (if (>= (third m) 0) gt (error "Grado totale del monomio < 0")))
    (error "Il parametro passato non è un monomio")))

;; monomial-coefficient/1
; Ritorna il coefficiente di un monomio.
(defun monomial-coefficient (m)
  (cond ((is-monomial m) (second m))))

;; monomials/1
; Ritorna la lista ordinata di tutti i monomi che appaiono nel polinomio
; TODO ordinamento! E anche controllo.
(defun monomials (p)
  (if (equal (first p) 'poly)
      (first (rest p)))) ; Spice some things up

;; as-monomial/1
; Esegue il parse di un monomio.
; Dato un input "in", esegue questi passagg:
; 1- costruisce un monomio non ordianto nella rappresentazione scelta
; 2- ordina il monomio appena generato
; 3- riduce monomio sommando tra loro gli esponenti delle variabili simili
(defun as-monomial (in)
  (mono-reduce
   (mono-sort 
    (mono-parse in)))) ; ex "as-monomial-unordered"

;===================================================================

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
                  (varpow-reduce (append
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
; (cioè il grado) un numero maggiore o uguale a 0.
(defun basic-monomial-checks (m)
  (if (and (= (length m) 4) (eq 'm (first m)))
      t
    (error "Il parametro passato non è un monomio.")))

;; TOOL
;; varpower-power/1
; Ritorna l'esponente di una variabile
; (v Exp Var)
(defun varpower-power (vp)
  (let ((exp (second vp)))
    (if (numberp exp) exp (error "L'esponente non è un numero."))))

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
; forma canonica, ma senza nessun tipo di ordinazione.
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
; Ritorna l'input se la sua valutazione risulta essere un numero, altrimenti nil.
(defun eval-as-number (in)
  (let ((result (handler-case (eval in) (error () nil) (warning () nil))))
    (when (numberp result) result)))

;; TOOL
;; parse-negative-pow/1
; Esegue il parse di un input del tipo (expt VAR ESP) trasformandolo nella forma
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
; Ritorna TRUE quando il parametro parratogli e' un operatore
(defun is-operator (c)
  (if (or (eql c '+) (eql c '-) (eql c '*) (eql c '/)) t nil))

;; TOOL
;; compare-degrees/2
; "Compares the degrees of the monomials in a poly"
; TODO Cambia i nomi!
; Non l'ho ancora capito
(defun compare-degrees (first-mono rest-monos))

;; TOOL
;; compare-varpowers/2
; "Compares the variables in monomials with the same TD (total degree)"
; Non l'ho ancora capito.
; Dati:
;   - v1 = (first vps1) -> es. (v 3 a)
;   - v2 = (first vps2) -> es. (v 2 b)
; Ritorna TRUE quando:
;   - v1 è alfabeticamente prima di v2
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