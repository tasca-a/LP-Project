;;;; -*- Mode: Lisp -*-
;;;; 845150 Tasca Alessandro


;; is-monomial/1
; Ritorna TRUE quando m � un monomio.
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
; Ritorna TRUE quando vp � una lista di varpower.
(defun is-varpower (vp)
  (and (listp vp)
       (eq 'v (first vp))
       (let ((p (varpower-power vp))
             (v (varpower-symbol vp)))
         (and (integerp p)
              (>= p 0)
              (symbolp v)))))

;; is-polynomial/1
; Ritorna TRUE quando p � un polinomio.
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
    (error "Il parametro passato non � un monomio")))

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
(defun as-polynomial  (in)
  (if (is-monomial in)
      (to-poly in)
    (append (list 'poly) (list (zero-c-remove (m-in-p-sum (poly-sort (poly-parse in))))))))       

;===================================================================


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
; Controlla se "in" � un polinomio.
; Se "in" non � un polinomio, esegue il parse.
(defun to-poly (in)
  (cond
   ((is-monomial in) (append (list 'poly) (list (list in))))
   ((is-polynomial in) (append (list 'poly) (poly-sort (list (monomials in)))))
   ((if (or (equal '* (first in)) (atom in)) (to-poly (as-monomial in))
      (as-polynomials in)))))
   ;(t (error "Input non valido!")

;; TOOL
;; poly-sort/1
(defun poly-sort (m)
  (stable-sort (copy-list m) #'comp-d))

;; TOOL
;; comp-d/2
; Ritorna TRUE se "m1" ha grado totale minore di "m2"
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
; (cio� il grado) un numero maggiore o uguale a 0.
(defun basic-monomial-checks (m)
  (if (and (= (length m) 4) (eq 'm (first m)))
      t
    (error "Il parametro passato non � un monomio.")))

;; TOOL
;; varpower-power/1
; Ritorna l'esponente di una variabile
; (v Exp Var)
(defun varpower-power (vp)
  (let ((exp (second vp)))
    (if (numberp exp) exp (error "L'esponente non � un numero."))))

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
;; compare-varpowers/2
; "Compares the variables in monomials with the same TD (total degree)"
; Non l'ho ancora capito.
; Dati:
;   - v1 = (first vps1) -> es. (v 3 a)
;   - v2 = (first vps2) -> es. (v 2 b)
; Ritorna TRUE quando:
;   - v1 � alfabeticamente prima di v2
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