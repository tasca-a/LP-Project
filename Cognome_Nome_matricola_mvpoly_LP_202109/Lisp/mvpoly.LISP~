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

;; monomial-degree/1
; Ritorna il grado totale di un monomio.
(defun monomial-degree (m)
  (cond ((and (= (length m) 4) (eq (first m) 'm))
         (let ((gt (third m)))
           (if (>= (third m) 0) gt (error "Grado totale del monomio < 0"))))))

;; monomial-coefficient/1
; Ritorna il coefficiente di un monomio.
(defun monomial-coefficient (m)
  (cond ((is-monomial m) (second m))))

;; var-powers/1
; Ritorna una lista contenente le varpowers di un monomio.
(defun var-powers (m)
  (cond ((and (= (length m) 4) (eq (first m) 'm))
         (fourth m))))

;; varpower-power/1
; Ritorna l'esponente di una variabile
; (v Exp Var)
(defun varpower-power (vp)
  (let ((exp (second vp)))
    (if (numberp exp) exp (error "L'esponente non è un numero."))))

;; varpower-symbol/1
; Ritorna il simbolo di variabile
; (v Exp Var)
(defun varpower-symbol (vp)
  (let ((sym (third vp)))
    (if (and (atom sym) (not (numberp sym)))
        sym
      (error "Il simbolo di variabile non e' un carattere"))))