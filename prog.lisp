(defun is-single-op (atm) 
    (and 
        (atom atm) (or (equal atm '+) (equal atm '*))
))

(defun is-double-op (atm) 
    (and
        (atom atm) (equal atm '\|)
))

(defun is-later (atm) 
    (and 
        (atom atm) (not (is-single-op atm)) (not (is-double-op atm))
))

(defun convert-prefix (infix buffer) ; преобразует инфиксную запись регулярного выражения в префиксную
(cond
    ((not infix ) buffer)
    ((is-later (car infix)) (cond
        ((not buffer) (convert-prefix (cdr infix) (cons (car infix) nil)))
        (T (convert-prefix (cdr infix) (list 'seq buffer (cons (car infix) nil))))
    ))
    ((is-single-op (car infix)) (cond 
        ((and (listp buffer) (equal (car buffer) 'seq)) (convert-prefix (cdr infix) (list 'seq (cadr buffer) (list (cond ((equal (car infix) '+) 'plus) (T 'star)) (caddr buffer)))))
        (T (convert-prefix (cdr infix) (list (cond ((equal (car infix) '+) 'plus) (T 'star)) buffer)))
    ))
    ((is-double-op (car infix)) (list 'or buffer (convert-prefix (cdr infix) nil)))

    (T (cond 
        ((not buffer) (convert-prefix (cdr infix) (convert-prefix (car infix) nil)))
        (T (convert-prefix (cdr infix) (list 'seq  buffer (convert-prefix (car infix) nil))))
    ))
))

(defun conv (infix) 
    (print (convert-prefix infix nil))
    (nfa-regexp-comp(convert-prefix infix nil))
)

(defun check-symbol (regexpression) ; проверка на спецсимвол
    (and (atom ( car regexpression)) (not (equal ( car regexpression) 'plus)) (not (equal ( car regexpression) 'or))(not (equal ( car regexpression) 'star)) (not (equal ( car regexpression) 'seq)))
)

(defun nfa-regexp-comp (regexpression)
         (cond ((check-symbol regexpression) (atom-nfa-create regexpression)) ; автомат для атома
               ((equal (car regexpression) 'seq)
                (seq-nfa-create 
                 (append (list(nfa-regexp-comp (cadr regexpression))) (list(nfa-regexp-comp (caddr regexpression)))))) ; автомат для ab
               ((equal (car regexpression) 'or) 
                (or-nfa-create 
                 ( append (list(nfa-regexp-comp (cadr regexpression))) (list(nfa-regexp-comp (caddr regexpression)))))) ; автомат для a|b
               ((equal (car regexpression) 'plus) 
                (plus-nfa-create  (nfa-regexp-comp (car (cdr regexpression)))))	; автомат для а+
               ((equal (car regexpression) 'star)
                (star-nfa-create 
                 (nfa-regexp-comp (car (cdr regexpression)))))	; автомат для а*
               (T NIL))
)


(defun atom-nfa-create (regexpression) ; автомат для атомарного
   (list(list (random 1000) (car regexpression) (random 1000))))

(defun seq-nfa-create (nfa-list) ; автомат для конкатенации
  (nfa-merge-seq (car nfa-list) (cadr nfa-list))) 	

(defun nfa-merge-seq (first-nfa second-nfa)
    (append  first-nfa (list(list (caddar(last first-nfa)) 'epsilon  (caar second-nfa))) second-nfa))


(defun or-nfa-create (nfa-list) ; автомат для альтернативы
   (nfa-merge-or (car nfa-list) (cadr nfa-list))) 
	 
(defun nfa-merge-or (first-nfa second-nfa)
  (let ((fin (random 1000)) (init (random 1000))) (append (list (list init 'epsilon  (caar first-nfa))) (list (list init 'epsilon  (caar second-nfa)))
          first-nfa  second-nfa
        (list (list (caddar(last first-nfa)) 'epsilon fin)) (list (list (caddar(last second-nfa)) 'epsilon  fin)))))

(defun plus-nfa-create (nfa); автомат для +
(let ((fin (random 1000))) (append nfa (list(list (caddar (last nfa))'epsilon  (caar nfa))) 
                                    (list(list  (caddar (last nfa)) 'epsilon fin)))))      

(defun star-nfa-create (nfa) ; автомат для *
(let ((fin (random 1000))) (append nfa (list(list (caddar (last nfa))'epsilon  (caar nfa))) 
                                    (list(list (caar nfa) 'epsilon fin ))
                                    (list(list  (caddar (last nfa)) 'epsilon fin)))))

(print (conv '(#\a \| #\b + #\a  )))
(print (conv '( #\a \| #\y #\x  \| #\a + )))
(print (conv '(#\a + \|   #\c *)))
(print (conv ' (#\a +   #\c * ) ))