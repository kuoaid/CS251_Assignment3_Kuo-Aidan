;TODO FIX FORMATTING!!!!!


;helper functions to extract elements of a LEGAL bst.
(define extract-left
  (lambda (bst)
    (car (cdr bst))
    )
  )

(define extract-right
  (lambda (bst)
    (car (cdr (cdr bst)))
    )
  )

(define extract-root
  (lambda (bst)
    (car bst)
    )
)

(define good-bst-length?
  (lambda (bst)
    (equal? (length bst) 3)
  )
)

;HELPER FUNCTION YOU SHOULD USE. This checks if bst is:
; a list,
;has 3 elements, and
;the last 2 elements are lists.
(define is-populated-bst?
  (lambda (bst)
  (cond
    ((not (list? bst)) #f) ;entry is a list
    ((not (good-bst-length? bst)) #f) ;entry has 3 elements (following 2 evaluates the right input)
    ((not (list? (extract-left bst))) #f) ;LEFT element is a list (LAST 2 ARE LISTS)
    ((not (list? (extract-right bst))) #f) ;RIGHT element is a list (LAST 2 ARE LISTS)
    (else #t))
  )
)

(define is-empty-bst?
  (lambda (bst)
      (equal? bst '())
    )
)

; QUESTION ONE.
; These functions respectively return the root node, left subtree, and right subtree of bst.
; If bst is not a list, return #f (Scheme's way of representing false).
; If bst does not contain exactly three entries, for which the last two are lists, return #f.
;(You can use the predicate list? to check if something is a list.)

;PERFECTLY WORKING
(define entry
  (lambda (bst)
    (if (is-populated-bst? bst) (extract-root bst) #f)))

(define left
  (lambda (bst)
  (if (is-populated-bst? bst) (extract-left bst) #f)))

(define right
  (lambda (bst)
  (if (is-populated-bst? bst) (extract-right bst) #f)))


;QUESTION 2
;This function returns a new tree whose root node is elt,
;whose left subtree is left-bst, and whose right subtree is right-bst.
;
;You should check that elt is in fact a number (you can use number?),
;and that left-bst and right-bst are either empty lists or lists of three elements of which the last two are lists.
;Return #f is the input is bad.
;(You don't need to recursively check all the way down to see if the tree is completely well-formed)

(define make-bst
  (lambda (elt left-bst right-bst)
    (cond
      ((not (number? elt)) #f)
      ((not (or (is-populated-bst? left-bst) (is-empty-bst? left-bst))) #f)
      ((not (or (is-populated-bst? right-bst) (is-empty-bst? right-bst))) #f)
      (else (append (append (list elt) (list left-bst)) (list right-bst)))
    )
  )
)

;INORDER WORKING
(define preorder
  (lambda (bst)
    (define left (extract-left bst))
    (define right (extract-right bst))
    (define root (list (extract-root bst)))
    (append
      root
      (append
        (if (not (is-empty-bst? left)) (preorder left) '())
          (if (not (is-empty-bst? right)) (preorder right) '())))
  )
)

(define postorder
  (lambda (bst)
    (define left (extract-left bst))
    (define right (extract-right bst))
    (define root (list (extract-root bst)))
    (append
      (append
        (if (not (is-empty-bst? left)) (preorder left) '())
          (if (not (is-empty-bst? right)) (preorder right) '())
      )
      root
    )
  )
)

(define inorder
  (lambda (bst)
    (define left (extract-left bst))
    (define right (extract-right bst))
    (define root (list (extract-root bst)))
    (append
      (append
        (if (not (is-empty-bst? left)) (preorder left) '())
          root
      )
      (if (not (is-empty-bst? right)) (preorder right) '())
    )
  )
)

;this is robust, but... results are weirdly repeated.
; (define preorder
;   (lambda (bst)
;     (define left (extract-left bst))
;     (define right (extract-right bst))
;     (define root (list (extract-root bst)))
;     (define result (list root))
;     (append result (append (if (not (is-empty-bst? left)) (preorder left)(if (not (is-empty-bst? right)) (preorder right) root)) (if (not (is-empty-bst? right)) (preorder right) root)))
;   )
; )


;so this passes 1 of 2 tests for PREORDER. This works, somewhat, but is definitely NOT robust.
; (define preorder
; (lambda (bst)
;   (define result (list (extract-root bst)))
;   (preorder-helper bst result)
; )
; )
;
; (define preorder-helper
;   (lambda (bst soFar)
;   (define left (extract-left bst))
;   (define right (extract-right bst))
;   (define root (list (extract-root bst)))
;   (cond
;     ((and (is-empty-bst? left) (is-empty-bst? right)) root)
;     ((not (is-empty-bst? left)) (append soFar (append(preorder-helper left soFar) (if (not (is-empty-bst? right)) (preorder-helper right soFar)))))
;     (else soFar)
;   )
;   ;if left is empty, return what you got so far.
;   )
; )
;
; '(1 2 3)
; (preorder '(1 (2 () ()) (3 () ())))
; ;
; '(8 (4 () (6 () (7 () ()))) (28 (22 () (23 () ())) ()))
; '(8 4 6 7 28 22 23)
; (preorder '(8 (4 () (6 () (7 () ()))) (28 (22 () (23 () ())) ())))
