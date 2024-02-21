(defun translate-predicate (predicate)
  (case (type-of predicate)
    ((nil) (cons nil nil))
    ((pair)
     (case (car predicate)
       ((not)
        (cons nil
              ;; Discard the 'and at the front of the translated query structure
              ;; and replace it with 'not.
              (cons 'not (cddr
                          ;; Treat a not as "none of the supplied arguments"
                          (translate-predicate
                           (cons 'and (cdr predicate)))))))
       ((and or)
        (let ((children
               (mapcar (function translate-predicate)
                       (cdr predicate))))
          (cons
           (reduce
            (function union)
            nil
            (mapcar (function car) children))
           (cons (car predicate) (mapcar (function cdr) children)))))
       ;; Allow Components to be required but not fetched.
       ((opt)
        (cons (car (translate-predicate (cadr predicate))) '(and)))
       ((with)
        (cons nil (cdr (translate-predicate (cadr predicate)))))
       ((rel)
        (let ((component (ecs-pair (ecs-lookup-by-name (cadr predicate))
                                   (ecs-lookup-by-name (caddr predicate)))))
          (cons (list component) component)))
       ((t)
        (wrong "Invalid predicate form" predicate))))
    ;; Resolve Component names
    ((symbol)
     (let ((component (ecs-lookup-by-name predicate)))
       (if component
           (cons (list component) component)
           (wrong "Nonexistent Component name" predicate))))
    ((entity relation)
     (cons (list predicate) predicate))
    ((t) (wrong "Invalid form of query" predicate))))
(defun reverse (l)
  (reduce (lambda (acc elem) (cons elem acc)) nil l))
(defun fixup-predicate (predicate)
  (let ((res (translate-predicate predicate)))
    (cons (reverse (car res)) (cdr res))))

(defmacro select predicate
  ;; Implicit and form at top level.
  (let ((res (fixup-predicate (cons 'and predicate))))
    `',res))

(defmacro ecsql (predicate names . body)
  (let* ((query (fixup-predicate predicate))
         (components (car query)))
    `(ecs-do-query ',query
                   (lambda (entity)
                     ((lambda ,names . ,body)
                      . ,(mapcar (lambda (component)
                                   `(ecs-get entity ',component))
                                 components))))))
;;; Example:
;;; (select Pos Vel) → ((vector Pos Vel) . (and Pos Vel))
