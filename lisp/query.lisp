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
          (print (list "Children: " children))
          (cons
           (reduce
            (function union)
            nil
            (mapcar (function car) children))
           (cons (car predicate) (mapcar (function cdr) children)))))
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

(defmacro select predicate
  ;; Implicit and form at top level.
  `(translate-predicate ',(cons 'and predicate)))
;;; Example:
;;; (select Pos Vel) → ((vector Pos Vel) . (and Pos Vel))
