;;; Struct metadata vector layout:
;;; vector(size struct-id members)
;;; Members:
;;; list(offset name-string type-symbol)

;;; → (list total-size offsets)
(defun struct-generate-offsets (members prefix offset)
  (let ((offsets nil))

    (while members
      (let* ((elt (car members))
             (type (cadr elt))
             (member-size (size-of type))
             (size 0)
             (name (concat prefix "-" (car elt))))

        (setq offsets (cons (list
                             offset
                             name
                             type)
                            offsets))
        (if (struct-metadata type)
            (setq offsets
                  (nconc (cadr (struct-generate-offsets
                                (struct-metadata-members (struct-metadata type))
                                name offset))
                         offsets)))

        (incq offset member-size)

        (setq members (cdr members))))

    (list offset offsets)))

(defun struct-metadata-size (metadata)
  (aref metadata 0))
(defun struct-metadata-id (metadata)
  (aref metadata 1))
(defun struct-metadata-members (metadata)
  (aref metadata 2))
(defun struct-metadata-printer (metadata)
  (aref metadata 3))

(defun struct-printer (struct-type name-string members)
  `(defun ,(intern (concat "print-" name-string "-to")) (stream obj)
     (assert (eq (type-of obj) ',struct-type))
     (fputs ,(concat "#*" name-string) stream)
     (prin1-to stream
               (list . ,(mapcar
                         (lambda (member)
                           `(,(intern (concat "get-" name-string "-" (car member))) obj))
                         members)))))

;;; Generate getters and setters for a struct from a description of the offsets
(defun struct-accessors (struct-type offsets)
  (let* ((my-metadata (struct-metadata struct-type))
         (my-name (symbol-name struct-type))
         (my-id (struct-metadata-id my-metadata))
         (my-member-names (mapcar (function car) (struct-metadata-members my-metadata)))
         (my-member-symbols (mapcar (function intern) my-member-names))
         (my-size (struct-metadata-size my-metadata)))
    `(progn
       ;; A special method to copy a whole struct.
       (defun ,(intern (concat "copy-" my-name)) (dest src)
         (let ((dest-type (type-of dest)))
           (assert (eq dest-type ',struct-type))
           (assert (eq dest-type (type-of src))))
         (--struct-set-vec dest 0 src ,(struct-metadata-size my-metadata)))
       ;; Constructor
       (defun ,(intern (concat "make-" my-name))
           ;; Produce argument list
           ,my-member-symbols
         ;; Allocate the structure
         (let ((me (--struct-allocate ,my-size ,my-id)))
           ;; Assign the initial values
           (progn
             . ,(mapcar
                 (lambda (member-name)
                   `(,(intern (concat "set-" my-name "-" member-name))
                      me
                      ,(intern member-name)))
                 my-member-names))
           ;; Return the produced struct
           me))

       ;; Getters and setters
       . ,(mapcar
           (lambda (entry)
             ;; entry: (offset name-string type)
             (let ((offset (car entry))
                   (name (cadr entry))
                   (type (caddr entry)))
               (let ((is-struct-type `(assert (eq (type-of structure) ',struct-type)))
                     (is-member-type `(assert (eq (type-of value) ',type)))
                     (getter (intern (concat "get-" name)))
                     (setter (intern (concat "set-" name))))
                 (print (list "member type of " struct-type ": " type))
                 (let ((member-metadata (struct-metadata type)))
                   (if member-metadata
                       (let ((size (struct-metadata-size member-metadata))
                             (id (struct-metadata-id member-metadata)))
                         (print "... it's a struct type")
                         `(progn
                            (defun ,getter (structure)
                              ,is-struct-type
                              (--struct-get-vec structure ,offset ,id))
                            (defun ,setter (structure value)
                              ,is-struct-type
                              ,is-member-type
                              (--struct-set-vec structure ,offset value ,size))))
                       (print "... it's a cell type")
                       `(progn
                          (defun ,getter (structure)
                            ,is-struct-type
                            (--struct-get-cell structure ,offset))
                          (defun ,setter (structure value)
                            ,is-struct-type
                            ,is-member-type
                            (--struct-set-cell structure ,offset value))))))))
           offsets))))

(defun prin1-struct-to (stream obj)
  (assert (structp obj))
  (funcall (struct-metadata-printer (struct-metadata (type-of obj))) stream obj))

(defun struct-install (name size offsets printer)
  (let ((id (--struct-register name)))
    (defname 'struct name (vector size id offsets printer))
    id))

(defmacro defstruct (name . members)
  (let* ((name-string (symbol-name name))
         (stringy-members
          (mapcar
           (lambda (member)
             (list (symbol-name (car member)) (cadr member)))
           members))
         (size-and-offsets (struct-generate-offsets stringy-members name-string 0))
         (printer-code (struct-printer name name-string stringy-members)))
    ;; The printer function is not defined at macroexpansion time, so we must defer adding it.
    (if (>= (length name-string) 128)
        (wrong "struct name is way too long" struct-type)

        (struct-install name (car size-and-offsets) stringy-members nil)
        `(let ((printer-function ,printer-code))
           ,(struct-accessors name (cadr size-and-offsets))
           ;; printer-code is a defun form, so we can get the function name with cadr.
           (aset (struct-metadata ',name) 3 (function ,(cadr printer-code)))))))
