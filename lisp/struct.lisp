;;; Struct metadata vector layout:
;;; vector(size struct-id members printer alignment)
;;; Members:
;;; list(offset name-string type-symbol size/B)

(defun struct-store-type-boxed-p (type)
  "Whether struct members of type TYPE should be stored as a boxed Object."
  (case type
    ((nil character i32 f32)
     nil)
    ((file vector undefined symbol pair primitive closure entity relation string)
     t)
    (t
     (wrong "Invalid primitive type name" type))))

(defun align-of (type)
  "Get the alignment of TYPE."
  (let ((metadata (struct-metadata type)))
    (if metadata
        (struct-metadata-alignment metadata)
        ;; Primitives' alignment is the same as their size.
        (size-of type))))

;;; → (list total-size offsets alignment)
(defun struct-generate-offsets (members prefix offset)
  "Generate getter names with the given PREFIX, and aligned offsets for MEMBERS.
A padding algorithm is used to generate aligned offsets that match those of C."
  (let ((offsets nil)
        (alignment 1))

    (while members
      (let* ((elt (car members))
             (type (cadr elt))
             (member-size (size-of type))
             (member-alignment (align-of type))
             (size 0)
             (name (concat prefix "-" (car elt))))

        ;; A struct's alignment is the maximum of its members' alignments, probably.
        (if (> member-alignment alignment)
            (setq alignment member-alignment))

        (let ((misalignment (% offset member-alignment)))
          (if (> misalignment 0)
              ;; Add padding so this member is aligned
              (incq offset (- member-alignment misalignment))))

        (setq offsets (cons (list
                             offset
                             name
                             type
                             member-size
                             (car elt))
                            offsets))
        (if (struct-metadata type)
            (setq offsets
                  (nconc (cadr (struct-generate-offsets
                                (struct-metadata-members (struct-metadata type))
                                name offset))
                         offsets)))

        (incq offset member-size)

        (setq members (cdr members))))

    (list offset offsets alignment)))

(defun struct-metadata-size (metadata)
  (aref metadata 0))
(defun struct-metadata-id (metadata)
  (aref metadata 1))
(defun struct-metadata-members (metadata)
  (aref metadata 2))
(defun struct-metadata-printer (metadata)
  (aref metadata 3))
(defun struct-metadata-alignment (metadata)
  (aref metadata 4))

(defun struct-printer (struct-type name-string members)
  "Generate a function that prints structs of type STRUCT-TYPE."
  `(defun ,(intern (concat "print-" name-string "-to")) (stream obj)
     ,(concat "Print OBJ of type " name-string " to STREAM.")
     (assert (eq (type-of obj) ',struct-type))
     (fputs ,(concat "#*" name-string) stream)
     (prin1-to stream
               (list . ,(mapcar
                         (lambda (member)
                           `(,(intern (concat name-string "-" (car member))) obj))
                         members)))))

(defun struct-member-types-string (stringy-members)
  "Generate a string listing the members of a struct and their types."
  (apply #'concat
         (mapcar (lambda (entry)
                   (concat "• " (car entry) ": " (symbol-name (cadr entry)) "
"))
                 stringy-members)))

(defun struct-accessors (struct-type offsets member-types-doc)
  "Generate getters and setters for STRUCT-TYPE based on the OFFSETS.
The OFFSETS are produced by struct-generate-offsets."
  (let* ((my-metadata (struct-metadata struct-type))
         (my-name (symbol-name struct-type))
         (my-id (struct-metadata-id my-metadata))
         (my-member-names (mapcar (function car) (struct-metadata-members my-metadata)))
         (my-member-symbols (mapcar (function intern) my-member-names))
         (my-size (struct-metadata-size my-metadata)))
    `(progn
       ;; Type test predicate
       (def-type-predicate ,(intern (concat my-name "p")) ,struct-type)
       ;; A special method to copy a whole struct.
       (defun ,(intern (concat "copy-" my-name)) (dest src)
         ,(concat "Copy the contents of " my-name " DEST to " my-name " SRC.")
         (let ((dest-type (type-of dest)))
           (assert (eq dest-type ',struct-type))
           (assert (eq dest-type (type-of src))))
         (--struct-set-vec dest 0 src ,my-size))
       ;; A setter for every member of a struct.
       (defun ,(intern (concat "set-" my-name))
           ;; Argument list
           (,struct-type . ,my-member-symbols)
         ;; Doc string
         ,(concat "Set the value of each member of " my-name ".
"
                  member-types-doc)
         (progn
           . ,(mapcar
               (lambda (member-name)
                 (list (intern (concat "set-" my-name "-" member-name))
                       struct-type
                       (intern member-name)))
               my-member-names))
         ;; Return the produced struct
         ,struct-type)
       ;; Constructor
       (defun ,(intern (concat "make-" my-name))
           ;; Produce argument list
           ,my-member-symbols
         ;; Doc string
         ,(concat "Create a new " my-name ".
"
                  member-types-doc)
         ;; Allocate the structure
         (let ((me (--struct-allocate ,my-size ,my-id)))
           ;; Assign the initial values by calling out to the "whole struct" setter.
           (,(intern (concat "set-" my-name)) me . ,my-member-symbols)))

       ;; Getters and setters
       . ,(mapcar
           (lambda (entry)
             ;; entry: (offset prefixed-name-string type size/B base-name-string)
             (let* ((offset (car entry))
                    (name (cadr entry))
                    (type (caddr entry))
                    (size (cadddr entry))
                    (base-name (cadddr (cdr entry)))
                    (base-name-sym (intern base-name))
                    (is-struct-type `(assert (eq (type-of ,struct-type) ',struct-type)))
                    (is-member-type `(assert (type-spec-matches ,base-name-sym ',type)))
                    (type-name (symbol-name type))
                    (getter (intern (concat name)))
                    (setter (intern (concat "set-" name)))
                    (getter-doc (concat "Get member " name " of " my-name ", type " type-name "."))
                    (setter-doc (concat "Set member " name " of " my-name ", type " type-name ", to " base-name ".")))
               (print (list "member type of " struct-type ": " type))
               (let ((member-metadata (struct-metadata type)))
                 (if member-metadata
                     ;; Struct member
                     (let ((id (struct-metadata-id member-metadata)))
                       `(progn
                          (defun ,getter (,struct-type)
                            ,getter-doc
                            ,is-struct-type
                            (--struct-get-vec ,struct-type ,offset ,id ,size))
                          (defun ,setter (,struct-type ,base-name-sym)
                            ,setter-doc
                            ,is-struct-type
                            ,is-member-type
                            (--struct-set-vec ,struct-type ,offset ,base-name-sym ,size))))
                     ;; Non-struct member
                     (if (struct-store-type-boxed-p type)
                         ;; Member stored as Object
                         `(progn
                            (defun ,getter (,struct-type)
                              ,getter-doc
                              ,is-struct-type
                              (--struct-get-object ,struct-type ,offset))
                            (defun ,setter (,struct-type ,base-name-sym)
                              ,setter-doc
                              ,is-struct-type
                              ,is-member-type
                              (--struct-set-object ,struct-type ,offset ,base-name-sym)))
                         ;; Member stored as raw Bytes
                         `(progn
                            (defun ,getter (,struct-type)
                              ,getter-doc
                              ,is-struct-type
                              (--struct-get-val ,struct-type ,offset ,(type-tag type) ,size))
                            (defun ,setter (,struct-type ,base-name-sym)
                              ,setter-doc
                              ,is-struct-type
                              ,is-member-type
                              (--struct-set-val ,struct-type ,offset ,base-name-sym ,size))))))))
           offsets))))

(defun prin1-struct-to (stream obj)
  (assert (structp obj))
  (funcall (struct-metadata-printer (struct-metadata (type-of obj))) stream obj))

(defun struct-install (name size offsets printer alignment)
  (let ((id (--struct-register name)))
    (defname 'struct name (vector size id offsets printer alignment))
    id))

(defmacro defstruct (name . members)
  (let* ((name-string (symbol-name name))
         (doc-string
          (if (stringp (car members))
              (let ((custom-doc (car members)))
                (setq members (cdr members))
                custom-doc)
              ""))
         (stringy-members
          (mapcar
           (lambda (member)
             (list (symbol-name (car member)) (cadr member)))
           members))
         (member-types-doc (struct-member-types-string stringy-members))
         (size-and-offsets (struct-generate-offsets stringy-members name-string 0))
         (printer-code (struct-printer name name-string stringy-members)))
    ;; The printer function is not defined at macroexpansion time, so we must defer adding it.
    (if (>= (length name-string) 128)
        (wrong "struct name is way too long" name)

        (struct-install name (car size-and-offsets) stringy-members nil (caddr size-and-offsets))
        `(let ((printer-function ,printer-code))
           ,(struct-accessors name (cadr size-and-offsets) member-types-doc)
           ;; Doc string
           (add-doc-string ',name
                           ,(concat
                             "A struct type.
"
                             doc-string
                             "
Members:
"
                             member-types-doc))

           ;; printer-code is a defun form, so we can get the function name with cadr.
           (aset (struct-metadata ',name) 3 (function ,(cadr printer-code)))))))
