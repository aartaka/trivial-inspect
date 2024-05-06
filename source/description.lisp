;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :trivial-inspect)

(defgeneric description (object &optional stream)
  (:method :before (object &optional stream)
    (let* ((type (first (uiop:ensure-list (type-of object)))))
      (format stream "~&~@(~a~) " type)))
  (:method (object &optional stream)
    (format stream "~s" object))
  (:documentation "Print human-readable description of OBJECT to STREAM.

Methods should include the most useful information and things that are
not suitable for the `fields' key-value format."))

(defmethod description ((object symbol) &optional stream)
  (if (keywordp object)
      (format stream "~a" object)
      (format stream
              "~a (~a~@[ to ~a~]~@[, ~{~a: ~s~^, ~}~])~@[~* [bound]~]~@[~* [fbound]~]~@[~* [class]~]"
              object
              (symbol-visibility object) (ignore-errors (package-name (symbol-package object)))
              (symbol-plist object)
              (boundp object) (fboundp object) (ignore-errors (find-class object nil)))))

;; TODO: integer binary layout (two's complement?).
(defmethod description ((object integer) &optional stream)
  (format stream
          "~s (~a bit~:p):
#b~b, #o~o, #x~x~
~{~&Universal time: ~2,'0d:~2,'0d:~2,'0d ~
~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~
~a~[th~;st~;nd~;rd~:;th~], year ~a.~} ~
~{~&Approximate UNIX time: ~2,'0d:~2,'0d:~2,'0d ~
~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~
~a~[th~;st~;nd~;rd~:;th~], year ~a.~}"
          object (integer-length object)
          object object object
          (when (>= object 0)
            (multiple-value-bind (second minute hour date month year)
                (decode-universal-time object)
              (list hour minute second month date (mod date 10) year)))
          ;; FIXME: Doesn't account for leap seconds.
          (when (>= object 0)
            (let* ((unix-epoch (encode-universal-time 0 0 0 1 1 1970))
                   (unadjusted-time (+ object unix-epoch)))
              (multiple-value-bind (usecond uminute uhour udate umonth uyear?)
                  (decode-universal-time unadjusted-time)
                (declare (ignorable usecond uminute uhour udate umonth))
                ;; Leap seconds, one per year.
                (multiple-value-bind (usecond uminute uhour udate umonth uyear)
                    (decode-universal-time (+ unadjusted-time (- uyear? 1970)))
                  (list uhour uminute usecond umonth udate (mod udate 10) uyear)))))))

;; TODO: float/double etc. binary layout
(defmethod description ((object float) &optional stream)
  (let ((general (format nil "~s" object))
        (exponential (format nil "~e" object)))
    (format stream "~s ~:[(~e)~;~]" object (equal general exponential) object)))

(defmethod description ((object ratio) &optional stream)
  (format stream "~s (~e)~:[~*~; ~f%~]"
          object object (<= object 1) (* 100 (coerce object 'float))))

(defmethod description ((object complex) &optional stream)
  (format stream "~s (~a+~ai)" object (realpart object) (imagpart object)))

(defmethod description ((object character) &optional stream)
  (if (not (graphic-char-p object))
      (format stream "~s (~d/#x~x)" object (char-code object) (char-code object))
      (format stream "~a (~d/#x~x/~a, ~:[punctuation~;~:[alphabetic~;numeric~]~])"
              object
              (char-code object) (char-code object) (char-name object)
              (alphanumericp object)
              (digit-char-p object))))

(defmethod description ((object cons) &optional stream)
  (if (not (consp (cdr object)))
      (format stream "(~s . ~s)" (car object) (cdr object))
      (call-next-method)))

;; TODO: ECL lists shadowed symbols and used-by list
(defmethod description ((object package) &optional stream)
  (format stream "~a~@[/~{~a~^/~}~] [exports ~a/~a~:[~*~;, uses ~{~a~^, ~}~]]~@[: ~a~]"
          (package-name object)
          (package-nicknames object)
          (length (external-symbols object))
          (length (all-symbols object))
          (package-use-list object)
          (mapcar #'package-name (package-use-list object))
          (documentation object t)))

(defmethod description ((object restart) &optional stream)
  (format stream "~s~@[~* (interactive)~]~@[:
~a~]"
          (restart-name object) (restart-interactive object)
          object))

(defmethod description ((object condition) &optional stream)
  (format stream "~s:
~a"
          object object))

(defmethod description ((object hash-table) &optional stream)
  (format stream "[~a, ~d/~d]~:[
 ~s~;~*~]"
          (let ((test (hash-table-test object)))
            (typecase test
              (function (nth-value 2 (function-lambda-expression test)))
              (t test)))
          (hash-table-count object) (hash-table-size object)
          (zerop (hash-table-count object))
          (loop for key being the hash-key in object
                  using (hash-value val)
                collect (list key val))))

(defmethod description ((object array) &optional stream) ; string too
  (format stream "~{~a~^ ~}[~{~d~^×~}~@[/~d~]]~@[ ~s~]"
          (uiop:ensure-list (array-element-type object))
          (array-dimensions object) (ignore-errors (fill-pointer object))
          object))

(defmethod description ((object stream) &optional stream)
  (labels ((directions (object)
             (uiop:ensure-list
              (cond
                ((typep object 'echo-stream) :echo)
                ((typep object 'broadcast-stream)
                 (mapcar (constantly :out)
                         (broadcast-stream-streams object)))
                ((typep object 'concatenated-stream)
                 (mapcar (constantly :in)
                         (concatenated-stream-streams object)))
                ((typep object 'synonym-stream)
                 (cons :synonym
                       (reduce #'append (mapcar #'directions
                                                (symbol-value (synonym-stream-symbol object))))))
                ((typep object 'two-way-stream) (list :in :out))
                ((input-stream-p object) :in)
                ((output-stream-p object) :out)))))
    (format stream "~{~a~^+~}~@[~a~]~:[~3*~;
~@[ ~a~]~@[#L~d~]~@[-~d~]~]"
            (directions object)
            (uiop:ensure-list (ignore-errors (stream-external-format object)))
            (uiop:file-stream-p object)
            (ignore-errors (pathname object))
            (ignore-errors (file-position object))
            (ignore-errors (file-length object)))))

(defmethod description ((object pathname) &optional stream)
  (format stream "~a~@[ -~*~a-> ~2:*~a~]"
          object
          (cond
            ((uiop:logical-pathname-p object)
             (translate-logical-pathname object))
            ((and (ignore-errors (uiop:native-namestring object))
                  (not (equal (namestring object)
                              (uiop:native-namestring object))))
             (uiop:native-namestring object))
            ((wild-pathname-p object)
             (wild-pathname-p object))
            (t (ignore-errors
                (unless (equal (truename object) object)
                  (truename object)))))
          (cond
            ((uiop:logical-pathname-p object) :logical)
            ((wild-pathname-p object) :wild)
            ((not (equal object (truename object))) :link))))

(defmethod description ((object function) &optional stream)
  (let ((name (nth-value 2 (function-lambda-expression object))))
    (format stream "~:[λ~*~;~a ~](~:[?~*~;~{~a~^ ~}~])~:[~2*~;
 : ~a -> ~a~]~:[~*~;
 ↑ ~s~]~@[
~a~]"
            (and name (symbolp name))
            name
            (not (eq :unknown (trivial-arguments:arglist object)))
            (trivial-arguments:arglist object)
            (not (eq :unknown (trivial-arguments:argtypes object)))
            (nth-value 0 (trivial-arguments:argtypes object))
            (nth-value 1 (trivial-arguments:argtypes object))
            (consp (function-closure-p object))
            (function-closure-p object)
            (documentation object t))))

(-> object-description ((or standard-object structure-object) (or stream boolean)))
(defun object-description (object stream)
  (format stream "~s~@[
~a~]"
          object (or (documentation (class-name (class-of object)) 'type)
                     (documentation (class-name (class-of object)) 'structure))))

(defmethod description ((object standard-object) &optional stream)
  (object-description object stream))

(defmethod description ((object structure-object) &optional stream)
  (object-description object stream))
