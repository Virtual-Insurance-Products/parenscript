(in-package #:parenscript)
(in-readtable :parenscript)

(defparameter *text-extents* nil
  "EQ hash table mapping conses read by the reader to the integer
interval of positions from the stream they were read in from.")

(defun extentify (xt form)
  (when (and *text-extents* xt)
    (unless (or (symbolp form) (numberp form))
      (setf (gethash form *text-extents*) xt)))
  form)

(defun xtent (form)
  "The text extent, if there is one, for FORM."
  (when *text-extents*
    (gethash form *text-extents*)))

(defun make-extent-preserving-readtable (prior-readtable)
  (let ((readtable (copy-readtable prior-readtable)))
    (flet ((wrap (char)
             (let ((curfn (get-macro-character char prior-readtable)))
               (flet ((newfn (stream char)
                        (cond ((and (eq char #\#) (not (eq (peek-char nil stream) #\()))
                               ;; we override # for vectors only
                               (funcall curfn stream char))
                              (t (let ((n (1- (file-position stream)))
                                       (form (funcall curfn stream char))
                                       (n+ (file-position stream)))
                                   (extentify (list n n+) form))))))
                 (set-macro-character char #'newfn nil readtable)))))
      (mapc #'wrap (list #\( #\' #\#))
      readtable)))

(defun extent-union (forms)
  (let (pos1 pos2)
    (loop :for (n n+) :in (mapcar #'xtent forms)
      :when n :do (setf pos1 (min n (or pos1 n)))
      :when n+ :do (setf pos2 (max n+ (or pos2 n+))))
    (when (and pos1 pos2)
      (list pos1 pos2))))

(defun make-progn (body)
  "Make a PROGN node for the forms in BODY with a source mapping
encompassing those forms."
  (extentify (extent-union body) `(progn ,@body)))

(defun make-return-from (tag form)
  "Make a return statement with the same source mapping as the form it returns."
  (extentify (xtent form) `(return-from ,tag ,form)))

(defparameter *source-maps* nil
  "A list associating text extents (intervals of stream positions) of
PS source code with text extents of corresponding JS code. These intervals
are form-based, not line-based; that is, they map s-exprs in the PS
source to the range of JS text emitted for that s-expr.")

(defun report-source-map (xt js1 js2)
  (destructuring-bind (ps1 ps2) xt
    (when (member (list ps1 ps2 js1 js2) *source-maps* :test #'equal)
      (error "Duplicate source map reported: ~a." (list ps1 ps2 js1 js2)))
    (push (list ps1 ps2 js1 js2) *source-maps*)))

(defmacro do-source-map (xt &body body)
  (let ((pos (gensym "pos")))
    `(let ((,pos (when ,xt (file-position *psw-stream*))))
       ,@body
       (when ,xt
         (report-source-map ,xt ,pos (file-position *psw-stream*))))))
