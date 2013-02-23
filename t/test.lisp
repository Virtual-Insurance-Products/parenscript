(in-package #:ps-test)

(defun normalize-whitespace (str)
  (substitute #\Space #\Newline (substitute #\Space #\Tab str)))

(defun same-space-between-statements (code)
  (let ((cl-ppcre:*use-bmh-matchers* nil))
    (cl-ppcre:regex-replace-all "\\s*;\\s*" code "; ")))

(defun remove-duplicate-spaces (str)
  (labels ((spacep (char) (and char (char= char #\Space)))
           (rds (list)
             (cond ((null list)
                    nil)
                   ((and (spacep (first list))
                         (spacep (second list)))
                    (rds (cons #\Space (cddr list))))
                   (t
                    (cons (car list) (rds (cdr list)))))))
    (coerce (rds (coerce str 'list)) 'string)))

(defun trim-spaces (str)
  (string-trim '(#\Space) str))

(defun remove-spaces-near-brackets (str)
  (let ((cl-ppcre:*use-bmh-matchers* nil))
    (reduce (lambda (str rex-pair)
              (cl-ppcre:regex-replace-all (first rex-pair) str (second rex-pair)))
            (cons str '(("\\[ " "[") (" \\]" "]") ("\\( " "(") (" \\)" ")"))))))

(defun normalize-js-code (str)
  (remove-spaces-near-brackets
   (trim-spaces
    (remove-duplicate-spaces
     (same-space-between-statements
      (normalize-whitespace str))))))

(defmacro test-ps-js (testspec parenscript javascript
                      &key (js-target-version *js-target-version*))
  (let ((testname (if (consp testspec) (first testspec) testspec))
        (srcmaps? (destructuring-bind (&key suppress-source-maps?)
                      (when (consp testspec) (cdr testspec))
                    (not suppress-source-maps?))))
    `(progn
       (test ,testname ()
             (is (string= (normalize-js-code ,javascript)
                          (normalize-js-code
                           (let ((*js-target-version* ,js-target-version))
                             (ps-doc* ',parenscript))))))
       ,(when srcmaps?
              ;; upcase because our readtable inverts it
              `(test ,(intern (string-upcase (format nil "~a--~a" testname 'srcmap))) ()
                     (is (string= (normalize-js-code ,javascript)
                                  (normalize-js-code
                                   (let ((*js-target-version* ,js-target-version))
                                     (streamify-and-compile-with-source-maps ',parenscript))))))))))

(defun streamify-and-compile-with-source-maps (form)
  (let ((s (make-string-output-stream)))
    (write form :stream s)
    (let ((parenscript (get-output-stream-string s)))
      (ps::with-blank-compilation-environment
        (with-input-from-string (s parenscript)
          (ps::ps-compile-stream s t))))))

(defun sort-by-js-then-ps-scope (source-maps)
  (sort source-maps
        (lambda (a b)
          (destructuring-bind (aps1 aps2 ajs1 ajs2) a
            (destructuring-bind (bps1 bps2 bjs1 bjs2) b
              (cond ((< ajs1 bjs1) t)
                    ((< bjs1 ajs1) nil)
                    ((< bjs2 ajs2) t) ;; prioritize the one that extends further
                    ((< aps1 bps1) t)
                    ((< bps1 aps1) nil)
                    ((< bps2 aps2) t) ;; ditto
                    ))))))

(defun ps-and-corresponding-js (parenscript)
  (ps::with-blank-compilation-environment
    (with-input-from-string (s parenscript)
      (multiple-value-bind (javascript source-maps) (ps::ps-compile-stream s t)
        (loop :for (ps1 ps2 js1 js2) :in (sort-by-js-then-ps-scope source-maps)
          :collect (list (subseq parenscript ps1 ps2) (subseq javascript js1 js2)))))))

(defun ps-and-js-with-topmost-removed (parenscript)
  (let ((pairs (ps-and-corresponding-js parenscript)))
    (cond ((not (cdr pairs)) pairs)
          (t
           ;; Remove duplicate case where the first ps-js pair is
           ;; identical to the second except for a trailing semicolon
           (destructuring-bind (ps1 js1) (first pairs)
             (destructuring-bind (ps2 js2) (second pairs)
               (if (and (equal ps1 ps2)
                        (= (length js1) (1+ (length js2)))
                        (equal js1 (concatenate 'string js2 ";")))
                   (cdr pairs)
                   pairs)))))))

(defmacro test-source-maps (testspec &body ps-and-js-pairs)
  (let ((testname (if (consp testspec) (first testspec) testspec))
        (parenscript (if (consp testspec) (second testspec) (caar ps-and-js-pairs))))
    `(test ,testname
       (let ((actual (ps-and-js-with-topmost-removed ,parenscript)))
         (is (= (length actual) ,(length ps-and-js-pairs))
             "Expected ~a source mappings, got ~a" ,(length ps-and-js-pairs) (length actual))
         (loop :for (actual-ps actual-js) :in actual
           :for (exp-ps exp-js) :in ',ps-and-js-pairs :for i :from 0 :do
           (is (equal actual-ps exp-ps) "Mapping ~a: expected ps ~s, got ~s" i exp-ps actual-ps)
           (is (equal actual-js exp-js) "Mapping ~a: expected js ~s, got ~s" i exp-js actual-js))))))

(defun jsarray (contents)
  (cl-js:js-array
   (make-array (length contents)
               :initial-contents (mapcar (lambda (x)
                                           (if (listp x)
                                               (jsarray x)
                                               x))
                                         contents)
               :adjustable t)))

(defmacro %test-js-eval (testname parenscript test-statement)
  `(test ,testname ()
     (cl-js:with-js-env ()
       (let ((js-result (cl-js:run-js (ps-doc* ',parenscript))))
         ,test-statement))))

(defmacro test-js-eval (testname parenscript result)
  `(%test-js-eval ,testname ,parenscript
     (is (equalp js-result
                 ,(if (atom result)
                      result
                      `(jsarray ,result))))))

(defmacro test-js-eval-epsilon (testname parenscript result)
  `(%test-js-eval ,testname ,parenscript
     (is (< (abs (- js-result ,result)) 0.0001))))

(def-suite parenscript-tests)
(def-suite output-tests :in parenscript-tests)
(def-suite package-system-tests :in parenscript-tests)
(def-suite eval-tests :in parenscript-tests)
(def-suite sourcemap-tests :in parenscript-tests)

(defun run-tests()
  (format t "Running output tests:~&")
  (run! 'output-tests)
  (format t "Running package system tests:~&")
  (run! 'package-system-tests)
  (format t "Running CL-JavaScript eval tests:~&")
  (run! 'eval-tests)
  (format t "Running source map tests:~&")
  (run! 'sourcemap-tests))
