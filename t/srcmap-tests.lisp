;; Copying and distribution of this file, with or without
;; modification, are permitted in any medium without royalty. This
;; file is offered as-is, without any warranty.

(in-package #:ps-test)
(named-readtables:in-readtable :parenscript)

(in-suite sourcemap-tests)

(test-source-maps srcmap-function-call
  ("(foo)" "foo()"))

(test-source-maps srcmap-nested-funcall
  ("(foo (bar))" "foo(bar())")
  ("(bar)" "bar()"))

(test-source-maps srcmap-funcall-with-literal-args
  ("(foo 1 \"abc\")" "foo(1, 'abc')"))

(test-source-maps srcmap-explicit-funcall
  ("(funcall foo 1 (bar))" "foo(1, bar())")
  ("(bar)" "bar()"))

(test-source-maps srcmap-this-apply
  ("(apply (foo) (bar) (baz))" "foo().apply(this, [bar()].concat(baz()))")
  ("(foo)" "foo()")
  ("(bar)" "bar()")
  ("(baz)" "baz()"))

(test-source-maps srcmap-object-apply
  ("(apply (@ obj foo) (bar))" "obj.foo.apply(obj, bar())")
  ("(@ obj foo)" "obj.foo")
  ("(bar)" "bar()"))

(test-source-maps srcmap-chain-apply
  ("(apply (getprop (make-an-object) foo 'bar) (baz))"
   "(function () {
    var _js1 = makeAnObject()[foo];
    var _js2 = _js1.bar;
    return _js2.apply(_js1, baz());
})()")
  ("(make-an-object)" "makeAnObject()")
  ;("'bar" "bar")
  ("(baz)" "baz()"))

(test-source-maps srcmap-simple-defun
  ("(defun foo (x y))"
   "function foo(x, y) {
    return null;
}")
  ("(x y)" "(x, y)"))

(test-source-maps srcmap-simple-defun-return
  ("(defun foo () (bar))"
   "function foo() {
    return bar();
}")
  ("(bar)" "return bar()")
  ("(bar)" "bar()"))

(test-source-maps srcmap-defun-statement-then-return
  ("(defun foo () (bar) (baz))"
   "function foo() {
    bar();
    return baz();
}")
  ("(bar)" "bar()")
  ("(baz)" "return baz()")
  ("(baz)" "baz()"))

(test-source-maps srcmap-top-level-lambda
  ("(lambda () (foo))"
   "(function () {
    return foo();
})")
  ("(foo)" "return foo()")
  ("(foo)" "foo()"))

(test-source-maps srcmap-lambda-in-defun
  ("(defun foo () (lambda (x) (bar x)))"
   "function foo() {
    return function (x) {
        return bar(x);
    };
}")
  ("(lambda (x) (bar x))"
   "return function (x) {
        return bar(x);
    }")
  ("(lambda (x) (bar x))"
   "function (x) {
        return bar(x);
    }")
  ("(x)" "(x)")
  ("(bar x)" "return bar(x)")
  ("(bar x)" "bar(x)"))

(test-source-maps srcmap-simple-if-statement
  ("(if a b c)"
   "if (a) {
    b;
} else {
    c;
}"))

(test-source-maps srcmap-simple-if-expression
  ("(defun foo (a b c) (if a b c))"
   "function foo(a, b, c) {
    return a ? b : c;
}")
  ("(a b c)" "(a, b, c)")
  ("(if a b c)" "return a ? b : c")
  ("(if a b c)" "a ? b : c"))

(test-source-maps srcmap-simplest-cond-statement
  ("(cond (t (foo)))"
   "if (true) {
    foo();
}")
  ("(foo)" "{
    foo();
}")
  ("(foo)" "foo()"))

(test-source-maps srcmap-multi-cond-statement
  ("(cond ((foo) (bar)) ((baz) (bing)) (t (biff)))"
   "if (foo()) {
    bar();
} else if (baz()) {
    bing();
} else {
    biff();
}")
  ("(foo)" "foo()")
  ("(bar)" "{
    bar();
}")
  ("(bar)" "bar()")
  ("(baz)" "baz()")
  ("(bing)" "{
    bing();
}")
  ("(bing)" "bing()")
  ("(biff)" "{
    biff();
}")
  ("(biff)" "biff()"))

(test-source-maps srcmap-return-cond
  ("(defun foo () (cond (a (bar)) (t (baz))))"
   "function foo() {
    if (a) {
        return bar();
    } else {
        return baz();
    };
}")
  ("(cond (a (bar)) (t (baz)))"
   "if (a) {
        return bar();
    } else {
        return baz();
    }")
  ("(bar)"
   "{
        return bar();
    }")
  ("(bar)" "return bar()")
  ("(bar)" "bar()")
  ("(baz)"
   "{
        return baz();
    }")
  ("(baz)" "return baz()")
  ("(baz)" "baz()"))

(test-source-maps srcmap-cond-expression
  ("(defun foo () (bar (cond (a (baz)) (t (bing)))))"
   "function foo() {
    return bar(a ? baz() : bing());
}")
  ("(bar (cond (a (baz)) (t (bing))))" "return bar(a ? baz() : bing())")
  ("(bar (cond (a (baz)) (t (bing))))" "bar(a ? baz() : bing())")
  ("(cond (a (baz)) (t (bing)))" "a ? baz() : bing()")
  ("(baz)" "baz()")
  ("(bing)" "bing()"))

(test-source-maps srcmap-switch
  ("(switch (foo) (1) (2 (bar) (baz) break) (default (bing)))"
   "switch (foo()) {
case 1:
case 2:
    bar();
    baz();
    break;
default:
    bing();
}")
  ("(foo)" "foo()")
  ("(1)" "case 1:")
  ("(2 (bar) (baz) break)"
   "case 2:
    bar();
    baz();
    break;")
  ("(bar)" "bar()")
  ("(baz)" "baz()")
  ("(default (bing))"
   "default:
    bing();")
  ("(bing)" "bing()"))

(test-source-maps srcmap-case-statement
  ("(case (foo) ((1 2) (bar) (baz)) (t (bing)))"
   "switch (foo()) {
case 1:
case 2:
    bar();
    baz();
    break;
default:
    bing();
}")
  ("(foo)" "foo()")
  ("((1 2) (bar) (baz))"
   "case 2:
    bar();
    baz();
    break;")
  ("(bar)" "bar()")
  ("(baz)" "baz()")
  ("(t (bing))"
   "default:
    bing();")
  ("(bing)" "bing()"))

(test-source-maps srcmap-return-case
  ("(defun foo (x) (case x ((1 \"one\") (bar)) (2) (t (baz))))"
   "function foo(x) {
    switch (x) {
    case 1:
    case 'one':
        return bar();
    case 2:
        return null;
    default:
        return baz();
    };
}")
  ("(x)" "(x)")
  ("(case x ((1 \"one\") (bar)) (2) (t (baz)))"
   "switch (x) {
    case 1:
    case 'one':
        return bar();
    case 2:
        return null;
    default:
        return baz();
    }")
  ("((1 \"one\") (bar))"
   "case 'one':
        return bar();")
  ;("(1 \"one\")" "'one'")
  ("(bar)" "return bar()")
  ("(bar)" "bar()")
  ("(2)" "case 2:
        return null;")
  ("(t (baz))" "default:
        return baz();")
  ("(baz)" "return baz()")
  ("(baz)" "baz()"))

(test-source-maps srcmap-arithmetic
  ("(* (+ 1 2) 3)" "(1 + 2) * 3")
  ("(+ 1 2)" "1 + 2"))

(test-source-maps srcmap-math-macros
  ("(foo (max x y) (floor x y) (evenp x) (rem x y) (mod x y) (sinh x))"
   "foo(Math.max(x, y), Math.floor(x / y), !(x % 2), x % y, (x % y + y) % y, (Math.exp(x) - Math.exp(-x)) / 2)")
  ("(max x y)" "Math.max(x, y)")
  ("(floor x y)" "Math.floor(x / y)")
  ("(evenp x)" "!(x % 2)")
  ("(rem x y)" "x % y")
  ("(mod x y)" "(x % y + y) % y")
  ("(sinh x)" "(Math.exp(x) - Math.exp(-x)) / 2"))

(test-source-maps srcmap-logic
  ("(and (or x y) (not (z)))" "(x || y) && !z()")
  ("(or x y)" "x || y")
  ("(not (z))" "!z()")
  ("(z)" "z()"))

(test-source-maps srcmap-equality
  ("(= (= a b) (not (= c d)))" "(a === b) === (c !== d)")
  ("(= a b)" "a === b")
  ("(not (= c d))" "c !== d"))

(test-source-maps srcmap-multi-equality
  ("(= x y z)"
   "(function () {
    var _cmp1 = y;
    return x === _cmp1 && _cmp1 === z;
})()"))

(test-source-maps srcmap-not-not-var
  ("(not (not x))" "x;"))

(test-source-maps srcmap-not-not-expr
  ("(not (not (foo)))" "foo()"))

(test-source-maps srcmap-concatenation
  ("(concatenate 'string (foo) (bar))" "foo() + bar()")
  ("(foo)" "foo()")
  ("(bar)" "bar()"))

(test-source-maps srcmap-append
  ("(append (foo) (bar) (baz))" "foo().concat(bar(), baz())")
  ("(foo)" "foo()")
  ("(bar)" "bar()")
  ("(baz)" "baz()"))

(test-source-maps srcmap-create
  ("(create a 1 b (foo))" "({ a : 1, b : foo() })")
  ("(foo)" "foo()"))

(test-source-maps srcmap-create-with-quotes
  ("(create 'test 123 'hello-world 567)" "({ 'test' : 123, 'helloWorld' : 567 })")
  ;; ("'test " "'test'")
  ;; ("'hello-world " "'helloWorld'")
  )

(test-source-maps srcmap-simple-var
  ("(var x (foo y))" "var x = foo(y)")
  ("(foo y)" "foo(y)"))

(test-source-maps srcmap-composite-var
  ("(var x (progn (foo) (bar)))"
   "foo();
var x = bar();")
  ("(foo)" "foo()")
  ("(bar)" "bar()"))

(test-source-maps srcmap-property-access
  ("(@ x y)" "x.y"))

(test-source-maps srcmap-nested-property-access
  ("(@ (@ x y) z)" "x.y.z")
  ("(@ x y)" "x.y"))

(test-source-maps srcmap-chained-property-access1
  ("(@ x y z)" "x.y.z"))

(test-source-maps srcmap-chained-property-access2
  ("(chain (foo x) bar (baz y) bing)" "foo(x).bar.baz(y).bing")
  ("(foo x)" "foo(x)"))

(test-source-maps srcmap-quoted-property-access
  ("(@ (foo) 'y)" "foo().y")
  ("(foo)" "foo()")
  ;; ("'y" "y")
  )

(test-source-maps srcmap-reserved-word-property-access
  ("(getprop x 'break)" "x['break']")
  ;; ("'break" "'break'")
  )

(test-source-maps srcmap-keyword-property-access
  ("(getprop x :y)" "x['y']"))

(test-source-maps srcmap-indexing-property-access
  ("(@ (foo) (bar))" "foo()[bar()]")
  ("(foo)" "foo()")
  ("(bar)" "bar()"))

(test-source-maps srcmap-assignment
  ("(setf x (foo))" "x = foo()") ; x is left out because atoms currently aren't sourcemapped
  ("(foo)" "foo()"))

(test-source-maps srcmap-place-assignment
  ("(setf (elt x y) (foo))" "x[y] = foo()")
  ("(elt x y)" "x[y]")
  ("(foo)" "foo()"))

(test-source-maps srcmap-nested-assignment
  ("(foo (setf x (setf y z)))" "foo(x = (y = z))")
  ("(setf x (setf y z))" "x = (y = z)")
  ("(setf y z)" "y = z"))

(test-source-maps srcmap-assignment-expression
  ("(foo (setf x y))" "foo(x = y)")
  ("(setf x y)" "x = y"))

(test-source-maps srcmap-batch-assignment
  ("(setf x (foo) y (bar))"
   "x = foo();
y = bar();")
  ("(setf x (foo) y (bar))" "x = foo()")
  ("(foo)" "foo()")
  ("(setf x (foo) y (bar))" "y = bar()")
  ("(bar)" "bar()"))

(test-source-maps srcmap-parallel-assignment
  ("(psetf (elt x 0) (foo) (elt y 1) (bar))"
   "(function () {
    var _js1 = foo();
    var _js2 = bar();
    x[0] = _js1;
    return y[1] = _js2;
})()")
  ("(foo)" "foo()")
  ("(bar)" "bar()")
  ("(elt x 0)" "x[0] = _js1")
  ("(elt x 0)" "x[0]")
  ("(elt y 1)" "return y[1] = _js2")
  ("(elt y 1)" "y[1] = _js2")
  ("(elt y 1)" "y[1]"))

(test-source-maps srcmap-assignment-of-properties
  ("(setf (@ x y) (setf (@ z w) (foo)))" "x.y = (z.w = foo())")
  ("(@ x y)" "x.y")
  ("(setf (@ z w) (foo))" "z.w = foo()")
  ("(@ z w)" "z.w")
  ("(foo)" "foo()"))

(test-source-maps srcmap-assignment-with-hoisted-side-effect
  ("(setf x (progn (foo) (bar)))"
   "foo();
x = bar();")
  ("(foo)" "foo()")
  ("(setf x (progn (foo) (bar)))" "x = bar()")
  ("(bar)" "bar()"))

(test-source-maps srcmap-assignment-with-hoisted-side-effect2
  ("(setf x (progn (foo) (progn (bar) (baz))))"
   "foo();
bar();
x = baz();")
  ("(foo)" "foo()")
  ("(bar)" "bar()")
  ("(setf x (progn (foo) (progn (bar) (baz))))" "x = baz()")
  ("(baz)" "baz()"))

(test-source-maps srcmap-assignment-with-comma-delimited-side-effect
  ("(foo (setf x (progn (bar) (baz))))" "foo((bar(), x = baz()))")
  ("(setf x (progn (bar) (baz)))" "bar(), x = baz()")
  ("(bar)" "bar()")
  ("(setf x (progn (bar) (baz)))" "x = baz()")
  ("(baz)" "baz()"))

(test-source-maps srcmap-regex
  ("(foo (regex \"a[bc]*d$\"))" "foo(/a[bc]*d$/)")
  ("(regex \"a[bc]*d$\")" "/a[bc]*d$/"))

(test-source-maps srcmap-array-literal-op
  ("([] (1 2) (3 4))" "[[1, 2], [3, 4]]")
  ("(1 2)" "[1, 2]")
  ("(3 4)" "[3, 4]"))

(test-source-maps srcmap-stringification
  ("(list (stringify \"one\") (stringify (foo) \"two\"))" "['one', [foo(), 'two'].join('')]")
  ;("(stringify \"one\")" "'one'")
  ("(stringify (foo) \"two\")" "[foo(), 'two'].join('')")
  ("(foo)" "foo()"))

(test-source-maps srcmap-incf
  ("(foo (incf x))" "foo(++x)")
  ("(incf x)" "++x"))

(test-source-maps srcmap-incf-args
  ("(progn (incf x (foo)) (incf y 1))"
   "x += foo();
++y;")
  ("(incf x (foo))" "x += foo()")
  ("(foo)" "foo()")
  ("(incf y 1)" "++y"))

(test-source-maps srcmap-nested-incf
  ("(incf x (incf x 3))"
   "(function () {
    var _ps_incr_place1 = (x += 3);
    return x += _ps_incr_place1;
})()")
  ("(incf x 3)" "x += 3"))

(test-source-maps srcmap-quote
  ("'(a b)" "['a', 'b']"))

(test-source-maps srcmap-vector
  ("#(a b)" "['a', 'b']"))

(test-source-maps srcmap-trivial-special-ops
  ("(list (instanceof (foo) Error) (typeof (bar)) (new (baz)))"
   "[(foo() instanceof Error), typeof bar(), new baz()]")
  ("(instanceof (foo) Error)" "(foo() instanceof Error)")
  ("(foo)" "foo()")
  ("(typeof (bar))" "typeof bar()")
  ("(bar)" "bar()")
  ("(new (baz))" "new baz()")
  ("(baz)" "baz()"))

(test-source-maps srcmap-return-from
  ("(defun foo () (when (bar) (return-from foo 123)) (baz))"
   "function foo() {
    if (bar()) {
        return 123;
    };
    return baz();
}")
  ("(when (bar) (return-from foo 123))"
   "if (bar()) {
        return 123;
    }")
  ("(bar)" "bar()")
  ("(return-from foo 123)" "return 123")
  ("(baz)" "return baz()")
  ("(baz)" "baz()"))

(test-source-maps srcmap-try-etc
  ("(try (throw (foo))
        (:catch (error) (bar))
        (:finally (baz)))"
   "try {
    throw foo();
} catch (error) {
    bar();
} finally {
    baz();
}")
  ("(throw (foo))"
   "{
    throw foo();
}")
  ("(throw (foo))" "throw foo()")
  ("(foo)" "foo()")
  ("(:catch (error) (bar))"
   " catch (error) {
    bar();
}")
  ("(bar)"
   "{
    bar();
}")
  ("(bar)" "bar()")
  ("(:finally (baz))"
   " finally {
    baz();
}")
  ("(:finally (baz))"
   "{
    baz();
}")
  ("(baz)" "baz()"))

(test-source-maps srcmap-return-try-etc
  ("(defun abc ()
    (try (bing)
         (:catch (error) (bar))
         (:finally (baz))))"
   "function abc() {
    try {
        return bing();
    } catch (error) {
        return bar();
    } finally {
        baz();
    };
}")
  ("(try (bing)
         (:catch (error) (bar))
         (:finally (baz)))"
   "try {
        return bing();
    } catch (error) {
        return bar();
    } finally {
        baz();
    }")
  ("(bing)"
   "{
        return bing();
    }")
  ("(bing)" "return bing()")
  ("(bing)" "bing()")
  ("(:catch (error) (bar))"
   " catch (error) {
        return bar();
    }")
  ("(bar)"
   "{
        return bar();
    }")
  ("(bar)" "return bar()")
  ("(bar)" "bar()")
  ("(:finally (baz))"
   " finally {
        baz();
    }")
  ("(:finally (baz))"
   "{
        baz();
    }")
  ("(baz)" "baz()"))

(test-source-maps srcmap-lexical-declarations
  ("(let ((a 1) (b 2)))"
   "(function () {
    var a = 1;
    var b = 2;
    return null;
})()")
  ("(a 1)" "var a = 1")
  ("(b 2)" "var b = 2"))

(test-source-maps srcmap-lexical-declarations-in-function
  ("(defun foo () (let ((a 1) (b 2))))"
   "function foo() {
    var a = 1;
    var b = 2;
    return null;
}")
  ("(a 1)" "var a = 1")
  ("(b 2)" "var b = 2"))

(test-source-maps srcmap-var-decl-as-expression
  ("(lambda () (setf x (var y 3)))"
   "(function () {
    var y;
    return x = (y = 3);
})")
  ("(setf x (var y 3))" "return x = (y = 3)")
  ("(setf x (var y 3))" "x = (y = 3)")
  ("(var y 3)" "y = 3"))

(test-source-maps srcmap-optional-arg
  ("(defun foo (&optional (a 1) (b (baz) b?)) (+ a b))" "function foo(a, b) {
    if (a === undefined) {
        a = 1;
    };
    var bwhat = b !== undefined;
    if (!bwhat) {
        b = baz();
    };
    return a + b;
}")
  ("(&optional (a 1) (b (baz) b?))" "(a, b)")
  ("(a 1)" "if (a === undefined) {
        a = 1;
    }")
  ("(b (baz) b?)" "if (!bwhat) {
        b = baz();
    }")
  ("(baz)" "b = baz()")
  ("(baz)" "baz()")
  ("(+ a b)" "return a + b")
  ("(+ a b)" "a + b"))

(test-source-maps srcmap-keyword-arg
  ("(defun foo (&key (a 1) (b (baz) b?)) (+ a b))" "function foo() {
    var _js2 = arguments.length;
    for (var n1 = 0; n1 < _js2; n1 += 2) {
        switch (arguments[n1]) {
        case 'a':
            a = arguments[n1 + 1];
            break;
        case 'b':
            b = arguments[n1 + 1];
            bwhat = true;
        };
    };
    var a = 'undefined' === typeof a ? 1 : a;
    var bwhat;
    var b = 'undefined' === typeof b ? baz() : b;
    return a + b;
}")
  ;("(&key (a 1) (b (baz) b?))" "()")
  ("(a 1)" "var a = 'undefined' === typeof a ? 1 : a")
  ("(b (baz) b?)" "var b = 'undefined' === typeof b ? baz() : b")
  ("(baz)" "baz()")
  ("(+ a b)" "return a + b")
  ("(+ a b)" "a + b"))

(test-source-maps srcmap-rest-arg
  ("(defun foo (a &rest bs) (+ a (length bs)))" "function foo(a) {
    var bs = [];
    for (var i1 = 0; i1 < arguments.length - 1; i1 += 1) {
        bs[i1] = arguments[i1 + 1];
    };
    return a + bs.length;
}")
  ("(a &rest bs)" "(a)")
  ("(+ a (length bs))" "return a + bs.length")
  ("(+ a (length bs))" "a + bs.length")
  ("(length bs)" "bs.length"))

(test-source-maps srcmap-simple-macro
  ("(progn (defmacro mac1 (x) `(foo ,x)) (bar (mac1 a) (mac1 b)))"
   "bar(foo(a), foo(b));")
  ("(bar (mac1 a) (mac1 b))" "bar(foo(a), foo(b))")
  ("(mac1 a)" "foo(a)")
  ("(mac1 b)" "foo(b)"))

(test-source-maps srcmap-nested-macro-call
  ("(progn (defmacro mac2 (x) `(bar ,x)) (mac2 (mac2 123)))" "bar(bar(123));")
  ("(mac2 (mac2 123))" "bar(bar(123))")
  ("(mac2 123)" "bar(123)"))

(test-source-maps srcmap-simple-local-macro
  ("(macrolet ((mac3 (x) `(foo ,x))) (bar (mac3 a) (mac3 b)))" "bar(foo(a), foo(b));")
  ("(bar (mac3 a) (mac3 b))" "bar(foo(a), foo(b))")
  ("(mac3 a)" "foo(a)")
  ("(mac3 b)" "foo(b)"))

(test-source-maps srcmap-simple-nested-macro
  ("(macrolet ((mac4 (x) `(foo ,x))) (bar (mac4 (mac4 x))))"
   "bar(foo(foo(x)));")
  ("(bar (mac4 (mac4 x)))" "bar(foo(foo(x)))")
  ("(mac4 (mac4 x))" "foo(foo(x))")
  ("(mac4 x)" "foo(x)"))

(test-source-maps srcmap-local-symbol-macro
  ("(symbol-macrolet ((a 1) (b (baz))) (foo a) (bar a b))"
   "foo(1);
bar(1, baz());")
  ("(foo a)" "foo(1)")
  ("(bar a b)" "bar(1, baz())")
  ("(baz)" "baz()"))

(test-source-maps srcmap-simple-symbol-macro
  ("(progn (define-symbol-macro sym1 (bar)) (foo (1+ sym1) sym1))"
   "foo(bar() + 1, bar());")
  ("(foo (1+ sym1) sym1)" "foo(bar() + 1, bar())")
  ("(bar)" "bar()")
  ("(1+ sym1)" "bar() + 1")
  ("(bar)" "bar()"))

(test-source-maps srcmap-local-symbol-macro-in-fn-body
  (
"(defun foo ()
   (aaa)
   (symbol-macrolet ((a 1))
     (bar (+ a a)))
   (bbb))"
"function foo() {
    aaa();
    bar(1 + 1);
    return bbb();
}")
  ("(aaa)" "aaa()")
  ("(bar (+ a a))" "bar(1 + 1)")
  ("(+ a a)" "1 + 1")
  ("(bbb)" "return bbb()")
  ("(bbb)" "bbb()"))

(test-source-maps srcmap-closure
  ("(let ((a 1))
   (defun foo ()
     (let ((a 2))
       (bar a))
     (bar a)))"
   "var a = 1;
function foo() {
    var a1 = 2;
    bar(a1);
    return bar(a);
};"
   )
  ("(a 1)" "var a = 1")
  (
"(defun foo ()
     (let ((a 2))
       (bar a))
     (bar a))"
"function foo() {
    var a1 = 2;
    bar(a1);
    return bar(a);
}")
  ("(a 2)" "var a1 = 2")
  ("(bar a)" "bar(a1)")
  ("(bar a)" "return bar(a)")
  ("(bar a)" "bar(a)"))

(test-source-maps srcmap-special-variable
  ("(progn (defvar x nil) (defun foo () (let ((x 1)))))"
   "var x = null;
function foo() {
    var x_tmpStack1;
    try {
        x_tmpStack1 = x;
        x = 1;
        return null;
    } finally {
        x = x_tmpStack1;
    };
};")
  ("(defvar x nil)" "var x = null")
  ("(defun foo () (let ((x 1))))"
   "function foo() {
    var x_tmpStack1;
    try {
        x_tmpStack1 = x;
        x = 1;
        return null;
    } finally {
        x = x_tmpStack1;
    };
}")
  ("(x 1)" "var x_tmpStack1")
  ("(x 1)" "x_tmpStack1 = x")
  ("(x 1)" "x = 1")
  ("(x 1)"
   " finally {
        x = x_tmpStack1;
    }")
  ("(x 1)"
   "{
        x = x_tmpStack1;
    }")
  ("(x 1)" "x = x_tmpStack1"))

(test-source-maps srcmap-declared-special
  ("(defun foo () (let ((x 1)) (declare (special x)) (bar x)))"
   "function foo() {
    var x_tmpStack1;
    try {
        x_tmpStack1 = x;
        x = 1;
        return bar(x);
    } finally {
        x = x_tmpStack1;
    };
}")
  ("(x 1)" "var x_tmpStack1")
  ("(x 1)" "x_tmpStack1 = x")
  ("(x 1)" "x = 1")
  ("(bar x)" "return bar(x)")
  ("(bar x)" "bar(x)")
  ("(x 1)"
   " finally {
        x = x_tmpStack1;
    }")
  ("(x 1)"
   "{
        x = x_tmpStack1;
    }")
  ("(x 1)" "x = x_tmpStack1"))

(test-source-maps srcmap-renamed-lexical
  ("(defun foo () (let ((foo 1)) (bar foo)))"
   "function foo() {
    var foo1 = 1;
    return bar(foo1);
}")
  ("(foo 1)" "var foo1 = 1")
  ("(bar foo)" "return bar(foo1)")
  ("(bar foo)" "bar(foo1)"))

(test-source-maps srcmap-fn-blocks
  (
"(defun foo (x)
   (block bar (when x (return-from bar 1)) (bing))
   (block baz (return-from baz 2)))"
"function foo(x) {
    bar: {
        if (x) {
            break bar;
        };
        bing();
    };
    return 2;
}")
  ("(x)" "(x)")
  ("(block bar (when x (return-from bar 1)) (bing))"
   "bar: {
        if (x) {
            break bar;
        };
        bing();
    }")
  ("(when x (return-from bar 1)) (bing)"
   "{
        if (x) {
            break bar;
        };
        bing();
    }")
  ("(when x (return-from bar 1))"
   "if (x) {
            break bar;
        }")
  ("(bing)" "bing()")
  ("(return-from baz 2)" "return 2"))

(test-source-maps srcmap-fn-block-return
  (
"(defun foo (x)
   (let ((y (block bar
              (when x (return-from bar (blah)))
              (bing))))
     (bing y))
  (block baz (return-from baz 2)))"
"function foo(x) {
    var y = (function () {
        if (x) {
            return blah();
        };
        return bing();
    })();
    bing(y);
    return 2;
}")
  ("(x)" "(x)")
  (     "(y (block bar
              (when x (return-from bar (blah)))
              (bing)))"
   "var y = (function () {
        if (x) {
            return blah();
        };
        return bing();
    })()")
  (        "(block bar
              (when x (return-from bar (blah)))
              (bing))"
    "(function () {
        if (x) {
            return blah();
        };
        return bing();
    })()")
  ("(when x (return-from bar (blah)))"
   "if (x) {
            return blah();
        }")
  ("(return-from bar (blah))" "return blah()")
  ("(blah)" "blah()")
  ("(bing)" "return bing()")
  ("(bing)" "bing()")
  ("(bing y)" "bing(y)")
  ("(return-from baz 2)" "return 2"))

(test-source-maps srcmap-defun-setf
  ("(defun (setf foo) (x y) (bar x y))"
   "function __setf_foo(x, y) {
    return bar(x, y);
}")
  ;("(setf foo)" "__setf_foo")
  ("(x y)" "(x, y)")
  ("(bar x y)" "return bar(x, y)")
  ("(bar x y)" "bar(x, y)"))

(test-source-maps srcmap-single-flet
  ("(flet ((bar () 123)) (bar))"
   "(function () {
    var bar = function () {
        return 123;
    };
    return bar();
})()")
  ("(bar () 123)"
   "var bar = function () {
        return 123;
    }")
  ("(bar)" "return bar()")
  ("(bar)" "bar()"))

(test-source-maps srcmap-multi-flet
  ("(defun foo () (flet ((bar (a) (bing a)) (baz (b) (bing b))) (bar (baz))))"
   "function foo() {
    var bar = function (a) {
        return bing(a);
    };
    var baz = function (b) {
        return bing(b);
    };
    return bar(baz());
}")
  ("(bar (a) (bing a))"
   "var bar = function (a) {
        return bing(a);
    }")
  ("(a)" "(a)")
  ("(bing a)" "return bing(a)")
  ("(bing a)" "bing(a)")
  ("(baz (b) (bing b))"
   "var baz = function (b) {
        return bing(b);
    }")
  ("(b)" "(b)")
  ("(bing b)" "return bing(b)")
  ("(bing b)" "bing(b)")
  ("(bar (baz))" "return bar(baz())")
  ("(bar (baz))" "bar(baz())")
  ("(baz)" "baz()"))

(test-source-maps srcmap-renamed-flet
  ("(defun foo () (let ((bar 2)) (flet ((bar () 123)) (bar)) (baz bar)))"
   "function foo() {
    var bar = 2;
    var bar1 = function () {
        return 123;
    };
    bar1();
    return baz(bar);
}")
  ("(bar 2)" "var bar = 2")
  ("(bar () 123)"
   "var bar1 = function () {
        return 123;
    }")
  ("(bar)" "bar1()")
  ("(baz bar)" "return baz(bar)")
  ("(baz bar)" "baz(bar)"))

(test-source-maps srcmap-flet-expression
  ("(defun foo () (setf x (flet ((bar () 123)))))"
   "function foo() {
    var bar;
    return x = (bar = function () {
        return 123;
    });
}")
  ("(setf x (flet ((bar () 123))))"
   "return x = (bar = function () {
        return 123;
    })")
  ("(setf x (flet ((bar () 123))))"
   "x = (bar = function () {
        return 123;
    })")
  ("(flet ((bar () 123)))"
   "bar = function () {
        return 123;
    }")
  ("(bar () 123)"
   "bar = function () {
        return 123;
    }"))

(test-source-maps srcmap-multi-labels
  (
"(labels ((foo (x) (1+ x))
          (bar (y) (+ 2 (foo y))))
   (bar (foo 1)))"
"(function () {
    var foo = function (x) {
        return x + 1;
    };
    var bar = function (y) {
        return 2 + foo(y);
    };
    return bar(foo(1));
})()")
  ("(foo (x) (1+ x))"
   "var foo = function (x) {
        return x + 1;
    }")
  ("(x)" "(x)")
  ("(1+ x)" "return x + 1")
  ("(1+ x)" "x + 1")
  ("(bar (y) (+ 2 (foo y)))"
   "var bar = function (y) {
        return 2 + foo(y);
    }")
  ("(y)" "(y)")
  ("(+ 2 (foo y))" "return 2 + foo(y)")
  ("(+ 2 (foo y))" "2 + foo(y)")
  ("(foo y)" "foo(y)")
  ("(bar (foo 1))" "return bar(foo(1))")
  ("(bar (foo 1))" "bar(foo(1))")
  ("(foo 1)" "foo(1)"))

(test-source-maps srcmap-non-local-return
  ("(defun foo () (flet ((bar () (return-from foo 123)))))"
"function foo() {
    try {
        var bar = function () {
            throw { '__ps_block_tag' : 'foo', '__ps_value1' : 123 };
        };
        return null;
    } catch (_ps_err1) {
        if (_ps_err1 && 'foo' === _ps_err1['__ps_block_tag']) {
            __PS_MV_REG = { 'tag' : arguments.callee, 'values' : _ps_err1['__ps_values'] };
            return _ps_err1['__ps_value1'];
        } else {
            throw _ps_err1;
        };
    };
}")
  ("(flet ((bar () (return-from foo 123))))"
   "{
        var bar = function () {
            throw { '__ps_block_tag' : 'foo', '__ps_value1' : 123 };
        };
        return null;
    }")
  ("(bar () (return-from foo 123))"
   "var bar = function () {
            throw { '__ps_block_tag' : 'foo', '__ps_value1' : 123 };
        }")
  ("(return-from foo 123)" "throw { '__ps_block_tag' : 'foo', '__ps_value1' : 123 }"))

(test-source-maps srcmap-multiple-values
  ("(lambda () (values (foo) (bar)))"
   "(function () {
    var val1_1 = foo();
    __PS_MV_REG = { 'tag' : arguments.callee, 'values' : [bar()] };
    return val1_1;
})")
  ("(values (foo) (bar))" "foo()")
  ("(bar)" "[bar()]")
  ("(bar)" "bar()"))

(test-source-maps srcmap-simple-mvb
  ("(multiple-value-bind (a b) (blah) (+ a b))"
   "(function () {
    var prevMv1 = 'undefined' === typeof __PS_MV_REG ? (__PS_MV_REG = undefined) : __PS_MV_REG;
    try {
        var a = blah();
        var _db2 = blah === __PS_MV_REG['tag'] ? __PS_MV_REG['values'] : [];
        var b = _db2[0];
        return a + b;
    } finally {
        __PS_MV_REG = prevMv1;
    };
})()")
  ("(blah)" "blah()")
  ("(+ a b)" "return a + b")
  ("(+ a b)" "a + b"))

(test-source-maps srcmap-progn-mvb
  ("(multiple-value-bind (a b) (progn (foo) (bar)) (+ a b))"
   "foo();
(function () {
    var prevMv1 = 'undefined' === typeof __PS_MV_REG ? (__PS_MV_REG = undefined) : __PS_MV_REG;
    try {
        var a = bar();
        var _db2 = bar === __PS_MV_REG['tag'] ? __PS_MV_REG['values'] : [];
        var b = _db2[0];
        return a + b;
    } finally {
        __PS_MV_REG = prevMv1;
    };
})();")
  ("(foo)" "foo()")
  ("(bar)" "bar()")
  ("(+ a b)" "return a + b")
  ("(+ a b)" "a + b"))

(test-source-maps srcmap-with-slots
  ("(lambda () (with-slots (a b) (foo) (+ a b)))"
   "(function () {
    var object1 = foo();
    return object1.a + object1.b;
})")
  ("(foo)" "foo()")
  ("(+ a b)" "return object1.a + object1.b")
  ("(+ a b)" "object1.a + object1.b"))

(test-source-maps srcmap-eval-when
  ("(eval-when (:execute) (foo (bar)))" "foo(bar());")
  ("(foo (bar))" "foo(bar())")
  ("(bar)" "bar()"))

(test-source-maps srcmap-while-loop
  ("(while (foo) (when (baz) (break)) (bar))"
   "while (foo()) {
    if (baz()) {
        break;
    };
    bar();
}")
  ("(foo)" "foo()")
  ("(when (baz) (break)) (bar)" "{
    if (baz()) {
        break;
    };
    bar();
}")
  ("(when (baz) (break))" "if (baz()) {
        break;
    }")
  ("(baz)" "baz()")
  ("(break)" "break")
  ("(bar)" "bar()"))

(test-source-maps srcmap-for-loop
  ("(for ((i 1) (j (foo))) ((< i 5) (= j (bar))) ((incf i) (decf j (baz))) (bing i j))"
   "for (var i = 1, j = foo(); i < 5, j === bar(); ++i, j -= baz()) {
    bing(i, j);
}")
  ("((i 1) (j (foo)))" "var i = 1, j = foo()")
  ("(i 1)" "var i = 1")
  ("(j (foo))" "j = foo()")
  ("(foo)" "foo()")
  ("((< i 5) (= j (bar)))" "i < 5, j === bar()")
  ("(< i 5)" "i < 5")
  ("(= j (bar))" "j === bar()")
  ("(bar)" "bar()")
  ("((incf i) (decf j (baz)))" "++i, j -= baz()")
  ("(incf i)" "++i")
  ("(decf j (baz))" "j -= baz()")
  ("(baz)" "baz()")
  ("(bing i j)"
   "{
    bing(i, j);
}")
  ("(bing i j)" "bing(i, j)"))

(test-source-maps srcmap-for-in-loop
  ("(for-in (a (foo)) (bar a))"
   "for (var a in foo()) {
    bar(a);
}")
  ("(a (foo))" "(var a in foo())")
  ("(foo)" "foo()")
  ("(bar a)" "{
    bar(a);
}")
  ("(bar a)" "bar(a)"))

(test-source-maps srcmap-do*-loop
  ("(do* ((a) b (c (array 1 2 3))
      (d 0 (1+ d))
      (e (aref c d) (aref c d)))
    ((or (= d (@ c length)) (= e 55)) (done))
  (setf a d b e)
  (foo a b))"
 "(function () {
    for (var a = null, b = null, c = [1, 2, 3], d = 0, e = c[d]; !(d === c.length || e === 55); d += 1, e = c[d]) {
        a = d;
        b = e;
        foo(a, b);
    };
    return done();
})()")
  ("((a) b (c (array 1 2 3))
      (d 0 (1+ d))
      (e (aref c d) (aref c d)))"
   "var a = null, b = null, c = [1, 2, 3], d = 0, e = c[d]")
  ("(a)" "var a = null")
  ("(c (array 1 2 3))" "c = [1, 2, 3]")
  ("(array 1 2 3)" "[1, 2, 3]")
  ("(d 0 (1+ d))" "d = 0")
  ("(e (aref c d) (aref c d))" "e = c[d]")
  ("(aref c d)" "c[d]")
  ("(or (= d (@ c length)) (= e 55))" "!(d === c.length || e === 55)")
  ("(or (= d (@ c length)) (= e 55))" "d === c.length || e === 55")
  ("(= d (@ c length))" "d === c.length")
  ("(@ c length)" "c.length")
  ("(= e 55)" "e === 55")
  ("((a) b (c (array 1 2 3))
      (d 0 (1+ d))
      (e (aref c d) (aref c d)))" "d += 1, e = c[d]")
  ("(d 0 (1+ d))" "d += 1")
  ("(e (aref c d) (aref c d))" "e = c[d]")
  ("(aref c d)" "c[d]")
  ("(setf a d b e)
  (foo a b)"
   "{
        a = d;
        b = e;
        foo(a, b);
    }")
  ("(setf a d b e)" "a = d")
  ("(setf a d b e)" "b = e")
  ("(foo a b)" "foo(a, b)")
  ("(done)" "return done()")
  ("(done)" "done()"))

(test-source-maps srcmap-do-loop
  ("(do ((i 0 (1+ i))
     (s 0 (+ s i (1+ i))))
    ((> i 10))
  (foo i s))"
   "(function () {
    var i = 0;
    var s = 0;
    for (; i <= 10; ) {
        foo(i, s);
        var _js1 = i + 1;
        var _js2 = s + i + (i + 1);
        i = _js1;
        s = _js2;
    };
})()")
  ("(i 0 (1+ i))" "var i = 0")
  ("(s 0 (+ s i (1+ i)))" "var s = 0")
  ("(> i 10)" "i <= 10")
  ("((i 0 (1+ i))
     (s 0 (+ s i (1+ i))))
    ((> i 10))
  (foo i s)"
   "{
        foo(i, s);
        var _js1 = i + 1;
        var _js2 = s + i + (i + 1);
        i = _js1;
        s = _js2;
    }")
  ("(foo i s)" "foo(i, s)")
  ("(1+ i)" "i + 1")
  ("(+ s i (1+ i))" "s + i + (i + 1)")
  ("(1+ i)" "i + 1"))

(test-source-maps srcmap-dotimes-loop
  ("(dotimes (i (foo) (bar)) (baz i))"
   "(function () {
    for (var i = 0; i < foo(); i += 1) {
        baz(i);
    };
    return bar();
})()")
  ("(foo)" "foo()")
  ("(baz i)"
   "{
        baz(i);
    }")
  ("(baz i)" "baz(i)")
  ("(bar)" "return bar()")
  ("(bar)" "bar()"))

(test-source-maps srcmap-dolist-loop
  ("(dolist (x (foo)) (bar x))"
   "(function () {
    for (var x = null, _js_arrvar2 = foo(), _js_idx1 = 0; _js_idx1 < _js_arrvar2.length; _js_idx1 += 1) {
        x = _js_arrvar2[_js_idx1];
        bar(x);
    };
})()")
  ("(foo)" "foo()")
  ("(bar x)"
   "{
        x = _js_arrvar2[_js_idx1];
        bar(x);
    }")
  ("(bar x)" "bar(x)"))

(test-source-maps srcmap-destructuring-bind
  ("(destructuring-bind (a (b c)) (foo) (bar a b c))" "(function () {
    var _db1 = foo();
    var a = _db1[0];
    var _db2 = _db1[1];
    var b = _db2[0];
    var c = _db2[1];
    return bar(a, b, c);
})()")
  ("(foo)" "foo()")
  ("(bar a b c)" "return bar(a, b, c)")
  ("(bar a b c)" "bar(a, b, c)"))

(test-source-maps srcmap-destructuring-bind-nohoist
  ("(destructuring-bind (a) (foo))"
   "(function () {
    var a = foo()[0];
    return null;
})()")
  ("(foo)" "foo()"))

;; (test-source-maps srcmap-long-defsetf
;;   ("(progn (defsetf baz (x y) (val) `(blah ,x ,y ,val)) (setf (baz 1 2) 3))"
;;    "(function () {
;;     var _js2 = 1;
;;     var _js3 = 2;
;;     var _js1 = 3;
;;     return blah(_js2, _js3, _js1);
;; })();")
;;   ("(setf (baz 1 2) 3)" "(function () {
;;     var _js2 = 1;
;;     var _js3 = 2;
;;     var _js1 = 3;
;;     return blah(_js2, _js3, _js1);
;; })()")
;;   ("(blah ,x ,y ,val)" "return blah(_js2, _js3, _js1)")
;;   ("(blah ,x ,y ,val)" "blah(_js2, _js3, _js1)"))

(test-source-maps srcmap-short-defsetf
  ("(progn (defsetf baz blah) (setf (baz (foo) 23) (bar)))" "blah(foo(), 23, bar());")
  ("(setf (baz (foo) 23) (bar))" "blah(foo(), 23, bar())")
  ("(foo)" "foo()")
  ("(bar)" "bar()"))

(in-package :ps)
