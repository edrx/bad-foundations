;; This file:
;;   https://anggtwu.net/bad-foundations/bad-foundations.lisp.html
;;   https://anggtwu.net/bad-foundations/bad-foundations.lisp
;;           (find-angg "bad-foundations/bad-foundations.lisp")
;; Author: Eduardo Ochs <eduardoochs@gmail.com>
;; Version: 2026mar18
;; License: GPL v2
;;
;; This file is loaded by:
;;   (find-baf "bad-foundations.mac")
;; and it calls some functions that are defined in:
;;   (find-baf "edrxbox.lisp")
;;   (find-baf "edrxbox-examples.lisp")
;;
;; «.matrix»			(to "matrix")
;; «.bmatrix»			(to "bmatrix")
;; «.matrix-tests»		(to "matrix-tests")
;; «.barematrix»		(to "barematrix")
;; «.barematrix-tests»		(to "barematrix-tests")
;; «.tex-quotient»		(to "tex-quotient")
;; «.tex-quotient-tests»	(to "tex-quotient-tests")
;; «.fp-and-gp»			(to "fp-and-gp")
;; «.fp-and-gp-tests»		(to "fp-and-gp-tests")
;;
;; «.ftolambda»			(to "ftolambda")
;; «.ftolambda-tests»		(to "ftolambda-tests")
;;
;; «.becomes»			(to "becomes")
;; «.becomes-tests»		(to "becomes-tests")
;; «.tex-underbrace»		(to "tex-underbrace")
;; «.tex-underbrace-tests»	(to "tex-underbrace-tests")
;; «.eqn»			(to "eqn")
;; «.eqn-tests»			(to "eqn-tests")
;; «.tex-verbatimbox»		(to "tex-verbatimbox")
;; «.tex-verbatimmatrix»	(to "tex-verbatimmatrix")
;; «.tex-verbatimbox-tests»	(to "tex-verbatimbox-tests")
;; «.tex-verbatimmatrix-tests»	(to "tex-verbatimmatrix-tests")
;; «.tex-myintegrate»		(to "tex-myintegrate")
;; «.tex-myintegrate-tests»	(to "tex-myintegrate-tests")
;;
;; «.ee_writefile»		(to "ee_writefile")
;; «.ee_writefile-tests»	(to "ee_writefile-tests")



;; «ftolambda»  (to ".ftolambda")
;; Used by: (find-baf "bad-foundations.mac" "_s_")
;;          (find-baf "bad-foundations.mac" "_s_" "__s_fstolambdas")
;;     See: (find-bafpage 14 "__s_fstolambdas")
;;          (find-baftext 14 "__s_fstolambdas")
;;
(defun $__s_ftolambda (subst)
  (destructuring-bind (head lhs rhs) subst
    (declare (ignorable head))
    (if (atom lhs)
	subst
	(destructuring-bind ((f . _) . args) lhs
	  (declare (ignorable _))
	  `((mequal simp) ,f ((lambda simp) ((mlist) ,@args) ,rhs))))))

#|
 «ftolambda-tests»  (to ".ftolambda-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
load("bad-foundations.lisp");
__s_ftolambda(a=42);
__s_ftolambda(f(x,y)=42+x+y);

|#



;;;  __  __       _        _               
;;; |  \/  | __ _| |_ _ __(_)_  _____  ___ 
;;; | |\/| |/ _` | __| '__| \ \/ / _ \/ __|
;;; | |  | | (_| | |_| |  | |>  <  __/\__ \
;;; |_|  |_|\__,_|\__|_|  |_/_/\_\___||___/
;;;                                        
;; «matrix»   (to ".matrix")
;; «bmatrix»  (to ".bmatrix")
;; See: (find-bafpage 9 "bmatrix")
;;      (find-baftext 9 "bmatrix")
;;
;; In this block we redefine `tex-matrix' to use LaTeX instead of TeX:
;;
;;   (%i1) tex1(matrix([42]));
;;   (%o1) \ifx\endpmatrix\undefined\pmatrix{\else\begin{pmatrix}\fi 42\cr   <-- TeX
;;         \ifx\endpmatrix\undefined}\else\end{pmatrix}\fi 
;;   (%i2) load("bad-foundations.lisp")$
;;   (%i3) tex1(matrix([42]));
;;   (%o3)           \begin{pmatrix}42\cr \end{pmatrix}                      <-- LaTeX
;;
;; and we also define a `bmatrix', that is displayed like a `matrix'
;; but is `tex'ed using square brackets instead of parentheses.
;;
;; The original definition of `tex-matrix' is here:
;;   (find-maximagitfile "src/mactex.lisp" "(defun tex-matrix")
;;   (find-maximagitfile "src/mactex.lisp" "(defun tex-matrix" "ifx")
;;
;; In the code below a "matrix" looks like
;;   ((mmatrix) ((mlist) a b) ...)
;; and a "row" looks like this:
;;              ((mlist) a b).
;;
(defprop $matrix  tex-matrix  tex)
(defprop $bmatrix tex-bmatrix tex)
(setf (get '$bmatrix 'dimension) 'dim-$matrix)

(defun tex-matrixrow (row) (tex-list (cdr row) nil (list "\\cr ") "&"))
(defun tex-matrixbody (matrix) (mapcan #'tex-matrixrow (cdr matrix)))

(defun tex-matrix (matrix l r)
  `(,@l
    "\\begin{pmatrix}"
    ,@(tex-matrixbody matrix)
    "\\end{pmatrix}"
    ,@r))

(defun tex-bmatrix (matrix l r)
  `(,@l
    "\\begin{bmatrix}"
    ,@(tex-matrixbody matrix)
    "\\end{bmatrix}"
    ,@r))

#|
 «matrix-tests»  (to ".matrix-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
tex1(matrix([42]));
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
load("bad-foundations.lisp");
tex1(matrix([42]));
tex1(bmatrix([42]));
     bmatrix([42]);

to_lisp();
  (tex-matrixrow #$[2,a,"b"]$)
  (tex-matrixbody #$[[a,b],[c,d]]$)
  (to-maxima)

|#


;; «barematrix»  (to ".barematrix")
;; See: (find-bafpage 9 "barematrix")
;;      (find-baftext 9 "barematrix")
;;      (find-es "maxima" "display_matrix_brackets")
;;      (find-maximamsg "59245670 202510 12" "RDodier: display_matrix_brackets")
;;      (find-maximamsg "59271831 202512 11" "RDodier: display_matrix_padding")
;;      (find-baf "bad-foundations.mac" "makebare")
;;
;; A `barematrix' is a `matrix' without the outer brackets, and a
;; `barematrixnp' is a `barematrix' with no padding - i.e., with no
;; space between lines and columns.
;;
;; The variables `display_matrix_brackets',
;; `display_matrix_padding_vertical' and
;; `display_matrix_padding_horizontal' were added to Maxima in
;; 2025oct12 and 2025dec11, and my definitions of `dim-$barematrix'
;; and `dim-$barematrixnp' set these variables temporarily. 
;;
;; If you're on a version of Maxima that is older than 2025oct12 then
;; a `matrix', a `barematrixe' and a `barematrixnp' will all be
;; displayed in the same way; if you're on a Maxima that is more
;; recent than 2025dec11 they will all be displayed differently, "in
;; the right way".
;;
(setf (get '$barematrix   'dimension) 'dim-$barematrix)
(setf (get '$barematrixnp 'dimension) 'dim-$barematrixnp)
(defprop $barematrix   tex-barematrix tex)
(defprop $barematrixnp tex-barematrix tex) ; will change in the future

(defun dim-$barematrix (form result)
  (let (($display_matrix_brackets nil))
    (dim-$matrix form result)))

(defun dim-$barematrixnp (form result)
  (let (($display_matrix_brackets nil)
	($display_matrix_padding_vertical nil)
	($display_matrix_padding_horizontal nil))
    (dim-$matrix form result)))

(defun tex-barematrix (matrix l r)
  `(,@l
    "\\begin{matrix}"
    ,@(tex-matrixbody matrix)
    "\\end{matrix}"
    ,@r))

#|
 «barematrix-tests»  (to ".barematrix-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
load("bad-foundations.lisp");
M1 : barematrix  ([a,b],[c,d]);
M2 : barematrixnp([a,b],[c,d]);
M3 : matrix     ([M1,e,f],[g,h,i],[j,k,M2]);
tex1(M1);
apply('barematrix,   map("[", ["ab", "cd", "ef"]));
apply('barematrixnp, map("[", ["ab", "cd", "ef"]));

|#




;; «tex-quotient»  (to ".tex-quotient")
;; In this block we redefine `tex-mquotient' to use LaTeX instead of TeX:
;;
;;   (%i1) tex1(a/b);
;;   (%o1)                            {{a}\over{b}}   <-- TeX
;;   (%i2) load("bad-foundations.lisp")$
;;   (%i3) tex1(a/b);
;;   (%o3)                            {\frac{a}{b}}   <-- LaTeX
;;
(defun tex-mquotient (q l r)
  (twoargcheck q)
  `(,@(tex (cadr  q) `(,@l "{\\frac{") () 'mparen 'mparen)
    ,@(tex (caddr q) (list "}{") `("}}" ,@r) 'mparen 'mparen)
    ))

#|
 «tex-quotient-tests»  (to ".tex-quotient-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
tex1(a/b);
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
load("bad-foundations.lisp");
tex1(a/b);

|#


;; «fp-and-gp»  (to ".fp-and-gp")
;; Make fp(x) and gp(x) by displayed as f'(x) and g'(x).
;; See: (find-baf "bad-foundations.mac" "fp-and-gp")
;;      (find-baf "edrxbox.lisp" "append")
;;      
(setf (get  '$fp  'dimension)  'dim-fp)
(setf (get  '$gp  'dimension)  'dim-gp)
(setf (get '|$Fp| 'dimension) '|dim-Fp|)

(defun dim-fp (form result)
  (run-in-edrxbox-dim
   (edrxboxvars-append-function "f'" (rest form))
   (edrxboxvars-push-x-y-end)))

(defun dim-gp (form result)
  (run-in-edrxbox-dim
   (edrxboxvars-append-function "g'" (rest form))
   (edrxboxvars-push-x-y-end)))

(defun |dim-Fp| (form result)
  (run-in-edrxbox-dim
   (edrxboxvars-append-function "F'" (rest form))
   (edrxboxvars-push-x-y-end)))

#|
 «fp-and-gp-tests»  (to ".fp-and-gp-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
load("bad-foundations.lisp");
[fp(g(x)), gp(x), Fp(x)];

|#



;; «becomes»  (to ".becomes")
;; `becomes' is mentioned briefly in this page of the article:
;;   (find-bafpage 13 "Prettifying")
;;   (find-baftext 13 "Prettifying")
;;   (find-bafpage 13 "lisptreem(V(S));")
;;   (find-baftext 13 "lisptreem(V(S));")
;;   (find-bafpage 13 "becomes")
;;   (find-baftext 13 "becomes")
;;
;; I tried to use the built-in `:=' in the prettification but its
;; display function has some quirks - it uses `dim-mdefine', that sets
;; temporarily `noundisp' and `stringdisp' to `true'. See:
;;   (find-maximagitfile "src/displa.lisp" "(defun dim-mdefine ")
;;   (find-es "maxima" "becomes")
;;
;; Used by: (find-baf "bad-foundations.mac" "_s_")
;;          (find-baf "bad-foundations.mac" "_s_" "__s_usebecomes")
;;
(displa-def $becomes dimension-infix " := " 80 80)
(defprop $becomes tex-infix tex)
(defprop $becomes (" := ") texsym)

#|
 «becomes-tests»  (to ".becomes-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
load("bad-foundations.lisp");
o : becomes(f(x),42*x);
tex1(o);

|#




;; «tex-underbrace»  (to ".tex-underbrace")
;; See: (find-baf "edrxbox-examples.lisp" "underbrace")
;;      (find-baf "bad-foundations.mac" "_ssu_")
;;      https://anggtwu.net/maxima-edrxbox.html#underbrace
;;
(defprop $underbrace tex-underbrace tex)

(defun tex-underbrace (uexpr l r)
  (let* ((args        (cdr uexpr))
	 (top-expr    (first args))
	 (bottom-expr (second args))
	 (top-tex     ($tex1 top-expr))
	 (bottom-tex  ($tex1 bottom-expr)))
  `(,@l
    ,(format nil "\\underbrace{~a}_{\\textstyle ~a}" top-tex bottom-tex)
    ,@r)))

;; «tex-underbrace-tests»  (to ".tex-underbrace-tests")
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
load("bad-foundations.lisp");
mybox(o) := [box(o)];
o1 : 23 / 456789;
o2 : a[b]^c;
o3 : underbrace(o1,o2);
mybox(o3);
tex1(o3);

|#



;; «eqn»  (to ".eqn")
;; See: (find-angg "MAXIMA/edrxbox-examples.lisp" "eqn")
;;      (find-angg "MAXIMA/edrxbox-examples.lisp" "eqn-tests")
;;
(defprop $eqn tex-eqn tex)

(defun tex-eqn (expr l r)
  (let* ((args (rest expr))
	 (n    (first args)))
  `(,@l
    ,(format nil "\\overset{\\text{(~a)}}{=}" n)
    ,@r)))

#|
 «eqn-tests»  (to ".eqn-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
load("bad-foundations.lisp");
o : eqn(42);
tex1(o);

|#



;;;                 _           _   _           _               
;;; __   _____ _ __| |__   __ _| |_(_)_ __ ___ | |__   _____  __
;;; \ \ / / _ \ '__| '_ \ / _` | __| | '_ ` _ \| '_ \ / _ \ \/ /
;;;  \ V /  __/ |  | |_) | (_| | |_| | | | | | | |_) | (_) >  < 
;;;   \_/ \___|_|  |_.__/ \__,_|\__|_|_| |_| |_|_.__/ \___/_/\_\
;;;                                                             
;; «tex-verbatimbox»     (to ".tex-verbatimbox")
;; «tex-verbatimmatrix»  (to ".tex-verbatimmatrix")
;;
;; The display code for `verbatimbox'es and `verbatimmatrix'es is here:
;;   (find-baf "edrxbox-examples.lisp" "verbatimbox")
;;   (find-baf "edrxbox-examples.lisp" "verbatimmatrix")
;;   (find-baf "edrxbox-examples.lisp" "verbatimbox-tests")
;;   (find-baf "edrxbox-examples.lisp" "verbatimmatrix-tests")
;;
;; In this block we define how to LaTeXify them. For example:
;;
;;   (%i1) o1 : verbatimbox("f_.", "| |", "a b");
;;                                         f_.
;;   (%o1)                                 | |
;;                                         a b
;;   (%i2) tex1(o1);
;;   (%o2) \vbtbgbox{\vbox{\vbthbox{f\char95 .}\vbthbox{|\ |}\vbthbox{a\ b}}}
;;
;; See:
;;   (find-LATEX "edrx21.sty" "defvbt")
;;   (find-LATEX "edrx21.sty" "defvbt" "\\def\\vbthbox")
;;   (find-LATEX "edrx21.sty" "defvbt" "\\def\\vbtbgbox")
;;   (find-es "lisp" "hash-table")
;; This is a rewrite in Lisp of:
;;   (find-angg "LUA/Verbatim3.lua" "Verbatim")
;;   (find-angg "LUA/Verbatim3.lua" "Verbatim" "Co.new")
;;
;; TODO: explain how to use this!

(defvar tex-verbatimbox-hash)
(setq   tex-verbatimbox-hash (make-hash-table))
(defun  tex-verbatimbox-hash-set (c str)
  (setf (gethash c tex-verbatimbox-hash) str))

(loop for c in '(#\  #\# #\$)
      do (tex-verbatimbox-hash-set c (format nil "\\~a" c)))

(loop for c in '(#\% #\& #\\ #\^ #\_ #\{ #\} #\~)
      do (tex-verbatimbox-hash-set c (format nil "\\char~a " (char-code c))))

(tex-verbatimbox-hash-set #\Newline (format nil "\\\\~%"))

(defun tex-verbatimbox-string (str)
  (let* ((chars (coerce str 'list))
	 (strs (loop for c in chars
		     collect (gethash c tex-verbatimbox-hash
				      (coerce (list c) 'string)))))
    (apply #'concatenate 'string strs)))

(defun tex-verbatimbox-strings (strs &optional (sep ""))
  (let* ((vbthboxes (loop for str in strs
			  collect (format nil "\\vbthbox{~a}~a"
					  (tex-verbatimbox-string str) sep)))
	 (body (apply #'concatenate 'string vbthboxes)))
    (format nil "\\myvcenter{\\vbtbgbox{\\vbox{~a~a}}}" sep body)))

(defun tex-verbatimbox (o l r)
  `(,@l ,(tex-verbatimbox-strings (cdr o)) ,@r))

(defun tex-verbatimmatrix (o l r)
  `(,@l ,(tex-verbatimbox-strings (map 'list #'second (cdr o))) ,@r))

(defprop $verbatimbox    tex-verbatimbox    tex)
(defprop $verbatimmatrix tex-verbatimmatrix tex)

;; «tex-verbatimbox-tests»  (to ".tex-verbatimbox-tests")
;; «tex-verbatimmatrix-tests»  (to ".tex-verbatimmatrix-tests")
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
load("bad-foundations.lisp");

o1 : matrix        (["f_."], ["| |"], ["a b"]);
o2 : barematrix    (["f_."], ["| |"], ["a b"]);
o3 : barematrixnp  (["f_."], ["| |"], ["a b"]);
o4 : verbatimbox   ( "f_.",   "| |",   "a b" );
o5 : verbatimmatrix(["f_."], ["| |"], ["a b"]);
[o1, o2, o3, o4, o5];
[box(o1), box(o2), box(o3), box(o4), box(o5)];
tex1(o4);
tex1(o5);

to_lisp();
  (tex-verbatimbox-string "ab ~")
  (tex-verbatimbox-strings '("ab ~" "cd"))
  (tex-verbatimbox-strings '("ab ~" "cd") (format nil "%~%  "))
  (to-maxima)

|#



;; «tex-myintegrate»  (to ".tex-myintegrate")
;; See: (find-baf "edrxbox-examples.lisp" "myintegrate")

(defprop $_d          tex-$_d          tex)
(defprop $myintegrate tex-$myintegrate tex)

(defun tex-$_d (o l r)
  (let ((u (second o)))
    `(,@l ,(format nil "d~a" ($tex1 u)) ,@r)))

(defun tex-$myintegrate (o l r)
  (let* ((args   (rest   o))
	 (nargs  (length args))
	 (fu     (first  args))
	 (u      (second args))
	 (a      (third  args))
	 (b      (fourth args))
	 (alt-a  (second args))
	 (alt-b  (third args))
	 (ab-tex (cond ((= nargs 4) (format nil "_{~a}^{~a}" ($tex1 a) ($tex1 b)))
		       ((= nargs 3) (format nil "_{~a}^{~a}" ($tex1 alt-a) ($tex1 alt-b)))
		       (t " ")))
	 (du-tex (cond ((= nargs 4) (format nil "\\;d~a" ($tex1 u)))
		       ((= nargs 2) (format nil "\\;d~a" ($tex1 u)))
		       (t "")))
	 (int-tex (format nil "\\int~a{~a~a}\\big." ab-tex ($tex1 fu) du-tex)))
  `(,@l
    ,int-tex
    ,@r)))

#|
 «tex-myintegrate-tests»  (to ".tex-myintegrate-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("bad-foundations.mac");
tex1(_d(x));


     myintegrate(f(x));
     myintegrate(f(x),x);
     myintegrate(f(x),x,a,b);
     myintegrate(f(x)  ,a,b);
     myintegrate(f(x) *. _d(x));
     myintegrate(f(x) *. _d(x), a, b);

tex1(myintegrate(f(x)));
tex1(myintegrate(f(x),x));
tex1(myintegrate(f(x),x,a,b));
tex1(myintegrate(f(x)  ,a,b));
tex1(myintegrate(f(x) *. dx));
tex1(myintegrate(f(x) *. _d(x)));
tex1(myintegrate(f(x) *. _d(x), a, b));

|#



;; «ee_writefile»  (to ".ee_writefile")
;; Inspired by: (find-angg "LUA/lua50init.lua" "fileexists" "ee_writefile =")
;;     Used by: (find-baf "sa-and-ga.mac")
;;        Uses: (find-es "lisp" "with-open-file")
;;
(defun $ee_writefile (fname str)
  (with-open-file (outfile fname
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
    (write-string str outfile))
  (format nil "Wrote ~s bytes to ~a" (length str) fname))

;; «ee_writefile-tests»  (to ".ee_writefile-tests")
