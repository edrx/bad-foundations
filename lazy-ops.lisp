;; This file:
;;   https://anggtwu.net/bad-foundations/lazy-ops.lisp.html
;;   https://anggtwu.net/bad-foundations/lazy-ops.lisp
;;           (find-angg "bad-foundations/lazy-ops.lisp")
;; Author: Eduardo Ochs <eduardoochs@gmail.com>
;;
;; «.my-copy-properties»	(to "my-copy-properties")
;; «.lazy-operations»		(to "lazy-operations")
;; «.lazy-operations-tests»	(to "lazy-operations-tests")
;; «.lazy-nouns»		(to "lazy-nouns")
;; «.lazy-nouns-tests»		(to "lazy-nouns-tests")
;; «.tex-_diff»			(to "tex-_diff")
;; «.tex-_diff-tests»		(to "tex-_diff-tests")


;;;  _                    
;;; | |    __ _ _____   _ 
;;; | |   / _` |_  / | | |
;;; | |__| (_| |/ /| |_| |
;;; |_____\__,_/___|\__, |
;;;                 |___/ 
;;
;; «my-copy-properties»  (to ".my-copy-properties")
;; `my-copy-properties' implements this idea from the article:
;;
;;   Another way to obtain operations that are simplified less, or
;;   that are not simplified at all, is to create new operations and
;;   copy most of the properties from the original versions to the
;;   copies, but omit the properties that say how they are simplified.
;;
;; See: (find-bafpage 12 "copy most of the properties")
;;      (find-baftext 12 "copy most of the properties")
;;      (find-bafpage 12 "lazy operations")
;;      (find-baftext 12 "lazy operations")
;;
(defun my-copy-properties (tosymbol fromsymbol keys)
  (loop for key in keys
	do (setf (get tosymbol key)
		 (get fromsymbol key))))

;; «lazy-operations»  (to ".lazy-operations")
;; See: (find-maxima-op-links "=")
;;      (find-maxima-op-links "*")
;;      (find-maxima-op-links "/")
;;      (find-maxima-op-links "+")
;;      (find-maxima-op-links "-")
;;      (find-maxima-op-links "^")
;;      
($infix "=." 80 80)
($nary  "=.")
(my-copy-properties '|$=| 'mequal
		    '($nary lbp rbp
		      dimension dissym
		      tex texsym))

($infix "^." 139 140)
($nary  "^.")
(my-copy-properties '|$^.| 'mexpt
		    '($nary lbp rbp
		      dimension dissym
		      tex texsym))

($infix "*." 120)
($nary  "*.")
(my-copy-properties '|$*.| 'mtimes
		    '($nary lbp rbp
		      dimension dissym
		      tex texsym))

($infix "/." 120 120)
($nary  "/.")				; why?
(my-copy-properties '|$/.| 'mquotient
		    '($nary lbp rbp
		      dimension dissym
		      tex texsym))

($infix "+." 100 134)
($nary  "+.")
(my-copy-properties '|$+.| 'mplus
		    '($nary lbp rbp
		      dimension dissym
		      tex texsym))

($infix "-." 100 134)
($nary  "-.")
(my-copy-properties '|$-.| 'mminus
		    '($nary lbp rbp
		      dimension dissym
		      tex texsym))

(setf (get '|$-.| 'tex) 'tex-infix)

#|
 «lazy-operations-tests»  (to ".lazy-operations-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
load("bad-foundations.lisp");

2 =. 3 =. 4;

 (find-maxima-op-links "^")
 (find-maxima-op-links "-")
to_lisp();
  (describe (define-symbol "^"))
  (describe (define-symbol "^."))
  (describe (define-symbol "-."))
  (to-maxima)

o : 5 -. 2;
tex1(o);

to_lisp();
        (get '|$-.| 'tex)
  (setf (get '|$-.| 'tex) 'tex-prefix)
  (setf (get '|$-.| 'tex) 'tex-infix)
  (to-maxima)

tex1(o);

|#


;; «lazy-nouns»  (to ".lazy-nouns")
;; "Lazy nouns" are like "lazy operations", but like this:
;;     diff(x^2, x)   <-- here `diff' is a verb
;;    'diff(x^2, x)   <-- here 'diff is a noun
;;    _diff(x^2, x)   <-- here _diff is a lazy noun
;;
;; Verbs do lots of simplifications.
;; Nouns do some simplifications.
;; My lazy nouns don't simplify anything.
;; See:
;;  (find-maximanode "Nouns and Verbs")
;;
;; Supersedes: (find-angg "MAXIMA/lazynouns.lisp")
;; except for the comments - lazynouns.lisp still has some comments
;; that I haven't ported to the newer versions of the code.

(my-copy-properties '$_diff      '%derivative '(dimension tex))
(my-copy-properties '$_integrate '%integrate  '(dimension tex))
(my-copy-properties '$_at        '%at         '(dimension tex))
(my-copy-properties '$_limit     '%limit      '(dimension tex))
(my-copy-properties '$_sqrt      '%sqrt       '(dimension tex))

#|
 «lazy-nouns-tests»  (to ".lazy-nouns-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("bad-foundations.mac");

[o1,o2] : [2*x^3, 1];
matrix(["", "diff",    "'diff",     "_diff"],
       [o1, diff(o1,x), 'diff(o1,x), _diff(o1,x)],
       [o2, diff(o2,x), 'diff(o2,x), _diff(o2,x)]);

[o1,o2] : [2*f(x), 2*x];
matrix(["", "integrate",    "'integrate",     "_integrate"],
       [o1, integrate(o1,x), 'integrate(o1,x), _integrate(o1,x)],
       [o2, integrate(o2,x), 'integrate(o2,x), _integrate(o2,x)]);

 at(a,b=c);
'at(a,b=c);
_at(a,b=c);
_limit(a,x,x0);

|#



;; «tex-_diff»  (to ".tex-_diff")
;; `diff' sometimes adds a `1' to its arguments - `diff(f(x),x)'
;; becomes `diff(f(x),x,1)'. Internally, this is:
;;
;;   (%i2) lisptree_config_lisp()$
;;   (%i3) lisptree2('diff(f(x),x));
;;                                       %DERIVATIVE__.___.
;;                           d           |            |   |
;;   (%o3)                   ── (f(x)) = $F           $X  1
;;                           dx          |                 
;;                                       $X                
;;
;; The hack below makes `_diff(u,x)' and`_diff(u,x,1)'
;; be LaTeXed in the same way. See:
;;   (find-maximanode "diff" "diff (<expr>, <x>)")
;;   (find-maximagitfile "src/comm.lisp" "(defmfun $diff ")
;;   (find-maximagitfile "src/comm.lisp" "(defun deriv ")
;;   (find-maximagitfile "src/comm.lisp" "(defun deriv " "(nconc e '(1))")
;;   (find-maximagitfile "src/mactex.lisp" "(defprop %derivative ")
;;   (find-maximagitfile "src/mactex.lisp" "(defun tex-derivative ")
;;      
(setf (get '$_diff 'tex) 'tex-derivative) ; default
(setf (get '$_diff 'tex) 'tex-_diff)	  ; uses the hack below

(defun tex-_diff (x l r)
  (destructuring-bind
   (head expr var1 . rest) x
   (declare (ignorable head))
   (if (not rest) (setq rest '(1)))
   (tex-derivative `((%derivative) ,expr ,var1 ,@rest) l r)))

#|
 «tex-_diff-tests»  (to ".tex-_diff-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("lazy-ops.lisp");
to_lisp();
  (setf (get '$_diff 'tex) 'tex-derivative) ; default
  (to-maxima)

o1 : 'diff(f(x),x);
o2 : _diff(f(x),x);
o3 : _diff(f(x),x,1);
args(o1);
args(o2);
args(o3);
tex1(o1);
tex1(o2);     "Bad"$
tex1(o3);

to_lisp();
  (setf (get '$_diff 'tex) 'tex-_diff) ; uses the hack
  (to-maxima)

args(o1);
args(o2);
args(o3);
tex1(o1);
tex1(o2);     "Good"$
tex1(o3);

|#







;; Local Variables:
;; coding:  utf-8-unix
;; End:
