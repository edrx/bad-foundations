;; This file:
;;   https://anggtwu.net/bad-foundations/display-delegate.lisp.html
;;   https://anggtwu.net/bad-foundations/display-delegate.lisp
;;           (find-angg "bad-foundations/display-delegate.lisp")
;;  Author: Eduardo Ochs <eduardoochs@gmail.com>
;;     See: https://anggtwu.net/eev-maxima.html#display_delegate
;; Licence: Public Domain
;; Version: 2026feb22
;;
;; This file defines the Maxima function `display_delegate', that
;; implements a way to define how certain Maxima objects will be
;; displayed - for example,
;;
;;   display_delegate('ccc, 'ccc_translate);
;; 
;; tells Maxima that an object like ccc(x,y,z) should be displayed by
;; first running ccc_translate(ccc(x,y,z)) and then delegating the
;; displaying to the result.

;; Index:
;; «.introduction»	(to "introduction")
;; «.prototypes»	(to "prototypes")
;; «.display_delegate»	(to "display_delegate")
;;   «.test-macro»	(to "test-macro")
;;   «.test-maxima»	(to "test-maxima")
;;   «.test-At2»	(to "test-At2")



;; «introduction»  (to ".introduction")
;; ==============
;; In the beginning of 2024 I saw that the package `qm-maxima' - in
;; <https://github.com/QMeqGR/qm-maxima> - displayed `bra(a)' as `<a|'
;; and `ket(a)' as '|a>'. I asked some questions in the mailing list,
;; and I explained that I was looking for a way to display the
;; coefficients of power series very compactly... in particular, I
;; wanted this:
;;
;;   (%i2) ccc(2,3,4);
;;   (%o2)              <2:3:4>
;;
;; These are the main messages in the mailing list:
;;
;;   (find-maximamsg "58827564 202410 12" "EricMajzoub: qm-maxima package")
;;   (find-maximamsg "59114438 202501 04" "Edrx: (displa-def $ket ...)")
;;   (find-maximamsg "59114461 202501 04" "RDodier: (defun reform-ccc ...)")
;;   (find-maximamsg "59114492 202501 04" "Edrx: that people can test in a Maxima REPL")
;;   (find-maximamsg "59114658 202501 04" "RDodier: second attempt")
;;
;;
;; «prototypes»  (to ".prototypes")
;; ============
;; Robert Dodier gave me several suggestions, and after many clean-ups
;; and and rewrites I converted his "second attempt" into this first
;; prototype:
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
to_lisp();
  (displa-def $aaa dimension-nary ":")
  (displa-def $bbb dimension-match "<" ">")
  (setf (get '$ccc 'dimension) 'dim-$ccc)

  (defun dim-$ccc (o result)
    (let ((new-o `(($bbb) (($aaa) ,@(rest o)))))
      (dimension new-o result 'mparen 'mparen nil 0)))

  (to-maxima)

    aaa(2,3,4);     /*  2:3:4  */
bbb(aaa(2,3,4));    /* <2:3:4> */
    ccc(2,3,4);     /* <2:3:4> */
 op(ccc(2,3,4));    /*   ccc   */

|#
;;
;; In the first prototype above when Maxima wants to display
;; `ccc(2,3,4)' the function `dim-$ccc' is called with `o' being
;; `ccc(2,3,4)', and it sets `new-o' to `bbb(aaa(2,3,4))' - using a
;; translation that is hardcoded in Lisp. Here is a way to do that
;; translation in Maxima - the "second prototype":
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
ccc_translate(o) := bbb(apply('aaa, args(o)));
ccc_translate(ccc(2,3,4));          /* bbb(aaa(2,3,4)) */

to_lisp();
  (displa-def $aaa dimension-nary ":")
  (displa-def $bbb dimension-match "<" ">")
  (setf (get '$ccc 'dimension) 'dim-$ccc)

  (defun dim-$ccc (o result)
    (let ((new-o (mfuncall '$ccc_translate o)))         ;; <- new
      (dimension new-o result 'mparen 'mparen nil 0)))

  (to-maxima)

    aaa(2,3,4);     /*  2:3:4  */
bbb(aaa(2,3,4));    /* <2:3:4> */
    ccc(2,3,4);     /* <2:3:4> */
 op(ccc(2,3,4));    /*   ccc   */

|#



;; «display_delegate»  (to ".display_delegate")
;; ==================
;; The Maxima function `$display_delegate', defined below, implements
;; a way to define how certain Maxima objects will be displayed - for
;; example,
;;
;;   display_delegate('ccc, 'ccc_translate);
;; 
;; tells Maxima that an object like ccc(x,y,z) should be displayed by
;; first running ccc_translate(ccc(x,y,z)) and then delegating the
;; displaying to the result.
;;
;; Note that `$display_delegate' "is" a Lisp macro defined in a funny
;; way, that uses `display_delegate-3' and `display_delegate-2' to
;; generate a progn. I find that style easier to understand and to
;; debug than a standard macro.
;;
;; Old version: (find-angg "MAXIMA/displa-delegate.lisp")
;;
(defun display_delegate-3 ($CCC $CCC_TRANSLATE DIM-CCC)
  `(progn
     ;;
     (setf (get ',$CCC 'dimension) ',DIM-CCC)
     (defun ,DIM-CCC (o result)
       (let* ((translated-o (mfuncall ',$CCC_TRANSLATE o))
	      (op-translated-o (caar translated-o))
	      (dim-translated-o (get op-translated-o 'dimension))
	      )
	 ;; (dimension translated-o result 'mparen 'mparen nil 0) ; <- fails in At2! Why?
	 (funcall (or dim-translated-o #'dimension-function) translated-o result)
	 ))
     ;;
     ))

(defun display_delegate-2 ($CCC $CCC_TRANSLATE)
  (let ((DIM-CCC (intern (format nil "DIM-~a" (symbol-name $CCC)))))
    (display_delegate-3 $CCC $CCC_TRANSLATE DIM-CCC)))

(defun $display_delegate ($CCC &optional $CCC_TRANSLATE)
  (if $CCC_TRANSLATE
      (eval (display_delegate-2 $CCC $CCC_TRANSLATE))
      (setf (get $CCC 'dimension) nil)))


;; «test-macro»  (to ".test-macro")
;; ------------
;; These tests show how `$display_delegate' works internally.
;; Compare the results of the calls to `display_delegate-3' and
;; `display_delegate-2' below with the result of the quoted progn
;; and with the code in the "second prototype" above.
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
to_lisp();
  (load "display-delegate.lisp")

  ' (progn
      ;;
      (setf (get '$ccc 'dimension) 'dim-$ccc)
      (defun dim-$ccc (o result)
        (let ((new-o (mfuncall '$ccc_translate o)))
        (dimension new-o result 'mparen 'mparen nil 0)))
      ;;
    )

  (display_delegate-3 '$ddd '$ddd_translate 'dim-$ddd)
  (display_delegate-3 '$ccc '$ccc_translate 'dim-$ccc)
  (display_delegate-2 '$ccc '$ccc_translate)
  (to-maxima)

|#


;; «test-maxima»  (to ".test-maxima")
;; -------------
;; These tests show how to use `display_delegate'
;; from Maxima.
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("display-delegate.lisp");
ccc_translate(o) := bbb(apply('aaa, args(o)));

          bbb(aaa(2,3,4));   /* bbb(aaa(2,3,4)) */
ccc_translate(ccc(2,3,4));   /* bbb(aaa(2,3,4)) */
              ccc(2,3,4);    /*     ccc(2,3,4)  */
           op(ccc(2,3,4));   /*     ccc         */

display_delegate('ccc, 'ccc_translate);

          bbb(aaa(2,3,4));   /* bbb(aaa(2,3,4)) */
ccc_translate(ccc(2,3,4));   /* bbb(aaa(2,3,4)) */
              ccc(2,3,4);    /* bbb(aaa(2,3,4)) */
           op(ccc(2,3,4));   /*     ccc         */

to_lisp();
  (displa-def $aaa dimension-nary ":")
  (displa-def $bbb dimension-match "<" ">")
  (to-maxima)

          bbb(aaa(2,3,4));   /*        <2:3:4>  */
ccc_translate(ccc(2,3,4));   /*        <2:3:4>  */
              ccc(2,3,4);    /*        <2:3:4>  */
           op(ccc(2,3,4));   /*     ccc         */

display_delegate('ccc);      /* deactivate the delegation */
              ccc(2,3,4);    /* bbb(aaa(2,3,4)); */

|#


;; «test-At2»  (to ".test-At2")
;; The "right way" to display antiderivatives is explained here:
;;   https://anggtwu.net/maxima-edrxbox.html#antideriv
;;   (find-baf "edrxbox-examples.lisp" "antideriv")
;;
;; Sometimes we don't way the display a new object in the "right way" -
;; a "quick way" is enough, like in the example below, that yields:
;;
;;   (%i5) o : At2(g(u),u,2,3);
;;                                  ┌                ┐
;;                                  │       |  u = 3 │
;;                                  │                │
;;   (%o5)                          │ g(u)  |        │
;;                                  │                │
;;                                  │       |  u = 2 │
;;                                  └                ┘
;;   (%i6) [op(o), args(o)];
;;   (%o6)                        [At2, [g(u), u, 2, 3]]
;;
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("display-delegate.lisp")$

display_delegate('At2, 'At2_translate);
At2_translate(o) := block(
    [fx,x,a,b],
    [fx,x,a,b] : args(o),
    matrix(["", "|", x=b],
           [fx, "|",  ""],
           ["", "|", x=a]))$

o : At2(g(u),u,2,3);
[op(o), args(o)];

|#

















