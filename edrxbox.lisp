\;; This file:
;;   https://anggtwu.net/bad-foundations/edrxbox.lisp.html
;;   https://anggtwu.net/bad-foundations/edrxbox.lisp
;;           (find-angg "bad-foundations/edrxbox.lisp")
;;                            (find-baf "edrxbox.lisp")
;;  Author: Eduardo Ochs <eduardoochs@gmail.com>
;;     See: https://anggtwu.net/maxima-edrxbox.html
;;          https://anggtwu.net/bad-foundations/edrxbox-examples.lisp.html
;;          https://anggtwu.net/bad-foundations/edrxbox-examples.lisp
;; Version: 2026mar01
;; License: GPL v2
;;
;; This file implements a simpler way to write `dim-*' functions.
;; The pretty-printer of Maxima is explained here,
;;   (find-maximanode "display2d")
;; with this example:
;;
;;   (%i1) x/(x^2+1);
;;                      x
;;   (%o1)            ------
;;                     2
;;                    x  + 1
;;
;; The example above calls the functions `dim-mquotient'
;; and `dimension-superscript' - two `dim-*' functions.
;; They are defined here:
;;
;;   (find-maximagitfile "src/displa.lisp" "(defun dim-mquotient ")
;;   (find-maximagitfile "src/displa.lisp" "(defun dimension-superscript ")
;;
;; It is very difficult to write new `dim-*' functions in the style
;; used in "displa.lisp" because the user/programmer has to write the
;; drawing instructions in a cryptic DSL in which many parts of the
;; code look reversed, and has to keep several variables, like the box
;; boundaries, in sync and with sane values, "by hand".
;; 
;; This is a work in progress, and the "edrx" in the names of its
;; functions is to remind people that I still intend to rewrite this
;; code and its comments several times before declaring that this code
;; is mature - at this moment it _may be_ just a private hack.
;;
;; For an introduction with screenshots, see:
;;
;;   https://anggtwu.net/maxima-edrxbox.html
;;
;; Some edrx-isms:
;;   (defun o () (interactive) (find-baf "edrxbox.lisp"))
;;   (defun e () (interactive) (find-baf "edrxbox-examples.lisp"))
;;   (defun oe () (interactive) (find-2a '(o) '(e)))


;; Introduction:
;;   «.low-level-terminology»	(to "low-level-terminology")
;;   «.the-displa-DSL»		(to "the-displa-DSL")
;;   «.edrxboxes»		(to "edrxboxes")
;;
;; Core:
;;   «.variables»		(to "variables")
;;   «.conversions»		(to "conversions")
;;   «.conversions-tests»	(to "conversions-tests")
;;   «.bounds»			(to "bounds")
;;   «.run-in-edrxbox»		(to "run-in-edrxbox")
;;   «.run-in-edrxbox-tests»	(to "run-in-edrxbox-tests")
;;   «.debug»			(to "debug")
;;   «.push»			(to "push")
;;   «.append»			(to "append")
;;   «.move»			(to "move")
;;   «.center»			(to "center")

;; Non-trivial `dim-*' functions implemented with edrxbox:
;;   «.underbrace»		(to "underbrace")
;;   «.underbrace-tests»	(to "underbrace-tests")
;;   «.verbatimbox»		(to "verbatimbox")
;;   «.verbatimbox-tests»	(to "verbatimbox-tests")
;;   «.hdw»			(to "hdw")
;;   «.hdw-tests»		(to "hdw-tests")
;;


;;;  _                       _                _ 
;;; | |    _____      __    | | _____   _____| |
;;; | |   / _ \ \ /\ / /____| |/ _ \ \ / / _ \ |
;;; | |__| (_) \ V  V /_____| |  __/\ V /  __/ |
;;; |_____\___/ \_/\_/      |_|\___| \_/ \___|_|
;;;                                             
;; «low-level-terminology»  (to ".low-level-terminology")
;;
;; Some low-level terminology
;; ==========================
;; When `display2d' is true Maxima draws objects by running the
;; `dim-*' functions inside the environment created by `displa'.
;; The code is here:
;;
;;   (find-maximanode "display2d")
;;   (find-maximagitfile "src/displa.lisp" "(defun displa ")
;;   (find-maximagitfile "src/displa.lisp"   "(cond ($display2d")
;;   (find-maximagitfile "src/displa.lisp"   "(dimension form ")
;;   (find-maximagitfile "src/displa.lisp"   "(output ")
;;   (find-maximagitfile "src/displa.lisp" "(defun dimension ")
;;   (find-maximagitfile "src/displa.lisp"   "(safe-get (caar form) 'dimension)")
;;
;; Here are some `dim-*' functions:
;;
;;   (find-maximagitfile "src/displa.lisp" "(defun dim-mquotient ")
;;   (find-maximagitfile "src/displa.lisp" "(defun dim-$matrix ")
;;   (find-maximagitfile "src/displa.lisp" "(defun dim-mbox ")
;;   (find-maximagitfile "src/displa.lisp" "(defun dimension-superscript ")
;;
;; This code shows how to get from "^" to `dimension-superscript':
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
to_lisp();
                  #$a^b$
            (caar #$a^b$)
  (safe-get (caar #$a^b$) 'dimension)

                      (define-symbol "^")
            (describe (define-symbol "^"))
                 (get (define-symbol "^") 'mheader)
            (car (get (define-symbol "^") 'mheader))
  (describe (car (get (define-symbol "^") 'mheader)))
       (get (car (get (define-symbol "^") 'mheader)) 'dimension)
|#
;; I will say, by a trivial abuse of language, that:
;;
;;     the `mheader' property of   "^"   is `mexpt',
;;   the `dimension' property of `mexpt' is `dimension-superscript',
;;   the `dimension' property of   "^"   is `dimension-superscript',
;;
;; Usually dimension properties are assigned by running the macro
;; `displa-def'. For example:
;;
;;   (find-maximagitfile "src/displa.lisp" "(displa-def mquotient ")
;;   (find-maximagitfile "src/displa.lisp" "(displa-def mexpt" "dimension-superscript)")
;;   (find-maximagitfile "src/displa.lisp" "(displa-def $matrix dim-$matrix)")
;;
;; but when people discuss setting dimension properties in the mailing
;; list they usually usually use a `setf' directly instead of calling
;; `displa-def' - they use this idiom:
;;
;;   (setf (get '$barematrix 'dimension) 'dim-$barematrix)
;;
;; I use that a lot. See, for example:
;;
;;   (find-angg "MAXIMA/2025-displa-tex.lisp" "barematrix")



;;;  _____ _                _ _           _         ____  ____  _     
;;; |_   _| |__   ___    __| (_)___ _ __ | | __ _  |  _ \/ ___|| |    
;;;   | | | '_ \ / _ \  / _` | / __| '_ \| |/ _` | | | | \___ \| |    
;;;   | | | | | |  __/ | (_| | \__ \ |_) | | (_| | | |_| |___) | |___ 
;;;   |_| |_| |_|\___|  \__,_|_|___/ .__/|_|\__,_| |____/|____/|_____|
;;;                                |_|                                
;;
;; «the-displa-DSL»  (to ".the-displa-DSL")
;;
;; The displa language
;; ===================
;; One of the arguments of the `dim-*' functions is always a variable
;; called "result", that contains drawing instructions; that variable
;; is modified by the `dim-*' function - with a series of "push"es -
;; and its new value is returned. The drawing instructions are in a
;; language, or DSL, that is described VERY briefly in these comments
;; in "displa.lisp":
;;
;;   <dimension string> ::= () | (<string element> . <dimension string>)
;;   <string element> ::= character |
;;                     (<column-relative> <row-relative> . <dimension string>) |
;;                     (<drawing function> . args)
;;   <column-relative> ::= <fixnum>
;;   <row-relative>    ::= <fixnum>
;;   <drawing function> ::= D-HBAR | D-VBAR | D-INTEGRALSIGN | ...
;;
;;   When a character appears in a dimension string, it is printed and
;;   the cursor moves forward a single position.  (The variable OLDCOL is
;;   incremented)  When a form with a fixnum car is encountered, the
;;   first two elements of the form are taken to be relative displacements
;;   for OLDCOL and OLDROW.  *** NOTE *** After drawing the cddr of the form,
;;   OLDROW is reset to its original value, but OLDCOL is left in the new
;;   position.  Why this is done is beyond me.  It only appears to complicate
;;   things.
;;
;; Here is a link to where the comments apprear in the source - they
;; appear just above the "defun output":
;;
;;   (find-maximagitfile "src/displa.lisp" "(defun output ")
;;
;; I will refer to that language as the "displa language", or the
;; "displa DSL". Here is an example of code in that in that language,
;;
;;   ((3 0)
;;    (-2 -2 #\g)
;;    (-4 -1 #\f #\e)
;;    (0 0 #\d #\c #\b #\a))
;;
;; and a translation of it for humans - note that many things in it
;; look reversed:
;;
;;   Start at (x,y)=(0,0). Typeset "abcd". Now (x,y)=(4,0).
;;   Move by x+=-4, y=-1. Now (x,y)=(0,-1). Typeset "ef". Now (x,y)=(2,-1).
;;   Move by x+=-2, y=-2. Now (x,y)=(0,-2). Typeset "g". Now (x,y)=(1,-2).
;;   Move by x+=3, y=0. Now (x,y)=(4,0), and we're after the "d" of the "abcd".
;;
;; Besides modifying the "result" the `dim-*' functions also return
;; the height, depth, and width of what they drew - and they do that
;; by changing the global variables `height', `depth', and `width'.
;; Also, in some cases - here I don't understand the rules well - the
;; `dim-*' functions are expected to set (x,y)=(width,0) before
;; returning, and some things may break when that convention is not
;; followed.
;;
;; The displa DSL only started to make sense to me when I studied, and
;; understood, Robert Dodier's `dim-antideriv', that is written in a
;; style that is more modern than the one used in "displa.lisp". See:
;;
;;   (find-maximamsg "59178281 202504 29" "RDodier: dim-antideriv.lisp")
;;   (find-maximamsg "59266596 202511 30" "RDodier/Edrx: One less bug...")
;;   https://anggtwu.net/MAXIMA/dim-antideriv.lisp.html
;;   https://anggtwu.net/MAXIMA/dim-antideriv.lisp
;;           (find-angg "MAXIMA/dim-antideriv.lisp")


;;;  _____    _           _                        
;;; | ____|__| |_ ____  _| |__   _____  _____  ___ 
;;; |  _| / _` | '__\ \/ / '_ \ / _ \ \/ / _ \/ __|
;;; | |__| (_| | |   >  <| |_) | (_) >  <  __/\__ \
;;; |_____\__,_|_|  /_/\_\_.__/ \___/_/\_\___||___/
;;;                                                
;; «edrxboxes»  (to ".edrxboxes")
;;
;; The edrxbox "language"
;; ======================
;; Here is an alternative - or, rather, a front-end - to the displa
;; language. If we run this
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
to_lisp();
  (load "edrxbox.lisp")
  (run-in-edrxbox
    (edrxboxvars-push-x-y-string 0 0 "abcd")
    (edrxboxvars-push-x-y-string 0 -1 "de")
    (edrxboxvars-push-x-y-string 0 -2 "f")
    (edrxboxvars-push-x-y-end)
    )
  (to-maxima)

|#
;; the output of the `run-in-edrxbox' sexp is this plist:
;;
;;   (:X 4 :Y 0 :HEIGHT 1 :DEPTH 2 :WIDTH 4
;;    :RESULT ((3 0)
;;             (-2 -2 #\f)
;;             (-4 -1 #\e #\d)
;;             (0 0 #\d #\c #\b #\a)))
;;
;; Here's how the `(run-in-edrxbox CODE)' above works.
;; `run-in-edrxbox' is a macro that:
;;
;;   1. uses a `let*' to create an environment in which the
;;      global/special variables `box-x', `box-y', `box-height',
;;      `box-depth', `box-width' and `box-result' describe an empty
;;      box,
;;
;;   2. runs the sexps in CODE,
;;
;;   3. returns the values of the `box-*' variables in a plist, and
;;      closes the `let*'.
;;
;;   4. The `edrxbox-*' functions keep track of the current (x,y)
;;      position, of the size of the drawing, and of the current
;;      "result" in the `box-*' variables, and adjust them at every
;;      step.
;;
;; If we run the example below - in which we replaced `run-in-edrxbox'
;; of the example above by a `run-in-edrxbox-dim' inside a `dim-*'
;; function,
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
to_lisp();
  (load "edrxbox.lisp")
  (setf (get '$foo 'dimension) 'dim-$foo)
  (defun dim-$foo (form result)
    (declare (ignorable form result))
    (run-in-edrxbox-dim
      (edrxboxvars-push-x-y-string 0 0 "abcd")
      (edrxboxvars-push-x-y-string 0 -1 "de")
      (edrxboxvars-push-x-y-string 0 -2 "f")
      (edrxboxvars-push-x-y-end)))
  (to-maxima)

linel : 40;
[box(foo())];
|#
;; The output of the last line is:
;;
;;   (%i3) [box(foo())];
;;                    ╔════╗
;;   (%o3)           [║abcd║]
;;                    ║de  ║
;;                    ║f   ║
;;                    ╚════╝


;;; __     __         _       _     _           
;;; \ \   / /_ _ _ __(_) __ _| |__ | | ___  ___ 
;;;  \ \ / / _` | '__| |/ _` | '_ \| |/ _ \/ __|
;;;   \ V / (_| | |  | | (_| | |_) | |  __/\__ \
;;;    \_/ \__,_|_|  |_|\__,_|_.__/|_|\___||___/
;;;                                             
;; «variables»  (to ".variables")
;; The code in "displm.lisp" and "displa.lisp" keep the state of the
;; current box in many special/global variables - `height', `depth',
;; `width', and many others - and put the drawing instructions in
;; `result', that is always an argument and a return value instead of
;; a being a global variable.
;;
;; I will refer to the variables `height', `depth', `width', and
;; `result' - as the "dimvars".
;;
;; Here we declare the special variables `box-height', `box-depth',
;; `box-width', `box-result', `box-x' and `box-y'; I will call them
;; the "edrxboxvars". By keeping the current (x,y) position in the
;; variables `box-x' and `box-y' we can make our code much simpler -
;; in the sense of more high-level, easier to read, write, and debug.
;;
;; See:
;;   (find-maximagitfile "src/displm.lisp" " (special")
;;   (find-maximagitfile "src/displa.lisp" "(defun displa ")
;;   (find-maximagitfile "src/displa.lisp" "(defun displa " "width")
;;   (find-maximagitfile "src/displa.lisp" "(defun dim-mquotient ")
;;   (find-maximagitfile "src/displa.lisp" "(defun dim-$matrix ")
;;   (find-maximagitfile "src/displa.lisp" "(defun dim-mbox ")

(defvar box-x)
(defvar box-y)
(defvar box-width)
(defvar box-height)
(defvar box-depth)
(defvar box-result)


;;;   ____                              _                 
;;;  / ___|___  _ ____   _____ _ __ ___(_) ___  _ __  ___ 
;;; | |   / _ \| '_ \ \ / / _ \ '__/ __| |/ _ \| '_ \/ __|
;;; | |__| (_) | | | \ V /  __/ |  \__ \ | (_) | | | \__ \
;;;  \____\___/|_| |_|\_/ \___|_|  |___/_|\___/|_| |_|___/
;;;                                                       
;; «conversions»  (to ".conversions")
;; Our basic conversions are these ones:
;;                                       
;;   dimvars <--> edrxboxvars <--> edrxbox <--- string
;;                                         <--- from
;; Some of them are very tricky.

(defun edrxboxvars-to-edrxbox ()
  (list :x      box-x
	:y      box-y
	:height box-height
	:depth  box-depth
	:width  box-width
	:result box-result))

(defun edrxboxvars-from-edrxbox (edrxbox)
  (setq box-x      (getf edrxbox :x)
        box-y      (getf edrxbox :y)
        box-height (getf edrxbox :height)
        box-depth  (getf edrxbox :depth)
        box-width  (getf edrxbox :width)
        box-result (getf edrxbox :result)))

(defmacro edrxboxvars-to-dimvars ()
"Export the current `box-*' variables to `height', `depth', `width', and `result'.
`result' is always a local variable, so this needs to be a macro.
This macro is called at the end of `run-in-edrxbox-dim'."
  `(progn
     (setq height box-height
	   depth  box-depth
	   width  box-width
	   result `(,@box-result ,@result)) ; <- non-idempotent!
     (update-heights height depth)
     result))

(defun edrxbox-from-string (string)
  (let* ((restring (reverse (exploden string)))
	 (width (length restring)))
    (list :height 1 :depth 0 :width width :result restring)))

;; See:
;; (find-maximagitfile "src/displa.lisp" "(defun displa " "(let ")
;; (find-maximagitfile "src/displa.lisp" "(defun displa " "(dimension ")
(defun edrxbox-from-form (form)
  (let* ((level 0)
	 (size 2)
	 (result (dimension form nil 'mparen 'mparen nil 0))) ; changes the dimvars!
    (list :height height :depth depth :width width :result result)))

;; «conversions-tests»  (to ".conversions-tests")
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
o1 : 'integrate(f(x), x, a, b);
to_lisp();
  (load "edrxbox.lisp")
  (edrxbox-from-string "abcd")
  (edrxbox-from-form #$o1$)
  (to-maxima)

|#


;;;                         _                      _           _               
;;;  _ __ _   _ _ __       (_)_ __         ___  __| |_ ____  _| |__   _____  __
;;; | '__| | | | '_ \ _____| | '_ \ _____ / _ \/ _` | '__\ \/ / '_ \ / _ \ \/ /
;;; | |  | |_| | | | |_____| | | | |_____|  __/ (_| | |   >  <| |_) | (_) >  < 
;;; |_|   \__,_|_| |_|     |_|_| |_|      \___|\__,_|_|  /_/\_\_.__/ \___/_/\_\
;;;                                                                            
;; «run-in-edrxbox»  (to ".run-in-edrxbox")
;; These functions and macros implement the environments
;; `run-in-edrxbox' and `run-in-edrxbox-dim'. I still don't
;; know in which cases `edrxbox-push-x-y-end' is needed, so
;; I'm keeping it out the `run-*' functions - we have to
;; call it explicitly.

(defmacro run-in-edrxbox (&rest code)
  "Run CODE in an edrxbox environment, and return an edrxbox."
  `(let* ((box-x 0) (box-y 0)
	  (box-height 1) (box-depth 0) (box-width 0)
	  (box-result nil))
     ,@code
     (edrxboxvars-to-edrxbox)))

(defmacro run-in-edrxbox-dim (&rest code)
  "This is a variant of `run-in-edrxbox' that runs CODE inside a `dim-*' function."
  `(let* ((box-x 0) (box-y 0)
	  (box-height 1) (box-depth 0) (box-width 0)
	  (box-result nil))
     ,@code
     (edrxboxvars-to-dimvars)))

;; «run-in-edrxbox-tests»  (to ".run-in-edrxbox-tests")
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
to_lisp();

  (defun foo-core ()
    (edrxboxvars-push-x-y-string 0 0 "abcd")
    (edrxboxvars-push-x-y-string 0 -1 "de")
    (edrxboxvars-push-x-y-string 0 -2 "f")
    (edrxboxvars-push-x-y-end))

  (run-in-edrxbox (foo-core))

  (setf (get '$foo 'dimension) 'dim-$foo)
  (defun dim-$foo (form result)
    (declare (ignorable form result))
    (run-in-edrxbox-dim (foo-core)))

  (to-maxima)

[box(foo())];

|#




;; «debug»  (to ".debug")
;; For print debugging.
(defun pr (&rest list) (format t "~S~%" list))
(defun prall ()
  (pr :x box-x :y box-y
      :height box-height :depth box-depth :width box-width
      :result box-result))


;; «bounds»  (to ".bounds")
(defun edrxboxvars-adjust-bounds (x y edrxbox)
  "This function adjusts the box size in the `box-*' variables
to make sure that the given edrxbox - draw at (x,y) - fits inside it."
  (setq box-height (max box-height (+ (getf edrxbox :height) y))
	box-depth  (max box-depth  (- (getf edrxbox :depth)  y))
	box-width  (max box-width  (+ (getf edrxbox :width)  x))))


;; «push»  (to ".push")
(defun edrxboxvars-push (item) (push item box-result))

(defun edrxboxvars-push-x-y-edrxbox (x y edrxbox)
  (edrxboxvars-push `(,(- x box-x) ,y ,@(getf edrxbox :result)))
  (edrxboxvars-adjust-bounds x y edrxbox)
  (setq box-x (+ x (getf edrxbox :width))
	box-y y))

(defun edrxboxvars-push-x-y-string (x y string)
  (edrxboxvars-push-x-y-edrxbox x y (edrxbox-from-string string)))

(defun edrxboxvars-push-x-y-end ()
  "Push to `box-results' a command to move to (x,y)=(box-width,0)."
  (edrxboxvars-push-x-y-string box-width 0 ""))


;; «append»  (to ".append")
(defun edrxboxvars-append-edrxbox (edrxbox)
  (edrxboxvars-push-x-y-edrxbox box-width 0 edrxbox))

(defun edrxboxvars-append-string (string)
  (edrxboxvars-append-edrxbox (edrxbox-from-string string)))

(defun edrxboxvars-append-form (form)
  (edrxboxvars-append-edrxbox (edrxbox-from-form form)))

(defun edrxboxvars-append-forms (forms &optional sep)
  (if (< 0 (length forms)) (edrxboxvars-append-form (car forms)))
  (loop for form in (cdr forms)
        do (edrxboxvars-append-string (or sep ", "))
        do (edrxboxvars-append-form form)))

(defun edrxboxvars-append-function (f-name f-args &optional o sep c)
  "This function can be used to write variants of `dimension-function'."
  (if (stringp f-name)
      (edrxboxvars-append-string f-name)
      (edrxboxvars-append-form   f-name))
  (edrxboxvars-append-string       (or o   "("))
  (edrxboxvars-append-forms f-args (or sep ", "))
  (edrxboxvars-append-string       (or c   ")")))


;; «move»  (to ".move")
(defun edrxbox-move (dx dy edrxbox)
  (setq dx (truncate dx))
  (setq dy (truncate dy))
  (run-in-edrxbox
   (edrxboxvars-push-x-y-edrxbox dx dy edrxbox)
   (edrxboxvars-push-x-y-end)))


;; «center»  (to ".center")
(defun edrxbox-hcenter (bigwidth edrxbox)
  (let* ((edrxboxwidth (getf edrxbox :width))
	 (extrawidth (- bigwidth edrxboxwidth)))
    (edrxbox-move (/ extrawidth 2) 0 edrxbox)))

(defun edrxbox-vcenter (edrxbox)
  (let* ((h (getf edrxbox :height))
	 (d (getf edrxbox :depth))
	 (extradepth (- d (- h 1))))
    (edrxbox-move 0 (/ extradepth 2) edrxbox)))


;; Local Variables:
;; coding:  utf-8-unix
;; End:


