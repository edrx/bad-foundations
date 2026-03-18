;; This file:
;;   https://anggtwu.net/bad-foundations/edrxbox-examples.lisp.html
;;   https://anggtwu.net/bad-foundations/edrxbox-examples.lisp
;;           (find-angg "bad-foundations/edrxbox-examples.lisp")
;;                            (find-baf "edrxbox-examples.lisp")
;; Author: Eduardo Ochs <eduardoochs@gmail.com>
;;     See: https://anggtwu.net/maxima-edrxbox.html
;;          https://anggtwu.net/bad-foundations/edrxbox.lisp.html
;;          https://anggtwu.net/bad-foundations/edrxbox.lisp
;; Version: 2026mar01
;; License: GPL v2

;; This file contains several examples of `dim-*' functions
;; implemented with edrxbox. See the links above!
;;
;; Some edrx-isms:
;;   (defun o () (interactive) (find-baf "edrxbox.lisp"))
;;   (defun e () (interactive) (find-baf "edrxbox-examples.lisp"))
;;   (defun oe () (interactive) (find-2a '(o) '(e)))

;; Index:
;; «.verbatimbox»		(to "verbatimbox")
;; «.verbatimmatrix»		(to "verbatimmatrix")
;; «.verbatimbox-tests»		(to "verbatimbox-tests")
;; «.verbatimmatrix-tests»	(to "verbatimmatrix-tests")
;; «.hdw»			(to "hdw")
;; «.hdw-tests»			(to "hdw-tests")
;; «.underbrace»		(to "underbrace")
;; «.underbrace-tests»		(to "underbrace-tests")
;; «.antideriv»			(to "antideriv")
;; «.antideriv-tests»		(to "antideriv-tests")
;; «.overset»			(to "overset")
;; «.overset-tests»		(to "overset-tests")
;; «.dim-edrxbox»		(to "dim-edrxbox")
;; «.dim-edrxbox-tests»		(to "dim-edrxbox-tests")
;; «._d»			(to "_d")
;; «._d-tests»			(to "_d-tests")
;; «.myintegrate»		(to "myintegrate")
;; «.myintegrate-tests»		(to "myintegrate-tests")



;;; __     __        _           _   _           _               
;;; \ \   / /__ _ __| |__   __ _| |_(_)_ __ ___ | |__   _____  __
;;;  \ \ / / _ \ '__| '_ \ / _` | __| | '_ ` _ \| '_ \ / _ \ \/ /
;;;   \ V /  __/ |  | |_) | (_| | |_| | | | | | | |_) | (_) >  < 
;;;    \_/ \___|_|  |_.__/ \__,_|\__|_|_| |_| |_|_.__/ \___/_/\_\
;;;                                                              
;; «verbatimbox»     (to ".verbatimbox")
;; «verbatimmatrix»  (to ".verbatimmatrix")

(setf (get '$verbatimbox 'dimension)
      'dim-$verbatimbox)

(setf (get '$verbatimmatrix 'dimension)
      'dim-$verbatimmatrix)

(defun verbatimbox-core (strings)
  (let* ((nstrings (length strings)))
    (loop for string in strings
	  for k from 0 to (- nstrings 1)
	  do (edrxboxvars-push-x-y-string 0 (- k) string))
    (edrxboxvars-push-x-y-end)))

(defun verbatimbox-edrxbox (strings)
  (run-in-edrxbox
   (verbatimbox-core strings)))

(defun verbatimbox-edrxbox-centered (strings)
  (edrxbox-vcenter
   (verbatimbox-edrxbox strings)))

(defun dim-$verbatimbox (form result)
  (let* ((strings (rest form)))
    (run-in-edrxbox-dim
     (edrxboxvars-from-edrxbox
      (verbatimbox-edrxbox-centered strings)))))

(defun dim-$verbatimmatrix (form result)
  (let* ((lines   (rest form))
	 (strings (map 'list #'second lines)))
    (run-in-edrxbox-dim
     (edrxboxvars-from-edrxbox
      (verbatimbox-edrxbox-centered strings)))))

;; «verbatimbox-tests»  (to ".verbatimbox-tests")
;; «verbatimmatrix-tests»  (to ".verbatimmatrix-tests")
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
o1 : verbatimbox("abcd",   "ef", "g");
o2 : verbatimbox("abcdoo", "ef", "g", "h", "i", "j");
o3 : verbatimmatrix(["abc"], ["de"]);
[box(o1)];
[box(o2)];
to_lisp();
  (load "edrxbox-examples.lisp")
  (verbatimbox-edrxbox          '("a" "b" "c" "d" "e"))
  (verbatimbox-edrxbox-centered '("a" "b" "c" "d" "e"))
  (to-maxima)

|#



;;;  _   _ ______        __
;;; | | | |  _ \ \      / /
;;; | |_| | | | \ \ /\ / / 
;;; |  _  | |_| |\ V  V /  
;;; |_| |_|____/  \_/\_/   
;;;                        
;; «hdw»  (to ".hdw")
;; Construct a box of a certain width, depth, and height.
;; The resulting box looks like this, but without the "|" and "-"s:
;;
;;   h
;;   |
;;   o-------w
;;   |
;;   |
;;   d
;;
;; These hdw-boxes are useful to debug other `dim-*' functions -
;; like antideriv, below.
;;
(setf (get '$hdw 'dimension)
      'dim-$hdw)

(defun dim-hdw-core (h d w)
  (edrxboxvars-push-x-y-string 0 0 "+")
  (if (> h 1) (edrxboxvars-push-x-y-string 0 (- h 1) "h"))
  (if (> d 0) (edrxboxvars-push-x-y-string 0 (- d)   "d"))
  (if (> w 1) (edrxboxvars-push-x-y-string (- w 1) 0 "w"))
  (edrxboxvars-push-x-y-end))

(defun dim-$hdw (form result)
  (let* ((args (rest form))
	 (h (first args))
	 (d (second args))
	 (w (third args)))
    (if (not (and (eq (length args) 3)
		  (integerp h) (>= h 1)
		  (integerp d) (>= d 0)
		  (integerp w) (>= w 1)))
	(dimension-function form result)
	(run-in-edrxbox-dim
	 (dim-hdw-core h d w)))))

;; «hdw-tests»  (to ".hdw-tests")
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
box(hdw());
box(hdw(a,b,c));
box(hdw(1,0,1));
[box(hdw(2,1,3))];
[box(hdw(3,4,6))];

|#



;;;              _   _     _           _       
;;;   __ _ _ __ | |_(_) __| | ___ _ __(_)_   __
;;;  / _` | '_ \| __| |/ _` |/ _ \ '__| \ \ / /
;;; | (_| | | | | |_| | (_| |  __/ |  | |\ V / 
;;;  \__,_|_| |_|\__|_|\__,_|\___|_|  |_| \_/  
;;;                                            
;; «antideriv»  (to ".antideriv")
;;        See: https://anggtwu.net/maxima-edrxbox.html#antideriv
;; Supersedes: (find-angg "MAXIMA/dim-antideriv.lisp")

(setf (get '$antideriv 'dimension)
      'dim-antideriv)

(defun dim-antideriv-core (a b c)
  (let* ((a-box      (edrxbox-from-form a))
         (b-box      (edrxbox-from-form b))
         (c-box      (edrxbox-from-form c))
         (a-height   (getf a-box :height))
         (b-height   (getf b-box :height))
         (c-height   (getf c-box :height))
         (a-depth    (getf a-box :depth))
         (b-depth    (getf b-box :depth))
         (c-depth    (getf c-box :depth))
         (abc-height (max a-height (+ b-height b-depth 1)))
         (abc-depth  (max a-depth  (+ c-height c-depth)))
         (b-y        (- abc-height b-height))
         (c-y        (- c-depth abc-depth))
         (vbar-x     (getf a-box :width))
         (bc-x       (+ vbar-x 1))
         (vbar-box   (list :height abc-height :depth abc-depth :width 1
                           :result `((d-vbar ,abc-height ,abc-depth)))))
    (edrxboxvars-push-x-y-edrxbox 0      0 a-box)
    (edrxboxvars-push-x-y-edrxbox vbar-x 0 vbar-box)
    (edrxboxvars-push-x-y-edrxbox bc-x b-y b-box)
    (edrxboxvars-push-x-y-edrxbox bc-x c-y c-box)
    (edrxboxvars-push-x-y-end)))

(defun dim-antideriv (form result)
  (let* ((args (rest form))
	 (a (first  args))
	 (b (second args))
	 (c (third  args)))
    (if (not (= (length args) 3))
	(dimension-function form result)
	(run-in-edrxbox-dim
	 (dim-antideriv-core a b c)))))

;; «antideriv-tests»  (to ".antideriv-tests")
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
antideriv(a,b,c);
antideriv(hdw(5,3,1), hdw(2,1,1), hdw(2,1,1));
antideriv(hdw(2,1,3), hdw(2,1,1), hdw(2,1,1));

antideriv(x^2, x=4, x=3);

|#




;;;                               _   
;;;   _____   _____ _ __ ___  ___| |_ 
;;;  / _ \ \ / / _ \ '__/ __|/ _ \ __|
;;; | (_) \ V /  __/ |  \__ \  __/ |_ 
;;;  \___/ \_/ \___|_|  |___/\___|\__|
;;;                                   
;; «overset»  (to ".overset")

(setf (get '$overset 'dimension)
      'dim-overset)

(defun dim-overset-core (a b)
  (let* ((a-box      (edrxbox-from-form a))
         (b-box      (edrxbox-from-form b))
         (a-width    (getf a-box :width))
         (b-width    (getf b-box :width))
         (a-depth    (getf a-box :depth))
         (b-height   (getf b-box :height))
         (max-width  (max a-width b-width))
         (a-y        (+ a-depth b-height)))
    (edrxboxvars-push-x-y-edrxbox 0 a-y (edrxbox-hcenter max-width a-box))
    (edrxboxvars-push-x-y-edrxbox 0 0   (edrxbox-hcenter max-width b-box))
    (edrxboxvars-push-x-y-end)))

(defun dim-overset (form result)
  (let* ((args (rest form))
	 (a (first  args))
	 (b (second args)))
    (run-in-edrxbox-dim
     (dim-overset-core a b))))

(setf (get '$eqn 'dimension) 'dim-eqn)

(defun dim-eqn (form result)
  (let* ((args (rest form))
	 (n (first args)))
    (run-in-edrxbox-dim
     (dim-overset-core (format nil "(~a)" n) "="))))

;; «overset-tests»  (to ".overset-tests")
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
o : overset("(42)", "=");
o : eqn(42);
[box(o)];

|#



;;;  _   _           _           _                        
;;; | | | |_ __   __| | ___ _ __| |__  _ __ __ _  ___ ___ 
;;; | | | | '_ \ / _` |/ _ \ '__| '_ \| '__/ _` |/ __/ _ \
;;; | |_| | | | | (_| |  __/ |  | |_) | | | (_| | (_|  __/
;;;  \___/|_| |_|\__,_|\___|_|  |_.__/|_|  \__,_|\___\___|
;;;                                                       
;; «underbrace»  (to ".underbrace")
;; See: (find-baf "edrxbox-examples.lisp" "underbrace")
;;      (find-baf "bad-foundations.lisp" "tex-underbrace")
;;      (find-baf "bad-foundations.mac" "_ssu_")
;;      https://anggtwu.net/maxima-edrxbox.html#underbrace

(setf (get '$underbrace 'dimension)
      'dim-$underbrace)

(defun dim-underbrace-core (a b)
  (let* ((a-box          (edrxbox-from-form a))
	 (b-box          (edrxbox-from-form b))
	 (max-width      (max (getf a-box :width) (getf b-box :width)))
	 (a-box-centered (edrxbox-hcenter max-width a-box))
	 (b-box-centered (edrxbox-hcenter max-width b-box))
	 (bar-box        (list :height 1 :depth 0 :width max-width
			       :result `((d-hbar ,max-width #\:))))
	 (bar-y          (+ (- (getf a-box :depth)) -1))
	 (b-y            (- bar-y (getf b-box :height))))
    (edrxboxvars-push-x-y-edrxbox 0 0 a-box-centered)
    (edrxboxvars-push-x-y-edrxbox 0 bar-y bar-box)
    (edrxboxvars-push-x-y-edrxbox 0 b-y b-box-centered)
    (edrxboxvars-push-x-y-end)))

(defun dim-$underbrace (form result)
  (let* ((args (rest form))
	 (a (first args))		; above the \underbrace
	 (b (second args)))		; below the \underbrace
    (if (not (eq (length args) 2))
	(dimension-function form result)
	(run-in-edrxbox-dim
	 (dim-underbrace-core a b)))))

;; «underbrace-tests»  (to ".underbrace-tests")
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
o : underbrace(bc^de, f[ghijk]);
o : underbrace(bc^de12345, f[ghijk]);
[box(o)];

|#






;;;      _ _                          _           _               
;;;   __| (_)_ __ ___         ___  __| |_ ____  _| |__   _____  __
;;;  / _` | | '_ ` _ \ _____ / _ \/ _` | '__\ \/ / '_ \ / _ \ \/ /
;;; | (_| | | | | | | |_____|  __/ (_| | |   >  <| |_) | (_) >  < 
;;;  \__,_|_|_| |_| |_|      \___|\__,_|_|  /_/\_\_.__/ \___/_/\_\
;;;                                                               
;; «dim-edrxbox»  (to ".dim-edrxbox")
;; This is a trick to convert edrxboxes to objects that can be
;; manipulated, and displayed, from Maxima; I learned the core
;; idea by asking questions about "taylor" in the mailing list.
;; If you run this,
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
ta : taylor (exp(x), x, 0, 3);
to_lisp();
  #$ta$
  (car #$ta$)
  (loop for flag in (car #$ta$)
        do (format t "~S~%" flag))
  (to-maxima)

|#
;; ...you will see that several of the flags in "ta" are lists:
;;
;;   MRAT
;;   SIMP
;;   (((MEXPT SIMP) $%E $X) $X)
;;   (#:G494 #:G495)
;;   (($X ((3 . 1)) 0 NIL #:G495 . 2))
;;   TRUNC
;;
;; Maxima ignores the flags that it doesn't understand, and only
;; the functions related to "taylor" and "mrat" pay attention to
;; the "list flags" above. In the diagram below
;;
;;                               (:HEIGHT 1 :DEPTH 0 :WIDTH 2 :RESULT (#\b #\a))
;;   -> (($EDRXBOX SIMP (EDRXBOX (:HEIGHT 1 :DEPTH 0 :WIDTH 2 :RESULT (#\b #\a)))))
;;   ->                          (:HEIGHT 1 :DEPTH 0 :WIDTH 2 :RESULT (#\b #\a))
;;
;; ...the first arrow shows what `edrxbox-to-$edrxbox' does;
;; the second arrow shows what `edrxbox-from-$edrxbox' does.
;; Run the tests below to understand more.

(setf (get '$edrxbox 'dimension)
      'dim-edrxbox)

(defun edrxbox-to-$edrxbox (edrxbox)
  `(($edrxbox simp (edrxbox ,edrxbox))))

(defun edrxbox-from-$edrxbox (form)
  (let* ((flags (cdar form)))
    (loop for flag in flags
	  if (and (consp flag) (eq (car flag) 'edrxbox))
	  return (cadr flag))))

(defun dim-edrxbox (form result)
  (let* ((edrxbox (edrxbox-from-$edrxbox form)))
    (if (not edrxbox)
	(dimension-function form result)
	(run-in-edrxbox-dim 
	 (edrxboxvars-from-edrxbox edrxbox)))))

(defun edrxbox-display (edrxbox)
  "Display `edrxbox' from Lisp."
  (let* ((form (edrxbox-to-$edrxbox edrxbox))
	 (formboxed `((MLIST SIMP) ((MBOX SIMP) ,form)))) ; [box(form)]
    (maxima-display formboxed)))

;; «dim-edrxbox-tests»  (to ".dim-edrxbox-tests")
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
edrxbox(42);
to_lisp();
  (edrxbox-to-$edrxbox (edrxbox-from-string "ab"))

  (defun foo (&optional add-end)
    (edrxboxvars-push-x-y-string 0 0 "abcd")
    (edrxboxvars-push-x-y-string 0 -1 "de")
    (edrxboxvars-push-x-y-string 0 -2 "f")
    (if add-end
        (edrxboxvars-push-x-y-end)))

  (defvar b-ok)
  (defvar b-bug)
  (setq b-ok  (run-in-edrxbox (foo 'add-end)))
  (setq b-bug (run-in-edrxbox (foo)))
  (edrxbox-display b-ok)
  (edrxbox-display b-bug)

  (defvar $o_ok)
  (defvar $o_bug)
  (setq $o_ok  (edrxbox-to-$edrxbox b-ok))
  (setq $o_bug (edrxbox-to-$edrxbox b-bug))
  (edrxbox-from-form $o_ok)
  (edrxbox-from-form $o_bug)
  (to-maxima)

[o_ok, box(o_ok), box(o_bug), o_bug];

|#



;;;          _ 
;;;       __| |
;;;      / _` |
;;;     | (_| |
;;;  ____\__,_|
;;; |_____|    
;;
;; «_d»  (to "._d")
;; `_d' is a `del' that is prettier and lazier -
;; it has no simplification rules.

(setf (get '$_d 'dimension)
      'dim-_d)

(defun dim-_d-core (var)
  (edrxboxvars-push-x-y-string  0 0 "d")
  (edrxboxvars-push-x-y-edrxbox 1 0 (edrxbox-from-form var)))

(defun dim-_d (form result)
  (let* ((args (rest form))
	 (var (first args)))
    (run-in-edrxbox-dim
     (dim-_d-core var))))

;; «_d-tests»  (to "._d-tests")
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
_d(x);
_d(y)/_d(x);

|#




;;;                  _       _                       _       
;;;  _ __ ___  _   _(_)_ __ | |_ ___  __ _ _ __ __ _| |_ ___ 
;;; | '_ ` _ \| | | | | '_ \| __/ _ \/ _` | '__/ _` | __/ _ \
;;; | | | | | | |_| | | | | | ||  __/ (_| | | | (_| | ||  __/
;;; |_| |_| |_|\__, |_|_| |_|\__\___|\__, |_|  \__,_|\__\___|
;;;            |___/                 |___/                   
;;
;; «myintegrate»  (to ".myintegrate")
;; `myintegrate' is a "lazy `integrate'", in this sense,
;;
;;   (find-bafpage 12 "lazy operations")
;;   (find-baftext 12 "lazy operations")
;;
;; but `integrate' can only be called with 2 or 4 arguments,
;; in these ways:
;;
;;   integrate(f(u), u);
;;   integrate(f(u), u, u=a, u=b);
;;
;; and `myintegrate' also supports being called with 1 and 3
;; arguments. In these new cases it supposes that the first argument
;; is to be interpreted as f(u)*du, like this,
;;
;;   integrate(f(u)*du);
;;   integrate(f(u)*du,  u=a, u=b);
;;
;; and this is useful for when we want to indicate a change of
;; variables with underbraces, and draw something like this:
;;
;;    x = b
;;   ⌠
;;   ⎮
;;   ⎮      f(g(x)) g'(x) dx
;;   ⎮        ::::  :::::
;;   ⌡         u     du
;;    x = a :::::::  ──
;;           f(u)    dx
;;                  ::::::::
;;                     du
;;
;; See: (find-baf "bad-foundations.lisp" "tex-myintegrate")
;;      (find-baf "edrx-myintegrates.mac")

(setf (get '$myintegrate 'dimension)
      'dim-$myintegrate)

(defun dim-myintegrate-core (fu &optional u a b has-u has-ab)
  (let* ((ints-box (list :height 3 :depth 2 :width 1 :result '((d-integralsign))))
	 (fu-box   (edrxbox-from-form fu))
	 (d-box    (if has-u  (edrxbox-from-string "d")))
	 (u-box    (if has-u  (edrxbox-from-form u)))
	 (a-box    (if has-ab (edrxbox-from-form a)))
	 (b-box    (if has-ab (edrxbox-from-form b)))
	 (a-y      (if has-ab -3))
	 (b-y      (if has-ab  3))
	 (a-width  (if has-ab (getf a-box :width) 0))
	 (b-width  (if has-ab (getf b-box :width) 0))
	 (ab-width (if has-ab (max a-width b-width) 0))
	 (fu-x     (if has-ab (+ 2 ab-width) 2))
	 (d-x      (if has-u (+ fu-x (getf fu-box :width) 1)))
	 (u-x      (if has-u (+ d-x 1))))
    (edrxboxvars-push-x-y-edrxbox 0    0 ints-box)
    (when has-ab
      (edrxboxvars-push-x-y-edrxbox 1 a-y a-box)
      (edrxboxvars-push-x-y-edrxbox 1 b-y b-box))
    (edrxboxvars-push-x-y-edrxbox fu-x 0 fu-box)
    (when has-u
      (edrxboxvars-push-x-y-edrxbox d-x  0 d-box)
      (edrxboxvars-push-x-y-edrxbox u-x  0 u-box))
    (edrxboxvars-push-x-y-end)))

(defun dim-$myintegrate (form result)
  (let* ((args  (rest   form))
	 (nargs (length args))
	 (fu    (first  args))
	 (u     (second args))
	 (a     (third  args))
	 (b     (fourth args))
	 (alt-a (second args))
	 (alt-b (third args)))
    (cond ((= nargs 1) (run-in-edrxbox-dim (dim-myintegrate-core fu)))
	  ((= nargs 2) (run-in-edrxbox-dim (dim-myintegrate-core fu u nil nil :has-u)))
	  ((= nargs 4) (run-in-edrxbox-dim (dim-myintegrate-core fu u a b :has-u :has-ab)))
	  ((= nargs 3) (run-in-edrxbox-dim (dim-myintegrate-core fu nil alt-a alt-b nil :has-ab)))
	  (t (dimension-function form result)))))

;; «myintegrate-tests»  (to ".myintegrate-tests")
#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrxbox.lisp");
load("edrxbox-examples.lisp");
infix("*.");
d*.x;

myintegrate(fu, u);
myintegrate(fu, u, u=a, u=b);
myintegrate(fu,    u=a, u=b);
myintegrate(fu);

o1 : underbrace(g(x), u);
o2 : underbrace(f(o1), f(u));
o3 : underbrace("g'"(x), du/dx);
o4 : underbrace(o3 *. dx, du);
o5 : o2 *. o4;
myintegrate(o5);
myintegrate(o5, x=a, x=b);

|#




;; Local Variables:
;; coding:  utf-8-unix
;; End:

