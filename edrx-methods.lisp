;; This file:
;;   https://anggtwu.net/bad-foundations/edrx-methods.lisp.html
;;   https://anggtwu.net/bad-foundations/edrx-methods.lisp
;;           (find-angg "bad-foundations/edrx-methods.lisp")
;;                            (find-baf "edrx-methods.mac")
;;  Author: Eduardo Ochs <eduardoochs@gmail.com>
;;     See: https://anggtwu.net/eev-maxima.html#methods
;; Version: 2026mar01
;; License: GPL v2
;;
;; This file implements OO in Maxima -
;; it makes "structures" behave as "classes" with "methods".
;; Very roughly, we have this:
;;
;;   defstruct(ab(a,b));              <-- defines the structure/class ab
;;   ab__f(o,x,y) := [o@a,o@b,x,y];   <-- defines a method f of the class ab
;;   o   : new(ab(2,3));              <-- creates an object o of the class ab
;;   o@@f(4,5);                       <-- calls the method f on the object o
;;
;; This was inspired by Lua.
;; In Lua ":" is a syntax sugar, and is translated in this way:
;;
;;        o:f   (4, 5)
;;   ==>  o.f(o, 4, 5)
;;
;; In Maxima the "@@" - defined below - is translated in this way:
;;
;;         defstruct(ab(a, b));
;;         o :   new(ab(2, 3));
;;         o@@f   (4, 5);
;;   ==>  ab__f(o, 4, 5);
;;
;; The methods for a structure "ab" are stored in functions with names
;; starting with "ab__".
;;
;; These two implementations are equivalent:
;;
;;   (find-baf "edrx-methods.mac")
;;   (find-baf "edrx-methods.lisp")
;;
;; «.core»		(to "core")
;; «.core-tests»	(to "core-tests")

(defun $@@3 (ab__f o args)
  ($apply ab__f `((mlist) ,o ,@(rest args))))

(defun $@@ (o f)
  (let* ((ab    ($op o))
         (ab__f ($concat ab "__" f)))
    `((lambda simp)
      ((mlist) ((mlist) $Args))
      (($@@3) ,ab__f ,o $Args))))	 

($infix "@@" 200 201)


#|
 «core-tests»  (to ".core-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("edrx-methods.mac");
load("edrx-methods.lisp");

defstruct(ab(a,b));
o   : new(ab(2,3));              " -->       ab(a=2, b=3)        "$
o@@f(4,5);                       " --> ab__f(ab(a=2, b=3), 4, 5) "$
ab__f(o,x,y) := [o@a,o@b,x,y];
o@@f(4,5);                       " -->              [2, 3, 4, 5] "$
o@@f;     " --> lambda([[Args]], @@3(ab__f, ab(a=2, b=3), Args)) "$

|#









