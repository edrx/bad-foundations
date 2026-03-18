;; This file:
;;   https://anggtwu.net/bad-foundations/lisptree.lisp.html
;;   https://anggtwu.net/bad-foundations/lisptree.lisp
;;           (find-angg "bad-foundations/lisptree.lisp")
;;                            (find-baf "lisptree.lisp")
;;     See: https://anggtwu.net/bad-foundations.html  <- has screenshots!
;;  Author: Eduardo Ochs <eduardoochs@gmail.com>
;; License: Public Domain
;; Date: 2026mar04
;;
;; This is the part of the code of Lisptree that is written in Lisp.
;; When LispTree does this
;;
;;   (%i1) lisptree(f(1,2));
;;                            f__.
;;   (%o1)                    |  |
;;                            1  2
;;
;; it is actually doing this series of conversions:
;;
;;        f(1,2)                                         <- a Maxima object
;;   ==>  ["f", "1", "2"]                                <- a Maxima tree
;;   ==>  ("f"  "1"  "2")                                <- a Lisp tree
;;   ==>                 ("f__."    "|  |"    "1  2")    <- a rect object
;;   ==>          matrix(["f__."], ["|  |"], ["1  2"])   <- a matrix
;;   ==>  verbatimmatrix(["f__."], ["|  |"], ["1  2"])   <- a verbatimmatrix
;;
;; This file implements the functions that do the steps in the middle -
;; these ones, that go from a Maxima tree to a (Maxima) matrix:
;;
;;        ["f", "1", "2"]                                <- a Maxima tree
;;   ==>  ("f"  "1"  "2")                                <- a Lisp tree
;;   ==>                 ("f__."    "|  |"    "1  2")    <- a rect object
;;   ==>          matrix(["f__."], ["|  |"], ["1  2"])   <- a matrix
;;
;; The function `$maximatree_to_matrix', defined at the end of this
;; file, does those three conversions at once. Here's an example of
;; what it does:
;;
;;   (%i2) maximatree_to_matrix(["f","1",["g","2","3"]]);
;;                              ┌         ┐
;;                              │ f__.    │
;;                              │         │
;;                              │ |  |    │
;;                              │         │
;;   (%o2)                      │ 1  g__. │
;;                              │         │
;;                              │    |  | │
;;                              │         │
;;                              │    2  3 │
;;                              └         ┘
;;
;; Index:
;; «.concat»			(to "concat")
;; «.concat-tests»		(to "concat-tests")
;; «.rect»			(to "rect")
;; «.rect-tests»		(to "rect-tests")
;; «.rect-fromtree»		(to "rect-fromtree")
;; «.rect-fromtree-tests»	(to "rect-fromtree-tests")
;; «.convert»			(to "convert")
;; «.convert-tests»		(to "convert-tests")
;; «.lisptree2»			(to "lisptree2")
;; «.lisptree2-tests»		(to "lisptree2-tests")


;; «concat»  (to ".concat")
;; Inspired by the "table.concat" of Lua and the "mapconcat" of
;; Emacs Lisp, that accept a separator as an optional argument...
;; TODO: These names are bad!!! Should I rename these functions?

(defvar *nl*   (format nil "~%"))
(defvar *nlsp* (format nil "~% "))

(defun concat (&rest strs)
  (apply 'concatenate 'string strs))

(defun myconcat (strings &optional (sep ""))
  (let ((list nil))
    (if strings	(push (car strings) list))
    (loop for string in (cdr strings)
	  do (push sep    list)
	  do (push string list))
    (apply 'concatenate 'string (reverse list))))

(defun mapconcat (f list &optional (sep ""))
  (myconcat (map 'list f list) sep))

#|
 «concat-tests»  (to ".concat-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
to_lisp();
  (load "rect.lisp")
  (myconcat '("a" "bc" "def"))
  (myconcat '("a" "bc" "def") "|")
  (myconcat '("a" "bc" "def") *nl*)
  (myconcat '("a" "bc" "def") *nlsp*)
  (to-maxima)

|#



;; «rect»  (to ".rect")
;; A "rect" object is a list of strings.
;; For example, this tree
;;
;;   f__.
;;   |  |
;;   a  g__.
;;      |  |
;;      b  c
;;
;; "is" this rect:
;;
;;   ("f__."
;;    "|  |"
;;    "a  g__."
;;    "   |  |"
;;    "   b  c")
;;
;; The functions below were inspired by the
;; basic functions in this Lua class:
;;   (find-angg "LUA/lua50init.lua" "Rect")
;;   (find-angg "LUA/Rect.lua")
;;
(defun rect-pad-string (wtotal string &optional char)
  "Pad STRING to the width WTOTAL."
  (let* ((wleft  (length string))
	 (wright (- wtotal wleft))
	 (spaces (make-string wright :initial-element (or char #\ ))))
    (if (< wleft wtotal)
	(concat string spaces)	   ; add spaces at the right if needed
	string)))		   ; or return STRING unchanged

(defun rect-from (o)
  (if (listp o) o (format nil "~a" o)))

(defun rect-pr (rect)		  ; for debugging
  (myconcat rect *nlsp*))

(defun rect-tostring (rect &optional sep)
  (myconcat rect (or sep *nl*)))

(defun rect-width (rect)
  (let* ((widths (mapcar #'length rect)))
    (apply #'max (cons 0 widths))))

(defun rect-extend (recta newheight)
  (let* ((a-height (length recta))
	 (rectb (loop for y from (+ a-height 1) to newheight
		      collect "")))
    `(,@recta ,@rectb)))

(defun rect-pad (rect &optional char)
  (let* ((width (rect-width rect)))
    (loop for line in rect
	  collect (rect-pad-string width line char))))

(defun rect-concat (rectl rectr)
  (let* ((heightl   (length rectl))
	 (heightr   (length rectr))
	 (maxheight (max heightl heightr)))
    (loop for linel in (rect-pad (rect-extend rectl maxheight))
	  for liner in           (rect-extend rectr maxheight)
	  collect (concat linel liner))))

#|
 «rect-tests»  (to ".rect-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
to_lisp();
  (load "rect.lisp")
  (defparameter r1 4)
  r1
  (to-maxima)

|#



;; «rect-fromtree»  (to ".rect-fromtree")
;; These functions were inspired by:
;;   (find-angg "LUA/lua50init.lua" "SynTree")
;;   (find-angg "LUA/Rect.lua" "SynTree")
;;
(defun rect-pin (name rect)
  `(,name "|" ,@rect))

(defun rect-add_ (rect)
  (let* ((rectw  (rect-width rect))
	 (line1  (car  rect))
	 (line1_ (rect-pad-string (+ 2 rectw) line1 #\_))
	 (rest   (rest rect)))
    `(,line1_ ,@rest)))

(defun rect-concat_ (rectl rectr)
  (rect-concat (rect-add_ rectl) rectr))

(defun rect-fromtree (o)
  (if (atom o)
      (list (rect-from o))
      (let* ((head  (format nil "~a" (car o)))
	     (args  (rest o))
	     (nargs (length args)))
	(cond ((=  nargs 0) (list (rect-from head)))
	      ((=  nargs 1) (rect-pin head (rect-fromtree (first args))))
	      ((>= nargs 2)
	       (let* ((firstarg    (first args))
		      (middleargs  (rest (butlast args)))
		      (lastarg     (car (last args)))
		      ;;
		      (firsttree   (rect-pin head (rect-fromtree firstarg)))
		      (lasttree    (rect-pin "."  (rect-fromtree lastarg)))
		      (middletrees (loop for arg in middleargs
					 collect (rect-pin "." (rect-fromtree arg))))
		      ;;
		      (result      lasttree))
		 ;;
		 (loop for tree in (reverse middletrees)
		       do (setq result (rect-concat_ tree result)))
		 (setq result (rect-concat_ firsttree result))
		 result))))))

#|
 «rect-fromtree-tests»  (to ".rect-fromtree-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
to_lisp();
  (load "rect.lisp")
  (defparameter r1 (rect-fromtree '(f 1 (g 2 3))))
                                                    r1
  (rect-pr                                          r1)
  (rect-pr                         (rect-pin "Name" r1))
  (rect-pr              (rect-add_ (rect-pin "Name" r1)))
  (rect-pr (rect-concat (rect-add_ (rect-pin "Name" r1)) r1))
  (rect-pr (rect-concat (rect-add_ (rect-pin "Name" r1)) (rect-pin "." r1)))
  (rect-pr (rect-concat_           (rect-pin "Name" r1)  (rect-pin "." r1)))
  (to-maxima)

|#





;; «convert»  (to ".convert")
;; `lisptree-convert' is the high-level way to do these conversions:
;;
;;        ["f", "1", "2"]                                <- a Maxima tree
;;   ==>  ("f"  "1"  "2")                                <- a Lisp tree
;;   ==>                 ("f__."    "|  |"    "1  2")    <- a rect object
;;   ==>          matrix(["f__."], ["|  |"], ["1  2"])   <- a matrix
;;
(defun maximatree-to-lisptree (maximatree)
  "This function converts a Maxima tree `mt' to a Lisp tree.
Some examples:
    (maximatree-to-lispytree '((mlist) 2 3))
-->                                   (2 3)
    (maximatree-to-lispytree #$[1,2,[3,4,5]]$)
-->                            (1 2 (3 4 5))"
  (if (atom maximatree)
      maximatree
      (map 'list #'maximatree-to-lisptree (rest maximatree))))

(defun lisptree-convert
    (o &key
	 mt->lt		  ; if non-nil convert a Maxima tree to a Lisp tree
	 lt->rect	  ; if non-nil convert a Lisp tree to a rect
	 ((:pad char))    ; if non-nil then pad the rect with this character
	 ((:concat sep))) ; if non-nil then concat lines of the rect with this separator
  "Apply a series of conversions on the object `o'.
This function is very fragile. Only call it with arguments that make sense!"
  (if mt->lt   (setq o (maximatree-to-lisptree o)))
  (if lt->rect (setq o (rect-fromtree o)))
  (if char     (setq o (rect-pad o char)))
  (if sep      (setq o (myconcat o sep)))
  o)

(defun $maximatree_to_matrix (mt)
  (let* ((lines (lisptree-convert mt :mt->lt t :lt->rect t :pad #\ ))
	 (matrixlines (loop for line in lines
			    collect `((mlist simp) ,line))))
    `(($matrix simp) ,@matrixlines)))

#|
 «convert-tests»  (to ".convert-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
to_lisp();
  (load "rect.lisp")
           (lisptree-convert '(f 1 (g 2 3)))
           (lisptree-convert '(f 1 (g 2 3)) :lt->rect t)
  (rect-pr (lisptree-convert '(f 1 (g 2 3)) :lt->rect t))
  (rect-pr (lisptree-convert '(f 1 (g 2 3)) :lt->rect t :pad #\.))
           (lisptree-convert '(f 1 (g 2 3)) :lt->rect t :pad #\. :concat *nlsp*)
  (rect-pr (lisptree-convert #$[f,1,[g,2,3]]$ :mt->lt t :lt->rect t))
  (to-maxima)

 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("rect.lisp");
o : [f,1,[g,2,3]];
maximatree_to_matrix(o);

|#



;; «lisptree2»  (to ".lisptree2")
;; An example:
;;
;;   (%i2) lisptree2q(2+3);
;;                                   +__.
;;   (%o2)                   2 + 3 = |  |
;;                                   2  3
;;   
;; The "2+3" at the left of the `=' is not simplified!
;; The easiest way to implement that is by using a macro
;; in Lisp and a `(mequal simp)' - see `$lisptree2' below.
;;
;; Uses: (find-baf "lisptree.mac" "lisptree")
;;       (find-baf "lisptree.mac" "lisptree" "lisptreeqm")
;;
(defun $lisptree2 (o)
  `((mequal simp) ,o ,(mfuncall '$lisptreeqm o)))

(defmacro $lisptree2q (o) `',($lisptree2 o))
(defmacro $lisptreeq2 (o) `',($lisptree2 o))

#|
 «lisptree2-tests»  (to ".lisptree2-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("lisptree.mac");
lisptree2 (a+b);
lisptree2q(a+b);
lisptree2q(2+3);

|#
