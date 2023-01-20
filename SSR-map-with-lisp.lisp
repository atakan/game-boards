
;(defun circle (&key (c '(0 0)) (r 1.0)
;		 (stroke "black" stroke-supplied-p) (fill "none" fill-supplied-p);
;		 (s-w 1 s-w-supplied-p))
;  (progn
;    (format t "<circle cx=\"~a\" cy=\"~a\" r=\"~a\"~%" (first c) (second c) r)
;    (format t "  style=\"")
;    (format t " stroke: ~a;" stroke)
;    (format t " fill: ~a" fill)
;    (format t "\"/>~%")))

;(defun circle2 (&key (c '(0 0)) (r 1.0)
;		 (stroke "black" stroke-supplied-p) (fill "none" fill-supplied-p)
;		 (s-w 1 s-w-supplied-p))
;  (progn
;    (format t "<circle cx=\"~a\" cy=\"~a\" r=\"~a\"~%" (first c) (second c) r)
;    (format t "  style=\"")
;    (format t "~:[~; stroke: ~a;~]" stroke-supplied-p stroke)
;    (format t "~:[~; fill: ~a;~]" fill-supplied-p fill)
;    (format t "~:[~; stroke-width: ~a;~]" s-w-supplied-p s-w)
;    (format t "\"/>~%")))

;(defun inner-circles ()
;  (let ((gc '(1600 1600))) ; gc: game-center?
;    (circle :c gc :r 715 :fill *far-c*)
;    (circle :c gc :r 620 :fill *middle-c*)
;    (circle :c gc :r 525 :fill *near-c*) 
;    (circle :c gc :r 430 :fill *central-c*)))


;; (loop for x from 0 below 4800 by 198 collect x)


;;;; define macros and functions
(defun svg-boilerplate ()
  (format t "<?xml version=\"1.0\"?>~%")
  (format t  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"~%")
  (format t  "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"))

(defmacro with (tag &rest body)
  "This is from Graham's book (p. 260)"
  `(progn
     (format t "~&<~(~A~)>~%" ',tag)
     ,@body
     (format t "~&</~(~A~)>~%" ',tag)))

(defmacro as (tag content)
  "This is from Graham's book (p. 260)"
  `(format t "<~(~A~)>~A</~(~A~)>~%"
	   ',tag ,content ',tag))

(defmacro with-svg ((&key (w 100) (h 100) (title "Def. Title")
		       (desc "Def. Desc"))
		    &rest body)
  "An obvious adaptation from above"
  `(progn
     (format t "~&<~(svg width=\"~A\" height=\"~A\"~)>~%" ',w ',h)
     (as desc ',desc)
     (as title ',title)
     ,@body
     (format t "~&</~(svg~)>~%")))


;;; a macro called defshape that allows defining a shape drawing function conveniently.
;;; this needs to be rewritten, a lot of the confusing pieces can actually
;;; reside outside this macro.
(eval-when (:compile-toplevel)
  (defun add-supplied-p-to-arglist (styleargs)
    (loop for sa in styleargs
	  collect (append sa `(,(intern (format nil "~a-SUPPLIED-P" (first sa))))))))

(defmacro defshape2 (elname ownargs styleargs (&optional (funname elname)))
  (let ( (fullstyleargs (add-supplied-p-to-arglist styleargs)))
    `(defun ,funname (&key ,@ownargs ,@fullstyleargs)
       (progn
	 (format t ,(format nil "<~(~a~) " elname))
;;	 (format t "cx=\"~a\" " cx)
	 ,@(loop for oa in ownargs
		 collect `(format t
				  ,(format nil "~(~a~)=\"~~,4f\" " (first oa))
				  ,(first oa)))
	 (if (or ,@(mapcar 'third fullstyleargs))
	     (progn
	       (format t "~& style=\"")
;;	       (format t "~:[~; stroke: ~a;~]" stroke-supplied-p stroke)
	       ,@(loop for sa in fullstyleargs
		       collect `(format t
					,(format nil "~~:[~~; ~(~a~): ~~a;~~]" (first sa))
					,(third sa) ,(first sa)))
	       (format t " \"")))
	 (format t "/>~%")))))

(defvar *far-c* "#f8d80d")
(defvar *middle-c* "#fc9100")
(defvar *near-c* "#f70000")
(defvar *central-c* "#877903")

(defvar *outer-circles-radii*
	(loop for i from 815 to 1615 by 100 collecting i))
(defvar *largest-radius* (car (last *outer-circles-radii*)))
(defvar *height* (* 2 *largest-radius*))
(defvar *width* (* 3 *largest-radius*))

;(defvar *ssargs* '( (stroke "black") (fill "none") (stroke-width 1.0)))
;(defvar *oargs* '( (cx 0.0) (cy 0.0) (r 1.0) ))

;; we use this macro to define circle drawing function, called draw-circle.
;; the given values are defaults and will be used if no value is provided
;; during function call. It would be better to set some defaults into variables
;; and use those as actual defaults in this function. e.g.,
;; (set-stroke-width 2.0) would set a variable and these would be used here.
(defshape2 circle ( (cx 0.0) (cy 0.0) (r 1.0))
    ( (stroke "black") (fill "none") (stroke-width 1.0))
    (draw-circle))

(defshape2 line ( (x1 0.0) (y1 0.0) (x2 1.0) (y2 1.0))
    ( (stroke "black") (stroke-width 1.0))
    (draw-line))

;;(pprint (macroexpand-1 '(defshape circle ( (cx 0.0) (cy 0.0) (r 1.0))
;;( (stroke "black") (fill "none") (stroke-width 1.0))
;;			 (circle3))))

;;; draw circles
;(defun inner-circles2 ()
;  (let ((gc (list *largest-radius* *largest-radius*))
;	(colors (list *far-c* *middle-c* *near-c* *central-c*))
;	(radii '(715 620 525 430)))
;    (mapc #'(lambda (rad col)
;	      (draw-circle :cx (first gc) :cy (second gc) :r rad :fill col))
;	  radii
;	  colors)))

(defun inner-circles ()
  ;; this draws the four inner circles.
  ;; they are drawn in a particular order, so they appropriately overlap.
  ;; I should probably change that and turn these into annular regions.
  ( let ( (cx *largest-radius*) (cy *largest-radius*)
	  (colors (list *far-c* *middle-c* *near-c* *central-c*))
	  (radii (list 715 620 525 430)))
    (loop for col in colors
	  for rad in radii
	  do (draw-circle :cx cx :cy cy :r rad
			  :fill col))))

;; then outer circles
(defun outer-circles ()
  (let ( (cx 1615) (cy 1615)
	 (radii '(815 915 1015 1115 1215 1315 1415 1515 1615)

(defun outer-circles ()
  ( let ( (cx *largest-radius*) (cy *largest-radius*)
	  (radii *outer-circles-radii*))
    (loop for rad in radii
	  for sw in (list 1 1 2 1 1 2 1 1 2) 
	  do (draw-circle :cx cx :cy cy :r rad
			  :fill "none" :stroke "black" :stroke-width sw))))

  
;;; draw radial lines, form the compartments

;;; maybe do this w/ recursion? an outer function that sets certain stuff
;;; keeps calling the inner function with a varying index
(defparameter *Nrooms* 60)
(defparameter *Nlevels* 9)

(defparameter *div-angles*
  (loop for i from 0 below *Nrooms*
	collect (* (+ i 1/2) (/ (* 2 pi) *Nrooms*)))) ;this is in radians
;(defparameter *div-S* (mapcar #'sin *div-angles*))
(defparameter *div-s* (make-array (length *div-angles*) 
				  :initial-contents (mapcar #'sin *div-angles*)))

;(defparameter *div-c* (mapcar #'cos *div-angles*))
(defparameter *div-c* (make-array (length *div-angles*) 
				  :initial-contents (mapcar #'cos *div-angles*)))

;(defun radial-lines (nrooms nlevels)
;  (if (and (= nrooms 0) (= nlevels 0)) nil
;      (if (= nlevels 0)
;	  (radial-lines (- nrooms 1) *Nlevels*)
;	  (let ( (ang-s (elt *div-s* (1- nrooms)))
;		 (ang-c (elt *div-c* (1- nrooms)))
;		 (r-in (elt (cons 715 *outer-circles-radii*) nlevels))
;		 (r-out (elt (cons 715 *outer-circles-radii*) (1+ nlevels))) )
;	    (format t "nrooms:~a nlevels:~a ~%" nrooms nlevels)
;;	    (draw-line :x1 (* r-in ang-c) :y1 (* r-in ang-s)
;;		       :x2 (* r-out ang-c) :y2 (* r-out ang-s))
;	    (radial-lines nrooms (1- nlevels) )))))


;; rewrite the following with inputs as room-no and level-no,
;; starting from 0 (or 1) and incrementing. That way we'll go from inside to
;; outside.
(defun radial-lines (nrooms nlevels)
  (if (= nrooms 0) nil
      (if (= nlevels 0)
	  (radial-lines (- nrooms 1) *Nlevels*)
	  (let ( (ang-s (elt *div-s* (1- nrooms)))
		 (ang-c (elt *div-c* (1- nrooms)))
		 (r-in (elt (cons 715 *outer-circles-radii*) (1- nlevels)))
		 (r-out (elt (cons 715 *outer-circles-radii*) nlevels)) ) 
;	    (draw-line :x1 (* r-in ang-c) :y1 (* r-in ang-s)
;		       :x2 (* r-out ang-c) :y2 (* r-out ang-s)
;		       :stroke "black" :stroke-width 4)
	    (draw-line :x1 (+ (* r-in ang-c) *largest-radius*)
		       :y1 (+ (* r-in ang-s) *largest-radius*)
		       :x2 (+ (* r-out ang-c) *largest-radius*) 
		       :y2 (+ (* r-out ang-s) *largest-radius*)
		       :stroke "black" :stroke-width 4)
	    (radial-lines nrooms (1- nlevels) )))))
  

;;; color the compartments

(defun make-RSSR-map ()
  (with-open-file (stream "deneme2.svg" :direction :output
					:if-exists :supersede)
    (let ((*standard-output* stream))
      (svg-boilerplate)
      (with-svg (:w 4800 :h 3200 :title "Circles")
	(inner-circles)
	(outer-circles)
	(radial-lines *Nrooms* *Nlevels*) ))))
