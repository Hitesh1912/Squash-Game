;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Racket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt") 
(require 2htdp/image)
(require 2htdp/universe)

(provide 
 MOUSE-COLOR
 MOUSE-RADIUS
 MOUSE-RACKET-DISTANCE
 RACKET-COLOR
 RACKET-WIDTH
  RACKET-HEIGHT
 HALF-RACKET-WIDTH
 INITIAL-RACKET-X
 INITIAL-RACKET-VX
 INITIAL-RACKET-Y
 INITIAL-RACKET-VY
 make-racket
 racket?
 racket-x
 racket-y
 racket-vx
 racket-vy
 racket-mx
 racket-my
 racket-selected?
 racket-after-mouse-event
 racket-after-button-down
 racket-after-drag
 racket-after-button-up
 in-racket?
 racket-collide-frontwall?)

;;; CONSTANTS

(define MOUSE-COLOR "blue")
(define MOUSE-RADIUS 3)
;;; Mouse is rendered as a circle drawn at current mouse co-ordinates,
;;; iff Racket is selected.
;;; color of circle:  MOUSE-COLOR
;;; radius of circle: MOUSE-RADIUS

(define MOUSE-RACKET-DISTANCE 20)
;;; The maximum distance of mouse from rectangle, to grab and select
;;; the rectangle.

(define RACKET-COLOR "brown")
(define RACKET-WIDTH 80)
(define HALF-RACKET-WIDTH (/ RACKET-WIDTH 2))
(define RACKET-HEIGHT 15)
;;; Racket is rendered as a rectangle of color: RACKET-COLOR
;;; with width: RACKET-WIDTH and height: RACKET-HEIGHT.

(define INITIAL-RACKET-X 330)
(define INITIAL-RACKET-Y 384)
;;; starting position of the racket at the start of the game,
;;; that is in ready-to-serve state
  
(define INITIAL-RACKET-VX 0)
(define INITIAL-RACKET-VY 0)
;;; initial velocity of racket in ready-to-serve state 

;;; Racket
;;; It is rendered as a rectangle in the simulation. 
;;;
;;; REPRESENTATION:
;;;
;;; A Racket is represented as a struct (make-racket x y vx vy mouse selected?)
;;; with the following fields:
;;; x  : Int  x-coordinate of the center of the racket, in pixels, 
;;;            relative to the origin of the scene, assuming the racket
;;;            to be a straight line of a fixed length
;;; y  : Int   y-coordinate of the center of the racket, in pixels,
;;;            relative to the origin of the scene
;;; vx : Int   x component of the racket's velocity, in pixels/tick
;;; vy : Int   y component of the racket's velocity, in pixels/tick
;;; mx,my:Int  describes the relative coordinates of mouse on grabbing and 
;;;                selecting the racket for smooth dragging
;;; selected? : Boolean   true iff the racket is selected

;;; IMPLEMENTATION:
(define-struct racket (x y vx vy mx my selected?))

;;; CONSTRUCTOR TEMPLATE
;;; (make-racket Integer Integer Integer Integer Mouse Boolean)

;;; OBSERVER TEMPLATE
;;; racket-fn : Racket -> ??
(define (racket-fn r)
  (... (racket-x r)
       (racket-y r)
       (racket-vx r)
       (racket-vy r)
       (racket-mx r)
       (racket-my r)
       (racket-selected? r)))


;;; IMPLEMENTATION:
(define-struct mouse (x y))

;;; CONSTRUCTOR TEMPLATE
;;; (make-mouse Integer Integer)

;;; OBSERVER TEMPLATE
;;; mouse-fn : Mouse -> ??
(define (mouse-fn m)
  (... (mouse-x m)
       (mouse-y m)))



;;;Functions 

;;; racket-after-mouse-event : Racket Int Int MouseEvent -> Racket
;; GIVEN: a racket, the x and y coordinates of a mouse event,
;;     and the mouse event
;; RETURNS: the racket as it should be after the given mouse event
;; EXAMPLES:
;; (racket-after-mouse-event
;;                        (make-racket 330 384 0 0 0 0 #false)
;;                           320
;;                            390
;;                            "button-down")
;;                -->   (make-racket 330 384 0 0 320 390 #t)
;; strategy: Cases on mouse event mev
(define (racket-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev "button-down") (racket-after-button-down r mx my)]
    [(mouse=? mev "drag") (racket-after-drag r mx my)]
    [(mouse=? mev "button-up") (racket-after-button-up r mx my)]
    [else r]))



;; racket-after-button-down :Racket  Integer Integer -> Racket
;; GIVEN: racket r , mouse x pos , mouse y pos
;; RETURNS: the racket following a button-down at the given location
;; EXAMPLES:
;;(racket-after-button-down
;;    (make-racket 330 384 0 0 0 0 #false) 320 390)
;; ->   (make-racket 330 384 0 0 320 390 #t)

;; STRATEGY: divide into cases on r,x and y 

(define (racket-after-button-down r x y)
  (if (in-racket? r x y)
      (make-racket (racket-x r) (racket-y r)
                   (racket-vx r) (racket-vy r)
                   x y
                   #true)r))

;; racket-after-drag : Racket Integer Integer -> Racket
;; GIVEN: racket r , mouse x pos , mouse y pos
;; RETURNS: the racket following a drag at the given location
;; EXAMPLES:
;; (racket-after-drag
;;                           (make-racket 330 384 0 0 #true)
;;                           320
;;                           390)
;;                          -> (make-racket 320 390 0 0 320 390 #true)
;; STRATEGY: Use template for Racket on r
(define (racket-after-drag r x y)
  (if (racket-selected? r) (make-racket
                            (- (racket-x r) (- (racket-mx r) x))
                            (- (racket-y r) (- (racket-my r) y))
                            (racket-vx r)
                            (racket-vy r)
                            x y
                            #true)
     r))


;; racket-after-button-up : Racket Integer Integer -> Racket
;; GIVEN: Racket r , mouse x pos , mouse y pos
;; RETURNS: the racket following a button-up at the given location
;; EXAMPLES :
;;racket-selected? (racket-after-button-up
;;                          (make-racket 330 384 0 0 0 0 #true)
;;                           330
;;                           380))
;;                     ->  #f
;; DESIGN STRATEGY: Use template for racket on r

(define (racket-after-button-up r x y)
  (make-racket (racket-x r) (racket-y r)
               (racket-vx r) (racket-vy r) 0 0 #false))

;; in-racket? : Racket Integer Integer -> Racket
;; GIVEN: Racket r , mouse x pos , mouse y pos
;; RETURNS true iff the given coordinate is inside the bounding region of
;; the given racket.
;; EXAMPLES:
;;(in-racket?
;;        (make-racket 330 384 0 0 0 0 #true)
;;           330
;;            384)
;;  --->  #t
;; DESIGN STRATEGY: Use observer template for Racket on r

(define (in-racket? r x y)
  (and
   (<= (- (racket-x r) MOUSE-RACKET-DISTANCE)
       x
       (+ (racket-x r) MOUSE-RACKET-DISTANCE))
   (<= 
    (- (racket-y r) MOUSE-RACKET-DISTANCE)
    y
    (+ (racket-y r) MOUSE-RACKET-DISTANCE))))



(define (racket-collide-frontwall? r)
  (or (< ( + (racket-y r) (racket-vy r)) 0) (= (racket-y r) 0)))


