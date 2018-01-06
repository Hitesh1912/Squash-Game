;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname World) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; WORLD - DATA DEFINITIONS


(require rackunit) 
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS 

(define FPS 25) ;;; frames per second

;;; total balls missed, after which game will be over
(define TOTAL-MISS 10)

;;; Dimensions of the playing window
(define WIDTH 580)   ;;; Window Width
(define HEIGHT 685)  ;;; Window Height

;;; scenes 
(define EMPTY-COURT (empty-scene WIDTH HEIGHT "white"))
;;; (define EMPTY-COURT (bitmap "court.jpg"))

(define RESETTING-COURT (empty-scene WIDTH HEIGHT "yellow"))
;;; Court turns from white to yellow for 3 seconds while resetting in which
;;; rally state resets to ready-to-serve state.

(define READY-COURT (empty-scene WIDTH HEIGHT "RoyalBlue"))

RESETTING-COURT

(define SPACE " ")
(define RESTART "r")
(define FREEZE "f")

(define BALL-COLOR "black")
(define BALL-RADIUS 5.5)
;;; Ball is rendered as a circle of color: BALL-COLOR
;;; and radius: BALL-RADIUS

;;; dimensions of the ball
;;;(define BALL-IMAGE (bitmap "ball.png"))
(define BALL-IMAGE (circle BALL-RADIUS "solid" BALL-COLOR))

;; starting state of ball and racket
(define START-X (/ WIDTH 2))
(define START-Y (/ HEIGHT 2))
(define START-VX 0)
(define START-VY 0)

;; ball in rally state
(define RALLY-VX (+ (* 2 (random 14)) -18))
(define RALLY-VY (+ -12 (random 4)))


(define RACKET-COLOR "green")
;;; Racket is rendered as a rectangle of color: RACKET-COLOR
;;; with width: RACKET-WIDTH and height: RACKET-HEIGHT.
  
;; dimensions of the racket
(define RW 75)  ;;; Window Width
(define RH 12)  ;;; Window Height
(define RACKET-IMAGE (rectangle RW RH "solid" RACKET-COLOR))
;; racket half length
(define HALF-LENGTH (/ RW 2))
(define HALF-HEIGHT (/ RH 2))

(define MOUSE-RACKET-DISTANCE 24)
;;; The maximum distance of mouse from rectangle, to grab and select
;;; the rectangle.

(define INITIAL-RACKET-X (/ WIDTH 2))
(define INITIAL-RACKET-Y (* HEIGHT 0.8))
;;; starting position of the racket at the start of the game,
;;; that is in ready-to-serve state
  
(define INITIAL-RACKET-VX 0)
(define INITIAL-RACKET-VY 0)
;;; initial velocity of racket in ready-to-serve state 

(define MOUSE-COLOR "blue")
(define MOUSE-RADIUS 4)
;;; Mouse is rendered as a circle drawn at current mouse co-ordinates,
;;; iff Racket is selected.
;;; color of circle:  MOUSE-COLOR
;;; radius of circle: MOUSE-RADIUS
(define MOUSE-POINTER (circle MOUSE-RADIUS "solid" MOUSE-COLOR))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; World
;;; It is rendered as a scene in the simulation. 

;;; REPRESENTATION:
;;;
;;; A World is represented as a struct
;;; (make-world balls racket state time playtime-ticker resetting-ticker score)
;;; with the following fields:
;;;
;;; balls   : BallList   describes the list of balls in the world 
;;; racket  : Racket     describes the position and veloctiy of the racket
;;; state   : State       represent the state 
;;; time    : NonNegInt  is the timer of the game - units? fps....
;;; miss    : NonNegInt   : Score as per scoring criterion

;;; EXPLAINATION:
;;;
;;; The reset process lasts for 3 seconds of real time.
;;;
;;; Starting the reset process starts the tick-counter(intially at 0).
;;; The tick-counter counts upto a total number of ticks equivalent to
;;; 3 seconds of real time.
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA DEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Ball
;;; It is rendered as a circle in the simulation. 
;;;
;;; REPRESENTATION:
;;;
;;; A Ball is represented as a struct (make-ball x y vx vy)
;;; with the following fields:
;;; x :  Int    x-coordinate of the center of the ball, in pixels,
;;;                relative to the origin of the scene
;;; y :  Int    y-coordinate of the center of the ball, in pixels,
;;;                relative to the origin of the scene
;;; vx : Int    x component of the ball's velocity, in pixels/tick
;;; vy : Int    y component of the ball's velocity, in pixels/tick
;;;
;;; EXPLAINATION:
;;;
;;; We use graphics coordinates, where top left corner denotes the origin,
;;; x increases towards right, and y increases as we go down.
;;; A positive value for vy means the ball or racket is moving downward.

;;; IMPLEMENTATION OF BALL
(define-struct ball (x y vx vy))

;;; CONSTRUCTOR TEMPLATE
;;; (make-ball Integer Integer Integer Integer NonNegInt)

;;; OBSERVER TEMPLATE
;;; ball-fn : Ball -> ??
(define (ball-fn b)
  (... (ball-x b)
       (ball-y b)
       (ball-vx b)
       (ball-vy b)))

;;; A BallList is represented as a list of Ball
;;;
;;; CONSTRUCTOR TEMPLATES:
;;; empty
;;; (cons b bl)
;;; -- WHERE
;;;    b  is a Ball
;;;    bl is a BallList
;;;
;;; OBSERVER TEMPLATE:
;;; ball-list-fn : BallList -> ??
;;;(define (ball-list-fn list)
;;;  (cond
;;;    [(empty? list) ...]
;;;    [else (...
;;;            (first list)
;;;	    (ball-list-fn (rest list)))]))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BALL FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ball-collide-wall? b)
  ( or (ball-collide-topwall? b) (ball-collide-leftwall? b)
       (ball-collide-rightwall? b)))


(define (ball-collide-topwall? b)
  (< ( + (ball-y b) (ball-vy b)) 0))

(define (ball-collide-leftwall? b)
  (< (+ (ball-x b) (ball-vx b)) 0))

(define (ball-collide-rightwall? b)
  (< WIDTH (+ (ball-x b) (ball-vx b))))


(define (any-ball-collide-racket? bl r)
  (ormap
   ;Ball -> Boolean
   ;GIVEN: a ball
   ;RETURNS: true iff given ball collides racket
   (lambda (b) (ball-collide-racket? b r))
   bl))

(define (after-ball-collide-wall b)
  (cond
    [(ball-collide-topwall? b) (after-ball-collide-topwall b)]
    [(ball-collide-leftwall? b) (after-ball-collide-leftwall b)]
    [(ball-collide-rightwall? b) (after-ball-collide-rightwall b)]
    ))


(define (ball-collide-racket? b r)
  ( collision-path?  (ball-x b)
                     (ball-y b)
                     (ball-vx b)
                     (ball-vy b)
                     (+ (racket-x r) (racket-vx r))
                     (+ (racket-y r) (racket-vy r))))


(define (collision-path? bx by bvx bvy rx ry )
  ( and (x-range? bx (- rx HALF-LENGTH) (+ rx HALF-LENGTH))
        (x-range? (+ bx bvx) (- rx HALF-LENGTH) (+ rx HALF-LENGTH))
        (< by ry)
        (<= ry (+ by bvy))))


(define (x-range? bx1 rx1 rx2 )
  ( and (< rx1 bx1 ) (< bx1 rx2)))


(define (ball-after-colliding-racket b r)
  (make-ball (ball-x b)
             (ball-y b)
             (ball-vx b)
             (- (racket-vy r) (ball-vy b))))


(define (after-ball-collide-topwall b)
  (make-ball (+ (ball-x b) (ball-vx b))
             (- (+ (ball-y b) (ball-vy b)))
             (ball-vx b)
             (- (ball-vy b)) ))


(define (after-ball-collide-leftwall b)
  (make-ball (-(+ (ball-x b) (ball-vx b)))
             (+ (ball-y b) (ball-vy b))
             (- (ball-vx b)) (ball-vy b)))

(define (after-ball-collide-rightwall b)
  (make-ball (- WIDTH (-(+ (ball-x b) (ball-vx b)) WIDTH))
             (+ (ball-y b) (ball-vy b))
             (-(ball-vx b))
             (ball-vy b) ))

(define (ball-next-move b)
  (make-ball
   (+ (ball-x b) (ball-vx b))
   (+ (ball-y b) (ball-vy b))
   (ball-vx b)
   (ball-vy b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RACKET FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (and
    (>= y (- (- (racket-y r) HALF-HEIGHT) 5))
    (>= (+ (+ (racket-y r) HALF-HEIGHT) 15) y))

   (and
    (>= x (- (- (racket-x r) HALF-LENGTH) 15))
    (>= (+ (+ (racket-x r) HALF-LENGTH) 15) x))))



(define (racket-collide-frontwall? r)
  (or (< ( + (racket-y r) (racket-vy r)) 0) (= (racket-y r) 0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WORLD FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; IMPLEMENTATION OF WORLD
(define-struct world
  (balls racket state time sec miss freeze))

;;; CONSTRUCTOR TEMPLATE:
;;; (make-world BallList Racket State NonNegInt NonNegReal NonNegInt NonNegInt)

;;; OBSERVER TEMPLATE:
;;; world-fn : World -> ??
#;(define (world-fn w)
    (... (world-balls w) 
         (world-racket w) 
         (world-state w)
         (world-time w)
         (world-sec w)
         (world-miss w)
         (world-freeze w)))


;; a STATE is represented by one of :

;; INTERPRETATION:

;; A State is one of
;; -- "ready"
;; -- "play"
;; -- "pause" 


;; CONSTRUCTOR TEMPLATE: Not needed.

;; OBSERVER TEMPLATE:
;; state-fn : State-> ?
#;(define (state-fn s)
    (cond
      [(string=? s "ready") ...]
      [(string=? s "play")  ...]
      [(string=? s "reset")   ...]))


;;;;;;;;;;;;;;; Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (simulation speed)
  (big-bang (initial-world (/ 1 FPS))          
            (on-tick world-after-tick (/ 1 FPS))
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;; initial-world : PosReal -> World
;;; GIVEN:   the speed of the simulation, in seconds per tick
;;;          (so larger numbers run slower)
;;; RETURNS: the ready-to-serve state of the world
;;; EXAMPLES:  (initial-world 0.5)
;;;            (initial-world 1/24)
;;; DESIGN STRATEGY: Use constructor template on World
(define (initial-world speed)
  (make-world
   (cons (make-ball START-X START-Y START-VX  START-VY) empty)
   (make-racket INITIAL-RACKET-X
                INITIAL-RACKET-Y
                INITIAL-RACKET-VX
                INITIAL-RACKET-VY
                0 0
                #false)
   "ready"                            
   0 0 0 0))


; world-after-key-event : World KeyEvent -> World
; GIVEN: a world w and a key event kev
; RETURNS: the world that should follow the given world
;     after the given key event
;;EXAMPLES :
;; (world-paused? (world-after-key-event (initial-world 1/24) " ")) -->  #f
; DESIGN STRATEGY : divide into cases on kev and w
(define (world-after-key-event w kev)
  (cond
    [(key=? kev SPACE) (world-state-after-space w)]
    [(key=? kev RESTART) (initial-world FPS)]
    ;    [(and (key=? kev B-KEY) (world-rally-state? w))
    ;     (add-balls-in-world w)]
    [(key=? kev FREEZE) (world-after-freeze-keypress w)]
    [else w]))


;;;;world-state-after-space : World -> World
;;GIVEN:  a world w 
;;RETURN: either a world in rally state if it was in ready-to-serve
;; or world in paused state if it was in paused-state
;;EXAMPLES:
;; (check-equal?
;;   ( world-paused? ((world-state-after-space
;;                      (world-in-rally-state
;;                                (initial-world 1/24))))
;;                   -> #t

;;DESIGN STRATEGY: divide into cases on world w state 
(define (world-state-after-space w)
  (cond
    [(and (string=? (world-state w) "pause") (>= (world-miss w) TOTAL-MISS))
     (initial-world FPS)]
    [(string=? (world-state w) "play") (world-in-paused-state w)]
    [(string=? (world-state w) "ready") (world-in-play-state w)]
    [(string=? (world-state w) "pause") (world-in-resume-state w)]))


(define (world-after-freeze-keypress w)
  (cond
    [(= 0 (world-freeze w)) (world-at-time-freeze w)]
    [(= 1 (world-freeze w)) (world-in-play-again-state w)]
    [else w ]))


;; world-after-mouse-event: World Int Int MouseEvent -> World
; GIVEN: a world, the x and y coordinates of a mouse event,
;     and the mouse event
; RETURNS: the world that should follow the given world after
;     the given mouse event

;; EXAMPLES:
;; (world-ready-to-serve?
;;    (world-after-mouse-event
;;       (initial-world 1/24) 330 380 "button-down"))
;;  ->#t

;;DESIGN STRATEGY: cases on world(w) state 

(define (world-after-mouse-event w mx my mev)
  (if (string=? (world-state w) "play")
      (make-world
       (world-balls w)
       (racket-after-mouse-event (world-racket w) mx my mev)
       "play"
       (world-time w)
       (world-sec w)
       (world-miss w)
       (world-freeze w))
      w))

;; world-in-paused-state : World -> World
;; GIVEN :  a world w in rally state
;; RETURNS: a world just like the given one, but with paused? toggled
;; DESIGN STRATEGY: use constructor template for World on w
;; EXAMPLES:
;; (world-paused? (world-in-paused-state (initial-world 1/24)))
;; -----> #t

(define (world-in-paused-state w)
  (make-world
   (world-balls w)
   (world-racket w)
   "pause"
   (world-time w)
   (world-sec w)
   (world-miss w)
   (world-freeze w)))


(define (world-in-resume-state w)
  (make-world
   (world-balls w)
   (world-racket w)
   "play"
   (world-time w)
   (world-sec w)
   (world-miss w)
   (world-freeze w)))


(define (world-at-game-over w)
  (make-world
   (world-balls w)
   (world-racket w)
   "pause"
   (world-time w)
   (world-sec w)
   TOTAL-MISS
   (world-freeze w)))

(define (world-in-play-state w)
  (make-world
   (ball-rally-state (world-balls w))
   (world-racket w)
   "play"
   1
   (/ 1 FPS)
   0
   0))

(define (world-in-play-again-state w)
  (make-world
   (ball-rally-state (world-balls w))
   (world-racket w)
   "play"
   (world-time w)
   (world-sec w)
   (world-miss w)
   2))

(define (world-at-time-freeze w)
  (make-world
   (ball-rally-state (world-balls w))
   (world-racket w)
   "play"
   (world-time w) (world-sec w) 0
   1))
   

(define (ball-rally-state bl)
  (map
   (lambda (b) (make-ball (ball-x b) (ball-y b) RALLY-VX RALLY-VY))
   bl))




;; world-after-tick : World -> World
;; GIVEN: a world  w
;; RETURNS: the world that should follow w after a tick.  If the world
;;   is paused, returns it unchanged.  Otherwise, builds a new world
;;   with updated ball and racket.
;; EXAMPLES:
;; (world-paused?(world-after-tick (initial-world 1/24))) -> #f

;; STRATEGY: Cases on world w, then use constructor template of World 
(define (world-after-tick w)
  (cond
    [(string=? (world-state w) "pause") w]
    [(string=? (world-state w) "ready") w]
    [(or (racket-collide-frontwall? (world-racket w))
         (= (world-miss w) TOTAL-MISS))   
     (world-at-game-over w)]
    [ else
      (world-during-play-state w)]))



(define (world-during-play-state w)
  (if (not(= 1 (world-freeze w)))
      (make-world
       (balls-after-tick (world-balls w) w)
       (racket-after-tick w)
       "play"
       (+ (world-time w) 1)
       (+ (world-sec w) 0.04)
       (update-miss w)
       (world-freeze w))

      (make-world
       (world-balls w)
       (racket-after-tick w)
       "play"
       (world-time w)
       (/ (world-time w) FPS)
       (world-miss w)
       (world-freeze w)))) 



(define (update-miss w)
  (local (; Ball->Boolean
          ;GIVEN: ball b in world
          ;RETURN: true iff given ball has not hit backwall 
          (define (ball-backwall-collide? b)
            (< HEIGHT (+ (ball-y b) (ball-vy b)))))
    (+ (world-miss w)(length (filter ball-backwall-collide? (world-balls w))))))


;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.
;;EXAMPLES:
;;(world-to-scene (initial-world 1/24)) ->
;      (scene-with-ball
;          (make-ball 330 384 0 0)
;           (scene-with-racket
;              (make-racket 330 384 0 0 0 0 #f)
;                   EMPTY-COURT))
;; STRATEGY: combine simpler functions
(define (world-to-scene w)
  (cond
    [(string=? (world-state w) "ready")
     (place-image
      (text/font "START GAME" 70 "WHITE" #f 'roman 'normal 'bold #f)
      (* 0.5 WIDTH) (* 0.35 HEIGHT)
      (place-image
       (text/font "Press 'space' to start."
                  30 "GREEN" #f 'roman 'normal 'bold #f)
       (* 0.5 WIDTH) (* 0.55 HEIGHT)
       (place-image
        (text/font "Press 'space' to pause the game."
                   30 "GREEN" #f 'roman 'normal 'bold #f)
        (* 0.5 WIDTH) (* 0.62 HEIGHT)
        (place-image
         (text/font "Press 'r' to restart."
                    30 "GREEN" #f 'roman 'normal 'bold #f)
         (* 0.5 WIDTH) (* 0.69 HEIGHT)
         (place-image
          (text/font "Press 'f' to freeze time. (can be used only once)"
                     25 "GREEN" #f 'roman 'normal 'bold #f)
          (* 0.5 WIDTH) (* 0.74 HEIGHT) 
          (place-image
           (text/font "Game ends if 10 balls hit the ground."
                      30 "white" #f 'roman 'normal 'bold #f)
           (* 0.5 WIDTH) (* 0.45 HEIGHT)
           (scene-with-balls-list
            (world-balls w)
            (scene-with-mouse
             (world-racket w)
             (scene-with-racket
              (world-racket w)
              READY-COURT)))))))))]

    
    [(string=? (world-state w) "play")
     (if (= (world-freeze w) 1)
     (place-image
      (text/font "TIME FREEZE - Racket can be moved."
                 30 "Forestgreen" #f 'roman 'normal 'bold #f)
      (* 0.5 WIDTH) (* 0.30 HEIGHT)
      (place-image
       (text/font
         (number->string (world-time w)) 14 "RED" #f 'modern 'normal 'normal #f)
       (+ 50 (* 0.82 WIDTH)) (* 0.04 HEIGHT)
       (place-image
        (text/font "Time:" 14 "RED" #f 'modern 'normal 'normal #f)
        (* 0.82 WIDTH) (* 0.04 HEIGHT)
        (place-image
         (text/font 
          (number->string (world-miss w))
          14 "RED" #f 'modern 'normal 'normal #f)
         (+ 72 (* 0.78 WIDTH)) (* 0.07 HEIGHT)
         (place-image
          (text/font "Balls Missed:" 14 "RED" #f 'modern 'normal 'normal #f)
          (* 0.78 WIDTH) (* 0.07 HEIGHT)
          (scene-with-balls-list
           (world-balls w)
           (scene-with-mouse
            (world-racket w)
            (scene-with-racket
             (world-racket w)
             (court-scene w)))))))))
     
     (place-image
      (text/font  
       (number->string (world-time w)) 14 "RED" #f 'modern 'normal 'normal #f)
      (+ 50 (* 0.82 WIDTH)) (* 0.04 HEIGHT)
      (place-image
       (text/font "Time:" 14 "RED" #f 'modern 'normal 'normal #f)
       (* 0.82 WIDTH) (* 0.04 HEIGHT)
       (place-image
        (text/font 
         (number->string (world-miss w))
         14 "RED" #f 'modern 'normal 'normal #f)
        (+ 72 (* 0.78 WIDTH)) (* 0.07 HEIGHT)
        (place-image
         (text/font "Balls Missed:" 14 "RED" #f 'modern 'normal 'normal #f)
         (* 0.78 WIDTH) (* 0.07 HEIGHT)
         (scene-with-balls-list
          (world-balls w)
          (scene-with-mouse
           (world-racket w)
           (scene-with-racket
            (world-racket w)
            (court-scene w)))))))))]   
    
[(and (string=? (world-state w) "pause") (>= (world-miss w) TOTAL-MISS))
 (place-image
  (text/font "Press 'r' or 'space' to restart."
             30 "GREEN" #f 'roman 'normal 'bold #f)
  (* 0.5 WIDTH) (* 0.7 HEIGHT)
  (place-image
   (text/font 
    (number->string (world-time w)) 14 "RED" #f 'modern 'normal 'normal #f)
   (+ 50 (* 0.82 WIDTH)) (* 0.04 HEIGHT)
   (place-image
    (text/font "Time:" 14 "RED" #f 'modern 'normal 'normal #f)
    (* 0.82 WIDTH) (* 0.04 HEIGHT)
    (place-image
     (text/font "GAME OVER" 45 "Blue" #f 'roman 'normal 'bold #f)
     (* 0.5 WIDTH) (* 0.40 HEIGHT)
     (place-image
      (text/font "SCORE : " 45 "Blue" #f 'roman 'normal 'bold #f)
      (* 0.43 WIDTH) (* 0.50 HEIGHT)
      (place-image
       (text/font  (number->string (world-time w))
                   45 "Blue" #f 'roman 'normal 'bold #f)
       (+ 160 (* 0.42 WIDTH)) (* 0.50 HEIGHT)
       (scene-with-balls-list
        (world-balls w)
        (scene-with-mouse
         (world-racket w)
         (scene-with-racket
          (world-racket w)
          EMPTY-COURT)))))))))] 
    
[(string=? (world-state w) "pause")
     
 (place-image
  (text/font 
   (number->string (world-time w)) 14 "RED" #f 'modern 'normal 'normal #f)
  (+ 50 (* 0.82 WIDTH)) (* 0.04 HEIGHT)
  (place-image
   (text/font "Time:" 14 "RED" #f 'modern'normal 'normal #f)
   (* 0.82 WIDTH) (* 0.04 HEIGHT)
   (place-image
    (text/font 
     (number->string (world-miss w)) 14 "RED" #f 'modern 'normal 'normal #f)
    (+ 72 (* 0.78 WIDTH)) (* 0.07 HEIGHT)
    (place-image
     (text/font "Balls Missed:" 14 "RED" #f 'modern 'normal 'normal #f)
     (* 0.78 WIDTH) (* 0.07 HEIGHT)
     (place-image
      (text/font "Game  Paused" 50 "RED" #f 'roman 'normal 'bold #f)
      (* 0.5 WIDTH) (* 0.42 HEIGHT)
      (scene-with-balls-list
       (world-balls w)
       (scene-with-mouse
        (world-racket w)
        (scene-with-racket
         (world-racket w)
         (court-scene w)))))))))] 
))
    
;;scene-with-mouse : Racket Scene -> Scene
;GIVEN: racket r and scene s
;;RETURN: mouse pointer in the given scene s
;;EXAMPLES:
;; (scene-with-mouse
;; (make-racket 330 384 0 0 2 2 #t)
;;  (world-to-scene (initial-world 1/24))) =>
;; (place-image
;;    MOUSE-POINTER
;;    2
;;    2
;;    (world-to-scene (initial-world 1/24)))
;;DESIGN STRATEGY: using in built function place-image
;;and using observer template

(define (scene-with-mouse r s)
  (if (racket-selected? r)
      (place-image
       MOUSE-POINTER
       (racket-mx r) (racket-my r)
       s)
      s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scene-with-balls-list: BallList Scene -> Scene
;; GIVEN: a scene and BallList bl
;;RETURNS: a scene like the given one , but with the list of ball in it 
;; EXAMPLES:
;;(scene-with-balls-list
;;    (list(make-ball 330 384 0 0))
;;   (scene-with-mouse
;;    (make-racket 330 384 0 0 0 0 #f)
;;     (scene-with-racket
;;      (make-racket 330 384 0 0 0 0 #f)
;;         EMPTY-COURT)))
;; => (place-image
;;    BALL-IMAGE
;    330
;    384
;   (scene-with-mouse
;     (make-racket 330 384 0 0 0 0 #f)
;     (scene-with-racket
;      (make-racket 330 384 0 0 0 0 #f)
;      EMPTY-COURT)))
;; DESIGN STRATEGY: usinh HOF Foldr on bl

;(define (scene-with-balls-list bl s)
;  (cond
;    [(empty? bl) s]
;    [else
;     (scene-with-balls-list
;      (rest bl)
;      (scene-with-ball (first bl) s))]))

(define (scene-with-balls-list bl s)
  (foldr scene-with-ball s bl))


;;scene-with-ball : Ball Scene -> Scene
;; GIVEN: a scene and a ball
;;RETURNS: a scene like the given one , but with the ball in it
;;EXAMPLES :
;;(scene-with-ball  (make-ball 330 384 0 0) 
;;                     (world-to-scene (initial-world 1/24)) )
;; -> (place-image
;;       BALL-IMAGE  330 384 (world-to-scene (initial-world 1/24)))

;; STRATEGY: using in built function place-image and using observer template
(define (scene-with-ball b s)
  (place-image
   BALL-IMAGE
   (ball-x b) (ball-y b)
   s)) 

;;scene-with-racket : Racket Scene -> Scene
;; GIVEN:  a racket and a scene
;;RETURNS: a scene like the given one, but with the racket in it
;EXAMPLES:
;;(scene-with-racket (make-racket 330 384 0 0 0 0 #f)
;;                      (world-to-scene (initial-world 1/24)) )
;; -> (place-image
;;    RACKET-IMAGE 330 384 (world-to-scene (initial-world 1/24)))

;; STRATEGY: using in built function place-image and using observer template
(define (scene-with-racket r s)
  (place-image
   RACKET-IMAGE
   (racket-x r) (racket-y r)
   s))



;;court-scene:World->Scene
;;GIVEN: a world in some state
;;RETURN : a scene for either paused state or unpaused state
;EXAMPLES:(court-scene (initial-world 1/24)) ->EMPTY-COURT
;;STRATEGY: using case for world w on world-paused?
(define (court-scene w)
  (if(string=? (world-state w) "pause") RESETTING-COURT  EMPTY-COURT))


(define (balls-after-tick bl w)
  (cond
    [(and (> (world-time w) 0) (= 0 (modulo (world-time w) (* FPS 2))))
     (cons (make-ball (+ 50 (random 500)) (+ 350 (random 150))
                      (+ (* 2 (random 10)) -12)  (+ -12 (random 6)))
           (map
            ;Ball-> Ball
            ;GIVEN: ball b
            ;RETURN: updated ball after tick 
            (lambda (b) (ball-after-tick b w))
            (local (; Ball->Boolean
                    ;GIVEN: ball b in world 
                    ;RETURN: true iff given ball has not hit backwall 
                    (define (ball-backwall-collision? b)
                      (not(< HEIGHT (+ (ball-y b) (ball-vy b))))))
              (filter ball-backwall-collision? bl))))]

    [else
     (map
      ;Ball-> Ball
      ;GIVEN: ball b
      ;RETURN: updated ball after tick 
      (lambda (b) (ball-after-tick b w))
      (local (; Ball->Boolean
              ;GIVEN: ball b in world
              ;RETURN: true iff given ball has not hit backwall 
              (define (ball-backwall-collision? b)
                (not(< HEIGHT (+ (ball-y b) (ball-vy b))))))
        (filter ball-backwall-collision? bl)))]))



(define (ball-after-tick b w)
  ;(ball-next-move b))
  (cond
    [(ball-collide-wall? b) (after-ball-collide-wall b)]
    [(ball-collide-racket? b (world-racket w))
     (ball-after-colliding-racket b (world-racket w))]
    [else (ball-next-move b)]))

(define (racket-after-tick w)
  (racket-next-move (world-racket w)))




(define (racket-next-move r)
  (make-racket
   (+ (racket-x r) (racket-vx r))
   (+ (racket-y r) (racket-vy r))
   (racket-vx r)
   (racket-vy r)
   (racket-mx r)
   (racket-my r)
   (racket-selected? r)))  




;;;; run simulation

(simulation 66)
