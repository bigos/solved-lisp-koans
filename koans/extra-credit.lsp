;; EXTRA CREDIT:
;;
;; Create a program that will play the Greed Game.
;; Rules for the game are in GREED_RULES.TXT.
;;
;; You already have a DiceSet class and score function you can use.
;; Write a player class and a Game class to complete the project.  This
;; is a free form assignment, so approach it however you desire.



(defclass player () ;-----------------------------------------------------
  ((name :reader get-name :initarg :name)
   (total-score :accessor total-score :initform 0)))

(defmethod add-score ((object player) score)
  (incf (total-score object) score))

(defclass dice-set () ;-----------------------------------------------------
  ((values :initform nil)))

(defmethod get-values ((object dice-set))
  (slot-value object 'values))

(defmethod roll (how-many (object dice-set))
  (setf (slot-value object 'values) nil)
  (dotimes (x how-many) 
    (push (1+ (random 6)) (slot-value object 'values)))
  (slot-value object 'values))

(defun score (dice)
  (flet ((sub-score (z set-val single-val)
	   (multiple-value-bind (sets singles) (floor (count z dice) 3)
	     (+ (* sets set-val)
		(* singles single-val)))))
    (let ((total 0))
      (loop for z from 1 to 6 do 	 	       
	   (cond ((eq z 1)		  
		  (incf total (sub-score z 1000 100)))
		 ((eq z 5) 	       
		  (incf total (sub-score z 500 50)))	     
		 (t 
		  (incf total (sub-score z (* 100 z) 0)))))
      total)))

(defclass game () ;-----------------------------------------------------
  ((players :reader players :initform nil)
   (dice :reader dice :initform (make-instance 'dice-set))))

(defmethod add-player ((object game) player-name)
  (setf (slot-value object 'players) 
	(append (players object) `(,(make-instance 'player :name player-name)))))

(defmethod winner ((object game))
  (let ((wins (car (players object))))
    (dolist (player (players object))
      (when (> (total-score player) (total-score wins))
	(setf wins player)))
    (format t "~2& ~s wins~%"  (get-name wins))))

(defmethod scores ((object game))
  (let ((score-table (make-array (length (players *game*)))))
    (format t "~s" score-table)
    ))

(defmethod play ((object game))
  (let ((sc))
    (format t "~&~%")
    (dolist (player (players object)) 
      (format t "~&~S is playing now~%"  (get-name player))
      (roll 7 (dice object))
      (add-score player (setf sc (score (get-values (dice object)))))
      (format t "dice ~s - score ~s - total score  ~s" (get-values (dice object)) sc  (total-score player))))
  (winner object))

(defmethod winner ((object game))
  (let ((wins (car (players object))))
    (dolist (player (players object))
      (when (> (total-score player) (total-score wins))
	(setf wins player)))
    (format t "~&~s ~s wins" object (get-name wins))))

(defmethod get-scores ((object game))
  (let ((scores))
    (dolist (player (players object))
      (push (list (get-name player) (total-score player)) scores))
    (sort scores #'> :key #'cadr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defparameter *game* (make-instance 'game))

(dolist (player '("Jacek" "Martin" "Chris"))
  (add-player *game* player))

(play *game*)
(winner *game*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test test-player-creation
    (let ((new-player (make-instance 'player :name "Jacek")))
      (assert-equal "Jacek" (get-name new-player))
      (assert-equal 0 (total-score new-player))))

(define-test test-adding-score
    (let ((player1 (make-instance 'player :name "Jacek"))
	  (player2 (make-instance 'player :name "Chris")))
      (add-score player1 7)
      (add-score player2 9)
      (assert-equal 7 (total-score player1))
      (assert-equal 9 (total-score player2))))

(define-test test-game-creation
    (let ((new-game (make-instance 'game)))
      (assert-equal nil (players new-game))
      (add-player new-game "Jacek")
      (assert-equal "Jacek" (get-name (first (players new-game))))
      (add-player new-game "Chris")
      (assert-equal "Chris" (get-name (second (players new-game))))
      (assert-equal 'dice-set (type-of (dice new-game)))
      (assert-equal 0 (score (get-values (dice new-game))))
      ))
