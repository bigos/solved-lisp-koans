;; EXTRA CREDIT:
;;
;; Create a program that will play the Greed Game.
;; Rules for the game are in GREED_RULES.TXT.
;;
;; You already have a DiceSet class and score function you can use.
;; Write a player class and a Game class to complete the project.  This
;; is a free form assignment, so approach it however you desire.



(defclass player ()
  ((name :reader name :initarg name)
   (score :accessor score :initarg score :initform 0)))

(defmethod add-score ((object player) score)
  (incf (slot-value object 'score) score))


(defclass game ()
  ((players :initform nil)))

(defmethod add-player ((object game) player)
  (push player (slot-value object 'players))
  )


(define-test test-player-creation
    (let ((new-player (make-instance 'player :name "Jacek")))
      (assert-equal "Jacek" (slot-value new-player 'name))
      (assert-equal 0 (slot-value new-player 'score))))
