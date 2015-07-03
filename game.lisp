;;;; Generic code for handling all games.
(in-package :server)
(syntax:use-syntax :clamp)

(deftem (game (:conc-name nil))
  need  
  players
  game-log)

(deftem (player (:conc-name nil))
  socket flags)

(def send (flags players &rest args)
  "Takes a single player or a list of players, and a flag or list of
   flags to print, and then uses format to print the strings to all of
   players that one of the given flags set."
  (each player (keep (mklist flags) (mklist players) :key #'flags :test #'intersection)
    (check-type player player)
    (let stream player!socket!socket-stream
      (apply #'format stream args)
      (force-output stream))))

(def send-log (game control &rest args)
  "Uses format with CONTROL and ARGS to store a string to the game
   log."
  (when show-output*
    (format t "~?" control args))
  (push (format nil "~?" control args) game!game-log)
  t)

(defgeneric start-game (game)
  (:documentation "Starts the actual game."))

(defgeneric read-input (game player &rest args)
  (:documentation "Reads the input for the game.")
  (:method :around (game (player player) &rest args)
    (declare (ignore args))
    ;; When a player makes an illegal move we will disconnect them.
    (handler-bind ((invalid-move #'disconnect-handler))
      (call-next-method))))

