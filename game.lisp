;;;; Generic code for handling all games.
(in-package :server)
(syntax:use-syntax :clamp)

(deftem (game (:conc-name nil))
  need  
  players
  game-log
  flags
  input-flags)

(deftem (player (:conc-name nil))
  socket flags input-flags)

(def send (flags players &rest args)
  "Takes a single player or a list of players, and a flag or list of
   flags to print, and then uses format to print the strings to all of
   players that one of the given flags set."
  (zap #'mklist flags)
  (zap #'mklist players)
  (each player (if (mem :all flags)
                   players
                   (keep flags players :key #'flags :test #'intersection))
    (check-type player player)
    (let stream player!socket!socket-stream
      (apply #'format stream args)
      (force-output stream)))
  (when (mem :log flags)
    (when show-output*
      (apply #'prf args))
    (push (apply #'format nil args) game*!game-log)))

(defgeneric start-game (game)
  (:documentation "Starts the actual game."))

(defgeneric read-input (game flag &rest args)
  (:documentation "Reads the input for the game.")
  (:method (game (player player) &rest args)
    (apply #'read-input game (input-flags player) args))
  (:method :around (game (flag symbol) &rest args)
    (declare (ignore args))
    ;; When a player makes an illegal move we will disconnect them.
    (handler-bind ((invalid-move #'disconnect-handler))
      (call-next-method))))

