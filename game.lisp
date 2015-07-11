;;;; Generic code for handling all games.
(in-package :server)
(syntax:use-syntax :clamp)

(deftem (game (:conc-name nil))
  need players game-log flags)

(deftem (player (:conc-name nil))
  socket flags)

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

(defgeneric read-input (game flag)
  (:documentation "Reads the input for the game.")
  (:method :around (game (flag symbol))
    ;; When a player makes an illegal move we will disconnect them.
    (handler-bind ((invalid-move #'disconnect-handler))
      (call-next-method))))

(mac defstart (game &body body)
  `(defmethod start-game ((,(gensym) ,game))
     (let player* (car game*!players)
       ,@body)))

(mac defread (game flag &body body)
  `(defmethod read-input ((,(gensym) ,game)
                          ;; If there is no flag don't specify on it
                          ,(if (no flag) (gensym) `(,(gensym) ,flag)))
     ,@body))

(def read-move (? flags)
  "Read a move from the given player with the format being one of FLAGS."
  (read-input game* (intersection player*!flags (mklist flags))))
