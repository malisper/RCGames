;;;; Generic code for handling all games.
(in-package :server)
(syntax:use-syntax :clamp)

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
    (let stream player!socket-stream
      (apply #'format stream args)
      (force-output stream)))
  (when (mem :log flags)
    (when show-output*
      (apply #'prf args))
    (push (apply #'format nil args) game*!game-log)))

(defgeneric start-game (game)
  (:documentation "Starts the actual game."))

(defmethod start-game :around (game)
  "Bind player* and game* to the proper values before starting the game."
  (withs (player* (car game*!players)
          game* player*!game)
    (call-next-method)))

(defmethod start-game :before (game)
  "Initialize all of the player numbers."
  (iter (for i from 1)
        (for player in game!players)
        (= player!num i)))

(defgeneric read-input (game flag)
  (:documentation "Reads the input for the game."))

(mac defstart (game &body body)
  `(defmethod start-game ((,(gensym) ,game))
     ,@body))

(mac defread (game flag &body body)
  `(defmethod read-input ((,(gensym) ,game)
                          ;; If there is no flag don't specify on it
                          ,(if (no flag) (gensym) `(,(gensym) ,flag)))
     ,@body))

(def read-move (? flags)
  "Read a move from the given player with the format being one of FLAGS."
  (let int (intersection player*!flags (mklist flags))
    (unless (or (no flags) (single int))
      (if (and flags int)
          (signal-invalid-flags "None of the flags in ~A are members of ~A." player*!flags flags)
          (signal-invalid-flags "Multiple of the flags in ~A are members of ~A." player*!flags flags)))
    (read-input game* (car int))))
