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
  (awhen (intersection '(:log :logp) flags)
    (when show-output*
      (if (mem :logp it)
          (apply #'prf "~A:~@?" player*!num args)
          (apply #'prf "~@?" args)))
    (push (apply #'format nil args) game*!game-log)))

(defgeneric start-game (game)
  (:documentation "Starts the actual game."))

(defmethod start-game :around (game)
  "Bind player* and game* to the proper values before starting the game."
  (withs (player* (car game!players)
          game* game)
    (call-next-method)))

(defmethod start-game :before (game)
  "Initialize all of the player numbers. And send that information to
   the players and to the log."
  (iter (for i from 1)
        (for player in game!players)
        (= player!num i))
  (send :log nil "~A ~A~%" (type-of game) (len game!players))
  (send :log nil "~{~{~A~^.~}~^ ~}~%" (map (compose [coerce _ 'list] #'get-peer-address) game!players))
  (each player game*!players
    (send :all player "~A~%" player!num)))

(defgeneric read-input (game flag)
  (:documentation "Reads the input for the game."))

(mac defstart (game &body body)
  "Define a function to start a game of type GAME."
  `(defmethod start-game ((,(gensym) ,game))
     ,@body))

(mac defread (game flag &body body)
  "Define a function to read the input from a player with FLAG
   set. That function should only parse the input and make sure it is
   in a valid format. It can, but doesn't have to check whether the
   move is a legal move."
  `(defmethod read-input ((,(gensym) ,game)
                          ;; If there is no flag don't specify on it
                          ,(if (no flag) (gensym) `(,(gensym) (eql ,flag))))
     ,@body))

(def read-move (? flags)
  "Read a move from the given player with the format being one of
   FLAGS. This will only parse the move and check that it is in a
   valid format. Does no validation with respect to the move being a
   legal move."
  (zap #'mklist flags)
  (let int (if (single flags) flags (intersection player*!flags flags))
    (unless (or (no flags) (single int))
      (if (and flags int)
          (signal-invalid-flags "None of the flags in ~A are members of ~A." player*!flags flags)
          (signal-invalid-flags "Multiple of the flags in ~A are members of ~A." player*!flags flags)))
    (read-input game* (car int))))
