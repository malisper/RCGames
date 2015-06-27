;;;; Generic code for handling all games.
(in-package :server)
(syntax:use-syntax :clamp)

(deftem (game (:conc-name nil))
  need  
  players
  game-log)

(deftem (player (:conc-name nil))
  socket)

(deftem (human (:include player)))
(deftem (ai (:include player)))

(def send (players &rest args)
  "Takes a single player or a list of players and uses format to print
   the string to all of them."
  (each player (mklist players)
    (withs (socket (if (player-p player) player!socket player)
            stream (if (isa socket 'usocket) socket!socket-stream socket))
      (apply #'format stream args)
      (force-output stream))))

(def send-ai (players &rest args)
  "Sends the message only to the ai players in PLAYERS."
  (apply #'send (keep #'ai-p (mklist players)) args))

(def send-hu (players &rest args)
  "Sends the message only to the human players in PLAYERS."
  (apply #'send (keep #'human-p (mklist players)) args))

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
  (:documentation "Reads the input for the game."))

(defmethod read-input :around (game (player human) &rest args)
  ;; We should give players multiple chances to enter correct input.
  (handler-bind ((invalid-move #'restart-turn-handler))
    (call-next-method)))

(defmethod read-input :around (game (player ai) &rest args)
  ;; We should give the ai a single chance before disconnecting them
  ;; when they make an invalid move.
  (handler-bind ((invalid-move #'disconnect-handler))
    (call-next-method)))
