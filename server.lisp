(in-package :server)
(syntax:use-syntax :clamp)

(define-condition game-error (simple-error) ())
(define-condition invalid-move (game-error) ())

(deftem (game (:conc-name nil))
  players)

(deftem (tic-tac-toe (:conc-name nil) (:include game))
  board current (need 2))

(deftem (player (:conc-name nil))
  socket)

(deftem (human (:include player)))
(deftem (ai (:include player)))

(defparameter timeout* 10)

(defparameter conts* (table)
  "A mapping of sockets to continuations that should be called when
   that socket is available.")

(defparameter temp-conts* (table)
  "A mapping of sockets to continuations that should be called when
   the socket and should be removed from this table when the socket is
   available.")

(defparameter sockets* '()
  "A list of all of the sockets we need to listen for. This includes
  all of the sockets that may disconnect.")

(defparameter socket->game* (table)
  "A table mapping each socket to the game it is associated with. This
   is in case a player close all of the other sockets.")

(defparameter game->sockets* (table)
  "A table mapping from a game to all of the sockets we need to
   disconnect if a player disconnects.")

(defparameter show-output* nil "If T, any output to any player will be shown.")

(mac defcont (name args1 args2 &body body)
  "Define a continuation. It takes two arguments, the arguments to
   create the continuation and the arguments actually passed to the
   continuation."
  `(def ,name ,args1
     ,(check (car body) #'stringp)
     (fn ,args2 ,@body)))

;; (def start-server (game-type ports &rest args)
;;   "Start a server playing a game of type GAME"
;;   (unwind-protect (do (each port (mklist ports)
;;                         (with (game (apply #'inst game-type args)
;;                                     listener (socket-listen *wildcard-host* port :reuse-address t))
;;                           (push listener sockets*)
;;                           (= conts*.listener (add-player game))))
;;                       (listening-loop))
;;     (mapc #'safely-close sockets*)
;;     (= sockets* '())))

(def start-server (game-type hport aport &rest args)
  "Start a server playing a game of type GAME"
  (unwind-protect (do (let game (apply #'inst game-type args)
                        (let listener (socket-listen *wildcard-host* hport :reuse-address t)
                          (push listener sockets*)
                          (push listener game->sockets*.game)
                          (= (cont listener) (add-player game 'human)))
                        (let listener (socket-listen *wildcard-host* aport :reuse-address t)
                          (push listener sockets*)
                          (push listener game->sockets*.game)
                          (= (cont listener) (add-player game 'ai))))
                      (listening-loop))
    (mapc [disconnect _ t] (keys game->sockets*))))

(def cont (socket)
  "Returns the permanent continuation for SOCKET."
  conts*.socket)

(def (setf cont) (continuation socket)
  "Sets the continuation for the given socket."
  (setf conts*.socket continuation))

(def temp-cont (socket)
  "Returns the temporary continuation for SOCKET. A continuation that
   will be removed as soon as it is returned."
  (prog1 temp-conts*.socket
    (rem-temp-cont socket)))

(def rem-cont (socket)
  "Remove the continuation for the given socket."
  (remhash socket conts*))

(def (setf temp-cont) (continuation socket)
  "Sets the temporary continuation for a given socket. It will be
   removed as soon as it is returned by temp-cont."
  (setf temp-conts*.socket continuation))

(def rem-temp-cont (socket)
  "Removes the temporary continuation for the given socket."
  (remhash socket temp-conts*))

(def listening-loop ()
  "The main loop for listening."
  (while sockets*
    (let sockets (wait-for-input sockets* :ready-only t)
      (each socket sockets
        (aif2 (and (~isa socket 'stream-server-usocket)
                   (is socket (peek-char nil (socket-stream socket) nil socket)))
                ;; We need to go through the loop again since we may
                ;; have disconnected some of the other ready sockets.
                (do (disconnect socket->game*.socket t)
                    (return))
              (cont socket)
                (call it socket)
              (temp-cont socket)
                (restart-case (call it socket)
                  (restart-turn ()
                    :report "Restart the current player's turn."
                    (= (temp-cont socket) it)
                    (send-hu socket->game*.socket!current "Enter a legal move.~%"))
                  (disconnect ()
                    :report "Disconnect the current game."
                    (disconnect socket->game*.socket t) (return)))
              :else
                (do (restart-case (error "No continuation for socket ~A." socket)
                      (ignore-input ()
                        :report "Ignore all of the new information from the socket."
                        (while (listen socket!socket-stream)
                          (read-line :from socket!socket-stream))))))))))

(def disconnect (game ? pdisc)
  "Disconnect a game."
  (when pdisc
    (send-hu game!players "A player disconnected, terminating the game.~%")
    (send-ai game!players "-1~%"))
  (zap #'set-difference sockets* game->sockets*.game)
  (each socket game->sockets*.game
    (remhash socket socket->game*)
    (rem-cont socket)
    (rem-temp-cont socket)
    (mapc #'socket-close game->sockets*.game))
  (remhash game game->sockets*))

(defcont add-player (game type) (listener)
  "Wait for all of the players to connect."
  (withs (socket (socket-accept listener)
          player (inst type :socket socket))
    (push socket game->sockets*.game)
    (push socket sockets*)
    (= socket->game*.socket game)
    (push player game!players)
    (when (is game!players!len game!need)
      (let listeners (set-difference game->sockets*.game (map #'socket game!players))
        (= game->sockets*.game (set-difference game->sockets*.game listeners))
        (= sockets* (set-difference sockets* listeners))
        (mapc #'socket-close listeners))
      (start-game game))))

(def send (players &rest args)
  "Takes a single player or a list of players and formats the string to all of them."
  (each player (acheck (mklist players) [not show-output*] (cons *standard-output* it))
    (withs (socket (if (player-p player) player!socket player)
            stream (if (isa socket 'usocket) socket!socket-stream socket))
      (apply #'format stream args)
      (force-output stream))))

(def send-ai (players &rest args)
  "Sends the message only to the ai players."
  (apply #'send (keep #'ai-p (mklist players)) args))

(def send-hu (players &rest args)
  "Sends the message only to the human players."
  (apply #'send (keep #'human-p (mklist players)) args))

(def start-game (game)
  "Initialize the actual game."
  (= game!board (make-array '(3 3) :initial-element nil))
  (= game!current (car game!players))
  (= (temp-cont game!current!socket) (play-turn game))
  (push game!current!socket sockets*)
  (send-hu game!players "~A" game ())
  
  (send-hu game!current "It is your turn.~%")
  (send-hu game!next    "It is your opponents turn.~%")
  
  (send-ai game!current "0~%")
  (send-ai game!next    "1~%"))
      
(defcont play-turn (game) (socket)
  "Performs a turn for the current player."
  (declare (ignore socket))
  (mvb (r c) (read-input game game!current)
    (= game!board.r.c (if (is game!current game!players!car) 'x 'o))
    (send-ai game!next "~A ~A~%" r c)
    (aif (winner game)
      (do (announce-winner game)
          (disconnect game))
      (do (send-hu game!players "~A" game ())
          (= game!current (next game))
          (send-hu game!current "Your turn.~%" ())
          (push game!current!socket sockets*)
          (= (temp-cont game!current!socket) (play-turn game))))))

(defmethod print-object ((game tic-tac-toe) stream)
  (let *standard-output* stream
    (up r 0 3
      (up c 0 3
        (prf "~:[ ~;~:*~A~]" game!board.r.c)
        (unless (is c 2)
          (pr "|")))
      (prn)
      (unless (is r 2)
        (prn "-----")))))

(def next (game)
  "Returns the next player in the game."
  (aif (cadr+mem game!current game!players)
    it
    (car game!players)))

(def winner (game)
  "Is there a winner of this game? If so return the piece of that player."
  (or (iter (for r from 0 below 3)
            (thereis (iter (for c from 0 below 3)
                           (always (and (is game!board.r.c game!board.r.0) game!board.r.0)))))
      (iter (for c from 0 below 3)
            (thereis (iter (for r from 0 below 3)
                           (always (and (is game!board.r.c game!board.0.c) game!board.0.c)))))
      (and game!board.0.0
           (is game!board.0.0 game!board.1.1)
           (is game!board.0.0 game!board.2.2)
           game!board.0.0)
      (and game!board.0.2
           (is game!board.0.2 game!board.1.1)
           (is game!board.0.2 game!board.2.0)
           game!board.0.2)
      (and (iter out
                 (for r from 0 below 3)
                 (iter (for c from 0 below 3)
                       (in out (always game!board.r.c))))
           'tie)))

(def announce-winner (game)
  "Announce the winner of the game."
  (if (is (winner game) 'tie)
      (do (send-hu game!players "It was a tie.~%")
          (send-ai game!players "0~%"))
      (do (send-hu game!players "~:[Player 2~;Player 1~] won!~%" (is (winner game) 'x))
          (send-ai game!players "~:[-1~;1~]~%" (is (winner game) 'o)))))

(defgeneric read-input (game player)
  (:documentation "Reads the input for the game."))

(defmethod read-input :around (game (player human))
  (handler-bind ((invalid-move #'restart-turn-handler))
    (call-next-method)))

(defmethod read-input :around (game (player ai))
  (handler-bind ((invalid-move #'disconnect-handler))
    (call-next-method)))

(defmethod read-input ((game tic-tac-toe) player)
  (ttt-read-input game player))

(def ttt-read-input (game player)
  "Read the input for a tic-tac-toe game. The only reason this
   function exists is because the input methods for a human and for a
   computer are the same."
  (let line (read-line :from player!socket!socket-stream)
    (mvb (match strings) (scan-to-strings "^(\\d) (\\d)\\s*$" line)
      (unless match
        (error 'invalid-move
               :format-control "'~A' is malformed input."
               :format-arguments (list )))
      (let (r c) (map #'parse-integer strings)
        (unless (<= 0 r 2)
          (error 'invalid-move
                 :format-control "The row index ~A is illegal."
                 :format-arguments (list r)))
        (unless (<= 0 c 2)
          (error 'invalid-move
                 :format-control "The column index ~A is illegal."
                 :format-arguments (list r)))
        (when game!board.r.c
          (error 'invalid-move :format-control "The square is already taken."))
        (values r c)))))

(def restart-turn-handler (&rest args)
  "A restart handler for restarting the current players turn."
  (declare (ignore args))
  (invoke-restart 'restart-turn))

(def disconnect-handler (&rest args)
  "A restart handler for disconnecting the current game."
  (declare (ignore args))
  (invoke-restart 'disconnect))

(def ignore-input-handler (&rest args)
  "A restart handler for ignoring the input from a socket with no continuation."
  (declare (ignore args))
  (invoke-restart 'ignore-input))
