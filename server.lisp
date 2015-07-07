;;;; This is the code for the main server.
(in-package :server)
(syntax:use-syntax :clamp)

(defparameter log-file* nil
  "The file to log all of the game information to.")

(defparameter show-output* t
  "If T, any data sent to the log will be printed to *standard-output*.")

(defparameter sockets* '()
  "A list of all of the sockets we need to listen for. This includes
  all of the sockets that may disconnect.")

(defparameter misc-sockets* '()
  "These are sockets that aren't attached to any game yet.")

(defparameter socket->game* (table)
  "A table mapping each socket to the game it is associated with. This
   is in case a player close all of the other sockets.")

(defparameter game->sockets* (table)
  "A table mapping from a game to all of the sockets we need to
   disconnect if a player disconnects.")

(defparameter game* nil
  "The current game being played. This is used as a dynamic variable
   so that any function can determine what the current game is.")

(defparameter disconnected-code* -1 "The code for when a player disconnects.")
(defparameter unknown-error-code* -2 "The code for when an unknown error occurs.")
(defparameter invalid-flag-code*  -3 "The code for when a player enters an illegal flag.")

(def start-server (games)
  "GAMES should be a list of duples Each one containing a game-type,
   and a port for players to connect to."
  (let game-sockets (mapeach (g p) games
                      (let socket (socket-listen *wildcard-host* p :reuse-address t)
                        (push socket misc-sockets*)
                        (list g socket)))
    (unwind-protect (do (each (game-type socket) game-sockets
                          (push socket sockets*)
                          ;; Using a zero dimensional array as a pointer
                          ;; so that both add-player closures always
                          ;; refer to the same game.
                          (let arr (make-array '())
                            (= arr!aref (inst game-type))
                            (set-cont socket (add-player arr))))
                        (listening-loop))
      (each socket misc-sockets*
        (socket-close socket)
        (= sockets* (rem socket sockets*))
        (rem-cont socket)
        (= misc-sockets* '()))
      (each game (copy-list (keys game->sockets*))
        (send :all game!players "~A~%" unknown-error-code*)
        (disconnect game)))))

(def listening-loop ()
  "The main loop for listening."
  (while sockets*
    (let sockets (wait-for-input sockets* :ready-only t)
      (each socket sockets
        (aif2 (and (~isa socket 'stream-server-usocket)
                   (is socket (peek-char nil (socket-stream socket) nil socket)))
              (do (send :all socket->game*.socket!players "~A~%" disconnected-code*)
                  (disconnect socket->game*.socket)
                  ;; We need to go through the loop again since we may
                  ;; have disconnected some of the other ready sockets.
                  (return))
              (with (socket* socket game* socket->game*.socket)
                (restart-case (call-cont socket)
                  (disconnect ()
                    :report "Disconnect the current game."
                    (send :all socket->game*.socket!players "~A~%" unknown-error-code*)
                    (disconnect)
                    (return)))))))))

(def disconnect (? (game game*))
  "Disconnect a game. Sends INFO to all of the human players and CODE
   to all of the AI players."
  (zap #'set-difference sockets* game->sockets*.game)
  (each socket game->sockets*.game
    (remhash socket socket->game*)
    (rem-cont socket)
    (socket-close socket))
  (remhash game game->sockets*)
  (wipe game!players)
  (when log-file*
    (w/file (*standard-output* log-file*
             :direction :output
             :if-exists :append
             :if-does-not-exist :create)
      (mapc #'pr (reverse game!game-log)))))

(defcont add-player (arr)
  "Connect a given player."
  (let socket (socket-accept socket*)
    (set-cont socket (read-flags arr))
    (push socket sockets*)
    (push socket misc-sockets*)))

(defcont read-flags (arr)
  "Reads the flags from the player."
  (withs (game arr!aref
          flags (parse-flags))
    (if (is flags :invalid)
        (do (format socket*!socket-stream "~A~%" invalid-flag-code*)
            (force-output socket*!socket-stream)
            (set-cont socket* (read-flags arr)))
        (let player (inst 'player :socket socket* :flags flags)
          (push socket* game->sockets*.game)
          (= socket->game*.socket* game)
          (= misc-sockets* (rem socket* misc-sockets*))
          (push player game!players)
          (when (is game!players!len game!need)
            ;; Have the continuation add players to a new game instead of
            ;; the current one. It is safe to use type-of as it will always
            ;; return the class-name of a class with a proper name.
            (= arr!aref (inst (type-of game)))
            (let game* game
              (start-game game)))))))

(def parse-flags ()
  "Returns the flags the socket wants to use. If they aren't valid
   returns :INVALID."
  (ado (read-line :from socket*!socket-stream)
       (tokens it)
       (check it
              [subsetp _ socket->game*!flags]
              :invalid)))
