;;;; This is the code for the main server.
(in-package :server)
(syntax:use-syntax :clamp)

(define-condition game-error (simple-error) ())
(define-condition invalid-move (game-error) ())

(defparameter log-file* nil
  "The file to log all of the game information to.")

(defparameter show-output* nil
  "If T, any data sent to the log will be printed to *standard-output*.")

(defparameter sockets* '()
  "A list of all of the sockets we need to listen for. This includes
  all of the sockets that may disconnect.")

(defparameter socket->game* (table)
  "A table mapping each socket to the game it is associated with. This
   is in case a player close all of the other sockets.")

(defparameter game->sockets* (table)
  "A table mapping from a game to all of the sockets we need to
   disconnect if a player disconnects.")

(defparameter disconnected-code* -1 "The code for when a player disconnects.")
(defparameter unknown-error-code* -2 "The code for when an unknown error occurs.")

(def start-server (games)
  "GAMES should be a list of triples. Each one containing a game-type,
   a port for humans to connect to, and a port for AI to connect to."
  (let game-sockets (mapeach (g h a) games
                      (list g
                            (socket-listen *wildcard-host* h)
                            (socket-listen *wildcard-host* a)))
    (unwind-protect (do (each (game-type hs as) game-sockets
                          (push hs sockets*)
                          (push as sockets*)
                          ;; Using a zero dimensional array as a pointer
                          ;; so that both add-player closures always
                          ;; refer to the same game.
                          (let arr (make-array '())
                               (= arr!aref (inst game-type))
                               (= (cont hs) (add-player arr 'human))
                               (= (cont as) (add-player arr 'ai))))
                        (listening-loop))
      (each (_ . ss) game-sockets
        (each s ss
          (socket-close s)
          (= sockets* (rem s sockets*))
          (rem-cont s)))
      (each game (copy-list (keys game->sockets*))
        (send-hu game!players "An error occured~%.")
        (send-ai game!players "~A~%" unknown-error-code*)
        (disconnect game)))))

(def listening-loop ()
  "The main loop for listening."
  (while sockets*
    (let sockets (wait-for-input sockets* :ready-only t)
      (each socket sockets
        (aif2 (and (~isa socket 'stream-server-usocket)
                   (is socket (peek-char nil (socket-stream socket) nil socket)))
                (do (send-hu socket->game*.socket!players "Some player quit.~%")
                    (send-ai socket->game*.socket!players "~A~%" disconnected-code*)
                    (disconnect socket->game*.socket)
                    ;; We need to go through the loop again since we may
                    ;; have disconnected some of the other ready sockets.
                    (return))
              (cont socket)
                (call it socket)
              (temp-cont socket)
                (restart-case (call it socket)
                  (restart-turn (&rest args)
                    :report "Restart the continuation for the socket."
                    (when args
                      (apply #'format socket!socket-stream args)
                      (force-output socket!socket-stream))
                    (= (temp-cont socket) it))
                  (disconnect ()
                    :report "Disconnect the current game."
                    (send-hu socket->game*.socket!players "An unknown error occured.~%")
                    (send-ai socket->game*.socket!players "~A~%" unknown-error-code*)
                    (disconnect socket->game*.socket)
                    (return)))
              :else
                (restart-case (error "No continuation for socket ~A." socket)
                  (ignore-input ()
                    :report "Ignore all of the new information from the socket."
                    (while (listen socket!socket-stream)
                      (read-line :from socket!socket-stream)))))))))

(def disconnect (game)
  "Disconnect a game. Sends INFO to all of the human players and CODE
   to all of the AI players."
  (zap #'set-difference sockets* game->sockets*.game)
  (each socket game->sockets*.game
    (remhash socket socket->game*)
    (rem-cont socket)
    (rem-temp-cont socket)
    (socket-close socket))
  (remhash game game->sockets*)
  (when log-file*
    (w/file (*standard-output* log-file*
             :direction :output
             :if-exists :append
             :if-does-not-exist :create)
      (mapc #'pr (reverse game!game-log)))))

(defcont add-player (arr type) (listener)
  "Wait for all of the players to connect."
  (withs (socket (socket-accept listener)
          player (inst type :socket socket)
          game arr!aref)
    (push socket game->sockets*.game)
    (push socket sockets*)
    (= socket->game*.socket game)
    (push player game!players)
    (when (is game!players!len game!need)
      (start-game game)
      ;; Have the continuation add players to a new game instead of
      ;; the current one. It is safe to use type-of as it will always
      ;; return the class-name of a class with a proper name.
      (= arr!aref (inst (type-of game))))))
