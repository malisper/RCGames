;;;; This is the code for the main server.
(in-package :server)
(syntax:use-syntax :clamp)

(defparameter log-file* nil
  "The file to log all of the game information to.")

(defparameter show-output* t
  "If T, any data sent to the log will be printed to *standard-output*.")

(defparameter sockets* '()
  "A list of all of the sockets we need to listen for.")

(def start-server (games)
  "GAMES should be a list of duples Each one containing a game-type,
   and a port for players to connect to."
  (let game-sockets (mapeach (g p) games
                      (let socket (socket-listen *wildcard-host* p :reuse-address t)
                        (change-class socket 'cont-server)
                        (push socket sockets*)
                        (list g socket)))
    (unwind-protect (do (each (game-type socket) game-sockets
                          ;; Using a zero dimensional array as a pointer
                          ;; so that both add-player closures always
                          ;; refer to the same game.
                          (let arr (make-array '())
                            (= arr!aref (inst game-type))
                            (= socket!cont (add-player arr))))
                        (listening-loop))
      (each socket sockets*
        (when (player-p socket)
          (send :all socket "~A~%" unknown-error-code*))
        (socket-close socket))
      (= sockets* nil))))

(def listening-loop ()
  "The main loop for listening."
  (while sockets*
    (let ready (wait-for-input sockets* :ready-only t)
      (each socket ready
        (aif2 (and (isa socket 'player)
                   (is socket (peek-char nil (socket-stream socket) nil socket)))
              (do (send :all socket!game!players "~A~%" disconnected-code*)
                  (disconnect socket!game)
                  ;; We need to go through the loop again since we may
                  ;; have disconnected some of the other ready sockets.
                  (return))
              
              ;; I may want to change this to handler-case, but right
              ;; now it seems reasonable that other places might want
              ;; to bind different handlers and maybe use the
              ;; disconnect restart.
              (restart-case (handler-bind ((game-error #'disconnect-handler))
                              (call-cont socket))
                (disconnect (c)
                  :report "Disconnect the current game."
                  (send :all socket "~A~%" c!code)
                  (send :all (rem socket socket!game!players) "~A~%" disconnected-code*)
                  (disconnect socket!game)
                  ;; Same as above with going through the loop again.
                  (return))))))))

(def disconnect (? (game game*))
  "Disconnect a game. Sends INFO to all of the human players and CODE
   to all of the AI players."
  (mapc #'socket-close game!players)
  (zap #'set-difference sockets* game!players)
  (when log-file*
    (w/file (*standard-output* log-file*
             :direction :output
             :if-exists :append
             :if-does-not-exist :create)
      (mapc #'pr (rev game!game-log)))))

(defcont add-player (arr)
  "Connect a given player."
  (let player player*!socket-accept
    (change-class player arr!aref!player-type)
    (push player sockets*)
    (= player!cont (read-flags arr))))

(defcont read-flags (arr)
  "Reads the flags from the player."
  ;; The dynamic value of game* needs to be seen by parse-flags.
  (withs (game* arr!aref
          flags (parse-flags))
    (if (is flags :invalid)
        (do (format player*!socket-stream "~A~%" invalid-flag-code*)
            (force-output player*!socket-stream)
            (= player*!cont (read-flags arr)))
        (do (= player*!flags flags)
            (= player*!game arr!aref)
            (push player* game*!players)
            (when (is game*!players!len game*!need)
              ;; Have the continuation add players to a new game instead of
              ;; the current one. It is safe to use type-of as it will always
              ;; return the class-name of a class with a proper name.
              (= arr!aref (inst (type-of game*)))
              (start-game game*))))))

(def parse-flags ()
  "Returns the flags the socket wants to use. If they aren't valid
   returns :INVALID."
  (ado (read-line :from player*!socket-stream)
       (tokens it)
       (map #'upcase it)
       (if (~valid it)
           :invalid
           (map [intern _ :keyword] it))))

(def valid (flags)
  "Are these flags valid?"
  (subsetp flags game*!flags :test #'string-equal))
