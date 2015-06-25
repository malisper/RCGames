;;;; This is the code for playing regular tic-tac-toe.
(in-package :server)
(syntax:use-syntax :clamp)

(deftem (tic-tac-toe (:conc-name nil) (:include game))
  (need 2)
  current
  (board (make-array '(3 3) :initial-element nil)))

(defmethod start-game ((game tic-tac-toe))
  "Initialize the actual game."
  (= game!current (car game!players))
  (= (temp-cont game!current!socket) (play-ttt-turn game))
  (push game!current!socket sockets*)
  (send-hu game!players "~A" game ())
  
  (send-hu game!current "It is your turn.~%")
  (send-hu game!next    "It is your opponents turn.~%")
  
  (send-ai game!current "0~%")
  (send-ai game!next    "1~%"))

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

(defcont play-ttt-turn (game) (socket)
  "Performs a turn for the current player."
  (declare (ignore socket))
  (mvb (r c) (read-input game game!current)
    (= game!board.r.c (if (is game!current game!players!car) 'x 'o))
    (aif (winner game)
      (do (announce-winner game)
          (disconnect game))
      (do (send-hu game!players "~A" game ())
          ;; We only want to send the move if the game is still going
          ;; on. If the game is over, there is no reason to send the
          ;; opponents move.
          (send-ai game!next "~A ~A~%" r c)
          (= game!current (next game))
          (send-hu game!current "Your turn.~%" ())
          (push game!current!socket sockets*)
          (= (temp-cont game!current!socket) (play-ttt-turn game))))))

