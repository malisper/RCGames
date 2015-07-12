;;;; This is the code for playing regular tic-tac-toe.
(defpackage :tic-tac-toe
  (:nicknames :ttt)
  (:use :clamp :experimental :iter :usocket :ppcre :server)
  (:shadowing-import-from :experimental
     :repeat :def :fn :defmemo :mac :while
     :until :in :coerce :with :summing :defmethod)
  (:shadowing-import-from :ppcre :split)
  (:shadow :next)
  (:export :tic-tac-toe))

(in-package :ttt)
(syntax:use-syntax :clamp)

(defparameter dims* 3 "The side length of the tic-tac-toe board.")

;; We only support one format for input and output in tic-tac-toe so
;; there are no additional flags added.
(deftem (tic-tac-toe (:conc-name nil) (:include game))
  (need 2)
  (board (make-array (list dims* dims*) :initial-element nil))
  (player-type 'ttt-player))

(deftem (ttt-player (:conc-name nil) (:include player))
  piece)

(defmethod print-object ((game tic-tac-toe) stream)
  (let *standard-output* stream
    (up r 0 dims*
      (up c 0 dims*
        (prf "~:[ ~;~:*~A~]" game!board.r.c)
        (unless (is c (dec dims*))
          (pr "|")))
      (prn)
      (unless (is r (dec dims*))
        (prn (make-string (+ dims* dims* -1) :initial-element #\-))))))

(defstart tic-tac-toe
  (= player*!piece 'x)
  (= player*!num 1)
  (= (piece+next) 'o)
  (= (num+next) 2)
  (= player*!cont (play-turn))
  
  (send :log nil "TTT ~A~%" (len game*!players))
  (each player game*!players
    (send :all player "~A~%" player!num)))

(defcont play-turn ()
  "Performs a turn for the current player."
  (let move (read-move)
    (validate move)
    (perform-move move)
    (send-move move)
    (if (winner)
      (do (announce-winner)
          (disconnect))
      (= (cont+next) (play-turn)))))

(def next ()
  "Returns the next player in the game."
  (aif (cadr+mem player* game*!players)
    it
    (car game*!players)))

(def winning-piece ()
  "If there is a winner of this game, return their piece."
  ;; By using 'and' in the way below, the return value will be the
  ;; winning player.
  (or (iter (for r from 0 below dims*)
            (thereis (iter (for c from 0 below dims*)
                           (always (and (is game*!board.r.c game*!board.r.0) game*!board.r.0)))))
      (iter (for c from 0 below dims*)
            (thereis (iter (for r from 0 below dims*)
                           (always (and (is game*!board.r.c game*!board.0.c) game*!board.0.c)))))
      (and (iter (for i from 0 below dims*)
                 (always (and (is game*!board.i.i game*!board.0.0))))
           game*!board.0.0)
      (and (iter (for r from 0 below dims*)
                 (for c from (dec dims*) downto 0)
                 (always (and (is game*!board.r.c (get game*!board.0 (dec dims*)))
                              game*!board.r.c)))
           (get game*!board.0 (dec dims*)))
      (and (iter out
                 (for r from 0 below dims*)
                 (iter (for c from 0 below dims*)
                       (in out (always game*!board.r.c))))
           'tie)))

(def winner ()
  "Is there a winner of this game*? If so return the piece of that
   player. Returns the symbol 'tie' if there is a tie."
  (awhen (winning-piece)
    (if (is it 'tie)
        it
        (find it game*!players :key #'piece))))

(def announce-winner ()
  "Announce the winner of the game*."
  (let winner (winner)
    (if (is winner 'tie)
        (send '(:all :log) game*!players "0~%")
        (send '(:all :log) game*!players "~A~%" winner!num))))

(defread tic-tac-toe nil
  (let line (read-line :from player*!socket-stream)
    (mvb (match strings) (scan-to-strings "^(\\d*) (\\d*)\\s*$" line)
      (unless match
        (signal-malformed-input "~S is malformed input. The format should be 'ROW COL'." (list (keep [isa _ 'standard-char] line))))
      (map #'parse-integer strings))))

(def validate (move)
  "Checks that the move is valid. If not, signals an error."
  (let (r c) move
    (unless (<= 0 r (dec dims*))
      (signal-invalid-move "The row ~A is illegal, should be between 0 and ~A." r (dec dims*)))
    (unless (<= 0 c (dec dims*))
      (signal-invalid-move "The column ~A is illegal, should be between 0 and ~A." c (dec dims*)))
    (when game*!board.r.c
      (signal-invalid-move "The square (~A,~A) is already taken." r c))))

(def send-move (move)
  "Send the move to all of the other players."
  (send :all (rem player* game*!players) "~{~A ~A~}~%" move))

(def perform-move (move)
  "Actually perform the move on the board. This is the only function
   allowed to mutate the board."
  (let (r c) move
    (= game*!board.r.c player*!piece)))
