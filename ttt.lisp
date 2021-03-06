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
;; we do not need to override the flags slot.
(deftem (tic-tac-toe (:conc-name nil) (:include board-game))
  (need 2)
  (board (make-array (list dims* dims*) :initial-element nil))
  (player-type 'ttt-player))

(deftem (ttt-player (:conc-name nil) (:include player))
  piece)

(defmethod print-object ((game tic-tac-toe) stream)
  (let *standard-output* stream
    (up r 0 dims*
      (up c 0 dims*
        (prf "~A" (aif game.r.c it!piece " "))
        (unless (is c (dec dims*))
          (pr "|")))
      (prn)
      (unless (is r (dec dims*))
        (prn (make-string (+ dims* dims* -1) :initial-element #\-))))))

(defstart tic-tac-toe
  (= player*!piece 'x)
  (= (piece+next) 'o)
  (= player*!cont (play-turn)))

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

(defread tic-tac-toe nil
  (let line (read-line :from player*!socket-stream)
    (mvb (match strings) (scan-to-strings "^(\\d*) (\\d*)\\s*$" line)
      (unless match
        (signal-malformed-input "'~A' is malformed input." (list (keep [isa _ 'standard-char] line))))
      (map #'parse-integer strings))))

(def validate (move)
  "Checks that the move is valid. If not, signals an error."
  (let (r c) move
    (unless (<= 0 r (dec dims*))
      (signal-invalid-move "The row ~A is illegal, should be between 0 and ~A." r (dec dims*)))
    (unless (<= 0 c (dec dims*))
      (signal-invalid-move "The column ~A is illegal, should be between 0 and ~A." c (dec dims*)))
    (when game*.r.c
      (signal-invalid-move "The square (~A,~A) is already taken." r c))))

(def perform-move (move)
  "Actually perform the move on the board. This is the only function
   allowed to mutate the board."
  (let (r c) move
    (= game*.r.c player*)))

(def send-move (move)
  "Send the move to all of the other players."
  (send '(:all :logp) (rem player* game*!players) "~{~A ~A~}~%" move))

(def next ()
  "Returns the next player in the game."
  (aif (cadr+mem player* game*!players)
    it
    (car game*!players)))

(def winner ()
  "If there is a winner of this game, return that player. Returns the
   symbol 'tie' if it is a tie."
  ;; By using 'and' in the way below, the return value will be the
  ;; winning player.
  (or (iter (for r from 0 below dims*)
            (thereis (iter (for c from 0 below dims*)
                           (always (and (is game*.r.c game*.r.0) game*.r.0)))))
      (iter (for c from 0 below dims*)
            (thereis (iter (for r from 0 below dims*)
                           (always (and (is game*.r.c game*.0.c) game*.0.c)))))
      (and (iter (for i from 0 below dims*)
                 (always (and (is game*.i.i game*.0.0))))
           game*.0.0)
      (and (iter (for r from 0 below dims*)
                 (for c from (dec dims*) downto 0)
                 (always (and (is game*.r.c (get game*.0 (dec dims*)))
                              game*.r.c)))
           (get game*.0 (dec dims*)))
      (and (iter out
                 (for r from 0 below dims*)
                 (iter (for c from 0 below dims*)
                       (in out (always game*.r.c))))
           'tie)))

(def announce-winner ()
  "Announce the winner of the game*. If it is a tie, send 0 to all of
   the players and the log. Otherwise send the player number."
  (let winner (winner)
    (if (is winner 'tie)
        (send '(:all :log) game*!players "0~%")
        (send '(:all :log) game*!players "~A~%" winner!num))))
