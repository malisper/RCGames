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
  (= (piece+next) 'o)
  (= player*!cont (play-turn))
  (send :log nil "TTT ~A~%" (len game*!players))
  (send :all player* "1~%")
  (send :all (next)  "2~%"))

(defcont play-turn ()
  "Performs a turn for the current player."
  (mvb (r c) (read-move)
    (= game*!board.r.c player*!piece)
    (send :log nil "~A: ~A ~A~%" (inc+pos player* game*!players) r c)
    (send :all (next) "~A ~A~%" r c)
    (if (winner)
      (do (announce-winner)
          (disconnect))
      (= (cont+next)
         (play-turn)))))

(def next ()
  "Returns the next player in the game."
  (aif (cadr+mem player* game*!players)
    it
    (car game*!players)))

(def winner ()
  "Is there a winner of this game*? If so return the piece of that player."
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

(def announce-winner ()
  "Announce the winner of the game*."
  (if (is (winner) 'tie)
      (send '(:all :log) "0~%")
      (send '(:all :log) game*!players "~:[1~;2~]~%" (is (winner) 'o))))

(defread tic-tac-toe nil
  (let line (read-line :from player*!socket-stream)
    (mvb (match strings) (scan-to-strings "^(\\d*) (\\d*)\\s*$" line)
      (unless match
        (signal-invalid-move "~S is malformed input." (list (keep [isa _ 'standard-char] line))))
      (let (r c) (map #'parse-integer strings)
           (unless (<= 0 r (dec dims*))
             (signal-invalid-move "The row index ~A is illegal." (list r)))
           (unless (<= 0 c (dec dims*))
             (signal-invalid-move  "The column index ~A is illegal." (list c)))
           (when game*!board.r.c
             (signal-invalid-move "The square is already taken."))
           (values r c)))))

