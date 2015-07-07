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

;; We only support one way of input and output so there are now
;; additional flags added..
(deftem (tic-tac-toe (:conc-name nil) (:include game))
  (need 2)
  current
  (board (make-array (list dims* dims*) :initial-element nil)))

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

(defmethod start-game ((game tic-tac-toe))
  "Initialize the actual game."
  (= game!current (car game!players))
  (set-cont socket* (play-turn))
  (send :log nil "TTT ~A~%" (len game!players))
  (send :all game!current "1~%")
  (send :all (next)    "2~%"))

(defcont play-turn ()
  "Performs a turn for the current player."
  (mvb (r c) (read-input game* game*!current)
    (= game*!board.r.c (if (is game*!current game*!players!car) 'x 'o))
    (send :log nil "~A: ~A ~A~%" (inc+pos game*!current game*!players) r c)
    (send :all (next) "~A ~A~%" r c)
    (if (winner game*)
      (do (announce-winner)
          (disconnect))
      (do (= game*!current (next))
          (set-cont game*!current!socket (play-turn))))))

(def next ()
  "Returns the next player in the game."
  (aif (cadr+mem game*!current game*!players)
    it
    (car game*!players)))

(def winner (game)
  "Is there a winner of this game? If so return the piece of that player."
  ;; By using and in the way below, thereturn value will be the
  ;; winning player.
  (or (iter (for r from 0 below dims*)
            (thereis (iter (for c from 0 below dims*)
                           (always (and (is game!board.r.c game!board.r.0) game!board.r.0)))))
      (iter (for c from 0 below dims*)
            (thereis (iter (for r from 0 below dims*)
                           (always (and (is game!board.r.c game!board.0.c) game!board.0.c)))))
      (and (iter (for i from 0 below dims*)
                 (always (and (is game!board.i.i game!board.0.0))))
           game!board.0.0)
      (and (iter (for r from 0 below dims*)
                 (for c from (dec dims*) downto 0)
                 (always (and (is game!board.r.c (get game!board.0 (dec dims*)))
                              game!board.r.c)))
           (get game!board.0 (dec dims*)))
      (and (iter out
                 (for r from 0 below dims*)
                 (iter (for c from 0 below dims*)
                       (in out (always game!board.r.c))))
           'tie)))

(def announce-winner ()
  "Announce the winner of the game*."
  (if (is (winner game*) 'tie)
      (send '(:all :log) "0~%")
      (send '(:all :log) game*!players "~:[1~;2~]~%" (is (winner game*) 'o))))

(defmethod read-input ((game tic-tac-toe) flag &rest args)
  "Read the input for a tic-tac-toe game"
  (declare (ignore flag args))
  (withs (player game!current
          line (read-line :from player!socket!socket-stream))
    (mvb (match strings) (scan-to-strings "^(\\d*) (\\d*)\\s*$" line)
      (unless match
        (error 'invalid-move
               :format-control "~S is malformed input."
               :format-arguments (list (keep [isa _ 'standard-char] line))))
      (let (r c) (map #'parse-integer strings)
        (unless (<= 0 r (dec dims*))
          (error 'invalid-move
                 :format-control "The row index ~A is illegal."
                 :format-arguments (list r)))
        (unless (<= 0 c (dec dims*))
          (error 'invalid-move
                 :format-control "The column index ~A is illegal."
                 :format-arguments (list c)))
        (when game!board.r.c
          (error 'invalid-move :format-control "The square is already taken."))
        (values r c)))))

