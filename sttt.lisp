;;;; This is for playing super tic-tac-toe.
(defpackage :super-tic-tac-toe
  (:nicknames :sttt)
  (:use :clamp :experimental :iter :usocket :ppcre :server)
  (:shadowing-import-from :experimental
     :repeat :def :fn :defmemo :mac :while
     :until :in :coerce :with :summing :defmethod)
  (:shadowing-import-from :ppcre :split)
  (:shadow :server :next)
  (:export :super-tic-tac-toe))

(in-package :sttt)
(syntax:use-syntax :clamp)

(defparameter dims* 3 "The side length of the super tic-tac-toe board.")

(deftem (super-tic-tac-toe (:conc-name nil) (:include board-game))
  (need 2)
  (board (make-array (n-of 4 dims*) :initial-element nil))
  (metaboard (make-array (n-of 2 dims*) :initial-element nil))
  (player-type 'sttt-player)
  (flags '(:always-four :maybe-four)))

(deftem (sttt-player (:conc-name nil) (:include player))
  piece)

(deftem (move (:conc-name nil))
  orow ocol irow icol)

(defmethod print-object ((game super-tic-tac-toe) stream)
  (let *standard-output* stream
    ;; orow -> outer row, icol -> inner column
    (up orow 0 dims*
      (up irow 0 dims*
        (up ocol 0 dims*
          (up icol 0 dims*
            (prf "~A" (aif game.orow.ocol.irow.icol it " "))
            (unless (is icol (dec dims*))
              (pr "|")))
          (unless (is ocol (dec dims*))
            (pr "||")))
        (prn)
        (unless (is irow (dec dims*))
          (up ocol 0 dims*
            (pr (make-string (+ dims* dims* -1) :initial-element #\-))
            (unless (is ocol (dec dims*))
              (pr "||")))
          (prn)))
      (unless (is orow (dec dims*))
        (repeat 2
          (up ocol 0 dims*
            (pr (make-string (+ dims* dims* -1) :initial-element #\-))
            (unless (is ocol (dec dims*))
              (pr "**")))
          (prn))))))

(defstart super-tic-tac-toe
  (= player*!piece 'x)
  (= (piece+next) 'o)
  (= player*!cont (play-turn)))

(def next ()
  "Returns the next player in the game."
  (aif (cadr+mem player* game*!players)
    it
    (car game*!players)))

(defcont play-turn (? prev-move)
  "A continuation for taking the move the player sends in and
   performing it."
  (let move (read-move)
    (validate move prev-move)
    (perform-move move)
    (send-move move prev-move)
    (if (winner)
      (do (announce-winner)
          (disconnect))
      (= (cont+next) (play-turn move)))))

;; Since we can represent both possible input formats for sttt with a
;; single regex, there is no reason to have multiple read functions.
(defread super-tic-tac-toe nil
  (let line (read-line :from player*!socket-stream)
    (mvb (match strings) (scan-to-strings "^(?:(\\d*) (\\d*) )?(\\d*) (\\d*)\\s*$" line)
      (unless match
        (signal-malformed-input "'~A' is malformed input." (list (keep [isa _ 'standard-char] line))))
      (let (orow ocol irow icol) (map (fif #'stringp #'parse-integer) strings)
        (make-move :orow orow :ocol ocol :irow irow :icol icol)))))

(def cell-winner (board)
  "Is there a winner for an individual cell."
  (or (iter (for r from 0 below dims*)
            (thereis (iter (for c from 0 below dims*)
                           (always (and (is board.r.c board.r.0) board.r.0)))))
      (iter (for c from 0 below dims*)
            (thereis (iter (for r from 0 below dims*)
                           (always (and (is board.r.c board.0.c) board.0.c)))))
      (and (iter (for i from 0 below dims*)
                 (always (and (is board.i.i board.0.0))))
           board.0.0)
      (and (iter (for r from 0 below dims*)
                 (for c from (dec dims*) downto 0)
                 (always (and (is board.r.c (get board.0 (dec dims*)))
                              board.r.c)))
           (get board.0 (dec dims*)))))

(def winner ()
  "Is there a winner for the entire game?"
  (up r 0 dims*
    (up c 0 dims*
      (or= game*!metaboard.r.c (cell-winner game*.r.c))))
  (or (iter (for r from 0 below dims*)
            (thereis (iter (for c from 0 below dims*)
                           (always (and (is game*!metaboard.r.c game*!metaboard.r.0) game*!metaboard.r.0)))))
      (iter (for c from 0 below dims*)
            (thereis (iter (for r from 0 below dims*)
                           (always (and (is game*!metaboard.r.c game*!metaboard.0.c) game*!metaboard.0.c)))))
      (and (iter (for i from 0 below dims*)
                 (always (and (is game*!metaboard.i.i game*!metaboard.0.0))))
           game*!metaboard.0.0)
      (and (iter (for r from 0 below dims*)
                 (for c from (dec dims*) downto 0)
                 (always (and (is game*!metaboard.r.c (get game*!metaboard.0 (dec dims*)))
                              game*!metaboard.r.c)))
           (get game*!metaboard.0 (dec dims*)))
      (and (iter out
                 (for orow from 0 below dims*)
                 (iter (for ocol from 0 below dims*)
                       (in out (always game*!metaboard.orow.ocol))))
           'tie)))

(def announce-winner ()
  "Announce the winner of the game*."
  (let winner (winner)
    (if (is winner 'tie)
        (send '(:all :log) game*!players "0~%")
        (send '(:all :log) game*!players "~A~%" winner!num))))

(def validate (move prev-move)
  "Checks that MOVE is a valid move based on information from
   PREV-MOVE. May modify MOVE to add some additional context."
  (with-accessors ((orow orow) (ocol ocol) (irow irow) (icol icol)) move
    (if (no prev-move)
        (unless orow
          (signal-invalid-move "No previous move, you need to specify the outer row ~
                                and outer column."))
        (with-accessors ((pirow irow) (picol icol)) prev-move
          (if orow
              (unless (and (is orow pirow) (is ocol picol))
                (signal-invalid-move "The outer row and outer column do not match the previous ~
                                      inner row and inner column."))
              (= orow pirow
                 ocol picol))))
    (unless (<= 0 orow (dec dims*))
      (signal-invalid-move "The outer row index ~A is illegal." orow))
    (unless (<= 0 ocol (dec dims*))
      (signal-invalid-move "The outer column index ~A is illegal." ocol))
    (unless (<= 0 irow (dec dims*))
      (signal-invalid-move "The inner row index ~A is illegal." irow))
    (unless (<= 0 icol (dec dims*))
      (signal-invalid-move "The inner column index is illegal." icol))
    (when game*!metaboard.orow.ocol
      (signal-invalid-move "A player already won that board."))
    (when game*.orow.ocol.irow.icol
      (signal-invalid-move "A player is already occupying that square."))
    move))

(def perform-move (move)
  "Performs the acutal move."
  (with-accessors ((orow orow) (ocol ocol) (irow irow) (icol icol)) move
    (= game*.orow.ocol.irow.icol player*)))

(def send-move (move prev-move)
  "Sends the move to all of the other players."
  (with-accessors ((orow orow) (ocol ocol) (irow irow) (icol icol)) move
    (send '(:always-four :logp) (rem player* game*!players) "~A ~A ~A ~A~%" orow ocol irow icol)
    (if (and prev-move (and (is prev-move!irow orow) (is prev-move!icol ocol)))
        (send :maybe-four (rem player* game*!players) "~A ~A~%" irow icol)
        (send :maybe-four (rem player* game*!players) "~A ~A ~A ~A~%" orow ocol irow icol))))
