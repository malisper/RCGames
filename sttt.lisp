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

(deftem (super-tic-tac-toe (:conc-name nil) (:include game))
  (need 2)
  current
  (board (make-array (n-of 4 dims*) :initial-element nil))
  (metaboard (make-array (n-of 2 dims*) :initial-element nil)))

(defmethod print-object ((game super-tic-tac-toe) stream)
  (let *standard-output* stream
    ;; orow -> outer row, icol -> inner column
    (up orow 0 dims*
      (up irow 0 dims*
        (up ocol 0 dims*
          (up icol 0 dims*
            (prf "~:[ ~;~:*~A~]" game!board.orow.ocol.irow.icol)
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

(defmethod start-game ((game super-tic-tac-toe))
  (= game!current (car game!players))
  (= (temp-cont game!current!socket) (play-turn game))
  (send-log game "STTT ~A~%" (len game!players))
  (send :hu game!players "~A" game ())
  
  (send :hu game!current "It is your turn.~%")
  (send :hu game!next    "It is your opponent's turn.~%")

  (send :ai game!current "1~%")
  (send :ai game!next    "2~%"))

(def next (game)
  "Returns the next player in the game."
  (aif (cadr+mem game!current game!players)
    it
    (car game!players)))

(defcont play-turn (game ? orow ocol) (socket)
  (declare (ignore socket))
  (mvb (orow ocol irow icol) (read-input game game!current orow ocol)
    (= game!board.orow.ocol.irow.icol (if (is game!current game!players!car) 'x 'o))
    (send-log game "~A:~A ~A ~A ~A~%" (inc+pos game!current game!players) orow ocol irow icol)
    (send :hu game!players "~A" game ())
    (if (winner game)
      (do (announce-winner game)
          (disconnect game))
      (do (= game!current (next game))
          (send :ai game!current "~A ~A ~A ~A~%" orow ocol irow icol)
          (send :hu game!current "Your turn.~%Your move on board ~A ~A.~%" irow icol)
          (= (temp-cont game!current!socket) (play-turn game irow icol))))))

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

(def winner (game)
  "Is there a winner for the entire game?"
  (up r 0 dims*
    (up c 0 dims*
      (or= game!metaboard.r.c (cell-winner game!board.r.c))))
  (or (iter (for r from 0 below dims*)
            (thereis (iter (for c from 0 below dims*)
                           (always (and (is game!metaboard.r.c game!metaboard.r.0) game!metaboard.r.0)))))
      (iter (for c from 0 below dims*)
            (thereis (iter (for r from 0 below dims*)
                           (always (and (is game!metaboard.r.c game!metaboard.0.c) game!metaboard.0.c)))))
      (and (iter (for i from 0 below dims*)
                 (always (and (is game!metaboard.i.i game!metaboard.0.0))))
           game!metaboard.0.0)
      (and (iter (for r from 0 below dims*)
                 (for c from (dec dims*) downto 0)
                 (always (and (is game!metaboard.r.c (get game!metaboard.0 (dec dims*)))
                              game!metaboard.r.c)))
           (get game!metaboard.0 (dec dims*)))
      (and (iter out
                 (for orow from 0 below dims*)
                 (iter (for ocol from 0 below dims*)
                       (iter (for irow from 0 below dims*)
                             (iter (for icol from 0 below dims*)
                                   (in out (always game!metaboard.orow.ocol.irow.icol))))))
           'tie)))

(def announce-winner (game)
  "Announce the winner of the game."
  (if (is (winner game) 'tie)
      (do (send :hu game!players "It was a tie.~%")
          (send :ai game!players "0~%")
          (send-log game!players "0~%"))
      (do (send :hu game!players "~:[Player 2~;Player 1~] won!~%" (is (winner game) 'x))
          (send :ai game!players "~:[1~;2~]~%" (is (winner game) 'o))
          (send-log game!players "~:[1~;2~]~%" (is (winner game) 'o)))))

(defmethod read-input ((game super-tic-tac-toe) player &rest args)
  "Read the input for a tic-tac-toe game"
  (let (orow ocol) args
    (let line (read-line :from player!socket!socket-stream)
      (if (and orow (notevery #'idfn (linearlize game!board.orow.ocol)))
          (mvb (match strings) (scan-to-strings "^(\\d*) (\\d)*\\s*$" line)
            (unless match
              (error 'invalid-move
                     :format-control "'~A' is malformed input."
                     :format-arguments (list )))
            (let (irow icol) (map #'parse-integer strings)
              #1=(do (unless (<= 0 irow (dec dims*))
                       (error 'invalid-move
                              :format-control "The inner row index ~A is illegal."
                              :format-arguments (list irow)))
                     (unless (<= 0 icol (dec dims*))
                       (error 'invalid-move
                              :format-control "The inner column index ~A is illegal."
                              :format-arguments (list icol)))
                     (when game!board.orow.ocol.irow.icol
                       (error 'invalid-move :format-control "The square is already taken."))
                     (values orow ocol irow icol))))
          ;; Code to run if we need to pick up the outer row and outer column.
          (mvb (match strings) (scan-to-strings "^(\\d*) (\\d*) (\\d*) (\\d*)\\s*$" line)
            (unless match
              (error 'invalid-move
                     :format-control "~S is malformed input."
                     :format-arguments (list (keep [isa _ 'standard-char] line))))
            (let (orow ocol irow icol) (map #'parse-integer strings)
              (unless (<= 0 orow (dec dims*))
                (error 'invalid-move
                       :format-control "The outer row ~A is illegal."
                       :format-arguments (list orow)))
              (unless (<= 0 ocol (dec dims*))
                (error 'invalid-move
                       :format-control "The outer column ~A is illegal."
                       :format-arguments (list ocol)))
              #1#))))))
