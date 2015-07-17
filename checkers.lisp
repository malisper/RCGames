(defpackage :checkers
  (:use :clamp :experimental :iter :usocket :ppcre :server)
  (:shadowing-import-from :experimental
     :repeat :def :fn :defmemo :mac :while
     :until :in :coerce :with :summing :defmethod)
  (:shadowing-import-from :ppcre :split)
  (:shadow :server :next)
  (:export :checkers))

(in-package :checkers)
(syntax:use-syntax :clamp)

(defparameter dims* 8 "The size of the board.")

(deftem (checkers (:conc-name nil) (:include board-game))
  (need 2)
  (flags '(:traditional :algebraic))
  (board (make-array (list dims* dims*) :initial-element nil)))

(defstruct (point (:constructor make-point (row col)) (:type list))
  row col)

(deftem (piece (:conc-name nil))
  direction player king)

(defparameter dirs* '(up down left right) "A list of all of the directions.")
(defparameter dirs->vects* '((up (-1 0)) (down (1 0)) (right (0 1)) (left (0 -1)))
  "An alist mapping from directions to vectors.")

(def dir->vect (dir)
  "Converts a direction to a vector."
  (alref dirs->vects* dir))

(defmethod print-object ((game checkers) *standard-output*)
  (repeat (inc+* dims* 2) (pr "-"))
  (prn)
  (up r 0 dims*
    (pr "|")
    (up c 0 dims*
      (pr (aif game.r.c it!player!num " "))
      (pr "|"))
    (prn)
    (repeat (inc+* dims* 2) (pr "-"))
    (prn)))

(def p+ (x y)
  "Adds two points together."
  (map #'+ x y))

(def next-point (point dir)
  "Gets the next point in a given direction."
  (p+ point (dir->vect dir)))

(defstart checkers
  (= game*!board (init-checkers-board))
  (= player*!cont (play-turn)))

(def init-checkers-board ()
  "Return a fresh checkers board."
  (ret board (make-array (list dims* dims*) :initial-element nil)
    (up r 0 3
      (iter (for c from (if (even r) 1 0) below dims* by 2)
            (= board.r.c (make-piece :player game*!players.0 :direction 'down))))
    (down r dims* (- dims* 3)
      (iter (for c from (if (even r) 1 0) below dims* by 2)
            (= board.r.c (make-piece :player game*!players.1 :direction 'up))))))

(defcont play-turn ()
  (let move (read-move '(:algebraic :traditional))
    (mvb (path pieces-to-remove) (validate move)
      (perform-move (car path) (last path) pieces-to-remove)
      (send-move path)
      (if (winner)
          (do (announce-winner)
              (disconnect))
          (= (cont+next) (play-turn))))))

(def next ()
  "Returns the next player in the game."
  (aif (cadr+mem player* game*!players)
    it
    (car game*!players)))

(defread checkers :traditional
  (let pieces (tokens (read-line :from player*!socket-stream)
                      (orf #'whitec (testify #\-)))
    (unless (every [scan "\\d*" _] pieces)
      (signal-malformed-input "One of the locations isn't a number."))
    (map traditional->point+parse-integer pieces)))

(def traditional->point (n)
  "Converts a traditional number to a point."
  (withs (row (floor (dec n) 4)
          col (+ (* 2 (mod (dec n) 4)) (if (evenp row) 1 0)))
    (list row col)))

(def point->traditional (p)
  "Converts the point to traditional format."
  ;; I'm really lazy.
  (or (iter (for i from 1 to 32)
            (finding i such-that (iso (traditional->point i) p)))
      (error "Invalid point")))

(defread checkers :algebraic
  (let pieces (tokens (read-line :from player*!socket-stream)
                      (orf #'whitec (testify #\-)))
    (unless (every [scan "[a-zA-Z]\\d" _] pieces)
      (signal-malformed-input "One of the squares doesn't match the format."))
    (map #'algebraic->point pieces)))

(def algebraic->point (a)
  "Figures out the point referenced from algebraic notation."
  (with (letter (upcase (char a 0))
         digit (digit (char a 1)))
    (list (- 8 digit)
          (- (char-code letter) (char-code #\A)))))

(def point->algebraic (p)
  "Figures out the algebraic notation that corresponds to the move."
  (let (r c) p
    (format nil "~A~A" (code-char (+ c (char-code #\A))) (- 8 r))))

(def send-move (move)
  "Sends the move to all of the players."
  (let other-players (rem player* game*!players)
    (send :algebraic other-players "~{~A~^-~}~%" (map #'point->algebraic move))
    (send '(:traditional :logp) other-players "~{~A~^-~}~%"
          (map #'point->traditional move))))

(def perform-move (start end pieces-to-remove)
  "Performs a move. The move should be a triple containing the
   starting square, the ending square, and the pieces to remove along the
   way."
  (= game*!board (make-array (array-dimensions game*!board)
                             :displaced-to (mapv [if (mem _ pieces-to-remove) nil _] 
                                                 (linearlize game*!board)))
     game*.end game*.start
     game*.start nil)
  (when (or (and (is game*.end!player!num 1) (caris end 7))
            (and (is game*.end!player!num 2) (caris end 0)))
    (set game*.end!king)))

(def valid (p)
  "Is this point a valid location?"
  (every [<= 0 _ (dec dims*)] p))

(def winner ()
  "Is there a winner? If so this function will return that player."
  (if (~find 1 (linearlize game*!board) :key [aand _ (num+player it)])
        game*!players.1
      (~find 2 (linearlize game*!board) :key [aand _ (num+player it)])
        game*!players.0))

(def announce-winner ()
  "Announce the winner of the game*. If it is a tie, send 0 to all of
   the players and the log. Otherwise send the player number."
  (let winner (winner)
    (if (is winner 'tie)
        (send '(:all :log) game*!players "0~%")
        (send '(:all :log) game*!players "~A~%" winner!num))))

(def dirs (piece)
  "What directions can this piece move in?"
  (cart (fn (x y) (p+ (dir->vect x)
                      (dir->vect y)))
        (if piece!king '(up down) (list piece!direction))
        '(left right)))

(def validate (move)
  "Returns two values, the path taken for the move and the pieces
   captured along the way."
  (let vals (find move (all-moves) :key #'car :test #'iso)
    (unless vals
      (signal-invalid-move "Invalid move."))
    (values-list vals)))

(def all-moves ()
  "Returns all of the moves the current player can make. Moves are
   represented as a list containing the path and the pieces captured
   on that path."
  (or (iter (for loc from 1 to 32)
            (for point = (traditional->point loc))
            (for piece = game*.point)
            (when (and piece (is piece!player player*))
              (appending (jumps piece point))))
      (iter (for loc from 1 to 32)
            (for point = (traditional->point loc))
            (for piece = game*.point)
            (when (and piece (is piece!player player*))
              (appending (single-move piece point))))))

(def single-move (piece loc)
  "Returns all of the locations we can get to by a single move."
  (accum a
    (each dir (dirs piece)
      (let next (p+ loc dir)
        (when (and (valid next) (not game*.next))
          (a (list (list loc next) '())))))))

(def jumps (piece loc ? (path (list loc)) (prev '()))
  "Returns all of the possible paths and the pieces that are captured
   along the way when jumping."
  (accum a
    (each dir (dirs piece)
      (let next (p+ loc dir)
        (when (valid next)
          (whenlet other game*.next
            (when (and (~is other!player player*) (~mem other prev))
              (let next-next (p+ next dir)
                (when (and (valid next-next) (not game*.next-next))
                  (let jumps (jumps piece next-next (cons next-next path) (cons other prev))
                    (if jumps
                        (map #'a jumps)
                        (a (list (rev (cons next-next path))
                                 (cons other prev))))))))))))))
