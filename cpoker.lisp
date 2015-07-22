(defpackage :cpoker
  (:use :clamp :experimental :iter :server :usocket :ppcre)
  (:shadowing-import-from :experimental
     :repeat :def :fn :defmemo :mac :while
     :until :in :coerce :with :summing :defmethod)
  (:shadowing-import-from :ppcre :split)
  (:shadow :next)
  (:export :cpoker))

(in-package :cpoker)
(syntax:use-syntax :clamp)

(defstruct (card (:conc-name nil) (:type list) (:constructor make-card (rank suit)))
  rank suit)

(defparameter ranks* '(a k q j 10 9 8 7 6 5 4 3 2)
  "A list of all of the ranks.")

(defparameter suits* '(c d h s)
  "A list of all of the suits.")

(deftem (cpoker (:conc-name nil) (:include game))
  (deck (make-deck))
  (need 3)
  (player-type 'cpoker-player))

(def top ()
  "Pops off the top card of the deck."
  (pop game*!deck))

(deftem (cpoker-player (:conc-name nil) (:include player))
  (hands (n-of 3 nil)))

(defstart cpoker
  (let cards (n-of 5 (top))
    (send '(:all :log) game*!players "~{~{~A~A~}~^ ~}~%" cards)
    (= player*!cont (init-turn cards))))

(defcont init-turn (cards)
  (let moves (read-move :init)
    (unless (is (len moves) (len cards))
      (signal-malformed-input "The wrong number of moves were entered."))
    ;; This entire thing prints one line so we need to add the player
    ;; number to the log.
    (send :logp nil "")
    (iter (for card in cards)
          (for move in moves)
          (validate move)
          (perform-move move card)
          (send '(:all :log) (rem player* game*!players) "~A " move)
          (finally (send '(:all :log) (rem player* game*!players) "~%")))
    (if (is (num+next) 1)
        (let next-card (top)
          (send '(:all :log) game*!players "~{~A~A~}~%" next-card)
          (= (cont+next) (play-turn next-card)))
        (let new-cards (n-of 5 (top))
          (send '(:all :log) game*!players "~{~{~A~A~}~^ ~}~%" new-cards)
          (= (cont+next) (init-turn new-cards))))))

(defcont play-turn (card)
  (let move (read-move :turn)
    (validate move)
    (perform-move move card)
    (send '(:all :logp) (rem player* game*!players) "~A~%" move)
    (if (over)
        (do (announce-winners)
            (disconnect))
        (let next-card (top)
          (send '(:all :log) game*!players "~{~A~A~}~%" next-card)
          (= (cont+next) (play-turn next-card))))))

(def next ()
  "Returns the next player in the game."
  (aif (cadr+mem player* game*!players)
    it
    (car game*!players)))

(defread cpoker :turn
  (let line (read-line :from player*!socket-stream)
    (mvb (match strings) (scan-to-strings "(\\d+)\\s*" line)
      (unless match
        (signal-malformed-input "'~A' is malformed input." (list (keep [isa _ 'standard-char] line))))
      (parse-integer strings.0))))

(defread cpoker :init
  (withs (line (read-line :from player*!socket-stream) tokens (tokens line))
    (unless (every [scan "\\d*" _] tokens)
      (signal-malformed-input "'~A' is malformed input." (list (keep [isa _ 'standard-char] tokens))))
    (map #'parse-integer tokens)))

(def validate (move)
  "Make sure the given move is valid."
  (unless (<= 1 move 3)
    (signal-invalid-move "The move ~A is not between 1 and 3." move))
  (when (and (is move 1) (is (len player*!hands.0) 3))
    (signal-invalid-move "The first hand is only allowed to have 3 cards."))
  (when (is (len (get player*!hands (dec move))) 5)
    (signal-invalid-move "The ~:R hand is only allowed to have 5 cards." move)))

(def perform-move (move card)
  "Performs the move with the given card."
  (push card (get player*!hands (dec move))))

(def announce-winners ()
  "Announce the winners of the game."
  (send '(:all :log) game*!players "~{~{~{~{~A~A~}~^ ~}~^~5@T~}~%~}" (map #'hands game*!players))
  (send '(:all :log) game*!players
        "~{~A~^ ~}~%" (let total (all-royalites game*!players)
                        (mapeach player game*!players
                          (+ (- (* game*!players!len!dec (if (valid player) (* 2 (player-royalties player)) 0))
                                total)
                             (+ -3
                                (* 2
                                   (iter (for i from 0 below 3)
                                         (counting (is player (best #'hand> game*!players [idfn _!hands.i])))))))))))

(def valid (player)
  "Did this player end up with a valid hand, as in did they not fault."
  (and (hand> player!hands.2 player!hands.1)
       (hand> player!hands.1 player!hands.0)))

(def over ()
  "Is the game over?"
  (is game*!deck!len (- 52 (* 13 game*!players!len))))

(mac hand-match (var hand &body clauses)
  "Does the hand satisify one of the hand patterns."
  (once-only (hand)
    `(iflet ,var
       ,@(mappendeach ((flush straight . ranks) body) (group clauses :by 2)
           `((hand-is ,hand ,flush ,straight ',ranks) ,body)))))

(def hand-is (hand flush straight ranks)
  "Test the hand depending on the other arguments. If FLUSH, test if
   the hand contains a flush. If STRAIGHT, test if the hand contains a
   straight. Test the rank counts matches RANKS. Return the score of
   the hand if the hand matches."
  (and (or (not flush) (flush hand))
       (or (not straight) (straight hand))
       (ranks-are hand ranks)))

(def convert (rank)
  "Converts a rank to a number."
  (case rank a 14 k 13 q 12 j 11 t rank))

(def rank< (x y)
  "Is the first rank less than the second?"
  (< (convert x) (convert y)))

(def rank> (x y)
  "Is the first rank greater than the second?"
  (> (convert x) (convert y)))

(def flush (hand)
  "Does this hand contain a flush?"
  (and (is hand!len 5)
       (every [is _!suit hand!car!suit] hand)))

(def straight (hand)
  "Does this hand contain a straight?"
  (withs (ranks  (map #'rank hand)
          scores (map #'convert ranks))
    (and (is hand!len 5)
         (every [is _ 1] (vals (counts scores)))
         (with (smallest (best #'< scores)
                sum (reduce #'+ scores))
           (or (is (+ sum (* smallest -5)) 10)
               (and (mem 'a ranks)
                    (is sum (+ 2 3 4 5 14))))))))

(def ranks-are (hand ranks)
  "Do the ranks in the HAND match the counts in RANKS? If so return a
   'score' which represents how good of a hand this hand is relative to
   hands of the same type."
  (assert (is (reduce #'+ ranks) 5) () "These ranks are not expecting five cards.")
  (ado (map #'rank hand)
       (counts it)
       (tablist it)
       ;; By sorting rank first and then stabily sorting by count, we
       ;; keep elements that occur the same number of times but
       ;; different rank in proper order.
       (sort #'rank> it #'car)
       (ssort #'> it #'cadr)
       (when (or (no ranks)
                 (iso (map #'cadr it)
                      (if (and (is hand!len 3)
                               (iso (lastcons ranks 2) '(1 1)))
                          (butlast ranks 2)
                          ranks)))
         (cars it))))

(mac gen-score-body (hand &rest hands)
  "Generates a code that will become the body of score. For each hand
   return a unique score which we can compare from lexicographically
   to see which is better."
  (w/uniq gvar
    `(hand-match ,gvar ,hand
       ,@(iter (for i downfrom 0)
               (for hand in hands)
               (collect hand)
               (collect `(cons ,i ,gvar))))))

(def score (hand)
  "Return a unique score for a hand which we can compare to see which
   is better."
  (gen-score-body hand
    (t t     1 1 1 1 1)
    (nil nil 4 1)
    (nil nil 3 2)
    (t nil   1 1 1 1 1)
    (nil t   1 1 1 1 1)
    (nil nil 3 1 1)
    (nil nil 2 2 1)
    (nil nil 2 1 1 1)
    (nil nil 1 1 1 1 1)))

(def score< (s1 s2)
  "Is the first score less than the second?"
  (iter (for i in s1)
        (for j in s2)
        (when (~iso i j)
          (return-from score< (values (rank< i j) t))))
  ;; Otherwise the scores are the same.
  (values nil nil))

(def hand> (h1 h2)
  "Is the first hand worse than the second. Return a second value that
   is nil if the hands are equivalent."
  (score< (score h2) (score h1)))

(def shuffle (cards)
  "Shuffles a list of cards."
  (if (no cards)
      '()
      (let next (rand-elt cards)
        (cons next (shuffle (rem next cards))))))

(def make-deck ()
  "Creates a new deck."
  (shuffle
    (accum a
      (each rank ranks*
        (each suit suits*
          (a (make-card rank suit)))))))

(def royalties-bottom (hand)
  "Calculates the royalites for the bottom hand."
  (or (hand-match x hand
        (t t     1 1 1 1 1) (if (find 'a hand :key #'rank) 25 15)
        (nil nil 4 1)       10
        (nil nil 3 2)       6
        (t nil   1 1 1 1 1) 4
        (nil t   1 1 1 1 1) 2)
      0))

(def royalties-middle (hand)
  "Calculates the royalites for the bottom hand."
  (or (hand-match x hand
        (t t     1 1 1 1 1) (if (find 'a hand :key #'rank) 50 30)
        (nil nil 4 1)       20
        (nil nil 3 2)       12
        (t nil   1 1 1 1 1) 8
        (nil t   1 1 1 1 1) 4
        (nil nil 3 1 1)     2)
      0))

(def royalites-top (hand)
  "Calculates the royalties for the top hand."
  (ado (map #'rank hand)
       (counts it)
       (tablist it)
       (best #'> it #'cadr)
       (max (case (cadr it)
              3 (- 22 (pos (car it) ranks*))
              2 (- 9  (pos (car it) ranks*))
              t 0)
            0)))

(def player-royalties (player)
  "Calculates the total royalties a single player won."
  (let (top mid bot) player!hands
    (+ (royalites-top top)
       (royalties-middle mid)
       (royalties-bottom bot))))

(def all-royalites (players)
  "Calulates the total amount of royalties won."
  (reduce #'+ (keep #'valid players) :key #'player-royalties))
