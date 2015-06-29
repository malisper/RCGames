;;;; The following is code that will generate test cases based on a
;;;; log file. It plays through the game and sees if it gets the same
;;;; result as the actual game.

(defpackage :game-server-tests
  (:use :clamp :clamp-experimental :iter :ppcre :usocket)
  (:shadowing-import-from :experimental
     :repeat :def :fn :defmemo :mac :while
     :until :in :coerce :with :summing :defmethod)
  (:shadowing-import-from :ppcre :split))

(in-package :game-server-tests)
(syntax:use-syntax :clamp)

(def test (log game-id port)
  "Given a log file and a game identifier, plays through all of the
   games in the log file."
  (w/infile *standard-input* log
    (with (game-scanner (create-scanner (mkstr "^" game-id " (\\d*)$"))
           move-scanner (create-scanner "^(\\d*): (.*)$")
           line-num 0)
      (whilet line (read-line :eof nil)
        (++ line-num)
        (mvb (match strings) (scan-to-strings game-scanner line)
          (when match
            (withs (num (parse-integer (last (coerce strings 'list)))
                    ps  (n-of num (socket-connect "127.0.0.1" port)))
              ;; We are sorting them based on the player number they are assigned.
              (unwind-protect (do (= ps (sort #'< ps [parse-integer+read-line :from (socket-stream _)]))
                                  (while t
                                    (let line (read-line)
                                         (++ line-num)
                                      (mvb (match strings) (scan-to-strings move-scanner line)
                                        (if (no match)
                                            ;; Then this is the last line of the game.
                                            (do (assert (iso (last-line ps!car!socket-stream)
                                                             line)
                                                        ()
                                                        "The results do not match up on line #~A."
                                                        line-num)
                                                ;; Exit the game loop
                                                (return))
                                            (let (player move) (coerce strings 'list)
                                              (zap dec+parse-integer player)
                                              (let stream ps.player!socket-stream
                                                (format stream "~A~%" move)
                                                (force-output stream))))))))
                (mapc #'socket-close ps))))))
      t)))

(def last-line (stream)
  "Returns the last line of output from a stream."
  (sleep .5)
  (ret prev nil
    (whilet line (and (listen stream) (read-line :from stream :eof nil))
      (= prev line))))

