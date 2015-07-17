;;;; This is code for implementing continuations for the server.
(in-package :server)
(syntax:use-syntax :clamp)

(defparameter game* nil "The current game being played.")
(defparameter player* nil "The current player and/or socket.")

(mac defcont (name args &body body)
  "Define a continuation. It takes two arguments, the arguments to
   create the continuation and the arguments actually passed to the
   continuation."
  `(def ,name ,args
     ,(check (car body) #'stringp)
     (fn () ,@body)))

(defmethod call-cont (obj)
  "Calls the continuation for OBJ which is an object that has a
   continuation.."
  (let player* obj
    (aif obj!cont
      (w/repeat-restart restart-continuation "Restart the current continuation being called."
        (call it))
      (restart-case (signal-no-continuation "No continuation found for the given object.")
        (ignore-input ()
          :report "Ignore all of the input from the obj."
          (clear-input obj!socket-stream))))))

(defmethod call-cont :around ((obj player))
  (let game* obj!game
    (call-next-method)))

(defmethod (setf cont) :around ((val function) (player player))
  "For a player we modify the continuation to remove itself when it is
   called if it still the player's continuation after being called."
  (call-next-method
   (afn ()
     (prog1 (call val)
       (when (is player!cont #'self)
         (wipe player!cont))))
   player))
