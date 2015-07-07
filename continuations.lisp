;;;; This is code for implementing continuations for the server.
(in-package :server)
(syntax:use-syntax :clamp)

(defparameter conts* (table)
  "A mapping of sockets to continuations that should be called when
   that socket is available.")

(defparameter socket* nil "The current socket being read from.")

(mac defcont (name args &body body)
  "Define a continuation. It takes two arguments, the arguments to
   create the continuation and the arguments actually passed to the
   continuation."
  `(def ,name ,args
     ,(check (car body) #'stringp)
     (fn () ,@body)))

(defmethod call-cont (socket)
  "Calls the continuation for a socket."
  (aif2 conts*.socket
        (w/repeat-restart restart-continuation "Restart the current continuation being called."
          (call it))
        (restart-case (signal-no-continuation "No continuation found for the given socket.")
          (ignore-input ()
            :report "Ignore all of the input from the socket."
            (while (listen socket!socket-stream)
              (read-line :from socket!socket-stream))))))

(defmethod call-cont :around ((socket stream-usocket))
  "For a game socket we want to remove the continuation after using it if it does not change."
  (let val conts*.socket
    (prog1 (call-next-method)
      (when (is val conts*.socket)
        (remhash socket conts*)))))

(def rem-cont (socket)
  "Remove the continuation for the given socket."
  (remhash socket conts*))

(def set-cont (socket cont)
  (= conts*.socket cont))
