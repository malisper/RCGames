;;;; This is code for implementing continuations for the server.
(in-package :server)
(syntax:use-syntax :clamp)

(defparameter conts* (table)
  "A mapping of sockets to continuations that should be called when
   that socket is available.")

(defparameter temp-conts* (table)
  "A mapping of sockets to continuations that should be called when
   the socket and should be removed from this table when the socket is
   available.")

(mac defcont (name args1 args2 &body body)
  "Define a continuation. It takes two arguments, the arguments to
   create the continuation and the arguments actually passed to the
   continuation."
  `(def ,name ,args1
     ,(check (car body) #'stringp)
     (fn ,args2 ,@body)))

(def cont (socket)
  "Returns the permanent continuation for SOCKET."
  conts*.socket)

(def (setf cont) (continuation socket)
  "Sets the continuation for the given socket."
  (setf conts*.socket continuation))

(def temp-cont (socket)
  "Returns the temporary continuation for SOCKET. A continuation that
   will be removed as soon as it is returned."
  (prog1 temp-conts*.socket
    (rem-temp-cont socket)))

(def rem-cont (socket)
  "Remove the continuation for the given socket."
  (remhash socket conts*))

(def (setf temp-cont) (continuation socket)
  "Sets the temporary continuation for a given socket. It will be
   removed as soon as it is returned by temp-cont."
  (setf temp-conts*.socket continuation))

(def rem-temp-cont (socket)
  "Removes the temporary continuation for the given socket."
  (remhash socket temp-conts*))
