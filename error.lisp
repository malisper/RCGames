;;;; These are the handlers for different restarts.
(in-package :server)
(syntax:use-syntax :clamp)

(mac define-simple-error (name)
  "Define a condition that inherits from simple-error. Also define a
   macro that makes will send an error with the given type."
  `(do (define-condition ,name (simple-error) ())
       (mac ,(symb 'signal- name) (format-control &rest format-arguments)
         ,(format nil "Signals a condition of type ~A." name)
         `(error ',',name :format-control ,format-control :format-arguments (list ,@format-arguments)))))

(mac w/repeat-restart (name report &body body)
  "Evaluates BODY in a scope where NAME is a restart that will
   reevaluate BODY."
  `(loop (restart-case (return ,@body)
           (,name () :report ,report))))

(define-simple-error no-continuation)
(define-simple-error game-error)
(define-simple-error invalid-move)
(define-simple-error malformed-input)
(define-simple-error invalid-flags)

(def restart-cont-handler (condition)
  "A restart handler for restarting the current players turn."
  (when (find-restart 'restart-cc)
    (invoke-restart 'restart-turn "~? Please make a valid move.~%"
                    (simple-condition-format-control condition)
                    (simple-condition-format-arguments condition))))

(def disconnect-handler (&rest args)
  "A restart handler for disconnecting the current game."
  (declare (ignore args))
  (when (find-restart 'disconnect)
    (invoke-restart 'disconnect)))

(def ignore-input-handler (&rest args)
  "A restart handler for ignoring the input from a player that
   performs out of turn.."
  (declare (ignore args))
  (when (find-restart 'ignore-input)
    (invoke-restart 'ignore-input)))
