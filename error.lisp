;;;; These are the handlers for different restarts.
(in-package :server)
(syntax:use-syntax :clamp)

(define-condition game-error (simple-error) ())

(mac define-game-error (name code)
  "Define a condition that inherits from simple-error. Also define a
   macro that makes will send an error with the given type."
  `(do (define-condition ,name (game-error) ((code :accessor code :allocation :class :initform ,code)))
       (mac ,(symb 'signal- name) (format-control &rest format-arguments)
         ,(format nil "Signals a condition of type ~A." name)
         `(error ',',name :format-control ,format-control :format-arguments (list ,@format-arguments)))))

(mac w/repeat-restart (name report &body body)
  "Evaluates BODY in a scope where NAME is a restart that will
   reevaluate BODY."
  `(loop (restart-case (return ,@body)
           (,name () :report ,report))))

(defparameter disconnected-code*  -1 "The code for when a different player disconnects.")
(defparameter unknown-error-code* -2 "The code for when an unknown error occurs.")

(define-game-error no-continuation -3)
(define-game-error invalid-move -4)
(define-game-error malformed-input -5)
(define-game-error invalid-flags -6)

(mac defhandler (name)
  "Define a handler for the restart of name NAME."
  (w/uniq gargs
    `(def ,(symb name '-handler) (&rest ,gargs)
       (when (find-restart ',name)
         (apply #'invoke-restart ',name ,gargs)))))

(defhandler restart-continuation)
(defhandler disconnect)
(defhandler ignore-input)
