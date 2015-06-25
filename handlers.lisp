;;;; These are the handlers for different restarts.
(in-package :server)
(syntax:use-syntax :clamp)

(def restart-turn-handler (&rest args)
  "A restart handler for restarting the current players turn."
  (declare (ignore args))
  (when (find-restart 'restart-turn)
    (invoke-restart 'restart-turn)))

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
