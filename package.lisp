(defpackage :server
  (:use :clamp :experimental :iter :usocket)
  (:shadowing-import-from :experimental
     :repeat :def :fn :defmemo :mac :while
     :until :in :coerce :with :summing :defmethod)
  (:export :start-server :disconnect
           :defcont :cont :rem-cont :set-cont
           :defstart :defread :read-move :send
           :socket* :game* :player*
           :game :need :players :flags
           :player :socket
           :game-error :signal-game-error
           :invalid-move :signal-invalid-move
           :invalid-flag :signal-invalid-flag))
