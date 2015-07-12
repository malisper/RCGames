(defpackage :server
  (:use :clamp :experimental :iter :usocket)
  (:shadowing-import-from :experimental
     :repeat :def :fn :defmemo :mac :while
     :until :in :coerce :with :summing :defmethod)
  (:export :start-server :disconnect
           :defcont :cont
           :defstart :defread :read-move :send
           :game* :player*
           :game :need :players :flags :player-type
           :player
           :game-error :signal-game-error
           :invalid-move :signal-invalid-move
           :invalid-flag :signal-invalid-flag))
