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
           :player :num
           :board-game :board

           :no-continuation :game-error
           :invalid-move :signal-invalid-move
           :invalid-flag :signal-invalid-flag
           :malformed-input :signal-malformed-input

           :disconnect-handler :ignore-input-handler :restart-continuation-handler))
