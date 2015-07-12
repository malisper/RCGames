(in-package :server)

;; An object that has a continuation.
(deftem (cont (:conc-name nil))
  cont)

;; A server socket that has a continuation.
(deftem (cont-server (:include cont stream-server-usocket)))

;; A player which both has a continuation and is the same thing as the
;; socket they are on.
(deftem (player (:conc-name nil) (:include cont stream-usocket))
  game flags)

(deftem (game (:conc-name nil))
  need players game-log flags (player-type 'player))
