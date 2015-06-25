(defpackage :server
  (:use :clamp :experimental :iter :usocket :ppcre)
  (:shadowing-import-from :experimental
     :repeat :def :fn :defmemo :mac :while
     :until :in :coerce :with :summing :defmethod)
  (:shadowing-import-from :ppcre
     :split))
