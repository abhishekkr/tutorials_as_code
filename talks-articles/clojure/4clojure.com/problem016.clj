; hello world
; Write a function which returns a personalized greeting.
; (= (__ "Dave") "Hello, Dave!")
(= ((fn [nam] (str "Hello, " nam "!")) "Dave") "Hello, Dave!")
