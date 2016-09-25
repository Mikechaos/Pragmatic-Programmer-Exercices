(def possible-formats ["4pm" "7:38pm" "23:42" "3:16" "3:16am"])

; Grammar
;
; <time>                 ::= <12-hour><meridiem-indicator> | <12-hour><separator><minute><meridiem-indicator> | <24-hour><separator><minute> | <12-hour><separator><minute>
; <12-hour>              ::= <digit> | "10" | "11" | "12"
; <24-hour>              ::= <first-digit-hour><digit> | "2"<second-digit-hour>
; <minute>               ::= <first-digit-minute><second-digit-minute> | ""
; <digit>                ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
; <first-digit-hour>     ::= "0" | "1"
; <second-digit-hour>    ::= "0" | "1" | "2" | "3" | "4"
; <first-digit-minute>   ::= "0" | "1" | "2" | "3" | "4" | "5"
; <second-digit-minute>  ::= <digit>
; <separator>            ::= ":" | ""
; <meridiem-indicator>   ::= "am" | "pm"
;


(defn trim-program
  "Returns a program with no 2 consecutive spaces"
  [program] (map #(replace % #"\s+" " ") program))

(def grammar (trim-program [
  "<time>                 ::= <12-hour><meridiem-indicator> | <12-hour><separator><minute><meridiem-indicator> | <24-hour><separator><minute> | <12-hour><separator><minute>"
  "<12-hour>              ::= <digit> | \"10\" | \"11\" | \"12\""
  "<24-hour>              ::= <first-digit-hour><digit> | \"2\"<second-digit-hour>"
  "<minute>               ::= <first-digit-minute><second-digit-minute>"
  "<digit>                ::= \"0\" | \"1\" | \"2\" | \"3\" | \"4\" | \"5\" | \"6\" | \"7\" | \"8\" | \"9\""
  "<first-digit-hour>     ::= \"0\" | \"1\""
  "<second-digit-hour>    ::= \"0\" | \"1\" | \"2\" | \"3\" | \"4\""
  "<first-digit-minute>   ::= \"0\" | \"1\" | \"2\" | \"3\" | \"4\" | \"5\""
  "<second-digit-minute>  ::= <digit>"
  "<separator>            ::= \":\""
  "<meridiem-indicator>   ::= \"am\" | \"pm\""]))

