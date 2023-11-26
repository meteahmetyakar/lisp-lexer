(defconstant KEYWORDS
    '(
        
        ("and" "KW_AND")
        ("or" "KW_OR")
        ("not" "KW_NOT")
        ("equal" "KW_EQUAL")
        ("less" "KW_LESS")
        ("nil" "KW_NIL")
        ("list" "KW_LIST")
        ("append" "KW_APPEND")
        ("concat" "KW_CONCAT")
        ("set" "KW_SET")
        ("def" "KW_DEF")
        ("for" "KW_FOR")
        ("if" "KW_IF")
        ("exit" "KW_EXIT")
        ("load" "KW_LOAD")
        ("display" "KW_DISPLAY")
        ("true" "KW_TRUE")
        ("false" "KW_FALSE")
    )
)



(defconstant OPERATORS
    '(

        ("+" "OP_PLUS")
        ("-" "OP_MINUS")
        ("/" "OP_DIV")
        ("*" "OP_MULT")
        ("(" "OP_OP")
        (")" "OP_CP")
        ("," "OP_COMMA")
    )
)



(defconstant WHITE_SPACES
    '(

        #\space
        #\tab
        #\newline
    )
)



(defconstant ZERO #\0)

(defconstant DIGITS
    '(

        #\0
        #\1
        #\2
        #\3
        #\4
        #\5
        #\6
        #\7
        #\8
        #\9
    )
)



(defconstant LETTERS
    '(

        (#\a #\A)
        (#\b #\B)
        (#\c #\C)
        (#\d #\D)
        (#\e #\E)
        (#\f #\F)
        (#\g #\G)
        (#\h #\H)
        (#\i #\I)
        (#\j #\J)
        (#\k #\K)
        (#\l #\L)
        (#\m #\M)
        (#\n #\N)
        (#\o #\O)
        (#\p #\P)
        (#\q #\Q)
        (#\r #\R)
        (#\s #\S)
        (#\t #\T)
        (#\u #\U)
        (#\v #\V)
        (#\w #\W)
        (#\x #\X)
        (#\y #\Y)
        (#\z #\Z)
    )
)


(defconstant syntax-error-token "SYNTAX_ERROR")
(defconstant white-space-token "WHITE_SPACE")
(defconstant comment-token "COMMENT")
(defconstant fraction-token "VALUEF")
(defconstant identifier-token "IDENTIFIER")

; states are initializing
(defparameter initState (make-hash-table ))
(defparameter commentState (make-hash-table ))
(defparameter integerState (make-hash-table ))
(defparameter fractionState (make-hash-table ))
(defparameter identifierState (make-hash-table ))


; this function add a state to given dfa
; if there is no value of given key, it creates an empty hashtable on it
; if value of given key is not a hashtable then we create a empty hashtable, empty hashtable connect to given hashtable,
; and value of key connect to empty hashtable
(defun addState(key dfa)

    (cond
        ((not (gethash key dfa))
            (setf (gethash key dfa) (make-hash-table ))
        )
        ((not (equal (type-of (gethash key dfa)) 'hash-table))
            (progn
                (setq tmpDFA (make-hash-table ))
                (setq value (gethash key dfa))
                (setf (gethash key dfa) tmpDFA)
                (setf (gethash 'default tmpDFA) value)
            )
        )
    )

    (gethash key dfa)
)

; it returns a charArray form of given str
(defun toCharArray (str)
    (setq charArray '())
    (loop for idx from 0 to (- (length str) 1)
        do (setq charArray (cons (char str idx) charArray))
    )
    (reverse charArray)
)

; it creates a state and adds to initState with given str characters
; for example if str is "gtu" then dfa is initState[g] -> gState[t] -> tState[u] -> value
(defun create&AddStatesToInitState (str &optional value)

    (setq charArray (toCharArray str))
    (setq firstChar (car charArray))

    ; we check length because some keywords such as operators have only 1 character That's why we don't need create states for it
    ; for example if str is "+" initState[+] -> value 
    (if (> (length charArray) 1) 
        (progn
            (setq currState (addState firstChar initState))
            (loop for curr-char in (cdr (reverse (cdr (reverse charArray))))
                do (setq currState (addState curr-char currState))
            )
            (setf (gethash (car (last charArray)) currState) value)
        )
        (setf (gethash firstChar initState) value)
    )
)

(defun addStatesToInitState (stateInfo)
    (create&AddStatesToInitState (first stateInfo) (second stateInfo))
)

; this function traverse entire given dfa and creates states from keys and add to given dfa each created states
(defun addStatesToEntireDfa (dfa keys defaultValue)

    (loop for key in keys
        do 
        (case (type-of (gethash key dfa))
            ('null (setf (gethash key dfa) defaultValue))
            ('hash-table 
                (progn
                    (when (null (gethash 'default (gethash key dfa))) (setf (gethash 'default (gethash key dfa)) defaultValue))
                    (addStatesToEntireDfa (gethash key dfa) keys defaultValue)
                )
            )
            (t
                (progn
                    (setq kht (addState key dfa))
                    (addStatesToEntireDfa kht keys defaultValue)
                )
            )
        )
    )
)

; it builds keyword dfa
(defun BUILD-KEYWORDS ()
    (loop for keyword in KEYWORDS
        do (addStatesToInitState keyword)
    )
)

; it builds operator dfa
(defun BUILD-OPERATORS ()
    (loop for operator in OPERATORS
        do (addStatesToInitState operator)
    )
)

; it builds comment dfa
; this works like [;]{2}.*
(defun BUILD-COMMENT ()
    (setf (gethash #\; initState) commentState)
    (setq secondCommentState (addState #\; commentState))
    (setf (gethash 'default secondCommentState)  secondCommentState)
    (setf (gethash #\newline secondCommentState) comment-token)
)

; it builds integer dfa
; this works like [1-9]+[0-9]*
(defun BUILD-INTEGER ()
    (setq zeroState (addState #\0 initState))
    (setf (gethash 'default zeroState) fraction-token)

    (setq digitExceptZero '())
    (loop for digit in DIGITS
        do (progn
            (setf (gethash digit zeroState) syntax-error-token)
            (when (not (equal digit ZERO)) (setq digitExceptZero (cons digit digitExceptZero)))
        )
    )

    (loop for digit in digitExceptZero
        do (setf (gethash digit initState) integerState)
    )

    (loop for digit in DIGITS
        do (setf (gethash digit integerState) integerState)
    )

    (setf (gethash 'default integerState) fraction-token)
)

; it builds fraction dfa
; this works like [0-9]+[b][0-9]*
(defun BUILD-FRACTION ()

    (setf zeroState (addState #\0 initState))
    (setf (gethash #\b zeroState) fractionState)
    (setf (gethash #\b integerState) fractionState)

    (setf digitExceptZero '())
    (loop for digit in DIGITS
        do (when (not (equal digit ZERO))
            (setf digitExceptZero (cons digit digitExceptZero))
        )
    )

    (loop for digit in DIGITS
        do (setf (gethash digit fractionState) fractionState)
    )

    (setf (gethash 'default fractionState) fraction-token)
)

; it builds identifier dfa
; this works like [A-Za-z_][A-Za-z0-9_]*
(defun BUILD-IDENTIFIER ()

    (setf (gethash #\_ initState) identifierState)
    (setf (gethash #\_ identifierState) identifierState)

    (setq letterList '())
    (loop for pair in LETTERS
        do
        (progn 
            (push (first pair) letterList)
            (push (second pair) letterList)
        )
    )    
        
    (setq letterList (reverse letterList)) 

    (loop for letter in letterList 
        do (setf (gethash letter identifierState) identifierState)
    )

    (addStatesToEntireDfa initState letterList identifierState)

    (loop for digit in DIGITS
        do 
        (progn
            (setq digitState (addState digit initState))
            (setf (gethash digit identifierState) identifierState)
            (loop for letter in letterList 
                do (setf (gethash letter digitState) syntax-error-token)
            )
        )
    )

    (loop for operator in OPERATORS
        do 
        (progn
            (setq operatorState (addState operator initState))
            (setf (gethash operator identifierState) syntax-error-token)
            (loop for letter in letterList 
                do (setf (gethash letter operatorState) syntax-error-token)
            )
        )
    )

    (setf (gethash 'default identifierState) identifier-token)
)

(defun BUILD-LEXER ()
    (format t "Lexer is building...") (terpri )
    (BUILD-KEYWORDS)    (format t "Keyword DFA created and connected to INIT-DFA") (terpri )
    (BUILD-OPERATORS)   (format t "Operator DFA created and connected to INIT-DFA") (terpri )
    (BUILD-COMMENT)     (format t "Comment DFA created and connected to INIT-DFA") (terpri )
    (BUILD-INTEGER)     (format t "Integer DFA created and connected to INIT-DFA") (terpri )
    (BUILD-IDENTIFIER)  (format t "Identifier DFA created and connected to INIT-DFA") (terpri )
    (BUILD-FRACTION)    (format t "Fraction DFA created and connected to INIT-DFA") (terpri )
    (format t "Lexer is built.") (terpri ) (terpri )
)

; it returns list of token by type-of given stateToken
(defun getStateAndTokenList (stateToken &optional defaultToken)
    (case (type-of stateToken)
        ('cons          (values-list stateToken))
        ('hash-table    (values stateToken defaultToken))
        (t              (values initState stateToken))
    )
)

; it determines next state and returns it
(defun getNextState (key dfa &optional prevToken)

    (if (null (gethash key dfa)) 
        (if (null (gethash 'default dfa))
            (values initState (if (isWhiteSpace key) white-space-token syntax-error-token) prevToken)
            (progn
                (multiple-value-setq (state defaultToken defaultPrevToken) (getStateAndTokenList (gethash 'default dfa)))
                (if (equal dfa state)
                    (values state (or prevToken defaultPrevToken))
                    (getNextState key state defaultToken)
                )
            )
        )
        (progn
            (multiple-value-setq (state token defaultPrevToken) (getStateAndTokenList (gethash key dfa)))
            (values state token (or prevToken defaultPrevToken))
        )
    )
)

; it checks given ch is white space 
(defun isWhiteSpace (ch)
    (loop for whiteSpace in WHITE_SPACES
        do (when (char= whiteSpace ch) (return-from isWhiteSpace t))
    )
    (return-from isWhiteSpace nil)
)

; it prints token and its value to console
(defun printToken (token charArray)

    (setq result (make-array 0
                                        :element-type 'character
                                        :fill-pointer 0
                                        :adjustable t))

    (loop for ch in charArray
        do 
        (progn
            (vector-push-extend ch result)
        )
    )

    (format T "| ~v:@<~A~> | ~v:@<~A~> |" 20 result 20 token)(terpri)
    (string result)

)

(defun gppinterpreter ()
    (BUILD-LEXER)   

    (setq file nil)
    (setq filename (car *args*))
    (when (not (null filename)) (setq file (open filename)))

    (setq readChars '())

    (setq currState initState)
    
    (format T "| ~v:@<~A~> | ~v:@<~A~> |" 20 "VALUE" 20 "TOKEN")(terpri)
    (format T " ---------------------------------------------")(terpri)

    (loop
        (when (not (or (null file) (listen file))) (return ))
        (setq ch (read-char file))
        (setq readChars (cons ch readChars))
        (multiple-value-setq (currState token prevToken) (getNextState ch currState))
        (cond
            ((equal token syntax-error-token)
                (progn
                    (format t "SYNTAX_ERROR ")

                    (loop
                        (when (isWhiteSpace ch) (return))
                        (setq ch (read-char file))
                        (setq readChars (cons ch readChars))
                    )
                    
                    (loop for ch2 in (reverse (cdr readChars))
                        do (write-char ch2)
                    )
                    (format t " cannot be tokenized") (terpri )
                    (return )
                )
            )
            ((isWhiteSpace ch)
                (progn
                    (when (not (null prevToken)) (printToken prevToken (reverse (cdr readChars))))

                    (when (not (or (null token) (equal token white-space-token))) (printToken token (reverse (cdr readChars))))

                    (when (not (null token)) (setq readChars '()))
                )
            )
            ((null token))
            (t  
                (progn
                    (when (not (null prevToken)) 
                        (progn 
                            (printToken prevToken (reverse (cdr readChars)))
                            (setq readChars (list (car readChars)))
                        )
                    )
                    (printToken token (reverse readChars))
                    (setq readChars '())
                )
            )
        )
    )
)

(gppinterpreter)