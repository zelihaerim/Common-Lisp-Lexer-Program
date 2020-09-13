; Author : ZELIHA ERIM
;School Number of Author : 151044065
;Gebze Technical University
; this program is reading .lisp file and splits content of file to lexemes with using common lisp programming language by calling Lexer function.

(defun File(listem-string filename-string) ; Reading file line by line and it stores them into string variable as named listem
    (with-open-file (file filename-string) 
      (loop for i from 0	
            for line = (read-line file nil nil)
            while line
            do (progn
                (setq listem-string (concatenate 'string listem-string line)) ;concatenate function appends 2 strings
           	)))
    listem-string ; returning string that is content of file
)
(defun erorrParent(listem-string parenthesis1 parenthesis2 index) ;parenthesis error checking, recursively
    (cond
        ((eq index (length listem-string)) ; base condition
            (cond
            	((< parenthesis1 parenthesis2) ; represents error 
            		(print "Error expected ( ")
            		nil							; if there is error function will return nil
            	)
            	((> parenthesis1 parenthesis2)
            		(print "Error expected ) ")
            		nil
            	)
            	(t
            		1	; if there is no error function will return true
            	)))
        ((string= (subseq listem index (+ index 1)) "(") ;counting left parenthesis
            (progn
                (setq parenthesis1 (+ 1 parenthesis1)); increment for counting right of parenthesis
                (setq index (+ 1 index))	 ; increment for next calling
                (erorrParent  listem-string parenthesis1 parenthesis2 index)
            ))
        ((string= (subseq listem index (+ index 1)) ")") ;counting right parenthesis
            (progn
                (setq parenthesis2 (+ 1 parenthesis2)) ; increment for counting right of parenthesis
                (setq index (+ 1 index))  ; increment for next calling
                (erorrParent  listem-string parenthesis1 parenthesis2 index)
            ))
        (t
            (setq index (+ 1 index))
            (erorrParent  listem-string parenthesis1 parenthesis2 index)
        )))
(defmacro concatenatef (s &rest strs)  ; this function adding 2 string or adding string and character
  `(setf ,s (concatenate 'string ,s ,@strs)))

(setq keyword (concatenate 'string "aonecsdf" "wi" ) );keyword strings contain first letter of these keywords:
;and, or, not, equal, append, concat,set deffun,while,if
	(defun FindAnd( listem-string temp) ;it is checking "and" keyword in listem-string by using given (index =temp)
		(if(string= (subseq listem temp (+ temp 3)) "and")
			1
			0))
	(defun FindAppend( listem-string temp) ;it is checking "append" keyword in listem-string by using given (index =temp)
		(if(string= (subseq listem temp (+
		 temp 6)) "append")
			1
			0))
	(defun FindOr( listem-string temp) ;it is checking "or" keyword in listem-string by using given (index =temp)
		(if(string= (subseq listem temp (+ temp 2)) "or")
			1
			0))
	(defun FindNot( listem-string temp) ;it is checking "not" keyword in listem-string by using given (index =temp)
		(if(string= (subseq listem temp (+ temp 3)) "not")
			1
			0))
	(defun FindEqual( listem-string temp) ;it is checking "equal" keyword in listem-string by using given (index =temp)
		(if(string= (subseq listem temp (+ temp 5)) "equal")
			1
			0))
	(defun FindExit( listem-string temp);it is checking "exit" keyword in listem-string by using given (index =temp)
		(if(string= (subseq listem temp (+ temp 4)) "exit")
			1
			0))
	(defun FindConcat( listem-string temp);it is checking "concat" keyword in listem-string by using given (index =temp)
		(if(string= (subseq listem temp (+ temp 6)) "concat")
			1
			0))
	(defun FindSet( listem-string temp);it is checking "set" keyword in listem-string by using given (index =temp)
		(if(string= (subseq listem temp (+ temp 3)) "set") 
			1	
			0))
	(defun FindDeffun( listem-string temp);it is checking "deffun" keyword in listem-string by using given (index =temp)
		(if(string= (subseq listem temp (+ temp 6)) "deffun")
			1
			0))
	(defun FindFor( listem-string temp);it is checking "for" keyword in listem-string by using given (index =temp)
		(if(string= (subseq listem temp (+ temp 3)) "for")
			1
			0))
	(defun FindWhile( listem-string temp);it is checking "while" keyword in listem-string by using given (index =temp)
		(if(string= (subseq listem temp (+ temp 5)) "while")
			1
			0))
	(defun FindIf( listem-string temp);it is checking "if" keyword in listem-string by using given (index =temp)
		(if(string= (subseq listem temp (+ temp 2)) "if")
			1
			0))
(defun isOperator (listem-string temp); it is checking given index is operator or not
	(setq Oper (concatenate 'string "+-/" "*" )) ; operators list
	(setq flag 0)
	(loop for i across Oper ; looping string of operator
		do
		(if (string= (subseq listem temp (+ temp 1)) i)
			(setq flag 1))) ; if it is operator return 1 by specifying flag as 1
	(if (eq flag 1)
		1
		0
	)
)
(defun allBlank(*str*-string); if all letters of the string are blank ,it will return 1
	(setq counter 0)
	(loop for i across *str*
		do
		(if(string= i " ")
		    (setq counter (+ counter 1)))
	)
	(if (eq counter (length *str*))
		1
		0
	)
)
(defun IdName (listem-string temp abcd-string)
	(loop while (< temp (length listem-string))
		do
		(progn 
			(if (and (eq (string= (subseq listem temp (+ temp 1)) " ") nil) (eq (string= (subseq listem temp (+ temp 1)) ")") nil) (eq 0 (isOperator listem temp)))
				(progn
					(concatenatef abcd (subseq listem temp (+ temp 1)))
					(setq temp (+ temp 1))
				)
				(progn
					(setq temp (length listem-string)) ; to end up while loop
				)
			)
		)
	)
	abcd-string
)
(defun key-Word (listem-string keyword-string temp keyW allThing-list) ; finding keywords
	(setq flag 0) ;Simply, function has keyword-string that contain first letter of keywords and finding listem's keyword
	(cond
		((string= keyW (subseq keyword 0 1) )
			(progn
				(cond
					((eq 1 (FindAnd listem-string temp))
						(progn
						(setq flag 1) ; if it finds keyword flag will be 1 then ı will return function 0 or 1 by controlling flag's value
						(setq a (list "keyword" "and"))
	            		(setq a (list a))
	            		(setq allThing (append allThing a)))
					)
					((eq 1 (FindAppend listem-string temp))
						(progn
						(setq flag 1)
						(setq a (list "keyword" "append"))
	            		(setq a (list a))
	            		(setq allThing (append allThing a)))
					)
					(t
						(if(eq (isOperator listem temp) 0)

							(progn
								(setq abcd (subseq listem temp (+ temp 1))) ;assign abcd,first letter which places after left parenthesis 
								(setq temp (+ temp 1))
								(IdName listem temp abcd) ; collecting second and other letter identifier
								(setq a (list abcd))
					    		(setq b (list "identifier"))
					    		(setq a (append b a))  ;identifier name and its token are appended
				            	(setq a (list a))
					    		(setq allThing (append allThing a))
							)
						)
					)
				)
			)
			)
		((string= keyW (subseq keyword 3 4) )
			(progn
				(cond
					((eq 1 (FindEqual listem-string temp))
						(progn
						(setq flag 1)
						(setq a (list "keyword" "equal"))
	            		(setq a (list a))
	            		(setq allThing (append allThing a)))
					)
					((eq 1 (FindExit listem-string temp))
											(progn
						(setq flag 1)
						(setq a (list "keyword" "exit"))
	            		(setq a (list a))
	            		(setq allThing (append allThing a)))
						)
					(t
						(if(eq (isOperator listem temp) 0)
							(progn
								(setq abcd (subseq listem temp (+ temp 1))) ;assign abcd,first letter which places after left parenthesis 
								(setq temp (+ temp 1))
								(IdName listem temp abcd) ; collecting second and other letter identifier
								(setq a (list abcd))
					    		(setq b (list "identifier"))
					    		(setq a (append b a))  ;identifier name and its token are appended
				            	(setq a (list a))
					    		(setq allThing (append allThing a))
							)
						)
					)
				)
			)
		)
		((string= keyW (subseq keyword 1 2) )
			(if(eq 1 (FindOr listem-string temp))
				(progn
					(setq flag 1)
					(setq a (list "keyword" "or"))
	            	(setq a (list a))
	            	(setq allThing (append allThing a)))
				(progn
					(if (eq (isOperator listem temp) 0)
						(progn
							(setq abcd (subseq listem temp (+ temp 1))) ;assign abcd,first letter which places after left parenthesis 
							(setq temp (+ temp 1))
							(IdName listem temp abcd) ; collecting second and other letter identifier
							(setq a (list abcd))
				    		(setq b (list "identifier"))
				    		(setq a (append b a))  ;identifier name and its token are appended
			            	(setq a (list a))
				    		(setq allThing (append allThing a))
						)
					)
				)
				))
		((string= keyW (subseq keyword 2 3) )
			(if(eq 1 (FindNot listem-string temp))
				(progn
					(setq flag 1)
					(setq a (list "keyword" "not"))
	            	(setq a (list a))
	            	(setq allThing (append allThing a)))

				(progn
					(if (eq (isOperator listem temp) 0)
						(progn
							(setq abcd (subseq listem temp (+ temp 1))) ;assign abcd,first letter which places after left parenthesis 
							(setq temp (+ temp 1))
							(IdName listem temp abcd) ; collecting second and other letter identifier
							(setq a (list abcd))
				    		(setq b (list "identifier"))
				    		(setq a (append b a))  ;identifier name and its token are appended
			            	(setq a (list a))
				    		(setq allThing (append allThing a))
						)
					)
				)

				))
		((string= keyW (subseq keyword 4 5) )
			(if(eq 1 (FindConcat listem-string temp))
				(progn
					(setq flag 1)
					(setq a (list "keyword" "concat"))
	            	(setq a (list a))
	            	(setq allThing (append allThing a)))
				(progn
					(if (eq (isOperator listem temp) 0)
						(progn
							(setq abcd (subseq listem temp (+ temp 1))) ;assign abcd,first letter which places after left parenthesis 
							(setq temp (+ temp 1))
							(IdName listem temp abcd) ; collecting second and other letter identifier
							(setq a (list abcd))
				    		(setq b (list "identifier"))
				    		(setq a (append b a))  ;identifier name and its token are appended
			            	(setq a (list a))
				    		(setq allThing (append allThing a))
						)
					)
				)

			))
		((string= keyW (subseq keyword 5 6) )
			(if(eq 1 (FindSet listem-string temp))
				(progn
					(setq flag 1)
					(setq a (list "keyword" "set"))
	            	(setq a (list a))
	            	(setq allThing (append allThing a)))
				(progn
					(if (eq (isOperator listem temp) 0)
						(progn
							(setq abcd (subseq listem temp (+ temp 1))) ;assign abcd,first letter which places after left parenthesis 
							(setq temp (+ temp 1))
							(IdName listem temp abcd) ; collecting second and other letter identifier
							(setq a (list abcd))
				    		(setq b (list "identifier"))
				    		(setq a (append b a))  ;identifier name and its token are appended
			            	(setq a (list a))
				    		(setq allThing (append allThing a))
						)
					)
				)


				))
		((string= keyW (subseq keyword 6 7) )
			(if(eq 1 (FindDeffun listem-string temp))
				(progn
					(setq flag 1)
					(setq a (list "keyword" "deffun"))
	            	(setq a (list a))

	            	(setq allThing (append allThing a))

	            		)
				(progn
					(if (eq (isOperator listem temp) 0)
						(progn
							(setq abcd (subseq listem temp (+ temp 1))) ;assign abcd,first letter which places after left parenthesis 
							(setq temp (+ temp 1))
							(IdName listem temp abcd) ; collecting second and other letter identifier
							(setq a (list abcd))
				    		(setq b (list "identifier"))
				    		(setq a (append b a))  ;identifier name and its token are appended
			            	(setq a (list a))
				    		(setq allThing (append allThing a))
						)
					)
					()
				)
				))
		((string= keyW (subseq keyword 7 8) )
			(if(eq 1 (FindFor listem-string temp))
				(progn
					(setq flag 1)
					(setq a (list "keyword" "for"))
	            	(setq a (list a))
	            	(setq allThing (append allThing a)))
				(progn
					(if (eq (isOperator listem temp) 0)
						(progn
							(setq abcd (subseq listem temp (+ temp 1))) ;assign abcd,first letter which places after left parenthesis 
							(setq temp (+ temp 1))
							(IdName listem temp abcd) ; collecting second and other letter identifier
							(setq a (list abcd))
				    		(setq b (list "identifier"))
				    		(setq a (append b a))  ;identifier name and its token are appended
			            	(setq a (list a))
				    		(setq allThing (append allThing a))
						)
					)
				)
				))
		((string= keyW (subseq keyword 8 9) )
			(if(eq 1 (FindWhile listem-string temp))
				(progn
					(setq flag 1)
					(setq a (list "keyword" "while"))
	            	(setq a (list a))
	            	(setq allThing (append allThing a)))
				(progn
					(if (eq (isOperator listem temp) 0)
						(progn
							(setq abcd (subseq listem temp (+ temp 1))) ;assign abcd,first letter which places after left parenthesis 
							(setq temp (+ temp 1))
							(IdName listem temp abcd) ; collecting second and other letter identifier
							(setq a (list abcd))
				    		(setq b (list "identifier"))
				    		(setq a (append b a))  ;identifier name and its token are appended
			            	(setq a (list a))
				    		(setq allThing (append allThing a))
						)
					)
				)
				))
		((string= keyW (subseq keyword 9 10) )
			(if(eq 1 (FindIf listem-string temp))
				(progn
					(setq flag 1)
					(setq a (list "keyword" "if"))
	            	(setq a (list a))
	            	(setq allThing (append allThing a)))
				(progn
					(if (eq (isOperator listem temp) 0)
						(progn
							(setq abcd (subseq listem temp (+ temp 1))) ;assign abcd,first letter which places after left parenthesis 
							(setq temp (+ temp 1))
							(IdName listem temp abcd) ; collecting second and other letter identifier
							(setq a (list abcd))
				    		(setq b (list "identifier"))
				    		(setq a (append b a))  ;identifier name and its token are appended
			            	(setq a (list a))
				    		(setq allThing (append allThing a))
						)
					)
				)
				))
		(t ; if sub sequence of string not first letter of keywords and not keyword it must be identifier name
			(if (eq (isOperator listem temp) 0)
				(progn
					(setq abcd (subseq listem temp (+ temp 1))); assign abcd,first letter which places after left parenthesis 
					(setq temp (+ temp 1))
					(IdName listem temp abcd) ; collecting second and other letter identifier
					(setq a (list abcd))
		    		(setq b (list "identifier"))
		    		(setq a (append b a)) ; identifier name and its token are appended
	            	(setq a (list a))
		    		(setq allThing (append allThing a))
				)))
	)
	(if (eq flag 1) ; if flag equals 1, function will return 1.
		1
		0
	)
	allThing
)

(defun Int-eger(*str*-string); this function checks string, that it is whether integer
	(defvar IntegerValue "0123456789") 
	(setq counter 0)
	(setq index 0)
	(loop for c across *str*
		do
		(progn
			(loop for i across IntegerValue
				do
				(if(string= (subseq *str* index (+ index 1)) i)
				    (setq counter (+ 1 counter))
				)
			)
			(setq index (+ 1 index))
		)
	)	
	(if (eq counter (length *str*))
		1
		0
	)
)
(defun FindTokens(listem-string keyword-string index allThing-list); finding tokens
    ;(defvar a nil)
    (loop for c across listem 
		do
		(cond
			((eq index (length listem-string))
				1
			)
	        ((string= (subseq listem index (+ index 1)) "(")
	            (progn
	            	(setq a (list "operator" "("))
	            	(setq a (list a))
	            	(setq allThing (append allThing a))
	                (setq index (+ 1 index))
	            	(setq keyW (subseq listem index (+ index 1)))
	                (key-Word listem-string keyword-string index keyW allThing)
	            )
	        )
	        ((string= (subseq listem index (+ index 1)) ")")
	            (progn
	            	(setq a (list "operator" ")"))
	            	(setq a (list a))
	            	(setq allThing (append allThing a))
	                (setq index (+ 1 index))
	            )
	        )
	        ((string= (subseq listem index (+ index 1)) "+")
	            (progn
	            	(setq a (list "operator" "+"))
	            	(setq a (list a))
	            	(setq allThing (append allThing a))
	                (setq index (+ 1 index))
	            )
	        )
	        ((and (string= (subseq listem index (+ index 1)) "-") (string= (subseq listem (+ index 1) (+ index 2)) " ") )
	            (progn
	            	(setq a (list "operator" "-"))
	            	(setq a (list a))
	            	(setq a (list a))
	            	(setq allThing (append allThing a))
	                (setq index (+ 1 index))
	            ) 
	        )
	        ; negative num
	        ((string= (subseq listem index (+ index 1)) "/")
	            (progn
	            	(setq a (list "operator" "/"))
	            	(setq a (list a))
	            	(setq allThing (append allThing a))
	                (setq index (+ 1 index))
	            )
	        ) 
	        ((string= (subseq listem index (+ index 2)) "**")
	            (progn
	            	(setq a (list "operator" "**"))
	            	(setq a (list a))
	            	(setq allThing (append allThing a))
	                (setq index (+ 2 index))
	            )
		    )
		    ((string= (subseq listem index (+ index 1)) "*")
	            (progn
	            	(setq a (list "operator" "*"))
	            	(setq a (list a))
	            	(setq allThing (append allThing a))
	                (setq index (+ 1 index))
	        	)
		    )
		    ((string= (subseq listem (- index 1) index) " ") ; if before letter there is a blank instead of left parenthesis,it means that this is first letter of id or integer value
		    	(progn
		    		(defvar *str* nil)
		    		(setq temp index)
		    		(loop while (and (not(string= (subseq listem temp (+ temp 1)) " ")) (not(string= (subseq listem temp (+ temp 1)) "(")) (not(string= (subseq listem temp (+ temp 1)) ")")))
						do
							(concatenatef *str* (subseq listem temp (+ temp 1)))
							(setq temp (+ temp 1))
					)  		
		    		(if (not (eq 1 (allBlank *str*))); if not blank
		    			(progn
		    				(if (eq (Int-eger *str*) 0) 
		    					(progn
		    						(setq a (list *str*))
		    						(setq b (list "identifier"))
		    						(setq a (list (append b a)))
		    						(setq allThing (append allThing a))
		    					)
		    					(progn ; else statement
		    						(setq a (list *str*));else it is integer
		    						(setq b (list "integer"))
		    						(setq a (list (append b a)))
		    						(setq allThing (append allThing a))
		    					)
		    				)
		    			)
		    		)
		    		(setq index (+ 1 index))
		    		(setq *str* nil) ; update *str* as nil, for next lexeme
		    	)
		    )
	        (t
	       		(setq index (+ 1 index))
	    	)
    	)
	)
    allThing
)
(defun lexer (filename-string) ; lexer function
	(setq listem nil)
	(setq update nil)
	(setq listem (File listem filename-string))
	(setq parenthesis1 0)
	(setq parenthesis2 0)
	(erorrParent listem parenthesis1 parenthesis2 0)
	(setq allThing nil)
	(setq keyword (concatenate 'string "aonecsdf" "wi" ) );and, or, not, equal, append, concat,
	(FindTokens listem keyword 0 allThing)
	(print allThing)
)
(lexer "input.txt")
;   End Of Program