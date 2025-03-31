#lang racket

;; Define structures for tokens
(struct token (type value line) #:transparent)

;; Token types
(define TOKEN-ID 'id)
(define TOKEN-NUM 'num)
(define TOKEN-READ 'read)
(define TOKEN-WRITE 'write)
(define TOKEN-IF 'if)
(define TOKEN-ENDIF 'endif)
(define TOKEN-SEMICOLON 'semicolon)
(define TOKEN-PLUS 'plus)
(define TOKEN-MINUS 'minus)
(define TOKEN-LPAREN 'lparen)
(define TOKEN-RPAREN 'rparen)
(define TOKEN-ASSIGN 'assign)
(define TOKEN-LT 'lt)         ;; Less than <
(define TOKEN-LTE 'lte)       ;; Less than or equal <=
(define TOKEN-GT 'gt)         ;; Greater than >
(define TOKEN-GTE 'gte)       ;; Greater than or equal >=
(define TOKEN-EQ 'eq)         ;; Equal ==
(define TOKEN-NEQ 'neq)       ;; Not equal !=
(define TOKEN-ERROR 'error)
(define TOKEN-EOF 'eof)

;; Global variables for lexical analysis
(define current-token null)
(define next-token-index 0)
(define tokens '())
(define current-line 1)

;; Function to tokenize the input string
(define (tokenize input-string)
  (set! tokens '())
  (set! current-line 1)
  
  (define lines (string-split input-string "\n"))
  (for ([line lines]
        [line-num (in-naturals 1)])
    (set! current-line line-num)
    (tokenize-line line line-num))
  
  (append tokens (list (token TOKEN-EOF "" (add1 (length lines))))))

;; Tokenize a single line
(define (tokenize-line line line-num)
  (define current-index 0)
  (define line-length (string-length line))
  
  (define (skip-whitespace)
    (when (and (< current-index line-length)
               (char-whitespace? (string-ref line current-index)))
      (set! current-index (add1 current-index))
      (skip-whitespace)))
  
  (let process-tokens ()
    (skip-whitespace)
    (when (< current-index line-length)
      (let ([current-char (string-ref line current-index)])
        (cond
          ;; Identifiers
          [(char-alphabetic? current-char)
           (let ([start-index current-index])
             (set! current-index (add1 current-index))
             (while (and (< current-index line-length)
                         (or (char-alphabetic? (string-ref line current-index))
                             (char-numeric? (string-ref line current-index))))
               (set! current-index (add1 current-index)))
             (let ([lexeme (substring line start-index current-index)])
               (cond
                 [(string=? lexeme "read") (set! tokens (append tokens (list (token TOKEN-READ lexeme line-num))))]
                 [(string=? lexeme "write") (set! tokens (append tokens (list (token TOKEN-WRITE lexeme line-num))))]
                 [(string=? lexeme "if") (set! tokens (append tokens (list (token TOKEN-IF lexeme line-num))))]
                 [(string=? lexeme "endif") (set! tokens (append tokens (list (token TOKEN-ENDIF lexeme line-num))))]
                 [else (set! tokens (append tokens (list (token TOKEN-ID lexeme line-num))))])
               (process-tokens)))]
          
          ;; Numbers
          [(char-numeric? current-char)
           (let ([start-index current-index])
             (set! current-index (add1 current-index))
             (while (and (< current-index line-length)
                         (char-numeric? (string-ref line current-index)))
               (set! current-index (add1 current-index)))
             (set! tokens (append tokens (list (token TOKEN-NUM (substring line start-index current-index) line-num))))
             (process-tokens))]
          
          ;; Semicolon
          [(char=? current-char #\;)
           (set! tokens (append tokens (list (token TOKEN-SEMICOLON ";" line-num))))
           (set! current-index (add1 current-index))
           (process-tokens)]
          
          ;; Plus
          [(char=? current-char #\+)
           (set! tokens (append tokens (list (token TOKEN-PLUS "+" line-num))))
           (set! current-index (add1 current-index))
           (process-tokens)]
          
          ;; Minus
          [(char=? current-char #\-)
           (set! tokens (append tokens (list (token TOKEN-MINUS "-" line-num))))
           (set! current-index (add1 current-index))
           (process-tokens)]
          
          ;; Left Parenthesis
          [(char=? current-char #\()
           (set! tokens (append tokens (list (token TOKEN-LPAREN "(" line-num))))
           (set! current-index (add1 current-index))
           (process-tokens)]
          
          ;; Right Parenthesis
          [(char=? current-char #\))
           (set! tokens (append tokens (list (token TOKEN-RPAREN ")" line-num))))
           (set! current-index (add1 current-index))
           (process-tokens)]
          
          ;; Assignment (=)
          [(char=? current-char #\=)
           (set! current-index (add1 current-index))
           (if (and (< current-index line-length)
                    (char=? (string-ref line current-index) #\=))
               (begin
                 (set! tokens (append tokens (list (token TOKEN-EQ "==" line-num))))
                 (set! current-index (add1 current-index)))
               (begin
                 (set! tokens (append tokens (list (token TOKEN-ASSIGN "=" line-num))))
                 (process-tokens)))]

          ;; Less than (<)
          [(char=? current-char #\<)
           (set! current-index (add1 current-index))
           (if (and (< current-index line-length)
                    (char=? (string-ref line current-index) #\=))
               (begin
                 (set! tokens (append tokens (list (token TOKEN-LTE "<=" line-num))))
                 (set! current-index (add1 current-index)))
               (begin
                 (set! tokens (append tokens (list (token TOKEN-LT "<" line-num))))
                 (process-tokens)))]
          
          ;; Greater than (>)
          [(char=? current-char #\>)
           (set! current-index (add1 current-index))
           (if (and (< current-index line-length)
                    (char=? (string-ref line current-index) #\=))
               (begin
                 (set! tokens (append tokens (list (token TOKEN-GTE ">=" line-num))))
                 (set! current-index (add1 current-index)))
               (begin
                 (set! tokens (append tokens (list (token TOKEN-GT ">" line-num))))
                 (process-tokens)))]
          
          ;; Not equal (!=)
          [(and (char=? current-char #\!)
                (< (add1 current-index) line-length)
                (char=? (string-ref line (add1 current-index)) #\=))
           (set! tokens (append tokens (list (token TOKEN-NEQ "!=" line-num))))
           (set! current-index (add1 current-index))
           (set! current-index (add1 current-index))
           (process-tokens)]
          
          ;; Error - Invalid character
          [else
           (set! tokens (append tokens (list (token TOKEN-ERROR (string current-char) line-num))))
           (set! current-index (add1 current-index))
           (process-tokens)])))))

;; Helper function for while loop in Racket
(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

;; Start parsing functions

;; Get the next token
(define (get-next-token)
  (if (< next-token-index (length tokens))
      (begin
        (set! current-token (list-ref tokens next-token-index))
        (set! next-token-index (add1 next-token-index))
        current-token)
      ;; When we run out of tokens, just keep returning EOF
      ;; without raising an error
      (set! current-token (token TOKEN-EOF "" current-line))))

;; Match the expected token type
(define (match expected-type)
  (if (eq? (token-type current-token) expected-type)
      (let ([matched-token current-token])
        (get-next-token)
        matched-token)
      (error (format "Syntax error at line ~a: Expected ~a but found ~a" 
                    (token-line current-token) 
                    expected-type 
                    (token-type current-token)))))

;; Check if current token is of expected type without consuming it
(define (check expected-type)
  (eq? (token-type current-token) expected-type))

;; Helper function to check if a token is a comparison operator
(define (is-comparison-token? token-type)
  (memq token-type (list TOKEN-LT TOKEN-LTE TOKEN-GT TOKEN-GTE TOKEN-EQ TOKEN-NEQ)))

;; Parse a comparison operator 
(define (parse-comparison)
  (cond
    [(check TOKEN-LT)  (match TOKEN-LT)]
    [(check TOKEN-LTE) (match TOKEN-LTE)]
    [(check TOKEN-GT)  (match TOKEN-GT)]
    [(check TOKEN-GTE) (match TOKEN-GTE)]
    [(check TOKEN-EQ)  (match TOKEN-EQ)]
    [(check TOKEN-NEQ) (match TOKEN-NEQ)]
    [else (error (format "Syntax error at line ~a: Expected comparison operator" 
                         (token-line current-token)))]))

;; <program> -> <stmt_list> $$
(define (parse-program)
  (let ([tree (list 'program)])
    (get-next-token) ; Initialize current-token
    (set! tree (append tree (list (parse-stmt-list))))
    ;; No EOF check - just return the parse tree
    tree))

;; <stmt_list> -> <stmt> <stmt_list> | epsilon
(define (parse-stmt-list)
  (if (or (check TOKEN-ID) 
          (check TOKEN-READ) 
          (check TOKEN-WRITE) 
          (check TOKEN-IF))
      (let ([tree (list 'stmt_list)])
        (set! tree (append tree (list (parse-stmt))))
        (set! tree (append tree (list (parse-stmt-list))))
        tree)
      '(stmt_list epsilon)))

;; <stmt> -> id = <expr>; | if (<expr>) <stmt_list> endif; | read id; | write <expr>;
(define (parse-stmt)
  (let ([tree (list 'stmt)])
    (cond
      [(check TOKEN-ID)
       (let ([id-token (match TOKEN-ID)])
         (set! tree (append tree (list (token-value id-token))))
         (match TOKEN-ASSIGN) ; Match equals sign
         (set! tree (append tree (list (parse-expr))))
         (match TOKEN-SEMICOLON)
         tree)]
      
      [(check TOKEN-IF)
       (match TOKEN-IF)
       (match TOKEN-LPAREN) ; Match opening parenthesis
       (let ([expr (parse-expr)])
         (match TOKEN-RPAREN) ; Match closing parenthesis
         (set! tree (append tree (list 'if)))
         (set! tree (append tree (list expr)))
         (let ([stmt-list (parse-stmt-list)])
           (set! tree (append tree (list stmt-list)))
           (match TOKEN-ENDIF)
           (match TOKEN-SEMICOLON)
           tree))]
      
      [(check TOKEN-READ)
       (match TOKEN-READ)
       (let ([id-token (match TOKEN-ID)])
         (set! tree (append tree (list 'read)))
         (set! tree (append tree (list (token-value id-token))))
         (match TOKEN-SEMICOLON)
         tree)]
      
      [(check TOKEN-WRITE)
       (match TOKEN-WRITE)
       (set! tree (append tree (list 'write)))
       (set! tree (append tree (list (parse-expr))))
       (match TOKEN-SEMICOLON)
       tree]
      
      [else
       (error (format "Syntax error at line ~a: Expected statement but found ~a" 
                     (token-line current-token) 
                     (token-type current-token)))])))

;; <expr> -> <term> [comparison <term>]
(define (parse-expr)
  (let ([tree (list 'expr)])
    (let ([term (parse-term)])
      (set! tree (append tree (list term)))
      
      ;; Check if there's a comparison operator
      (if (is-comparison-token? (token-type current-token))
          (let ([comparison-op (parse-comparison)]
                [second-term (parse-term)])
            (set! tree (append tree 
                               (list (token-type comparison-op) 
                                     second-term)))
            tree)
          tree))))

;; <term> -> id | num | + <term> | - <term> | (expr)
(define (parse-term)
  (cond
    [(check TOKEN-ID)
     (let ([id-token (match TOKEN-ID)])
       (parse-term-tail (token-value id-token)))]
    
    [(check TOKEN-NUM)
     (let ([num-token (match TOKEN-NUM)])
       (parse-term-tail (token-value num-token)))]
    
    [(check TOKEN-PLUS)
     (match TOKEN-PLUS)
     (list '+ (parse-term))]
    
    [(check TOKEN-MINUS)
     (match TOKEN-MINUS)
     (list '- (parse-term))]
    
    [(check TOKEN-LPAREN)
     (match TOKEN-LPAREN)
     (let ([expr (parse-expr)])
       (match TOKEN-RPAREN)
       expr)]
    
    [else 
     (error (format "Syntax error at line ~a: Unexpected token" 
                    (token-line current-token)))]))

;; <term_tail> -> + <term> | - <term> | epsilon
(define (parse-term-tail initial-value)
  (cond
    [(check TOKEN-PLUS)
     (match TOKEN-PLUS)
     (list initial-value '+ (parse-term))]
    
    [(check TOKEN-MINUS)
     (match TOKEN-MINUS)
     (list initial-value '- (parse-term))]
    
    [else initial-value]))

;; Main parse function
(define (parse filename)
  (with-handlers 
      ([exn:fail? (lambda (e) (format "~a" (exn-message e)))])
    (let* ([input-string (file->string filename)]
           [tokens (tokenize input-string)]
           [parse-tree (parse-program)])
      (format "Accept\n~a" parse-tree))))

;; Function to read a file as a string
(define (file->string filename)
  (call-with-input-file filename
    (lambda (in)
      (let loop ([chars '()]
                 [char (read-char in)])
        (if (eof-object? char)
            (list->string (reverse chars))
            (loop (cons char chars) (read-char in)))))))

(displayln (parse "file3.txt"))