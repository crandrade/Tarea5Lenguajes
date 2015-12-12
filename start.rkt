#lang play

#|
<expr> ::= <num>
         | <id>
         | <bool>
         | (if <expr> <expr> <expr>)
         | (+ <expr> <expr>)
         | '< <s-expr> <s-expr>)
         | (* <s-expr> <s-expr>)
         | (= <s-expr> <s-expr>)
         | (- <s-expr> <s-expr>)
         | (and <s-expr> <s-expr>)
         | (or <s-expr> <s-expr>)
         | (not <s-expr> <s-expr>)         
         | (seqn <expr> <expr>)
         | (local { <def> ...} <expr>)

<def>    ::= (define <id> <expr>)


;EXTENSION PARA CLASE Y OBJETOS
<expr>  ::= ... (todo lo anterior)        
         | (class <expr> <member> ...)
         | (class <expr> <: <expr> <member> ...)
         | (new <expr>)
         | this
         | (super id <expr> ...)
         | (set <expr> <id> <expr>)
         | (get <expr> <id>)
         | (send <expr> <id> <expr> ...)

<member> ::= 
        | (field <id> <s-expr>)
        | (method <id> (list <id> ...) <s-expr>)

|#

(deftype Expr
  (num n)
  (bool b)
  (id s)   
  (binop f l r)
  (unop f s)
  (my-if c tb fb)  
  (seqn expr1 expr2)  
  (lcal defs body)
  (class father methods fields)
  (new e)
  (get e s)
  (set e s expr)
  (send e id nexpr)
  (this))

;; values
(deftype Val
  (numV n)
  (boolV b)
  (classV fields methods env))

(deftype Def
  (my-def id expr)
  (field id expr)
  (method fun)
  (father id)
  (Object))

;-----------------------------------------------------


#|-----------------------------
Environment abstract data type
 
empty-env        :: Env
env-lookup       :: Sym Env -> Val
multi-extend-env :: List<Sym> List<Val> Env -> Env
extend-frame-env! :: Sym Val Env -> Env 


 
representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv hash env)) 

(def empty-env (mtEnv))

#|
env-lookup:: Sym Env -> Val
Busca un símbol en el ambiente retornando su valor asociado
|#
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv hash rest)
     (if (hash-has-key? hash x)
         (hash-ref hash x)
         (env-lookup x rest))]))

#|
multi-extend-env:: List(Sym) List(Expr) Env -> Env
Crea un nuevo ambiente asociandos los símbolos a sus valores
|#
(define (multi-extend-env ids exprs env)
  (if (= (length ids) (length exprs))
      (aEnv (make-hash (map cons ids exprs)) env)
      (error "wrong_input, mismatched lengths")))

#|
extend-frame-env!:: Sym Val Env -> Void
Adiciona un nuevo par (Sym, Val) al frame del ambiente usando mutación.
Este método no crea un nuevo ambiente.
|#
(define (extend-frame-env! id val env)
  (match env
    [(mtEnv) (aEnv (make-hash (list (cons id val))) env)]
    [(aEnv h rEnv) (let* ([l (hash->list h)]
                          [la (cons (cons id val) l)])
                    (set-aEnv-hash! env (make-hash la)))]))
;-----------------------------------------------------

;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]    
    [(? boolean?) (bool s-expr)]
    [(list '* l r) (binop * (parse l) (parse r))]
    [(list '+ l r) (binop + (parse l) (parse r))]
    [(list '- l r) (binop - (parse l) (parse r))]
    [(list '< l r) (binop < (parse l) (parse r))]
    [(list '= l r) (binop = (parse l) (parse r))]    
    [(list 'or l r) (binop (λ (i d) (or i d)) (parse l) (parse r))]
    [(list 'and l r) (binop (λ (i d) (and i d)) (parse l) (parse r))]
    [(list 'not b) (unop not (parse b))]
    [(list 'class '<: cname cargs ...)
      (class (father (parse cname)) (map parse-def (filter (λ (x) (symbol=? 'field (car x))) cargs)) (map parse-def (filter (λ (y) (symbol=? 'method (car y))) cargs)))]
    [(list 'class cargs ...) 
      (class (father (Object)) (map parse-def (filter (λ (x) (symbol=? 'field (car x))) cargs)) (map parse-def (filter (λ (y) (symbol=? 'method (car y))) cargs)))]
    [(list 'new c) (new (parse c))]
    [(list 'send a b c) (send a b (parse c))]
    [(list 'get e s) (get e s)]
    [(list 'set e s val) (set e s (parse val))]
    [(list 'method id args body) (method id args (parse body))]
    [(list 'field id args) (field id (parse args))]
    [(list 'if c t f) (my-if (parse c)
                             (parse t)
                             (parse f))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]    
    [(list 'local (list e ...)  b)
     (lcal (map parse-def e) (parse b))]
    ))


;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list 'define id b) (my-def id (parse b))]
    [(list 'field fname init) (field fname (parse init))]
    [(list 'method mname args body) (method (lambda args (parse body)))]))

;; interp :: Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]    
    [(bool b) (boolV b)]    
    [(binop f l r) (make-val (f (open-val (interp l env)) (open-val (interp r env))))]
    [(unop f s) (make-val (f (open-val (interp s env))))]
    [(my-if c t f)
     (def (boolV cnd) (interp c env))
     (if cnd
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env)]
    [(new c) (env-lookup c env)]
    [(class father fields methods) 
     (define (classV fields methods env) (env-lookup (father-id father) env))
     (map (λ (x) (interp-def x env)) fields)]
    [(seqn expr1 expr2) (begin 
                          (interp expr1 env)
                          (interp expr2 env))]
    [(lcal defs body)
     (let* ([new-env (multi-extend-env '() '() env)])
           (for-each (λ(x)
                       (let ([in-def (interp-def x new-env)])
                         (extend-frame-env! (car in-def) (cdr in-def) new-env)
                         #t)) defs)       
       (interp body new-env))     
     ]))

;; open-val :: Val -> Scheme Value
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]
    ))

;; make-val :: Scheme Value -> Val
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]
    ))

;; interp-def :: Def, Env -> Expr
(define (interp-def a-def env)
  (match a-def
    [(my-def id body) (cons id (interp body env))]
    [(field id body) (cons id (interp body env))]
    [(method fun) fun]
    ))

;; run :: s-expr -> Val
(define (run s-expr)
  (interp (parse s-expr) empty-env))

#|
run-val:: s-expr -> Scheme-Val + Val
A run version that returns scheme values for primitive values and mini-scheme values for classes and objects
|#
(define (run-val s-expr)
  (define val (interp (parse s-expr) empty-env))
  (match val
    [(numV n) n]
    [(boolV b) b]
    [x x]))



;; this es dinámico