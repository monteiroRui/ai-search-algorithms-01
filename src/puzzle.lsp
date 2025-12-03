;;;; puzzle.lisp
;;;; Lógica específica do Peg Solitaire (Construída com base no LAB07)
;;;; Projeto01 IA - 25/26

;;; Representação do Tabuleiro (exemplo de teste incluído)
(defun tabuleiro-inicial ()
"Tabuleiro inicial padrão com uma posição vazia no centro."
  '(
    (nil nil 1 1 1 nil nil)
    (nil nil 1 1 1 nil nil)
    (1 1 1 1 1 1 1)
    (1 1 1 0 1 1 1)
    (1 1 1 1 1 1 1)
    (nil nil 1 1 1 nil nil)
    (nil nil 1 1 1 nil nil)))

;;; Seletores

;; Teste: (linha 1 (tabuleiro-inicial)) 
(defun linha (i tabuleiro)
"Função que recebe um índice e o tabuleiro e retorna uma lista que representa essa linha do
 tabuleiro."
  (nth (1- i) tabuleiro))

;; Teste: (coluna 1 (tabuleiro-inicial)) 
(defun coluna (j tabuleiro)
"Função que recebe um índice e o tabuleiro e retorna uma lista que representa essa coluna do
 tabuleiro."
  (mapcar (lambda (linha) (nth (1- j) linha)) tabuleiro))

;; Teste: (celula 4 4 (tabuleiro-inicial))
(defun celula (i j tabuleiro)
"Função que recebe dois índices e o tabuleiro e retorna o valor presente nessa célula do
 tabuleiro."
  (let ((row (linha i tabuleiro)))
    (if row (nth (1- j) row) nil)))

;;; Funções auxiliares

;; Teste: (celula-valida 1 1 (tabuleiro-inicial))
(defun celula-valida (i j tabuleiro)
"Função predicado que recebe os índices da linha e da coluna e o tabuleiro."
  (let ((val (celula i j tabuleiro)))
    (if (or (equal val 1) (equal val 0)) t nil)))

;; Teste: (substituir-posicao 3 (linha 1 (tabuleiro-inicial)) 0)
(defun substituir-posicao (n lista valor)
"Função que recebe um índice, uma lista e um valor e substitui pelo valor
 pretendido nessa posição."
  (append (subseq lista 0 (1- n))
          (list valor)
          (nthcdr n lista)))

;; Teste: (substituir 1 3 (tabuleiro-inicial) 0)
(defun substituir (i j tabuleiro valor)
"Função que recebe dois índices, o tabuleiro e um valor. A função deverá retornar o
 tabuleiro com a célula substituída pelo valor pretendido."
  (let* ((linha-atual (linha i tabuleiro))
         (nova-linha (substituir-posicao j linha-atual valor)))
    (substituir-posicao i tabuleiro nova-linha)))

;;; Operadores

;; Teste: (operador-cd 4 2 (tabuleiro-inicial))
(defun operador-cd (i j tabuleiro)
"Movimento de captura para a direita: (i,j) = (i, j+2)"
  (when (and (>= j 1) (<= j 5)        ; j+2 tem de caber no [1,7]
             (celula-valida i j tabuleiro)
             (equal (celula i j tabuleiro) 1)
             (equal (celula i (+ j 1) tabuleiro) 1)
             (equal (celula i (+ j 2) tabuleiro) 0))
    (let ((t1 (substituir i j tabuleiro 0)))
      (let ((t2 (substituir i (+ j 1) t1 0)))
        (substituir i (+ j 2) t2 1)))))

;; Teste: (operador-ce 4 6 (tabuleiro-inicial))
(defun operador-ce (i j tabuleiro)
"Movimento de captura para a esquerda: (i,j) = (i, j-2)"
  (when (and (> j 2)                  ; j-2 >= 1
             (celula-valida i j tabuleiro)
             (equal (celula i j tabuleiro) 1)
             (equal (celula i (- j 1) tabuleiro) 1)
             (equal (celula i (- j 2) tabuleiro) 0))
    (let ((t1 (substituir i j tabuleiro 0)))
      (let ((t2 (substituir i (- j 1) t1 0)))
        (substituir i (- j 2) t2 1)))))

;; Teste: (operador-cc 6 4 (tabuleiro-inicial)) 
(defun operador-cc (i j tabuleiro)
"Movimento de captura para cima: (i,j) = (i-2, j)"
  (when (and (> i 2)                  ; i-2 >= 1
             (celula-valida i j tabuleiro)
             (equal (celula i j tabuleiro) 1)
             (equal (celula (- i 1) j tabuleiro) 1)
             (equal (celula (- i 2) j tabuleiro) 0))
    (let ((t1 (substituir i j tabuleiro 0)))
      (let ((t2 (substituir (- i 1) j t1 0)))
        (substituir (- i 2) j t2 1)))))

;; Teste: (operador-cb 2 4 (tabuleiro-inicial)) 
(defun operador-cb (i j tabuleiro)
"Movimento de captura para baixo: (i,j) = (i+2, j)"
  (when (and (>= i 1) (<= i 5)        ; i+2 <= 7
             (celula-valida i j tabuleiro)
             (equal (celula i j tabuleiro) 1)
             (equal (celula (+ i 1) j tabuleiro) 1)
             (equal (celula (+ i 2) j tabuleiro) 0))
    (let ((t1 (substituir i j tabuleiro 0)))
      (let ((t2 (substituir (+ i 1) j t1 0)))
        (substituir (+ i 2) j t2 1)))))

;;; Objetivo

;; Teste: (objetivo? (tabuleiro-inicial))
(defun objetivo? (tabuleiro)
  (= (contar-pecas tabuleiro) 1))

;; Teste: (contar-pecas (tabuleiro-inicial))
(defun contar-pecas (tabuleiro)
  (reduce #'+ (mapcar (lambda (linha)
                        (count 1 linha))
                      tabuleiro)))

;;; Geração de sucessores

;; Teste: (aplicar-operadores 4 2 (tabuleiro-inicial))
(defun aplicar-operadores (i j tabuleiro)
"Aplica todos os operadores possíveis na posição (i,j) e devolve
 uma lista de novos tabuleiros válidos."
  (remove nil
          (list
           (operador-cd i j tabuleiro)
           (operador-ce i j tabuleiro)
           (operador-cc i j tabuleiro)
           (operador-cb i j tabuleiro))))

(defun gera-sucessores-aux (i j tabuleiro)
"Função auxiliar recursiva para varrer o tabuleiro e gerar sucessores."
  (cond
   ((> i 7) '())
   ((> j 7) (gera-sucessores-aux (1+ i) 1 tabuleiro))
   (t
    (let ((depois-desta-pos (if (equal (celula i j tabuleiro) 1)
                                (aplicar-operadores i j tabuleiro)
                              '())))
      (append depois-desta-pos
              (gera-sucessores-aux i (1+ j) tabuleiro))))))

;; Teste: (gera-sucessores (tabuleiro-inicial))
;; Teste: (length (gera-sucessores (tabuleiro-inicial)))
(defun gera-sucessores (tabuleiro)
"Devolve a lista de todos os tabuleiros alcançáveis com um único movimento."
  (gera-sucessores-aux 1 1 tabuleiro))

;;; Heurísticas

;; Teste: (h1 (tabuleiro-inicial))
(defun h1 (tabuleiro)
  (/ 1 (1+ (length (gera-sucessores tabuleiro)))))

;;; ============================================================================
;;; PROBLEMAS PREDEFINIDOS
;;; ============================================================================

(defun problema-a ()
  "Problema A - quase resolvido, faltam 2 peças"
  '((nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)
    (0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0)
    (0 0 0 0 1 1 0)
    (nil nil 0 1 0 nil nil)
    (nil nil 0 0 0 nil nil)))

(defun problema-b ()
  "Problema B - 3 peças restantes"
  '((nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)
    (0 0 0 0 0 0 0)
    (0 0 0 1 0 0 0)
    (0 0 0 1 1 1 0)
    (nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)))

(defun problema-c ()
  "Problema C - 5 peças restantes"
  '((nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)
    (0 0 0 0 1 0 0)
    (0 0 0 0 1 1 0)
    (0 0 0 1 1 1 0)
    (nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)))

(defun problema-d ()
  "Problema D - 9 peças restantes"
  '((nil nil 0 0 0 nil nil)
    (nil nil 0 0 1 nil nil)
    (0 0 0 0 1 1 0)
    (0 0 0 0 1 1 1)
    (0 0 0 1 1 1 1)
    (nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)))

(defun problema-e ()
  "Problema E - 10 peças restantes"
  '((nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)
    (0 0 0 1 1 1 1)
    (0 0 0 0 1 1 1)
    (0 0 0 1 1 1 1)
    (nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)))

(defun problema-f ()
  "Problema F - tabuleiro inicial completo (32 peças)"
  (tabuleiro-inicial))