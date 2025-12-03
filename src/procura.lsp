;;;; procura.lisp
;;;; Implementação dos algoritmos de procura independentes do domínio
;;;; Projeto Nº 1 - Solitário
;;;; Inteligência Artificial 2025/2026

;;; ============================================================================
;;; ESTRUTURA DE NÓ
;;; ============================================================================

(defun criar-no (estado &optional (pai nil) (profundidade 0) (operador nil) (custo-g 0) (custo-h 0))
  "Cria um nó com o estado, pai, profundidade, operador usado, custo g e heurística h"
  (list estado pai profundidade operador custo-g custo-h))

(defun no-estado (no)
  "Retorna o estado do nó"
  (first no))

(defun no-pai (no)
  "Retorna o nó pai"
  (second no))

(defun no-profundidade (no)
  "Retorna a profundidade do nó"
  (third no))

(defun no-operador (no)
  "Retorna o operador que gerou este nó"
  (fourth no))

(defun no-custo-g (no)
  "Retorna o custo g (profundidade/custo do caminho)"
  (fifth no))

(defun no-custo-h (no)
  "Retorna o custo heurístico h"
  (sixth no))

(defun no-custo-f (no)
  "Retorna o custo total f = g + h"
  (+ (no-custo-g no) (no-custo-h no)))

;;; ============================================================================
;;; FUNÇÕES AUXILIARES
;;; ============================================================================

(defun no-existep (estado lista-nos)
  "Verifica se um estado já existe numa lista de nós"
  (cond
    ((null lista-nos) nil)
    ((equal estado (no-estado (first lista-nos))) t)
    (t (no-existep estado (rest lista-nos)))))

(defun no-solucaop (no fn-solucao)
  "Verifica se um nó é solução usando a função de teste fornecida"
  (funcall fn-solucao (no-estado no)))

(defun ordenar-nos (lista-nos &optional (comparador #'<))
  "Ordena uma lista de nós por f(n) = g(n) + h(n)"
  (sort (copy-list lista-nos) comparador :key #'no-custo-f))

(defun expandir-no (no operadores fn-heuristica &optional (profundidade-max most-positive-fixnum))
  "Expande um nó aplicando todos os operadores possíveis
   Retorna lista de nós sucessores"
  (let ((profundidade-atual (no-profundidade no)))
    (cond
      ((>= profundidade-atual profundidade-max) nil)
      (t (mapcan #'(lambda (op)
                     (let ((novo-estado (funcall op (no-estado no))))
                       (when novo-estado
                         (let* ((nova-profundidade (1+ profundidade-atual))
                                (novo-custo-g nova-profundidade)
                                (novo-custo-h (if fn-heuristica
                                                  (funcall fn-heuristica novo-estado)
                                                  0)))
                           (list (criar-no novo-estado 
                                          no 
                                          nova-profundidade 
                                          op 
                                          novo-custo-g 
                                          novo-custo-h))))))
                 operadores)))))

(defun obter-caminho (no &optional (caminho nil))
  "Reconstrói o caminho desde o nó inicial até ao nó dado
   Retorna lista de nós do início ao fim"
  (if (null no)
      caminho
      (obter-caminho (no-pai no) (cons no caminho))))

(defun obter-operadores-caminho (no)
  "Retorna a lista de operadores aplicados no caminho até este nó"
  (let ((caminho (obter-caminho no)))
    (mapcar #'no-operador (rest caminho))))

;;; ============================================================================
;;; ESTATÍSTICAS DE PROCURA
;;; ============================================================================

(defun calcular-penetrancia (profundidade-solucao nos-gerados)
  "Calcula a penetrância: profundidade / nós gerados"
  (if (zerop nos-gerados)
      0
      (/ profundidade-solucao nos-gerados)))

(defun calcular-fator-ramificacao (nos-gerados profundidade &optional (precisao 0.01))
  "Calcula o fator de ramificação médio usando método iterativo
   Resolve: nos-gerados = (b^(d+1) - 1) / (b - 1)"
  (cond
    ((or (zerop profundidade) (zerop nos-gerados)) 0)
    (t (calcular-fator-ramificacao-iterativo nos-gerados profundidade 1.0 precisao))))

(defun calcular-fator-ramificacao-iterativo (nos-gerados profundidade b precisao)
  "Método iterativo para calcular fator de ramificação"
  (let* ((resultado (/ (- (expt b (1+ profundidade)) 1) (- b 1)))
         (diferenca (abs (- resultado nos-gerados))))
    (cond
      ((< diferenca precisao) b)
      ((< resultado nos-gerados) 
       (calcular-fator-ramificacao-iterativo nos-gerados profundidade (+ b 0.1) precisao))
      (t (calcular-fator-ramificacao-iterativo nos-gerados profundidade (- b 0.01) precisao)))))

;;; ============================================================================
;;; PROCURA EM LARGURA (BFS)
;;; ============================================================================

(defun bfs (estado-inicial operadores fn-solucao &optional (tempo-inicio (get-internal-real-time)))
  "Procura em Largura (Breadth-First Search)
   Retorna: (solução nos-gerados nos-expandidos penetrância fator-ramificação tempo)"
  (bfs-aux (list (criar-no estado-inicial)) 
           operadores 
           fn-solucao 
           0 
           1 
           tempo-inicio))

(defun bfs-aux (abertos operadores fn-solucao nos-expandidos nos-gerados tempo-inicio)
  "Função auxiliar recursiva para BFS"
  (cond
    ((null abertos) nil)
    (t (let* ((no-atual (first abertos))
              (resto-abertos (rest abertos)))
         (cond
           ((no-solucaop no-atual fn-solucao)
            (let* ((tempo-fim (get-internal-real-time))
                   (tempo-decorrido (/ (- tempo-fim tempo-inicio) internal-time-units-per-second))
                   (profundidade (no-profundidade no-atual))
                   (penetrancia (calcular-penetrancia profundidade nos-gerados))
                   (fator-ram (calcular-fator-ramificacao nos-gerados profundidade)))
              (list no-atual nos-gerados nos-expandidos penetrancia fator-ram tempo-decorrido)))
           (t (let* ((sucessores (expandir-no no-atual operadores nil))
                     (novos-sucessores (remove-if #'(lambda (s)
                                                      (no-existep (no-estado s) abertos))
                                                  sucessores))
                     (novos-abertos (append resto-abertos novos-sucessores))
                     (novos-gerados (+ nos-gerados (length novos-sucessores))))
                (bfs-aux novos-abertos 
                        operadores 
                        fn-solucao 
                        (1+ nos-expandidos) 
                        novos-gerados 
                        tempo-inicio))))))))

;;; ============================================================================
;;; PROCURA EM PROFUNDIDADE (DFS)
;;; ============================================================================

(defun dfs (estado-inicial operadores fn-solucao profundidade-max &optional (tempo-inicio (get-internal-real-time)))
  "Procura em Profundidade (Depth-First Search)
   Retorna: (solução nos-gerados nos-expandidos penetrância fator-ramificação tempo)"
  (dfs-aux (list (criar-no estado-inicial)) 
           operadores 
           fn-solucao 
           profundidade-max
           0 
           1 
           tempo-inicio))

(defun dfs-aux (abertos operadores fn-solucao profundidade-max nos-expandidos nos-gerados tempo-inicio)
  "Função auxiliar recursiva para DFS"
  (cond
    ((null abertos) nil)
    (t (let* ((no-atual (first abertos))
              (resto-abertos (rest abertos)))
         (cond
           ((no-solucaop no-atual fn-solucao)
            (let* ((tempo-fim (get-internal-real-time))
                   (tempo-decorrido (/ (- tempo-fim tempo-inicio) internal-time-units-per-second))
                   (profundidade (no-profundidade no-atual))
                   (penetrancia (calcular-penetrancia profundidade nos-gerados))
                   (fator-ram (calcular-fator-ramificacao nos-gerados profundidade)))
              (list no-atual nos-gerados nos-expandidos penetrancia fator-ram tempo-decorrido)))
           (t (let* ((sucessores (expandir-no no-atual operadores nil profundidade-max))
                     (novos-sucessores (remove-if #'(lambda (s)
                                                      (no-existep (no-estado s) abertos))
                                                  sucessores))
                     (novos-abertos (append novos-sucessores resto-abertos))
                     (novos-gerados (+ nos-gerados (length novos-sucessores))))
                (dfs-aux novos-abertos 
                        operadores 
                        fn-solucao 
                        profundidade-max
                        (1+ nos-expandidos) 
                        novos-gerados 
                        tempo-inicio))))))))

;;; ============================================================================
;;; PROCURA A* (A-STAR)
;;; ============================================================================

(defun a-star (estado-inicial operadores fn-solucao fn-heuristica &optional (tempo-inicio (get-internal-real-time)))
  "Procura A* (A-Star)
   Retorna: (solução nos-gerados nos-expandidos penetrância fator-ramificação tempo)"
  (let ((custo-h-inicial (funcall fn-heuristica estado-inicial)))
    (a-star-aux (list (criar-no estado-inicial nil 0 nil 0 custo-h-inicial))
                operadores
                fn-solucao
                fn-heuristica
                nil
                0
                1
                tempo-inicio)))

(defun a-star-aux (abertos operadores fn-solucao fn-heuristica fechados nos-expandidos nos-gerados tempo-inicio)
  "Função auxiliar recursiva para A*"
  (cond
    ((null abertos) nil)
    (t (let* ((abertos-ordenados (ordenar-nos abertos))
              (no-atual (first abertos-ordenados))
              (resto-abertos (rest abertos-ordenados)))
         (cond
           ((no-solucaop no-atual fn-solucao)
            (let* ((tempo-fim (get-internal-real-time))
                   (tempo-decorrido (/ (- tempo-fim tempo-inicio) internal-time-units-per-second))
                   (profundidade (no-profundidade no-atual))
                   (penetrancia (calcular-penetrancia profundidade nos-gerados))
                   (fator-ram (calcular-fator-ramificacao nos-gerados profundidade)))
              (list no-atual nos-gerados nos-expandidos penetrancia fator-ram tempo-decorrido)))
           (t (let* ((sucessores (expandir-no no-atual operadores fn-heuristica))
                     (novos-sucessores (remove-if #'(lambda (s)
                                                      (or (no-existep (no-estado s) fechados)
                                                          (no-existep (no-estado s) abertos)))
                                                  sucessores))
                     (novos-abertos (append resto-abertos novos-sucessores))
                     (novos-fechados (cons no-atual fechados))
                     (novos-gerados (+ nos-gerados (length novos-sucessores))))
                (a-star-aux novos-abertos
                           operadores
                           fn-solucao
                           fn-heuristica
                           novos-fechados
                           (1+ nos-expandidos)
                           novos-gerados
                           tempo-inicio))))))))

;;; ============================================================================
;;; FUNÇÕES DE UTILIDADE PARA RESULTADOS
;;; ============================================================================

(defun extrair-solucao (resultado)
  "Extrai o nó solução do resultado"
  (first resultado))

(defun extrair-nos-gerados (resultado)
  "Extrai o número de nós gerados"
  (second resultado))

(defun extrair-nos-expandidos (resultado)
  "Extrai o número de nós expandidos"
  (third resultado))

(defun extrair-penetrancia (resultado)
  "Extrai a penetrância"
  (fourth resultado))

(defun extrair-fator-ramificacao (resultado)
  "Extrai o fator de ramificação"
  (fifth resultado))

(defun extrair-tempo (resultado)
  "Extrai o tempo de execução"
  (sixth resultado))