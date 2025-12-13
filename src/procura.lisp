;;;; procura.lisp
;;;; Algoritmos de procura: BFS, DFS e A*
;;;; Projeto01 IA - 25/26

;;; =========================================================
;;; Estrutura do nó
;;; =========================================================

(defun cria-no (estado &optional (g 0) (pai nil) (h 0))
  "Cria um nó: (estado g pai h)."
  (list estado g pai h))

(defun no-estado (no) (car no))
(defun no-g (no) (second no))
(defun no-pai (no) (third no))
(defun no-h (no) (fourth no))

(defun no-f (no)
  "f(n) = g(n) + h(n)"
  (+ (no-g no) (no-h no)))

;;; =========================================================
;;; Funções auxiliares
;;; =========================================================

(defun construir-caminho (no-final)
  "Reconstrói o caminho desde o nó inicial até ao nó final."
  (labels ((aux (n acc)
             (if (null n)
                 acc
               (aux (no-pai n) (cons (no-estado n) acc)))))
    (aux no-final '())))

(defun no-existep (estado lista-nos)
  "Verifica se ESTADO já existe numa LISTA de NÓS."
  (dolist (n lista-nos nil)
    (when (equalp estado (no-estado n))
      (return t))))

(defun estado-existep (estado lista-estados)
  "Verifica se ESTADO já existe numa LISTA de ESTADOS."
  (dolist (e lista-estados nil)
    (when (equalp estado e)
      (return t))))

(defun tempo-segundos (t0 t1)
  (/ (- t1 t0) (float internal-time-units-per-second)))

;;; =========================================================
;;; BFS
;;; =========================================================

(defun bfs (estado-inicial objetivo-p sucessores-fn)
  (let ((t0 (get-internal-real-time)))
    (labels
        ((bfs-aux (abertos fechados nos-gerados nos-expandidos total-sucessores)
           (cond
             ((null abertos)
              (let ((tempo (tempo-segundos t0 (get-internal-real-time))))
                (values nil
                        nos-gerados
                        nos-expandidos
                        (if (> nos-expandidos 0)
                            (/ total-sucessores (float nos-expandidos))
                            0.0)
                        0.0
                        tempo)))

             (t
              (let* ((no-atual (car abertos))
                     (resto (cdr abertos))
                     (estado (no-estado no-atual)))

                (if (funcall objetivo-p estado)
                    (let* ((tempo (tempo-segundos t0 (get-internal-real-time)))
                           (prof (no-g no-atual))
                           (pen (if (> nos-gerados 0)
                                    (/ prof (float nos-gerados))
                                    0.0)))
                      (values no-atual
                              nos-gerados
                              nos-expandidos
                              (if (> nos-expandidos 0)
                                  (/ total-sucessores (float nos-expandidos))
                                  0.0)
                              pen
                              tempo))

                  (let* ((sucessores (funcall sucessores-fn estado))
                         (novo-total-sucessores
                          (+ total-sucessores (length sucessores)))

                         (novos-estados
                          (remove-if
                           (lambda (e)
                             (or (no-existep e fechados)
                                 (no-existep e abertos)))
                           sucessores))

                         (novos-nos
                          (mapcar
                           (lambda (e)
                             (cria-no e (1+ (no-g no-atual)) no-atual))
                           novos-estados)))

                    (bfs-aux
                     (append resto novos-nos)
                     (cons no-atual fechados)
                     (+ nos-gerados (length novos-nos))
                     (1+ nos-expandidos)
                     novo-total-sucessores))))))))
      (bfs-aux (list (cria-no estado-inicial))
               '()
               1
               0
               0))))

;;; =========================================================
;;; DFS
;;; =========================================================

(defun dfs (estado-inicial objetivo-p sucessores-fn profundidade-max)
  (let ((t0 (get-internal-real-time)))
    (labels
        ((dfs-aux (abertos fechados nos-gerados nos-expandidos total-sucessores)
           (cond
             ((null abertos)
              (let ((tempo (tempo-segundos t0 (get-internal-real-time))))
                (values nil
                        nos-gerados
                        nos-expandidos
                        (if (> nos-expandidos 0)
                            (/ total-sucessores (float nos-expandidos))
                            0.0)
                        0.0
                        tempo)))

             (t
              (let* ((no-atual (car abertos))
                     (resto (cdr abertos))
                     (estado (no-estado no-atual)))

                (cond
                  ((funcall objetivo-p estado)
                   (let* ((tempo (tempo-segundos t0 (get-internal-real-time)))
                          (prof (no-g no-atual))
                          (pen (if (> nos-gerados 0)
                                   (/ prof (float nos-gerados))
                                   0.0)))
                     (values no-atual
                             nos-gerados
                             nos-expandidos
                             (if (> nos-expandidos 0)
                                 (/ total-sucessores (float nos-expandidos))
                                 0.0)
                             pen
                             tempo)))

                  ((>= (no-g no-atual) profundidade-max)
                   (dfs-aux resto
                            fechados
                            nos-gerados
                            nos-expandidos
                            total-sucessores))

                  (t
                   (let* ((sucessores (funcall sucessores-fn estado))
                          (novo-total-sucessores
                           (+ total-sucessores (length sucessores)))

                          (novos-estados
                           (remove-if
                            (lambda (e)
                              (or (no-existep e fechados)
                                  (no-existep e abertos)))
                            sucessores))

                          (novos-nos
                           (mapcar
                            (lambda (e)
                              (cria-no e (1+ (no-g no-atual)) no-atual))
                            novos-estados)))

                     (dfs-aux
                      (append novos-nos resto)
                      (cons no-atual fechados)
                      (+ nos-gerados (length novos-nos))
                      (1+ nos-expandidos)
                      novo-total-sucessores)))))))))
      (dfs-aux (list (cria-no estado-inicial))
               '()
               1
               0
               0))))

;;; =========================================================
;;; A*
;;; =========================================================

(defun insere-ordenado-f (no lista)
  (cond
    ((null lista) (list no))
    ((<= (no-f no) (no-f (car lista)))
     (cons no lista))
    (t
     (cons (car lista)
           (insere-ordenado-f no (cdr lista))))))

(defun insere-muitos-ordenado-f (nos lista)
  (if (null nos)
      lista
      (insere-muitos-ordenado-f
       (cdr nos)
       (insere-ordenado-f (car nos) lista))))

(defun a-star (estado-inicial objetivo-p sucessores-fn
                 &optional (heuristica-fn #'h1))
  "A* clássico, iterativo."
  (let* ((t0 (get-internal-real-time))
         (abertos
          (list (cria-no estado-inicial
                          0
                          nil
                          (funcall heuristica-fn estado-inicial))))
         (fechados '())
         (nos-gerados 1)
         (nos-expandidos 0)
         (total-sucessores 0))

    (labels
        ((finalizar (no-solucao)
           (let* ((tempo (tempo-segundos t0 (get-internal-real-time)))
                  (prof (if no-solucao (no-g no-solucao) 0))
                  (ramo (if (> nos-expandidos 0)
                            (/ total-sucessores
                               (float nos-expandidos))
                            0.0))
                  (pen (if (and no-solucao (> nos-gerados 0))
                           (/ prof (float nos-gerados))
                           0.0)))
             (values no-solucao
                     nos-gerados
                     nos-expandidos
                     ramo
                     pen
                     tempo))))

      (loop
        (when (null abertos)
          (return (finalizar nil)))

        (let* ((no-atual (car abertos))
               (estado (no-estado no-atual)))
          (setf abertos (cdr abertos))

          (when (estado-existep estado fechados)
            (continue))

          (when (funcall objetivo-p estado)
            (return (finalizar no-atual)))

          (setf fechados (cons estado fechados))
          (incf nos-expandidos)

          (let* ((sucessores (funcall sucessores-fn estado)))
            (incf total-sucessores (length sucessores))

            (let* ((novos-estados
                    (remove-if
                     (lambda (e)
                       (or (estado-existep e fechados)
                           (no-existep e abertos)))
                     sucessores))
                   (novos-nos
                    (mapcar
                     (lambda (e)
                       (cria-no e
                                (1+ (no-g no-atual))
                                no-atual
                                (funcall heuristica-fn e)))
                     novos-estados)))

              (incf nos-gerados (length novos-nos))
              (setf abertos
                    (insere-muitos-ordenado-f
                     novos-nos
                     abertos)))))))))
