;;;; procura.lisp
;;;; Algoritmos de procura: BFS e DFS
;;;; Projeto01 IA - 25/26

;;; Estrutura do no

(defun cria-no (estado &optional (g 0) (pai nil))
  "Cria um no."
  (list estado g pai))

(defun no-estado (no)
  "Retorna o estado do no."
  (car no))

(defun no-g (no)
  "Retorna o custo/profundidade do no."
  (second no))

(defun no-pai (no)
  "Retorna o no pai do no."
  (third no))

;;; Funcoes aux

(defun construir-caminho (no-final)
  "Reconstrui o caminho desde o no inicial ate ao no-final."
  (labels ((aux (n acc)
             (if (null n)
                 acc
               (aux (no-pai n) (cons (no-estado n) acc)))))
    (aux no-final '())))

(defun no-existep (estado lista)
  "Verifica se ESTADO ja existe numa LISTA de NOS."
  (dolist (n lista nil)
    (when (equalp estado (no-estado n))
      (return t))))

(defun tempo-segundos (t0 t1)
  (/ (- t1 t0) (float internal-time-units-per-second)))

;;; BFS

(defun abertos-bfs (resto novos)
  (append resto novos))

(defun bfs (estado-inicial objetivo-p sucessores-fn)
  (let ((t0 (get-internal-real-time)))
    (labels
        ((bfs-aux (abertos fechados
                   nos-gerados nos-expandidos total-sucessores)

           (cond
             ((null abertos)
              (let* ((t1 (get-internal-real-time))
                     (tempo (/ (- t1 t0)
                               (float internal-time-units-per-second))))
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
                    (let* ((t1 (get-internal-real-time))
                           (tempo (/ (- t1 t0)
                                     (float internal-time-units-per-second)))
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
                         (novos-expandidos (1+ nos-expandidos))
                         (novo-total-sucessores
                          (+ total-sucessores (length sucessores)))

                         (novos-estados
                          (remove-if
                           (lambda (e)
                             (or (no-existep e fechados)
                                 (no-existep e abertos)))
                           sucessores))

                         (novos-nos
                          (mapcar (lambda (e)
                                    (cria-no e (1+ (no-g no-atual)) no-atual))
                                  novos-estados))

                         (novos-gerados
                          (+ nos-gerados (length novos-nos)))

                         (nova-fronteira
                          (append resto novos-nos))

                         (novos-fechados
                          (cons no-atual fechados)))

                    (bfs-aux nova-fronteira
                             novos-fechados
                             novos-gerados
                             novos-expandidos
                             novo-total-sucessores))))))))
      (bfs-aux (list (cria-no estado-inicial))
               '()
               1    
               0
               0))))

;;; DFS

(defun abertos-dfs (resto novos)
  (append novos resto))

(defun dfs (estado-inicial objetivo-p sucessores-fn profundidade-max)
  (let ((t0 (get-internal-real-time)))
    (labels
        ((dfs-aux (abertos fechados
                   nos-gerados nos-expandidos total-sucessores)

           (cond

             ((null abertos)
              (let* ((t1 (get-internal-real-time))
                     (tempo (/ (- t1 t0)
                               (float internal-time-units-per-second))))
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
                   (let* ((t1 (get-internal-real-time))
                          (tempo (/ (- t1 t0)
                                    (float internal-time-units-per-second)))
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
                          (novos-expandidos (1+ nos-expandidos))
                          (novo-total-sucessores
                           (+ total-sucessores (length sucessores)))

                          (novos-estados
                           (remove-if
                            (lambda (e)
                              (or (no-existep e fechados)
                                  (no-existep e abertos)))
                            sucessores))

                          (novos-nos
                           (mapcar (lambda (e)
                                     (cria-no e (1+ (no-g no-atual)) no-atual))
                                   novos-estados))

                          (novos-gerados
                           (+ nos-gerados (length novos-nos)))

                          (nova-fronteira
                           (append novos-nos resto))

                          (novos-fechados
                           (cons no-atual fechados)))

                     (dfs-aux nova-fronteira
                              novos-fechados
                              novos-gerados
                              novos-expandidos
                              novo-total-sucessores)))))))))

      (dfs-aux (list (cria-no estado-inicial))
               '()
               1   
               0
               0))))