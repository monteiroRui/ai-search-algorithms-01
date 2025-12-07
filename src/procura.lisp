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

;;; BFS

(defun abertos-bfs (resto novos)
  (append resto novos))

(defun bfs (estado-inicial objetivo-p sucessores-fn)
  (let ((abertos (list (cria-no estado-inicial)))
        (fechados '()))
    (labels ((bfs-aux (abertos fechados)
               (if (null abertos)
                   nil
                 (let* ((no-atual (car abertos))
                        (resto (cdr abertos))
                        (estado (no-estado no-atual)))
                   (if (funcall objetivo-p estado)
                       no-atual
                     (let* ((sucessores-estados
                              (funcall sucessores-fn estado))
                            (novos-nos-estados
                              (remove-if
                               (lambda (e)
                                 (or (no-existep e fechados)
                                     (no-existep e abertos)))
                               sucessores-estados))
                            (novos-nos
                              (mapcar (lambda (e)
                                        (cria-no e (1+ (no-g no-atual)) no-atual))
                                      novos-nos-estados))
                            (nova-fronteira
                              (abertos-bfs resto novos-nos))
                            (novos-fechados
                              (cons no-atual fechados)))
                       (bfs-aux nova-fronteira novos-fechados)))))))
      (bfs-aux abertos fechados))))

;;; DFS

(defun abertos-dfs (resto novos)
  (append novos resto))

(defun dfs (estado-inicial objetivo-p sucessores-fn profundidade-max)
  (let ((abertos (list (cria-no estado-inicial)))
        (fechados '()))
    (labels ((dfs-aux (abertos fechados)
               (if (null abertos)
                   nil
                 (let* ((no-atual (car abertos))
                        (resto (cdr abertos))
                        (estado (no-estado no-atual)))
                   (if (funcall objetivo-p estado)
                       no-atual
                     (if (>= (no-g no-atual) profundidade-max)
                         (dfs-aux resto fechados)
                       (let* ((sucessores-estados
                                (funcall sucessores-fn estado))
                              (novos-estados
                                (remove-if
                                 (lambda (e)
                                   (or (no-existep e fechados)
                                       (no-existep e abertos)))
                                 sucessores-estados))
                              (novos-nos
                                (mapcar (lambda (e)
                                          (cria-no e (1+ (no-g no-atual)) no-atual))
                                        novos-estados))
                              (nova-fronteira
                                (abertos-dfs resto novos-nos))
                              (novos-fechados
                                (cons no-atual fechados)))
                         (dfs-aux nova-fronteira novos-fechados))))))))
      (dfs-aux abertos fechados))))
