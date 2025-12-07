;;;; projeto.lisp
;;;; Projeto01 IA - 25/26
;;;; Requer: puzzle.lisp, procura.lisp, problemas.dat

;;; Carregar modulos

(defparameter *base-path*
  (or *load-pathname*
      *compile-file-pathname*
      (error "Nao foi possivel determinar o caminho base.")))

(load (merge-pathnames "puzzle.lisp" *base-path*))
(load (merge-pathnames "procura.lisp" *base-path*))

;;; Ler ficheiro problemas.dat

(defun ler-problemas (ficheiro)
"Lê uma lista de tabuleiros separados por ###"
  (with-open-file (f ficheiro :direction :input)
    (let ((problemas '())
          (acc ""))

      (loop for linha = (read-line f nil 'eof)
            until (eq linha 'eof) do

              (cond
                ((string= linha "###")
                 (when (plusp (length (string-trim " " acc)))
                   (push (read-from-string acc) problemas))
                 (setf acc ""))

                (t
                 (when (stringp linha)
                   (setf acc
                         (concatenate 'string acc linha (string #\Newline)))))))

      (when (plusp (length (string-trim " " acc)))
        (push (read-from-string acc) problemas))

      (nreverse problemas))))

;;; Impressão do tabuleiro

(defun imprime-tabuleiro (tab)
  (format t "~%---------------- Tabuleiro ----------------~%")
  (dolist (linha tab)
    (dolist (c linha)
      (format t "~A "
              (cond
                ((null c) " ") 
                ((= c 1) "1")   
                ((= c 0) "0"))) 
      )
    (format t "~%"))
  (format t "---------------------------------------------~%"))

;;; Menus

(defun menu-problema (lista)
  (format t "~%=========== SELECIONE O PROBLEMA ===========~%")
  (loop for i from 1 to (length lista) do
        (format t "~A - Problema ~A~%" i i))
  (format t "Escolha: ")
  (let ((n (read)))
    (nth (1- n) lista)))

(defun menu-algoritmo ()
  (format t "~%=========== SELECIONE O ALGORITMO ==========~%")
  (format t "1 - BFS (Procura em Largura)~%")
  (format t "2 - DFS (Procura em Profundidade)~%")
  (format t "Escolha: ")
  (case (read)
    (1 'bfs)
    (2 'dfs)
    (t (menu-algoritmo))))

;;; Mostrar caminho encontrado

(defun mostrar-caminho (no-final)
  (let ((caminho (construir-caminho no-final)))
    (format t "~%========= SOLUCAO ENCONTRADA =========~%")
    (format t "Profundidade da solucao: ~A~%" (no-g no-final))
    (format t "Numero de passos: ~A~%" (length caminho))
    (format t "~%===== Caminho (estado por estado) =====~%")
    (dolist (estado caminho)
      (imprime-tabuleiro estado))
    (format t "~%========================================~%")
    no-final))

;;; Funcao principal do programa

(defun iniciar ()
  (format t "~%===== PROJETO 01 - PEG SOLITAIRE SOLVER =====~%")
  (format t "~%A carregar problemas...~%")

  (let* ((problemas (ler-problemas (merge-pathnames "problemas.dat" *base-path*)))
         (problema-escolhido (menu-problema problemas))
         (algoritmo (menu-algoritmo)))

    (format t "~%Problema escolhido:~%")
    (imprime-tabuleiro problema-escolhido)

    (cond

      ;; BFS
      ((eq algoritmo 'bfs)
       (format t "~%Executando BFS...~%")
       (let ((resultado (bfs problema-escolhido #'objetivo? #'gera-sucessores)))
         (if resultado
             (mostrar-caminho resultado)
             (format t "~%NAO FOI ENCONTRADA SOLUCAO.~%"))))

      ;; DFS
      ((eq algoritmo 'dfs)
       (format t "~%Qual a profundidade limite? ")
       (let ((lim (read)))
         (format t "~%Executando DFS com limite ~A...~%" lim)
         (let ((resultado (dfs problema-escolhido #'objetivo? #'gera-sucessores lim)))
           (if resultado
               (mostrar-caminho resultado)
               (format t "~%NAO FOI ENCONTRADA SOLUCAO.~%")))))

      (t
       (format t "~%Algoritmo invalido.~%")))))
